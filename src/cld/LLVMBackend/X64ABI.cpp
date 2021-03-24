#include "X64ABI.hpp"

#include "CodeGenerator.hpp"

cld::CGLLVM::X64ABI::X64ABI(const llvm::DataLayout& dataLayout) : CommonABIImpl(dataLayout) {}

namespace
{
std::vector<llvm::Type*> flatten(llvm::Type* type)
{
    std::vector<llvm::Type*> result;
    if (type->isStructTy())
    {
        for (std::size_t i = 0; i < type->getStructNumElements(); i++)
        {
            auto* element = type->getStructElementType(i);
            auto temp = flatten(element);
            result.insert(result.end(), temp.begin(), temp.end());
        }
    }
    else if (type->isArrayTy())
    {
        auto temp = flatten(type->getArrayElementType());
        for (std::size_t i = 0; i < type->getArrayNumElements(); i++)
        {
            result.insert(result.end(), temp.begin(), temp.end());
        }
    }
    else
    {
        result.emplace_back(type);
    }
    return result;
}

std::tuple<cld::CGLLVM::X64ABIImpl::Adjustments::Arg, llvm::Type * CLD_NON_NULL, llvm::Type * CLD_NULLABLE>
    flattenSingleArg(const llvm::DataLayout& dataLayout, llvm::Type* type, std::uint8_t* takenIntegers = nullptr,
                     std::uint8_t* takenFloats = nullptr)
{
    using namespace cld::CGLLVM::X64ABIImpl;

    constexpr std::uint8_t availableIntegerRegisters = 6;
    constexpr std::uint8_t availableFloatingPointerRegisters = 8;
    Adjustments::Arg dest;
    std::size_t retIndex = 0;
    std::array<llvm::Type*, 2> ret = {};

    std::uint8_t takenIntegerRegisters = takenIntegers ? *takenIntegers : 0;
    std::uint8_t takenFloatingPointRegisters = takenFloats ? *takenFloats : 0;
    const auto flat = flatten(type);
    auto iter = flat.begin();
    while (iter != flat.end())
    {
        const auto begin = iter;
        bool encounteredInteger = false;
        std::size_t size = 0;
        std::size_t currentAlignment = 0;
        while (size < 8 && iter != flat.end())
        {
            const auto alignment = dataLayout.getABITypeAlign(*iter).value();
            const auto temp = cld::roundUpTo(size, alignment);
            if (temp >= 8)
            {
                break;
            }
            size = temp;
            if ((*iter)->isIntOrPtrTy())
            {
                encounteredInteger = true;
            }
            if ((*iter)->isX86_FP80Ty())
            {
                if (*iter != type)
                {
                    return {OnStack{}, type, nullptr};
                }
            }
            currentAlignment = std::max(currentAlignment, alignment);
            const auto typeSize = dataLayout.getTypeAllocSize(*iter).getKnownMinSize();
            size += typeSize;
            iter++;
        }
        size = cld::roundUpTo(size, currentAlignment);
        if (encounteredInteger)
        {
            // We encountered at least one integer therefore even if a floating point type was in there
            // it's gotta go into a integer register
            if (takenIntegerRegisters >= availableIntegerRegisters)
            {
                if (type->isStructTy())
                {
                    return {OnStack{}, type, nullptr};
                }
                return {Unchanged{}, type, nullptr};
            }

            takenIntegerRegisters++;
            if (type->isStructTy() && !std::holds_alternative<MultipleArgs>(dest))
            {
                dest = MultipleArgs{};
            }
            if (type->isStructTy())
            {
                ret[retIndex++] = llvm::IntegerType::get(type->getContext(), size * 8);
            }
            else
            {
                ret[retIndex++] = type;
            }
            if (auto* multiArgs = std::get_if<MultipleArgs>(&dest))
            {
                multiArgs->size++;
            }
            continue;
        }
        if (std::distance(begin, iter) == 2 && (*begin)->isFloatTy() && ((*(begin + 1))->isFloatTy()))
        {
            // Two floats can be packed as a single 64 bit value int oa  xmm register. This is represented as a vector
            // in LLVM IR
            if (takenFloatingPointRegisters >= availableFloatingPointerRegisters)
            {
                if (type->isStructTy())
                {
                    return {OnStack{}, type, nullptr};
                }
                return {Unchanged{}, type, nullptr};
            }
            takenFloatingPointRegisters++;
            if (!std::holds_alternative<MultipleArgs>(dest))
            {
                dest = MultipleArgs{};
            }
            ret[retIndex++] = llvm::FixedVectorType::get(*begin, 2);
            cld::get<MultipleArgs>(dest).size++;
            continue;
        }

        CLD_ASSERT(std::distance(begin, iter) == 1);
        // Must be a floating point type because if it were integer it would have taken the encounteredInteger branch
        // above
        if (takenFloatingPointRegisters >= availableFloatingPointerRegisters)
        {
            if (type->isStructTy())
            {
                return {OnStack{}, type, nullptr};
            }
            return {Unchanged{}, type, nullptr};
        }
        takenFloatingPointRegisters++;
        if (type->isStructTy() && !std::holds_alternative<MultipleArgs>(dest))
        {
            dest = MultipleArgs{};
        }
        ret[retIndex++] = *begin;
        if (auto* multiArgs = std::get_if<MultipleArgs>(&dest))
        {
            multiArgs->size++;
        }
    }
    if (takenFloats)
    {
        *takenFloats = takenFloatingPointRegisters;
    }
    if (takenIntegers)
    {
        *takenIntegers = takenIntegerRegisters;
    }
    return {dest, ret[0], ret[1]};
}

} // namespace

cld::CGLLVM::X64ABIImpl::Adjustments cld::CGLLVM::X64ABI::applyPlatformABIImpl(llvm::Type*& returnType,
                                                                               std::vector<llvm::Type*>& arguments)
{
    X64ABIImpl::Adjustments adjustments;
    adjustments.arguments.resize(arguments.size());
    std::uint8_t takenIntegerRegisters = 0;
    std::uint8_t takenFloatingPointRegisters = 0;
    std::size_t transFormIndex = 0;
    for (auto arg = arguments.begin(); arg != arguments.end(); transFormIndex++, arg++)
    {
        auto& dest = adjustments.arguments[transFormIndex];
        if (m_dataLayout.getTypeAllocSizeInBits(*arg) > 128)
        {
            dest = X64ABIImpl::OnStack{};
            *arg = (*arg)->getPointerTo(0);
            continue;
        }
        std::pair<llvm::Type*, llvm::Type*> types;
        std::tie(dest, types.first, types.second) =
            flattenSingleArg(m_dataLayout, *arg, &takenIntegerRegisters, &takenFloatingPointRegisters);
        if (std::holds_alternative<X64ABIImpl::OnStack>(dest))
        {
            *arg = (*arg)->getPointerTo(0);
        }
        else if (std::holds_alternative<X64ABIImpl::MultipleArgs>(dest))
        {
            *arg = types.first;
            if (types.second)
            {
                arg++;
                arg = arguments.insert(arg, types.second);
            }
        }
    }
    if (!returnType->isVoidTy())
    {
        std::uint32_t size = m_dataLayout.getTypeAllocSizeInBits(returnType);
        if (size > 128)
        {
            adjustments.returnType = X64ABIImpl::PointerToTemporary{};
            arguments.insert(arguments.begin(), returnType->getPointerTo(0));
            returnType = llvm::Type::getVoidTy(returnType->getContext());
        }
        else
        {
            bool wasStruct = returnType->isStructTy();
            std::pair<llvm::Type*, llvm::Type*> types;
            std::tie(std::ignore, types.first, types.second) = flattenSingleArg(m_dataLayout, returnType);
            if (types.second)
            {
                returnType = llvm::StructType::get(types.first, types.second);
            }
            else
            {
                returnType = types.first;
            }
            if (wasStruct)
            {
                adjustments.returnType = X64ABIImpl::Flattened{};
            }
        }
    }
    return adjustments;
}

llvm::AttributeList cld::CGLLVM::X64ABI::generateFunctionAttributes(llvm::AttributeList attributesIn,
                                                                    const llvm::FunctionType* llvmFunctionType,
                                                                    const Semantics::FunctionType& functionType,
                                                                    const Semantics::ProgramInterface& programInterface)
{
    auto& adjustment = getAdjustment(functionType);
    std::size_t argStart = 0;
    llvm::LLVMContext& context = llvmFunctionType->getContext();
    if (std::holds_alternative<X64ABIImpl::PointerToTemporary>(adjustment.returnType))
    {
        attributesIn = attributesIn.addAttribute(context, 1, llvm::Attribute::StructRet);
        attributesIn = attributesIn.addAttribute(context, 1, llvm::Attribute::NoAlias);
        argStart = 1;
    }
    else if (std::holds_alternative<X64ABIImpl::Unchanged>(adjustment.returnType)
             && Semantics::isInteger(functionType.getReturnType()))
    {
        auto& prim = functionType.getReturnType().cast<Semantics::PrimitiveType>();
        if (prim.getBitCount() < 32)
        {
            if (prim.isSigned())
            {
                attributesIn = attributesIn.addAttribute(context, 0, llvm::Attribute::SExt);
            }
            else
            {
                attributesIn = attributesIn.addAttribute(context, 0, llvm::Attribute::ZExt);
            }
        }
    }
    std::size_t origArgI = 0;
    for (std::size_t i = argStart; i < llvmFunctionType->getNumParams(); origArgI++)
    {
        cld::match(
            adjustment.arguments[origArgI],
            [&](X64ABIImpl::Unchanged) {
                cld::ScopeExit exit{[&] { i++; }};
                auto& arg = *functionType.getParameters()[origArgI].type;
                if (!Semantics::isInteger(arg))
                {
                    return;
                }
                auto& prim = arg.cast<Semantics::PrimitiveType>();
                if (prim.getBitCount() >= 32)
                {
                    return;
                }
                if (prim.isSigned())
                {
                    attributesIn = attributesIn.addParamAttribute(context, i, llvm::Attribute::SExt);
                }
                else
                {
                    attributesIn = attributesIn.addParamAttribute(context, i, llvm::Attribute::ZExt);
                }
            },
            [&](X64ABIImpl::OnStack) {
                cld::ScopeExit exit{[&] { i++; }};
                auto& arg = *functionType.getParameters()[origArgI].type;
                attributesIn = attributesIn.addParamAttribute(
                    context, i,
                    llvm::Attribute::getWithByValType(context,
                                                      llvmFunctionType->getParamType(i)->getPointerElementType()));
                auto minAlignment = m_dataLayout.getPointerABIAlignment(0);
                attributesIn = attributesIn.addParamAttribute(
                    context, i,
                    llvm::Attribute::getWithAlignment(
                        context,
                        llvm::Align(std::max<std::size_t>(minAlignment.value(), arg.getAlignOf(programInterface)))));
            },
            [&](X64ABIImpl::MultipleArgs multipleArgs) { i += multipleArgs.size; });
    }
    return attributesIn;
}

void cld::CGLLVM::X64ABI::generateFunctionEntry(
    CodeGenerator& codeGenerator, const llvm::Function* llvmFunction, const Semantics::FunctionType& functionType,
    const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>& paramDecls)
{
    auto& adjustment = getAdjustment(functionType);
    m_currentFunctionABI = &adjustment;
    std::size_t argStart = 0;
    if (std::holds_alternative<X64ABIImpl::PointerToTemporary>(adjustment.returnType))
    {
        argStart = 1;
    }
    else if (std::holds_alternative<X64ABIImpl::Flattened>(adjustment.returnType))
    {
        m_returnSlot = codeGenerator.createAllocaAtTop(llvmFunction->getReturnType());
        m_returnSlot->setAlignment(
            llvm::Align(functionType.getReturnType().getAlignOf(codeGenerator.getProgramInterface())));
    }

    std::size_t origArgI = 0;
    auto* llvmFunctionType = llvmFunction->getFunctionType();
    for (std::size_t i = argStart; i < llvmFunctionType->getNumParams(); origArgI++)
    {
        auto& paramDecl = paramDecls[origArgI];
        auto* operand = llvmFunction->getArg(i);
        cld::match(
            adjustment.arguments[origArgI],
            [&](X64ABIImpl::Unchanged) {
                cld::ScopeExit exit{[&] { i++; }};
                auto* var = codeGenerator.createAllocaAtTop(operand->getType(), paramDecl->getNameToken()->getText());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(codeGenerator.getProgramInterface())));
                codeGenerator.addLValue(*paramDecl, var);
                codeGenerator.createStore(operand, var, paramDecl->getType().isVolatile());
            },
            [&](X64ABIImpl::OnStack) {
                cld::ScopeExit exit{[&] { i++; }};
                codeGenerator.addLValue(*paramDecl, Value(operand, operand->getPointerAlignment(m_dataLayout)));
            },
            [&](X64ABIImpl::MultipleArgs multipleArgs) {
                cld::ScopeExit exit{[&] { i += multipleArgs.size; }};
                auto* var = codeGenerator.createAllocaAtTop(codeGenerator.visit(paramDecl->getType()),
                                                            paramDecl->getNameToken()->getText());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(codeGenerator.getProgramInterface())));
                codeGenerator.addLValue(*paramDecl, var);
                std::vector<llvm::Type*> elements;
                elements.push_back(llvmFunctionType->getParamType(i));
                if (multipleArgs.size == 2)
                {
                    elements.push_back(llvmFunctionType->getParamType(i + 1));
                }
                auto structType = codeGenerator.createBitCast(
                    var, llvm::StructType::get(llvmFunctionType->getContext(), elements)->getPointerTo(0), false);
                auto firstElement = codeGenerator.createInBoundsGEP(
                    structType, {codeGenerator.getBuilder().getInt32(0), codeGenerator.getBuilder().getInt32(0)});
                codeGenerator.createStore(llvmFunction->getArg(i), firstElement, paramDecl->getType().isVolatile());
                if (multipleArgs.size == 2)
                {
                    auto secondElement = codeGenerator.createInBoundsGEP(
                        structType, {codeGenerator.getBuilder().getInt32(0), codeGenerator.getBuilder().getInt32(1)});
                    codeGenerator.createStore(llvmFunction->getArg(i + 1), secondElement,
                                              paramDecl->getType().isVolatile());
                }
            });
    }
}

llvm::Value* cld::CGLLVM::X64ABI::generateValueReturn(CodeGenerator& codeGenerator, Value value)
{
    return cld::match(
        m_currentFunctionABI->returnType, [=](X64ABIImpl::Unchanged) -> llvm::Value* { return value; },
        [&](X64ABIImpl::PointerToTemporary) -> llvm::Value* {
            codeGenerator.createStore(value.value,
                                      codeGenerator.valueOf(codeGenerator.getCurrentFunction()->getArg(0),
                                                            codeGenerator.getCurrentFunction()->getParamAlign(0)),
                                      false);
            return nullptr;
        },
        [&](X64ABIImpl::Flattened) -> llvm::Value* {
            codeGenerator.createStore(
                value, codeGenerator.createBitCast(m_returnSlot, value.value->getType()->getPointerTo(0)), false);
            return codeGenerator.createLoad(m_returnSlot, false);
        });
}

namespace
{
cld::CGLLVM::Value loadFromStack(const llvm::DataLayout& dataLayout, cld::CGLLVM::CodeGenerator& codeGenerator,
                                 cld::CGLLVM::Value vaList, llvm::Type* destType)
{
    auto& builder = codeGenerator.getBuilder();
    auto overflowArea = codeGenerator.createInBoundsGEP(vaList, {builder.getInt64(0), builder.getInt32(2)});
    auto loadedStackPointer = codeGenerator.createLoad(overflowArea, false);
    loadedStackPointer.alignment = llvm::Align(8);
    if (dataLayout.getABITypeAlign(destType) >= 16)
    {
        // Align to 16 bytes
        llvm::IntegerType* intPtrType = dataLayout.getIntPtrType(builder.getContext(), 0);
        auto* temp = builder.CreatePtrToInt(loadedStackPointer, intPtrType);
        temp = builder.CreateAdd(temp, llvm::ConstantInt::get(intPtrType, 15));
        auto mask = llvm::APInt::getAllOnesValue(intPtrType->getBitWidth());
        mask.clearLowBits(4);
        temp = builder.CreateAnd(temp, llvm::ConstantInt::get(intPtrType, mask));
        loadedStackPointer =
            codeGenerator.valueOf(builder.CreateIntToPtr(temp, loadedStackPointer.value->getType()), llvm::Align(16));
    }

    auto incremented = codeGenerator.createGEP(
        loadedStackPointer,
        builder.getInt64(cld::roundUpTo(dataLayout.getTypeAllocSize(destType).getKnownMinSize(), 8)));
    loadedStackPointer = codeGenerator.createBitCast(loadedStackPointer, llvm::PointerType::getUnqual(destType));
    codeGenerator.createStore(incremented, overflowArea, false);
    return loadedStackPointer;
}
} // namespace

cld::CGLLVM::Value cld::CGLLVM::X64ABI::generateVAArg(CodeGenerator& codeGenerator, Value vaList,
                                                      const Semantics::Type& type)
{
    auto* destType = codeGenerator.visit(type);
    std::size_t sizeOf = type.getSizeOf(codeGenerator.getProgramInterface());
    llvm::IRBuilder<>& builder = codeGenerator.getBuilder();
    if (m_dataLayout.getTypeAllocSizeInBits(destType) > 128)
    {
    OnStack:
        auto loadedStackPointer = loadFromStack(m_dataLayout, codeGenerator, vaList, destType);

        auto* allocaInst = codeGenerator.createAllocaAtTop(destType, "va_arg.ret");
        allocaInst->setAlignment(llvm::Align(type.getAlignOf(codeGenerator.getProgramInterface())));
        builder.CreateLifetimeStart(allocaInst, builder.getInt64(sizeOf));
        builder.CreateMemCpy(allocaInst, allocaInst->getAlign(), loadedStackPointer, *loadedStackPointer.alignment,
                             sizeOf);
        return codeGenerator.createLoad(allocaInst, false);
    }
    auto [transform, first, second] = flattenSingleArg(m_dataLayout, destType);
    if (std::holds_alternative<X64ABIImpl::OnStack>(transform))
    {
        // I have yet to decide what is the lesser evil. Not repeating myself, or using goto...
        goto OnStack;
    }

    Value gpOffset = nullptr;
    llvm::Value* gpCount = nullptr;
    Value fpOffset = nullptr;
    llvm::Value* fpCount = nullptr;

    llvm::Value* cond = nullptr;

    if (first->isIntOrPtrTy() || (second && second->isIntOrPtrTy()))
    {
        gpOffset = codeGenerator.createInBoundsGEP(vaList, {builder.getInt64(0), builder.getInt32(0)});
        gpCount = codeGenerator.createLoad(gpOffset, false);
        // if the offset is 48 bytes it is full (8 Bytes * 6 Integer registers). To fit two integers we
        // need an offset of 32, to fit one we need max 40
        auto maxOffset = first->isIntOrPtrTy() && second && second->isIntOrPtrTy() ? 32 : 40;
        cond = builder.CreateICmpULE(gpCount, builder.getInt32(maxOffset));
    }
    if (first->isFPOrFPVectorTy() || (second && second->isFPOrFPVectorTy()))
    {
        fpOffset = codeGenerator.createInBoundsGEP(vaList, {builder.getInt64(0), builder.getInt32(1)});
        fpCount = codeGenerator.createLoad(fpOffset, false);
        // fpOffset comes after gpOffset therefore all fp registers are used when fpOffset
        // is 8 Bytes * 6 Integer registers + 16 Bytes * 8 Floating point registers
        auto maxOffset = (first->isFPOrFPVectorTy() && second && second->isFPOrFPVectorTy()) ? 144 : 160;
        if (!cond)
        {
            cond = builder.CreateICmpULE(fpCount, builder.getInt32(maxOffset));
        }
        else
        {
            cond = builder.CreateAnd(cond, builder.CreateICmpULE(fpCount, builder.getInt32(maxOffset)));
        }
    }
    CLD_ASSERT(cond);

    auto* function = builder.GetInsertBlock()->getParent();
    auto* inRegister = llvm::BasicBlock::Create(builder.getContext(), "va_arg.inRegister", function);
    auto* onStack = llvm::BasicBlock::Create(builder.getContext(), "va_arg.onStack", function);
    auto* contBlock = llvm::BasicBlock::Create(builder.getContext(), "va_arg.continue", function);
    builder.CreateCondBr(cond, inRegister, onStack);

    builder.SetInsertPoint(inRegister);
    auto regArea = codeGenerator.createInBoundsGEP(vaList, {builder.getInt64(0), builder.getInt32(3)});
    regArea = codeGenerator.createLoad(regArea, false);
    Value regValue = nullptr;
    if (!second)
    {
        if (first->isIntOrPtrTy())
        {
            // TODO: Handle case where types is aligned higher than 8 bytes;
            regArea = codeGenerator.createGEP(regArea, gpCount);
            regArea.alignment = llvm::Align(8);
            regValue = codeGenerator.createBitCast(regArea, llvm::PointerType::getUnqual(destType));
            auto* incremented = builder.CreateAdd(gpCount, builder.getInt32(8));
            codeGenerator.createStore(incremented, gpOffset, false);
            builder.CreateBr(contBlock);
        }
        else
        {
            // TODO: Handle case where types is aligned higher than 16 bytes
            regArea = codeGenerator.createGEP(regArea, fpCount);
            regArea.alignment = llvm::Align(16);
            regValue = codeGenerator.createBitCast(regArea, llvm::PointerType::getUnqual(destType));

            auto* incremented = builder.CreateAdd(fpCount, builder.getInt32(16));
            codeGenerator.createStore(incremented, fpOffset, false);
            builder.CreateBr(contBlock);
        }
    }
    else
    {
        auto* allocaInst = codeGenerator.createAllocaAtTop(destType, "va_arg.temp");
        allocaInst->setAlignment(llvm::Align(type.getAlignOf(codeGenerator.getProgramInterface())));
        builder.CreateLifetimeStart(allocaInst, builder.getInt64(sizeOf));
        auto dest = codeGenerator.createBitCast(
            allocaInst, llvm::PointerType::getUnqual(llvm::StructType::get(first, second)), false);

        auto regAreaOfFirst = codeGenerator.createGEP(regArea, first->isFPOrFPVectorTy() ? fpCount : gpCount);
        regAreaOfFirst.alignment = llvm::Align(first->isFPOrFPVectorTy() ? 16 : 8);
        regAreaOfFirst = codeGenerator.createBitCast(regAreaOfFirst, llvm::PointerType::getUnqual(first));

        regAreaOfFirst = codeGenerator.createLoad(regAreaOfFirst, false);
        auto destFirst = codeGenerator.createInBoundsGEP(dest, {builder.getInt64(0), builder.getInt32(0)});
        codeGenerator.createStore(regAreaOfFirst, destFirst, false);

        llvm::Value* offsetOfSecond = second->isFPOrFPVectorTy() ? fpCount : gpCount;
        // If both are initialized then we are loading from one floating point and one integer register.
        // Otherwise we are loading from two registers. In that case we need to apply an extra offset
        // as we have already loaded previously
        if (static_cast<bool>(fpOffset) != static_cast<bool>(gpOffset))
        {
            offsetOfSecond =
                builder.CreateAdd(offsetOfSecond, llvm::ConstantInt::get(offsetOfSecond->getType(), fpOffset ? 16 : 8));
        }

        auto regAreaOfSecond = codeGenerator.createGEP(regArea, offsetOfSecond);
        regAreaOfSecond.alignment = llvm::Align(second->isFPOrFPVectorTy() ? 16 : 8);
        regAreaOfSecond = codeGenerator.createBitCast(regAreaOfSecond, llvm::PointerType::getUnqual(second));

        regAreaOfSecond = codeGenerator.createLoad(regAreaOfSecond, false);
        auto destSecond = codeGenerator.createInBoundsGEP(dest, {builder.getInt64(0), builder.getInt32(1)});
        codeGenerator.createStore(regAreaOfSecond, destSecond, false);

        if (static_cast<bool>(fpOffset) != static_cast<bool>(gpOffset))
        {
            if (fpOffset)
            {
                fpCount = builder.CreateAdd(fpCount, llvm::ConstantInt::get(fpCount->getType(), 32));
                codeGenerator.createStore(fpCount, fpOffset, false);
            }
            else
            {
                gpCount = builder.CreateAdd(gpCount, llvm::ConstantInt::get(gpCount->getType(), 16));
                codeGenerator.createStore(gpCount, gpOffset, false);
            }
        }
        else
        {
            fpCount = builder.CreateAdd(fpCount, llvm::ConstantInt::get(fpCount->getType(), 16));
            codeGenerator.createStore(fpCount, fpOffset, false);
            gpCount = builder.CreateAdd(gpCount, llvm::ConstantInt::get(gpCount->getType(), 8));
            codeGenerator.createStore(gpCount, gpOffset, false);
        }
        regValue = allocaInst;
        builder.CreateBr(contBlock);
    }

    builder.SetInsertPoint(onStack);
    auto stackValue = loadFromStack(m_dataLayout, codeGenerator, vaList, destType);
    builder.CreateBr(contBlock);

    builder.SetInsertPoint(contBlock);
    auto* phi = builder.CreatePHI(llvm::PointerType::getUnqual(destType), 2);
    phi->addIncoming(regValue, inRegister);
    phi->addIncoming(stackValue, onStack);

    if (!Semantics::isRecord(type))
    {
        return codeGenerator.createLoad(
            codeGenerator.valueOf(phi, std::min(*regValue.alignment, *stackValue.alignment)), false);
    }

    auto* allocaInst = codeGenerator.createAllocaAtTop(destType, "va_arg.ret");
    allocaInst->setAlignment(llvm::Align(type.getAlignOf(codeGenerator.getProgramInterface())));
    builder.CreateLifetimeStart(allocaInst, builder.getInt64(sizeOf));
    builder.CreateMemCpy(allocaInst, allocaInst->getAlign(), phi, std::min(*regValue.alignment, *stackValue.alignment),
                         sizeOf);
    return codeGenerator.createLoad(allocaInst, false);
}

cld::CGLLVM::Value cld::CGLLVM::X64ABI::generateFunctionCall(CodeGenerator& codeGenerator, Value callee,
                                                             llvm::FunctionType* llvmFunctionType,
                                                             const Semantics::FunctionType& functionType,
                                                             std::vector<llvm::Value*>&& arguments)
{
    auto& adjustments = getAdjustment(functionType);
    llvm::AllocaInst* returnSlot = nullptr;
    if (std::holds_alternative<X64ABIImpl::PointerToTemporary>(adjustments.returnType))
    {
        returnSlot = codeGenerator.createAllocaAtTop(llvmFunctionType->getParamType(0)->getPointerElementType(), "ret");
        returnSlot->setAlignment(
            llvm::Align(functionType.getReturnType().getAlignOf(codeGenerator.getProgramInterface())));
        arguments.insert(arguments.begin(), returnSlot);
    }
    std::size_t i = 0;
    for (auto iter = arguments.begin() + (returnSlot ? 1 : 0); iter != arguments.end(); iter++, i++)
    {
        std::size_t llvmFnI = iter - arguments.begin();
        cld::match(
            adjustments.arguments[i], [&](X64ABIImpl::Unchanged) {},
            [&](X64ABIImpl::OnStack) {
                // structs rvalues don't exist in LLVM IR so this should be sound?
                auto* load = llvm::cast<llvm::LoadInst>(*iter);
                *iter = load->getPointerOperand();
                load->eraseFromParent();
            },
            [&](X64ABIImpl::MultipleArgs multipleArgs) {
                auto* load = llvm::cast<llvm::LoadInst>(*iter);
                if (multipleArgs.size == 1)
                {
                    auto* paramType = llvmFunctionType->getParamType(llvmFnI);
                    auto cast = codeGenerator.createSafeBitCast(
                        codeGenerator.valueOf(load->getPointerOperand(), load->getAlign()), paramType->getPointerTo(0));
                    *iter = codeGenerator.createLoad(cast, load->isVolatile());
                }
                else
                {
                    auto* firstType = llvmFunctionType->getParamType(llvmFnI);
                    auto* secondType = llvmFunctionType->getParamType(llvmFnI + 1);
                    auto cast = codeGenerator.createSafeBitCast(
                        codeGenerator.valueOf(load->getPointerOperand(), load->getAlign()),
                        llvm::StructType::get(firstType, secondType)->getPointerTo(0));
                    auto firstValue = codeGenerator.createInBoundsGEP(
                        cast, {codeGenerator.getBuilder().getInt64(0), codeGenerator.getBuilder().getInt32(0)});
                    auto secondValue = codeGenerator.createInBoundsGEP(
                        cast, {codeGenerator.getBuilder().getInt64(0), codeGenerator.getBuilder().getInt32(1)});
                    *iter = codeGenerator.createLoad(firstValue, load->isVolatile());
                    iter++;
                    iter = arguments.insert(iter, codeGenerator.createLoad(secondValue, load->isVolatile()));
                }
                load->eraseFromParent();
            });
    }
    auto* calleeFunctionType = llvm::cast<llvm::FunctionType>(callee.value->getType()->getPointerElementType());
    auto* result = codeGenerator.getBuilder().CreateCall(calleeFunctionType, callee.value, arguments);
    auto attributes = result->getAttributes();
    attributes = generateFunctionAttributes(std::move(attributes), llvmFunctionType, functionType,
                                            codeGenerator.getProgramInterface());
    result->setAttributes(std::move(attributes));
    return cld::match(
        adjustments.returnType, [&](X64ABIImpl::Unchanged) { return codeGenerator.valueOf(result); },
        [&](X64ABIImpl::PointerToTemporary) {
            CLD_ASSERT(returnSlot);
            return codeGenerator.createLoad(returnSlot, false);
        },
        [&](X64ABIImpl::Flattened) {
            auto* flattened = codeGenerator.createAllocaAtTop(result->getType());
            flattened->setAlignment(llvm::Align(m_dataLayout.getABITypeAlign(result->getType())));
            codeGenerator.createStore(result, flattened, false);

            auto cast = codeGenerator.createSafeBitCast(
                flattened, codeGenerator.visit(functionType.getReturnType())->getPointerTo(0));
            return codeGenerator.createLoad(cast, false);
        });
}

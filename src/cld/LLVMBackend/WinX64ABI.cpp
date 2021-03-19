
#include "WinX64ABI.hpp"

#include "CodeGenerator.hpp"

cld::CGLLVM::WinX64ABI::WinX64ABI(const llvm::DataLayout& dataLayout) : ABIImplementation(dataLayout) {}

void cld::CGLLVM::WinX64ABI::applyPlatformABI(const Semantics::FunctionType& functionType, llvm::Type*& returnType,
                                              std::vector<llvm::Type*>& arguments)
{
    auto result = m_adjustments.find(functionType);
    if (result != m_adjustments.end())
    {
        switch (result->second.returnType)
        {
            case IntegerRegister:
                returnType =
                    llvm::IntegerType::get(returnType->getContext(), m_dataLayout.getTypeAllocSizeInBits(returnType));
                break;
            case PointerToTemporary: returnType = returnType->getPointerTo(0); break;
            default: break;
        }
        for (auto& iter : arguments)
        {
            switch (result->second.returnType)
            {
                case IntegerRegister:
                    iter = llvm::IntegerType::get(iter->getContext(), m_dataLayout.getTypeAllocSizeInBits(iter));
                    break;
                case PointerToTemporary: iter = iter->getPointerTo(0); break;
                default: break;
            }
        }
        return;
    }
    Adjustments adjustments;
    adjustments.arguments.resize(arguments.size(), Nothing);
    for (auto iter = arguments.begin(); iter != arguments.end(); iter++)
    {
        if (!(*iter)->isStructTy() && !(*iter)->isX86_FP80Ty())
        {
            continue;
        }
        std::uint32_t size = m_dataLayout.getTypeAllocSizeInBits(*iter);
        if (m_dataLayout.isLegalInteger(size))
        {
            *iter = llvm::IntegerType::get((*iter)->getContext(), size);
            adjustments.arguments[iter - arguments.begin()] = IntegerRegister;
        }
        else
        {
            *iter = (*iter)->getPointerTo(0);
            adjustments.arguments[iter - arguments.begin()] = PointerToTemporary;
        }
    }
    if (!returnType->isVoidTy())
    {
        std::uint32_t size = m_dataLayout.getTypeAllocSizeInBits(returnType);
        if (m_dataLayout.isLegalInteger(size) && returnType->isStructTy())
        {
            returnType = llvm::IntegerType::get(returnType->getContext(), size);
            adjustments.returnType = IntegerRegister;
        }
        else if (!m_dataLayout.isLegalInteger(size))
        {
            arguments.insert(arguments.begin(), returnType->getPointerTo(0));
            returnType = llvm::Type::getVoidTy(returnType->getContext());
            adjustments.returnType = PointerToTemporary;
        }
    }
    m_adjustments[functionType] = adjustments;
}

llvm::AttributeList cld::CGLLVM::WinX64ABI::generateFunctionAttributes(llvm::AttributeList attributesIn,
                                                                       const llvm::FunctionType* llvmFunctionType,
                                                                       const Semantics::FunctionType& functionType)
{
    auto adjustments = m_adjustments.find(functionType);
    CLD_ASSERT(adjustments != m_adjustments.end());
    if (adjustments->second.returnType == PointerToTemporary)
    {
        attributesIn = attributesIn.addAttribute(llvmFunctionType->getContext(), 1, llvm::Attribute::StructRet);
        attributesIn = attributesIn.addAttribute(llvmFunctionType->getContext(), 1, llvm::Attribute::NoAlias);
    }
    return attributesIn;
}

void cld::CGLLVM::WinX64ABI::generateFunctionEntry(
    CodeGenerator& codeGenerator, const llvm::Function* llvmFunction, const Semantics::FunctionType& functionType,
    const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>& paramDecls)
{
    auto adjustments = m_adjustments.find(functionType);
    CLD_ASSERT(adjustments != m_adjustments.end());
    m_currentFunctionABI = &adjustments->second;
    std::size_t argStart = 0;
    if (m_currentFunctionABI->returnType == PointerToTemporary)
    {
        argStart = 1;
    }
    else if (m_currentFunctionABI->returnType == IntegerRegister)
    {
        m_returnSlot = codeGenerator.createAllocaAtTop(llvmFunction->getReturnType());
        m_returnSlot->setAlignment(
            llvm::Align(functionType.getReturnType().getAlignOf(codeGenerator.getProgramInterface())));
    }

    for (std::size_t i = argStart; i < llvmFunction->arg_size(); i++)
    {
        auto& paramDecl = paramDecls[i - argStart];
        auto* operand = llvmFunction->getArg(i);
        switch (m_currentFunctionABI->arguments[i - argStart])
        {
            case Nothing:
            {
                auto* var = codeGenerator.createAllocaAtTop(operand->getType(), paramDecl->getNameToken()->getText());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(codeGenerator.getProgramInterface())));
                codeGenerator.addLValue(*paramDecl, var);
                codeGenerator.createStore(operand, var, paramDecl->getType().isVolatile());
                break;
            }
            case IntegerRegister:
            {
                auto* var = codeGenerator.createAllocaAtTop(codeGenerator.visit(paramDecl->getType()),
                                                            paramDecl->getNameToken()->getText());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(codeGenerator.getProgramInterface())));
                codeGenerator.addLValue(*paramDecl, var);
                auto cast = codeGenerator.createBitCast(var, operand->getType()->getPointerTo(0), false);
                codeGenerator.createStore(operand, cast, paramDecl->getType().isVolatile());
                break;
            }
            case PointerToTemporary:
                codeGenerator.addLValue(*paramDecl, Value(operand, operand->getPointerAlignment(m_dataLayout)));
                break;
        }
    }
}

llvm::Value* cld::CGLLVM::WinX64ABI::generateValueReturn(CodeGenerator& codeGenerator, Value value)
{
    if (m_currentFunctionABI->returnType == PointerToTemporary)
    {
        codeGenerator.createStore(value.value,
                                  codeGenerator.valueOf(codeGenerator.getCurrentFunction()->getArg(0),
                                                        codeGenerator.getCurrentFunction()->getParamAlign(0)),
                                  false);
        return nullptr;
    }
    if (m_currentFunctionABI->returnType == IntegerRegister)
    {
        codeGenerator.createStore(
            value, codeGenerator.createBitCast(m_returnSlot, value.value->getType()->getPointerTo(0)), false);
        return codeGenerator.createLoad(m_returnSlot, false);
    }
    return value;
}

cld::CGLLVM::Value cld::CGLLVM::WinX64ABI::generateVAArg(CodeGenerator& codeGenerator, Value vaList,
                                                         const Semantics::Type& type)
{
    auto increment =
        codeGenerator.createInBoundsGEP(vaList, {codeGenerator.getBuilder().getInt64(m_dataLayout.getPointerSize(0))});
    vaList.alignment = llvm::Align(8);
    codeGenerator.createStore(increment,
                              Value(llvm::cast<llvm::LoadInst>(vaList.value)->getPointerOperand(),
                                    llvm::cast<llvm::LoadInst>(vaList.value)->getAlign()),
                              false);
    auto* destType = codeGenerator.visit(type);

    std::size_t sizeOf = type.getSizeOf(codeGenerator.getProgramInterface());
    auto exprAlign = type.getAlignOf(codeGenerator.getProgramInterface());
    if (!destType->isStructTy() && !destType->isX86_FP80Ty())
    {
        vaList = codeGenerator.createBitCast(vaList, llvm::PointerType::getUnqual(destType));
        return codeGenerator.createLoad(vaList, false);
    }
    if (!m_dataLayout.isLegalInteger(sizeOf * 8))
    {
        vaList = codeGenerator.createBitCast(vaList, destType->getPointerTo(0)->getPointerTo(0));
        vaList = codeGenerator.createLoad(vaList, false);
    }

    auto* allocaInst = codeGenerator.createAllocaAtTop(destType, "va_arg.ret");
    allocaInst->setAlignment(llvm::Align(exprAlign));
    codeGenerator.getBuilder().CreateLifetimeStart(allocaInst, codeGenerator.getBuilder().getInt64(sizeOf));
    codeGenerator.getBuilder().CreateMemCpy(allocaInst, allocaInst->getAlign(), vaList, *vaList.alignment, sizeOf);
    return codeGenerator.createLoad(allocaInst, false);
}

cld::CGLLVM::Value cld::CGLLVM::WinX64ABI::generateFunctionCall(CodeGenerator& codeGenerator, Value callee,
                                                                llvm::FunctionType* llvmFunctionType,
                                                                const Semantics::FunctionType& functionType,
                                                                std::vector<llvm::Value*>&& arguments)
{
    auto adjustments = m_adjustments.find(functionType);
    CLD_ASSERT(adjustments != m_adjustments.end());
    llvm::AllocaInst* returnSlot = nullptr;
    std::size_t llvmFnI = 0;
    if (adjustments->second.returnType == PointerToTemporary)
    {
        llvmFnI = 1;
        returnSlot = codeGenerator.createAllocaAtTop(llvmFunctionType->getParamType(0)->getPointerElementType(), "ret");
        returnSlot->setAlignment(
            llvm::Align(functionType.getReturnType().getAlignOf(codeGenerator.getProgramInterface())));
    }
    for (std::size_t i = 0; i < arguments.size(); i++)
    {
        switch (adjustments->second.arguments[i])
        {
            case Nothing: break;
            case IntegerRegister:
            {
                auto* load = llvm::cast<llvm::LoadInst>(arguments[i]);
                auto integer =
                    codeGenerator.createBitCast(codeGenerator.valueOf(load->getPointerOperand(), load->getAlign()),
                                                llvmFunctionType->getParamType(llvmFnI + i)->getPointerTo(0), false);
                arguments[i] = codeGenerator.createLoad(integer, load->isVolatile());
                load->eraseFromParent();
                break;
            }
            case PointerToTemporary:
            {
                auto* ret = codeGenerator.createAllocaAtTop(arguments[i]->getType());
                ret->setAlignment(
                    llvm::Align(functionType.getArguments()[i].first.getAlignOf(codeGenerator.getProgramInterface())));
                if (arguments[i]->getType()->isX86_FP80Ty())
                {
                    codeGenerator.createStore(arguments[i], ret, false);
                }
                else
                {
                    auto* load = llvm::cast<llvm::LoadInst>(arguments[i]);
                    codeGenerator.getBuilder().CreateMemCpy(
                        ret, ret->getAlign(), load->getPointerOperand(), load->getAlign(),
                        functionType.getArguments()[i].first.getSizeOf(codeGenerator.getProgramInterface()));
                    load->eraseFromParent();
                }
                arguments[i] = ret;
                break;
            }
        }
    }
    if (returnSlot)
    {
        arguments.insert(arguments.begin(), returnSlot);
    }
    auto* calleeFunctionType = llvm::cast<llvm::FunctionType>(callee.value->getType()->getPointerElementType());
    auto* result = codeGenerator.getBuilder().CreateCall(calleeFunctionType, callee.value, arguments);
    auto attributes = result->getAttributes();
    attributes = generateFunctionAttributes(std::move(attributes), calleeFunctionType, functionType);
    result->setAttributes(std::move(attributes));
    switch (adjustments->second.returnType)
    {
        case Nothing: return codeGenerator.valueOf(result);
        case PointerToTemporary:
        {
            CLD_ASSERT(returnSlot);
            return codeGenerator.createLoad(returnSlot, false);
        }
        case IntegerRegister:
        {
            auto* intValue = codeGenerator.createAllocaAtTop(result->getType());
            intValue->setAlignment(llvm::Align(m_dataLayout.getABITypeAlign(result->getType())));
            codeGenerator.createStore(result, intValue, false);

            auto cast = codeGenerator.createSafeBitCast(
                intValue, codeGenerator.visit(functionType.getReturnType())->getPointerTo(0));
            return codeGenerator.createLoad(cast, false);
        }
    }
}

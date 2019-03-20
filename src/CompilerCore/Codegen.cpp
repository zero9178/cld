#include "Parser.hpp"

#include <sstream>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/TargetRegistry.h>

namespace
{
    void castPrimitive(llvm::Value*& value,
                       bool isSigned,
                       llvm::Type* destType,
                       bool destIsSigned,
                       OpenCL::Parser::Context& context)
    {
        if (value->getType() != destType)
        {
            if (value->getType()->isIntegerTy())
            {
                if (destType->isIntegerTy())
                {
                    value = context.builder.CreateIntCast(value, destType, isSigned);
                }
                else if (destType->isFloatingPointTy())
                {
                    if (isSigned)
                    {
                        value = context.builder.CreateSIToFP(value, destType);
                    }
                    else
                    {
                        value = context.builder.CreateUIToFP(value, destType);
                    }
                }
                else if (destType->isPointerTy())
                {
                    if (value->getType()->getIntegerBitWidth() != 64)
                    {
                        value = context.builder.CreateIntCast(value, context.builder.getInt64Ty(), isSigned);
                    }
                    value = context.builder.CreateIntToPtr(value, destType);
                }
                else
                {
                    throw std::runtime_error("Cannot convert type");
                }
            }
            else if (value->getType()->isFloatingPointTy())
            {
                if (destType->isFloatingPointTy())
                {
                    value = context.builder.CreateFPCast(value, destType);
                }
                else if (destType->isIntegerTy())
                {
                    if (destIsSigned)
                    {
                        value = context.builder.CreateFPToSI(value, destType);
                    }
                    else
                    {
                        value = context.builder.CreateFPToUI(value, destType);
                    }
                }
                else
                {
                    throw std::runtime_error("Cannot convert type");
                }
            }
            else if (value->getType()->isPointerTy())
            {
                if (destType->isIntegerTy())
                {
                    value = context.builder.CreatePtrToInt(value, destType);
                }
                else
                {
                    throw std::runtime_error("Cannot convert pointer type to other");
                }
            }
            else if (value->getType()->isArrayTy())
            {
                if (llvm::isa<llvm::LoadInst>(value) && destType->isIntOrPtrTy())
                {
                    auto* zero = context.builder.getInt32(0);
                    value = context.builder.CreateInBoundsGEP(llvm::cast<llvm::LoadInst>(value)->getPointerOperand(),
                                                              {zero, zero});
                    if (destType->isIntegerTy())
                    {
                        value = context.builder.CreatePtrToInt(value, destType);
                    }
                }
                else
                {
                    throw std::runtime_error("Cannot convert array type to other");
                }
            }
        }
        isSigned = destIsSigned;
    }

    void castToDouble(llvm::Value*& value, bool isSigned, OpenCL::Parser::Context& context)
    {
        if (!value->getType()->isDoubleTy())
        {
            if (value->getType()->isIntegerTy())
            {
                if (isSigned)
                {
                    value = context.builder.CreateSIToFP(value, context.builder.getDoubleTy());
                }
                else
                {
                    value = context.builder.CreateUIToFP(value, context.builder.getDoubleTy());
                }
            }
            else if (value->getType()->isFloatingPointTy())
            {
                value = context.builder.CreateFPCast(value, context.builder.getDoubleTy());
            }
            else
            {
                throw std::runtime_error("Can't cast to double");
            }
        }
    }

    void castToFloat(llvm::Value*& value, bool isSigned, OpenCL::Parser::Context& context)
    {
        if (!value->getType()->isFloatTy())
        {
            if (value->getType()->isIntegerTy())
            {
                if (isSigned)
                {
                    value = context.builder.CreateSIToFP(value, context.builder.getFloatTy());
                }
                else
                {
                    value = context.builder.CreateUIToFP(value, context.builder.getFloatTy());
                }
            }
            else if (value->getType()->isFloatingPointTy())
            {
                value = context.builder.CreateFPCast(value, context.builder.getFloatTy());
            }
            else
            {
                throw std::runtime_error("Can't cast to double");
            }
        }
    }

    void arithmeticCast(llvm::Value*& lhs,
                        bool lhsIsSigned,
                        llvm::Value*& rhs,
                        bool rhsIsSigned,
                        OpenCL::Parser::Context& context)
    {
        if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy())
        {
            castToDouble(lhs, lhsIsSigned, context);
            castToDouble(rhs, rhsIsSigned, context);
        }
        else if (lhs->getType()->isFloatTy() || rhs->getType()->isFloatTy())
        {
            castToFloat(lhs, lhsIsSigned, context);
            castToFloat(rhs, rhsIsSigned, context);
        }
        else if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy())
        {
            if (lhs->getType()->getIntegerBitWidth() < 32u
                || lhs->getType()->getIntegerBitWidth() < rhs->getType()->getIntegerBitWidth())
            {
                lhs = context.builder.CreateIntCast(lhs,
                                                    context.builder.getIntNTy(std::max(32u,
                                                                                       rhs->getType()
                                                                                          ->getIntegerBitWidth())),
                                                    lhsIsSigned);
            }
            if (rhs->getType()->getIntegerBitWidth() < 32u
                || rhs->getType()->getIntegerBitWidth() < lhs->getType()->getIntegerBitWidth())
            {
                rhs = context.builder.CreateIntCast(rhs,
                                                    context.builder.getIntNTy(std::max(32u,
                                                                                       lhs->getType()
                                                                                          ->getIntegerBitWidth())),
                                                    rhsIsSigned);
            }
        }
        else if (!rhs->getType()->isPointerTy() || !lhs->getType()->isPointerTy())
        {
            throw std::runtime_error("Can't cast to common type");
        }
    }

    std::size_t getAlignment(llvm::Type* type)
    {
        if (type->isPointerTy())
        {
            return 8;
        }
        else if (type->isIntegerTy())
        {
            if (type->getIntegerBitWidth() <= 64)
            {
                return type->getIntegerBitWidth() / 8;
            }
            else
            {
                return 16;
            }
        }
        else if (type->isFloatingPointTy())
        {
            if (type->isFloatTy())
            {
                return 4;
            }
            else if (type->isDoubleTy())
            {
                return 8;
            }
            else
            {
                throw std::runtime_error("Not implemented yet");
            }
        }
        else if (type->isStructTy())
        {
            auto* structType = llvm::cast<llvm::StructType>(type);
            std::size_t alignment = 0;
            for (std::size_t i = 0; i < structType->getStructNumElements(); i++)
            {
                alignment = std::max(alignment, getAlignment(structType->getStructElementType(i)));
            }
            return alignment;
        }
        else if (type->isArrayTy())
        {
            return getAlignment(type->getArrayElementType());
        }
        else
        {
            throw std::runtime_error("Not implemented yet");
        }
    }

    std::unordered_map<void*, llvm::DIType*> cache;

    llvm::DIType* toDwarfType(std::shared_ptr<OpenCL::Parser::Type> type, OpenCL::Parser::Context& context)
    {
        auto* llvmType = type->type(context);
        if (auto result = cache.find(llvmType); result != cache.end())
        {
            return result->second;
        }
        if (llvmType->isIntegerTy() || llvmType->isFloatingPointTy())
        {
            auto primitive = std::dynamic_pointer_cast<OpenCL::Parser::PrimitiveType>(type);
            auto result = context.debugBuilder
                                 ->createBasicType(type->name(), primitive->getBitCount(), [llvmType, &type]
                                 {
                                     if (llvmType->isIntegerTy())
                                     {
                                         if (llvmType->getIntegerBitWidth() == 8)
                                         {
                                             if (type->isSigned())
                                             {
                                                 return llvm::dwarf::DW_ATE_signed_char;
                                             }
                                             else
                                             {
                                                 return llvm::dwarf::DW_ATE_unsigned_char;
                                             }
                                         }
                                         else
                                         {
                                             if (type->isSigned())
                                             {
                                                 return llvm::dwarf::DW_ATE_signed;
                                             }
                                             else
                                             {
                                                 return llvm::dwarf::DW_ATE_unsigned;
                                             }
                                         }
                                     }
                                     else if (llvmType->isFloatingPointTy())
                                     {
                                         return llvm::dwarf::DW_ATE_float;
                                     }
                                     else
                                     {
                                         throw std::runtime_error("Not implemented yet");
                                     }
                                 }());
            cache[llvmType] = result;
            return result;
        }
        else if (llvmType->isPointerTy())
        {
            auto pointer = std::dynamic_pointer_cast<OpenCL::Parser::PointerType>(type);
            auto result = context.debugBuilder
                                 ->createPointerType(toDwarfType(pointer->getType().clone(), context), 64, 64);
            cache[llvmType] = result;
            return result;
        }
        else if (llvmType->isArrayTy())
        {
            auto array = std::dynamic_pointer_cast<OpenCL::Parser::ArrayType>(type);
            auto arrayedType = toDwarfType(array->getType()->clone(), context);
            auto* llvmArrayType = array->type(context);
            auto result = context.debugBuilder
                                 ->createArrayType(context.module->getDataLayout().getTypeSizeInBits(llvmArrayType),
                                                   getAlignment(llvmArrayType),
                                                   arrayedType,
                                                   llvm::MDTuple::get(context.context,
                                                                      context.debugBuilder->getOrCreateSubrange(0,
                                                                                                                array
                                                                                                                    ->getSize())));
            cache[llvmType] = result;
            return result;
        }
        else if (llvmType->isVoidTy())
        {
            return nullptr;
        }
        throw std::runtime_error("Not implemented yet");
    }

    void emitLocation(const OpenCL::Parser::Node* node, OpenCL::Parser::Context& context)
    {
        if (!node)
        {
            context.builder.SetCurrentDebugLocation(llvm::DebugLoc());
            return;
        }
        auto* scope = context.debugScope.empty() ? context.debugUnit : context.debugScope.back();
        context.builder.SetCurrentDebugLocation(llvm::DebugLoc::get(node->getLine(), node->getColumn(), scope));
    }

    llvm::Constant* getZeroFor(llvm::Type* type)
    {
        if (type->isIntegerTy())
        {
            return llvm::ConstantInt::get(type, 0);
        }
        else if (type->isFloatingPointTy())
        {
            return llvm::ConstantFP::get(type, 0);
        }
        else if (type->isPointerTy())
        {
            return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
        }
        else if (type->isStructTy())
        {
            auto* structType = llvm::cast<llvm::StructType>(type);
            std::size_t count = structType->getStructNumElements();
            std::vector<llvm::Constant*> constants;
            constants.reserve(count);
            for (std::size_t i = 0; i < count; i++)
            {
                constants.push_back(getZeroFor(structType->getTypeAtIndex(i)));
            }
            return llvm::ConstantStruct::get(structType, constants);
        }
        else if (type->isArrayTy())
        {
            auto* arrayType = llvm::cast<llvm::ArrayType>(type);
            std::size_t count = arrayType->getArrayNumElements();
            std::vector<llvm::Constant*> constants(count, getZeroFor(arrayType->getArrayElementType()));
            return llvm::ConstantArray::get(arrayType, constants);
        }
        else
        {
            throw std::runtime_error("Not implemented yet");
        }
    }

    std::size_t elementsNeededForType(llvm::Type* type)
    {
        if (type->isArrayTy())
        {
            return elementsNeededForType(type->getArrayElementType()) * type->getArrayNumElements();
        }
        else if (type->isStructTy())
        {
            std::size_t sum = 0;
            for (std::size_t i = 0; i < type->getStructNumElements(); i++)
            {
                sum += elementsNeededForType(type->getStructElementType(i));
            }
            return sum;
        }
        else
        {
            return 1;
        }
    }

    void match(llvm::ArrayRef<typename OpenCL::Parser::InitializerListBlock::variant> list,
               llvm::Value* pointer,
               const std::shared_ptr<OpenCL::Parser::Type>& type, OpenCL::Parser::Context& context)
    {
        auto* allocaType = type->type(context);
        if (list.empty())
        {
            if (allocaType->isStructTy() || allocaType->isArrayTy())
            {
                context.builder.CreateStore(getZeroFor(allocaType), pointer);
            }
            else
            {
                throw std::runtime_error("Scalar initializer can not be empty");
            }
        }
        else if (!allocaType->isStructTy() && !allocaType->isArrayTy())
        {
            auto[value, otherType] = std::visit([&context](auto&& value) -> std::pair<llvm::Value*,
                                                                                      std::shared_ptr<OpenCL::Parser::Type>>
                                                {
                                                    using T = std::decay_t<decltype(value)>;
                                                    if constexpr(std::is_same_v<T,
                                                                                OpenCL::Parser::InitializerListBlock>)
                                                    {
                                                        throw std::runtime_error(
                                                            "Only single level of braces allowed for scalar initialization");
                                                    }
                                                    else
                                                    {
                                                        return value->codegen(context);
                                                    }
                                                }, list[0]);
            castPrimitive(value, otherType->isSigned(), allocaType, type->isSigned(), context);
            context.builder.CreateStore(value, pointer);
        }
        else if (allocaType->isStructTy())
        {
            auto& structInfo = context.structs.at(allocaType->getStructName());
            std::size_t i = 0;
            for (auto& iter : structInfo.types)
            {
                auto* zero = context.builder.getInt32(0);
                auto* index = context.builder.getInt32(i);
                auto* member = context.builder.CreateInBoundsGEP(pointer, {zero, index});
                if (i >= list.size())
                {
                    context.builder.CreateStore(getZeroFor(iter->type(context)), member);
                }
                else
                {
                    std::visit([&context, member, iter](auto&& value)
                               {
                                   using T = std::decay_t<decltype(value)>;
                                   if constexpr(std::is_same_v<T, OpenCL::Parser::InitializerListBlock>)
                                   {
                                       match(value.getNonCommaExpressionsAndBlocks(), member, iter, context);
                                   }
                                   else
                                   {
                                       auto[newValue, otherType] = value->codegen(context);
                                       if (llvm::LoadInst* load = llvm::dyn_cast<llvm::LoadInst>(newValue);load &&
                                           llvm::isa<llvm::GlobalValue>(load->getPointerOperand())
                                           && load->getType()->getArrayElementType() == context.builder.getInt8Ty())
                                       {
                                           auto* zero = context.builder.getInt32(0);
                                           context.builder.CreateMemCpy(member,
                                                                        0,
                                                                        context.builder.CreateInBoundsGEP(load->getPointerOperand(),{zero,zero}),
                                                                        0,
                                                                        load->getType()->getArrayNumElements());
                                       }
                                       else
                                       {
                                           castPrimitive(newValue,
                                                         otherType->isSigned(),
                                                         iter->type(context),
                                                         iter->isSigned(),
                                                         context);
                                           context.builder.CreateStore(newValue, member);
                                       }
                                   }
                               }, list[i]);
                }
                i++;
            }
        }
        else if (allocaType->isArrayTy())
        {
            auto* zero = context.builder.getInt32(0);
            auto arrayType = std::dynamic_pointer_cast<OpenCL::Parser::ArrayType>(type);
            auto elementSize = elementsNeededForType(allocaType->getArrayElementType());
            std::shared_ptr heldType = arrayType->getType()->clone();
            std::size_t i = 0;
            for (auto iter = list.begin(); iter < list.end(); i++)
            {
                if (i >= allocaType->getArrayNumElements())
                {
                    throw std::runtime_error("More elements specified in initializer list than elements in array");
                }
                auto* index = context.builder.getInt32(i);
                auto* member = context.builder.CreateInBoundsGEP(pointer, {zero, index});
                auto end = iter + elementSize > list.end() ? list.end() : iter + elementSize;
                auto result = std::find_if(iter, end, [](auto&& value)
                {
                    return std::holds_alternative<OpenCL::Parser::InitializerListBlock>(value);
                });
                if (result == iter && std::holds_alternative<OpenCL::Parser::InitializerListBlock>(*result))
                {
                    match(std::get<OpenCL::Parser::InitializerListBlock>(*result).getNonCommaExpressionsAndBlocks(),
                          member,
                          heldType,
                          context);
                    iter++;
                }
                else
                {
                    if (iter == result)
                    {
                        result++;
                    }
                    match({iter, result}, member, heldType, context);
                    iter = result;
                }
            }
            for (; i < allocaType->getArrayNumElements(); i++)
            {
                auto* index = context.builder.getInt32(i);
                auto* member = context.builder.CreateInBoundsGEP(pointer, {zero, index});
                context.builder.CreateStore(getZeroFor(allocaType->getArrayElementType()), member);
            }
        }
    }

    llvm::Value* toBool(llvm::Value* value, OpenCL::Parser::Context& context)
    {
        if (value->getType()->isIntegerTy())
        {
            if (value->getType()->getIntegerBitWidth() > 1)
            {
                return context.builder
                              .CreateICmpNE(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 0));
            }
            return value;
        }
        else if (value->getType()->isFloatingPointTy())
        {
            return context.builder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
        }
        else if (value->getType()->isPointerTy())
        {
            return context.builder.CreateICmpNE(value,
                                                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(value
                                                                                                                 ->getType())));
        }
        else if (value->getType()->isArrayTy())
        {
            return context.builder.getIntN(1, 0);
        }
        else
        {
            return nullptr;
        }
    }
}

void OpenCL::Parser::Context::addValueToScope(const std::string& name, const OpenCL::Parser::Context::tuple& value)
{
    m_namedValues.back()[name] = value;
}

void OpenCL::Parser::Program::codegen(OpenCL::Parser::Context& context) const
{
    context.module = std::make_unique<llvm::Module>("main", context.context);
    std::string error;
    const std::string tripleStr = llvm::sys::getProcessTriple();
    llvm::Triple t(tripleStr);
    if (t.isOSBinFormatCOFF())
    {
        t.setObjectFormat(llvm::Triple::ELF);
    }
    auto target = llvm::TargetRegistry::lookupTarget(t.str(), error);
    if (!target)
    {
        throw std::runtime_error(error);
    }
    auto targetMachine = target->createTargetMachine(t.str(), "generic", "", {}, {});
    context.module->setDataLayout(targetMachine->createDataLayout());
    context.module->setTargetTriple(t.str());
    context.debugBuilder = new llvm::DIBuilder(*context.module);
    context.debugBuilder->finalize();
    context.debugUnit = context.debugBuilder->createFile("input.c", "../src");
    context.debugBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C99, context.debugUnit,
                                            "OpenCL Compiler", false, "", 0);
    for (auto& iter : getGlobals())
    {
        iter->codegen(context);
    }
    context.debugBuilder->finalize();
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::GlobalDeclaration::codegen(OpenCL::Parser::Context& context) const
{
    auto[constant, sign] = getOptionalValue() ? getOptionalValue()->codegen(context) : decltype(getOptionalValue()
        ->codegen(context)){};
    if (constant
        && (constant->getType() != getType()->type(context) || (sign && sign->isSigned()) != getType()->isSigned()))
    {
        castPrimitive(constant, sign->isSigned(), getType()->type(context), getType()->isSigned(), context);
    }

    auto* newGlobal = new llvm::GlobalVariable(*context.module,
                                               getType()->type(context),
                                               getType()->isConst(),
                                               llvm::GlobalVariable::LinkageTypes::ExternalLinkage,
                                               constant ? llvm::cast<llvm::Constant>(constant) :
                                               getZeroFor(getType()->type(context)),
                                               getName());
    context.debugBuilder->createGlobalVariableExpression(context.debugUnit,
                                                         getName(),
                                                         getName(),
                                                         context.debugUnit,
                                                         getLine(),
                                                         toDwarfType(getType(), context),
                                                         false);
    context.addGlobal(getName(), {newGlobal, getType()});
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::Function::codegen(OpenCL::Parser::Context& context) const
{
    if (!context.hasFunction(getName()))
    {
        {
            std::vector<const Type*> types;
            std::transform(getArguments().begin(), getArguments().end(), std::back_inserter(types), [](const auto& pair)
            {
                return pair.first.get();
            });
            context.addFunction(getName(), {getReturnType(), std::move(types)});
            context.functionRetType = getReturnType().get();
        }
        {
            std::set<std::size_t> needsByVal;
            std::vector<llvm::Type*> types;
            std::size_t i = 0;
            bool isStruct = dynamic_cast<const StructType*>(getReturnType().get());
            for (auto&[type, name] : getArguments())
            {
                if (!dynamic_cast<const StructType*>(type.get()))
                {
                    types.emplace_back(type->type(context));
                }
                else
                {
                    types.emplace_back(llvm::PointerType::getUnqual(type->type(context)));
                    needsByVal.insert(i + (isStruct ? 2 : 1));
                }
                i++;
            }
            if (isStruct)
            {
                types.insert(types.begin(), llvm::PointerType::getUnqual(getReturnType()->type(context)));
            }
            auto* ft = llvm::FunctionType::get(isStruct ? context.builder.getVoidTy() : getReturnType()->type(context),
                                               types,
                                               false);
            auto* fun = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, getName(), context.module.get());
            if (isStruct)
            {
                fun->addAttribute(1, llvm::Attribute::get(context.context, llvm::Attribute::StructRet));
                fun->addAttribute(1, llvm::Attribute::get(context.context, llvm::Attribute::NoAlias));
            }
            for (auto& iter : needsByVal)
            {
                fun->addAttribute(iter, llvm::Attribute::get(context.context, llvm::Attribute::ByVal));
            }
        }
    }
    if (!getBlockStatement())
    {
        return {};
    }
    context.currentFunction = context.module->getFunction(getName());
    std::size_t i = 0;
    for (auto& iter : context.currentFunction->args())
    {
        if (iter.hasAttribute(llvm::Attribute::StructRet))
        {
            continue;
        }
        if (!getArguments()[i].second.empty())
        {
            iter.setName(getArguments()[i++].second);
        }
    }

    std::vector<llvm::Metadata*> types;
    types.push_back(toDwarfType(getReturnType(), context));
    for (auto& iter : getArguments())
    {
        types.push_back(toDwarfType(iter.first, context));
    }
    auto* spType = context.debugBuilder->createSubroutineType({llvm::MDTuple::get(context.context, types)});
    auto* sp = context.debugBuilder
                      ->createFunction(context.debugUnit,
                                       getName(),
                                       {},
                                       context.debugUnit,
                                       getLine(),
                                       spType,
                                       false,
                                       true,
                                       getScopeLine());
    context.currentFunction->setSubprogram(sp);
    context.debugScope.push_back(sp);
    auto* bb = llvm::BasicBlock::Create(context.context, "entry", context.currentFunction);
    context.builder.SetInsertPoint(bb);
    context.clearScope();
    i = 0;
    emitLocation(nullptr, context);
    for (auto& iter : context.currentFunction->args())
    {
        if (iter.hasAttribute(llvm::Attribute::StructRet))
        {
            continue;
        }
        if (getArguments()[i].second.empty())
        {
            continue;
        }
        llvm::AllocaInst* alloca;
        auto createDebug = [&]
        {
            auto* local = context.debugBuilder->createParameterVariable(sp,
                                                                        iter.getName(),
                                                                        i,
                                                                        context.debugUnit,
                                                                        getLine(),
                                                                        toDwarfType(getArguments()[i].first, context));
            context.debugBuilder->insertDeclare(alloca,
                                                local,
                                                context.debugBuilder->createExpression(),
                                                llvm::DebugLoc::get(getLine(), 0, sp),
                                                context.builder.GetInsertBlock());
        };
        llvm::IRBuilder<>
            tmpB(&context.currentFunction->getEntryBlock(), context.currentFunction->getEntryBlock().begin());
        if (!iter.hasByValAttr())
        {
            alloca = tmpB.CreateAlloca(iter.getType());
            createDebug();
            alloca->setAlignment(getAlignment(iter.getType()));
            context.builder.CreateStore(&iter, alloca);
            context.addValueToScope(iter.getName(), {alloca, getArguments()[i++].first});
        }
        else
        {
            auto* ptrType = llvm::cast<llvm::PointerType>(iter.getType());
            alloca = tmpB.CreateAlloca(ptrType->getPointerElementType());
            createDebug();
            alloca->setAlignment(getAlignment(ptrType->getPointerElementType()));
            auto* zero = context.builder.getInt32(0);
            auto* value = context.builder.CreateInBoundsGEP(alloca, {zero, zero});
            context.builder.CreateStore(&iter, value);
            context.addValueToScope(iter.getName(), {alloca, getArguments()[i++].first});
        }
    }

    emitLocation(getBlockStatement(), context);
    getBlockStatement()->codegen(context);
    auto& block = context.currentFunction->back();
    if (block.empty() || !block.back().isTerminator())
    {
        if (!getReturnType()->isVoid())
        {
            auto* type = getReturnType()->type(context);
            llvm::Value* value = nullptr;
            if (type->isIntegerTy())
            {
                value = llvm::ConstantInt::get(type, 0);
            }
            else if (type->isFloatingPointTy())
            {
                value = llvm::ConstantFP::get(type, 0);
            }
            context.builder.CreateRet(value);
        }
        else
        {
            context.builder.CreateRetVoid();
        }
    }

    context.debugScope.pop_back();

    context.debugBuilder->finalize();
    if (llvm::verifyFunction(*context.currentFunction, &llvm::errs()))
    {
        context.currentFunction->print(llvm::outs());
        std::terminate();
    }

    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::Declarations::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    llvm::IRBuilder<> tmpB(&context.currentFunction->getEntryBlock(), context.currentFunction->getEntryBlock().begin());
    for (auto&[type, name, optionalExpression] : getDeclarations())
    {
        if (auto array = std::dynamic_pointer_cast<ArrayType>(type);array && array->getSize() == 0)
        {
            if (!optionalExpression)
            {
                throw std::runtime_error("Initializer list needed to deduce size of array");
            }
            auto* result = dynamic_cast<InitializerListBlock*>(optionalExpression.get());
            if (!result)
            {
                throw std::runtime_error("Initializer list needed to deduce size of array");
            }
            auto elementSize = elementsNeededForType(array->getType()->type(context));
            std::size_t i = 0;
            for (auto iter = result->getNonCommaExpressionsAndBlocks().begin();
                 iter < result->getNonCommaExpressionsAndBlocks().end(); i++)
            {
                auto end = std::find_if(iter,
                                        iter + elementSize > result->getNonCommaExpressionsAndBlocks().end() ? result
                                            ->getNonCommaExpressionsAndBlocks().end() : iter + elementSize,
                                        [](auto&& value)
                                        {
                                            return std::holds_alternative<OpenCL::Parser::InitializerListBlock>(value);
                                        });
                if (iter == end)
                {
                    iter++;
                }
                else
                {
                    iter = end;
                }
            }
            array->setSize(i);
            if (!array->getSize())
            {
                throw std::runtime_error("Array can't be of size 0");
            }
        }
        auto* allocaType = type->type(context);
        auto* alloca = tmpB.CreateAlloca(allocaType, nullptr);
        auto* sp = context.debugScope.empty() ? context.debugUnit : context.debugScope
                                                                           .back();
        auto* di = context.debugBuilder
                          ->createAutoVariable(sp,
                                               name,
                                               context.debugUnit,
                                               getLine(),
                                               toDwarfType(type, context));
        context.debugBuilder->insertDeclare(alloca,
                                            di,
                                            context.debugBuilder->createExpression(),
                                            llvm::DebugLoc::get(getLine(), getColumn(), sp),
                                            context.builder.GetInsertBlock());
        alloca->setAlignment(getAlignment(allocaType));
        context.addValueToScope(name, {alloca, type});
        if (optionalExpression)
        {
            if (dynamic_cast<const InitializerListScalarExpression*>(optionalExpression.get()))
            {
                auto[value, otherType] = optionalExpression->codegen(context);
                castPrimitive(value, otherType->isSigned(), allocaType, type->isSigned(), context);
                context.builder.CreateStore(value, alloca);
            }
            else if (auto result = dynamic_cast<const InitializerListBlock*>(optionalExpression.get()))
            {
                if (allocaType->isArrayTy() && result->getNonCommaExpressionsAndBlocks().size() == 1)
                {
                    auto* zero = context.builder.getInt32(0);
                    for (std::size_t i = 0; i < allocaType->getArrayNumElements(); i++)
                    {
                        auto* index = context.builder.getInt32(i);
                        auto* member = context.builder.CreateInBoundsGEP(alloca, {zero, index});
                        std::visit([&context, member, type = std::dynamic_pointer_cast<ArrayType>(type)](auto&& value)
                                   {
                                       using T = std::decay_t<decltype(value)>;
                                       if constexpr(std::is_same_v<T, OpenCL::Parser::InitializerListBlock>)
                                       {
                                           match(value.getNonCommaExpressionsAndBlocks(),
                                                 member,
                                                 type->getType()->clone(),
                                                 context);
                                       }
                                       else
                                       {
                                           auto[newValue, otherType] = value->codegen(context);
                                           castPrimitive(newValue,
                                                         otherType->isSigned(),
                                                         type->getType()->type(context),
                                                         type->getType()->isSigned(),
                                                         context);
                                           context.builder.CreateStore(newValue, member);
                                       }
                                   }, result->getNonCommaExpressionsAndBlocks()[0]);
                    }
                }
                match(result->getNonCommaExpressionsAndBlocks(), alloca, type, context);
            }
        }
    }
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ReturnStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    bool isStruct = dynamic_cast<const StructType*>(context.functionRetType);
    if (!isStruct)
    {
        auto* retType = context.currentFunction->getReturnType();
        auto[value, sign] = getExpression().codegen(context);
        castPrimitive(value, sign->isSigned(), retType, context.functionRetType->isSigned(), context);
        context.builder.CreateRet(value);
        return {};
    }
    else
    {
        auto[value, sign] = getExpression().codegen(context);
        auto* args = context.currentFunction->args().begin();
        if (value->getType() != args->getType()->getPointerElementType())
        {
            throw std::runtime_error("Struct values are not the same");
        }
        context.builder.CreateStore(value, args);
        context.builder.CreateRetVoid();
        return {};
    }
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ExpressionStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    if (getOptionalExpression())
    {
        return getOptionalExpression()->codegen(context);
    }
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::IfStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, sign] = getExpression().codegen(context);
    value = toBool(value, context);
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* thenBB = llvm::BasicBlock::Create(context.context, "then", function);
    auto* elseBB = getElseBranch() ? llvm::BasicBlock::Create(context.context, "else") : nullptr;
    auto* mergeBB = llvm::BasicBlock::Create(context.context, "ifcont");

    context.builder.CreateCondBr(value, thenBB, elseBB ? elseBB : mergeBB);

    context.builder.SetInsertPoint(thenBB);
    getBranch().codegen(context);

    if (!context.builder.GetInsertBlock()->getTerminator())
    {
        context.builder.CreateBr(mergeBB);
    }

    if (elseBB)
    {
        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);
        getElseBranch()->codegen(context);
        if (!context.builder.GetInsertBlock()->getTerminator())
        {
            context.builder.CreateBr(mergeBB);
        }
    }

    function->getBasicBlockList().push_back(mergeBB);
    context.builder.SetInsertPoint(mergeBB);

    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::BlockStatement::codegen(OpenCL::Parser::Context& context) const
{
    context.pushScope();
    context.debugScope.push_back(context.debugBuilder->createLexicalBlock(context.debugScope.back(),
                                                                          context.debugUnit,
                                                                          getLine(),
                                                                          getColumn()));
    for (auto& iter : getBlockItems())
    {
        iter->codegen(context);
        if (dynamic_cast<ReturnStatement*>(iter.get())
            || dynamic_cast<BreakStatement*>(iter.get())
            || dynamic_cast<ContinueStatement*>(iter.get()))
        {
            break;
        }
    }
    context.debugScope.pop_back();
    context.popScope();
    return {};
}

namespace
{
    void doForLoop(const OpenCL::Parser::Expression* controlling,
                   const OpenCL::Parser::Expression* post,
                   const OpenCL::Parser::Statement& statement,
                   OpenCL::Parser::Context& context)
    {
        auto* function = context.builder.GetInsertBlock()->getParent();

        auto* postBB = llvm::BasicBlock::Create(context.context, "post", function);
        auto* condBB = llvm::BasicBlock::Create(context.context, "cond");
        context.builder.CreateBr(condBB);
        auto* blockBB = llvm::BasicBlock::Create(context.context, "block");
        auto* endBB = llvm::BasicBlock::Create(context.context, "end");

        context.breakBlocks.push_back(endBB);
        context.continueBlocks.push_back(postBB);

        context.builder.SetInsertPoint(postBB);
        if (post)
        {
            post->codegen(context);
        }
        context.builder.CreateBr(condBB);

        function->getBasicBlockList().push_back(condBB);
        context.builder.SetInsertPoint(condBB);
        auto* value = controlling ? controlling->codegen(context).first : context.builder.getInt1(true);
        value = toBool(value, context);
        context.builder.CreateCondBr(value, blockBB, endBB);

        function->getBasicBlockList().push_back(blockBB);
        context.builder.SetInsertPoint(blockBB);
        statement.codegen(context);
        context.builder.CreateBr(postBB);

        function->getBasicBlockList().push_back(endBB);
        context.builder.SetInsertPoint(endBB);
        context.breakBlocks.pop_back();
        context.continueBlocks.pop_back();
    }
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ForStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    if (getInitial())
    {
        getInitial()->codegen(context);
    }
    doForLoop(getControlling(), getPost(), getStatement(), context);
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ForDeclarationStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    context.pushScope();
    context.debugScope.push_back(context.debugBuilder->createLexicalBlock(context.debugScope.back(),
                                                                          context.debugUnit,
                                                                          getLine(),
                                                                          getColumn()));
    getInitial().codegen(context);
    doForLoop(getControlling(), getPost(), getStatement(), context);
    context.debugScope.pop_back();
    context.popScope();
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::HeadWhileStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* condBB = llvm::BasicBlock::Create(context.context, "cond", function);
    context.builder.CreateBr(condBB);
    auto* blockBB = llvm::BasicBlock::Create(context.context, "block");
    auto* endBB = llvm::BasicBlock::Create(context.context, "end");

    context.breakBlocks.push_back(endBB);
    context.continueBlocks.push_back(condBB);

    context.builder.SetInsertPoint(condBB);
    auto* value = getExpression().codegen(context).first;
    value = toBool(value, context);
    context.builder.CreateCondBr(value, blockBB, endBB);

    function->getBasicBlockList().push_back(blockBB);
    context.builder.SetInsertPoint(blockBB);
    getStatement().codegen(context);
    context.builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(endBB);
    context.builder.SetInsertPoint(endBB);
    context.breakBlocks.pop_back();
    context.continueBlocks.pop_back();
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::FootWhileStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* blockBB = llvm::BasicBlock::Create(context.context, "block", function);
    context.builder.CreateBr(blockBB);
    auto* condBB = llvm::BasicBlock::Create(context.context, "cond");
    auto* endBB = llvm::BasicBlock::Create(context.context, "end");

    context.breakBlocks.push_back(endBB);
    context.continueBlocks.push_back(condBB);

    context.builder.SetInsertPoint(blockBB);
    getStatement().codegen(context);
    context.builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(condBB);
    context.builder.SetInsertPoint(condBB);
    auto* value = getExpression().codegen(context).first;
    value = toBool(value, context);
    context.builder.CreateCondBr(value, blockBB, endBB);

    function->getBasicBlockList().push_back(endBB);
    context.builder.SetInsertPoint(endBB);
    context.breakBlocks.pop_back();
    context.continueBlocks.pop_back();
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::Expression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto left = getNonCommaExpression().codegen(context);
    auto right = getOptionalNonCommaExpression() ? getOptionalNonCommaExpression()->codegen(context)
                                                 : decltype(getOptionalNonCommaExpression()->codegen(context)){};
    return right.first ? right : left;
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::AssignmentExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getUnaryFactor().codegen(context);
    if (sign->isConst())
    {
        throw std::runtime_error("Can't assign value to const");
    }
    auto* load = llvm::dyn_cast<llvm::LoadInst>(left);
    if (!load)
    {
        throw std::runtime_error("Not allowed to assign to non lvalue");
    }
    switch (getAssignOperator())
    {
    case AssignOperator::NoOperator:
    {
        auto[value, sign] = getNonCommaExpression().codegen(context);
        castPrimitive(value, sign->isSigned(), left->getType(), sign->isSigned(), context);
        context.builder.CreateStore(value, load->getPointerOperand());
        break;
    }
    case AssignOperator::PlusAssign:
    {
        llvm::Value* current = left;
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        if (current->getType()->isIntegerTy())
        {
            current = context.builder.CreateAdd(current, newValue);
        }
        else if (current->getType()->isFloatingPointTy())
        {
            current = context.builder.CreateFAdd(current, newValue);
        }
        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), context);
        context.builder.CreateStore(current, load->getPointerOperand());
        break;
    }
    case AssignOperator::MinusAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        if (current->getType()->isIntegerTy())
        {
            current = context.builder.CreateSub(current, newValue);
        }
        else if (current->getType()->isFloatingPointTy())
        {
            current = context.builder.CreateFSub(current, newValue);
        }
        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), context);
        context.builder.CreateStore(current, load->getPointerOperand());
        break;
    }
    case AssignOperator::DivideAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        if (current->getType()->isIntegerTy())
        {
            if (sign || newSign)
            {
                current = context.builder.CreateSDiv(current, newValue);
            }
            else
            {
                current = context.builder.CreateUDiv(current, newValue);
            }
        }
        else
        {
            current = context.builder.CreateFDiv(current, newValue);
        }
        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), context);
        context.builder.CreateStore(current, load->getPointerOperand());
        break;
    }
    case AssignOperator::MultiplyAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        if (current->getType()->isIntegerTy())
        {
            current = context.builder.CreateMul(current, newValue);
        }
        else
        {
            current = context.builder.CreateFMul(current, newValue);
        }
        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), context);
        context.builder.CreateStore(current, load->getPointerOperand());
        break;
    }
    case AssignOperator::ModuloAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        context.builder.CreateStore(context.builder.CreateSRem(current, newValue), load->getPointerOperand());
        break;
    }
    case AssignOperator::LeftShiftAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        context.builder.CreateStore(context.builder.CreateShl(current, newValue), load->getPointerOperand());
        break;
    }
    case AssignOperator::RightShiftAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        context.builder.CreateStore(context.builder.CreateAShr(current, newValue), load->getPointerOperand());
        break;
    }
    case AssignOperator::BitAndAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        context.builder.CreateStore(context.builder.CreateAnd(current, newValue), load->getPointerOperand());
        break;
    }
    case AssignOperator::BitOrAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        context.builder.CreateStore(context.builder.CreateOr(current, newValue), load->getPointerOperand());
        break;
    }
    case AssignOperator::BitXorAssign:
    {
        llvm::Value* current = context.builder.CreateLoad(left);
        auto[newValue, newSign] = getNonCommaExpression().codegen(context);
        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), context);
        context.builder.CreateStore(context.builder.CreateXor(current, newValue), load->getPointerOperand());
        break;
    }
    }
    return {context.builder.CreateLoad(load->getPointerOperand()), sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::Term::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getCastExpression().codegen(context);
    for (auto&[op, cast] : getOptionalCastExpressions())
    {
        auto[right, rsign] = cast.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), context);

        switch (op)
        {
        case BinaryDotOperator::BinaryMultiply:
            if (left->getType()->isIntegerTy())
            {
                left = context.builder.CreateMul(left, right);
            }
            else
            {
                left = context.builder.CreateFMul(left, right);
            }
            break;
        case BinaryDotOperator::BinaryDivide:
            if (left->getType()->isIntegerTy())
            {
                if (sign->isSigned() || rsign->isSigned())
                {
                    left = context.builder.CreateSDiv(left, right, "divtmp");
                }
                else
                {
                    left = context.builder.CreateUDiv(left, right);
                }
            }
            else
            {
                left = context.builder.CreateFDiv(left, right);
            }
            break;
        case BinaryDotOperator::BinaryRemainder:
            if (left->getType()->isIntegerTy())
            {
                if (sign->isSigned() || rsign->isSigned())
                {
                    left = context.builder.CreateSRem(left, right);
                }
                else
                {
                    left = context.builder.CreateURem(left, right);
                }
            }
            else
            {
                throw std::runtime_error("Invalid operands to %");
            }
            break;
        }
        if (std::static_pointer_cast<PrimitiveType>(sign)->getBitCount()
            < std::static_pointer_cast<PrimitiveType>(rsign)->getBitCount())
        {
            sign = rsign;
        }
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::AdditiveExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getTerm().codegen(context);
    for (auto&[op, term] : getOptionalTerms())
    {
        auto[right, rsign] = term.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), context);

        switch (op)
        {
        case BinaryDashOperator::BinaryPlus:
        {
            if (left->getType()->isIntegerTy())
            {
                left = context.builder.CreateAdd(left, right);
            }
            else
            {
                left = context.builder.CreateFAdd(left, right);
            }
            break;
        }
        case BinaryDashOperator::BinaryMinus:
        {
            if (left->getType()->isIntegerTy())
            {
                left = context.builder.CreateSub(left, right);
            }
            else
            {
                left = context.builder.CreateFSub(left, right);
            }
            break;
        }
        }
        if (std::static_pointer_cast<PrimitiveType>(sign)->getBitCount()
            < std::static_pointer_cast<PrimitiveType>(rsign)->getBitCount())
        {
            sign = rsign;
        }
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ShiftExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getAdditiveExpression().codegen(context);
    for (auto&[op, rel] : getOptionalAdditiveExpressions())
    {
        auto[right, rsign] = rel.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), context);
        if (!left->getType()->isIntegerTy())
        {
            throw std::runtime_error("Only integer types allowed in shift expressions");
        }
        switch (op)
        {
        case ShiftOperator::Right:left = context.builder.CreateAShr(left, right);
            break;
        case ShiftOperator::Left:left = context.builder.CreateShl(left, right);
        }
        if (std::static_pointer_cast<PrimitiveType>(sign)->getBitCount()
            < std::static_pointer_cast<PrimitiveType>(rsign)->getBitCount())
        {
            sign = rsign;
        }
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::RelationalExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getShiftExpression().codegen(context);
    for (auto&[op, rel] : getOptionalRelationalExpressions())
    {
        auto[right, rsign] = rel.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), context);

        switch (op)
        {
        case RelationalOperator::LessThan:
            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
            {
                if (sign->isSigned() || rsign->isSigned())
                {
                    left = context.builder.CreateICmpSLT(left, right);
                }
                else
                {
                    left = context.builder.CreateICmpULT(left, right);
                }
            }
            else if (left->getType()->isFloatingPointTy())
            {
                left = context.builder.CreateFCmpULT(left, right);
            }
            break;
        case RelationalOperator::LessThanOrEqual:
            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
            {
                if (sign->isSigned() || rsign->isSigned())
                {
                    left = context.builder.CreateICmpSLE(left, right);
                }
                else
                {
                    left = context.builder.CreateICmpULE(left, right);
                }
            }
            else if (left->getType()->isFloatingPointTy())
            {
                left = context.builder.CreateFCmpULE(left, right);
            }
            break;
        case RelationalOperator::GreaterThan:
            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
            {
                if (sign->isSigned() || rsign->isSigned())
                {
                    left = context.builder.CreateICmpSGT(left, right);
                }
                else
                {
                    left = context.builder.CreateICmpUGT(left, right);
                }
            }
            else if (left->getType()->isFloatingPointTy())
            {
                left = context.builder.CreateFCmpUGT(left, right);
            }
            break;
        case RelationalOperator::GreaterThanOrEqual:
            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
            {
                if (sign->isSigned() || rsign->isSigned())
                {
                    left = context.builder.CreateICmpSGE(left, right);
                }
                else
                {
                    left = context.builder.CreateICmpUGE(left, right);
                }
            }
            else if (left->getType()->isFloatingPointTy())
            {
                left = context.builder.CreateFCmpUGE(left, right);
            }
            break;
        }
        sign = std::make_shared<PrimitiveType>(32, false, false, true);
        left = context.builder.CreateZExt(left, context.builder.getInt32Ty());
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::EqualityExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getRelationalExpression().codegen(context);
    for (auto&[op, factor] : getOptionalRelationalExpressions())
    {
        auto[right, rsign] = factor.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), context);

        switch (op)
        {
        case EqualityOperator::Equal:
            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
            {
                left = context.builder.CreateICmpEQ(left, right);
            }
            else if (left->getType()->isFloatingPointTy())
            {
                left = context.builder.CreateFCmpUEQ(left, right);
            }
            break;
        case EqualityOperator::NotEqual:
            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
            {
                left = context.builder.CreateICmpNE(left, right);
            }
            else
            {
                left = context.builder.CreateFCmpUNE(left, right);
            }
            break;
        }
        sign = std::make_shared<PrimitiveType>(32, false, false, true);
        left = context.builder.CreateZExt(left, context.builder.getInt32Ty());
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::LogicalAndExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getBitOrExpression().codegen(context);
    for (auto& factor : getOptionalBitOrExpressions())
    {
        auto* function = context.builder.GetInsertBlock()->getParent();
        if (left->getType()->isFloatingPointTy())
        {
            left = context.builder.CreateFCmpUNE(left, llvm::ConstantFP::get(left->getType(), 0));
        }
        else if (left->getType()->isIntegerTy() && left->getType()->getIntegerBitWidth() > 1)
        {
            left = context.builder.CreateICmpNE(left, context.builder.getInt32(0));
        }
        auto* thenBB = llvm::BasicBlock::Create(context.context, "then", function);
        auto* elseBB = llvm::BasicBlock::Create(context.context, "else");
        auto* mergeBB = llvm::BasicBlock::Create(context.context, "ifcond");

        context.builder.CreateCondBr(left, thenBB, elseBB);

        context.builder.SetInsertPoint(thenBB);
        auto[right, rsign] = factor.codegen(context);
        if (right->getType()->isFloatingPointTy())
        {
            right = context.builder.CreateFCmpUNE(right, llvm::ConstantFP::get(left->getType(), 0));
        }
        else if (right->getType()->isIntegerTy() && right->getType()->getIntegerBitWidth() > 1)
        {
            right = context.builder.CreateICmpNE(right, context.builder.getInt32(0));
        }
        right = context.builder.CreateZExt(right, context.builder.getInt32Ty());
        context.builder.CreateBr(mergeBB);
        thenBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);

        context.builder.CreateBr(mergeBB);
        elseBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        context.builder.SetInsertPoint(mergeBB);
        auto* pn = context.builder.CreatePHI(context.builder.getInt32Ty(), 2, "iftmp");
        pn->addIncoming(right, thenBB);
        pn->addIncoming(context.builder.getInt32(0), elseBB);
        left = pn;
        sign = std::make_shared<PrimitiveType>(32, false, false, true);
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::LogicalOrExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getAndExpression().codegen(context);
    for (auto& factor : getOptionalAndExpressions())
    {
        auto* function = context.builder.GetInsertBlock()->getParent();
        if (left->getType()->isFloatingPointTy())
        {
            left = context.builder.CreateFCmpUNE(left, llvm::ConstantFP::get(left->getType(), 0));
        }
        else if (left->getType()->isIntegerTy() && left->getType()->getIntegerBitWidth() > 1)
        {
            left = context.builder.CreateICmpNE(left, context.builder.getInt32(0));
        }
        auto* thenBB = llvm::BasicBlock::Create(context.context, "then", function);
        auto* elseBB = llvm::BasicBlock::Create(context.context, "else");
        auto* mergeBB = llvm::BasicBlock::Create(context.context, "ifcond");

        context.builder.CreateCondBr(left, elseBB, thenBB);

        context.builder.SetInsertPoint(thenBB);
        auto[right, rsign] = factor.codegen(context);
        if (right->getType()->isFloatingPointTy())
        {
            right = context.builder.CreateFCmpUNE(right, llvm::ConstantFP::get(left->getType(), 0));
        }
        else if (right->getType()->isIntegerTy() && right->getType()->getIntegerBitWidth() > 1)
        {
            right = context.builder.CreateICmpNE(right, context.builder.getInt32(0));
        }
        right = context.builder.CreateZExt(right, context.builder.getInt32Ty());
        context.builder.CreateBr(mergeBB);
        thenBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);

        context.builder.CreateBr(mergeBB);
        elseBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        context.builder.SetInsertPoint(mergeBB);
        auto* pn = context.builder.CreatePHI(context.builder.getInt32Ty(), 2);
        pn->addIncoming(right, thenBB);
        pn->addIncoming(context.builder.getInt32(1), elseBB);
        left = pn;
        sign = std::make_shared<PrimitiveType>(32, false, false, true);
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ConditionalExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, vsign] = getLogicalOrExpression().codegen(context);
    if (getOptionalExpression() && getOptionalConditionalExpression())
    {
        value = toBool(value, context);
        auto* function = context.builder.GetInsertBlock()->getParent();

        auto* thenBB = llvm::BasicBlock::Create(context.context, "then", function);
        auto* elseBB = llvm::BasicBlock::Create(context.context, "else");
        auto* mergeBB = llvm::BasicBlock::Create(context.context, "ifcont");

        context.builder.CreateCondBr(value, thenBB, elseBB);

        context.builder.SetInsertPoint(thenBB);
        auto[thenV, tsign] = getOptionalExpression()->codegen(context);

        context.builder.CreateBr(mergeBB);
        thenBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);

        auto[elseV, esign] = getOptionalConditionalExpression()->codegen(context);

        context.builder.CreateBr(mergeBB);
        elseBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        context.builder.SetInsertPoint(mergeBB);
        arithmeticCast(thenV, tsign->isSigned(), elseV, esign->isSigned(), context);
        auto* pn = context.builder.CreatePHI(thenV->getType(), 2);
        pn->addIncoming(thenV, thenBB);
        pn->addIncoming(elseV, elseBB);
        return {pn, tsign};
    }
    return {value, vsign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::BreakStatement::codegen(Context& context) const
{
    emitLocation(this, context);
    context.builder.CreateBr(context.breakBlocks.back());
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::ContinueStatement::codegen(Context& context) const
{
    emitLocation(this, context);
    context.builder.CreateBr(context.continueBlocks.back());
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::BitAndExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getEqualityExpression().codegen(context);
    for (auto& factor : getOptionalEqualityExpressions())
    {
        auto[right, rsign] = factor.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, sign->isSigned(), context);
        left = context.builder.CreateAnd(left, right);
        if (std::static_pointer_cast<PrimitiveType>(sign)->getBitCount()
            < std::static_pointer_cast<PrimitiveType>(rsign)->getBitCount())
        {
            sign = rsign;
        }
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::BitXorExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getBitAndExpression().codegen(context);
    for (auto& factor : getOptionalBitAndExpressions())
    {
        auto[right, rsign] = factor.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, sign->isSigned(), context);
        left = context.builder.CreateXor(left, right);
        if (std::static_pointer_cast<PrimitiveType>(sign)->getBitCount()
            < std::static_pointer_cast<PrimitiveType>(rsign)->getBitCount())
        {
            sign = rsign;
        }
    }
    return {left, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::BitOrExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[left, sign] = getBitXorExpression().codegen(context);
    for (auto& factor : getOptionalBitXorExpressions())
    {
        auto[right, rsign] = factor.codegen(context);
        arithmeticCast(left, sign->isSigned(), right, sign->isSigned(), context);
        left = context.builder.CreateOr(left, right);
        if (std::static_pointer_cast<PrimitiveType>(sign)->getBitCount()
            < std::static_pointer_cast<PrimitiveType>(rsign)->getBitCount())
        {
            sign = rsign;
        }
    }
    return {left, sign};
}

llvm::Type* OpenCL::Parser::PrimitiveType::type(Context& context) const
{
    if (isFloatingPoint())
    {
        if (getBitCount() == 32)
        {
            return context.builder.getFloatTy();
        }
        else if (getBitCount() == 64)
        {
            return context.builder.getDoubleTy();
        }
        else
        {
            throw std::runtime_error("Invalid bit size for floating point");
        }
    }
    else if (getBitCount())
    {
        return context.builder.getIntNTy(getBitCount());
    }
    else
    {
        return context.builder.getVoidTy();
    }
}

llvm::Type* OpenCL::Parser::PointerType::type(OpenCL::Parser::Context& context) const
{
    auto* type = getType().type(context);
    if (!type->isVoidTy())
    {
        return llvm::PointerType::getUnqual(type);
    }
    else
    {
        return context.builder.getInt64Ty();
    }
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PrimaryExpressionIdentifier::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, sign] = context.getNamedValue(getIdentifier());
    if (!value)
    {
        return {context.module->getFunction(getIdentifier()), sign};
    }
    return {context.builder.CreateLoad(value), sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PrimaryExpressionConstant::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    return std::visit([&context](auto&& value) -> std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>>
                      {
                          using T = std::decay_t<decltype(value)>;
                          if constexpr(std::is_same_v<T, std::int32_t>)
                          {
                              return {context.builder.getInt32(value),
                                      std::make_shared<PrimitiveType>(32, false, false, true)};
                          }
                          else if constexpr(std::is_same_v<T, std::uint32_t>)
                          {
                              return {context.builder.getInt32(value),
                                      std::make_shared<PrimitiveType>(32, false, false, true)};
                          }
                          else if constexpr(std::is_same_v<T, std::int64_t>)
                          {
                              return {context.builder.getInt64(value),
                                      std::make_shared<PrimitiveType>(64, false, false, true)};
                          }
                          else if constexpr(std::is_same_v<T, std::uint64_t>)
                          {
                              return {context.builder.getInt64(value),
                                      std::make_shared<PrimitiveType>(64, false, false, false)};
                          }
                          else if constexpr(std::is_same_v<T, float>)
                          {
                              return {llvm::ConstantFP::get(llvm::Type::getFloatTy(context.context), value),
                                      std::make_shared<PrimitiveType>(32, false, true, true)};
                          }
                          else if constexpr(std::is_same_v<T, double>)
                          {
                              return {llvm::ConstantFP::get(llvm::Type::getDoubleTy(context.context), value),
                                      std::make_shared<PrimitiveType>(64, false, true, true)};
                          }
                          else if constexpr(std::is_same_v<T, std::string>)
                          {
                              return {context.builder.CreateLoad(context.builder.CreateGlobalString(value)),
                                      std::make_shared<PointerType>(std::make_unique<PrimitiveType>(8,
                                                                                                    false,
                                                                                                    false,
                                                                                                    true), false)};
                          }
                          else
                          {
                              throw std::runtime_error("Not implemented");
                          }
                      }, getValue());
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PrimaryExpressionParenthese::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    return getExpression().codegen(context);
}

std::int64_t OpenCL::Parser::PrimaryExpressionParenthese::solveConstantExpression() const
{
    return getExpression().solveConstantExpression();
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionPrimaryExpression::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    return getPrimaryExpression().codegen(context);
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionSubscript::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, sign] = getPostFixExpression().codegen(context);
    if (llvm::isa<llvm::ArrayType>(value->getType()))
    {
        auto arrayType = std::dynamic_pointer_cast<ArrayType>(sign);
        auto* arrayPointer = llvm::cast<llvm::LoadInst>(value)->getPointerOperand();
        auto* index = getExpression().codegen(context).first;
        auto* zero = context.builder.getIntN(index->getType()->getIntegerBitWidth(), 0);
        return {context.builder.CreateLoad(context.builder.CreateInBoundsGEP(arrayPointer, {zero, index})),
                arrayType->getType()->clone()};
    }
    else if (llvm::isa<llvm::PointerType>(value->getType()))
    {
        auto pointerType = std::dynamic_pointer_cast<PointerType>(sign);
        auto* index = getExpression().codegen(context).first;
        return {context.builder.CreateLoad(context.builder.CreateInBoundsGEP(value, index)),
                pointerType->getType().clone()};
    }
    else
    {
        return {};
    }
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionDot::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto* structValue = getPostFixExpression().codegen(context).first;
    auto* structLoad = llvm::cast<llvm::LoadInst>(structValue);
    auto* type = llvm::dyn_cast<llvm::StructType>(structValue->getType());
    if (!type)
    {
        throw std::runtime_error("Can only apply . to struct or union");
    }
    auto* zero = context.builder.getInt32(0);
    auto& structInfo = context.structs.at(structValue->getType()->getStructName());
    if (!structInfo.isUnion)
    {
        auto* index = context.builder.getInt32(structInfo.order.at(getIdentifier()));
        auto memberType = structInfo.types.at(index->getValue().getLimitedValue());
        auto* pointer = context.builder.CreateInBoundsGEP(structLoad->getPointerOperand(), {zero, index});
        return {context.builder.CreateLoad(pointer), memberType};
    }
    else
    {
        auto memberType = structInfo.types.at(structInfo.order.at(getIdentifier()));
        auto* pointer = context.builder.CreateInBoundsGEP(structLoad->getPointerOperand(), {zero, zero});
        auto* cast = context.builder.CreateBitCast(pointer, llvm::PointerType::getUnqual(memberType->type(context)));
        return {context.builder.CreateLoad(cast), memberType};
    }
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::UnaryExpressionPostFixExpression::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    return getPostFixExpression().codegen(context);
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::UnaryExpressionUnaryOperator::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[rhs, sign] = getUnaryExpression().codegen(context);
    switch (getAnOperator())
    {
    case UnaryOperator::Increment:
    {
        llvm::Value* newValue = nullptr;
        if (rhs->getType()->isIntegerTy())
        {
            newValue = context.builder.CreateAdd(rhs, context.builder.getIntN(rhs->getType()->getIntegerBitWidth(), 1));
        }
        else if (rhs->getType()->isFloatingPointTy())
        {
            newValue = context.builder.CreateFAdd(rhs, llvm::ConstantFP::get(rhs->getType(), 0));
        }
        else
        {
            throw std::runtime_error("Cannot apply unary ++ to type");
        }
        if (!llvm::isa<llvm::LoadInst>(rhs))
        {
            throw std::runtime_error("Cannot apply unary ++ to non lvalue");
        }
        context.builder.CreateStore(newValue, llvm::cast<llvm::LoadInst>(rhs)->getPointerOperand());
        return {newValue, sign};
    }
    case UnaryOperator::Decrement:
    {
        llvm::Value* newValue = nullptr;
        if (rhs->getType()->isIntegerTy())
        {
            newValue = context.builder.CreateSub(rhs, context.builder.getIntN(rhs->getType()->getIntegerBitWidth(), 1));
        }
        else if (rhs->getType()->isFloatingPointTy())
        {
            newValue = context.builder.CreateFSub(rhs, llvm::ConstantFP::get(rhs->getType(), 0));
        }
        else
        {
            throw std::runtime_error("Cannot apply unary -- to type");
        }
        if (!llvm::isa<llvm::LoadInst>(rhs))
        {
            throw std::runtime_error("Cannot apply unary -- to non lvalue");
        }
        context.builder.CreateStore(newValue, llvm::cast<llvm::LoadInst>(rhs)->getPointerOperand());
        return {newValue, sign};
    }
    case UnaryOperator::Ampersand:
    {
        if (!llvm::isa<llvm::LoadInst>(rhs))
        {
            throw std::runtime_error("Cannot take address of type");
        }
        return {llvm::cast<llvm::LoadInst>(rhs)->getPointerOperand(), sign};
    }
    case UnaryOperator::Asterisk:
    {
        if (auto result = std::dynamic_pointer_cast<PointerType>(sign);result)
        {
            return {context.builder.CreateLoad(rhs), result->getType().clone()};
        }
        else
        {
            throw std::runtime_error("Can't apply unary * to non pointer type");
        }
    }
    case UnaryOperator::Plus:
    {
        if (rhs->getType()->isIntegerTy() && rhs->getType()->getIntegerBitWidth() < 32)
        {
            rhs = context.builder.CreateIntCast(rhs, context.builder.getInt32Ty(), sign->isSigned());
        }
        return {rhs, sign};
    }
    case UnaryOperator::Minus:
    {
        if (rhs->getType()->isIntegerTy())
        {
            return {context.builder.CreateNeg(rhs),
                    std::make_shared<PrimitiveType>(std::static_pointer_cast<PrimitiveType>(sign)->getBitCount(),
                                                    false,
                                                    false,
                                                    true)};
        }
        else
        {
            return {context.builder.CreateFNeg(rhs),
                    std::make_shared<PrimitiveType>(std::static_pointer_cast<PrimitiveType>(sign)->getBitCount(),
                                                    false,
                                                    true,
                                                    true)};
        }
    }
    case UnaryOperator::BitNot:
    {
        if (!rhs->getType()->isIntegerTy())
        {
            throw std::runtime_error("Cannot apply ~ to non integer type");
        }
        return {context.builder.CreateNot(rhs), sign};
    }
    case UnaryOperator::LogicalNot:
    {
        if (rhs->getType()->isIntegerTy() && rhs->getType()->getIntegerBitWidth() < 1)
        {
            rhs = context.builder.CreateICmpNE(rhs, context.builder.getIntN(rhs->getType()->getIntegerBitWidth(), 0));
        }
        else if (rhs->getType()->isFloatingPointTy())
        {
            rhs = context.builder.CreateFCmpUNE(rhs, llvm::ConstantFP::get(rhs->getType(), 0));
        }
        else
        {
            throw std::runtime_error("Cannot apply ! operator to specified type");
        }
        return {context.builder.CreateZExt(context.builder.CreateNot(rhs), context.builder.getInt32Ty()), sign};
    }
    }
    return {};
}

int64_t OpenCL::Parser::UnaryExpressionUnaryOperator::solveConstantExpression() const
{
    auto value = getUnaryExpression().solveConstantExpression();
    switch (getAnOperator())
    {
    case UnaryOperator::Increment:
    case UnaryOperator::Decrement:
    case UnaryOperator::Ampersand:
    case UnaryOperator::Asterisk:return Node::solveConstantExpression();
    case UnaryOperator::Plus:return value;
    case UnaryOperator::Minus:return -value;
    case UnaryOperator::BitNot:return ~value;
    case UnaryOperator::LogicalNot:return !value;
    }
    return value;
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::UnaryExpressionSizeOf::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    return std::visit([&context](auto&& value) -> std::pair<llvm::Value*, std::shared_ptr<Type>>
                      {
                          using T = std::decay_t<decltype(value)>;
                          if constexpr(std::is_same_v<T, std::unique_ptr<OpenCL::Parser::UnaryExpression>>)
                          {
                              throw std::runtime_error("Not implemented yet");
                          }
                          else
                          {
                              auto size = context.module->getDataLayout().getTypeAllocSize(value->type(context));
                              return {context.builder.getIntN(64, size),
                                      std::make_unique<PrimitiveType>(64, false, false, false)};
                          }
                      }, getUnaryOrType());
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::CastExpression::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    return std::visit([&context](auto&& value) -> std::pair<llvm::Value*, std::shared_ptr<Type>>
                      {
                          using T = std::decay_t<decltype(value)>;
                          if constexpr(std::is_same_v<T, std::unique_ptr<UnaryExpression>>)
                          {
                              return value->codegen(context);
                          }
                          else
                          {
                              auto&[type, cast] = value;
                              auto[rhs, sign] = cast->codegen(context);
                              castPrimitive(rhs, sign->isSigned(), type->type(context), type->isSigned(), context);
                              return {rhs, type};
                          }
                      }, getUnaryOrCast());
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionFunctionCall::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto value = getPostFixExpression().codegen(context).first;
    if (!value->getType()->isFunctionTy() && !value->getType()->isPointerTy()
        && llvm::cast<llvm::PointerType>(value->getType())->getElementType()->isFunctionTy())
    {
        throw std::runtime_error("Called object is not a function or function pointer");
    }
    auto function = context.getFunction(value->getName());
    std::vector<llvm::Value*> arguments;
    std::size_t i = 0;
    for (auto& iter : getOptionalAssignmentExpressions())
    {
        auto[arg, signarg] = iter->codegen(context);
        if (!dynamic_cast<const StructType*>(function.arguments[i]))
        {
            castPrimitive(arg,
                          signarg->isSigned(),
                          function.arguments[i]->type(context),
                          function.arguments[i]->isSigned(),
                          context);
            arguments.emplace_back(arg);
        }
        else
        {
            auto* load = llvm::cast<llvm::LoadInst>(arg);
            llvm::IRBuilder<>
                tmpB(&context.currentFunction->getEntryBlock(), context.currentFunction->getEntryBlock().begin());
            auto* alloca = tmpB.CreateAlloca(load->getType());
            alloca->setAlignment(getAlignment(load->getType()));
            auto* cast = context.builder.CreateBitCast(alloca, context.builder.getInt8PtrTy());
            auto* castSource = context.builder.CreateBitCast(load->getPointerOperand(), context.builder.getInt8PtrTy());
            auto* one = context.builder.getInt32(1);
            auto* size = context.builder.CreateGEP(load->getType(),
                                                   llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(load->getType())),
                                                   one);
            context.builder.CreateMemCpy(cast,
                                         0,
                                         castSource,
                                         0,
                                         context.builder.CreatePtrToInt(size, context.builder.getInt32Ty()));
            arguments.emplace_back(alloca);
        }
        i++;
    }
    if (!dynamic_cast<const StructType*>(function.retType.get()))
    {
        return {context.builder.CreateCall(value, arguments), function.retType};
    }
    else
    {
        llvm::IRBuilder<>
            tmpB(&context.currentFunction->getEntryBlock(), context.currentFunction->getEntryBlock().begin());
        auto* alloca = tmpB.CreateAlloca(function.retType->type(context));
        alloca->setAlignment(getAlignment(function.retType->type(context)));
        arguments.insert(arguments.begin(), alloca);
        context.builder.CreateCall(value, arguments);
        return {context.builder.CreateLoad(alloca), function.retType};
    }
}

llvm::Type* OpenCL::Parser::ArrayType::type(OpenCL::Parser::Context& context) const
{
    return llvm::ArrayType::get(getType()->type(context), getSize());
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionIncrement::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, sign] = getPostFixExpression().codegen(context);
    auto* load = llvm::cast_or_null<llvm::LoadInst>(value);
    if (!load)
    {
        throw std::runtime_error("Can't increment non lvalue");
    }
    llvm::Value* newValue = nullptr;
    if (value->getType()->isIntegerTy())
    {
        newValue = context.builder.CreateAdd(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 1));
    }
    else if (value->getType()->isFloatingPointTy())
    {
        newValue = context.builder.CreateFAdd(value, llvm::ConstantFP::get(value->getType(), 1));
    }
    else if (value->getType()->isPointerTy())
    {
        newValue = context.builder.CreateInBoundsGEP(value, context.builder.getInt32(1));
    }
    else
    {
        throw std::runtime_error("Can't increment value that is not an integer or floating point type");
    }
    context.builder.CreateStore(newValue, load->getPointerOperand());
    return {value, sign};
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionDecrement::codegen(
    OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, sign] = getPostFixExpression().codegen(context);
    auto* load = llvm::cast_or_null<llvm::LoadInst>(value);
    if (!load)
    {
        throw std::runtime_error("Can't increment non lvalue");
    }
    llvm::Value* newValue = nullptr;
    if (value->getType()->isIntegerTy())
    {
        newValue = context.builder.CreateSub(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 1));
    }
    else if (value->getType()->isFloatingPointTy())
    {
        newValue = context.builder.CreateFSub(value, llvm::ConstantFP::get(value->getType(), 1));
    }
    else if (value->getType()->isPointerTy())
    {
        newValue = context.builder.CreateInBoundsGEP(value, context.builder.getInt32(-1));
    }
    else
    {
        throw std::runtime_error("Can't increment value that is not an integer or floating point type");
    }
    context.builder.CreateStore(newValue, load->getPointerOperand());
    return {value, sign};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::StructOrUnionDeclaration::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    OpenCL::Parser::Context::StructOrUnion structType;
    std::vector<llvm::Type*> types;
    auto* sp = context.debugScope.empty() ? context.debugUnit : context.debugScope.back();
    auto* llvmStruct = llvm::StructType::create(context.context, (isUnion() ? "union." : "struct.") + getName());
    std::transform(getTypes().begin(), getTypes().end(), std::back_inserter(types), [&](const auto& pair)
    {
        structType.order.insert({pair.second, structType.types.size()});
        structType.types.push_back(pair.first);
        return pair.first->type(context);
    });
    if (!isUnion())
    {
        llvmStruct->setBody(types);
    }
    else
    {
        llvm::Type
            * maxElement = *std::max_element(types.begin(), types.end(), [&context](llvm::Type* lhs, llvm::Type* rhs)
        {
            auto lhsSize = context.module->getDataLayout().getTypeAllocSize(lhs);
            auto rhsSize = context.module->getDataLayout().getTypeAllocSize(rhs);
            return lhsSize < rhsSize;
        });
        llvmStruct->setBody(maxElement);
    }
    llvm::DICompositeType* fw_decl, * res;
    cache[llvmStruct] = fw_decl = context.debugBuilder
                                         ->createReplaceableCompositeType(isUnion() ? llvm::dwarf::DW_TAG_union_type
                                                                                    : llvm::dwarf::DW_TAG_structure_type,
                                                                          getName(),
                                                                          sp,
                                                                          context.debugUnit,
                                                                          getLine());
    std::vector<llvm::Metadata*> subTypes;
    std::size_t i = 0;
    for (auto& iter : structType.types)
    {
        auto* member = toDwarfType(iter, context);
        auto* memberType = iter->type(context);
        subTypes.push_back(context.debugBuilder->createMemberType(sp,
                                                                  std::find_if(structType.order.begin(),
                                                                               structType.order.end(),
                                                                               [i](const std::pair<const std::string,
                                                                                                   std::uint64_t>& pair)
                                                                               {
                                                                                   return pair.second == i;
                                                                               })->first,
                                                                  context.debugUnit,
                                                                  getLine(),
                                                                  context.module->getDataLayout()
                                                                         .getTypeSizeInBits(memberType),
                                                                  getAlignment(memberType),
                                                                  context.module->getDataLayout()
                                                                         .getStructLayout(llvmStruct)
                                                                         ->getElementOffsetInBits(i),
                                                                  llvm::DINode::DIFlags::FlagAccessibility,
                                                                  member));
        i++;
    }
    if (!isUnion())
    {
        res = context.debugBuilder->createStructType(sp,
                                                     getName(),
                                                     context.debugUnit,
                                                     getLine(),
                                                     context.module->getDataLayout()
                                                            .getTypeAllocSizeInBits(llvmStruct),
                                                     getAlignment(llvmStruct) * 8,
                                                     llvm::DINode::DIFlags::FlagAccessibility,
                                                     nullptr,
                                                     llvm::MDTuple::get(context.context, subTypes));
    }
    else
    {
        res = context.debugBuilder->createUnionType(sp,
                                                    getName(),
                                                    context.debugUnit,
                                                    getLine(),
                                                    context.module->getDataLayout()
                                                           .getTypeAllocSizeInBits(llvmStruct),
                                                    getAlignment(llvmStruct) * 8,
                                                    llvm::DINode::DIFlags::FlagAccessibility,
                                                    llvm::MDTuple::get(context.context, subTypes));
    }
    llvm::TempMDNode fwd_decl(fw_decl);
    context.debugBuilder->replaceTemporary(std::move(fwd_decl), res);
    cache[llvmStruct] = res;
    structType.isUnion = isUnion();
    context.structs[(isUnion() ? "union." : "struct.") + getName()] = structType;
    return {};
}

llvm::Type* OpenCL::Parser::StructType::type(OpenCL::Parser::Context& context) const
{
    return context.module->getTypeByName("struct." + getName());
}

llvm::Type* OpenCL::Parser::UnionType::type(OpenCL::Parser::Context& context) const
{
    return context.module->getTypeByName("union." + getName());
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionArrow::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto* structValue = getPostFixExpression().codegen(context).first;
    auto* type = llvm::dyn_cast<llvm::StructType>(structValue->getType()->getPointerElementType());
    if (!type)
    {
        throw std::runtime_error("Can only apply -> to pointer to struct or union");
    }
    auto* zero = context.builder.getInt32(0);
    auto& structInfo = context.structs.at(type->getName());
    if (!structInfo.isUnion)
    {
        auto* index = context.builder.getInt32(structInfo.order[getIdentifier()]);
        auto memberType = structInfo.types[index->getValue().getLimitedValue()];
        auto* pointer = context.builder.CreateInBoundsGEP(structValue, {zero, index});
        return {context.builder.CreateLoad(pointer), memberType};
    }
    else
    {
        auto memberType = structInfo.types.at(structInfo.order.at(getIdentifier()));
        auto* pointer = context.builder.CreateInBoundsGEP(structValue, {zero, zero});
        auto* cast = context.builder.CreateBitCast(pointer, llvm::PointerType::getUnqual(memberType->type(context)));
        return {context.builder.CreateLoad(cast), memberType};
    }
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::SwitchStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto[value, sign] = getExpression().codegen(context);
    auto* defaultBlock = llvm::BasicBlock::Create(context.context, "default");
    auto* thenBlock = llvm::BasicBlock::Create(context.context, "then");
    context.breakBlocks.push_back(thenBlock);
    context.switchStack.emplace_back(context.builder.CreateSwitch(value, defaultBlock), sign->isSigned());
    getStatement().codegen(context);
    auto* function = context.builder.GetInsertBlock()->getParent();
    if (!std::any_of(function->getBasicBlockList().begin(),
                     function->getBasicBlockList().end(),
                     [defaultBlock](const llvm::BasicBlock& block)
                     {
                         return defaultBlock == &block;
                     }))
    {
        function->getBasicBlockList().push_back(defaultBlock);
        context.builder.SetInsertPoint(defaultBlock);
        context.builder.CreateBr(defaultBlock);
    }
    if (!defaultBlock->getTerminator())
    {
        context.builder.SetInsertPoint(defaultBlock);
        context.builder.CreateBr(thenBlock);
    }
    function->getBasicBlockList().push_back(thenBlock);
    context.builder.SetInsertPoint(thenBlock);
    context.breakBlocks.pop_back();
    context.switchStack.pop_back();
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::DefaultStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    if (context.switchStack.empty())
    {
        throw std::runtime_error("default without switch statement");
    }
    auto* function = context.builder.GetInsertBlock()->getParent();
    auto* block = context.switchStack.back().first->getDefaultDest();
    if (context.switchStack.back().first->getNumCases() > 0)
    {
        auto* successor = (context.switchStack.back().first->case_begin()
            + (context.switchStack.back().first->getNumCases() - 1))
            ->getCaseSuccessor();
        if (!successor->getTerminator())
        {
            context.builder.CreateBr(block);
        }
    }
    if (std::any_of(function->getBasicBlockList().begin(),
                    function->getBasicBlockList().end(),
                    [block](const llvm::BasicBlock& dblock)
                    {
                        return block == &dblock;
                    }))
    {
        throw std::runtime_error("There can only be a single default statement");
    }
    function->getBasicBlockList().push_back(block);
    context.builder.SetInsertPoint(block);
    getStatement().codegen(context);
    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::CaseStatement::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    if (context.switchStack.empty())
    {
        throw std::runtime_error("case without switch statement");
    }
    auto[value, sign] = getConstant().codegen(context);
    if (value->getType() != context.switchStack.back().first->getCondition()->getType())
    {
        castPrimitive(value,
                      sign->isSigned(),
                      context.switchStack.back().first->getCondition()->getType(),
                      context.switchStack.back().second,
                      context);
    }
    auto* function = context.builder.GetInsertBlock()->getParent();
    auto* newBlock = llvm::BasicBlock::Create(context.context);
    if (context.switchStack.back().first->getNumCases() > 0)
    {
        auto* successor = (context.switchStack.back().first->case_begin()
            + (context.switchStack.back().first->getNumCases() - 1))
            ->getCaseSuccessor();
        if (!successor->getTerminator())
        {
            context.builder.CreateBr(newBlock);
        }
    }
    function->getBasicBlockList().push_back(newBlock);
    auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value);
    if (!constant)
    {
        throw std::runtime_error("Expected constant expression after case");
    }
    context.switchStack.back().first->addCase(constant, newBlock);
    context.builder.SetInsertPoint(newBlock);
    if (getStatement())
    {
        getStatement()->codegen(context);
    }

    return {};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::PostFixExpressionTypeInitializer::codegen(OpenCL::Parser::Context& context) const
{
    emitLocation(this, context);
    auto* type = getType()->type(context);
    llvm::IRBuilder<>
        tmpB(&context.currentFunction->getEntryBlock(), context.currentFunction->getEntryBlock().begin());
    auto* alloca = tmpB.CreateAlloca(type);
    alloca->setAlignment(getAlignment(type));
    if (type->isStructTy())
    {
        auto* zero = context.builder.getInt32(0);
        auto& structInfo = context.structs.at(type->getStructName());
        if (getNonCommaExpressions().size() < type->getStructNumElements())
        {
            throw std::runtime_error("Amount of values in intializer not equal to fields in struct");
        }
        for (std::size_t i = 0; i < type->getStructNumElements(); i++)
        {
            auto[value, ntype] = getNonCommaExpressions().at(i)->codegen(context);
            if (ntype->type(context) != type->getStructElementType(i))
            {
                castPrimitive(value,
                              ntype->isSigned(),
                              type->getStructElementType(i),
                              structInfo.types.at(i)->isSigned(),
                              context);
            }
            auto* index = context.builder.getInt32(i);
            auto* field = context.builder.CreateInBoundsGEP(alloca, {zero, index});
            context.builder.CreateStore(value, field);
        }
    }
    else
    {
        if (getNonCommaExpressions().empty())
        {
            throw std::runtime_error("Amount of values unequal to 1");
        }
        auto[value, ntype] = getNonCommaExpressions()[0]->codegen(context);
        context.builder.CreateStore(value, alloca);
    }
    return {context.builder.CreateLoad(alloca), getType()};
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::TypedefDeclaration::codegen(OpenCL::Parser::Context& context) const
{
    if (getOptionalStructOrUnion())
    {
        return getOptionalStructOrUnion()->codegen(context);
    }
    return {};
}

std::pair<llvm::Value*, std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::InitializerListScalarExpression::codegen(
    OpenCL::Parser::Context& context) const
{
    return getExpression().codegen(context);
}

std::pair<llvm::Value*,
          std::shared_ptr<OpenCL::Parser::Type>> OpenCL::Parser::InitializerListBlock::codegen(OpenCL::Parser::Context& context) const
{
    (void)context;
    return {};
}



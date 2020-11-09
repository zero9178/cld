#include "Codegen.hpp"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#ifndef NDEBUG
    #include <llvm/IR/Verifier.h>
#endif

#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/SemanticUtil.hpp>
#include <cld/Support/Filesystem.hpp>
#include <cld/Support/ScopeExit.hpp>
#include <cld/Support/ValueReset.h>

#include <numeric>

namespace
{
class CodeGenerator final
{
    llvm::Module& m_module;
    const cld::Semantics::ProgramInterface& m_programInterface;
    const cld::SourceInterface& m_sourceInterface;
    const cld::CGLLVM::Options& m_options;
    cld::Triple m_triple;

    template <class T, class = void>
    struct hasGetAlign : std::false_type
    {
    };

    template <class T>
    struct hasGetAlign<T, std::void_t<decltype(std::declval<T>().getAlign())>> : std::true_type
    {
    };

    struct Value
    {
        llvm::Value* value;
        llvm::MaybeAlign alignment;

        template <class T>
        Value(T* value,
              std::enable_if_t<!hasGetAlign<T>{} || std::is_same_v<llvm::LoadInst, T>, llvm::MaybeAlign> alignment = {})
            : value(value), alignment(alignment)
        {
            CLD_ASSERT(!this->value || !this->value->getType()->isPointerTy()
                       || this->value->getType()->getPointerElementType()->isFunctionTy()
                       || this->alignment.hasValue());
        }

        Value(std::nullptr_t) : value(nullptr) {}

        template <class T, std::enable_if_t<hasGetAlign<T>{} && !std::is_same_v<llvm::LoadInst, T>>* = nullptr>
        Value(T* value) : Value(static_cast<llvm::Value*>(value), value->getAlign())
        {
        }

        Value(llvm::GetElementPtrInst*) = delete;

        Value(llvm::Function*) = delete;

        Value(llvm::IntToPtrInst*) = delete;

        // Remove eventually
        operator llvm::Value*() const
        {
            return value;
        }
    };

    template <class T>
    std::enable_if_t<std::is_base_of_v<llvm::Value, T>, Value> valueOf(T* value, llvm::MaybeAlign alignment = {})
    {
        if constexpr (std::is_same_v<T, llvm::Function>)
        {
            return Value(static_cast<llvm::Value*>(value), m_module.getDataLayout().getFunctionPtrAlign().getValueOr(
                                                               m_module.getDataLayout().getPointerABIAlignment(0)));
        }
        else if constexpr (hasGetAlign<T>{} && !std::is_same_v<llvm::LoadInst, T>)
        {
            return Value(value);
        }
        else
        {
            if (alignment)
            {
                CLD_ASSERT(value->getType()->isPointerTy());
                return Value(value, alignment);
            }
            if (!value->getType()->isPointerTy())
            {
                return Value(value, {});
            }
            if (value->getType()->getPointerElementType()->isFunctionTy())
            {
                return Value(value, m_module.getDataLayout().getFunctionPtrAlign().getValueOr(
                                        m_module.getDataLayout().getPointerABIAlignment(0)));
            }
            return Value(value, m_module.getDataLayout().getABITypeAlign(value->getType()->getPointerElementType()));
        }
    }

    // void* for now although strictly speaking the pointers can only be const Declaration* or const FunctionDefinition*
    // but hashing the variant seems overkill? I am not sure
    std::unordered_map<const void*, Value> m_lvalues;

    using TypeVariantKey = std::variant<cld::Semantics::StructType, cld::Semantics::UnionType>;

    std::unordered_map<TypeVariantKey, llvm::Type*> m_types;
    std::unordered_map<std::variant<cld::Semantics::StructType, cld::Semantics::UnionType, cld::Semantics::EnumType>,
                       llvm::DIType*>
        m_debugTypes;

    struct ABITransformations
    {
        enum Change
        {
            Unchanged,
            IntegerRegister,
            PointerToTemporary,
            OnStack,
            Flattened
        };
        struct MultipleArgs
        {
            std::size_t size;
        };
        using Variant = std::variant<Change, MultipleArgs>;
        Change returnType;              // Never OnStack
        std::vector<Variant> arguments; // Never Flattened
    };

    std::unordered_map<cld::Semantics::FunctionType, ABITransformations> m_functionABITransformations;
    std::unordered_map<cld::Semantics::LoopStatements, llvm::BasicBlock*> m_continueTargets;
    std::unordered_map<cld::Semantics::BreakableStatements, llvm::BasicBlock*> m_breakTargets;
    std::unordered_map<const cld::Semantics::LabelStatement*, llvm::BasicBlock*> m_labels;
    struct Switch
    {
        llvm::SwitchInst* llvmSwitch;
        llvm::BasicBlock* defaultBlock;
    };
    std::unordered_map<const cld::Semantics::SwitchStatement*, Switch> m_switches;

    llvm::IRBuilder<> m_builder{m_module.getContext()};
    llvm::Function* m_currentFunction = nullptr;
    const ABITransformations* m_currentFunctionABI = nullptr;
    llvm::AllocaInst* m_returnSlot = nullptr;
    std::unordered_map<std::shared_ptr<const cld::Semantics::ExpressionBase>, llvm::Value*> m_valSizes;
    std::unordered_map<const cld::Semantics::Declaration * CLD_NON_NULL, llvm::AllocaInst*> m_stackSaves;
    std::unordered_map<std::string_view, llvm::GlobalVariable*> m_cGlobalVariables;

    llvm::Value* toBool(llvm::Value* value)
    {
        if (value->getType()->isIntegerTy())
        {
            if (auto* cast = llvm::dyn_cast<llvm::CastInst>(value);
                cast && cast->getSrcTy() == m_builder.getInt1Ty() && cast->getNumUses() == 0)
            {
                auto* result = cast->getOperand(0);
                cast->eraseFromParent();
                return result;
            }
            return m_builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0));
        }
        if (value->getType()->isPointerTy())
        {
            return m_builder.CreateICmpNE(
                value, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(value->getType())));
        }

        return m_builder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
    }

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

    std::tuple<ABITransformations::Variant, llvm::Type * CLD_NON_NULL, llvm::Type * CLD_NULLABLE>
        flattenSingleArg(llvm::Type* type, std::uint8_t* takenIntegers = nullptr, std::uint8_t* takenFloats = nullptr)
    {
        constexpr std::uint8_t availableIntegerRegisters = 6;
        constexpr std::uint8_t availableFloatingPointRegisters = 8;
        ABITransformations::Variant dest;
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
                const auto alignment = m_module.getDataLayout().getABITypeAlign(*iter).value();
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
                // If we have a single fp80 then it is passed normally if it wasn't part of the struct but passed
                // on the stack if it was in the struct. Very weird I know. Only need to handle this case of a
                // single fp80 as it is 128 bytes on x64 (due to padding) and we wouldn't be here if the struct
                // was any larger
                if ((*iter)->isX86_FP80Ty())
                {
                    if (*iter != type)
                    {
                        return {ABITransformations::OnStack, type, nullptr};
                    }
                }
                currentAlignment = std::max(currentAlignment, alignment);
                const auto typeSize = m_module.getDataLayout().getTypeAllocSize(*iter).getKnownMinSize();
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
                    return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
                            nullptr};
                }

                takenIntegerRegisters++;
                if (type->isStructTy() && !std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                {
                    dest = ABITransformations::MultipleArgs{};
                }
                if (type->isStructTy())
                {
                    ret[retIndex++] = m_builder.getIntNTy(size * 8);
                }
                else
                {
                    ret[retIndex++] = type;
                }
                if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                {
                    cld::get<ABITransformations::MultipleArgs>(dest).size++;
                }

                continue;
            }
            if (std::distance(begin, iter) == 2 && (*begin)->isFloatTy() && ((*(begin + 1))->isFloatTy()))
            {
                // Two floats can be packed as a single 64 bit value into a xmm register. This is represented as
                // a vector in LLVM IR
                if (takenFloatingPointRegisters >= availableFloatingPointRegisters)
                {
                    return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
                            nullptr};
                }
                takenFloatingPointRegisters++;
                if (!std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                {
                    dest = ABITransformations::MultipleArgs{};
                }
                ret[retIndex++] = llvm::FixedVectorType::get(m_builder.getFloatTy(), 2);
                cld::get<ABITransformations::MultipleArgs>(dest).size++;
                continue;
            }

            CLD_ASSERT(std::distance(begin, iter) == 1);
            // Must be a floating point type because if it were integer it would have taken the
            // encounteredInteger branch above
            if (takenFloatingPointRegisters >= availableFloatingPointRegisters)
            {
                return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
                        nullptr};
            }
            takenFloatingPointRegisters++;
            if (type->isStructTy() && !std::holds_alternative<ABITransformations::MultipleArgs>(dest))
            {
                dest = ABITransformations::MultipleArgs{};
            }
            ret[retIndex++] = *begin;
            if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
            {
                cld::get<ABITransformations::MultipleArgs>(dest).size++;
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

    ABITransformations applyPlatformABI(llvm::Type*& returnType, std::vector<llvm::Type*>& arguments)
    {
        ABITransformations transformations;
        transformations.returnType = ABITransformations::Unchanged;
        transformations.arguments.resize(arguments.size(), ABITransformations::Unchanged);
        if (m_triple.getPlatform() == cld::Platform::Windows && m_triple.getArchitecture() == cld::Architecture::x86_64)
        {
            for (auto iter = arguments.begin(); iter != arguments.end(); iter++)
            {
                if (!(*iter)->isStructTy() && !(*iter)->isX86_FP80Ty())
                {
                    continue;
                }
                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(*iter);
                if (m_module.getDataLayout().isLegalInteger(size))
                {
                    transformations.arguments[iter - arguments.begin()] = ABITransformations::IntegerRegister;
                    *iter = m_builder.getIntNTy(size);
                }
                else
                {
                    transformations.arguments[iter - arguments.begin()] = ABITransformations::PointerToTemporary;
                    *iter = llvm::PointerType::getUnqual(*iter);
                }
            }
            if (returnType->isVoidTy())
            {
                return transformations;
            }
            std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
            if (m_module.getDataLayout().isLegalInteger(size) && returnType->isStructTy())
            {
                transformations.returnType = ABITransformations::IntegerRegister;
                returnType = m_builder.getIntNTy(size);
            }
            else if (!m_module.getDataLayout().isLegalInteger(size))
            {
                transformations.returnType = ABITransformations::PointerToTemporary;
                arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
                returnType = m_builder.getVoidTy();
            }
        }
        else if (m_triple.getArchitecture() == cld::Architecture::x86_64)
        {
            std::uint8_t takenIntegerRegisters = 0;
            std::uint8_t takenFloatingPointRegisters = 0;
            std::size_t transFormIndex = 0;
            for (auto arg = arguments.begin(); arg != arguments.end(); transFormIndex++, arg++)
            {
                auto& dest = transformations.arguments[transFormIndex];
                if (m_module.getDataLayout().getTypeAllocSizeInBits(*arg) > 128)
                {
                    dest = ABITransformations::OnStack;
                    *arg = llvm::PointerType::getUnqual(*arg);
                    continue;
                }
                std::pair<llvm::Type*, llvm::Type*> types;
                std::tie(dest, types.first, types.second) =
                    flattenSingleArg(*arg, &takenIntegerRegisters, &takenFloatingPointRegisters);
                if (std::holds_alternative<ABITransformations::Change>(dest)
                    && cld::get<ABITransformations::Change>(dest) == ABITransformations::OnStack)
                {
                    *arg = llvm::PointerType::getUnqual(*arg);
                }
                else if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                {
                    *arg = types.first;
                    if (types.second)
                    {
                        arg++;
                        arg = arguments.insert(arg, types.second);
                    }
                }
            }
            takenIntegerRegisters = 0;
            takenFloatingPointRegisters = 0;
            if (!returnType->isVoidTy())
            {
                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
                if (size > 128)
                {
                    transformations.returnType = ABITransformations::PointerToTemporary;
                    arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
                    returnType = m_builder.getVoidTy();
                }
                else
                {
                    auto* prevReturnType = returnType;
                    auto [temp, firstType, secondType] =
                        flattenSingleArg(returnType, &takenIntegerRegisters, &takenFloatingPointRegisters);
                    if (secondType)
                    {
                        returnType = llvm::StructType::get(firstType, secondType);
                    }
                    else
                    {
                        returnType = firstType;
                    }
                    if (prevReturnType->isStructTy())
                    {
                        transformations.returnType = ABITransformations::Flattened;
                    }
                }
            }
        }
        return transformations;
    }

    template <class T>
    void applyFunctionAttributes(T& attributeApply, llvm::FunctionType* CLD_NON_NULL functionType,
                                 const cld::Semantics::FunctionType& ft,
                                 const std::vector<std::unique_ptr<cld::Semantics::Declaration>>* paramDecls = nullptr)
    {
        auto transformations = m_functionABITransformations.find(ft);
        CLD_ASSERT(transformations != m_functionABITransformations.end());
        std::size_t argStart = 0;
        if (transformations->second.returnType == ABITransformations::PointerToTemporary)
        {
            attributeApply.addAttribute(1, llvm::Attribute::StructRet);
            attributeApply.addAttribute(1, llvm::Attribute::NoAlias);
            argStart = 1;
        }
        else if (transformations->second.returnType == ABITransformations::Unchanged
                 && cld::Semantics::isInteger(ft.getReturnType())
                 && cld::get<cld::Semantics::PrimitiveType>(ft.getReturnType().getVariant()).getBitCount() < 32)
        {
            if (cld::get<cld::Semantics::PrimitiveType>(ft.getReturnType().getVariant()).isSigned())
            {
                attributeApply.addAttribute(0, llvm::Attribute::SExt);
            }
            else
            {
                attributeApply.addAttribute(0, llvm::Attribute::ZExt);
            }
        }
        std::size_t origArgI = 0;
        for (std::size_t i = argStart; i < functionType->getNumParams(); origArgI++)
        {
            auto& argument = transformations->second.arguments[origArgI];
            if (std::holds_alternative<ABITransformations::Change>(argument))
            {
                auto change = cld::get<ABITransformations::Change>(argument);
                if (change == ABITransformations::Unchanged)
                {
                    auto& arg = ft.getArguments()[origArgI].first;
                    if (cld::Semantics::isInteger(arg)
                        && cld::get<cld::Semantics::PrimitiveType>(arg.getVariant()).getBitCount() < 32)
                    {
                        if (cld::get<cld::Semantics::PrimitiveType>(arg.getVariant()).isSigned())
                        {
                            attributeApply.addParamAttr(i, llvm::Attribute::SExt);
                        }
                        else
                        {
                            attributeApply.addParamAttr(i, llvm::Attribute::ZExt);
                        }
                    }
                }
                else if (change == ABITransformations::OnStack)
                {
                    auto& arg = ft.getArguments()[origArgI].first;
                    attributeApply.addParamAttr(
                        i, llvm::Attribute::getWithByValType(m_builder.getContext(),
                                                             functionType->getParamType(i)->getPointerElementType()));
                    attributeApply.addParamAttr(
                        i, llvm::Attribute::getWithAlignment(
                               m_builder.getContext(), std::max(llvm::Align(arg.getAlignOf(m_programInterface)),
                                                                m_module.getDataLayout().getPointerABIAlignment(0))));
                    i++;
                    continue;
                }
                if (change == ABITransformations::PointerToTemporary || !paramDecls)
                {
                    i++;
                    continue;
                }
                auto& paramDecl = (*paramDecls)[origArgI];
                auto* operand = functionType->getParamType(i);
                i++;
                if (change == ABITransformations::Unchanged)
                {
                    auto* var = createAllocaAtTop(operand, paramDecl->getNameToken()->getText());
                    var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                    m_lvalues.emplace(paramDecl.get(), var);
                    continue;
                }
                auto* var = createAllocaAtTop(visit(paramDecl->getType()), paramDecl->getNameToken()->getText());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                m_lvalues.emplace(paramDecl.get(), var);
            }
            else if (std::holds_alternative<ABITransformations::MultipleArgs>(argument))
            {
                auto& multiArgs = cld::get<ABITransformations::MultipleArgs>(argument);
                i += multiArgs.size;
                if (!paramDecls)
                {
                    continue;
                }
                auto& paramDecl = (*paramDecls)[origArgI];
                auto* var = createAllocaAtTop(visit(paramDecl->getType()), paramDecl->getNameToken()->getText());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                m_lvalues.emplace(paramDecl.get(), var);
            }
        }
    }

    Value add(Value lhs, const cld::Semantics::Type& lhsType, Value rhs, const cld::Semantics::Type& rhsType)
    {
        if (cld::Semantics::isArithmetic(lhsType) && cld::Semantics::isArithmetic(rhsType))
        {
            if (cld::Semantics::isInteger(lhsType))
            {
                if (cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
                {
                    return m_builder.CreateNSWAdd(lhs, rhs);
                }

                return m_builder.CreateAdd(lhs, rhs);
            }

            return m_builder.CreateFAdd(lhs, rhs);
        }

        auto pointer = lhs.value->getType()->isPointerTy() ? lhs : rhs;
        auto integer = pointer.value == lhs.value ? rhs : lhs;
        auto& pointerType = pointer.value == lhs.value ? lhsType : rhsType;
        integer = m_builder.CreateIntCast(
            integer.value, m_builder.getInt64Ty(),
            cld::get<cld::Semantics::PrimitiveType>((pointer.value == lhs.value ? rhsType : lhsType).getVariant())
                .isSigned());
        if (!cld::Semantics::isVariableLengthArray(
                cld::get<cld::Semantics::PointerType>(pointerType.getVariant()).getElementType()))
        {
            return createGEP(pointer, integer.value);
        }
        auto& array = cld::get<cld::Semantics::PointerType>(pointerType.getVariant()).getElementType();
        Value product = integer;
        for (auto& iter : cld::Semantics::RecursiveVisitor(array, cld::Semantics::ARRAY_TYPE_NEXT_FN))
        {
            llvm::Value* value;
            if (std::holds_alternative<cld::Semantics::ArrayType>(iter.getVariant()))
            {
                value = m_builder.getInt64(cld::get<cld::Semantics::ArrayType>(iter.getVariant()).getSize());
            }
            else
            {
                value = m_valSizes[cld::get<cld::Semantics::ValArrayType>(iter.getVariant()).getExpression()];
            }
            product = m_builder.CreateMul(product.value, value);
        }
        return createGEP(pointer, product.value);
    }

    Value sub(Value lhs, const cld::Semantics::Type& lhsType, Value rhs, const cld::Semantics::Type& rhsType)
    {
        if (cld::Semantics::isArithmetic(lhsType) && cld::Semantics::isArithmetic(rhsType))
        {
            if (cld::Semantics::isInteger(lhsType))
            {
                if (cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
                {
                    return m_builder.CreateNSWSub(lhs, rhs);
                }

                return m_builder.CreateSub(lhs, rhs);
            }

            return m_builder.CreateFSub(lhs, rhs);
        }

        CLD_ASSERT(lhs.value->getType()->isPointerTy());
        if (rhs.value->getType()->isIntegerTy())
        {
            rhs = valueOf(m_builder.CreateNeg(rhs.value));
            rhs = valueOf(
                m_builder.CreateIntCast(rhs.value, m_builder.getInt64Ty(),
                                        cld::get<cld::Semantics::PrimitiveType>(rhsType.getVariant()).isSigned()));
            return createGEP(lhs, rhs.value);
        }

        return m_builder.CreatePtrDiff(lhs, rhs);
    }

    Value mul(Value lhs, const cld::Semantics::Type& lhsType, Value rhs, const cld::Semantics::Type&)
    {
        if (cld::Semantics::isInteger(lhsType))
        {
            if (cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
            {
                return m_builder.CreateNSWMul(lhs, rhs);
            }

            return m_builder.CreateMul(lhs, rhs);
        }

        return m_builder.CreateFMul(lhs, rhs);
    }

    Value div(Value lhs, const cld::Semantics::Type& lhsType, Value rhs, const cld::Semantics::Type&)
    {
        if (cld::Semantics::isInteger(lhsType))
        {
            if (cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
            {
                return m_builder.CreateSDiv(lhs, rhs);
            }

            return m_builder.CreateUDiv(lhs, rhs);
        }

        return m_builder.CreateFDiv(lhs, rhs);
    }

    Value mod(Value lhs, const cld::Semantics::Type& lhsType, Value rhs, const cld::Semantics::Type&)
    {
        if (cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
        {
            return m_builder.CreateSRem(lhs, rhs);
        }

        return m_builder.CreateURem(lhs, rhs);
    }

    Value shl(Value lhs, const cld::Semantics::Type&, Value rhs, const cld::Semantics::Type& rhsType)
    {
        if (lhs.value->getType() != rhs.value->getType())
        {
            rhs = m_builder.CreateIntCast(rhs, lhs.value->getType(),
                                          cld::get<cld::Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
        }
        return m_builder.CreateShl(lhs, rhs);
    }

    Value shr(Value lhs, const cld::Semantics::Type& lhsType, Value rhs, const cld::Semantics::Type& rhsType)
    {
        if (lhs.value->getType() != rhs.value->getType())
        {
            rhs = m_builder.CreateIntCast(rhs, lhs.value->getType(),
                                          cld::get<cld::Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
        }
        if (!cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
        {
            return m_builder.CreateLShr(lhs, rhs);
        }
        return m_builder.CreateAShr(lhs, rhs);
    }

    Value cast(Value value, const cld::Semantics::Type& from, const cld::Semantics::Type& to)
    {
        if (std::holds_alternative<cld::Semantics::PointerType>(to.getVariant()))
        {
            if (cld::Semantics::isInteger(from))
            {
                return valueOf(m_builder.CreateIntToPtr(value, visit(to)));
            }
            // User requested and apparently does not care about alignment if this were to go wrong
            return createPointerCast(value, visit(to));
        }
        if (cld::Semantics::isBool(to))
        {
            return m_builder.CreateIntCast(toBool(value), visit(to), false);
        }
        if (cld::Semantics::isInteger(to) && cld::Semantics::isInteger(from))
        {
            return m_builder.CreateIntCast(value, visit(to),
                                           cld::get<cld::Semantics::PrimitiveType>(from.getVariant()).isSigned());
        }
        if (cld::Semantics::isArithmetic(to) && cld::Semantics::isInteger(from))
        {
            if (cld::get<cld::Semantics::PrimitiveType>(from.getVariant()).isSigned())
            {
                return m_builder.CreateSIToFP(value, visit(to));
            }

            return m_builder.CreateUIToFP(value, visit(to));
        }
        if (cld::Semantics::isInteger(to))
        {
            if (std::holds_alternative<cld::Semantics::PointerType>(from.getVariant()))
            {
                return m_builder.CreatePtrToInt(value, visit(to));
            }
            if (cld::get<cld::Semantics::PrimitiveType>(to.getVariant()).isSigned())
            {
                return m_builder.CreateFPToSI(value, visit(to));
            }

            return m_builder.CreateFPToUI(value, visit(to));
        }
        return m_builder.CreateFPCast(value, visit(to));
    }

    template <class T>
    Value createLoad(T*, std::enable_if_t<!hasGetAlign<T>{}, bool>) = delete;

    Value createLoad(Value ptr, bool isVolatile)
    {
        return valueOf(m_builder.CreateAlignedLoad(ptr.value, ptr.alignment, isVolatile));
    }

    template <class T>
    void createStore(T*, std::enable_if_t<!hasGetAlign<T>{}, bool>) = delete;

    void createStore(llvm::Value* value, Value ptr, bool isVolatile)
    {
        m_builder.CreateAlignedStore(value, ptr.value, ptr.alignment, isVolatile);
    }

    llvm::AllocaInst* createAllocaAtTop(llvm::Type* type, std::string_view name = {})
    {
        llvm::IRBuilderBase::InsertPointGuard guard(m_builder);
        m_builder.SetInsertPoint(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
        return m_builder.CreateAlloca(type, nullptr, llvm::StringRef{name});
    }

    llvm::Align calcAlign(llvm::Value* gep, llvm::Align alignment)
    {
        bool deleteInst = false;
        cld::ScopeExit exit([&] {
            if (deleteInst)
            {
                gep->deleteValue();
            }
        });
        auto* constExpr = llvm::dyn_cast<llvm::ConstantExpr>(gep);
        if (constExpr)
        {
            deleteInst = true;
            gep = constExpr->getAsInstruction();
        }
        auto* gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(gep);
        if (!gepInst)
        {
            return m_module.getDataLayout().getABITypeAlign(gep->getType()->getPointerElementType());
        }
        llvm::APInt integer(m_module.getDataLayout().getPointerSizeInBits(0), 0);
        if (gepInst->accumulateConstantOffset(m_module.getDataLayout(), integer))
        {
            for (std::uint64_t value = alignment.value(); value > 0; value >>= 1)
            {
                if (integer.getZExtValue() % value == 0)
                {
                    return llvm::Align(value);
                }
            }
            CLD_UNREACHABLE;
        }
        return m_module.getDataLayout().getABITypeAlign(gep->getType()->getPointerElementType());
    }

    Value createGEP(Value ptr, llvm::ArrayRef<llvm::Value*> indices)
    {
        auto* result = m_builder.CreateGEP(ptr.value, indices);
        return valueOf(result, calcAlign(result, *ptr.alignment));
    }

    Value createInBoundsGEP(Value ptr, llvm::ArrayRef<llvm::Value*> indices)
    {
        auto* result = m_builder.CreateInBoundsGEP(ptr.value, indices);
        return valueOf(result, calcAlign(result, *ptr.alignment));
    }

    Value createPointerCast(Value ptr, llvm::Type* pointerType)
    {
        return valueOf(m_builder.CreateBitCast(ptr, pointerType));
    }

    Value createBitCast(Value ptr, llvm::Type* pointerType, bool checked = true)
    {
        CLD_ASSERT(!checked
                   || (ptr.alignment && pointerType->isPointerTy()
                       && (pointerType->getPointerElementType()->isFunctionTy()
                           || *ptr.alignment
                                  >= m_module.getDataLayout().getABITypeAlign(pointerType->getPointerElementType()))));
        return valueOf(m_builder.CreateBitCast(ptr, pointerType), ptr.alignment);
    }

    Value createSafeBitCast(Value ptr, llvm::Type* pointerType)
    {
        CLD_ASSERT(ptr.alignment);
        if (*ptr.alignment >= m_module.getDataLayout().getABITypeAlign(pointerType->getPointerElementType()))
        {
            // Only if the destination element type is allowed to have the alignment of the source pointer is this cast
            // valid
            return valueOf(m_builder.CreateBitCast(ptr, pointerType), ptr.alignment);
        }

        auto* alloca = createAllocaAtTop(pointerType->getPointerElementType());
        alloca->setAlignment(m_module.getDataLayout().getABITypeAlign(pointerType->getPointerElementType()));
        m_builder.CreateLifetimeStart(alloca, m_builder.getInt64(m_module.getDataLayout().getTypeAllocSize(
                                                  pointerType->getPointerElementType())));
        m_builder.CreateMemCpy(alloca, alloca->getAlign(), ptr, *ptr.alignment,
                               m_module.getDataLayout().getTypeAllocSize(pointerType->getPointerElementType()));
        return alloca;
    }

    Value getStringLiteralData(llvm::Type* elementType, const cld::Semantics::Constant::Variant& value)
    {
        if (std::holds_alternative<std::string>(value))
        {
            return Value(llvm::ConstantDataArray::getString(m_module.getContext(), cld::get<std::string>(value)),
                         llvm::Align(1));
        }

        auto& str = cld::get<cld::Lexer::NonCharString>(value);
        std::uint8_t size = 0;
        switch (m_sourceInterface.getLanguageOptions().wcharUnderlyingType)
        {
            case cld::LanguageOptions::WideCharType ::UnsignedShort:
                size = m_sourceInterface.getLanguageOptions().sizeOfShort;
                break;
            case cld::LanguageOptions::WideCharType ::Int:
                size = m_sourceInterface.getLanguageOptions().sizeOfInt;
                break;
        }
        switch (size)
        {
            case 2:
            {
                std::vector<std::uint16_t> convertedData(str.characters.size());
                std::transform(str.characters.begin(), str.characters.end(), convertedData.begin(),
                               [](std::uint32_t value) -> std::uint16_t { return value; });
                std::vector<char> rawData(convertedData.size() * 2);
                std::memcpy(rawData.data(), convertedData.data(), rawData.size());
                return Value(llvm::ConstantDataArray::getRaw(llvm::StringRef(rawData.data(), rawData.size()),
                                                             convertedData.size(), elementType),
                             llvm::Align(2));
            }
            case 4:
            {
                std::vector<char> rawData(str.characters.size() * 4);
                std::memcpy(rawData.data(), str.characters.data(), rawData.size());
                return Value(llvm::ConstantDataArray::getRaw(llvm::StringRef(rawData.data(), rawData.size()),
                                                             str.characters.size(), elementType),
                             llvm::Align(4));
            }
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visitStaticInitializerList(const cld::Semantics::InitializerList& initializerList,
                                            const cld::Semantics::Type& type, llvm::Type* llvmType)
    {
        struct Aggregate
        {
            std::vector<std::variant<llvm::Constant*, Aggregate>> vector;
            std::optional<std::uint32_t> unionIndex;
        };
        Aggregate constants;
        auto genAggregate =
            cld::YComb{[&](auto&& self, const cld::Semantics::Type& type, Aggregate& aggregate) -> void {
                if (cld::Semantics::isStruct(type))
                {
                    auto fields = m_programInterface.getFieldLayout(type);
                    aggregate.vector.resize(fields.size());
                    for (const auto* iter = fields.begin(); iter != fields.end(); iter++)
                    {
                        if (!cld::Semantics::isAggregate(*iter->type))
                        {
                            continue;
                        }
                        auto& vector = aggregate.vector[iter - fields.begin()].emplace<Aggregate>();
                        self(*iter->type, vector);
                    }
                }
                else if (cld::Semantics::isArray(type))
                {
                    auto& array = cld::get<cld::Semantics::ArrayType>(type.getVariant());
                    std::variant<llvm::Constant*, Aggregate> value;
                    if (cld::Semantics::isAggregate(array.getType()))
                    {
                        auto& vector = value.emplace<Aggregate>();
                        self(array.getType(), vector);
                    }
                    aggregate.vector.resize(array.getSize(), value);
                }
            }};
        genAggregate(type, constants);

        for (auto& [path, expression] : initializerList.getFields())
        {
            auto replacement = visit(*expression);
            llvm::Constant** value = [&, &path = path, &expression = expression]() -> llvm::Constant** {
                Aggregate* current = &constants;
                const cld::Semantics::Type* currentType = &type;
                for (auto& iter : llvm::ArrayRef(path).drop_back())
                {
                    if (cld::Semantics::isUnion(*currentType))
                    {
                        auto fields = m_programInterface.getFieldLayout(*currentType);
                        if (current->vector.empty() || current->unionIndex != iter)
                        {
                            current->vector.resize(1);
                            current->vector[0] = {};
                            current->unionIndex = iter;
                            if (cld::Semantics::isAggregate(*fields[iter].type))
                            {
                                auto& vector = current->vector.back().emplace<Aggregate>();
                                genAggregate(*fields[iter].type, vector);
                            }
                        }
                        currentType = fields[iter].type.get();
                        current = &cld::get<Aggregate>(current->vector[0]);
                        continue;
                    }
                    if (cld::Semantics::isStruct(*currentType))
                    {
                        currentType = m_programInterface.getFieldLayout(*currentType)[iter].type.get();
                    }
                    else if (cld::Semantics::isArray(*currentType))
                    {
                        currentType = &cld::Semantics::getArrayElementType(*currentType);
                    }
                    current = &cld::get<Aggregate>(current->vector[iter]);
                }
                if (cld::Semantics::isStringLiteralExpr(*expression))
                {
                    auto& constant = expression->cast<cld::Semantics::Constant>();
                    auto& aggregate = cld::get<Aggregate>(current->vector[path.back()]);
                    auto size = std::min(aggregate.vector.size(),
                                         cld::match(
                                             constant.getValue(),
                                             [](const std::string& str) -> std::size_t { return str.size() + 1; },
                                             [](const cld::Lexer::NonCharString& nonCharString) -> std::size_t {
                                                 return nonCharString.characters.size();
                                             },
                                             [](const auto&) -> std::size_t { CLD_UNREACHABLE; }));
                    auto* elementType = cld::match(
                        constant.getValue(),
                        [&](const std::string&) -> llvm::Type* {
                            return visit(cld::Semantics::PrimitiveType::createChar(
                                false, false, m_programInterface.getLanguageOptions()));
                        },
                        [&](const cld::Lexer::NonCharString&) -> llvm::Type* {
                            return visit(cld::Semantics::PrimitiveType::createWcharT(
                                false, false, m_programInterface.getLanguageOptions()));
                        },
                        [](const auto&) -> llvm::Type* { CLD_UNREACHABLE; });
                    for (std::size_t i = 0; i < size; i++)
                    {
                        auto* constantValue = cld::match(
                            constant.getValue(),
                            [&](const std::string& str) -> llvm::Constant* {
                                if (i == str.size())
                                {
                                    return llvm::ConstantInt::get(elementType, 0);
                                }
                                return llvm::ConstantInt::get(elementType, str[i], true);
                            },
                            [&](const cld::Lexer::NonCharString& str) -> llvm::Constant* {
                                return llvm::ConstantInt::get(elementType, str.characters[i], false);
                            },
                            [](const auto&) -> llvm::Constant* { CLD_UNREACHABLE; });
                        aggregate.vector[i] = constantValue;
                    }
                    return nullptr;
                }
                if (cld::Semantics::isUnion(*currentType))
                {
                    auto fields = m_programInterface.getFieldLayout(*currentType);
                    if (current->vector.empty() || current->unionIndex != path.back())
                    {
                        current->vector.resize(1);
                        current->vector[0] = {};
                        current->unionIndex = path.back();
                        if (cld::Semantics::isAggregate(*fields[path.back()].type))
                        {
                            auto& vector = current->vector.back().emplace<Aggregate>();
                            genAggregate(*fields[path.back()].type, vector);
                        }
                    }
                    return &cld::get<llvm::Constant*>(current->vector[0]);
                }
                return &cld::get<llvm::Constant*>(current->vector[path.back()]);
            }();
            if (value)
            {
                *value = llvm::cast<llvm::Constant>(replacement.value);
            }
        }

        return cld::YComb{[&](auto&& self, const cld::Semantics::Type& type, llvm::Type* llvmType,
                              const Aggregate& aggregate) -> llvm::Constant* {
            if (cld::Semantics::isStruct(type))
            {
                std::vector<llvm::Constant*> elements;
                std::vector<llvm::Type*> elementTypes;
                auto fields = m_programInterface.getFieldLayout(type);
                for (std::size_t i = 0; i < aggregate.vector.size();)
                {
                    if (!fields[i].bitFieldBounds)
                    {
                        elements.push_back(cld::match(
                            aggregate.vector[i],
                            [&](const Aggregate& subAggregate) -> llvm::Constant* {
                                return self(*fields[i].type, llvmType->getStructElementType(fields[i].layoutIndex),
                                            subAggregate);
                            },
                            [&](llvm::Constant* constant) {
                                if (constant)
                                {
                                    return constant;
                                }
                                return llvm::Constant::getNullValue(
                                    llvmType->getStructElementType(fields[i].layoutIndex));
                            }));
                        elementTypes.push_back(elements.back()->getType());
                        i++;
                        continue;
                    }
                    elements.push_back(
                        llvm::Constant::getNullValue(llvmType->getStructElementType(fields[i].layoutIndex)));
                    elementTypes.push_back(elements.back()->getType());
                    for (; i < aggregate.vector.size() && fields[i].bitFieldBounds; i++)
                    {
                        auto* value = cld::match(
                            aggregate.vector[i],
                            [&](const Aggregate& subAggregate) -> llvm::Constant* {
                                return self(*fields[i].type, llvmType->getStructElementType(fields[i].layoutIndex),
                                            subAggregate);
                            },
                            [&](llvm::Constant* constant) -> llvm::Constant* {
                                if (constant)
                                {
                                    return constant;
                                }
                                return llvm::Constant::getNullValue(
                                    llvmType->getStructElementType(fields[i].layoutIndex));
                            });
                        auto size = fields[i].bitFieldBounds->second - fields[i].bitFieldBounds->first;
                        auto* mask = llvm::ConstantInt::get(value->getType(), (1u << size) - 1);
                        value = llvm::ConstantExpr::getAnd(value, mask);
                        value = llvm::ConstantExpr::getShl(
                            value, llvm::ConstantInt::get(value->getType(), fields[i].bitFieldBounds->first));
                        mask = llvm::ConstantExpr::getShl(
                            mask, llvm::ConstantInt::get(mask->getType(), fields[i].bitFieldBounds->first));
                        mask = llvm::ConstantExpr::getNot(mask);
                        elements.back() = llvm::ConstantExpr::getAnd(elements.back(), mask);
                        elements.back() = llvm::ConstantExpr::getOr(elements.back(), value);
                    }
                }
                return llvm::ConstantStruct::get(llvm::StructType::get(m_module.getContext(), elementTypes), elements);
            }
            if (cld::Semantics::isArray(type))
            {
                bool isSame = true;
                llvm::Type* elementType = nullptr;
                std::vector<llvm::Type*> elementTypes;
                std::vector<llvm::Constant*> elements;
                for (std::size_t i = 0; i < aggregate.vector.size(); i++)
                {
                    elements.push_back(cld::match(
                        aggregate.vector[i],
                        [&](llvm::Constant* constant) {
                            if (constant)
                            {
                                return constant;
                            }
                            return llvm::Constant::getNullValue(llvmType->getArrayElementType());
                        },
                        [&](const Aggregate& subAggregate) -> llvm::Constant* {
                            return self(cld::get<cld::Semantics::ArrayType>(type.getVariant()).getType(),
                                        llvmType->getArrayElementType(), subAggregate);
                        }));
                    elementTypes.push_back(elements.back()->getType());
                    if (elementType == nullptr)
                    {
                        elementType = elementTypes.back();
                    }
                    else
                    {
                        isSame = isSame && elementType == elementTypes.back();
                    }
                }
                if (isSame)
                {
                    return llvm::ConstantArray::get(llvm::ArrayType::get(elementType, elements.size()), elements);
                }
                return llvm::ConstantStruct::get(llvm::StructType::get(m_module.getContext(), elementTypes), elements);
            }
            // Union
            auto fields = m_programInterface.getFieldLayout(type);
            auto* llvmSubType = visit(*fields[*aggregate.unionIndex].type);
            llvm::Constant* element = cld::match(
                aggregate.vector[0], [](llvm::Constant* constant) -> llvm::Constant* { return constant; },
                [&](const Aggregate& subAggregate) -> llvm::Constant* {
                    return self(*fields[*aggregate.unionIndex].type, llvmSubType, subAggregate);
                });
            auto paddingSize = m_module.getDataLayout().getTypeAllocSize(llvmType).getKnownMinSize()
                               - m_module.getDataLayout().getTypeAllocSize(element->getType()).getKnownMinSize();
            if (paddingSize == 0)
            {
                return llvm::ConstantStruct::get(llvm::StructType::get(element->getType()), element);
            }
            auto* padding = llvm::ArrayType::get(m_builder.getInt8Ty(), paddingSize);
            auto* newType = llvm::StructType::get(element->getType(), padding);

            return llvm::ConstantStruct::get(newType, {element, llvm::UndefValue::get(padding)});
        }}(type, llvmType, constants);
    }

    void runDestructors(std::size_t from, std::size_t toExclusive)
    {
        while (from > 0 && from != toExclusive)
        {
            // Destructors must be run backwards in order of declaration
            for (auto iter = m_programInterface.getScopes()[from].declarations.rbegin();
                 iter != m_programInterface.getScopes()[from].declarations.rend(); iter++)
            {
                if (!std::holds_alternative<cld::Semantics::Declaration*>(iter->second.declared))
                {
                    continue;
                }
                const auto* decl = cld::get<cld::Semantics::Declaration*>(iter->second.declared);
                if (cld::Semantics::isVariableLengthArray(decl->getType()))
                {
                    auto* alloca = m_stackSaves[decl];
                    if (alloca)
                    {
                        auto loaded = createLoad(alloca, false);
                        m_builder.CreateIntrinsic(llvm::Intrinsic::stackrestore, {}, {loaded});
                    }
                    continue;
                }
            }
            from = m_programInterface.getScopes()[from].previousScope;
        }
    }

    void runDestructors(std::size_t scope)
    {
        runDestructors(scope, m_programInterface.getScopes()[scope].previousScope);
    }

    static bool isBuiltinFunctionCall(const cld::Semantics::ExpressionBase& expression)
    {
        if (!expression.is<cld::Semantics::Conversion>())
        {
            return false;
        }
        auto& conversion = expression.cast<cld::Semantics::Conversion>();
        if (conversion.getKind() != cld::Semantics::Conversion::LValue)
        {
            return false;
        }
        if (!conversion.getExpression().is<cld::Semantics::DeclarationRead>())
        {
            return false;
        }
        auto& decl = conversion.getExpression().cast<cld::Semantics::DeclarationRead>();
        return decl.getDeclRead().is<cld::Semantics::BuiltinFunction>();
    }

    static const cld::Semantics::BuiltinFunction&
        getBuiltinFunctionCall(const cld::Semantics::ExpressionBase& expression)
    {
        CLD_ASSERT(expression.is<cld::Semantics::Conversion>());
        auto& conversion = expression.cast<cld::Semantics::Conversion>();
        CLD_ASSERT(conversion.getKind() == cld::Semantics::Conversion::LValue);
        CLD_ASSERT(conversion.getExpression().is<cld::Semantics::DeclarationRead>());
        auto& decl = conversion.getExpression().cast<cld::Semantics::DeclarationRead>();
        return decl.getDeclRead().cast<cld::Semantics::BuiltinFunction>();
    }

    void visitVoidExpression(const cld::Semantics::ExpressionBase& expression)
    {
        if (!m_builder.GetInsertBlock())
        {
            return;
        }
        auto instr = visit(expression);
        if (llvm::isa_and_nonnull<llvm::Instruction>(instr.value) && instr.value->getNumUses() == 0
            && !llvm::cast<llvm::Instruction>(instr.value)->mayHaveSideEffects())
        {
            llvm::cast<llvm::Instruction>(instr.value)->eraseFromParent();
        }
    }

    Value boolToi1(Value value)
    {
        if (auto* cast = llvm::dyn_cast<llvm::CastInst>(value.value);
            cast && cast->getSrcTy() == m_builder.getInt1Ty() && cast->getNumUses() == 0)
        {
            auto* result = cast->getOperand(0);
            cast->eraseFromParent();
            return result;
        }
        return m_builder.CreateTrunc(value.value, m_builder.getInt1Ty());
    }

    std::optional<llvm::DIBuilder> m_debugInfo;
    std::vector<llvm::DIFile*> m_fileIdToFile;
    llvm::DIScope* m_currentDebugScope = nullptr;
    std::vector<llvm::DIScope*> m_scopeIdToScope{m_programInterface.getScopes().size()};

    llvm::DIFile* getFile(const cld::Lexer::CToken* CLD_NULLABLE iter) const
    {
        if (!iter)
        {
            return m_currentDebugScope->getFile();
        }
        return m_fileIdToFile[iter->getFileId()];
    }

    unsigned getLine(const cld::Lexer::CToken* CLD_NULLABLE iter) const
    {
        if (!iter)
        {
            return 0;
        }
        return iter->getLine(m_sourceInterface);
    }

    unsigned getColumn(const cld::Lexer::CToken* CLD_NULLABLE iter) const
    {
        if (!iter)
        {
            return 0;
        }
        return iter->getLine(m_sourceInterface);
    }

    llvm::DILocation* getLocation(const cld::Lexer::CToken* CLD_NULLABLE iter) const
    {
        CLD_ASSERT(m_currentDebugScope);
        return llvm::DILocation::get(m_module.getContext(), getLine(iter), getColumn(iter), m_currentDebugScope);
    }

public:
    explicit CodeGenerator(llvm::Module& module, const cld::Semantics::ProgramInterface& programInterface,
                           const cld::SourceInterface& sourceInterface, cld::Triple triple,
                           const cld::CGLLVM::Options& options)
        : m_module(module),
          m_programInterface(programInterface),
          m_sourceInterface(sourceInterface),
          m_options(options),
          m_triple(triple)
    {
        auto fullPath = cld::fs::u8path(m_sourceInterface.getFiles()[1].path);
        module.setSourceFileName(fullPath.filename().u8string());
        if (m_options.debugEmission == cld::CGLLVM::DebugEmission::None)
        {
            return;
        }
        m_debugInfo.emplace(m_module);
        llvm::DIFile* mainFile;
        for (auto& iter : m_sourceInterface.getFiles())
        {
            auto path = cld::fs::u8path(iter.path);
            auto dir = path;
            dir.remove_filename();
            auto* file = m_debugInfo->createFile(path.filename().u8string(), dir.u8string());
            if (path == fullPath)
            {
                mainFile = file;
            }
            m_fileIdToFile.push_back(file);
        }
        if (!mainFile)
        {
            return;
        }
        llvm::DICompileUnit::DebugEmissionKind kind = llvm::DICompileUnit::NoDebug;
        switch (m_options.debugEmission)
        {
            case cld::CGLLVM::DebugEmission::None: kind = llvm::DICompileUnit::NoDebug; break;
            case cld::CGLLVM::DebugEmission::Line: kind = llvm::DICompileUnit::LineTablesOnly; break;
            case cld::CGLLVM::DebugEmission::Default:
            case cld::CGLLVM::DebugEmission::Extended: kind = llvm::DICompileUnit::FullDebug; break;
        }
        m_currentDebugScope = m_debugInfo->createCompileUnit(llvm::dwarf::DW_LANG_C99, mainFile, "cld",
                                                             options.ol != llvm::CodeGenOpt::None, "", 0, {}, kind);
    }

    ~CodeGenerator()
    {
        if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
        {
            m_debugInfo->finalize();
        }
    }

    llvm::Type* visit(const cld::Semantics::Type& type)
    {
        return cld::match(
            type.getVariant(),
            [&](const cld::Semantics::PrimitiveType& primitiveType) -> llvm::Type* {
                switch (primitiveType.getKind())
                {
                    case cld::Semantics::PrimitiveType::Char:
                    case cld::Semantics::PrimitiveType::SignedChar:
                    case cld::Semantics::PrimitiveType::UnsignedChar: return m_builder.getInt8Ty();
                    case cld::Semantics::PrimitiveType::Bool:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfUnderlineBool * 8);
                    case cld::Semantics::PrimitiveType::UnsignedShort:
                    case cld::Semantics::PrimitiveType::Short:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfShort * 8);
                    case cld::Semantics::PrimitiveType::Int:
                    case cld::Semantics::PrimitiveType::UnsignedInt:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfInt * 8);
                    case cld::Semantics::PrimitiveType::Long:
                    case cld::Semantics::PrimitiveType::UnsignedLong:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfLong * 8);
                    case cld::Semantics::PrimitiveType::UnsignedLongLong:
                    case cld::Semantics::PrimitiveType::LongLong: return m_builder.getInt64Ty();
                    case cld::Semantics::PrimitiveType::Float: return m_builder.getFloatTy();
                    case cld::Semantics::PrimitiveType::Double: return m_builder.getDoubleTy();
                    case cld::Semantics::PrimitiveType::LongDouble:
                        switch (m_sourceInterface.getLanguageOptions().sizeOfLongDoubleBits)
                        {
                            case 64: return m_builder.getDoubleTy();
                            case 80: return llvm::Type::getX86_FP80Ty(m_module.getContext());
                            case 128: return llvm::Type::getFP128Ty(m_module.getContext());
                        }
                        CLD_UNREACHABLE;
                    case cld::Semantics::PrimitiveType::Void: return m_builder.getVoidTy();
                }
                CLD_UNREACHABLE;
            },
            [&](const cld::Semantics::ArrayType& arrayType) -> llvm::Type* {
                auto* elementType = visit(arrayType.getType());
                if (cld::Semantics::isVariableLengthArray(arrayType.getType()))
                {
                    return elementType;
                }
                return llvm::ArrayType::get(elementType, arrayType.getSize());
            },
            [&](const cld::Semantics::FunctionType& functionType) -> llvm::Type* {
                auto* returnType = visit(functionType.getReturnType());
                std::vector<llvm::Type*> args;
                for (auto& [type, name] : functionType.getArguments())
                {
                    (void)name;
                    args.push_back(visit(cld::Semantics::adjustParameterType(type)));
                }
                auto transformation = applyPlatformABI(returnType, args);
                m_functionABITransformations.emplace(functionType, transformation);
                return llvm::FunctionType::get(returnType, args, functionType.isLastVararg());
            },
            [&](const cld::Semantics::PointerType& pointerType) -> llvm::Type* {
                if (cld::Semantics::isVoid(pointerType.getElementType()))
                {
                    return m_builder.getInt8PtrTy();
                }
                auto* elementType = visit(pointerType.getElementType());
                return llvm::PointerType::getUnqual(elementType);
            },
            [&](const cld::Semantics::StructType& structType) -> llvm::Type* {
                auto result = m_types.find(structType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto* structDef = m_programInterface.getStructDefinition(structType.getId());
                auto* type = llvm::StructType::create(m_module.getContext(),
                                                      structType.isAnonymous() ? "struct.anon" : structType.getName());
                m_types.insert({structType, type});
                if (!structDef)
                {
                    return type;
                }

                std::vector<llvm::Type*> fields;
                for (auto& iter : structDef->getMemLayout())
                {
                    fields.push_back(visit(iter.type));
                }
                type->setBody(fields);
                return type;
            },
            [&](const cld::Semantics::UnionType& unionType) -> llvm::Type* {
                auto result = m_types.find(unionType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto* unionDef = m_programInterface.getUnionDefinition(unionType.getId());
                if (!unionDef)
                {
                    auto* type = llvm::StructType::create(m_module.getContext(),
                                                          unionType.isAnonymous() ? "union.anon" : unionType.getName());
                    m_types.insert({unionType, type});
                    return type;
                }
                const cld::Semantics::Type* largestAlignment = nullptr;
                std::size_t largestSize = 0;
                for (auto& iter : unionDef->getFieldLayout())
                {
                    if (!largestAlignment)
                    {
                        largestAlignment = iter.type.get();
                    }
                    else if (largestAlignment->getAlignOf(m_programInterface)
                             < iter.type->getAlignOf(m_programInterface))
                    {
                        largestAlignment = iter.type.get();
                    }
                    largestSize = std::max(largestSize, iter.type->getSizeOf(m_programInterface));
                }
                CLD_ASSERT(largestAlignment);
                largestSize = cld::roundUpTo(largestSize, largestAlignment->getAlignOf(m_programInterface));
                auto* type = unionType.isAnonymous() ?
                                 llvm::StructType::create(m_module.getContext()) :
                                 llvm::StructType::create(m_module.getContext(), unionType.getName());
                std::vector<llvm::Type*> body = {visit(*largestAlignment)};
                if (largestSize > largestAlignment->getSizeOf(m_programInterface))
                {
                    body.emplace_back(llvm::ArrayType::get(
                        m_builder.getInt8Ty(), largestSize - largestAlignment->getSizeOf(m_programInterface)));
                }
                type->setBody(body);
                m_types.insert({unionType, type});
                return type;
            },
            [&](const cld::Semantics::AbstractArrayType& arrayType) -> llvm::Type* {
                auto* elementType = visit(arrayType.getType());
                if (cld::Semantics::isVariableLengthArray(arrayType.getType()))
                {
                    return elementType;
                }
                return llvm::ArrayType::get(elementType, 0);
            },
            [&](const std::monostate&) -> llvm::Type* { CLD_UNREACHABLE; },
            [&](const cld::Semantics::EnumType& enumType) -> llvm::Type* {
                auto* enumDef = m_programInterface.getEnumDefinition(enumType.getId());
                CLD_ASSERT(enumDef);
                return visit(enumDef->getType());
            },
            [&](const cld::Semantics::ValArrayType& valArrayType) -> llvm::Type* {
                auto expression = m_valSizes.find(valArrayType.getExpression());
                if (expression == m_valSizes.end() && m_currentFunction && m_builder.GetInsertBlock())
                {
                    m_valSizes.emplace(valArrayType.getExpression(),
                                       m_builder.CreateIntCast(visit(*valArrayType.getExpression()),
                                                               m_builder.getInt64Ty(),
                                                               cld::get<cld::Semantics::PrimitiveType>(
                                                                   valArrayType.getExpression()->getType().getVariant())
                                                                   .isSigned()));
                }
                return visit(valArrayType.getType());
            });
    }

    llvm::DIType* visitDebug(const cld::Semantics::Type& type)
    {
        auto* result = cld::match(
            type.getVariant(), [](std::monostate) -> llvm::DIType* { CLD_UNREACHABLE; },
            [&](const cld::Semantics::PrimitiveType& primitive) -> llvm::DIType* {
                std::string_view name;
                unsigned encoding = 0;
                switch (primitive.getKind())
                {
                    case cld::Semantics::PrimitiveType::Char:
                        name = "char";
                        encoding = m_programInterface.getLanguageOptions().charIsSigned ?
                                       llvm::dwarf::DW_ATE_signed_char :
                                       llvm::dwarf::DW_ATE_unsigned_char;
                        break;
                    case cld::Semantics::PrimitiveType::SignedChar:
                        name = "signed char";
                        encoding = llvm::dwarf::DW_ATE_signed_char;
                        break;
                    case cld::Semantics::PrimitiveType::UnsignedChar:
                        name = "unsigned char";
                        encoding = llvm::dwarf::DW_ATE_unsigned_char;
                        break;
                    case cld::Semantics::PrimitiveType::Bool:
                        name = "bool";
                        encoding = llvm::dwarf::DW_ATE_boolean;
                        break;
                    case cld::Semantics::PrimitiveType::Short:
                        name = "short";
                        encoding = llvm::dwarf::DW_ATE_signed;
                        break;
                    case cld::Semantics::PrimitiveType::UnsignedShort:
                        name = "unsigned short";
                        encoding = llvm::dwarf::DW_ATE_unsigned;
                        break;
                    case cld::Semantics::PrimitiveType::Int:
                        name = "int";
                        encoding = llvm::dwarf::DW_ATE_signed;
                        break;
                    case cld::Semantics::PrimitiveType::UnsignedInt:
                        name = "unsigned int";
                        encoding = llvm::dwarf::DW_ATE_unsigned;
                        break;
                    case cld::Semantics::PrimitiveType::Long:
                        name = "long";
                        encoding = llvm::dwarf::DW_ATE_signed;
                        break;
                    case cld::Semantics::PrimitiveType::UnsignedLong:
                        name = "unsigned long";
                        encoding = llvm::dwarf::DW_ATE_unsigned;
                        break;
                    case cld::Semantics::PrimitiveType::LongLong:
                        name = "long long";
                        encoding = llvm::dwarf::DW_ATE_signed;
                        break;
                    case cld::Semantics::PrimitiveType::UnsignedLongLong:
                        name = "unsigned long long";
                        encoding = llvm::dwarf::DW_ATE_unsigned;
                        break;
                    case cld::Semantics::PrimitiveType::Float:
                        name = "float";
                        encoding = llvm::dwarf::DW_ATE_float;
                        break;
                    case cld::Semantics::PrimitiveType::Double:
                        name = "double";
                        encoding = llvm::dwarf::DW_ATE_float;
                        break;
                    case cld::Semantics::PrimitiveType::LongDouble:
                        name = "long double";
                        encoding = llvm::dwarf::DW_ATE_float;
                        break;
                    case cld::Semantics::PrimitiveType::Void:
                        name = "void";
                        encoding = llvm::dwarf::DW_ATE_unsigned;
                        break;
                }
                return m_debugInfo->createBasicType(name, primitive.getBitCount(), encoding);
            },
            [&](const cld::Semantics::PointerType& pointerType) -> llvm::DIType* {
                auto* element = visitDebug(pointerType.getElementType());
                auto* pointer =
                    m_debugInfo->createPointerType(element, m_programInterface.getLanguageOptions().sizeOfVoidStar);
                if (pointerType.isRestricted())
                {
                    pointer = m_debugInfo->createQualifiedType(llvm::dwarf::DW_TAG_restrict_type, pointer);
                }
                return pointer;
            },
            [&](const cld::Semantics::ArrayType& arrayType) -> llvm::DIType* {
                auto* element = visitDebug(arrayType.getType());
                return m_debugInfo->createArrayType(
                    arrayType.getSizeOf(m_programInterface) * 8, arrayType.getType().getAlignOf(m_programInterface) * 8,
                    element,
                    m_debugInfo->getOrCreateArray(llvm::DISubrange::get(m_module.getContext(), arrayType.getSize())));
            },
            [&](const cld::Semantics::AbstractArrayType& arrayType) -> llvm::DIType* {
                auto* element = visitDebug(arrayType.getType());
                return m_debugInfo->createArrayType(
                    arrayType.getType().getSizeOf(m_programInterface) * 8,
                    arrayType.getType().getAlignOf(m_programInterface) * 8, element,
                    m_debugInfo->getOrCreateArray(llvm::DISubrange::get(m_module.getContext(), 1)));
            },
            [&](const cld::Semantics::ValArrayType&) -> llvm::DIType* {
                // TODO:
                CLD_UNREACHABLE;
            },
            [&](const cld::Semantics::StructType& structType) -> llvm::DIType* {
                auto result = m_debugTypes.find(structType);
                if (result != m_debugTypes.end())
                {
                    return result->second;
                }
                auto* structDef = m_programInterface.getStructDefinition(structType.getId());
                if (!structDef)
                {
                    auto* structFwdDecl = m_debugInfo->createForwardDecl(
                        llvm::dwarf::DW_TAG_structure_type, structType.getName(),
                        m_scopeIdToScope[m_programInterface.getStructScope(structType.getId())],
                        getFile(m_programInterface.getStructLoc(structType.getId())),
                        getLine(m_programInterface.getStructLoc(structType.getId())));
                    m_debugTypes.emplace(structType, structFwdDecl);
                    return structFwdDecl;
                }
                auto* structFwdDecl = m_debugInfo->createReplaceableCompositeType(
                    llvm::dwarf::DW_TAG_structure_type, structType.getName(),
                    m_scopeIdToScope[m_programInterface.getStructScope(structType.getId())],
                    getFile(m_programInterface.getStructLoc(structType.getId())),
                    getLine(m_programInterface.getStructLoc(structType.getId())));
                m_debugTypes.emplace(structType, structFwdDecl);
                std::vector<llvm::Metadata*> elements;
                for (auto& iter : structDef->getFields())
                {
                    auto* subType = visitDebug(*iter.second.type);
                    std::size_t offset = 0;
                    const cld::Semantics::Type* currentType = &type;
                    for (auto index : iter.second.indices)
                    {
                        if (cld::Semantics::isStruct(*currentType))
                        {
                            const auto& memoryLayout = m_programInterface.getMemoryLayout(*currentType);
                            offset += memoryLayout[index].offset;
                            currentType = &memoryLayout[index].type;
                        }
                        else
                        {
                            currentType = m_programInterface.getFieldLayout(*currentType)[index].type.get();
                        }
                    }
                    if (!iter.second.bitFieldBounds)
                    {
                        elements.push_back(m_debugInfo->createMemberType(
                            structFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                            iter.second.type->getSizeOf(m_programInterface) * 8,
                            iter.second.type->getAlignOf(m_programInterface) * 8, offset * 8,
                            llvm::DINode::DIFlags::FlagZero, subType));
                        continue;
                    }
                    elements.push_back(m_debugInfo->createBitFieldMemberType(
                        structFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                        iter.second.bitFieldBounds->second - iter.second.bitFieldBounds->first,
                        8 * offset + iter.second.bitFieldBounds->first, 8 * offset, llvm::DINode::DIFlags::FlagZero,
                        subType));
                }
                auto* debugStructDef = llvm::DICompositeType::getDistinct(
                    m_module.getContext(), llvm::dwarf::DW_TAG_structure_type, structType.getName(),
                    getFile(m_programInterface.getStructLoc(structType.getId())),
                    getLine(m_programInterface.getStructLoc(structType.getId())),
                    m_scopeIdToScope[m_programInterface.getStructScope(structType.getId())], nullptr,
                    structType.getSizeOf(m_programInterface) * 8, structType.getAlignOf(m_programInterface) * 8, 0,
                    llvm::DINode::DIFlags::FlagZero, m_debugInfo->getOrCreateArray(elements), 0, nullptr);
                structFwdDecl->replaceAllUsesWith(debugStructDef);
                m_debugTypes.insert_or_assign(structType, debugStructDef);
                return debugStructDef;
            },
            [&](const cld::Semantics::UnionType& unionType) -> llvm::DIType* {
                auto result = m_debugTypes.find(unionType);
                if (result != m_debugTypes.end())
                {
                    return result->second;
                }
                auto* unionDefinition = m_programInterface.getUnionDefinition(unionType.getId());
                if (!unionDefinition)
                {
                    auto* structFwdDecl = m_debugInfo->createForwardDecl(
                        llvm::dwarf::DW_TAG_union_type, unionType.getName(),
                        m_scopeIdToScope[m_programInterface.getUnionScope(unionType.getId())],
                        getFile(m_programInterface.getUnionLoc(unionType.getId())),
                        getLine(m_programInterface.getUnionLoc(unionType.getId())));
                    m_debugTypes.emplace(unionType, structFwdDecl);
                    return structFwdDecl;
                }
                auto* unionFwdDecl = m_debugInfo->createReplaceableCompositeType(
                    llvm::dwarf::DW_TAG_union_type, unionType.getName(),
                    m_scopeIdToScope[m_programInterface.getUnionScope(unionType.getId())],
                    getFile(m_programInterface.getUnionLoc(unionType.getId())),
                    getLine(m_programInterface.getUnionLoc(unionType.getId())));
                m_debugTypes.emplace(unionType, unionFwdDecl);
                std::vector<llvm::Metadata*> elements;
                for (auto& iter : unionDefinition->getFields())
                {
                    auto* subType = visitDebug(*iter.second.type);
                    std::size_t offset = 0;
                    const cld::Semantics::Type* currentType = &type;
                    for (auto index : iter.second.indices)
                    {
                        if (cld::Semantics::isStruct(*currentType))
                        {
                            const auto& memoryLayout = m_programInterface.getMemoryLayout(*currentType);
                            offset += memoryLayout[index].offset;
                            currentType = &memoryLayout[index].type;
                        }
                        else
                        {
                            currentType = m_programInterface.getFieldLayout(*currentType)[index].type.get();
                        }
                    }
                    if (!iter.second.bitFieldBounds)
                    {
                        elements.push_back(m_debugInfo->createMemberType(
                            unionFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                            iter.second.type->getSizeOf(m_programInterface) * 8,
                            iter.second.type->getAlignOf(m_programInterface) * 8, offset * 8,
                            llvm::DINode::DIFlags::FlagZero, subType));
                        continue;
                    }
                    elements.push_back(m_debugInfo->createBitFieldMemberType(
                        unionFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                        iter.second.bitFieldBounds->second - iter.second.bitFieldBounds->first,
                        8 * offset + iter.second.bitFieldBounds->first, 8 * offset, llvm::DINode::DIFlags::FlagZero,
                        subType));
                }
                auto* debugUnionDef = llvm::DICompositeType::getDistinct(
                    m_module.getContext(), llvm::dwarf::DW_TAG_union_type, unionType.getName(),
                    getFile(m_programInterface.getStructLoc(unionType.getId())),
                    getLine(m_programInterface.getStructLoc(unionType.getId())),
                    m_scopeIdToScope[m_programInterface.getStructScope(unionType.getId())], nullptr,
                    unionType.getSizeOf(m_programInterface) * 8, unionType.getAlignOf(m_programInterface) * 8, 0,
                    llvm::DINode::DIFlags::FlagZero, m_debugInfo->getOrCreateArray(elements), 0, nullptr);
                unionFwdDecl->replaceAllUsesWith(debugUnionDef);
                m_debugTypes.insert_or_assign(unionType, debugUnionDef);
                return debugUnionDef;
            },
            [&](const cld::Semantics::FunctionType& functionType) -> llvm::DIType* {
                std::vector<llvm::Metadata*> parameters;
                if (cld::Semantics::isVoid(functionType.getReturnType()))
                {
                    parameters.push_back(nullptr);
                }
                else
                {
                    parameters.push_back(visitDebug(functionType.getReturnType()));
                }
                for (auto& [type, name] : functionType.getArguments())
                {
                    (void)name;
                    parameters.push_back(visitDebug(type));
                }
                return m_debugInfo->createSubroutineType(m_debugInfo->getOrCreateTypeArray(parameters));
            },
            [&](const cld::Semantics::EnumType& enumType) -> llvm::DIType* {
                auto result = m_debugTypes.find(enumType);
                if (result != m_debugTypes.end())
                {
                    return result->second;
                }
                auto* enumDef = m_programInterface.getEnumDefinition(enumType.getId());
                CLD_ASSERT(enumDef); // Currently not possible
                std::vector<llvm::Metadata*> enumerators;
                for (auto& [name, value] : enumDef->getValues())
                {
                    enumerators.push_back(
                        m_debugInfo->createEnumerator(name, value.getSExtValue(), value.isUnsigned()));
                }
                auto* underlying = visitDebug(enumDef->getType());
                auto* debugEnumDef = m_debugInfo->createEnumerationType(
                    m_scopeIdToScope[m_programInterface.getEnumScope(enumType.getId())], enumType.getName(),
                    getFile(m_programInterface.getEnumLoc(enumType.getId())),
                    getLine(m_programInterface.getEnumLoc(enumType.getId())),
                    enumType.getSizeOf(m_programInterface) * 8, enumType.getAlignOf(m_programInterface) * 8,
                    m_debugInfo->getOrCreateArray(enumerators), underlying);
                m_debugTypes.insert_or_assign(enumType, debugEnumDef);
                return debugEnumDef;
            });
        if (type.isVolatile())
        {
            result = m_debugInfo->createQualifiedType(llvm::dwarf::DW_TAG_volatile_type, result);
        }
        if (type.isConst())
        {
            result = m_debugInfo->createQualifiedType(llvm::dwarf::DW_TAG_const_type, result);
        }
        if (!type.isTypedef())
        {
            return result;
        }
        // TODO:
        // return m_debugInfo->createTypedef(result,type.getName(),);
        return result;
    }

    void visit(const cld::Semantics::TranslationUnit& translationUnit)
    {
        for (auto& iter : translationUnit.getGlobals())
        {
            cld::match(
                iter,
                [&](const std::unique_ptr<cld::Semantics::FunctionDefinition>& functionDefinition) {
                    visit(*functionDefinition);
                },
                [&](const std::unique_ptr<cld::Semantics::Declaration>& declaration) {
                    auto global = visit(*declaration);
                    if (llvm::isa_and_nonnull<llvm::GlobalVariable>(global.value))
                    {
                        m_cGlobalVariables.emplace(declaration->getNameToken()->getText(),
                                                   llvm::cast<llvm::GlobalVariable>(global.value));
                    }
                });
        }
    }

    Value visit(const cld::Semantics::Declaration& declaration)
    {
        llvm::Function::LinkageTypes linkageType = llvm::GlobalValue::ExternalLinkage;
        switch (declaration.getLinkage())
        {
            case cld::Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
            case cld::Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
            case cld::Semantics::Linkage::None: break;
        }
        if (std::holds_alternative<cld::Semantics::FunctionType>(declaration.getType().getVariant()))
        {
            if (!m_options.emitAllDecls && !declaration.isUsed())
            {
                return nullptr;
            }
            auto* function = m_module.getFunction(declaration.getNameToken()->getText());
            if (function)
            {
                m_lvalues.emplace(&declaration, valueOf(function));
                return valueOf(function);
            }
            auto* ft = llvm::cast<llvm::FunctionType>(visit(declaration.getType()));
            function = llvm::Function::Create(ft, linkageType, -1,
                                              llvm::StringRef{declaration.getNameToken()->getText()}, &m_module);
            applyFunctionAttributes(*function, ft,
                                    cld::get<cld::Semantics::FunctionType>(declaration.getType().getVariant()));
            if (!m_options.reloc)
            {
                function->setDSOLocal(true);
            }
            m_lvalues.emplace(&declaration, valueOf(function));
            return valueOf(function);
        }
        if (declaration.getLifetime() == cld::Semantics::Lifetime::Static)
        {
            if (!m_options.emitAllDecls && declaration.getLinkage() == cld::Semantics::Linkage::Internal
                && !declaration.isUsed())
            {
                return nullptr;
            }
            auto& declType = [&]() -> decltype(auto) {
                if (m_currentFunction)
                {
                    return declaration.getType();
                }

                auto& decl =
                    m_programInterface.getScopes()[0].declarations.at(declaration.getNameToken()->getText()).declared;
                return cld::get<cld::Semantics::Declaration*>(decl)->getType();
            }();
            auto* type = visit(declType);

            auto* prevType = type;
            llvm::Constant* constant = nullptr;
            if (declaration.getInitializer() && declaration.getKind() != cld::Semantics::Declaration::DeclarationOnly)
            {
                constant = llvm::cast<llvm::Constant>(visit(*declaration.getInitializer(), declType, type).value);
                type = constant->getType();
            }
            else if (declaration.getKind() != cld::Semantics::Declaration::DeclarationOnly)
            {
                constant = llvm::Constant::getNullValue(type);
            }
            if (m_currentFunction && declaration.getKind() != cld::Semantics::Declaration::DeclarationOnly)
            {
                linkageType = llvm::GlobalValue::InternalLinkage;
            }
            else if (declaration.getLinkage() != cld::Semantics::Linkage::Internal
                     && declaration.getKind() == cld::Semantics::Declaration::TentativeDefinition)
            {
                linkageType = llvm::GlobalValue::CommonLinkage;
            }

            llvm::GlobalVariable* global = nullptr;
            if (m_currentFunction || m_cGlobalVariables.count(declaration.getNameToken()->getText()) == 0)
            {
                global = new llvm::GlobalVariable(
                    m_module, type, declType.isConst() && linkageType != llvm::GlobalValue::CommonLinkage, linkageType,
                    constant, llvm::StringRef{declaration.getNameToken()->getText()});
                if (m_programInterface.isCompleteType(declType)
                    || std::holds_alternative<cld::Semantics::AbstractArrayType>(declType.getVariant()))
                {
                    if (m_triple.getArchitecture() == cld::Architecture::x86_64 && cld::Semantics::isArray(declType)
                        && !std::holds_alternative<cld::Semantics::AbstractArrayType>(declType.getVariant())
                        && declType.getSizeOf(m_programInterface) >= 16)
                    {
                        global->setAlignment(llvm::Align(16));
                    }
                    else
                    {
                        global->setAlignment(llvm::Align(declType.getAlignOf(m_programInterface)));
                    }
                }
                if (!m_options.reloc)
                {
                    global->setDSOLocal(true);
                }
            }
            else
            {
                global = m_cGlobalVariables[declaration.getNameToken()->getText()];
                global->setConstant(declType.isConst() && linkageType != llvm::GlobalValue::CommonLinkage);
                global->setLinkage(linkageType);
                global->setInitializer(constant);
            }

            m_lvalues.emplace(&declaration, createBitCast(global, llvm::PointerType::getUnqual(prevType)));
            return global;
        }
        if (!m_options.emitAllDecls && !declaration.isUsed())
        {
            return nullptr;
        }
        auto* type = visit(declaration.getType());
        // Place all allocas up top except variably modified types
        llvm::AllocaInst* var = nullptr;
        if (cld::Semantics::isVariableLengthArray(declaration.getType()))
        {
            if (!m_builder.GetInsertBlock())
            {
                return nullptr;
            }
            llvm::Value* value = nullptr;
            cld::Semantics::RecursiveVisitor visitor(declaration.getType(), cld::Semantics::ARRAY_TYPE_NEXT_FN);
            bool valSeen = false;
            for (auto& iter : visitor)
            {
                if (std::holds_alternative<cld::Semantics::ValArrayType>(iter.getVariant()))
                {
                    valSeen = true;
                    if (!value)
                    {
                        value = m_valSizes[cld::get<cld::Semantics::ValArrayType>(iter.getVariant()).getExpression()];
                        continue;
                    }
                    value = m_builder.CreateMul(
                        value, m_valSizes[cld::get<cld::Semantics::ValArrayType>(iter.getVariant()).getExpression()]);
                }
                else
                {
                    if (!value)
                    {
                        value = m_builder.getInt64(cld::get<cld::Semantics::ArrayType>(iter.getVariant()).getSize());
                        continue;
                    }
                    if (!valSeen)
                    {
                        value = m_builder.CreateMul(
                            m_builder.getInt64(cld::get<cld::Semantics::ArrayType>(iter.getVariant()).getSize()),
                            value);
                        continue;
                    }
                    break;
                }
            }

            auto* stackSave = createAllocaAtTop(m_builder.getInt8PtrTy(0), "stack.save");
            m_stackSaves[&declaration] = stackSave;
            auto* stack = m_builder.CreateIntrinsic(llvm::Intrinsic::stacksave, {}, {});
            createStore(stack, stackSave, false);
            var = m_builder.CreateAlloca(type, value, llvm::StringRef{declaration.getNameToken()->getText()});
            var->setAlignment(m_module.getDataLayout().getStackAlignment());
        }
        else
        {
            var = createAllocaAtTop(type, declaration.getNameToken()->getText());
            if (m_triple.getArchitecture() == cld::Architecture::x86_64
                && cld::Semantics::isArray(declaration.getType())
                && declaration.getType().getSizeOf(m_programInterface) >= 16)
            {
                var->setAlignment(llvm::Align(16));
            }
            else
            {
                var->setAlignment(llvm::Align(declaration.getType().getAlignOf(m_programInterface)));
            }
            if (m_options.debugEmission > cld::CGLLVM::DebugEmission::Line)
            {
                auto* local = m_debugInfo->createAutoVariable(
                    m_currentDebugScope, declaration.getNameToken()->getText(), getFile(declaration.getNameToken()),
                    getLine(declaration.getNameToken()), visitDebug(declaration.getType()), true,
                    llvm::DINode::FlagZero, var->getAlign().value() * 8);
                if (m_builder.GetInsertBlock())
                {
                    m_debugInfo->insertDeclare(var, local, m_debugInfo->createExpression(),
                                               getLocation(declaration.getNameToken()), m_builder.GetInsertBlock());
                }
            }
        }

        m_lvalues.emplace(&declaration, var);
        if (m_builder.GetInsertBlock())
        {
            if (!cld::Semantics::isVariableLengthArray(declaration.getType()))
            {
                auto* size = m_builder.getInt64(declaration.getType().getSizeOf(m_programInterface));
                m_builder.CreateLifetimeStart(var, llvm::cast<llvm::ConstantInt>(size));
            }
            if (declaration.getInitializer())
            {
                visit(*declaration.getInitializer(), declaration.getType(), var);
            }
        }
        return var;
    }

    void visit(const cld::Semantics::FunctionDefinition& functionDefinition)
    {
        if (!m_options.emitAllDecls && functionDefinition.getLinkage() != cld::Semantics::Linkage::External
            && !functionDefinition.isUsed())
        {
            return;
        }
        auto* function = m_module.getFunction(functionDefinition.getNameToken()->getText());
        if (!function)
        {
            llvm::Function::LinkageTypes linkageType;
            switch (functionDefinition.getLinkage())
            {
                case cld::Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
                case cld::Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
                case cld::Semantics::Linkage::None: CLD_UNREACHABLE;
            }
            auto* ft = llvm::cast<llvm::FunctionType>(visit(functionDefinition.getType()));
            function = llvm::Function::Create(ft, linkageType, -1,
                                              llvm::StringRef{functionDefinition.getNameToken()->getText()}, &m_module);
        }
        m_lvalues.emplace(&functionDefinition, valueOf(function));
        auto& ft = cld::get<cld::Semantics::FunctionType>(functionDefinition.getType().getVariant());
        if (functionDefinition.getInlineKind() == cld::Semantics::InlineKind::InlineDefinition
            && functionDefinition.getLinkage() == cld::Semantics::Linkage::External)
        {
            applyFunctionAttributes(*function, function->getFunctionType(), ft);
            return;
        }

        if (!m_options.reloc)
        {
            function->setDSOLocal(true);
        }

        auto* bb = llvm::BasicBlock::Create(m_module.getContext(), "entry", function);
        m_builder.SetInsertPoint(bb);
        cld::ScopeExit insertionPointReset{[&] {
            m_builder.ClearInsertionPoint();
            m_builder.SetCurrentDebugLocation({});
        }};

        m_currentFunction = function;
        cld::ValueReset currentFunctionReset(m_currentFunction, nullptr);

        std::optional<cld::ValueReset<llvm::DIScope*>> subProgramReset;
        if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
        {
            auto tempParams = llvm::MDNode::getTemporary(m_module.getContext(), {});
            auto* subRoutineType = m_debugInfo->createSubroutineType(tempParams.get());
            llvm::DISubprogram::DISPFlags spFlags = llvm::DISubprogram::SPFlagDefinition;
            if (functionDefinition.getNameToken()->getText() == "main")
            {
                spFlags |= llvm::DISubprogram::SPFlagMainSubprogram;
            }
            auto* subProgram = m_debugInfo->createFunction(
                getFile(functionDefinition.getNameToken()), functionDefinition.getNameToken()->getText(),
                functionDefinition.getNameToken()->getText(), getFile(functionDefinition.getNameToken()),
                getLine(functionDefinition.getNameToken()), subRoutineType,
                getLine(functionDefinition.getCompoundStatement().getOpenBrace()),
                ft.isKandR() ? llvm::DINode::FlagZero : llvm::DINode::FlagPrototyped, spFlags);
            m_currentFunction->setSubprogram(subProgram);
            subProgramReset.emplace(m_currentDebugScope, m_currentDebugScope);
            m_scopeIdToScope[functionDefinition.getCompoundStatement().getScope()] = m_currentDebugScope = subProgram;

            std::vector<llvm::Metadata*> parameters;
            if (m_options.debugEmission > cld::CGLLVM::DebugEmission::Line)
            {
                if (cld::Semantics::isVoid(ft.getReturnType()))
                {
                    parameters.push_back(nullptr);
                }
                else
                {
                    parameters.push_back(visitDebug(ft.getReturnType()));
                }
                for (auto& [type, name] : ft.getArguments())
                {
                    (void)name;
                    parameters.push_back(visitDebug(cld::Semantics::adjustParameterType(type)));
                }
            }
            auto* typeArray = llvm::MDTuple::get(m_module.getContext(), parameters);
            m_debugInfo->replaceTemporary(std::move(tempParams), typeArray);

            m_builder.SetCurrentDebugLocation({});
        }

        applyFunctionAttributes(*m_currentFunction, m_currentFunction->getFunctionType(), ft,
                                &functionDefinition.getParameterDeclarations());

        auto transformations = m_functionABITransformations.find(ft);
        CLD_ASSERT(transformations != m_functionABITransformations.end());
        m_currentFunctionABI = &transformations->second;
        cld::ValueReset currentFunctionABIReset(m_currentFunctionABI, nullptr);
        if (transformations->second.returnType == ABITransformations::Flattened
            || transformations->second.returnType == ABITransformations::IntegerRegister)
        {
            m_returnSlot = createAllocaAtTop(function->getReturnType());
            m_returnSlot->setAlignment(llvm::Align(ft.getReturnType().getAlignOf(m_programInterface)));
        }
        else
        {
            m_returnSlot = nullptr;
        }

        std::size_t argStart = 0;
        if (transformations->second.returnType == ABITransformations::PointerToTemporary)
        {
            argStart = 1;
        }
        std::size_t origArgI = 0;
        for (std::size_t i = argStart; i < function->arg_size(); origArgI++)
        {
            auto& paramDecl = functionDefinition.getParameterDeclarations()[origArgI];
            if (std::holds_alternative<ABITransformations::Change>(transformations->second.arguments[origArgI]))
            {
                auto* operand = function->getArg(i);
                i++;
                auto change = cld::get<ABITransformations::Change>(transformations->second.arguments[origArgI]);
                if (change == ABITransformations::PointerToTemporary || change == ABITransformations::OnStack)
                {
                    if (operand->getType()->isPointerTy())
                    {
                        m_lvalues.emplace(paramDecl.get(),
                                          Value(operand, operand->getPointerAlignment(m_module.getDataLayout())));
                    }
                    else
                    {
                        m_lvalues.emplace(paramDecl.get(), valueOf(operand, operand->getParamAlign()));
                    }
                    continue;
                }
                auto alloc = m_lvalues.at(paramDecl.get());
                if (change == ABITransformations::Unchanged)
                {
                    createStore(operand, alloc, paramDecl->getType().isVolatile());
                    continue;
                }

                auto cast = createBitCast(alloc, llvm::PointerType::getUnqual(operand->getType()), false);
                createStore(operand, cast, paramDecl->getType().isVolatile());
            }
            else
            {
                auto& multiArg =
                    cld::get<ABITransformations::MultipleArgs>(transformations->second.arguments[origArgI]);
                CLD_ASSERT(multiArg.size <= 2 && multiArg.size > 0);
                auto alloc = m_lvalues.at(paramDecl.get());
                std::vector<llvm::Type*> elements;
                elements.push_back(function->getArg(i)->getType());
                if (multiArg.size == 2)
                {
                    elements.push_back(function->getArg(i + 1)->getType());
                }
                auto structType = createBitCast(
                    alloc, llvm::PointerType::getUnqual(llvm::StructType::get(m_builder.getContext(), elements)),
                    false);
                auto firstElement = createInBoundsGEP(structType, {m_builder.getInt32(0), m_builder.getInt32(0)});
                createStore(function->getArg(i), firstElement, paramDecl->getType().isVolatile());
                if (multiArg.size == 2)
                {
                    auto secondElement = createInBoundsGEP(structType, {m_builder.getInt32(0), m_builder.getInt32(1)});
                    createStore(function->getArg(i + 1), secondElement, paramDecl->getType().isVolatile());
                }
                i += multiArg.size;
            }
        }
        for (auto& [type, name] : ft.getArguments())
        {
            // Go through the visit of each parameter again in case one them of was a variably modified type.
            // The expressions of these could previously not be evaluated as there was no function block to
            // evaluate the expressions nor parameters transferred that's why we're doing it now
            (void)name;
            if (cld::Semantics::isVariablyModified(type))
            {
                visit(type);
            }
        }

        visit(functionDefinition.getCompoundStatement());
        if (m_builder.GetInsertBlock())
        {
            if (cld::Semantics::isVoid(ft.getReturnType()))
            {
                m_builder.CreateRetVoid();
            }
            else if (functionDefinition.getNameToken()->getText() == "main"
                     && ft.getReturnType()
                            == cld::Semantics::PrimitiveType::createInt(false, false,
                                                                        m_programInterface.getLanguageOptions()))
            {
                m_builder.CreateRet(m_builder.getIntN(m_programInterface.getLanguageOptions().sizeOfInt * 8, 0));
            }
            else
            {
                m_builder.CreateUnreachable();
            }
        }
        if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
        {
            m_debugInfo->finalizeSubprogram(m_currentFunction->getSubprogram());
        }
#ifndef NDEBUG
        if (llvm::verifyFunction(*m_currentFunction, &llvm::errs()))
        {
            m_currentFunction->print(llvm::errs(), nullptr, false, true);
            std::terminate();
        }
#endif
    }

    void visit(const cld::Semantics::CompoundStatement& compoundStatement)
    {
        std::optional<cld::ValueReset<llvm::DIScope*>> reset;
        if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
        {
            if (!m_scopeIdToScope[compoundStatement.getScope()])
            {
                auto* parent =
                    m_scopeIdToScope[m_programInterface.getScopes()[compoundStatement.getScope()].previousScope];
                CLD_ASSERT(parent);
                m_scopeIdToScope[compoundStatement.getScope()] = m_debugInfo->createLexicalBlock(
                    parent, getFile(compoundStatement.getOpenBrace()), getLine(compoundStatement.getOpenBrace()),
                    getColumn(compoundStatement.getOpenBrace()));
            }
            reset.emplace(m_currentDebugScope, m_currentDebugScope);
            m_currentDebugScope = m_scopeIdToScope[compoundStatement.getScope()];
        }
        for (auto& iter : compoundStatement.getCompoundItems())
        {
            if (std::holds_alternative<std::shared_ptr<const cld::Semantics::ExpressionBase>>(iter))
            {
                auto& expr = cld::get<std::shared_ptr<const cld::Semantics::ExpressionBase>>(iter);
                auto result = m_valSizes.emplace(
                    expr, m_builder.CreateIntCast(
                              visit(*expr).value, m_builder.getInt64Ty(),
                              cld::get<cld::Semantics::PrimitiveType>(expr->getType().getVariant()).isSigned()));
                (void)result;
                CLD_ASSERT(result.second);
            }
            else if (std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter))
            {
                visit(*cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter));
            }
            else if (std::holds_alternative<cld::IntrVarPtr<cld::Semantics::Statement>>(iter))
            {
                visit(*cld::get<cld::IntrVarPtr<cld::Semantics::Statement>>(iter));
            }
        }
        if (m_builder.GetInsertBlock())
        {
            runDestructors(compoundStatement.getScope());
        }
    }

    void visit(const cld::Semantics::Statement& statement)
    {
        statement.match([&](const auto& statement) { visit(statement); },
                        [&](const cld::Semantics::ExpressionStatement& expressionStatement) {
                            if (!expressionStatement.getExpression() || !m_builder.GetInsertBlock())
                            {
                                return;
                            }
                            visitVoidExpression(*expressionStatement.getExpression());
                        });
    }

    void visit(const cld::Semantics::ReturnStatement& returnStatement)
    {
        if (!m_builder.GetInsertBlock())
        {
            return;
        }
        if (!returnStatement.getExpression())
        {
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRetVoid();
            m_builder.ClearInsertionPoint();
            return;
        }

        auto* function = m_currentFunction;
        auto value = visit(*returnStatement.getExpression());
        if (m_currentFunctionABI->returnType == ABITransformations::PointerToTemporary)
        {
            createStore(value.value, valueOf(function->getArg(0), function->getParamAlign(0)), false);
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRetVoid();
        }
        else if (m_currentFunctionABI->returnType == ABITransformations::Flattened
                 || m_currentFunctionABI->returnType == ABITransformations::IntegerRegister)
        {
            // TODO: Check this again
            if (m_returnSlot->getAlign() == value.alignment)
            {
                m_builder.CreateAlignedStore(
                    value.value, createBitCast(m_returnSlot, llvm::PointerType::getUnqual(value.value->getType())),
                    m_returnSlot->getAlign());
            }
            else
            {
                auto* ptr = llvm::cast<llvm::LoadInst>(value.value)->getPointerOperand();
                m_builder.CreateMemCpy(m_returnSlot, m_returnSlot->getAlign(), ptr,
                                       llvm::cast<llvm::LoadInst>(value.value)->getAlign(),
                                       returnStatement.getExpression()->getType().getSizeOf(m_programInterface));
                llvm::cast<llvm::LoadInst>(value.value)->eraseFromParent();
            }
            auto ret = createLoad(m_returnSlot, false);
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRet(ret);
        }
        else
        {
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRet(value.value);
        }
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::ForStatement& forStatement)
    {
        std::optional<cld::ValueReset<llvm::DIScope*>> reset;
        cld::match(
            forStatement.getInitial(), [](std::monostate) {},
            [&](const std::vector<std::unique_ptr<cld::Semantics::Declaration>>& declaration) {
                if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
                {
                    if (!m_scopeIdToScope[forStatement.getScope()])
                    {
                        auto* parent =
                            m_scopeIdToScope[m_programInterface.getScopes()[forStatement.getScope()].previousScope];
                        CLD_ASSERT(parent);
                        m_scopeIdToScope[forStatement.getScope()] = llvm::DILexicalBlock::get(
                            m_module.getContext(), parent, getFile(forStatement.getForToken()),
                            getLine(forStatement.getForToken()), getColumn(forStatement.getForToken()));
                    }
                    reset.emplace(m_currentDebugScope, m_currentDebugScope);
                    m_currentDebugScope = m_scopeIdToScope[forStatement.getScope()];
                }
                for (auto& iter : declaration)
                {
                    visit(*iter);
                }
            },
            [&](const cld::IntrVarPtr<cld::Semantics::ExpressionBase>& expression) {
                visitVoidExpression(*expression);
            });
        auto* controlling = llvm::BasicBlock::Create(m_module.getContext(), "for.controlling", m_currentFunction);
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(controlling);
        }
        m_builder.SetInsertPoint(controlling);
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "for.body", m_currentFunction);
        llvm::BasicBlock* contBlock =
            llvm::BasicBlock::Create(m_module.getContext(), "for.continue", m_currentFunction);
        m_breakTargets[&forStatement] = contBlock;
        if (forStatement.getControlling())
        {
            auto value = visit(*forStatement.getControlling());
            value = boolToi1(value);
            m_builder.CreateCondBr(value.value, body, contBlock);
        }
        else
        {
            m_builder.CreateBr(body);
        }
        m_builder.SetInsertPoint(body);
        auto* iteration = forStatement.getIteration() ?
                              llvm::BasicBlock::Create(m_module.getContext(), "for.iteration", m_currentFunction) :
                              controlling;
        m_continueTargets[&forStatement] = iteration;
        visit(forStatement.getStatement());
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(iteration);
        }
        if (forStatement.getIteration())
        {
            m_builder.SetInsertPoint(iteration);
            visitVoidExpression(*forStatement.getIteration());
            m_builder.CreateBr(controlling);
        }
        m_builder.SetInsertPoint(contBlock);
        if (std::holds_alternative<std::vector<std::unique_ptr<cld::Semantics::Declaration>>>(
                forStatement.getInitial()))
        {
            // If the for statement held declarations we must run the destructors for those declarations as soon as we
            // leave the statement
            runDestructors(forStatement.getScope());
        }
    }

    void visit(const cld::Semantics::IfStatement& ifStatement)
    {
        Value expression = nullptr;
        llvm::BasicBlock* trueBranch = nullptr;
        if (m_builder.GetInsertBlock())
        {
            expression = visit(ifStatement.getExpression());
            expression = boolToi1(expression);
            trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.true", m_currentFunction);
        }
        if (!ifStatement.getFalseBranch())
        {
            auto* contBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.continue", m_currentFunction);
            if (m_builder.GetInsertBlock())
            {
                m_builder.CreateCondBr(expression.value, trueBranch, contBranch);
                m_builder.SetInsertPoint(trueBranch);
            }
            visit(ifStatement.getTrueBranch());
            if (m_builder.GetInsertBlock())
            {
                m_builder.CreateBr(contBranch);
            }
            m_builder.SetInsertPoint(contBranch);
            return;
        }
        llvm::BasicBlock* falseBranch = nullptr;
        if (m_builder.GetInsertBlock())
        {
            falseBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.false", m_currentFunction);
            m_builder.CreateCondBr(expression.value, trueBranch, falseBranch);
            m_builder.SetInsertPoint(trueBranch);
        }
        visit(ifStatement.getTrueBranch());
        auto* trueBlock = m_builder.GetInsertBlock();
        auto* trueTerminator = m_builder.GetInsertBlock() ? m_builder.GetInsertBlock()->getTerminator() : nullptr;
        if (falseBranch)
        {
            m_builder.SetInsertPoint(falseBranch);
        }
        visit(*ifStatement.getFalseBranch());
        auto* falseBlock = m_builder.GetInsertBlock();
        auto* falseTerminator = m_builder.GetInsertBlock() ? m_builder.GetInsertBlock()->getTerminator() : nullptr;
        if ((trueBlock && !trueTerminator) || (falseBlock && !falseTerminator))
        {
            auto* contBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.continue", m_currentFunction);
            if (trueBlock && !trueTerminator)
            {
                m_builder.SetInsertPoint(trueBlock);
                m_builder.CreateBr(contBranch);
            }
            if (falseBlock && !falseTerminator)
            {
                m_builder.SetInsertPoint(falseBlock);
                m_builder.CreateBr(contBranch);
            }
            m_builder.SetInsertPoint(contBranch);
        }
        else
        {
            m_builder.ClearInsertionPoint();
        }
    }

    void visit(const cld::Semantics::HeadWhileStatement& headWhileStatement)
    {
        auto* controlling = llvm::BasicBlock::Create(m_module.getContext(), "while.controlling", m_currentFunction);
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(controlling);
        }
        m_continueTargets[&headWhileStatement] = controlling;
        m_builder.SetInsertPoint(controlling);
        auto expression = visit(headWhileStatement.getExpression());
        expression = boolToi1(expression);
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "while.continue", m_currentFunction);
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "while.body", m_currentFunction);
        m_builder.CreateCondBr(expression.value, body, contBlock);
        m_builder.SetInsertPoint(body);
        m_breakTargets[&headWhileStatement] = contBlock;
        visit(headWhileStatement.getStatement());
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(controlling);
        }
        m_builder.SetInsertPoint(contBlock);
    }

    void visit(const cld::Semantics::FootWhileStatement& footWhileStatement)
    {
        auto* controlling = llvm::BasicBlock::Create(m_module.getContext(), "do_while.controlling", m_currentFunction);
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "do_while.continue", m_currentFunction);
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "do_while.body", m_currentFunction);
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(body);
        }
        m_continueTargets[&footWhileStatement] = controlling;
        m_breakTargets[&footWhileStatement] = contBlock;
        m_builder.SetInsertPoint(body);
        visit(footWhileStatement.getStatement());
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(controlling);
        }
        m_builder.SetInsertPoint(controlling);
        auto expression = visit(footWhileStatement.getExpression());
        expression = boolToi1(expression);
        m_builder.CreateCondBr(expression.value, body, contBlock);
        m_builder.SetInsertPoint(contBlock);
    }

    void visit(const cld::Semantics::BreakStatement& breakStatement)
    {
        if (!m_builder.GetInsertBlock())
        {
            return;
        }
        runDestructors(breakStatement.getScope(),
                       cld::match(breakStatement.getBreakableStatement(), [](auto* ptr) { return ptr->getScope(); }));
        m_builder.CreateBr(m_breakTargets[breakStatement.getBreakableStatement()]);
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::ContinueStatement& continueStatement)
    {
        if (!m_builder.GetInsertBlock())
        {
            return;
        }
        runDestructors(continueStatement.getScope(),
                       cld::match(continueStatement.getLoopStatement(), [](auto* ptr) { return ptr->getScope(); }));
        m_builder.CreateBr(m_continueTargets[continueStatement.getLoopStatement()]);
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::SwitchStatement& switchStatement)
    {
        auto expression = m_builder.GetInsertBlock() ? visit(switchStatement.getExpression()) : nullptr;
        auto& switchData = m_switches[&switchStatement];
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "switch.continue", m_currentFunction);
        if (switchStatement.getDefaultStatement())
        {
            switchData.defaultBlock =
                llvm::BasicBlock::Create(m_module.getContext(), "switch.default", m_currentFunction);
        }
        auto* switchStmt =
            expression.value ?
                m_builder.CreateSwitch(expression.value, switchData.defaultBlock ? switchData.defaultBlock : contBlock,
                                       switchStatement.getCases().size()) :
                nullptr;
        switchData.llvmSwitch = switchStmt;
        m_builder.ClearInsertionPoint();
        m_breakTargets[&switchStatement] = contBlock;
        visit(switchStatement.getStatement());
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(contBlock);
        }
        m_builder.SetInsertPoint(contBlock);
    }

    void visit(const cld::Semantics::DefaultStatement& defaultStatement)
    {
        auto& switchData = m_switches[&defaultStatement.getSwitchStatement()];
        auto* bb = switchData.defaultBlock;
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(bb);
        }
        m_builder.SetInsertPoint(bb);
        visit(defaultStatement.getStatement());
    }

    void visit(const cld::Semantics::CaseStatement& caseStatement, llvm::BasicBlock* bb = nullptr)
    {
        auto& switchData = m_switches[&caseStatement.getSwitchStatement()];
        if (!bb && (switchData.llvmSwitch || m_builder.GetInsertBlock()))
        {
            bb = llvm::BasicBlock::Create(m_module.getContext(), "switch.case", m_currentFunction);
            if (m_builder.GetInsertBlock())
            {
                m_builder.CreateBr(bb);
            }
        }
        if (caseStatement.getStatement().is<cld::Semantics::CaseStatement>())
        {
            if (switchData.llvmSwitch)
            {
                llvm::Constant* val = llvm::ConstantInt::get(switchData.llvmSwitch->getCondition()->getType(),
                                                             caseStatement.getConstant());
                switchData.llvmSwitch->addCase(llvm::cast<llvm::ConstantInt>(val), bb);
            }
            visit(caseStatement.getStatement().cast<cld::Semantics::CaseStatement>(), bb);
            return;
        }
        if (bb)
        {
            if (switchData.llvmSwitch)
            {
                llvm::Constant* val = llvm::ConstantInt::get(switchData.llvmSwitch->getCondition()->getType(),
                                                             caseStatement.getConstant());
                switchData.llvmSwitch->addCase(llvm::cast<llvm::ConstantInt>(val), bb);
            }
            m_builder.SetInsertPoint(bb);
        }
        visit(caseStatement.getStatement());
    }

    void visit(const cld::Semantics::GotoStatement& gotoStatement)
    {
        if (!m_builder.GetInsertBlock())
        {
            return;
        }
        // Unlike in loops and other constructs a label can be anywhere in the whole function and the goto
        // can be anywhere in the whole function. Therefore we must find the first scope that both are part of.
        // That scope is the exclusive end of all scopes whose declarations must be destructed
        std::unordered_set<std::size_t> labelScopes;
        {
            auto currScope = gotoStatement.getLabel()->getScope();
            while (currScope != static_cast<std::size_t>(-1))
            {
                labelScopes.insert(currScope);
                currScope = m_programInterface.getScopes()[currScope].previousScope;
            }
        }
        auto commonScope = gotoStatement.getScope();
        while (commonScope != static_cast<std::size_t>(-1) && labelScopes.count(commonScope) == 0)
        {
            commonScope = m_programInterface.getScopes()[commonScope].previousScope;
        }

        runDestructors(gotoStatement.getScope(), commonScope);
        auto* bb = m_labels[gotoStatement.getLabel()];
        if (!bb)
        {
            bb = m_labels[gotoStatement.getLabel()] = llvm::BasicBlock::Create(
                m_module.getContext(), llvm::StringRef{gotoStatement.getLabel()->getIdentifier()->getText()},
                m_currentFunction);
        }
        m_builder.CreateBr(bb);
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::LabelStatement& labelStatement)
    {
        auto* bb = m_labels[&labelStatement];
        if (!bb)
        {
            bb = m_labels[&labelStatement] = llvm::BasicBlock::Create(
                m_module.getContext(), llvm::StringRef{labelStatement.getIdentifier()->getText()}, m_currentFunction);
        }
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(bb);
        }
        m_builder.SetInsertPoint(bb);
        visit(labelStatement.getStatement());
    }

    void visit(const cld::Semantics::GNUASMStatement&)
    {
        // TODO:
        llvm::errs() << "GNU ASM Statements are not yet implemented. Sorry";
        std::terminate();
    }

    Value visit(const cld::Semantics::ExpressionBase& expression)
    {
        if (m_currentDebugScope && !llvm::isa<llvm::DICompileUnit>(m_currentDebugScope))
        {
            m_builder.SetCurrentDebugLocation(getLocation(expression.begin()));
        }
        return expression.match([](const cld::Semantics::ErrorExpression&) -> Value { CLD_UNREACHABLE; },
                                [&](const auto& value) -> Value { return visit(value); });
    }

    Value visit(const cld::Semantics::Constant& constant)
    {
        auto* type = visit(constant.getType());
        if (std::holds_alternative<llvm::APSInt>(constant.getValue()))
        {
            return valueOf(llvm::Constant::getIntegerValue(type, cld::get<llvm::APSInt>(constant.getValue())));
        }
        if (std::holds_alternative<llvm::APFloat>(constant.getValue()))
        {
            return valueOf(llvm::ConstantFP::get(type, cld::get<llvm::APFloat>(constant.getValue())));
        }

        auto array = getStringLiteralData(type->getArrayElementType(), constant.getValue());
        auto* global =
            new llvm::GlobalVariable(m_module, array.value->getType(), true, llvm::GlobalValue::PrivateLinkage,
                                     llvm::cast<llvm::Constant>(array.value));
        global->setAlignment(array.alignment);
        global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        return global;
    }

    Value visit(const cld::Semantics::DeclarationRead& declarationRead)
    {
        auto result = declarationRead.getDeclRead().match(
            [&](const cld::Semantics::Declaration& declaration) { return m_lvalues.find(&declaration); },
            [&](const cld::Semantics::FunctionDefinition& functionDefinition) {
                return m_lvalues.find(&functionDefinition);
            },
            [&](const cld::Semantics::BuiltinFunction&) -> decltype(m_lvalues)::iterator { CLD_UNREACHABLE; });
        CLD_ASSERT(result != m_lvalues.end());
        return result->second;
    }

    Value visit(const cld::Semantics::Conversion& conversion)
    {
        auto value = visit(conversion.getExpression());
        switch (conversion.getKind())
        {
            case cld::Semantics::Conversion::LValue:
            {
                if (cld::Semantics::isArray(conversion.getExpression().getType())
                    && !cld::Semantics::isVariableLengthArray(conversion.getExpression().getType()))
                {
                    return createInBoundsGEP(value, {m_builder.getInt64(0), m_builder.getInt64(0)});
                }
                if (std::holds_alternative<cld::Semantics::FunctionType>(
                        conversion.getExpression().getType().getVariant())
                    || m_programInterface.isBitfieldAccess(conversion.getExpression())
                    || cld::Semantics::isVariableLengthArray(conversion.getExpression().getType()))
                {
                    return value;
                }

                return createLoad(value, conversion.getExpression().getType().isVolatile());
            }
            case cld::Semantics::Conversion::IntegerPromotion:
            {
                auto& prevType = conversion.getExpression().getType();
                if (cld::Semantics::isEnum(prevType))
                {
                    // This should be a noop for enums
                    return value;
                }
                return valueOf(
                    m_builder.CreateIntCast(value.value, visit(conversion.getType()),
                                            cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned()));
            }
            case cld::Semantics::Conversion::Implicit:
            {
                auto& prevType = conversion.getExpression().getType();
                auto& newType = conversion.getType();
                if (cld::Semantics::isBool(newType))
                {
                    return valueOf(m_builder.CreateIntCast(toBool(value.value), visit(newType), false));
                }
                if (std::holds_alternative<cld::Semantics::PointerType>(newType.getVariant()))
                {
                    if (cld::Semantics::isInteger(prevType))
                    {
                        return valueOf(m_builder.CreateIntToPtr(value.value, visit(newType)));
                    }
                    if (cld::Semantics::isArray(prevType))
                    {
                        return createInBoundsGEP(value, {m_builder.getInt64(0), m_builder.getInt64(0)});
                    }
                    return createPointerCast(value, visit(newType));
                }
                [[fallthrough]];
            }
            case cld::Semantics::Conversion::ArithmeticConversion:
            {
                auto& prevType = conversion.getExpression().getType();
                auto& newType = conversion.getType();
                if (cld::Semantics::isInteger(prevType) && cld::Semantics::isInteger(newType))
                {
                    return valueOf(m_builder.CreateIntCast(
                        value.value, visit(newType),
                        cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned()));
                }
                if (cld::Semantics::isInteger(prevType))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned())
                    {
                        return valueOf(m_builder.CreateSIToFP(value.value, visit(newType)));
                    }

                    return valueOf(m_builder.CreateUIToFP(value.value, visit(newType)));
                }
                if (cld::Semantics::isInteger(newType))
                {
                    if (std::holds_alternative<cld::Semantics::PointerType>(prevType.getVariant()))
                    {
                        return valueOf(m_builder.CreatePtrToInt(value.value, visit(newType)));
                    }
                    if (cld::get<cld::Semantics::PrimitiveType>(newType.getVariant()).isSigned())
                    {
                        return valueOf(m_builder.CreateFPToSI(value.value, visit(newType)));
                    }

                    return valueOf(m_builder.CreateFPToUI(value.value, visit(newType)));
                }
                return valueOf(m_builder.CreateFPCast(value.value, visit(newType)));
            }
            case cld::Semantics::Conversion::DefaultArgumentPromotion:
            {
                auto& prevType = conversion.getExpression().getType();
                if (cld::Semantics::isInteger(prevType))
                {
                    return valueOf(m_builder.CreateIntCast(
                        value.value, visit(conversion.getType()),
                        cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned()));
                }
                return valueOf(m_builder.CreateFPCast(value.value, visit(conversion.getType())));
            }
        }
        CLD_UNREACHABLE;
    }

    Value visit(const cld::Semantics::MemberAccess& memberAccess)
    {
        auto value = visit(memberAccess.getRecordExpression());
        auto& type =
            std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant()) ?
                cld::get<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().getVariant())
                    .getElementType() :
                memberAccess.getRecordExpression().getType();
        if (!std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant())
            && memberAccess.getRecordExpression().getValueCategory() != cld::Semantics::ValueCategory::Lvalue)
        {
            // Struct access is only ever allowed on pointers or lvalues except through the return value of a function
            // then it's also allowed to be an rvalue
            auto* load = llvm::cast<llvm::LoadInst>(value.value);
            value = Value(load->getPointerOperand(), load->getAlign());
            load->eraseFromParent();
        }
        auto& cldField = memberAccess.getField();
        auto indices = llvm::ArrayRef(cldField.indices);
        auto parentTypes = cldField.parentTypes;
        parentTypes.insert(parentTypes.begin(), std::make_shared<const cld::Semantics::Type>(type));
        Value field = value;
        for (auto iter = parentTypes.begin(); iter != parentTypes.end(); iter++)
        {
            auto index = iter - parentTypes.begin();
            if (cld::Semantics::isStruct(**iter))
            {
                field = createInBoundsGEP(field, {m_builder.getInt64(0), m_builder.getInt32(indices[index])});
            }
            else
            {
                auto* destTy = visit(iter + 1 == parentTypes.end() ? *cldField.type : **(iter + 1));
                field = createBitCast(field, llvm::PointerType::getUnqual(destTy));
            }
        }

        if (!cldField.bitFieldBounds)
        {
            // If the record expression is the return value of a function and this is a dot access not arrow access
            // we must load because an rvalue is returned and no lvalue conversion will load for us
            if (!std::holds_alternative<cld::Semantics::PointerType>(
                    memberAccess.getRecordExpression().getType().getVariant())
                && (memberAccess.getRecordExpression().getValueCategory() != cld::Semantics::ValueCategory::Lvalue))
            {
                // Arrays are generally passed around as llvm pointers to llvm arrays to be able to decay them to
                // pointers. Best example for this are string literals which for this reason are global variables
                if (std::holds_alternative<cld::Semantics::ArrayType>(cldField.type->getVariant()))
                {
                    return field;
                }
                return createLoad(field, type.isVolatile());
            }
            return field;
        }

        auto loaded = createLoad(field, memberAccess.getType().isVolatile());
        auto upLeft = loaded.value->getType()->getPrimitiveSizeInBits() - cldField.bitFieldBounds->second;
        auto* shl = m_builder.CreateShl(loaded.value, llvm::ConstantInt::get(loaded.value->getType(), upLeft));
        auto* shrConstant = llvm::ConstantInt::get(loaded.value->getType(), upLeft + cldField.bitFieldBounds->first);
        if (cld::get<cld::Semantics::PrimitiveType>(memberAccess.getType().getVariant()).isSigned())
        {
            return valueOf(m_builder.CreateAShr(shl, shrConstant));
        }

        return valueOf(m_builder.CreateLShr(shl, shrConstant));
    }

    Value visit(const cld::Semantics::BinaryOperator& binaryExpression)
    {
        auto lhs = visit(binaryExpression.getLeftExpression());
        switch (binaryExpression.getKind())
        {
            case cld::Semantics::BinaryOperator::Addition:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return add(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Subtraction:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return sub(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Multiply:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return mul(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Divide:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return div(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Modulo:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return mod(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::LeftShift:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return shl(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::RightShift:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return shr(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::GreaterThan:
            case cld::Semantics::BinaryOperator::LessOrEqual:
            case cld::Semantics::BinaryOperator::GreaterOrEqual:
            case cld::Semantics::BinaryOperator::Equal:
            case cld::Semantics::BinaryOperator::NotEqual:
            case cld::Semantics::BinaryOperator::LessThan:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                llvm::CmpInst::Predicate predicate;
                bool fp = cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType())
                          && !cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType());
                bool isSigned = cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType())
                                && cld::get<cld::Semantics::PrimitiveType>(
                                       binaryExpression.getLeftExpression().getType().getVariant())
                                       .isSigned();
                switch (binaryExpression.getKind())
                {
                    case cld::Semantics::BinaryOperator::GreaterThan:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UGT;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SGT;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_UGT;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::LessOrEqual:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_ULE;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SLE;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_ULE;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::GreaterOrEqual:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UGE;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SGE;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_UGE;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::Equal:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UEQ;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_EQ;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::NotEqual:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UGT;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_NE;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::LessThan:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_ULT;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SLT;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_ULT;
                        }
                        break;
                    default: CLD_UNREACHABLE;
                }
                auto* value = m_builder.CreateCmp(predicate, lhs, rhs);
                return m_builder.CreateZExt(value, visit(cld::Semantics::PrimitiveType::createInt(
                                                       false, false, m_sourceInterface.getLanguageOptions())));
            }
            case cld::Semantics::BinaryOperator::BitOr:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateOr(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::BitAnd:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateAnd(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::BitXor:
            {
                auto rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateXor(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::LogicAnd:
            {
                if (!m_currentFunction)
                {
                    if (llvm::cast<llvm::Constant>(lhs.value)->isNullValue())
                    {
                        return lhs;
                    }
                    return visit(binaryExpression.getRightExpression());
                }
                lhs = boolToi1(lhs);
                auto* falseBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.false", m_currentFunction);
                auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.true", m_currentFunction);
                auto* continueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.continue", m_currentFunction);
                m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
                m_builder.SetInsertPoint(trueBranch);
                auto rhs = visit(binaryExpression.getRightExpression());
                rhs = boolToi1(rhs);
                m_builder.CreateBr(continueBranch);
                trueBranch = m_builder.GetInsertBlock();
                m_builder.SetInsertPoint(falseBranch);
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(continueBranch);
                auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
                phi->addIncoming(m_builder.getFalse(), falseBranch);
                phi->addIncoming(rhs, trueBranch);
                return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
            }
            case cld::Semantics::BinaryOperator::LogicOr:
            {
                if (!m_currentFunction)
                {
                    if (!llvm::cast<llvm::Constant>(lhs.value)->isNullValue())
                    {
                        return lhs;
                    }
                    return visit(binaryExpression.getRightExpression());
                }
                lhs = boolToi1(lhs);
                auto* falseBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicOr.false", m_currentFunction);
                auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicOr.true", m_currentFunction);
                auto* continueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "logicOr.continue", m_currentFunction);
                m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
                m_builder.SetInsertPoint(falseBranch);
                auto rhs = visit(binaryExpression.getRightExpression());
                rhs = boolToi1(rhs);
                m_builder.CreateBr(continueBranch);
                falseBranch = m_builder.GetInsertBlock();
                m_builder.SetInsertPoint(trueBranch);
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(continueBranch);
                auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
                phi->addIncoming(m_builder.getTrue(), trueBranch);
                phi->addIncoming(rhs, falseBranch);
                return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
            }
        }
        CLD_UNREACHABLE;
    }

    Value visit(const cld::Semantics::Cast& cast)
    {
        auto value = visit(cast.getExpression());
        auto& prevType = cast.getExpression().getType();
        auto& newType = cast.getType();
        return this->cast(value, prevType, newType);
    }

    Value visit(const cld::Semantics::UnaryOperator& unaryOperator)
    {
        auto value = visit(unaryOperator.getOperand());
        bool isVolatile = unaryOperator.getOperand().getType().isVolatile();
        switch (unaryOperator.getKind())
        {
            case cld::Semantics::UnaryOperator::AddressOf:
            case cld::Semantics::UnaryOperator::Dereference:
                // The difference between address of and dereference is that an lvalue conversion follows a dereference
                return value;
            case cld::Semantics::UnaryOperator::PostIncrement:
            {
                auto prev = createLoad(value, isVolatile);
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        auto* result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                    else
                    {
                        auto* result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    auto* result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    auto result = createGEP(prev, {m_builder.getInt32(1)});
                    createStore(result, value, isVolatile);
                }
                return prev;
            }
            case cld::Semantics::UnaryOperator::PostDecrement:
            {
                auto prev = createLoad(value, isVolatile);
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        auto* result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                    else
                    {
                        auto* result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    auto* result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    auto result = createGEP(prev, {m_builder.getInt32(-1)});
                    createStore(result, value, isVolatile);
                }
                return prev;
            }
            case cld::Semantics::UnaryOperator::PreIncrement:
            {
                Value result = nullptr;
                auto prev = createLoad(value, isVolatile);
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                    else
                    {
                        result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    result = createGEP(prev, {m_builder.getInt32(1)});
                    createStore(result, value, isVolatile);
                }
                return result;
            }
            case cld::Semantics::UnaryOperator::PreDecrement:
            {
                Value result = nullptr;
                auto prev = createLoad(value, isVolatile);
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                    else
                    {
                        result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                        createStore(result, value, isVolatile);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    result = createGEP(prev, {m_builder.getInt32(-1)});
                    createStore(result, value, isVolatile);
                }
                return result;
            }
            case cld::Semantics::UnaryOperator::Plus: return value;
            case cld::Semantics::UnaryOperator::Minus:
            {
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        return m_builder.CreateNSWNeg(value);
                    }
                    return m_builder.CreateNeg(value);
                }

                return m_builder.CreateFNeg(value);
            }
            case cld::Semantics::UnaryOperator::BooleanNegate:
            {
                value = m_builder.CreateNot(boolToi1(value));
                return m_builder.CreateZExt(value, visit(cld::Semantics::PrimitiveType::createInt(
                                                       false, false, m_sourceInterface.getLanguageOptions())));
            }
            case cld::Semantics::UnaryOperator::BitwiseNegate: return m_builder.CreateNot(value);
        }
        CLD_UNREACHABLE;
    }

    Value visit(const cld::Semantics::SizeofOperator& sizeofOperator)
    {
        if (sizeofOperator.getSize())
        {
            auto* type = visit(
                cld::Semantics::PrimitiveType::createSizeT(false, false, m_programInterface.getLanguageOptions()));
            return llvm::ConstantInt::get(type, *sizeofOperator.getSize());
        }
        const cld::Semantics::Type& type = cld::match(
            sizeofOperator.getVariant(),
            [](const cld::Semantics::SizeofOperator::TypeVariant& typeVariant) -> const cld::Semantics::Type& {
                return typeVariant.type;
            },
            [](const cld::IntrVarPtr<cld::Semantics::ExpressionBase>& expression) -> const cld::Semantics::Type& {
                return expression->getType();
            });
        auto& elementType = [&]() -> decltype(auto) {
            auto* currType = &type;
            while (cld::Semantics::isArray(*currType))
            {
                currType = &cld::Semantics::getArrayElementType(*currType);
            }
            return *currType;
        }();
        llvm::Value* value = m_builder.getInt64(elementType.getSizeOf(m_programInterface));
        for (auto& iter : cld::Semantics::RecursiveVisitor(type, cld::Semantics::ARRAY_TYPE_NEXT_FN))
        {
            llvm::Value* temp;
            if (std::holds_alternative<cld::Semantics::ArrayType>(iter.getVariant()))
            {
                temp = m_builder.getInt64(cld::get<cld::Semantics::ArrayType>(iter.getVariant()).getSize());
            }
            else
            {
                temp = m_valSizes[cld::get<cld::Semantics::ValArrayType>(iter.getVariant()).getExpression()];
            }
            value = m_builder.CreateMul(value, temp);
        }
        return value;
    }

    Value visit(const cld::Semantics::SubscriptOperator& subscriptOperator)
    {
        auto& integer = cld::Semantics::isInteger(subscriptOperator.getLeftExpression().getType()) ?
                            subscriptOperator.getLeftExpression() :
                            subscriptOperator.getRightExpression();
        auto* pointer = &integer == &subscriptOperator.getLeftExpression() ? &subscriptOperator.getRightExpression() :
                                                                             &subscriptOperator.getLeftExpression();

        if (!pointer->is<cld::Semantics::Conversion>()
            || pointer->cast<cld::Semantics::Conversion>().getKind() != cld::Semantics::Conversion::LValue
            || !std::holds_alternative<cld::Semantics::ValArrayType>(
                pointer->cast<cld::Semantics::Conversion>().getExpression().getType().getVariant()))
        {
            auto llvmInteger = visit(integer);
            auto llvmPointer = visit(*pointer);

            llvmInteger = m_builder.CreateIntCast(
                llvmInteger, m_builder.getInt64Ty(),
                cld::get<cld::Semantics::PrimitiveType>(integer.getType().getVariant()).isSigned());
            return createGEP(llvmPointer, {llvmInteger});
        }

        std::vector<llvm::Value*> products = {m_builder.CreateIntCast(
            visit(integer), m_builder.getInt64Ty(),
            cld::get<cld::Semantics::PrimitiveType>(integer.getType().getVariant()).isSigned())};
        llvm::Value* dimensionProduct = nullptr;
        while (pointer->is<cld::Semantics::Conversion>()
               && pointer->cast<cld::Semantics::Conversion>().getKind() == cld::Semantics::Conversion::LValue)
        {
            auto& subExpr = pointer->cast<cld::Semantics::Conversion>().getExpression();
            if (!subExpr.is<cld::Semantics::SubscriptOperator>())
            {
                break;
            }
            auto& subOp = subExpr.cast<cld::Semantics::SubscriptOperator>();
            auto& subPointer = cld::Semantics::isInteger(subOp.getLeftExpression().getType()) ?
                                   subOp.getRightExpression() :
                                   subOp.getLeftExpression();
            auto& subInteger =
                &subPointer == &subOp.getLeftExpression() ? subOp.getRightExpression() : subOp.getLeftExpression();
            llvm::Value* newInt = visit(subInteger);
            newInt = m_builder.CreateIntCast(
                newInt, m_builder.getInt64Ty(),
                cld::get<cld::Semantics::PrimitiveType>(subInteger.getType().getVariant()).isSigned());
            llvm::Value* newDimension;
            if (std::holds_alternative<cld::Semantics::ArrayType>(subExpr.getType().getVariant()))
            {
                newDimension =
                    m_builder.getInt64(cld::get<cld::Semantics::ArrayType>(subExpr.getType().getVariant()).getSize());
            }
            else
            {
                newDimension =
                    m_valSizes[cld::get<cld::Semantics::ValArrayType>(subExpr.getType().getVariant()).getExpression()];
            }
            if (!dimensionProduct)
            {
                dimensionProduct = newDimension;
            }
            else
            {
                dimensionProduct = m_builder.CreateMul(dimensionProduct, newDimension);
            }
            products.push_back(m_builder.CreateMul(newInt, dimensionProduct));
            pointer = &subPointer;
        }
        auto basePointer = visit(*pointer);
        auto* sum = std::accumulate(products.begin() + 1, products.end(), products.front(),
                                    [&](llvm::Value* lhs, llvm::Value* rhs) { return m_builder.CreateAdd(lhs, rhs); });
        return createGEP(basePointer, {sum});
    }

    Value visit(const cld::Semantics::Conditional& conditional)
    {
        auto boolean = visit(conditional.getBoolExpression());
        boolean = boolToi1(boolean);
        if (!m_currentFunction)
        {
            // We are in a constant expression, most likely in an initializer constant expression
            auto* constant = llvm::cast<llvm::Constant>(boolean.value);
            if (constant->isNullValue())
            {
                return visit(conditional.getFalseExpression());
            }
            return visit(conditional.getTrueExpression());
        }

        auto* trueBranch = llvm::BasicBlock::Create(m_builder.getContext(), "cond.true", m_currentFunction);
        auto* falseBranch = llvm::BasicBlock::Create(m_builder.getContext(), "cond.false", m_currentFunction);
        auto* contBr = llvm::BasicBlock::Create(m_builder.getContext(), "cond.continue", m_currentFunction);
        m_builder.CreateCondBr(boolean, trueBranch, falseBranch);
        m_builder.SetInsertPoint(trueBranch);
        auto trueValue = visit(conditional.getTrueExpression());
        m_builder.CreateBr(contBr);
        trueBranch = m_builder.GetInsertBlock();
        m_builder.SetInsertPoint(falseBranch);
        auto falseValue = visit(conditional.getFalseExpression());
        m_builder.CreateBr(contBr);
        falseBranch = m_builder.GetInsertBlock();
        m_builder.SetInsertPoint(contBr);
        auto* phi = m_builder.CreatePHI(trueValue.value->getType(), 2);
        phi->addIncoming(trueValue, trueBranch);
        phi->addIncoming(falseValue, falseBranch);
        if (trueValue.value->getType()->isPointerTy())
        {
            return valueOf(phi, std::min(*trueValue.alignment, *falseValue.alignment));
        }
        return phi;
    }

    Value visit(const cld::Semantics::Assignment& assignment)
    {
        if (!m_programInterface.isBitfieldAccess(assignment.getLeftExpression()))
        {
            auto lhs = visit(assignment.getLeftExpression());
            auto rhs = visit(assignment.getRightExpression());
            if (assignment.getKind() != cld::Semantics::Assignment::Simple)
            {
                auto load = createLoad(lhs, assignment.getLeftExpression().getType().isVolatile());
                if (cld::Semantics::isArithmetic(assignment.getLeftExpression().getType()))
                {
                    load = cast(load, assignment.getLeftExpression().getType(), assignment.getLeftCalcType());
                }
                switch (assignment.getKind())
                {
                    case cld::Semantics::Assignment::Simple: CLD_UNREACHABLE;
                    case cld::Semantics::Assignment::Plus:
                        rhs = add(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Minus:
                        rhs = sub(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Divide:
                        rhs = div(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Multiply:
                        rhs = mul(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Modulo:
                        rhs = mod(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::LeftShift:
                        rhs = shl(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::RightShift:
                        rhs = shr(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::BitAnd: rhs = m_builder.CreateAnd(load, rhs); break;
                    case cld::Semantics::Assignment::BitOr: rhs = m_builder.CreateOr(load, rhs); break;
                    case cld::Semantics::Assignment::BitXor: rhs = m_builder.CreateXor(load, rhs); break;
                }
                rhs = cast(rhs, assignment.getRightExpression().getType(), assignment.getLeftExpression().getType());
            }
            createStore(rhs, lhs, assignment.getLeftExpression().getType().isVolatile());
            return createLoad(lhs, assignment.getLeftExpression().getType().isVolatile());
        }
        auto& memberAccess = assignment.getLeftExpression().cast<cld::Semantics::MemberAccess>();
        auto lhsRecord = visit(memberAccess.getRecordExpression());
        auto& type =
            std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant()) ?
                cld::get<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().getVariant())
                    .getElementType() :
                memberAccess.getRecordExpression().getType();
        auto rhsValue = visit(assignment.getRightExpression());

        auto& cldField = memberAccess.getField();
        auto field = lhsRecord;
        auto indices = llvm::ArrayRef(cldField.indices);
        auto parentTypes = cldField.parentTypes;
        parentTypes.insert(parentTypes.begin(), std::make_shared<const cld::Semantics::Type>(type));
        for (auto iter = parentTypes.begin(); iter != parentTypes.end(); iter++)
        {
            auto index = iter - parentTypes.begin();
            if (cld::Semantics::isStruct(type))
            {
                field = createInBoundsGEP(field, {m_builder.getInt64(0), m_builder.getInt32(indices[index])});
            }
            else
            {
                auto* destTy = visit(iter + 1 == parentTypes.end() ? *cldField.type : **(iter + 1));
                field = createBitCast(field, llvm::PointerType::getUnqual(destTy));
            }
        }

        auto loaded = createLoad(field, type.isVolatile());
        auto size = cldField.bitFieldBounds->second - cldField.bitFieldBounds->first;
        llvm::Value* mask = llvm::ConstantInt::get(rhsValue.value->getType(), (1u << size) - 1);
        if (assignment.getKind() != cld::Semantics::Assignment::Simple)
        {
            llvm::Value* load =
                m_builder.CreateAShr(loaded, llvm::ConstantInt::get(mask->getType(), cldField.bitFieldBounds->first));
            load = m_builder.CreateAnd(load, mask);
            load = cast(load, assignment.getLeftCalcType(), assignment.getRightExpression().getType());
            switch (assignment.getKind())
            {
                case cld::Semantics::Assignment::Simple: CLD_UNREACHABLE;
                case cld::Semantics::Assignment::Plus:
                    rhsValue =
                        add(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Minus:
                    rhsValue =
                        sub(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Divide:
                    rhsValue =
                        div(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Multiply:
                    rhsValue =
                        mul(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Modulo:
                    rhsValue =
                        mod(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::LeftShift:
                    rhsValue =
                        shl(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::RightShift:
                    rhsValue =
                        shr(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::BitAnd: rhsValue = m_builder.CreateAnd(load, rhsValue); break;
                case cld::Semantics::Assignment::BitOr: rhsValue = m_builder.CreateOr(load, rhsValue); break;
                case cld::Semantics::Assignment::BitXor: rhsValue = m_builder.CreateXor(load, rhsValue); break;
            }
        }
        rhsValue = m_builder.CreateAnd(rhsValue, mask);
        rhsValue = m_builder.CreateShl(
            rhsValue, llvm::ConstantInt::get(rhsValue.value->getType(), cldField.bitFieldBounds->first));
        mask = m_builder.CreateShl(mask, llvm::ConstantInt::get(mask->getType(), cldField.bitFieldBounds->first));
        mask = m_builder.CreateNot(mask);
        // TODO: Types could mismatch
        loaded = m_builder.CreateAnd(loaded, mask);
        auto* result = m_builder.CreateOr(loaded, rhsValue);
        createStore(result, field, type.isVolatile());
        return result;
    }

    Value visit(const cld::Semantics::CommaExpression& commaExpression)
    {
        for (auto& iter : commaExpression.getCommaExpressions())
        {
            visitVoidExpression(*iter.first);
        }
        return visit(commaExpression.getLastExpression());
    }

    Value visit(const cld::Semantics::CallExpression& call)
    {
        if (isBuiltinFunctionCall(call.getFunctionExpression()))
        {
            auto builtin = getBuiltinFunctionCall(call.getFunctionExpression());
            switch (builtin.getKind())
            {
                case cld::Semantics::BuiltinFunction::VAStart:
                {
                    auto list = visit(*call.getArgumentExpressions()[0]);
                    if (llvm::isa<llvm::LoadInst>(list.value))
                    {
                        auto* prev = llvm::cast<llvm::LoadInst>(list.value);
                        list = Value(prev->getPointerOperand(), prev->getAlign());
                        prev->eraseFromParent();
                    }
                    list = createBitCast(list, m_builder.getInt8PtrTy());
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::vastart, {}, {list});
                }
                case cld::Semantics::BuiltinFunction::VAEnd:
                {
                    auto list = visit(*call.getArgumentExpressions()[0]);
                    if (llvm::isa<llvm::LoadInst>(list.value))
                    {
                        auto* prev = llvm::cast<llvm::LoadInst>(list.value);
                        list = Value(prev->getPointerOperand(), prev->getAlign());
                        prev->eraseFromParent();
                    }
                    list = createBitCast(list, m_builder.getInt8PtrTy());
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::vaend, {}, {list});
                }
                case cld::Semantics::BuiltinFunction::VACopy:
                {
                    auto list1 = visit(*call.getArgumentExpressions()[0]);
                    if (llvm::isa<llvm::LoadInst>(list1.value))
                    {
                        auto* prev = llvm::cast<llvm::LoadInst>(list1.value);
                        list1 = Value(prev->getPointerOperand(), prev->getAlign());
                        prev->eraseFromParent();
                    }
                    list1 = createBitCast(list1, m_builder.getInt8PtrTy());
                    auto list2 = visit(*call.getArgumentExpressions()[1]);
                    if (llvm::isa<llvm::LoadInst>(list2.value))
                    {
                        auto* prev = llvm::cast<llvm::LoadInst>(list2.value);
                        list2 = Value(prev->getPointerOperand(), prev->getAlign());
                        prev->eraseFromParent();
                    }
                    list2 = createBitCast(list2, m_builder.getInt8PtrTy());
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::vacopy, {}, {list1, list2});
                }
                case cld::Semantics::BuiltinFunction::LLAbs:
                case cld::Semantics::BuiltinFunction::LAbs:
                case cld::Semantics::BuiltinFunction::Abs:
                {
                    auto longLong = visit(*call.getArgumentExpressions()[0]);
                    auto* neg = m_builder.CreateNSWNeg(longLong);
                    auto* isNegative =
                        m_builder.CreateICmpSLT(longLong, llvm::ConstantInt::get(longLong.value->getType(), 0));
                    return m_builder.CreateSelect(isNegative, neg, longLong);
                }
                case cld::Semantics::BuiltinFunction::FAbs:
                case cld::Semantics::BuiltinFunction::FAbsf:
                case cld::Semantics::BuiltinFunction::FAbsl:
                {
                    auto floatingPoint = visit(*call.getArgumentExpressions()[0]);
                    return m_builder.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, floatingPoint);
                }
                case cld::Semantics::BuiltinFunction::Inf:
                    return llvm::ConstantFP::getInfinity(m_builder.getDoubleTy());
                case cld::Semantics::BuiltinFunction::Inff:
                    return llvm::ConstantFP::getInfinity(m_builder.getFloatTy());
                case cld::Semantics::BuiltinFunction::Infl:
                {
                    auto* type = visit(cld::Semantics::PrimitiveType::createLongDouble(
                        false, false, m_sourceInterface.getLanguageOptions()));
                    return llvm::ConstantFP::getInfinity(type);
                }
                case cld::Semantics::BuiltinFunction::SyncSynchronize:
                    return m_builder.CreateFence(llvm::AtomicOrdering::SequentiallyConsistent);
                case cld::Semantics::BuiltinFunction::ReturnAddress:
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::returnaddress, {},
                                                     {visit(*call.getArgumentExpressions()[0])});
                case cld::Semantics::BuiltinFunction::ExtractReturnAddr:
                    // TODO:
                    return visit(*call.getArgumentExpressions()[0]);
                case cld::Semantics::BuiltinFunction::FRobReturnAddr:
                    // TODO:
                    return visit(*call.getArgumentExpressions()[0]);
                case cld::Semantics::BuiltinFunction::FrameAddress:
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::frameaddress, {},
                                                     {visit(*call.getArgumentExpressions()[0])});
                case cld::Semantics::BuiltinFunction::ExpectWithProbability:
                {
                    auto ret = visit(*call.getArgumentExpressions()[0]);
                    auto expected = visit(*call.getArgumentExpressions()[1]);
                    auto probability = visit(*call.getArgumentExpressions()[2]);
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::expect_with_probability, {ret.value->getType()},
                                                     {ret, expected, probability});
                }
                case cld::Semantics::BuiltinFunction::Expect:
                {
                    auto ret = visit(*call.getArgumentExpressions()[0]);
                    auto expected = visit(*call.getArgumentExpressions()[1]);
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::expect, {ret.value->getType()}, {ret, expected});
                }
                case cld::Semantics::BuiltinFunction::ClearCache:
                {
                    auto first = visit(*call.getArgumentExpressions()[0]);
                    auto second = visit(*call.getArgumentExpressions()[1]);
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::clear_cache, {}, {first, second});
                }
                case cld::Semantics::BuiltinFunction::Prefetch:
                {
                    auto address = visit(*call.getArgumentExpressions()[0]);
                    llvm::Value* rw = m_builder.getInt32(0);
                    llvm::Value* locality = m_builder.getInt32(3);
                    if (call.getArgumentExpressions().size() > 1)
                    {
                        auto rwValue = visit(*call.getArgumentExpressions()[1]);
                        rw = m_builder.CreateIntCast(rwValue.value, m_builder.getInt32Ty(), false);
                    }
                    if (call.getArgumentExpressions().size() > 2)
                    {
                        auto localityValue = visit(*call.getArgumentExpressions()[2]);
                        locality = m_builder.CreateIntCast(localityValue.value, m_builder.getInt32Ty(), false);
                    }

                    auto* i8Star = m_builder.CreateBitCast(address, m_builder.getInt8PtrTy());
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::prefetch, {i8Star->getType()},
                                                     {i8Star, rw, locality, m_builder.getInt32(1)});
                }
                case cld::Semantics::BuiltinFunction::Unreachable: return m_builder.CreateUnreachable();
                case cld::Semantics::BuiltinFunction::Trap:
                    return m_builder.CreateIntrinsic(llvm::Intrinsic::trap, {}, {});
            }
            CLD_UNREACHABLE;
        }
        auto function = visit(call.getFunctionExpression());
        auto cldFt = cld::get<cld::Semantics::FunctionType>(
            cld::get<cld::Semantics::PointerType>(call.getFunctionExpression().getType().getVariant())
                .getElementType()
                .getVariant());
        auto* ft = llvm::cast<llvm::FunctionType>(function.value->getType()->getPointerElementType());
        auto transformation = m_functionABITransformations.find(cldFt);
        CLD_ASSERT(transformation != m_functionABITransformations.end());
        bool isKandR = cldFt.isKandR();
        if (isKandR || cldFt.isLastVararg())
        {
            std::vector<std::pair<cld::Semantics::Type, std::string_view>> arguments;
            for (auto& iter : call.getArgumentExpressions())
            {
                arguments.emplace_back(iter->getType(), "");
            }
            auto callerFt =
                cld::Semantics::FunctionType::create(cldFt.getReturnType(), std::move(arguments), false, false);
            ft = llvm::cast<llvm::FunctionType>(visit(callerFt));
            cldFt = cld::get<cld::Semantics::FunctionType>(callerFt.getVariant());
            transformation = m_functionABITransformations.find(cldFt);
            CLD_ASSERT(transformation != m_functionABITransformations.end());
        }

        std::size_t llvmFnI = 0;
        std::vector<llvm::Value*> arguments;
        llvm::AllocaInst* returnSlot = nullptr;
        if (transformation->second.returnType == ABITransformations::PointerToTemporary)
        {
            llvmFnI = 1;

            returnSlot = createAllocaAtTop(ft->getParamType(0)->getPointerElementType(), "ret");
            returnSlot->setAlignment(llvm::Align(call.getType().getAlignOf(m_programInterface)));
            m_builder.CreateLifetimeStart(returnSlot, m_builder.getInt64(call.getType().getSizeOf(m_programInterface)));
            arguments.emplace_back(returnSlot);
        }

        for (auto iter = call.getArgumentExpressions().begin(); iter != call.getArgumentExpressions().end(); iter++)
        {
            const std::size_t currentIndex = iter - call.getArgumentExpressions().begin();
            auto value = visit(**iter);
            if (std::holds_alternative<ABITransformations::Change>(transformation->second.arguments[currentIndex]))
            {
                llvmFnI++;
                auto change = cld::get<ABITransformations::Change>(transformation->second.arguments[currentIndex]);
                if (change == ABITransformations::Unchanged)
                {
                    arguments.emplace_back(value);
                    continue;
                }
                if (change == ABITransformations::OnStack)
                {
                    // structs rvalues don't exist in LLVM IR so this should be sound?
                    auto* load = llvm::cast<llvm::LoadInst>(value.value);
                    arguments.emplace_back(load->getPointerOperand());
                    load->eraseFromParent();
                    continue;
                }
                if (change == ABITransformations::PointerToTemporary)
                {
                    auto* ret = createAllocaAtTop(value.value->getType());
                    ret->setAlignment(llvm::Align((*iter)->getType().getAlignOf(m_programInterface)));
                    m_builder.CreateLifetimeStart(ret,
                                                  m_builder.getInt64((*iter)->getType().getSizeOf(m_programInterface)));
                    if (value.value->getType()->isX86_FP80Ty())
                    {
                        createStore(value, ret, false);
                    }
                    else
                    {
                        auto* load = llvm::cast<llvm::LoadInst>(value.value);
                        m_builder.CreateMemCpy(ret, ret->getAlign(), load->getPointerOperand(), load->getAlign(),
                                               (*iter)->getType().getSizeOf(m_programInterface));
                        load->eraseFromParent();
                    }
                    arguments.emplace_back(ret);
                    continue;
                }
                // Integer register
                auto* load = llvm::cast<llvm::LoadInst>(value.value);
                auto integer = createBitCast(valueOf(load->getPointerOperand(), load->getAlign()),
                                             llvm::PointerType::getUnqual(m_builder.getIntNTy(
                                                 m_module.getDataLayout().getTypeAllocSizeInBits(load->getType()))),
                                             false);
                arguments.emplace_back(createLoad(integer, load->isVolatile()));
                load->eraseFromParent();
                continue;
            }
            auto* load = llvm::cast<llvm::LoadInst>(value.value);
            auto& multiIndex =
                cld::get<ABITransformations::MultipleArgs>(transformation->second.arguments[currentIndex]);
            if (multiIndex.size == 1)
            {
                auto* paramType = ft->getParamType(llvmFnI);
                auto cast = createSafeBitCast(valueOf(load->getPointerOperand(), load->getAlign()),
                                              llvm::PointerType::getUnqual(paramType));
                arguments.emplace_back(createLoad(cast, load->isVolatile()));
            }
            else
            {
                CLD_ASSERT(multiIndex.size == 2);
                auto* firstType = ft->getParamType(llvmFnI);
                auto* secondType = ft->getParamType(llvmFnI + 1);
                auto cast =
                    createSafeBitCast(valueOf(load->getPointerOperand(), load->getAlign()),
                                      llvm::PointerType::getUnqual(llvm::StructType::get(firstType, secondType)));
                auto firstValue = createInBoundsGEP(cast, {m_builder.getInt64(0), m_builder.getInt32(0)});
                auto secondValue = createInBoundsGEP(cast, {m_builder.getInt64(0), m_builder.getInt32(1)});
                arguments.emplace_back(createLoad(firstValue, load->isVolatile()));
                arguments.emplace_back(createLoad(secondValue, load->isVolatile()));
            }
            load->eraseFromParent();
            llvmFnI += multiIndex.size;
        }
        if (isKandR)
        {
            function = createBitCast(function, llvm::PointerType::getUnqual(ft));
        }
        auto* result = m_builder.CreateCall(
            isKandR ? ft : llvm::cast<llvm::FunctionType>(function.value->getType()->getPointerElementType()), function,
            arguments);
        applyFunctionAttributes(*result, ft, cldFt);
        switch (transformation->second.returnType)
        {
            case ABITransformations::Unchanged: return valueOf(result);
            case ABITransformations::PointerToTemporary:
            {
                return createLoad(returnSlot, false);
            }
            case ABITransformations::IntegerRegister:
            case ABITransformations::Flattened:
            {
                auto* intValue = createAllocaAtTop(result->getType());
                intValue->setAlignment(llvm::Align(m_module.getDataLayout().getABITypeAlign(result->getType())));
                m_builder.CreateLifetimeStart(
                    intValue, m_builder.getInt64(m_module.getDataLayout().getTypeAllocSize(result->getType())));
                createStore(result, intValue, false);

                auto cast = createSafeBitCast(intValue, llvm::PointerType::getUnqual(visit(call.getType())));
                return createLoad(cast, false);
            }
            default: CLD_UNREACHABLE;
        }
    }

    Value visit(const cld::Semantics::CompoundLiteral& compoundLiteral)
    {
        auto* type = visit(compoundLiteral.getType());
        if (compoundLiteral.hasStaticLifetime())
        {
            llvm::Constant* constant = nullptr;
            constant = llvm::cast<llvm::Constant>(
                visit(compoundLiteral.getInitializer(), compoundLiteral.getType(), type).value);
            type = constant->getType();
            auto* global = new llvm::GlobalVariable(m_module, type, true, llvm::GlobalValue::PrivateLinkage, constant);
            global->setAlignment(llvm::MaybeAlign(compoundLiteral.getType().getAlignOf(m_programInterface)));
            return global;
        }

        auto* var = createAllocaAtTop(type);
        var->setAlignment(llvm::Align(compoundLiteral.getType().getAlignOf(m_programInterface)));
        if (m_builder.GetInsertBlock())
        {
            visit(compoundLiteral.getInitializer(), compoundLiteral.getType(), var);
        }
        return var;
    }

    Value x64LoadFromStack(Value vaList, llvm::Type* destType)
    {
        auto overflowArea = createInBoundsGEP(vaList, {m_builder.getInt64(0), m_builder.getInt32(2)});
        auto loadedStackPointer = createLoad(overflowArea, false);
        loadedStackPointer.alignment = llvm::Align(8);
        if (m_module.getDataLayout().getABITypeAlign(destType) >= 16)
        {
            // Align to 16 bytes
            llvm::IntegerType* intPtrType = m_module.getDataLayout().getIntPtrType(m_module.getContext(), 0);
            auto* temp = m_builder.CreatePtrToInt(loadedStackPointer, intPtrType);
            temp = m_builder.CreateAdd(temp, llvm::ConstantInt::get(intPtrType, 15));
            auto mask = llvm::APInt::getAllOnesValue(intPtrType->getBitWidth());
            mask.clearLowBits(4);
            temp = m_builder.CreateAnd(temp, llvm::ConstantInt::get(intPtrType, mask));
            loadedStackPointer =
                valueOf(m_builder.CreateIntToPtr(temp, loadedStackPointer.value->getType()), llvm::Align(16));
        }

        auto incremented = createGEP(loadedStackPointer,
                                     m_builder.getInt64(cld::roundUpTo(
                                         m_module.getDataLayout().getTypeAllocSize(destType).getKnownMinSize(), 8)));
        loadedStackPointer = createBitCast(loadedStackPointer, llvm::PointerType::getUnqual(destType));
        createStore(incremented, overflowArea, false);
        return loadedStackPointer;
    }

    Value visit(const cld::Semantics::BuiltinVAArg& vaArg)
    {
        auto vaList = visit(vaArg.getExpression());
        std::size_t sizeOf = vaArg.getType().getSizeOf(m_programInterface);

        switch (m_programInterface.getLanguageOptions().vaListKind)
        {
            case cld::LanguageOptions::BuiltInVaList::CharPtr:
            case cld::LanguageOptions::BuiltInVaList::VoidPtr:
            {
                auto increment =
                    createInBoundsGEP(vaList, {m_builder.getInt64(m_module.getDataLayout().getPointerSize(0))});
                vaList.alignment = llvm::Align(8);
                createStore(increment,
                            Value(llvm::cast<llvm::LoadInst>(vaList.value)->getPointerOperand(),
                                  llvm::cast<llvm::LoadInst>(vaList.value)->getAlign()),
                            false);
                auto* destType = visit(vaArg.getType());

                auto exprAlign = vaArg.getType().getAlignOf(m_programInterface);
                if (m_triple.getPlatform() == cld::Platform::Windows
                    && m_triple.getArchitecture() == cld::Architecture::x86_64)
                {
                    if (!destType->isStructTy() && !destType->isX86_FP80Ty())
                    {
                        vaList = createBitCast(vaList, llvm::PointerType::getUnqual(destType));
                        return createLoad(vaList, false);
                    }
                    if (!m_module.getDataLayout().isLegalInteger(sizeOf * 8))
                    {
                        vaList =
                            createBitCast(vaList, llvm::PointerType::getUnqual(llvm::PointerType::getUnqual(destType)));
                        vaList = createLoad(vaList, false);
                    }
                }

                auto* allocaInst = createAllocaAtTop(destType, "va_arg.ret");
                allocaInst->setAlignment(llvm::Align(exprAlign));
                m_builder.CreateLifetimeStart(allocaInst, m_builder.getInt64(sizeOf));
                m_builder.CreateMemCpy(allocaInst, allocaInst->getAlign(), vaList, *vaList.alignment, sizeOf);
                return createLoad(allocaInst, false);
            }
            case cld::LanguageOptions::BuiltInVaList::x86_64ABI:
            {
                auto* destType = visit(vaArg.getType());
                if (m_module.getDataLayout().getTypeAllocSizeInBits(destType) > 128)
                {
                OnStack:
                    auto loadedStackPointer = x64LoadFromStack(vaList, destType);

                    auto* allocaInst = createAllocaAtTop(destType, "va_arg.ret");
                    allocaInst->setAlignment(llvm::Align(vaArg.getType().getAlignOf(m_programInterface)));
                    m_builder.CreateLifetimeStart(allocaInst, m_builder.getInt64(sizeOf));
                    m_builder.CreateMemCpy(allocaInst, allocaInst->getAlign(), loadedStackPointer,
                                           *loadedStackPointer.alignment, sizeOf);
                    return createLoad(allocaInst, false);
                }
                auto [transform, first, second] = flattenSingleArg(destType);
                if (auto* change = std::get_if<ABITransformations::Change>(&transform);
                    change && *change == ABITransformations::OnStack)
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
                    gpOffset = createInBoundsGEP(vaList, {m_builder.getInt64(0), m_builder.getInt32(0)});
                    gpCount = createLoad(gpOffset, false);
                    // if the offset is 48 bytes it is full (8 Bytes * 6 Integer registers). To fit two integers we
                    // need an offset of 32, to fit one we need max 40
                    auto maxOffset = first->isIntOrPtrTy() && second && second->isIntOrPtrTy() ? 32 : 40;
                    cond = m_builder.CreateICmpULE(gpCount, m_builder.getInt32(maxOffset));
                }
                if (first->isFPOrFPVectorTy() || (second && second->isFPOrFPVectorTy()))
                {
                    fpOffset = createInBoundsGEP(vaList, {m_builder.getInt64(0), m_builder.getInt32(1)});
                    fpCount = createLoad(fpOffset, false);
                    // fpOffset comes after gpOffset therefore all fp registers are used when fpOffset
                    // is 8 Bytes * 6 Integer registers + 16 Bytes * 8 Floating point registers
                    auto maxOffset = (first->isFPOrFPVectorTy() && second && second->isFPOrFPVectorTy()) ? 144 : 160;
                    if (!cond)
                    {
                        cond = m_builder.CreateICmpULE(fpCount, m_builder.getInt32(maxOffset));
                    }
                    else
                    {
                        cond =
                            m_builder.CreateAnd(cond, m_builder.CreateICmpULE(fpCount, m_builder.getInt32(maxOffset)));
                    }
                }
                CLD_ASSERT(cond);

                auto* inRegister =
                    llvm::BasicBlock::Create(m_module.getContext(), "va_arg.inRegister", m_currentFunction);
                auto* onStack = llvm::BasicBlock::Create(m_module.getContext(), "va_arg.onStack", m_currentFunction);
                auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "va_arg.continue", m_currentFunction);
                m_builder.CreateCondBr(cond, inRegister, onStack);

                m_builder.SetInsertPoint(inRegister);
                auto regArea = createInBoundsGEP(vaList, {m_builder.getInt64(0), m_builder.getInt32(3)});
                regArea = createLoad(regArea, false);
                Value regValue = nullptr;
                if (!second)
                {
                    if (first->isIntOrPtrTy())
                    {
                        // TODO: Handle case where types is aligned higher than 8 bytes;
                        regArea = createGEP(regArea, gpCount);
                        regArea.alignment = llvm::Align(8);
                        regValue = createBitCast(regArea, llvm::PointerType::getUnqual(destType));
                        auto* incremented = m_builder.CreateAdd(gpCount, m_builder.getInt32(8));
                        createStore(incremented, gpOffset, false);
                        m_builder.CreateBr(contBlock);
                    }
                    else
                    {
                        // TODO: Handle case where types is aligned higher than 16 bytes
                        regArea = createGEP(regArea, fpCount);
                        regArea.alignment = llvm::Align(16);
                        regValue = createBitCast(regArea, llvm::PointerType::getUnqual(destType));

                        auto* incremented = m_builder.CreateAdd(fpCount, m_builder.getInt32(16));
                        createStore(incremented, fpOffset, false);
                        m_builder.CreateBr(contBlock);
                    }
                }
                else
                {
                    auto* allocaInst = createAllocaAtTop(destType, "va_arg.temp");
                    allocaInst->setAlignment(llvm::Align(vaArg.getType().getAlignOf(m_programInterface)));
                    m_builder.CreateLifetimeStart(allocaInst, m_builder.getInt64(sizeOf));
                    auto dest = createBitCast(
                        allocaInst, llvm::PointerType::getUnqual(llvm::StructType::get(first, second)), false);

                    auto regAreaOfFirst = createGEP(regArea, first->isFPOrFPVectorTy() ? fpCount : gpCount);
                    regAreaOfFirst.alignment = llvm::Align(first->isFPOrFPVectorTy() ? 16 : 8);
                    regAreaOfFirst = createBitCast(regAreaOfFirst, llvm::PointerType::getUnqual(first));

                    regAreaOfFirst = createLoad(regAreaOfFirst, false);
                    auto destFirst = createInBoundsGEP(dest, {m_builder.getInt64(0), m_builder.getInt32(0)});
                    createStore(regAreaOfFirst, destFirst, false);

                    llvm::Value* offsetOfSecond = second->isFPOrFPVectorTy() ? fpCount : gpCount;
                    // If both are initialized then we are loading from one floating point and one integer register.
                    // Otherwise we are loading from two registers. In that case we need to apply an extra offset
                    // as we have already loaded previously
                    if (static_cast<bool>(fpOffset) != static_cast<bool>(gpOffset))
                    {
                        offsetOfSecond = m_builder.CreateAdd(
                            offsetOfSecond, llvm::ConstantInt::get(offsetOfSecond->getType(), fpOffset ? 16 : 8));
                    }

                    auto regAreaOfSecond = createGEP(regArea, offsetOfSecond);
                    regAreaOfSecond.alignment = llvm::Align(second->isFPOrFPVectorTy() ? 16 : 8);
                    regAreaOfSecond = createBitCast(regAreaOfSecond, llvm::PointerType::getUnqual(second));

                    regAreaOfSecond = createLoad(regAreaOfSecond, false);
                    auto destSecond = createInBoundsGEP(dest, {m_builder.getInt64(0), m_builder.getInt32(1)});
                    createStore(regAreaOfSecond, destSecond, false);

                    if (static_cast<bool>(fpOffset) != static_cast<bool>(gpOffset))
                    {
                        if (fpOffset)
                        {
                            fpCount = m_builder.CreateAdd(fpCount, llvm::ConstantInt::get(fpCount->getType(), 32));
                            createStore(fpCount, fpOffset, false);
                        }
                        else
                        {
                            gpCount = m_builder.CreateAdd(gpCount, llvm::ConstantInt::get(gpCount->getType(), 16));
                            createStore(gpCount, gpOffset, false);
                        }
                    }
                    else
                    {
                        fpCount = m_builder.CreateAdd(fpCount, llvm::ConstantInt::get(fpCount->getType(), 16));
                        createStore(fpCount, fpOffset, false);
                        gpCount = m_builder.CreateAdd(gpCount, llvm::ConstantInt::get(gpCount->getType(), 8));
                        createStore(gpCount, gpOffset, false);
                    }
                    regValue = allocaInst;
                    m_builder.CreateBr(contBlock);
                }

                m_builder.SetInsertPoint(onStack);
                auto stackValue = x64LoadFromStack(vaList, destType);
                m_builder.CreateBr(contBlock);

                m_builder.SetInsertPoint(contBlock);
                auto* phi = m_builder.CreatePHI(llvm::PointerType::getUnqual(destType), 2);
                phi->addIncoming(regValue, inRegister);
                phi->addIncoming(stackValue, onStack);

                if (!cld::Semantics::isRecord(vaArg.getType()))
                {
                    return createLoad(valueOf(phi, std::min(*regValue.alignment, *stackValue.alignment)), false);
                }

                auto* allocaInst = createAllocaAtTop(destType, "va_arg.ret");
                allocaInst->setAlignment(llvm::Align(vaArg.getType().getAlignOf(m_programInterface)));
                m_builder.CreateLifetimeStart(allocaInst, m_builder.getInt64(sizeOf));
                m_builder.CreateMemCpy(allocaInst, allocaInst->getAlign(), phi,
                                       std::min(*regValue.alignment, *stackValue.alignment), sizeOf);
                return createLoad(allocaInst, false);
            }
        }
        CLD_UNREACHABLE;
    }

    Value visit(const cld::Semantics::BuiltinOffsetOf& offsetOf)
    {
        return llvm::ConstantInt::get(visit(offsetOf.getType()), offsetOf.getOffset());
    }

    Value visit(const cld::Semantics::Initializer& initializer, const cld::Semantics::Type& type,
                std::variant<Value, llvm::Type*> pointer)
    {
        return cld::match(
            initializer,
            [&](const cld::IntrVarPtr<cld::Semantics::ExpressionBase>& expression) -> Value {
                if (std::holds_alternative<Value>(pointer))
                {
                    auto value = visit(*expression);
                    if (cld::Semantics::isStringLiteralExpr(*expression))
                    {
                        m_builder.CreateMemCpy(cld::get<Value>(pointer), cld::get<Value>(pointer).alignment, value,
                                               value.alignment, expression->getType().getSizeOf(m_programInterface));
                        return nullptr;
                    }
                    createStore(value, cld::get<Value>(pointer), type.isVolatile());
                    return nullptr;
                }
                if (cld::Semantics::isStringLiteralExpr(*expression))
                {
                    auto& constant = expression->cast<cld::Semantics::Constant>();
                    return getStringLiteralData(visit(expression->getType())->getArrayElementType(),
                                                constant.getValue());
                }
                return visit(*expression);
            },
            [&](const cld::Semantics::InitializerList& initializerList) -> Value {
                if (std::holds_alternative<llvm::Type*>(pointer))
                {
                    return visitStaticInitializerList(initializerList, type, cld::get<llvm::Type*>(pointer));
                }
                auto value = cld::get<Value>(pointer);
                m_builder.CreateMemSet(value, m_builder.getInt8(0), type.getSizeOf(m_programInterface), value.alignment,
                                       type.isVolatile());
                for (auto& [path, expression] : initializerList.getFields())
                {
                    auto subValue = visit(*expression);
                    auto currentPointer = value;
                    const cld::Semantics::Type* currentType = &type;
                    std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
                    for (auto iter : path)
                    {
                        if (cld::Semantics::isStruct(*currentType))
                        {
                            auto fieldLayout = m_programInterface.getFieldLayout(*currentType);
                            currentPointer =
                                createInBoundsGEP(currentPointer, {m_builder.getInt64(0),
                                                                   m_builder.getInt32(fieldLayout[iter].layoutIndex)});
                            currentType = fieldLayout[iter].type.get();
                            bitFieldBounds = fieldLayout[iter].bitFieldBounds;
                        }
                        else if (cld::Semantics::isUnion(*currentType))
                        {
                            auto fields = m_programInterface.getFieldLayout(*currentType);
                            currentType = fields[iter].type.get();
                            currentPointer =
                                createBitCast(currentPointer, llvm::PointerType::getUnqual(visit(*currentType)));
                            bitFieldBounds = fields[iter].bitFieldBounds;
                        }
                        else
                        {
                            currentType = &cld::Semantics::getArrayElementType(*currentType);
                            currentPointer =
                                createInBoundsGEP(currentPointer, {m_builder.getInt64(0), m_builder.getInt64(iter)});
                        }
                    }
                    if (!bitFieldBounds)
                    {
                        if (cld::Semantics::isStringLiteralExpr(*expression))
                        {
                            m_builder.CreateMemCpy(currentPointer, currentPointer.alignment, subValue,
                                                   subValue.alignment,
                                                   expression->getType().getSizeOf(m_programInterface));
                            continue;
                        }
                        createStore(subValue, currentPointer, type.isVolatile());
                        continue;
                    }
                    llvm::Value* loaded = createLoad(currentPointer, type.isVolatile());
                    auto size = bitFieldBounds->second - bitFieldBounds->first;
                    llvm::Value* mask = llvm::ConstantInt::get(subValue.value->getType(), (1u << size) - 1);
                    subValue = m_builder.CreateAnd(subValue, mask);
                    subValue = m_builder.CreateShl(
                        subValue, llvm::ConstantInt::get(subValue.value->getType(), bitFieldBounds->first));
                    mask = m_builder.CreateShl(mask, llvm::ConstantInt::get(mask->getType(), bitFieldBounds->first));
                    mask = m_builder.CreateNot(mask);
                    loaded = m_builder.CreateAnd(loaded, mask);
                    auto* result = m_builder.CreateOr(loaded, subValue);
                    createStore(result, currentPointer, type.isVolatile());
                    continue;
                }
                return nullptr;
            });
    }
};
} // namespace

std::unique_ptr<llvm::TargetMachine> cld::CGLLVM::generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                               Triple triple, const Options& options)
{
    llvm::Triple llvmTriple;
    switch (triple.getArchitecture())
    {
        case cld::Architecture::x86: llvmTriple.setArch(llvm::Triple::ArchType::x86); break;
        case cld::Architecture::x86_64: llvmTriple.setArch(llvm::Triple::ArchType::x86_64); break;
        case cld::Architecture::Unknown: llvmTriple.setArch(llvm::Triple::ArchType::UnknownArch); break;
    }
    switch (triple.getPlatform())
    {
        case cld::Platform::Windows: llvmTriple.setOS(llvm::Triple::OSType::Win32); break;
        case cld::Platform::Linux: llvmTriple.setOS(llvm::Triple::OSType::Linux); break;
        case cld::Platform::Unknown: llvmTriple.setOS(llvm::Triple::OSType::UnknownOS); break;
    }
    switch (triple.getEnvironment())
    {
        case cld::Environment::GNU: llvmTriple.setEnvironment(llvm::Triple::GNU); break;
        case cld::Environment::MSVC: llvmTriple.setEnvironment(llvm::Triple::MSVC); break;
        case cld::Environment::Unknown: llvmTriple.setEnvironment(llvm::Triple::UnknownEnvironment); break;
    }
    module.setTargetTriple(llvmTriple.normalize());
    std::string error;
    auto* targetM = llvm::TargetRegistry::lookupTarget(module.getTargetTriple(), error);
    if (!targetM)
    {
        llvm::errs() << "Target lookup failed with error: " << error;
        return {};
    }
    auto machine = std::unique_ptr<llvm::TargetMachine>(
        targetM->createTargetMachine(module.getTargetTriple(), "generic", "", {}, options.reloc, {}, options.ol));
    module.setDataLayout(machine->createDataLayout());
    CodeGenerator codeGenerator(module, program, program.getSourceObject(), triple, options);
    codeGenerator.visit(program.getTranslationUnit());
    return machine;
}

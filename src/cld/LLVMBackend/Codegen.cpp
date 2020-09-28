#include "Codegen.hpp"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <cld/Common/Filesystem.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/SemanticUtil.hpp>

#include <numeric>

namespace
{
class CodeGenerator final
{
    llvm::Module& m_module;
    const cld::Semantics::ProgramInterface& m_programInterface;
    const cld::SourceInterface& m_sourceInterface;
    cld::Triple m_triple;
    // void* for now although strictly speaking the pointers can only be const Declaration* or const FunctionDefinition*
    // but hashing the variant seems overkill? I am not sure
    std::unordered_map<const void*, llvm::Value * CLD_NON_NULL> m_lvalues;

    using TypeVariantKey = std::variant<cld::Semantics::StructType, cld::Semantics::UnionType,
                                        cld::Semantics::AnonymousUnionType, cld::Semantics::AnonymousStructType>;

    struct TypeHasher
    {
        const cld::Semantics::ProgramInterface& programInterface;

        std::size_t operator()(const TypeVariantKey& variant) const noexcept
        {
            return cld::rawHashCombine(std::hash<std::size_t>{}(variant.index()),
                                       cld::match(
                                           variant,
                                           [&](const cld::Semantics::StructType& structType) -> std::size_t {
                                               return std::hash<std::string_view>{}(structType.getName());
                                           },
                                           [&](const cld::Semantics::UnionType& unionType) -> std::size_t {
                                               return std::hash<std::string_view>{}(unionType.getName());
                                           },
                                           [](const cld::Semantics::AnonymousStructType& structType) -> std::size_t {
                                               return std::hash<std::uint64_t>{}(structType.getId());
                                           },
                                           [](const cld::Semantics::AnonymousUnionType& unionType) -> std::size_t {
                                               return std::hash<std::uint64_t>{}(unionType.getId());
                                           }));
        }
    };

    struct TypeEqual
    {
        const cld::Semantics::ProgramInterface& programInterface;

        bool operator()(const TypeVariantKey& lhs, const TypeVariantKey& rhs) const noexcept
        {
            if (lhs.index() != rhs.index())
            {
                return false;
            }
            return cld::match(lhs, [this, &rhs](const auto& value) -> bool {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<cld::Semantics::StructType, T>)
                {
                    auto& other = cld::get<T>(rhs);
                    return programInterface.getStructDefinition(value.getName(), value.getScopeOrId())
                           == programInterface.getStructDefinition(other.getName(), other.getScopeOrId());
                }
                else if constexpr (std::is_same_v<cld::Semantics::UnionType, T>)
                {
                    auto& other = cld::get<T>(rhs);
                    return programInterface.getUnionDefinition(value.getName(), value.getScopeOrId())
                           == programInterface.getUnionDefinition(other.getName(), other.getScopeOrId());
                }
                else
                {
                    return value == cld::get<T>(rhs);
                }
            });
        }
    };

    std::unordered_map<TypeVariantKey, llvm::Type*, TypeHasher, TypeEqual> m_types{0,
                                                                                   {m_programInterface},
                                                                                   {m_programInterface}};

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

    std::unordered_map<const llvm::FunctionType*, ABITransformations> m_functionABITransformations;
    std::unordered_map<cld::Semantics::LoopStatements, llvm::BasicBlock*> m_continueTargets;
    std::unordered_map<cld::Semantics::BreakableStatements, llvm::BasicBlock*> m_breakTargets;
    std::unordered_map<const cld::Semantics::LabelStatement*, llvm::BasicBlock*> m_labels;
    struct Switch
    {
        std::unordered_map<const cld::Semantics::CaseStatement*, llvm::BasicBlock*> cases;
        llvm::BasicBlock* defaultBlock;
    };
    std::unordered_map<const cld::Semantics::SwitchStatement*, Switch> m_switches;

    llvm::IRBuilder<> m_builder{m_module.getContext()};
    llvm::Function* m_currentFunction = nullptr;
    llvm::Value* m_returnSlot = nullptr;
    llvm::DIBuilder m_debugInfo{m_module};
    std::unordered_map<std::shared_ptr<const cld::Semantics::Expression>, llvm::Value*> m_valSizes;
    std::unordered_map<const cld::Semantics::Declaration * CLD_NON_NULL, llvm::AllocaInst*> m_stackSaves;
    std::unordered_map<std::string_view, llvm::GlobalVariable*> m_cGlobalVariables;

    llvm::Value* toBool(llvm::Value* value)
    {
        if (value->getType()->isIntegerTy())
        {
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
        flattenSingleArg(llvm::Type* type, std::uint8_t& takenIntegers, std::uint8_t& takenFloats)
    {
        constexpr std::uint8_t availableIntegerRegisters = 6;
        constexpr std::uint8_t availableFloatingPointRegisters = 8;
        ABITransformations::Variant dest;
        std::size_t retIndex = 0;
        std::array<llvm::Type*, 2> ret = {};

        std::uint8_t takenIntegerRegisters = takenIntegers;
        std::uint8_t takenFloatingPointRegisters = takenFloats;
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
                const auto rest = size % alignment;
                if (rest != 0)
                {
                    if (size + alignment - rest >= 8)
                    {
                        break;
                    }
                    size += alignment - rest;
                }
                if ((*iter)->isIntOrPtrTy())
                {
                    encounteredInteger = true;
                }
                currentAlignment = std::max(currentAlignment, alignment);
                const auto typeSize = m_module.getDataLayout().getTypeAllocSize(*iter).getKnownMinSize();
                size += typeSize;
                iter++;
            }
            const auto rest = size % currentAlignment;
            if (rest != 0)
            {
                size += currentAlignment - rest;
            }
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
        takenFloats = takenFloatingPointRegisters;
        takenIntegers = takenIntegerRegisters;
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
                    flattenSingleArg(*arg, takenIntegerRegisters, takenFloatingPointRegisters);
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
                        flattenSingleArg(returnType, takenIntegerRegisters, takenFloatingPointRegisters);
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
        auto transformations = m_functionABITransformations.find(functionType);
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
                 && cld::get<cld::Semantics::PrimitiveType>(ft.getReturnType().getVariant()).isSigned())
        {
            attributeApply.addAttribute(0, llvm::Attribute::SExt);
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
                        && cld::get<cld::Semantics::PrimitiveType>(arg.getVariant()).isSigned())
                    {
                        attributeApply.addParamAttr(i, llvm::Attribute::SExt);
                    }
                }
                else if (change == ABITransformations::OnStack)
                {
                    attributeApply.addParamAttr(
                        i, llvm::Attribute::getWithByValType(m_builder.getContext(),
                                                             functionType->getParamType(i)->getPointerElementType()));
                    attributeApply.addParamAttr(
                        i,
                        llvm::Attribute::getWithAlignment(
                            m_builder.getContext(), llvm::Align(m_module.getDataLayout().getPointerABIAlignment(0))));
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
                    auto* var = m_builder.CreateAlloca(operand);
                    var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                    m_lvalues.emplace(paramDecl.get(), var);
                    continue;
                }
                auto* var = m_builder.CreateAlloca(visit(paramDecl->getType()));
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
                auto* var = m_builder.CreateAlloca(visit(paramDecl->getType()));
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                m_lvalues.emplace(paramDecl.get(), var);
            }
        }
    }

    llvm::Value* add(llvm::Value* lhs, const cld::Semantics::Type& lhsType, llvm::Value* rhs,
                     const cld::Semantics::Type& rhsType)
    {
        if (cld::Semantics::isArithmetic(lhsType) && cld::Semantics::isArithmetic(rhsType))
        {
            if (cld::Semantics::isInteger(lhsType))
            {
                if (!cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
                {
                    return m_builder.CreateNSWAdd(lhs, rhs);
                }

                return m_builder.CreateAdd(lhs, rhs);
            }

            return m_builder.CreateFAdd(lhs, rhs);
        }

        auto* pointer = lhs->getType()->isPointerTy() ? lhs : rhs;
        auto* integer = pointer == lhs ? rhs : lhs;
        auto& pointerType = pointer == lhs ? lhsType : rhsType;
        integer = m_builder.CreateIntCast(
            integer, m_builder.getInt64Ty(),
            cld::get<cld::Semantics::PrimitiveType>((pointer == lhs ? rhsType : lhsType).getVariant()).isSigned());
        if (!cld::Semantics::isVariableLengthArray(
                cld::get<cld::Semantics::PointerType>(pointerType.getVariant()).getElementType()))
        {
            return m_builder.CreateGEP(pointer, integer);
        }
        auto& array = cld::get<cld::Semantics::PointerType>(pointerType.getVariant()).getElementType();
        llvm::Value* product = integer;
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
            product = m_builder.CreateMul(product, value);
        }
        return m_builder.CreateGEP(pointer, product);
    }

    llvm::Value* sub(llvm::Value* lhs, const cld::Semantics::Type& lhsType, llvm::Value* rhs,
                     const cld::Semantics::Type& rhsType)
    {
        if (cld::Semantics::isArithmetic(lhsType) && cld::Semantics::isArithmetic(rhsType))
        {
            if (cld::Semantics::isInteger(lhsType))
            {
                if (!cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
                {
                    return m_builder.CreateNSWSub(lhs, rhs);
                }

                return m_builder.CreateSub(lhs, rhs);
            }

            return m_builder.CreateFSub(lhs, rhs);
        }

        if (lhs->getType()->isPointerTy())
        {
            if (rhs->getType()->isIntegerTy())
            {
                rhs = m_builder.CreateNeg(rhs);
                rhs = m_builder.CreateIntCast(rhs, m_builder.getInt64Ty(),
                                              cld::get<cld::Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
                return m_builder.CreateGEP(lhs, rhs);
            }

            return m_builder.CreatePtrDiff(lhs, rhs);
        }

        if (rhs->getType()->isIntegerTy())
        {
            lhs = m_builder.CreateNeg(lhs);
            lhs = m_builder.CreateIntCast(lhs, m_builder.getInt64Ty(),
                                          cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned());
            return m_builder.CreateGEP(rhs, lhs);
        }

        return m_builder.CreatePtrDiff(lhs, rhs);
    }

    llvm::Value* mul(llvm::Value* lhs, const cld::Semantics::Type& lhsType, llvm::Value* rhs,
                     const cld::Semantics::Type&)
    {
        if (cld::Semantics::isInteger(lhsType))
        {
            return m_builder.CreateNSWMul(lhs, rhs);
        }

        return m_builder.CreateFMul(lhs, rhs);
    }

    llvm::Value* div(llvm::Value* lhs, const cld::Semantics::Type& lhsType, llvm::Value* rhs,
                     const cld::Semantics::Type&)
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

    llvm::Value* mod(llvm::Value* lhs, const cld::Semantics::Type& lhsType, llvm::Value* rhs,
                     const cld::Semantics::Type&)
    {
        if (cld::get<cld::Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
        {
            return m_builder.CreateSRem(lhs, rhs);
        }

        return m_builder.CreateURem(lhs, rhs);
    }

    llvm::Value* shl(llvm::Value* lhs, const cld::Semantics::Type&, llvm::Value* rhs,
                     const cld::Semantics::Type& rhsType)
    {
        if (lhs->getType() != rhs->getType())
        {
            rhs = m_builder.CreateIntCast(rhs, lhs->getType(),
                                          cld::get<cld::Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
        }
        return m_builder.CreateShl(lhs, rhs);
    }

    llvm::Value* shr(llvm::Value* lhs, const cld::Semantics::Type&, llvm::Value* rhs,
                     const cld::Semantics::Type& rhsType)
    {
        if (lhs->getType() != rhs->getType())
        {
            rhs = m_builder.CreateIntCast(rhs, lhs->getType(),
                                          cld::get<cld::Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
        }
        return m_builder.CreateAShr(lhs, rhs);
    }

    llvm::Value* cast(llvm::Value* value, const cld::Semantics::Type& from, const cld::Semantics::Type& to)
    {
        if (std::holds_alternative<cld::Semantics::PointerType>(to.getVariant()))
        {
            if (cld::Semantics::isInteger(from))
            {
                return m_builder.CreateIntToPtr(value, visit(to));
            }
            return m_builder.CreatePointerCast(value, visit(to));
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

    llvm::Constant* getStringLiteralData(llvm::Type* elementType, const cld::Semantics::Constant::Variant& value)
    {
        if (std::holds_alternative<std::string>(value))
        {
            return llvm::ConstantDataArray::getString(m_module.getContext(), cld::get<std::string>(value));
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
                return llvm::ConstantDataArray::getRaw(llvm::StringRef(rawData.data(), rawData.size()),
                                                       convertedData.size(), elementType);
            }
            case 4:
            {
                std::vector<char> rawData(str.characters.size() * 4);
                std::memcpy(rawData.data(), str.characters.data(), rawData.size());
                return llvm::ConstantDataArray::getRaw(llvm::StringRef(rawData.data(), rawData.size()),
                                                       str.characters.size(), elementType);
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
                    auto fields = m_programInterface.getFields(type);
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
            auto* replacement = visit(expression);
            llvm::Constant** value = [&, &path = path, &expression = expression]() -> llvm::Constant** {
                Aggregate* current = &constants;
                const cld::Semantics::Type* currentType = &type;
                for (auto& iter : llvm::ArrayRef(path).drop_back())
                {
                    if (cld::Semantics::isUnion(*currentType))
                    {
                        auto fields = m_programInterface.getFields(*currentType);
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
                        currentType = m_programInterface.getFields(*currentType)[iter].type.get();
                    }
                    else if (cld::Semantics::isArray(*currentType))
                    {
                        currentType = &cld::Semantics::getArrayElementType(*currentType);
                    }
                    current = &cld::get<Aggregate>(current->vector[iter]);
                }
                if (cld::Semantics::isStringLiteralExpr(expression))
                {
                    auto& constant = cld::get<cld::Semantics::Constant>(expression.getVariant());
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
                return &cld::get<llvm::Constant*>(current->vector[path.back()]);
            }();
            if (value)
            {
                *value = llvm::cast<llvm::Constant>(replacement);
            }
        }

        return cld::YComb{[&](auto&& self, const cld::Semantics::Type& type, llvm::Type* llvmType,
                              const Aggregate& aggregate) -> llvm::Constant* {
            if (cld::Semantics::isStruct(type))
            {
                std::vector<llvm::Constant*> elements;
                auto fields = m_programInterface.getFields(type);
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
                        i++;
                        continue;
                    }
                    elements.push_back(
                        llvm::Constant::getNullValue(llvmType->getStructElementType(fields[i].layoutIndex)));
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
                return llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(llvmType), elements);
            }
            if (cld::Semantics::isArray(type))
            {
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
                }
                return llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(llvmType), elements);
            }
            // Union
            auto fields = m_programInterface.getFields(type);
            auto* llvmSubType = visit(*fields[*aggregate.unionIndex].type);
            llvm::Constant* element = cld::match(
                aggregate.vector[0], [](llvm::Constant* constant) -> llvm::Constant* { return constant; },
                [&](const Aggregate& subAggregate) -> llvm::Constant* {
                    return self(*fields[*aggregate.unionIndex].type, llvmSubType, subAggregate);
                });
            auto* padding = llvm::ArrayType::get(
                m_builder.getInt8Ty(),
                m_module.getDataLayout().getTypeAllocSize(llvmType).getKnownMinSize()
                    - m_module.getDataLayout().getTypeAllocSize(element->getType()).getKnownMinSize());
            auto* newType = llvm::StructType::get(element->getType(), padding);

            return llvm::ConstantStruct::get(newType, {element, llvm::UndefValue::get(padding)});
        }}(type, llvmType, constants);
    }

    void runDestructors(std::int64_t from, std::int64_t toExclusive)
    {
        while (from > 0 && from != toExclusive)
        {
            // Destructors must be run backwards in order of declaration
            for (auto iter = m_programInterface.getScopes()[from].declarations.rbegin();
                 iter != m_programInterface.getScopes()[from].declarations.rend(); iter++)
            {
                if (!std::holds_alternative<const cld::Semantics::Declaration*>(iter->second.declared))
                {
                    continue;
                }
                const auto* decl = cld::get<const cld::Semantics::Declaration*>(iter->second.declared);
                if (cld::Semantics::isVariableLengthArray(decl->getType()))
                {
                    auto* alloca = m_stackSaves[decl];
                    CLD_ASSERT(alloca);
                    auto* loaded = m_builder.CreateLoad(alloca);
                    m_builder.CreateIntrinsic(llvm::Intrinsic::stackrestore, {}, {loaded});
                    continue;
                }
            }
            from = m_programInterface.getScopes()[from].previousScope;
        }
    }

    void runDestructors(std::int64_t scope)
    {
        runDestructors(scope, m_programInterface.getScopes()[scope].previousScope);
    }

public:
    explicit CodeGenerator(llvm::Module& module, const cld::Semantics::ProgramInterface& programInterface,
                           const cld::SourceInterface& sourceInterface, cld::Triple triple)
        : m_module(module), m_programInterface(programInterface), m_sourceInterface(sourceInterface), m_triple(triple)
    {
        auto fullPath = cld::fs::u8path(m_sourceInterface.getFiles()[1].path);
        module.setSourceFileName(fullPath.filename().u8string());
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
                auto* ft = llvm::FunctionType::get(returnType, args, functionType.isLastVararg());
                m_functionABITransformations.emplace(ft, transformation);
                return ft;
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
                auto* structDef =
                    m_programInterface.getStructDefinition(structType.getName(), structType.getScopeOrId());
                auto* type = llvm::StructType::create(m_module.getContext(), structType.getName());
                m_types.insert({structType, type});
                if (!structDef)
                {
                    return type;
                }

                std::vector<llvm::Type*> fields;
                for (auto& iter : structDef->getLayout())
                {
                    fields.push_back(visit(iter));
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
                auto* unionDef = m_programInterface.getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
                if (!unionDef)
                {
                    auto* type = llvm::StructType::get(m_module.getContext());
                    m_types.insert({unionType, type});
                    return type;
                }
                auto largestField = std::max_element(
                    unionDef->getFields().begin(), unionDef->getFields().end(),
                    [&](const cld::Semantics::Field& lhs, const cld::Semantics::Field& rhs) {
                        return lhs.type->getSizeOf(m_programInterface) < rhs.type->getSizeOf(m_programInterface);
                    });
                auto* type = llvm::StructType::get(m_module.getContext(), llvm::ArrayRef(visit(*largestField->type)));
                m_types.insert({unionType, type});
                return type;
            },
            [&](const cld::Semantics::AnonymousStructType& structType) -> llvm::Type* {
                auto result = m_types.find(structType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                std::vector<llvm::Type*> fields;
                for (auto& iter : structType.getLayout())
                {
                    fields.push_back(visit(iter));
                }
                auto* type = llvm::StructType::get(m_module.getContext(), fields);
                m_types.insert({structType, type});
                return type;
            },
            [&](const cld::Semantics::AnonymousUnionType& unionType) -> llvm::Type* {
                auto result = m_types.find(unionType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto largestField = std::max_element(
                    unionType.getFields().begin(), unionType.getFields().end(),
                    [&](const cld::Semantics::Field& lhs, const cld::Semantics::Field& rhs) {
                        return lhs.type->getSizeOf(m_programInterface) < rhs.type->getSizeOf(m_programInterface);
                    });
                auto* type = llvm::StructType::get(m_module.getContext(), llvm::ArrayRef(visit(*largestField->type)));
                m_types.insert({unionType, type});
                return type;
            },
            [&](const cld::Semantics::AbstractArrayType&) -> llvm::Type* { CLD_UNREACHABLE; },
            [&](const std::monostate&) -> llvm::Type* { CLD_UNREACHABLE; },
            [&](const cld::Semantics::EnumType& enumType) -> llvm::Type* {
                auto* enumDef = m_programInterface.getEnumDefinition(enumType.getName(), enumType.getScopeOrId());
                CLD_ASSERT(enumDef);
                return visit(enumDef->getType());
            },
            [&](const cld::Semantics::AnonymousEnumType& enumType) -> llvm::Type* { return visit(enumType.getType()); },
            [&](const cld::Semantics::ValArrayType& valArrayType) -> llvm::Type* {
                auto expression = m_valSizes.find(valArrayType.getExpression());
                if (expression == m_valSizes.end() && m_currentFunction)
                {
                    m_valSizes.emplace(valArrayType.getExpression(),
                                       m_builder.CreateIntCast(visit(*valArrayType.getExpression()),
                                                               m_builder.getInt64Ty(),
                                                               cld::get<cld::Semantics::PrimitiveType>(
                                                                   valArrayType.getExpression()->getType().getVariant())
                                                                   .isSigned()));
                }
                return visit(valArrayType.getType());
            },
            [&](const cld::Semantics::BuiltinType&) -> llvm::Type* {
                // TODO:
                CLD_UNREACHABLE;
            });
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
                    auto* global = visit(*declaration);
                    if (llvm::isa<llvm::GlobalVariable>(global))
                    {
                        m_cGlobalVariables.emplace(declaration->getNameToken()->getText(),
                                                   llvm::cast<llvm::GlobalVariable>(global));
                    }
                });
        }
    }

    llvm::Value* visit(const cld::Semantics::Declaration& declaration)
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
            auto* function = m_module.getFunction(declaration.getNameToken()->getText());
            if (function)
            {
                m_lvalues.emplace(&declaration, function);
                return function;
            }
            auto* ft = llvm::cast<llvm::FunctionType>(visit(declaration.getType()));
            function = llvm::Function::Create(ft, linkageType, -1,
                                              llvm::StringRef{declaration.getNameToken()->getText()}, &m_module);
            applyFunctionAttributes(*function, ft,
                                    cld::get<cld::Semantics::FunctionType>(declaration.getType().getVariant()));
            m_lvalues.emplace(&declaration, function);
            return function;
        }
        auto* type = visit(declaration.getType());
        if (declaration.getLifetime() == cld::Semantics::Lifetime::Static)
        {
            llvm::Constant* constant = nullptr;
            if (declaration.getInitializer() && declaration.getKind() != cld::Semantics::Declaration::DeclarationOnly)
            {
                constant =
                    llvm::cast<llvm::Constant>(visit(*declaration.getInitializer(), declaration.getType(), type));
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
                    m_module, type, declaration.getType().isConst() && linkageType != llvm::GlobalValue::CommonLinkage,
                    linkageType, constant, llvm::StringRef{declaration.getNameToken()->getText()});
                global->setAlignment(llvm::MaybeAlign(declaration.getType().getAlignOf(m_programInterface)));
            }
            else
            {
                global = m_cGlobalVariables[declaration.getNameToken()->getText()];
                global->setConstant(declaration.getType().isConst() && linkageType != llvm::GlobalValue::CommonLinkage);
                global->setLinkage(linkageType);
                global->setInitializer(constant);
            }

            m_lvalues.emplace(&declaration, global);
            return global;
        }
        // Place all allocas up top except variably modified types
        llvm::AllocaInst* var = nullptr;
        if (cld::Semantics::isVariableLengthArray(declaration.getType()))
        {
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
            llvm::IRBuilder<> temp(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
            auto* stackSave = temp.CreateAlloca(m_builder.getInt8PtrTy(0), nullptr, "stack.save");
            m_stackSaves[&declaration] = stackSave;
            auto* stack = m_builder.CreateIntrinsic(llvm::Intrinsic::stacksave, {}, {});
            m_builder.CreateStore(stack, stackSave);
            var = m_builder.CreateAlloca(type, value, llvm::StringRef{declaration.getNameToken()->getText()});
            var->setAlignment(m_module.getDataLayout().getStackAlignment());
        }
        else
        {
            llvm::IRBuilder<> temp(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
            var = temp.CreateAlloca(type, nullptr, llvm::StringRef{declaration.getNameToken()->getText()});
            var->setAlignment(llvm::Align(declaration.getType().getAlignOf(m_programInterface)));
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
            m_lvalues.emplace(&functionDefinition, function);
        }
        auto* bb = llvm::BasicBlock::Create(m_module.getContext(), "entry", function);
        m_builder.SetInsertPoint(bb);
        m_currentFunction = function;

        auto& ft = cld::get<cld::Semantics::FunctionType>(functionDefinition.getType().getVariant());
        applyFunctionAttributes(*function, function->getFunctionType(), ft,
                                &functionDefinition.getParameterDeclarations());
        auto transformations = m_functionABITransformations.find(function->getFunctionType());
        CLD_ASSERT(transformations != m_functionABITransformations.end());
        if (transformations->second.returnType == ABITransformations::Flattened
            || transformations->second.returnType == ABITransformations::IntegerRegister)
        {
            m_returnSlot = m_builder.CreateAlloca(function->getReturnType());
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
                    m_lvalues[paramDecl.get()] = operand;
                    continue;
                }
                auto* alloc = m_lvalues[paramDecl.get()];
                if (change == ABITransformations::Unchanged)
                {
                    m_builder.CreateStore(operand, alloc, paramDecl->getType().isVolatile());
                    continue;
                }
                auto* cast = m_builder.CreateBitCast(alloc, llvm::PointerType::getUnqual(operand->getType()));
                m_builder.CreateAlignedStore(operand, cast, llvm::cast<llvm::AllocaInst>(alloc)->getAlign(),
                                             paramDecl->getType().isVolatile());
            }
            else
            {
                auto& multiArg =
                    cld::get<ABITransformations::MultipleArgs>(transformations->second.arguments[origArgI]);
                CLD_ASSERT(multiArg.size <= 2 && multiArg.size > 0);
                auto* alloc = m_lvalues[paramDecl.get()];
                std::vector<llvm::Type*> elements;
                elements.push_back(function->getArg(i)->getType());
                if (multiArg.size == 2)
                {
                    elements.push_back(function->getArg(i + 1)->getType());
                }
                auto* structType = m_builder.CreateBitCast(
                    alloc, llvm::PointerType::getUnqual(llvm::StructType::get(m_builder.getContext(), elements)));
                auto* firstElement =
                    m_builder.CreateInBoundsGEP(structType, {m_builder.getInt32(0), m_builder.getInt32(0)});
                m_builder.CreateStore(function->getArg(i), firstElement, paramDecl->getType().isVolatile());
                if (multiArg.size == 2)
                {
                    auto* secondElement =
                        m_builder.CreateInBoundsGEP(structType, {m_builder.getInt32(0), m_builder.getInt32(1)});
                    m_builder.CreateStore(function->getArg(i + 1), secondElement, paramDecl->getType().isVolatile());
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
            visit(type);
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
        m_builder.ClearInsertionPoint();
        m_currentFunction = nullptr;
    }

    void visit(const cld::Semantics::CompoundStatement& compoundStatement)
    {
        for (auto& iter : compoundStatement.getCompoundItems())
        {
            if (std::holds_alternative<std::shared_ptr<const cld::Semantics::Expression>>(iter))
            {
                auto& expr = cld::get<std::shared_ptr<const cld::Semantics::Expression>>(iter);
                auto result = m_valSizes.emplace(
                    expr, m_builder.CreateIntCast(
                              visit(*expr), m_builder.getInt64Ty(),
                              cld::get<cld::Semantics::PrimitiveType>(expr->getType().getVariant()).isSigned()));
                (void)result;
                CLD_ASSERT(result.second);
            }
            else if (std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter))
            {
                visit(*cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter));
            }
            else if (std::holds_alternative<std::unique_ptr<cld::Semantics::Statement>>(iter))
            {
                visit(*cld::get<std::unique_ptr<cld::Semantics::Statement>>(iter));
            }
        }
        if (m_builder.GetInsertBlock())
        {
            runDestructors(compoundStatement.getScope());
        }
    }

    void visit(const cld::Semantics::Statement& statement)
    {
        cld::match(
            statement.getVariant(), [&](const auto* statement) { visit(*statement); },
            [&](const cld::Semantics::ExpressionStatement* expressionStatement) {
                if (!expressionStatement->getExpression() || !m_builder.GetInsertBlock())
                {
                    return;
                }
                auto* instr = visit(*expressionStatement->getExpression());
                if (llvm::isa_and_nonnull<llvm::Instruction>(instr) && instr->getNumUses() == 0
                    && !llvm::cast<llvm::Instruction>(instr)->mayHaveSideEffects())
                {
                    llvm::cast<llvm::Instruction>(instr)->eraseFromParent();
                }
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
        auto transformation = m_functionABITransformations.find(function->getFunctionType());
        CLD_ASSERT(transformation != m_functionABITransformations.end());
        auto* value = visit(*returnStatement.getExpression());
        if (transformation->second.returnType == ABITransformations::PointerToTemporary)
        {
            m_builder.CreateStore(value, function->getArg(0));
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRetVoid();
        }
        else if (transformation->second.returnType == ABITransformations::Flattened
                 || transformation->second.returnType == ABITransformations::IntegerRegister)
        {
            auto* bitCast = m_builder.CreateBitCast(m_returnSlot, llvm::PointerType::getUnqual(value->getType()));
            m_builder.CreateStore(value, bitCast);
            auto* ret = m_builder.CreateLoad(m_returnSlot);
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRet(ret);
        }
        else
        {
            runDestructors(returnStatement.getScope(), 0);
            m_builder.CreateRet(value);
        }
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::ForStatement& forStatement)
    {
        cld::match(
            forStatement.getInitial(), [](std::monostate) {},
            [&](const std::vector<std::unique_ptr<cld::Semantics::Declaration>>& declaration) {
                for (auto& iter : declaration)
                {
                    visit(*iter);
                }
            },
            [&](const cld::Semantics::Expression& expression) {
                if (!m_builder.GetInsertBlock())
                {
                    return;
                }
                visit(expression);
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
            auto* value = visit(*forStatement.getControlling());
            value = m_builder.CreateTrunc(value, m_builder.getInt1Ty());
            m_builder.CreateCondBr(value, body, contBlock);
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
            visit(*forStatement.getIteration());
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
        llvm::Value* expression = nullptr;
        llvm::BasicBlock* trueBranch = nullptr;
        if (m_builder.GetInsertBlock())
        {
            expression = visit(ifStatement.getExpression());
            expression = m_builder.CreateTrunc(expression, m_builder.getInt1Ty());
            trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.true", m_currentFunction);
        }
        if (!ifStatement.getFalseBranch())
        {
            auto* contBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.continue", m_currentFunction);
            if (m_builder.GetInsertBlock())
            {
                m_builder.CreateCondBr(expression, trueBranch, contBranch);
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
            m_builder.CreateCondBr(expression, trueBranch, falseBranch);
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
        auto* expression = visit(headWhileStatement.getExpression());
        expression = m_builder.CreateTrunc(expression, m_builder.getInt1Ty());
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "while.continue", m_currentFunction);
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "while.body", m_currentFunction);
        m_builder.CreateCondBr(expression, body, contBlock);
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
        auto* expression = visit(footWhileStatement.getExpression());
        expression = m_builder.CreateTrunc(expression, m_builder.getInt1Ty());
        m_builder.CreateCondBr(expression, body, contBlock);
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
        auto* expression = m_builder.GetInsertBlock() ? visit(switchStatement.getExpression()) : nullptr;
        auto& switchData = m_switches[&switchStatement];
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "switch.continue", m_currentFunction);
        if (switchStatement.getDefaultStatement())
        {
            switchData.defaultBlock =
                llvm::BasicBlock::Create(m_module.getContext(), "switch.default", m_currentFunction);
        }
        auto* switchStmt =
            expression ? m_builder.CreateSwitch(
                expression, switchData.defaultBlock ? switchData.defaultBlock : contBlock, switchData.cases.size()) :
                         nullptr;
        m_builder.ClearInsertionPoint();
        for (auto& [value, theCase] : switchStatement.getCases())
        {
            auto iter = switchData.cases.emplace(
                theCase, llvm::BasicBlock::Create(m_module.getContext(), "switch.case", m_currentFunction));
            if (switchStmt && expression)
            {
                switchStmt->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(expression->getType(), value)),
                                    iter.first->second);
            }
        }
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

    void visit(const cld::Semantics::CaseStatement& caseStatement)
    {
        auto& switchData = m_switches[&caseStatement.getSwitchStatement()];
        auto* bb = switchData.cases[&caseStatement];
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(bb);
        }
        m_builder.SetInsertPoint(bb);
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
        std::unordered_set<std::int64_t> labelScopes;
        {
            auto currScope = gotoStatement.getLabel()->getScope();
            while (currScope >= 0)
            {
                labelScopes.insert(currScope);
                currScope = m_programInterface.getScopes()[currScope].previousScope;
            }
        }
        auto commonScope = gotoStatement.getScope();
        while (commonScope >= 0 && labelScopes.count(commonScope) == 0)
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

    llvm::Value* visit(const cld::Semantics::Expression& expression)
    {
        return cld::match(
            expression.getVariant(),
            [](const std::pair<cld::Lexer::CTokenIterator, cld::Lexer::CTokenIterator>&) -> llvm::Value* {
                CLD_UNREACHABLE;
            },
            [&](const auto& value) -> llvm::Value* { return visit(expression, value); });
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Constant& constant)
    {
        auto* type = visit(expression.getType());
        if (std::holds_alternative<llvm::APSInt>(constant.getValue()))
        {
            return llvm::Constant::getIntegerValue(type, cld::get<llvm::APSInt>(constant.getValue()));
        }
        if (std::holds_alternative<llvm::APFloat>(constant.getValue()))
        {
            return llvm::ConstantFP::get(type, cld::get<llvm::APFloat>(constant.getValue()));
        }

        auto* array = getStringLiteralData(type->getArrayElementType(), constant.getValue());
        auto* global =
            new llvm::GlobalVariable(m_module, array->getType(), true, llvm::GlobalValue::PrivateLinkage, array);
        global->setAlignment(llvm::MaybeAlign(1));
        global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        return global;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::DeclarationRead& declarationRead)
    {
        auto result = cld::match(
            declarationRead.getDeclRead(),
            [&](const cld::Semantics::Declaration* declaration) { return m_lvalues.find(declaration); },
            [&](const cld::Semantics::FunctionDefinition* functionDefinition) {
                return m_lvalues.find(functionDefinition);
            });
        CLD_ASSERT(result != m_lvalues.end());
        return result->second;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Conversion& conversion)
    {
        auto* value = visit(conversion.getExpression());
        switch (conversion.getKind())
        {
            case cld::Semantics::Conversion::LValue:
            {
                if (std::holds_alternative<cld::Semantics::ArrayType>(conversion.getExpression().getType().getVariant())
                    && !cld::Semantics::isVariableLengthArray(conversion.getExpression().getType()))
                {
                    auto* zero = llvm::ConstantInt::get(m_builder.getIntPtrTy(m_module.getDataLayout()), 0);
                    return m_builder.CreateInBoundsGEP(value, {zero, zero});
                }
                if (std::holds_alternative<cld::Semantics::FunctionType>(
                        conversion.getExpression().getType().getVariant())
                    || m_programInterface.isBitfieldAccess(conversion.getExpression())
                    || cld::Semantics::isVariableLengthArray(conversion.getExpression().getType()))
                {
                    return value;
                }

                return m_builder.CreateLoad(value->getType()->getPointerElementType(), value,
                                            conversion.getExpression().getType().isVolatile());
            }
            case cld::Semantics::Conversion::IntegerPromotion:
            {
                auto& prevType = conversion.getExpression().getType();
                return m_builder.CreateIntCast(
                    value, visit(expression.getType()),
                    cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned());
            }
            case cld::Semantics::Conversion::Implicit:
            {
                auto& prevType = conversion.getExpression().getType();
                auto& newType = expression.getType();
                if (cld::Semantics::isBool(newType))
                {
                    return m_builder.CreateIntCast(toBool(value), visit(newType), false);
                }
                if (std::holds_alternative<cld::Semantics::PointerType>(newType.getVariant()))
                {
                    if (cld::Semantics::isInteger(prevType))
                    {
                        return m_builder.CreateIntToPtr(value, visit(newType));
                    }
                    return m_builder.CreatePointerCast(value, visit(newType));
                }
                [[fallthrough]];
            }
            case cld::Semantics::Conversion::ArithmeticConversion:
            {
                auto& prevType = conversion.getExpression().getType();
                auto& newType = expression.getType();
                if (cld::Semantics::isInteger(prevType) && cld::Semantics::isInteger(newType))
                {
                    return m_builder.CreateIntCast(
                        value, visit(newType),
                        cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned());
                }
                if (cld::Semantics::isInteger(prevType))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned())
                    {
                        return m_builder.CreateSIToFP(value, visit(newType));
                    }

                    return m_builder.CreateUIToFP(value, visit(newType));
                }
                if (cld::Semantics::isInteger(newType))
                {
                    if (std::holds_alternative<cld::Semantics::PointerType>(prevType.getVariant()))
                    {
                        return m_builder.CreatePtrToInt(value, visit(newType));
                    }
                    if (cld::get<cld::Semantics::PrimitiveType>(newType.getVariant()).isSigned())
                    {
                        return m_builder.CreateFPToSI(value, visit(newType));
                    }

                    return m_builder.CreateFPToUI(value, visit(newType));
                }
                return m_builder.CreateFPCast(value, visit(newType));
            }
            case cld::Semantics::Conversion::DefaultArgumentPromotion:
            {
                auto& prevType = conversion.getExpression().getType();
                if (cld::Semantics::isInteger(prevType))
                {
                    return m_builder.CreateIntCast(
                        value, visit(expression.getType()),
                        cld::get<cld::Semantics::PrimitiveType>(prevType.getVariant()).isSigned());
                }
                return m_builder.CreateFPCast(value, visit(expression.getType()));
            }
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::MemberAccess& memberAccess)
    {
        auto* value = visit(memberAccess.getRecordExpression());
        auto& type =
            std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant()) ?
                cld::get<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().getVariant())
                    .getElementType() :
                memberAccess.getRecordExpression().getType();
        if (std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant()))
        {
            if (!std::holds_alternative<cld::Semantics::CallExpression>(
                    memberAccess.getRecordExpression().getVariant()))
            {
                value = m_builder.CreateLoad(value, memberAccess.getRecordExpression().getType().isVolatile());
            }
        }
        else if (std::holds_alternative<cld::Semantics::CallExpression>(
                     memberAccess.getRecordExpression().getVariant()))
        {
            // Struct access is only ever allowed on pointers or lvalue except if it's the return value of a function
            // then it's also allowed to be an rvalue
            auto* load = llvm::cast<llvm::LoadInst>(value);
            value = load->getPointerOperand();
            load->eraseFromParent();
        }
        auto index = memberAccess.getMemberIndex();
        llvm::ArrayRef<cld::Semantics::Field> fields = m_programInterface.getFields(type);

        llvm::Value* field = nullptr;
        if (cld::Semantics::isStruct(type))
        {
            auto* zero = m_builder.getInt64(0);
            auto* member = m_builder.getInt32(fields[index].layoutIndex);
            field = m_builder.CreateInBoundsGEP(value, {zero, member});
        }
        else
        {
            auto* destTy = visit(*fields[index].type);
            field = m_builder.CreateBitCast(value, llvm::PointerType::getUnqual(destTy));
        }
        if (!fields[index].bitFieldBounds)
        {
            // If the record expression is the return value of a function and this is a dot access not arrow access
            // we must load because an rvalue is returned and no lvalue conversion will load for us
            if (!std::holds_alternative<cld::Semantics::PointerType>(
                    memberAccess.getRecordExpression().getType().getVariant())
                && std::holds_alternative<cld::Semantics::CallExpression>(
                    memberAccess.getRecordExpression().getVariant()))
            {
                return m_builder.CreateLoad(field, type.isVolatile());
            }
            return field;
        }

        auto* loaded = m_builder.CreateLoad(field, expression.getType().isVolatile());
        auto upLeft = loaded->getType()->getPrimitiveSizeInBits() - fields[index].bitFieldBounds->second;
        auto* shl = m_builder.CreateShl(loaded, llvm::ConstantInt::get(loaded->getType(), upLeft));
        auto* shrConstant = llvm::ConstantInt::get(loaded->getType(), upLeft + fields[index].bitFieldBounds->first);
        if (cld::get<cld::Semantics::PrimitiveType>(expression.getType().getVariant()).isSigned())
        {
            return m_builder.CreateAShr(shl, shrConstant);
        }

        return m_builder.CreateLShr(shl, shrConstant);
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::BinaryOperator& binaryExpression)
    {
        auto* lhs = visit(binaryExpression.getLeftExpression());
        switch (binaryExpression.getKind())
        {
            case cld::Semantics::BinaryOperator::Addition:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return add(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Subtraction:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return sub(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Multiply:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return mul(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Divide:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return div(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::Modulo:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return mod(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::LeftShift:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return shl(lhs, binaryExpression.getLeftExpression().getType(), rhs,
                           binaryExpression.getRightExpression().getType());
            }
            case cld::Semantics::BinaryOperator::RightShift:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
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
                auto* rhs = visit(binaryExpression.getRightExpression());
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
                auto* rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateOr(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::BitAnd:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateAnd(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::BitXor:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateXor(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::LogicAnd:
            {
                lhs = m_builder.CreateTrunc(lhs, m_builder.getInt1Ty());
                auto* falseBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.false", m_currentFunction);
                auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.true", m_currentFunction);
                auto* continueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.continue", m_currentFunction);
                m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
                m_builder.SetInsertPoint(trueBranch);
                auto* rhs = visit(binaryExpression.getRightExpression());
                rhs = m_builder.CreateTrunc(rhs, m_builder.getInt1Ty());
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(falseBranch);
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(continueBranch);
                auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
                phi->addIncoming(lhs, falseBranch);
                phi->addIncoming(rhs, trueBranch);
                return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
            }
            case cld::Semantics::BinaryOperator::LogicOr:
            {
                lhs = m_builder.CreateTrunc(lhs, m_builder.getInt1Ty());
                auto* falseBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicOr.false", m_currentFunction);
                auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicOr.true", m_currentFunction);
                auto* continueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "logicOr.continue", m_currentFunction);
                m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
                m_builder.SetInsertPoint(falseBranch);
                auto* rhs = visit(binaryExpression.getRightExpression());
                rhs = m_builder.CreateTrunc(rhs, m_builder.getInt1Ty());
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(trueBranch);
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(continueBranch);
                auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
                phi->addIncoming(lhs, trueBranch);
                phi->addIncoming(rhs, falseBranch);
                return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
            }
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Cast& cast)
    {
        auto* value = visit(cast.getExpression());
        auto& prevType = cast.getExpression().getType();
        auto& newType = expression.getType();
        return this->cast(value, prevType, newType);
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::UnaryOperator& unaryOperator)
    {
        auto* value = visit(unaryOperator.getOperand());
        switch (unaryOperator.getKind())
        {
            case cld::Semantics::UnaryOperator::AddressOf:
            case cld::Semantics::UnaryOperator::Dereference:
                // The difference between address of and dereference is that an lvalue conversion follows a dereference
                return value;
            case cld::Semantics::UnaryOperator::PostIncrement:
            {
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        auto* result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        auto* result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    auto* result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    auto* result = m_builder.CreateGEP(prev, m_builder.getInt32(1));
                    m_builder.CreateStore(result, value);
                }
                return prev;
            }
            case cld::Semantics::UnaryOperator::PostDecrement:
            {
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        auto* result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        auto* result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    auto* result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    auto* result = m_builder.CreateGEP(prev, m_builder.getInt32(-1));
                    m_builder.CreateStore(result, value);
                }
                return prev;
            }
            case cld::Semantics::UnaryOperator::PreIncrement:
            {
                llvm::Value* result = nullptr;
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    result = m_builder.CreateGEP(prev, m_builder.getInt32(1));
                    m_builder.CreateStore(result, value);
                }
                return result;
            }
            case cld::Semantics::UnaryOperator::PreDecrement:
            {
                llvm::Value* result = nullptr;
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().getVariant())
                            .isSigned())
                    {
                        result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().getVariant()))
                {
                    result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    result = m_builder.CreateGEP(prev, m_builder.getInt32(-1));
                    m_builder.CreateStore(result, value);
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
                value = m_builder.CreateNot(m_builder.CreateTrunc(value, m_builder.getInt1Ty()));
                return m_builder.CreateZExt(value, visit(cld::Semantics::PrimitiveType::createInt(
                                                       false, false, m_sourceInterface.getLanguageOptions())));
            }
            case cld::Semantics::UnaryOperator::BitwiseNegate: return m_builder.CreateNot(value);
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::SizeofOperator& sizeofOperator)
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
            [](const std::unique_ptr<cld::Semantics::Expression>& expression) -> const cld::Semantics::Type& {
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

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::SubscriptOperator& subscriptOperator)
    {
        auto& integer = cld::Semantics::isInteger(subscriptOperator.getLeftExpression().getType()) ?
                            subscriptOperator.getLeftExpression() :
                            subscriptOperator.getRightExpression();
        auto* pointer = &integer == &subscriptOperator.getLeftExpression() ? &subscriptOperator.getRightExpression() :
                                                                             &subscriptOperator.getLeftExpression();

        if (!std::holds_alternative<cld::Semantics::Conversion>(pointer->getVariant())
            || cld::get<cld::Semantics::Conversion>(pointer->getVariant()).getKind()
                   != cld::Semantics::Conversion::LValue
            || !std::holds_alternative<cld::Semantics::ValArrayType>(
                cld::get<cld::Semantics::Conversion>(pointer->getVariant()).getExpression().getType().getVariant()))
        {
            auto* llvmInteger = visit(integer);
            auto* llvmPointer = visit(*pointer);

            llvmInteger = m_builder.CreateIntCast(
                llvmInteger, m_builder.getInt64Ty(),
                cld::get<cld::Semantics::PrimitiveType>(integer.getType().getVariant()).isSigned());
            return m_builder.CreateGEP(llvmPointer, llvmInteger);
        }

        std::vector<llvm::Value*> products = {m_builder.CreateIntCast(
            visit(integer), m_builder.getInt64Ty(),
            cld::get<cld::Semantics::PrimitiveType>(integer.getType().getVariant()).isSigned())};
        llvm::Value* dimensionProduct = nullptr;
        while (std::holds_alternative<cld::Semantics::Conversion>(pointer->getVariant())
               && cld::get<cld::Semantics::Conversion>(pointer->getVariant()).getKind()
                      == cld::Semantics::Conversion::LValue)
        {
            auto& subExpr = cld::get<cld::Semantics::Conversion>(pointer->getVariant()).getExpression();
            if (!std::holds_alternative<cld::Semantics::SubscriptOperator>(subExpr.getVariant()))
            {
                break;
            }
            auto& subOp = cld::get<cld::Semantics::SubscriptOperator>(subExpr.getVariant());
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
        auto* basePointer = visit(*pointer);
        auto* sum = std::accumulate(products.begin() + 1, products.end(), products.front(),
                                    [&](llvm::Value* lhs, llvm::Value* rhs) { return m_builder.CreateAdd(lhs, rhs); });
        return m_builder.CreateGEP(basePointer, sum);
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::Conditional& conditional)
    {
        auto* boolean = visit(conditional.getBoolExpression());
        boolean = m_builder.CreateTrunc(boolean, m_builder.getInt1Ty());
        auto* trueBranch = llvm::BasicBlock::Create(m_builder.getContext(), "cond.true", m_currentFunction);
        auto* falseBranch = llvm::BasicBlock::Create(m_builder.getContext(), "cond.false", m_currentFunction);
        auto* contBr = llvm::BasicBlock::Create(m_builder.getContext(), "cond.continue", m_currentFunction);
        m_builder.CreateCondBr(boolean, trueBranch, falseBranch);
        m_builder.SetInsertPoint(trueBranch);
        auto* trueValue = visit(conditional.getTrueExpression());
        m_builder.CreateBr(contBr);
        m_builder.SetInsertPoint(falseBranch);
        auto* falseValue = visit(conditional.getFalseExpression());
        m_builder.CreateBr(contBr);
        m_builder.SetInsertPoint(contBr);
        auto* phi = m_builder.CreatePHI(trueValue->getType(), 2);
        phi->addIncoming(trueValue, trueBranch);
        phi->addIncoming(falseValue, falseBranch);
        return phi;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::Assignment& assignment)
    {
        if (!m_programInterface.isBitfieldAccess(assignment.getLeftExpression()))
        {
            auto* lhs = visit(assignment.getLeftExpression());
            auto* rhs = visit(assignment.getRightExpression());
            if (assignment.getKind() != cld::Semantics::Assignment::Simple)
            {
                llvm::Value* load = m_builder.CreateLoad(lhs->getType()->getPointerElementType(), lhs,
                                                         assignment.getLeftExpression().getType().isVolatile());
                if (cld::Semantics::isArithmetic(assignment.getLeftExpression().getType()))
                {
                    load =
                        cast(load, assignment.getLeftExpression().getType(), assignment.getRightExpression().getType());
                }
                switch (assignment.getKind())
                {
                    case cld::Semantics::Assignment::Simple: CLD_UNREACHABLE;
                    case cld::Semantics::Assignment::Plus:
                        rhs = add(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Minus:
                        rhs = sub(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Divide:
                        rhs = div(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Multiply:
                        rhs = mul(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::Modulo:
                        rhs = mod(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::LeftShift:
                        rhs = shl(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::RightShift:
                        rhs = shr(load, assignment.getLeftExpression().getType(), rhs,
                                  assignment.getRightExpression().getType());
                        break;
                    case cld::Semantics::Assignment::BitAnd: rhs = m_builder.CreateAnd(load, rhs); break;
                    case cld::Semantics::Assignment::BitOr: rhs = m_builder.CreateOr(load, rhs); break;
                    case cld::Semantics::Assignment::BitXor: rhs = m_builder.CreateXor(load, rhs); break;
                }
                rhs = cast(rhs, assignment.getRightExpression().getType(), assignment.getLeftExpression().getType());
            }
            m_builder.CreateStore(rhs, lhs, assignment.getLeftExpression().getType().isVolatile());
            return m_builder.CreateLoad(lhs->getType()->getPointerElementType(), lhs,
                                        assignment.getLeftExpression().getType().isVolatile());
        }
        auto& memberAccess = cld::get<cld::Semantics::MemberAccess>(assignment.getLeftExpression().getVariant());
        auto* lhsRecord = visit(memberAccess.getRecordExpression());
        auto& type =
            std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant()) ?
                cld::get<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().getVariant())
                    .getElementType() :
                memberAccess.getRecordExpression().getType();
        if (std::holds_alternative<cld::Semantics::PointerType>(
                memberAccess.getRecordExpression().getType().getVariant()))
        {
            lhsRecord = m_builder.CreateLoad(lhsRecord, memberAccess.getRecordExpression().getType().isVolatile());
        }
        auto* rhsValue = visit(assignment.getRightExpression());

        auto& field = m_programInterface.getFields(type)[memberAccess.getMemberIndex()];
        llvm::Value* fieldPtr = nullptr;

        if (cld::Semantics::isStruct(type))
        {
            auto* zero = m_builder.getInt64(0);
            auto* member = m_builder.getInt32(field.layoutIndex);
            fieldPtr = m_builder.CreateInBoundsGEP(lhsRecord, {zero, member});
        }
        else
        {
            fieldPtr = m_builder.CreateBitCast(lhsRecord, llvm::PointerType::getUnqual(visit(*field.type)));
        }

        llvm::Value* loaded = m_builder.CreateLoad(fieldPtr, type.isVolatile());
        auto size = field.bitFieldBounds->second - field.bitFieldBounds->first;
        llvm::Value* mask = llvm::ConstantInt::get(rhsValue->getType(), (1u << size) - 1);
        if (assignment.getKind() != cld::Semantics::Assignment::Simple)
        {
            llvm::Value* load =
                m_builder.CreateAShr(loaded, llvm::ConstantInt::get(mask->getType(), field.bitFieldBounds->first));
            load = m_builder.CreateAnd(load, mask);
            load = cast(load, assignment.getLeftExpression().getType(), assignment.getRightExpression().getType());
            switch (assignment.getKind())
            {
                case cld::Semantics::Assignment::Simple: CLD_UNREACHABLE;
                case cld::Semantics::Assignment::Plus:
                    rhsValue = add(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Minus:
                    rhsValue = sub(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Divide:
                    rhsValue = div(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Multiply:
                    rhsValue = mul(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::Modulo:
                    rhsValue = mod(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::LeftShift:
                    rhsValue = shl(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::RightShift:
                    rhsValue = shr(load, assignment.getLeftExpression().getType(), rhsValue,
                                   assignment.getRightExpression().getType());
                    break;
                case cld::Semantics::Assignment::BitAnd: rhsValue = m_builder.CreateAnd(load, rhsValue); break;
                case cld::Semantics::Assignment::BitOr: rhsValue = m_builder.CreateOr(load, rhsValue); break;
                case cld::Semantics::Assignment::BitXor: rhsValue = m_builder.CreateXor(load, rhsValue); break;
            }
            load = cast(load, assignment.getRightExpression().getType(), assignment.getLeftExpression().getType());
        }
        rhsValue = m_builder.CreateAnd(rhsValue, mask);
        rhsValue =
            m_builder.CreateShl(rhsValue, llvm::ConstantInt::get(rhsValue->getType(), field.bitFieldBounds->first));
        mask = m_builder.CreateShl(mask, llvm::ConstantInt::get(mask->getType(), field.bitFieldBounds->first));
        mask = m_builder.CreateNot(mask);
        loaded = m_builder.CreateAnd(loaded, mask);
        auto* result = m_builder.CreateOr(loaded, rhsValue);
        m_builder.CreateStore(result, fieldPtr, type.isVolatile());
        return result;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::CommaExpression& commaExpression)
    {
        for (auto& iter : commaExpression.getCommaExpressions())
        {
            auto* instr = visit(iter.first);
            if (llvm::isa<llvm::Instruction>(instr) && instr->getNumUses() == 0
                && !llvm::cast<llvm::Instruction>(instr)->mayHaveSideEffects())
            {
                llvm::cast<llvm::Instruction>(instr)->eraseFromParent();
            }
        }
        return visit(commaExpression.getLastExpression());
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::CallExpression& call)
    {
        auto* function = visit(call.getFunctionExpression());
        auto cldFt = cld::get<cld::Semantics::FunctionType>(
            cld::get<cld::Semantics::PointerType>(call.getFunctionExpression().getType().getVariant())
                .getElementType()
                .getVariant());
        auto* ft = llvm::cast<llvm::FunctionType>(function->getType()->getPointerElementType());
        auto transformation = m_functionABITransformations.find(ft);
        CLD_ASSERT(transformation != m_functionABITransformations.end());
        bool isKandR = cldFt.isKandR();
        if (isKandR || cldFt.isLastVararg())
        {
            std::vector<std::pair<cld::Semantics::Type, std::string_view>> arguments;
            for (auto& iter : call.getArgumentExpressions())
            {
                arguments.emplace_back(iter.getType(), "");
            }
            auto callerFt =
                cld::Semantics::FunctionType::create(cldFt.getReturnType(), std::move(arguments), false, false);
            ft = llvm::cast<llvm::FunctionType>(visit(callerFt));
            transformation = m_functionABITransformations.find(ft);
            CLD_ASSERT(transformation != m_functionABITransformations.end());
            cldFt = cld::get<cld::Semantics::FunctionType>(callerFt.getVariant());
        }

        std::size_t llvmFnI = 0;
        std::vector<llvm::Value*> arguments;
        llvm::AllocaInst* returnSlot = nullptr;
        if (transformation->second.returnType == ABITransformations::PointerToTemporary)
        {
            llvmFnI = 1;
            llvm::IRBuilder<> temp(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
            returnSlot = temp.CreateAlloca(ft->getParamType(0)->getPointerElementType(), nullptr, "ret");
            returnSlot->setAlignment(llvm::Align(expression.getType().getAlignOf(m_programInterface)));
            m_builder.CreateLifetimeStart(returnSlot,
                                          m_builder.getInt64(expression.getType().getSizeOf(m_programInterface)));
            arguments.emplace_back(returnSlot);
        }
        else if (transformation->second.returnType == ABITransformations::Flattened
                 || transformation->second.returnType == ABITransformations::IntegerRegister)
        {
            llvm::IRBuilder<> temp(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
            returnSlot = temp.CreateAlloca(visit(expression.getType()), nullptr, "ret");
            returnSlot->setAlignment(llvm::Align(expression.getType().getAlignOf(m_programInterface)));
            m_builder.CreateLifetimeStart(returnSlot,
                                          m_builder.getInt64(expression.getType().getSizeOf(m_programInterface)));
        }

        for (auto iter = call.getArgumentExpressions().begin(); iter != call.getArgumentExpressions().end(); iter++)
        {
            const std::size_t currentIndex = iter - call.getArgumentExpressions().begin();
            auto* value = visit(*iter);
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
                    auto* load = llvm::cast<llvm::LoadInst>(value);
                    arguments.emplace_back(load->getPointerOperand());
                    load->eraseFromParent();
                    continue;
                }
                if (change == ABITransformations::PointerToTemporary)
                {
                    llvm::IRBuilder<> temp(&m_currentFunction->getEntryBlock(),
                                           m_currentFunction->getEntryBlock().begin());
                    auto* ret = temp.CreateAlloca(value->getType());
                    ret->setAlignment(llvm::Align(iter->getType().getAlignOf(m_programInterface)));
                    m_builder.CreateLifetimeStart(ret,
                                                  m_builder.getInt64(iter->getType().getSizeOf(m_programInterface)));
                    if (value->getType()->isX86_FP80Ty())
                    {
                        m_builder.CreateStore(value, ret);
                    }
                    else
                    {
                        auto* load = llvm::cast<llvm::LoadInst>(value);
                        m_builder.CreateMemCpy(ret, llvm::MaybeAlign(), load->getPointerOperand(), llvm::MaybeAlign(),
                                               iter->getType().getSizeOf(m_programInterface));
                        load->eraseFromParent();
                    }
                    arguments.emplace_back(ret);
                    continue;
                }
                // Integer register
                auto* load = llvm::cast<llvm::LoadInst>(value);
                auto* integer = m_builder.CreateBitCast(
                    load->getPointerOperand(), llvm::PointerType::getUnqual(m_builder.getIntNTy(
                                                   m_module.getDataLayout().getTypeAllocSizeInBits(load->getType()))));
                arguments.emplace_back(m_builder.CreateLoad(integer, load->isVolatile()));
                load->eraseFromParent();
                continue;
            }
            auto* load = llvm::cast<llvm::LoadInst>(value);
            auto& multiIndex =
                cld::get<ABITransformations::MultipleArgs>(transformation->second.arguments[currentIndex]);
            if (multiIndex.size == 1)
            {
                auto* paramType = ft->getParamType(llvmFnI);
                auto* cast =
                    m_builder.CreateBitCast(load->getPointerOperand(), llvm::PointerType::getUnqual(paramType));
                arguments.emplace_back(m_builder.CreateLoad(cast, load->isVolatile()));
            }
            else
            {
                CLD_ASSERT(multiIndex.size == 2);
                auto* firstType = ft->getParamType(llvmFnI);
                auto* secondType = ft->getParamType(llvmFnI + 1);
                auto* cast =
                    m_builder.CreateBitCast(load->getPointerOperand(),
                                            llvm::PointerType::getUnqual(llvm::StructType::get(firstType, secondType)));
                auto* firstValue = m_builder.CreateInBoundsGEP(cast, {m_builder.getInt64(0), m_builder.getInt32(0)});
                auto* secondValue = m_builder.CreateInBoundsGEP(cast, {m_builder.getInt64(0), m_builder.getInt32(1)});
                arguments.emplace_back(m_builder.CreateLoad(firstValue, load->isVolatile()));
                arguments.emplace_back(m_builder.CreateLoad(secondValue, load->isVolatile()));
            }
            load->eraseFromParent();
            llvmFnI += multiIndex.size;
        }
        if (isKandR)
        {
            function = m_builder.CreateBitCast(function, llvm::PointerType::getUnqual(ft));
        }
        auto* result = m_builder.CreateCall(
            isKandR ? ft : llvm::cast<llvm::FunctionType>(function->getType()->getPointerElementType()), function,
            arguments);
        applyFunctionAttributes(*result, ft, cldFt);
        switch (transformation->second.returnType)
        {
            case ABITransformations::Unchanged: return result;
            case ABITransformations::PointerToTemporary:
            {
                return m_builder.CreateLoad(returnSlot);
            }
            case ABITransformations::IntegerRegister:
            case ABITransformations::Flattened:
            {
                auto* cast = m_builder.CreateBitCast(returnSlot, llvm::PointerType::getUnqual(ft->getReturnType()));
                m_builder.CreateStore(result, cast);
                return m_builder.CreateLoad(returnSlot);
            }
            default: CLD_UNREACHABLE;
        }
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression,
                       const cld::Semantics::CompoundLiteral& compoundLiteral)
    {
        auto* type = visit(expression.getType());
        if (compoundLiteral.hasStaticLifetime())
        {
            llvm::Constant* constant = nullptr;
            constant = llvm::cast<llvm::Constant>(visit(compoundLiteral.getInitializer(), expression.getType(), type));
            type = constant->getType();
            auto* global = new llvm::GlobalVariable(m_module, type, true, llvm::GlobalValue::PrivateLinkage, constant);
            global->setAlignment(llvm::MaybeAlign(expression.getType().getAlignOf(m_programInterface)));
            return global;
        }
        llvm::IRBuilder<> temp(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
        auto* var = temp.CreateAlloca(type);
        var->setAlignment(llvm::Align(expression.getType().getAlignOf(m_programInterface)));
        if (m_builder.GetInsertBlock())
        {
            visit(compoundLiteral.getInitializer(), expression.getType(), var);
        }
        return var;
    }

    llvm::Value* CLD_NULLABLE visit(const cld::Semantics::Initializer& initializer, const cld::Semantics::Type& type,
                                    std::variant<llvm::Value*, llvm::Type*> pointer)
    {
        return cld::match(
            initializer,
            [&](const cld::Semantics::Expression& expression) -> llvm::Value* {
                if (std::holds_alternative<llvm::Value*>(pointer))
                {
                    auto* value = visit(expression);
                    if (cld::Semantics::isStringLiteralExpr(expression))
                    {
                        m_builder.CreateMemCpy(cld::get<llvm::Value*>(pointer), llvm::MaybeAlign(), value,
                                               llvm::MaybeAlign(), expression.getType().getSizeOf(m_programInterface));
                        return nullptr;
                    }
                    m_builder.CreateStore(value, cld::get<llvm::Value*>(pointer), type.isVolatile());
                    return nullptr;
                }
                if (cld::Semantics::isStringLiteralExpr(expression))
                {
                    auto& constant = cld::get<cld::Semantics::Constant>(expression.getVariant());
                    return getStringLiteralData(visit(expression.getType())->getArrayElementType(),
                                                constant.getValue());
                }
                return visit(expression);
            },
            [&](const cld::Semantics::InitializerList& initializerList) -> llvm::Value* {
                if (std::holds_alternative<llvm::Type*>(pointer))
                {
                    return visitStaticInitializerList(initializerList, type, cld::get<llvm::Type*>(pointer));
                }
                auto* value = cld::get<llvm::Value*>(pointer);
                m_builder.CreateMemSet(value, m_builder.getInt8(0), type.getSizeOf(m_programInterface),
                                       llvm::MaybeAlign(), type.isVolatile());
                for (auto& [path, expression] : initializerList.getFields())
                {
                    auto* subValue = visit(expression);
                    auto* currentPointer = value;
                    const cld::Semantics::Type* currentType = &type;
                    std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
                    for (auto iter : path)
                    {
                        if (cld::Semantics::isStruct(*currentType))
                        {
                            auto fields = m_programInterface.getFields(*currentType);
                            currentPointer = m_builder.CreateInBoundsGEP(
                                currentPointer, {m_builder.getInt64(0), m_builder.getInt32(fields[iter].layoutIndex)});
                            currentType = fields[iter].type.get();
                            bitFieldBounds = fields[iter].bitFieldBounds;
                        }
                        else if (cld::Semantics::isUnion(*currentType))
                        {
                            auto fields = m_programInterface.getFields(*currentType);
                            currentType = fields[iter].type.get();
                            currentPointer = m_builder.CreateBitCast(currentPointer,
                                                                     llvm::PointerType::getUnqual(visit(*currentType)));
                            bitFieldBounds = fields[iter].bitFieldBounds;
                        }
                        else
                        {
                            currentType = &cld::Semantics::getArrayElementType(*currentType);
                            currentPointer = m_builder.CreateInBoundsGEP(
                                currentPointer, {m_builder.getInt64(0), m_builder.getInt64(iter)});
                        }
                    }
                    if (!bitFieldBounds)
                    {
                        if (cld::Semantics::isStringLiteralExpr(expression))
                        {
                            m_builder.CreateMemCpy(currentPointer, llvm::MaybeAlign(), subValue, llvm::MaybeAlign(),
                                                   expression.getType().getSizeOf(m_programInterface));
                            continue;
                        }
                        m_builder.CreateStore(subValue, currentPointer, type.isVolatile());
                        continue;
                    }
                    llvm::Value* loaded = m_builder.CreateLoad(currentPointer, type.isVolatile());
                    auto size = bitFieldBounds->second - bitFieldBounds->first;
                    llvm::Value* mask = llvm::ConstantInt::get(subValue->getType(), (1u << size) - 1);
                    subValue = m_builder.CreateAnd(subValue, mask);
                    subValue = m_builder.CreateShl(subValue,
                                                   llvm::ConstantInt::get(subValue->getType(), bitFieldBounds->first));
                    mask = m_builder.CreateShl(mask, llvm::ConstantInt::get(mask->getType(), bitFieldBounds->first));
                    mask = m_builder.CreateNot(mask);
                    loaded = m_builder.CreateAnd(loaded, mask);
                    auto* result = m_builder.CreateOr(loaded, subValue);
                    m_builder.CreateStore(result, currentPointer, type.isVolatile());
                    continue;
                }
                return nullptr;
            });
    }
};
} // namespace

std::unique_ptr<llvm::TargetMachine> cld::CGLLVM::generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                               Triple triple, llvm::Optional<llvm::Reloc::Model> reloc,
                                                               llvm::CodeGenOpt::Level ol)
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
        targetM->createTargetMachine(module.getTargetTriple(), "generic", "", {}, reloc, {}, ol));
    module.setDataLayout(machine->createDataLayout());
    CodeGenerator codeGenerator(module, program, program.getSourceObject(), triple);
    codeGenerator.visit(program.getTranslationUnit());
    return machine;
}


#pragma once

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>

#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/SemanticUtil.hpp>
#include <cld/Support/Filesystem.hpp>
#include <cld/Support/ScopeExit.hpp>
#include <cld/Support/ValueReset.h>

#include <numeric>

#include "ABIImplementation.hpp"
#include "Codegen.hpp"

namespace cld::CGLLVM
{
namespace detail
{
template <class T, class = void>
struct hasGetAlign : std::false_type
{
};

template <class T>
struct hasGetAlign<T, std::void_t<decltype(std::declval<T>().getAlign())>> : std::true_type
{
};
} // namespace detail

struct Value
{
    llvm::Value* value;
    llvm::MaybeAlign alignment;

    template <class T>
    Value(T* value, std::enable_if_t<!detail::hasGetAlign<T>{} || std::is_same_v<llvm::LoadInst, T>, llvm::MaybeAlign>
                        alignment = {})
        : value(value), alignment(alignment)
    {
        CLD_ASSERT(!this->value || !this->value->getType()->isPointerTy()
                   || this->value->getType()->getPointerElementType()->isFunctionTy() || this->alignment.hasValue());
    }

    Value(std::nullptr_t) : value(nullptr) {}

    template <class T, std::enable_if_t<detail::hasGetAlign<T>{} && !std::is_same_v<llvm::LoadInst, T>>* = nullptr>
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

class CodeGenerator final
{
    llvm::Module& m_module;
    const Semantics::ProgramInterface& m_programInterface;
    const SourceInterface& m_sourceInterface;
    const CGLLVM::Options& m_options;
    Triple m_triple;

    std::unordered_map<const Semantics::Useable*, Value> m_lvalues;

    using TypeVariantKey = std::variant<Semantics::StructType, Semantics::UnionType>;

    std::unordered_map<TypeVariantKey, llvm::Type*> m_types;
    std::unordered_map<std::variant<Semantics::StructType, Semantics::UnionType, Semantics::EnumType>, llvm::DIType*>
        m_debugTypes;

    //    struct ABITransformations
    //    {
    //        enum Change
    //        {
    //            Unchanged,
    //            IntegerRegister,
    //            PointerToTemporary,
    //            OnStack,
    //            Flattened
    //        };
    //        struct MultipleArgs
    //        {
    //            std::size_t size;
    //        };
    //        using Variant = std::variant<Change, MultipleArgs>;
    //        Change returnType;              // Never OnStack
    //        std::vector<Variant> arguments; // Never Flattened
    //    };
    //    std::unordered_map<Semantics::FunctionType, ABITransformations> m_functionABITransformations;

    std::unordered_map<Semantics::LoopStatements, llvm::BasicBlock*> m_continueTargets;
    std::unordered_map<Semantics::BreakableStatements, llvm::BasicBlock*> m_breakTargets;
    std::unordered_map<const Semantics::LabelStatement*, llvm::BasicBlock*> m_labels;
    struct Switch
    {
        llvm::SwitchInst* llvmSwitch;
        llvm::BasicBlock* defaultBlock;
    };
    std::unordered_map<const Semantics::SwitchStatement*, Switch> m_switches;

    llvm::IRBuilder<> m_builder{m_module.getContext()};
    llvm::Function* m_currentFunction = nullptr;
    // const ABITransformations* m_currentFunctionABI = nullptr;
    llvm::AllocaInst* m_returnSlot = nullptr;
    std::unordered_map<std::shared_ptr<const Semantics::ExpressionBase>, llvm::Value*> m_valSizes;
    std::unordered_map<const Semantics::VariableDeclaration * CLD_NON_NULL, llvm::AllocaInst*> m_stackSaves;
    std::unordered_map<std::string_view, llvm::GlobalVariable*> m_cGlobalVariables;

    //    std::vector<llvm::Type*> flatten(llvm::Type* type)
    //    {
    //        std::vector<llvm::Type*> result;
    //        if (type->isStructTy())
    //        {
    //            for (std::size_t i = 0; i < type->getStructNumElements(); i++)
    //            {
    //                auto* element = type->getStructElementType(i);
    //                auto temp = flatten(element);
    //                result.insert(result.end(), temp.begin(), temp.end());
    //            }
    //        }
    //        else if (type->isArrayTy())
    //        {
    //            auto temp = flatten(type->getArrayElementType());
    //            for (std::size_t i = 0; i < type->getArrayNumElements(); i++)
    //            {
    //                result.insert(result.end(), temp.begin(), temp.end());
    //            }
    //        }
    //        else
    //        {
    //            result.emplace_back(type);
    //        }
    //        return result;
    //    }
    //
    //    std::tuple<ABITransformations::Variant, llvm::Type * CLD_NON_NULL, llvm::Type * CLD_NULLABLE>
    //        flattenSingleArg(llvm::Type* type, std::uint8_t* takenIntegers = nullptr, std::uint8_t* takenFloats =
    //        nullptr)
    //    {
    //        constexpr std::uint8_t availableIntegerRegisters = 6;
    //        constexpr std::uint8_t availableFloatingPointRegisters = 8;
    //        ABITransformations::Variant dest;
    //        std::size_t retIndex = 0;
    //        std::array<llvm::Type*, 2> ret = {};
    //
    //        std::uint8_t takenIntegerRegisters = takenIntegers ? *takenIntegers : 0;
    //        std::uint8_t takenFloatingPointRegisters = takenFloats ? *takenFloats : 0;
    //        const auto flat = flatten(type);
    //        auto iter = flat.begin();
    //        while (iter != flat.end())
    //        {
    //            const auto begin = iter;
    //            bool encounteredInteger = false;
    //            std::size_t size = 0;
    //            std::size_t currentAlignment = 0;
    //            while (size < 8 && iter != flat.end())
    //            {
    //                const auto alignment = m_module.getDataLayout().getABITypeAlign(*iter).value();
    //                const auto temp = cld::roundUpTo(size, alignment);
    //                if (temp >= 8)
    //                {
    //                    break;
    //                }
    //                size = temp;
    //                if ((*iter)->isIntOrPtrTy())
    //                {
    //                    encounteredInteger = true;
    //                }
    //                // If we have a single fp80 then it is passed normally if it wasn't part of the struct but passed
    //                // on the stack if it was in the struct. Very weird I know. Only need to handle this case of a
    //                // single fp80 as it is 128 bytes on x64 (due to padding) and we wouldn't be here if the struct
    //                // was any larger
    //                if ((*iter)->isX86_FP80Ty())
    //                {
    //                    if (*iter != type)
    //                    {
    //                        return {ABITransformations::OnStack, type, nullptr};
    //                    }
    //                }
    //                currentAlignment = std::max(currentAlignment, alignment);
    //                const auto typeSize = m_module.getDataLayout().getTypeAllocSize(*iter).getKnownMinSize();
    //                size += typeSize;
    //                iter++;
    //            }
    //            size = cld::roundUpTo(size, currentAlignment);
    //            if (encounteredInteger)
    //            {
    //                // We encountered at least one integer therefore even if a floating point type was in there
    //                // it's gotta go into a integer register
    //                if (takenIntegerRegisters >= availableIntegerRegisters)
    //                {
    //                    return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged,
    //                    type,
    //                            nullptr};
    //                }
    //
    //                takenIntegerRegisters++;
    //                if (type->isStructTy() && !std::holds_alternative<ABITransformations::MultipleArgs>(dest))
    //                {
    //                    dest = ABITransformations::MultipleArgs{};
    //                }
    //                if (type->isStructTy())
    //                {
    //                    ret[retIndex++] = m_builder.getIntNTy(size * 8);
    //                }
    //                else
    //                {
    //                    ret[retIndex++] = type;
    //                }
    //                if (auto* multiArgs = std::get_if<ABITransformations::MultipleArgs>(&dest))
    //                {
    //                    multiArgs->size++;
    //                }
    //
    //                continue;
    //            }
    //            if (std::distance(begin, iter) == 2 && (*begin)->isFloatTy() && ((*(begin + 1))->isFloatTy()))
    //            {
    //                // Two floats can be packed as a single 64 bit value into a xmm register. This is represented as
    //                // a vector in LLVM IR
    //                if (takenFloatingPointRegisters >= availableFloatingPointRegisters)
    //                {
    //                    return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged,
    //                    type,
    //                            nullptr};
    //                }
    //                takenFloatingPointRegisters++;
    //                if (!std::holds_alternative<ABITransformations::MultipleArgs>(dest))
    //                {
    //                    dest = ABITransformations::MultipleArgs{};
    //                }
    //                ret[retIndex++] = llvm::FixedVectorType::get(m_builder.getFloatTy(), 2);
    //                cld::get<ABITransformations::MultipleArgs>(dest).size++;
    //                continue;
    //            }
    //
    //            CLD_ASSERT(std::distance(begin, iter) == 1);
    //            // Must be a floating point type because if it were integer it would have taken the
    //            // encounteredInteger branch above
    //            if (takenFloatingPointRegisters >= availableFloatingPointRegisters)
    //            {
    //                return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
    //                        nullptr};
    //            }
    //            takenFloatingPointRegisters++;
    //            if (type->isStructTy() && !std::holds_alternative<ABITransformations::MultipleArgs>(dest))
    //            {
    //                dest = ABITransformations::MultipleArgs{};
    //            }
    //            ret[retIndex++] = *begin;
    //            if (auto* multiArgs = std::get_if<ABITransformations::MultipleArgs>(&dest))
    //            {
    //                multiArgs->size++;
    //            }
    //        }
    //        if (takenFloats)
    //        {
    //            *takenFloats = takenFloatingPointRegisters;
    //        }
    //        if (takenIntegers)
    //        {
    //            *takenIntegers = takenIntegerRegisters;
    //        }
    //        return {dest, ret[0], ret[1]};
    //    }
    //
    //    ABITransformations applyPlatformABI(llvm::Type*& returnType, std::vector<llvm::Type*>& arguments)
    //    {
    //        ABITransformations transformations;
    //        transformations.returnType = ABITransformations::Unchanged;
    //        transformations.arguments.resize(arguments.size(), ABITransformations::Unchanged);
    //        if (m_triple.getPlatform() == cld::Platform::Windows && m_triple.getArchitecture() ==
    //        cld::Architecture::x86_64)
    //        {
    //            for (auto iter = arguments.begin(); iter != arguments.end(); iter++)
    //            {
    //                if (!(*iter)->isStructTy() && !(*iter)->isX86_FP80Ty())
    //                {
    //                    continue;
    //                }
    //                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(*iter);
    //                if (m_module.getDataLayout().isLegalInteger(size))
    //                {
    //                    transformations.arguments[iter - arguments.begin()] = ABITransformations::IntegerRegister;
    //                    *iter = m_builder.getIntNTy(size);
    //                }
    //                else
    //                {
    //                    transformations.arguments[iter - arguments.begin()] = ABITransformations::PointerToTemporary;
    //                    *iter = llvm::PointerType::getUnqual(*iter);
    //                }
    //            }
    //            if (returnType->isVoidTy())
    //            {
    //                return transformations;
    //            }
    //            std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
    //            if (m_module.getDataLayout().isLegalInteger(size) && returnType->isStructTy())
    //            {
    //                transformations.returnType = ABITransformations::IntegerRegister;
    //                returnType = m_builder.getIntNTy(size);
    //            }
    //            else if (!m_module.getDataLayout().isLegalInteger(size))
    //            {
    //                transformations.returnType = ABITransformations::PointerToTemporary;
    //                arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
    //                returnType = m_builder.getVoidTy();
    //            }
    //        }
    //        else if (m_triple.getArchitecture() == cld::Architecture::x86_64)
    //        {
    //            std::uint8_t takenIntegerRegisters = 0;
    //            std::uint8_t takenFloatingPointRegisters = 0;
    //            std::size_t transFormIndex = 0;
    //            for (auto arg = arguments.begin(); arg != arguments.end(); transFormIndex++, arg++)
    //            {
    //                auto& dest = transformations.arguments[transFormIndex];
    //                if (m_module.getDataLayout().getTypeAllocSizeInBits(*arg) > 128)
    //                {
    //                    dest = ABITransformations::OnStack;
    //                    *arg = llvm::PointerType::getUnqual(*arg);
    //                    continue;
    //                }
    //                std::pair<llvm::Type*, llvm::Type*> types;
    //                std::tie(dest, types.first, types.second) =
    //                    flattenSingleArg(*arg, &takenIntegerRegisters, &takenFloatingPointRegisters);
    //                if (auto* change = std::get_if<ABITransformations::Change>(&dest);
    //                    change && *change == ABITransformations::OnStack)
    //                {
    //                    *arg = llvm::PointerType::getUnqual(*arg);
    //                }
    //                else if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
    //                {
    //                    *arg = types.first;
    //                    if (types.second)
    //                    {
    //                        arg++;
    //                        arg = arguments.insert(arg, types.second);
    //                    }
    //                }
    //            }
    //            if (!returnType->isVoidTy())
    //            {
    //                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
    //                if (size > 128)
    //                {
    //                    transformations.returnType = ABITransformations::PointerToTemporary;
    //                    arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
    //                    returnType = m_builder.getVoidTy();
    //                }
    //                else
    //                {
    //                    auto* prevReturnType = returnType;
    //                    auto [temp, firstType, secondType] = flattenSingleArg(returnType);
    //                    if (secondType)
    //                    {
    //                        returnType = llvm::StructType::get(firstType, secondType);
    //                    }
    //                    else
    //                    {
    //                        returnType = firstType;
    //                    }
    //                    if (prevReturnType->isStructTy())
    //                    {
    //                        transformations.returnType = ABITransformations::Flattened;
    //                    }
    //                }
    //            }
    //        }
    //        return transformations;
    //    }
    //
    //    template <class T>
    //    void applyFunctionAttributes(
    //        T& attributeApply, llvm::FunctionType* CLD_NON_NULL functionType, const Semantics::FunctionType& ft,
    //        const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>* paramDecls = nullptr)
    //    {
    //        auto transformations = m_functionABITransformations.find(ft);
    //        CLD_ASSERT(transformations != m_functionABITransformations.end());
    //        std::size_t argStart = 0;
    //        if (transformations->second.returnType == ABITransformations::PointerToTemporary)
    //        {
    //            attributeApply.addAttribute(1, llvm::Attribute::StructRet);
    //            attributeApply.addAttribute(1, llvm::Attribute::NoAlias);
    //            argStart = 1;
    //        }
    //        else if (transformations->second.returnType == ABITransformations::Unchanged
    //                 && Semantics::isInteger(ft.getReturnType())
    //                 && cld::get<Semantics::PrimitiveType>(ft.getReturnType().getVariant()).getBitCount() < 32)
    //        {
    //            if (cld::get<Semantics::PrimitiveType>(ft.getReturnType().getVariant()).isSigned())
    //            {
    //                attributeApply.addAttribute(0, llvm::Attribute::SExt);
    //            }
    //            else
    //            {
    //                attributeApply.addAttribute(0, llvm::Attribute::ZExt);
    //            }
    //        }
    //        std::size_t origArgI = 0;
    //        for (std::size_t i = argStart; i < functionType->getNumParams(); origArgI++)
    //        {
    //            auto& argument = transformations->second.arguments[origArgI];
    //            if (std::holds_alternative<ABITransformations::Change>(argument))
    //            {
    //                auto change = cld::get<ABITransformations::Change>(argument);
    //                if (change == ABITransformations::Unchanged)
    //                {
    //                    auto& arg = ft.getArguments()[origArgI].first;
    //                    if (Semantics::isInteger(arg)
    //                        && cld::get<Semantics::PrimitiveType>(arg.getVariant()).getBitCount() < 32)
    //                    {
    //                        if (cld::get<Semantics::PrimitiveType>(arg.getVariant()).isSigned())
    //                        {
    //                            attributeApply.addParamAttr(i, llvm::Attribute::SExt);
    //                        }
    //                        else
    //                        {
    //                            attributeApply.addParamAttr(i, llvm::Attribute::ZExt);
    //                        }
    //                    }
    //                }
    //                else if (change == ABITransformations::OnStack)
    //                {
    //                    auto& arg = ft.getArguments()[origArgI].first;
    //                    attributeApply.addParamAttr(
    //                        i, llvm::Attribute::getWithByValType(m_builder.getContext(),
    //                                                             functionType->getParamType(i)->getPointerElementType()));
    //                    attributeApply.addParamAttr(
    //                        i, llvm::Attribute::getWithAlignment(
    //                               m_builder.getContext(), std::max(llvm::Align(arg.getAlignOf(m_programInterface)),
    //                                                                m_module.getDataLayout().getPointerABIAlignment(0))));
    //                    i++;
    //                    continue;
    //                }
    //                if (change == ABITransformations::PointerToTemporary || !paramDecls)
    //                {
    //                    i++;
    //                    continue;
    //                }
    //                auto& paramDecl = (*paramDecls)[origArgI];
    //                auto* operand = functionType->getParamType(i);
    //                i++;
    //                if (change == ABITransformations::Unchanged)
    //                {
    //                    auto* var = createAllocaAtTop(operand, paramDecl->getNameToken()->getText());
    //                    var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
    //                    m_lvalues.emplace(paramDecl.get(), var);
    //                    continue;
    //                }
    //                auto* var = createAllocaAtTop(visit(paramDecl->getType()), paramDecl->getNameToken()->getText());
    //                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
    //                m_lvalues.emplace(paramDecl.get(), var);
    //            }
    //            else if (std::holds_alternative<ABITransformations::MultipleArgs>(argument))
    //            {
    //                auto& multiArgs = cld::get<ABITransformations::MultipleArgs>(argument);
    //                i += multiArgs.size;
    //                if (!paramDecls)
    //                {
    //                    continue;
    //                }
    //                auto& paramDecl = (*paramDecls)[origArgI];
    //                auto* var = createAllocaAtTop(visit(paramDecl->getType()), paramDecl->getNameToken()->getText());
    //                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
    //                m_lvalues.emplace(paramDecl.get(), var);
    //            }
    //        }
    //    }

    std::optional<llvm::DIBuilder> m_debugInfo;
    std::vector<llvm::DIFile*> m_fileIdToFile;
    llvm::DIScope* m_currentDebugScope = nullptr;
    std::vector<llvm::DIScope*> m_scopeIdToScope{m_programInterface.getScopes().size()};

    std::unique_ptr<ABIImplementation> m_abi;

public:
    explicit CodeGenerator(llvm::Module& module, const Semantics::ProgramInterface& programInterface,
                           const cld::SourceInterface& sourceInterface, cld::Triple triple,
                           const cld::CGLLVM::Options& options);

    ~CodeGenerator()
    {
        if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
        {
            m_debugInfo->finalize();
        }
    }

    const llvm::Module& getModule() const
    {
        return m_module;
    }

    const Semantics::ProgramInterface& getProgramInterface() const
    {
        return m_programInterface;
    }

    const cld::SourceInterface& getSourceInterface() const
    {
        return m_sourceInterface;
    }

    llvm::Function* getCurrentFunction() const
    {
        return m_currentFunction;
    }

    template <class T>
    std::enable_if_t<std::is_base_of_v<llvm::Value, T>, Value> valueOf(T* value, llvm::MaybeAlign alignment = {})
    {
        if constexpr (std::is_same_v<T, llvm::Function>)
        {
            return Value(static_cast<llvm::Value*>(value), m_module.getDataLayout().getFunctionPtrAlign().getValueOr(
                                                               m_module.getDataLayout().getPointerABIAlignment(0)));
        }
        else if constexpr (detail::hasGetAlign<T>{} && !std::is_same_v<llvm::LoadInst, T>)
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

    void addLValue(const Semantics::Useable& lvalue, Value value);

    llvm::Value* toBool(llvm::Value* value);

    Value add(Value lhs, const Semantics::Type& lhsType, Value rhs, const Semantics::Type& rhsType);

    Value sub(Value lhs, const Semantics::Type& lhsType, Value rhs, const Semantics::Type& rhsType);

    Value mul(Value lhs, const Semantics::Type& lhsType, Value rhs, const Semantics::Type&);

    Value div(Value lhs, const Semantics::Type& lhsType, Value rhs, const Semantics::Type&);

    Value mod(Value lhs, const Semantics::Type& lhsType, Value rhs, const Semantics::Type&);

    Value shl(Value lhs, const Semantics::Type&, Value rhs, const Semantics::Type& rhsType);

    Value shr(Value lhs, const Semantics::Type& lhsType, Value rhs, const Semantics::Type& rhsType);

    Value cast(Value value, const Semantics::Type& from, const Semantics::Type& to);

    template <class T>
    Value createLoad(T*, std::enable_if_t<!detail::hasGetAlign<T>{}, bool>) = delete;

    Value createLoad(Value ptr, bool isVolatile);

    template <class T>
    void createStore(T*, std::enable_if_t<!detail::hasGetAlign<T>{}, bool>) = delete;

    void createStore(llvm::Value* value, Value ptr, bool isVolatile);

    llvm::AllocaInst* createAllocaAtTop(llvm::Type* type, std::string_view name = {});

    Value createGEP(Value ptr, llvm::ArrayRef<llvm::Value*> indices);

    Value createInBoundsGEP(Value ptr, llvm::ArrayRef<llvm::Value*> indices);

    Value createPointerCast(Value ptr, llvm::Type* pointerType);

    Value createBitCast(Value ptr, llvm::Type* pointerType, bool checked = true);

    Value createSafeBitCast(Value ptr, llvm::Type* pointerType);

    Value getStringLiteralData(llvm::Type* elementType, const Semantics::Constant::Variant& value);

    void runDestructors(std::size_t from, std::size_t toExclusive);

    void runDestructors(std::size_t scope)
    {
        runDestructors(scope, m_programInterface.getScopes()[scope].previousScope);
    }

    llvm::IRBuilder<>& getBuilder()
    {
        return m_builder;
    }

    Value boolToi1(Value value);

    llvm::DIFile* getFile(const Lexer::CToken* CLD_NULLABLE iter) const
    {
        if (!iter)
        {
            return m_currentDebugScope->getFile();
        }
        return m_fileIdToFile[iter->getFileId()];
    }

    unsigned getLine(const Lexer::CToken* CLD_NULLABLE iter) const
    {
        if (!iter)
        {
            return 0;
        }
        return iter->getLine(m_sourceInterface);
    }

    unsigned getColumn(const Lexer::CToken* CLD_NULLABLE iter) const
    {
        if (!iter)
        {
            return 0;
        }
        return iter->getLine(m_sourceInterface);
    }

    llvm::DILocation* getLocation(const Lexer::CToken* CLD_NULLABLE iter) const
    {
        CLD_ASSERT(m_currentDebugScope);
        return llvm::DILocation::get(m_module.getContext(), getLine(iter), getColumn(iter), m_currentDebugScope);
    }

    void visitVoidExpression(const Semantics::ExpressionBase& expression)
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

    llvm::Type* visit(const Semantics::Type& type);

    llvm::DIType* visitDebug(const Semantics::Type& type);

    void visit(const Semantics::TranslationUnit& translationUnit);

    Value visit(const Semantics::FunctionDeclaration& declaration);

    Value visit(const Semantics::VariableDeclaration& declaration);

    void visit(const Semantics::FunctionDefinition& functionDefinition);

    void visit(const Semantics::CompoundStatement& compoundStatement);

    void visit(const Semantics::Statement& statement);

    void visit(const Semantics::ReturnStatement& returnStatement);

    void visit(const Semantics::ForStatement& forStatement);

    void visit(const Semantics::IfStatement& ifStatement);

    void visit(const Semantics::HeadWhileStatement& headWhileStatement);

    void visit(const Semantics::FootWhileStatement& footWhileStatement);

    void visit(const Semantics::BreakStatement& breakStatement);

    void visit(const Semantics::ContinueStatement& continueStatement);

    void visit(const Semantics::SwitchStatement& switchStatement);

    void visit(const Semantics::DefaultStatement& defaultStatement);

    void visit(const Semantics::CaseStatement& caseStatement, llvm::BasicBlock* bb = nullptr);

    void visit(const Semantics::GotoStatement& gotoStatement);

    void visit(const Semantics::LabelStatement& labelStatement);

    void visit(const Semantics::GNUASMStatement&);

    Value visit(const Semantics::ExpressionBase& expression);

    Value visit(const Semantics::Constant& constant);

    Value visit(const Semantics::DeclarationRead& declarationRead);

    Value visit(const Semantics::Conversion& conversion);

    Value visit(const Semantics::MemberAccess& memberAccess);

    Value visit(const Semantics::BinaryOperator& binaryExpression);

    Value visit(const Semantics::Cast& cast);

    Value visit(const Semantics::UnaryOperator& unaryOperator);

    Value visit(const Semantics::SizeofOperator& sizeofOperator);

    Value visit(const Semantics::SubscriptOperator& subscriptOperator);

    Value visit(const Semantics::Conditional& conditional);

    Value visit(const Semantics::Assignment& assignment);

    Value visit(const Semantics::CommaExpression& commaExpression);

    Value visit(const Semantics::CallExpression& call);

    Value visit(const Semantics::CompoundLiteral& compoundLiteral);

    Value x64LoadFromStack(Value vaList, llvm::Type* destType);

    Value visit(const Semantics::BuiltinVAArg& vaArg);

    Value visit(const Semantics::BuiltinOffsetOf& offsetOf);

    Value visit(const Semantics::Initializer& initializer, const Semantics::Type& type,
                std::variant<Value, llvm::Type*> pointer);
};

} // namespace cld::CGLLVM

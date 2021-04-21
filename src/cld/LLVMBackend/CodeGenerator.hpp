
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
    llvm::AllocaInst* m_returnSlot = nullptr;
    std::unordered_map<std::shared_ptr<const Semantics::ExpressionBase>, llvm::Value*> m_valSizes;
    std::unordered_map<const Semantics::VariableDeclaration * CLD_NON_NULL, llvm::AllocaInst*> m_stackSaves;
    std::unordered_map<std::string_view, llvm::GlobalVariable*> m_cGlobalVariables;

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

    Value createBitCast(Value ptr, llvm::Type* pointerType);

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

    Value visit(const Semantics::BuiltinVAArg& vaArg);

    Value visit(const Semantics::BuiltinOffsetOf& offsetOf);

    Value visit(const Semantics::Initializer& initializer, const Semantics::Type& type,
                std::variant<Value, llvm::Type*> pointer);
};

} // namespace cld::CGLLVM

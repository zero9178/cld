#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <cld/Support/Expected.hpp>
#include <cld/Support/ScopeExit.hpp>
#include <cld/Support/ValueReset.h>
#include <cld/Support/function_ref.hpp>

#include <functional>

#include "ConstValue.hpp"
#include "Message.hpp"
#include "ProgramInterface.hpp"
#include "Syntax.hpp"

namespace cld::Semantics
{
class SemanticAnalysis final : public ProgramInterface
{
    std::int64_t m_currentScope = 0;
    const SourceInterface& m_sourceInterface;
    llvm::raw_ostream* m_reporter;
    bool* m_errors;
    std::function<bool(std::string_view)> m_definedCallback;
    struct FunctionScope
    {
        const FunctionDefinition* CLD_NON_NULL currentFunction;
        std::unordered_map<std::string_view, const LabelStatement * CLD_NON_NULL> labels;
    };
    std::int64_t m_currentFunctionScope = -1;
    std::vector<FunctionScope> m_functionScopes;
    bool m_inStaticInitializer = false;
    bool m_inFunctionPrototype = false;
    bool m_extensionsEnabled = false;
    std::vector<std::pair<Lexer::CTokenIterator, GotoStatement * CLD_NON_NULL>> m_scheduledGotos;
    std::vector<LoopStatements> m_loopStatements;
    std::vector<BreakableStatements> m_breakableStatements;
    struct SwitchStack
    {
        const SwitchStatement* CLD_NON_NULL switchStatement;
        std::map<llvm::APSInt, const CaseStatement * CLD_NON_NULL> cases;
        const DefaultStatement* CLD_NULLABLE defaultStmt;
    };
    std::vector<SwitchStack> m_switchStatements;
    std::unordered_map<Lexer::CTokenIterator, std::size_t> m_anonymousTagToID;

    [[nodiscard]] ValueReset<bool> changeFunctionPrototypeScope(bool newValue)
    {
        auto result = cld::ValueReset(m_inFunctionPrototype, m_inFunctionPrototype);
        m_inFunctionPrototype = newValue;
        return result;
    }

    void resolveGotos();

    [[nodiscard]] auto pushFunctionScope(const FunctionDefinition& functionDefinition)
    {
        m_functionScopes.push_back({&functionDefinition, {}});
        m_currentFunctionScope = m_functionScopes.size() - 1;
        return cld::ScopeExit([this] {
            resolveGotos();
            m_currentFunctionScope = -1;
        });
    }

    [[nodiscard]] bool inFunction() const
    {
        return m_currentFunctionScope >= 0;
    }

    [[nodiscard]] FunctionScope* getCurrentFunctionScope()
    {
        if (m_currentFunctionScope >= 0)
        {
            return &m_functionScopes[m_currentFunctionScope];
        }
        return nullptr;
    }

    [[nodiscard]] ValueReset<std::int64_t> pushScope()
    {
        m_scopes[m_currentScope].subScopes.push_back(m_scopes.size());
        m_scopes.push_back({m_currentScope, {}, {}, {}});
        m_currentScope = m_scopes.size() - 1;
        return cld::ValueReset(m_currentScope, m_scopes.back().previousScope);
    }

    [[nodiscard]] auto pushLoop(LoopStatements loop)
    {
        m_loopStatements.push_back(loop);
        cld::match(loop, [&](auto&& value) { m_breakableStatements.emplace_back(value); });
        return cld::ScopeExit([&] {
            m_loopStatements.pop_back();
            m_breakableStatements.pop_back();
        });
    }

    [[nodiscard]] auto pushSwitch(const SwitchStatement& switchStatement)
    {
        m_breakableStatements.push_back(&switchStatement);
        m_switchStatements.push_back({&switchStatement, {}, nullptr});
        return cld::ScopeExit([&] {
            m_breakableStatements.pop_back();
            m_switchStatements.pop_back();
        });
    }

    bool m_inParameter = false;

    template <class T>
    void handleParameterList(Type& type, const Syntax::ParameterTypeList* CLD_NULLABLE parameterTypeList,
                             T&& returnTypeLoc,
                             cld::function_ref<void(const Type&, Lexer::CTokenIterator,
                                                    const std::vector<Syntax::DeclarationSpecifier>&, bool)>
                                 paramCallback = {});

    template <class T>
    void handleArray(Type& type, const std::vector<Syntax::TypeQualifier>& typeQualifiers,
                     const Syntax::AssignmentExpression* CLD_NULLABLE assignmentExpression,
                     const Lexer::CToken* isStatic, bool valArray, T&& returnTypeLoc);

    [[nodiscard]] bool isTypedef(std::string_view name) const;

    [[nodiscard]] bool isTypedefInScope(std::string_view name) const;

    [[nodiscard]] const Semantics::Type* CLD_NULLABLE getTypedef(std::string_view name);

    [[nodiscard]] const DeclarationInScope::Variant* CLD_NULLABLE getBuiltinFuncDecl(std::string_view name);

    [[nodiscard]] const DeclarationInScope::Variant* CLD_NULLABLE lookupDecl(std::string_view name)
    {
        return lookupDecl(name, m_currentScope);
    }

    [[nodiscard]] const DeclarationInScope::Variant* CLD_NULLABLE lookupDecl(std::string_view name, std::int64_t scope);

    bool log(const Message& message);

    static std::tuple<bool, bool, bool> getQualifiers(const std::vector<Syntax::TypeQualifier>& typeQualifiers);

    using PossiblyAbstractQualifierRef =
        std::variant<const Syntax::AbstractDeclarator * CLD_NULLABLE, const Syntax::Declarator * CLD_NON_NULL>;

    using DeclarationOrSpecifierQualifier =
        std::variant<const Syntax::TypeSpecifier * CLD_NON_NULL, const Syntax::TypeQualifier * CLD_NON_NULL,
                     const Syntax::StorageClassSpecifier * CLD_NON_NULL, const Syntax::FunctionSpecifier * CLD_NON_NULL,
                     const Syntax::GNUAttributes * CLD_NON_NULL>;

    Type typeSpecifiersToType(bool isConst, bool isVolatile,
                              const std::vector<const Syntax::TypeSpecifier * CLD_NON_NULL>& typeSpec);

    Type declaratorsToTypeImpl(const std::vector<DeclarationOrSpecifierQualifier>& directAbstractDeclaratorParentheses,
                               const PossiblyAbstractQualifierRef& parameterList,
                               const std::vector<Syntax::Declaration>& declarations = {},
                               cld::function_ref<void(const Type&, Lexer::CTokenIterator,
                                                      const std::vector<Syntax::DeclarationSpecifier>&, bool)>
                                   paramCallback = {});

    Type primitiveTypeSpecifiersToType(bool isConst, bool isVolatile,
                                       const std::vector<const Syntax::TypeSpecifier * CLD_NON_NULL>& typeSpecs);

    Scope& getCurrentScope()
    {
        return m_scopes[m_currentScope];
    }

    const Scope& getCurrentScope() const
    {
        return m_scopes[m_currentScope];
    }

    bool hasFlexibleArrayMember(const Type& type) const;

    bool typesAreCompatible(const Type& lhs, const Type& rhs, bool leftIsFuncDefinition = false) const;

    Type defaultArgumentPromotion(const Type& type) const;

    Type integerPromotion(const Type& type) const;

    Type compositeType(const Type& lhs, const Type& rhs) const;

    ExpressionValue lvalueConversion(ExpressionValue&& expression);

    static Type lvalueConversion(Type type);

    ExpressionValue defaultArgumentPromotion(ExpressionValue&& type);

    ExpressionValue integerPromotion(ExpressionValue&& expression);

    static ExpressionValue toBool(ExpressionValue&& expression);

    void arithmeticConversion(ExpressionValue& lhs, ExpressionValue& rhs);

    void arithmeticConversion(Type& lhs, ExpressionValue& rhs);

    bool isModifiableLValue(const ExpressionBase& expression) const;

    bool isConst(const Type& type) const;

    void reportNoMember(const Type& recordType, const Lexer::CToken& identifier);

    std::optional<std::pair<Type, const Field * CLD_NON_NULL>>
        checkMemberAccess(const Type& recordType, const Syntax::PostFixExpression& postFixExpr,
                          const Lexer::CToken& identifier);

    ExpressionValue checkIncrementAndDecrement(const Syntax::Node& node, UnaryOperator::Kind kind,
                                               ExpressionValue&& value, Lexer::CTokenIterator opToken);

    ExpressionValue doBitOperators(ExpressionValue&& lhs, BinaryOperator::Kind kind, Lexer::CTokenIterator token,
                                   ExpressionValue&& rhs);

    ExpressionValue doLogicOperators(ExpressionValue&& lhs, BinaryOperator::Kind kind, Lexer::CTokenIterator token,
                                     ExpressionValue&& rhs);

public:
    enum Mode
    {
        Integer,
        Arithmetic,
        Initialization
    };

private:
    ConstValue evaluate(const ExpressionBase& expression, Mode mode,
                        cld::function_ref<void(const Message&)> logger) const;

    template <class T>
    [[nodiscard]] const T* CLD_NULLABLE lookupType(std::string_view name) const
    {
        return ProgramInterface::lookupType<T>(name, m_currentScope);
    }

    template <class T>
    Type declaratorsToType(const std::vector<T>& declarationOrSpecifierQualifiers,
                           const Syntax::AbstractDeclarator* declarator = nullptr)
    {
        std::vector<DeclarationOrSpecifierQualifier> temp(declarationOrSpecifierQualifiers.size());
        std::transform(declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(), temp.begin(),
                       [](auto&& value) {
                           return cld::match(value, [](auto&& valueInVariant) -> DeclarationOrSpecifierQualifier {
                               return &valueInVariant;
                           });
                       });
        return declaratorsToTypeImpl(std::move(temp), declarator);
    }

    template <class T>
    Type declaratorsToType(const std::vector<T>& declarationOrSpecifierQualifiers, const Syntax::Declarator& declarator,
                           const std::vector<Syntax::Declaration>& declarations = {},
                           cld::function_ref<void(const Type&, Lexer::CTokenIterator,
                                                  const std::vector<Syntax::DeclarationSpecifier>&, bool)>
                               paramCallback = {})
    {
        std::vector<DeclarationOrSpecifierQualifier> temp(declarationOrSpecifierQualifiers.size());
        std::transform(declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(), temp.begin(),
                       [](auto&& value) {
                           return cld::match(value, [](auto&& valueInVariant) -> DeclarationOrSpecifierQualifier {
                               return &valueInVariant;
                           });
                       });
        return declaratorsToTypeImpl(std::move(temp), &declarator, declarations, paramCallback);
    }

    bool doAssignmentLikeConstraints(const Type& lhsTyp, ExpressionValue& rhsValue,
                                     cld::function_ref<void()> mustBeArithmetic,
                                     cld::function_ref<void()> mustBeArithmeticOrPointer,
                                     cld::function_ref<void()> incompleteType,
                                     cld::function_ref<void()> incompatibleTypes, cld::function_ref<void()> notICE,
                                     cld::function_ref<void(const ConstValue&)> notNull,
                                     cld::function_ref<void()> mustBePointer,
                                     cld::function_ref<void()> voidFunctionPointers);

    ExpressionValue doSingleElementInitialization(const Syntax::Node& node, const Type& type,
                                                  ExpressionValue&& expression, bool staticLifetime, std::size_t* size);

    void checkForIllegalSwitchJumps(std::tuple<const Lexer::CToken&, const Lexer::CToken&> loc,
                                    const SwitchStatement& switchStatement, bool isCaseOrDefault);

    void createBuiltins();

    [[nodiscard]] ValueReset<bool> enableExtensions(bool extensions);

    [[nodiscard]] bool extensionsEnabled(const Lexer::CToken* token = nullptr);

public:
    explicit SemanticAnalysis(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter = &llvm::errs(),
                              bool* errors = nullptr, std::function<bool(std::string_view)> definedCallback = {})
        : m_sourceInterface(sourceInterface),
          m_reporter(reporter),
          m_errors(errors),
          m_definedCallback(std::move(definedCallback))
    {
        createBuiltins();
    }

    Expected<ConstValue, std::vector<Message>> evaluateConstantExpression(const ExpressionBase& constantExpression,
                                                                          Mode mode = Integer);

    [[nodiscard]] const LanguageOptions& getLanguageOptions() const override
    {
        return m_sourceInterface.getLanguageOptions();
    }

    TranslationUnit visit(const Syntax::TranslationUnit& node);

    std::vector<TranslationUnit::Variant> visit(const Syntax::FunctionDefinition& node);

    using DeclRetVariant = std::variant<std::unique_ptr<Declaration>, std::shared_ptr<const ExpressionBase>>;

    std::vector<DeclRetVariant> visit(const Syntax::Declaration& node);

    std::unique_ptr<Statement> visit(const Syntax::Statement& node);

    ExpressionValue visit(const Syntax::Expression& node);

    ExpressionValue visit(const Syntax::AssignmentExpression& node);

    ExpressionValue visit(const Syntax::PrimaryExpression& node);

    ExpressionValue visit(const Syntax::PrimaryExpressionIdentifier& node);

    ExpressionValue visit(const Syntax::PrimaryExpressionConstant& node);

    ExpressionValue visit(const Syntax::PrimaryExpressionParentheses& node);

    ExpressionValue visit(const Syntax::PrimaryExpressionBuiltinVAArg& node);

    ExpressionValue visit(const Syntax::PrimaryExpressionBuiltinOffsetOf& node);

    ExpressionValue visit(const Syntax::PostFixExpression& node);

    ExpressionValue visit(const Syntax::PostFixExpressionPrimaryExpression& node);

    ExpressionValue visit(const Syntax::PostFixExpressionSubscript& node);

    ExpressionValue visit(const Syntax::PostFixExpressionDot& node);

    ExpressionValue visit(const Syntax::PostFixExpressionArrow& node);

    ExpressionValue visit(const Syntax::PostFixExpressionFunctionCall& node);

    ExpressionValue visit(const Syntax::PostFixExpressionIncrement& node);

    ExpressionValue visit(const Syntax::PostFixExpressionDecrement& node);

    ExpressionValue visit(const Syntax::PostFixExpressionTypeInitializer& node);

    ExpressionValue visit(const Syntax::UnaryExpression& node);

    ExpressionValue visit(const Syntax::UnaryExpressionPostFixExpression& node);

    ExpressionValue visit(const Syntax::UnaryExpressionUnaryOperator& node);

    ExpressionValue visit(const Syntax::UnaryExpressionSizeOf& node);

    ExpressionValue visit(const Syntax::UnaryExpressionDefined& node);

    ExpressionValue visit(const Syntax::CastExpression& node);

    ExpressionValue visit(const Syntax::Term& node);

    ExpressionValue visit(const Syntax::AdditiveExpression& node);

    ExpressionValue visit(const Syntax::ShiftExpression& node);

    ExpressionValue visit(const Syntax::RelationalExpression& node);

    ExpressionValue visit(const Syntax::EqualityExpression& node);

    ExpressionValue visit(const Syntax::BitAndExpression& node);

    ExpressionValue visit(const Syntax::BitXorExpression& node);

    ExpressionValue visit(const Syntax::BitOrExpression& node);

    ExpressionValue visit(const Syntax::LogicalAndExpression& node);

    ExpressionValue visit(const Syntax::LogicalOrExpression& node);

    ExpressionValue visit(const Syntax::ConditionalExpression& node);

    Initializer visit(const Syntax::Initializer& node, const Type& type, bool staticLifetime = false,
                      std::size_t* size = nullptr);

    Initializer visit(const Syntax::InitializerList& node, const Type& type, bool staticLifetime = false,
                      std::size_t* size = nullptr);

    [[nodiscard]] std::unique_ptr<CompoundStatement> visit(const Syntax::CompoundStatement& node,
                                                           bool pushScope = true);

    [[nodiscard]] std::vector<CompoundStatement::Variant> visit(const Syntax::CompoundItem& node);

    [[nodiscard]] std::unique_ptr<ReturnStatement> visit(const Syntax::ReturnStatement& node);

    [[nodiscard]] std::unique_ptr<IfStatement> visit(const Syntax::IfStatement& node);

    [[nodiscard]] std::unique_ptr<ForStatement> visit(const Syntax::ForStatement& node);

    [[nodiscard]] std::unique_ptr<HeadWhileStatement> visit(const Syntax::HeadWhileStatement& node);

    [[nodiscard]] std::unique_ptr<FootWhileStatement> visit(const Syntax::FootWhileStatement& node);

    [[nodiscard]] std::unique_ptr<BreakStatement> visit(const Syntax::BreakStatement& node);

    [[nodiscard]] std::unique_ptr<ContinueStatement> visit(const Syntax::ContinueStatement& node);

    [[nodiscard]] std::unique_ptr<SwitchStatement> visit(const Syntax::SwitchStatement& node);

    [[nodiscard]] std::unique_ptr<DefaultStatement> visit(const Syntax::DefaultStatement& node);

    [[nodiscard]] std::unique_ptr<CaseStatement> visit(const Syntax::CaseStatement& node);

    [[nodiscard]] std::unique_ptr<GotoStatement> visit(const Syntax::GotoStatement& node);

    [[nodiscard]] std::unique_ptr<LabelStatement> visit(const Syntax::LabelStatement& node);

    [[nodiscard]] std::unique_ptr<GNUASMStatement> visit(const Syntax::GNUASMStatement& node);
};

} // namespace cld::Semantics

#pragma once

#include <cld/Support/Expected.hpp>
#include <cld/Support/ScopeExit.hpp>
#include <cld/Support/ValueReset.h>
#include <cld/Support/function_ref.hpp>

#include <functional>

#include "ConstValue.hpp"
#include "DiagnosticUtil.hpp"
#include "Message.hpp"
#include "ProgramInterface.hpp"
#include "Syntax.hpp"

namespace cld::Semantics
{
template <class T = void>
struct ParsedAttribute
{
    Lexer::CTokenIterator name;
    std::vector<diag::PointRange> expressionRanges;
    T attribute;
};

template <>
struct ParsedAttribute<void>
{
    enum TriedOn
    {
        Nothing = 0,
        Function = 0b1,
        Tag = 0b10,
        Type = 0b100,
        Variable = 0b1000,
    };
    std::underlying_type_t<TriedOn> attempts;
    Lexer::CTokenIterator name;
    std::vector<diag::PointRange> expressionRanges;
    AllAttributes attribute;
};

class SemanticAnalysis final : public ProgramInterface
{
    std::size_t m_currentScope = GLOBAL_SCOPE;
    const SourceInterface& m_sourceInterface;
    llvm::raw_ostream* m_reporter;
    bool* m_errors;
    std::function<bool(std::string_view)> m_definedCallback;
    struct FunctionScope
    {
        const FunctionDefinition* CLD_NON_NULL currentFunction;
        std::unordered_map<std::string_view, const LabelStatement * CLD_NON_NULL> labels;
    };
    std::size_t m_currentFunctionScope = END_OF_SCOPES;
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
        return cld::ScopeExit(
            [this]
            {
                resolveGotos();
                m_currentFunctionScope = END_OF_SCOPES;
            });
    }

    [[nodiscard]] bool inFunction() const
    {
        return m_currentFunctionScope != END_OF_SCOPES;
    }

    [[nodiscard]] FunctionScope* getCurrentFunctionScope()
    {
        if (m_currentFunctionScope != END_OF_SCOPES)
        {
            return &m_functionScopes[m_currentFunctionScope];
        }
        return nullptr;
    }

    void diagnoseUnusedLocals();

    [[nodiscard]] auto pushScope()
    {
        m_scopes[m_currentScope].subScopes.push_back(m_scopes.size());
        m_scopes.push_back({m_currentScope, {}, {}, {}});
        m_currentScope = m_scopes.size() - 1;
        return cld::ScopeExit(
            [&, prev = m_scopes.back().previousScope]
            {
                diagnoseUnusedLocals();
                m_currentScope = prev;
            });
    }

    [[nodiscard]] auto pushLoop(LoopStatements loop)
    {
        m_loopStatements.push_back(loop);
        cld::match(loop, [&](auto&& value) { m_breakableStatements.emplace_back(value); });
        return cld::ScopeExit(
            [&]
            {
                m_loopStatements.pop_back();
                m_breakableStatements.pop_back();
            });
    }

    [[nodiscard]] auto pushSwitch(const SwitchStatement& switchStatement)
    {
        m_breakableStatements.push_back(&switchStatement);
        m_switchStatements.push_back({&switchStatement, {}, nullptr});
        return cld::ScopeExit(
            [&]
            {
                m_breakableStatements.pop_back();
                m_switchStatements.pop_back();
            });
    }

    bool m_inParameter = false;

    void handleParameterList(
        IntrVarValue<Type>& type, const Syntax::ParameterTypeList* CLD_NULLABLE parameterTypeList,
        const diag::PointRange& returnTypeLoc,
        cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&,
                               std::vector<ParsedAttribute<>>&&)>
            paramCallback = {});

    void handleArray(IntrVarValue<Type>& type, const std::vector<Syntax::TypeQualifier>& typeQualifiers,
                     const Syntax::AssignmentExpression* CLD_NULLABLE assignmentExpression,
                     const Lexer::CToken* isStatic, bool valArray, const diag::PointRange& returnTypeLoc);

    [[nodiscard]] bool isTypedef(std::string_view name) const;

    [[nodiscard]] bool isTypedefInScope(std::string_view name) const;

    [[nodiscard]] const TypedefInfo* CLD_NULLABLE getTypedef(std::string_view name)
    {
        if (auto* info = std::get_if<TypedefInfo*>(lookupDecl(name)))
        {
            return *info;
        }
        return nullptr;
    }

    [[nodiscard]] const DeclarationInScope::Variant* CLD_NULLABLE getBuiltinFuncDecl(std::string_view name);

    [[nodiscard]] const DeclarationInScope::Variant* CLD_NULLABLE lookupDecl(std::string_view name)
    {
        return lookupDecl(name, m_currentScope);
    }

    [[nodiscard]] const DeclarationInScope::Variant* CLD_NULLABLE lookupDecl(std::string_view name, std::int64_t scope);

    bool log(const Message& message);

    static std::tuple<bool, bool, bool> getQualifiers(const std::vector<Syntax::TypeQualifier>& typeQualifiers);

    static std::tuple<bool, bool, bool>
        getQualifiers(const std::vector<std::variant<Syntax::TypeQualifier, Syntax::GNUAttributes>>& typeQualifiers);

    using PossiblyAbstractQualifierRef =
        std::variant<const Syntax::AbstractDeclarator * CLD_NULLABLE, const Syntax::Declarator * CLD_NON_NULL>;

    using DeclarationOrSpecifierQualifier =
        std::variant<const Syntax::TypeSpecifier * CLD_NON_NULL, const Syntax::TypeQualifier * CLD_NON_NULL,
                     const Syntax::StorageClassSpecifier * CLD_NON_NULL, const Syntax::FunctionSpecifier * CLD_NON_NULL,
                     const Syntax::GNUAttributes * CLD_NON_NULL>;

    IntrVarValue<Type> typeSpecifiersToType(bool isConst, bool isVolatile,
                                            const std::vector<const Syntax::TypeSpecifier * CLD_NON_NULL>& typeSpec);

    IntrVarValue<Type> structOrUnionSpecifierToType(bool isConst, bool isVolatile,
                                                    const Syntax::StructOrUnionSpecifier& structOrUnion);

    IntrVarValue<Type> applyDeclaratorsImpl(
        IntrVarValue<Type>&& type, const PossiblyAbstractQualifierRef& parameterList,
        const std::vector<Syntax::Declaration>& declarations = {},
        cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&,
                               std::vector<ParsedAttribute<>>&&)>
            paramCallback = {},
        std::vector<ParsedAttribute<>>* attributesOut = nullptr);

    IntrVarValue<Type>
        qualifiersToTypeImpl(const std::vector<DeclarationOrSpecifierQualifier>& directAbstractDeclaratorParentheses,
                             std::vector<ParsedAttribute<>>* attributesOut = nullptr);

    PrimitiveType
        primitiveTypeSpecifiersToType(bool isConst, bool isVolatile,
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

    bool typesAreCompatible(const Type& lhs, const Type& rhs, bool leftIsFuncDefinition = false);

    IntrVarValue<Type> defaultArgumentPromotion(IntrVarValue<Type> type);

    IntrVarValue<Type> integerPromotion(IntrVarValue<Type> type);

    IntrVarValue<Type> compositeType(const Type& lhs, const Type& rhs);

    IntrVarPtr<ExpressionBase> lvalueConversion(IntrVarPtr<ExpressionBase>&& expression);

    IntrVarValue<Type> lvalueConversion(IntrVarValue<Type> type);

    IntrVarPtr<ExpressionBase> defaultArgumentPromotion(IntrVarPtr<ExpressionBase>&& type);

    IntrVarPtr<ExpressionBase> integerPromotion(IntrVarPtr<ExpressionBase>&& expression);

    std::unique_ptr<Conversion> toBool(IntrVarPtr<ExpressionBase>&& expression);

    void arithmeticConversion(
        std::variant<IntrVarPtr<ExpressionBase> * CLD_NON_NULL, IntrVarValue<Type> * CLD_NON_NULL> lhs,
        IntrVarPtr<ExpressionBase>& rhs);

    bool isModifiableLValue(const ExpressionBase& expression) const;

    bool isConst(const Type& type) const;

    void reportNoMember(const Type& recordType, const Lexer::CToken& identifier);

    void reportNotApplicableAttributes(const std::vector<ParsedAttribute<>>& attributes);

    std::optional<std::pair<IntrVarValue<Type>, const Field * CLD_NON_NULL>>
        checkMemberAccess(const Type& recordType, const Syntax::PostFixExpression& postFixExpr,
                          const Lexer::CToken& identifier);

    IntrVarPtr<ExpressionBase> checkIncrementAndDecrement(const Syntax::Node& node, UnaryOperator::Kind kind,
                                                          IntrVarPtr<ExpressionBase>&& value,
                                                          Lexer::CTokenIterator opToken);

    bool checkVectorBinaryOp(const IntrVarPtr<ExpressionBase>& lhs, Lexer::CTokenIterator token,
                             const IntrVarPtr<ExpressionBase>& rhs);

    void checkVectorCompoundAssign(const IntrVarPtr<ExpressionBase>& lhs, const Type& lhsType,
                                   Lexer::CTokenIterator token, const IntrVarPtr<ExpressionBase>& rhs);

    void checkForDeprecatedType(const Type& type);

    VectorType vectorCompResultType(const VectorType& vectorType, const LanguageOptions& options);

    std::unique_ptr<BinaryOperator> doBitOperators(IntrVarPtr<ExpressionBase>&& lhs, BinaryOperator::Kind kind,
                                                   Lexer::CTokenIterator token, IntrVarPtr<ExpressionBase>&& rhs);

    std::unique_ptr<BinaryOperator> doLogicOperators(IntrVarPtr<ExpressionBase>&& lhs, BinaryOperator::Kind kind,
                                                     Lexer::CTokenIterator token, IntrVarPtr<ExpressionBase>&& rhs);

    enum class NPCCheck
    {
        Success,
        WrongType,
        NotConstExpr
    };

    cld::Expected<NPCCheck, ConstValue> checkNullPointerConstant(const ExpressionBase& expression);

    cld::Expected<NPCCheck, ConstValue> checkPointerOperandsForNPC(const ExpressionBase& possibleNPC,
                                                                   const Type& otherType);

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
    [[nodiscard]] T* CLD_NULLABLE lookupType(std::string_view name)
    {
        return ProgramInterface::lookupType<T>(name, m_currentScope);
    }

    template <class T>
    [[nodiscard]] const T* CLD_NULLABLE lookupType(std::string_view name) const
    {
        return ProgramInterface::lookupType<T>(name, m_currentScope);
    }

    template <class T>
    IntrVarValue<Type> declaratorsToType(const std::vector<T>& declarationOrSpecifierQualifiers,
                                         const Syntax::AbstractDeclarator* declarator = nullptr,
                                         std::vector<ParsedAttribute<>>* attributes = nullptr)
    {
        auto type = qualifiersToType(declarationOrSpecifierQualifiers, attributes);
        return applyDeclarator(std::move(type), declarator, attributes);
    }

    template <class T>
    IntrVarValue<Type> declaratorsToType(
        const std::vector<T>& declarationOrSpecifierQualifiers, const Syntax::Declarator& declarator,
        const std::vector<Syntax::Declaration>& declarations = {},
        cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&,
                               std::vector<ParsedAttribute<>>&&)>
            paramCallback = {},
        std::vector<ParsedAttribute<>>* attributes = nullptr)
    {
        auto type = qualifiersToType(declarationOrSpecifierQualifiers, attributes);
        return applyDeclarator(std::move(type), declarator, declarations, std::move(paramCallback), attributes);
    }

    template <class T>
    IntrVarValue<Type> qualifiersToType(const std::vector<T>& declarationOrSpecifierQualifiers,
                                        std::vector<ParsedAttribute<>>* attributes)
    {
        std::vector<DeclarationOrSpecifierQualifier> temp(declarationOrSpecifierQualifiers.size());
        std::transform(declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(), temp.begin(),
                       [](auto&& value)
                       {
                           return cld::match(value,
                                             [](auto&& valueInVariant) -> DeclarationOrSpecifierQualifier
                                             { return &valueInVariant; });
                       });
        return qualifiersToTypeImpl(temp, attributes);
    }

    IntrVarValue<Type> applyDeclarator(IntrVarValue<Type> type, const Syntax::AbstractDeclarator* declarator = nullptr,
                                       std::vector<ParsedAttribute<>>* attributes = nullptr)
    {
        return applyDeclaratorsImpl(std::move(type), declarator, {}, {}, attributes);
    }

    IntrVarValue<Type> applyDeclarator(
        IntrVarValue<Type> type, const Syntax::Declarator& declarator,
        const std::vector<Syntax::Declaration>& declarations = {},
        cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&,
                               std::vector<ParsedAttribute<>>&&)>
            paramCallback = {},
        std::vector<ParsedAttribute<>>* attributes = nullptr)
    {
        return applyDeclaratorsImpl(std::move(type), &declarator, declarations, std::move(paramCallback), attributes);
    }

    bool doAssignmentLikeConstraints(const Type& lhsTyp, IntrVarPtr<ExpressionBase>& rhsValue,
                                     cld::function_ref<void()> mustBeArithmetic,
                                     cld::function_ref<void()> mustBeArithmeticOrPointer,
                                     cld::function_ref<void()> incompleteType,
                                     cld::function_ref<void()> incompatibleTypes, cld::function_ref<void()> notICE,
                                     cld::function_ref<void(const ConstValue&)> notNull,
                                     cld::function_ref<void()> mustBePointer,
                                     cld::function_ref<void()> voidFunctionPointers);

    IntrVarPtr<ExpressionBase> doSingleElementInitialization(const Syntax::Node& node, const Type& type,
                                                             IntrVarPtr<ExpressionBase>&& expression,
                                                             bool staticLifetime, std::size_t* size);

    void checkForIllegalSwitchJumps(std::tuple<const Lexer::CToken&, const Lexer::CToken&> loc,
                                    const SwitchStatement& switchStatement, bool isCaseOrDefault);

    void createBuiltinTypes();

    void createAttributes();

    std::pair<tsl::ordered_map<std::string_view, DeclarationInScope>::iterator, bool>
        insertTypedef(TypedefInfo typedefInfo);

    [[nodiscard]] ValueReset<bool> enableExtensions(bool extensions);

    [[nodiscard]] bool extensionsEnabled(const Lexer::CToken* token = nullptr);

    std::unique_ptr<CallExpression> visitVAStart(const Syntax::PostFixExpressionFunctionCall& node,
                                                 IntrVarPtr<ExpressionBase>&& function);

    std::unique_ptr<CallExpression> visitPrefetch(const Syntax::PostFixExpressionFunctionCall& node,
                                                  IntrVarPtr<ExpressionBase>&& function);

    std::unique_ptr<CallExpression> visitExpectWithProbability(const Syntax::PostFixExpressionFunctionCall& node,
                                                               IntrVarPtr<ExpressionBase>&& function);

    std::unique_ptr<CallExpression> visitSyncBuiltinWithT(const Syntax::PostFixExpressionFunctionCall& node,
                                                          IntrVarPtr<ExpressionBase>&& function);

    IntrVarPtr<ExpressionBase> checkFunctionArg(std::size_t i, IntrVarValue<Type> paramType,
                                                IntrVarPtr<ExpressionBase>&& expression);

    bool checkFunctionCount(const ExpressionBase& function, const FunctionType& ft,
                            const Syntax::PostFixExpressionFunctionCall& node);

    std::unique_ptr<FunctionDeclaration>
        visitFunctionDeclaration(Lexer::CTokenIterator loc, FunctionType&& type,
                                 const Syntax::StorageClassSpecifier* storageClassSpecifier, bool isInline,
                                 std::vector<ParsedAttribute<>>&& attributes);

public:
    explicit SemanticAnalysis(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter = &llvm::errs(),
                              bool* errors = nullptr, std::function<bool(std::string_view)> definedCallback = {})
        : m_sourceInterface(sourceInterface),
          m_reporter(reporter),
          m_errors(errors),
          m_definedCallback(std::move(definedCallback))
    {
        createBuiltinTypes();
        createAttributes();
    }

    Expected<ConstValue, std::vector<Message>> evaluateConstantExpression(const ExpressionBase& constantExpression,
                                                                          Mode mode = Integer);

    [[nodiscard]] const LanguageOptions& getLanguageOptions() const override
    {
        return m_sourceInterface.getLanguageOptions();
    }

    TranslationUnit visit(const Syntax::TranslationUnit& node);

    std::unique_ptr<FunctionDefinition> visit(const Syntax::FunctionDefinition& node);

    using DeclRetVariant = std::variant<IntrVarPtr<Declaration>, std::shared_ptr<const ExpressionBase>>;

    std::vector<DeclRetVariant> visit(const Syntax::Declaration& node);

    IntrVarPtr<Statement> visit(const Syntax::Statement& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::Expression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::AssignmentExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PrimaryExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PrimaryExpressionIdentifier& node);

    std::unique_ptr<Constant> visit(const Syntax::PrimaryExpressionConstant& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PrimaryExpressionParentheses& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PrimaryExpressionBuiltinVAArg& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PrimaryExpressionBuiltinOffsetOf& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionPrimaryExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionSubscript& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionDot& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionArrow& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionFunctionCall& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionIncrement& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionDecrement& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::PostFixExpressionTypeInitializer& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::UnaryExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::UnaryExpressionPostFixExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::UnaryExpressionUnaryOperator& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::UnaryExpressionSizeOf& node);

    std::unique_ptr<Constant> visit(const Syntax::UnaryExpressionDefined& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::CastExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::Term& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::AdditiveExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::ShiftExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::RelationalExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::EqualityExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::BitAndExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::BitXorExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::BitOrExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::LogicalAndExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::LogicalOrExpression& node);

    IntrVarPtr<ExpressionBase> visit(const Syntax::ConditionalExpression& node);

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

    [[nodiscard]] std::vector<ParsedAttribute<>> visit(const Syntax::GNUAttributes& node);

    using AffectsFunction = std::variant<not_null<FunctionDeclaration>, not_null<FunctionDefinition>>;

    using AffectsType = std::variant<std::pair<not_null<IntrVarValue<Type>>, diag::PointRange>>;

    using AffectsTag =
        std::variant<not_null<StructInfo>, not_null<UnionInfo>, not_null<EnumInfo>, not_null<TypedefInfo>>;

    using AffectsTagType = VariantUnion<AffectsType, AffectsTag>;

    using AffectsVariable = std::variant<not_null<VariableDeclaration>>;

    using AffectsTypeVariable = VariantUnion<AffectsType, AffectsVariable>;

    using AffectsTagTypeVariable = VariantUnion<AffectsTag, AffectsType, AffectsVariable>;

    using AffectsTagVariable = VariantUnion<AffectsTag, AffectsType, AffectsVariable>;

    using AffectsVariableFunction = VariantUnion<AffectsFunction, AffectsVariable>;

    using AffectsTagVariableFunction = VariantUnion<AffectsFunction, AffectsVariable, AffectsTag>;

    using AffectsAll = VariantUnion<AffectsFunction, AffectsVariable, AffectsType, AffectsTag>;

    struct FunctionContext
    {
        bool hasInlineSpecifier;
    };

    using CallingContext = std::variant<std::monostate, FunctionContext>;

    [[nodiscard]] std::vector<ParsedAttribute<>> applyAttributes(AffectsAll applicant,
                                                                 std::vector<ParsedAttribute<>>&& attributes,
                                                                 const CallingContext& context = {});

    template <class T>
    using SuitableApplicant = typename std::conditional_t<
        variantTypesContainV<T, FunctionAttribute>,
        std::conditional<variantTypesContainV<T, VariableAttribute>,
                         std::conditional_t<variantTypesContainV<T, TypeAttribute>, AffectsTagVariableFunction,
                                            AffectsVariableFunction>,
                         AffectsFunction>,
        std::conditional<
            variantTypesContainV<T, VariableAttribute>,
            std::conditional_t<variantTypesContainV<T, TypeAttribute>, AffectsTagVariable, AffectsVariable>,
            AffectsTag>>::type;

    template <class T>
    auto applyFallback(SuitableApplicant<T> applicant, const ParsedAttribute<T>& attribute)
        -> std::enable_if_t<variantTypesContainV<T, VariantUnion<FunctionAttribute, TypeAttribute, VariableAttribute>>>
    {
        cld::match(applicant, [&](auto holder) { holder->addAttribute(attribute.attribute); });
    }

    void apply(AffectsTagVariableFunction applicant, const ParsedAttribute<AlignedAttribute>& attribute);

    void apply(AffectsTypeVariable applicant, const ParsedAttribute<VectorSizeAttribute>& attribute);

    void apply(AffectsVariableFunction declaration, const ParsedAttribute<UsedAttribute>& attribute);

    void apply(AffectsFunction declaration, const ParsedAttribute<GnuInlineAttribute>& attribute,
               const CallingContext& context);

    void apply(AffectsVariableFunction declaration, const ParsedAttribute<DllImportAttribute>& attribute);

    void apply(AffectsTagVariableFunction declaration, const ParsedAttribute<DeprecatedAttribute>& attribute);

    void apply(AffectsFunction declaration, const ParsedAttribute<NonnullAttribute>& attribute);

private:
    std::unordered_map<std::string,
                       std::optional<AllAttributes> (SemanticAnalysis::*)(const Syntax::GNUAttributes::GNUAttribute&)>
        m_attributesParser;

    template <class T>
    std::optional<AllAttributes> parseAttribute(const Syntax::GNUAttributes::GNUAttribute& attribute);

    bool parseMember(std::uint64_t& unsignedInteger, Lexer::CTokenIterator attributeName,
                     const Syntax::AssignmentExpression* expression);

    bool parseMember(Lexer::CTokenIterator& identifier, Lexer::CTokenIterator attributeName,
                     const Syntax::AssignmentExpression* expression);

    bool parseMember(const Useable*& useable, Lexer::CTokenIterator attributeName,
                     const Syntax::AssignmentExpression* expression);

    bool parseMember(std::string& text, Lexer::CTokenIterator attributeName,
                     const Syntax::AssignmentExpression* expression);

    template <class T>
    bool parseMember(std::optional<T>& optional, Lexer::CTokenIterator attributeName,
                     const Syntax::AssignmentExpression* expression);
};

} // namespace cld::Semantics

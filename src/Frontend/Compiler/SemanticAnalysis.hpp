#pragma once

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/ScopeExit.h>

#include <unordered_map>

#include "ConstantEvaluator.hpp"
#include "Message.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace cld::Semantics
{
enum class StructDefTag : std::size_t
{
};
enum class UnionDefTag : std::size_t
{
};
enum class EnumDefTag : std::size_t
{
};
class SemanticAnalysis final
{
    struct DeclarationInScope
    {
        Lexer::CTokenIterator identifier;
        using Variant = std::variant<const Declaration*, const FunctionDefinition*, Type, ConstRetType>;
        Variant declared;
    };

    struct TagTypeInScope
    {
        Lexer::CTokenIterator identifier;
        struct StructDecl
        {
        };
        struct UnionDecl
        {
        };
        using Variant = std::variant<StructDecl, UnionDecl, StructDefTag, UnionDefTag, EnumDefTag>;
        Variant tagType;
    };
    const SourceInterface& m_sourceInterface;
    llvm::raw_ostream* m_reporter;
    struct Scope
    {
        std::int64_t previousScope;
        std::unordered_map<std::string_view, DeclarationInScope> declarations;
        std::unordered_map<std::string_view, TagTypeInScope> types;
    };
    std::int64_t m_currentScope = 0;
    std::vector<Scope> m_scopes = {Scope{-1, {}, {}}};
    std::vector<StructDefinition> m_structDefinitions;
    std::vector<UnionDefinition> m_unionDefinitions;
    std::vector<EnumDefinition> m_enumDefinitions;

    constexpr static std::uint64_t IS_SCOPE = 1ull << 63;
    constexpr static std::uint64_t SCOPE_OR_ID_MASK = ~(1ull << 63);

    auto pushScope()
    {
        m_scopes.push_back({m_currentScope, {}, {}});
        m_currentScope = m_scopes.size() - 1;
        return llvm::make_scope_exit([this, scope = m_scopes.back().previousScope] { m_currentScope = scope; });
    }

    template <class Expression>
    Expected<ConstRetType, std::vector<Message>> evaluateConstantExpression(Expression& constantExpression,
                                                                            ConstantEvaluator::Mode mode)
    {
        std::vector<Message> messages;
        ConstantEvaluator evaluator(
            m_sourceInterface,
            [this](std::string_view name) -> std::optional<ConstRetType> {
                const auto* result = lookupDecl(name);
                if (!result || !std::holds_alternative<ConstRetType>(*result))
                {
                    return {};
                }
                return cld::get<ConstRetType>(*result);
            },
            this,
            [&](const Message& message) {
                if (message.getSeverity() == Severity::Error)
                {
                    messages.push_back(message);
                }
                else
                {
                    log(message);
                }
            },
            mode);
        auto result = evaluator.visit(constantExpression);
        if (messages.empty())
        {
            return {std::move(result)};
        }
        return {std::move(messages)};
    }

    template <class T>
    void handleParameterList(Type& type, const Syntax::ParameterTypeList* parameterTypeList, T&& returnTypeLoc);

    template <class T>
    void handleArray(Type& type, const std::vector<Syntax::TypeQualifier>& typeQualifiers,
                     const Syntax::AssignmentExpression* assignmentExpression, bool isStatic, bool valarray,
                     T&& returnTypeLoc);

    [[nodiscard]] bool isTypedef(std::string_view name) const;

    [[nodiscard]] bool isTypedefInScope(std::string_view name) const;

    [[nodiscard]] const Semantics::Type* getTypedef(std::string_view name) const;

    [[nodiscard]] const DeclarationInScope::Variant* lookupDecl(std::string_view name) const
    {
        return lookupDecl(name, m_currentScope);
    }

    [[nodiscard]] const DeclarationInScope::Variant* lookupDecl(std::string_view name, std::int64_t scope) const;

    void log(const Message& message);

    static std::tuple<bool, bool, bool> getQualifiers(const std::vector<Syntax::TypeQualifier>& typeQualifiers);

    using PossiblyAbstractQualifierRef = std::variant<const Syntax::AbstractDeclarator*, const Syntax::Declarator*>;

    using DeclarationOrSpecifierQualifier =
        std::variant<const Syntax::TypeSpecifier*, const Syntax::TypeQualifier*, const Syntax::StorageClassSpecifier*,
                     const Syntax::FunctionSpecifier*>;

    Type typeSpecifiersToType(bool isConst, bool isVolatile, const std::vector<const Syntax::TypeSpecifier*>& typeSpec);

    Type declaratorsToTypeImpl(const std::vector<DeclarationOrSpecifierQualifier>& directAbstractDeclaratorParentheses,
                               const PossiblyAbstractQualifierRef& parameterList = {},
                               const std::vector<Syntax::Declaration>& declarations = {});

    Type primitiveTypeSpecifiersToType(bool isConst, bool isVolatile,
                                       const std::vector<const Syntax::TypeSpecifier*>& typeSpecs);

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

    Type adjustParameterType(const Type& type) const;

    Type compositeType(const Type& lhs, const Type& rhs) const;

    static Expression lvalueConversion(Expression expression);

    Expression integerPromotion(Expression expression) const;

public:
    explicit SemanticAnalysis(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter = &llvm::errs())
        : m_sourceInterface(sourceInterface), m_reporter(reporter)
    {
    }

    template <class T>
    Type declaratorsToType(const std::vector<T>& declarationOrSpecifierQualifiers,
                           const Syntax::AbstractDeclarator* declarator = nullptr,
                           const std::vector<Syntax::Declaration>& declarations = {})
    {
        std::vector<DeclarationOrSpecifierQualifier> temp(declarationOrSpecifierQualifiers.size());
        std::transform(declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(), temp.begin(),
                       [](auto&& value) {
                           return cld::match(value, [](auto&& valueInVariant) -> DeclarationOrSpecifierQualifier {
                               return &valueInVariant;
                           });
                       });
        return declaratorsToTypeImpl(std::move(temp), declarator, declarations);
    }

    template <class T>
    Type declaratorsToType(const std::vector<T>& declarationOrSpecifierQualifiers, const Syntax::Declarator& declarator,
                           const std::vector<Syntax::Declaration>& declarations = {})
    {
        std::vector<DeclarationOrSpecifierQualifier> temp(declarationOrSpecifierQualifiers.size());
        std::transform(declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(), temp.begin(),
                       [](auto&& value) {
                           return cld::match(value, [](auto&& valueInVariant) -> DeclarationOrSpecifierQualifier {
                               return &valueInVariant;
                           });
                       });
        return declaratorsToTypeImpl(std::move(temp), &declarator, declarations);
    }

    bool isCompleteType(const Type& type) const;

    bool isVariablyModified(const Type& type) const;

    const LanguageOptions& getLanguageOptions() const
    {
        return m_sourceInterface.getLanguageOptions();
    }

    template <class T>
    [[nodiscard]] const T* lookupType(std::string_view name) const
    {
        return lookupType<T>(name, m_currentScope);
    }

    template <class T>
    [[nodiscard]] const T* lookupType(std::string_view name, std::int64_t scope) const
    {
        auto curr = scope;
        while (curr >= 0)
        {
            auto result = m_scopes[curr].types.find(name);
            if (result != m_scopes[curr].types.end())
            {
                if (auto* ptr = std::get_if<T>(&result->second.tagType))
                {
                    return ptr;
                }
            }
            curr = m_scopes[curr].previousScope;
        }
        return nullptr;
    }

    StructDefinition* getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                          std::uint64_t* idOut = nullptr);

    const StructDefinition* getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                std::uint64_t* idOut = nullptr) const;

    EnumDefinition* getEnumDefinition(std::string_view name, std::uint64_t scopeOrId, std::uint64_t* idOut = nullptr);

    const EnumDefinition* getEnumDefinition(std::string_view name, std::uint64_t scopeOrId,
                                            std::uint64_t* idOut = nullptr) const;

    UnionDefinition* getUnionDefinition(std::string_view name, std::uint64_t scopeOrId, std::uint64_t* idOut = nullptr);

    const UnionDefinition* getUnionDefinition(std::string_view name, std::uint64_t scopeOrId,
                                              std::uint64_t* idOut = nullptr) const;

    TranslationUnit visit(const Syntax::TranslationUnit& node);

    std::vector<TranslationUnit::Variant> visit(const Syntax::FunctionDefinition& node);

    std::vector<TranslationUnit::Variant> visit(const Syntax::Declaration& node);

    CompoundStatement visit(const Syntax::CompoundStatement& node, bool pushScope = true);

    std::vector<CompoundStatement::Variant> visit(const Syntax::CompoundItem& node);

    Statement visit(const Syntax::Statement& node);

    Expression visit(const Syntax::Expression& node);

    Expression visit(const Syntax::AssignmentExpression& node);

    Expression visit(const Syntax::PrimaryExpression& node);

    Expression visit(const Syntax::PrimaryExpressionIdentifier& node);

    Expression visit(const Syntax::PrimaryExpressionConstant& node);

    Expression visit(const Syntax::PrimaryExpressionParentheses& node);

    Expression visit(const Syntax::PostFixExpression& node);

    Expression visit(const Syntax::PostFixExpressionPrimaryExpression& node);

    Expression visit(const Syntax::PostFixExpressionSubscript& node);

    Expression visit(const Syntax::PostFixExpressionDot& node);

    Expression visit(const Syntax::PostFixExpressionArrow& node);

    Expression visit(const Syntax::PostFixExpressionFunctionCall& node);

    Expression visit(const Syntax::PostFixExpressionIncrement& node);

    Expression visit(const Syntax::PostFixExpressionDecrement& node);

    Expression visit(const Syntax::PostFixExpressionTypeInitializer& node);

    Expression visit(const Syntax::UnaryExpression& node);

    Expression visit(const Syntax::UnaryExpressionPostFixExpression& node);

    Expression visit(const Syntax::UnaryExpressionUnaryOperator& node);

    Expression visit(const Syntax::UnaryExpressionSizeOf& node);

    Expression visit(const Syntax::UnaryExpressionDefined& node);

    Expression visit(const Syntax::CastExpression& node);

    Expression visit(const Syntax::Term& node);

    Expression visit(const Syntax::AdditiveExpression& node);

    Expression visit(const Syntax::ShiftExpression& node);

    Expression visit(const Syntax::RelationalExpression& node);

    Expression visit(const Syntax::EqualityExpression& node);

    Expression visit(const Syntax::BitAndExpression& node);

    Expression visit(const Syntax::BitXorExpression& node);

    Expression visit(const Syntax::BitOrExpression& node);

    Expression visit(const Syntax::LogicalAndExpression& node);

    Expression visit(const Syntax::LogicalOrExpression& node);

    Expression visit(const Syntax::ConditionalExpression& node);
};
} // namespace cld::Semantics

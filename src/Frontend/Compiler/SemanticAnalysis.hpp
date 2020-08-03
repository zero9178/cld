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
        struct EnumDecl
        {
        };
        struct StructDecl
        {
        };
        struct UnionDecl
        {
        };
        using Variant =
            std::variant<EnumDecl, StructDecl, UnionDecl, StructDefinition, UnionDefinition, EnumDefinition>;
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
            [this](const Syntax::TypeName& typeName) {
                return declaratorsToType(typeName.getSpecifierQualifiers(), typeName.getAbstractDeclarator());
            },
            [this](std::string_view name) -> ConstRetType {
                const auto* result = lookupDecl(name);
                if (!result || !std::holds_alternative<ConstRetType>(*result))
                {
                    return {};
                }
                return cld::get<ConstRetType>(*result);
            },
            [&](ConstantEvaluator::TypeInfo info, const Type& type,
                llvm::ArrayRef<Lexer::CToken> loc) -> Expected<std::size_t, Message> {
                switch (info)
                {
                    case ConstantEvaluator::TypeInfo::Size: return sizeOf(type, loc);
                    case ConstantEvaluator::TypeInfo::Alignment: return alignOf(type, loc);
                }
                CLD_UNREACHABLE;
            },
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

    [[nodiscard]] bool isTypedef(std::string_view name) const;

    [[nodiscard]] bool isTypedefInScope(std::string_view name) const;

    [[nodiscard]] const Semantics::Type* getTypedef(std::string_view name) const;

    [[nodiscard]] const DeclarationInScope::Variant* lookupDecl(std::string_view name) const
    {
        return lookupDecl(name, m_currentScope);
    }

    [[nodiscard]] const DeclarationInScope::Variant* lookupDecl(std::string_view name, std::int64_t scope) const;

    [[nodiscard]] const TagTypeInScope::Variant* lookupType(std::string_view name) const
    {
        return lookupType(name, m_currentScope);
    }

    [[nodiscard]] const TagTypeInScope::Variant* lookupType(std::string_view name, std::int64_t scope) const;

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

    bool isCompleteType(const Type& type) const;

    bool typesAreCompatible(const Type& lhs, const Type& rhs) const;

public:
    explicit SemanticAnalysis(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter = nullptr)
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

    Expected<std::size_t, Message> sizeOf(const Type& structType, llvm::ArrayRef<Lexer::CToken> loc = {}) const;

    Expected<std::size_t, Message> alignOf(const Type& structType, llvm::ArrayRef<Lexer::CToken> loc = {}) const;

    TranslationUnit visit(const Syntax::TranslationUnit& node);

    std::vector<TranslationUnit::Variant> visit(const Syntax::FunctionDefinition& node);

    std::vector<TranslationUnit::Variant> visit(const Syntax::Declaration& node);
};
} // namespace cld::Semantics

#pragma once

#include <unordered_map>

#include <bitset2.hpp>

#include "Limits.hpp"
#include "Message.hpp"
#include "Syntax.hpp"

namespace cld::Parser
{
class FatalParserError : public std::exception
{
};

class Context final
{
    const SourceInterface& m_sourceInterface;
    llvm::raw_ostream* m_reporter;
    struct DeclarationLocation
    {
        Lexer::CTokenIterator begin;
        Lexer::CTokenIterator end;
        Lexer::CTokenIterator identifier;
    };

    struct Declaration
    {
        DeclarationLocation location;
        bool isTypedef{};
    };
    std::vector<std::unordered_map<std::string_view, Declaration>> m_currentScope{1};
    std::size_t m_errorCount = 0;
    bool m_inPreprocessor;

public:
    using TokenBitSet =
        Bitset2::bitset2<static_cast<std::underlying_type_t<Lexer::TokenType>>(Lexer::TokenType::TOKEN_MAX_VALUE) + 1>;

private:
    TokenBitSet m_recoverySet{false};

    friend class TokenBitReseter;

    class TokenBitReseter final
    {
        TokenBitSet m_original;
        Context& m_context;

    public:
        explicit TokenBitReseter(TokenBitSet original, Context& context);

        operator Context&()
        {
            return m_context;
        }

        ~TokenBitReseter();

        TokenBitReseter(const TokenBitReseter&) = delete;

        TokenBitReseter(TokenBitReseter&&) = delete;

        TokenBitReseter& operator=(const TokenBitReseter&) = delete;

        TokenBitReseter& operator=(TokenBitReseter&&) noexcept = delete;
    };

    std::uint64_t m_bracketMax = Limits::Parser::MAX_BRACKET_DEPTH;
    std::uint64_t m_parenthesesDepth = 0;
    std::uint64_t m_squareBracketDepth = 0;
    std::uint64_t m_braceDepth = 0;

    class Decrementer
    {
        std::uint64_t& m_value;

    public:
        explicit Decrementer(std::uint64_t& value) : m_value(value) {}

        ~Decrementer()
        {
            m_value--;
        }

        Decrementer(const Decrementer&) = delete;
        Decrementer& operator=(const Decrementer&) = delete;
        Decrementer(Decrementer&&) = delete;
        Decrementer& operator=(Decrementer&&) = delete;
    };

public:
    template <class... Args>
    constexpr static TokenBitSet fromTokenTypes(Args&&... tokenTypes);

    explicit Context(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter = &llvm::errs(),
                     bool inPreprocessor = false);

    ~Context() = default;

    Context(const Context&) = delete;

    Context(Context&&) = delete;

    Context& operator=(const Context&) = delete;

    Context& operator=(Context&&) = delete;

    bool isInPreprocessor() const;

    TokenBitReseter withRecoveryTokens(const TokenBitSet& tokenBitSet);

    void addTypedef(std::string_view name, DeclarationLocation declarator);

    [[nodiscard]] bool isTypedef(std::string_view name) const;

    [[nodiscard]] bool isTypedefInScope(std::string_view name) const;

    void log(const Message& message);

    void addToScope(std::string_view name, DeclarationLocation declarator);

    [[nodiscard]] const Parser::Context::DeclarationLocation* getLocationOf(std::string_view name) const;

    void pushScope();

    void popScope();

    [[nodiscard]] std::size_t getCurrentErrorCount() const;

    const SourceInterface& getSourceInterface() const;

    void skipUntil(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, TokenBitSet additional = {});

    [[nodiscard]] Decrementer parenthesesEntered(Lexer::CTokenIterator bracket);

    [[nodiscard]] Decrementer squareBracketEntered(Lexer::CTokenIterator bracket);

    [[nodiscard]] Decrementer braceEntered(Lexer::CTokenIterator bracket);

    std::uint64_t getBracketMax() const;

    void setBracketMax(uint64_t bracketMax);
};

std::pair<cld::Syntax::TranslationUnit, bool> buildTree(const CSourceObject& sourceObject,
                                                        llvm::raw_ostream* reporter = &llvm::errs());

cld::Syntax::TranslationUnit parseTranslationUnit(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                  Context& context);

std::optional<Syntax::ExternalDeclaration> parseExternalDeclaration(Lexer::CTokenIterator& begin,
                                                                    Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::Declaration> parseDeclaration(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                    Context& context);

std::optional<Syntax::DeclarationSpecifier> parseDeclarationSpecifier(Lexer::CTokenIterator& begin,
                                                                      Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::SpecifierQualifier> parseSpecifierQualifier(Lexer::CTokenIterator& begin,
                                                                  Lexer::CTokenIterator end, Context& context);

std::vector<Syntax::SpecifierQualifier> parseSpecifierQualifierList(Lexer::CTokenIterator& begin,
                                                                    Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::Declarator> parseDeclarator(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                  Context& context);

std::optional<Syntax::DirectDeclarator> parseDirectDeclarator(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                              Context& context);

Syntax::ParameterTypeList parseParameterTypeList(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                 Context& context);

Syntax::AbstractDeclarator parseAbstractDeclarator(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                   Context& context);

std::optional<Syntax::DirectAbstractDeclarator>
    parseDirectAbstractDeclarator(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context);

Syntax::ParameterList parseParameterList(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context);

Syntax::Pointer parsePointer(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::StructOrUnionSpecifier> parseStructOrUnionSpecifier(Lexer::CTokenIterator& begin,
                                                                          Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::EnumSpecifier> parseEnumSpecifier(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                        Context& context);

std::optional<Syntax::CompoundStatement> parseCompoundStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                                cld::Parser::Context& context, bool pushScope = true);

std::optional<Syntax::CompoundItem> parseCompoundItem(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                      Context& context);

std::optional<Syntax::Initializer> parseInitializer(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                    Context& context);

std::optional<Syntax::InitializerList> parseInitializerList(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                            Context& context);

std::optional<Syntax::Statement> parseStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                Context& context);

Syntax::ReturnStatement parseReturnStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::IfStatement> parseIfStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                    Context& context);

std::optional<Syntax::SwitchStatement> parseSwitchStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                            Context& context);

std::optional<Syntax::ForStatement> parseForStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                      Context& context);

std::optional<Syntax::HeadWhileStatement> parseHeadWhileStatement(Lexer::CTokenIterator& begin,
                                                                  Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::FootWhileStatement> parseFootWhileStatement(Lexer::CTokenIterator& begin,
                                                                  Lexer::CTokenIterator end, Context& context);

Syntax::Expression parseExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::AssignmentExpression> parseAssignmentExpression(Lexer::CTokenIterator& begin,
                                                                      Lexer::CTokenIterator end, Context& context);

cld::Syntax::ConditionalExpression parseConditionalExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                              Context& context);

cld::Syntax::LogicalOrExpression parseLogicalOrExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                          Context& context);

cld::Syntax::LogicalAndExpression parseLogicalAndExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                            Context& context);

cld::Syntax::BitOrExpression parseBitOrExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                  Context& context);

cld::Syntax::BitXorExpression parseBitXorExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                    Context& context);

cld::Syntax::BitAndExpression parseBitAndExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                    Context& context);

std::optional<Syntax::EqualityExpression> parseEqualityExpression(Lexer::CTokenIterator& begin,
                                                                  Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::RelationalExpression> parseRelationalExpression(Lexer::CTokenIterator& begin,
                                                                      Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::ShiftExpression> parseShiftExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                            Context& context);

std::optional<Syntax::AdditiveExpression> parseAdditiveExpression(Lexer::CTokenIterator& begin,
                                                                  Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::Term> parseTerm(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context);

std::optional<Syntax::TypeName> parseTypeName(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                              Context& context);

std::optional<Syntax::CastExpression> parseCastExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                          Context& context);

std::optional<Syntax::UnaryExpression> parseUnaryExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                            Context& context);

std::optional<Syntax::PostFixExpression> parsePostFixExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                                Context& context);
} // namespace cld::Parser

template <class... Args>
constexpr cld::Parser::Context::TokenBitSet cld::Parser::Context::fromTokenTypes(Args&&... tokenTypes)
{
    static_assert((std::is_same_v<std::decay_t<Args>, Lexer::TokenType> && ...));
    return (TokenBitSet() | ...
            | TokenBitSet().set(static_cast<std::underlying_type_t<Lexer::TokenType>>(tokenTypes), true));
}

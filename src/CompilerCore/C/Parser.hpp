#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include <bitset2.hpp>
#include <tl/function_ref.hpp>

#include "Message.hpp"
#include "Semantics.hpp"
#include "SourceObject.hpp"
#include "Syntax.hpp"

namespace OpenCL::Parser
{
    class FatalParserError : public std::exception
    {
    };

    class Context final
    {
        std::ostream* m_reporter;
        struct DeclarationLocation
        {
            SourceObject::const_iterator begin;
            SourceObject::const_iterator end;
            SourceObject::const_iterator identifier;
        };

        struct Declaration
        {
            DeclarationLocation location;
            bool isTypedef{};
        };
        std::vector<std::map<std::string, Declaration>> m_currentScope{1};

        const SourceObject& m_sourceObject;
        std::size_t m_errorCount = 0;

    public:
        using TokenBitSet =
            Bitset2::bitset2<static_cast<std::underlying_type_t<Lexer::TokenType>>(Lexer::TokenType::TOKEN_MAX_VALUE)
                             + 1>;

    private:
        TokenBitSet m_recoverySet{false};

        friend class TokenBitReseter;

        class TokenBitReseter final
        {
            TokenBitSet m_original;
            Context& m_context;

        public:
            explicit TokenBitReseter(TokenBitSet original, Context& context);

#pragma clang diagnostic push
#pragma ide diagnostic ignored "hicpp-explicit-conversions"
            operator Context&()
            {
                return m_context;
            }
#pragma clang diagnostic pop

            ~TokenBitReseter();

            TokenBitReseter(const TokenBitReseter&) = delete;

            TokenBitReseter(TokenBitReseter&&) = delete;

            TokenBitReseter& operator=(const TokenBitReseter&) = delete;

            TokenBitReseter& operator=(TokenBitReseter&&) noexcept = delete;
        };

        std::uint64_t m_bracketMax = 256;
        std::uint64_t m_parenthesesDepth = 0;
        std::uint64_t m_squareBracketDepth = 0;
        std::uint64_t m_braceDepth = 0;

    public:
        template <class... Args>
        constexpr static TokenBitSet fromTokenTypes(Args&&... tokenTypes);

        explicit Context(const SourceObject& sourceObject, std::ostream* reporter = &std::cerr);

        ~Context() = default;

        Context(const Context&) = delete;

        Context(Context&&) = delete;

        Context& operator=(const Context&) = delete;

        Context& operator=(Context&&) = delete;

        const SourceObject& getSourceObject() const;

        TokenBitReseter withRecoveryTokens(const TokenBitSet& tokenBitSet);

        void addTypedef(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] bool isTypedef(const std::string& name) const;

        [[nodiscard]] bool isTypedefInScope(const std::string& name) const;

        void log(std::vector<Message> messages);

        [[nodiscard]] SourceObject::const_iterator getLineStart(SourceObject::const_iterator iter) const;

        [[nodiscard]] SourceObject::const_iterator getLineEnd(SourceObject::const_iterator iter) const;

        void addToScope(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] const Parser::Context::DeclarationLocation* getLocationOf(const std::string& name) const;

        void pushScope();

        void popScope();

        [[nodiscard]] std::size_t getCurrentErrorCount() const;

        void skipUntil(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                       TokenBitSet additional = {});

        void parenthesesEntered(SourceObject::const_iterator bracket);

        void parenthesesLeft();

        void squareBracketEntered(SourceObject::const_iterator bracket);

        void squareBracketLeft();

        void braceEntered(SourceObject::const_iterator bracket);

        void braceLeft();
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const SourceObject& sourceObject,
                                                               std::ostream* reporter = &std::cerr);

    OpenCL::Syntax::TranslationUnit parseTranslationUnit(SourceObject::const_iterator& begin,
                                                         SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::ExternalDeclaration> parseExternalDeclaration(SourceObject::const_iterator& begin,
                                                                        SourceObject::const_iterator end,
                                                                        Context& context);

    std::optional<Syntax::Declaration> parseDeclaration(SourceObject::const_iterator& begin,
                                                        SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::DeclarationSpecifier> parseDeclarationSpecifier(SourceObject::const_iterator& begin,
                                                                          SourceObject::const_iterator end,
                                                                          Context& context);

    std::optional<Syntax::SpecifierQualifier> parseSpecifierQualifier(SourceObject::const_iterator& begin,
                                                                      SourceObject::const_iterator end,
                                                                      Context& context);

    std::vector<Syntax::SpecifierQualifier> parseSpecifierQualifierList(SourceObject::const_iterator& begin,
                                                                        SourceObject::const_iterator end,
                                                                        Context& context);

    std::optional<Syntax::Declarator> parseDeclarator(SourceObject::const_iterator& begin,
                                                      SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::DirectDeclarator> parseDirectDeclarator(SourceObject::const_iterator& begin,
                                                                  SourceObject::const_iterator end, Context& context);

    Syntax::ParameterTypeList parseParameterTypeList(SourceObject::const_iterator& begin,
                                                     SourceObject::const_iterator end, Context& context);

    Syntax::AbstractDeclarator parseAbstractDeclarator(SourceObject::const_iterator& begin,
                                                       SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::DirectAbstractDeclarator> parseDirectAbstractDeclarator(SourceObject::const_iterator& begin,
                                                                                  SourceObject::const_iterator end,
                                                                                  Context& context);

    Syntax::ParameterList parseParameterList(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                                             Context& context);

    Syntax::Pointer parsePointer(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                                 Context& context);

    std::optional<Syntax::StructOrUnionSpecifier> parseStructOrUnionSpecifier(SourceObject::const_iterator& begin,
                                                                              SourceObject::const_iterator end,
                                                                              Context& context);

    std::optional<Syntax::EnumSpecifier> parseEnumSpecifier(SourceObject::const_iterator& begin,
                                                            SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::CompoundStatement> parseCompoundStatement(SourceObject::const_iterator& begin,
                                                                    SourceObject::const_iterator end,
                                                                    OpenCL::Parser::Context& context,
                                                                    bool pushScope = true);

    std::optional<Syntax::CompoundItem> parseCompoundItem(SourceObject::const_iterator& begin,
                                                          SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::Initializer> parseInitializer(SourceObject::const_iterator& begin,
                                                        SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::InitializerList> parseInitializerList(SourceObject::const_iterator& begin,
                                                                SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::Statement> parseStatement(SourceObject::const_iterator& begin,
                                                    SourceObject::const_iterator end, Context& context);

    Syntax::ReturnStatement parseReturnStatement(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                                                 Context& context);

    std::optional<Syntax::IfStatement> parseIfStatement(SourceObject::const_iterator& begin,
                                                        SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::SwitchStatement> parseSwitchStatement(SourceObject::const_iterator& begin,
                                                                SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::ForStatement> parseForStatement(SourceObject::const_iterator& begin,
                                                          SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::HeadWhileStatement> parseHeadWhileStatement(SourceObject::const_iterator& begin,
                                                                      SourceObject::const_iterator end,
                                                                      Context& context);

    std::optional<Syntax::FootWhileStatement> parseFootWhileStatement(SourceObject::const_iterator& begin,
                                                                      SourceObject::const_iterator end,
                                                                      Context& context);

    Syntax::Expression parseExpression(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                                       Context& context);

    std::optional<Syntax::AssignmentExpression> parseAssignmentExpression(SourceObject::const_iterator& begin,
                                                                          SourceObject::const_iterator end,
                                                                          Context& context);

    OpenCL::Syntax::ConditionalExpression parseConditionalExpression(SourceObject::const_iterator& begin,
                                                                     SourceObject::const_iterator end,
                                                                     Context& context);

    OpenCL::Syntax::LogicalOrExpression parseLogicalOrExpression(SourceObject::const_iterator& begin,
                                                                 SourceObject::const_iterator end, Context& context);

    OpenCL::Syntax::LogicalAndExpression parseLogicalAndExpression(SourceObject::const_iterator& begin,
                                                                   SourceObject::const_iterator end, Context& context);

    OpenCL::Syntax::BitOrExpression parseBitOrExpression(SourceObject::const_iterator& begin,
                                                         SourceObject::const_iterator end, Context& context);

    OpenCL::Syntax::BitXorExpression parseBitXorExpression(SourceObject::const_iterator& begin,
                                                           SourceObject::const_iterator end, Context& context);

    OpenCL::Syntax::BitAndExpression parseBitAndExpression(SourceObject::const_iterator& begin,
                                                           SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::EqualityExpression> parseEqualityExpression(SourceObject::const_iterator& begin,
                                                                      SourceObject::const_iterator end,
                                                                      Context& context);

    std::optional<Syntax::RelationalExpression> parseRelationalExpression(SourceObject::const_iterator& begin,
                                                                          SourceObject::const_iterator end,
                                                                          Context& context);

    std::optional<Syntax::ShiftExpression> parseShiftExpression(SourceObject::const_iterator& begin,
                                                                SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::AdditiveExpression> parseAdditiveExpression(SourceObject::const_iterator& begin,
                                                                      SourceObject::const_iterator end,
                                                                      Context& context);

    std::optional<Syntax::Term> parseTerm(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                                          Context& context);

    std::optional<Syntax::TypeName> parseTypeName(SourceObject::const_iterator& begin, SourceObject::const_iterator end,
                                                  Context& context);

    std::optional<Syntax::CastExpression> parseCastExpression(SourceObject::const_iterator& begin,
                                                              SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::UnaryExpression> parseUnaryExpression(SourceObject::const_iterator& begin,
                                                                SourceObject::const_iterator end, Context& context);

    std::optional<Syntax::PostFixExpression> parsePostFixExpression(SourceObject::const_iterator& begin,
                                                                    SourceObject::const_iterator end, Context& context);
} // namespace OpenCL::Parser

template <class... Args>
constexpr OpenCL::Parser::Context::TokenBitSet OpenCL::Parser::Context::fromTokenTypes(Args&&... tokenTypes)
{
    static_assert((std::is_same_v<std::decay_t<Args>, Lexer::TokenType> && ...));
    return (TokenBitSet() | ...
            | TokenBitSet().set(static_cast<std::underlying_type_t<Lexer::TokenType>>(tokenTypes), true));
}

#endif // OPENCLPARSER_PARSER_HPP

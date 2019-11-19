#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include <bitset2.hpp>
#include <tl/function_ref.hpp>

#include "Message.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace OpenCL::Parser
{
    class FatalParserError : public std::exception
    {
    };

    class Context final
    {
        const SourceObject& m_sourceObject;
        llvm::raw_ostream* m_reporter;
        struct DeclarationLocation
        {
            std::vector<Lexer::Token>::const_iterator begin;
            std::vector<Lexer::Token>::const_iterator end;
            std::vector<Lexer::Token>::const_iterator identifier;
        };

        struct Declaration
        {
            DeclarationLocation location;
            bool isTypedef{};
        };
        std::vector<std::map<std::string, Declaration>> m_currentScope{1};
        std::size_t m_errorCount = 0;
        bool m_inPreprocessor;

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

        explicit Context(const SourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(),
                         bool inPreprocessor = false);

        ~Context() = default;

        Context(const Context&) = delete;

        Context(Context&&) = delete;

        Context& operator=(const Context&) = delete;

        Context& operator=(Context&&) = delete;

        bool isInPreprocessor() const;

        TokenBitReseter withRecoveryTokens(const TokenBitSet& tokenBitSet);

        void addTypedef(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] bool isTypedef(const std::string& name) const;

        [[nodiscard]] bool isTypedefInScope(const std::string& name) const;

        void log(std::vector<Message> messages);

        void addToScope(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] const Parser::Context::DeclarationLocation* getLocationOf(const std::string& name) const;

        void pushScope();

        void popScope();

        [[nodiscard]] std::size_t getCurrentErrorCount() const;

        const SourceObject& getSourceObject() const;

        void skipUntil(std::vector<Lexer::Token>::const_iterator& begin, std::vector<Lexer::Token>::const_iterator end,
                       TokenBitSet additional = {});

        void parenthesesEntered(std::vector<Lexer::Token>::const_iterator bracket);

        void parenthesesLeft();

        void squareBracketEntered(std::vector<Lexer::Token>::const_iterator bracket);

        void squareBracketLeft();

        void braceEntered(std::vector<Lexer::Token>::const_iterator bracket);

        void braceLeft();
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const SourceObject& sourceObject,
                                                               llvm::raw_ostream* reporter = &llvm::errs());

    OpenCL::Syntax::TranslationUnit parseTranslationUnit(std::vector<Lexer::Token>::const_iterator& begin,
                                                         std::vector<Lexer::Token>::const_iterator end,
                                                         Context& context);

    std::optional<Syntax::ExternalDeclaration>
        parseExternalDeclaration(std::vector<Lexer::Token>::const_iterator& begin,
                                 std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::Declaration> parseDeclaration(std::vector<Lexer::Token>::const_iterator& begin,
                                                        std::vector<Lexer::Token>::const_iterator end,
                                                        Context& context);

    std::optional<Syntax::DeclarationSpecifier>
        parseDeclarationSpecifier(std::vector<Lexer::Token>::const_iterator& begin,
                                  std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::SpecifierQualifier> parseSpecifierQualifier(std::vector<Lexer::Token>::const_iterator& begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      Context& context);

    std::vector<Syntax::SpecifierQualifier>
        parseSpecifierQualifierList(std::vector<Lexer::Token>::const_iterator& begin,
                                    std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::Declarator> parseDeclarator(std::vector<Lexer::Token>::const_iterator& begin,
                                                      std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::DirectDeclarator> parseDirectDeclarator(std::vector<Lexer::Token>::const_iterator& begin,
                                                                  std::vector<Lexer::Token>::const_iterator end,
                                                                  Context& context);

    Syntax::ParameterTypeList parseParameterTypeList(std::vector<Lexer::Token>::const_iterator& begin,
                                                     std::vector<Lexer::Token>::const_iterator end, Context& context);

    Syntax::AbstractDeclarator parseAbstractDeclarator(std::vector<Lexer::Token>::const_iterator& begin,
                                                       std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::DirectAbstractDeclarator>
        parseDirectAbstractDeclarator(std::vector<Lexer::Token>::const_iterator& begin,
                                      std::vector<Lexer::Token>::const_iterator end, Context& context);

    Syntax::ParameterList parseParameterList(std::vector<Lexer::Token>::const_iterator& begin,
                                             std::vector<Lexer::Token>::const_iterator end, Context& context);

    Syntax::Pointer parsePointer(std::vector<Lexer::Token>::const_iterator& begin,
                                 std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::StructOrUnionSpecifier>
        parseStructOrUnionSpecifier(std::vector<Lexer::Token>::const_iterator& begin,
                                    std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::EnumSpecifier> parseEnumSpecifier(std::vector<Lexer::Token>::const_iterator& begin,
                                                            std::vector<Lexer::Token>::const_iterator end,
                                                            Context& context);

    std::optional<Syntax::CompoundStatement> parseCompoundStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                                    std::vector<Lexer::Token>::const_iterator end,
                                                                    OpenCL::Parser::Context& context,
                                                                    bool pushScope = true);

    std::optional<Syntax::CompoundItem> parseCompoundItem(std::vector<Lexer::Token>::const_iterator& begin,
                                                          std::vector<Lexer::Token>::const_iterator end,
                                                          Context& context);

    std::optional<Syntax::Initializer> parseInitializer(std::vector<Lexer::Token>::const_iterator& begin,
                                                        std::vector<Lexer::Token>::const_iterator end,
                                                        Context& context);

    std::optional<Syntax::InitializerList> parseInitializerList(std::vector<Lexer::Token>::const_iterator& begin,
                                                                std::vector<Lexer::Token>::const_iterator end,
                                                                Context& context);

    std::optional<Syntax::Statement> parseStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                    std::vector<Lexer::Token>::const_iterator end, Context& context);

    Syntax::ReturnStatement parseReturnStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                 std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::IfStatement> parseIfStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                        std::vector<Lexer::Token>::const_iterator end,
                                                        Context& context);

    std::optional<Syntax::SwitchStatement> parseSwitchStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                                std::vector<Lexer::Token>::const_iterator end,
                                                                Context& context);

    std::optional<Syntax::ForStatement> parseForStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                          std::vector<Lexer::Token>::const_iterator end,
                                                          Context& context);

    std::optional<Syntax::HeadWhileStatement> parseHeadWhileStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      Context& context);

    std::optional<Syntax::FootWhileStatement> parseFootWhileStatement(std::vector<Lexer::Token>::const_iterator& begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      Context& context);

    Syntax::Expression parseExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                       std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::AssignmentExpression>
        parseAssignmentExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                  std::vector<Lexer::Token>::const_iterator end, Context& context);

    OpenCL::Syntax::ConditionalExpression parseConditionalExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                     std::vector<Lexer::Token>::const_iterator end,
                                                                     Context& context);

    OpenCL::Syntax::LogicalOrExpression parseLogicalOrExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                 std::vector<Lexer::Token>::const_iterator end,
                                                                 Context& context);

    OpenCL::Syntax::LogicalAndExpression parseLogicalAndExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                   std::vector<Lexer::Token>::const_iterator end,
                                                                   Context& context);

    OpenCL::Syntax::BitOrExpression parseBitOrExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                         std::vector<Lexer::Token>::const_iterator end,
                                                         Context& context);

    OpenCL::Syntax::BitXorExpression parseBitXorExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                           std::vector<Lexer::Token>::const_iterator end,
                                                           Context& context);

    OpenCL::Syntax::BitAndExpression parseBitAndExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                           std::vector<Lexer::Token>::const_iterator end,
                                                           Context& context);

    std::optional<Syntax::EqualityExpression> parseEqualityExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      Context& context);

    std::optional<Syntax::RelationalExpression>
        parseRelationalExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                  std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::ShiftExpression> parseShiftExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                std::vector<Lexer::Token>::const_iterator end,
                                                                Context& context);

    std::optional<Syntax::AdditiveExpression> parseAdditiveExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      Context& context);

    std::optional<Syntax::Term> parseTerm(std::vector<Lexer::Token>::const_iterator& begin,
                                          std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::TypeName> parseTypeName(std::vector<Lexer::Token>::const_iterator& begin,
                                                  std::vector<Lexer::Token>::const_iterator end, Context& context);

    std::optional<Syntax::CastExpression> parseCastExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                              std::vector<Lexer::Token>::const_iterator end,
                                                              Context& context);

    std::optional<Syntax::UnaryExpression> parseUnaryExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                std::vector<Lexer::Token>::const_iterator end,
                                                                Context& context);

    std::optional<Syntax::PostFixExpression> parsePostFixExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                    std::vector<Lexer::Token>::const_iterator end,
                                                                    Context& context);
} // namespace OpenCL::Parser

template <class... Args>
constexpr OpenCL::Parser::Context::TokenBitSet OpenCL::Parser::Context::fromTokenTypes(Args&&... tokenTypes)
{
    static_assert((std::is_same_v<std::decay_t<Args>, Lexer::TokenType> && ...));
    return (TokenBitSet() | ...
            | TokenBitSet().set(static_cast<std::underlying_type_t<Lexer::TokenType>>(tokenTypes), true));
}

#endif // OPENCLPARSER_PARSER_HPP

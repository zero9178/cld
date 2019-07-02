#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Semantics.hpp"
#include "Syntax.hpp"
#include "Message.hpp"
#include "../Common/UniqueResource.hpp"

#include <functional>

/**
 * For error recovery we use panic mode which means that if a production fails due to a terminal not being present
 * we return from the function. The caller is the responsible of finding the next sync token.
 *
 * One thing that may seem a bit unintuitive at first is that if we have a production like
 * <E> := .... <A> ...
 * and <A> fails we can't construct an object of type E as we have no A even after successfully syncing and parsing
 * all tokens after A. Therefore we are forced to also return an empty optional from the parser Function for E. The caller
 * will then try to sync but should immediately succeed so this is not seen as an issue.
 */

namespace OpenCL::Parser
{
    using Tokens = std::vector<OpenCL::Lexer::Token>;

    namespace ErrorMessages
    {
        constexpr auto EXPECTED_N = Format("Expected {}");

        constexpr auto EXPECTED_N_BEFORE_N = Format("Expected {} before {}");

        constexpr auto EXPECTED_N_AFTER_N = Format("Expected {} after {}");

        constexpr auto EXPECTED_N_INSTEAD_OF_N = Format("Expected {} instead of {}");

        constexpr auto MISSING_PARAMETER_NAME = "Parameter name omitted in function definition";
    }

    namespace Notes
    {
        constexpr auto
            TYPEDEF_OVERSHADOWED_BY_DECLARATION = Format("{} is a typedef but overshadowed by declaration here:");

        constexpr auto
            IDENTIFIER_IS_TYPDEF = Format("{} is a typename and not an identifier due to typedef declaration here:");

        constexpr auto TO_MATCH_N_HERE = Format("To match {} here:");
    }

    class ParsingContext final
    {
        std::ostream* m_reporter;
        bool m_errorsOccured = false;
        struct DeclarationLocation
        {
            Tokens::const_iterator begin;
            Tokens::const_iterator end;
            Tokens::const_iterator identifier;
        };
        std::vector<std::map<std::string, DeclarationLocation>> m_currentScope{1};
        std::vector<std::map<std::string, DeclarationLocation>> m_typedefs{1};
        std::vector<Tokens::const_iterator> m_start{};
        std::size_t m_errorCount = 0;

        class Branch
        {
            ParsingContext& context;
            Tokens::const_iterator& m_begin;
            Tokens::const_iterator m_curr;
            std::vector<Message> m_errors;
            using CriteriaFunction = std::function<bool(std::vector<Lexer::Token>::const_iterator,
                                                        std::vector<Lexer::Token>::const_iterator)>;
            CriteriaFunction m_criteria;

            friend class ParsingContext;

        public:

            Branch(ParsingContext& context,
                   std::vector<Lexer::Token>::const_iterator& begin,
                   CriteriaFunction&& criteria);

            ~Branch();

            explicit operator bool() const;

            std::vector<Lexer::Token>::const_iterator& getCurrent();
        };

        std::vector<std::vector<Branch*>> m_branches;

        void logImpl(Message&& error);

    public:
        std::map<std::string, Semantics::RecordType> structOrUnions;

        void addTypedef(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] bool isTypedef(const std::string& name) const;

        void logError(std::string message,
                      Tokens::const_iterator end,
                      std::optional<Modifier> modifier = {},
                      std::vector<Message::Note> notes = {});

        explicit ParsingContext(std::ostream* reporter = &std::cerr) : m_reporter(reporter)
        {}

        ~ParsingContext() = default;

        ParsingContext(const ParsingContext&) = delete;

        ParsingContext(ParsingContext&&) = delete;

        ParsingContext& operator=(const ParsingContext&) = delete;

        ParsingContext& operator=(ParsingContext&&) = delete;

        void addToScope(std::string name, DeclarationLocation declarator);

        [[nodiscard]] const Parser::ParsingContext::DeclarationLocation* getLocationOf(const std::string& name) const;

        [[nodiscard]] bool isInScope(const std::string& name) const;

        void pushScope();

        void popScope();

        [[nodiscard]] bool isErrorsOccured() const;

        [[nodiscard]]
        UniqueResource setDiagnosticStart(Tokens::const_iterator start)
        {
            m_start.push_back(start);
            return UniqueResource([this]
                                  {
                                      m_start.pop_back();
                                  });
        }

        template <class F>
        auto doBacktracking(F&& f)
        {
            m_branches.emplace_back();
            UniqueResource resource([this]
                                    {
                                        m_branches.pop_back();
                                    });
            return std::forward<F>(f)();
        }

        std::unique_ptr<Branch> createBranch(Tokens::const_iterator& begin,
                                             Branch::CriteriaFunction&& criteria = {});

        [[nodiscard]] std::size_t getCurrentErrorCount() const;
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const std::vector<Lexer::Token>& tokens,
                                                               std::ostream* reporter = &std::cerr);

    OpenCL::Syntax::TranslationUnit
    parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::ExternalDeclaration>
    parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Declaration> parseDeclaration(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::DeclarationSpecifier>
    parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::SpecifierQualifier>
    parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Declarator> parseDeclarator(Tokens::const_iterator& begin,
                                                      Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::DirectDeclarator>
    parseDirectDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::ParameterTypeList>
    parseParameterTypeList(Tokens::const_iterator& tokens, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::AbstractDeclarator>
    parseAbstractDeclarator(Tokens::const_iterator& tokens, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::DirectAbstractDeclarator>
    parseDirectAbstractDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                  ParsingContext& context);

    std::optional<Syntax::ParameterList>
    parseParameterList(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Pointer> parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                ParsingContext& context);

    std::optional<Syntax::StructOrUnionSpecifier>
    parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::EnumSpecifier>
    parseEnumSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::CompoundStatement>
    parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin,
                           Tokens::const_iterator end,
                           OpenCL::Parser::ParsingContext& context,
                           bool pushScope = true);

    std::optional<Syntax::CompoundItem>
    parseCompoundItem(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Initializer> parseInitializer(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::InitializerList>
    parseInitializerList(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Statement> parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                    ParsingContext& context);

    std::optional<Syntax::ReturnStatement> parseReturnStatement(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                ParsingContext& context);

    std::optional<Syntax::IfStatement> parseIfStatement(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end,
                                                        ParsingContext& context);

    std::optional<Syntax::SwitchStatement> parseSwitchStatement(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                ParsingContext& context);

    std::optional<Syntax::ForStatement> parseForStatement(Tokens::const_iterator& begin,
                                                          Tokens::const_iterator end,
                                                          ParsingContext& context);

    std::optional<Syntax::Expression> parseExpression(Tokens::const_iterator& begin,
                                                      Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::AssignmentExpression>
    parseAssignmentExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::AssignmentExpressionAssignment>
    parseAssignmentExpressionAssignment(Tokens::const_iterator& begin,
                                        Tokens::const_iterator end,
                                        ParsingContext& context,
                                        bool* reachedAssignment);

    std::optional<Syntax::ConditionalExpression>
    parseConditionalExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::LogicalOrExpression>
    parseLogicalOrExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::LogicalAndExpression>
    parseLogicalAndExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::BitOrExpression>
    parseBitOrExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::BitXorExpression>
    parseBitXorExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::BitAndExpression>
    parseBitAndExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::EqualityExpression>
    parseEqualityExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::RelationalExpression>
    parseRelationalExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::ShiftExpression>
    parseShiftExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::AdditiveExpression>
    parseAdditiveExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Term> parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          ParsingContext& context);

    std::optional<Syntax::TypeName> parseTypeName(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                  ParsingContext& context);

    std::optional<Syntax::CastExpression>
    parseCastExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::UnaryExpression>
    parseUnaryExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::PostFixExpression>
    parsePostFixExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::PrimaryExpression>
    parsePrimaryExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);
} // namespace OpenCL::Parser

#endif // OPENCLPARSER_PARSER_HPP

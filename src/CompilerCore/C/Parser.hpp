#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include <functional>

#include "../Common/UniqueResource.hpp"
#include "Message.hpp"
#include "ParserResult.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

/**
 * For error recovery we use panic mode which means that if a production fails due to a terminal not being present
 * we return from the function. The caller is the responsible of finding the next sync token.
 *
 * One thing that may seem a bit unintuitive at first is that if we have a production like
 * <E> := .... <A> ...
 * and <A> fails we can't construct an object of type E as we have no A even after successfully syncing and parsing
 * all tokens after A. Therefore we are forced to also return an empty optional from the parser Function for E. The
 * caller will then try to sync but should immediately succeed so this is not seen as an issue.
 */

namespace OpenCL::Parser
{
    using Tokens = std::vector<OpenCL::Lexer::Token>;

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

        struct Declaration
        {
            DeclarationLocation location;
            bool isTypedef{};
        };
        std::vector<std::map<std::string, Declaration>> m_currentScope{1};
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
            Branch(ParsingContext& context, std::vector<Lexer::Token>::const_iterator& begin,
                   CriteriaFunction&& criteria);

            ~Branch();

            explicit operator bool() const;

            std::vector<Lexer::Token>::const_iterator& getCurrent();
        };

        std::vector<std::vector<Branch*>> m_branches;

        void logImpl(Message&& error);

    public:
        void addTypedef(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] bool isTypedef(const std::string& name) const;

        [[nodiscard]] bool isTypedefInScope(const std::string& name) const;

        void logError(std::string message, Tokens::const_iterator end, std::optional<Modifier> modifier = {},
                      std::vector<Message::Note> notes = {});

        explicit ParsingContext(std::ostream* reporter = &std::cerr) : m_reporter(reporter) {}

        ~ParsingContext() = default;

        ParsingContext(const ParsingContext&) = delete;

        ParsingContext(ParsingContext&&) = delete;

        ParsingContext& operator=(const ParsingContext&) = delete;

        ParsingContext& operator=(ParsingContext&&) = delete;

        void addToScope(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] const Parser::ParsingContext::DeclarationLocation* getLocationOf(const std::string& name) const;

        void pushScope();

        void popScope();

        [[nodiscard]] bool isErrorsOccured() const;

        [[nodiscard]] auto setDiagnosticStart(Tokens::const_iterator start)
        {
            m_start.push_back(start);
            return UniqueResource([this] { m_start.pop_back(); });
        }

        template <class F>
        auto doBacktracking(F&& f)
        {
            m_branches.emplace_back();
            UniqueResource resource([this] { m_branches.pop_back(); });
            return std::forward<F>(f)();
        }

        std::unique_ptr<Branch> createBranch(Tokens::const_iterator& begin, Branch::CriteriaFunction&& criteria = {});

        [[nodiscard]] std::size_t getCurrentErrorCount() const;
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const std::vector<Lexer::Token>& tokens,
                                                               std::ostream* reporter = &std::cerr);

    OpenCL::Syntax::TranslationUnit parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                         ParsingContext& context);

    ParserResult<Syntax::ExternalDeclaration>
        parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::Declaration> parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                       ParsingContext& context);

    ParserResult<Syntax::DeclarationSpecifier>
        parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::SpecifierQualifier>
        parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::Declarator> parseDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                     ParsingContext& context);

    ParserResult<Syntax::DirectDeclarator> parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                 Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::ParameterTypeList> parseParameterTypeList(Tokens::const_iterator& tokens,
                                                                   Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::AbstractDeclarator>
        parseAbstractDeclarator(Tokens::const_iterator& tokens, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::DirectAbstractDeclarator> parseDirectAbstractDeclarator(Tokens::const_iterator& begin,
                                                                                 Tokens::const_iterator end,
                                                                                 ParsingContext& context);

    ParserResult<Syntax::ParameterList> parseParameterList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                           ParsingContext& context);

    ParserResult<Syntax::Pointer> parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                               ParsingContext& context);

    ParserResult<Syntax::StructOrUnionSpecifier>
        parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::EnumSpecifier> parseEnumSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                           ParsingContext& context);

    ParserResult<Syntax::CompoundStatement> parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                   Tokens::const_iterator end,
                                                                   OpenCL::Parser::ParsingContext& context,
                                                                   bool pushScope = true);

    ParserResult<Syntax::CompoundItem> parseCompoundItem(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                         ParsingContext& context);

    ParserResult<Syntax::Initializer> parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                       ParsingContext& context);

    ParserResult<Syntax::InitializerList> parseInitializerList(Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::Statement> parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                   ParsingContext& context);

    ParserResult<Syntax::ReturnStatement> parseReturnStatement(Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::IfStatement> parseIfStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                       ParsingContext& context);

    ParserResult<Syntax::SwitchStatement> parseSwitchStatement(Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::ForStatement> parseForStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                         ParsingContext& context);

    ParserResult<Syntax::HeadWhileStatement>
        parseHeadWhileStatement(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::FootWhileStatement>
        parseFootWhileStatement(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::Expression> parseExpression(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                     ParsingContext& context);

    ParserResult<Syntax::AssignmentExpression>
        parseAssignmentExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::AssignmentExpressionAssignment>
        parseAssignmentExpressionAssignment(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                            ParsingContext& context, bool* reachedAssignment);

    ParserResult<Syntax::ConditionalExpression>
        parseConditionalExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::LogicalOrExpression>
        parseLogicalOrExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::LogicalAndExpression>
        parseLogicalAndExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::BitOrExpression> parseBitOrExpression(Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::BitXorExpression> parseBitXorExpression(Tokens::const_iterator& begin,
                                                                 Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::BitAndExpression> parseBitAndExpression(Tokens::const_iterator& begin,
                                                                 Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::EqualityExpression>
        parseEqualityExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::RelationalExpression>
        parseRelationalExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::ShiftExpression> parseShiftExpression(Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::AdditiveExpression>
        parseAdditiveExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::Term> parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                         ParsingContext& context);

    ParserResult<Syntax::TypeName> parseTypeName(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                 ParsingContext& context);

    ParserResult<Syntax::CastExpression> parseCastExpression(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                             ParsingContext& context);

    ParserResult<Syntax::UnaryExpression> parseUnaryExpression(Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::PostFixExpression> parsePostFixExpression(Tokens::const_iterator& begin,
                                                                   Tokens::const_iterator end, ParsingContext& context);

    ParserResult<Syntax::PrimaryExpression> parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                   Tokens::const_iterator end, ParsingContext& context);
} // namespace OpenCL::Parser

#endif // OPENCLPARSER_PARSER_HPP

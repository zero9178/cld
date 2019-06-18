#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Semantics.hpp"
#include "Syntax.hpp"
#include "Message.hpp"
#include "../Common/UniqueResource.hpp"

#include <functional>

namespace OpenCL::Parser
{
    using Tokens = std::vector<OpenCL::Lexer::Token>;

    namespace ErrorMessages
    {
        constexpr auto MISSING_DECLARATION_SPECIFIER = "Expected Storage specifier or typename before name";

        constexpr auto EXPECTED_N = Format("Expected {}");

        constexpr auto EXPECTED_N_BEFORE_N = Format("Expected {} before {}");

        constexpr auto EXPECTED_N_AFTER_N = Format("Expected {} after {}");

        constexpr auto EXPECTED_N_INSTEAD_OF_N = Format("Expected {} instead of {}");

        constexpr auto MISSING_PARAMETER_NAME = "Parameter name omitted in function definition";
    }

    namespace Notes
    {
        constexpr auto STRUCT_CANT_BE_EMPTY = "struct must have at least one field";

        constexpr auto UNION_CANT_BE_EMPTY = "union must have at least one field";
    }

    class ParsingContext final
    {
        std::ostream* m_reporter;
        bool m_errorsOccured = false;
        std::vector<std::set<std::string>> m_currentScope{1};
        std::vector<std::set<std::string>> m_typedefs{1};
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

        friend class Branch;

        void logImpl(Message&& error);

    public:
        std::map<std::string, Semantics::RecordType> structOrUnions;

        void addTypedef(const std::string& name);

        bool isTypedef(const std::string& name) const;

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

        void addToScope(std::string name);

        bool isInScope(const std::string& name) const;

        void pushScope();

        void popScope();

        bool isErrorsOccured() const;

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
            auto deleter = [this](void*)
            {
                m_branches.pop_back();
            };
            std::unique_ptr<void, decltype(deleter)> ptr((void*)1, deleter);
            return std::forward<F>(f)();
        }

        std::unique_ptr<Branch> createBranch(Tokens::const_iterator& begin,
                                             Branch::CriteriaFunction&& criteria = {});

        std::size_t getCurrentErrorCount() const;
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const std::vector<Lexer::Token>& tokens,
                                                               std::ostream* reporter = &std::cerr);

    OpenCL::Syntax::TranslationUnit
    parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::ExternalDeclaration>
    parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::FunctionDefinition>
    parseFunctionDefinition(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

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

    std::optional<Syntax::EnumDeclaration>
    parseEnumDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

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

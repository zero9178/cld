#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Semantics.hpp"
#include "Syntax.hpp"

#include <map>

namespace OpenCL::Parser
{
    using Tokens = std::vector<OpenCL::Lexer::Token>;

    class ParsingContext final
    {
        std::vector<std::set<std::string>> m_currentScope;
        std::vector<std::string> m_errors;

    public:
        std::vector<std::set<std::string>> typedefs;
        std::map<std::string, Semantics::RecordType> structOrUnions;

        bool isTypedef(const std::string& name) const;

        void logError(const std::string& text);

        ParsingContext()
        {
            m_currentScope.emplace_back();
            typedefs.emplace_back();
        }

        ~ParsingContext() = default;

        ParsingContext(const ParsingContext&) = delete;

        ParsingContext(ParsingContext&&) = delete;

        ParsingContext& operator=(const ParsingContext&) = delete;

        ParsingContext& operator=(ParsingContext&&) = delete;

        void addToScope(std::string name)
        {
            m_currentScope.back().insert(std::move(name));
        }

        bool isInScope(const std::string& name) const
        {
            for (auto& iter : m_currentScope)
            {
                if (iter.count(name))
                {
                    return true;
                }
            }
            return false;
        }

        void pushScope()
        {
            m_currentScope.emplace_back();
            typedefs.emplace_back();
        }

        void popScope()
        {
            m_currentScope.pop_back();
            typedefs.emplace_back();
        }

        std::vector<std::string>& getErrors();
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const std::vector<Lexer::Token>& tokens);

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
    parseCompoundStatement(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::CompoundItem>
    parseCompoundItem(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Initializer> parseInitializer(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::InitializerList>
    parseInitializerList(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    std::optional<Syntax::Statement> parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
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

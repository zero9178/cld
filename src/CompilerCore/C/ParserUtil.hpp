#ifndef OPENCLPARSER_PARSERUTIL_HPP
#define OPENCLPARSER_PARSERUTIL_HPP

#include "ErrorMessages.hpp"
#include "Parser.hpp"

namespace OpenCL::Parser
{
    bool isAssignment(Lexer::TokenType type);

    bool isInCommonSet(const Lexer::Token& token);

    bool isInCommonSet(Lexer::TokenType tokenType);

    template <class T = void>
    bool expect(Lexer::TokenType expected, std::vector<OpenCL::Lexer::Token>::const_iterator& curr,
                OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context,
                std::vector<Message::Note> notes = {}, [[maybe_unused]] T* value = nullptr)
    {
        if (curr >= end || curr->getTokenType() != expected)
        {
            if (curr >= end)
            {
                context.logError(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args(Lexer::tokenName(expected)),
                    findSemicolonOrEOL(curr, end),
                                 Modifier{end - 1, end, Modifier::InsertAtEnd, Lexer::tokenValue(expected)},
                                 std::move(notes));
            }
            else
            {
                context.logError(OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(Lexer::tokenName(expected),
                                                                                             '\'' + curr->emitBack())
                                     + '\'',
                                 findSemicolonOrEOL(curr, end), Modifier{curr, curr + 1, Modifier::PointAtBeginning},
                                 std::move(notes));
            }
            return false;
        }
        else
        {
            if constexpr (!std::is_void_v<T>)
            {
                if (value)
                {
                    *value = std::get<T>(curr->getValue());
                }
            }
            curr++;
            return true;
        }
    }

    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&&... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G)->Y<G>;

    // firstIsInTranslationUnit not needed

    bool firstIsInExternalDeclaration(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInFunctionDefinition(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDeclaration(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDeclarationSpecifier(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInSpecifierQualifier(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDeclarator(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDirectDeclarator(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInParameterTypeList(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInAbstractDeclarator(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDirectAbstractDeclarator(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInParameterList(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInPointer(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInStructOrUnionSpecifier(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInEnumSpecifier(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInEnumDeclaration(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInCompoundStatement(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInCompoundItem(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInInitializer(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInInitializerList(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInStatement(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInAssignmentExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInConditionalExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInLogicalOrExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInLogicalAndExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInBitOrExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInBitXorExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInBitAndExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInEqualityExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInRelationalExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInShiftExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInAdditiveExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInTerm(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInTypeName(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInCastExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInUnaryExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInPostFixExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInPrimaryExpression(const Lexer::Token& token, const OpenCL::Parser::ParsingContext& context);
} // namespace OpenCL::Parser

#endif // OPENCLPARSER_PARSERUTIL_HPP

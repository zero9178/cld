#ifndef OPENCLPARSER_PARSERUTIL_HPP
#define OPENCLPARSER_PARSERUTIL_HPP

#include "ErrorMessages.hpp"
#include "Parser.hpp"

namespace OpenCL::Parser
{
    bool isAssignment(Lexer::TokenType type);

    template <class T = void>
    bool expect(Lexer::TokenType expected, std::vector<OpenCL::Lexer::Token>::const_iterator& curr,
                Tokens::const_iterator end, Context& context, std::vector<Message::Note> notes = {}, [[maybe_unused]] T* value = nullptr)
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

    void skipUntil(Tokens::const_iterator& begin, Tokens::const_iterator end, InRecoverySet recoverySet);

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

    bool firstIsInExternalDeclaration(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInFunctionDefinition(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInDeclaration(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInDeclarationSpecifier(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInSpecifierQualifier(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInDirectDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInParameterTypeList(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInAbstractDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInDirectAbstractDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInParameterList(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInPointer(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInStructOrUnionSpecifier(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInEnumSpecifier(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInEnumDeclaration(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInCompoundStatement(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInCompoundItem(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInInitializer(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInInitializerList(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInStatement(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInAssignmentExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInConditionalExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInLogicalOrExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInLogicalAndExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInBitOrExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInBitXorExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInBitAndExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInEqualityExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInRelationalExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInShiftExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInAdditiveExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInTerm(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInTypeName(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInCastExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInUnaryExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInPostFixExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);

    bool firstIsInPrimaryExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context);
} // namespace OpenCL::Parser

#endif // OPENCLPARSER_PARSERUTIL_HPP

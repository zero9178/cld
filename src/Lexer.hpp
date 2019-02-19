#ifndef OPENCLPARSER_LEXER_HPP
#define OPENCLPARSER_LEXER_HPP

#include <string>
#include <vector>
#include <variant>

namespace OpenCL::Lexer
{
    enum class TokenType
    {
         Identifier,

         OpenParanthese,

         CloseParanthese,

         OpenBrace,

         CloseBrace,

         IntegerLiteral,

         SemiColon,

         Comma,

         Negation,

         BitWiseNegation,

         LogicalNegation,

         Addition,

         Multiplication,

         Division,

         Modulo,

         LogicAnd,

         LogicOr,

         BitAnd,

         BitOr,

         BitXor,

         Equal,

         NotEqual,

         LessThan,

         LessThanOrEqual,

         GreaterThan,

         GreaterThanOrEqual,

         Assignment,

         PlusAssign,

         MinusAssign,

         DivideAssign,

         MultiplyAssign,

         ModuloAssign,

         ShiftLeftAssign,

         ShiftRightAssign,

         BitAndAssign,

         BitOrAssign,

         BitXorAssign,

         ShiftRight,

         ShiftLeft,

         Increment,

         Decrement,

         Colon,

         QuestionMark,

         IntKeyword,

         ReturnKeyword,

         BreakKeyword,

         ContinueKeyword,

         DoKeyword,

         ElseKeyword,

         ForKeyword,

         IfKeyword,

         WhileKeyword,
    };

    class Token
    {
        TokenType m_tokenType;
        std::variant<std::monostate,std::uint64_t,std::string> m_value;

    public:
    };

    std::vector<Token> tokenize(const std::string& source);
}

#endif //OPENCLPARSER_LEXER_HPP

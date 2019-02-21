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
        OpenParenthese,
        CloseParenthese,
        OpenBrace,
        CloseBrace,
        IntegerLiteral,
        StringLiteral,
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
        VoidKeyword,
        CharKeyword,
        ShortKeyword,
        IntKeyword,
        LongKeyword,
        FloatKeyword,
        DoubleKeyword,
        SignedKeyword,
        UnsignedKeyword,
        TypedefKeyword,
        ExternKeyword,
        StaticKeyword,
        AutoKeyword,
        RegisterKeyword,
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
        std::uint64_t m_line;
        std::uint64_t m_column;
        using variant = std::variant<std::monostate, std::uint64_t, std::string>;
        TokenType m_tokenType;
        variant m_value;

    public:

        explicit Token(std::uint64_t line, std::uint64_t column, TokenType tokenType) noexcept
            : m_line(line), m_column(column), m_tokenType(tokenType)
        {}

        template <class T>
        Token(std::uint64_t line, std::uint64_t column, TokenType tokenType, T&& value)
            : m_line(line), m_column(column), m_tokenType(tokenType), m_value(std::forward<T>(value))
        {}

        TokenType getTokenType() const
        {
            return m_tokenType;
        }

        const variant& getValue() const
        {
            return m_value;
        }
    };

    std::vector<Token> tokenize(const std::string& source);
}

#endif //OPENCLPARSER_LEXER_HPP

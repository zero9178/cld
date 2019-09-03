#ifndef OPENCLPARSER_LEXER_HPP
#define OPENCLPARSER_LEXER_HPP

#include <iostream>
#include <string>
#include <variant>
#include <vector>

namespace OpenCL::Lexer
{
    enum class TokenType
    {
        Identifier,
        OpenParenthese,
        CloseParenthese,
        OpenBrace,
        CloseBrace,
        Literal,
        StringLiteral,
        SemiColon,
        Comma,
        Minus,
        BitWiseNegation,
        LogicalNegation,
        Plus,
        Asterisk,
        Division,
        Percent,
        LogicAnd,
        LogicOr,
        Ampersand,
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
        ConstKeyword,
        RestrictKeyword,
        SizeofKeyword,
        VolatileKeyword,
        InlineKeyword,
        ReturnKeyword,
        BreakKeyword,
        ContinueKeyword,
        DoKeyword,
        ElseKeyword,
        ForKeyword,
        IfKeyword,
        WhileKeyword,
        OpenSquareBracket,
        CloseSquareBracket,
        StructKeyword,
        Dot,
        Arrow,
        SwitchKeyword,
        CaseKeyword,
        DefaultKeyword,
        UnionKeyword,
        EnumKeyword,
        GotoKeyword,
        Ellipse,
        Pound,
        DoublePound
    };

    class Token;

    std::vector<Token> tokenize(std::string source, std::ostream* reporter = &std::cerr);

    class Token
    {
        std::uint64_t m_line;
        std::uint64_t m_column;
        std::uint64_t m_length;
        using variant = std::variant<std::monostate, std::int32_t, std::uint32_t, std::int64_t, std::uint64_t, float,
                                     double, std::string>;
        TokenType m_tokenType;
        variant m_value;
        std::string m_valueRepresentation;

        friend std::vector<Token> tokenize(std::string source, std::ostream* reporter);

    public:
        using ValueType = variant;

        Token(std::uint64_t line, std::uint64_t column, std::uint64_t length, TokenType tokenType) noexcept
            : Token(line, column, length, tokenType, std::monostate{}, "")
        {
        }

        template <class T>
        Token(std::uint64_t line, std::uint64_t column, std::uint64_t length, TokenType tokenType, T&& value,
              std::string valueRepresentation)
            : m_line(line),
              m_column(column),
              m_length(length),
              m_tokenType(tokenType),
              m_value(std::forward<T>(value)),
              m_valueRepresentation(std::move(valueRepresentation))
        {
        }

        [[nodiscard]] TokenType getTokenType() const
        {
            return m_tokenType;
        }

        [[nodiscard]] const variant& getValue() const
        {
            return m_value;
        }

        [[nodiscard]] std::uint64_t getLine() const
        {
            return m_line;
        }

        [[nodiscard]] std::uint64_t getColumn() const
        {
            return m_column;
        }

        [[nodiscard]] std::uint64_t getLength() const;

        [[nodiscard]] std::string emitBack() const;
    };

    /**
     * @param tokenType Token
     * @return name of the token. If the token is a punctuator its surrounded in '
     */
    std::string tokenName(TokenType tokenType);

    /**
     * @param tokenType Token
     * @return generic value of the token. For non punctuators this is just a description. eg. identifier
     */
    std::string tokenValue(TokenType tokenType);

    std::string reconstruct(std::vector<Token>::const_iterator begin, std::vector<Token>::const_iterator end);
} // namespace OpenCL::Lexer

#endif // OPENCLPARSER_LEXER_HPP

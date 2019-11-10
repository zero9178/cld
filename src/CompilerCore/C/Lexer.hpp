#ifndef OPENCLPARSER_LEXER_HPP
#define OPENCLPARSER_LEXER_HPP

#include <iostream>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace OpenCL
{
    class SourceObject;

    enum class Language;

    namespace Lexer
    {
        enum class TokenType : std::uint8_t
        {
            Identifier,
            OpenParentheses,
            CloseParentheses,
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
            VoidKeyword,        ///<[C,OpenCL]
            CharKeyword,        ///<[C,OpenCL]
            ShortKeyword,       ///<[C,OpenCL]
            IntKeyword,         ///<[C,OpenCL]
            LongKeyword,        ///<[C,OpenCL]
            FloatKeyword,       ///<[C,OpenCL]
            DoubleKeyword,      ///<[C,OpenCL]
            SignedKeyword,      ///<[C,OpenCL]
            UnsignedKeyword,    ///<[C,OpenCL]
            TypedefKeyword,     ///<[C,OpenCL]
            ExternKeyword,      ///<[C,OpenCL]
            StaticKeyword,      ///<[C,OpenCL]
            AutoKeyword,        ///<[C,OpenCL]
            RegisterKeyword,    ///<[C,OpenCL]
            ConstKeyword,       ///<[C,OpenCL]
            RestrictKeyword,    ///<[C,OpenCL]
            SizeofKeyword,      ///<[C,OpenCL]
            DefinedKeyword,     ///<[PP]
            Newline,            ///<[PP]
            VolatileKeyword,    ///<[C,OpenCL]
            InlineKeyword,      ///<[C,OpenCL]
            ReturnKeyword,      ///<[C,OpenCL]
            BreakKeyword,       ///<[C,OpenCL]
            ContinueKeyword,    ///<[C,OpenCL]
            DoKeyword,          ///<[C,OpenCL]
            ElseKeyword,        ///<[C,OpenCL]
            ForKeyword,         ///<[C,OpenCL]
            IfKeyword,          ///<[C,OpenCL]
            WhileKeyword,       ///<[C,OpenCL]
            OpenSquareBracket,  ///<[C,OpenCL]
            CloseSquareBracket, ///<[C,OpenCL]
            StructKeyword,      ///<[C,OpenCL]
            Dot,
            Arrow,
            SwitchKeyword,  ///<[C,OpenCL]
            CaseKeyword,    ///<[C,OpenCL]
            DefaultKeyword, ///<[C,OpenCL]
            UnionKeyword,   ///<[C,OpenCL]
            EnumKeyword,    ///<[C,OpenCL]
            GotoKeyword,    ///<[C,OpenCL]
            UnderlineBool,  ///<[C,OpenCL]
            Ellipse,
            Pound,
            DoublePound,
            Backslash, ///<[PP]
            TOKEN_MAX_VALUE = DoublePound
        };

        class Token;

        SourceObject tokenize(std::string source, Language language, std::ostream* reporter = &std::cerr);

        class Token
        {
            std::uint64_t m_line;   ///<
            std::uint64_t m_column; ///<
            std::uint64_t m_length; ///<
            using variant = std::variant<std::monostate, std::int32_t, std::uint32_t, std::int64_t, std::uint64_t,
                                         float, double, std::string, std::wstring>;
            TokenType m_tokenType;             ///< Type of the token
            variant m_value;                   ///< Optional value of the token
            std::string m_valueRepresentation; ///< Original spelling of the token
            std::uint64_t m_macroId = 0;       ///< MacroID. All tokens with the same ID have been inserted by the same
                                         ///< macro substitution. ID of 0 means the the token originated from the Lexer
            std::uint64_t m_sourceLine;   ///<
            std::uint64_t m_sourceColumn; ///<
            std::uint64_t m_sourceLength; ///<

        public:
            using ValueType = variant;

            Token(std::uint64_t line, std::uint64_t column, std::uint64_t length, TokenType tokenType) noexcept;

            Token(std::uint64_t line, std::uint64_t column, std::uint64_t length, TokenType tokenType, variant value,
                  std::string valueRepresentation);

            [[nodiscard]] TokenType getTokenType() const noexcept;

            [[nodiscard]] const variant& getValue() const noexcept;

            [[nodiscard]] bool macroInserted() const noexcept;

            [[nodiscard]] std::uint64_t getLine() const noexcept;

            void setLine(std::uint64_t line) noexcept;

            [[nodiscard]] std::uint64_t getColumn() const noexcept;

            void setColumn(std::uint64_t column) noexcept;

            [[nodiscard]] std::uint64_t getLength() const noexcept;

            void setLength(std::uint64_t length) noexcept;

            [[nodiscard]] std::uint64_t getMacroId() const noexcept;

            void setMacroId(std::uint64_t macroId) noexcept;

            [[nodiscard]] std::uint64_t getSourceLine() const noexcept;

            void setSourceLine(std::uint64_t sourceLine) noexcept;

            [[nodiscard]] std::uint64_t getSourceColumn() const noexcept;

            void setSourceColumn(std::uint64_t sourceColumn) noexcept;

            [[nodiscard]] std::uint64_t getSourceLength() const noexcept;

            void setSourceLength(std::uint64_t sourceLength) noexcept;

            [[nodiscard]] std::string emitBack() const;
        };

        struct ConcatReturn
        {
            Token left;
            std::optional<Token> right{};
        };

        std::optional<ConcatReturn> concat(const OpenCL::Lexer::Token& lhs, const OpenCL::Lexer::Token& rhs,
                                           std::ostream* reporter);

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

        std::string reconstructTrimmed(std::vector<Token>::const_iterator begin,
                                       std::vector<Token>::const_iterator end);
    } // namespace Lexer
} // namespace OpenCL

#endif // OPENCLPARSER_LEXER_HPP

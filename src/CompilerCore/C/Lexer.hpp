#ifndef OPENCLPARSER_LEXER_HPP
#define OPENCLPARSER_LEXER_HPP

#include <llvm/Support/raw_ostream.h>

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "LanguageOptions.hpp"

namespace OpenCL
{
    class SourceObject;

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
            Pound,         ///<[PP]
            DoublePound,   ///<[PP]
            Miscellaneous, ///<[PP]
            TOKEN_MAX_VALUE = DoublePound,
        };

        class Token;

        SourceObject tokenize(std::string source, LanguageOptions languageOptions = LanguageOptions::native(),
                              bool inPreprocessor = false, llvm::raw_ostream* reporter = &llvm::errs());

        struct NonCharString
        {
            enum Type
            {
                Wide
            } type;
            std::vector<std::uint32_t> characters;
        };

        class Token
        {
            using variant = std::variant<std::monostate, std::int32_t, std::uint32_t, std::int64_t, std::uint64_t,
                                         float, double, std::string, NonCharString>;
            variant m_value;              ///< Optional value of the token
            std::string m_representation; ///< Original spelling of the token
            std::uint64_t m_macroId = 0;  /**< MacroID. All tokens with the same ID have been inserted by the same macro
                                             substitution. ID of 0 means the the token originated from the Lexer*/
            std::uint64_t m_offset;       /**< Offset of the token. That is bytes offset to the first character of the
                                               token from the beginning of the file*/
            std::uint64_t m_sourceOffset; /**< Original offset of the token in the source code. For a token originating
                                           from the replacement list of a macro declaration it will points to it's
                                           original location in the replacement list. Therefore many tokens inside of a
                                           source file can have the same offset*/
            TokenType m_tokenType;        ///< Type of the token

        public:
            using ValueType = variant;

            Token(std::uint64_t offset, TokenType tokenType, std::string representation,
                  variant value = std::monostate{});

            [[nodiscard]] TokenType getTokenType() const noexcept;

            [[nodiscard]] const variant& getValue() const noexcept;

            [[nodiscard]] bool macroInserted() const noexcept;

            [[nodiscard]] std::uint64_t getOffset() const noexcept;

            [[nodiscard]] std::uint64_t getSourceOffset() const noexcept;

            [[nodiscard]] std::size_t getLength() const noexcept;

            [[nodiscard]] std::uint64_t getMacroId() const noexcept;

            void setMacroId(std::uint64_t macroId) noexcept;

            [[nodiscard]] std::string getRepresentation() const;
        };

        /**
         * @param tokenType Token
         * @return name of the token. If the token is a punctuation it's surrounded in '
         */
        std::string tokenName(TokenType tokenType);

        /**
         * @param tokenType Token
         * @return generic value of the token. For non punctuations this is just a description. eg. identifier
         */
        std::string tokenValue(TokenType tokenType);

        std::string reconstruct(std::vector<Token>::const_iterator begin, std::vector<Token>::const_iterator end);

        std::string reconstructTrimmed(std::vector<Token>::const_iterator begin,
                                       std::vector<Token>::const_iterator end);
    } // namespace Lexer
} // namespace OpenCL

#endif // OPENCLPARSER_LEXER_HPP

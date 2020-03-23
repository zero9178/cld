#pragma once

#pragma warning(push, 0)
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/Support/raw_ostream.h>
#pragma warning(pop)

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "LanguageOptions.hpp"

namespace cld
{
class SourceObject;
class PPSourceObject;

namespace Lexer
{
enum class TokenType : std::uint8_t
{
    Identifier,
    OpenParentheses,
    CloseParentheses,
    OpenBrace,
    CloseBrace,
    Literal, ///<[C,OpenCL]
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
    PPNumber,      ///<[PP]
    Backslash,     ///<[PP]
    Pound,         ///<[PP]
    DoublePound,   ///<[PP]
    Miscellaneous, ///<[PP]
    TOKEN_MAX_VALUE = Miscellaneous,
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

    bool operator==(const std::wstring& wideString) const
    {
        if (type != Wide)
        {
            return false;
        }
        else
        {
            return std::equal(characters.begin(), characters.end(), wideString.begin(), wideString.end(),
                              [](auto lhs, auto rhs) { return (std::int64_t)lhs == (std::int64_t)rhs; });
        }
    }
};

class Token
{
    using variant = std::variant<std::monostate, llvm::APSInt, llvm::APFloat, std::string, NonCharString>;
    variant m_value;              ///< Optional value of the token
    std::string m_representation; ///< Original spelling of the token
    std::uint64_t m_macroId = 0;  /**< MacroID. All tokens with the same ID have been inserted by the same macro
                                     substitution. ID of 0 means the the token originated from the Lexer*/
    std::uint64_t m_offset;       /**< Offset of the token. That is bytes offset to the first character of the
                                       token from the beginning of the file of the very original source code passed
                                       from the user. This value is not unique as after preprocessing all inserted
                                       tokens have the offset of the original position in the replacement list*/
    std::uint64_t
        m_afterPPOffset;   /**< Effective offset of the token after preprocessing. Must be equal to m_offset if
                              m_macroId == 0. This value changes during pre processing and is therefore mutable.*/
    TokenType m_tokenType; ///< Type of the token

public:
    enum class Type
    {
        None,
        Int,
        UnsignedInt,
        Long,
        UnsignedLong,
        LongLong,
        UnsignedLongLong,
        Float,
        Double,
        LongDouble
    };

private:
    Type m_type;

public:
    using ValueType = variant;

    Token(std::uint64_t offset, TokenType tokenType, std::string representation, variant value = std::monostate{},
          Type type = Type::None);

    [[nodiscard]] TokenType getTokenType() const noexcept;

    [[nodiscard]] const variant& getValue() const noexcept;

    [[nodiscard]] Type getType() const;

    [[nodiscard]] bool macroInserted() const noexcept;

    [[nodiscard]] std::uint64_t getOffset() const noexcept;

    [[nodiscard]] std::uint64_t getPPOffset() const noexcept;

    void setPPOffset(std::uint64_t ppOffset) noexcept;

    [[nodiscard]] std::size_t getLength() const noexcept;

    [[nodiscard]] std::uint64_t getMacroId() const noexcept;

    void setMacroId(std::uint64_t macroId) noexcept;

    [[nodiscard]] const std::string& getRepresentation() const;

    [[nodiscard]] std::uint64_t getLine(const SourceObject& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getPPLine(const PPSourceObject& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getColumn(const SourceObject& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getPPColumn(const PPSourceObject& sourceObject) const noexcept;
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

std::string reconstruct(const SourceObject& sourceObject, std::vector<Token>::const_iterator begin,
                        std::vector<Token>::const_iterator end);

std::string reconstructTrimmed(const SourceObject& sourceObject, std::vector<cld::Lexer::Token>::const_iterator begin,
                               std::vector<cld::Lexer::Token>::const_iterator end);

std::string constructPP(const PPSourceObject& sourceObject, std::vector<Token>::const_iterator begin,
                        std::vector<Token>::const_iterator end);

std::string constructPPTrimmed(const PPSourceObject& sourceObject, std::vector<cld::Lexer::Token>::const_iterator begin,
                               std::vector<cld::Lexer::Token>::const_iterator end);
} // namespace Lexer
} // namespace cld


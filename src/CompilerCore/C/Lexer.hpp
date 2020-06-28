#pragma once

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/Common/Util.hpp>

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <variant>
#include <vector>

#include "LanguageOptions.hpp"
#include "SourceInterface.hpp"

namespace cld
{
namespace Lexer
{
class TokenBase;
class CToken;
class PPToken;
} // namespace Lexer

template <class T>
class SourceObject;

using CSourceObject = SourceObject<Lexer::CToken>;
using PPSourceObject = SourceObject<Lexer::PPToken>;

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

using TokenIterator = const TokenBase*;
using CTokenIterator = const CToken*;
using PPTokenIterator = const PPToken*;

PPSourceObject tokenize(std::string_view source, LanguageOptions languageOptions = LanguageOptions::native(),
                        llvm::raw_ostream* reporter = &llvm::errs(), bool* errorsOccured = nullptr,
                        std::string_view sourceFile = "<stdin>");

CSourceObject toCTokens(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(),
                        bool* errorsOccured = nullptr);

std::vector<CToken> toCTokens(PPTokenIterator begin, PPTokenIterator end, const PPSourceObject& sourceObject,
                              llvm::raw_ostream* reporter = &llvm::errs(), bool* errorsOccured = nullptr);

class TokenBase
{
protected:
    bool m_leadingWhitespace : 1;
    TokenType m_tokenType;   ///< Type of the token
    std::uint32_t m_macroId; ///< MacroID. All tokens with the same ID have been inserted by the same macro
    std::uint32_t m_fileID;
    std::uint64_t m_offset; ///< Offset of the token. That is bytes offset to the first character of the
    std::uint64_t m_length;

    TokenBase() = default;

    TokenBase(TokenType tokenType, std::uint64_t offset, std::uint64_t length, std::uint32_t fileID,
              std::uint32_t macroID)
        : m_leadingWhitespace(false),
          m_tokenType(tokenType),
          m_macroId(macroID),
          m_fileID(fileID),
          m_offset(offset),
          m_length(length)
    {
    }

public:
    [[nodiscard]] std::string_view getRepresentation(const SourceInterface& sourceObject) const;

    [[nodiscard]] std::uint64_t getLine(const SourceInterface& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getColumn(const SourceInterface& sourceObject) const noexcept;

    [[nodiscard]] TokenType getTokenType() const noexcept
    {
        return m_tokenType;
    }

    [[nodiscard]] bool isMacroInserted() const noexcept
    {
        return static_cast<bool>(m_macroId);
    }

    [[nodiscard]] std::uint64_t getOffset() const noexcept
    {
        return m_offset;
    }

    [[nodiscard]] std::size_t getLength() const noexcept
    {
        return m_length;
    }

    [[nodiscard]] std::uint32_t getMacroId() const noexcept
    {
        return m_macroId;
    }

    void setMacroId(std::uint32_t macroId) noexcept
    {
        m_macroId = macroId;
    }

    [[nodiscard]] std::uint32_t getFileId() const
    {
        return m_fileID;
    }

    void setFileId(std::uint32_t fileId) noexcept
    {
        m_fileID = fileId;
    }

    [[nodiscard]] bool hasLeadingWhitespace() const noexcept
    {
        return m_leadingWhitespace;
    }

    void setLeadingWhitespace(bool leadingWhitespace) noexcept
    {
        m_leadingWhitespace = leadingWhitespace;
    }
};

using IntervalMap = std::vector<std::tuple<std::uint64_t, std::uint64_t, std::uint64_t>>;

class PPToken final : public TokenBase
{
    std::string m_value;
    std::uint64_t m_charSpaceOffset;
    std::uint64_t m_charSpaceLength; /**< Length of the token after trigraphs and Backslash Newline pairs in it's
                                      representation have been removed*/

public:
    PPToken(TokenType tokenType, std::uint64_t offset, std::uint64_t length, std::uint64_t charSpaceOffset,
            std::uint64_t charSpaceLength, std::uint32_t fileID, std::uint32_t macroID = 0, std::string_view value = {})
        : TokenBase(tokenType, offset, length, fileID, macroID),
          m_value(value.begin(), value.end()),
          m_charSpaceOffset(charSpaceOffset),
          m_charSpaceLength(charSpaceLength)
    {
    }

    [[nodiscard]] std::uint64_t getCharSpaceOffset() const noexcept
    {
        return m_charSpaceOffset;
    }

    [[nodiscard]] std::uint64_t getCharSpaceLength() const
    {
        return m_charSpaceLength;
    }

    [[nodiscard]] std::string_view getValue() const noexcept
    {
        return m_value;
    }
};

static_assert(!std::is_polymorphic_v<PPToken>);

struct NonCharString
{
    std::vector<std::uint32_t> characters;
    enum Type : std::uint8_t
    {
        Wide
    } type;

    bool operator==(const std::wstring& wideString) const
    {
        if (type != Wide)
        {
            return false;
        }
        return std::equal(characters.begin(), characters.end(), wideString.begin(), wideString.end());
    }

    bool operator==(const NonCharString& other) const
    {
        if (type != other.type)
        {
            return false;
        }
        return std::equal(characters.begin(), characters.end(), other.characters.begin(), other.characters.end());
    }
};

class CToken final : public TokenBase
{
    using variant = std::variant<std::monostate, llvm::APSInt, llvm::APFloat, std::string, NonCharString>;
    variant m_value; ///< Optional value of the token

public:
    enum class Type : std::uint8_t
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

    CToken(TokenType tokenType, std::uint64_t offset, std::uint64_t length, std::uint32_t fileId, std::uint32_t macroId,
           variant value = std::monostate{}, Type type = Type::None)
        : TokenBase(tokenType, offset, length, fileId, macroId), m_value(std::move(value)), m_type(type)
    {
    }

    [[nodiscard]] const variant& getValue() const noexcept
    {
        return m_value;
    }

    [[nodiscard]] Type getType() const
    {
        return m_type;
    }
};

static_assert(!std::is_polymorphic_v<CToken>);

/**
 * @param tokenType Token
 * @return name of the token. If the token is a punctuation it's surrounded in '
 */
std::string_view tokenName(TokenType tokenType);

/**
 * @param tokenType Token
 * @return generic value of the token. For non punctuations this is just a description. eg. identifier
 */
std::string_view tokenValue(TokenType tokenType);

/**
 * Normalizes the spelling of a token by removing backslash newline pairs and replacing trigraphs
 *
 * Does not change Digraphs as those are distinct and different spellings of a particular token type
 * @param tokenSpelling Spelling of a token
 * @return Normalized spelling of that token
 */
std::string normalizeSpelling(std::string_view tokenSpelling);

bool needsWhitespaceInBetween(TokenType left, TokenType right) noexcept;

// std::string constructPP(const PPSourceObject& sourceObject, PPTokenIterator begin, PPTokenIterator end);
//
// std::string constructPPTrimmed(const PPSourceObject& sourceObject, PPTokenIterator begin, PPTokenIterator end);
} // namespace Lexer
} // namespace cld

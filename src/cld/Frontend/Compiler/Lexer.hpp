#pragma once

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/Support/raw_ostream.h>

#include <cld/Support/Util.hpp>

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <variant>
#include <vector>

#include "CustomDiag.hpp"
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
    Int128Keyword,  ///<[C,OpenCL]
    GNUAttribute,   ///<[GNUC]
    GNUExtension,   ///<[GNUC]
    GNUTypeOf,      ///<[GNUC]
    GNUASM,         ///<[GNUC]
    Ellipse,
    PPNumber,      ///<[PP]
    Backslash,     ///<[PP]
    Pound,         ///<[PP]
    DoublePound,   ///<[PP]
    Miscellaneous, ///<[PP]
    TOKEN_MAX_VALUE = Miscellaneous,
};

bool isText(TokenType tokenType);

using TokenIterator = const TokenBase* CLD_NON_NULL;
using CTokenIterator = const CToken* CLD_NON_NULL;
using PPTokenIterator = const PPToken* CLD_NON_NULL;

PPSourceObject tokenize(std::string source, not_null<const LanguageOptions> languageOptions,
                        llvm::raw_ostream* reporter = &llvm::errs(), bool* errorsOccured = nullptr,
                        std::string_view sourcePath = "<stdin>");

CSourceObject toCTokens(PPSourceObject&& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(),
                        bool* errorsOccurred = nullptr);

CSourceObject toCTokens(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(),
                        bool* errorsOccurred = nullptr);

std::vector<CToken> toCTokens(PPTokenIterator begin, PPTokenIterator end, const PPSourceInterface& sourceInterface,
                              llvm::raw_ostream* reporter = &llvm::errs(), bool* errorsOccurred = nullptr);

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
    enum Type : std::uint8_t
    {
        None,
        UnsignedShort, // wchar_t on Windows
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

    static Type fromUnderlyingType(LanguageOptions::UnderlyingType underlyingType)
    {
        switch (underlyingType)
        {
            case LanguageOptions::UnderlyingType::UnsignedShort: return UnsignedShort;
            case LanguageOptions::UnderlyingType::Int: return Int;
            case LanguageOptions::UnderlyingType::UnsignedInt: return UnsignedInt;
            case LanguageOptions::UnderlyingType::Long: return Long;
            case LanguageOptions::UnderlyingType::UnsignedLong: return UnsignedLong;
            case LanguageOptions::UnderlyingType::LongLong: return LongLong;
            case LanguageOptions::UnderlyingType::UnsignedLongLong: return UnsignedLongLong;
        }
        CLD_UNREACHABLE;
    }

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

    [[nodiscard]] std::string_view getText() const noexcept
    {
        return cld::get<std::string>(m_value);
    }

    [[nodiscard]] Type getType() const noexcept
    {
        return m_type;
    }

    void setType(Type type) noexcept
    {
        m_type = type;
    }

    void setValue(const variant& value) noexcept
    {
        m_value = value;
    }
};

static_assert(!std::is_polymorphic_v<CToken>);

std::optional<CToken> parseStringLiteral(const PPToken& ppToken, const PPSourceInterface& sourceInterface,
                                         llvm::raw_ostream* reporter = nullptr);

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

} // namespace Lexer

namespace diag
{
template <>
struct CustomFormat<U't', U'o', U'k', U'e', U'n', U'T', U'y', U'p', U'e'>
{
    template <class T>
    std::string operator()(const T& token) const
    {
        static_assert(std::is_base_of_v<Lexer::TokenBase, T>, "Argument to %tokenType must be a token class");
        auto view = Lexer::tokenName(token.getTokenType());
        return std::string(view.begin(), view.end());
    }
};

template <>
struct StringConverter<Lexer::TokenType>
{
    static std::string inFormat(Lexer::TokenType arg, const SourceInterface*)
    {
        return cld::to_string(tokenName(arg));
    }

    static std::string inArg(Lexer::TokenType arg, const SourceInterface*)
    {
        return cld::to_string(tokenValue(arg));
    }
};

template <>
struct StringConverter<Lexer::TokenBase>
{
    static std::string inFormat(const Lexer::TokenBase& arg, const SourceInterface* sourceInterface)
    {
        CLD_ASSERT(sourceInterface);
        auto spelling = Lexer::normalizeSpelling(arg.getRepresentation(*sourceInterface));
        for (std::size_t i = 0; i < spelling.size(); i++)
        {
            if (spelling[i] != '\n')
            {
                continue;
            }
            spelling.replace(i, 1, "\\n");
        }
        return "'" + spelling + "'";
    }

    static std::string inArg(const Lexer::TokenBase& arg, const SourceInterface* sourceInterface)
    {
        CLD_ASSERT(sourceInterface);
        auto spelling = Lexer::normalizeSpelling(arg.getRepresentation(*sourceInterface));
        for (std::size_t i = 0; i < spelling.size(); i++)
        {
            if (spelling[i] != '\n')
            {
                continue;
            }
            spelling.replace(i, 1, "\\n");
        }
        return spelling;
    }
};

template <>
struct StringConverter<Lexer::CToken> : StringConverter<Lexer::TokenBase>
{
};

template <>
struct StringConverter<Lexer::PPToken> : StringConverter<Lexer::TokenBase>
{
};

} // namespace diag

} // namespace cld

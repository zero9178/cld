#pragma once

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/Common/Util.hpp>

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "LanguageOptions.hpp"
#include "SourceObject.hpp"

namespace cld
{
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

template <class T>
using TokenIterator = typename std::vector<T>::const_iterator;
using CTokenIterator = TokenIterator<CToken>;
using PPTokenIterator = TokenIterator<PPToken>;


PPSourceObject tokenize(std::string_view source, LanguageOptions languageOptions = LanguageOptions::native(),
                        llvm::raw_ostream* reporter = &llvm::errs(), bool* errorsOccured = nullptr,std::string_view sourceFile = "<stdin>");

CSourceObject toCTokens(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(),
                        bool* errorsOccured = nullptr);

std::vector<CToken> toCTokens(PPTokenIterator begin,PPTokenIterator end, const PPSourceObject& sourceObject,
                              llvm::raw_ostream* reporter = &llvm::errs(), bool* errorsOccured = nullptr);

template <class Derived>
class TokenBase
{
protected:
    std::uint64_t m_length;
    std::uint64_t m_macroId = 0;   /**< MacroID. All tokens with the same ID have been inserted by the same macro
                                      substitution. ID of 0 means the the token originated from the Lexer*/
    std::uint64_t m_offset;        /**< Offset of the token. That is bytes offset to the first character of the
                                        token from the beginning of the file of the very original source code passed
                                        from the user. This value is not unique as after preprocessing all inserted
                                        tokens have the offset of the original position in the replacement list*/
    std::uint64_t m_afterPPOffset; /**< Effective offset of the token after preprocessing. Must be equal to m_offset if
                            m_macroId == 0. This value changes during pre processing and is therefore mutable.*/
    std::uint64_t m_fileID = 0;
    TokenType m_tokenType; ///< Type of the token

    TokenBase() = default;

    TokenBase(std::uint64_t length, std::uint64_t offset, std::uint64_t afterPPOffset, TokenType tokenType)
        : m_length(length), m_offset(offset), m_afterPPOffset(afterPPOffset), m_tokenType(tokenType)
    {
    }

public:
    [[nodiscard]] std::string_view getRepresentation(const SourceObject<Derived>& sourceObject) const
    {
        return std::string_view(sourceObject.getFiles()[m_fileID].source).substr(m_offset, m_length);
    }

    [[nodiscard]] std::uint64_t getLine(const SourceObject<Derived>& sourceObject) const noexcept
    {
        return sourceObject.getLineNumber(m_fileID, m_offset);
    }

    [[nodiscard]] std::uint64_t getPPLine(const SourceObject<Derived>& sourceObject) const noexcept
    {
        return sourceObject.getPPLineNumber(m_fileID, m_afterPPOffset);
    }

    [[nodiscard]] std::uint64_t getColumn(const SourceObject<Derived>& sourceObject) const noexcept
    {
        auto line = sourceObject.getLineNumber(m_fileID, m_offset);
        return m_offset - sourceObject.getLineStartOffset(m_fileID, line) + 1;
    }

    [[nodiscard]] std::uint64_t getPPColumn(const SourceObject<Derived>& sourceObject) const noexcept
    {
        auto line = sourceObject.getPPLineNumber(m_fileID, m_afterPPOffset);
        return m_afterPPOffset - sourceObject.getPPLineStartOffset(m_fileID, line) + 1;
    }

    [[nodiscard]] TokenType getTokenType() const noexcept
    {
        return m_tokenType;
    }

    [[nodiscard]] bool macroInserted() const noexcept
    {
        return m_macroId;
    }

    [[nodiscard]] std::uint64_t getOffset() const noexcept
    {
        return m_offset;
    }

    [[nodiscard]] std::uint64_t getPPOffset() const noexcept
    {
        return m_afterPPOffset;
    }

    void setPPOffset(std::uint64_t ppOffset) noexcept
    {
        m_afterPPOffset = ppOffset;
    }

    [[nodiscard]] std::size_t getLength() const noexcept
    {
        return m_length;
    }

    [[nodiscard]] std::uint64_t getMacroId() const noexcept
    {
        return m_macroId;
    }

    void setMacroId(std::uint64_t macroId) noexcept
    {
        m_macroId = macroId;
    }

    [[nodiscard]] std::uint64_t getFileId() const
    {
        return m_fileID;
    }

    void setFileId(std::uint64_t fileId)
    {
        m_fileID = fileId;
    }
};

class PPToken : public TokenBase<PPToken>
{
    std::string m_value;
    std::uint64_t m_charSpaceOffset; /**< Offset to the token in bytes after trigraphs have been replaced and
                                      Backslash Newline pairs have been spliced*/
    std::uint64_t m_charSpaceLength; /**< Length of the token if any trigraphs and Backslash Newline pairs in it's
                                      representation have been removed*/
public:
    PPToken(TokenType tokenType, std::uint64_t offset, std::uint64_t length, std::uint64_t charSpaceOffset,
            std::uint64_t charSpaceLength, std::string_view value = {});

    [[nodiscard]] std::uint64_t getCharSpaceLength() const
    {
        return m_charSpaceLength;
    }

    [[nodiscard]] std::uint64_t getCharSpaceOffset() const
    {
        return m_charSpaceOffset;
    }

    [[nodiscard]] const std::string& getValue() const noexcept
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

class CToken : public TokenBase<CToken>
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

    CToken(TokenType tokenType, std::uint64_t offset, std::uint64_t length, variant value = std::monostate{},
           Type type = Type::None);

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

std::string constructPP(const PPSourceObject& sourceObject, PPTokenIterator begin, PPTokenIterator end);

std::string constructPPTrimmed(const PPSourceObject& sourceObject, PPTokenIterator begin, PPTokenIterator end);
} // namespace Lexer
} // namespace cld

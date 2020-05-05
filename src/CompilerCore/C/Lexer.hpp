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

SourceObject tokenize(std::string_view source, LanguageOptions languageOptions = LanguageOptions::native(),
                      bool isInPreprocessor = false, llvm::raw_ostream* reporter = &llvm::errs(),
                      bool* errorsOccured = nullptr);

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

class Token
{
    using variant = std::variant<std::monostate, llvm::APSInt, llvm::APFloat, std::string, NonCharString>;
    variant m_value; ///< Optional value of the token
    const char* m_originalSource = nullptr;
    std::uint64_t m_length;
    std::uint64_t m_macroId = 0; /**< MacroID. All tokens with the same ID have been inserted by the same macro
                                    substitution. ID of 0 means the the token originated from the Lexer*/
    std::uint64_t m_offset;      /**< Offset of the token. That is bytes offset to the first character of the
                                      token from the beginning of the file of the very original source code passed
                                      from the user. This value is not unique as after preprocessing all inserted
                                      tokens have the offset of the original position in the replacement list*/
    std::uint64_t
        m_afterPPOffset; /**< Effective offset of the token after preprocessing. Must be equal to m_offset if
                            m_macroId == 0. This value changes during pre processing and is therefore mutable.*/
    std::uint64_t m_charSpaceOffset; /**< Offset to the token in bytes after trigraphs have been replaced and
                                      Backslash Newline pairs have been spliced*/
    std::uint64_t m_charSpaceLength; /**< Length of the token if any trigraphs and Backslash Newline pairs in it's
                                      representation have been removed*/
    std::uint64_t m_fileID = 0;
    TokenType m_tokenType; ///< Type of the token

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
    bool m_isBuiltin;

    Token(std::uint64_t offset, TokenType tokenType, std::uint64_t length, std::uint64_t charSpaceOffset,
          std::uint64_t charSpaceLength, variant value, Type type, const char* builtinRepresentation);

public:
    using ValueType = variant;

    Token(std::uint64_t offset, TokenType tokenType, std::uint64_t length, std::uint64_t charSpaceOffset,
          std::uint64_t charSpaceLength, variant value = std::monostate{}, Type type = Type::None);

    static Token builtinToken(TokenType tokenType, variant value, std::string_view representation);

    ~Token()
    {
        if (m_isBuiltin)
        {
            delete[] m_originalSource;
        }
    }

    Token(const Token& rhs)
    {
        m_value = rhs.m_value;
        m_isBuiltin = rhs.m_isBuiltin;
        if (!m_isBuiltin)
        {
            m_originalSource = rhs.m_originalSource;
        }
        else
        {
            auto* buffer = new char[rhs.m_length];
            std::memcpy(buffer, rhs.m_originalSource, rhs.m_length);
            m_originalSource = buffer;
        }
        m_length = rhs.m_length;
        m_macroId = rhs.m_macroId;
        m_offset = rhs.m_offset;
        m_afterPPOffset = rhs.m_afterPPOffset;
        m_charSpaceOffset = rhs.m_charSpaceOffset;
        m_charSpaceLength = rhs.m_charSpaceLength;
        m_tokenType = rhs.m_tokenType;
        m_type = rhs.m_type;
    }

    Token& operator=(const Token& rhs)
    {
        if (m_isBuiltin)
        {
            delete[] m_originalSource;
        }
        m_value = rhs.m_value;
        m_isBuiltin = rhs.m_isBuiltin;
        if (!m_isBuiltin)
        {
            m_originalSource = rhs.m_originalSource;
        }
        else
        {
            auto* buffer = new char[rhs.m_length];
            std::memcpy(buffer, rhs.m_originalSource, rhs.m_length);
            m_originalSource = buffer;
        }
        m_length = rhs.m_length;
        m_macroId = rhs.m_macroId;
        m_offset = rhs.m_offset;
        m_afterPPOffset = rhs.m_afterPPOffset;
        m_charSpaceOffset = rhs.m_charSpaceOffset;
        m_charSpaceLength = rhs.m_charSpaceLength;
        m_tokenType = rhs.m_tokenType;
        m_type = rhs.m_type;
        return *this;
    }

    Token(Token&& rhs) noexcept
    {
        m_value = std::move(rhs.m_value);
        m_isBuiltin = rhs.m_isBuiltin;
        m_originalSource = std::exchange(rhs.m_originalSource, nullptr);
        m_length = rhs.m_length;
        m_macroId = rhs.m_macroId;
        m_offset = rhs.m_offset;
        m_afterPPOffset = rhs.m_afterPPOffset;
        m_charSpaceOffset = rhs.m_charSpaceOffset;
        m_charSpaceLength = rhs.m_charSpaceLength;
        m_tokenType = rhs.m_tokenType;
        m_type = rhs.m_type;
    }

    Token& operator=(Token&& rhs) noexcept
    {
        if (m_isBuiltin)
        {
            delete[] m_originalSource;
        }
        m_value = std::move(rhs.m_value);
        m_isBuiltin = rhs.m_isBuiltin;
        m_originalSource = std::exchange(rhs.m_originalSource, nullptr);
        m_length = rhs.m_length;
        m_macroId = rhs.m_macroId;
        m_offset = rhs.m_offset;
        m_afterPPOffset = rhs.m_afterPPOffset;
        m_charSpaceOffset = rhs.m_charSpaceOffset;
        m_charSpaceLength = rhs.m_charSpaceLength;
        m_tokenType = rhs.m_tokenType;
        m_type = rhs.m_type;
        return *this;
    }

    [[nodiscard]] TokenType getTokenType() const noexcept
    {
        return m_tokenType;
    }

    [[nodiscard]] const variant& getValue() const noexcept
    {
        return m_value;
    }

    [[nodiscard]] Type getType() const
    {
        return m_type;
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

    [[nodiscard]] std::string_view getRepresentation() const
    {
        CLD_ASSERT(m_originalSource);
        return std::string_view(m_originalSource + m_offset, m_length);
    }

    [[nodiscard]] std::uint64_t getLine(const SourceObject& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getPPLine(const PPSourceObject& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getColumn(const SourceObject& sourceObject) const noexcept;

    [[nodiscard]] std::uint64_t getPPColumn(const PPSourceObject& sourceObject) const noexcept;

    [[nodiscard]] const char* getOriginalSource() const noexcept;

    [[nodiscard]] std::uint64_t getCharSpaceLength() const
    {
        return m_charSpaceLength;
    }

    [[nodiscard]] std::uint64_t getCharSpaceOffset() const
    {
        return m_charSpaceOffset;
    }

    [[nodiscard]] bool isBuiltin() const
    {
        return m_isBuiltin;
    }

    void setOriginalSource(const char* originalSource) noexcept
    {
        CLD_ASSERT(!m_isBuiltin);
        m_originalSource = originalSource;
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

using TokenIterator = std::vector<Token>::const_iterator;

std::string constructPP(const PPSourceObject& sourceObject, TokenIterator begin, TokenIterator end);

std::string constructPPTrimmed(const PPSourceObject& sourceObject, TokenIterator begin, TokenIterator end);
} // namespace Lexer
} // namespace cld

#include "Lexer.hpp"

#include <llvm/ADT/IntervalMap.h>
#include <llvm/ADT/ScopeExit.h>
#include <llvm/Support/ConvertUTF.h>
#include <llvm/Support/Format.h>
#include <llvm/Support/Unicode.h>
#include <llvm/Support/UnicodeCharRanges.h>
#include <llvm/Support/WithColor.h>

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <ctre.hpp>
#include <numeric>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include "ErrorMessages.hpp"
#include "SourceObject.hpp"

using namespace cld::Lexer;

using IntervalMap = llvm::IntervalMap<std::uint64_t, std::pair<std::uint64_t, std::uint64_t>>;

namespace
{
bool isKeyword(const std::string& characters)
{
    return characters == "auto" || characters == "double" || characters == "int" || characters == "struct"
           || characters == "break" || characters == "else" || characters == "long" || characters == "switch"
           || characters == "case" || characters == "enum" || characters == "register" || characters == "typedef"
           || characters == "char" || characters == "extern" || characters == "return" || characters == "union"
           || characters == "const" || characters == "float" || characters == "short" || characters == "unsigned"
           || characters == "continue" || characters == "for" || characters == "signed" || characters == "void"
           || characters == "default" || characters == "goto" || characters == "sizeof" || characters == "volatile"
           || characters == "restrict" || characters == "do" || characters == "if" || characters == "static"
           || characters == "while" || characters == "inline" || characters == "_Bool";
}

TokenType charactersToKeyword(const std::string& characters)
{
    using namespace cld::Lexer;
    if (characters == "auto")
    {
        return TokenType::AutoKeyword;
    }
    if (characters == "double")
    {
        return TokenType::DoubleKeyword;
    }
    if (characters == "int")
    {
        return TokenType::IntKeyword;
    }
    if (characters == "struct")
    {
        return TokenType::StructKeyword;
    }
    if (characters == "break")
    {
        return TokenType::BreakKeyword;
    }
    if (characters == "else")
    {
        return TokenType::ElseKeyword;
    }
    if (characters == "long")
    {
        return TokenType::LongKeyword;
    }
    if (characters == "switch")
    {
        return TokenType::SwitchKeyword;
    }
    if (characters == "case")
    {
        return TokenType::CaseKeyword;
    }
    if (characters == "enum")
    {
        return TokenType::EnumKeyword;
    }
    if (characters == "register")
    {
        return TokenType::RegisterKeyword;
    }
    if (characters == "typedef")
    {
        return TokenType::TypedefKeyword;
    }
    if (characters == "char")
    {
        return TokenType::CharKeyword;
    }
    if (characters == "extern")
    {
        return TokenType::ExternKeyword;
    }
    if (characters == "return")
    {
        return TokenType::ReturnKeyword;
    }
    if (characters == "union")
    {
        return TokenType::UnionKeyword;
    }
    if (characters == "const")
    {
        return TokenType::ConstKeyword;
    }
    if (characters == "float")
    {
        return TokenType::FloatKeyword;
    }
    if (characters == "short")
    {
        return TokenType::ShortKeyword;
    }
    if (characters == "unsigned")
    {
        return TokenType::UnsignedKeyword;
    }
    if (characters == "continue")
    {
        return TokenType::ContinueKeyword;
    }
    if (characters == "for")
    {
        return TokenType::ForKeyword;
    }
    if (characters == "signed")
    {
        return TokenType::SignedKeyword;
    }
    if (characters == "default")
    {
        return TokenType::DefaultKeyword;
    }
    if (characters == "goto")
    {
        return TokenType::GotoKeyword;
    }
    if (characters == "sizeof")
    {
        return TokenType::SizeofKeyword;
    }
    if (characters == "volatile")
    {
        return TokenType::VolatileKeyword;
    }
    if (characters == "do")
    {
        return TokenType::DoKeyword;
    }
    if (characters == "if")
    {
        return TokenType::IfKeyword;
    }
    if (characters == "static")
    {
        return TokenType::StaticKeyword;
    }
    if (characters == "while")
    {
        return TokenType::WhileKeyword;
    }
    if (characters == "void")
    {
        return TokenType::VoidKeyword;
    }
    if (characters == "restrict")
    {
        return TokenType::RestrictKeyword;
    }
    if (characters == "inline")
    {
        return TokenType::InlineKeyword;
    }
    if (characters == "_Bool")
    {
        return TokenType::UnderlineBool;
    }
    CLD_UNREACHABLE;
}

// C99 Annex D
constexpr llvm::sys::UnicodeCharRange C99AllowedIDCharRanges[] = {
    // Latin (1)
    {0x00AA, 0x00AA},

    // Special characters (1)
    {0x00B5, 0x00B5},
    {0x00B7, 0x00B7},

    // Latin (2)
    {0x00BA, 0x00BA},
    {0x00C0, 0x00D6},
    {0x00D8, 0x00F6},
    {0x00F8, 0x01F5},
    {0x01FA, 0x0217},
    {0x0250, 0x02A8},

    // Special characters (2)
    {0x02B0, 0x02B8},
    {0x02BB, 0x02BB},
    {0x02BD, 0x02C1},
    {0x02D0, 0x02D1},
    {0x02E0, 0x02E4},
    {0x037A, 0x037A},

    // Greek (1)
    {0x0386, 0x0386},
    {0x0388, 0x038A},
    {0x038C, 0x038C},
    {0x038E, 0x03A1},
    {0x03A3, 0x03CE},
    {0x03D0, 0x03D6},
    {0x03DA, 0x03DA},
    {0x03DC, 0x03DC},
    {0x03DE, 0x03DE},
    {0x03E0, 0x03E0},
    {0x03E2, 0x03F3},

    // Cyrillic
    {0x0401, 0x040C},
    {0x040E, 0x044F},
    {0x0451, 0x045C},
    {0x045E, 0x0481},
    {0x0490, 0x04C4},
    {0x04C7, 0x04C8},
    {0x04CB, 0x04CC},
    {0x04D0, 0x04EB},
    {0x04EE, 0x04F5},
    {0x04F8, 0x04F9},

    // Armenian (1)
    {0x0531, 0x0556},

    // Special characters (3)
    {0x0559, 0x0559},

    // Armenian (2)
    {0x0561, 0x0587},

    // Hebrew
    {0x05B0, 0x05B9},
    {0x05BB, 0x05BD},
    {0x05BF, 0x05BF},
    {0x05C1, 0x05C2},
    {0x05D0, 0x05EA},
    {0x05F0, 0x05F2},

    // Arabic (1)
    {0x0621, 0x063A},
    {0x0640, 0x0652},

    // Digits (1)
    {0x0660, 0x0669},

    // Arabic (2)
    {0x0670, 0x06B7},
    {0x06BA, 0x06BE},
    {0x06C0, 0x06CE},
    {0x06D0, 0x06DC},
    {0x06E5, 0x06E8},
    {0x06EA, 0x06ED},

    // Digits (2)
    {0x06F0, 0x06F9},

    // Devanagari and Special character 0x093D.
    {0x0901, 0x0903},
    {0x0905, 0x0939},
    {0x093D, 0x094D},
    {0x0950, 0x0952},
    {0x0958, 0x0963},

    // Digits (3)
    {0x0966, 0x096F},

    // Bengali (1)
    {0x0981, 0x0983},
    {0x0985, 0x098C},
    {0x098F, 0x0990},
    {0x0993, 0x09A8},
    {0x09AA, 0x09B0},
    {0x09B2, 0x09B2},
    {0x09B6, 0x09B9},
    {0x09BE, 0x09C4},
    {0x09C7, 0x09C8},
    {0x09CB, 0x09CD},
    {0x09DC, 0x09DD},
    {0x09DF, 0x09E3},

    // Digits (4)
    {0x09E6, 0x09EF},

    // Bengali (2)
    {0x09F0, 0x09F1},

    // Gurmukhi (1)
    {0x0A02, 0x0A02},
    {0x0A05, 0x0A0A},
    {0x0A0F, 0x0A10},
    {0x0A13, 0x0A28},
    {0x0A2A, 0x0A30},
    {0x0A32, 0x0A33},
    {0x0A35, 0x0A36},
    {0x0A38, 0x0A39},
    {0x0A3E, 0x0A42},
    {0x0A47, 0x0A48},
    {0x0A4B, 0x0A4D},
    {0x0A59, 0x0A5C},
    {0x0A5E, 0x0A5E},

    // Digits (5)
    {0x0A66, 0x0A6F},

    // Gurmukhi (2)
    {0x0A74, 0x0A74},

    // Gujarti
    {0x0A81, 0x0A83},
    {0x0A85, 0x0A8B},
    {0x0A8D, 0x0A8D},
    {0x0A8F, 0x0A91},
    {0x0A93, 0x0AA8},
    {0x0AAA, 0x0AB0},
    {0x0AB2, 0x0AB3},
    {0x0AB5, 0x0AB9},
    {0x0ABD, 0x0AC5},
    {0x0AC7, 0x0AC9},
    {0x0ACB, 0x0ACD},
    {0x0AD0, 0x0AD0},
    {0x0AE0, 0x0AE0},

    // Digits (6)
    {0x0AE6, 0x0AEF},

    // Oriya and Special character 0x0B3D
    {0x0B01, 0x0B03},
    {0x0B05, 0x0B0C},
    {0x0B0F, 0x0B10},
    {0x0B13, 0x0B28},
    {0x0B2A, 0x0B30},
    {0x0B32, 0x0B33},
    {0x0B36, 0x0B39},
    {0x0B3D, 0x0B43},
    {0x0B47, 0x0B48},
    {0x0B4B, 0x0B4D},
    {0x0B5C, 0x0B5D},
    {0x0B5F, 0x0B61},

    // Digits (7)
    {0x0B66, 0x0B6F},

    // Tamil
    {0x0B82, 0x0B83},
    {0x0B85, 0x0B8A},
    {0x0B8E, 0x0B90},
    {0x0B92, 0x0B95},
    {0x0B99, 0x0B9A},
    {0x0B9C, 0x0B9C},
    {0x0B9E, 0x0B9F},
    {0x0BA3, 0x0BA4},
    {0x0BA8, 0x0BAA},
    {0x0BAE, 0x0BB5},
    {0x0BB7, 0x0BB9},
    {0x0BBE, 0x0BC2},
    {0x0BC6, 0x0BC8},
    {0x0BCA, 0x0BCD},

    // Digits (8)
    {0x0BE7, 0x0BEF},

    // Telugu
    {0x0C01, 0x0C03},
    {0x0C05, 0x0C0C},
    {0x0C0E, 0x0C10},
    {0x0C12, 0x0C28},
    {0x0C2A, 0x0C33},
    {0x0C35, 0x0C39},
    {0x0C3E, 0x0C44},
    {0x0C46, 0x0C48},
    {0x0C4A, 0x0C4D},
    {0x0C60, 0x0C61},

    // Digits (9)
    {0x0C66, 0x0C6F},

    // Kannada
    {0x0C82, 0x0C83},
    {0x0C85, 0x0C8C},
    {0x0C8E, 0x0C90},
    {0x0C92, 0x0CA8},
    {0x0CAA, 0x0CB3},
    {0x0CB5, 0x0CB9},
    {0x0CBE, 0x0CC4},
    {0x0CC6, 0x0CC8},
    {0x0CCA, 0x0CCD},
    {0x0CDE, 0x0CDE},
    {0x0CE0, 0x0CE1},

    // Digits (10)
    {0x0CE6, 0x0CEF},

    // Malayam
    {0x0D02, 0x0D03},
    {0x0D05, 0x0D0C},
    {0x0D0E, 0x0D10},
    {0x0D12, 0x0D28},
    {0x0D2A, 0x0D39},
    {0x0D3E, 0x0D43},
    {0x0D46, 0x0D48},
    {0x0D4A, 0x0D4D},
    {0x0D60, 0x0D61},

    // Digits (11)
    {0x0D66, 0x0D6F},

    // Thai...including Digits { 0x0E50, 0x0E59 }
    {0x0E01, 0x0E3A},
    {0x0E40, 0x0E5B},

    // Lao (1)
    {0x0E81, 0x0E82},
    {0x0E84, 0x0E84},
    {0x0E87, 0x0E88},
    {0x0E8A, 0x0E8A},
    {0x0E8D, 0x0E8D},
    {0x0E94, 0x0E97},
    {0x0E99, 0x0E9F},
    {0x0EA1, 0x0EA3},
    {0x0EA5, 0x0EA5},
    {0x0EA7, 0x0EA7},
    {0x0EAA, 0x0EAB},
    {0x0EAD, 0x0EAE},
    {0x0EB0, 0x0EB9},
    {0x0EBB, 0x0EBD},
    {0x0EC0, 0x0EC4},
    {0x0EC6, 0x0EC6},
    {0x0EC8, 0x0ECD},

    // Digits (12)
    {0x0ED0, 0x0ED9},

    // Lao (2)
    {0x0EDC, 0x0EDD},

    // Tibetan (1)
    {0x0F00, 0x0F00},
    {0x0F18, 0x0F19},

    // Digits (13)
    {0x0F20, 0x0F33},

    // Tibetan (2)
    {0x0F35, 0x0F35},
    {0x0F37, 0x0F37},
    {0x0F39, 0x0F39},
    {0x0F3E, 0x0F47},
    {0x0F49, 0x0F69},
    {0x0F71, 0x0F84},
    {0x0F86, 0x0F8B},
    {0x0F90, 0x0F95},
    {0x0F97, 0x0F97},
    {0x0F99, 0x0FAD},
    {0x0FB1, 0x0FB7},
    {0x0FB9, 0x0FB9},

    // Georgian
    {0x10A0, 0x10C5},
    {0x10D0, 0x10F6},

    // Latin (3)
    {0x1E00, 0x1E9B},
    {0x1EA0, 0x1EF9},

    // Greek (2)
    {0x1F00, 0x1F15},
    {0x1F18, 0x1F1D},
    {0x1F20, 0x1F45},
    {0x1F48, 0x1F4D},
    {0x1F50, 0x1F57},
    {0x1F59, 0x1F59},
    {0x1F5B, 0x1F5B},
    {0x1F5D, 0x1F5D},
    {0x1F5F, 0x1F7D},
    {0x1F80, 0x1FB4},
    {0x1FB6, 0x1FBC},

    // Special characters (4)
    {0x1FBE, 0x1FBE},

    // Greek (3)
    {0x1FC2, 0x1FC4},
    {0x1FC6, 0x1FCC},
    {0x1FD0, 0x1FD3},
    {0x1FD6, 0x1FDB},
    {0x1FE0, 0x1FEC},
    {0x1FF2, 0x1FF4},
    {0x1FF6, 0x1FFC},

    // Special characters (5)
    {0x203F, 0x2040},

    // Latin (4)
    {0x207F, 0x207F},

    // Special characters (6)
    {0x2102, 0x2102},
    {0x2107, 0x2107},
    {0x210A, 0x2113},
    {0x2115, 0x2115},
    {0x2118, 0x211D},
    {0x2124, 0x2124},
    {0x2126, 0x2126},
    {0x2128, 0x2128},
    {0x212A, 0x2131},
    {0x2133, 0x2138},
    {0x2160, 0x2182},
    {0x3005, 0x3007},
    {0x3021, 0x3029},

    // Hiragana
    {0x3041, 0x3093},
    {0x309B, 0x309C},

    // Katakana
    {0x30A1, 0x30F6},
    {0x30FB, 0x30FC},

    // Bopmofo [sic]
    {0x3105, 0x312C},

    // CJK Unified Ideographs
    {0x4E00, 0x9FA5},

    // Hangul,
    {0xAC00, 0xD7A3}};

// C99 6.4.2.1p3: The initial character [of an identifier] shall not be a
// universal character name designating a digit.
// C99 Annex D defines these characters as "Digits".
constexpr llvm::sys::UnicodeCharRange C99DisallowedInitialIDCharRanges[] = {
    {0x0660, 0x0669}, {0x06F0, 0x06F9}, {0x0966, 0x096F}, {0x09E6, 0x09EF}, {0x0A66, 0x0A6F},
    {0x0AE6, 0x0AEF}, {0x0B66, 0x0B6F}, {0x0BE7, 0x0BEF}, {0x0C66, 0x0C6F}, {0x0CE6, 0x0CEF},
    {0x0D66, 0x0D6F}, {0x0E50, 0x0E59}, {0x0ED0, 0x0ED9}, {0x0F20, 0x0F33}};

constexpr llvm::sys::UnicodeCharRange UnicodeWhitespaceCharRanges[] = {
    {0x0085, 0x0085}, {0x00A0, 0x00A0}, {0x1680, 0x1680}, {0x180E, 0x180E}, {0x2000, 0x200A},
    {0x2028, 0x2029}, {0x202F, 0x202F}, {0x205F, 0x205F}, {0x3000, 0x3000}};

template <class Iter>
std::uint8_t getNumBytesForUTF8(Iter begin, Iter end)
{
    CLD_ASSERT(begin != end);
    std::uint8_t des;
    std::int8_t temp = *begin;
    std::memcpy(&des, &temp, 1);
    if (des >= 0b11111000)
    {
        return 1;
    }
    else
    {
        auto step = llvm::getNumBytesForUTF8(*begin);
        for (std::uint8_t i = 1; i < step; i++)
        {
            if (begin + i == end || (*(begin + i) & 0b11000000) != 0b10000000)
            {
                return i;
            }
        }
        return step;
    }
}

std::uint8_t getNumUTF8ForUTF32(std::uint32_t c)
{
    if (c <= 0x7F)
    {
        return 1;
    }
    if (c <= 0x7FF)
    {
        return 2;
    }
    if (c <= 0xFFFF)
    {
        return 3;
    }
    return 4;
}

struct Start;
struct CharacterLiteral;
struct StringLiteral;
struct Text;
struct PreprocessingNumber;
struct MaybeUC;
struct UniversalCharacter;
struct Number;
struct Dot;
struct Punctuation;
struct LineComment;
struct BlockComment;
struct AfterInclude;
struct L;

using StateMachine =
    std::variant<Start, CharacterLiteral, StringLiteral, Text, PreprocessingNumber, MaybeUC, UniversalCharacter,
                 LineComment, BlockComment, Number, Dot, Punctuation, AfterInclude, L>;

class Context
{
    cld::LanguageOptions m_languageOptions;
    bool m_inPreprocessor;
    llvm::raw_ostream* m_reporter;
    std::vector<Token> m_result;
    std::string_view m_sourceSpace;
    std::string_view m_characterSpace;
    std::uint64_t& m_offset;
    std::vector<std::uint64_t> m_lineStarts;
    const IntervalMap& m_characterToSourceSpace;
    bool m_errorsOccured = false;
    struct ThingsToCache
    {
        std::vector<std::int64_t> mappingAdded;
        std::string line;
        std::int64_t delta;
        std::vector<std::uint64_t> deletedArrows;
    };
    mutable std::unordered_map<std::uint64_t, ThingsToCache> m_lineCache;

    std::uint64_t getLineNumber(std::uint64_t offset) const noexcept
    {
        auto result = std::lower_bound(m_lineStarts.begin(), m_lineStarts.end(), offset);
        CLD_ASSERT(result != m_lineStarts.end());
        return result == m_lineStarts.begin() ?
                   1 :
                   std::distance(m_lineStarts.begin(), *result != offset ? result - 1 : result) + 1;
    }

    std::uint64_t getLineStartOffset(std::uint64_t line) const noexcept
    {
        CLD_ASSERT(line - 1 < m_lineStarts.size());
        return m_lineStarts[line - 1];
    }

    std::uint64_t getLineEndOffset(std::uint64_t line) const noexcept
    {
        CLD_ASSERT(line - 1 < m_lineStarts.size());
        return line == m_lineStarts.size() ? m_sourceSpace.size() : m_lineStarts[line];
    }

    std::string toSafeUTF8(std::string_view inputLine, std::uint64_t lineStart, std::vector<std::int64_t>& deltas,
                           std::vector<std::uint64_t>& arrows, std::vector<std::int64_t>& mapping) const
    {
        std::vector<std::uint64_t> wereDeleted;
        std::string line;
        mapping.resize(lineStart);
        if (auto cacheEntry = m_lineCache.find(lineStart); cacheEntry == m_lineCache.end())
        {
            deltas.emplace_back();
            auto prevSize = mapping.size();
            auto sourceStart = inputLine.data();
            const auto sourceEnd = inputLine.data() + inputLine.size();
            std::vector<llvm::UTF32> utf32;
            utf32.reserve(inputLine.size());
            while (sourceStart != sourceEnd)
            {
                llvm::ConversionResult result;
                do
                {
                    const std::uint64_t currentOldOffset = lineStart + sourceStart - inputLine.data();
                    utf32.emplace_back();
                    const auto prev = sourceStart;
                    result = llvm::convertUTF8Sequence(reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                                       reinterpret_cast<const llvm::UTF8*>(sourceEnd), &utf32.back(),
                                                       llvm::strictConversion);
                    if (result == llvm::conversionOK)
                    {
                        mapping.resize(currentOldOffset + std::distance(prev, sourceStart), deltas.back());

                        if (!llvm::sys::unicode::isPrintable(utf32.back()))
                        {
                            const auto step = getNumUTF8ForUTF32(utf32.back());
                            if (step == 4)
                            {
                                wereDeleted.push_back(currentOldOffset + 3);
                            }
                            deltas.back() += 3 - step;
                            utf32.back() = utf32.back() <= 0x1F ? 0x2400 + utf32.back() : 0xFFFD;
                        }
                    }
                    else
                    {
                        utf32.pop_back();
                    }
                } while (result == llvm::conversionOK && sourceStart != sourceEnd);

                if (result != llvm::conversionOK)
                {
                    const uint64_t currentOldOffset = lineStart + sourceStart - inputLine.data();
                    utf32.push_back(0xFFFD);
                    if (sourceStart != sourceEnd)
                    {
                        const auto step = getNumBytesForUTF8(sourceStart, sourceEnd);
                        mapping.resize(currentOldOffset + step, deltas.back());
                        if (step == 4)
                        {
                            wereDeleted.push_back(currentOldOffset + 3);
                        }
                        deltas.back() += 3 - step;
                        sourceStart += step;
                    }
                }
            }

            const std::uint64_t oldEndOffset = lineStart + sourceStart - inputLine.data();
            mapping.resize(oldEndOffset + 1, deltas.back());
            line.resize(4 * utf32.size(), '\0');
            const auto* targetStart = utf32.data();
            auto lineMemStart = line.data() + line.size() - 4 * utf32.size();
            llvm::ConvertUTF32toUTF8(reinterpret_cast<const llvm::UTF32**>(&targetStart), utf32.data() + utf32.size(),
                                     reinterpret_cast<llvm::UTF8**>(&lineMemStart),
                                     reinterpret_cast<llvm::UTF8*>(line.data() + line.size()), llvm::strictConversion);
            line.resize(line.size() - std::distance(lineMemStart, line.data() + line.size()));

            m_lineCache.insert(
                {lineStart,
                 {std::vector(mapping.begin() + prevSize, mapping.end()), line, deltas.back(), wereDeleted}});
        }
        else
        {
            line = cacheEntry->second.line;
            deltas.push_back(cacheEntry->second.delta);
            mapping.insert(mapping.end(), cacheEntry->second.mappingAdded.begin(),
                           cacheEntry->second.mappingAdded.end());
            wereDeleted = cacheEntry->second.deletedArrows;
        }

        arrows.erase(std::remove_if(arrows.begin(), arrows.end(),
                                    [&wereDeleted](std::uint64_t offset) {
                                        return std::any_of(
                                            wereDeleted.begin(), wereDeleted.end(),
                                            [offset](std::uint64_t forbidden) { return forbidden == offset; });
                                    }),
                     arrows.end());
        return line;
    }

    std::vector<std::vector<std::uint64_t>>
        transformToPrintSpace(std::vector<std::uint64_t>&& arrows, std::uint64_t startLine, std::uint64_t endLine,
                              std::vector<std::int64_t>& deltas, std::vector<std::string>& lines,
                              std::uint64_t& underlineStart, std::uint64_t& underlineEnd) const
    {
        std::vector<std::vector<std::uint64_t>> columnsOfArrowsForLine(endLine - startLine + 1);
        lines.reserve(columnsOfArrowsForLine.size());
        deltas.reserve(columnsOfArrowsForLine.size());

        std::vector<std::int64_t> mapping;
        for (auto i = startLine; i <= endLine; i++)
        {
            const auto start = getLineStartOffset(i);
            const auto end = getLineEndOffset(i);
            const auto stringView = m_sourceSpace.substr(start, end - start - 1);

            lines.push_back(toSafeUTF8(stringView, start, deltas, arrows, mapping));
        }

        underlineStart += mapping[underlineStart];
        underlineEnd += mapping[underlineEnd];

        if (!arrows.empty())
        {
            for (auto i = startLine; i <= endLine; i++)
            {
                const auto begin = getLineStartOffset(i);
                const auto end = getLineEndOffset(i) - 1 + deltas[i - startLine];
                // For every arrow, transform the byte offset into a character width offset, then check if the byte
                // offset points to one of the bytes of a code point and make it point at the full width of it
                std::vector<std::vector<uint64_t>> replacements;
                std::uint64_t currentWidth = 0;
                for (auto j = begin; j != end;)
                {
                    const std::uint8_t step = getNumBytesForUTF8(lines[i - startLine].begin() + (j - begin),
                                                                 lines[i - startLine].begin() + (end - begin));
                    const auto size =
                        llvm::sys::unicode::columnWidthUTF8({lines[i - startLine].data() + j - begin, step});
                    std::vector<std::uint64_t> columns(size);
                    std::iota(columns.begin(), columns.end(), currentWidth);
                    replacements.resize(replacements.size() + step, columns);
                    currentWidth += size;
                    j += step;
                }

                for (auto iter : arrows)
                {
                    if (iter < begin || iter >= end - deltas[i - startLine])
                    {
                        continue;
                    }
                    CLD_ASSERT(iter + mapping[iter] - begin < replacements.size());
                    const auto& vector = replacements[iter + mapping[iter] - begin];
                    columnsOfArrowsForLine[i - startLine].insert(columnsOfArrowsForLine[i - startLine].end(),
                                                                 vector.begin(), vector.end());
                }
            }
        }
        return columnsOfArrowsForLine;
    }

    void report(const std::string& prefix, llvm::raw_ostream::Colors colour, const std::string& message,
                std::uint64_t location, std::vector<std::uint64_t>&& arrows, std::uint64_t underlineStart,
                std::uint64_t underlineEnd) const
    {
        if (!m_reporter)
        {
            return;
        }

        std::sort(arrows.begin(), arrows.end());
        arrows.erase(std::unique(arrows.begin(), arrows.end()), arrows.end());
        location = map(location).first;
        underlineStart = map(underlineStart).first;
        underlineEnd = map(underlineEnd - 1).second;
        {
            std::vector<std::uint64_t> result;
            for (auto iter = arrows.cbegin(); iter != arrows.cend(); iter++)
            {
                auto [lower, upper] = map(*iter);
                if (iter != arrows.cbegin() && *iter - 1 == *(iter - 1) && !result.empty()
                    && result.back() + 1 != lower)
                {
                    // A whole range is supposed to be under an arrow but because some characters in-between don't exist
                    // in character space (like a backslash newline pair) we add them here so it's prettier!
                    auto skipped = m_sourceSpace.substr(result.back() + 1, lower - result.back() - 1);
                    if (skipped == "\\\n" || skipped == "?\?/\n")
                    {
                        auto back = result.back();
                        result.resize(result.size() + skipped.size());
                        std::iota(result.begin() + result.size() - skipped.size(), result.end(), back + 1);
                    }
                }
                result.resize(result.size() + upper - lower);
                std::iota(result.begin() + result.size() - (upper - lower), result.end(), lower);
            }
            arrows = std::move(result);
        }

        const auto startLine = getLineNumber(underlineStart);
        const auto endLine = getLineNumber(underlineEnd);
        std::vector<std::int64_t> deltas;
        std::vector<std::string> lines;

        const auto lineNumber = getLineNumber(location);
        *m_reporter << lineNumber << ':' << location - getLineStartOffset(lineNumber) << ": ";
        llvm::WithColor(*m_reporter, colour, true) << prefix << ": ";
        llvm::WithColor(*m_reporter, llvm::raw_ostream::SAVEDCOLOR, true) << message << '\n';
        std::size_t numSize = 0;

        std::vector<std::vector<std::uint64_t>> columnsOfArrowsForLine =
            transformToPrintSpace(std::move(arrows), startLine, endLine, deltas, lines, underlineStart, underlineEnd);
        numSize = std::to_string(endLine).size();
        const auto remainder = numSize % 4;
        if (remainder)
        {
            numSize += 4 - remainder;
        }

        for (auto i = startLine; i <= endLine; i++)
        {
            // Text
            *m_reporter << llvm::format_decimal(i, static_cast<unsigned>(numSize)) << " | ";

            auto string = lines[i - startLine];
            const auto& arrowsForLine = columnsOfArrowsForLine[i - startLine];
            if (i != startLine && i != endLine)
            {
                // The token spans multiple lines and we are neither at the first nor last line. Therefore
                // this line consist only of the token
                llvm::WithColor(*m_reporter, colour).get() << string;
            }
            else if (i == startLine && i == endLine)
            {
                // The token does not span lines and starts as well as ends here
                auto column = underlineStart - getLineStartOffset(i);
                *m_reporter << string.substr(0, column);
                llvm::WithColor(*m_reporter, colour).get() << string.substr(column, underlineEnd - underlineStart);
                *m_reporter << string.substr(column + underlineEnd - underlineStart);
            }
            else if (i == startLine)
            {
                // The token starts here and does not end here
                auto column = underlineStart - getLineStartOffset(i);
                *m_reporter << string.substr(0, column);
                llvm::WithColor(*m_reporter, colour).get() << string.substr(column);
            }
            else
            {
                // The token ends here and did not start here
                auto endColumn = underlineEnd - getLineStartOffset(i);
                llvm::WithColor(*m_reporter, colour).get() << string.substr(0, endColumn);
                *m_reporter << string.substr(endColumn);
            }
            *m_reporter << '\n';

            // Underline + Arrows
            m_reporter->indent(static_cast<unsigned>(numSize)) << " | ";
            if (i != startLine && i != endLine)
            {
                // The token spans multiple lines and we are neither at the first nor last line. Therefore
                // this line consist only of the token
                auto underline = cld::stringOfSameWidth(string, '~');
                for (auto iter : arrowsForLine)
                {
                    CLD_ASSERT(iter < underline.size());
                    underline[iter] = '^';
                }
                llvm::WithColor(*m_reporter, colour) << underline;
            }
            else if (i == startLine && i == endLine)
            {
                // The token does not span lines and starts as well as ends here
                auto column = underlineStart - getLineStartOffset(i);
                auto space = cld::stringOfSameWidth(string.substr(0, column), ' ');
                *m_reporter << space;
                auto underline = cld::stringOfSameWidth(string.substr(column, underlineEnd - underlineStart), '~');
                for (auto iter : arrowsForLine)
                {
                    CLD_ASSERT(iter - space.size() < underline.size());
                    underline[iter - space.size()] = '^';
                }
                llvm::WithColor(*m_reporter, colour) << underline;
            }
            else if (i == startLine)
            {
                // The token starts here and does not end here
                auto column = underlineStart - getLineStartOffset(i);
                auto space = cld::stringOfSameWidth(string.substr(0, column), ' ');
                *m_reporter << space;
                auto underline = cld::stringOfSameWidth(string.substr(column), '~');
                for (auto iter : arrowsForLine)
                {
                    CLD_ASSERT(iter - space.size() < underline.size());
                    underline[iter - space.size()] = '^';
                }
                llvm::WithColor(*m_reporter, colour) << underline;
            }
            else
            {
                // The token ends here and did not start here
                auto endColumn = underlineEnd - getLineStartOffset(i);
                auto underline = cld::stringOfSameWidth(string.substr(0, endColumn), '~');
                for (auto iter : arrowsForLine)
                {
                    CLD_ASSERT(iter < underline.size());
                    underline[iter] = '^';
                }
                llvm::WithColor(*m_reporter, colour) << underline;
            }
            *m_reporter << '\n';
        }
        m_reporter->flush();
    }

    std::pair<std::uint64_t, std::uint64_t> map(std::uint64_t offset) const
    {
        auto actualIter = m_characterToSourceSpace.find(offset);
        CLD_ASSERT(actualIter != m_characterToSourceSpace.end());
        auto denominator = actualIter.stop() - actualIter.start() + 1;
        CLD_ASSERT(denominator != 0);
        auto nominatorRange = actualIter.value().second - actualIter.value().first;
        auto lowBound = (offset - actualIter.start()) * nominatorRange / denominator + actualIter.value().first;
        auto highBound = (offset + 1 - actualIter.start()) * nominatorRange / denominator + actualIter.value().first;
        return {lowBound, highBound};
    }

public:
    std::uint64_t tokenStartOffset;

    Context(const std::string& sourceSpace, const IntervalMap& characterToSourceSpace,
            const std::string& characterSpace, std::uint64_t& offset, std::vector<std::uint64_t> lineStarts,
            cld::LanguageOptions languageOptions, bool inPreprocessor, llvm::raw_ostream* reporter) noexcept
        : m_languageOptions(languageOptions),
          m_inPreprocessor(inPreprocessor),
          m_reporter(reporter),
          m_sourceSpace(sourceSpace),
          m_characterSpace(characterSpace),
          m_offset(offset),
          m_lineStarts(std::move(lineStarts)),
          m_characterToSourceSpace(characterToSourceSpace)
    {
    }

    void reportError(const std::string& message, std::uint64_t location, std::vector<std::uint64_t> arrows = {})
    {
        m_errorsOccured = true;
        report("error", llvm::raw_ostream::RED, message, location, std::move(arrows), tokenStartOffset, m_offset);
    }

    void reportNote(const std::string& message, std::uint64_t location, std::vector<std::uint64_t> arrows = {})
    {
        report("note", llvm::raw_ostream::CYAN, message, location, std::move(arrows), tokenStartOffset, m_offset);
    }

    void reportWarning(const std::string& message, std::uint64_t location, std::vector<std::uint64_t> arrows = {})
    {
        report("warning", llvm::raw_ostream::MAGENTA, message, location, std::move(arrows), tokenStartOffset, m_offset);
    }

    void reportError(const std::string& message, std::uint64_t location, std::uint64_t start, std::uint64_t end,
                     std::vector<std::uint64_t> arrows = {})
    {
        m_errorsOccured = true;
        report("error", llvm::raw_ostream::RED, message, location, std::move(arrows), start, end);
    }

    void reportNote(const std::string& message, std::uint64_t location, std::uint64_t start, std::uint64_t end,
                    std::vector<std::uint64_t> arrows = {})
    {
        report("note", llvm::raw_ostream::CYAN, message, location, std::move(arrows), start, end);
    }

    void reportWarning(const std::string& message, std::uint64_t location, std::uint64_t start, std::uint64_t end,
                       std::vector<std::uint64_t> arrows = {})
    {
        report("warning", llvm::raw_ostream::MAGENTA, message, location, std::move(arrows), start, end);
    }

    [[nodiscard]] std::uint64_t getOffset() const
    {
        return m_offset;
    }

    template <class F>
    void withOffset(std::uint64_t offset, F&& f)
    {
        auto exit = llvm::make_scope_exit([offset = m_offset, this]() { m_offset = offset; });
        m_offset = offset;
        std::forward<F>(f)();
    }

    [[nodiscard]] cld::LanguageOptions getLanguageOptions() const
    {
        return m_languageOptions;
    }

    [[nodiscard]] const std::vector<cld::Lexer::Token>& getResult() const& noexcept
    {
        return m_result;
    }

    [[nodiscard]] std::vector<cld::Lexer::Token> getResult() && noexcept
    {
        return m_result;
    }

    bool isInPreprocessor() const
    {
        return m_inPreprocessor;
    }

    void push(std::uint64_t start, std::uint64_t end, TokenType tokenType, Token::ValueType value = {},
              Token::Type type = Token::Type::None)
    {
        start = map(start).first;
        end = map(end - 1).second;

        auto view = m_sourceSpace.substr(start, end - start);
        m_result.emplace_back(start, tokenType, std::string(view.begin(), view.end()), std::move(value), type);
    }

    void push(TokenType tokenType, Token::ValueType value = {}, Token::Type type = Token::Type::None)
    {
        push(tokenStartOffset, m_offset, tokenType, std::move(value), type);
    }

    void push(std::uint64_t diff, TokenType tokenType, Token::ValueType value = {},
              Token::Type type = Token::Type::None)
    {
        push(tokenStartOffset, m_offset - diff, tokenType, std::move(value), type);
    }

    [[nodiscard]] std::string_view view(std::uint64_t startOffset, std::uint64_t endOffset) const
    {
        return m_characterSpace.substr(startOffset, endOffset - startOffset);
    }

    [[nodiscard]] std::string_view currentView() const
    {
        return view(tokenStartOffset, m_offset);
    }

    bool isErrorsOccured() const
    {
        return m_errorsOccured;
    }
};

/**
 * Callee responsible for right format
 * @param value string that either consist of 4 or 8 hex digits
 * @return Unicode value
 */
std::optional<std::uint32_t> universalCharacterToValue(std::string_view value, std::uint64_t startOffset,
                                                       std::uint64_t endOffset, Context& context)
{
    CLD_ASSERT(value.size() == 4 || value.size() == 8);
    auto result = std::stoul(std::string{value.begin(), value.end()}, nullptr, 16);
    if (result < 0xA0)
    {
        if (result != '$' && result != '@' && result != '`')
        {
            if (!context.isInPreprocessor())
            {
                std::vector<std::uint64_t> arrows(value.size());
                std::iota(arrows.begin(), arrows.end(), endOffset - value.size());
                context.reportError(cld::ErrorMessages::Lexer::INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                        value, cld::ErrorMessages::Lexer::VALUE_MUSTNT_BE_LESS_THAN_A0),
                                    endOffset - value.size(), startOffset, endOffset, std::move(arrows));
            }
            return {};
        }
    }
    else if (result >= 0xD800 && result <= 0xDFFF)
    {
        if (!context.isInPreprocessor())
        {
            std::vector<std::uint64_t> arrows(value.size());
            std::iota(arrows.begin(), arrows.end(), endOffset - value.size());
            context.reportError(cld::ErrorMessages::Lexer::INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                    value, cld::ErrorMessages::Lexer::VALUE_MUSTNT_BE_IN_RANGE),
                                endOffset - value.size(), startOffset, endOffset, std::move(arrows));
        }
        return {};
    }
    return result;
}

std::uint32_t octalToValue(std::string_view value)
{
    return std::stoul(std::string(value.begin(), value.end()), nullptr, 8);
}

std::uint32_t hexToValue(std::string_view value)
{
    return std::stoul(std::string(value.begin(), value.end()), nullptr, 16);
}

std::optional<std::uint32_t> escapeCharToValue(char escape, std::uint64_t backslash, Context& context)
{
    switch (escape)
    {
        case '\'': return '\'';
        case '"': return '"';
        case 'b': return '\b';
        case 't': return '\t';
        case '\\': return '\\';
        case 'f': return '\f';
        case 'v': return '\v';
        case '?': return '\?';
        case 'n': return '\n';
        case 'a': return '\a';
        case 'r': return '\r';
        case ' ':
        {
            context.reportError(cld::ErrorMessages::Lexer::EXPECTED_CHARACTER_AFTER_BACKSLASH, backslash + 1,
                                {backslash, backslash + 1});
            return {};
        }
        default:
        {
            context.reportError(cld::ErrorMessages::Lexer::INVALID_ESCAPE_SEQUENCE_N.args(std::string("\\") + escape),
                                backslash + 1, {backslash, backslash + 1});
            return {};
        }
    }
}

std::pair<std::vector<llvm::UTF32>, bool> processCharacters(const std::string& characters, Context& context, bool wide,
                                                            const char* literalType)
{
    std::uint32_t largestCharacter = [&context, wide]() -> std::uint32_t {
        return wide ? 0xFFFFFFFFu >> (32 - 8 * context.getLanguageOptions().getSizeOfWChar()) : 0x7F;
    }();
    std::vector<llvm::UTF32> result;
    result.resize(characters.size());
    auto* resultStart = result.data();
    auto* resultEnd = result.data() + result.size();

    const auto* end = characters.data() + characters.size();
    bool errorOccured = false;
    for (const auto* iter = characters.data(); iter != end;)
    {
        auto offset = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data());
        if (*iter == '\n')
        {
            context.reportError(cld::ErrorMessages::Lexer::NEWLINE_IN_N_USE_BACKLASH_N.args(literalType),
                                context.tokenStartOffset, {offset});
            iter++;
            errorOccured = true;
            continue;
        }
        if (*iter != '\\')
        {
            const auto* start = iter;
            iter = std::find_if(iter, end, [](char c) { return c == '\\' || c == '\n'; });

            auto res = llvm::ConvertUTF8toUTF32(reinterpret_cast<const llvm::UTF8**>(&start),
                                                reinterpret_cast<const llvm::UTF8*>(iter), &resultStart, resultEnd,
                                                llvm::strictConversion);
            if (res != llvm::conversionOK)
            {
                context.reportError(cld::ErrorMessages::Lexer::INVALID_UTF8_SEQUENCE, context.tokenStartOffset,
                                    {context.tokenStartOffset + (wide ? 2 : 1) + (start - characters.data())});
                errorOccured = true;
            }
            continue;
        }
        // We can assume that if *iter == '\\' that iter + 1 != end. That is because if *iter == '\\' and
        // iter + 1 == end the last character would be '\\' and following that '\'' or '\"'.
        // Therefore the character literal wouldn't have ended and we wouldn't be here.
        if (iter[1] == 'u' || iter[1] == 'U')
        {
            bool big = iter[1] == 'U';
            iter += 2;
            if (iter == end
                || (!(*iter >= '0' || *iter <= '9') && !(*iter >= 'a' && *iter <= 'f')
                    && !(*iter >= 'A' && *iter <= 'F')))
            {
                // First character followed after \u or \U is not a hex digit or its the end of string
                // Let's assume the user thought \u might be an escape character
                auto start = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.reportError(cld::ErrorMessages::Lexer::INVALID_ESCAPE_SEQUENCE_N.args(big ? "\\U" : "\\u"),
                                    start, {start, start + 1});
                errorOccured = true;
                continue;
            }
            else
            {
                auto hexStart = iter;
                auto hexEnd = std::find_if(
                    hexStart, hexStart + std::min<std::size_t>(std::distance(hexStart, end), big ? 8 : 4), [](char c) {
                        return !(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F');
                    });
                if (std::distance(hexStart, hexEnd) != (big ? 8 : 4))
                {
                    auto start = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data() - 2);
                    std::vector<std::uint64_t> arrows = {start, start + 1};
                    arrows.resize(2 + std::distance(hexStart, hexEnd));
                    std::iota(arrows.begin() + 2, arrows.end(), start + 2);
                    context.reportError(
                        cld::ErrorMessages::Lexer::INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS.args(
                            std::to_string((big ? 8 : 4) - std::distance(hexStart, hexEnd))),
                        start, std::move(arrows));
                    errorOccured = true;
                    iter = hexEnd;
                    continue;
                }
                auto uc = universalCharacterToValue({hexStart, static_cast<std::size_t>(hexEnd - hexStart)}, offset,
                                                    offset + 2 + (big ? 8 : 4), context);
                if (uc)
                {
                    *resultStart = *uc;
                    resultStart++;
                }
                else
                {
                    errorOccured = true;
                }
                iter = hexEnd;
                continue;
            }
        }
        else if (iter[1] == 'x')
        {
            iter += 2;
            auto lastHex = std::find_if(iter, end, [](char c) {
                return !(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F');
            });
            if (lastHex == iter)
            {
                auto start = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.reportError(cld::ErrorMessages::Lexer::AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED, start,
                                    {start, start + 1});
                errorOccured = true;
                continue;
            }

            *resultStart = hexToValue({iter, static_cast<std::size_t>(lastHex - iter)});
            resultStart++;

            iter = lastHex;
        }
        else if (iter[1] >= '0' && iter[1] <= '9')
        {
            // We take 8 and 9 here as well to tell the user its an invalid octal instead of an invalid simple
            // escape sequence
            iter++;
            auto lastOctal = std::find_if(iter, iter + std::min<std::size_t>(3, std::distance(iter, end)),
                                          [](char c) { return c < '0' || c > '7'; });
            if (lastOctal == iter)
            {
                // First character is 8 or 9. That's why we didn't encounter a single octal digit.
                // Also since there must be at least one character after \, lastOctal is definitely not end
                // here.
                context.reportError(cld::ErrorMessages::Lexer::INVALID_OCTAL_CHARACTER.args(std::string(1, *lastOctal)),
                                    offset, {offset, offset + 1});
                errorOccured = true;
                continue;
            }

            *resultStart = octalToValue({iter, static_cast<std::size_t>(lastOctal - iter)});
            resultStart++;

            iter = lastOctal;
        }
        else
        {
            // Escape sequence or illegal escape
            auto start = offset;
            auto character = escapeCharToValue(iter[1], start, context);
            if (character)
            {
                *resultStart = *character;
                resultStart++;
            }
            else
            {
                errorOccured = true;
            }
            iter += 2;
        }
    }

    for (auto iter = result.data(); iter != resultStart; iter++)
    {
        if (*iter > largestCharacter)
        {
            context.reportError(cld::ErrorMessages::Lexer::CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE,
                                context.tokenStartOffset);
            errorOccured = true;
        }
    }
    result.resize(std::distance(result.data(), resultStart));
    return {result, errorOccured};
}

template <class T, class... Args>
std::pair<Token::ValueType, Token::Type> castInteger(std::uint64_t integer,
                                                     std::array<Token::Type, sizeof...(Args) + 1> types)
{
    // Clang
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
    // GCC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
    // MSVC
#pragma warning(push)
#pragma warning(disable : 4018)
#pragma warning(disable : 4389)
    if constexpr (std::is_signed_v<T>)
    {
        if (llvm::APInt::getSignedMaxValue(sizeof(T) * 8).uge(integer))
        {
            return {{llvm::APSInt(llvm::APInt(sizeof(T) * 8, integer), !std::is_signed_v<T>)}, types[0]};
        }
    }
    else
    {
        if (llvm::APInt::getMaxValue(sizeof(T) * 8).uge(integer))
        {
            return {{llvm::APSInt(llvm::APInt(sizeof(T) * 8, integer), !std::is_signed_v<T>)}, types[0]};
        }
    }
    if constexpr (sizeof...(Args) != 0)
    {
        std::array<Token::Type, sizeof...(Args)> second;
        std::copy(types.begin(), types.end() - 1, second.begin());
        return castInteger<Args...>(integer, second);
    }
    else
    {
        CLD_UNREACHABLE;
    }
#pragma GCC diagnostic pop
#pragma clang diagnostic pop
#pragma warning(pop)
}

std::optional<std::pair<Token::ValueType, Token::Type>> processNumber(const char* begin, const char* end,
                                                                      std::uint64_t beginLocation, Context& context)
{
    CLD_ASSERT(std::distance(begin, end) >= 1);
    bool isHex = std::distance(begin, end) >= 2 && *begin == '0' && (*(begin + 1) == 'x' || *(begin + 1) == 'X');
    std::vector<char> legalValues(10);
    std::iota(legalValues.begin(), legalValues.end(), '0');
    if (isHex)
    {
        legalValues.resize(10 + 12);
        std::iota(legalValues.begin() + 10, legalValues.end(), 'A');
        std::iota(legalValues.begin() + 10 + 6, legalValues.end(), 'a');
    }
    bool isFloat = false;
    auto searchFunction = [&legalValues, &isFloat](char c) mutable {
        if (c == '.' && !isFloat)
        {
            isFloat = true;
            return false;
        }
        return std::none_of(legalValues.begin(), legalValues.end(), [c](char allowed) { return allowed == c; });
    };
    auto suffixBegin = std::find_if(begin + (isHex ? 2 : 0), end, searchFunction);
    // If it's a float it might still have an exponent part. If it's non hex this is e [optional + or -] then
    // again followed by digits. If it's a hex then its p [optional + or -]. We check if it's either an
    // then continue our search
    constexpr unsigned toLower = 32;
    bool errorsOccurred = false;
    if (suffixBegin != end && (*suffixBegin | toLower) == (isHex ? 'p' : 'e'))
    {
        isFloat = true;
        suffixBegin++;
        if (suffixBegin != end && (*suffixBegin == '+' || *suffixBegin == '-'))
        {
            suffixBegin++;
        }
        auto prev = suffixBegin;
        suffixBegin = std::find_if(suffixBegin, end, searchFunction);
        if (prev == suffixBegin)
        {
            context.reportError(cld::ErrorMessages::Lexer::EXPECTED_DIGITS_AFTER_EXPONENT,
                                beginLocation + std::distance(begin, suffixBegin), context.tokenStartOffset,
                                context.getOffset() - 1);
            errorsOccurred = true;
        }
    }
    else if (isHex && isFloat)
    {
        context.reportError(cld::ErrorMessages::Lexer::BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT,
                            context.getOffset() - 1, context.tokenStartOffset, context.getOffset() - 1);
        errorsOccurred = true;
    }

    bool isHexOrOctal = isHex;
    if (!isHex && !isFloat && *begin == '0')
    {
        isHexOrOctal = true;
        auto result = std::find_if(begin, suffixBegin, [](char c) { return c >= '8'; });
        while (result != suffixBegin)
        {
            errorsOccurred = true;
            context.reportError(cld::ErrorMessages::Lexer::INVALID_OCTAL_CHARACTER.args(std::string(1, *result)),
                                beginLocation + std::distance(begin, result), context.tokenStartOffset,
                                context.getOffset() - 1, {beginLocation + std::distance(begin, result)});
            result = std::find_if(result + 1, suffixBegin, [](char c) { return c >= '8'; });
        }
    }

    auto suffix = std::string(suffixBegin, std::distance(suffixBegin, end));
    std::unordered_set<std::string_view> set;
    if (!isFloat)
    {
        set = {"u", "U", "ul", "Ul", "uL", "UL", "uLL", "ULL", "ull", "Ull", "l", "L", "ll", "LL", ""};
    }
    else
    {
        set = {"f", "l", "F", "L", ""};
    }
    if (set.count(suffix) == 0)
    {
        std::vector<std::uint64_t> arrows(suffix.size());
        std::iota(arrows.begin(), arrows.end(), beginLocation + std::distance(begin, suffixBegin));
        context.reportError(cld::ErrorMessages::Lexer::INVALID_LITERAL_SUFFIX.args(suffix),
                            beginLocation + std::distance(begin, suffixBegin), context.tokenStartOffset,
                            context.getOffset() - 1, std::move(arrows));
        return {};
    }

    if (errorsOccurred)
    {
        return {};
    }
    if (!isFloat)
    {
        std::string number(begin, suffixBegin);
        llvm::APInt test;
        llvm::StringRef(number).getAsInteger(0, test);
        if (test.getBitWidth() > 64)
        {
            context.reportError(cld::ErrorMessages::Lexer::INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE, beginLocation,
                                context.tokenStartOffset, context.getOffset() - 1);
            return {};
        }
        char* endPtr;
        auto integer = std::strtoull(number.data(), &endPtr, 0);
        CLD_ASSERT(*endPtr == '\0');
        if (suffix.empty())
        {
            switch (context.getLanguageOptions().getSizeOfInt())
            {
                case 2:
                    if (isHexOrOctal)
                    {
                        if (context.getLanguageOptions().getSizeOfLong() == 4)
                        {
                            return castInteger<std::int16_t, std::uint16_t, std::int32_t, std::uint32_t, std::int64_t,
                                               std::uint64_t>(integer,
                                                              {Token::Type::Int, Token::Type::UnsignedInt,
                                                               Token::Type::Long, Token::Type::UnsignedLong,
                                                               Token::Type::LongLong, Token::Type::UnsignedLongLong});
                        }
                        return castInteger<std::int16_t, std::uint16_t, std::int64_t, std::uint64_t>(
                            integer,
                            {Token::Type::Int, Token::Type::UnsignedInt, Token::Type::Long, Token::Type::UnsignedLong});
                    }
                    if (context.getLanguageOptions().getSizeOfLong() == 4)
                    {
                        return castInteger<std::int16_t, std::int32_t, std::int64_t>(
                            integer, {Token::Type::Int, Token::Type::Long, Token::Type::LongLong});
                    }
                    return castInteger<std::int16_t, std::int64_t>(integer, {Token::Type::Int, Token::Type::Long});
                case 4:
                    if (isHexOrOctal)
                    {
                        if (context.getLanguageOptions().getSizeOfLong() == 4)
                        {
                            return castInteger<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t>(
                                integer, {Token::Type::Int, Token::Type::UnsignedInt, Token::Type::LongLong,
                                          Token::Type::UnsignedLongLong});
                        }
                        return castInteger<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t>(
                            integer,
                            {Token::Type::Int, Token::Type::UnsignedInt, Token::Type::Long, Token::Type::UnsignedLong});
                    }
                    if (context.getLanguageOptions().getSizeOfLong() == 4)
                    {
                        return castInteger<std::int32_t, std::int64_t>(integer,
                                                                       {Token::Type::Int, Token::Type::LongLong});
                    }
                    return castInteger<std::int32_t, std::int64_t>(integer, {Token::Type::Int, Token::Type::Long});
                case 8:
                    if (isHexOrOctal)
                    {
                        return castInteger<std::int64_t, std::uint64_t>(integer,
                                                                        {Token::Type::Int, Token::Type::UnsignedInt});
                    }
                    return castInteger<std::int64_t>(integer, {Token::Type::Int});
                default: CLD_UNREACHABLE;
            }
        }
        else if (suffix == "u" || suffix == "U")
        {
            switch (context.getLanguageOptions().getSizeOfInt())
            {
                case 2:
                    if (context.getLanguageOptions().getSizeOfLong() == 4)
                    {
                        return castInteger<std::uint16_t, std::uint32_t, std::uint64_t>(
                            integer,
                            {Token::Type::UnsignedInt, Token::Type::UnsignedLong, Token::Type::UnsignedLongLong});
                    }
                    return castInteger<std::uint16_t, std::uint64_t>(
                        integer, {Token::Type::UnsignedInt, Token::Type::UnsignedLong});
                case 4:
                    if (context.getLanguageOptions().getSizeOfLong() == 4)
                    {
                        return castInteger<std::uint32_t, std::uint64_t>(
                            integer, {Token::Type::UnsignedInt, Token::Type::UnsignedLongLong});
                    }
                    return castInteger<std::uint32_t, std::uint64_t>(
                        integer, {Token::Type::UnsignedInt, Token::Type::UnsignedLong});
                case 8: return castInteger<std::uint64_t>(integer, {Token::Type::UnsignedInt});
                default: CLD_UNREACHABLE;
            }
        }
        else if (suffix == "L" || suffix == "l")
        {
            if (isHexOrOctal)
            {
                if (context.getLanguageOptions().getSizeOfLong() == 4)
                {
                    return castInteger<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t>(
                        integer, {Token::Type::Long, Token::Type::UnsignedLong, Token::Type::LongLong,
                                  Token::Type::UnsignedLongLong});
                }
                return castInteger<std::int64_t, std::uint64_t>(integer,
                                                                {Token::Type::Long, Token::Type::UnsignedLong});
            }
            if (context.getLanguageOptions().getSizeOfLong() == 4)
            {
                return castInteger<std::int32_t, std::int64_t>(integer, {Token::Type::Long, Token::Type::LongLong});
            }
            return castInteger<std::int64_t>(integer, {Token::Type::Long});
        }
        else if (suffix.size() == 2
                 && std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u' || c == 'U'; })
                 && std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'l' || c == 'L'; }))
        {
            if (context.getLanguageOptions().getSizeOfLong() == 4)
            {
                return castInteger<std::uint32_t, std::uint64_t>(
                    integer, {Token::Type::UnsignedLong, Token::Type::UnsignedLongLong});
            }
            return castInteger<std::uint64_t>(integer, {Token::Type::UnsignedLong});
        }
        else if (suffix == "ll" || suffix == "LL")
        {
            if (isHexOrOctal)
            {
                return castInteger<std::int64_t, std::uint64_t>(integer,
                                                                {Token::Type::LongLong, Token::Type::UnsignedLongLong});
            }
            return castInteger<std::int64_t>(integer, {Token::Type::LongLong});
        }
        else if (suffix.size() == 3
                 && std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u' || c == 'U'; })
                 && (suffix.find("LL") != std::string_view::npos || suffix.find("ll") != std::string_view::npos))
        {
            return castInteger<std::uint64_t>(integer, {Token::Type::UnsignedLongLong});
        }
        else
        {
            CLD_UNREACHABLE;
        }
    }
    else
    {
        auto input = (*begin == '.' ? "0" : "") + std::string(begin, suffixBegin);
        if (suffix.empty())
        {
            return {{llvm::APFloat(llvm::APFloat::IEEEdouble(), input), Token::Type::Double}};
        }
        else if (suffix == "f" || suffix == "F")
        {
            return {{llvm::APFloat(llvm::APFloat::IEEEsingle(), input), Token::Type::Float}};
        }
        else if (suffix == "l" || suffix == "L")
        {
            switch (context.getLanguageOptions().getSizeOfLongDoubleBits())
            {
                case 64: return {{llvm::APFloat(llvm::APFloat::IEEEdouble(), input), Token::Type::LongDouble}};
                case 80: return {{llvm::APFloat(llvm::APFloat::x87DoubleExtended(), input), Token::Type::LongDouble}};
                case 128: return {{llvm::APFloat(llvm::APFloat::IEEEquad(), input), Token::Type::LongDouble}};
                default: CLD_UNREACHABLE;
            }
        }
    }
    CLD_UNREACHABLE;
}

std::uint8_t codePointToUtf8ByteCount(std::uint32_t codepoint)
{
    if (codepoint <= 0x007F)
    {
        return 1;
    }
    if (codepoint <= 0x07FF)
    {
        return 2;
    }
    if (codepoint <= 0xFFFF)
    {
        return 3;
    }
    if (codepoint <= 0x10FFFF)
    {
        return 4;
    }
    CLD_UNREACHABLE;
}

struct Start final
{
    StateMachine advance(std::uint32_t c, Context& context);
};

struct CharacterLiteral final
{
    bool wide = false;
    std::string characters;

    std::pair<StateMachine, bool> advance(char c, Context& context);
};

struct StringLiteral final
{
    bool wide = false;
    std::string characters;

    std::pair<StateMachine, bool> advance(char c, Context& context);
};

struct Text final
{
    std::string characters;

    std::pair<StateMachine, bool> advance(std::uint32_t c, Context& context);
};

struct PreprocessingNumber final
{
    std::string characters{};

    std::pair<StateMachine, bool> advance(std::uint32_t c, Context& context);
};

struct MaybeUC final
{
    std::unique_ptr<StateMachine> prevState{};
    bool error = false;

    std::pair<StateMachine, bool> advance(std::uint32_t c, Context& context) noexcept;
};

struct UniversalCharacter final
{
    bool big;
    std::optional<std::variant<Text, PreprocessingNumber>> suspHolder{};
    llvm::SmallVector<char, 8> characters{};

    std::pair<StateMachine, bool> advance(std::uint32_t c, Context& context);
};

struct LineComment final
{
    std::pair<StateMachine, bool> advance(char c, Context& context) noexcept;
};

struct BlockComment final
{
    std::optional<char> lastChar{};

    StateMachine advance(char c, Context& context) noexcept;
};

struct Number final
{
    std::string numberChars{};

    std::pair<StateMachine, bool> advance(std::uint32_t c, Context& context);
};

struct Dot final
{
    std::uint8_t dotCount = 1;

    std::pair<StateMachine, bool> advance(char c, Context& context);
};

struct Punctuation final
{
    std::underlying_type_t<TokenType> first;

private:
    static constexpr std::underlying_type_t<TokenType> POUND_PERCENT =
        std::numeric_limits<std::underlying_type_t<TokenType>>::max() - 1;

public:
    std::pair<StateMachine, bool> advance(char c, Context& context);
};

struct AfterInclude final
{
    char delimiter;
    std::string characters{};

    StateMachine advance(char c, Context& context);
};

struct L final
{
    static std::pair<StateMachine, bool> advance(char c, Context& context) noexcept;
};

StateMachine Start::advance(std::uint32_t c, Context& context)
{
    switch (c)
    {
        case '.': return Dot{};
        case '\'': return CharacterLiteral{};
        case '"':
        {
            if (context.isInPreprocessor() && context.getResult().size() >= 2
                && context.getResult()[context.getResult().size() - 2].getTokenType() == TokenType::Pound
                && context.getResult().back().getTokenType() == TokenType::Identifier
                && cld::get<std::string>(context.getResult()[context.getResult().size() - 1].getValue()) == "include")
            {
                return AfterInclude{'"'};
            }
            return StringLiteral{};
        }
        case 'L': return L{};
        case '\\': return MaybeUC{};
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        {
            if (!context.isInPreprocessor())
            {
                return Number{{static_cast<char>(c)}};
            }
            else
            {
                return PreprocessingNumber{{static_cast<char>(c)}};
            }
        }
        case '<':
        {
            if (context.isInPreprocessor() && context.getResult().size() >= 2
                && context.getResult()[context.getResult().size() - 2].getTokenType() == TokenType::Pound
                && context.getResult()[context.getResult().size() - 1].getTokenType() == TokenType::Identifier
                && cld::get<std::string>(context.getResult()[context.getResult().size() - 1].getValue()) == "include")
            {
                return AfterInclude{'>'};
            }
            return Punctuation{static_cast<int>(TokenType::LessThan)};
        }
        case '#': return Punctuation{static_cast<int>(TokenType::Pound)};
        case ':': return Punctuation{static_cast<int>(TokenType::Colon)};
        case '-': return Punctuation{static_cast<int>(TokenType::Minus)};
        case '>': return Punctuation{static_cast<int>(TokenType::GreaterThan)};
        case '&': return Punctuation{static_cast<int>(TokenType::Ampersand)};
        case '|': return Punctuation{static_cast<int>(TokenType::BitOr)};
        case '+': return Punctuation{static_cast<int>(TokenType::Plus)};
        case '=': return Punctuation{static_cast<int>(TokenType::Assignment)};
        case '!': return Punctuation{static_cast<int>(TokenType::LogicalNegation)};
        case '*': return Punctuation{static_cast<int>(TokenType::Asterisk)};
        case '/': return Punctuation{static_cast<int>(TokenType::Division)};
        case '%': return Punctuation{static_cast<int>(TokenType::Percent)};
        case '^': return Punctuation{static_cast<int>(TokenType::BitXor)};
        case '~': context.push(context.getOffset() - 1, context.getOffset(), TokenType::BitWiseNegation); return *this;
        case '(': context.push(context.getOffset() - 1, context.getOffset(), TokenType::OpenParentheses); return *this;
        case ')': context.push(context.getOffset() - 1, context.getOffset(), TokenType::CloseParentheses); return *this;
        case '{': context.push(context.getOffset() - 1, context.getOffset(), TokenType::OpenBrace); return *this;
        case '}': context.push(context.getOffset() - 1, context.getOffset(), TokenType::CloseBrace); return *this;
        case '[':
            context.push(context.getOffset() - 1, context.getOffset(), TokenType::OpenSquareBracket);
            return *this;
        case ']':
            context.push(context.getOffset() - 1, context.getOffset(), TokenType::CloseSquareBracket);
            return *this;
        case ';': context.push(context.getOffset() - 1, context.getOffset(), TokenType::SemiColon); return *this;
        case ',': context.push(context.getOffset() - 1, context.getOffset(), TokenType::Comma); return *this;
        case '?': context.push(context.getOffset() - 1, context.getOffset(), TokenType::QuestionMark); return *this;
        case '\n':
            if (context.isInPreprocessor())
            {
                context.push(context.getOffset() - 1, context.getOffset(), TokenType::Newline);
                return *this;
            }
            [[fallthrough]];
        default:
        {
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                || (llvm::sys::UnicodeCharSet(C99AllowedIDCharRanges).contains(c)
                    && !llvm::sys::UnicodeCharSet(C99DisallowedInitialIDCharRanges).contains(c)))
            {
                std::string buffer(4, ' ');
                auto* start = buffer.data();
                llvm::ConvertCodePointToUTF8(c, start);
                buffer.resize(std::distance(buffer.data(), start));
                return Text{std::move(buffer)};
            }
            else if (c != ' ' && c != '\t' && c != '\r' && c != '\f' && c != '\v' && c != '\n'
                     && !llvm::sys::UnicodeCharSet(UnicodeWhitespaceCharRanges).contains(c))
            {
                if (!llvm::sys::unicode::isPrintable(c))
                {
                    std::string buffer = "\\U";
                    llvm::raw_string_ostream ss(buffer);
                    ss << llvm::format_hex_no_prefix(c, 8);
                    ss.flush();
                    auto size = getNumUTF8ForUTF32(c);
                    std::vector<std::uint64_t> arrows(size);
                    std::iota(arrows.begin(), arrows.end(), context.getOffset() - size);
                    context.reportError(cld::ErrorMessages::Lexer::NON_PRINTABLE_CHARACTER_N.args(std::move(buffer)),
                                        context.getOffset() - size, context.getOffset() - size, context.getOffset(),
                                        std::move(arrows));
                }
                else if (!context.isInPreprocessor())
                {
                    std::string buffer(4, ' ');
                    auto* start = buffer.data();
                    llvm::ConvertCodePointToUTF8(c, start);
                    buffer.resize(std::distance(buffer.data(), start));
                    std::vector<std::uint64_t> arrows(buffer.size());
                    std::iota(arrows.begin(), arrows.end(), context.getOffset() - buffer.size());
                    context.reportError(cld::ErrorMessages::Lexer::UNEXPECTED_CHARACTER.args(buffer),
                                        context.getOffset() - buffer.size(), context.getOffset() - buffer.size(),
                                        context.getOffset(), std::move(arrows));
                }
                else
                {
                    context.push(context.getOffset() - 1, context.getOffset(), TokenType::Miscellaneous);
                }
            }
            return *this;
        }
    }
}

std::pair<StateMachine, bool> CharacterLiteral::advance(char c, Context& context)
{
    if ((c == '\n' || c == '\r') && context.isInPreprocessor())
    {
        context.push(1, TokenType::Miscellaneous);
        return {Start{}, false};
    }
    if (c == '\'' && (characters.empty() || characters.back() != '\\'))
    {
        if (context.isInPreprocessor())
        {
            context.push(TokenType::Literal, characters);
            return {Start{}, true};
        }
        auto [result, errorOccured] =
            processCharacters(characters, context, wide, cld::ErrorMessages::Lexer::CHARACTER_LITERAL);
        if (result.empty())
        {
            if (!errorOccured)
            {
                std::vector<std::uint64_t> arrows(context.getOffset() - context.tokenStartOffset);
                std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
                context.reportError(cld::ErrorMessages::Lexer::CHARACTER_LITERAL_CANNOT_BE_EMPTY,
                                    context.tokenStartOffset, std::move(arrows));
            }
            return {Start{}, true};
        }
        else if (result.size() > 1)
        {
            std::vector<std::uint64_t> arrows(context.getOffset() - context.tokenStartOffset);
            std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
            context.reportWarning(cld::ErrorMessages::Lexer::DISCARDING_ALL_BUT_FIRST_CHARACTER,
                                  context.tokenStartOffset, std::move(arrows));
        }

        if (wide)
        {
            context.push(TokenType::Literal,
                         llvm::APSInt(llvm::APInt(context.getLanguageOptions().getSizeOfWChar() * 8, result[0]),
                                      !context.getLanguageOptions().isWCharSigned()));
        }
        else
        {
            context.push(TokenType::Literal,
                         llvm::APSInt(llvm::APInt(32, static_cast<std::uint8_t>(result[0]), true), false));
        }
        return {Start{}, true};
    }
    characters += c;
    return {std::move(*this), true};
}

std::pair<StateMachine, bool> StringLiteral::advance(char c, Context& context)
{
    if ((c == '\n' || c == '\r') && context.isInPreprocessor())
    {
        context.push(1, TokenType::Miscellaneous);
        return {Start{}, false};
    }
    if (c == '"' && (characters.empty() || characters.back() != '\\'))
    {
        if (context.isInPreprocessor())
        {
            context.push(cld::Lexer::TokenType::StringLiteral, characters);
            return {Start{}, true};
        }
        auto [result, errorOccured] =
            processCharacters(characters, context, wide, cld::ErrorMessages::Lexer::STRING_LITERAL);
        if (errorOccured)
        {
            return {Start{}, true};
        }
        if (!wide || context.getLanguageOptions().getSizeOfWChar() == 1)
        {
            const auto* start = result.data();
            std::vector<llvm::UTF8> utf8(result.size() * 5);
            auto* dest = utf8.data();
            auto conversion = llvm::ConvertUTF32toUTF8(&start, start + result.size(), &dest, dest + utf8.size(),
                                                       llvm::strictConversion);
            if (conversion != llvm::conversionOK)
            {
                // Due to error occurred being true at failed utf8 to utf 32 conversion this
                // code can't be reached
                CLD_UNREACHABLE;
            }
            else if (wide)
            {
                context.push(TokenType::StringLiteral, NonCharString{NonCharString::Wide, {utf8.data(), dest}});
            }
            else
            {
                context.push(TokenType::StringLiteral, std::string{utf8.data(), dest});
            }
        }
        else
        {
            switch (context.getLanguageOptions().getSizeOfWChar())
            {
                case 2:
                {
                    const auto* start = result.data();
                    std::vector<llvm::UTF16> utf16(result.size() * 3);
                    auto* dest = utf16.data();
                    auto conversion = llvm::ConvertUTF32toUTF16(&start, start + result.size(), &dest,
                                                                dest + utf16.size(), llvm::strictConversion);
                    if (conversion != llvm::conversionOK)
                    {
                        CLD_UNREACHABLE; // While error occurred is true due to failed utf8 to utf 32
                                         // conversion this code can't be reached
                    }
                    else
                    {
                        context.push(TokenType::StringLiteral,
                                     NonCharString{NonCharString::Wide, {utf16.data(), dest}});
                    }
                    break;
                }
                case 4:
                {
                    context.push(TokenType::StringLiteral, NonCharString{NonCharString::Wide, result});
                    break;
                }
                default: CLD_UNREACHABLE;
            }
        }
        return {Start{}, true};
    }
    characters += c;
    return {std::move(*this), true};
}

std::pair<StateMachine, bool> L::advance(char c, Context&) noexcept
{
    if (c == '"')
    {
        return {StringLiteral{true, {}}, true};
    }
    else if (c == '\'')
    {
        return {CharacterLiteral{true, {}}, true};
    }
    else if (c == '\\')
    {
        return {MaybeUC{std::make_unique<StateMachine>(L{})}, true};
    }
    else
    {
        return {Text{"L"}, false};
    }
}

std::pair<StateMachine, bool> Text::advance(std::uint32_t c, Context& context)
{
    if (c == '\\')
    {
        return {MaybeUC{std::make_unique<StateMachine>(std::move(*this))}, true};
    }
    else if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') && !(c >= '0' && c <= '9') && c != '_'
             && !llvm::sys::UnicodeCharSet(C99AllowedIDCharRanges).contains(c))
    {
        if (!context.isInPreprocessor() && isKeyword(characters))
        {
            context.push(1, charactersToKeyword(characters));
        }
        else
        {
            context.push(1, TokenType::Identifier, std::move(characters));
        }
        return {Start{}, false};
    }
    characters.resize(characters.size() + 4);
    auto* start = characters.data() + characters.size() - 4;
    llvm::ConvertCodePointToUTF8(c, start);
    characters.resize(characters.size() - std::distance(start, characters.data() + characters.size()));
    return {std::move(*this), true};
}

std::pair<StateMachine, bool> PreprocessingNumber::advance(std::uint32_t c, Context& context)
{
    CLD_ASSERT(context.isInPreprocessor());
    constexpr std::uint8_t toLower = 32;
    if (c == '\\')
    {
        return {MaybeUC{std::make_unique<StateMachine>(std::move(*this))}, true};
    }
    else if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') && !(c >= '0' && c <= '9') && c != '_' && c != '.'
             && (((characters.back() | toLower) != 'e' && (characters.back() | toLower) != 'p')
                 || (c != '+' && c != '-'))
             && !llvm::sys::UnicodeCharSet(C99AllowedIDCharRanges).contains(c))
    {
        context.push(1, TokenType::PPNumber, std::move(characters));
        return {Start{}, false};
    }
    characters.resize(characters.size() + 4);
    auto* start = characters.data() + characters.size() - 4;
    llvm::ConvertCodePointToUTF8(c, start);
    characters.resize(characters.size() - std::distance(start, characters.data() + characters.size()));
    return {std::move(*this), true};
}

std::pair<StateMachine, bool> MaybeUC::advance(std::uint32_t c, Context& context) noexcept
{
    if (c == 'u' || c == 'U')
    {
        // If Universal character, it's in or starting an identifier, not in a character or string literal
        if (prevState)
        {
            if (std::holds_alternative<Text>(*prevState))
            {
                return {UniversalCharacter{c == 'U', cld::get<Text>(std::move(*prevState))}, true};
            }
            else if (std::holds_alternative<L>(*prevState))
            {
                return {UniversalCharacter{c == 'U', Text{"L"}}, true};
            }
            else if (std::holds_alternative<PreprocessingNumber>(*prevState))
            {
                return {UniversalCharacter{c == 'U', cld::get<PreprocessingNumber>(std::move(*prevState))}, true};
            }
            else
            {
                CLD_UNREACHABLE;
            }
        }
        return {UniversalCharacter{c == 'U'}, true};
    }
    else if (context.isInPreprocessor())
    {
        if (prevState)
        {
            context.withOffset(context.getOffset() - 1,
                               [&] { cld::match(*prevState, [&](auto&& value) { value.advance(' ', context); }); });
        }
        context.push(context.getOffset() - 2, context.getOffset() - 1, TokenType::Backslash);
        return {Start{}, false};
    }
    else if (c == ' ' || c == '\t' || c == '\r' || c == '\f' || c == '\v'
             || llvm::sys::UnicodeCharSet(UnicodeWhitespaceCharRanges).contains(c))
    {
        error = true;
        return {std::move(*this), true};
    }
    else if (c == '\n')
    {
        if (context.isInPreprocessor())
        {
            context.push(1, TokenType::Backslash);
            return {Start{}, false};
        }
        auto result = context.currentView().rfind('\\');
        CLD_ASSERT(result != std::string_view::npos);
        result += context.tokenStartOffset;
        std::vector<std::uint64_t> arrows(context.getOffset() - result - 1);
        std::iota(arrows.begin(), arrows.end(), result);
        context.reportError(cld::ErrorMessages::Lexer::NO_WHITESPACE_ALLOWED_BETWEEN_BACKSLASH_AND_NEWLINE, result,
                            result, context.getOffset() - 1, std::move(arrows));
        return {Start{}, true};
    }

    auto location = context.currentView().rfind('\\');
    CLD_ASSERT(location != std::string_view::npos);
    location += context.tokenStartOffset;
    context.reportError(cld::ErrorMessages::Lexer::STRAY_N_IN_PROGRAM.args("\\"), location, location, location + 1,
                        {location});
    return {Start{}, false};
}

std::pair<StateMachine, bool> UniversalCharacter::advance(std::uint32_t c, Context& context)
{
    if (!(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F'))
    {
        std::size_t result = context.currentView().rfind('\\');
        CLD_ASSERT(result != std::string_view::npos);
        result += context.tokenStartOffset;
        if (!context.isInPreprocessor())
        {
            context.reportError(cld::ErrorMessages::Lexer::STRAY_N_IN_PROGRAM.args("\\"), result, result, result + 1,
                                {result});
            context.reportNote(
                cld::Notes::Lexer::UNIVERSAL_CHARACTER_REQUIRES_N_MORE_DIGITS.args((big ? 8 : 4) - characters.size()),
                result, result, context.getOffset() - codePointToUtf8ByteCount(c));
            return {Start{}, false};
        }
        // We are actually not a univeral character. In non PP we would error. Instead we now need to push the
        // suspendate state if there is one and then push a backslash token followed by Text containing a u or U that
        // will then handle whatever character we just encountered
        if (suspHolder)
        {
            context.withOffset(result + 1,
                               [&] { cld::match(*suspHolder, [&](auto&& value) { value.advance(' ', context); }); });
        }
        context.push(result, result + 1, TokenType::Backslash);
        context.tokenStartOffset = result + 1;
        return {Text{big ? "U" : "u"}, false};
    }

    characters.push_back(static_cast<char>(c));
    if (characters.size() != (big ? 8 : 4))
    {
        return {std::move(*this), true};
    }

    std::size_t ucStart = context.currentView().rfind('\\');
    CLD_ASSERT(ucStart != std::string_view::npos);
    ucStart += context.tokenStartOffset;
    auto result =
        universalCharacterToValue({characters.data(), characters.size()}, ucStart, context.getOffset(), context);
    if (!result)
    {
        if (!context.isInPreprocessor())
        {
            return {Start{}, true};
        }
        if (suspHolder)
        {
            context.withOffset(ucStart + 1,
                               [&] { cld::match(*suspHolder, [&](auto&& value) { value.advance(' ', context); }); });
        }
        context.push(ucStart, ucStart + 1, TokenType::Backslash);
        context.tokenStartOffset = ucStart + 1;
        auto view = context.currentView();
        return {Text{{view.begin(), view.end()}}, true};
    }
    if (suspHolder && std::holds_alternative<PreprocessingNumber>(*suspHolder))
    {
        // In an PP Number we only go through the whole mechanism to check if the universal character is invalid.
        // Otherwise different tokens need to be generated
        auto pp = cld::get<PreprocessingNumber>(std::move(*suspHolder));
        pp.characters += context.currentView().substr(ucStart - context.tokenStartOffset);
        return {std::move(pp), true};
    }
    // According to 6.4.2.1 Paragraph 3 of the C99 Standard these restrictions apply only to an identifier.
    // This means even if a identifier-nondigit element is allowed in a PP Number it doesn't have to meet those
    // requirements per my interpretation
    if (!llvm::sys::UnicodeCharSet(C99AllowedIDCharRanges).contains(*result)
        || !(suspHolder || !llvm::sys::UnicodeCharSet(C99DisallowedInitialIDCharRanges).contains(*result)))
    {
        if (!context.isInPreprocessor())
        {
            std::vector<std::uint64_t> arrows(context.getOffset() - (ucStart));
            std::iota(arrows.begin(), arrows.end(), ucStart);
            context.reportError(cld::ErrorMessages::Lexer::UNEXPECTED_CHARACTER.args(
                                    (big ? "\\U" : "\\u" + std::string(characters.begin(), characters.end()))),
                                ucStart, ucStart, context.getOffset(), std::move(arrows));
            return {Start{}, true};
        }
        if (suspHolder)
        {
            context.withOffset(ucStart + 1,
                               [&] { cld::match(*suspHolder, [&](auto&& value) { value.advance(' ', context); }); });
        }
        context.push(ucStart, ucStart + 1, TokenType::Backslash);
        context.push(ucStart + 1, ucStart + 2, TokenType::Identifier, big ? "U" : "u");
        context.tokenStartOffset = ucStart + 2;
        auto view = context.currentView();
        return {PreprocessingNumber{{view.begin(), view.end()}}, true};
    }
    // This line is needed because a text can start with a universal character. Luckily a Preprocessing number can't
    // So we only reach this line if suspHolder is either empty or contains a Text
    auto newText = suspHolder ? cld::get<Text>(std::move(*suspHolder)) : Text{};
    newText.characters.resize(newText.characters.size() + 4);
    auto start = newText.characters.data() + newText.characters.size() - 4;
    llvm::ConvertCodePointToUTF8(*result, start);
    newText.characters.resize(newText.characters.size()
                              - std::distance(start, newText.characters.data() + newText.characters.size()));
    return {std::move(newText), true};
}

std::pair<StateMachine, bool> Number::advance(std::uint32_t c, Context& context)
{
    CLD_ASSERT(!context.isInPreprocessor());
    // Consuming all characters first before then doing a proper parse
    if ((c >= '0' && c <= '9') || c == '.' || llvm::sys::UnicodeCharSet(C99AllowedIDCharRanges).contains(c)
        || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        || (!numberChars.empty() && (c == '+' || c == '-')
            && (numberChars.back() == 'e' || numberChars.back() == 'E' || numberChars.back() == 'p'
                || numberChars.back() == 'P')))
    {
        numberChars.resize(numberChars.size() + 4);
        auto start = numberChars.data() + numberChars.size() - 4;
        llvm::ConvertCodePointToUTF8(c, start);
        numberChars.resize(numberChars.size() - std::distance(start, numberChars.data() + numberChars.size()));
        return {*this, true};
    }
    auto result =
        processNumber(numberChars.data(), numberChars.data() + numberChars.size(), context.tokenStartOffset, context);
    if (result)
    {
        context.push(1, TokenType::Literal, std::move(result->first), result->second);
    }
    return {Start{}, false};
}

std::pair<StateMachine, bool> Dot::advance(char c, Context& context)
{
    if (c == '.')
    {
        if (++dotCount == 3)
        {
            context.push(TokenType::Ellipse);
            return {Start{}, true};
        }
        return {std::move(*this), true};
    }
    else if (c >= '0' && c <= '9')
    {
        if (dotCount == 2)
        {
            context.push(context.tokenStartOffset, context.tokenStartOffset + 1, TokenType::Dot);
        }
        if (!context.isInPreprocessor())
        {
            return {Number{"."}, false};
        }
        else
        {
            return {PreprocessingNumber{"."}, false};
        }
    }
    CLD_ASSERT(dotCount >= 0 && dotCount < 3);
    context.push(context.tokenStartOffset, context.tokenStartOffset + 1, TokenType::Dot);
    if (dotCount == 2)
    {
        context.push(context.tokenStartOffset + 1, context.tokenStartOffset + 2, TokenType::Dot);
    }
    return {Start{}, false};
}

std::pair<StateMachine, bool> Punctuation::advance(char c, Context& context)
{
    switch (first)
    {
        case static_cast<int>(TokenType::Minus):
            switch (c)
            {
                case '>': context.push(TokenType::Arrow); return {Start{}, true};
                case '-': context.push(TokenType::Decrement); return {Start{}, true};
                case '=': context.push(TokenType::MinusAssign); return {Start{}, true};
                default: context.push(1, TokenType::Minus); return {Start{}, false};
            }
        case static_cast<int>(TokenType::GreaterThan):
            switch (c)
            {
                case '>': first = static_cast<int>(TokenType::ShiftRight); return {std::move(*this), true};
                case '=': context.push(TokenType::GreaterThanOrEqual); return {Start{}, true};
                default: context.push(1, TokenType::GreaterThan); return {Start{}, false};
            }
        case static_cast<int>(TokenType::LessThan):
            switch (c)
            {
                case '<': first = static_cast<int>(TokenType::ShiftLeft); return {std::move(*this), true};
                case '=': context.push(TokenType::LessThanOrEqual); return {Start{}, true};
                case ':': context.push(TokenType::OpenSquareBracket); return {Start{}, true};
                case '%': context.push(TokenType::OpenBrace); return {Start{}, true};
                default: context.push(1, TokenType::LessThan); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Ampersand):
            switch (c)
            {
                case '&': context.push(TokenType::LogicAnd); return {Start{}, true};
                case '=': context.push(TokenType::BitAndAssign); return {Start{}, true};
                default: context.push(1, TokenType::Ampersand); return {Start{}, false};
            }
        case static_cast<int>(TokenType::BitOr):
            switch (c)
            {
                case '|': context.push(TokenType::LogicOr); return {Start{}, true};
                case '=': context.push(TokenType::BitOrAssign); return {Start{}, true};
                default: context.push(1, TokenType::BitOr); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Plus):
            switch (c)
            {
                case '+': context.push(TokenType::Increment); return {Start{}, true};
                case '=': context.push(TokenType::PlusAssign); return {Start{}, true};
                default: context.push(1, TokenType::Plus); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Assignment):
            switch (c)
            {
                case '=': context.push(TokenType::Equal); return {Start{}, true};
                default: context.push(1, TokenType::Assignment); return {Start{}, false};
            }
        case static_cast<int>(TokenType::LogicalNegation):
            switch (c)
            {
                case '=': context.push(TokenType::NotEqual); return {Start{}, true};
                default: context.push(1, TokenType::LogicalNegation); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Asterisk):
            switch (c)
            {
                case '=': context.push(TokenType::MultiplyAssign); return {Start{}, true};
                default: context.push(1, TokenType::Asterisk); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Division):
            switch (c)
            {
                case '=': context.push(TokenType::DivideAssign); return {Start{}, true};
                case '/': return {LineComment{}, true};
                case '*': return {BlockComment{}, true};
                default: context.push(1, TokenType::Division); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Percent):
            switch (c)
            {
                case '=': context.push(TokenType::ModuloAssign); return {Start{}, true};
                case '>': context.push(TokenType::CloseBrace); return {Start{}, true};
                case ':': first = static_cast<int>(TokenType::Pound); return {std::move(*this), true};
                default: context.push(1, TokenType::Percent); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Colon):
            switch (c)
            {
                case '>': context.push(TokenType::CloseSquareBracket); return {Start{}, true};
                default: context.push(1, TokenType::Colon); return {Start{}, false};
            }
        case static_cast<int>(TokenType::BitXor):
            switch (c)
            {
                case '=': context.push(TokenType::BitXorAssign); return {Start{}, true};
                default: context.push(1, TokenType::BitXor); return {Start{}, false};
            }
        case static_cast<int>(TokenType::ShiftLeft):
            switch (c)
            {
                case '=': context.push(TokenType::ShiftLeftAssign); return {Start{}, true};
                default: context.push(1, TokenType::ShiftLeft); return {Start{}, false};
            }
        case static_cast<int>(TokenType::ShiftRight):
            switch (c)
            {
                case '=': context.push(TokenType::ShiftRightAssign); return {Start{}, true};
                default: context.push(1, TokenType::ShiftRight); return {Start{}, false};
            }
        case static_cast<int>(TokenType::Pound):
            switch (c)
            {
                case '#': context.push(TokenType::DoublePound); return {Start{}, true};
                case '%': first = POUND_PERCENT; return {std::move(*this), true};
                default:
                    context.push(context.tokenStartOffset,
                                 context.tokenStartOffset + (context.currentView().front() == '#' ? 1 : 2),
                                 TokenType::Pound);
                    return {Start{}, false};
            }
        case POUND_PERCENT:
            if (c == ':')
            {
                context.push(TokenType::DoublePound);
                return {Start{}, true};
            }
            else
            {
                context.push(context.tokenStartOffset,
                             context.tokenStartOffset + (context.currentView().front() == '#' ? 1 : 2),
                             TokenType::Pound);
                context.push(context.getOffset() - 2, context.getOffset() - 1, TokenType::Percent);
                return {Start{}, false};
            }
        default: CLD_UNREACHABLE;
    }
}

StateMachine AfterInclude::advance(char c, Context& context)
{
    if (c != delimiter)
    {
        characters += c;
        return std::move(*this);
    }
    context.push(TokenType::StringLiteral, std::move(characters));
    return Start{};
}

std::pair<StateMachine, bool> LineComment::advance(char c, Context&) noexcept
{
    if (c == '\n')
    {
        return {Start{}, false};
    }
    return {std::move(*this), true};
}

StateMachine BlockComment::advance(char c, Context&) noexcept
{
    if (lastChar && *lastChar == '*' && c == '/')
    {
        return Start{};
    }
    lastChar = c;
    return *this;
}

template <class T>
struct FirstArgOfMethod;

template <class R, class C, class U, class... Args>
struct FirstArgOfMethod<R (C::*)(U, Args...)>
{
    using Type = U;
};

template <class R, class C, class U, class... Args>
struct FirstArgOfMethod<R (C::*)(U, Args...) noexcept>
{
    using Type = U;
};

template <class R, class U, class... Args>
struct FirstArgOfMethod<R (*)(U, Args...)>
{
    using Type = U;
};

template <class R, class U, class... Args>
struct FirstArgOfMethod<R (*)(U, Args...) noexcept>
{
    using Type = U;
};
} // namespace

constexpr static auto pattern = ctll::fixed_string{"(\\?\\?/|\\\\)\n|\\?\\?[=()'<!>\\-/]"};

cld::SourceObject cld::Lexer::tokenize(std::string source, LanguageOptions languageOptions, bool isInPreprocessor,
                                       llvm::raw_ostream* reporter, bool* errorsOccured)
{
    if (errorsOccured)
    {
        *errorsOccured = false;
    }
    source += '\n';

    StateMachine stateMachine;
    std::uint64_t offset = 0;
    std::vector<std::uint64_t> starts = {0};
    for (auto iter : source)
    {
        offset++;
        if (iter == '\n')
        {
            starts.push_back(offset);
        }
    }
    offset = 0;

    std::string charactersSpace;
    IntervalMap::Allocator allocator;
    IntervalMap characterToSourceSpace(allocator);
    charactersSpace.reserve(source.size());
    {
        auto stringView = std::string_view(source);
        for (auto& iter : ctre::range<pattern>(stringView))
        {
            auto view = iter.view();
            auto pos = view.data() - source.data();
            auto prefix = stringView.substr(0, view.data() - stringView.data());
            if (prefix.length() != 0)
            {
                characterToSourceSpace.insert(charactersSpace.size(), charactersSpace.size() + prefix.size() - 1,
                                              {pos - prefix.size(), pos});
            }
            charactersSpace += prefix;
            if (!iter.get<1>())
            {
                // If the first and only group didn't match it's a trigraph
                // While backslashes are removed and therefore not replaced we need to now replace the
                // trigraph
                static const std::unordered_map<std::string_view, char> mapping = {
                    {"?\?=", '#'}, {"?\?(", '['}, {"?\?/", '\\'}, {"?\?)", ']'}, {"?\?'", '^'},
                    {"?\?<", '{'}, {"?\?!", '|'}, {"?\?>", '}'},  {"?\?-", '~'}};
                auto result = mapping.find(view);
                CLD_ASSERT(result != mapping.end());
                characterToSourceSpace.insert(charactersSpace.size(), charactersSpace.size(), {pos, pos + view.size()});
                charactersSpace += result->second;
            }
            stringView.remove_prefix(prefix.size() + view.size());
        }
        if (!stringView.empty())
        {
            characterToSourceSpace.insert(charactersSpace.size(), charactersSpace.size() + stringView.size() - 1,
                                          {source.size() - stringView.size(), source.size()});
            charactersSpace += stringView;
        }
    }

    Context context(source, characterToSourceSpace, charactersSpace, offset, starts, languageOptions, isInPreprocessor,
                    reporter);
    const auto* end = charactersSpace.data() + charactersSpace.size();
    for (const auto* iter = charactersSpace.data(); iter != end;)
    {
        std::uint64_t step = 1;
        std::uint64_t prevOffset = offset;
        auto visitor = [iter, &step, &stateMachine, &context, &offset, end, prevOffset](auto&& state) mutable -> bool {
            using T = std::decay_t<decltype(state)>;
            constexpr bool needsCodepoint =
                std::is_same_v<std::uint32_t, typename FirstArgOfMethod<decltype(&T::advance)>::Type>;
            std::conditional_t<needsCodepoint, std::uint32_t, char> c{};
            if constexpr (needsCodepoint)
            {
                llvm::UTF32 result;
                auto start = iter;
                if (llvm::convertUTF8Sequence(reinterpret_cast<const llvm::UTF8**>(&start),
                                              reinterpret_cast<const llvm::UTF8*>(end), &result, llvm::strictConversion)
                    != llvm::conversionOK)
                {
                    step = getNumBytesForUTF8(start, end);
                    context.reportError(ErrorMessages::Lexer::INVALID_UTF8_SEQUENCE, offset, offset, offset + step,
                                        {offset});
                    return false;
                }
                c = result;
                step = std::distance(iter, start);
            }
            else
            {
                c = *iter;
            }
            offset = prevOffset + step;
            if constexpr (std::is_convertible_v<decltype(state.advance(c, context)), bool>)
            {
                return !state.advance(c, context);
            }
            else if constexpr (std::is_same_v<StateMachine, decltype(state.advance(c, context))>)
            {
                stateMachine = state.advance(c, context);
                if constexpr (std::is_same_v<std::decay_t<decltype(state)>, Start>)
                {
                    if (!std::holds_alternative<Start>(stateMachine))
                    {
                        context.tokenStartOffset = offset - step;
                    }
                }
                return false;
            }
            else if constexpr (std::is_void_v<decltype(state.advance(c, context))>)
            {
                state.advance(c, context);
                return false;
            }
            else
            {
                bool proceed;
                auto&& [lhs, rhs] = state.advance(c, context);
                if constexpr (std::is_same_v<std::decay_t<decltype(lhs)>, bool>)
                {
                    stateMachine = std::move(rhs);
                    proceed = lhs;
                }
                else
                {
                    stateMachine = std::move(lhs);
                    proceed = rhs;
                }
                if constexpr (std::is_same_v<std::decay_t<decltype(state)>, Start>)
                {
                    if (!std::holds_alternative<Start>(stateMachine))
                    {
                        context.tokenStartOffset = offset - step;
                    }
                }
                return !proceed;
            }
        };
        while (cld::match(stateMachine, visitor))
        {
            offset = prevOffset;
        }
        offset = prevOffset + step;
        iter += step;
    }
    cld::match(
        stateMachine, [](auto&&) {},
        [&context](CharacterLiteral&) {
            std::vector<std::uint64_t> arrows(context.getOffset() - 1 - context.tokenStartOffset);
            std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
            context.reportError(ErrorMessages::Lexer::UNTERMINATED_N.args(ErrorMessages::Lexer::CHARACTER_LITERAL),
                                context.tokenStartOffset, context.tokenStartOffset, context.getOffset() - 1,
                                std::move(arrows));
        },
        [&context](StringLiteral&) {
            std::vector<std::uint64_t> arrows(context.getOffset() - 1 - context.tokenStartOffset);
            std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
            context.reportError(ErrorMessages::Lexer::UNTERMINATED_N.args(ErrorMessages::Lexer::STRING_LITERAL),
                                context.tokenStartOffset, context.tokenStartOffset, context.getOffset() - 1,
                                std::move(arrows));
        },
        [&context](AfterInclude&) {
            std::vector<std::uint64_t> arrows(context.getOffset() - 1 - context.tokenStartOffset);
            std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
            context.reportError(ErrorMessages::Lexer::UNTERMINATED_N.args(ErrorMessages::Lexer::INCLUDE_DIRECTIVE),
                                context.tokenStartOffset, context.tokenStartOffset, context.getOffset() - 1,
                                std::move(arrows));
        },
        [&context](BlockComment&) {
            std::vector<std::uint64_t> arrows(context.getOffset() - 1 - context.tokenStartOffset);
            std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
            context.reportError(ErrorMessages::Lexer::UNTERMINATED_N.args(ErrorMessages::Lexer::BLOCK_COMMENT),
                                context.tokenStartOffset, context.tokenStartOffset, context.getOffset() - 1,
                                std::move(arrows));
        });
    if (errorsOccured)
    {
        *errorsOccured = context.isErrorsOccured();
    }

    return SourceObject(std::move(starts), std::move(context).getResult(), languageOptions);
}

std::string cld::Lexer::tokenName(cld::Lexer::TokenType tokenType)
{
    switch (tokenType)
    {
        case TokenType::Identifier: return "identifier";
        case TokenType::OpenParentheses: return "'('";
        case TokenType::CloseParentheses: return "')'";
        case TokenType::OpenBrace: return "'{'";
        case TokenType::CloseBrace: return "'}'";
        case TokenType::StringLiteral: return "string literal";
        case TokenType::Literal: return "literal";
        case TokenType::SemiColon: return "';'";
        case TokenType::Comma: return "','";
        case TokenType::Minus: return "'-'";
        case TokenType::BitWiseNegation: return "'~'";
        case TokenType::LogicalNegation: return "'!'";
        case TokenType::Plus: return "'+'";
        case TokenType::Asterisk: return "'*'";
        case TokenType::Division: return "'/'";
        case TokenType::Percent: return "'%'";
        case TokenType::LogicAnd: return "'&&'";
        case TokenType::LogicOr: return "'||'";
        case TokenType::Ampersand: return "'&'";
        case TokenType::BitOr: return "'|'";
        case TokenType::BitXor: return "'^'";
        case TokenType::Equal: return "'=='";
        case TokenType::NotEqual: return "'!='";
        case TokenType::LessThan: return "'<'";
        case TokenType::LessThanOrEqual: return "'<='";
        case TokenType::GreaterThan: return "'>'";
        case TokenType::GreaterThanOrEqual: return "'>='";
        case TokenType::Assignment: return "'='";
        case TokenType::PlusAssign: return "'+='";
        case TokenType::MinusAssign: return "'-='";
        case TokenType::DivideAssign: return "'/='";
        case TokenType::MultiplyAssign: return "'*='";
        case TokenType::ModuloAssign: return "'%='";
        case TokenType::ShiftLeftAssign: return "'<<='";
        case TokenType::ShiftRightAssign: return "'>>='";
        case TokenType::BitAndAssign: return "'&='";
        case TokenType::BitOrAssign: return "'|='";
        case TokenType::BitXorAssign: return "'^='";
        case TokenType::ShiftRight: return "'>>'";
        case TokenType::ShiftLeft: return "'<<'";
        case TokenType::Increment: return "'++'";
        case TokenType::Decrement: return "'--'";
        case TokenType::Colon: return "':'";
        case TokenType::QuestionMark: return "'?'";
        case TokenType::VoidKeyword: return "'void'";
        case TokenType::CharKeyword: return "'char'";
        case TokenType::ShortKeyword: return "'short'";
        case TokenType::IntKeyword: return "'int'";
        case TokenType::LongKeyword: return "'long'";
        case TokenType::FloatKeyword: return "'float'";
        case TokenType::DoubleKeyword: return "'double'";
        case TokenType::SignedKeyword: return "'signed'";
        case TokenType::UnsignedKeyword: return "'unsigned'";
        case TokenType::TypedefKeyword: return "'typedef'";
        case TokenType::ExternKeyword: return "'extern'";
        case TokenType::StaticKeyword: return "'static'";
        case TokenType::AutoKeyword: return "'auto'";
        case TokenType::RegisterKeyword: return "'register'";
        case TokenType::ConstKeyword: return "'const'";
        case TokenType::SizeofKeyword: return "'sizeof'";
        case TokenType::ReturnKeyword: return "'return'";
        case TokenType::BreakKeyword: return "'break'";
        case TokenType::ContinueKeyword: return "'continue'";
        case TokenType::DoKeyword: return "'do'";
        case TokenType::ElseKeyword: return "'else'";
        case TokenType::ForKeyword: return "'for'";
        case TokenType::IfKeyword: return "'if'";
        case TokenType::WhileKeyword: return "'while'";
        case TokenType::OpenSquareBracket: return "'['";
        case TokenType::CloseSquareBracket: return "']'";
        case TokenType::StructKeyword: return "'struct'";
        case TokenType::Dot: return "'.'";
        case TokenType::Arrow: return "'->'";
        case TokenType::SwitchKeyword: return "'switch'";
        case TokenType::CaseKeyword: return "'case'";
        case TokenType::DefaultKeyword: return "'default'";
        case TokenType::UnionKeyword: return "'union'";
        case TokenType::VolatileKeyword: return "'volatile'";
        case TokenType::EnumKeyword: return "'enum'";
        case TokenType::GotoKeyword: return "'goto'";
        case TokenType::Ellipse: return "'...'";
        case TokenType::RestrictKeyword: return "'restrict'";
        case TokenType::InlineKeyword: return "'inline'";
        case TokenType::Pound: return "'#'";
        case TokenType::DoublePound: return "'##'";
        case TokenType::Newline: return "newline";
        case TokenType::UnderlineBool: return "'_Bool'";
        case TokenType::PPNumber: return "preprocessing number";
        case TokenType::Miscellaneous: CLD_UNREACHABLE;
        case TokenType::Backslash: return "backslash";
    }
    CLD_UNREACHABLE;
}

std::string cld::Lexer::tokenValue(cld::Lexer::TokenType tokenType)
{
    switch (tokenType)
    {
        case TokenType::Identifier: return "identifier";
        case TokenType::OpenParentheses: return "(";
        case TokenType::CloseParentheses: return ")";
        case TokenType::OpenBrace: return "{";
        case TokenType::CloseBrace: return "}";
        case TokenType::StringLiteral: return "string literal";
        case TokenType::Literal: return "literal";
        case TokenType::SemiColon: return ";";
        case TokenType::Comma: return ",";
        case TokenType::Minus: return "-";
        case TokenType::BitWiseNegation: return "~";
        case TokenType::LogicalNegation: return "!";
        case TokenType::Plus: return "+";
        case TokenType::Asterisk: return "*";
        case TokenType::Division: return "/";
        case TokenType::Percent: return "%";
        case TokenType::LogicAnd: return "&&";
        case TokenType::LogicOr: return "||";
        case TokenType::Ampersand: return "&";
        case TokenType::BitOr: return "|";
        case TokenType::BitXor: return "^";
        case TokenType::Equal: return "==";
        case TokenType::NotEqual: return "!=";
        case TokenType::LessThan: return "<";
        case TokenType::LessThanOrEqual: return "<=";
        case TokenType::GreaterThan: return ">";
        case TokenType::GreaterThanOrEqual: return ">=";
        case TokenType::Assignment: return "=";
        case TokenType::PlusAssign: return "+=";
        case TokenType::MinusAssign: return "-=";
        case TokenType::DivideAssign: return "/=";
        case TokenType::MultiplyAssign: return "*=";
        case TokenType::ModuloAssign: return "%=";
        case TokenType::ShiftLeftAssign: return "<<=";
        case TokenType::ShiftRightAssign: return ">>=";
        case TokenType::BitAndAssign: return "&=";
        case TokenType::BitOrAssign: return "|=";
        case TokenType::BitXorAssign: return "^=";
        case TokenType::ShiftRight: return ">>";
        case TokenType::ShiftLeft: return "<<";
        case TokenType::Increment: return "++";
        case TokenType::Decrement: return "--";
        case TokenType::Colon: return ":";
        case TokenType::QuestionMark: return "?";
        case TokenType::VoidKeyword: return "void";
        case TokenType::CharKeyword: return "char";
        case TokenType::ShortKeyword: return "short";
        case TokenType::IntKeyword: return "int";
        case TokenType::LongKeyword: return "long";
        case TokenType::FloatKeyword: return "float";
        case TokenType::DoubleKeyword: return "double";
        case TokenType::SignedKeyword: return "signed";
        case TokenType::UnsignedKeyword: return "unsigned";
        case TokenType::TypedefKeyword: return "typedef";
        case TokenType::ExternKeyword: return "extern";
        case TokenType::StaticKeyword: return "static";
        case TokenType::AutoKeyword: return "auto";
        case TokenType::RegisterKeyword: return "register";
        case TokenType::ConstKeyword: return "const";
        case TokenType::SizeofKeyword: return "sizeof";
        case TokenType::ReturnKeyword: return "return";
        case TokenType::BreakKeyword: return "break";
        case TokenType::ContinueKeyword: return "continue";
        case TokenType::DoKeyword: return "do";
        case TokenType::ElseKeyword: return "else";
        case TokenType::ForKeyword: return "for";
        case TokenType::IfKeyword: return "if";
        case TokenType::WhileKeyword: return "while";
        case TokenType::OpenSquareBracket: return "[";
        case TokenType::CloseSquareBracket: return "]";
        case TokenType::StructKeyword: return "struct";
        case TokenType::Dot: return ".";
        case TokenType::Arrow: return "->";
        case TokenType::SwitchKeyword: return "switch";
        case TokenType::CaseKeyword: return "case";
        case TokenType::DefaultKeyword: return "default";
        case TokenType::UnionKeyword: return "union";
        case TokenType::VolatileKeyword: return "volatile";
        case TokenType::EnumKeyword: return "enum";
        case TokenType::GotoKeyword: return "goto";
        case TokenType::Ellipse: return "...";
        case TokenType::RestrictKeyword: return "restrict";
        case TokenType::InlineKeyword: return "inline";
        case TokenType::Pound: return "#";
        case TokenType::DoublePound: return "##";
        case TokenType::Newline: return "Newline";
        case TokenType::UnderlineBool: return "_Bool";
        case TokenType::PPNumber: return "Preprocessing number";
        case TokenType::Miscellaneous: CLD_UNREACHABLE;
        case TokenType::Backslash: return "Backslash";
    }
    CLD_UNREACHABLE;
}

cld::Lexer::Token::Token(std::uint64_t offset, TokenType tokenType, std::string representation, variant value,
                         Type type)
    : m_value(std::move(value)),
      m_representation(std::move(representation)),
      m_offset(offset),
      m_afterPPOffset(offset),
      m_tokenType(tokenType),
      m_type(type)
{
    CLD_ASSERT(!m_representation.empty());
}

std::uint64_t Token::getLine(const cld::SourceObject& sourceObject) const noexcept
{
    return sourceObject.getLineNumber(getOffset());
}

std::uint64_t Token::getPPLine(const cld::PPSourceObject& sourceObject) const noexcept
{
    return sourceObject.getPPLineNumber(getPPOffset());
}

std::uint64_t Token::getColumn(const cld::SourceObject& sourceObject) const noexcept
{
    auto line = sourceObject.getLineNumber(getOffset());
    return getOffset() - sourceObject.getLineStartOffset(line);
}

std::uint64_t Token::getPPColumn(const cld::PPSourceObject& sourceObject) const noexcept
{
    auto line = sourceObject.getPPLineNumber(getPPOffset());
    return getPPOffset() - sourceObject.getPPLineStartOffset(line);
}

std::string cld::Lexer::reconstruct(const SourceObject& sourceObject, TokenIterator begin, TokenIterator end)
{
    if (begin == end)
    {
        return {};
    }
    auto lineNumber = sourceObject.getLineNumber(begin->getOffset());
    return std::string(lineNumber - 1, '\n')
           + std::string(begin->getOffset() - sourceObject.getLineStartOffset(lineNumber), ' ')
           + reconstructTrimmed(sourceObject, begin, end);
}

std::string cld::Lexer::reconstructTrimmed(const SourceObject& sourceObject, TokenIterator begin, TokenIterator end)
{
    std::string result;
    if (begin != end)
    {
        result.reserve((end - 1)->getOffset() + (end - 1)->getLength() - begin->getOffset());
    }
    for (auto curr = begin; curr != end; curr++)
    {
        if (curr != begin)
        {
            auto prev = curr - 1;
            auto currLineNumber = sourceObject.getLineNumber(curr->getOffset());
            auto prevLineNumber = sourceObject.getLineNumber(prev->getOffset() + prev->getLength());
            if (currLineNumber == prevLineNumber)
            {
                result.resize(result.size() + curr->getOffset() - (prev->getOffset() + prev->getLength()), ' ');
            }
            else
            {
                result.resize(result.size() + currLineNumber - prevLineNumber, '\n');
                result.resize(result.size() + curr->getOffset() - sourceObject.getLineStartOffset(currLineNumber), ' ');
            }
        }
        result += curr->getRepresentation();
    }
    return result;
}

std::string cld::Lexer::constructPP(const PPSourceObject& sourceObject, TokenIterator begin, TokenIterator end)
{
    if (begin == end)
    {
        return {};
    }
    auto lineNumber = sourceObject.getPPLineNumber(begin->getPPOffset());
    return std::string(lineNumber - 1, '\n')
           + std::string(begin->getPPOffset() - sourceObject.getPPLineStartOffset(lineNumber), ' ')
           + constructPPTrimmed(sourceObject, begin, end);
}

std::string cld::Lexer::constructPPTrimmed(const PPSourceObject& sourceObject, TokenIterator begin, TokenIterator end)
{
    std::string result;
    if (begin != end)
    {
        result.reserve((end - 1)->getPPOffset() + (end - 1)->getLength() - begin->getPPOffset());
    }
    for (auto curr = begin; curr != end; curr++)
    {
        if (curr != begin)
        {
            auto prev = curr - 1;
            auto currLineNumber = sourceObject.getLineNumber(curr->getPPOffset());
            auto prevLineNumber = sourceObject.getLineNumber(prev->getPPOffset() + prev->getLength());
            if (currLineNumber == prevLineNumber)
            {
                result.resize(result.size() + curr->getPPOffset() - (prev->getPPOffset() + prev->getLength()), ' ');
            }
            else
            {
                result.resize(result.size() + currLineNumber - prevLineNumber, '\n');
                result.resize(result.size() + curr->getPPOffset() - sourceObject.getPPLineStartOffset(currLineNumber),
                              ' ');
            }
        }
        result += curr->getRepresentation();
    }
    return result;
}

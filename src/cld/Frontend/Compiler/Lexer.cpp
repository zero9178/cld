#include "Lexer.hpp"

#include <llvm/Support/ConvertUTF.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/UnicodeCharRanges.h>

#include <cld/Support/ScopeExit.hpp>
#include <cld/Support/Text.hpp>

#include <algorithm>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <unordered_set>

#include <ctre.hpp>

#include "ErrorMessages.hpp"
#include "SourceObject.hpp"

using namespace cld::Lexer;

namespace
{
bool isKeyword(std::string_view characters, const cld::LanguageOptions& languageOptions)
{
    return characters == "auto" || characters == "double" || characters == "int" || characters == "struct"
           || characters == "break" || characters == "else" || characters == "long" || characters == "switch"
           || characters == "case" || characters == "enum" || characters == "register" || characters == "typedef"
           || characters == "char" || characters == "extern" || characters == "return" || characters == "union"
           || characters == "const" || characters == "float" || characters == "short" || characters == "unsigned"
           || characters == "continue" || characters == "for" || characters == "signed" || characters == "void"
           || characters == "default" || characters == "goto" || characters == "sizeof" || characters == "volatile"
           || characters == "restrict" || characters == "do" || characters == "if" || characters == "static"
           || characters == "while" || characters == "inline" || characters == "_Bool" || characters == "__attribute__"
           || characters == "__attribute" || characters == "__inline__" || characters == "__inline"
           || characters == "__extension__" || characters == "__typeof__" || characters == "__typeof"
           || characters == "__asm__" || characters == "__asm" || characters == "__volatile__"
           || characters == "__volatile" || characters == "__const__" || characters == "__const"
           || characters == "__restrict__" || characters == "__restrict" || characters == "__signed__"
           || characters == "__signed"
           || (languageOptions.extension == cld::LanguageOptions::Extension::GNU
               && (characters == "asm" || characters == "typeof"));
}

TokenType charactersToKeyword(std::string_view characters)
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
    if (characters == "const" || characters == "__const__" || characters == "__const")
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
    if (characters == "signed" || characters == "__signed__" || characters == "__signed")
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
    if (characters == "volatile" || characters == "__volatile__" || characters == "__volatile")
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
    if (characters == "restrict" || characters == "__restrict__" || characters == "__restrict")
    {
        return TokenType::RestrictKeyword;
    }
    if (characters == "inline" || characters == "__inline__" || characters == "__inline")
    {
        return TokenType::InlineKeyword;
    }
    if (characters == "_Bool")
    {
        return TokenType::UnderlineBool;
    }
    if (characters == "__attribute__" || characters == "__attribute")
    {
        return TokenType::GNUAttribute;
    }
    if (characters == "__extension__")
    {
        return TokenType::GNUExtension;
    }
    if (characters == "__typeof__" || characters == "typeof" || characters == "__typeof")
    {
        return TokenType::GNUTypeOf;
    }
    if (characters == "__asm__" || characters == "asm" || characters == "__asm")
    {
        return TokenType::GNUASM;
    }
    CLD_UNREACHABLE;
}

#pragma region UnicodeRanges

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

const llvm::sys::UnicodeCharSet C99AllowedIDChar(C99AllowedIDCharRanges);

// C99 6.4.2.1p3: The initial character [of an identifier] shall not be a
// universal character name designating a digit.
// C99 Annex D defines these characters as "Digits".
constexpr llvm::sys::UnicodeCharRange C99DisallowedInitialIDCharRanges[] = {
    {0x0660, 0x0669}, {0x06F0, 0x06F9}, {0x0966, 0x096F}, {0x09E6, 0x09EF}, {0x0A66, 0x0A6F},
    {0x0AE6, 0x0AEF}, {0x0B66, 0x0B6F}, {0x0BE7, 0x0BEF}, {0x0C66, 0x0C6F}, {0x0CE6, 0x0CEF},
    {0x0D66, 0x0D6F}, {0x0E50, 0x0E59}, {0x0ED0, 0x0ED9}, {0x0F20, 0x0F33}};

const llvm::sys::UnicodeCharSet C99DisallowedInitialIDChar(C99DisallowedInitialIDCharRanges);

#pragma endregion UnicodeRanges

std::uint8_t getNumUTF8ForUTF32(std::uint32_t c)
{
    if (c <= 0x007F)
    {
        return 1;
    }
    if (c <= 0x07FF)
    {
        return 2;
    }
    if (c <= 0xFFFF)
    {
        return 3;
    }
    if (c <= 0x10FFFF)
    {
        return 4;
    }
    CLD_UNREACHABLE;
}

template <class Iter>
Iter findUTF8StartByte(Iter begin, Iter end)
{
    return std::find_if(begin, end, [](std::uint8_t c) { return c >= 0b1100'0000 || c < 0b1000'0000; });
}

struct Start;
struct CharacterLiteral;
struct StringLiteral;
struct Text;
struct PreprocessingNumber;
struct MaybeUC;
struct UniversalCharacter;
struct Dot;
struct Punctuation;
struct LineComment;
struct BlockComment;
struct AfterInclude;
struct L;

using StateMachine = std::variant<Start, CharacterLiteral, StringLiteral, Text, PreprocessingNumber, MaybeUC,
                                  UniversalCharacter, LineComment, BlockComment, Dot, Punctuation, AfterInclude, L>;

std::pair<std::uint64_t, std::uint64_t> map(const IntervalMap& intervalMap, std::uint64_t offset)
{
    auto result = std::lower_bound(intervalMap.begin(), intervalMap.end(), offset,
                                   [](auto&& lhs, auto&& rhs) { return std::get<0>(lhs) < rhs; });
    CLD_ASSERT(result != intervalMap.end());
    if (std::get<0>(*result) > offset)
    {
        result--;
    }
    const auto next = result + 1;
    if (next == intervalMap.end())
    {
        CLD_ASSERT(intervalMap.size() >= 2);
        // does this make sense?
        return {std::get<2>(intervalMap[intervalMap.size() - 2]), std::get<2>(intervalMap[intervalMap.size() - 2])};
    }
    const auto denominator = std::get<0>(*next) - std::get<0>(*result);
    CLD_ASSERT(denominator != 0);
    const auto nominatorRange = std::get<2>(*result) - std::get<1>(*result);
    const auto lowBound = (offset - std::get<0>(*result)) * nominatorRange / denominator + std::get<1>(*result);
    const auto highBound = (offset + 1 - std::get<0>(*result)) * nominatorRange / denominator + std::get<1>(*result);
    return {lowBound, highBound};
}

template <class Arg>
decltype(auto) mapArgument(const IntervalMap& intervalMap, Arg&& arg)
{
    using T = std::decay_t<Arg>;
    if constexpr (std::is_integral_v<T>)
    {
        return map(intervalMap, arg).first;
    }
    else if constexpr (cld::IsTupleLike<T>{})
    {
        static_assert(std::tuple_size_v<T> == 3 || std::tuple_size_v<T> == 2);
        if constexpr (std::tuple_size_v<T> == 2)
        {
            return std::apply(
                [&intervalMap](auto&&... args) {
                    return std::tuple(mapArgument(intervalMap, std::forward<decltype(args)>(args))...);
                },
                std::forward<Arg>(arg));
        }
        else
        {
            using T1 = std::tuple_element_t<0, T>;
            using T3 = std::tuple_element_t<2, T>;
            static_assert(
                std::is_base_of_v<cld::Lexer::TokenBase,
                                  std::decay_t<T3>> || std::is_base_of_v<cld::Lexer::TokenBase, std::decay_t<T1>>);
            if constexpr (std::is_base_of_v<cld::Lexer::TokenBase, std::decay_t<T1>>)
            {
                return std::make_tuple(std::get<0>(arg), map(intervalMap, std::get<1>(arg)).first,
                                       map(intervalMap, std::get<2>(arg)).second);
            }
            else
            {
                return std::make_tuple(map(intervalMap, std::get<0>(arg)).first,
                                       map(intervalMap, std::get<1>(arg)).second, std::get<2>(arg));
            }
        }
    }
    else
    {
        return std::forward<Arg>(arg);
    }
}

template <class Diag, class Tuple, std::size_t... ints>
auto mapArguments(const Diag&, const IntervalMap& intervalMap, Tuple&& tuple, std::index_sequence<ints...>)
{
    return std::make_tuple([&tuple, &intervalMap](auto value) {
        constexpr auto i = decltype(value)::value;
        if constexpr ((bool)(Diag::constraints[i] & Diag::LocationConstraint))
        {
            return mapArgument(intervalMap, std::get<i>(tuple));
        }
        else
        {
            return std::get<i>(tuple);
        }
    }(std::integral_constant<std::size_t, ints>{})...);
}

class Context : private cld::SourceInterface
{
    llvm::raw_ostream* m_reporter;
    std::vector<PPToken> m_result;
    std::string_view m_sourceSpace;
    std::string_view m_characterSpace;
    std::uint64_t& m_offset;
    std::vector<std::uint64_t> m_lineStarts;
    const IntervalMap& m_characterToSourceSpace;
    bool m_errorsOccurred = false;
    std::optional<std::uint64_t> m_lastBlockCommentEndPos;
    cld::Source::File m_fakeFile;
    const cld::LanguageOptions& m_languageOptions;

    std::uint64_t getLineNumber(std::uint32_t, std::uint64_t offset) const noexcept override
    {
        auto result = std::lower_bound(m_lineStarts.begin(), m_lineStarts.end(), offset);
        CLD_ASSERT(result != m_lineStarts.end());
        return std::distance(m_lineStarts.begin(), result) + (*result == offset ? 1 : 0);
    }

    std::uint64_t getLineStartOffset(std::uint32_t, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(line - 1 < m_lineStarts.size());
        return m_lineStarts[line - 1];
    }

    std::uint64_t getLineEndOffset(std::uint32_t, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(line - 1 < m_lineStarts.size());
        if (line != m_lineStarts.size())
        {
            return m_lineStarts[line] - 1;
        }
        return m_sourceSpace.size();
    }

    llvm::ArrayRef<cld::Source::File> getFiles() const noexcept override
    {
        return m_fakeFile;
    }

    llvm::ArrayRef<cld::Source::PPRecord> getSubstitutions() const noexcept override
    {
        return {};
    }

    const cld::LanguageOptions& getLanguageOptions() const noexcept override
    {
        return m_languageOptions;
    }

    std::uint64_t getLineNumber(std::uint64_t offset) const noexcept
    {
        return getLineNumber(0, offset);
    }

    std::uint64_t getLineStartOffset(std::uint64_t line) const noexcept
    {
        return getLineStartOffset(0, line);
    }

    std::uint64_t getLineEndOffset(std::uint64_t line) const noexcept
    {
        return getLineEndOffset(0, line);
    }

public:
    std::uint64_t tokenStartOffset;

    Context(std::string_view sourceSpace, const IntervalMap& characterToSourceSpace, std::string_view characterSpace,
            std::uint64_t& offset, std::vector<std::uint64_t> lineStarts, llvm::raw_ostream* reporter,
            std::string_view path, const cld::LanguageOptions& languageOptions) noexcept
        : m_reporter(reporter),
          m_sourceSpace(sourceSpace),
          m_characterSpace(characterSpace),
          m_offset(offset),
          m_lineStarts(std::move(lineStarts)),
          m_characterToSourceSpace(characterToSourceSpace),
          m_fakeFile{cld::to_string(path), cld::to_string(sourceSpace), {}, {}, false},
          m_languageOptions(languageOptions)
    {
    }

    template <class Diag, class Location, class... Args>
    void report(const Diag& diag, Location&& location, Args&&... args)
    {
        if (diag.getSeverity() == cld::Severity::Error)
        {
            m_errorsOccurred = true;
        }
        if (m_reporter)
        {
            std::apply(
                [&](auto&&... args) {
                    if constexpr (std::is_integral_v<std::decay_t<Location>>)
                    {
                        *m_reporter << diag.args(map(m_characterToSourceSpace, location).first, *this,
                                                 std::forward<decltype(args)>(args)...);
                        return;
                    }
                    if constexpr (cld::IsTupleLike<std::decay_t<Location>>{})
                    {
                        if constexpr (std::tuple_size_v<std::decay_t<Location>> == 2)
                        {
                            using T1 = std::decay_t<std::tuple_element_t<0, std::decay_t<Location>>>;
                            using T2 = std::decay_t<std::tuple_element_t<1, std::decay_t<Location>>>;
                            if constexpr (std::is_integral_v<T1> && std::is_integral_v<T2>)
                            {
                                *m_reporter << diag.args(map(std::get<0>(location), map(std::get<1>(location))), *this,
                                                         std::forward<decltype(args)>(args)...);
                            }
                        }
                    }
                    *m_reporter << diag.args(mapArgument(m_characterToSourceSpace, location), *this,
                                             std::forward<decltype(args)>(args)...);
                },
                mapArguments(diag, m_characterToSourceSpace, std::forward_as_tuple(std::forward<Args>(args)...),
                             std::index_sequence_for<Args...>{}));
        }
    }

    [[nodiscard]] std::uint64_t getOffset() const
    {
        return m_offset;
    }

    template <class F>
    void withOffset(std::uint64_t offset, F&& f)
    {
        auto exit = cld::ScopeExit([offset = m_offset, this]() { m_offset = offset; });
        m_offset = offset;
        std::forward<F>(f)();
    }

    [[nodiscard]] const std::vector<cld::Lexer::PPToken>& getResult() const noexcept
    {
        return m_result;
    }

    [[nodiscard]] std::vector<cld::Lexer::PPToken>& getResult() noexcept
    {
        return m_result;
    }

    void push(std::uint64_t start, std::uint64_t end, TokenType tokenType, std::string value = {})
    {
        bool leadingWhitespace = m_lastBlockCommentEndPos == start;
        if (!leadingWhitespace)
        {
            // Find the first byte of a UTF-8 Sequence
            auto iter = findUTF8StartByte(std::make_reverse_iterator(m_characterSpace.begin() + start),
                                          m_characterSpace.rend());
            if (iter != m_characterSpace.rend())
            {
                std::uint32_t codePoint;
                auto* sourceStart = m_characterSpace.data() + (iter.base() - m_characterSpace.begin() - 1);
                auto result =
                    llvm::convertUTF8Sequence(reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                              reinterpret_cast<const llvm::UTF8*>(m_characterSpace.data() + start),
                                              &codePoint, llvm::strictConversion);
                if (result == llvm::conversionOK)
                {
                    leadingWhitespace = codePoint != '\n' && cld::isWhitespace(codePoint);
                }
            }
        }
        auto sourceStart = map(m_characterToSourceSpace, start).first;
        auto sourceEnd = map(m_characterToSourceSpace, end - 1).second;
        auto& newToken = m_result.emplace_back(tokenType, sourceStart, sourceEnd - sourceStart, start, end - start, 0,
                                               0, std::move(value));
        newToken.setLeadingWhitespace(leadingWhitespace);
    }

    void push(TokenType tokenType, std::string value = {})
    {
        push(tokenStartOffset, m_offset, tokenType, std::move(value));
    }

    void push(std::uint64_t diff, TokenType tokenType, std::string value = {})
    {
        push(tokenStartOffset, m_offset - diff, tokenType, std::move(value));
    }

    [[nodiscard]] std::string_view view(std::uint64_t startOffset, std::uint64_t endOffset) const
    {
        return m_characterSpace.substr(startOffset, endOffset - startOffset);
    }

    [[nodiscard]] std::string_view currentView() const
    {
        return view(tokenStartOffset, m_offset);
    }

    bool errorsOccurred() const
    {
        return m_errorsOccurred;
    }

    void setLastBlockCommentEndPos(std::uint64_t lastBlockCommentEndPos)
    {
        m_lastBlockCommentEndPos = lastBlockCommentEndPos;
    }
};

std::uint32_t hexToValue(const std::string& value)
{
    char* end = const_cast<char*>(value.data() + value.size());
    auto result = std::strtoul(value.data(), &end, 16);
    CLD_ASSERT(*end == '\0');
    return result;
}

constexpr auto UTF32_MAX = 0x10FFFF;

/**
 * Callee responsible for right format
 * @param value string that either consist of 4 or 8 hex digits
 * @return Unicode value
 */
template <class T, class PosArg>
std::optional<std::uint32_t> universalCharacterToValue(std::string_view value, std::uint64_t location, PosArg&& arg,
                                                       T& context)
{
    CLD_ASSERT(value.size() == 4 || value.size() == 8);
    auto result = hexToValue(std::string(value.begin(), value.end()));
    if (result < 0xA0)
    {
        if (result != '$' && result != '@' && result != '`')
        {
            context.report(cld::Errors::Lexer::INVALID_UC_VALUE_MUSTNT_BE_LESS_THAN_A0, location,
                           std::forward<PosArg>(arg));
            return {};
        }
    }
    else if (result > UTF32_MAX)
    {
        context.report(cld::Errors::Lexer::INVALID_UC_VALUE_MUST_FIT_IN_UTF32, location, std::forward<PosArg>(arg));
        return {};
    }
    else if ((result >= 0xD800 && result <= 0xDFFF))
    {
        context.report(cld::Errors::Lexer::INVALID_UC_VALUE_MUSTNT_BE_IN_RANGE, location, std::forward<PosArg>(arg));
        return {};
    }
    return result;
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
    std::string characters{};

    std::pair<StateMachine, bool> advance(char c, Context& context);
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

    std::pair<StateMachine, bool> advance(char c, Context& context);
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
            if (context.getResult().size() >= 2
                && context.getResult()[context.getResult().size() - 2].getTokenType() == TokenType::Pound
                && context.getResult().back().getTokenType() == TokenType::Identifier
                && context.getResult()[context.getResult().size() - 1].getValue() == "include")
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
            return PreprocessingNumber{{static_cast<char>(c)}};
        }
        case '<':
        {
            if (context.getResult().size() >= 2
                && context.getResult()[context.getResult().size() - 2].getTokenType() == TokenType::Pound
                && context.getResult()[context.getResult().size() - 1].getTokenType() == TokenType::Identifier
                && context.getResult()[context.getResult().size() - 1].getValue() == "include")
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
        case '\n': context.push(context.getOffset() - 1, context.getOffset(), TokenType::Newline); return *this;
        default:
        {
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                || (C99AllowedIDChar.contains(c) && !C99DisallowedInitialIDChar.contains(c)))
            {
                std::string buffer(4, ' ');
                auto* start = buffer.data();
                llvm::ConvertCodePointToUTF8(c, start);
                buffer.resize(std::distance(buffer.data(), start));
                return Text{std::move(buffer)};
            }
            if (cld::isWhitespace(c))
            {
                return *this;
            }
            if (!llvm::sys::unicode::isPrintable(c))
            {
                std::string buffer = "\\U";
                buffer.reserve(10);
                llvm::raw_string_ostream ss(buffer);
                ss << llvm::format_hex_no_prefix(c, 8);
                ss.flush();
                auto size = getNumUTF8ForUTF32(c);
                context.report(cld::Errors::Lexer::NON_PRINTABLE_CHARACTER_N, context.getOffset() - size, buffer,
                               std::pair{context.getOffset() - size, context.getOffset()});
            }
            else
            {
                context.push(context.getOffset() - getNumUTF8ForUTF32(c), context.getOffset(),
                             TokenType::Miscellaneous);
            }
            return *this;
        }
    }
}

std::pair<StateMachine, bool> CharacterLiteral::advance(char c, Context& context)
{
    if (c == '\''
        && (std::find_if_not(characters.rbegin(), characters.rend(), [](char c) { return c == '\\'; })
            - characters.rbegin())
                   % 2
               == 0)
    {
        context.push(TokenType::Literal, characters);
        return {Start{}, true};
    }
    characters += c;
    if ((c == '\n' || c == '\r'))
    {
        context.push(TokenType::Literal, characters);
        return {Start{}, false};
    }
    return {std::move(*this), true};
}

std::pair<StateMachine, bool> StringLiteral::advance(char c, Context& context)
{
    if (c == '"'
        && (std::find_if_not(characters.rbegin(), characters.rend(), [](char c) { return c == '\\'; })
            - characters.rbegin())
                   % 2
               == 0)
    {
        context.push(TokenType::StringLiteral, characters);
        return {Start{}, true};
    }
    characters += c;
    if ((c == '\n' || c == '\r'))
    {
        context.push(TokenType::StringLiteral, characters);
        return {Start{}, false};
    }
    return {std::move(*this), true};
}

std::pair<StateMachine, bool> L::advance(char c, Context&) noexcept
{
    if (c == '"')
    {
        return {StringLiteral{true, {}}, true};
    }
    if (c == '\'')
    {
        return {CharacterLiteral{true, {}}, true};
    }
    if (c == '\\')
    {
        return {MaybeUC{std::make_unique<StateMachine>(L{})}, true};
    }

    return {Text{"L"}, false};
}

std::pair<StateMachine, bool> Text::advance(std::uint32_t c, Context& context)
{
    if (c == '\\')
    {
        return {MaybeUC{std::make_unique<StateMachine>(std::move(*this))}, true};
    }

    if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') && !(c >= '0' && c <= '9') && c != '_'
        && !C99AllowedIDChar.contains(c))
    {
        context.push(getNumUTF8ForUTF32(c), TokenType::Identifier, std::move(characters));
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
    constexpr std::uint8_t toLower = 32;
    if (c == '\\')
    {
        return {MaybeUC{std::make_unique<StateMachine>(std::move(*this))}, true};
    }

    if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') && !(c >= '0' && c <= '9') && c != '_' && c != '.'
        && (((characters.back() | toLower) != 'e' && (characters.back() | toLower) != 'p') || (c != '+' && c != '-'))
        && !C99AllowedIDChar.contains(c))
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
        if (!prevState)
        {
            return {UniversalCharacter{c == 'U'}, true};
        }
        if (std::holds_alternative<Text>(*prevState))
        {
            return {UniversalCharacter{c == 'U', cld::get<Text>(std::move(*prevState))}, true};
        }
        if (std::holds_alternative<L>(*prevState))
        {
            return {UniversalCharacter{c == 'U', Text{"L"}}, true};
        }
        if (std::holds_alternative<PreprocessingNumber>(*prevState))
        {
            return {UniversalCharacter{c == 'U', cld::get<PreprocessingNumber>(std::move(*prevState))}, true};
        }
        CLD_UNREACHABLE;
    }

    if (prevState)
    {
        context.withOffset(context.getOffset() - getNumUTF8ForUTF32(c),
                           [&] { cld::match(*prevState, [&](auto&& value) { value.advance(' ', context); }); });
    }
    context.push(context.getOffset() - getNumUTF8ForUTF32(c) - 1, context.getOffset() - getNumUTF8ForUTF32(c),
                 TokenType::Backslash);
    return {Start{}, false};
}

std::pair<StateMachine, bool> UniversalCharacter::advance(char c, Context& context)
{
    if (!(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F'))
    {
        std::size_t result = context.view(context.tokenStartOffset, context.getOffset() - 1).rfind('\\');
        CLD_ASSERT(result != std::string_view::npos);
        result += context.tokenStartOffset;
        // TODO: Warning that the universal character is treated as backslash followed by identifier
        if (suspHolder)
        {
            context.withOffset(result + 1,
                               [&] { cld::match(*suspHolder, [&](auto&& value) { value.advance(' ', context); }); });
        }
        context.push(result, result + 1, TokenType::Backslash);
        context.tokenStartOffset++;
        return {Text{(big ? "U" : "u") + characters}, false};
    }

    characters.push_back(c);
    if (characters.size() != (big ? 8 : 4))
    {
        return {std::move(*this), true};
    }

    std::size_t ucStart = context.currentView().rfind('\\');
    CLD_ASSERT(ucStart != std::string_view::npos);
    ucStart += context.tokenStartOffset;
    auto result = universalCharacterToValue(characters, ucStart, std::pair{ucStart, context.getOffset()}, context);
    if (!result)
    {
        return {Start{}, true};
    }
    // The Universal Character is a valid character from here on
    if (suspHolder && std::holds_alternative<PreprocessingNumber>(*suspHolder))
    {
        // In an PP Number we only go through the whole mechanism to check if the universal character is invalid.
        // Otherwise different tokens need to be generated
        auto pp = cld::get<PreprocessingNumber>(std::move(*suspHolder));
        pp.characters += context.currentView().substr(ucStart - context.tokenStartOffset);
        return {std::move(pp), true};
    }

    // suspHolder is either empty or a Text from here on

    // According to 6.4.2.1 Paragraph 3 of the C99 Standard these restrictions apply only to an identifier.
    // This means even if a identifier-nondigit element is allowed in a PP Number it doesn't have to meet those
    // requirements per my interpretation
    if (!C99AllowedIDChar.contains(*result) || !(suspHolder || !C99DisallowedInitialIDChar.contains(*result)))
    {
        // The value of the universal character cannot continue the Text that we have or is not able to start a new
        // Text. We therefore push Text if it's in suspHolder, push a backslash and push the rest as a new Text

        if (suspHolder)
        {
            // TODO: Warning that the universal character is treated as backslash followed by identifier
            context.withOffset(ucStart + 1,
                               [&] { cld::match(*suspHolder, [&](auto&& value) { value.advance(' ', context); }); });
            context.push(ucStart, ucStart + 1, TokenType::Backslash);
            context.tokenStartOffset++;
            return {Text{(big ? "U" : "u") + characters}, true};
        }

        context.report(cld::Errors::Lexer::UNEXPECTED_CHARACTER, ucStart, std::pair{ucStart, context.getOffset()});
        return {Start{}, true};
    }
    // This line is needed because a text can start with a universal character. Luckily a Preprocessing number can't
    // So we only reach this line if suspHolder is either empty or contains a Text
    auto newText = suspHolder ? cld::get<Text>(std::move(*suspHolder)) : Text{};
    newText.characters.resize(newText.characters.size() + 4);
    auto* start = newText.characters.data() + newText.characters.size() - 4;
    llvm::ConvertCodePointToUTF8(*result, start);
    newText.characters.resize(newText.characters.size()
                              - std::distance(start, newText.characters.data() + newText.characters.size()));
    return {std::move(newText), true};
}

std::pair<StateMachine, bool> Dot::advance(char c, Context& context)
{
    if (c == '.')
    {
        if (++dotCount != 3)
        {
            return {std::move(*this), true};
        }
        context.push(TokenType::Ellipse);
        return {Start{}, true};
    }

    if (c >= '0' && c <= '9')
    {
        if (dotCount == 2)
        {
            context.push(context.tokenStartOffset, context.tokenStartOffset + 1, TokenType::Dot);
        }
        return {PreprocessingNumber{"."}, false};
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

std::pair<StateMachine, bool> AfterInclude::advance(char c, Context& context)
{
    if (c != delimiter && c != '\n')
    {
        characters += c;
        return {std::move(*this), true};
    }

    if (c != '\n')
    {
        context.push(TokenType::StringLiteral, std::move(characters));
        return {Start{}, true};
    }

    // c is '\n' at this point

    if (delimiter == '"')
    {
        // If the delimiter is a " it means that the whole token would otherwise be interpreted as a string literal
        // if there wasn't a include directive in front. Therefore no macro expansion could make this a valid include
        // directive

        context.report(cld::Errors::Lexer::UNTERMINATED_INCLUDE_DIRECTIVE, context.tokenStartOffset,
                       std::pair{context.tokenStartOffset, context.getOffset() - 1});

        // if the delimiter is a < however then subsequent tokens will later be retokenized in the preprocessor
        // which might form a valid include after macro substitution
    }
    context.push(1, TokenType::StringLiteral, std::move(characters));
    return {Start{}, false};
}

std::pair<StateMachine, bool> LineComment::advance(char c, Context&) noexcept
{
    if (c == '\n')
    {
        return {Start{}, false};
    }
    return {std::move(*this), true};
}

StateMachine BlockComment::advance(char c, Context& context) noexcept
{
    if (lastChar && *lastChar == '*' && c == '/')
    {
        context.setLastBlockCommentEndPos(context.getOffset());
        return Start{};
    }
    lastChar = c;
    return *this;
}

template <class T>
struct [[maybe_unused]] FirstArgOfMethod;

template <class R, class C, class U, class... Args>
struct [[maybe_unused]] FirstArgOfMethod<R (C::*)(U, Args...)>
{
    using Type = U;
};

template <class R, class C, class U, class... Args>
struct [[maybe_unused]] FirstArgOfMethod<R (C::*)(U, Args...) noexcept>
{
    using Type = U;
};

template <class R, class U, class... Args>
struct [[maybe_unused]] FirstArgOfMethod<R (*)(U, Args...)>
{
    using Type = U;
};

template <class R, class U, class... Args>
struct [[maybe_unused]] FirstArgOfMethod<R (*)(U, Args...) noexcept>
{
    using Type = U;
};
} // namespace

constexpr static auto pattern = ctll::fixed_string{"(\\?\\?/|\\\\)[\n]|\\?\\?[=()'<!>\\-/]"};

cld::PPSourceObject cld::Lexer::tokenize(std::string source, LanguageOptions languageOptions,
                                         llvm::raw_ostream* reporter, bool* errorsOccured, std::string_view sourcePath)
{
    if (errorsOccured)
    {
        *errorsOccured = false;
    }

    constexpr static std::string_view UTF8_BOM = "\xEF\xBB\xBF";
    if (source.size() >= 3 && source.substr(0, 3) == UTF8_BOM)
    {
        source = source.substr(3);
    }

    {
        std::string::size_type pos = 0;
        while ((pos = source.find("\r\n", pos)) != source.npos)
        {
            source.erase(pos, 1);
        }
    }

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
    starts.push_back(offset + 1);
    offset = 0;

    std::string charactersSpace;
    IntervalMap characterToSourceSpace;
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
                characterToSourceSpace.emplace_back(charactersSpace.size(), pos - prefix.size(), pos);
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
                characterToSourceSpace.emplace_back(charactersSpace.size(), pos, pos + view.size());
                charactersSpace += result->second;
            }
            stringView.remove_prefix(prefix.size() + view.size());
        }
        if (!stringView.empty())
        {
            characterToSourceSpace.emplace_back(charactersSpace.size(), source.size() - stringView.size(),
                                                source.size());
            charactersSpace += stringView;
        }
        characterToSourceSpace.emplace_back(charactersSpace.size(), 0, 0);
    }

    Context context(source, characterToSourceSpace, charactersSpace, offset, starts, reporter, sourcePath,
                    languageOptions);
    const auto* end = charactersSpace.data() + charactersSpace.size();
    for (const auto* iter = charactersSpace.data(); iter != end;)
    {
        std::uint64_t step = 1;
        std::uint64_t prevOffset = offset;
        auto visitor = [iter, &step, &stateMachine, &context, &offset, end, prevOffset](auto&& state) -> bool {
            using T = std::decay_t<decltype(state)>;
            constexpr bool needsCodepoint =
                std::is_same_v<std::uint32_t, typename FirstArgOfMethod<decltype(&T::advance)>::Type>;
            std::conditional_t<needsCodepoint, std::uint32_t, char> c{};
            if constexpr (needsCodepoint)
            {
                llvm::UTF32 result;
                const auto* start = iter;
                if (llvm::convertUTF8Sequence(reinterpret_cast<const llvm::UTF8**>(&start),
                                              reinterpret_cast<const llvm::UTF8*>(end), &result, llvm::strictConversion)
                    != llvm::conversionOK)
                {
                    step = getNumBytesForUTF8(start, end);
                    context.report(Errors::Lexer::INVALID_UTF8_SEQUENCE, offset, std::pair{offset, offset + step});
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
    bool needNewline = cld::match(
        stateMachine,
        [&context](BlockComment&) {
            context.report(Errors::Lexer::UNTERMINATED_BLOCK_COMMENT, context.tokenStartOffset,
                           std::pair{context.tokenStartOffset, context.getOffset()});
            return false;
        },
        [&context](StringLiteral&) {
            context.report(Errors::Lexer::UNTERMINATED_STRING_LITERAL, context.tokenStartOffset,
                           std::pair{context.tokenStartOffset, context.getOffset()});
            return false;
        },
        [&context](CharacterLiteral&) {
            context.report(Errors::Lexer::UNTERMINATED_CHARACTER_LITERAL, context.tokenStartOffset,
                           std::pair{context.tokenStartOffset, context.getOffset()});
            return false;
        },
        [&context](AfterInclude&) {
            context.report(Errors::Lexer::UNTERMINATED_INCLUDE_DIRECTIVE, context.tokenStartOffset,
                           std::pair{context.tokenStartOffset, context.getOffset()});
            return false;
        },
        [](Start&) { return false; }, [](LineComment&) { return false; }, [](auto&&) { return true; });
    if (needNewline)
    {
        offset++;
        characterToSourceSpace.back() = {charactersSpace.size(), source.size(), source.size()};
        characterToSourceSpace.emplace_back(charactersSpace.size() + 1, 0, 0);
        auto visitor = [offset, &context, &stateMachine](auto&& state) -> bool {
            if constexpr (std::is_same_v<std::decay_t<decltype(state)>, Start>)
            {
                return false;
            }
            std::uint32_t c = '\n';
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
                        context.tokenStartOffset = offset - 1;
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
                        context.tokenStartOffset = offset - 1;
                    }
                }
                return !proceed;
            }
        };
        while (cld::match(stateMachine, visitor))
            ;
    }

    if (errorsOccured)
    {
        *errorsOccured = context.errorsOccurred();
    }

    return PPSourceObject(
        context.getResult(),
        {Source::File{to_string(sourcePath), std::move(source), std::move(starts), context.getResult(), false}},
        languageOptions, {}, {{std::move(characterToSourceSpace)}});
}

[[nodiscard]] std::string_view cld::Lexer::TokenBase::getRepresentation(const SourceInterface& sourceObject) const
{
    return std::string_view(sourceObject.getFiles()[(std::uint64_t)m_fileID].source).substr(m_offset, m_length);
}

[[nodiscard]] std::uint64_t cld::Lexer::TokenBase::getLine(const SourceInterface& sourceObject) const noexcept
{
    return sourceObject.getLineNumber(m_fileID, m_offset);
}

[[nodiscard]] std::uint64_t cld::Lexer::TokenBase::getColumn(const SourceInterface& sourceObject) const noexcept
{
    auto line = sourceObject.getLineNumber(m_fileID, m_offset);
    return m_offset - sourceObject.getLineStartOffset(m_fileID, line) + 1;
}

namespace
{
struct ConversionContext
{
    llvm::raw_ostream* reporter;
    const cld::PPSourceInterface& sourceInterface;
    const cld::Lexer::PPToken& token;
    bool* errorsOccurred;

    template <class Diag, class Location, class... Args>
    void report(const Diag& diag, Location&& location, Args&&... args) const
    {
        if (diag.getSeverity() == cld::Severity::Error && errorsOccurred)
        {
            *errorsOccurred = true;
        }
        if (reporter)
        {
            std::apply(
                [&](auto&&... args) {
                    if constexpr (std::is_integral_v<std::decay_t<Location>>)
                    {
                        *reporter << diag.args(
                            std::forward_as_tuple(
                                token, map(sourceInterface.getIntervalMaps()[token.getFileId()], location).first),
                            sourceInterface, std::forward<decltype(args)>(args)...);
                        return;
                    }
                    if constexpr (cld::IsTupleLike<std::decay_t<Location>>{})
                    {
                        if constexpr (std::tuple_size_v<std::decay_t<Location>> == 2)
                        {
                            using T1 = std::decay_t<std::tuple_element_t<0, std::decay_t<Location>>>;
                            using T2 = std::decay_t<std::tuple_element_t<1, std::decay_t<Location>>>;
                            if constexpr (std::is_integral_v<T1> && std::is_integral_v<T2>)
                            {
                                *reporter << diag.args(std::forward_as_tuple(token, map(std::get<0>(location)),
                                                                             map(std::get<1>(location))),
                                                       sourceInterface, std::forward<decltype(args)>(args)...);
                            }
                        }
                    }
                    *reporter << diag.args(mapArgument(sourceInterface.getIntervalMaps()[token.getFileId()], location),
                                           sourceInterface, std::forward<decltype(args)>(args)...);
                },
                mapArguments(diag, sourceInterface.getIntervalMaps()[token.getFileId()],
                             std::forward_as_tuple(std::forward<Args>(args)...), std::index_sequence_for<Args...>{}));
        }
    }
};

std::optional<std::uint32_t> escapeCharToValue(char escape, std::uint64_t backslash, ConversionContext& context)
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
            context.report(cld::Errors::Lexer::EXPECTED_CHARACTER_AFTER_BACKSLASH, backslash + 1,
                           std::forward_as_tuple(context.token, backslash, backslash + 2));

            return {};
        }
        default:
        {
            context.report(cld::Errors::Lexer::INVALID_ESCAPE_SEQUENCE_N, backslash + 1, std::string("\\") + escape,
                           std::forward_as_tuple(context.token, backslash, backslash + 1));

            return {};
        }
    }
}

std::uint32_t octalToValue(const std::string& value)
{
    char* end = const_cast<char*>(value.data() + value.size());
    auto result = std::strtoul(value.data(), &end, 8);
    CLD_ASSERT(*end == '\0');
    return result;
}

enum class Literal
{
    CharLiteral,
    StringLiteral,
};

std::pair<std::vector<llvm::UTF32>, bool> processCharacters(std::string_view characters, ConversionContext& context,
                                                            bool wide, Literal literalType)
{
    const std::uint32_t largestCharacter = [&context, wide]() -> std::uint32_t {
        std::uint8_t size = 1;
        if (wide)
        {
            switch (context.sourceInterface.getLanguageOptions().wcharUnderlyingType)
            {
                case cld::LanguageOptions::WideCharType ::Int:
                    size = context.sourceInterface.getLanguageOptions().sizeOfInt;
                    break;
                case cld::LanguageOptions::WideCharType ::UnsignedShort:
                    size = context.sourceInterface.getLanguageOptions().sizeOfShort;
                    break;
            }
        }
        return 0xFFFFFFFFu >> (32 - 8 * size);
    }();
    std::vector<llvm::UTF32> result;
    result.resize(characters.size());
    auto* resultStart = result.data();
    auto* resultEnd = result.data() + result.size();

    const auto* end = characters.data() + characters.size();
    bool errorOccured = false;
    for (const auto* iter = characters.data(); iter != end;)
    {
        auto offset = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data());
        if (*iter == '\n')
        {
            if (literalType == Literal::CharLiteral)
            {
                context.report(cld::Errors::Lexer::NEWLINE_IN_CHARACTER_LITERAL_USE_BACKLASH_N, offset,
                               std::forward_as_tuple(context.token, offset));
            }
            else
            {
                context.report(cld::Errors::Lexer::NEWLINE_IN_STRING_LITERAL_USE_BACKLASH_N, offset,
                               std::forward_as_tuple(context.token, offset));
            }
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
                auto invalidStart = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (start - characters.data());
                auto invalidEnd = invalidStart + cld::getNumBytesForUTF8(start, end) - 1;
                context.report(cld::Errors::Lexer::INVALID_UTF8_SEQUENCE, invalidStart,
                               std::make_tuple(context.token, invalidStart, invalidEnd));
                errorOccured = true;
            }
            continue;
        }
        // Normally we could assume that if *iter == '\\' that iter + 1 != end. That is because if *iter == '\\' and
        // iter + 1 == end the last character would be '\\' and following that '\'' or '\"'.
        // Therefore the character literal wouldn't have ended and we wouldn't be here.

        // But if we didn't go through a preprocessor a #include followed by a string literal could be present and
        // therefore contain such a character. For now we will just break at such an occurrence but in the future
        // we might simply want to disable the special handling of string literals after #include for API consumers
        // that do not want to preprocess
        if (iter + 1 == end)
        {
            break;
        }

        if (iter[1] == 'u' || iter[1] == 'U')
        {
            bool big = iter[1] == 'U';
            iter += 2;
            if (iter == end
                || (!(*iter >= '0' && *iter <= '9') && !(*iter >= 'a' && *iter <= 'f')
                    && !(*iter >= 'A' && *iter <= 'F')))
            {
                // First character followed after \u or \U is not a hex digit or its the end of string
                // Let's assume the user thought \u might be an escape character
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.report(cld::Errors::Lexer::INVALID_ESCAPE_SEQUENCE_N, start,
                               std::string_view(big ? "\\U" : "\\u"),
                               std::forward_as_tuple(context.token, start, start + 1));
                errorOccured = true;
                continue;
            }

            const auto* hexStart = iter;
            const auto* hexEnd = std::find_if(
                hexStart, hexStart + std::min<std::size_t>(std::distance(hexStart, end), big ? 8 : 4),
                [](char c) { return !(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F'); });
            if (std::distance(hexStart, hexEnd) != (big ? 8 : 4))
            {
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data()) - 2;
                context.report(
                    cld::Errors::Lexer::INVALID_UC_EXPECTED_N_MORE_DIGITS, start,
                    (big ? 8 : 4) - std::distance(hexStart, hexEnd),
                    std::forward_as_tuple(context.token, start, start + std::distance(hexStart, hexEnd) + 1));
                errorOccured = true;
                iter = hexEnd;
                continue;
            }
            auto uc = universalCharacterToValue(
                {hexStart, static_cast<std::size_t>(hexEnd - hexStart)}, offset,
                std::forward_as_tuple(context.token, offset, offset + 1 + (big ? 8 : 4)), context);
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
        if (iter[1] == 'x')
        {
            iter += 2;
            const auto* lastHex = std::find_if(iter, end, [](char c) {
                return !(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F');
            });
            if (lastHex == iter)
            {
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.report(cld::Errors::Lexer::AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED, start,
                               std::forward_as_tuple(context.token, start, start + 1));
                errorOccured = true;
                continue;
            }

            llvm::APInt input;
            llvm::StringRef(iter, static_cast<std::size_t>(lastHex - iter)).getAsInteger(16, input);
            auto rhs = llvm::APInt(input.getBitWidth(), UTF32_MAX);
            if (input.getBitWidth() >= 21 && input.ugt(rhs))
            {
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.report(cld::Errors::Lexer::INVALID_HEX_VALUE_MUST_FIT_IN_UTF32, start,
                               std::forward_as_tuple(context.token, start, start + 1 + lastHex - iter));
                errorOccured = true;
                iter = lastHex;
                continue;
            }
            auto value = input.getZExtValue();
            if (value >= 0xD800 && value <= 0xDFFF)
            {
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.report(cld::Errors::Lexer::INVALID_HEX_VALUE_MUSTNT_BE_IN_RANGE, start,
                               std::forward_as_tuple(context.token, start, start + 1 + lastHex - iter));
                errorOccured = true;
                iter = lastHex;
                continue;
            }
            if (value > largestCharacter)
            {
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data() - 2);
                context.report(cld::Errors::Lexer::CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE, start,
                               std::forward_as_tuple(context.token, start, start + 1 + lastHex - iter));
                errorOccured = true;
                iter = lastHex;
                continue;
            }
            *resultStart = value;
            resultStart++;

            iter = lastHex;
        }
        else if (iter[1] >= '0' && iter[1] <= '9')
        {
            // We take 8 and 9 here as well to tell the user its an invalid octal instead of an invalid simple
            // escape sequence
            iter++;
            const auto* lastOctal = std::find_if(iter, iter + std::min<std::size_t>(3, std::distance(iter, end)),
                                                 [](char c) { return c < '0' || c > '7'; });
            if (lastOctal == iter)
            {
                // First character is 8 or 9. That's why we didn't encounter a single octal digit.
                // Also since there must be at least one character after \, lastOctal is definitely not end
                // here.
                context.report(cld::Errors::Lexer::INVALID_OCTAL_CHARACTER, offset, std::string_view(lastOctal, 1),
                               std::forward_as_tuple(context.token, offset, offset + 1));
                errorOccured = true;
                continue;
            }

            auto value = octalToValue({iter, static_cast<std::size_t>(lastOctal - iter)});
            if (value > largestCharacter)
            {
                auto start = context.token.getCharSpaceOffset() + (wide ? 2 : 1) + (iter - characters.data() - 1);
                context.report(cld::Errors::Lexer::CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE, start,
                               std::forward_as_tuple(context.token, start, start + lastOctal - iter));
                errorOccured = true;
                iter = lastOctal;
                continue;
            }
            *resultStart = value;
            resultStart++;

            iter = lastOctal;
        }
        else
        {
            // Escape sequence or illegal escape
            auto character = escapeCharToValue(iter[1], offset, context);
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

    if (literalType == Literal::CharLiteral)
    {
        for (auto* iter = result.data(); iter != resultStart; iter++)
        {
            if (*iter > largestCharacter)
            {
                context.report(cld::Errors::Lexer::CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE, context.token, context.token);
                errorOccured = true;
            }
        }
    }
    result.resize(std::distance(result.data(), resultStart));
    return {result, errorOccured};
}

template <class T, class... Args>
std::pair<CToken::ValueType, CToken::Type> castInteger(std::uint64_t integer,
                                                       std::array<CToken::Type, sizeof...(Args) + 1> types)
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
        std::array<CToken::Type, sizeof...(Args)> second;
        std::copy(types.begin() + 1, types.end(), second.begin());
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

std::optional<std::pair<CToken::ValueType, CToken::Type>>
    processNumber(const char* begin, const char* end, std::uint64_t beginLocation, ConversionContext& context)
{
    CLD_ASSERT(std::distance(begin, end) >= 1);
    // If the number is just "0x", treat the x as a suffix instead of as a hex prefix
    bool isHex = std::distance(begin, end) > 2 && *begin == '0' && (*(begin + 1) == 'x' || *(begin + 1) == 'X')
                 && ((*(begin + 2) >= '0' && *(begin + 2) <= '9') || (*(begin + 2) >= 'a' && *(begin + 2) <= 'f')
                     || (*(begin + 2) >= 'A' && *(begin + 2) <= 'F'));
    cld::MaxVector<char, 22> legalValues(10);
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
    const auto* suffixBegin = std::find_if(begin + (isHex ? 2 : 0), end, searchFunction);
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
        const auto* prev = suffixBegin;
        // The exponent of a hex floating point number is actually normal decimal digits not hex
        if (isHex)
        {
            legalValues.resize(10);
        }
        suffixBegin = std::find_if(suffixBegin, end, searchFunction);
        if (prev == suffixBegin)
        {
            auto result = findUTF8StartByte(std::make_reverse_iterator(prev), std::make_reverse_iterator(begin));
            context.report(cld::Errors::Lexer::EXPECTED_DIGITS_AFTER_EXPONENT,
                           context.token.getCharSpaceOffset() + context.token.getCharSpaceLength(),
                           context.token.getCharSpaceOffset() + result.base() - begin - 1, context.token);
            errorsOccurred = true;
        }
    }
    else if (isHex && isFloat)
    {
        context.report(cld::Errors::Lexer::BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT,
                       context.token.getCharSpaceOffset() + context.token.getCharSpaceLength(), context.token);
        errorsOccurred = true;
    }

    bool isHexOrOctal = isHex;
    if (!isHex && !isFloat && *begin == '0')
    {
        isHexOrOctal = true;
        const auto* result = std::find_if(begin, suffixBegin, [](char c) { return c >= '8'; });
        while (result != suffixBegin)
        {
            errorsOccurred = true;
            auto arrowBegin = beginLocation + std::distance(begin, result);
            context.report(cld::Errors::Lexer::INVALID_OCTAL_CHARACTER, arrowBegin, std::string(1, *result),
                           std::forward_as_tuple(context.token, arrowBegin));
            result = std::find_if(result + 1, suffixBegin, [](char c) { return c >= '8'; });
        }
    }

    auto suffix = std::string(suffixBegin, std::distance(suffixBegin, end));
    std::unordered_set<std::string_view> set;
    if (!isFloat)
    {
        set = {"u",  "U",  "ul",  "Ul",  "uL",  "UL",  "uLL", "ULL", "ull", "Ull", "lu", "lU",
               "Lu", "LU", "LLu", "LLU", "llu", "llU", "l",   "L",   "ll",  "LL",  ""};
    }
    else
    {
        set = {"f", "l", "F", "L", ""};
    }
    if (set.count(suffix) == 0)
    {
        auto arrowBegin = beginLocation + std::distance(begin, suffixBegin);
        context.report(cld::Errors::Lexer::INVALID_LITERAL_SUFFIX, arrowBegin, suffix,
                       std::forward_as_tuple(context.token, arrowBegin, arrowBegin + suffix.size()));
        return {};
    }

    if (errorsOccurred)
    {
        return {};
    }
    if (!isFloat)
    {
        bool unsignedConsidered =
            isHexOrOctal || std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u' || c == 'U'; });
        std::string number(begin, suffixBegin);
        llvm::APInt test;
        llvm::StringRef(number).getAsInteger(0, test);
        if (test.getActiveBits() > (unsignedConsidered ? 64u : 63u))
        {
            context.report(cld::Errors::Lexer::INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE, beginLocation, context.token);
            return {};
        }
        char* endPtr;
        auto integer = std::strtoull(number.data(), &endPtr, 0);
        CLD_ASSERT(*endPtr == '\0');
        if (suffix.empty())
        {
            switch (context.sourceInterface.getLanguageOptions().sizeOfInt)
            {
                case 2:
                    if (isHexOrOctal)
                    {
                        if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                        {
                            return castInteger<std::int16_t, std::uint16_t, std::int32_t, std::uint32_t, std::int64_t,
                                               std::uint64_t>(integer,
                                                              {CToken::Type::Int, CToken::Type::UnsignedInt,
                                                               CToken::Type::Long, CToken::Type::UnsignedLong,
                                                               CToken::Type::LongLong, CToken::Type::UnsignedLongLong});
                        }
                        return castInteger<std::int16_t, std::uint16_t, std::int64_t, std::uint64_t>(
                            integer, {CToken::Type::Int, CToken::Type::UnsignedInt, CToken::Type::Long,
                                      CToken::Type::UnsignedLong});
                    }
                    if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                    {
                        return castInteger<std::int16_t, std::int32_t, std::int64_t>(
                            integer, {CToken::Type::Int, CToken::Type::Long, CToken::Type::LongLong});
                    }
                    return castInteger<std::int16_t, std::int64_t>(integer, {CToken::Type::Int, CToken::Type::Long});
                case 4:
                    if (isHexOrOctal)
                    {
                        if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                        {
                            return castInteger<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t>(
                                integer, {CToken::Type::Int, CToken::Type::UnsignedInt, CToken::Type::LongLong,
                                          CToken::Type::UnsignedLongLong});
                        }
                        return castInteger<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t>(
                            integer, {CToken::Type::Int, CToken::Type::UnsignedInt, CToken::Type::Long,
                                      CToken::Type::UnsignedLong});
                    }
                    if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                    {
                        return castInteger<std::int32_t, std::int64_t>(integer,
                                                                       {CToken::Type::Int, CToken::Type::LongLong});
                    }
                    return castInteger<std::int32_t, std::int64_t>(integer, {CToken::Type::Int, CToken::Type::Long});
                case 8:
                    if (isHexOrOctal)
                    {
                        return castInteger<std::int64_t, std::uint64_t>(integer,
                                                                        {CToken::Type::Int, CToken::Type::UnsignedInt});
                    }
                    return castInteger<std::int64_t>(integer, {CToken::Type::Int});
                default: CLD_UNREACHABLE;
            }
        }
        else if (suffix == "u" || suffix == "U")
        {
            switch (context.sourceInterface.getLanguageOptions().sizeOfInt)
            {
                case 2:
                    if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                    {
                        return castInteger<std::uint16_t, std::uint32_t, std::uint64_t>(
                            integer,
                            {CToken::Type::UnsignedInt, CToken::Type::UnsignedLong, CToken::Type::UnsignedLongLong});
                    }
                    return castInteger<std::uint16_t, std::uint64_t>(
                        integer, {CToken::Type::UnsignedInt, CToken::Type::UnsignedLong});
                case 4:
                    if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                    {
                        return castInteger<std::uint32_t, std::uint64_t>(
                            integer, {CToken::Type::UnsignedInt, CToken::Type::UnsignedLongLong});
                    }
                    return castInteger<std::uint32_t, std::uint64_t>(
                        integer, {CToken::Type::UnsignedInt, CToken::Type::UnsignedLong});
                case 8: return castInteger<std::uint64_t>(integer, {CToken::Type::UnsignedInt});
                default: CLD_UNREACHABLE;
            }
        }
        else if (suffix == "L" || suffix == "l")
        {
            if (isHexOrOctal)
            {
                if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
                {
                    return castInteger<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t>(
                        integer, {CToken::Type::Long, CToken::Type::UnsignedLong, CToken::Type::LongLong,
                                  CToken::Type::UnsignedLongLong});
                }
                return castInteger<std::int64_t, std::uint64_t>(integer,
                                                                {CToken::Type::Long, CToken::Type::UnsignedLong});
            }
            if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
            {
                return castInteger<std::int32_t, std::int64_t>(integer, {CToken::Type::Long, CToken::Type::LongLong});
            }
            return castInteger<std::int64_t>(integer, {CToken::Type::Long});
        }
        else if (suffix.size() == 2
                 && std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u' || c == 'U'; })
                 && std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'l' || c == 'L'; }))
        {
            if (context.sourceInterface.getLanguageOptions().sizeOfLong == 4)
            {
                return castInteger<std::uint32_t, std::uint64_t>(
                    integer, {CToken::Type::UnsignedLong, CToken::Type::UnsignedLongLong});
            }
            return castInteger<std::uint64_t>(integer, {CToken::Type::UnsignedLong});
        }
        else if (suffix == "ll" || suffix == "LL")
        {
            if (isHexOrOctal)
            {
                return castInteger<std::int64_t, std::uint64_t>(
                    integer, {CToken::Type::LongLong, CToken::Type::UnsignedLongLong});
            }
            return castInteger<std::int64_t>(integer, {CToken::Type::LongLong});
        }
        else if (suffix.size() == 3
                 && std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u' || c == 'U'; })
                 && (suffix.find("LL") != std::string_view::npos || suffix.find("ll") != std::string_view::npos))
        {
            return castInteger<std::uint64_t>(integer, {CToken::Type::UnsignedLongLong});
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
            llvm::APFloat number(llvm::APFloat::IEEEdouble());
            auto result = number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
            if (!result)
            {
                CLD_UNREACHABLE;
            }
            return {{std::move(number), CToken::Type::Double}};
        }
        if (suffix == "f" || suffix == "F")
        {
            llvm::APFloat number(llvm::APFloat::IEEEsingle());
            auto result = number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
            if (!result)
            {
                CLD_UNREACHABLE;
            }
            return {{std::move(number), CToken::Type::Float}};
        }
        if (suffix == "l" || suffix == "L")
        {
            switch (context.sourceInterface.getLanguageOptions().sizeOfLongDoubleBits)
            {
                case 64:
                {
                    llvm::APFloat number(llvm::APFloat::IEEEdouble());
                    auto result = number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
                    if (!result)
                    {
                        CLD_UNREACHABLE;
                    }
                    return {{std::move(number), CToken::Type::LongDouble}};
                }
                case 80:
                {
                    llvm::APFloat number(llvm::APFloat::x87DoubleExtended());
                    auto result = number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
                    if (!result)
                    {
                        CLD_UNREACHABLE;
                    }
                    return {{std::move(number), CToken::Type::LongDouble}};
                }
                case 128:
                {
                    llvm::APFloat number(llvm::APFloat::IEEEquad());
                    auto result = number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
                    if (!result)
                    {
                        CLD_UNREACHABLE;
                    }
                    return {{std::move(number), CToken::Type::LongDouble}};
                }
                default: CLD_UNREACHABLE;
            }
        }
    }
    CLD_UNREACHABLE;
}
} // namespace

cld::CSourceObject cld::Lexer::toCTokens(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter,
                                         bool* errorsOccurred)
{
    return CSourceObject(toCTokens(sourceObject.data().data(), sourceObject.data().data() + sourceObject.data().size(),
                                   sourceObject, reporter, errorsOccurred),
                         sourceObject.getFiles(), sourceObject.getLanguageOptions(), sourceObject.getSubstitutions());
}

std::vector<cld::Lexer::CToken> cld::Lexer::toCTokens(PPTokenIterator begin, PPTokenIterator end,
                                                      const PPSourceInterface& sourceInterface,
                                                      llvm::raw_ostream* reporter, bool* errorsOccurred)
{
    if (errorsOccurred)
    {
        *errorsOccurred = false;
    }
    std::vector<CToken> result;
    for (const auto* iter = begin; iter != end; iter++)
    {
        switch (iter->getTokenType())
        {
            case TokenType::Miscellaneous:
                if (errorsOccurred)
                {
                    *errorsOccurred = true;
                }
                if (reporter)
                {
                    *reporter << Errors::Lexer::UNEXPECTED_CHARACTER.args(*iter, sourceInterface, *iter);
                }
                break;
            case TokenType::Pound:
            case TokenType::DoublePound:
            case TokenType::Backslash:
                if (errorsOccurred)
                {
                    *errorsOccurred = true;
                }
                if (reporter)
                {
                    *reporter << Errors::Lexer::STRAY_N_IN_PROGRAM.args(*iter, sourceInterface, *iter);
                }
                break;
            case TokenType::Newline: break;
            case TokenType::Identifier:
                // Identifiers need to always be valid
                if (isKeyword(iter->getValue(), sourceInterface.getLanguageOptions()))
                {
                    result.emplace_back(charactersToKeyword(iter->getValue()), iter->getOffset(), iter->getLength(),
                                        iter->getFileId(), iter->getMacroId());
                    break;
                }
                result.emplace_back(TokenType::Identifier, iter->getOffset(), iter->getLength(), iter->getFileId(),
                                    iter->getMacroId(), cld::to_string(iter->getValue()));
                break;
            case TokenType::PPNumber:
            {
                ConversionContext context{reporter, sourceInterface, *iter, errorsOccurred};
                auto number = processNumber(iter->getValue().data(), iter->getValue().data() + iter->getValue().size(),
                                            iter->getCharSpaceOffset(), context);
                if (number)
                {
                    result.emplace_back(TokenType::Literal, iter->getOffset(), iter->getLength(), iter->getFileId(),
                                        iter->getMacroId(), std::move(number->first), number->second);
                }
                break;
            }
            case TokenType::StringLiteral:
            {
                auto maybeToken = parseStringLiteral(*iter, sourceInterface, reporter);
                if (!maybeToken)
                {
                    if (errorsOccurred)
                    {
                        *errorsOccurred = true;
                    }
                }
                else
                {
                    result.push_back(std::move(*maybeToken));
                }
                break;
            }
            case TokenType::Literal:
            {
                ConversionContext context{reporter, sourceInterface, *iter, errorsOccurred};
                bool wide = iter->getRepresentation(sourceInterface)[0] == 'L';
                auto [chars, errorOccured] = processCharacters(iter->getValue(), context, wide, Literal::CharLiteral);
                if (chars.empty())
                {
                    if (!errorOccured)
                    {
                        errorOccured = true;
                        if (reporter)
                        {
                            *reporter << Errors::Lexer::CHARACTER_LITERAL_CANNOT_BE_EMPTY.args(*iter, sourceInterface,
                                                                                               *iter);
                        }
                    }
                    break;
                }

                if (reporter && chars.size() > 1)
                {
                    *reporter << Warnings::Lexer::DISCARDING_ALL_BUT_FIRST_CHARACTER.args(*iter, sourceInterface,
                                                                                          *iter);
                }

                if (wide)
                {
                    result.emplace_back(
                        TokenType::Literal, iter->getOffset(), iter->getLength(), iter->getFileId(), iter->getMacroId(),
                        llvm::APSInt(llvm::APInt((sourceInterface.getLanguageOptions().wcharUnderlyingType
                                                          == LanguageOptions::WideCharType::UnsignedShort ?
                                                      sourceInterface.getLanguageOptions().sizeOfShort :
                                                      sourceInterface.getLanguageOptions().sizeOfInt)
                                                     * 8,
                                                 chars[0]),
                                     sourceInterface.getLanguageOptions().wcharUnderlyingType
                                         == LanguageOptions::WideCharType::UnsignedShort),
                        sourceInterface.getLanguageOptions().wcharUnderlyingType == LanguageOptions::WideCharType::Int ?
                            CToken::Int :
                            CToken::UnsignedShort);
                }
                else
                {
                    result.emplace_back(TokenType::Literal, iter->getOffset(), iter->getLength(), iter->getFileId(),
                                        iter->getMacroId(),
                                        llvm::APSInt(llvm::APInt(sourceInterface.getLanguageOptions().sizeOfInt * 8,
                                                                 static_cast<std::uint8_t>(chars[0]), true),
                                                     false),
                                        CToken::Int);
                }
                break;
            }
            default:
                result.emplace_back(iter->getTokenType(), iter->getOffset(), iter->getLength(), iter->getFileId(),
                                    iter->getMacroId());
        }
    }
    return result;
}

std::optional<cld::Lexer::CToken> cld::Lexer::parseStringLiteral(const PPToken& ppToken,
                                                                 const PPSourceInterface& sourceInterface,
                                                                 llvm::raw_ostream* reporter)
{
    CLD_ASSERT(ppToken.getTokenType() == cld::Lexer::TokenType::StringLiteral);
    bool errorsOccurred = false;
    ConversionContext context{reporter, sourceInterface, ppToken, &errorsOccurred};
    bool wide = ppToken.getRepresentation(sourceInterface)[0] == 'L';
    auto [chars, errorOccured] = processCharacters(ppToken.getValue(), context, wide, Literal::StringLiteral);
    if (errorOccured)
    {
        return {};
    }
    if (!wide)
    {
        const auto* start = chars.data();
        std::vector<llvm::UTF8> utf8(chars.size() * 4);
        auto* dest = utf8.data();
        if (llvm::ConvertUTF32toUTF8(&start, start + chars.size(), &dest, dest + utf8.size(), llvm::strictConversion)
            != llvm::conversionOK)
        {
            CLD_UNREACHABLE;
        }

        if (wide)
        {
            return CToken(TokenType::StringLiteral, ppToken.getOffset(), ppToken.getLength(), ppToken.getFileId(),
                          ppToken.getMacroId(), NonCharString{{utf8.data(), dest}, NonCharString::Wide});
        }
        return CToken(TokenType::StringLiteral, ppToken.getOffset(), ppToken.getLength(), ppToken.getFileId(),
                      ppToken.getMacroId(), std::string(utf8.data(), dest));
    }

    std::uint8_t size = 0;
    switch (sourceInterface.getLanguageOptions().wcharUnderlyingType)
    {
        case LanguageOptions::WideCharType ::UnsignedShort:
            size = sourceInterface.getLanguageOptions().sizeOfShort;
            break;
        case LanguageOptions::WideCharType ::Int: size = sourceInterface.getLanguageOptions().sizeOfInt; break;
    }

    switch (size)
    {
        case 4:
        {
            return CToken(TokenType::StringLiteral, ppToken.getOffset(), ppToken.getLength(), ppToken.getFileId(),
                          ppToken.getMacroId(), NonCharString{chars, NonCharString::Wide});
            break;
        }
        case 2:
        {
            const auto* start = chars.data();
            std::vector<llvm::UTF16> utf16(chars.size() * 2);
            auto* dest = utf16.data();
            if (llvm::ConvertUTF32toUTF16(&start, start + chars.size(), &dest, dest + utf16.size(),
                                          llvm::strictConversion)
                != llvm::conversionOK)
            {
                CLD_UNREACHABLE;
            }
            return CToken(TokenType::StringLiteral, ppToken.getOffset(), ppToken.getLength(), ppToken.getFileId(),
                          ppToken.getMacroId(), NonCharString{{utf16.data(), dest}, NonCharString::Wide});
        }
        default: CLD_UNREACHABLE;
    }
}

std::string_view cld::Lexer::tokenName(cld::Lexer::TokenType tokenType)
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
        case TokenType::Backslash: return "'\\'";
        case TokenType::GNUAttribute: return "'__attribute__'";
        case TokenType::GNUExtension: return "'__extension__'";
        case TokenType::GNUASM: return "'asm'";
        case TokenType::GNUTypeOf: return "'typeof'";
    }
    CLD_UNREACHABLE;
}

std::string_view cld::Lexer::tokenValue(cld::Lexer::TokenType tokenType)
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
        case TokenType::Newline: return "newline";
        case TokenType::UnderlineBool: return "_Bool";
        case TokenType::PPNumber: return "preprocessing number";
        case TokenType::Miscellaneous: CLD_UNREACHABLE;
        case TokenType::Backslash: return "\\";
        case TokenType::GNUAttribute: return "__attribute__";
        case TokenType::GNUExtension: return "__extension__";
        case TokenType::GNUASM: return "asm";
        case TokenType::GNUTypeOf: return "typeof";
    }
    CLD_UNREACHABLE;
}

std::string cld::Lexer::normalizeSpelling(std::string_view tokenSpelling)
{
    std::string result;
    for (auto& iter : ctre::range<pattern>(tokenSpelling))
    {
        auto view = iter.view();
        result += tokenSpelling.substr(0, view.data() - tokenSpelling.data());
        if (!iter.get<1>())
        {
            static const std::unordered_map<std::string_view, char> mapping = {
                {"?\?=", '#'}, {"?\?(", '['}, {"?\?/", '\\'}, {"?\?)", ']'}, {"?\?'", '^'},
                {"?\?<", '{'}, {"?\?!", '|'}, {"?\?>", '}'},  {"?\?-", '~'}};
            auto rep = mapping.find(view);
            CLD_ASSERT(rep != mapping.end());
            result += rep->second;
        }
        tokenSpelling.remove_prefix(view.data() + view.size() - tokenSpelling.data());
    }
    result += tokenSpelling;
    return result;
}

bool cld::Lexer::needsWhitespaceInBetween(TokenType left, TokenType right) noexcept
{
    switch (left)
    {
        case TokenType::Identifier:
        case TokenType::VoidKeyword:
        case TokenType::CharKeyword:
        case TokenType::ShortKeyword:
        case TokenType::IntKeyword:
        case TokenType::LongKeyword:
        case TokenType::FloatKeyword:
        case TokenType::DoubleKeyword:
        case TokenType::SignedKeyword:
        case TokenType::UnsignedKeyword:
        case TokenType::TypedefKeyword:
        case TokenType::ExternKeyword:
        case TokenType::StaticKeyword:
        case TokenType::AutoKeyword:
        case TokenType::RegisterKeyword:
        case TokenType::ConstKeyword:
        case TokenType::RestrictKeyword:
        case TokenType::SizeofKeyword:
        case TokenType::VolatileKeyword:
        case TokenType::InlineKeyword:
        case TokenType::ReturnKeyword:
        case TokenType::BreakKeyword:
        case TokenType::ContinueKeyword:
        case TokenType::DoKeyword:
        case TokenType::ElseKeyword:
        case TokenType::ForKeyword:
        case TokenType::IfKeyword:
        case TokenType::WhileKeyword:
        case TokenType::StructKeyword:
        case TokenType::SwitchKeyword:
        case TokenType::CaseKeyword:
        case TokenType::DefaultKeyword:
        case TokenType::UnionKeyword:
        case TokenType::EnumKeyword:
        case TokenType::GotoKeyword:
        case TokenType::UnderlineBool:
        case TokenType::GNUAttribute:
            switch (right)
            {
                case TokenType::Backslash:
                case TokenType::Identifier:
                case TokenType::VoidKeyword:
                case TokenType::CharKeyword:
                case TokenType::ShortKeyword:
                case TokenType::IntKeyword:
                case TokenType::LongKeyword:
                case TokenType::FloatKeyword:
                case TokenType::DoubleKeyword:
                case TokenType::SignedKeyword:
                case TokenType::UnsignedKeyword:
                case TokenType::TypedefKeyword:
                case TokenType::ExternKeyword:
                case TokenType::StaticKeyword:
                case TokenType::AutoKeyword:
                case TokenType::RegisterKeyword:
                case TokenType::ConstKeyword:
                case TokenType::RestrictKeyword:
                case TokenType::SizeofKeyword:
                case TokenType::VolatileKeyword:
                case TokenType::InlineKeyword:
                case TokenType::ReturnKeyword:
                case TokenType::BreakKeyword:
                case TokenType::ContinueKeyword:
                case TokenType::DoKeyword:
                case TokenType::ElseKeyword:
                case TokenType::ForKeyword:
                case TokenType::IfKeyword:
                case TokenType::WhileKeyword:
                case TokenType::StructKeyword:
                case TokenType::SwitchKeyword:
                case TokenType::CaseKeyword:
                case TokenType::DefaultKeyword:
                case TokenType::UnionKeyword:
                case TokenType::EnumKeyword:
                case TokenType::GotoKeyword:
                case TokenType::UnderlineBool:
                case TokenType::GNUAttribute:
                case TokenType::StringLiteral:
                case TokenType::Literal:
                case TokenType::PPNumber: return true;
                default: break;
            }
            break;
        case TokenType::Minus:
            switch (right)
            {
                case TokenType::Minus:
                case TokenType::GreaterThan:
                case TokenType::ShiftRight:
                case TokenType::Decrement:
                case TokenType::Arrow:
                case TokenType::ShiftRightAssign:
                case TokenType::GreaterThanOrEqual:
                case TokenType::Equal:
                case TokenType::Assignment:
                case TokenType::MinusAssign: return true;
                default: break;
            }
            break;
        case TokenType::LogicalNegation:
            switch (right)
            {
                case TokenType::Assignment:
                case TokenType::Equal: return true;
                default: break;
            }
            break;
        case TokenType::Plus:
            switch (right)
            {
                case TokenType::Plus:
                case TokenType::Increment:
                case TokenType::Equal:
                case TokenType::Assignment:
                case TokenType::PlusAssign: return true;
                default: break;
            }
            break;
        case TokenType::Asterisk:
            switch (right)
            {
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::Division:
            switch (right)
            {
                case TokenType::Division:
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::Percent:
            switch (right)
            {
                case TokenType::GreaterThan:
                case TokenType::Colon:
                case TokenType::GreaterThanOrEqual:
                case TokenType::ShiftRight:
                case TokenType::ShiftRightAssign:
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::Ampersand:
            switch (right)
            {
                case TokenType::Ampersand:
                case TokenType::LogicAnd:
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::BitOr:
            switch (right)
            {
                case TokenType::BitOr:
                case TokenType::LogicOr:
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::BitXor:
            switch (right)
            {
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::LessThan:
            switch (right)
            {
                case TokenType::Colon:
                case TokenType::LessThan:
                case TokenType::LessThanOrEqual:
                case TokenType::ShiftLeft:
                case TokenType::ShiftLeftAssign:
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::GreaterThan:
            switch (right)
            {
                case TokenType::GreaterThan:
                case TokenType::GreaterThanOrEqual:
                case TokenType::ShiftRight:
                case TokenType::ShiftRightAssign:
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::Assignment:
            switch (right)
            {
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::ShiftRight:
            switch (right)
            {
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::ShiftLeft:
            switch (right)
            {
                case TokenType::Equal:
                case TokenType::Assignment: return true;
                default: break;
            }
            break;
        case TokenType::Colon:
            switch (right)
            {
                case TokenType::GreaterThan:
                case TokenType::GreaterThanOrEqual:
                case TokenType::ShiftRight:
                case TokenType::ShiftRightAssign: return true;
                default: break;
            }
            break;
        case TokenType::Dot:
            switch (right)
            {
                case TokenType::Ellipse:
                case TokenType::Dot: return true;
                default: break;
            }
            break;
        case TokenType::PPNumber:
            switch (right)
            {
                case TokenType::Identifier:
                case TokenType::VoidKeyword:
                case TokenType::CharKeyword:
                case TokenType::ShortKeyword:
                case TokenType::IntKeyword:
                case TokenType::LongKeyword:
                case TokenType::FloatKeyword:
                case TokenType::DoubleKeyword:
                case TokenType::SignedKeyword:
                case TokenType::UnsignedKeyword:
                case TokenType::TypedefKeyword:
                case TokenType::ExternKeyword:
                case TokenType::StaticKeyword:
                case TokenType::AutoKeyword:
                case TokenType::RegisterKeyword:
                case TokenType::ConstKeyword:
                case TokenType::RestrictKeyword:
                case TokenType::SizeofKeyword:
                case TokenType::VolatileKeyword:
                case TokenType::InlineKeyword:
                case TokenType::ReturnKeyword:
                case TokenType::BreakKeyword:
                case TokenType::ContinueKeyword:
                case TokenType::DoKeyword:
                case TokenType::ElseKeyword:
                case TokenType::ForKeyword:
                case TokenType::IfKeyword:
                case TokenType::WhileKeyword:
                case TokenType::StructKeyword:
                case TokenType::SwitchKeyword:
                case TokenType::CaseKeyword:
                case TokenType::DefaultKeyword:
                case TokenType::UnionKeyword:
                case TokenType::EnumKeyword:
                case TokenType::GotoKeyword:
                case TokenType::UnderlineBool:
                case TokenType::GNUAttribute:
                case TokenType::Plus:
                case TokenType::Minus:
                case TokenType::Increment:
                case TokenType::Decrement:
                case TokenType::PlusAssign:
                case TokenType::MinusAssign:
                case TokenType::Arrow:
                case TokenType::PPNumber: return true;
                default: break;
            }
            break;
        case TokenType::Backslash:
            switch (right)
            {
                case TokenType::Identifier:
                case TokenType::UnsignedKeyword:
                case TokenType::UnionKeyword: return true;
                default: break;
            }
            break;
        case TokenType::Pound:
            switch (right)
            {
                case TokenType::Pound:
                case TokenType::DoublePound: return true;
                default: break;
            }
            break;
        default: break;
    }
    return false;
}

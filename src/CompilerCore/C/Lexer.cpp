#include "Lexer.hpp"

#include <llvm/Support/ConvertUTF.h>
#include <llvm/Support/Format.h>
#include <llvm/Support/Unicode.h>
#include <llvm/Support/UnicodeCharRanges.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <cassert>
#include <numeric>
#include <regex>
#include <string_view>

#include "ErrorMessages.hpp"
#include "SourceObject.hpp"

using namespace OpenCL::Lexer;

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
        using namespace OpenCL::Lexer;
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
        OPENCL_UNREACHABLE;
    }

    //    Token charactersToNumber(std::ostream* reporter, const std::string& literal, std::uint64_t line,
    //                             std::uint64_t column, const std::string& lineText, bool isPreprocessor)
    //    {
    //        if (literal.find('.') == std::string::npos
    //            && ((literal.size() >= 2 && (literal.substr(0, 2) == "0x" || literal.substr(0, 2) == "0X"))
    //                || (literal.find('e') == std::string::npos && literal.find('E') == std::string::npos))
    //            && literal.find('p') == std::string::npos && literal.find('P') == std::string::npos)
    //        {
    //            static std::regex numbers("(0x)?[0-9a-fA-F]+");
    //            bool isHexOrOctal = literal[0] == '0';
    //            std::smatch match;
    //            std::regex_search(literal, match, numbers);
    //            std::string filtered = match[0];
    //
    //            std::string suffix = literal.substr(filtered.size(), literal.size() - filtered.size());
    //            auto originalSuffix = suffix;
    //            if (std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u'; }))
    //            {
    //                auto erase = std::remove(suffix.begin(), suffix.end(), 'u');
    //                suffix.erase(erase, suffix.end());
    //
    //                char* endptr = nullptr;
    //                std::uint64_t number = std::strtoull(filtered.c_str(), &endptr, 0);
    //                if (isPreprocessor || suffix == "ll" || suffix == "LL")
    //                {
    //                    return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
    //                                                literal, number);
    //                }
    //                else
    //                {
    //                    if (!suffix.empty() && suffix != "l" && suffix != "L")
    //                    {
    //                        reportError(reporter,
    //                                    OpenCL::ErrorMessages::Lexer::INVALID_INTEGER_LITERAL_SUFFIX.args(originalSuffix),
    //                                    line, column, lineText,
    //                                    {{column + literal.size() - originalSuffix.size(), column + literal.size()}});
    //                    }
    //                    if (number > std::numeric_limits<std::uint32_t>::max())
    //                    {
    //                        return OpenCL::Lexer::Token(line, column, literal.size(),
    //                        OpenCL::Lexer::TokenType::Literal,
    //                                                    literal, number);
    //                    }
    //                    else
    //                    {
    //                        return OpenCL::Lexer::Token(line, column, literal.size(),
    //                        OpenCL::Lexer::TokenType::Literal,
    //                                                    literal, static_cast<std::uint32_t>(number));
    //                    }
    //                }
    //            }
    //            else
    //            {
    //                char* endptr = nullptr;
    //                std::uint64_t number = std::strtoull(filtered.c_str(), &endptr, 0);
    //                if (isPreprocessor || suffix == "ll" || suffix == "LL")
    //                {
    //                    if (isHexOrOctal && number >
    //                    static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
    //                    {
    //                        return OpenCL::Lexer::Token(line, column, literal.size(),
    //                        OpenCL::Lexer::TokenType::Literal,
    //                                                    literal, number);
    //                    }
    //                    else
    //                    {
    //                        return OpenCL::Lexer::Token(line, column, literal.size(),
    //                        OpenCL::Lexer::TokenType::Literal,
    //                                                    literal, static_cast<std::int64_t>(number));
    //                    }
    //                }
    //                else
    //                {
    //                    if (!suffix.empty() && suffix != "l" && suffix != "L")
    //                    {
    //                        reportError(reporter,
    //                                    OpenCL::ErrorMessages::Lexer::INVALID_INTEGER_LITERAL_SUFFIX.args(originalSuffix),
    //                                    line, column, lineText,
    //                                    {{column + literal.size() - originalSuffix.size(), column + literal.size()}});
    //                    }
    //                    if (number > static_cast<std::uint64_t>(std::numeric_limits<std::int32_t>::max()))
    //                    {
    //                        if (isHexOrOctal && number <= std::numeric_limits<std::uint32_t>::max())
    //                        {
    //                            return OpenCL::Lexer::Token(line, column, literal.size(),
    //                            OpenCL::Lexer::TokenType::Literal,
    //                                                        literal, static_cast<std::uint32_t>(number));
    //                        }
    //                        else if (isHexOrOctal
    //                                 && number > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
    //                        {
    //                            return OpenCL::Lexer::Token(line, column, literal.size(),
    //                            OpenCL::Lexer::TokenType::Literal,
    //                                                        literal, number);
    //                        }
    //                        else
    //                        {
    //                            return OpenCL::Lexer::Token(line, column, literal.size(),
    //                            OpenCL::Lexer::TokenType::Literal,
    //                                                        literal, static_cast<std::int64_t>(number));
    //                        }
    //                    }
    //                    else
    //                    {
    //                        return OpenCL::Lexer::Token(line, column, literal.size(),
    //                        OpenCL::Lexer::TokenType::Literal,
    //                                                    literal, static_cast<std::int32_t>(number));
    //                    }
    //                }
    //            }
    //        }
    //        else
    //        {
    //            char* endptr = nullptr;
    //            if (literal.back() == 'f' || literal.back() == 'F')
    //            {
    //                auto filtered = literal.substr(0, literal.size() - 1);
    //                float number = std::strtof(filtered.c_str(), &endptr);
    //                if (endptr != filtered.c_str() + filtered.size())
    //                {
    //                    reportError(reporter,
    //                    OpenCL::ErrorMessages::Lexer::INVALID_FLOATING_POINT_LITERAL.args(literal),
    //                                line, column, lineText, {{column, column + literal.size()}});
    //                }
    //                return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
    //                literal,
    //                                            number);
    //            }
    //            else
    //            {
    //                if (literal.size() >= 2 && literal[0] == '0' && std::tolower(literal[1]) == 'x'
    //                    && std::none_of(literal.begin(), literal.end(), [](char c) { return c == 'p' || c == 'P'; }))
    //                {
    //                    reportError(reporter,
    //                    OpenCL::ErrorMessages::Lexer::BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT,
    //                                line, column, lineText, {{column, column + literal.size()}});
    //                }
    //                double number = std::strtod(literal.c_str(), &endptr);
    //                if (endptr != literal.c_str() + literal.size())
    //                {
    //                    reportError(reporter,
    //                    OpenCL::ErrorMessages::Lexer::INVALID_FLOATING_POINT_LITERAL.args(literal),
    //                                line, column, lineText, {{column, column + literal.size()}});
    //                }
    //                return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
    //                literal,
    //                                            number);
    //            }
    //        }
    //    }

    llvm::raw_ostream& operator<<(llvm::raw_ostream& os, std::string_view sv)
    {
        return os.write(sv.data(), sv.size());
    }

    int charWidth(int UCS)
    {
        if (!llvm::sys::unicode::isPrintable(UCS))
            return llvm::sys::unicode::ErrorNonPrintableCharacter;

        // Sorted list of non-spacing and enclosing combining mark intervals as
        // defined in "3.6 Combination" of
        // http://www.unicode.org/versions/Unicode6.2.0/UnicodeStandard-6.2.pdf
        static const llvm::sys::UnicodeCharRange CombiningCharacterRanges[] = {
            {0x0300, 0x036F},   {0x0483, 0x0489},   {0x0591, 0x05BD},   {0x05BF, 0x05BF},   {0x05C1, 0x05C2},
            {0x05C4, 0x05C5},   {0x05C7, 0x05C7},   {0x0610, 0x061A},   {0x064B, 0x065F},   {0x0670, 0x0670},
            {0x06D6, 0x06DC},   {0x06DF, 0x06E4},   {0x06E7, 0x06E8},   {0x06EA, 0x06ED},   {0x0711, 0x0711},
            {0x0730, 0x074A},   {0x07A6, 0x07B0},   {0x07EB, 0x07F3},   {0x0816, 0x0819},   {0x081B, 0x0823},
            {0x0825, 0x0827},   {0x0829, 0x082D},   {0x0859, 0x085B},   {0x08E4, 0x08FE},   {0x0900, 0x0902},
            {0x093A, 0x093A},   {0x093C, 0x093C},   {0x0941, 0x0948},   {0x094D, 0x094D},   {0x0951, 0x0957},
            {0x0962, 0x0963},   {0x0981, 0x0981},   {0x09BC, 0x09BC},   {0x09C1, 0x09C4},   {0x09CD, 0x09CD},
            {0x09E2, 0x09E3},   {0x0A01, 0x0A02},   {0x0A3C, 0x0A3C},   {0x0A41, 0x0A42},   {0x0A47, 0x0A48},
            {0x0A4B, 0x0A4D},   {0x0A51, 0x0A51},   {0x0A70, 0x0A71},   {0x0A75, 0x0A75},   {0x0A81, 0x0A82},
            {0x0ABC, 0x0ABC},   {0x0AC1, 0x0AC5},   {0x0AC7, 0x0AC8},   {0x0ACD, 0x0ACD},   {0x0AE2, 0x0AE3},
            {0x0B01, 0x0B01},   {0x0B3C, 0x0B3C},   {0x0B3F, 0x0B3F},   {0x0B41, 0x0B44},   {0x0B4D, 0x0B4D},
            {0x0B56, 0x0B56},   {0x0B62, 0x0B63},   {0x0B82, 0x0B82},   {0x0BC0, 0x0BC0},   {0x0BCD, 0x0BCD},
            {0x0C3E, 0x0C40},   {0x0C46, 0x0C48},   {0x0C4A, 0x0C4D},   {0x0C55, 0x0C56},   {0x0C62, 0x0C63},
            {0x0CBC, 0x0CBC},   {0x0CBF, 0x0CBF},   {0x0CC6, 0x0CC6},   {0x0CCC, 0x0CCD},   {0x0CE2, 0x0CE3},
            {0x0D41, 0x0D44},   {0x0D4D, 0x0D4D},   {0x0D62, 0x0D63},   {0x0DCA, 0x0DCA},   {0x0DD2, 0x0DD4},
            {0x0DD6, 0x0DD6},   {0x0E31, 0x0E31},   {0x0E34, 0x0E3A},   {0x0E47, 0x0E4E},   {0x0EB1, 0x0EB1},
            {0x0EB4, 0x0EB9},   {0x0EBB, 0x0EBC},   {0x0EC8, 0x0ECD},   {0x0F18, 0x0F19},   {0x0F35, 0x0F35},
            {0x0F37, 0x0F37},   {0x0F39, 0x0F39},   {0x0F71, 0x0F7E},   {0x0F80, 0x0F84},   {0x0F86, 0x0F87},
            {0x0F8D, 0x0F97},   {0x0F99, 0x0FBC},   {0x0FC6, 0x0FC6},   {0x102D, 0x1030},   {0x1032, 0x1037},
            {0x1039, 0x103A},   {0x103D, 0x103E},   {0x1058, 0x1059},   {0x105E, 0x1060},   {0x1071, 0x1074},
            {0x1082, 0x1082},   {0x1085, 0x1086},   {0x108D, 0x108D},   {0x109D, 0x109D},   {0x135D, 0x135F},
            {0x1712, 0x1714},   {0x1732, 0x1734},   {0x1752, 0x1753},   {0x1772, 0x1773},   {0x17B4, 0x17B5},
            {0x17B7, 0x17BD},   {0x17C6, 0x17C6},   {0x17C9, 0x17D3},   {0x17DD, 0x17DD},   {0x180B, 0x180D},
            {0x18A9, 0x18A9},   {0x1920, 0x1922},   {0x1927, 0x1928},   {0x1932, 0x1932},   {0x1939, 0x193B},
            {0x1A17, 0x1A18},   {0x1A56, 0x1A56},   {0x1A58, 0x1A5E},   {0x1A60, 0x1A60},   {0x1A62, 0x1A62},
            {0x1A65, 0x1A6C},   {0x1A73, 0x1A7C},   {0x1A7F, 0x1A7F},   {0x1B00, 0x1B03},   {0x1B34, 0x1B34},
            {0x1B36, 0x1B3A},   {0x1B3C, 0x1B3C},   {0x1B42, 0x1B42},   {0x1B6B, 0x1B73},   {0x1B80, 0x1B81},
            {0x1BA2, 0x1BA5},   {0x1BA8, 0x1BA9},   {0x1BAB, 0x1BAB},   {0x1BE6, 0x1BE6},   {0x1BE8, 0x1BE9},
            {0x1BED, 0x1BED},   {0x1BEF, 0x1BF1},   {0x1C2C, 0x1C33},   {0x1C36, 0x1C37},   {0x1CD0, 0x1CD2},
            {0x1CD4, 0x1CE0},   {0x1CE2, 0x1CE8},   {0x1CED, 0x1CED},   {0x1CF4, 0x1CF4},   {0x1DC0, 0x1DE6},
            {0x1DFC, 0x1DFF},   {0x20D0, 0x20F0},   {0x2CEF, 0x2CF1},   {0x2D7F, 0x2D7F},   {0x2DE0, 0x2DFF},
            {0x302A, 0x302D},   {0x3099, 0x309A},   {0xA66F, 0xA672},   {0xA674, 0xA67D},   {0xA69F, 0xA69F},
            {0xA6F0, 0xA6F1},   {0xA802, 0xA802},   {0xA806, 0xA806},   {0xA80B, 0xA80B},   {0xA825, 0xA826},
            {0xA8C4, 0xA8C4},   {0xA8E0, 0xA8F1},   {0xA926, 0xA92D},   {0xA947, 0xA951},   {0xA980, 0xA982},
            {0xA9B3, 0xA9B3},   {0xA9B6, 0xA9B9},   {0xA9BC, 0xA9BC},   {0xAA29, 0xAA2E},   {0xAA31, 0xAA32},
            {0xAA35, 0xAA36},   {0xAA43, 0xAA43},   {0xAA4C, 0xAA4C},   {0xAAB0, 0xAAB0},   {0xAAB2, 0xAAB4},
            {0xAAB7, 0xAAB8},   {0xAABE, 0xAABF},   {0xAAC1, 0xAAC1},   {0xAAEC, 0xAAED},   {0xAAF6, 0xAAF6},
            {0xABE5, 0xABE5},   {0xABE8, 0xABE8},   {0xABED, 0xABED},   {0xFB1E, 0xFB1E},   {0xFE00, 0xFE0F},
            {0xFE20, 0xFE26},   {0x101FD, 0x101FD}, {0x10A01, 0x10A03}, {0x10A05, 0x10A06}, {0x10A0C, 0x10A0F},
            {0x10A38, 0x10A3A}, {0x10A3F, 0x10A3F}, {0x11001, 0x11001}, {0x11038, 0x11046}, {0x11080, 0x11081},
            {0x110B3, 0x110B6}, {0x110B9, 0x110BA}, {0x11100, 0x11102}, {0x11127, 0x1112B}, {0x1112D, 0x11134},
            {0x11180, 0x11181}, {0x111B6, 0x111BE}, {0x116AB, 0x116AB}, {0x116AD, 0x116AD}, {0x116B0, 0x116B5},
            {0x116B7, 0x116B7}, {0x16F8F, 0x16F92}, {0x1D167, 0x1D169}, {0x1D17B, 0x1D182}, {0x1D185, 0x1D18B},
            {0x1D1AA, 0x1D1AD}, {0x1D242, 0x1D244}, {0xE0100, 0xE01EF},
        };
        static const llvm::sys::UnicodeCharSet CombiningCharacters(CombiningCharacterRanges);

        if (CombiningCharacters.contains(UCS))
            return 0;

        static const llvm::sys::UnicodeCharRange DoubleWidthCharacterRanges[] = {
            // Hangul Jamo
            {0x1100, 0x11FF},
            // Deprecated fullwidth angle brackets
            {0x2329, 0x232A},
            // CJK Misc, CJK Unified Ideographs, Yijing Hexagrams, Yi
            // excluding U+303F (IDEOGRAPHIC HALF FILL SPACE)
            {0x2E80, 0x303E},
            {0x3040, 0xA4CF},
            // Hangul
            {0xAC00, 0xD7A3},
            {0xD7B0, 0xD7C6},
            {0xD7CB, 0xD7FB},
            // CJK Unified Ideographs
            {0xF900, 0xFAFF},
            // Vertical forms
            {0xFE10, 0xFE19},
            // CJK Compatibility Forms + Small Form Variants
            {0xFE30, 0xFE6F},
            // Fullwidth forms
            {0xFF01, 0xFF60},
            {0xFFE0, 0xFFE6},
            // CJK Unified Ideographs
            {0x20000, 0x2A6DF},
            {0x2A700, 0x2B81F},
            {0x2F800, 0x2FA1F}};
        static const llvm::sys::UnicodeCharSet DoubleWidthCharacters(DoubleWidthCharacterRanges);

        if (DoubleWidthCharacters.contains(UCS))
            return 2;
        return 1;
    }

    int columnWidthUTF8Safe(std::string_view sv)
    {
        int columnWidth = 0;
        int length;
        for (std::size_t i = 0, e = sv.size(); i < e; i += length)
        {
            length = llvm::getNumBytesForUTF8(sv[i]);
            if (length <= 0 || i + length > sv.size())
            {
                length = 1;
                columnWidth++;
                continue;
            }
            llvm::UTF32 buf[1];
            const llvm::UTF8* start = reinterpret_cast<const llvm::UTF8*>(sv.data() + i);
            llvm::UTF32* target = &buf[0];
            if (llvm::conversionOK
                != llvm::ConvertUTF8toUTF32(&start, start + length, &target, target + 1, llvm::strictConversion))
            {
                columnWidth += length * charWidth(0xFFFD);
                continue;
            }
            int width = charWidth(buf[0]);
            if (width < 0)
            {
                return llvm::sys::unicode::ErrorNonPrintableCharacter;
            }
            columnWidth += width;
        }
        return columnWidth;
    }

    std::string stringOfSameWidth(std::string_view original, char characterToReplace)
    {
        auto utf8Width = columnWidthUTF8Safe(original);
        return std::string(utf8Width < 0 ? original.size() : utf8Width, characterToReplace);
    }

    class Context
    {
        OpenCL::LanguageOptions m_languageOptions;
        bool m_inPreprocessor;
        llvm::raw_ostream* m_reporter;
        std::vector<Token> m_result;
        std::string_view m_source;
        std::uint64_t& m_offset;
        std::vector<std::uint64_t> m_lineStarts;

        std::uint64_t getLineNumber(std::uint64_t offset) const noexcept
        {
            auto result = std::lower_bound(m_lineStarts.begin(), m_lineStarts.end(), offset);
            return result == m_lineStarts.begin() ? 1 : std::distance(m_lineStarts.begin(), result - 1) + 1;
        }

        std::uint64_t getLineStartOffset(std::uint64_t line) const noexcept
        {
            assert(line - 1 < m_lineStarts.size());
            return m_lineStarts[line - 1];
        }

        std::uint64_t getLineEndOffset(std::uint64_t line) const noexcept
        {
            assert(line - 1 < m_lineStarts.size());
            return line == m_lineStarts.size() ? m_source.size() : m_lineStarts[line];
        }

        void report(const std::string& suffix, llvm::raw_ostream::Colors colour, const std::string& message,
                    const std::uint64_t& location, const std::vector<uint64_t>& arrows) const
        {
            assert(!m_inPreprocessor);
            if (!m_reporter)
            {
                return;
            }

            auto startLine = 0;
            auto endLine = 0;
            std::vector<std::string_view> lines;
            auto numSize = 0;
            {
                auto line = getLineNumber(location);
                *m_reporter << line << ':' << location - getLineStartOffset(line) << ": ";
                llvm::WithColor(*m_reporter, colour) << suffix << ": ";
                *m_reporter << message << '\n';
                startLine = getLineNumber(tokenStartOffset);
                endLine = getLineNumber(m_offset);
                lines.reserve(endLine - startLine + 1);
                for (auto i = startLine; i <= endLine; i++)
                {
                    auto start = getLineStartOffset(i);
                    lines.push_back(m_source.substr(start, getLineEndOffset(i) - start - 1));
                }
                numSize = std::to_string(line).size();
                auto remainder = numSize % 4;
                if (remainder)
                {
                    numSize += 4 - remainder;
                }
            }
            std::vector<std::vector<std::uint64_t>> columnsOfArrowsForLine(lines.size());
            for (auto i = startLine; i <= endLine; i++)
            {
                auto begin = getLineStartOffset(i);
                auto end = getLineEndOffset(i);
                for (auto iter : arrows)
                {
                    if (iter < begin || iter >= end - 1)
                    {
                        continue;
                    }
                    auto printed = lines[i - startLine].substr(0, iter - begin);
                    auto utf8Width = columnWidthUTF8Safe({printed.data(), printed.size()});
                    columnsOfArrowsForLine[i - startLine].push_back(iter - begin
                                                                    - (utf8Width < 0 ? 0 : printed.size() - utf8Width));
                }
            }
            for (auto i = startLine; i <= endLine; i++)
            {
                // Text
                *m_reporter << llvm::format_decimal(i, numSize) << '|';

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
                    auto column = tokenStartOffset - getLineStartOffset(i);
                    *m_reporter << string.substr(0, column);
                    llvm::WithColor(*m_reporter, colour).get()
                        << string.substr(column, m_offset - tokenStartOffset + 1);
                    *m_reporter << string.substr(m_offset + 1);
                }
                else if (i == startLine)
                {
                    // The token starts here and does not end here
                    auto column = tokenStartOffset - getLineStartOffset(i);
                    *m_reporter << string.substr(0, column);
                    llvm::WithColor(*m_reporter, colour).get() << string.substr(column);
                }
                else
                {
                    // The token ends here and did not start here
                    auto endColumn = m_offset + 1 - getLineStartOffset(i);
                    llvm::WithColor(*m_reporter, colour).get() << string.substr(0, endColumn);
                    *m_reporter << string.substr(endColumn);
                }
                *m_reporter << '\n';

                // Underline + Arrows
                m_reporter->indent(numSize) << '|';
                if (i != startLine && i != endLine)
                {
                    // The token spans multiple lines and we are neither at the first nor last line. Therefore
                    // this line consist only of the token
                    auto underline = stringOfSameWidth(string, '~');
                    for (auto iter : arrowsForLine)
                    {
                        assert(iter < underline.size());
                        underline[iter] = '^';
                    }
                    llvm::WithColor(*m_reporter, colour) << underline;
                }
                else if (i == startLine && i == endLine)
                {
                    // The token does not span lines and starts as well as ends here
                    auto column = tokenStartOffset - getLineStartOffset(i);
                    *m_reporter << stringOfSameWidth(string.substr(0, column), ' ');
                    auto underline = stringOfSameWidth(string.substr(column, m_offset - tokenStartOffset + 1), '~');
                    for (auto iter : arrowsForLine)
                    {
                        assert(iter - column < underline.size());
                        underline[iter - column] = '^';
                    }
                    llvm::WithColor(*m_reporter, colour) << underline;
                }
                else if (i == startLine)
                {
                    // The token starts here and does not end here
                    auto column = tokenStartOffset - getLineStartOffset(i);
                    *m_reporter << stringOfSameWidth(string.substr(0, column), ' ');
                    auto underline = stringOfSameWidth(string.substr(column), '~');
                    for (auto iter : arrowsForLine)
                    {
                        assert(iter < underline.size());
                        underline[iter] = '^';
                    }
                    llvm::WithColor(m_reporter->indent(column), colour) << underline;
                }
                else
                {
                    // The token ends here and did not start here
                    auto endColumn = 1 + m_offset - getLineStartOffset(i);
                    auto underline = stringOfSameWidth(string.substr(0, endColumn), '~');
                    for (auto iter : arrowsForLine)
                    {
                        assert(iter < underline.size());
                        underline[iter] = '^';
                    }
                    llvm::WithColor(*m_reporter, colour) << underline;
                }
                *m_reporter << '\n';
            }
            m_reporter->flush();
        }

    public:
        std::uint64_t tokenStartOffset;

        Context(const std::string& source, std::uint64_t& offset, std::vector<std::uint64_t> lineStarts,
                OpenCL::LanguageOptions languageOptions, bool inPreprocessor, llvm::raw_ostream* reporter) noexcept
            : m_languageOptions(languageOptions),
              m_inPreprocessor(inPreprocessor),
              m_reporter(reporter),
              m_source(source),
              m_offset(offset),
              m_lineStarts(std::move(lineStarts))
        {
        }

        void reportError(const std::string& message, std::uint64_t location, std::vector<std::uint64_t> arrows = {})
        {
            report("error", llvm::raw_ostream::RED, message, location, arrows);
        }

        void reportWarning(const std::string& message, std::uint64_t location, std::vector<std::uint64_t> arrows = {})
        {
            report("warning", llvm::raw_ostream::CYAN, message, location, arrows);
        }

        [[nodiscard]] OpenCL::LanguageOptions getLanguageOptions() const
        {
            return m_languageOptions;
        }

        [[nodiscard]] const std::vector<OpenCL::Lexer::Token>& getResult() const& noexcept
        {
            return m_result;
        }

        [[nodiscard]] std::vector<OpenCL::Lexer::Token> getResult() && noexcept
        {
            return m_result;
        }

        bool isInPreprocessor() const
        {
            return m_inPreprocessor;
        }

        void push(TokenType tokenType, Token::ValueType value = {}) noexcept
        {
            auto view = m_source.substr(tokenStartOffset, m_offset - tokenStartOffset + 1);
            m_result.emplace_back(tokenStartOffset, tokenType, std::string(view.begin(), view.end()), std::move(value));
        }
    };

    struct Start;
    struct CharacterLiteral;
    struct StringLiteral;
    struct Text;
    struct LineComment;
    struct BlockComment;
    struct Number;
    struct AfterInclude;
    struct L;

    using StateMachine =
        std::variant<Start, CharacterLiteral, StringLiteral, Text, LineComment, BlockComment, Number, AfterInclude, L>;

    struct Start final
    {
        StateMachine advance(char c, Context& context) noexcept;
    };

    struct CharacterLiteral final
    {
        bool wide = false;
        std::string characters;

        StateMachine advance(char c, Context& context) noexcept;
    };

    struct StringLiteral final
    {
        bool wide = false;
        std::string characters;

        StateMachine advance(char c, Context& context) noexcept;
    };

    struct Text final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct LineComment final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct BlockComment final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct Number final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct AfterInclude final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct L final
    {
        std::pair<StateMachine, bool> advance(char, Context& context) noexcept;
    };

    StateMachine Start::advance(char c, Context&) noexcept
    {
        switch (c)
        {
            case '\'': return CharacterLiteral{};
            case '"': return StringLiteral{};
            case 'L': return L{};
            default: return *this;
        }
    }

    /**
     * Callee responsible for right format
     * @param value string that either consist of 4 or 8 hex digits
     * @return Unicode value
     */
    std::optional<std::uint32_t> universalCharacterToValue(std::string_view value, std::uint64_t offsetOfFirstC,
                                                           Context& context)
    {
        auto result = std::stoul(std::string{value.begin(), value.end()}, nullptr, 16);
        if (result < 0xA0)
        {
            if (result != '$' && result != '@' && result != '`')
            {
                std::vector<std::uint64_t> arrows(value.size());
                std::iota(arrows.begin(), arrows.end(), offsetOfFirstC);
                context.reportError(
                    OpenCL::ErrorMessages::Lexer::INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                        value, OpenCL::ErrorMessages::Lexer::VALUE_MUSTNT_BE_LESS_THAN_A0),
                    offsetOfFirstC, std::move(arrows));
                return {};
            }
        }
        else if (result >= 0xD800 && result <= 0xDFFF)
        {
            std::vector<std::uint64_t> arrows(value.size());
            std::iota(arrows.begin(), arrows.end(), offsetOfFirstC);
            context.reportError(OpenCL::ErrorMessages::Lexer::INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                    value, OpenCL::ErrorMessages::Lexer::VALUE_MUSTNT_BE_IN_RANGE),
                                offsetOfFirstC, std::move(arrows));
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
                context.reportError(OpenCL::ErrorMessages::Lexer::EXPECTED_CHARACTER_AFTER_BACKSLASH, backslash + 1,
                                    {backslash, backslash + 1});
                return {};
            }
            default:
            {
                context.reportError(
                    OpenCL::ErrorMessages::Lexer::INVALID_ESCAPE_SEQUENCE_N.args(std::string("\\") + escape),
                    backslash + 1, {backslash, backslash + 1});
                return {};
            }
        }
    }

    std::pair<std::vector<llvm::UTF32>, bool> processCharacters(const std::string& characters, Context& context,
                                                                bool wide)
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
            if (*iter == '\n')
            {
                context.reportError(OpenCL::ErrorMessages::Lexer::NEWLINE_IN_N_USE_BACKLASH_N.args("Character literal"),
                                    context.tokenStartOffset,
                                    {context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data())});
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
                    context.reportError(OpenCL::ErrorMessages::Lexer::INVALID_UTF8_SEQUENCE, context.tokenStartOffset,
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
                    context.reportError(
                        OpenCL::ErrorMessages::Lexer::INVALID_ESCAPE_SEQUENCE_N.args(big ? "\\U" : "\\u"), start,
                        {start, start + 1});
                    errorOccured = true;
                    continue;
                }
                else
                {
                    auto hexStart = iter;
                    auto hexEnd = std::find_if(
                        hexStart, hexStart + std::min<std::size_t>(std::distance(hexStart, end), big ? 8 : 4),
                        [](char c) {
                            return !(c >= '0' || c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F');
                        });
                    if (std::distance(hexStart, hexEnd) != (big ? 8 : 4))
                    {
                        auto start = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data() - 2);
                        std::vector<std::uint64_t> arrows = {start, start + 1};
                        arrows.resize(2 + std::distance(hexStart, hexEnd));
                        std::iota(arrows.begin() + 2, arrows.end(), start + 2);
                        context.reportError(
                            OpenCL::ErrorMessages::Lexer::INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS.args(
                                std::to_string((big ? 8 : 4) - std::distance(hexStart, hexEnd))),
                            start, std::move(arrows));
                        errorOccured = true;
                        iter = hexEnd;
                        continue;
                    }
                    auto uc = universalCharacterToValue(
                        {hexStart, static_cast<std::size_t>(hexEnd - hexStart)},
                        context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data()), context);
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
                    context.reportError(OpenCL::ErrorMessages::Lexer::AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED, start,
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
                    auto start = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data()) - 1;
                    context.reportError(
                        OpenCL::ErrorMessages::Lexer::INVALID_OCTAL_CHARACTER.args(std::string(1, *lastOctal)), start,
                        {start, start + 1});
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
                auto start = context.tokenStartOffset + (wide ? 2 : 1) + (iter - characters.data());
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
                context.reportError(OpenCL::ErrorMessages::Lexer::CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE,
                                    context.tokenStartOffset);
                errorOccured = true;
            }
        }
        result.resize(std::distance(result.data(), resultStart));
        return {result, errorOccured};
    }

    StateMachine CharacterLiteral::advance(char c, Context& context) noexcept
    {
        if (c == '\'' && (characters.empty() || characters.back() != '\\'))
        {
            auto [result, errorOccured] = processCharacters(characters, context, wide);
            if (result.empty())
            {
                if (!errorOccured)
                {
                    std::vector<std::uint64_t> arrows(wide ? 3 : 2);
                    std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
                    context.reportError(OpenCL::ErrorMessages::Lexer::CHARACTER_LITERAL_CANNOT_BE_EMPTY,
                                        context.tokenStartOffset, std::move(arrows));
                }
                return {};
            }
            else if (result.size() > 1)
            {
                std::vector<std::uint64_t> arrows((wide ? 3 : 2) + characters.size());
                std::iota(arrows.begin(), arrows.end(), context.tokenStartOffset);
                context.reportWarning(OpenCL::ErrorMessages::Lexer::DISCARDING_ALL_BUT_FIRST_CHARACTER,
                                      context.tokenStartOffset, std::move(arrows));
            }

            if (!context.isInPreprocessor())
            {
                if (wide)
                {
                    std::int32_t value;
                    std::memcpy(&value, &result[0], sizeof(std::int32_t));
                    context.push(TokenType::Literal, value);
                }
                else
                {
                    context.push(TokenType::Literal, static_cast<std::int32_t>(static_cast<std::uint8_t>(result[0])));
                }
            }
            else
            {
                context.push(TokenType::Literal, static_cast<std::int64_t>(result[0]));
            }
            return Start{};
        }
        characters += c;
        return std::move(*this);
    }

    StateMachine StringLiteral::advance(char c, Context& context) noexcept
    {
        if (c == '"' && (characters.empty() || characters.back() != '\\'))
        {
            bool followsInclude =
                context.isInPreprocessor() && context.getResult().size() >= 2
                && (context.getResult()[context.getResult().size() - 2].getTokenType() == TokenType::Pound
                    && context.getResult().back().getTokenType() == TokenType::Identifier
                    && std::get<std::string>(context.getResult().back().getValue()) == "include");
            if (followsInclude && !wide)
            {
                context.push(TokenType::StringLiteral, characters);
                return Start{};
            }

            auto [result, errorOccured] = processCharacters(characters, context, wide);
            if (!errorOccured)
            {
                if (!wide || context.getLanguageOptions().getSizeOfWChar() == 1)
                {
                    const auto* start = result.data();
                    std::vector<llvm::UTF8> utf8(result.size() * 5);
                    auto* dest = utf8.data();
                    auto conversion = llvm::ConvertUTF32toUTF8(&start, start + result.size(), &dest, dest + utf8.size(),
                                                               llvm::strictConversion);
                    if (conversion != llvm::conversionOK)
                    {
                        OPENCL_UNREACHABLE; // While error occurred is true due to failed utf8 to utf 32 conversion this
                                            // code can't be reached
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
                                OPENCL_UNREACHABLE; // While error occurred is true due to failed utf8 to utf 32
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
                        default: OPENCL_UNREACHABLE;
                    }
                }
            }
            return Start{};
        }
        characters += c;
        return std::move(*this);
    }

    std::pair<StateMachine, bool> L::advance(char c, Context& context) noexcept
    {
        if (c == '"')
        {
            return {StringLiteral{true}, true};
        }
        else if (c == '\'')
        {
            return {CharacterLiteral{true}, true};
        }
        else
        {
            return {Text{/*TODO:"L" */}, false};
        }
    }
} // namespace

OpenCL::SourceObject OpenCL::Lexer::tokenize(std::string source, LanguageOptions languageOptions, bool isInPreprocessor,
                                             llvm::raw_ostream* reporter)
{
    if (source.empty() || source.back() != ' ')
    {
        source += '\n';
    }
    static std::regex identifierMatch("[a-zA-Z_]\\w*");

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
    Context context(source, offset, starts, languageOptions, isInPreprocessor, reporter);
    for (auto iter : source)
    {
        while (std::visit(
            [iter, &stateMachine, &context, offset](auto&& state) mutable -> bool {
                if constexpr (std::is_convertible_v<decltype(state.advance(iter, context)), bool>)
                {
                    return !state.advance(iter, context);
                }
                else if constexpr (std::is_same_v<StateMachine, decltype(state.advance(iter, context))>)
                {
                    stateMachine = state.advance(iter, context);
                    if constexpr (std::is_same_v<std::decay_t<decltype(state)>, Start>)
                    {
                        if (!std::holds_alternative<Start>(stateMachine))
                        {
                            context.tokenStartOffset = offset;
                        }
                    }
                    return false;
                }
                else if constexpr (std::is_void_v<decltype(state.advance(iter, context))>)
                {
                    state.advance(iter, context);
                    return false;
                }
                else
                {
                    auto&& [lhs, rhs] = state.advance(iter, context);
                    bool advance;
                    if constexpr (std::is_same_v<std::decay_t<decltype(lhs)>, bool>)
                    {
                        stateMachine = std::move(rhs);
                        advance = lhs;
                    }
                    else
                    {
                        stateMachine = std::move(lhs);
                        advance = rhs;
                    }
                    if constexpr (std::is_same_v<std::decay_t<decltype(state)>, Start>)
                    {
                        if (!std::holds_alternative<Start>(stateMachine))
                        {
                            context.tokenStartOffset = offset;
                        }
                    }
                    return !advance;
                }
            },
            stateMachine))
            ;
        offset++;
    }

    return SourceObject(std::move(starts), std::move(context).getResult(), languageOptions);
}

std::string OpenCL::Lexer::Token::getRepresentation() const
{
    return m_representation;
}

std::string OpenCL::Lexer::tokenName(OpenCL::Lexer::TokenType tokenType)
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
        case TokenType::DefinedKeyword: return "'defined'";
        case TokenType::Newline: return "'Newline'";
        case TokenType::UnderlineBool: return "'_Bool'";
        case TokenType::Miscellaneous: OPENCL_UNREACHABLE;
    }
    OPENCL_UNREACHABLE;
}

std::string OpenCL::Lexer::tokenValue(OpenCL::Lexer::TokenType tokenType)
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
        case TokenType::DefinedKeyword: return "defined";
        case TokenType::Newline: return "Newline";
        case TokenType::UnderlineBool: return "_Bool";
        case TokenType::Miscellaneous: OPENCL_UNREACHABLE;
    }
    OPENCL_UNREACHABLE;
}

bool OpenCL::Lexer::Token::macroInserted() const noexcept
{
    return m_macroId;
}

const OpenCL::Lexer::Token::variant& OpenCL::Lexer::Token::getValue() const noexcept
{
    return m_value;
}

OpenCL::Lexer::TokenType OpenCL::Lexer::Token::getTokenType() const noexcept
{
    return m_tokenType;
}

OpenCL::Lexer::Token::Token(std::uint64_t offset, TokenType tokenType, std::string representation, variant value)
    : m_value(std::move(value)),
      m_representation(std::move(representation)),
      m_offset(offset),
      m_sourceOffset(offset),
      m_tokenType(tokenType)
{
    assert(!m_representation.empty());
}

std::uint64_t OpenCL::Lexer::Token::getMacroId() const noexcept
{
    return m_macroId;
}

void OpenCL::Lexer::Token::setMacroId(std::uint64_t macroId) noexcept
{
    m_macroId = macroId;
}

std::size_t Token::getLength() const noexcept
{
    return m_representation.size();
}

std::uint64_t Token::getOffset() const noexcept
{
    return m_offset;
}

std::uint64_t Token::getSourceOffset() const noexcept
{
    return m_sourceOffset;
}

std::string OpenCL::Lexer::reconstruct(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                       std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    //    if (begin == end)
    //    {
    //        return {};
    //    }
    //    return std::string(begin->getLine() - 1, '\n') + std::string(begin->getColumn(), ' ')
    //           + reconstructTrimmed(begin, end);
}

std::string OpenCL::Lexer::reconstructTrimmed(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                              std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    //    std::string result;
    //    for (auto curr = begin; curr != end; curr++)
    //    {
    //        if (curr != begin)
    //        {
    //            auto prev = curr - 1;
    //            if (curr->getLine() == prev->getLine())
    //            {
    //                result += std::string(curr->getColumn() - (prev->getColumn() + prev->getLength()), ' ');
    //            }
    //            else
    //            {
    //                result += '\n' + std::string(curr->getColumn(), ' ');
    //            }
    //        }
    //        result += curr->getRepresentation();
    //    }
    //    return result;
}

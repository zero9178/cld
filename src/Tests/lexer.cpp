#include "catch.hpp"

#pragma warning(push, 0)
#include <llvm/Support/Format.h>
#pragma warning(pop)

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include <array>

#include "TestConfig.hpp"

using namespace cld::ErrorMessages::Lexer;
using namespace cld::Notes::Lexer;
using namespace Catch::Matchers;

#define LEXER_OUTPUTS_WITH(input, match)                                         \
    do                                                                           \
    {                                                                            \
        std::string s;                                                           \
        llvm::raw_string_ostream ss(s);                                          \
        cld::Lexer::tokenize(input, cld::LanguageOptions::native(), false, &ss); \
        CHECK_THAT(s, match);                                                    \
        if (!s.empty())                                                          \
        {                                                                        \
            cld::Lexer::tokenize(input);                                         \
        }                                                                        \
    } while (0)

#define PP_LEXER_OUTPUTS_WITH(input, match)                                     \
    do                                                                          \
    {                                                                           \
        std::string s;                                                          \
        llvm::raw_string_ostream ss(s);                                         \
        cld::Lexer::tokenize(input, cld::LanguageOptions::native(), true, &ss); \
        CHECK_THAT(s, match);                                                   \
        if (!s.empty())                                                         \
        {                                                                       \
            cld::Lexer::tokenize(input, cld::LanguageOptions::native(), true);  \
        }                                                                       \
    } while (0)

namespace
{
[[nodiscard]] cld::SourceObject lexes(std::string_view code,
                                      const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto result = cld::Lexer::tokenize({code.begin(), code.end()}, options, false, &ss);
    REQUIRE(buffer.empty());
    return result;
}

[[nodiscard]] cld::SourceObject pplexes(std::string_view code,
                                        const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto result = cld::Lexer::tokenize({code.begin(), code.end()}, options, true, &ss);
    REQUIRE(buffer.empty());
    return result;
}
} // namespace

SCENARIO("Lexing Identifiers", "[lexer]")
{
    GIVEN("Initial characters")
    {
        std::vector<const char*> allowedCharacters = {
            "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",   "s",
            "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",   "L",
            "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_", "ª", "µ", "·",   "º",
            "À", "Ö", "ʰ", "Ά", "Є", "Ա", "ՙ", "ա", "ְ",  "ء", "ې", "ँ",  "ঁ",  "ৰ", "ਂ",  "ଁ",  "ஂ",  "ªµ·º"};
        for (auto c : allowedCharacters)
        {
            THEN(c)
            {
                auto result = lexes(c);
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == c);
            }
        }
        AND_THEN("an universal character")
        {
            auto result = lexes("\\u00B5");
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "µ");
            CHECK(result.data()[0].getRepresentation() == "\\u00B5");
        }
    }
    GIVEN("non initial characters")
    {
        std::vector<const char*> allowedCharacters = {
            "_a", "_b", "_c", "_d", "_e", "_f", "_g", "_h", "_i", "_j", "_k", "_l", "_m", "_n", "_o", "_p", "_q",
            "_r", "_s", "_t", "_u", "_v", "_w", "_x", "_y", "_z", "_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7",
            "_8", "_9", "_A", "_B", "_C", "_D", "_E", "_F", "_G", "_H", "_I", "_J", "_K", "_L", "_M", "_N", "_O",
            "_P", "_Q", "_R", "_S", "_T", "_U", "_V", "_W", "_X", "_Y", "_Z", "__", "_ª", "_µ", "_·", "_º", "_À",
            "_Ö", "_ʰ", "_Ά", "_Є", "_Ա", "_ՙ", "_ա", "_ְ",  "ء_", "ې_", "_ँ",  "_ঁ",  "_ৰ", "_ਂ",  "_ଁ",  "_ஂ",  "_ªµ·º"};
        for (auto c : allowedCharacters)
        {
            THEN(c)
            {
                auto result = lexes(c);
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == c);
                CHECK(result.data()[0].getRepresentation() == c);
            }
        }
        AND_THEN("an universal character")
        {
            auto result = lexes("_\\u00B5");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "_µ");
            CHECK(result.data()[0].getRepresentation() == "_\\u00B5");
        }
    }
    GIVEN("A universal character")
    {
        WHEN("incomplete")
        {
            LEXER_OUTPUTS_WITH("_\\ute", Catch::Contains(STRAY_N_IN_PROGRAM.args("\\"))
                                             && Catch::Contains(UNIVERSAL_CHARACTER_REQUIRES_N_MORE_DIGITS.args(4)));
        }
        WHEN("disallowed")
        {
            LEXER_OUTPUTS_WITH("_\\u0099", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                               "0099", VALUE_MUSTNT_BE_LESS_THAN_A0)));
        }
    }
    GIVEN("Keywords")
    {
        {
            using namespace cld::Lexer;
            auto result = lexes(
                "auto enum restrict unsigned break extern return void case float short volatile char for signed while const goto sizeof continue if static default inline struct do int switch double long typedef else register union _Bool");
            std::vector correct = {TokenType::AutoKeyword,     TokenType::EnumKeyword,     TokenType::RestrictKeyword,
                                   TokenType::UnsignedKeyword, TokenType::BreakKeyword,    TokenType::ExternKeyword,
                                   TokenType::ReturnKeyword,   TokenType::VoidKeyword,     TokenType::CaseKeyword,
                                   TokenType::FloatKeyword,    TokenType::ShortKeyword,    TokenType::VolatileKeyword,
                                   TokenType::CharKeyword,     TokenType::ForKeyword,      TokenType::SignedKeyword,
                                   TokenType::WhileKeyword,    TokenType::ConstKeyword,    TokenType::GotoKeyword,
                                   TokenType::SizeofKeyword,   TokenType::ContinueKeyword, TokenType::IfKeyword,
                                   TokenType::StaticKeyword,   TokenType::DefaultKeyword,  TokenType::InlineKeyword,
                                   TokenType::StructKeyword,   TokenType::DoKeyword,       TokenType::IntKeyword,
                                   TokenType::SwitchKeyword,   TokenType::DoubleKeyword,   TokenType::LongKeyword,
                                   TokenType::TypedefKeyword,  TokenType::ElseKeyword,     TokenType::RegisterKeyword,
                                   TokenType::UnionKeyword,    TokenType::UnderlineBool};
            std::vector<TokenType> tokens;
            tokens.reserve(result.data().size());
            std::transform(result.data().begin(), result.data().end(), std::back_inserter(tokens),
                           [](const Token& token) { return token.getTokenType(); });
            CHECK_THAT(tokens, Catch::Equals(correct));
        }
    }
}

TEST_CASE("Lexing backslashes", "[lexer]")
{
    SECTION("Newline after backslash")
    {
        SECTION("Normal identifier")
        {
            auto result = lexes("te\\\nst");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "test");
            CHECK(result.data()[0].getRepresentation() == "te\\\nst");
        }
        SECTION("Universal character")
        {
            auto result = lexes("_\\\\\nu00B5");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "_µ");
            CHECK(result.data()[0].getRepresentation() == "_\\\\\nu00B5");
            WHEN("incomplete and multiline")
            {
                LEXER_OUTPUTS_WITH("_\\\\\nute",
                                   Catch::Contains(STRAY_N_IN_PROGRAM.args("\\"))
                                       && Catch::Contains(UNIVERSAL_CHARACTER_REQUIRES_N_MORE_DIGITS.args(4)));
            }
            std::string buffer;
            llvm::raw_string_ostream ss(buffer);
            result = cld::Lexer::tokenize("\x01\\u\\\n00B5", cld::LanguageOptions::native(), false, &ss);
            ss.flush();
            CHECK_THAT(buffer, Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U00000001")));
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "µ");
            CHECK(result.data()[0].getRepresentation() == "\\u\\\n00B5");
        }
        SECTION("Keywords")
        {
            auto result = lexes("f\\\nor");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::ForKeyword);
            CHECK(result.data()[0].getRepresentation() == "f\\\nor");
        }
        SECTION("Number")
        {
            auto result = lexes("5\\\ne+3");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 5e+3);
            CHECK(result.data()[0].getRepresentation() == "5\\\ne+3");
            SECTION("Floating point")
            {
                result = lexes(".\\\n5e+3");
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(fp.convertToDouble() == .5e+3);
                CHECK(result.data()[0].getRepresentation() == ".\\\n5e+3");
            }
            SECTION("Errors")
            {
                LEXER_OUTPUTS_WITH("5.\\\n5f5", Catch::Contains(INVALID_LITERAL_SUFFIX.args("f5")));
            }
        }
        SECTION("Dots")
        {
            auto result = lexes(".\\\n.");
            REQUIRE(result.data().size() == 2);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Dot);
            CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Dot);
            CHECK(result.data()[0].getOffset() == 0);
            CHECK(result.data()[1].getOffset() == 3);
            CHECK(result.data()[0].getRepresentation() == ".");
            CHECK(result.data()[1].getRepresentation() == ".");
        }
        SECTION("Character literals")
        {
            auto result = lexes("L\\\n'5'");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            CHECK(result.data()[0].getOffset() == 0);
            CHECK(result.data()[0].getRepresentation() == "L\\\n'5'");
            LEXER_OUTPUTS_WITH("L\\\n'\\90'", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
        }
    }
    SECTION("Whitespace after backslash")
    {
        LEXER_OUTPUTS_WITH("i\\    \nf", Catch::Contains(NO_WHITESPACE_ALLOWED_BETWEEN_BACKSLASH_AND_NEWLINE));
        LEXER_OUTPUTS_WITH("i\\  5  \nf", Catch::Contains(STRAY_N_IN_PROGRAM.args("\\")));
        WHEN("Chained")
        {
            LEXER_OUTPUTS_WITH("i\\ \n\\ \n\\ \n\\ \nf",
                               Catch::Contains(NO_WHITESPACE_ALLOWED_BETWEEN_BACKSLASH_AND_NEWLINE));
        }
    }
    SECTION("Fuzzer discoveries")
    {
        cld::Lexer::tokenize("\\-");
        cld::Lexer::tokenize("&\\\x1d.");
        auto result = lexes("N\\\n;");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::SemiColon);
        CHECK(result.data()[0].getRepresentation() == "N");
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "N");
    }
}

TEST_CASE("Lexing trigraphs", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = lexes("?\?= ?\?( ?\?) ?\?' ?\?< ?\?! ?\?> ?\?-");
        REQUIRE(result.data().size() == 8);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Pound);
        CHECK(result.data()[0].getRepresentation() == "?\?=");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::OpenSquareBracket);
        CHECK(result.data()[1].getRepresentation() == "?\?(");
        CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::CloseSquareBracket);
        CHECK(result.data()[2].getRepresentation() == "?\?)");
        CHECK(result.data()[3].getTokenType() == cld::Lexer::TokenType::BitXor);
        CHECK(result.data()[3].getRepresentation() == "?\?'");
        CHECK(result.data()[4].getTokenType() == cld::Lexer::TokenType::OpenBrace);
        CHECK(result.data()[4].getRepresentation() == "?\?<");
        CHECK(result.data()[5].getTokenType() == cld::Lexer::TokenType::BitOr);
        CHECK(result.data()[5].getRepresentation() == "?\?!");
        CHECK(result.data()[6].getTokenType() == cld::Lexer::TokenType::CloseBrace);
        CHECK(result.data()[6].getRepresentation() == "?\?>");
        CHECK(result.data()[7].getTokenType() == cld::Lexer::TokenType::BitWiseNegation);
        CHECK(result.data()[7].getRepresentation() == "?\?-");
    }
    SECTION("Backslash")
    {
        SECTION("Newline after backslash")
        {
            SECTION("Normal identifier")
            {
                auto result = lexes("te?\?/\nst");
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == "test");
                CHECK(result.data()[0].getRepresentation() == "te?\?/\nst");
            }
            SECTION("Universal character")
            {
                auto result = lexes("_?\?/?\?/\nu00B5");
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == "_µ");
                CHECK(result.data()[0].getRepresentation() == "_?\?/?\?/\nu00B5");
                WHEN("incomplete and multiline")
                {
                    LEXER_OUTPUTS_WITH("_?\?/?\?/\nute",
                                       Catch::Contains(STRAY_N_IN_PROGRAM.args("\\"))
                                           && Catch::Contains(UNIVERSAL_CHARACTER_REQUIRES_N_MORE_DIGITS.args(4)));
                }
                std::string buffer;
                llvm::raw_string_ostream ss(buffer);
                result = cld::Lexer::tokenize("\x01?\?/u?\?/\n00B5", cld::LanguageOptions::native(), false, &ss);
                ss.flush();
                CHECK_THAT(buffer, Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U00000001")));
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == "µ");
                CHECK(result.data()[0].getRepresentation() == "?\?/u?\?/\n00B5");
            }
            SECTION("Keywords")
            {
                auto result = lexes("f?\?/\nor");
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::ForKeyword);
                CHECK(result.data()[0].getRepresentation() == "f?\?/\nor");
            }
            SECTION("Number")
            {
                auto result = lexes("5?\?/\ne+3");
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(fp.convertToDouble() == 5e+3);
                CHECK(result.data()[0].getRepresentation() == "5?\?/\ne+3");
                SECTION("Floating point")
                {
                    result = lexes(".?\?/\n5e+3");
                    REQUIRE(result.data().size() == 1);
                    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                    REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                    fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                    CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                    CHECK(fp.convertToDouble() == .5e+3);
                    CHECK(result.data()[0].getRepresentation() == ".?\?/\n5e+3");
                }
                SECTION("Errors")
                {
                    LEXER_OUTPUTS_WITH("5.?\?/\n5f5", Catch::Contains(INVALID_LITERAL_SUFFIX.args("f5")));
                }
            }
            SECTION("Dots")
            {
                auto result = lexes(".?\?/\n.");
                REQUIRE(result.data().size() == 2);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Dot);
                CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Dot);
                CHECK(result.data()[0].getOffset() == 0);
                CHECK(result.data()[1].getOffset() == 5);
                CHECK(result.data()[0].getRepresentation() == ".");
                CHECK(result.data()[1].getRepresentation() == ".");
            }
            SECTION("Character literals")
            {
                auto result = lexes("L?\?/\n'5'");
                REQUIRE(result.data().size() == 1);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                CHECK(result.data()[0].getOffset() == 0);
                CHECK(result.data()[0].getRepresentation() == "L?\?/\n'5'");
                LEXER_OUTPUTS_WITH("L?\?/\n'?\?/90'", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
            }
        }
        SECTION("Whitespace after backslash")
        {
            LEXER_OUTPUTS_WITH("i?\?/    \nf", Catch::Contains(NO_WHITESPACE_ALLOWED_BETWEEN_BACKSLASH_AND_NEWLINE));
            LEXER_OUTPUTS_WITH("i?\?/  5  \nf", Catch::Contains(STRAY_N_IN_PROGRAM.args("\\")));
            WHEN("Chained")
            {
                LEXER_OUTPUTS_WITH("i?\?/ \n?\?/ \n?\?/ \n?\?/ \nf",
                                   Catch::Contains(NO_WHITESPACE_ALLOWED_BETWEEN_BACKSLASH_AND_NEWLINE));
            }
        }
        SECTION("Fuzzer discoveries")
        {
            cld::Lexer::tokenize("?\?/-");
            cld::Lexer::tokenize("&?\?/\x1d.");
            auto result = lexes("N?\?/\n;");
            REQUIRE(result.data().size() == 2);
            CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
            CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::SemiColon);
            CHECK(result.data()[0].getRepresentation() == "N");
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "N");
        }
    }
    SECTION("Multi character token")
    {
        auto result = lexes("%:?\?=");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::DoublePound);
        CHECK(result.data()[0].getRepresentation() == "%:?\?=");
        result = lexes("#?\?=");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::DoublePound);
        CHECK(result.data()[0].getRepresentation() == "#?\?=");
    }
}

TEST_CASE("Lexing character literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = lexes("'5'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<llvm::APSInt>(result.data()[0].getValue()));
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == '5');
        CHECK(value.getBitWidth() == sizeof(int) * 8);
        CHECK(value.isSigned());
        LEXER_OUTPUTS_WITH("''", Catch::Contains(CHARACTER_LITERAL_CANNOT_BE_EMPTY));
    }
    SECTION("Escape characters")
    {
        std::array results = {
            std::pair{"'\\''", '\''}, std::pair{"'\\\"'", '"'}, std::pair{"'\\?'", '\?'}, std::pair{"'\\a'", '\a'},
            std::pair{"'\\b'", '\b'}, std::pair{"'\\f'", '\f'}, std::pair{"'\\n'", '\n'}, std::pair{"'\\r'", '\r'},
            std::pair{"'\\t'", '\t'}, std::pair{"'\\v'", '\v'},
        };
        for (auto [input, chara] : results)
        {
            DYNAMIC_SECTION(input)
            {
                auto result = lexes(input);
                REQUIRE(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APSInt>(result.data()[0].getValue()));
                auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
                CHECK(value == chara);
                CHECK(value.getBitWidth() == 8 * sizeof(int));
                CHECK(value.isSigned());
            }
        }
        LEXER_OUTPUTS_WITH("'\\o'", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\o")));
        LEXER_OUTPUTS_WITH("L'\\o'", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\o")));
        LEXER_OUTPUTS_WITH("'\\ '", Catch::Contains(EXPECTED_CHARACTER_AFTER_BACKSLASH));
    }
    SECTION("Octals")
    {
        LEXER_OUTPUTS_WITH("'\\9'", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
        auto result = lexes("'\\070'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == '\070');
        CHECK(value.getBitWidth() == 8 * sizeof(int));
        CHECK(value.isSigned());
    }
    SECTION("Hex")
    {
        LEXER_OUTPUTS_WITH("'\\xG'", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        LEXER_OUTPUTS_WITH("'\\x'", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        auto result = lexes("'\\x070'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == '\x070');
        CHECK(value.getBitWidth() == 8 * sizeof(int));
        CHECK(value.isSigned());
    }
    SECTION("Multibyte")
    {
        auto result = lexes("L'5'", cld::Tests::x64windowsGnu);
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == L'5');
        CHECK(value.getBitWidth() == 16);
        CHECK(value.isUnsigned());
    }
    SECTION("Universal characters")
    {
        LEXER_OUTPUTS_WITH("'\\u'", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\u")));
        LEXER_OUTPUTS_WITH("'\\U'", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\U")));
        LEXER_OUTPUTS_WITH("'\\u56'", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS.args("2")));
        LEXER_OUTPUTS_WITH("'\\u0090'", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                            "0090", VALUE_MUSTNT_BE_LESS_THAN_A0)));
        LEXER_OUTPUTS_WITH("L'\\uD977'", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                             "D977", VALUE_MUSTNT_BE_IN_RANGE)));
        LEXER_OUTPUTS_WITH("'\\U56'", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS.args("6")));
        LEXER_OUTPUTS_WITH("'\\U00000090'", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                                "00000090", VALUE_MUSTNT_BE_LESS_THAN_A0)));
        LEXER_OUTPUTS_WITH("L'\\U0000D977'", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                                 "0000D977", VALUE_MUSTNT_BE_IN_RANGE)));
        auto result = lexes("L'\\u3435'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == L'\u3435');
        CHECK(value.getBitWidth() == 8 * sizeof(wchar_t));
        CHECK(value.isSigned() == std::is_signed_v<wchar_t>);
        result = lexes("L'\\U00003435'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == L'\u3435');
        CHECK(value.getBitWidth() == 8 * sizeof(wchar_t));
        CHECK(value.isSigned() == std::is_signed_v<wchar_t>);
    }
    LEXER_OUTPUTS_WITH("'asdf'", Catch::Contains(DISCARDING_ALL_BUT_FIRST_CHARACTER));
    LEXER_OUTPUTS_WITH("'as\ndawd'", Catch::Contains(NEWLINE_IN_N_USE_BACKLASH_N.args(CHARACTER_LITERAL)));
}

TEST_CASE("Lexing unicode", "[lexer]")
{
    SECTION("Characters")
    {
        SECTION("UTF-16")
        {
            auto result = lexes("L'貓'", cld::Tests::x64windowsGnu);
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
            CHECK(value == u'貓');
            CHECK(value.getBitWidth() == 16);
            CHECK(value.isUnsigned());
            LEXER_OUTPUTS_WITH("'貓'", Catch::Contains(CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE));
            LEXER_OUTPUTS_WITH("'🍌'", Catch::Contains(CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE));
        }
        SECTION("UTF-32")
        {
            auto result = lexes("L'🍌'", cld::Tests::x64linux);
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
            CHECK(value == U'🍌');
            CHECK(value.getBitWidth() == 32);
            CHECK(value.isSigned());
        }
    }
}

TEST_CASE("Lexing string literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = lexes("\"test\"");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "test");
        result = lexes("\"\"");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "");
        result = lexes("L\"\"");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<cld::Lexer::NonCharString>(result.data()[0].getValue()));
        CHECK(std::get<cld::Lexer::NonCharString>(result.data()[0].getValue()) == L"");
    }
    SECTION("Escapes")
    {
        auto result = lexes(R"("\\\b\t\'\f\v\?\n\a\r\"")");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "\\\b\t\'\f\v\?\n\a\r\"");
    }
    SECTION("Octals")
    {
        LEXER_OUTPUTS_WITH("\"\\9\"", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
        auto result = lexes("\"\\070\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        REQUIRE(std::get<std::string>(result.data()[0].getValue()) == "\070");
    }
    SECTION("Hex")
    {
        LEXER_OUTPUTS_WITH("\"\\xG\"", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        LEXER_OUTPUTS_WITH("\"\\x\"", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        auto result = lexes("\"\\x070\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        REQUIRE(std::get<std::string>(result.data()[0].getValue()) == "\x070");
    }
    SECTION("Multibyte")
    {
        auto result =
            lexes("L\"5\"", cld::LanguageOptions(cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 8));
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<cld::Lexer::NonCharString>(result.data()[0].getValue()));
        REQUIRE(std::get<cld::Lexer::NonCharString>(result.data()[0].getValue()) == L"5");
    }
    SECTION("Universal characters")
    {
        LEXER_OUTPUTS_WITH("\"\\u\"", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\u")));
        LEXER_OUTPUTS_WITH("\"\\U\"", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\U")));
        LEXER_OUTPUTS_WITH("\"\\u56\"", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS.args("2")));
        LEXER_OUTPUTS_WITH("\"\\u0090\"", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                              "0090", VALUE_MUSTNT_BE_LESS_THAN_A0)));
        LEXER_OUTPUTS_WITH("L\"\\uD977\"", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                               "D977", VALUE_MUSTNT_BE_IN_RANGE)));
        LEXER_OUTPUTS_WITH("\"\\U56\"", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS.args("6")));
        LEXER_OUTPUTS_WITH("\"\\U00000090\"", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                                  "00000090", VALUE_MUSTNT_BE_LESS_THAN_A0)));
        LEXER_OUTPUTS_WITH("L\"\\U0000D977\"", Catch::Contains(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N.args(
                                                   "0000D977", VALUE_MUSTNT_BE_IN_RANGE)));
        auto result = lexes("L\"\\u3435\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<cld::Lexer::NonCharString>(result.data()[0].getValue()));
        REQUIRE(std::get<cld::Lexer::NonCharString>(result.data()[0].getValue()) == L"\u3435");
        result = lexes("L\"\\U00003435\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<cld::Lexer::NonCharString>(result.data()[0].getValue()));
        REQUIRE(std::get<cld::Lexer::NonCharString>(result.data()[0].getValue()) == L"\u3435");
    }
    LEXER_OUTPUTS_WITH("\"as\ndawd\"", Catch::Contains(NEWLINE_IN_N_USE_BACKLASH_N.args(STRING_LITERAL)));
}

TEST_CASE("Lexing Number Literals", "[lexer]")
{
    SECTION("Integers")
    {
        SECTION("Signed")
        {
            auto result = lexes("534534");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Int);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
            CHECK(value == 534534);
            CHECK(value.getBitWidth() == sizeof(int) * 8);
            CHECK(value.isSigned());
        }
        SECTION("Unsigned")
        {
            auto result = lexes("534534u");
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::UnsignedInt);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
            CHECK(value == 534534);
            CHECK(value.getBitWidth() == sizeof(unsigned int) * 8);
            CHECK(value.isUnsigned());
            LEXER_OUTPUTS_WITH("5u5", Catch::Contains(INVALID_LITERAL_SUFFIX.args("u5")));
        }
        LEXER_OUTPUTS_WITH("5x3", Catch::Contains(INVALID_LITERAL_SUFFIX.args("x3")));
    }
    SECTION("Floating point")
    {
        SECTION("Double")
        {
            auto result = lexes("534534.0");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 534534.0);
        }
        SECTION("Float")
        {
            auto result = lexes("534534.f");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Float);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            REQUIRE(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEsingle);
            CHECK(fp.convertToFloat() == 534534.f);
            result = lexes(std::to_string(std::numeric_limits<float>::max()) + "f");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Float);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            REQUIRE(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEsingle);
            CHECK(fp.convertToFloat() == std::numeric_limits<float>::max());
        }
        SECTION("Long double")
        {
            SECTION("64 bit")
            {
                auto result = lexes("534534.0L", cld::Tests::x64windowsMsvc);
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::LongDouble);
                REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(fp.convertToDouble() == 534534.0);
            }
            SECTION("80 bit")
            {
                auto result = lexes("1.18e4900L", cld::Tests::x64windowsGnu);
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::LongDouble);
                REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_x87DoubleExtended);
                CHECK(fp.bitwiseIsEqual(llvm::APFloat(llvm::APFloat::x87DoubleExtended(), "1.18e4900")));
            }
            SECTION("128 bit")
            {
                auto result = lexes(
                    "1.18e4900L", cld::LanguageOptions(cld::LanguageOptions::C99, 1, true, 4, false, 2, 4, 8, 128, 8));
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::LongDouble);
                REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEquad);
                CHECK(fp.bitwiseIsEqual(llvm::APFloat(llvm::APFloat::IEEEquad(), "1.18e4900")));
            }
        }
        SECTION("Dot only")
        {
            auto result = lexes(".5");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 0.5);
        }
        LEXER_OUTPUTS_WITH("0.5.3", Catch::Contains(INVALID_LITERAL_SUFFIX.args(".3")));
        LEXER_OUTPUTS_WITH("0.5.3F", Catch::Contains(INVALID_LITERAL_SUFFIX.args(".3F")));
        LEXER_OUTPUTS_WITH("0.53fF", Catch::Contains(INVALID_LITERAL_SUFFIX.args("fF")));
        std::array<std::pair<const char*, double>, 6> results = {
            std::pair{"1e-19", 1e-19},   std::pair{"2e32", 2e32},   std::pair{"01e-19", 01e-19},
            std::pair{"01E-19", 01e-19}, std::pair{"02e32", 02e32}, std::pair{"02E32", 02e32}};
        for (auto [input, resulting] : results)
        {
            DYNAMIC_SECTION(input)
            {
                auto result = lexes(input);
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
                REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
                auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
                CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(fp.convertToDouble() == resulting);
            }
        }
    }
    SECTION("Octal")
    {
        auto result = lexes("070");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Int);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 070);
        CHECK(value.getBitWidth() == 8 * sizeof(int));
        CHECK(value.isSigned());
        LEXER_OUTPUTS_WITH("08z1", Catch::Contains(INVALID_OCTAL_CHARACTER.args("8"))
                                       && Catch::Contains(INVALID_LITERAL_SUFFIX.args("z1")));
    }
    SECTION("Hex")
    {
        SECTION("Integer")
        {
            auto result = lexes("0x38");
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Int);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
            CHECK(value == 0x38);
            CHECK(value.getBitWidth() == 8 * sizeof(int));
            CHECK(value.isSigned());
        }
        SECTION("Floating point")
        {
            auto result = lexes("0x0.DE488631p8");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            auto fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 0x0.DE488631p8);

            result = lexes("0x0.DE488631P8");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 0x0.DE488631p8);

            result = lexes("0x0.DE488631P+8");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 0x0.DE488631p8);

            result = lexes("0x0.DE488631P-8");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Double);
            REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<llvm::APFloat>(result.data()[0].getValue()));
            fp = std::get<llvm::APFloat>(result.data()[0].getValue());
            CHECK(llvm::APFloat::SemanticsToEnum(fp.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(fp.convertToDouble() == 0x0.DE488631p-8);
        }

        LEXER_OUTPUTS_WITH("0x0.5", Catch::Contains(BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT));
        LEXER_OUTPUTS_WITH("0x0.5p", Catch::Contains(EXPECTED_DIGITS_AFTER_EXPONENT));
        LEXER_OUTPUTS_WITH("0x5x5", Catch::Contains(INVALID_LITERAL_SUFFIX.args("x5")));
    }
    SECTION("Integer type selection")
    {
        auto test = [](const std::string& text, const llvm::APSInt& result) {
            {
                INFO(text);
                auto tokens = lexes(text);
                REQUIRE_FALSE(tokens.data().empty());
                CHECK(tokens.data().size() == 1);
                REQUIRE(tokens.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<llvm::APSInt>(tokens.data()[0].getValue()));
                CHECK(std::get<llvm::APSInt>(tokens.data()[0].getValue()) == result);
            }
        };
        test("5", llvm::APSInt(llvm::APInt(32, 5), false));
        auto overUInt32 = static_cast<std::int64_t>(std::numeric_limits<std::int32_t>::max()) * 2 + 6;
        test(std::to_string(overUInt32), llvm::APSInt(llvm::APInt(64, overUInt32), false));
        test(std::to_string(overUInt32) + 'u', llvm::APSInt(llvm::APInt(64, overUInt32)));
        test("0x5", llvm::APSInt(llvm::APInt(32, 5), false));
        test("0xFFFFFFFF", llvm::APSInt(llvm::APInt(32, 0xFFFFFFFF)));

        std::string output;

        llvm::raw_string_ostream str(output);
        str << llvm::format_hex(overUInt32, 0);
        test(str.str(), llvm::APSInt(llvm::APInt(64, overUInt32), false));
        output.clear();
        auto overInt64 = static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()) + 6;
        str << llvm::format_hex(overInt64, 0);
        test(str.str(), llvm::APSInt(llvm::APInt(64, overInt64)));
        output.clear();
        str << llvm::format_hex(overInt64, 0) << "ll";
        test(str.str(), llvm::APSInt(llvm::APInt(64, overInt64)));
        output.clear();
        str << llvm::format_hex(overInt64, 0) << "ull";
        test(str.str(), llvm::APSInt(llvm::APInt(64, overInt64)));
        output.clear();
    }
    SECTION("Longs")
    {
        auto result = lexes("534534l");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Long);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 534534);
        CHECK(value.getBitWidth() == 8 * sizeof(long));
        CHECK(value.isSigned());

        result = lexes("534534L");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::Long);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 534534);
        CHECK(value.getBitWidth() == 8 * sizeof(long));
        CHECK(value.isSigned());

        result = lexes("534534uL");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::UnsignedLong);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 534534);
        CHECK(value.getBitWidth() == 8 * sizeof(unsigned long));
        CHECK(value.isUnsigned());
    }
    SECTION("Long longs")
    {
        auto result = lexes("534534ll");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::LongLong);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        auto value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 534534);
        CHECK(value.getBitWidth() == 8 * sizeof(long long));
        CHECK(value.isSigned());

        result = lexes("534534LL");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::LongLong);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 534534);
        CHECK(value.getBitWidth() == 8 * sizeof(long long));
        CHECK(value.isSigned());

        result = lexes("534534uLL");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        CHECK(result.data()[0].getType() == cld::Lexer::Token::Type::UnsignedLongLong);
        REQUIRE(result.data()[0].getTokenType() == cld::Lexer::TokenType::Literal);
        value = std::get<llvm::APSInt>(result.data()[0].getValue());
        CHECK(value == 534534);
        CHECK(value.getBitWidth() == 8 * sizeof(unsigned long long));
        CHECK(value.isUnsigned());

        LEXER_OUTPUTS_WITH("534534lL", Catch::Contains(INVALID_LITERAL_SUFFIX.args("lL")));
        LEXER_OUTPUTS_WITH("534534Ll", Catch::Contains(INVALID_LITERAL_SUFFIX.args("Ll")));
    }
    SECTION("Too big")
    {
        LEXER_OUTPUTS_WITH("18446744073709551618", Catch::Contains(INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE));
        LEXER_OUTPUTS_WITH("0x1FFFFFFFFFFFFFFFF", Catch::Contains(INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE));
        LEXER_OUTPUTS_WITH("077777777777777777777777", Catch::Contains(INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE));
    }
}

TEST_CASE("Lexing Punctuation", "[lexer]")
{
    auto result =
        lexes(". ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>= ~ ^: ^ ==");
    REQUIRE(result.data().size() == 35);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::Dot);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::Ellipse);
    CHECK(result.data().at(2).getTokenType() == cld::Lexer::TokenType::GreaterThan);
    CHECK(result.data().at(3).getTokenType() == cld::Lexer::TokenType::Arrow);
    CHECK(result.data().at(4).getTokenType() == cld::Lexer::TokenType::ShiftRight);
    CHECK(result.data().at(5).getTokenType() == cld::Lexer::TokenType::LessThan);
    CHECK(result.data().at(6).getTokenType() == cld::Lexer::TokenType::ShiftLeft);
    CHECK(result.data().at(7).getTokenType() == cld::Lexer::TokenType::Ampersand);
    CHECK(result.data().at(8).getTokenType() == cld::Lexer::TokenType::LogicAnd);
    CHECK(result.data().at(9).getTokenType() == cld::Lexer::TokenType::BitOr);
    CHECK(result.data().at(10).getTokenType() == cld::Lexer::TokenType::LogicOr);
    CHECK(result.data().at(11).getTokenType() == cld::Lexer::TokenType::Plus);
    CHECK(result.data().at(12).getTokenType() == cld::Lexer::TokenType::Increment);
    CHECK(result.data().at(13).getTokenType() == cld::Lexer::TokenType::Minus);
    CHECK(result.data().at(14).getTokenType() == cld::Lexer::TokenType::Decrement);
    CHECK(result.data().at(15).getTokenType() == cld::Lexer::TokenType::Assignment);
    CHECK(result.data().at(16).getTokenType() == cld::Lexer::TokenType::Equal);
    CHECK(result.data().at(17).getTokenType() == cld::Lexer::TokenType::NotEqual);
    CHECK(result.data().at(18).getTokenType() == cld::Lexer::TokenType::GreaterThanOrEqual);
    CHECK(result.data().at(19).getTokenType() == cld::Lexer::TokenType::LessThanOrEqual);
    CHECK(result.data().at(20).getTokenType() == cld::Lexer::TokenType::PlusAssign);
    CHECK(result.data().at(21).getTokenType() == cld::Lexer::TokenType::MinusAssign);
    CHECK(result.data().at(22).getTokenType() == cld::Lexer::TokenType::DivideAssign);
    CHECK(result.data().at(23).getTokenType() == cld::Lexer::TokenType::MultiplyAssign);
    CHECK(result.data().at(24).getTokenType() == cld::Lexer::TokenType::ModuloAssign);
    CHECK(result.data().at(25).getTokenType() == cld::Lexer::TokenType::BitAndAssign);
    CHECK(result.data().at(26).getTokenType() == cld::Lexer::TokenType::BitOrAssign);
    CHECK(result.data().at(27).getTokenType() == cld::Lexer::TokenType::BitXorAssign);
    CHECK(result.data().at(28).getTokenType() == cld::Lexer::TokenType::ShiftLeftAssign);
    CHECK(result.data().at(29).getTokenType() == cld::Lexer::TokenType::ShiftRightAssign);
    CHECK(result.data().at(30).getTokenType() == cld::Lexer::TokenType::BitWiseNegation);
    CHECK(result.data().at(31).getTokenType() == cld::Lexer::TokenType::BitXor);
    CHECK(result.data().at(32).getTokenType() == cld::Lexer::TokenType::Colon);
    CHECK(result.data().at(33).getTokenType() == cld::Lexer::TokenType::BitXor);
    CHECK(result.data().at(34).getTokenType() == cld::Lexer::TokenType::Equal);
    result = lexes("=>");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Assignment);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::GreaterThan);
    result = lexes("&&=");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::LogicAnd);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Assignment);
    result = lexes("..5");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Dot);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Literal);
}

TEST_CASE("Lexing unknown characters", "[lexer]")
{
    LEXER_OUTPUTS_WITH("貓 〺", Catch::Contains(UNEXPECTED_CHARACTER.args("〺")));
    LEXER_OUTPUTS_WITH("$", Catch::Contains(UNEXPECTED_CHARACTER.args("$")));
    LEXER_OUTPUTS_WITH("\\u0024", Catch::Contains(UNEXPECTED_CHARACTER.args("\\u0024")));
    LEXER_OUTPUTS_WITH("_\\\\\nu0024", Catch::Contains(UNEXPECTED_CHARACTER.args("\\u0024")));
}

TEST_CASE("Lexing positions", "[lexer]")
{
    using namespace cld::Lexer;
    SECTION("Random tokens")
    {
        auto result = lexes(
            "(){}[];,:? '5'\"text\"test 5343 0.534 .343 0x3 /*\n   */text / * . ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>=");
        REQUIRE(result.data().size() == 50);
        std::array correct = {std::tuple{TokenType::OpenParentheses, 1u, 0u, 1u},
                              std::tuple{TokenType::CloseParentheses, 1u, 1u, 1u},
                              std::tuple{TokenType::OpenBrace, 1u, 2u, 1u},
                              std::tuple{TokenType::CloseBrace, 1u, 3u, 1u},
                              std::tuple{TokenType::OpenSquareBracket, 1u, 4u, 1u},
                              std::tuple{TokenType::CloseSquareBracket, 1u, 5u, 1u},
                              std::tuple{TokenType::SemiColon, 1u, 6u, 1u},
                              std::tuple{TokenType::Comma, 1u, 7u, 1u},
                              std::tuple{TokenType::Colon, 1u, 8u, 1u},
                              std::tuple{TokenType::QuestionMark, 1u, 9u, 1u},
                              std::tuple{TokenType::Literal, 1u, 11u, 3u},
                              std::tuple{TokenType::StringLiteral, 1u, 14u, 6u},
                              std::tuple{TokenType::Identifier, 1u, 20u, 4u},
                              std::tuple{TokenType::Literal, 1u, 25u, 4u},
                              std::tuple{TokenType::Literal, 1u, 30u, 5u},
                              std::tuple{TokenType::Literal, 1u, 36u, 4u},
                              std::tuple{TokenType::Literal, 1u, 41u, 3u},
                              std::tuple{TokenType::Identifier, 2u, 5u, 4u},
                              std::tuple{TokenType::Division, 2u, 10u, 1u},
                              std::tuple{TokenType::Asterisk, 2u, 12u, 1u},
                              std::tuple{TokenType::Dot, 2u, 14u, 1u},
                              std::tuple{TokenType::Ellipse, 2u, 16u, 3u},
                              std::tuple{TokenType::GreaterThan, 2u, 20u, 1u},
                              std::tuple{TokenType::Arrow, 2u, 22u, 2u},
                              std::tuple{TokenType::ShiftRight, 2u, 25u, 2u},
                              std::tuple{TokenType::LessThan, 2u, 28u, 1u},
                              std::tuple{TokenType::ShiftLeft, 2u, 30u, 2u},
                              std::tuple{TokenType::Ampersand, 2u, 33u, 1u},
                              std::tuple{TokenType::LogicAnd, 2u, 35u, 2u},
                              std::tuple{TokenType::BitOr, 2u, 38u, 1u},
                              std::tuple{TokenType::LogicOr, 2u, 40u, 2u},
                              std::tuple{TokenType::Plus, 2u, 43u, 1u},
                              std::tuple{TokenType::Increment, 2u, 45u, 2u},
                              std::tuple{TokenType::Minus, 2u, 48u, 1u},
                              std::tuple{TokenType::Decrement, 2u, 50u, 2u},
                              std::tuple{TokenType::Assignment, 2u, 53u, 1u},
                              std::tuple{TokenType::Equal, 2u, 55u, 2u},
                              std::tuple{TokenType::NotEqual, 2u, 58u, 2u},
                              std::tuple{TokenType::GreaterThanOrEqual, 2u, 61u, 2u},
                              std::tuple{TokenType::LessThanOrEqual, 2u, 64u, 2u},
                              std::tuple{TokenType::PlusAssign, 2u, 67u, 2u},
                              std::tuple{TokenType::MinusAssign, 2u, 70u, 2u},
                              std::tuple{TokenType::DivideAssign, 2u, 73u, 2u},
                              std::tuple{TokenType::MultiplyAssign, 2u, 76u, 2u},
                              std::tuple{TokenType::ModuloAssign, 2u, 79u, 2u},
                              std::tuple{TokenType::BitAndAssign, 2u, 82u, 2u},
                              std::tuple{TokenType::BitOrAssign, 2u, 85u, 2u},
                              std::tuple{TokenType::BitXorAssign, 2u, 88u, 2u},
                              std::tuple{TokenType::ShiftLeftAssign, 2u, 91u, 3u},
                              std::tuple{TokenType::ShiftRightAssign, 2u, 95u, 3u}};
        for (std::size_t i = 0; i < correct.size(); i++)
        {
            DYNAMIC_SECTION("Token " << result.data()[i].getRepresentation())
            {
                CHECK(result.data()[i].getTokenType() == std::get<0>(correct[i]));
                CHECK(result.getLineNumber(result.data()[i].getOffset()) == std::get<1>(correct[i]));
                CHECK(result.data()[i].getOffset()
                          - result.getLineStartOffset(result.getLineNumber(result.data()[i].getOffset()))
                      == std::get<2>(correct[i]));
                CHECK(result.data()[i].getLength() == std::get<3>(correct[i]));
            }
        }
    }
    SECTION("Actual program")
    {
        auto result = lexes(R"(void updateHeight(TreeNode* h)
{
    if (height(h->right) > height(h->left))
    {
        h->height = height(h->right);
    }
    else
    {
        h->height = height(h->left);
    }
    5 + 7 - .53f + "dwawd""test"id
} )");
        REQUIRE(result.data().size() == 61);
        std::array correct = {std::tuple{TokenType::VoidKeyword, 1, 0, 4},
                              std::tuple{TokenType::Identifier, 1, 5, 12},
                              std::tuple{TokenType::OpenParentheses, 1, 17, 1},
                              std::tuple{TokenType::Identifier, 1, 18, 8},
                              std::tuple{TokenType::Asterisk, 1, 26, 1},
                              std::tuple{TokenType::Identifier, 1, 28, 1},
                              std::tuple{TokenType::CloseParentheses, 1, 29, 1},
                              std::tuple{TokenType::OpenBrace, 2, 0, 1},
                              std::tuple{TokenType::IfKeyword, 3, 4, 2},
                              std::tuple{TokenType::OpenParentheses, 3, 7, 1},
                              std::tuple{TokenType::Identifier, 3, 8, 6},
                              std::tuple{TokenType::OpenParentheses, 3, 14, 1},
                              std::tuple{TokenType::Identifier, 3, 15, 1},
                              std::tuple{TokenType::Arrow, 3, 16, 2},
                              std::tuple{TokenType::Identifier, 3, 18, 5},
                              std::tuple{TokenType::CloseParentheses, 3, 23, 1},
                              std::tuple{TokenType::GreaterThan, 3, 25, 1},
                              std::tuple{TokenType::Identifier, 3, 27, 6},
                              std::tuple{TokenType::OpenParentheses, 3, 33, 1},
                              std::tuple{TokenType::Identifier, 3, 34, 1},
                              std::tuple{TokenType::Arrow, 3, 35, 2},
                              std::tuple{TokenType::Identifier, 3, 37, 4},
                              std::tuple{TokenType::CloseParentheses, 3, 41, 1},
                              std::tuple{TokenType::CloseParentheses, 3, 42, 1},
                              std::tuple{TokenType::OpenBrace, 4, 4, 1},
                              std::tuple{TokenType::Identifier, 5, 8, 1},
                              std::tuple{TokenType::Arrow, 5, 9, 2},
                              std::tuple{TokenType::Identifier, 5, 11, 6},
                              std::tuple{TokenType::Assignment, 5, 18, 1},
                              std::tuple{TokenType::Identifier, 5, 20, 6},
                              std::tuple{TokenType::OpenParentheses, 5, 26, 1},
                              std::tuple{TokenType::Identifier, 5, 27, 1},
                              std::tuple{TokenType::Arrow, 5, 28, 2},
                              std::tuple{TokenType::Identifier, 5, 30, 5},
                              std::tuple{TokenType::CloseParentheses, 5, 35, 1},
                              std::tuple{TokenType::SemiColon, 5, 36, 1},
                              std::tuple{TokenType::CloseBrace, 6, 4, 1},
                              std::tuple{TokenType::ElseKeyword, 7, 4, 4},
                              std::tuple{TokenType::OpenBrace, 8, 4, 1},
                              std::tuple{TokenType::Identifier, 9, 8, 1},
                              std::tuple{TokenType::Arrow, 9, 9, 2},
                              std::tuple{TokenType::Identifier, 9, 11, 6},
                              std::tuple{TokenType::Assignment, 9, 18, 1},
                              std::tuple{TokenType::Identifier, 9, 20, 6},
                              std::tuple{TokenType::OpenParentheses, 9, 26, 1},
                              std::tuple{TokenType::Identifier, 9, 27, 1},
                              std::tuple{TokenType::Arrow, 9, 28, 2},
                              std::tuple{TokenType::Identifier, 9, 30, 4},
                              std::tuple{TokenType::CloseParentheses, 9, 34, 1},
                              std::tuple{TokenType::SemiColon, 9, 35, 1},
                              std::tuple{TokenType::CloseBrace, 10, 4, 1},
                              std::tuple{TokenType::Literal, 11, 4, 1},
                              std::tuple{TokenType::Plus, 11, 6, 1},
                              std::tuple{TokenType::Literal, 11, 8, 1},
                              std::tuple{TokenType::Minus, 11, 10, 1},
                              std::tuple{TokenType::Literal, 11, 12, 4},
                              std::tuple{TokenType::Plus, 11, 17, 1},
                              std::tuple{TokenType::StringLiteral, 11, 19, 7},
                              std::tuple{TokenType::StringLiteral, 11, 26, 6},
                              std::tuple{TokenType::Identifier, 11, 32, 2},
                              std::tuple{TokenType::CloseBrace, 12, 0, 1}};
        for (std::size_t i = 0; i < correct.size(); i++)
        {
            DYNAMIC_SECTION("Token " << i << ": " << result.data()[i].getRepresentation())
            {
                CHECK(std::tuple(result.data()[i].getTokenType(), result.getLineNumber(result.data()[i].getOffset()),
                                 result.data()[i].getOffset()
                                     - result.getLineStartOffset(result.getLineNumber(result.data()[i].getOffset())),
                                 result.data()[i].getLength())
                      == correct[i]);
            }
        }
    }
}

TEST_CASE("Lexing digraphs", "[lexer]")
{
    auto result = lexes("<: :> <% %> %: %:%:");
    REQUIRE(result.data().size() == 6);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::OpenSquareBracket);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::CloseSquareBracket);
    CHECK(result.data().at(2).getTokenType() == cld::Lexer::TokenType::OpenBrace);
    CHECK(result.data().at(3).getTokenType() == cld::Lexer::TokenType::CloseBrace);
    CHECK(result.data().at(4).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(5).getTokenType() == cld::Lexer::TokenType::DoublePound);
    CHECK(cld::Lexer::reconstructTrimmed(result, result.data().begin(), result.data().end()) == "<: :> <% %> %: %:%:");

    result = lexes("%: %: #%: %:# # %: %: #");
    REQUIRE(result.data().size() == 8);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(2).getTokenType() == cld::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(3).getTokenType() == cld::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(4).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(5).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(6).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(7).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(cld::Lexer::reconstructTrimmed(result, result.data().begin(), result.data().end())
          == "%: %: #%: %:# # %: %: #");

    result = lexes("#%");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::Percent);
    CHECK(result.data().at(0).getOffset() == 0);
    CHECK(result.data().at(1).getOffset() == 1);
    CHECK(result.data().at(0).getRepresentation() == "#");
    CHECK(result.data().at(1).getRepresentation() == "%");

    result = lexes("%:\\\n%");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::Percent);
    CHECK(result.data().at(0).getRepresentation() == "%:");
    CHECK(result.data().at(1).getRepresentation() == "%");

    result = lexes("#\\\n%");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::Percent);
    CHECK(result.data().at(0).getRepresentation() == "#");
    CHECK(result.data().at(1).getRepresentation() == "%");

    result = lexes("%:\\\n:");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == cld::Lexer::TokenType::Colon);
    CHECK(result.data().at(0).getRepresentation() == "%:");
    CHECK(result.data().at(1).getRepresentation() == ":");

    result = lexes("%:\\\n%:");
    REQUIRE(result.data().size() == 1);
    CHECK(result.data().at(0).getTokenType() == cld::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(0).getRepresentation() == "%:\\\n%:");
}

TEST_CASE("Lexing input reconstruction", "[lexer]")
{
    auto source =
        "ad(){}<% %> \"test\" 52345 ; , - ~ ! + * / % && || & | ^ == != < <= > >= = += -= /= *= %= <<= >>= &= |= ^= >> << ++ -- : ? void char short int long float double signed unsigned typedef extern static auto register const sizeof return break continue do else for if while [] struct . -> switch case default union volatile enum goto ... restrict inline # ##";
    auto result = lexes(source);
    REQUIRE(cld::Lexer::reconstructTrimmed(result, result.data().begin(), result.data().end()) == source);
}

TEST_CASE("Lexing include directives", "[lexer]")
{
    SECTION("< >")
    {
        auto result = pplexes("#include <agejf 4er325öüöü-3/3423354f\\wd3rf?ß>");
        REQUIRE(result.data().size() == 4);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Pound);
        REQUIRE(result.data()[1].getTokenType() == cld::Lexer::TokenType::Identifier);
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "include");
        REQUIRE(result.data()[2].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "agejf 4er325öüöü-3/3423354f\\wd3rf?ß");
        CHECK(result.data()[2].getRepresentation() == "<agejf 4er325öüöü-3/3423354f\\wd3rf?ß>");
    }
    SECTION("\" \"")
    {
        auto result = pplexes("#include \"agejf 4er325öüöü-3/3423354f\\nwd3rf?ß\"");
        REQUIRE(result.data().size() == 4);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Pound);
        REQUIRE(result.data()[1].getTokenType() == cld::Lexer::TokenType::Identifier);
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "include");
        REQUIRE(result.data()[2].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "agejf 4er325öüöü-3/3423354f\\nwd3rf?ß");
        CHECK(result.data()[2].getRepresentation() == "\"agejf 4er325öüöü-3/3423354f\\nwd3rf?ß\"");
    }
}

TEST_CASE("Lexing universal character as suffix", "[lexer]")
{
    SECTION("Interrupting universal character")
    {
        std::string buffer;
        llvm::raw_string_ostream ss(buffer);
        auto result = cld::Lexer::tokenize("\\u\\u00B5", cld::LanguageOptions::native(), false, &ss);
        CHECK_THAT(buffer, Catch::Contains(STRAY_N_IN_PROGRAM.args("\\")));
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "µ");
        CHECK(result.data()[0].getRepresentation() == "\\u00B5");
    }
    SECTION("Punctuation")
    {
        auto result = lexes("=\\u00B5");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Assignment);
        CHECK(result.data()[0].getRepresentation() == "=");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[1].getValue()));
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "µ");
        CHECK(result.data()[1].getRepresentation() == "\\u00B5");
    }
    SECTION("Dots")
    {
        auto result = lexes(".\\u00B5");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Dot);
        CHECK(result.data()[0].getRepresentation() == ".");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[1].getValue()));
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "µ");
        CHECK(result.data()[1].getRepresentation() == "\\u00B5");

        result = lexes("..\\u00B5");
        REQUIRE(result.data().size() == 3);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Dot);
        CHECK(result.data()[0].getRepresentation() == ".");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Dot);
        CHECK(result.data()[1].getRepresentation() == ".");
        CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[2].getValue()));
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "µ");
        CHECK(result.data()[2].getRepresentation() == "\\u00B5");
    }
}

TEST_CASE("Lexing Comments", "[lexer]")
{
    SECTION("Line comments")
    {
        auto result = lexes("test//wdwdw\ntest");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getOffset() == 0);
        CHECK(result.data()[0].getLine(result) == 1);
        CHECK(result.data()[0].getColumn(result) == 1);
        CHECK(result.data()[1].getOffset() == 12);
        CHECK(result.data()[1].getLine(result) == 2);
        CHECK(result.data()[1].getColumn(result) == 1);
        result = lexes("test//wdwdw\\\ntest");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getOffset() == 0);
        CHECK(result.data()[0].getLine(result) == 1);
        CHECK(result.data()[0].getColumn(result) == 1);
    }
    SECTION("Block comments")
    {
        auto result = lexes("test/*wdwdw*/test");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getOffset() == 0);
        CHECK(result.data()[0].getLine(result) == 1);
        CHECK(result.data()[0].getColumn(result) == 1);
        CHECK(result.data()[1].getOffset() == 13);
        CHECK(result.data()[1].getLine(result) == 1);
        CHECK(result.data()[1].getColumn(result) == 14);
        result = lexes("test/*wdwdw*\\\n/test");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getOffset() == 0);
        CHECK(result.data()[0].getLine(result) == 1);
        CHECK(result.data()[0].getColumn(result) == 1);
        CHECK(result.data()[1].getOffset() == 15);
        CHECK(result.data()[1].getLine(result) == 2);
        CHECK(result.data()[1].getColumn(result) == 2);
        LEXER_OUTPUTS_WITH("ad\n/*/", Catch::Contains(UNTERMINATED_N.args(BLOCK_COMMENT)));
    }
}

TEST_CASE("Lexing unterminated tokens", "[lexer]")
{
    LEXER_OUTPUTS_WITH("ad\n\"test", Catch::Contains(UNTERMINATED_N.args(STRING_LITERAL)));
    LEXER_OUTPUTS_WITH("ad\n'test", Catch::Contains(UNTERMINATED_N.args(CHARACTER_LITERAL)));
    LEXER_OUTPUTS_WITH("ad\n/*test", Catch::Contains(UNTERMINATED_N.args(BLOCK_COMMENT)));
}

TEST_CASE("Lexing Preprocessing numbers", "[lexer]")
{
    SECTION("Simple")
    {
        auto result = pplexes("5");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "5");
    }
    SECTION("Starting with dot")
    {
        auto result = pplexes(".5");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == ".5");
        result = pplexes("..5");
        REQUIRE(result.data().size() == 3);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Dot);
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[1].getValue()));
        CHECK(std::get<std::string>(result.data()[1].getValue()) == ".5");
    }
    SECTION("Containing identifier")
    {
        auto result = pplexes("5Ex");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "5Ex");
    }
    SECTION("Sign after e,E,p or P")
    {
        std::vector<std::string_view> text = {"5E+", "5E-", "5e+", "5e-", "5P+", "5P-", "5p+", "5p-"};
        for (auto iter : text)
        {
            DYNAMIC_SECTION(iter)
            {
                auto result = pplexes(iter);
                REQUIRE(result.data().size() == 2);
                CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == iter);
            }
        }
    }
    SECTION("Containing dot")
    {
        auto result = pplexes("0.5.3F");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "0.5.3F");
    }
}

TEST_CASE("Lexing Preprocessor universal characters", "[lexer]")
{
    SECTION("Valid")
    {
        auto result = pplexes("0\\u00B5");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "0\\u00B5");
    }
    SECTION("Incomplete")
    {
        auto result = pplexes("0\\ute");
        REQUIRE(result.data().size() == 4);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "0");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Backslash);
        CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[2].getValue()));
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "ute");
    }
    SECTION("Disallowed Value")
    {
        auto result = pplexes("0\\u0099");
        REQUIRE(result.data().size() == 4);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "0");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Backslash);
        CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[2].getValue()));
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "u0099");
    }
    SECTION("Disallowed in identifier")
    {
        auto result = pplexes("a\\u0024");
        REQUIRE(result.data().size() == 5);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "a");
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Backslash);
        CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::Identifier);
        REQUIRE(std::holds_alternative<std::string>(result.data()[2].getValue()));
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "u");
        CHECK(result.data()[3].getTokenType() == cld::Lexer::TokenType::PPNumber);
        REQUIRE(std::holds_alternative<std::string>(result.data()[3].getValue()));
        CHECK(std::get<std::string>(result.data()[3].getValue()) == "0024");
    }
    SECTION("Backslash")
    {
        auto result = pplexes("\\ ");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Backslash);
        CHECK(result.data()[0].getOffset() == 0);
        result = pplexes("u\\ e");
        REQUIRE(result.data().size() == 4);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Identifier);
        CHECK(result.data()[0].getOffset() == 0);
        CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Backslash);
        CHECK(result.data()[1].getOffset() == 1);
        CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::Identifier);
        CHECK(result.data()[2].getOffset() == 3);
    }
}

TEST_CASE("Lexing Preprocessor unterminated characters", "[lexer]")
{
    SECTION("String literal")
    {
        auto result = pplexes("\"test");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Miscellaneous);
    }
    SECTION("String literal")
    {
        auto result = pplexes("'test");
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Miscellaneous);
    }
    PP_LEXER_OUTPUTS_WITH("ad\n/*test", Catch::Contains(UNTERMINATED_N.args(BLOCK_COMMENT)));
    PP_LEXER_OUTPUTS_WITH("ad\n#include \"test", Catch::Contains(UNTERMINATED_N.args(INCLUDE_DIRECTIVE)));
    PP_LEXER_OUTPUTS_WITH("ad\n#include <test", Catch::Contains(UNTERMINATED_N.args(INCLUDE_DIRECTIVE)));
}

TEST_CASE("Lexing Preprocessor Miscellaneous")
{
    auto result = pplexes("〺");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Miscellaneous);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Newline);
    result = pplexes("$");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Miscellaneous);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Newline);
}

TEST_CASE("Lexing Preprocessor Newlines")
{
    // Sanity check
    auto result = lexes("\n");
    CHECK(result.data().empty());
    result = pplexes("#if 0\n50\n#endif");
    REQUIRE(result.data().size() == 9);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Identifier);
    CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::PPNumber);
    CHECK(result.data()[3].getTokenType() == cld::Lexer::TokenType::Newline);
    CHECK(result.data()[4].getTokenType() == cld::Lexer::TokenType::PPNumber);
    CHECK(result.data()[5].getTokenType() == cld::Lexer::TokenType::Newline);
    CHECK(result.data()[6].getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data()[7].getTokenType() == cld::Lexer::TokenType::Identifier);
    CHECK(result.data()[8].getTokenType() == cld::Lexer::TokenType::Newline);
    result = pplexes("#if 0\\\n50\n#endif");
    REQUIRE(result.data().size() == 7);
    CHECK(result.data()[0].getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data()[1].getTokenType() == cld::Lexer::TokenType::Identifier);
    CHECK(result.data()[2].getTokenType() == cld::Lexer::TokenType::PPNumber);
    CHECK(result.data()[3].getTokenType() == cld::Lexer::TokenType::Newline);
    CHECK(result.data()[4].getTokenType() == cld::Lexer::TokenType::Pound);
    CHECK(result.data()[5].getTokenType() == cld::Lexer::TokenType::Identifier);
    CHECK(result.data()[6].getTokenType() == cld::Lexer::TokenType::Newline);
}

namespace
{
std::string toS(const std::vector<std::uint8_t>& data)
{
    std::string input(data.size(), ' ');
    std::transform(data.begin(), data.end(), input.begin(), [](std::uint8_t byte) -> char {
        char result;
        std::memcpy(&result, &byte, 1);
        return result;
    });
    return input;
}
} // namespace

TEST_CASE("Lexing invalid characters", "[lexer]")
{
    LEXER_OUTPUTS_WITH("\xaez\xf0\x9e\xbb\xaf", Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0x1, 0x1, 0x1, 0x1, '\n', 0}),
                       Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U00000001"))
                           && Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U00000000")));
    LEXER_OUTPUTS_WITH(toS({'\n', '*', 0x9b}), Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0xde, '\n'}), Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0xa, 0x0}), Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U00000000")));
    LEXER_OUTPUTS_WITH(toS({'L', '\'', 0xE7, 0xB1, 0x92, 0xE0, 0x80, 0x80, '\''}),
                       Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0xE7, 0xB1, 0x92, 0b11110000, 0b10011110, 0b10111011, 0b10101111}),
                       Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U0001eeef")));
    LEXER_OUTPUTS_WITH(toS({0xE7, 0xB1, 0x92, 0b11110000, 0b10011110, 0b10111011, 0b10101111, 'a'}),
                       Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U0001eeef")));
    LEXER_OUTPUTS_WITH(
        toS({'L', '\'', 0xE7, 0xB1, 0x92, '\'', ' ', 'L', '\'', 0xE7, 0xB1, 0x92, 0xE0, 0x80, 0x80, '\''}),
        Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({'L', '\'', 0xE0, 0x80, 0x80, '\''}), Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({'L', '"', 0xE7, 0xB1, 0x92, 0xE0, 0x80, 0x80, '"'}),
                       Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({
                           'L',
                           '"',
                           0xE7,
                           0xB1,
                           0x92,
                           '"',
                           ' ',
                           'L',
                           '"',
                           0xE7,
                           0xB1,
                           0x92,
                           0xE0,
                           0x80,
                           0x80,
                           '"',
                       }),
                       Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({'L', '"', 0xE0, 0x80, 0x80, '"'}), Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0xE7, 0xB1, 0x92, 0xE0, 0x80, 0x80}), Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0xea}), Catch::Contains(INVALID_UTF8_SEQUENCE));
    LEXER_OUTPUTS_WITH(toS({0x0, 0xa}), Catch::Contains(NON_PRINTABLE_CHARACTER_N.args("\\U00000000")));
    LEXER_OUTPUTS_WITH("\xaez\xd2\x89",
                       Catch::Contains(INVALID_UTF8_SEQUENCE) && Catch::Contains(UNEXPECTED_CHARACTER.args("҉")));
}

TEST_CASE("Lexing fuzzer discoveries", "[lexer]")
{
    cld::Lexer::tokenize("#define   ue(efi,n\\");
    cld::Lexer::tokenize("#if\n"
                         "i3#if\n"
                         "  if\n"
                         "\n"
                         "#if\n"
                         "#if\n"
                         "\n"
                         "se\n"
                         "#else\n"
                         "#if\n"
                         "#else\n"
                         "            ef)\n"
                         "  8\\uEEEEEEEEEEEEEEEEEEEEEse\n"
                         "#",
                         cld::LanguageOptions::native(), true);
    cld::Lexer::tokenize("/:**:*/\\\\\\\\\\\\\\\\\\\\\\\x0a\x0a\x1c\\\\%%)>%%%//m]m]m]0\xf5z\x00z");
    cld::Lexer::tokenize(toS({
        0x2b, 0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x56, 0x6e, 0x5b, 0x2e, 0x0,  0x0,  0x2e, 0x27, 0x2e, 0x0,
        0x0,  0x2e, 0x5d, 0x5d, 0x5d, 0x5d, 0x66, 0xa,  0x2e, 0x5d, 0x5d, 0x5d, 0x5d, 0x98, 0x5d, 0x5d, 0x5d, 0x5d,
        0x5d, 0x5d, 0x2a, 0x2b, 0x25, 0x2a, 0x2a, 0x2b, 0x25, 0x25, 0x2a, 0x2a, 0xa,  0x25, 0x2a, 0x59, 0x1b, 0x37,
        0x37, 0x37, 0x5d, 0x5d, 0x2a, 0x59, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x5d, 0x2a,
        0x2b, 0x25, 0x2a, 0x6e, 0x3a, 0x3b, 0x5d, 0x5d, 0x2a, 0x2b, 0x25, 0x0,  0x0,  0x34,
    }));
    cld::Lexer::tokenize(
        ",&(((((((((((XW_(z((((((((((((((((((\\((((((/AA 910U(4U0U(((                                                                                                      (\n"
        "((((((((    (((((\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B\u000B(((((((((  0U ,       '  /,&/1 (bK  9099.4) A.>0  (******791 4 $ 6 : 449999041)\n"
        "!V1 (b'(6,6:&/h N)K51(* 44***7. 6 :)  a**((   U0AA\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\" \"\"1\"\"\"\"\"\"\"\"\"A\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"5\"\"\"\"U\"\"\"\"\"\"\"\"\"\"\"\"\"\";Y);XW 0U U ");
    cld::Lexer::tokenize("har_^c\"6?c0. 0r_^\x0a\x0ar!!0z?!? u\x0a   ");
    cld::Lexer::tokenize("h\"&u20U^58v\\u .L<bF\".A !< G\x0a"
                         "30bG\x0a0G\x0a:::!\x0a<0");
}

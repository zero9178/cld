#include "catch.hpp"

#include <llvm/Support/Format.h>

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include <array>

#include "TestConfig.hpp"

using namespace OpenCL::ErrorMessages::Lexer;

using namespace Catch::Matchers;

CATCH_REGISTER_ENUM(
    OpenCL::Lexer::TokenType, OpenCL::Lexer::TokenType::Identifier, OpenCL::Lexer::TokenType::OpenParentheses,
    OpenCL::Lexer::TokenType::CloseParentheses, OpenCL::Lexer::TokenType::OpenBrace,
    OpenCL::Lexer::TokenType::CloseBrace, OpenCL::Lexer::TokenType::Literal, OpenCL::Lexer::TokenType::StringLiteral,
    OpenCL::Lexer::TokenType::SemiColon, OpenCL::Lexer::TokenType::Comma, OpenCL::Lexer::TokenType::Minus,
    OpenCL::Lexer::TokenType::BitWiseNegation, OpenCL::Lexer::TokenType::LogicalNegation,
    OpenCL::Lexer::TokenType::Plus, OpenCL::Lexer::TokenType::Asterisk, OpenCL::Lexer::TokenType::Division,
    OpenCL::Lexer::TokenType::Percent, OpenCL::Lexer::TokenType::LogicAnd, OpenCL::Lexer::TokenType::LogicOr,
    OpenCL::Lexer::TokenType::Ampersand, OpenCL::Lexer::TokenType::BitOr, OpenCL::Lexer::TokenType::BitXor,
    OpenCL::Lexer::TokenType::Equal, OpenCL::Lexer::TokenType::NotEqual, OpenCL::Lexer::TokenType::LessThan,
    OpenCL::Lexer::TokenType::LessThanOrEqual, OpenCL::Lexer::TokenType::GreaterThan,
    OpenCL::Lexer::TokenType::GreaterThanOrEqual, OpenCL::Lexer::TokenType::Assignment,
    OpenCL::Lexer::TokenType::PlusAssign, OpenCL::Lexer::TokenType::MinusAssign, OpenCL::Lexer::TokenType::DivideAssign,
    OpenCL::Lexer::TokenType::MultiplyAssign, OpenCL::Lexer::TokenType::ModuloAssign,
    OpenCL::Lexer::TokenType::ShiftLeftAssign, OpenCL::Lexer::TokenType::ShiftRightAssign,
    OpenCL::Lexer::TokenType::BitAndAssign, OpenCL::Lexer::TokenType::BitOrAssign,
    OpenCL::Lexer::TokenType::BitXorAssign, OpenCL::Lexer::TokenType::ShiftRight, OpenCL::Lexer::TokenType::ShiftLeft,
    OpenCL::Lexer::TokenType::Increment, OpenCL::Lexer::TokenType::Decrement, OpenCL::Lexer::TokenType::Colon,
    OpenCL::Lexer::TokenType::QuestionMark, OpenCL::Lexer::TokenType::VoidKeyword,
    OpenCL::Lexer::TokenType::CharKeyword, OpenCL::Lexer::TokenType::ShortKeyword, OpenCL::Lexer::TokenType::IntKeyword,
    OpenCL::Lexer::TokenType::LongKeyword, OpenCL::Lexer::TokenType::FloatKeyword,
    OpenCL::Lexer::TokenType::DoubleKeyword, OpenCL::Lexer::TokenType::SignedKeyword,
    OpenCL::Lexer::TokenType::UnsignedKeyword, OpenCL::Lexer::TokenType::TypedefKeyword,
    OpenCL::Lexer::TokenType::ExternKeyword, OpenCL::Lexer::TokenType::StaticKeyword,
    OpenCL::Lexer::TokenType::AutoKeyword, OpenCL::Lexer::TokenType::RegisterKeyword,
    OpenCL::Lexer::TokenType::ConstKeyword, OpenCL::Lexer::TokenType::RestrictKeyword,
    OpenCL::Lexer::TokenType::SizeofKeyword, OpenCL::Lexer::TokenType::VolatileKeyword,
    OpenCL::Lexer::TokenType::InlineKeyword, OpenCL::Lexer::TokenType::ReturnKeyword,
    OpenCL::Lexer::TokenType::BreakKeyword, OpenCL::Lexer::TokenType::ContinueKeyword,
    OpenCL::Lexer::TokenType::DoKeyword, OpenCL::Lexer::TokenType::ElseKeyword, OpenCL::Lexer::TokenType::ForKeyword,
    OpenCL::Lexer::TokenType::IfKeyword, OpenCL::Lexer::TokenType::WhileKeyword,
    OpenCL::Lexer::TokenType::OpenSquareBracket, OpenCL::Lexer::TokenType::CloseSquareBracket,
    OpenCL::Lexer::TokenType::StructKeyword, OpenCL::Lexer::TokenType::Dot, OpenCL::Lexer::TokenType::Arrow,
    OpenCL::Lexer::TokenType::SwitchKeyword, OpenCL::Lexer::TokenType::CaseKeyword,
    OpenCL::Lexer::TokenType::DefaultKeyword, OpenCL::Lexer::TokenType::UnionKeyword,
    OpenCL::Lexer::TokenType::EnumKeyword, OpenCL::Lexer::TokenType::GotoKeyword, OpenCL::Lexer::TokenType::Ellipse)

#define LEXER_OUTPUTS_WITH(input, match)                                               \
    do                                                                                 \
    {                                                                                  \
        std::string s;                                                                 \
        llvm::raw_string_ostream ss(s);                                                \
        OpenCL::Lexer::tokenize(input, OpenCL::LanguageOptions::native(), false, &ss); \
        CHECK_THAT(s, match);                                                          \
        if (!s.empty() && OpenCL::colourConsoleOutput)                                 \
        {                                                                              \
            OpenCL::Lexer::tokenize(input);                                            \
        }                                                                              \
    } while (0)

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
                auto result = OpenCL::Lexer::tokenize(c);
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == c);
            }
        }
        AND_THEN("an universal character")
        {
            auto result = OpenCL::Lexer::tokenize("\\u00B5");
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
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
                auto result = OpenCL::Lexer::tokenize(c);
                REQUIRE(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
                REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
                CHECK(std::get<std::string>(result.data()[0].getValue()) == c);
            }
        }
        AND_THEN("an universal character")
        {
            auto result = OpenCL::Lexer::tokenize("_\\u00B5");
            REQUIRE(result.data().size() == 1);
            CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
            REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
            CHECK(std::get<std::string>(result.data()[0].getValue()) == "_µ");
            CHECK(result.data()[0].getRepresentation() == "_\\u00B5");
        }
    }
    GIVEN("A universal character")
    {
        WHEN("incomplete")
        {
            LEXER_OUTPUTS_WITH("_\\ute", Catch::Contains(STRAY_N_IN_PROGRAM.args("\\")));
        }
        WHEN("incomplete and multiline")
        {
            LEXER_OUTPUTS_WITH("_\\\\\nute", Catch::Contains(STRAY_N_IN_PROGRAM.args("\\")));
        }
    }
}

TEST_CASE("Lexing Number Literals", "[lexer]")
{
    SECTION("Integers")
    {
        SECTION("Signed")
        {
            auto result = OpenCL::Lexer::tokenize("534534");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
            CHECK(std::get<std::int32_t>(result.data()[0].getValue()) == 534534);
        }
        SECTION("Unsigned")
        {
            auto result = OpenCL::Lexer::tokenize("534534u");
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::uint32_t>(result.data()[0].getValue()));
            CHECK(std::get<std::uint32_t>(result.data()[0].getValue()) == 534534);

            LEXER_OUTPUTS_WITH("5u5", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("u5")));
        }
        LEXER_OUTPUTS_WITH("5x3", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("x3")));
    }
    SECTION("Floating point")
    {
        SECTION("Double")
        {
            auto result = OpenCL::Lexer::tokenize("534534.0");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 534534.0);
        }
        SECTION("Float")
        {
            auto result = OpenCL::Lexer::tokenize("534534.f");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<float>(result.data()[0].getValue()));
            CHECK(std::get<float>(result.data()[0].getValue()) == 534534.f);
        }
        SECTION("Dot only")
        {
            auto result = OpenCL::Lexer::tokenize(".5");
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0.5);
        }
        LEXER_OUTPUTS_WITH("0.5.3", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0.5.3")));
        LEXER_OUTPUTS_WITH("0.5.3F", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0.5.3F")));
        LEXER_OUTPUTS_WITH("0.53fF", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0.53fF")));
        std::array<std::pair<const char*, double>, 6> results = {
            std::pair{"1e-19", 1e-19},   std::pair{"2e32", 2e32},   std::pair{"01e-19", 01e-19},
            std::pair{"01E-19", 01e-19}, std::pair{"02e32", 02e32}, std::pair{"02E32", 02e32}};
        for (auto [input, resulting] : results)
        {
            DYNAMIC_SECTION(input)
            {
                auto result = OpenCL::Lexer::tokenize(input);
                REQUIRE_FALSE(result.data().empty());
                CHECK(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
                CHECK(std::get<double>(result.data()[0].getValue()) == resulting);
            }
        }
    }
    SECTION("Octal")
    {
        auto result = OpenCL::Lexer::tokenize("070");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<int32_t>(result.data()[0].getValue()));
        CHECK(std::get<int32_t>(result.data()[0].getValue()) == 56);
    }
    SECTION("Hex")
    {
        SECTION("Integer")
        {
            auto result = OpenCL::Lexer::tokenize("0x38");
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<int32_t>(result.data()[0].getValue()));
            CHECK(std::get<int32_t>(result.data()[0].getValue()) == 56);
        }
        SECTION("Floating point")
        {
            auto result = OpenCL::Lexer::tokenize("0x0.DE488631p8");
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p8);

            result = OpenCL::Lexer::tokenize("0x0.DE488631P8");
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p8);

            result = OpenCL::Lexer::tokenize("0x0.DE488631P+8");
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p8);

            result = OpenCL::Lexer::tokenize("0x0.DE488631P-8");
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p-8);
        }

        LEXER_OUTPUTS_WITH("0x0.5", Catch::Contains(BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT));
        LEXER_OUTPUTS_WITH("0x0.5p", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0x0.5p")));
        LEXER_OUTPUTS_WITH("0x5x5", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("x5")));
    }
    SECTION("Integer type selection")
    {
        auto test = [](const std::string& text, auto result) {
            using ResultingType = std::decay_t<decltype(result)>;
            DYNAMIC_SECTION(text)
            {
                auto tokens = OpenCL::Lexer::tokenize(text);
                REQUIRE_FALSE(tokens.data().empty());
                CHECK(tokens.data().size() == 1);
                REQUIRE(tokens.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<ResultingType>(tokens.data()[0].getValue()));
                CHECK(std::get<ResultingType>(tokens.data()[0].getValue()) == result);
            }
        };
        test("5", std::int32_t(5));
        auto overUInt32 = static_cast<std::int64_t>(std::numeric_limits<std::int32_t>::max()) * 2 + 6;
        test(std::to_string(overUInt32), std::int64_t(overUInt32));
        test(std::to_string(overUInt32) + 'u', std::uint64_t(overUInt32));
        test("0x5", std::int32_t(5));
        test("0xFFFFFFFF", std::uint32_t(0xFFFFFFFF));

        std::string output;

        llvm::raw_string_ostream str(output);
        str << llvm::format_hex(overUInt32, 0);
        test(str.str(), overUInt32);
        output.clear();
        auto overInt64 = static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()) + 6;
        str << llvm::format_hex(overInt64, 0);
        test(str.str(), std::uint64_t(overInt64));
        output.clear();
        str << llvm::format_hex(overInt64, 0) << "ll";
        test(str.str(), std::uint64_t(overInt64));
        output.clear();
        str << "0x" << llvm::format_hex(overInt64, 0) << "ull";
        test(str.str(), std::uint64_t(overInt64));
        output.clear();
    }
    SECTION("Long longs")
    {
        auto result = OpenCL::Lexer::tokenize("534534ll");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int64_t>(result.data()[0].getValue()));
        CHECK(std::get<std::int64_t>(result.data()[0].getValue()) == 534534);

        result = OpenCL::Lexer::tokenize("534534LL");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int64_t>(result.data()[0].getValue()));
        CHECK(std::get<std::int64_t>(result.data()[0].getValue()) == 534534);

        result = OpenCL::Lexer::tokenize("534534uLL");
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::uint64_t>(result.data()[0].getValue()));
        CHECK(std::get<std::uint64_t>(result.data()[0].getValue()) == 534534);

        LEXER_OUTPUTS_WITH("534534lL", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("lL")));
        LEXER_OUTPUTS_WITH("534534Ll", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("Ll")));
    }
}

TEST_CASE("Lexing Punctuators", "[lexer]")
{
    auto result = OpenCL::Lexer::tokenize(
        ". ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>= ~ ^: ^ ==");
    REQUIRE(result.data().size() == 35);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::Dot);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::Ellipse);
    CHECK(result.data().at(2).getTokenType() == OpenCL::Lexer::TokenType::GreaterThan);
    CHECK(result.data().at(3).getTokenType() == OpenCL::Lexer::TokenType::Arrow);
    CHECK(result.data().at(4).getTokenType() == OpenCL::Lexer::TokenType::ShiftRight);
    CHECK(result.data().at(5).getTokenType() == OpenCL::Lexer::TokenType::LessThan);
    CHECK(result.data().at(6).getTokenType() == OpenCL::Lexer::TokenType::ShiftLeft);
    CHECK(result.data().at(7).getTokenType() == OpenCL::Lexer::TokenType::Ampersand);
    CHECK(result.data().at(8).getTokenType() == OpenCL::Lexer::TokenType::LogicAnd);
    CHECK(result.data().at(9).getTokenType() == OpenCL::Lexer::TokenType::BitOr);
    CHECK(result.data().at(10).getTokenType() == OpenCL::Lexer::TokenType::LogicOr);
    CHECK(result.data().at(11).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.data().at(12).getTokenType() == OpenCL::Lexer::TokenType::Increment);
    CHECK(result.data().at(13).getTokenType() == OpenCL::Lexer::TokenType::Minus);
    CHECK(result.data().at(14).getTokenType() == OpenCL::Lexer::TokenType::Decrement);
    CHECK(result.data().at(15).getTokenType() == OpenCL::Lexer::TokenType::Assignment);
    CHECK(result.data().at(16).getTokenType() == OpenCL::Lexer::TokenType::Equal);
    CHECK(result.data().at(17).getTokenType() == OpenCL::Lexer::TokenType::NotEqual);
    CHECK(result.data().at(18).getTokenType() == OpenCL::Lexer::TokenType::GreaterThanOrEqual);
    CHECK(result.data().at(19).getTokenType() == OpenCL::Lexer::TokenType::LessThanOrEqual);
    CHECK(result.data().at(20).getTokenType() == OpenCL::Lexer::TokenType::PlusAssign);
    CHECK(result.data().at(21).getTokenType() == OpenCL::Lexer::TokenType::MinusAssign);
    CHECK(result.data().at(22).getTokenType() == OpenCL::Lexer::TokenType::DivideAssign);
    CHECK(result.data().at(23).getTokenType() == OpenCL::Lexer::TokenType::MultiplyAssign);
    CHECK(result.data().at(24).getTokenType() == OpenCL::Lexer::TokenType::ModuloAssign);
    CHECK(result.data().at(25).getTokenType() == OpenCL::Lexer::TokenType::BitAndAssign);
    CHECK(result.data().at(26).getTokenType() == OpenCL::Lexer::TokenType::BitOrAssign);
    CHECK(result.data().at(27).getTokenType() == OpenCL::Lexer::TokenType::BitXorAssign);
    CHECK(result.data().at(28).getTokenType() == OpenCL::Lexer::TokenType::ShiftLeftAssign);
    CHECK(result.data().at(29).getTokenType() == OpenCL::Lexer::TokenType::ShiftRightAssign);
    CHECK(result.data().at(30).getTokenType() == OpenCL::Lexer::TokenType::BitWiseNegation);
    CHECK(result.data().at(31).getTokenType() == OpenCL::Lexer::TokenType::BitXor);
    CHECK(result.data().at(32).getTokenType() == OpenCL::Lexer::TokenType::Colon);
    CHECK(result.data().at(33).getTokenType() == OpenCL::Lexer::TokenType::BitXor);
    CHECK(result.data().at(34).getTokenType() == OpenCL::Lexer::TokenType::Equal);
    result = OpenCL::Lexer::tokenize("=>");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Assignment);
    CHECK(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::GreaterThan);
    result = OpenCL::Lexer::tokenize("&&=");
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::LogicAnd);
    CHECK(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Assignment);
}

TEST_CASE("Lexing character literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = OpenCL::Lexer::tokenize("'5'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == '5');
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
                auto result = OpenCL::Lexer::tokenize(input);
                REQUIRE(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
                REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == chara);
            }
        }
        LEXER_OUTPUTS_WITH("'\\o'", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\o")));
        LEXER_OUTPUTS_WITH("L'\\o'", Catch::Contains(INVALID_ESCAPE_SEQUENCE_N.args("\\o")));
    }
    SECTION("Octals")
    {
        LEXER_OUTPUTS_WITH("'\\9'", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
        auto result = OpenCL::Lexer::tokenize("'\\070'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == '\070');
    }
    SECTION("Hex")
    {
        LEXER_OUTPUTS_WITH("'\\xG'", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        LEXER_OUTPUTS_WITH("'\\x'", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        auto result = OpenCL::Lexer::tokenize("'\\x070'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == '\x070');
    }
    SECTION("Multibyte")
    {
        auto result = OpenCL::Lexer::tokenize(
            "L'5'", OpenCL::LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 2, 2, 4, 4, 80));
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == L'5');
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
        auto result = OpenCL::Lexer::tokenize("L'\\u3435'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == L'\u3435');
        result = OpenCL::Lexer::tokenize("L'\\U00003435'");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == L'\U00003435');
    }
    LEXER_OUTPUTS_WITH("'asdf'", Catch::Contains(DISCARDING_ALL_BUT_FIRST_CHARACTER));
    LEXER_OUTPUTS_WITH("'as\ndawd'", Catch::Contains(NEWLINE_IN_N_USE_BACKLASH_N.args(CHARACTER_LITERAL)));
}

TEST_CASE("Lexing unicode in literals", "[lexer]")
{
    SECTION("Characters")
    {
        SECTION("UTF-16")
        {
            auto result = OpenCL::Lexer::tokenize(
                "L'貓'", OpenCL::LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 2, 2, 4, 4, 80));
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
            REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == u'貓');
            LEXER_OUTPUTS_WITH("'貓'", Catch::Contains(CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE));
            LEXER_OUTPUTS_WITH("'🍌'", Catch::Contains(CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE));
        }
        SECTION("UTF-32")
        {
            auto result = OpenCL::Lexer::tokenize(
                "L'🍌'", OpenCL::LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 4, 2, 4, 4, 80));
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
            REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == U'🍌');
        }
        LEXER_OUTPUTS_WITH(
            (std::array<char, 11>{'L', '\'', -24, -78, -109, (char)0xE0, (char)0x80, (char)0x80, '\'', 0}).data(),
            Catch::Contains(INVALID_UTF8_SEQUENCE));
        LEXER_OUTPUTS_WITH((std::array<char, 18>{'L', '\'', -24, -78, -109, '\'', ' ', 'L', '\'', -24, -78, -109,
                                                 (char)0xE0, (char)0x80, (char)0x80, '\'', 0})
                               .data(),
                           Catch::Contains(INVALID_UTF8_SEQUENCE));
        LEXER_OUTPUTS_WITH((std::array<char, 8>{'L', '\'', (char)0xE0, (char)0x80, (char)0x80, '\'', 0}).data(),
                           Catch::Contains(INVALID_UTF8_SEQUENCE));
        LEXER_OUTPUTS_WITH(
            (std::array<char, 11>{'L', '"', -24, -78, -109, (char)0xE0, (char)0x80, (char)0x80, '"', 0}).data(),
            Catch::Contains(INVALID_UTF8_SEQUENCE));
        LEXER_OUTPUTS_WITH((std::array<char, 18>{'L', '"', -24, -78, -109, '"', ' ', 'L', '"', -24, -78, -109,
                                                 (char)0xE0, (char)0x80, (char)0x80, '"', 0})
                               .data(),
                           Catch::Contains(INVALID_UTF8_SEQUENCE));
        LEXER_OUTPUTS_WITH((std::array<char, 8>{'L', '"', (char)0xE0, (char)0x80, (char)0x80, '"', 0}).data(),
                           Catch::Contains(INVALID_UTF8_SEQUENCE));
    }
}

TEST_CASE("Lexing string literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = OpenCL::Lexer::tokenize("\"test\"");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "test");
        result = OpenCL::Lexer::tokenize("\"\"");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "");
        result = OpenCL::Lexer::tokenize("L\"\"");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()));
        CHECK(std::get<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()) == L"");
    }
    SECTION("Escapes")
    {
        auto result = OpenCL::Lexer::tokenize(R"("\\\b\t\'\f\v\?\n\a\r\"")");
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "\\\b\t\'\f\v\?\n\a\r\"");
    }
    SECTION("Octals")
    {
        LEXER_OUTPUTS_WITH("\"\\9\"", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
        auto result = OpenCL::Lexer::tokenize("\"\\070\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        REQUIRE(std::get<std::string>(result.data()[0].getValue()) == "\070");
    }
    SECTION("Hex")
    {
        LEXER_OUTPUTS_WITH("\"\\xG\"", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        LEXER_OUTPUTS_WITH("\"\\x\"", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        auto result = OpenCL::Lexer::tokenize("\"\\x070\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        REQUIRE(std::get<std::string>(result.data()[0].getValue()) == "\x070");
    }
    SECTION("Multibyte")
    {
        auto result = OpenCL::Lexer::tokenize(
            "L\"5\"", OpenCL::LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 2, 2, 4, 4, 80));
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()));
        REQUIRE(std::get<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()) == L"5");
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
        auto result = OpenCL::Lexer::tokenize("L\"\\u3435\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()));
        REQUIRE(std::get<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()) == L"\u3435");
        result = OpenCL::Lexer::tokenize("L\"\\U00003435\"");
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()));
        REQUIRE(std::get<OpenCL::Lexer::NonCharString>(result.data()[0].getValue()) == L"\U00003435");
    }
    LEXER_OUTPUTS_WITH("\"as\ndawd\"", Catch::Contains(NEWLINE_IN_N_USE_BACKLASH_N.args(STRING_LITERAL)));
}

TEST_CASE("Lexing keywords", "[lexer]")
{
    using namespace OpenCL::Lexer;
    auto result = OpenCL::Lexer::tokenize(
        "auto enum restrict unsigned break extern return void case float short volatile char for signed while const goto sizeof continue if static default inline struct do int switch double long typedef else register union _Bool");
    std::vector correct = {
        TokenType::AutoKeyword,     TokenType::EnumKeyword,   TokenType::RestrictKeyword, TokenType::UnsignedKeyword,
        TokenType::BreakKeyword,    TokenType::ExternKeyword, TokenType::ReturnKeyword,   TokenType::VoidKeyword,
        TokenType::CaseKeyword,     TokenType::FloatKeyword,  TokenType::ShortKeyword,    TokenType::VolatileKeyword,
        TokenType::CharKeyword,     TokenType::ForKeyword,    TokenType::SignedKeyword,   TokenType::WhileKeyword,
        TokenType::ConstKeyword,    TokenType::GotoKeyword,   TokenType::SizeofKeyword,   TokenType::ContinueKeyword,
        TokenType::IfKeyword,       TokenType::StaticKeyword, TokenType::DefaultKeyword,  TokenType::InlineKeyword,
        TokenType::StructKeyword,   TokenType::DoKeyword,     TokenType::IntKeyword,      TokenType::SwitchKeyword,
        TokenType::DoubleKeyword,   TokenType::LongKeyword,   TokenType::TypedefKeyword,  TokenType::ElseKeyword,
        TokenType::RegisterKeyword, TokenType::UnionKeyword,  TokenType::UnderlineBool};
    std::vector<TokenType> tokens;
    tokens.reserve(result.data().size());
    std::transform(result.data().begin(), result.data().end(), std::back_inserter(tokens),
                   [](const Token& token) { return token.getTokenType(); });
    CHECK_THAT(tokens, Catch::Equals(correct));
}

TEST_CASE("Lexing positions", "[lexer]")
{
    using namespace OpenCL::Lexer;
    SECTION("Random tokens")
    {
        auto result = OpenCL::Lexer::tokenize(
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
        auto result = OpenCL::Lexer::tokenize(R"(void updateHeight(TreeNode* h)
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
    auto result = OpenCL::Lexer::tokenize("<: :> <% %> %: %:%:");
    REQUIRE(result.data().size() == 6);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::OpenSquareBracket);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::CloseSquareBracket);
    CHECK(result.data().at(2).getTokenType() == OpenCL::Lexer::TokenType::OpenBrace);
    CHECK(result.data().at(3).getTokenType() == OpenCL::Lexer::TokenType::CloseBrace);
    CHECK(result.data().at(4).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(5).getTokenType() == OpenCL::Lexer::TokenType::DoublePound);
    CHECK(OpenCL::Lexer::reconstructTrimmed(result.data().begin(), result.data().end()) == "<: :> <% %> %: %:%:");

    result = OpenCL::Lexer::tokenize("%: %: #%: %:# # %: %: #");
    REQUIRE(result.data().size() == 8);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(2).getTokenType() == OpenCL::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(3).getTokenType() == OpenCL::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(4).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(5).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(6).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(7).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(OpenCL::Lexer::reconstructTrimmed(result.data().begin(), result.data().end()) == "%: %: #%: %:# # %: %: #");
}

TEST_CASE("Lexing input reconstruction", "[lexer]")
{
    auto source =
        "ad(){}<% %> \"test\" 52345 ; , - ~ ! + * / % && || & | ^ == != < <= > >= = += -= /= *= %= <<= >>= &= |= ^= >> << ++ -- : ? void char short int long float double signed unsigned typedef extern static auto register const sizeof return break continue do else for if while [] struct . -> switch case default union volatile enum goto ... restrict inline # ##";
    auto result = OpenCL::Lexer::tokenize(source);
    REQUIRE(OpenCL::Lexer::reconstructTrimmed(result.data().begin(), result.data().end()) == source);
}

TEST_CASE("Lexing include directives", "[lexer]")
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    SECTION("< >")
    {
        auto result = OpenCL::Lexer::tokenize("#include <agejf 4er325öüöü-3/3423354f\\wd3rf?ß>",
                                              OpenCL::LanguageOptions::native(), false, &ss);
        CHECK(ss.str().empty());
        REQUIRE(result.data().size() == 3);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Pound);
        REQUIRE(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "include");
        REQUIRE(result.data()[2].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "agejf 4er325öüöü-3/3423354f\\wd3rf?ß");
        CHECK(result.data()[2].getRepresentation() == "<agejf 4er325öüöü-3/3423354f\\wd3rf?ß>");
    }
    SECTION("\" \"")
    {
        auto result = OpenCL::Lexer::tokenize("#include \"agejf 4er325öüöü-3/3423354f\\wd3rf?ß\"",
                                              OpenCL::LanguageOptions::native(), false, &ss);
        CHECK(ss.str().empty());
        REQUIRE(result.data().size() == 3);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Pound);
        REQUIRE(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "include");
        REQUIRE(result.data()[2].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "agejf 4er325öüöü-3/3423354f\\wd3rf?ß");
        CHECK(result.data()[2].getRepresentation() == "\"agejf 4er325öüöü-3/3423354f\\wd3rf?ß\"");
    }
}

#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include <array>
#include <sstream>

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

#define LEXER_FAILS_WITH(string, match)                            \
    do                                                             \
    {                                                              \
        std::stringstream ss;                                      \
        OpenCL::Lexer::tokenize(string, OpenCL::Language::C, &ss); \
        auto s = ss.str();                                         \
        CHECK_THAT(s, match);                                      \
        if (!s.empty() && OpenCL::colourConsoleOutput)             \
        {                                                          \
            OpenCL::Lexer::tokenize(string, OpenCL::Language::C);  \
        }                                                          \
    } while (0)

TEST_CASE("Lexing Number Literals", "[lexer]")
{
    SECTION("Integers")
    {
        SECTION("Signed")
        {
            auto result = OpenCL::Lexer::tokenize("534534", OpenCL::Language::C);
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
            CHECK(std::get<std::int32_t>(result.data()[0].getValue()) == 534534);
        }
        SECTION("Unsigned")
        {
            auto result = OpenCL::Lexer::tokenize("534534u", OpenCL::Language::C);
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::uint32_t>(result.data()[0].getValue()));
            CHECK(std::get<std::uint32_t>(result.data()[0].getValue()) == 534534);

            LEXER_FAILS_WITH("5u5", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("u5")));
        }
        LEXER_FAILS_WITH("5x3", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("x3")));
    }
    SECTION("Floating point")
    {
        SECTION("Double")
        {
            auto result = OpenCL::Lexer::tokenize("534534.0", OpenCL::Language::C);
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 534534.0);
        }
        SECTION("Float")
        {
            auto result = OpenCL::Lexer::tokenize("534534.f", OpenCL::Language::C);
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<float>(result.data()[0].getValue()));
            CHECK(std::get<float>(result.data()[0].getValue()) == 534534.f);
        }
        SECTION("Dot only")
        {
            auto result = OpenCL::Lexer::tokenize(".5", OpenCL::Language::C);
            REQUIRE_FALSE(result.data().empty());
            CHECK(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0.5);
        }
        LEXER_FAILS_WITH("0.5.3", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0.5.3")));
        LEXER_FAILS_WITH("0.5.3F", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0.5.3F")));
        LEXER_FAILS_WITH("0.53fF", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0.53fF")));
        std::array<std::pair<const char*, double>, 6> results = {
            std::pair{"1e-19", 1e-19},   std::pair{"2e32", 2e32},   std::pair{"01e-19", 01e-19},
            std::pair{"01E-19", 01e-19}, std::pair{"02e32", 02e32}, std::pair{"02E32", 02e32}};
        for (auto [input, resulting] : results)
        {
            DYNAMIC_SECTION(input)
            {
                auto result = OpenCL::Lexer::tokenize(input, OpenCL::Language::C);
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
        auto result = OpenCL::Lexer::tokenize("070", OpenCL::Language::C);
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
            auto result = OpenCL::Lexer::tokenize("0x38", OpenCL::Language::C);
            REQUIRE_FALSE(result.data().empty());
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<int32_t>(result.data()[0].getValue()));
            CHECK(std::get<int32_t>(result.data()[0].getValue()) == 56);
        }
        SECTION("Floating point")
        {
            auto result = OpenCL::Lexer::tokenize("0x0.DE488631p8", OpenCL::Language::C);
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p8);

            result = OpenCL::Lexer::tokenize("0x0.DE488631P8", OpenCL::Language::C);
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p8);

            result = OpenCL::Lexer::tokenize("0x0.DE488631P+8", OpenCL::Language::C);
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p8);

            result = OpenCL::Lexer::tokenize("0x0.DE488631P-8", OpenCL::Language::C);
            REQUIRE(result.data().size() == 1);
            REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result.data()[0].getValue()));
            CHECK(std::get<double>(result.data()[0].getValue()) == 0x0.DE488631p-8);
        }

        LEXER_FAILS_WITH("0x0.5", Catch::Contains(BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT));
        LEXER_FAILS_WITH("0x0.5p", Catch::Contains(INVALID_FLOATING_POINT_LITERAL.args("0x0.5p")));
        LEXER_FAILS_WITH("0x5x5", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("x5")));
    }
    SECTION("Integer type selection")
    {
        auto test = [](const std::string& text, auto result) {
            using ResultingType = std::decay_t<decltype(result)>;
            DYNAMIC_SECTION(text)
            {
                std::ostringstream ss(text);
                auto tokens = OpenCL::Lexer::tokenize(ss.str(), OpenCL::Language::C);
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
        std::stringstream str;

        str << "0x" << std::hex << overUInt32;
        test(str.str(), overUInt32);
        str = std::stringstream{};
        auto overInt64 = static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()) + 6;
        str << "0x" << std::hex << overInt64;
        test(str.str(), std::uint64_t(overInt64));
        str = std::stringstream{};
        str << "0x" << std::hex << overInt64 << "ll";
        test(str.str(), std::uint64_t(overInt64));
        str = std::stringstream{};
        str << "0x" << std::hex << overInt64 << "ull";
        test(str.str(), std::uint64_t(overInt64));
        str = std::stringstream{};
    }
    SECTION("Long longs")
    {
        auto result = OpenCL::Lexer::tokenize("534534ll", OpenCL::Language::C);
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int64_t>(result.data()[0].getValue()));
        CHECK(std::get<std::int64_t>(result.data()[0].getValue()) == 534534);

        result = OpenCL::Lexer::tokenize("534534LL", OpenCL::Language::C);
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int64_t>(result.data()[0].getValue()));
        CHECK(std::get<std::int64_t>(result.data()[0].getValue()) == 534534);

        result = OpenCL::Lexer::tokenize("534534uLL", OpenCL::Language::C);
        REQUIRE_FALSE(result.data().empty());
        CHECK(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::uint64_t>(result.data()[0].getValue()));
        CHECK(std::get<std::uint64_t>(result.data()[0].getValue()) == 534534);

        LEXER_FAILS_WITH("534534lL", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("lL")));
        LEXER_FAILS_WITH("534534Ll", Catch::Contains(INVALID_INTEGER_LITERAL_SUFFIX.args("Ll")));
    }
}

TEST_CASE("Lexing weird characters", "[lexer]")
{
    OpenCL::Lexer::tokenize("\xAA test", OpenCL::Language::C, nullptr);
    auto result = OpenCL::Lexer::tokenize("L", OpenCL::Language::C);
    REQUIRE(result.data().size() == 1);
    CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
    REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
    CHECK(std::get<std::string>(result.data()[0].getValue()) == "L");
    result = OpenCL::Lexer::tokenize("V=V==L+E", OpenCL::Language::C);
    REQUIRE(result.data().size() == 7);
    CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
    CHECK(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Assignment);
    CHECK(result.data()[2].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
    CHECK(result.data()[3].getTokenType() == OpenCL::Lexer::TokenType::Equal);
    CHECK(result.data()[4].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
    CHECK(result.data()[5].getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.data()[6].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
    REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
    REQUIRE(std::holds_alternative<std::string>(result.data()[2].getValue()));
    REQUIRE(std::holds_alternative<std::string>(result.data()[4].getValue()));
    REQUIRE(std::holds_alternative<std::string>(result.data()[6].getValue()));
    REQUIRE(std::get<std::string>(result.data()[0].getValue()) == "V");
    REQUIRE(std::get<std::string>(result.data()[2].getValue()) == "V");
    REQUIRE(std::get<std::string>(result.data()[4].getValue()) == "L");
    REQUIRE(std::get<std::string>(result.data()[6].getValue()) == "E");
}

TEST_CASE("Lexing Punctuators", "[lexer]")
{
    auto result = OpenCL::Lexer::tokenize(
        ". ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>= ~ ^: ^ ==",
        OpenCL::Language::C);
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
    result = OpenCL::Lexer::tokenize("=>", OpenCL::Language::C);
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Assignment);
    CHECK(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::GreaterThan);
    result = OpenCL::Lexer::tokenize("&&=", OpenCL::Language::C);
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::LogicAnd);
    CHECK(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Assignment);
}

TEST_CASE("Lexing comments", "[lexer]")
{
    auto result = OpenCL::Lexer::tokenize("+/*wadjljzgawdaw8zdwagawizgdaw*/+", OpenCL::Language::C);
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    result = OpenCL::Lexer::tokenize("+//wadjljzgawdaw8zdwagawizgdaw*/\n+", OpenCL::Language::C);
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.data().at(1).getLine() == 2);
    result = OpenCL::Lexer::tokenize("+/*/++++++*/*", OpenCL::Language::C);
    REQUIRE(result.data().size() == 2);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::Asterisk);
    LEXER_FAILS_WITH("/*", Catch::Contains(OpenCL::ErrorMessages::Lexer::UNTERMINATED_COMMENT));
    SECTION("Multiline")
    {
        result = OpenCL::Lexer::tokenize("1/\\\n/dwadwadaw\n34", OpenCL::Language::C);
        REQUIRE(result.data().size() == 2);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        CHECK(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[1].getValue()));
        CHECK(std::get<std::int32_t>(result.data()[0].getValue()) == 1);
        CHECK(std::get<std::int32_t>(result.data()[1].getValue()) == 34);
        CHECK(result.data()[0].getLine() == 1);
        CHECK(result.data()[0].getColumn() == 0);
        CHECK(result.data()[1].getLine() == 3);
        CHECK(result.data()[1].getColumn() == 0);
    }
}

TEST_CASE("Lexing character literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = OpenCL::Lexer::tokenize("'5'", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == '5');
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
                auto result = OpenCL::Lexer::tokenize(input, OpenCL::Language::C);
                REQUIRE(result.data().size() == 1);
                REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
                REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == chara);
            }
        }
    }
    SECTION("Octals")
    {
        LEXER_FAILS_WITH("'\\9'", Catch::Contains(INVALID_OCTAL_CHARACTER.args("9")));
        LEXER_FAILS_WITH("'\\0700'", Catch::Contains(CHARACTER_MUSTNT_HAVE_HIGHER_VALUE_THAN_MAXIMUM_VALUE_OF_UCHAR));
        auto result = OpenCL::Lexer::tokenize("'\\070'", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == '\070');
    }
    SECTION("Hex")
    {
        LEXER_FAILS_WITH("'\\xG'", Catch::Contains(INVALID_HEXADECIMAL_CHARACTER.args("G")));
        LEXER_FAILS_WITH("'\\x'", Catch::Contains(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED));
        LEXER_FAILS_WITH("'\\x0700'", Catch::Contains(CHARACTER_MUSTNT_HAVE_HIGHER_VALUE_THAN_MAXIMUM_VALUE_OF_UCHAR));
        auto result = OpenCL::Lexer::tokenize("'\\x070'", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == '\x070');
    }
    SECTION("Multibyte")
    {
        auto result = OpenCL::Lexer::tokenize("L'5'", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        REQUIRE(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result.data()[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result.data()[0].getValue()) == L'5');
    }
    SECTION("Fails")
    {
        LEXER_FAILS_WITH("'\n'", Catch::Contains(NEWLINE_IN_N_USE_BACKLASH_N.args("character literal")));
        LEXER_FAILS_WITH("'aa'", Catch::Contains(INCORRECT_CHARACTER_LITERAL.args("aa")));
    }
}

TEST_CASE("Lexing string literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = OpenCL::Lexer::tokenize("\"test\"", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "test");
    }
    SECTION("Escapes")
    {
        auto result = OpenCL::Lexer::tokenize(R"("dwadawdwa\n\r\f\\ab\x07\"")", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::string>(result.data()[0].getValue()));
        CHECK(std::get<std::string>(result.data()[0].getValue()) == "dwadawdwa\n\r\f\\ab\x07\"");
    }
    SECTION("Multibyte")
    {
        auto result = OpenCL::Lexer::tokenize("L\"test\"", OpenCL::Language::C);
        REQUIRE(result.data().size() == 1);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        REQUIRE(std::holds_alternative<std::wstring>(result.data()[0].getValue()));
        CHECK(std::get<std::wstring>(result.data()[0].getValue()) == L"test");
    }
    SECTION("Fails")
    {
        LEXER_FAILS_WITH("\"\n\"", Catch::Contains(NEWLINE_IN_N_USE_BACKLASH_N.args("string literal")));
        LEXER_FAILS_WITH(R"("\S")", Catch::Contains(INCORRECT_CHARACTER_LITERAL.args("\\S")));
    }
}

TEST_CASE("Lexing keywords", "[lexer]")
{
    using namespace OpenCL::Lexer;
    auto result = OpenCL::Lexer::tokenize(
        "auto enum restrict unsigned break extern return void case float short volatile char for signed while const goto sizeof continue if static default inline struct do int switch double long typedef else register union _Bool",
        OpenCL::Language::C);
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
    std::transform(result.begin(), result.end(), std::back_inserter(tokens),
                   [](const Token& token) { return token.getTokenType(); });
    CHECK_THAT(tokens, Catch::Equals(correct));
}

TEST_CASE("Lexing positions", "[lexer]")
{
    using namespace OpenCL::Lexer;
    SECTION("Random tokens")
    {
        auto result = OpenCL::Lexer::tokenize(
            "(){}[];,:? '5'\"text\"test 5343 0.534 .343 0x3 /*\n   */text / * . ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>=",
            OpenCL::Language::C);
        REQUIRE(result.data().size() == 50);
        std::array correct = {std::tuple{TokenType::OpenParentheses, 1, 0, 1},
                              std::tuple{TokenType::CloseParentheses, 1, 1, 1},
                              std::tuple{TokenType::OpenBrace, 1, 2, 1},
                              std::tuple{TokenType::CloseBrace, 1, 3, 1},
                              std::tuple{TokenType::OpenSquareBracket, 1, 4, 1},
                              std::tuple{TokenType::CloseSquareBracket, 1, 5, 1},
                              std::tuple{TokenType::SemiColon, 1, 6, 1},
                              std::tuple{TokenType::Comma, 1, 7, 1},
                              std::tuple{TokenType::Colon, 1, 8, 1},
                              std::tuple{TokenType::QuestionMark, 1, 9, 1},
                              std::tuple{TokenType::Literal, 1, 11, 3},
                              std::tuple{TokenType::StringLiteral, 1, 14, 6},
                              std::tuple{TokenType::Identifier, 1, 20, 4},
                              std::tuple{TokenType::Literal, 1, 25, 4},
                              std::tuple{TokenType::Literal, 1, 30, 5},
                              std::tuple{TokenType::Literal, 1, 36, 4},
                              std::tuple{TokenType::Literal, 1, 41, 3},
                              std::tuple{TokenType::Identifier, 2, 5, 4},
                              std::tuple{TokenType::Division, 2, 10, 1},
                              std::tuple{TokenType::Asterisk, 2, 12, 1},
                              std::tuple{TokenType::Dot, 2, 14, 1},
                              std::tuple{TokenType::Ellipse, 2, 16, 3},
                              std::tuple{TokenType::GreaterThan, 2, 20, 1},
                              std::tuple{TokenType::Arrow, 2, 22, 2},
                              std::tuple{TokenType::ShiftRight, 2, 25, 2},
                              std::tuple{TokenType::LessThan, 2, 28, 1},
                              std::tuple{TokenType::ShiftLeft, 2, 30, 2},
                              std::tuple{TokenType::Ampersand, 2, 33, 1},
                              std::tuple{TokenType::LogicAnd, 2, 35, 2},
                              std::tuple{TokenType::BitOr, 2, 38, 1},
                              std::tuple{TokenType::LogicOr, 2, 40, 2},
                              std::tuple{TokenType::Plus, 2, 43, 1},
                              std::tuple{TokenType::Increment, 2, 45, 2},
                              std::tuple{TokenType::Minus, 2, 48, 1},
                              std::tuple{TokenType::Decrement, 2, 50, 2},
                              std::tuple{TokenType::Assignment, 2, 53, 1},
                              std::tuple{TokenType::Equal, 2, 55, 2},
                              std::tuple{TokenType::NotEqual, 2, 58, 2},
                              std::tuple{TokenType::GreaterThanOrEqual, 2, 61, 2},
                              std::tuple{TokenType::LessThanOrEqual, 2, 64, 2},
                              std::tuple{TokenType::PlusAssign, 2, 67, 2},
                              std::tuple{TokenType::MinusAssign, 2, 70, 2},
                              std::tuple{TokenType::DivideAssign, 2, 73, 2},
                              std::tuple{TokenType::MultiplyAssign, 2, 76, 2},
                              std::tuple{TokenType::ModuloAssign, 2, 79, 2},
                              std::tuple{TokenType::BitAndAssign, 2, 82, 2},
                              std::tuple{TokenType::BitOrAssign, 2, 85, 2},
                              std::tuple{TokenType::BitXorAssign, 2, 88, 2},
                              std::tuple{TokenType::ShiftLeftAssign, 2, 91, 3},
                              std::tuple{TokenType::ShiftRightAssign, 2, 95, 3}};
        for (std::size_t i = 0; i < correct.size(); i++)
        {
            DYNAMIC_SECTION("Token " << result.data()[i].emitBack())
            {
                CHECK(result.data()[i].getTokenType() == std::get<0>(correct[i]));
                CHECK(result.data()[i].getLine() == std::get<1>(correct[i]));
                CHECK(result.data()[i].getColumn() == std::get<2>(correct[i]));
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
} )",
                                              OpenCL::Language::C);
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
            DYNAMIC_SECTION("Token " << i << ": " << result.data()[i].emitBack())
            {
                CHECK(std::tuple(result.data()[i].getTokenType(), result.data()[i].getLine(),
                                 result.data()[i].getColumn(), result.data()[i].getLength())
                      == correct[i]);
            }
        }
    }
}

TEST_CASE("Lexing digraphs", "[lexer]")
{
    auto result = OpenCL::Lexer::tokenize("<: :> <% %> %: %:%:", OpenCL::Language::C);
    REQUIRE(result.data().size() == 6);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::OpenSquareBracket);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::CloseSquareBracket);
    CHECK(result.data().at(2).getTokenType() == OpenCL::Lexer::TokenType::OpenBrace);
    CHECK(result.data().at(3).getTokenType() == OpenCL::Lexer::TokenType::CloseBrace);
    CHECK(result.data().at(4).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(5).getTokenType() == OpenCL::Lexer::TokenType::DoublePound);
    CHECK(OpenCL::Lexer::reconstructTrimmed(result.begin(), result.end()) == "<: :> <% %> %: %:%:");

    result = OpenCL::Lexer::tokenize("%: %: #%: %:# # %: %: #", OpenCL::Language::C);
    REQUIRE(result.data().size() == 8);
    CHECK(result.data().at(0).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(1).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(2).getTokenType() == OpenCL::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(3).getTokenType() == OpenCL::Lexer::TokenType::DoublePound);
    CHECK(result.data().at(4).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(5).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(6).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(result.data().at(7).getTokenType() == OpenCL::Lexer::TokenType::Pound);
    CHECK(OpenCL::Lexer::reconstructTrimmed(result.begin(), result.end()) == "%: %: #%: %:# # %: %: #");
}

TEST_CASE("Lexing input reconstruction", "[lexer]")
{
    auto source =
        "ad(){}<% %> \"test\" 52345 ; , - ~ ! + * / % && || & | ^ == != < <= > >= = += -= /= *= %= <<= >>= &= |= ^= >> << ++ -- : ? void char short int long float double signed unsigned typedef extern static auto register const sizeof return break continue do else for if while [] struct . -> switch case default union volatile enum goto ... restrict inline # ##";
    auto result = OpenCL::Lexer::tokenize(source, OpenCL::Language::C);
    REQUIRE(OpenCL::Lexer::reconstructTrimmed(result.begin(), result.end()) == source);
}

TEST_CASE("Lexing multiline token", "[lexer]")
{
    std::string source = "\"test\"\n\"yes\"";
    auto result = OpenCL::Lexer::tokenize(source, OpenCL::Language::C);
    REQUIRE(result.data().size() == 2);
    CHECK(result.data()[0].getColumn() == 0);
    CHECK(result.data()[0].getLine() == 1);
    CHECK(result.data()[0].getLength() == 6);
    CHECK(result.data()[1].getColumn() == 0);
    CHECK(result.data()[1].getLine() == 2);
    CHECK(result.data()[1].getLength() == 5);
    REQUIRE(OpenCL::Lexer::reconstructTrimmed(result.begin(), result.end())
            == "\"test\"\n"
               "\"yes\"");
}

TEST_CASE("Lexing include directives", "[lexer]")
{
    std::stringstream ss;
    SECTION("< >")
    {
        auto result =
            OpenCL::Lexer::tokenize("#include <agejf 4er325öüöü-3/3423354f\\wd3rf?ß>", OpenCL::Language::C, &ss);
        CHECK(ss.str().empty());
        REQUIRE(result.data().size() == 3);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Pound);
        REQUIRE(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "include");
        REQUIRE(result.data()[2].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "agejf 4er325öüöü-3/3423354f\\wd3rf?ß");
        CHECK(result.data()[2].emitBack() == "<agejf 4er325öüöü-3/3423354f\\wd3rf?ß>");
    }
    SECTION("\" \"")
    {
        auto result =
            OpenCL::Lexer::tokenize("#include \"agejf 4er325öüöü-3/3423354f\\wd3rf?ß\"", OpenCL::Language::C, &ss);
        CHECK(ss.str().empty());
        REQUIRE(result.data().size() == 3);
        CHECK(result.data()[0].getTokenType() == OpenCL::Lexer::TokenType::Pound);
        REQUIRE(result.data()[1].getTokenType() == OpenCL::Lexer::TokenType::Identifier);
        CHECK(std::get<std::string>(result.data()[1].getValue()) == "include");
        REQUIRE(result.data()[2].getTokenType() == OpenCL::Lexer::TokenType::StringLiteral);
        CHECK(std::get<std::string>(result.data()[2].getValue()) == "agejf 4er325öüöü-3/3423354f\\wd3rf?ß");
        CHECK(result.data()[2].emitBack() == "\"agejf 4er325öüöü-3/3423354f\\wd3rf?ß\"");
    }
}

namespace OpenCL::Lexer
{
    bool operator==(const OpenCL::Lexer::Token& lhs, const OpenCL::Lexer::Token& rhs)
    {
        return std::tuple(lhs.getLine(), lhs.getColumn(), lhs.getLength(), lhs.getTokenType(), lhs.getValue(),
                          lhs.emitBack())
               == std::tuple(rhs.getLine(), rhs.getColumn(), rhs.getLength(), rhs.getTokenType(), rhs.getValue(),
                             rhs.emitBack());
    }

    bool operator==(const OpenCL::Lexer::ConcatReturn& lhs, const OpenCL::Lexer::ConcatReturn& rhs)
    {
        return std::tie(lhs.left, lhs.right) == std::tie(rhs.left, rhs.right);
    }
} // namespace OpenCL::Lexer

namespace Catch
{
    template <>
    struct StringMaker<OpenCL::Lexer::Token>
    {
        static std::string convert(const OpenCL::Lexer::Token& value)
        {
            return "Token(" + std::to_string(value.getLine()) + ", " + std::to_string(value.getColumn()) + ", "
                   + std::to_string(value.getLength()) + ", "
                   + StringMaker<OpenCL::Lexer::TokenType>::convert(value.getTokenType()) + ", "
                   + StringMaker<OpenCL::Lexer::Token::ValueType>::convert(value.getValue()) + ", " + value.emitBack()
                   + ")";
        }
    };

    template <>
    struct StringMaker<OpenCL::Lexer::ConcatReturn>
    {
        static std::string convert(const OpenCL::Lexer::ConcatReturn& value)
        {
            return '{' + StringMaker<OpenCL::Lexer::Token>::convert(value.left) + ", "
                   + StringMaker<std::optional<OpenCL::Lexer::Token>>::convert(value.right) + '}';
        }
    };
} // namespace Catch

TEST_CASE("Lexing concat", "[lexer]")
{
    using namespace OpenCL::Lexer;
    std::vector inputs = {std::pair{Token(1, 0, 3, TokenType::Identifier, "foo", "foo"),
                                    Token(1, 3, 3, TokenType::Identifier, "bar", "bar")},
                          std::pair{Token(1, 0, 3, TokenType::Identifier, "foo", "foo"),
                                    Token(1, 3, 5, TokenType::StringLiteral, "bar", "\"bar\"")},
                          std::pair{Token(1, 0, 3, TokenType::Identifier, "foo", "foo"),
                                    Token(1, 3, 6, TokenType::StringLiteral, L"bar", "L\"bar\"")},
                          std::pair{Token(1, 0, 3, TokenType::Identifier, "foo", "foo"),
                                    Token(1, 4, 6, TokenType::StringLiteral, L"bar", "L\"bar\"")},
                          std::pair{Token(1, 0, 2, TokenType::Identifier, "re", "re"),
                                    Token(1, 2, 4, TokenType::Identifier, "turn", "turn")},
                          std::pair{Token(1, 0, 2, TokenType::Identifier, "re", "re"),
                                    Token(2, 0, 4, TokenType::Identifier, "turn", "turn")},
                          std::pair{Token(1, 0, 1, TokenType::Identifier, "L", "L"),
                                    Token(1, 1, 5, TokenType::StringLiteral, "foo", "\"foo\"")},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 1, TokenType::Minus)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 1, TokenType::Assignment)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 1, TokenType::GreaterThan)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 2, TokenType::Decrement)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 2, TokenType::MinusAssign)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 2, TokenType::Arrow)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 2, TokenType::ShiftRight)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 3, TokenType::ShiftRightAssign)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 2, TokenType::GreaterThanOrEqual)},
                          std::pair{Token(1, 0, 1, TokenType::Minus), Token(1, 1, 2, TokenType::Equal)},
                          std::pair{Token(1, 0, 1, TokenType::LogicalNegation), Token(1, 1, 1, TokenType::Assignment)},
                          std::pair{Token(1, 0, 2, TokenType::LogicalNegation), Token(1, 2, 1, TokenType::Equal)},
                          std::pair{Token(1, 0, 1, TokenType::Plus), Token(1, 1, 1, TokenType::Plus)},
                          std::pair{Token(1, 0, 1, TokenType::Plus), Token(1, 1, 1, TokenType::Assignment)},
                          std::pair{Token(1, 0, 1, TokenType::Plus), Token(1, 1, 2, TokenType::Increment)},
                          std::pair{Token(1, 0, 1, TokenType::Plus), Token(1, 1, 2, TokenType::PlusAssign)},
                          std::pair{Token(1, 0, 1, TokenType::Plus), Token(1, 1, 2, TokenType::Equal)},
                          std::pair{Token(1, 0, 1, TokenType::Asterisk), Token(1, 1, 1, TokenType::Assignment)},
                          std::pair{Token(1, 0, 2, TokenType::Asterisk), Token(1, 2, 1, TokenType::Equal)},
                          std::pair{Token(1, 0, 1, TokenType::Division), Token(1, 1, 1, TokenType::Assignment)},
                          std::pair{Token(1, 0, 2, TokenType::Division), Token(1, 2, 1, TokenType::Equal)}};
    std::vector<std::optional<ConcatReturn>> correct = {
        ConcatReturn{Token(1, 0, 6, TokenType::Identifier, "foobar", "foobar")},
        ConcatReturn{Token(1, 0, 3, TokenType::Identifier, "foo", "foo"),
                     Token(1, 3, 5, TokenType::StringLiteral, "bar", "\"bar\"")},
        ConcatReturn{Token(1, 0, 4, TokenType::Identifier, "fooL", "fooL"),
                     Token(1, 4, 5, TokenType::StringLiteral, "bar", "\"bar\"")},
        ConcatReturn{Token(1, 0, 3, TokenType::Identifier, "foo", "foo"),
                     Token(1, 4, 6, TokenType::StringLiteral, L"bar", "L\"bar\"")},
        ConcatReturn{Token(1, 0, 6, TokenType::ReturnKeyword)},
        ConcatReturn{Token(1, 0, 2, TokenType::Identifier, "re", "re"),
                     Token(2, 0, 4, TokenType::Identifier, "turn", "turn")},
        ConcatReturn{Token(1, 0, 6, TokenType::StringLiteral, L"foo", "L\"foo\"")},
        ConcatReturn{Token(1, 0, 2, TokenType::Decrement)},
        ConcatReturn{Token(1, 0, 2, TokenType::MinusAssign)},
        ConcatReturn{Token(1, 0, 2, TokenType::Arrow)},
        ConcatReturn{Token(1, 0, 2, TokenType::Decrement), Token(1, 2, 1, TokenType::Minus)},
        ConcatReturn{Token(1, 0, 2, TokenType::Decrement), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::Decrement), Token(1, 2, 1, TokenType::GreaterThan)},
        ConcatReturn{Token(1, 0, 2, TokenType::Arrow), Token(1, 2, 1, TokenType::GreaterThan)},
        ConcatReturn{Token(1, 0, 2, TokenType::Arrow), Token(1, 2, 2, TokenType::GreaterThanOrEqual)},
        ConcatReturn{Token(1, 0, 2, TokenType::Arrow), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::MinusAssign), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::NotEqual)},
        ConcatReturn{Token(1, 0, 2, TokenType::NotEqual), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::Increment)},
        ConcatReturn{Token(1, 0, 2, TokenType::PlusAssign)},
        ConcatReturn{Token(1, 0, 2, TokenType::Increment), Token(1, 2, 1, TokenType::Plus)},
        ConcatReturn{Token(1, 0, 2, TokenType::Increment), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::PlusAssign), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::MultiplyAssign)},
        ConcatReturn{Token(1, 0, 2, TokenType::MultiplyAssign), Token(1, 2, 1, TokenType::Assignment)},
        ConcatReturn{Token(1, 0, 2, TokenType::DivideAssign)},
        ConcatReturn{Token(1, 0, 2, TokenType::DivideAssign), Token(1, 2, 1, TokenType::Assignment)}};
    REQUIRE(inputs.size() == correct.size());
    for (std::size_t i = 0; i < inputs.size(); i++)
    {
        auto& [lhs, rhs] = inputs[i];
        DYNAMIC_SECTION('[' + std::to_string(i) + "] Combining '" << lhs.emitBack() << "' and '" << rhs.emitBack()
                                                                  << "'")
        {
            auto result = concat(lhs, rhs, nullptr);
            INFO(Catch::StringMaker<Token>::convert(lhs));
            INFO(Catch::StringMaker<Token>::convert(rhs));
            CHECK(correct[i] == result);
        }
    }
}

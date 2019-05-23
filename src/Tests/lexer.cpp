#include <CompilerCore/C/Lexer.hpp>
#include "catch.hpp"
#include <sstream>

using namespace Catch::Matchers;

TEST_CASE("Number Literals", "[lexer]")
{
    SECTION("Integers")
    {
        SECTION("Signed")
        {
            auto result = OpenCL::Lexer::tokenize("534534");
            REQUIRE_FALSE(result.empty());
            CHECK(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::int32_t>(result[0].getValue()));
            CHECK(std::get<std::int32_t>(result[0].getValue()) == 534534);
        }
        SECTION("Unsigned")
        {
            auto result = OpenCL::Lexer::tokenize("534534u");
            REQUIRE_FALSE(result.empty());
            REQUIRE(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::uint32_t>(result[0].getValue()));
            CHECK(std::get<std::uint32_t>(result[0].getValue()) == 534534);

            CHECK_THROWS(OpenCL::Lexer::tokenize("5u5"));
        }
    }
    SECTION("Floating point")
    {
        SECTION("Double")
        {
            auto result = OpenCL::Lexer::tokenize("534534.0");
            REQUIRE_FALSE(result.empty());
            CHECK(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result[0].getValue()));
            CHECK(std::get<double>(result[0].getValue()) == 534534.0);
        }
        SECTION("Float")
        {
            auto result = OpenCL::Lexer::tokenize("534534.f");
            REQUIRE_FALSE(result.empty());
            CHECK(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<float>(result[0].getValue()));
            CHECK(std::get<float>(result[0].getValue()) == 534534.f);
        }
        SECTION("Dot only")
        {
            auto result = OpenCL::Lexer::tokenize(".5");
            REQUIRE_FALSE(result.empty());
            CHECK(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<double>(result[0].getValue()));
            CHECK(std::get<double>(result[0].getValue()) == 0.5);
        }
        CHECK_THROWS(OpenCL::Lexer::tokenize("0.5.3"));
        std::array results = {
            std::pair{"1e-19", 1e-19},
            std::pair{"2e32", 2e32},
            std::pair{"01e-19", 01e-19},
            std::pair{"02e32", 02e32},
        };
        for (auto[input, resulting] : results)
        {
            DYNAMIC_SECTION(input)
            {
                auto result = OpenCL::Lexer::tokenize(input);
                REQUIRE_FALSE(result.empty());
                CHECK(result.size() == 1);
                REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<double>(result[0].getValue()));
                CHECK(std::get<double>(result[0].getValue()) == resulting);
            }
        }
    }
    SECTION("Octal")
    {
        auto result = OpenCL::Lexer::tokenize("070");
        REQUIRE_FALSE(result.empty());
        CHECK(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<int32_t>(result[0].getValue()));
        CHECK(std::get<int32_t>(result[0].getValue()) == 56);
    }
    SECTION("Hex")
    {
        auto result = OpenCL::Lexer::tokenize("0x38");
        REQUIRE_FALSE(result.empty());
        CHECK(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<int32_t>(result[0].getValue()));
        CHECK(std::get<int32_t>(result[0].getValue()) == 56);

        CHECK_THROWS(OpenCL::Lexer::tokenize("0x5x5"));
    }
    SECTION("Integer type selection")
    {
        SECTION("int32 to uint32")
        {
            std::ostringstream ss;
            ss << static_cast<std::uint32_t>(std::numeric_limits<std::int32_t>::max()) + 1;
            auto result = OpenCL::Lexer::tokenize(ss.str());
            REQUIRE_FALSE(result.empty());
            CHECK(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::uint32_t>(result[0].getValue()));
            CHECK(std::get<std::uint32_t>(result[0].getValue()) ==
                static_cast<std::uint32_t>(std::numeric_limits<std::int32_t>::max()) + 1);
        }
        SECTION("uint32 to int64")
        {
            std::ostringstream ss;
            ss << static_cast<std::uint64_t>(std::numeric_limits<std::uint32_t>::max()) + 1;
            auto result = OpenCL::Lexer::tokenize(ss.str());
            REQUIRE_FALSE(result.empty());
            CHECK(result.size() == 1);
            REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
            REQUIRE(std::holds_alternative<std::int64_t>(result[0].getValue()));
            CHECK(std::get<std::int64_t>(result[0].getValue()) ==
                static_cast<std::uint64_t>(std::numeric_limits<std::uint32_t>::max()) + 1);
        }
    }
    SECTION("Long longs")
    {
        auto result = OpenCL::Lexer::tokenize("534534ll");
        REQUIRE_FALSE(result.empty());
        CHECK(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int64_t>(result[0].getValue()));
        CHECK(std::get<std::int64_t>(result[0].getValue()) == 534534);
        result = OpenCL::Lexer::tokenize("534534LL");
        REQUIRE_FALSE(result.empty());
        CHECK(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int64_t>(result[0].getValue()));
        CHECK(std::get<std::int64_t>(result[0].getValue()) == 534534);

        CHECK_THROWS(OpenCL::Lexer::tokenize("534534lL"));
        CHECK_THROWS(OpenCL::Lexer::tokenize("534534Ll"));
    }
}

TEST_CASE("AmbiguousOperators", "[lexer]")
{
    auto result =
        OpenCL::Lexer::tokenize(". ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>=");
    REQUIRE(result.size() == 30);
    CHECK(result.at(0).getTokenType() == OpenCL::Lexer::TokenType::Dot);
    CHECK(result.at(1).getTokenType() == OpenCL::Lexer::TokenType::Ellipse);
    CHECK(result.at(2).getTokenType() == OpenCL::Lexer::TokenType::GreaterThan);
    CHECK(result.at(3).getTokenType() == OpenCL::Lexer::TokenType::Arrow);
    CHECK(result.at(4).getTokenType() == OpenCL::Lexer::TokenType::ShiftRight);
    CHECK(result.at(5).getTokenType() == OpenCL::Lexer::TokenType::LessThan);
    CHECK(result.at(6).getTokenType() == OpenCL::Lexer::TokenType::ShiftLeft);
    CHECK(result.at(7).getTokenType() == OpenCL::Lexer::TokenType::Ampersand);
    CHECK(result.at(8).getTokenType() == OpenCL::Lexer::TokenType::LogicAnd);
    CHECK(result.at(9).getTokenType() == OpenCL::Lexer::TokenType::BitOr);
    CHECK(result.at(10).getTokenType() == OpenCL::Lexer::TokenType::LogicOr);
    CHECK(result.at(11).getTokenType() == OpenCL::Lexer::TokenType::Addition);
    CHECK(result.at(12).getTokenType() == OpenCL::Lexer::TokenType::Increment);
    CHECK(result.at(13).getTokenType() == OpenCL::Lexer::TokenType::Negation);
    CHECK(result.at(14).getTokenType() == OpenCL::Lexer::TokenType::Decrement);
    CHECK(result.at(15).getTokenType() == OpenCL::Lexer::TokenType::Assignment);
    CHECK(result.at(16).getTokenType() == OpenCL::Lexer::TokenType::Equal);
    CHECK(result.at(17).getTokenType() == OpenCL::Lexer::TokenType::NotEqual);
    CHECK(result.at(18).getTokenType() == OpenCL::Lexer::TokenType::GreaterThanOrEqual);
    CHECK(result.at(19).getTokenType() == OpenCL::Lexer::TokenType::LessThanOrEqual);
    CHECK(result.at(20).getTokenType() == OpenCL::Lexer::TokenType::PlusAssign);
    CHECK(result.at(21).getTokenType() == OpenCL::Lexer::TokenType::MinusAssign);
    CHECK(result.at(22).getTokenType() == OpenCL::Lexer::TokenType::DivideAssign);
    CHECK(result.at(23).getTokenType() == OpenCL::Lexer::TokenType::MultiplyAssign);
    CHECK(result.at(24).getTokenType() == OpenCL::Lexer::TokenType::ModuloAssign);
    CHECK(result.at(25).getTokenType() == OpenCL::Lexer::TokenType::BitAndAssign);
    CHECK(result.at(26).getTokenType() == OpenCL::Lexer::TokenType::BitOrAssign);
    CHECK(result.at(27).getTokenType() == OpenCL::Lexer::TokenType::BitXorAssign);
    CHECK(result.at(28).getTokenType() == OpenCL::Lexer::TokenType::ShiftLeftAssign);
    CHECK(result.at(29).getTokenType() == OpenCL::Lexer::TokenType::ShiftRightAssign);
}

TEST_CASE("Comments", "[lexer]")
{
    auto result = OpenCL::Lexer::tokenize("+/*wadjljzgawdaw8zdwagawizgdaw*/+");
    REQUIRE(result.size() == 2);
    CHECK(result.at(0).getTokenType() == OpenCL::Lexer::TokenType::Addition);
    CHECK(result.at(1).getTokenType() == OpenCL::Lexer::TokenType::Addition);
    result = OpenCL::Lexer::tokenize("+//wadjljzgawdaw8zdwagawizgdaw*/\n+");
    REQUIRE(result.size() == 2);
    CHECK(result.at(0).getTokenType() == OpenCL::Lexer::TokenType::Addition);
    CHECK(result.at(1).getTokenType() == OpenCL::Lexer::TokenType::Addition);
    CHECK(result.at(1).getLine() == 2);
}

TEST_CASE("Character literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = OpenCL::Lexer::tokenize("'5'");
        REQUIRE(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result[0].getValue()) == '5');
    }
    SECTION("Escape characters")
    {
        std::array results = {
            std::pair{"'\\''", '\''},
            std::pair{"'\\\"'", '"'},
            std::pair{"'\\?'", '\?'},
            std::pair{"'\\a'", '\a'},
            std::pair{"'\\b'", '\b'},
            std::pair{"'\\f'", '\f'},
            std::pair{"'\\n'", '\n'},
            std::pair{"'\\r'", '\r'},
            std::pair{"'\\t'", '\t'},
            std::pair{"'\\v'", '\v'},
        };
        for (auto[input, chara] : results)
        {
            DYNAMIC_SECTION(input)
            {
                auto result = OpenCL::Lexer::tokenize(input);
                REQUIRE(result.size() == 1);
                REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
                REQUIRE(std::holds_alternative<std::int32_t>(result[0].getValue()));
                REQUIRE(std::get<std::int32_t>(result[0].getValue()) == chara);
            }
        }
    }
    SECTION("Octals")
    {
        CHECK_THROWS(OpenCL::Lexer::tokenize("'\\9'"));
        CHECK_THROWS(OpenCL::Lexer::tokenize("'\\0700'"));
        auto result = OpenCL::Lexer::tokenize("'\\070'");
        REQUIRE(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result[0].getValue()) == '\070');
    }
    SECTION("Hex")
    {
        CHECK_THROWS(OpenCL::Lexer::tokenize("'\\xG'"));
        CHECK_THROWS(OpenCL::Lexer::tokenize("'\\x'"));
        auto result = OpenCL::Lexer::tokenize("'\\x070'");
        REQUIRE(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::int32_t>(result[0].getValue()));
        REQUIRE(std::get<std::int32_t>(result[0].getValue()) == '\x070');
    }
    CHECK_THROWS(OpenCL::Lexer::tokenize("'\'\n"));
}

TEST_CASE("String literals", "[lexer]")
{
    SECTION("Normal")
    {
        auto result = OpenCL::Lexer::tokenize("\"test\"");
        REQUIRE(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::string>(result[0].getValue()));
        CHECK(std::get<std::string>(result[0].getValue()) == "test");
    }
    SECTION("Escapes")
    {
        auto result = OpenCL::Lexer::tokenize(R"("dwadawdwa\n\r\f\\ab\x07\"")");
        REQUIRE(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::string>(result[0].getValue()));
        CHECK(std::get<std::string>(result[0].getValue()) == "dwadawdwa\n\r\f\\ab\x07\"");
    }
    SECTION("Concatenation")
    {
        auto result = OpenCL::Lexer::tokenize(R"("dwadawdwa""dwadwadawdwa")");
        REQUIRE(result.size() == 1);
        REQUIRE(result[0].getTokenType() == OpenCL::Lexer::TokenType::Literal);
        REQUIRE(std::holds_alternative<std::string>(result[0].getValue()));
        CHECK(std::get<std::string>(result[0].getValue()) == "dwadawdwa""dwadwadawdwa");
    }
    CHECK_THROWS(OpenCL::Lexer::tokenize("\"\n"));
}

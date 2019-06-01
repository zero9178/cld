#include <CompilerCore/C/Lexer.hpp>
#include "catch.hpp"
#include <sstream>

using namespace Catch::Matchers;

CATCH_REGISTER_ENUM(OpenCL::Lexer::TokenType,
                    OpenCL::Lexer::TokenType::Identifier,
                    OpenCL::Lexer::TokenType::OpenParenthese,
                    OpenCL::Lexer::TokenType::CloseParenthese,
                    OpenCL::Lexer::TokenType::OpenBrace,
                    OpenCL::Lexer::TokenType::CloseBrace,
                    OpenCL::Lexer::TokenType::Literal,
                    OpenCL::Lexer::TokenType::SemiColon,
                    OpenCL::Lexer::TokenType::Comma,
                    OpenCL::Lexer::TokenType::Minus,
                    OpenCL::Lexer::TokenType::BitWiseNegation,
                    OpenCL::Lexer::TokenType::LogicalNegation,
                    OpenCL::Lexer::TokenType::Plus,
                    OpenCL::Lexer::TokenType::Asterisk,
                    OpenCL::Lexer::TokenType::Division,
                    OpenCL::Lexer::TokenType::Modulo,
                    OpenCL::Lexer::TokenType::LogicAnd,
                    OpenCL::Lexer::TokenType::LogicOr,
                    OpenCL::Lexer::TokenType::Ampersand,
                    OpenCL::Lexer::TokenType::BitOr,
                    OpenCL::Lexer::TokenType::BitXor,
                    OpenCL::Lexer::TokenType::Equal,
                    OpenCL::Lexer::TokenType::NotEqual,
                    OpenCL::Lexer::TokenType::LessThan,
                    OpenCL::Lexer::TokenType::LessThanOrEqual,
                    OpenCL::Lexer::TokenType::GreaterThan,
                    OpenCL::Lexer::TokenType::GreaterThanOrEqual,
                    OpenCL::Lexer::TokenType::Assignment,
                    OpenCL::Lexer::TokenType::PlusAssign,
                    OpenCL::Lexer::TokenType::MinusAssign,
                    OpenCL::Lexer::TokenType::DivideAssign,
                    OpenCL::Lexer::TokenType::MultiplyAssign,
                    OpenCL::Lexer::TokenType::ModuloAssign,
                    OpenCL::Lexer::TokenType::ShiftLeftAssign,
                    OpenCL::Lexer::TokenType::ShiftRightAssign,
                    OpenCL::Lexer::TokenType::BitAndAssign,
                    OpenCL::Lexer::TokenType::BitOrAssign,
                    OpenCL::Lexer::TokenType::BitXorAssign,
                    OpenCL::Lexer::TokenType::ShiftRight,
                    OpenCL::Lexer::TokenType::ShiftLeft,
                    OpenCL::Lexer::TokenType::Increment,
                    OpenCL::Lexer::TokenType::Decrement,
                    OpenCL::Lexer::TokenType::Colon,
                    OpenCL::Lexer::TokenType::QuestionMark,
                    OpenCL::Lexer::TokenType::VoidKeyword,
                    OpenCL::Lexer::TokenType::CharKeyword,
                    OpenCL::Lexer::TokenType::ShortKeyword,
                    OpenCL::Lexer::TokenType::IntKeyword,
                    OpenCL::Lexer::TokenType::LongKeyword,
                    OpenCL::Lexer::TokenType::FloatKeyword,
                    OpenCL::Lexer::TokenType::DoubleKeyword,
                    OpenCL::Lexer::TokenType::SignedKeyword,
                    OpenCL::Lexer::TokenType::UnsignedKeyword,
                    OpenCL::Lexer::TokenType::TypedefKeyword,
                    OpenCL::Lexer::TokenType::ExternKeyword,
                    OpenCL::Lexer::TokenType::StaticKeyword,
                    OpenCL::Lexer::TokenType::AutoKeyword,
                    OpenCL::Lexer::TokenType::RegisterKeyword,
                    OpenCL::Lexer::TokenType::ConstKeyword,
                    OpenCL::Lexer::TokenType::RestrictKeyword,
                    OpenCL::Lexer::TokenType::SizeofKeyword,
                    OpenCL::Lexer::TokenType::VolatileKeyword,
                    OpenCL::Lexer::TokenType::InlineKeyword,
                    OpenCL::Lexer::TokenType::ReturnKeyword,
                    OpenCL::Lexer::TokenType::BreakKeyword,
                    OpenCL::Lexer::TokenType::ContinueKeyword,
                    OpenCL::Lexer::TokenType::DoKeyword,
                    OpenCL::Lexer::TokenType::ElseKeyword,
                    OpenCL::Lexer::TokenType::ForKeyword,
                    OpenCL::Lexer::TokenType::IfKeyword,
                    OpenCL::Lexer::TokenType::WhileKeyword,
                    OpenCL::Lexer::TokenType::OpenSquareBracket,
                    OpenCL::Lexer::TokenType::CloseSquareBracket,
                    OpenCL::Lexer::TokenType::StructKeyword,
                    OpenCL::Lexer::TokenType::Dot,
                    OpenCL::Lexer::TokenType::Arrow,
                    OpenCL::Lexer::TokenType::SwitchKeyword,
                    OpenCL::Lexer::TokenType::CaseKeyword,
                    OpenCL::Lexer::TokenType::DefaultKeyword,
                    OpenCL::Lexer::TokenType::UnionKeyword,
                    OpenCL::Lexer::TokenType::EnumKeyword,
                    OpenCL::Lexer::TokenType::GotoKeyword,
                    OpenCL::Lexer::TokenType::Ellipse)

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
    CHECK(result.at(11).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.at(12).getTokenType() == OpenCL::Lexer::TokenType::Increment);
    CHECK(result.at(13).getTokenType() == OpenCL::Lexer::TokenType::Minus);
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
    CHECK(result.at(0).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.at(1).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    result = OpenCL::Lexer::tokenize("+//wadjljzgawdaw8zdwagawizgdaw*/\n+");
    REQUIRE(result.size() == 2);
    CHECK(result.at(0).getTokenType() == OpenCL::Lexer::TokenType::Plus);
    CHECK(result.at(1).getTokenType() == OpenCL::Lexer::TokenType::Plus);
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

TEST_CASE("Positions", "[lexer]")
{
    using namespace OpenCL::Lexer;
    SECTION("Random tokens")
    {
        auto result = OpenCL::Lexer::tokenize(
            "(){}[];,:? '5'\"text\"test 5343 0.534 .343 0x3 /*\n   */text / * . ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>=");
        REQUIRE(result.size() == 50);
        std::array<std::tuple<OpenCL::Lexer::TokenType, std::uint64_t, std::uint64_t, std::uint64_t>, 50> correct
            = {{
                   {TokenType::OpenParenthese, 1, 0, 1},
                   {TokenType::CloseParenthese, 1, 1, 1},
                   {TokenType::OpenBrace, 1, 2, 1},
                   {TokenType::CloseBrace, 1, 3, 1},
                   {TokenType::OpenSquareBracket, 1, 4, 1},
                   {TokenType::CloseSquareBracket, 1, 5, 1},
                   {TokenType::SemiColon, 1, 6, 1},
                   {TokenType::Comma, 1, 7, 1},
                   {TokenType::Colon, 1, 8, 1},
                   {TokenType::QuestionMark, 1, 9, 1},
                   {TokenType::Literal, 1, 11, 3},
                   {TokenType::Literal, 1, 14, 6},
                   {TokenType::Identifier, 1, 20, 4},
                   {TokenType::Literal, 1, 26, 4},
                   {TokenType::Literal, 1, 31, 5},
                   {TokenType::Literal, 1, 37, 4},
                   {TokenType::Literal, 1, 42, 3},
                   {TokenType::Identifier, 2, 5, 4},
                   {TokenType::Division, 2, 10, 1},
                   {TokenType::Asterisk, 2, 12, 1},
                   {TokenType::Dot, 2, 14, 1},
                   {TokenType::Ellipse, 2, 16, 3},
                   {TokenType::GreaterThan, 2, 20, 1},
                   {TokenType::Arrow, 2, 22, 2},
                   {TokenType::ShiftRight, 2, 25, 2},
                   {TokenType::LessThan, 2, 28, 1},
                   {TokenType::ShiftLeft, 2, 30, 2},
                   {TokenType::Ampersand, 2, 33, 1},
                   {TokenType::LogicAnd, 2, 35, 2},
                   {TokenType::BitOr, 2, 38, 1},
                   {TokenType::LogicOr, 2, 40, 2},
                   {TokenType::Plus, 2, 43, 1},
                   {TokenType::Increment, 2, 45, 2},
                   {TokenType::Minus, 2, 48, 1},
                   {TokenType::Decrement, 2, 50, 2},
                   {TokenType::Assignment, 2, 53, 1},
                   {TokenType::Equal, 2, 55, 2},
                   {TokenType::NotEqual, 2, 58, 2},
                   {TokenType::GreaterThanOrEqual, 2, 61, 2},
                   {TokenType::LessThanOrEqual, 2, 64, 2},
                   {TokenType::PlusAssign, 2, 67, 2},
                   {TokenType::MinusAssign, 2, 70, 2},
                   {TokenType::DivideAssign, 2, 73, 2},
                   {TokenType::MultiplyAssign, 2, 76, 2},
                   {TokenType::ModuloAssign, 2, 79, 2},
                   {TokenType::BitAndAssign, 2, 82, 2},
                   {TokenType::BitOrAssign, 2, 85, 2},
                   {TokenType::BitXorAssign, 2, 88, 2},
                   {TokenType::ShiftLeftAssign, 2, 91, 3},
                   {TokenType::ShiftRightAssign, 2, 95, 3}
               }};
        for (std::size_t i = 0; i < correct.size(); i++)
        {
            DYNAMIC_SECTION("Token " << result[i].emitBack())
            {
                CHECK(result[i].getTokenType() == std::get<0>(correct[i]));
                CHECK(result[i].getLine() == std::get<1>(correct[i]));
                CHECK(result[i].getColumn() == std::get<2>(correct[i]));
                CHECK(result[i].getLength() == std::get<3>(correct[i]));
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
})");
        REQUIRE(result.size() == 60);
        std::array<std::tuple<OpenCL::Lexer::TokenType, std::uint64_t, std::uint64_t, std::uint64_t>, 60> correct
            = {{
                   {TokenType::VoidKeyword, 1, 0, 4},
                   {TokenType::Identifier, 1, 5, 12},
                   {TokenType::OpenParenthese, 1, 17, 1},
                   {TokenType::Identifier, 1, 18, 8},
                   {TokenType::Asterisk, 1, 26, 1},
                   {TokenType::Identifier, 1, 28, 1},
                   {TokenType::CloseParenthese, 1, 29, 1},
                   {TokenType::OpenBrace, 2, 0, 1},
                   {TokenType::IfKeyword, 3, 4, 2},
                   {TokenType::OpenParenthese, 3, 7, 1},
                   {TokenType::Identifier, 3, 8, 6},
                   {TokenType::OpenParenthese, 3, 14, 1},
                   {TokenType::Identifier, 3, 15, 1},
                   {TokenType::Arrow, 3, 16, 2},
                   {TokenType::Identifier, 3, 18, 5},
                   {TokenType::CloseParenthese, 3, 23, 1},
                   {TokenType::GreaterThan, 3, 25, 1},
                   {TokenType::Identifier, 3, 27, 6},
                   {TokenType::OpenParenthese, 3, 33, 1},
                   {TokenType::Identifier, 3, 34, 1},
                   {TokenType::Arrow, 3, 35, 2},
                   {TokenType::Identifier, 3, 37, 4},
                   {TokenType::CloseParenthese, 3, 41, 1},
                   {TokenType::CloseParenthese, 3, 42, 1},
                   {TokenType::OpenBrace, 4, 4, 1},
                   {TokenType::Identifier, 5, 8, 1},
                   {TokenType::Arrow, 5, 9, 2},
                   {TokenType::Identifier, 5, 11, 6},
                   {TokenType::Assignment, 5, 18, 1},
                   {TokenType::Identifier, 5, 20, 6},
                   {TokenType::OpenParenthese, 5, 26, 1},
                   {TokenType::Identifier, 5, 27, 1},
                   {TokenType::Arrow, 5, 28, 2},
                   {TokenType::Identifier, 5, 30, 5},
                   {TokenType::CloseParenthese, 5, 35, 1},
                   {TokenType::SemiColon, 5, 36, 1},
                   {TokenType::CloseBrace, 6, 4, 1},
                   {TokenType::ElseKeyword, 7, 4, 4},
                   {TokenType::OpenBrace, 8, 4, 1},
                   {TokenType::Identifier, 9, 8, 1},
                   {TokenType::Arrow, 9, 9, 2},
                   {TokenType::Identifier, 9, 11, 6},
                   {TokenType::Assignment, 9, 18, 1},
                   {TokenType::Identifier, 9, 20, 6},
                   {TokenType::OpenParenthese, 9, 26, 1},
                   {TokenType::Identifier, 9, 27, 1},
                   {TokenType::Arrow, 9, 28, 2},
                   {TokenType::Identifier, 9, 30, 4},
                   {TokenType::CloseParenthese, 9, 34, 1},
                   {TokenType::SemiColon, 9, 35, 1},
                   {TokenType::CloseBrace, 10, 4, 1},
                   {TokenType::Literal, 11, 4, 1},
                   {TokenType::Plus, 11, 6, 1},
                   {TokenType::Literal, 11, 8, 1},
                   {TokenType::Minus, 11, 10, 1},
                   {TokenType::Literal, 11, 12, 4},
                   {TokenType::Plus, 11, 17, 1},
                   {TokenType::Literal, 11, 19, 13},
                   {TokenType::Identifier, 11, 32, 2},
                   {TokenType::CloseBrace, 12, 0, 1}
               }};
        for (std::size_t i = 0; i < correct.size(); i++)
        {
            DYNAMIC_SECTION("Token " << i << ": " << result[i].emitBack())
            {
                CHECK(std::tuple(result[i].getTokenType(),
                                 result[i].getLine(),
                                 result[i].getColumn(),
                                 result[i].getLength()) == correct[i]);
            }
        }
    }
}

TEST_CASE("Input reconstruction", "[lexer]")
{
    auto source = R"(void updateHeight(TreeNode* h)
{
    if (height(h->right) > height(h->left))
    {
        h->height = height(h->right);
    }
    else
    {
        h->height = height(h->left);
    }
    5 + 7 - .53f + "dwawd""test"
})";
    auto result = OpenCL::Lexer::tokenize(source);
    REQUIRE(OpenCL::Lexer::reconstruct(result.begin(), result.end()) == source);
}

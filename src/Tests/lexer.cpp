#include <gtest/gtest.h>
#include <CompilerCore/Lexer.hpp>

TEST(Lexer,IntegerLiterals)
{
    auto result = OpenCL::Lexer::tokenize("534534");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<std::int32_t>(result[0].getValue()));
    EXPECT_EQ(std::get<std::int32_t>(result[0].getValue()),534534);

    result = OpenCL::Lexer::tokenize("534534u");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<std::uint32_t>(result[0].getValue()));
    EXPECT_EQ(std::get<std::uint32_t>(result[0].getValue()),534534);

    result = OpenCL::Lexer::tokenize("534534.0");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<double>(result[0].getValue()));
    EXPECT_EQ(std::get<double>(result[0].getValue()),534534.0);

    result = OpenCL::Lexer::tokenize("534534.f");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<float>(result[0].getValue()));
    EXPECT_EQ(std::get<float>(result[0].getValue()),534534.f);

    result = OpenCL::Lexer::tokenize("070");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<int32_t>(result[0].getValue()));
    EXPECT_EQ(std::get<int32_t>(result[0].getValue()),56);

    result = OpenCL::Lexer::tokenize("0x38");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<int32_t>(result[0].getValue()));
    EXPECT_EQ(std::get<int32_t>(result[0].getValue()),56);

    {
        std::ostringstream ss;
        ss<< static_cast<std::uint32_t>(std::numeric_limits<std::int32_t>::max()) + 1;
        result = OpenCL::Lexer::tokenize(ss.str());
        ASSERT_FALSE(result.empty());
        EXPECT_EQ(result.size(),1);
        ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
        ASSERT_TRUE(std::holds_alternative<std::uint32_t>(result[0].getValue()));
        EXPECT_EQ(std::get<std::uint32_t>(result[0].getValue()),static_cast<std::uint32_t>(std::numeric_limits<std::int32_t>::max()) + 1);
    }

    {
        std::ostringstream ss;
        ss<< static_cast<std::uint64_t>(std::numeric_limits<std::uint32_t>::max()) + 1;
        result = OpenCL::Lexer::tokenize(ss.str());
        ASSERT_FALSE(result.empty());
        EXPECT_EQ(result.size(),1);
        ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
        ASSERT_TRUE(std::holds_alternative<std::int64_t>(result[0].getValue()));
        EXPECT_EQ(std::get<std::int64_t>(result[0].getValue()),static_cast<std::uint64_t>(std::numeric_limits<std::uint32_t>::max()) + 1);
    }

    result = OpenCL::Lexer::tokenize("534534ll");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<std::int64_t>(result[0].getValue()));
    EXPECT_EQ(std::get<std::int64_t>(result[0].getValue()),534534);

    result = OpenCL::Lexer::tokenize("534534LL");
    ASSERT_FALSE(result.empty());
    EXPECT_EQ(result.size(),1);
    ASSERT_EQ(result[0].getTokenType(),OpenCL::Lexer::TokenType::Literal);
    ASSERT_TRUE(std::holds_alternative<std::int64_t>(result[0].getValue()));
    EXPECT_EQ(std::get<std::int64_t>(result[0].getValue()),534534);

    EXPECT_ANY_THROW(OpenCL::Lexer::tokenize("534534lL"));
    EXPECT_ANY_THROW(OpenCL::Lexer::tokenize("534534Ll"));
}

TEST(Lexer,AmbiguousOperators)
{
    auto result = OpenCL::Lexer::tokenize(". ... > -> >> < << & && | || + ++ - -- = == != >= <= += -= /= *= %= &= |= ^= <<= >>=");
    std::reverse(result.begin(),result.end());
    ASSERT_EQ(result.size(),30);
    EXPECT_EQ(result.at(0).getTokenType(),OpenCL::Lexer::TokenType::Dot);
    EXPECT_EQ(result.at(1).getTokenType(),OpenCL::Lexer::TokenType::Ellipse);
    EXPECT_EQ(result.at(2).getTokenType(),OpenCL::Lexer::TokenType::GreaterThan);
    EXPECT_EQ(result.at(3).getTokenType(),OpenCL::Lexer::TokenType::Arrow);
    EXPECT_EQ(result.at(4).getTokenType(),OpenCL::Lexer::TokenType::ShiftRight);
    EXPECT_EQ(result.at(5).getTokenType(),OpenCL::Lexer::TokenType::LessThan);
    EXPECT_EQ(result.at(6).getTokenType(),OpenCL::Lexer::TokenType::ShiftLeft);
    EXPECT_EQ(result.at(7).getTokenType(),OpenCL::Lexer::TokenType::Ampersand);
    EXPECT_EQ(result.at(8).getTokenType(),OpenCL::Lexer::TokenType::LogicAnd);
    EXPECT_EQ(result.at(9).getTokenType(),OpenCL::Lexer::TokenType::BitOr);
    EXPECT_EQ(result.at(10).getTokenType(),OpenCL::Lexer::TokenType::LogicOr);
    EXPECT_EQ(result.at(11).getTokenType(),OpenCL::Lexer::TokenType::Addition);
    EXPECT_EQ(result.at(12).getTokenType(),OpenCL::Lexer::TokenType::Increment);
    EXPECT_EQ(result.at(13).getTokenType(),OpenCL::Lexer::TokenType::Negation);
    EXPECT_EQ(result.at(14).getTokenType(),OpenCL::Lexer::TokenType::Decrement);
    EXPECT_EQ(result.at(15).getTokenType(),OpenCL::Lexer::TokenType::Assignment);
    EXPECT_EQ(result.at(16).getTokenType(),OpenCL::Lexer::TokenType::Equal);
    EXPECT_EQ(result.at(17).getTokenType(),OpenCL::Lexer::TokenType::NotEqual);
    EXPECT_EQ(result.at(18).getTokenType(),OpenCL::Lexer::TokenType::GreaterThanOrEqual);
    EXPECT_EQ(result.at(19).getTokenType(),OpenCL::Lexer::TokenType::LessThanOrEqual);
    EXPECT_EQ(result.at(20).getTokenType(),OpenCL::Lexer::TokenType::PlusAssign);
    EXPECT_EQ(result.at(21).getTokenType(),OpenCL::Lexer::TokenType::MinusAssign);
    EXPECT_EQ(result.at(22).getTokenType(),OpenCL::Lexer::TokenType::DivideAssign);
    EXPECT_EQ(result.at(23).getTokenType(),OpenCL::Lexer::TokenType::MultiplyAssign);
    EXPECT_EQ(result.at(24).getTokenType(),OpenCL::Lexer::TokenType::ModuloAssign);
    EXPECT_EQ(result.at(25).getTokenType(),OpenCL::Lexer::TokenType::BitAndAssign);
    EXPECT_EQ(result.at(26).getTokenType(),OpenCL::Lexer::TokenType::BitOrAssign);
    EXPECT_EQ(result.at(27).getTokenType(),OpenCL::Lexer::TokenType::BitXorAssign);
    EXPECT_EQ(result.at(28).getTokenType(),OpenCL::Lexer::TokenType::ShiftLeftAssign);
    EXPECT_EQ(result.at(29).getTokenType(),OpenCL::Lexer::TokenType::ShiftRightAssign);
}

TEST(Lexer,Comments)
{
    auto result = OpenCL::Lexer::tokenize("+/*wadjljzgawdaw8zdwagawizgdaw*/+");
    ASSERT_EQ(result.size(),2);
    EXPECT_EQ(result.at(0).getTokenType(),OpenCL::Lexer::TokenType::Addition);
    EXPECT_EQ(result.at(1).getTokenType(),OpenCL::Lexer::TokenType::Addition);
    result = OpenCL::Lexer::tokenize("+//wadjljzgawdaw8zdwagawizgdaw*/\n+");
    ASSERT_EQ(result.size(),2);
    EXPECT_EQ(result.at(0).getTokenType(),OpenCL::Lexer::TokenType::Addition);
    EXPECT_EQ(result.at(1).getTokenType(),OpenCL::Lexer::TokenType::Addition);
}

TEST(Lexer,Identifier)
{
    auto result = OpenCL::Lexer::tokenize(R"(TreeNode root = {"Markus", "Boeck"};)");
    std::reverse(result.begin(),result.end());
    ASSERT_EQ(result.size(),9);
    EXPECT_EQ(result.at(0).getTokenType(),OpenCL::Lexer::TokenType::Identifier);
    EXPECT_EQ(std::get<std::string>(result.at(0).getValue()),"TreeNode");
}
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

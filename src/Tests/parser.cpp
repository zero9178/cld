#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>
#include <fstream>
#include "catch.hpp"

TEST_CASE("Declaration parsing", "[parser]")
{
    auto program = R"(int main()
{
    int r;
    int *i = &r,*f = i;
}
)";
    auto tokens = OpenCL::Lexer::tokenize(program);
    auto expected = OpenCL::Parser::buildTree(tokens);
    INFO("The error is:\"" << (expected.hasError() ? expected.error().getText() : "") << '\\');
    REQUIRE(expected);
}

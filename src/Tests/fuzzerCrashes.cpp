#include "catch.hpp"

#include <CompilerCore/C/Parser.hpp>

namespace
{
    void parse(const std::string& source)
    {
        std::stringstream ss;
        auto tokens = OpenCL::Lexer::tokenize(source, &ss);
        if (!ss.str().empty() || tokens.empty())
        {
            return;
        }

        OpenCL::Parser::buildTree(tokens, &ss);
    }
} // namespace

TEST_CASE("Fuzzer discoveries", "[fuzzer]")
{
    parse("Y'\x0a\x0a");
    parse("(auto:");
    parse("\x8e.8..");
    parse(R"(('


I=')");
    parse("YYu{\x0a:");
    parse("(n{-((do(n");
    parse("+goto2n[0(sizeof(");
    parse("({(switch");
}

#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>
#include <sstream>
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
    REQUIRE(expected.second);
}

namespace
{
    template <class Matcher>
    void sourceProducesError(const std::string& source, const Matcher& matches)
    {
        std::ostringstream ss;
        std::vector<OpenCL::Lexer::Token> tokens;
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source));
        auto tree = OpenCL::Parser::buildTree(tokens, &ss);
        CHECK_FALSE(tree.second);
        CHECK_THAT(ss.str(), matches);
        INFO(ss.str());
    }

    class ProducesNErrors : public Catch::MatcherBase<std::string>
    {
        std::size_t m_allowedErrors;

    public:

        explicit ProducesNErrors(std::size_t n) : m_allowedErrors(n)
        {}

        bool match(const std::string& arg) const override
        {
            std::size_t occurrences = 0,pos = 0;
            while((pos = arg.find("error: ",pos)) != std::string::npos)
            {
                occurrences++;
                pos += 7;
            }
            return occurrences == m_allowedErrors;
        }

    protected:
        std::string describe() const override
        {
            return " has " + std::to_string(m_allowedErrors) + " errors ";
        }
    };

    class ProducesNoErrors : public ProducesNErrors
    {
    public:

        ProducesNoErrors() : ProducesNErrors(0)
        {}
    };
}

TEST_CASE("External definitions", "[parser]")
{
    sourceProducesError("i;", Catch::Contains(OpenCL::Parser::ErrorMessages::MISSING_DECLARATION_SPECIFIER) && ProducesNErrors(1));
    sourceProducesError("i{}", Catch::Contains(OpenCL::Parser::ErrorMessages::MISSING_DECLARATION_SPECIFIER) && ProducesNErrors(1));
}

ANON_TEST_CASE()
{
    std::ostringstream ss;
    std::vector<OpenCL::Lexer::Token> tokens;
    REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize("i;"));
    OpenCL::Parser::buildTree(tokens, &ss);
}

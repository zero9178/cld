#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>
#include <sstream>
#include <termcolor.hpp>
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
    void sourceProduces(const std::string& source, const Matcher& matches)
    {
        DYNAMIC_SECTION(source)
        {
            std::ostringstream ss;
            std::vector<OpenCL::Lexer::Token> tokens;
            REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source));
            auto tree = OpenCL::Parser::buildTree(tokens, &ss);
            auto string = ss.str();
            REQUIRE_FALSE(tree.second);
            CHECK_THAT(string, matches);
            OpenCL::Parser::buildTree(tokens);
        }
    }

    class ProducesNErrors : public Catch::MatcherBase<std::string>
    {
        std::size_t m_allowedErrors;

    public:

        explicit ProducesNErrors(std::size_t n) : m_allowedErrors(n)
        {}

        bool match(const std::string& arg) const override
        {
            std::size_t occurrences = 0, pos = 0;
            while ((pos = arg.find("error: ", pos)) != std::string::npos)
            {
                occurrences++;
                pos += 7;
            }
            return occurrences == m_allowedErrors;
        }

    protected:
        std::string describe() const override
        {
            return "has " + std::to_string(m_allowedErrors) + " error" + (m_allowedErrors == 1 ? " " : "s ");
        }
    };

    class ProducesNoErrors : public ProducesNErrors
    {
    public:

        ProducesNoErrors() : ProducesNErrors(0)
        {}
    };

    class ProducesNNotes : public Catch::MatcherBase<std::string>
    {
        std::size_t m_allowedNotes;

    public:

        explicit ProducesNNotes(std::size_t n) : m_allowedNotes(n)
        {}

        bool match(const std::string& arg) const override
        {
            std::size_t occurrences = 0, pos = 0;
            while ((pos = arg.find("note: ", pos)) != std::string::npos)
            {
                occurrences++;
                pos += 7;
            }
            return occurrences == m_allowedNotes;
        }

    protected:
        std::string describe() const override
        {
            return "has " + std::to_string(m_allowedNotes) + " note" + (m_allowedNotes == 1 ? " " : "s ");
        }
    };

    class ProducesNoNotes : public ProducesNNotes
    {
    public:

        ProducesNoNotes() : ProducesNNotes(0)
        {}
    };
}

TEST_CASE("Global Declarations", "[parser]")
{
    sourceProduces("i;",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::MISSING_DECLARATION_SPECIFIER)
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args(";"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i ft",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N.args(";", "ft"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("typedef int aa;"
                   "aa aa;", ProducesNoErrors());
}

TEST_CASE("Function definitions", "[parser]")
{

    sourceProduces("i{}",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::MISSING_DECLARATION_SPECIFIER)
                       && ProducesNErrors(1) && ProducesNoNotes());
}

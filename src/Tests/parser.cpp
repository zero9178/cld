#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>
#include <sstream>
#include "catch.hpp"

#define sourceProduces(source, matches)\
do\
{\
        std::ostringstream ss;\
        std::vector<OpenCL::Lexer::Token> tokens;\
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source));\
        auto tree = OpenCL::Parser::buildTree(tokens, &ss);\
        auto string = ss.str();\
        CHECK_THAT(string, matches);\
        OpenCL::Parser::buildTree(tokens);\
}while(0)


namespace
{
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
            return "has " + std::to_string(m_allowedErrors) + " error" + (m_allowedErrors == 1 ? "" : "s");
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
            return "has " + std::to_string(m_allowedNotes) + " note" + (m_allowedNotes == 1 ? "" : "s");
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
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_BEFORE_N
                                       .args("storage specifier or typename", "'i'"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("';'"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i ft",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N.args("';'", "'ft'"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i,",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N
                                       .args(OpenCL::Format::List(", ", " or ", "'('", "identifier")))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i,+=",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                       .args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'+='"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i, +=",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                       .args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'+='"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i,,f",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                       .args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "','"))
                       && Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("';'"))
                       && ProducesNErrors(2) && ProducesNoNotes());
}

TEST_CASE("Declaration Specifiers", "[paser]")
{
    SECTION("Typedef scoping")
    {
        sourceProduces("typedef int aa;"
                       "aa aa;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("typedef int aa;"
                       "aa aa;aa bb;", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_BEFORE_N
                                                           .args("storage specifier or typename", "'aa'"))
                           && Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                                  .args("';'", "'bb'"))
                           && Catch::Contains(OpenCL::Parser::Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'"))
                           && ProducesNErrors(2) && ProducesNNotes(1));

        sourceProduces("typedef int aa;"
                       "aa\n aa;const aa bb;", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                                                   .args("';'", "'bb'"))
                           && Catch::Contains(OpenCL::Parser::Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'"))
                           && ProducesNErrors(1) && ProducesNNotes(1));
    }
    sourceProduces(
        "typedef int aa; typedef extern static auto register const restrict volatile inline void char short "
        "int long float double signed unsigned f;", ProducesNoErrors() && ProducesNoNotes());
    SECTION("Structs and unions")
    {
        sourceProduces(
            "struct", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_AFTER_N
                                          .args(OpenCL::Format::List(", ", " or ", "identifier", "'{'"), "struct")) &&
            ProducesNErrors(1) && ProducesNoNotes()
        );
        sourceProduces(
            "struct;", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                           .args("identifier", "';'")) &&
            ProducesNErrors(1) && ProducesNoNotes()
        );
        sourceProduces(
            "struct i;", ProducesNoErrors() && ProducesNoNotes()
        );
        sourceProduces(
            "struct i", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("';'"))
            && ProducesNErrors(1) && ProducesNoNotes()
        );
        sourceProduces("union i;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{i;};",
                       Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_BEFORE_N.args("typename", "'i'"))
                           && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("struct i{};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{unsigned int:5;};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{unsigned int r:5;};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{unsigned int:5};",
                       Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                           && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("struct i{unsigned int:5",
                       Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("';'"))
                           && Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("'}'"))
                           && Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("';'"))
                           && ProducesNErrors(3) && ProducesNoNotes());
    }
    SECTION("Enums")
    {
        sourceProduces(
            "enum", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_AFTER_N
                                        .args("identifier", "enum")) &&
            ProducesNErrors(1) && ProducesNoNotes()
        );
        sourceProduces(
            "enum;", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                         .args("identifier", "';'")) &&
            ProducesNErrors(1) && ProducesNoNotes()
        );
        sourceProduces(
            "enum i;", ProducesNoErrors() && ProducesNoNotes()
        );
        sourceProduces(
            "enum i", Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N.args("';'"))
            && ProducesNErrors(1) && ProducesNoNotes()
        );
        sourceProduces("enum i{i};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum i{test};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {test,};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {test,ft};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {test,ft,};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum i{test ft};",
                       Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N.args("','", "'ft'"))
                           && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("enum {test,,ft};",
                       Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N.args("identifier", "','"))
                           && ProducesNErrors(1) && ProducesNoNotes());
    }
}

TEST_CASE("Function definitions", "[parser]")
{
    sourceProduces("i{}",
                   Catch::Contains(OpenCL::Parser::ErrorMessages::EXPECTED_N_BEFORE_N
                                       .args("storage specifier or typename", "'i'"))
                       && ProducesNErrors(1) && ProducesNoNotes());
}

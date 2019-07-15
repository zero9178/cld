#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include <sstream>

#include "TestConfig.hpp"

#define sourceProduces(source, matches)                            \
    do                                                             \
    {                                                              \
        std::ostringstream ss;                                     \
        std::vector<OpenCL::Lexer::Token> tokens;                  \
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source)); \
        auto tree = OpenCL::Parser::buildTree(tokens, &ss);        \
        auto string = ss.str();                                    \
        CHECK_THAT(string, matches);                               \
        if (OpenCL::colourConsoleOutput)                           \
        {                                                          \
            OpenCL::Parser::buildTree(tokens);                     \
            if (!string.empty())                                   \
            {                                                      \
                std::cerr << std::endl;                            \
            }                                                      \
        }                                                          \
    } while (0)

namespace
{
    class ProducesNErrors : public Catch::MatcherBase<std::string>
    {
        std::size_t m_allowedErrors;

    public:
        explicit ProducesNErrors(std::size_t n) : m_allowedErrors(n) {}

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
        ProducesNoErrors() : ProducesNErrors(0) {}

    protected:
        std::string describe() const override
        {
            return "has no errors";
        }
    };

    class ProducesNNotes : public Catch::MatcherBase<std::string>
    {
        std::size_t m_allowedNotes;

    public:
        explicit ProducesNNotes(std::size_t n) : m_allowedNotes(n) {}

        bool match(const std::string& arg) const override
        {
            std::size_t occurrences = 0, pos = 0;
            while ((pos = arg.find("note: ", pos)) != std::string::npos)
            {
                occurrences++;
                pos += 6;
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
        ProducesNoNotes() : ProducesNNotes(0) {}

    protected:
        std::string describe() const override
        {
            return "has no notes";
        }
    };
} // namespace

using namespace OpenCL::Notes;
using namespace OpenCL::ErrorMessages;
using namespace OpenCL::ErrorMessages::Parser;

TEST_CASE("Parse Global Declarations", "[parser]")
{
    sourceProduces("i;", Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "'i'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i", Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i ft", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'ft'")) && ProducesNErrors(1)
                                   && ProducesNoNotes());
    sourceProduces("int i,", Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier")))
                                 && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i,+=", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                                   OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'+='"))
                                   && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i, +=", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                                    OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'+='"))
                                    && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces(
        "int i,,f",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "','"))
            && Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(2) && ProducesNoNotes());
}

TEST_CASE("Parse Declaration Specifiers", "[paser]")
{
    SECTION("Typedef scoping")
    {
        sourceProduces("typedef int aa;void foo(){aa aa;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("typedef int aa;void foo(){aa aa;aa bb;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                           && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                           && ProducesNNotes(1));
        sourceProduces("typedef int aa;void foo(){aa aa;const aa bb;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                           && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                           && ProducesNNotes(1));
        sourceProduces("typedef int i;void foo(i){}", Catch::Contains(MISSING_PARAMETER_NAME)
                                                          && Catch::Contains(IDENTIFIER_IS_TYPDEF.args("'i'"))
                                                          && ProducesNErrors(1) && ProducesNNotes(1));
        sourceProduces("typedef int i;void foo(a,i){}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"))
                           && Catch::Contains(IDENTIFIER_IS_TYPDEF.args("'i'")) && ProducesNErrors(1)
                           && ProducesNNotes(1));
        sourceProduces("typedef int aa;int foo(int aa){aa i;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'"))
                           && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                           && ProducesNNotes(1));
        sourceProduces("typedef int aa;"
                       "enum aa; aa r;",
                       ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("typedef signed int t;t f(t (t));", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("typedef int aa;"
                       "enum {aa,};",
                       Catch::Contains(REDEFINITION_OF_SYMBOL_N.args("'aa'"))
                           && Catch::Contains(PREVIOUSLY_DECLARED_HERE) && ProducesNErrors(1) && ProducesNNotes(1));
    }
    sourceProduces("typedef int aa; typedef extern static auto register const restrict volatile inline void char short "
                   "int long float double signed unsigned f;",
                   ProducesNoErrors() && ProducesNoNotes());
    SECTION("Structs and unions")
    {
        sourceProduces("struct", Catch::Contains(EXPECTED_N_AFTER_N.args(
                                     OpenCL::Format::List(", ", " or ", "identifier", "'{'"), "struct"))
                                     && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("struct;", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'"))
                                      && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("struct i;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i", Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("union i;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{i;};", Catch::Contains(EXPECTED_N_BEFORE_N.args("typename", "'i'"))
                                            && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("struct i{};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{unsigned int:5;};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{unsigned int r:5;};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("struct i{unsigned int:5};", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                        && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("struct i{unsigned int:5",
                       Catch::Contains(EXPECTED_N.args("';'")) && Catch::Contains(EXPECTED_N.args("'}'"))
                           && Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(3) && ProducesNoNotes());
    }
    SECTION("Enums")
    {
        sourceProduces("enum", Catch::Contains(EXPECTED_N_AFTER_N.args("identifier", "enum")) && ProducesNErrors(1)
                                   && ProducesNoNotes());
        sourceProduces("enum;", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNErrors(1)
                                    && ProducesNoNotes());
        sourceProduces("enum i;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum i", Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("enum i{i};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum i{test};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {test,};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {test,ft};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum {test,ft,};", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("enum i{test ft};", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'ft'"))
                                               && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("enum {test,,ft};", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "','"))
                                               && ProducesNErrors(1) && ProducesNoNotes());
    }
}

TEST_CASE("Parse Function definitions", "[parser]")
{
    sourceProduces("i{}", Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "'i'"))
                              && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int i() int f{}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'{'")) && ProducesNErrors(1)
                                          && ProducesNoNotes());
    sourceProduces("int foo(int,int[5]){}",
                   Catch::Contains(MISSING_PARAMETER_NAME) && ProducesNErrors(2) && ProducesNoNotes());
}

TEST_CASE("Parse Declarator", "[parser]")
{
    sourceProduces("int * const * volatile *i;", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int * const * volatile *(i;", Catch::Contains(EXPECTED_N.args("')'"))
                                                      && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                      && ProducesNErrors(1) && ProducesNNotes(1));
    sourceProduces("int * const * volatile *i(int f;", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                           && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                           && ProducesNErrors(1) && ProducesNNotes(1));
    sourceProduces("int foo(int,int[5]);", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo(int i,int f[5],);",
                   Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "')'"))
                       && ProducesNErrors(1) && ProducesNoNotes());
    sourceProduces("int foo(i,f e);", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNErrors(1)
                                          && ProducesNoNotes());
    sourceProduces("int foo(i,f e,r)", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNErrors(2)
                                           && Catch::Contains(EXPECTED_N.args("';'")) && ProducesNoNotes());
    sourceProduces("int foo(i,f", Catch::Contains(EXPECTED_N.args("')'"))
                                      && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                                      && ProducesNNotes(1));
    sourceProduces("int foo[", Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                                   && ProducesNErrors(1) && ProducesNNotes(1));
    sourceProduces("int foo[static 5];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[static const 5];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[const static 5];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[const *];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[*];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[5];", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo[*[];", Catch::Contains(EXPECTED_N.args("']'"))
                                       && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(1)
                                       && ProducesNNotes(1));
    sourceProduces("int foo(int();", Catch::Contains(EXPECTED_N.args("')'"))
                                         && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                                         && ProducesNNotes(1));
    sourceProduces("int foo(int(int));", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo(int([5]));", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo(int());", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo(int[*]);", ProducesNoErrors() && ProducesNoNotes());
    sourceProduces("int foo(int (i)(int));", ProducesNoErrors() && ProducesNoNotes());
}

TEST_CASE("Parse Statements", "[parser]")
{
    SECTION("Return")
    {
        sourceProduces("void foo(){return;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){return 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){return}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                 && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){return 5}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                   && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){return 5",
                       Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("if")
    {
        sourceProduces("void foo(){if 5);}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'"))
                                                 && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){if(5;int i}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                     && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                     && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                     && ProducesNErrors(2) && ProducesNNotes(1));
        sourceProduces("void foo(){if(5);else;}", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("switch")
    {
        sourceProduces("void foo(){switch(5);}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){switch 5);}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'"))
                                                     && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){switch(5;int i}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                         && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                         && ProducesNErrors(2) && ProducesNNotes(1));
    }
    SECTION("for")
    {
        sourceProduces("void foo(){for int i = 0; i < 5; i++);}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'int'")) && ProducesNErrors(1)
                           && ProducesNoNotes());
        sourceProduces("void foo(){int i;for i = 0; i < 5; i++);}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'i'")) && ProducesNErrors(1)
                           && ProducesNoNotes());
        sourceProduces("void foo(){for(int i = 0 i < 5; i++);}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                           && ProducesNoNotes());
        sourceProduces("void foo(){for(int i = 0; i < 5 i++);}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                           && ProducesNoNotes());
        sourceProduces("void foo(){for(int i = 0 i < 5 i++);}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(2)
                           && ProducesNoNotes());
        sourceProduces("void foo(){for(int i = 0;i < 5;i++;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                           && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1) && ProducesNNotes(1));
    }
    SECTION("while, do ... while")
    {
        sourceProduces("void foo(){while(5);}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){do;while(5);}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){while 5);}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'"))
                                                    && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){while(5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                   && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                                                   && ProducesNNotes(1));
        sourceProduces("void foo(){do;while 5);}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'"))
                                                       && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){do;while(5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                      && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                      && ProducesNErrors(1) && ProducesNNotes(1));
        sourceProduces("void foo(){do;(5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                 && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                 && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'while'", "'('"))
                                                 && Catch::Contains(TO_MATCH_N_HERE.args("'do'")) && ProducesNErrors(2)
                                                 && ProducesNNotes(2));
    }
    SECTION("break and continue")
    {
        sourceProduces("void foo(){break;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){continue;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){break}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){continue}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'"))
                                                   && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Label and goto")
    {
        sourceProduces("void foo(){test:;goto test;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){typedef int test;test:;goto test;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){test:;goto 0x3;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'0x3'")) && ProducesNErrors(1)
                           && ProducesNoNotes());
    }
}

TEST_CASE("Parse Expressions", "[parser]")
{
    SECTION("Primary expressions")
    {
        sourceProduces("int i = 0;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("int i = wdawd;", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("int i = ((((5))));", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){i;\"string\"\"can also be concatenated\";34234;}",
                       ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){int i =;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                           OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                           && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("int i =", Catch::Contains(EXPECTED_N.args(
                                      OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                      && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Postfix expressions")
    {
        sourceProduces("void foo(){i(53,42,32);}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){i(53 42,32,53 43;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'42'"))
                           && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'43'"))
                           && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                           && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(3) && ProducesNNotes(1));
        sourceProduces(
            "void foo(){i(53,42,32,[5];}",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(2) && ProducesNNotes(1));

        sourceProduces("void foo(){(int){5};}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){(int){5 8};}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'}'", "'8'"))
                                                      && Catch::Contains(TO_MATCH_N_HERE.args("'{'"))
                                                      && ProducesNErrors(1) && ProducesNNotes(1));

        sourceProduces("void foo(){i[5;}", Catch::Contains(EXPECTED_N.args("']'"))
                                               && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(1)
                                               && ProducesNNotes(1));
        sourceProduces("void foo(){i[];}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                           OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                           && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){i[5];}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){i++;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){i--;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){i.m;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){i.;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'"))
                                              && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){i.[];}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                           && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                               OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                           && ProducesNErrors(2) && ProducesNoNotes());

        sourceProduces("void foo(){i->m;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){i->;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'"))
                                               && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){i->[];}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                           && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                               OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                           && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Unary expressions")
    {
        sourceProduces("void foo(){sizeof(int);}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){sizeof(5);}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){sizeof(int;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                      && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                      && ProducesNErrors(1) && ProducesNNotes(1));
        sourceProduces("void foo(){sizeof(5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                                                    && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                                                    && ProducesNErrors(1) && ProducesNNotes(1));

        sourceProduces("void foo(){sizeof sizeof(int);}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){++i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){--i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){&i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){*i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){+i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){-i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){!i;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){~i;}", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Cast expression")
    {
        sourceProduces("void foo(){(int)5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){(int)5 8;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'8'"))
                                                    && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){(int)", Catch::Contains(EXPECTED_N.args(
                                               OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                               && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Term")
    {
        sourceProduces("void foo(){5 * 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 / 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 % 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){5 * / 5;}", Catch::Contains(EXPECTED_N.args(
                                                   OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                   && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ % / 5;}", Catch::Contains(EXPECTED_N.args(
                                                  OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                  && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Additive")
    {
        sourceProduces("void foo(){5 + 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 - 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){5 + - -5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){ + + - - 5;}", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Shift")
    {
        sourceProduces("void foo(){5 << 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 >> 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces(
            "void foo(){5 << >> 5;}",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ << >> 5;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                           OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'<<'"))
                           && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Relational")
    {
        sourceProduces("void foo(){5 < 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 > 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 <= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 >= 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){5 < > 5;}", Catch::Contains(EXPECTED_N.args(
                                                   OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                   && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ <= >= 5;}", Catch::Contains(EXPECTED_N.args(
                                                    OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                    && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Equality")
    {
        sourceProduces("void foo(){5 == 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 != 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces(
            "void foo(){5 == != 5;}",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ == != 5;}", Catch::Contains(EXPECTED_N.args(
                                                    OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                    && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Bitand")
    {
        sourceProduces("void foo(){5 & 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 & &5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){5 & & 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){ & & 5;}", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("BitXor")
    {
        sourceProduces("void foo(){5 ^ 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){5 ^ ^ 5;}", Catch::Contains(EXPECTED_N.args(
                                                   OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                   && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ ^ ^ 5;}", Catch::Contains(EXPECTED_N.args(
                                                  OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                  && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("BitOr")
    {
        sourceProduces("void foo(){5 | 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces("void foo(){5 | | 5;}", Catch::Contains(EXPECTED_N.args(
                                                   OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                   && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ | | 5;}", Catch::Contains(EXPECTED_N.args(
                                                  OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                  && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("LogicalAnd")
    {
        sourceProduces("void foo(){5 && 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces(
            "void foo(){5 && && 5;}",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ && && 5;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                           OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'&&'"))
                           && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("LogicalOr")
    {
        sourceProduces("void foo(){5 || 5;}", ProducesNoErrors() && ProducesNoNotes());

        sourceProduces(
            "void foo(){5 || || 5;}",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        sourceProduces("void foo(){ || || 5;}",
                       Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                           OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'||'"))
                           && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Conditional")
    {
        sourceProduces("void foo(){5 ? 5 : 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 ? 5  5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                                                    && Catch::Contains(TO_MATCH_N_HERE.args("'?'"))
                                                    && ProducesNErrors(1) && ProducesNNotes(1));
        sourceProduces("void foo(){5 ? 5 ? 5 : 5 5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                                                           && Catch::Contains(TO_MATCH_N_HERE.args("'?'"))
                                                           && ProducesNErrors(1) && ProducesNNotes(1));
        sourceProduces("void foo(){5 ? 5 : 5 ? 5;}", Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "';'"))
                                                         && Catch::Contains(TO_MATCH_N_HERE.args("'?'"))
                                                         && ProducesNErrors(1) && ProducesNNotes(1));
    }
    SECTION("Assignment")
    {
        sourceProduces("void foo(){5 = 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 += 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 -= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 /= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 *= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 %= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 <<= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 >>= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 &= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 |= 5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 ^= 5;}", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Expressions")
    {
        sourceProduces("void foo(){5,5;}", ProducesNoErrors() && ProducesNoNotes());
        sourceProduces("void foo(){5 +,5 +;}", Catch::Contains(EXPECTED_N.args(
                                                   OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                                                   && ProducesNErrors(2) && ProducesNoNotes());
    }
}

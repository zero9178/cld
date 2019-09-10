#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include <sstream>

#include "TestConfig.hpp"

#define treeProduces(source, matches)                              \
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

#define functionProduces(parser, source, matches)                                                       \
    do                                                                                                  \
    {                                                                                                   \
        std::ostringstream ss;                                                                          \
        std::vector<OpenCL::Lexer::Token> tokens;                                                       \
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source));                                      \
        OpenCL::Parser::Context context(tokens.cbegin(), tokens.cend(), &ss);                           \
        auto begin = tokens.cbegin();                                         \
        parser(begin, tokens.cend(), context);                                \
        auto string = ss.str();                                                                         \
        CHECK((!string.empty() || begin == tokens.cend()));                                             \
        CHECK_THAT(string, matches);                                                                    \
        if (OpenCL::colourConsoleOutput)                                                                \
        {                                                                                               \
            auto begin2 = tokens.cbegin();                                                              \
            OpenCL::Parser::Context context2(tokens.cbegin(), tokens.cend()); \
            parser(begin2, tokens.cend(), context2);                          \
            if (!string.empty())                                                                        \
            {                                                                                           \
                std::cerr << std::endl;                                                                 \
            }                                                                                           \
        }                                                                                               \
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
using namespace OpenCL::Parser;

TEST_CASE("Parse specifier qualifier list", "[parser]")
{
    treeProduces("void foo(){typedef int i;sizeof(const i f);}",
                 Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "'f'"))
                     && Catch::Contains(TO_MATCH_N_HERE.args("'('")));
}

TEST_CASE("Parse external declaration", "[parser]")
{
    functionProduces(parseExternalDeclaration, "int;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i",
                     Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "i{}",
                     Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "'i'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f;{}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i(void) {}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int () int f;{}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "')'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f{}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'{'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo(int,int[5]){}",
                     Catch::Contains(MISSING_PARAMETER_NAME) && ProducesNErrors(2) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo = 5;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo = 5",
                     Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo =;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int[;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(2) && ProducesNNotes(1));
    functionProduces(parseExternalDeclaration, "int[ =;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "'='"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(3) && ProducesNNotes(1));
    functionProduces(parseExternalDeclaration, "int foo,bar = 5;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo,bar;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo,[;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(2) && ProducesNNotes(1));
    functionProduces(parseExternalDeclaration, "int foo,bar =;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo,[ =;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "'='"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(3) && ProducesNNotes(1));
    functionProduces(parseExternalDeclaration, "typedef int [;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(2) && ProducesNNotes(1));
    treeProduces("typedef int i;void foo(i){}", Catch::Contains(MISSING_PARAMETER_NAME)
                                                    && Catch::Contains(IDENTIFIER_IS_TYPEDEF.args("'i'"))
                                                    && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseExternalDeclaration, "typedef int [;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(2) && ProducesNNotes(1));
    treeProduces("\n"
                 "void barFunc(int,char);\n"
                 "\n"
                 "void (*foo(foo,i,bar))(int,char) short foo,i,bar;\n"
                 "{\n"
                 "    return barFunc;\n"
                 "}",
                 ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int i;void foo(int i,i i)"
                 "{"
                 "}",
                 Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("typename", "'i'"))
                     && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'i'")) && ProducesNErrors(1)
                     && ProducesNNotes(1));
    treeProduces("typedef int i,bar;void foo(int i,bar i)"
                 "{"
                 "}",
                 ProducesNoErrors() && ProducesNoNotes());
}

TEST_CASE("Parser typedef scoping and resolution", "[parser]")
{
    treeProduces("typedef int aa;void foo(){aa aa;}", ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int aa;void foo(){aa aa;const aa;}", ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int aa;"
                 "enum aa; aa r;",
                 ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef signed int t;t f(t (t));", ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int aa;"
                 "enum {aa,};",
                 Catch::Contains(REDEFINITION_OF_SYMBOL_N.args("'aa'")) && Catch::Contains(PREVIOUSLY_DECLARED_HERE)
                     && ProducesNErrors(1) && ProducesNNotes(1));
}

TEST_CASE("Parse Declaration Specifiers", "[parser]")
{
    treeProduces("typedef int aa; typedef extern static auto register const restrict volatile inline void char short "
                 "int long float double signed unsigned f;",
                 ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarationSpecifier, "struct i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarationSpecifier, "union i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarationSpecifier, "enum i", ProducesNoErrors() && ProducesNoNotes());
}

TEST_CASE("Parse Specifier Qualifiers", "[parser]")
{
    functionProduces(
        parseSpecifierQualifierList,
        "const void long float double signed restrict volatile char short int long float double signed unsigned",
        ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseSpecifierQualifier, "struct i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseSpecifierQualifier, "union i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseSpecifierQualifier, "enum i", ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int i;void foo()"
                 "{"
                 "i i;struct { i; } r;"
                 "}",
                 Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("typename", "'i'"))
                     && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'i'")) && ProducesNErrors(1)
                     && ProducesNNotes(1));
}

TEST_CASE("Parse structs and unions", "[parser]")
{
    functionProduces(parseStructOrUnionSpecifier, "int",
                     Catch::Contains(EXPECTED_N.args("struct or union")) && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseStructOrUnionSpecifier, "struct",
        Catch::Contains(EXPECTED_N_AFTER_N.args(OpenCL::Format::List(", ", " or ", "identifier", "'{'"), "struct"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseStructOrUnionSpecifier, "union",
        Catch::Contains(EXPECTED_N_AFTER_N.args(OpenCL::Format::List(", ", " or ", "identifier", "'{'"), "union"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "union i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i {int foo, bar;int foobar;}",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{i;}",
                     Catch::Contains(EXPECTED_N_BEFORE_N.args("typename", "'i'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{}",
                     Catch::Contains(N_REQUIRES_AT_LEAST_ONE_N.args("struct", "field")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "union i{}",
                     Catch::Contains(N_REQUIRES_AT_LEAST_ONE_N.args("union", "field")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5;}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int r:5;}",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int foo:;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseStructOrUnionSpecifier, "struct i{unsigned int;}",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "';'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5",
                     Catch::Contains(EXPECTED_N.args("';'")) && Catch::Contains(EXPECTED_N.args("'}'"))
                         && ProducesNErrors(2) && ProducesNoNotes());
}

TEST_CASE("Parse enums", "[parser]")
{
    functionProduces(parseEnumSpecifier, "enum",
                     Catch::Contains(EXPECTED_N_AFTER_N.args("identifier", "enum")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{i}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {}",
                     Catch::Contains(N_REQUIRES_AT_LEAST_ONE_N.args("enum", "value")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test = 5}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {test,}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {test,ft}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {test,ft,}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test = 5",
                     Catch::Contains(EXPECTED_N.args("'}'")) && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test ft}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'ft'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {test,,ft}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "','")) && ProducesNErrors(1)
                         && ProducesNoNotes());
}

TEST_CASE("Parse Declaration", "[parser]")
{
    functionProduces(parseDeclaration, "int foo;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo,bar;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int[ =;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "'='"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(3) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "typedef int [;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(2) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int foo =;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo = 5;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo = 5",
                     Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseDeclaration, "typedef int foo;", ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int aa;void foo(){aa aa;const aa bb;}",
                 Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                     && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                     && ProducesNNotes(1));
    treeProduces("typedef int aa;void foo(){aa aa;const aa;}", ProducesNoErrors() && ProducesNoNotes());
}

TEST_CASE("Parse Declarators and DirectDeclarators", "[parser]")
{
    functionProduces(parseDeclarator, "* const * volatile *i", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "* const * volatile *(i",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "* const * volatile *i(int f",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo(int,int[5])", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(int i,int f[5],)",
                     Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "')'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f e)",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,int)",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'int'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,int)",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'int'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f e,r)",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo[",
                     Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "*",
                     Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier")))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)(int)",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)()",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo()", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(foo bar int)",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'bar'")) && ProducesNErrors(2)
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "'int'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo[static const volatile restrict 5]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[static 5]",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[const static 5]",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[*]",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[5]",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[]",
        Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[const static 5]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[const *]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[const restrict volatile",
                     Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo[*]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[5]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[*[]",
                     Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo(int()",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclarator, "foo(int(int))", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(int([5]))", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(int())", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(int[*])", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(int (i)(int))", ProducesNoErrors() && ProducesNoNotes());
    treeProduces("typedef int i;void foo(a,i){}",
                 Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"))
                     && Catch::Contains(IDENTIFIER_IS_TYPEDEF.args("'i'")) && ProducesNErrors(1) && ProducesNNotes(1));
}

TEST_CASE("Parse parameter (type) list", "[parser]")
{
    functionProduces(parseParameterTypeList, "int,...", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int,int", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int const volatile restrict *", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int const volatile restrict *[]",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int const volatile restrict *foo",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(((foo)))",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(*(*(*foo)))",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(*(*(*)))",
                     ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseParameterTypeList, "int,",
                     Catch::Contains(EXPECTED_N.args("storage specifier or typename")) && ProducesNErrors(1)
                         && ProducesNoNotes());
}

TEST_CASE("Parse Abstract Declarator and Direct Abstract Declarator", "[parser]")
{
    functionProduces(parseAbstractDeclarator, "* const * volatile *", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "* const * volatile *(",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseAbstractDeclarator, "* const * volatile *(int f",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseAbstractDeclarator, "(int,int[5])", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "(int i,int f[5],)",
                     Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "')'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[",
                     Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseAbstractDeclarator, "*", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "(",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseAbstractDeclarator, "()", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[*]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[5]", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[*[]",
                     Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseAbstractDeclarator, "(int()",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseAbstractDeclarator, "(int(int))", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "(int([5]))", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "(int())", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "(int[*])", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "(int (i)(int))", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "]",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'(' or '['", "']'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
}

TEST_CASE("Parse Statements", "[parser]")
{
    SECTION("Return")
    {
        functionProduces(parseStatement, "return;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "return 5;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "return",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "return 5",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "return 5",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseReturnStatement, "5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'return'", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
    SECTION("if")
    {
        functionProduces(parseStatement, "if 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "if(5;int i",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseStatement, "if(5);else;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "if(5)case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "if(5)case:;else;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseIfStatement, "(5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'if'", "'('")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
    SECTION("switch")
    {
        functionProduces(parseStatement, "switch(5);", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "switch 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "switch(5;int i",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseStatement, "switch(5)case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseSwitchStatement, "(5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'switch'", "'('")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
    SECTION("for")
    {
        functionProduces(parseStatement, "for int i = 0; i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'int'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for i = 0; i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0 i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0 i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0; i < 5 i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0 i < 5 i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(2)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0;i < 5;i++;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseStatement, "for(",
                         Catch::Contains(EXPECTED_N.args("expression or declaration")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0;",
                         Catch::Contains(EXPECTED_N.args("expression")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0;i < 0;",
                         Catch::Contains(EXPECTED_N.args("expression")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseForStatement, "(;;);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'for'", "'('")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "for(;;)case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Head while")
    {
        functionProduces(parseStatement, "while(5);", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "while 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "while(5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseHeadWhileStatement, "(5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'while'", "'('")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "while(5)case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Foot while")
    {
        functionProduces(parseStatement, "do;while(5);", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "do;while(5)",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "do;while 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "do;while(5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseStatement, "do;(5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'while'", "'('"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'do'"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(2)
                             && ProducesNNotes(2));
        functionProduces(parseStatement, "do case:;while(5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseFootWhileStatement, ";while(5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'do'", "';'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
    SECTION("break and continue")
    {
        functionProduces(parseStatement, "break;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "continue;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "break",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "continue",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Default and case")
    {
        functionProduces(parseStatement, "default:;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "default;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "';'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "default:case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "case 5:;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "case 5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "';'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "case 5:case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Label and goto")
    {
        functionProduces(parseStatement, "test:;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "test:case:;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "goto test;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "goto test",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStatement, "{test:;goto test;}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "{typedef int test;test:;goto test;}",
                         ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "{test:;goto 0x3;}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'0x3'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
    SECTION("Expression Statement")
    {
        functionProduces(parseStatement, "test;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "test",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        treeProduces("typedef int aa;void foo(){aa aa;aa bb;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                         && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                         && ProducesNNotes(1));
        treeProduces("typedef int aa;int foo(int aa){aa i;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'"))
                         && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                         && ProducesNNotes(1));
    }
    SECTION("Compound statement")
    {
        functionProduces(parseStatement, "{}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStatement, "{",
                         Catch::Contains(EXPECTED_N.args("'}'")) && Catch::Contains(TO_MATCH_N_HERE.args("'{'"))
                             && ProducesNErrors(1) && ProducesNNotes(1));
        functionProduces(parseCompoundStatement, "}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'{'", "'}'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStatement, "{case:;}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
    }
}

TEST_CASE("Parse Initializer and Initializer List", "[parser]")
{
    functionProduces(parseInitializer, "5", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseInitializer, "{5}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,3}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseInitializer, "{[5].m = 5}", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseInitializer, "]",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,",
                     Catch::Contains(EXPECTED_N.args("'}'")) && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseInitializer, "{5 3}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'3'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,[3}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "'}'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'='", "'}'"))
                         && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'}'"))
                         && ProducesNErrors(3) && ProducesNNotes(1));
    functionProduces(parseInitializer, "{[5]. = 5}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'='")) && ProducesNErrors(1)
                         && ProducesNoNotes());
}

TEST_CASE("Parse Expressions", "[parser]")
{
    SECTION("Primary expressions")
    {
        functionProduces(parsePostFixExpression, "0", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "wdawd", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "\"wdawd\"", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "((((5))))", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "((((]))]))",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "']'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(2)
                             && ProducesNNotes(1));
        functionProduces(parsePostFixExpression, "((((]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                             && ProducesNErrors(5) && ProducesNNotes(4));
        treeProduces("void foo(){i;\"string\"\"can also be concatenated\";34234;}",
                     ProducesNoErrors() && ProducesNoNotes());
        treeProduces("void foo(){int i =;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                         OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
        treeProduces(
            "int i =",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Postfix expressions")
    {
        functionProduces(parsePostFixExpression, "i(53,42,32)", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i(53 42,32,53 43",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'42'"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'43'"))
                             && Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                             && ProducesNErrors(3) && ProducesNNotes(1));
        functionProduces(parsePostFixExpression, "i(53,42,32]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "']'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(
            parsePostFixExpression, "i(53,42,32,[5]",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                && ProducesNErrors(2) && ProducesNNotes(1));

        functionProduces(parsePostFixExpression, "(int){5}", ProducesNoErrors() && ProducesNoNotes());
        treeProduces("void foo(){(int[{5};}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "'{'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && Catch::Contains(EXPECTED_N.args("')'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(2) && ProducesNNotes(2));
        functionProduces(parsePostFixExpression, "(int){5 8}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'8'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int)5}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'{'", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int)5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'{'", "'5'"))
                             && Catch::Contains(EXPECTED_N.args("'}'")) && ProducesNErrors(2) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int){5,}", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i[5",
                         Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                             && ProducesNErrors(1) && ProducesNNotes(1));
        functionProduces(parsePostFixExpression, "i[]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i[5]", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i++", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "]++",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "]()",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i--", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "]--",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i.m", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "].m",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.",
                         Catch::Contains(EXPECTED_N.args("identifier")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.[]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                                 OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(2) && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i->m", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "]->m",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i->",
                         Catch::Contains(EXPECTED_N.args("identifier")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i->[]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                                 OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Unary expressions")
    {
        functionProduces(parseUnaryExpression, "sizeof(int)", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "sizeof(5)", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(parseUnaryExpression, "sizeof(int",
                         Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                             && ProducesNErrors(1) && ProducesNNotes(1));
        functionProduces(parseUnaryExpression, "sizeof(5",
                         Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                             && ProducesNErrors(1) && ProducesNNotes(1));

        functionProduces(parseUnaryExpression, "sizeof sizeof(int)", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "sizeof ]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "sizeof(int*[)",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "')'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(
            parseUnaryExpression, "+(int)",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());

        functionProduces(parseUnaryExpression, "++i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "--i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "&i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "*i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "+(int)i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "+i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "-i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "!i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "~i", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Type name")
    {
        functionProduces(parseTypeName, "int", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseTypeName, "int[5]", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseTypeName, "int(*)(int,char)", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Cast expression")
    {
        functionProduces(parseCastExpression, "(int)5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(
            parseCastExpression, "(int)",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("Term")
    {
        functionProduces(parseTerm, "5 * 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseTerm, "5 / 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseTerm, "5 % 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseTerm, "5 * / 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseTerm, "5 * % 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseTerm, "5 * ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseTerm, " % / 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Additive")
    {
        functionProduces(parseAdditiveExpression, "5 + 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAdditiveExpression, "5 - 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseAdditiveExpression, "5 + () + 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseAdditiveExpression, "() + 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());

        functionProduces(parseAdditiveExpression, "5 + - -5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAdditiveExpression, " + + - - 5", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Shift")
    {
        functionProduces(parseShiftExpression, "5 << 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseShiftExpression, "5 >> 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseShiftExpression, "5 << >> 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseShiftExpression, "5 << ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseShiftExpression, "5 << << 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseShiftExpression, " << >> 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'<<'"))
                             && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Relational")
    {
        functionProduces(parseRelationalExpression, "5 < 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseRelationalExpression, "5 > 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseRelationalExpression, "5 <= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseRelationalExpression, "5 >= 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseRelationalExpression, "5 < > 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseRelationalExpression, "5 < ) > 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseRelationalExpression, " <= >= 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Equality")
    {
        functionProduces(parseEqualityExpression, "5 == 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEqualityExpression, "5 != 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseEqualityExpression, "5 == != 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseEqualityExpression, "5 == ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseEqualityExpression, " == != 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Bitand")
    {
        functionProduces(
            parseBitAndExpression, "5 & ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseBitAndExpression, "5 & ]",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseBitAndExpression, "5 & () & 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseBitAndExpression, "() & 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & &5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & & 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseBitAndExpression, " & & 5", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("BitXor")
    {
        functionProduces(parseBitXorExpression, "5 ^ 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseBitXorExpression, "5 ^ ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());

        functionProduces(
            parseBitXorExpression, "5 ^ ^ 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseBitXorExpression, "^ ^ 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("BitOr")
    {
        functionProduces(parseBitOrExpression, "5 | 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseBitOrExpression, "5 | | 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseBitOrExpression, "5 | ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseBitOrExpression, " | | 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("LogicalAnd")
    {
        functionProduces(parseLogicalAndExpression, "5 && 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseLogicalAndExpression, "5 && && 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseLogicalAndExpression, "5 && ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseLogicalAndExpression, " && && 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'&&'"))
                             && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("LogicalOr")
    {
        functionProduces(parseLogicalOrExpression, "5 || 5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(
            parseLogicalOrExpression, "5 || || 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(
            parseLogicalOrExpression, "5 || ()",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseLogicalOrExpression, " || || 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'||'"))
                             && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Conditional")
    {
        functionProduces(parseConditionalExpression, "5 ? 5 : 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseConditionalExpression, "5 ? (5 : 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "':'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseConditionalExpression, "5 ? 5  5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'?'")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseConditionalExpression, "5 ? 5 ? 5 : 5 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'?'")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(
            parseConditionalExpression, "5 ? 5 : 5 ? 5",
            Catch::Contains(EXPECTED_N.args("':'")) && Catch::Contains(TO_MATCH_N_HERE.args("'?'"))
                && Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNNotes(1));
    }
    SECTION("Assignment")
    {
        functionProduces(parseAssignmentExpression, "5 = 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(
            parseAssignmentExpression, "] = ]",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 += 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 -= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 /= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 *= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 %= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 <<= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 >>= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 &= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 |= 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 ^= 5", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("Expressions")
    {
        functionProduces(parseExpression, "5,5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(
            parseExpression, "5 +,5 +",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
        functionProduces(
            parseExpression, "5,",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(1) && ProducesNoNotes());
    }
}


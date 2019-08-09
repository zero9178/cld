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

#define functionProduces(parser, source, matches)                  \
    do                                                             \
    {                                                              \
        std::ostringstream ss;                                     \
        std::vector<OpenCL::Lexer::Token> tokens;                  \
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source)); \
        OpenCL::Parser::ParsingContext context(&ss);               \
        auto begin = tokens.cbegin();                              \
        auto ds = context.setDiagnosticStart(begin);               \
        parser(begin, tokens.cend(), context);                     \
        auto string = ss.str();                                    \
        CHECK_THAT(string, matches);                               \
        if (OpenCL::colourConsoleOutput)                           \
        {                                                          \
            auto begin2 = tokens.cbegin();                         \
            OpenCL::Parser::ParsingContext context2;               \
            auto ds2 = context2.setDiagnosticStart(begin2);        \
            parser(begin2, tokens.cend(), context2);               \
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
using namespace OpenCL::Parser;

TEST_CASE("Parse Declaration Specifiers", "[parser]")
{
    SECTION("Typedef scoping")
    {
        treeProduces("typedef int aa;void foo(){aa aa;}", ProducesNoErrors() && ProducesNoNotes());
        treeProduces("typedef int aa;void foo(){aa aa;aa bb;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                         && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                         && ProducesNNotes(1));
        treeProduces("typedef int aa;void foo(){aa aa;const aa bb;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                         && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                         && ProducesNNotes(1));
        treeProduces("typedef int i;void foo(i){}", Catch::Contains(MISSING_PARAMETER_NAME)
                                                        && Catch::Contains(IDENTIFIER_IS_TYPDEF.args("'i'"))
                                                        && ProducesNErrors(1) && ProducesNNotes(1));
        treeProduces("typedef int i;void foo(a,i){}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"))
                         && Catch::Contains(IDENTIFIER_IS_TYPDEF.args("'i'")) && ProducesNErrors(1)
                         && ProducesNNotes(1));
        treeProduces("typedef int aa;int foo(int aa){aa i;}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'"))
                         && Catch::Contains(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")) && ProducesNErrors(1)
                         && ProducesNNotes(1));
        treeProduces("typedef int aa;"
                     "enum aa; aa r;",
                     ProducesNoErrors() && ProducesNoNotes());
        treeProduces("typedef signed int t;t f(t (t));", ProducesNoErrors() && ProducesNoNotes());
        treeProduces("typedef int aa;"
                     "enum {aa,};",
                     Catch::Contains(REDEFINITION_OF_SYMBOL_N.args("'aa'")) && Catch::Contains(PREVIOUSLY_DECLARED_HERE)
                         && ProducesNErrors(1) && ProducesNNotes(1));
    }
    treeProduces("typedef int aa; typedef extern static auto register const restrict volatile inline void char short "
                 "int long float double signed unsigned f;",
                 ProducesNoErrors() && ProducesNoNotes());
    SECTION("Structs and unions")
    {
        functionProduces(
            parseStructOrUnionSpecifier, "struct",
            Catch::Contains(EXPECTED_N_AFTER_N.args(OpenCL::Format::List(", ", " or ", "identifier", "'{'"), "struct"))
                && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "union i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i{i;}",
                         Catch::Contains(EXPECTED_N_BEFORE_N.args("typename", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i{}",
                         Catch::Contains(N_REQUIRES_AT_LEAST_ONE_N.args("struct", "field")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5;}",
                         ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int r:5;}",
                         ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5",
                         Catch::Contains(EXPECTED_N.args("';'")) && Catch::Contains(EXPECTED_N.args("'}'"))
                             && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Enums")
    {
        functionProduces(parseEnumSpecifier, "enum",
                         Catch::Contains(EXPECTED_N_AFTER_N.args("identifier", "enum")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum i", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum i{i};", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum {}",
                         Catch::Contains(N_REQUIRES_AT_LEAST_ONE_N.args("enum", "value")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum i{test}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum {test,}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum {test,ft}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum {test,ft,}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum i{test ft}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'ft'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseEnumSpecifier, "enum {test,,ft}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "','")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
}

TEST_CASE("Parse Function definitions", "[parser]")
{
    functionProduces(parseExternalDeclaration, "i{}",
                     Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "'i'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f{}",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'{'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo(int,int[5]){}",
                     Catch::Contains(MISSING_PARAMETER_NAME) && ProducesNErrors(2) && ProducesNoNotes());
}

TEST_CASE("Parse Declaration", "[parser]")
{
    functionProduces(parseDeclaration, "int * const * volatile *i;", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int * const * volatile *(i;",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int * const * volatile *i(int f;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int foo(int,int[5]);", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(int i,int f[5],);",
                     Catch::Contains(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "')'"))
                         && ProducesNErrors(1) && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(i,f e);",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNErrors(1)
                         && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(i,f e,r)",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNErrors(2)
                         && Catch::Contains(EXPECTED_N.args("';'")) && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(i,f;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int foo[;",
                     Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && Catch::Contains(TO_MATCH_N_HERE.args("'['")) && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int foo[static 5];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[static const 5];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[const static 5];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[const *];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[*];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[5];", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo[*[];",
                     Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int foo(int();",
                     Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                         && ProducesNErrors(1) && ProducesNNotes(1));
    functionProduces(parseDeclaration, "int foo(int(int));", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(int([5]));", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(int());", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(int[*]);", ProducesNoErrors() && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo(int (i)(int));", ProducesNoErrors() && ProducesNoNotes());
}

TEST_CASE("Parse Statements", "[parser]")
{
    SECTION("Return")
    {
        functionProduces(parseReturnStatement, "return;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseReturnStatement, "return 5;", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseReturnStatement, "return",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseReturnStatement, "return 5",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parseReturnStatement, "return 5",
                         Catch::Contains(EXPECTED_N.args("';'")) && ProducesNErrors(1) && ProducesNoNotes());
    }
    SECTION("if")
    {
        functionProduces(parseIfStatement, "if 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseIfStatement, "if(5;int i",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseIfStatement, "if(5);else;", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("switch")
    {
        functionProduces(parseSwitchStatement, "switch(5);", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseSwitchStatement, "switch 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseSwitchStatement, "switch(5;int i",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
    }
    SECTION("for")
    {
        functionProduces(parseForStatement, "for int i = 0; i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'int'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseForStatement, "for i = 0; i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseForStatement, "for(int i = 0 i < 5; i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseForStatement, "for(int i = 0; i < 5 i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseForStatement, "for(int i = 0 i < 5 i++);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNErrors(2)
                             && ProducesNoNotes());
        functionProduces(parseForStatement, "for(int i = 0;i < 5;i++;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
    }
    SECTION("while, do ... while")
    {
        functionProduces(parseHeadWhileStatement, "while(5);", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseFootWhileStatement, "do;while(5);}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseHeadWhileStatement, "while 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseHeadWhileStatement, "while(5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseFootWhileStatement, "do;while 5);",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
        functionProduces(parseFootWhileStatement, "do;while(5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseFootWhileStatement, "do;(5;",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'while'", "'('"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'do'")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
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
    SECTION("Label and goto")
    {
        functionProduces(parseCompoundStatement, "{test:;goto test;}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseCompoundStatement, "{typedef int test;test:;goto test;}",
                         ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseCompoundStatement, "{test:;goto 0x3;}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'0x3'")) && ProducesNErrors(1)
                             && ProducesNoNotes());
    }
}

TEST_CASE("Parse Expressions", "[parser]")
{
    SECTION("Primary expressions")
    {
        functionProduces(parsePrimaryExpression, "0", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePrimaryExpression, "wdawd", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePrimaryExpression, "\"wdawd\"", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePrimaryExpression, "((((5))))", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePrimaryExpression, "((((]))]))",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("')'", "']'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'('")) && ProducesNErrors(2)
                             && ProducesNNotes(1));
        functionProduces(parsePrimaryExpression, "((((]",
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
        functionProduces(
            parsePostFixExpression, "i(53,42,32,[5]",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && Catch::Contains(EXPECTED_N.args("')'")) && Catch::Contains(TO_MATCH_N_HERE.args("'('"))
                && ProducesNErrors(2) && ProducesNNotes(1));

        functionProduces(parsePostFixExpression, "(int){5}", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int){5 8}",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("'}'", "'8'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'{'")) && ProducesNErrors(1)
                             && ProducesNNotes(1));

        functionProduces(parsePostFixExpression, "i[5",
                         Catch::Contains(EXPECTED_N.args("']'")) && Catch::Contains(TO_MATCH_N_HERE.args("'['"))
                             && ProducesNErrors(1) && ProducesNNotes(1));
        functionProduces(parsePostFixExpression, "i[]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i[5]", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i++", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i--", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i.m", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.",
                         Catch::Contains(EXPECTED_N.args("identifier")) && ProducesNErrors(1) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.[]",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                             && Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                                 OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNErrors(2) && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i->m", ProducesNoErrors() && ProducesNoNotes());
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
            parseEqualityExpression, " == != 5",
            Catch::Contains(EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")))
                && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Bitand")
    {
        functionProduces(parseBitAndExpression, "5 & 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & &5", ProducesNoErrors() && ProducesNoNotes());

        functionProduces(parseBitAndExpression, "5 & & 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseBitAndExpression, " & & 5", ProducesNoErrors() && ProducesNoNotes());
    }
    SECTION("BitXor")
    {
        functionProduces(parseBitXorExpression, "5 ^ 5", ProducesNoErrors() && ProducesNoNotes());

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
        functionProduces(parseLogicalOrExpression, " || || 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args(
                             OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), "'||'"))
                             && ProducesNErrors(2) && ProducesNoNotes());
    }
    SECTION("Conditional")
    {
        functionProduces(parseConditionalExpression, "5 ? 5 : 5", ProducesNoErrors() && ProducesNoNotes());
        functionProduces(parseConditionalExpression, "5 ? 5  5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'?'")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseConditionalExpression, "5 ? 5 ? 5 : 5 5",
                         Catch::Contains(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                             && Catch::Contains(TO_MATCH_N_HERE.args("'?'")) && ProducesNErrors(1)
                             && ProducesNNotes(1));
        functionProduces(parseConditionalExpression, "5 ? 5 : 5 ? 5",
                         Catch::Contains(EXPECTED_N.args("':'")) && Catch::Contains(TO_MATCH_N_HERE.args("'?'"))
                             && ProducesNErrors(1) && ProducesNNotes(1));
    }
    SECTION("Assignment")
    {
        functionProduces(parseAssignmentExpression, "5 = 5", ProducesNoErrors() && ProducesNoNotes());
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
    }
}

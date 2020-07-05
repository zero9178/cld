#include "catch.hpp"

#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Compiler/Parser.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

#include "TestConfig.hpp"

#define treeProduces(source, matches)                                               \
    do                                                                              \
    {                                                                               \
        std::string string;                                                         \
        llvm::raw_string_ostream ss(string);                                        \
        cld::PPSourceObject tokens;                                                 \
        tokens = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), &ss); \
        REQUIRE(string.empty());                                                    \
        auto ctokens = cld::Lexer::toCTokens(tokens, &ss);                          \
        REQUIRE(string.empty());                                                    \
        auto tree = cld::Parser::buildTree(ctokens, &ss);                           \
        CHECK_THAT(string, matches);                                                \
        cld::Parser::buildTree(ctokens);                                            \
        if (!string.empty())                                                        \
        {                                                                           \
            llvm::errs() << '\n';                                                   \
        }                                                                           \
    } while (0)

#define functionProduces(parser, source, matches)                                               \
    do                                                                                          \
    {                                                                                           \
        std::string string;                                                                     \
        llvm::raw_string_ostream ss(string);                                                    \
        cld::PPSourceObject tokens;                                                             \
        tokens = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), &ss);             \
        REQUIRE(string.empty());                                                                \
        auto ctokens = cld::Lexer::toCTokens(tokens, &ss);                                      \
        REQUIRE(string.empty());                                                                \
        cld::Parser::Context context(ctokens, &ss);                                             \
        auto begin = std::as_const(ctokens).data().data();                                      \
        parser(begin, ctokens.data().data() + ctokens.data().size(), context);                  \
        {                                                                                       \
            INFO("Function " #parser " with source " << source);                                \
            CHECK((!string.empty() || begin == ctokens.data().data() + ctokens.data().size())); \
        }                                                                                       \
        CHECK_THAT(string, matches);                                                            \
        {                                                                                       \
            auto begin2 = std::as_const(ctokens).data().data();                                 \
            cld::Parser::Context context2(ctokens);                                             \
            parser(begin2, ctokens.data().data() + ctokens.data().size(), context2);            \
            if (!string.empty())                                                                \
            {                                                                                   \
                llvm::errs() << '\n';                                                           \
            }                                                                                   \
        }                                                                                       \
    } while (0)

using namespace cld::Notes;
using namespace cld::Errors;
using namespace cld::Errors::Parser;
using namespace cld::Parser;

TEST_CASE("Parse specifier qualifier list", "[parser]")
{
    treeProduces("void foo(){typedef int i;sizeof(const i f);}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "'f'"))
                     && ProducesNote(TO_MATCH_N_HERE.args("'('")));
}

TEST_CASE("Parse external declaration", "[parser]")
{
    functionProduces(parseExternalDeclaration, "int;", ProducesNothing());
    functionProduces(parseExternalDeclaration, "int i", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "i{}",
                     ProducesError(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "'i'"))
                         && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f;{}", ProducesNothing());
    functionProduces(parseExternalDeclaration, "int i(void) {}", ProducesNothing());
    functionProduces(parseExternalDeclaration, "int () int f;{}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "')'")) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f{}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'{'")) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo(int,int[5]){}",
                     ProducesError(MISSING_PARAMETER_NAME) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo = 5;", ProducesNothing());
    functionProduces(parseExternalDeclaration, "int foo = 5",
                     ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int[;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseExternalDeclaration, "int[ =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "'='"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'")));
    functionProduces(parseExternalDeclaration, "int foo,bar = 5;", ProducesNothing());
    functionProduces(parseExternalDeclaration, "int foo,bar;", ProducesNothing());
    functionProduces(parseExternalDeclaration, "int foo,[;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseExternalDeclaration, "int foo,bar =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo,[ =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "'='"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'")));
    functionProduces(parseExternalDeclaration, "typedef int [;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    treeProduces("typedef int i;void foo(i){}",
                 ProducesError(MISSING_PARAMETER_NAME) && ProducesNote(IDENTIFIER_IS_TYPEDEF.args("'i'")));
    functionProduces(parseExternalDeclaration, "typedef int [;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    treeProduces("\n"
                 "void barFunc(int,char);\n"
                 "\n"
                 "void (*foo(foo,i,bar))(int,char) short foo,i,bar;\n"
                 "{\n"
                 "    return barFunc;\n"
                 "}",
                 ProducesNothing());
    treeProduces("typedef int i;void foo(int i,i i)"
                 "{"
                 "}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N.args("typename", "'i'"))
                     && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'i'")));
    treeProduces("typedef int i,bar;void foo(int i,bar i)"
                 "{"
                 "}",
                 ProducesNothing());
}

TEST_CASE("Parser typedef scoping and resolution", "[parser]")
{
    treeProduces("typedef int aa;void foo(){aa aa;}", ProducesNothing());
    treeProduces("typedef int aa;void foo(){aa aa;const aa;}", ProducesNothing());
    treeProduces("typedef int aa;"
                 "enum aa; aa r;",
                 ProducesNothing());
    treeProduces("typedef signed int t;t f(t (t));", ProducesNothing());
    treeProduces("typedef int aa;"
                 "enum {aa,};",
                 ProducesError(REDEFINITION_OF_SYMBOL_N.args("'aa'")) && ProducesNote(PREVIOUSLY_DECLARED_HERE));
}

TEST_CASE("Parse Declaration Specifiers", "[parser]")
{
    treeProduces("typedef int aa; typedef extern static auto register const restrict volatile inline void char short "
                 "int long float double signed unsigned f;",
                 ProducesNothing());
    functionProduces(parseDeclarationSpecifier, "struct i", ProducesNothing());
    functionProduces(parseDeclarationSpecifier, "union i", ProducesNothing());
    functionProduces(parseDeclarationSpecifier, "enum i", ProducesNothing());
}

TEST_CASE("Parse Specifier Qualifiers", "[parser]")
{
    functionProduces(
        parseSpecifierQualifierList,
        "const void long float double signed restrict volatile char short int long float double signed unsigned",
        ProducesNothing());
    functionProduces(parseSpecifierQualifier, "struct i", ProducesNothing());
    functionProduces(parseSpecifierQualifier, "union i", ProducesNothing());
    functionProduces(parseSpecifierQualifier, "enum i", ProducesNothing());
    treeProduces("typedef int i;void foo()"
                 "{"
                 "i i;struct { i; } r;"
                 "}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N.args("typename", "'i'"))
                     && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'i'")));
}

TEST_CASE("Parse structs and unions", "[parser]")
{
    functionProduces(parseStructOrUnionSpecifier, "int",
                     ProducesError(EXPECTED_N.args("struct or union")) && ProducesNoNotes());
    functionProduces(
        parseStructOrUnionSpecifier, "struct",
        ProducesError(EXPECTED_N_AFTER_N.args(cld::Format::List(", ", " or ", "identifier", "'{'"), "struct"))
            && ProducesNoNotes());
    functionProduces(
        parseStructOrUnionSpecifier, "union",
        ProducesError(EXPECTED_N_AFTER_N.args(cld::Format::List(", ", " or ", "identifier", "'{'"), "union"))
            && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i", ProducesNothing());
    functionProduces(parseStructOrUnionSpecifier, "union i", ProducesNothing());
    functionProduces(parseStructOrUnionSpecifier, "struct i {int foo, bar;int foobar;}", ProducesNothing());
    functionProduces(parseStructOrUnionSpecifier, "struct i{i;}",
                     ProducesError(EXPECTED_N_BEFORE_N.args("typename", "'i'")) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{}",
                     ProducesError(N_REQUIRES_AT_LEAST_ONE_N.args("struct", "field")) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "union i{}",
                     ProducesError(N_REQUIRES_AT_LEAST_ONE_N.args("union", "field")) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5;}", ProducesNothing());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int r:5;}", ProducesNothing());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'}'")) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int foo:;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNoNotes());
    functionProduces(
        parseStructOrUnionSpecifier, "struct i{unsigned int;}",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "';'"))
            && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5",
                     ProducesError(EXPECTED_N.args("';'")) && ProducesError(EXPECTED_N.args("'}'"))
                         && ProducesNoNotes());
}

TEST_CASE("Parse enums", "[parser]")
{
    functionProduces(parseEnumSpecifier, "enum",
                     ProducesError(EXPECTED_N_AFTER_N.args("identifier", "enum")) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "';'")) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum i{i}", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum {}",
                     ProducesError(N_REQUIRES_AT_LEAST_ONE_N.args("enum", "value")) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test}", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum i{test = 5}", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum {test,}", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum {test,ft}", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum {test,ft,}", ProducesNothing());
    functionProduces(parseEnumSpecifier, "enum i{test = 5", ProducesError(EXPECTED_N.args("'}'")) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test ft}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'ft'")) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {test,,ft}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "','")) && ProducesNoNotes());
}

TEST_CASE("Parse Declaration", "[parser]")
{
    functionProduces(parseDeclaration, "int foo;", ProducesNothing());
    functionProduces(parseDeclaration, "int;", ProducesNothing());
    functionProduces(parseDeclaration, "int foo,bar;", ProducesNothing());
    functionProduces(parseDeclaration, "int[ =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "'='"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'")));
    functionProduces(parseDeclaration, "typedef int [;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or identifier", "'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "';'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseDeclaration, "int foo =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo = 5;", ProducesNothing());
    functionProduces(parseDeclaration, "int foo = 5", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
    functionProduces(parseDeclaration, "typedef int foo;", ProducesNothing());
    treeProduces("typedef int aa;void foo(){aa aa;const aa bb;}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                     && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")));
    treeProduces("typedef int aa;void foo(){aa aa;const aa;}", ProducesNothing());
}

TEST_CASE("Parse Declarators and DirectDeclarators", "[parser]")
{
    functionProduces(parseDeclarator, "* const * volatile *i", ProducesNothing());
    functionProduces(parseDeclarator, "* const * volatile *(i",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseDeclarator, "* const * volatile *i(int f",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseDeclarator, "foo(int,int[5])", ProducesNothing());
    functionProduces(parseDeclarator, "foo(int i,int f[5],)",
                     ProducesError(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "')'"))
                         && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f e)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'int'")) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'int'")) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f e,r)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'e'")) && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseDeclarator, "foo[",
                     ProducesError(EXPECTED_N.args("']'")) && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseDeclarator, "*",
                     ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "'('", "identifier")))
                         && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)(int)",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)()",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseDeclarator, "foo()", ProducesNothing());
    functionProduces(parseDeclarator, "foo(foo bar int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'bar'"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "'int'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseDeclarator, "foo[static const volatile restrict 5]", ProducesNothing());
    functionProduces(
        parseDeclarator, "(int)[static 5]",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[const static 5]",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[*]",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[5]",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(
        parseDeclarator, "(int)[]",
        ProducesError(EXPECTED_N_INSTEAD_OF_N.args(cld::Format::List(", ", " or ", "'('", "identifier"), "'int'"))
            && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[]", ProducesNothing());
    functionProduces(parseDeclarator, "foo[const static 5]", ProducesNothing());
    functionProduces(parseDeclarator, "foo[const *]", ProducesNothing());
    functionProduces(parseDeclarator, "foo[const restrict volatile",
                     ProducesError(EXPECTED_N.args("']'")) && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseDeclarator, "foo[*]", ProducesNothing());
    functionProduces(parseDeclarator, "foo[5]", ProducesNothing());
    functionProduces(parseDeclarator, "foo[*[]",
                     ProducesError(EXPECTED_N.args("']'")) && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseDeclarator, "foo(int()",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseDeclarator, "foo(int(int))", ProducesNothing());
    functionProduces(parseDeclarator, "foo(int([5]))", ProducesNothing());
    functionProduces(parseDeclarator, "foo(int())", ProducesNothing());
    functionProduces(parseDeclarator, "foo(int[*])", ProducesNothing());
    functionProduces(parseDeclarator, "foo(int (i)(int))", ProducesNothing());
    treeProduces("typedef int i;void foo(a,i){}", ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"))
                                                      && ProducesNote(IDENTIFIER_IS_TYPEDEF.args("'i'")));
}

TEST_CASE("Parse parameter (type) list", "[parser]")
{
    functionProduces(parseParameterTypeList, "int,...", ProducesNothing());
    functionProduces(parseParameterTypeList, "int,int", ProducesNothing());
    functionProduces(parseParameterTypeList, "int const volatile restrict *", ProducesNothing());
    functionProduces(parseParameterTypeList, "int const volatile restrict *[]", ProducesNothing());
    functionProduces(parseParameterTypeList, "int const volatile restrict *foo", ProducesNothing());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(((foo)))", ProducesNothing());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(*(*(*foo)))", ProducesNothing());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(*(*(*)))", ProducesNothing());
    functionProduces(parseParameterTypeList, "int,",
                     ProducesError(EXPECTED_N.args("storage specifier or typename")) && ProducesNoNotes());
}

TEST_CASE("Parse Abstract Declarator and Direct Abstract Declarator", "[parser]")
{
    functionProduces(parseAbstractDeclarator, "* const * volatile *", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "* const * volatile *(",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseAbstractDeclarator, "* const * volatile *(int f",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseAbstractDeclarator, "(int,int[5])", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "(int i,int f[5],)",
                     ProducesError(EXPECTED_N_BEFORE_N.args("storage specifier or typename", "')'"))
                         && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[",
                     ProducesError(EXPECTED_N.args("']'")) && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseAbstractDeclarator, "*", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "(",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseAbstractDeclarator, "()", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "[]", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "[*]", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "[5]", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "[*[]",
                     ProducesError(EXPECTED_N.args("']'")) && ProducesNote(TO_MATCH_N_HERE.args("'['")));
    functionProduces(parseAbstractDeclarator, "(int()",
                     ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    functionProduces(parseAbstractDeclarator, "(int(int))", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "(int([5]))", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "(int())", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "(int[*])", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "(int (i)(int))", ProducesNothing());
    functionProduces(parseAbstractDeclarator, "]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'(' or '['", "']'")) && ProducesNoNotes());
}

TEST_CASE("Parse Statements", "[parser]")
{
    SECTION("Return")
    {
        functionProduces(parseStatement, "return;", ProducesNothing());
        functionProduces(parseStatement, "return 5;", ProducesNothing());
        functionProduces(parseStatement, "return", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "return 5", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "return 5", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        functionProduces(parseReturnStatement, "5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'return'", "'5'")) && ProducesNoNotes());
    }
    SECTION("if")
    {
        functionProduces(parseStatement, "if 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNoNotes());
        functionProduces(parseStatement, "if(5;int i",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseStatement, "if(5);else;", ProducesNothing());
        functionProduces(parseStatement, "if(5)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
        functionProduces(parseStatement, "if(5)case:;else;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
        functionProduces(parseIfStatement, "(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'if'", "'('")) && ProducesNoNotes());
    }
    SECTION("switch")
    {
        functionProduces(parseStatement, "switch(5);", ProducesNothing());
        functionProduces(parseStatement, "switch 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNoNotes());
        functionProduces(parseStatement, "switch(5;int i",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseStatement, "switch(5)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
        functionProduces(parseSwitchStatement, "(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'switch'", "'('")) && ProducesNoNotes());
    }
    SECTION("for")
    {
        functionProduces(parseStatement, "for int i = 0; i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'('", "'int'")) && ProducesNoNotes());
        functionProduces(parseStatement, "for i = 0; i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'('", "'i'")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0 i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0 i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0; i < 5 i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0 i < 5 i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0;i < 5;i++;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseStatement, "for(",
                         ProducesError(EXPECTED_N.args("expression or declaration")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0;",
                         ProducesError(EXPECTED_N.args("expression")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0;i < 0;",
                         ProducesError(EXPECTED_N.args("expression")) && ProducesNoNotes());
        functionProduces(parseForStatement, "(;;);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'for'", "'('")) && ProducesNoNotes());
        functionProduces(parseStatement, "for(;;)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
    }
    SECTION("Head while")
    {
        functionProduces(parseStatement, "while(5);", ProducesNothing());
        functionProduces(parseStatement, "while 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNoNotes());
        functionProduces(parseStatement, "while(5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseHeadWhileStatement, "(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'while'", "'('")) && ProducesNoNotes());
        functionProduces(parseStatement, "while(5)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
    }
    SECTION("Foot while")
    {
        functionProduces(parseStatement, "do;while(5);", ProducesNothing());
        functionProduces(parseStatement, "do;while(5)", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "do;while 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'('", "'5'")) && ProducesNoNotes());
        functionProduces(parseStatement, "do;while(5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseStatement, "do;(5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'while'", "'('"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'do'"))
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "';'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseStatement, "do case:;while(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
        functionProduces(parseFootWhileStatement, ";while(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'do'", "';'")) && ProducesNoNotes());
    }
    SECTION("break and continue")
    {
        functionProduces(parseStatement, "break;", ProducesNothing());
        functionProduces(parseStatement, "continue;", ProducesNothing());
        functionProduces(parseStatement, "break", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "continue", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
    }
    SECTION("Default and case")
    {
        functionProduces(parseStatement, "default:;", ProducesNothing());
        functionProduces(parseStatement, "default;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("':'", "';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "default:case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
        functionProduces(parseStatement, "case 5:;", ProducesNothing());
        functionProduces(parseStatement, "case 5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("':'", "';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "case 5:case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
    }
    SECTION("Label and goto")
    {
        functionProduces(parseStatement, "test:;", ProducesNothing());
        functionProduces(parseStatement, "test:case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
        functionProduces(parseStatement, "goto test;", ProducesNothing());
        functionProduces(parseStatement, "goto test", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        functionProduces(parseStatement, "{test:;goto test;}", ProducesNothing());
        functionProduces(parseStatement, "{typedef int test;test:;goto test;}", ProducesNothing());
        functionProduces(parseStatement, "{test:;goto 0x3;}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'0x3'")) && ProducesNoNotes());
    }
    SECTION("Expression Statement")
    {
        functionProduces(parseStatement, "test;", ProducesNothing());
        functionProduces(parseStatement, "test", ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
        treeProduces("typedef int aa;void foo(){aa aa;aa bb;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'bb'"))
                         && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")));
        treeProduces("typedef int aa;int foo(int aa){aa i;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("';'", "'i'"))
                         && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION.args("'aa'")));
    }
    SECTION("Compound statement")
    {
        functionProduces(parseStatement, "{}", ProducesNothing());
        functionProduces(parseStatement, "{",
                         ProducesError(EXPECTED_N.args("'}'")) && ProducesNote(TO_MATCH_N_HERE.args("'{'")));
        functionProduces(parseCompoundStatement, "}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'{'", "'}'")) && ProducesNoNotes());
        functionProduces(parseStatement, "{case:;}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "':'"))
                             && ProducesNoNotes());
    }
}

TEST_CASE("Parse Initializer and Initializer List", "[parser]")
{
    functionProduces(parseInitializer, "5", ProducesNothing());
    functionProduces(parseInitializer, "{5}", ProducesNothing());
    functionProduces(parseInitializer, "{5,}", ProducesNothing());
    functionProduces(parseInitializer, "{5,3}", ProducesNothing());
    functionProduces(parseInitializer, "{[5].m = 5}", ProducesNothing());
    functionProduces(parseInitializer, "]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                         && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,", ProducesError(EXPECTED_N.args("'}'")) && ProducesNoNotes());
    functionProduces(parseInitializer, "{5 3}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'3'")) && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,[3}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "'}'"))
                         && ProducesNote(TO_MATCH_N_HERE.args("'['"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'='", "'}'"))
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "'}'")));
    functionProduces(parseInitializer, "{[5]. = 5}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'='")) && ProducesNoNotes());
}

TEST_CASE("Parse Expressions", "[parser]")
{
    SECTION("Primary expressions")
    {
        functionProduces(parsePostFixExpression, "0", ProducesNothing());
        functionProduces(parsePostFixExpression, "wdawd", ProducesNothing());
        functionProduces(parsePostFixExpression, "\"wdawd\"", ProducesNothing());
        functionProduces(parsePostFixExpression, "((((5))))", ProducesNothing());
        functionProduces(parsePostFixExpression, "((((]))]))",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "']'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parsePostFixExpression, "((((]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        treeProduces("void foo(){i;\"string\"\"can also be concatenated\";34234;}", ProducesNothing());
        treeProduces("void foo(){int i =;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                         cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "';'"))
                         && ProducesNoNotes());
        treeProduces("int i =",
                     ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                         && ProducesError(EXPECTED_N.args("';'")) && ProducesNoNotes());
    }
    SECTION("Postfix expressions")
    {
        functionProduces(parsePostFixExpression, "i(53,42,32)", ProducesNothing());
        functionProduces(parsePostFixExpression, "i(53 42,32,53 43",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'42'"))
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'43'"))
                             && ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parsePostFixExpression, "i(53,42,32]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "']'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parsePostFixExpression, "i(53,42,32,[5]",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));

        functionProduces(parsePostFixExpression, "(int){5}", ProducesNothing());
        treeProduces("void foo(){(int[{5};}", ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "'{'"))
                                                  && ProducesNote(TO_MATCH_N_HERE.args("'['"))
                                                  && ProducesError(EXPECTED_N.args("')'"))
                                                  && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parsePostFixExpression, "(int){5 8}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("','", "'8'")) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int)5}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'{'", "'5'")) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int)5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'{'", "'5'"))
                             && ProducesError(EXPECTED_N.args("'}'")) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int){5,}", ProducesNothing());

        functionProduces(parsePostFixExpression, "i[5",
                         ProducesError(EXPECTED_N.args("']'")) && ProducesNote(TO_MATCH_N_HERE.args("'['")));
        functionProduces(parsePostFixExpression, "i[]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i[5]", ProducesNothing());

        functionProduces(parsePostFixExpression, "i++", ProducesNothing());
        functionProduces(parsePostFixExpression, "]++",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "]()",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i--", ProducesNothing());
        functionProduces(parsePostFixExpression, "]--",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i.m", ProducesNothing());
        functionProduces(parsePostFixExpression, "].m",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.",
                         ProducesError(EXPECTED_N.args("identifier")) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.[]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                                 cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i->m", ProducesNothing());
        functionProduces(parsePostFixExpression, "]->m",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i->",
                         ProducesError(EXPECTED_N.args("identifier")) && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i->[]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'['"))
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                                 cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
    }
    SECTION("Unary expressions")
    {
        functionProduces(parseUnaryExpression, "sizeof(int)", ProducesNothing());
        functionProduces(parseUnaryExpression, "sizeof(5)", ProducesNothing());

        functionProduces(parseUnaryExpression, "sizeof(int",
                         ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseUnaryExpression, "sizeof(5",
                         ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));

        functionProduces(parseUnaryExpression, "sizeof sizeof(int)", ProducesNothing());
        functionProduces(parseUnaryExpression, "sizeof ]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "']'"))
                             && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "sizeof(int*[)",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("']'", "')'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'['")));
        functionProduces(parseUnaryExpression, "+(int)",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());

        functionProduces(parseUnaryExpression, "++i", ProducesNothing());
        functionProduces(parseUnaryExpression, "--i", ProducesNothing());
        functionProduces(parseUnaryExpression, "&i", ProducesNothing());
        functionProduces(parseUnaryExpression, "*i", ProducesNothing());
        functionProduces(parseUnaryExpression, "+(int)i", ProducesNothing());
        functionProduces(parseUnaryExpression, "+i", ProducesNothing());
        functionProduces(parseUnaryExpression, "-i", ProducesNothing());
        functionProduces(parseUnaryExpression, "!i", ProducesNothing());
        functionProduces(parseUnaryExpression, "~i", ProducesNothing());
    }
    SECTION("Type name")
    {
        functionProduces(parseTypeName, "int", ProducesNothing());
        functionProduces(parseTypeName, "int[5]", ProducesNothing());
        functionProduces(parseTypeName, "int(*)(int,char)", ProducesNothing());
    }
    SECTION("Cast expression")
    {
        functionProduces(parseCastExpression, "(int)5", ProducesNothing());
        functionProduces(parseCastExpression, "(int)",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
    SECTION("Term")
    {
        functionProduces(parseTerm, "5 * 5", ProducesNothing());
        functionProduces(parseTerm, "5 / 5", ProducesNothing());
        functionProduces(parseTerm, "5 % 5", ProducesNothing());

        functionProduces(parseTerm, "5 * / 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseTerm, "5 * % 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseTerm, "5 * ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseTerm, " % / 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
    SECTION("Additive")
    {
        functionProduces(parseAdditiveExpression, "5 + 5", ProducesNothing());
        functionProduces(parseAdditiveExpression, "5 - 5", ProducesNothing());

        functionProduces(parseAdditiveExpression, "5 + () + 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseAdditiveExpression, "() + 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());

        functionProduces(parseAdditiveExpression, "5 + - -5", ProducesNothing());
        functionProduces(parseAdditiveExpression, " + + - - 5", ProducesNothing());
    }
    SECTION("Shift")
    {
        functionProduces(parseShiftExpression, "5 << 5", ProducesNothing());
        functionProduces(parseShiftExpression, "5 >> 5", ProducesNothing());

        functionProduces(parseShiftExpression, "5 << >> 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseShiftExpression, "5 << ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseShiftExpression, "5 << << 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseShiftExpression, " << >> 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "'<<'"))
                             && ProducesNoNotes());
    }
    SECTION("Relational")
    {
        functionProduces(parseRelationalExpression, "5 < 5", ProducesNothing());
        functionProduces(parseRelationalExpression, "5 > 5", ProducesNothing());
        functionProduces(parseRelationalExpression, "5 <= 5", ProducesNothing());
        functionProduces(parseRelationalExpression, "5 >= 5", ProducesNothing());

        functionProduces(parseRelationalExpression, "5 < > 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseRelationalExpression, "5 < ) > 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseRelationalExpression, " <= >= 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
    SECTION("Equality")
    {
        functionProduces(parseEqualityExpression, "5 == 5", ProducesNothing());
        functionProduces(parseEqualityExpression, "5 != 5", ProducesNothing());

        functionProduces(parseEqualityExpression, "5 == != 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseEqualityExpression, "5 == ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseEqualityExpression, " == != 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
    SECTION("Bitand")
    {
        functionProduces(parseBitAndExpression, "5 & ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & ]",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & () & 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "() & 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & 5", ProducesNothing());
        functionProduces(parseBitAndExpression, "5 & &5", ProducesNothing());
        functionProduces(parseBitAndExpression, "5 & & 5", ProducesNothing());
        functionProduces(parseBitAndExpression, " & & 5", ProducesNothing());
    }
    SECTION("BitXor")
    {
        functionProduces(parseBitXorExpression, "5 ^ 5", ProducesNothing());

        functionProduces(parseBitXorExpression, "5 ^ ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());

        functionProduces(parseBitXorExpression, "5 ^ ^ 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitXorExpression, "^ ^ 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
    SECTION("BitOr")
    {
        functionProduces(parseBitOrExpression, "5 | 5", ProducesNothing());

        functionProduces(parseBitOrExpression, "5 | | 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitOrExpression, "5 | ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseBitOrExpression, " | | 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
    SECTION("LogicalAnd")
    {
        functionProduces(parseLogicalAndExpression, "5 && 5", ProducesNothing());

        functionProduces(parseLogicalAndExpression, "5 && && 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseLogicalAndExpression, "5 && ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseLogicalAndExpression, " && && 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "'&&'"))
                             && ProducesNoNotes());
    }
    SECTION("LogicalOr")
    {
        functionProduces(parseLogicalOrExpression, "5 || 5", ProducesNothing());

        functionProduces(parseLogicalOrExpression, "5 || || 5",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseLogicalOrExpression, "5 || ()",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseLogicalOrExpression, " || || 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args(
                             cld::Format::List(", ", " or ", "literal", "identifier", "'('"), "'||'"))
                             && ProducesNoNotes());
    }
    SECTION("Conditional")
    {
        functionProduces(parseConditionalExpression, "5 ? 5 : 5", ProducesNothing());
        functionProduces(parseConditionalExpression, "5 ? (5 : 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("')'", "':'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'('")));
        functionProduces(parseConditionalExpression, "5 ? 5  5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'?'")));
        functionProduces(parseConditionalExpression, "5 ? 5 ? 5 : 5 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("':'", "'5'"))
                             && ProducesNote(TO_MATCH_N_HERE.args("'?'")));
        functionProduces(
            parseConditionalExpression, "5 ? 5 : 5 ? 5",
            ProducesError(EXPECTED_N.args("':'")) && ProducesNote(TO_MATCH_N_HERE.args("'?'"))
                && ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('"))));
    }
    SECTION("Assignment")
    {
        functionProduces(parseAssignmentExpression, "5 = 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "] = ]",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 += 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 -= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 /= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 *= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 %= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 <<= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 >>= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 &= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 |= 5", ProducesNothing());
        functionProduces(parseAssignmentExpression, "5 ^= 5", ProducesNothing());
    }
    SECTION("Expressions")
    {
        functionProduces(parseExpression, "5,5", ProducesNothing());
        functionProduces(parseExpression, "5 +,5 +",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
        functionProduces(parseExpression, "5,",
                         ProducesError(EXPECTED_N.args(cld::Format::List(", ", " or ", "literal", "identifier", "'('")))
                             && ProducesNoNotes());
    }
}

#if defined(NDEBUG) || !defined(_WIN32)
    #ifndef __has_feature
        #define UNRESTRICTED_STACK
    #else
        #if !__has_feature(address_sanitizer) && !defined(__SANITIZE_ADDRESS__)
            #define UNRESTRICTED_STACK
        #endif
    #endif
#endif

#if defined(UNRESTRICTED_STACK) && defined(LLVM_ENABLE_EXCEPTIONS)
TEST_CASE("Parser limits", "[parser]")
{
    SECTION("Parenthese expression")
    {
        auto source = "int main(void){" + std::string(cld::Limits::Parser::MAX_BRACKET_DEPTH + 1, '(');
        treeProduces(source, ProducesError(cld::Errors::Parser::MAXIMUM_N_DEPTH_OF_N_EXCEEDED.args(
                                 "bracket", cld::Limits::Parser::MAX_BRACKET_DEPTH)));
    }
    SECTION("Direct Declarator")
    {
        auto source = "int" + std::string(cld::Limits::Parser::MAX_BRACKET_DEPTH + 1, '(');
        treeProduces(source, ProducesError(cld::Errors::Parser::MAXIMUM_N_DEPTH_OF_N_EXCEEDED.args(
                                 "bracket", cld::Limits::Parser::MAX_BRACKET_DEPTH)));
    }
    SECTION("Compound statement")
    {
        auto source = "int main(void)" + std::string(cld::Limits::Parser::MAX_BRACKET_DEPTH + 1, '{');
        treeProduces(source, ProducesError(cld::Errors::Parser::MAXIMUM_N_DEPTH_OF_N_EXCEEDED.args(
                                 "bracket", cld::Limits::Parser::MAX_BRACKET_DEPTH)));
    }
}
#endif

namespace
{
void parse(const std::string& source)
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    auto tokens = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), &ss);
    if (!ss.str().empty() || tokens.data().empty())
    {
        return;
    }
    auto ctokens = cld::Lexer::toCTokens(tokens,&ss);
    if (!ss.str().empty() || tokens.data().empty())
    {
        return;
    }

    cld::Parser::buildTree(ctokens, &ss);
}

[[maybe_unused]] void excludeFromAddressSanitizer()
{
    parse(
        "*l=((((((((((((((((((((((((((((((((((((((((((((((((((((((( (((((((((((((((((((li(((((((((((((( (((((((((((((((((((( ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( (((((((((li(((((((((((((( (((((((((((((((((((( ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((({(((((((((((((((((((((((((h((((((((((((((((((((((((((((((((((((((((({(((((((((((((((((((((((((h((((((((((((((((((((((((( (((((((((((((((((((({((((((((((((((((((((((((u");
    parse(
        "([([[(([([((n([([[([[(([([(([([8[[(([[([(([([([([[[([[([([[(([([(([([8[[(([[([([[(([([(([([8[[(([[([[([[([[(([([(([([8[[([([[([[(([([(([([8[[(([([8[[(([[([(([([([([[[(([([[[[(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([([-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([8[[(([[([(([([([([[[(([([[[[(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([[([[(([(?[(([([(([([8[[(([([8[[(([[([[([(([([([[[[([((([([(([([7[[((-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([[[[(([([(([([([[([([7[[(([[([((([(");
    parse(
        "([([[(([([((n([([[([[(8[[(([([8[[(([[([(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[>=[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[([(((((A[[[[(([[[[[[([[([[(([([(([([8[[(([([8[[(([[([(([([([([[[([([([([[[(([([[[[(([([(([([([[([([[(([([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[(k[(([[([([(([([8[[((A[[[[(([[[[>=[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[([(((((A[[[[(([[[[[[([[([[(([([(([([8[[(([([8[[(([[([(([([([([[[(([([[[[(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[(k[((([([(([([7[[((-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([[[[(([([(([([([[([([7[[(([[([((([(");
}
} // namespace

TEST_CASE("Parser fuzzer discoveries", "[parser]")
{
    parse(
        " un union u union{A^union\\\n"
        "J  union u union{An uninion nu[ union V    ucontinuen union nu[ union V^union  un union nu[ union V^^uun union u union{An union u  ni uu[ union V^ V^^union un uou inn union{A^union u5n union u union{An uninion nu[ union V^^union  un union nu[ union V^^union uu[ uon u union{A");
    parse(
        "i=<o%=<o<d\u000Bi=<o%=<o<d\u000B-i=>o\u000B-i=<o<o=<o%=<o<d\u000B-i=>o\u000B-i=<o<o<d\u000B<o-i=<o\u000Bi=>o\u000B-i=<o<o=<o%=<o<-i=<i=<o<o<\\\n"
        "<o-i=<o\u000B-i=<o<o<d\u000B<o<d-<d\u000B<->");
    parse("struct{\n"
          "{f   +H{U");
    parse("Y'\x0a\x0a");
    parse("(auto:");
    parse("\x8e.8..");
    parse(R"(('


I=')");
    parse("YYu{\x0a:");
    parse("(n{-((do(n");
    parse("+goto2n[0(sizeof(");
    parse("({(switch");
    parse("({if\"{");
    parse("\xd5.2f2f");
    parse("d{while");
    parse("*[( \x09(5ev");
    parse("Ao(int,{,[");
    parse("[1 U( \x091x");
    parse("v\x09(3.f.");
    parse("\x09(=[9l8lu");
    parse("enum {*(");
    parse("k{struct{{;");
    parse("it(\"{}%\"    ");
    parse("*{(enum{)=9");
    parse("(=({A{enum k{=(");
    parse("\"{}Hy\"([H{");
    parse("typedef(z;H");
    parse(R"(("""""""""\""""""""_=)");
    parse("(+cittsa=%=[([(typedef(( ( D;D[(");
    parse("( ({o[(void(o[(void([((c(([[((Tm(c(([[(enum[(T(,*(=_%(void(o[a[in{=");
    parse("id(o[(voi ([([(enum[(..,(void(+k{m++(({([(enum[(_id(+k{m++(({([(enum[(_%[(volatile([(e(vp==iivp==i");
    parse("(const(6(*foro([void(o,(void([) 6(*foro([void(o,(void([) [(void(_enum[(void%=l{[*(cona");
    parse(R"(f{u&elsaeifyur{u{y""y--y-Na(N[ "\\\\\\\\\\\\\\\\\\\\\\\\\\X\\\\\\\\\\\"yN[&ry ""&ry "volatilegis-)");
    parse("z{|if{iw,_if  (long(:U [o[(long(([o((else(long(:U [o[(lo(([o((long(((((_||f((");
    parse(
        "F>>=u{cc mase enum mase   q{enum mase en   q{enum mase enum ase enum mase   q{enum mase en  mase enum mau{+r++++++++se[   Kq{enum mase en   q{enmase en   u{case-.sh{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{->{{{{{{{A{ {{{{ort!s if .l{{{{{{{{{{{{{{{{((union{e((((((union\n"
        "\n"
        "\n"
        "{e((((((union\n"
        "\n"
        "\n"
        "                                                                                                                                                                                               ");
    parse("IN[\"*[\\* 8F*\n"
          "\"(] 4");
    // Causes stack overflow when using address sanitizer due to address sanitizer possibly using 3x as much stack space
    // according to documentations
    // Also causes __chckstk to throw on windows when compiling in debug mode
#ifdef UNRESTRICTED_STACK
    excludeFromAddressSanitizer();
#endif
    parse("V=V==L+E");
}

#undef treeProduces
#undef functionProduces

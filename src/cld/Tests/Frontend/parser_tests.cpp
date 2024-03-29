#include <catch.hpp>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>

#include "TestConfig.hpp"

#define treeProduces(source, matches)                         \
    do                                                        \
    {                                                         \
        std::string string;                                   \
        llvm::raw_string_ostream ss(string);                  \
        cld::PPSourceObject tokens;                           \
        auto options = cld::LanguageOptions::native();        \
        tokens = cld::Lexer::tokenize(source, &options, &ss); \
        REQUIRE(string.empty());                              \
        auto ctokens = cld::Lexer::toCTokens(tokens, &ss);    \
        REQUIRE(string.empty());                              \
        auto tree = cld::Parser::buildTree(ctokens, &ss);     \
        CHECK_THAT(string, matches);                          \
        cld::Parser::buildTree(ctokens);                      \
        if (!string.empty())                                  \
        {                                                     \
            llvm::errs() << '\n';                             \
        }                                                     \
    } while (0)

#define functionProduces(parser, source, matches)                                               \
    do                                                                                          \
    {                                                                                           \
        std::string string;                                                                     \
        llvm::raw_string_ostream ss(string);                                                    \
        cld::PPSourceObject tokens;                                                             \
        auto options = cld::LanguageOptions::native();                                          \
        tokens = cld::Lexer::tokenize(source, &options, &ss);                                   \
        REQUIRE(string.empty());                                                                \
        auto ctokens = cld::Lexer::toCTokens(tokens, &ss);                                      \
        REQUIRE(string.empty());                                                                \
        cld::Parser::Context context(ctokens, &ss);                                             \
        auto begin = std::as_const(ctokens).data().data();                                      \
        parser(begin, ctokens.data().data() + ctokens.data().size(), context);                  \
        {                                                                                       \
            UNSCOPED_INFO("Function " #parser " with source " << source);                       \
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
                 ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "'f'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
}

TEST_CASE("Parse external declaration", "[parser]")
{
    treeProduces("typedef int largeInt __attribute__ ((__mode__ (TI)));", ProducesNoErrors());
    treeProduces(
        "void *__attribute__((__cdecl__)) bsearch(const void *_Key,const void *_Base,long _NumOfElements,long _SizeOfElements,int (__attribute__((__cdecl__)) *_PtFuncCompare)(const void *,const void *));",
        ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int;", ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int i", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "i{}",
                     ProducesError(EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME_BEFORE_N, "'i'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f;{}", ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int i(void) {}", ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int () int f;{}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "')'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int i() int f{}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'{'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo(int,int[5]){}",
                     ProducesError(MISSING_PARAMETER_NAME) && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo = 5;", ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int foo = 5", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int[;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "';'")
                         && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseExternalDeclaration, "int[ =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "'='") && ProducesNote(TO_MATCH_N_HERE, "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'"));
    functionProduces(parseExternalDeclaration, "int foo,bar = 5;", ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int foo,bar;", ProducesNoErrors());
    functionProduces(parseExternalDeclaration, "int foo,[;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "';'")
                         && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseExternalDeclaration, "int foo,bar =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'") && ProducesNoNotes());
    functionProduces(parseExternalDeclaration, "int foo,[ =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "'='") && ProducesNote(TO_MATCH_N_HERE, "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'"));
    functionProduces(parseExternalDeclaration, "typedef int [;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "';'")
                         && ProducesNote(TO_MATCH_N_HERE, "'['"));
    treeProduces("typedef int i;void foo(i){}",
                 ProducesError(MISSING_PARAMETER_NAME) && ProducesNote(IDENTIFIER_IS_TYPEDEF, "'i'"));
    functionProduces(parseExternalDeclaration, "typedef int [;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "';'")
                         && ProducesNote(TO_MATCH_N_HERE, "'['"));
    treeProduces("\n"
                 "void barFunc(int,char);\n"
                 "\n"
                 "void (*foo(foo,i,bar))(int,char) short foo,i,bar;\n"
                 "{\n"
                 "    return barFunc;\n"
                 "}",
                 ProducesNoErrors());
    treeProduces("typedef int i;void foo(int i,i i)"
                 "{"
                 "}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N, "typename", "'i'")
                     && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "'i'"));
    treeProduces("typedef int i,bar;void foo(int i,bar i)"
                 "{"
                 "}",
                 ProducesNoErrors());
}

TEST_CASE("Parser typedef scoping and resolution", "[parser]")
{
    treeProduces("typedef int aa;void foo(){aa aa;}", ProducesNoErrors());
    treeProduces("typedef int aa;void foo(){aa aa;const aa;}", ProducesNoErrors());
    treeProduces("typedef int aa;"
                 "enum aa; aa r;",
                 ProducesNoErrors());
    treeProduces("typedef signed int t;t f(t (t));", ProducesNoErrors());
    treeProduces("typedef int aa;"
                 "enum {aa,};",
                 ProducesNoErrors());
    SECTION("Builtin typedef")
    {
        treeProduces("void foo(__builtin_va_list i);", ProducesNoErrors());
    }
}

TEST_CASE("Parse Declaration Specifiers", "[parser]")
{
    treeProduces("typedef int aa; typedef extern static auto register const restrict volatile inline void char short "
                 "int long float double signed unsigned f;",
                 ProducesNoErrors());
    functionProduces(parseDeclarationSpecifier, "struct i", ProducesNoErrors());
    functionProduces(parseDeclarationSpecifier, "union i", ProducesNoErrors());
    functionProduces(parseDeclarationSpecifier, "enum i", ProducesNoErrors());
}

TEST_CASE("Parse Specifier Qualifiers", "[parser]")
{
    functionProduces(
        parseSpecifierQualifierList,
        "const void long float double signed restrict volatile char short int long float double signed unsigned",
        ProducesNoErrors());
    functionProduces(parseSpecifierQualifier, "struct i", ProducesNoErrors());
    functionProduces(parseSpecifierQualifier, "union i", ProducesNoErrors());
    functionProduces(parseSpecifierQualifier, "enum i", ProducesNoErrors());
    treeProduces("typedef int i;void foo()"
                 "{"
                 "i i;struct { i; } r;"
                 "}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N, "typename", "'i'")
                     && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "'i'"));
}

TEST_CASE("Parse structs and unions", "[parser]")
{
    functionProduces(parseStructOrUnionSpecifier, "int",
                     ProducesError(EXPECTED_N_OR_N_INSTEAD_OF_N, "'struct'", "'union'", "'int'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct",
                     ProducesError(EXPECTED_N_OR_N_AFTER_N, "identifier", "'{'", "'struct'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "union",
                     ProducesError(EXPECTED_N_OR_N_AFTER_N, "identifier", "'{'", "'union'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "';'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i", ProducesNoErrors());
    functionProduces(parseStructOrUnionSpecifier, "union i", ProducesNoErrors());
    functionProduces(parseStructOrUnionSpecifier, "struct i {int foo, bar;int foobar;}", ProducesNoErrors());
    functionProduces(parseStructOrUnionSpecifier, "struct i{i;}",
                     ProducesError(EXPECTED_TYPENAME_BEFORE_N, "'i'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{}",
                     ProducesError(STRUCT_REQUIRES_AT_LEAST_ONE_FIELD) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "union i{}",
                     ProducesError(UNION_REQUIRES_AT_LEAST_ONE_FIELD) && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5;}", ProducesNoErrors());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int r:5;}", ProducesNoErrors());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'}'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int foo:;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'") && ProducesNoNotes());
    functionProduces(parseStructOrUnionSpecifier, "struct i{unsigned int:5",
                     ProducesError(EXPECTED_N, "';'") && ProducesError(EXPECTED_N, "'}'")
                         && ProducesNote(TO_MATCH_N_HERE, "'{'"));
}

TEST_CASE("Parse enums", "[parser]")
{
    functionProduces(parseEnumSpecifier, "enum",
                     ProducesError(EXPECTED_N_AFTER_N, "identifier", "'enum'") && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "';'") && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum i{i}", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum {}",
                     ProducesError(ENUM_REQUIRES_AT_LEAST_ONE_VALUE) && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum i{test}", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum i{test = 5}", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum {test,}", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum {test,ft}", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum {test,ft,}", ProducesNoErrors());
    functionProduces(parseEnumSpecifier, "enum i{test = 5",
                     ProducesError(EXPECTED_N, "'}'") && ProducesNote(TO_MATCH_N_HERE, "'{'"));
    functionProduces(parseEnumSpecifier, "enum i{test ft}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'ft'") && ProducesNoNotes());
    functionProduces(parseEnumSpecifier, "enum {test,,ft}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "','") && ProducesNoNotes());
}

TEST_CASE("Parse Declaration", "[parser]")
{
    treeProduces("int fooc(int[const 5]);", ProducesNoErrors());
    treeProduces("int f(int (*)(char *),double (*)[]);", ProducesNoErrors());
    functionProduces(parseDeclaration, "int foo;", ProducesNoErrors());
    functionProduces(parseDeclaration, "int;", ProducesNoErrors());
    functionProduces(parseDeclaration, "int foo,bar;", ProducesNoErrors());
    functionProduces(parseDeclaration, "int[ =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "'='") && ProducesNote(TO_MATCH_N_HERE, "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'"));
    functionProduces(parseDeclaration, "typedef int [;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "';'")
                         && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseDeclaration, "int foo =;",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'") && ProducesNoNotes());
    functionProduces(parseDeclaration, "int foo = 5;", ProducesNoErrors());
    functionProduces(parseDeclaration, "int foo = 5", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
    functionProduces(parseDeclaration, "typedef int foo;", ProducesNoErrors());
    treeProduces("typedef int aa;void foo(){aa aa;const aa bb;}",
                 ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'bb'")
                     && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "'aa'"));
    treeProduces("typedef int aa;void foo(){aa aa;const aa;}", ProducesNoErrors());
}

TEST_CASE("Parse Declarators and DirectDeclarators", "[parser]")
{
    functionProduces(parseDeclarator, "* const * volatile *i", ProducesNoErrors());
    functionProduces(parseDeclarator, "* const * volatile *(i",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseDeclarator, "* const * volatile *i(int f",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseDeclarator, "foo(int,int[5])", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo(int i,int f[5],)",
                     ProducesError(EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME_BEFORE_N, "')'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f e)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'e'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f e,r)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'e'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(i,f",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseDeclarator, "foo[", ProducesError(EXPECTED_N, "']'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseDeclarator, "*", ProducesError(EXPECTED_N, "'(' or identifier") && ProducesNoNotes());
    functionProduces(parseDeclarator, "(int)(int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "(int)()",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo(", ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseDeclarator, "foo()", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo(foo bar int)",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'bar'")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "'int'")
                         && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseDeclarator, "foo[static const volatile restrict 5]", ProducesNoErrors());
    functionProduces(parseDeclarator, "(int)[static 5]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "(int)[const static 5]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "(int)[*]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "(int)[5]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "(int)[]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or identifier", "'int'") && ProducesNoNotes());
    functionProduces(parseDeclarator, "foo[]", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo[const static 5]", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo[const *]", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo[const restrict volatile",
                     ProducesError(EXPECTED_N, "']'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseDeclarator, "foo[*]", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo[5]", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo[*[]",
                     ProducesError(EXPECTED_N, "']'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseDeclarator, "foo(int()",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseDeclarator, "foo(int(int))", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo(int([5]))", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo(int())", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo(int[*])", ProducesNoErrors());
    functionProduces(parseDeclarator, "foo(int (i)(int))", ProducesNoErrors());
    treeProduces("typedef int i;void foo(a,i){}", ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "typename")
                                                      && ProducesNote(IDENTIFIER_IS_TYPEDEF, "'i'"));
}

TEST_CASE("Parse parameter (type) list", "[parser]")
{
    functionProduces(parseParameterTypeList, "int,...", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int,int", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int const volatile restrict *", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int const volatile restrict *[]", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int const volatile restrict *foo", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(((foo)))", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(*(*(*foo)))", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int const volatile restrict *(*(*(*)))", ProducesNoErrors());
    functionProduces(parseParameterTypeList, "int,",
                     ProducesError(EXPECTED_N, "storage specifier or typename") && ProducesNoNotes());
}

TEST_CASE("Parse Abstract Declarator and Direct Abstract Declarator", "[parser]")
{
    functionProduces(parseAbstractDeclarator, "* const * volatile *", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "* const * volatile *(",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseAbstractDeclarator, "* const * volatile *(int f",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseAbstractDeclarator, "(int,int[5])", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "(int i,int f[5],)",
                     ProducesError(EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME_BEFORE_N, "')'") && ProducesNoNotes());
    functionProduces(parseAbstractDeclarator, "[",
                     ProducesError(EXPECTED_N, "']'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseAbstractDeclarator, "*", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "(",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseAbstractDeclarator, "()", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "[]", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "[*]", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "[5]", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "[*[]",
                     ProducesError(EXPECTED_N, "']'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
    functionProduces(parseAbstractDeclarator, "(int()",
                     ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
    functionProduces(parseAbstractDeclarator, "(int(int))", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "(int([5]))", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "(int())", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "(int[*])", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "(int (i)(int))", ProducesNoErrors());
    functionProduces(parseAbstractDeclarator, "]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "'(' or '['", "']'") && ProducesNoNotes());
}

TEST_CASE("Parse Statements", "[parser]")
{
    SECTION("Return")
    {
        functionProduces(parseStatement, "return;", ProducesNoErrors());
        functionProduces(parseStatement, "return 5;", ProducesNoErrors());
        functionProduces(parseStatement, "return", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "return 5", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "return 5", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        functionProduces(parseReturnStatement, "5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'return'", "'5'") && ProducesNoNotes());
    }
    SECTION("if")
    {
        functionProduces(parseStatement, "if 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'('", "'5'") && ProducesNoNotes());
        functionProduces(parseStatement, "if(5;int i",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "';'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseStatement, "if(5);else;", ProducesNoErrors());
        functionProduces(parseStatement, "if(5)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
        functionProduces(parseStatement, "if(5)case:;else;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
        functionProduces(parseIfStatement, "(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'if'", "'('") && ProducesNoNotes());
    }
    SECTION("switch")
    {
        functionProduces(parseStatement, "switch(5);", ProducesNoErrors());
        functionProduces(parseStatement, "switch 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'('", "'5'") && ProducesNoNotes());
        functionProduces(parseStatement, "switch(5;int i",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "';'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseStatement, "switch(5)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
        functionProduces(parseSwitchStatement, "(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'switch'", "'('") && ProducesNoNotes());
    }
    SECTION("for")
    {
        functionProduces(parseStatement, "for int i = 0; i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'('", "'int'") && ProducesNoNotes());
        functionProduces(parseStatement, "for i = 0; i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'('", "'i'") && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0 i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'i'") && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0 i < 5; i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'i'") && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0; i < 5 i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'i'") && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0 i < 5 i++);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'i'") && ProducesNoNotes());
        functionProduces(parseStatement, "for(int i = 0;i < 5;i++;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "';'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseStatement, "for(",
                         ProducesError(EXPECTED_N, "expression or declaration") && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0;", ProducesError(EXPECTED_N, "expression") && ProducesNoNotes());
        functionProduces(parseStatement, "for(i = 0;i < 0;",
                         ProducesError(EXPECTED_N, "expression") && ProducesNoNotes());
        functionProduces(parseForStatement, "(;;);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'for'", "'('") && ProducesNoNotes());
        functionProduces(parseStatement, "for(;;)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
    }
    SECTION("Head while")
    {
        functionProduces(parseStatement, "while(5);", ProducesNoErrors());
        functionProduces(parseStatement, "while 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'('", "'5'") && ProducesNoNotes());
        functionProduces(parseStatement, "while(5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "';'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseHeadWhileStatement, "(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'while'", "'('") && ProducesNoNotes());
        functionProduces(parseStatement, "while(5)case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
    }
    SECTION("Foot while")
    {
        functionProduces(parseStatement, "do;while(5);", ProducesNoErrors());
        functionProduces(parseStatement, "do;while(5)", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "do;while 5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'('", "'5'") && ProducesNoNotes());
        functionProduces(parseStatement, "do;while(5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "';'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(
            parseStatement, "do;(5;",
            ProducesError(EXPECTED_N_INSTEAD_OF_N, "'while'", "'('") && ProducesNote(TO_MATCH_N_HERE, "'do'")
                && ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "';'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseStatement, "do case:;while(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
        functionProduces(parseFootWhileStatement, ";while(5);",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'do'", "';'") && ProducesNoNotes());
    }
    SECTION("break and continue")
    {
        functionProduces(parseStatement, "break;", ProducesNoErrors());
        functionProduces(parseStatement, "continue;", ProducesNoErrors());
        functionProduces(parseStatement, "break", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "continue", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
    }
    SECTION("Default and case")
    {
        functionProduces(parseStatement, "default:;", ProducesNoErrors());
        functionProduces(parseStatement, "default;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "':'", "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "default:case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
        functionProduces(parseStatement, "case 5:;", ProducesNoErrors());
        functionProduces(parseStatement, "case 5;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "':'", "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "case 5:case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
    }
    SECTION("Label and goto")
    {
        functionProduces(parseStatement, "test:;", ProducesNoErrors());
        functionProduces(parseStatement, "test:case:;",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "':'")
                             && ProducesNoNotes());
        functionProduces(parseStatement, "goto test;", ProducesNoErrors());
        functionProduces(parseStatement, "goto test", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        functionProduces(parseStatement, "{test:;goto test;}", ProducesNoErrors());
        functionProduces(parseStatement, "{typedef int test;test:;goto test;}", ProducesNoErrors());
        functionProduces(parseStatement, "{test:;goto 0x3;}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'0x3'") && ProducesNoNotes());
    }
    SECTION("Expression Statement")
    {
        functionProduces(parseStatement, "test;", ProducesNoErrors());
        functionProduces(parseStatement, "test", ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
        treeProduces("typedef int aa;void foo(){aa aa;aa bb;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'bb'")
                         && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "'aa'"));
        treeProduces("typedef int aa;int foo(int aa){aa i;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "';'", "'i'")
                         && ProducesNote(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "'aa'"));
    }
    SECTION("Compound statement")
    {
        functionProduces(parseStatement, "{}", ProducesNoErrors());
        functionProduces(parseStatement, "{", ProducesError(EXPECTED_N, "'}'") && ProducesNote(TO_MATCH_N_HERE, "'{'"));
        functionProduces(parseCompoundStatement, "}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'{'", "'}'") && ProducesNoNotes());
        functionProduces(parseStatement, "{case:;}",
                         ProducesError(EXPECTED_LITERAL_N_OR_N_INSTEAD_OF_N, "identifier", "'('", "':'")
                             && ProducesNoNotes());
    }
}

TEST_CASE("Parse Initializer and Initializer List", "[parser]")
{
    functionProduces(parseInitializer, "5", ProducesNoErrors());
    functionProduces(parseInitializer, "{5}", ProducesNoErrors());
    functionProduces(parseInitializer, "{5,}", ProducesNoErrors());
    functionProduces(parseInitializer, "{5,3}", ProducesNoErrors());
    functionProduces(parseInitializer, "{[5].m = 5}", ProducesNoErrors());
    functionProduces(parseInitializer, "]",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'") && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,", ProducesError(EXPECTED_N, "'}'") && ProducesNoNotes());
    functionProduces(parseInitializer, "{5 3}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'3'") && ProducesNoNotes());
    functionProduces(parseInitializer, "{5,[3}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "'}'") && ProducesNote(TO_MATCH_N_HERE, "'['")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "'='", "'}'")
                         && ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "'}'"));
    functionProduces(parseInitializer, "{[5]. = 5}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'='") && ProducesNoNotes());
}

TEST_CASE("Parse Expressions", "[parser]")
{
    SECTION("Primary expressions")
    {
        functionProduces(parsePostFixExpression, "0", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "wdawd", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "\"wdawd\"", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "((((5))))", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "((((]))]))",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "']'")
                             && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parsePostFixExpression, "((((]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        treeProduces("void foo(){i;\"string\"\"can also be concatenated\";34234;}", ProducesNoErrors());
        treeProduces("void foo(){int i =;}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "';'") && ProducesNoNotes());
        treeProduces("int i =", ProducesError(EXPECTED_N, "literal, identifier or '('")
                                    && ProducesError(EXPECTED_N, "';'") && ProducesNoNotes());
    }
    SECTION("Postfix expressions")
    {
        functionProduces(parsePostFixExpression, "i(53,42,32)", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "i(53 42,32,53 43",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'42'")
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'43'")
                             && ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parsePostFixExpression, "i(53,42,32]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "']'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parsePostFixExpression, "i(53,42,32,[5]",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesError(EXPECTED_N, "')'")
                             && ProducesNote(TO_MATCH_N_HERE, "'('"));

        functionProduces(parsePostFixExpression, "(int){5}", ProducesNoErrors());
        treeProduces("void foo(){(int[{5};}",
                     ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "'{'") && ProducesNote(TO_MATCH_N_HERE, "'['")
                         && ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parsePostFixExpression, "(int){5 8}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "','", "'8'") && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int)5}",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'{'", "'5'") && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int)5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "'{'", "'5'") && ProducesError(EXPECTED_N, "'}'")
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "(int){5,}", ProducesNoErrors());

        functionProduces(parsePostFixExpression, "i[5",
                         ProducesError(EXPECTED_N, "']'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
        functionProduces(parsePostFixExpression, "i[]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i[5]", ProducesNoErrors());

        functionProduces(parsePostFixExpression, "i++", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "]++",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "]()",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i--", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "]--",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i.m", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "].m",
                         ProducesError(EXPECTED_LITERAL_N_OR_N_INSTEAD_OF_N, "identifier", "'('", "']'")
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.", ProducesError(EXPECTED_N, "identifier") && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i.[]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'['")
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());

        functionProduces(parsePostFixExpression, "i->m", ProducesNoErrors());
        functionProduces(parsePostFixExpression, "]->m",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i->", ProducesError(EXPECTED_N, "identifier") && ProducesNoNotes());
        functionProduces(parsePostFixExpression, "i->[]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'['")
                             && ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());
    }
    SECTION("Unary expressions")
    {
        functionProduces(parseUnaryExpression, "sizeof(int)", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "sizeof(5)", ProducesNoErrors());

        functionProduces(parseUnaryExpression, "sizeof(int",
                         ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseUnaryExpression, "sizeof(5",
                         ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));

        functionProduces(parseUnaryExpression, "sizeof sizeof(int)", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "sizeof ]",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "']'")
                             && ProducesNoNotes());
        functionProduces(parseUnaryExpression, "sizeof(int*[)",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "']'", "')'") && ProducesNote(TO_MATCH_N_HERE, "'['"));
        functionProduces(parseUnaryExpression, "+(int)",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());

        functionProduces(parseUnaryExpression, "++i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "--i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "&i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "*i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "+(int)i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "+i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "-i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "!i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "~i", ProducesNoErrors());
        functionProduces(parseUnaryExpression, "__builtin_va_arg(f,int)", ProducesNoErrors());
    }
    SECTION("Type name")
    {
        functionProduces(parseTypeName, "int", ProducesNoErrors());
        functionProduces(parseTypeName, "int[5]", ProducesNoErrors());
        functionProduces(parseTypeName, "int(*)(int,char)", ProducesNoErrors());
    }
    SECTION("Cast expression")
    {
        functionProduces(parseCastExpression, "(int)5", ProducesNoErrors());
        functionProduces(parseCastExpression, "(int)",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
    SECTION("Term")
    {
        functionProduces(parseTerm, "5 * 5", ProducesNoErrors());
        functionProduces(parseTerm, "5 / 5", ProducesNoErrors());
        functionProduces(parseTerm, "5 % 5", ProducesNoErrors());

        functionProduces(parseTerm, "5 * / 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseTerm, "5 * % 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseTerm, "5 * ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseTerm, " % / 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
    SECTION("Additive")
    {
        functionProduces(parseAdditiveExpression, "5 + 5", ProducesNoErrors());
        functionProduces(parseAdditiveExpression, "5 - 5", ProducesNoErrors());

        functionProduces(parseAdditiveExpression, "5 + () + 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseAdditiveExpression, "() + 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());

        functionProduces(parseAdditiveExpression, "5 + - -5", ProducesNoErrors());
        functionProduces(parseAdditiveExpression, " + + - - 5", ProducesNoErrors());
    }
    SECTION("Shift")
    {
        functionProduces(parseShiftExpression, "5 << 5", ProducesNoErrors());
        functionProduces(parseShiftExpression, "5 >> 5", ProducesNoErrors());

        functionProduces(parseShiftExpression, "5 << >> 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseShiftExpression, "5 << ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseShiftExpression, "5 << << 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseShiftExpression, " << >> 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "'<<'")
                             && ProducesNoNotes());
    }
    SECTION("Relational")
    {
        functionProduces(parseRelationalExpression, "5 < 5", ProducesNoErrors());
        functionProduces(parseRelationalExpression, "5 > 5", ProducesNoErrors());
        functionProduces(parseRelationalExpression, "5 <= 5", ProducesNoErrors());
        functionProduces(parseRelationalExpression, "5 >= 5", ProducesNoErrors());

        functionProduces(parseRelationalExpression, "5 < > 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseRelationalExpression, "5 < ) > 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseRelationalExpression, " <= >= 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
    SECTION("Equality")
    {
        functionProduces(parseEqualityExpression, "5 == 5", ProducesNoErrors());
        functionProduces(parseEqualityExpression, "5 != 5", ProducesNoErrors());

        functionProduces(parseEqualityExpression, "5 == != 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseEqualityExpression, "5 == ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseEqualityExpression, " == != 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
    SECTION("Bitand")
    {
        functionProduces(parseBitAndExpression, "5 & ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & ]",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & () & 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "() & 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitAndExpression, "5 & 5", ProducesNoErrors());
        functionProduces(parseBitAndExpression, "5 & &5", ProducesNoErrors());
        functionProduces(parseBitAndExpression, "5 & & 5", ProducesNoErrors());
        functionProduces(parseBitAndExpression, " & & 5", ProducesNoErrors());
    }
    SECTION("BitXor")
    {
        functionProduces(parseBitXorExpression, "5 ^ 5", ProducesNoErrors());

        functionProduces(parseBitXorExpression, "5 ^ ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());

        functionProduces(parseBitXorExpression, "5 ^ ^ 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitXorExpression, "^ ^ 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
    SECTION("BitOr")
    {
        functionProduces(parseBitOrExpression, "5 | 5", ProducesNoErrors());

        functionProduces(parseBitOrExpression, "5 | | 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitOrExpression, "5 | ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseBitOrExpression, " | | 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
    SECTION("LogicalAnd")
    {
        functionProduces(parseLogicalAndExpression, "5 && 5", ProducesNoErrors());

        functionProduces(parseLogicalAndExpression, "5 && && 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseLogicalAndExpression, "5 && ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseLogicalAndExpression, " && && 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "'&&'")
                             && ProducesNoNotes());
    }
    SECTION("LogicalOr")
    {
        functionProduces(parseLogicalOrExpression, "5 || 5", ProducesNoErrors());

        functionProduces(parseLogicalOrExpression, "5 || || 5",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseLogicalOrExpression, "5 || ()",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseLogicalOrExpression, " || || 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "literal, identifier or '('", "'||'")
                             && ProducesNoNotes());
    }
    SECTION("Conditional")
    {
        functionProduces(parseConditionalExpression, "5 ? 5 : 5", ProducesNoErrors());
        functionProduces(parseConditionalExpression, "5 ? (5 : 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "')'", "':'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        functionProduces(parseConditionalExpression, "5 ? 5  5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "':'", "'5'") && ProducesNote(TO_MATCH_N_HERE, "'?'"));
        functionProduces(parseConditionalExpression, "5 ? 5 ? 5 : 5 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "':'", "'5'") && ProducesNote(TO_MATCH_N_HERE, "'?'"));
        functionProduces(parseConditionalExpression, "5 ? 5 : 5 ? 5",
                         ProducesError(EXPECTED_N, "':'") && ProducesNote(TO_MATCH_N_HERE, "'?'")
                             && ProducesError(EXPECTED_N, "literal, identifier or '('"));
    }
    SECTION("Assignment")
    {
        functionProduces(parseAssignmentExpression, "5 = 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "] = ]",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseAssignmentExpression, "5 += 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 -= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 /= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 *= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 %= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 <<= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 >>= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 &= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 |= 5", ProducesNoErrors());
        functionProduces(parseAssignmentExpression, "5 ^= 5", ProducesNoErrors());
    }
    SECTION("Expressions")
    {
        functionProduces(parseExpression, "5,5", ProducesNoErrors());
        functionProduces(parseExpression, "5 +,5 +",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
        functionProduces(parseExpression, "5,",
                         ProducesError(EXPECTED_N, "literal, identifier or '('") && ProducesNoNotes());
    }
}

TEST_CASE("Parse ASM Statement", "[parser]")
{
    treeProduces("void foo() {"
                 "__asm__ __volatile__ (\"rep stos{\" \"q|q\" \"}\"\n"
                 "      : \"+D\" (Dest), \"+c\" (Count) \n"
                 "      : [Data] \"a\" (Data) \n"
                 "      : \"memory\");\n"
                 "}",
                 ProducesNoErrors());
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

TEST_CASE("Parser limits", "[parser]")
{
#if defined(UNRESTRICTED_STACK) && defined(LLVM_ENABLE_EXCEPTIONS)
    SECTION("Parenthese expression")
    {
        auto source = "int main(void){" + std::string(cld::Limits::Parser::MAX_BRACKET_DEPTH + 1, '(');
        treeProduces(source, ProducesError(cld::Errors::Parser::MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED,
                                           cld::Limits::Parser::MAX_BRACKET_DEPTH));
    }
    SECTION("Direct Declarator")
    {
        auto source = "int" + std::string(cld::Limits::Parser::MAX_BRACKET_DEPTH + 1, '(');
        treeProduces(source, ProducesError(cld::Errors::Parser::MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED,
                                           cld::Limits::Parser::MAX_BRACKET_DEPTH));
    }
    SECTION("Compound statement")
    {
        auto source = "int main(void)" + std::string(cld::Limits::Parser::MAX_BRACKET_DEPTH + 1, '{');
        treeProduces(source, ProducesError(cld::Errors::Parser::MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED,
                                           cld::Limits::Parser::MAX_BRACKET_DEPTH));
    }

#endif
}

namespace
{
void parse(const std::string& source)
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    bool errorsOccured = false;
    auto options = cld::LanguageOptions::native();
    auto tokens = cld::Lexer::tokenize(std::move(source), &options, &ss, &errorsOccured);
    if (!ss.str().empty() || tokens.data().empty() || errorsOccured)
    {
        return;
    }
    auto ctokens = cld::Lexer::toCTokens(tokens, &ss);
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
    parse("s \n"
          "WWWW    S((S((((((long(foz\n"
          "[");
    parse(" |s __extension__    ");
    parse("__attribute~_%= (");
    parse("  ...L __attribute__");
    parse("__attribute__;*=e");
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
#if defined(UNRESTRICTED_STACK) && defined(LLVM_ENABLE_EXCEPTIONS)
    excludeFromAddressSanitizer();
#endif
    parse("V=V==L+E");
    parse("?I5\"e     \tn\"G");
}

#undef treeProduces
#undef functionProduces

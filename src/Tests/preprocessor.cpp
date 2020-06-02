#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Text.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include "TestConfig.hpp"

#define preprocessResult(source)                                                                       \
    [](const std::string& str) {                                                                       \
        std::string storage;                                                                           \
        llvm::raw_string_ostream ss(storage);                                                          \
        auto tokens = cld::Lexer::tokenize(str, cld::LanguageOptions::native(), &ss);                  \
        INFO(ss.str());                                                                                \
        REQUIRE(ss.str().empty());                                                                     \
        auto ret = cld::PP::preprocess(std::move(tokens), &ss);                                        \
        INFO(ss.str());                                                                                \
        REQUIRE(ss.str().empty());                                                                     \
        return cld::Lexer::constructPP(ret, ret.data().data(), ret.data().data() + ret.data().size()); \
    }(source)

#define preprocessTest(source)                                                        \
    [](const std::string& str) {                                                      \
        std::string storage;                                                          \
        llvm::raw_string_ostream ss(storage);                                         \
        auto tokens = cld::Lexer::tokenize(str, cld::LanguageOptions::native(), &ss); \
        INFO(ss.str());                                                               \
        REQUIRE(ss.str().empty());                                                    \
        auto ret = cld::PP::preprocess(std::move(tokens), &ss);                       \
        return ret;                                                                   \
    }(source)

#define PP_OUTPUTS_WITH(input, match)                                                   \
    do                                                                                  \
    {                                                                                   \
        std::string s;                                                                  \
        llvm::raw_string_ostream ss(s);                                                 \
        auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &ss); \
        INFO(ss.str());                                                                 \
        REQUIRE(ss.str().empty());                                                      \
        cld::PP::preprocess(std::move(tokens), &ss);                                    \
        CHECK_THAT(s, match);                                                           \
        if (!s.empty())                                                                 \
        {                                                                               \
            tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &ss);  \
            cld::PP::preprocess(std::move(tokens));                                     \
        }                                                                               \
    } while (0)

using namespace cld::Errors::PP;
using namespace cld::Warnings::PP;
using namespace cld::Notes;

TEST_CASE("C99 Standard examples", "[PP]")
{
    SECTION("6.10.3.5 'Scope of macro definitions'")
    {
        SECTION("Example 1")
        {
            auto ret = preprocessResult("#define TABSIZE 100\nint table[TABSIZE];");
            CHECK(ret == "\nint table[100];");
        }
        SECTION("Example 2")
        {
            auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))");
            CHECK(ret.empty());
        }
        SECTION("Example 3")
        {
            SECTION("Partial")
            {
                auto ret = preprocessResult("#define x 3\n"
                                            "#define f(a) f(x * (a))\n"
                                            "#undef x\n"
                                            "#define x 2\n"
                                            "#define g f\n"
                                            "#define z z[0]\n"
                                            "#define t(a) a\n"
                                            "% t(t(g)(0) + t)(1);");
                //% t(t(g)(0) + t)(1);  Finds macro t
                //% t(f(0) + t)(1);     After macro substitution of the arguments
                //% f(0) + t(1);        After argument substitution
                //% f(2 * (0)) + t(1);  After rescanning
                CHECK(ret == "\n\n\n\n\n\n\n% f(2 * (0)) + t(1);");
            }
            return;
            SECTION("Complete")
            {
                auto ret = preprocessResult("#define x 3\n"
                                            "#define f(a) f(x * (a))\n"
                                            "#undef x\n"
                                            "#define x 2\n"
                                            "#define g f\n"
                                            "#define z z[0]\n"
                                            "#define h g(~\n"
                                            "#define m(a) a(w)\n"
                                            "#define w 0,1\n"
                                            "#define t(a) a\n"
                                            "#define p() int\n"
                                            "#define q(x) x\n"
                                            "#define r(x,y) x ## y\n"
                                            "#define str(x) # x\n"
                                            "f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);\n"
                                            "g(x+(3,4)-w) | h 5) & m\n"
                                            "(f)^m(m);\n"
                                            "p() i[q()] = { q(1), r(2,3), r(4,), r(,5), r(,) };\n"
                                            "char c[2][6] = { str(hello), str() };");
                CHECK(ret
                      == "\n\n\n\n\n\n\n\n\n\n\n\n\n\nf(2* (y+1))+ f(2* (f(2* (z[0])))) % f(2 * (0)) + t(1);\n"
                         "f(2 * (2+(3,4)-0,1)) | f(2 * (~ 5)) & f(2 * (0,1))^m(0,1);\n"
                         "int i[] = { 1, 23, 4, 5, };\n"
                         "char c[2][6] = { \"hello\", \"\" };");
            }
        }
        return;
        SECTION("Example 5")
        {
            auto ret = preprocessResult("#define t(x,y,z) x ## y ## z\n"
                                        "int j[] = { t(1,2,3), t(,4,5), t(6,,7), t(8,9,),\n"
                                        "t(10,,), t(,11,), t(,,12), t(,,) };");
            CHECK(ret
                  == "\nint j[] = { 123, 45, 67, 89,\n"
                     "10, 11, 12, };");
        }
        SECTION("Example 7")
        {
            auto ret = preprocessResult("#define debug(...) fprintf(stderr, __VA_ARGS__)\n"
                                        "#define showlist(...) puts(#__VA_ARGS__)\n"
                                        "#define report(test, ...) ((test)?puts(#test):\\\n"
                                        "printf(__VA_ARGS__))\n"
                                        "debug(\"Flag\");\n"
                                        "debug(\"X = %d\\n\", x);\n"
                                        "showlist(The first, second, and third items.);\n"
                                        "report(x>y, \"x is %d but y is %d\", x, y);");
            CHECK(ret
                  == "\n\n\nfprintf(stderr, \"Flag\" );\n"
                     "fprintf(stderr, \"X = %d\\n\", x );\n"
                     "puts( \"The first, second, and third items.\" );\n"
                     "((x>y)?puts(\"x>y\"):\n"
                     "printf(\"x is %d but y is %d\", x, y));");
        }
    }
}

TEST_CASE("Text line", "[PP]")
{
    SECTION("Backslash Newline")
    {
        auto ret = preprocessResult(R"(Multi\
line output!
Normal line)");
        CHECK_THAT(ret, ProducesLines("Multiline output!\nNormal line"));
    }
    SECTION("Trigraphs Newline")
    {
        auto ret = preprocessResult("struct ?\?< ?\?> a;");
        CHECK_THAT(ret, ProducesLines("struct { } a;"));
    }
}

TEST_CASE("Macros", "[PP]")
{
    SECTION("6.10.3.2 Duplicates")
    {
        PP_OUTPUTS_WITH("#define macroName\n#define macroName",
                        ProducesWarning(N_REDEFINED.args("'macroName'")) && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName()\n#define macroName()",
                        ProducesWarning(N_REDEFINED.args("'macroName'")) && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName(a)\n#define macroName(a)",
                        ProducesWarning(N_REDEFINED.args("'macroName'")) && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName(...)\n#define macroName(...)",
                        ProducesWarning(N_REDEFINED.args("'macroName'")) && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName ad\n#define macroName ad",
                        ProducesWarning(N_REDEFINED.args("'macroName'")) && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName ad\n#define macroName a",
                        ProducesError(REDEFINITION_OF_MACRO_N.args("'macroName'"))
                            && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName()\n#define macroName",
                        ProducesError(REDEFINITION_OF_MACRO_N.args("'macroName'"))
                            && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName(...)\n#define macroName()",
                        ProducesError(REDEFINITION_OF_MACRO_N.args("'macroName'"))
                            && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    }
    SECTION("6.10.3.5 __VA_ARGS__ not allowed")
    {
        PP_OUTPUTS_WITH("#define macroName __VA_ARGS__", ProducesError(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST));
        PP_OUTPUTS_WITH("#define macroName() __VA_ARGS__", ProducesError(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST));
        PP_OUTPUTS_WITH("#define macroName(a) __VA_ARGS__", ProducesError(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST));
        PP_OUTPUTS_WITH("#define macroName(...) __VA_ARGS__", ProducesNothing());
        PP_OUTPUTS_WITH("#define macroName(a,...) __VA_ARGS__", ProducesNothing());
    }
    SECTION("6.10.8.4 Defining builtin macros")
    {
        constexpr std::array PREDEFINED_MACRO_NAMES = {"__DATE__",          "__FILE__",
                                                       "__LINE__",          "__STDC__",
                                                       "__STDC_HOSTED__",   "__STDC_MB_MIGHT_NEQ_WC__",
                                                       "__STDC_VERSION__",  "__TIME__",
                                                       "__STC_IEC_559__",   "__STDC_IEC_559_COMPLEX__",
                                                       "__STDC_ISO_10646__"};
        for (std::string_view iter : PREDEFINED_MACRO_NAMES)
        {
            DYNAMIC_SECTION(iter)
            {
                PP_OUTPUTS_WITH(
                    "#define " + cld::to_string(iter),
                    ProducesError(DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + cld::to_string(iter) + '\'')));
            }
        }
    }
    SECTION("6.10.8.4 Undefining builtin macros")
    {
        constexpr std::array PREDEFINED_MACRO_NAMES = {"__DATE__",          "__FILE__",
                                                       "__LINE__",          "__STDC__",
                                                       "__STDC_HOSTED__",   "__STDC_MB_MIGHT_NEQ_WC__",
                                                       "__STDC_VERSION__",  "__TIME__",
                                                       "__STC_IEC_559__",   "__STDC_IEC_559_COMPLEX__",
                                                       "__STDC_ISO_10646__"};
        for (std::string_view iter : PREDEFINED_MACRO_NAMES)
        {
            DYNAMIC_SECTION(iter)
            {
                PP_OUTPUTS_WITH(
                    "#undef " + cld::to_string(iter),
                    ProducesError(UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + cld::to_string(iter) + '\'')));
            }
        }
    }
    PP_OUTPUTS_WITH("#define defined", ProducesError(DEFINED_CANNOT_BE_USED_AS_MACRO_NAME));
}

TEST_CASE("Object like Macros", "[PP]")
{
    SECTION("Normal")
    {
        auto ret = preprocessResult("#define FUNC (1 + 3)\nint main(void) {\n    return FUNC;\n}\n");
        CHECK_THAT(ret, ProducesLines("\nint main(void) {\n    return (1 + 3);\n}"));
        auto tokens = preprocessTest("#define FUNC (1 + 3)\nint main(void) {\n    return FUNC;\n}\n");
        REQUIRE(tokens.data().size() == 14);
        CHECK(std::all_of(tokens.data().begin(), tokens.data().begin() + 7,
                          [](auto&& token) { return !token.macroInserted(); }));
        CHECK(std::all_of(tokens.data().begin() + 7, tokens.data().begin() + 12,
                          [](auto&& token) { return token.getMacroId() == cld::Lexer::MacroID(1); }));
        CHECK(std::all_of(tokens.data().begin() + 13, tokens.data().end(),
                          [](auto&& token) { return !token.macroInserted(); }));
        REQUIRE(tokens.getSubstitutions().size() == 1);
        CHECK(tokens.getSubstitutions()[0].macroIdentifier.getOffset() == 8);
        CHECK(tokens.getSubstitutions()[0].macroIdentifier.getLength() == 4);
    }
    SECTION("Empty")
    {
        auto ret = preprocessResult("#define TABSIZE\nint table[TABSIZE];");
        CHECK_THAT(ret, ProducesLines("\nint table[];"));
    }
    SECTION("Nested")
    {
        auto ret = preprocessResult(
            R"(#define FUNC (1 + 3)
#define NESTED FUNC * FUNC
int main(void) {
    return NESTED;
})");
        CHECK_THAT(ret, ProducesLines("\n\nint main(void) {\n    return (1 + 3) * (1 + 3);\n}"));
        auto tokens = preprocessTest(R"(#define FUNC (1 + 3)
#define NESTED FUNC * FUNC
int main(void) {
    return NESTED;
})");
        REQUIRE(tokens.data().size() == 20);
        CHECK(std::all_of(tokens.data().begin(), tokens.data().begin() + 7,
                          [](auto&& token) { return !token.macroInserted(); }));
        CHECK(std::all_of(tokens.data().begin() + 7, tokens.data().begin() + 12,
                          [](auto&& token) { return token.getMacroId() == cld::Lexer::MacroID(2); }));
        CHECK(std::all_of(tokens.data().begin() + 12, tokens.data().begin() + 13,
                          [](auto&& token) { return token.getMacroId() == cld::Lexer::MacroID(1); }));
        CHECK(std::all_of(tokens.data().begin() + 13, tokens.data().begin() + 18,
                          [](auto&& token) { return token.getMacroId() == cld::Lexer::MacroID(3); }));
        CHECK(std::all_of(tokens.data().begin() + 18, tokens.data().end(),
                          [](auto&& token) { return !token.macroInserted(); }));
    }
    SECTION("At beginning of line")
    {
        auto ret = preprocessResult("#define INT int\nINT table[100];");
        CHECK_THAT(ret, ProducesLines("\nint table[100];"));
    }
    SECTION("Succeeding macros")
    {
        auto ret = preprocessResult("#define LONG long\nLONG LONG table[100];");
        CHECK_THAT(ret, ProducesLines("\nlong long table[100];"));
    }
    SECTION("Yielding __LINE__")
    {
        auto ret = preprocessResult("#define VALUE __LINE__\nlong table[VALUE];");
        CHECK_THAT(ret, ProducesLines("\nlong table[2];"));
    }
    SECTION("Recursive")
    {
        auto ret = preprocessResult(
            R"(#define FUNC (1 + 3)
#define NESTED NESTED * FUNC
int main(void) {
    return NESTED;
})");
        CHECK_THAT(ret, ProducesLines(R"(

int main(void) {
    return NESTED * (1 + 3);
})"));
    }
    SECTION("Following trigraphs")
    {
        auto ret = preprocessResult("#define TABSIZE\nint table?\?(TABSIZE?\?);");
        CHECK_THAT(ret, ProducesLines("\nint table[];"));
        ret = preprocessResult("#define TABSIZE 100\nint table?\?(TABSIZE?\?);");
        CHECK_THAT(ret, ProducesLines("\nint table[100];"));
    }
    SECTION("Rescanning behaviour")
    {
        auto ret = preprocessResult("#define A B\n"
                                    "#define B(a) a\n"
                                    "A\n"
                                    "(0)");
        CHECK_THAT(ret, ProducesLines("\n\n0"));
    }
}

TEST_CASE("Builtin macros", "[PP]")
{
    SECTION("Date")
    {
        auto ret = preprocessResult("__DATE__");
        // Make sure to change regex in the year 10000
        CHECK_THAT(ret, Catch::Matchers::Matches("\"\\w+ \\d{1,2} \\d{4}\""));
    }
    SECTION("Time")
    {
        auto ret = preprocessResult("__TIME__");
        CHECK_THAT(ret, Catch::Matchers::Matches("\"\\d{1,2}:\\d{1,2}:\\d{1,2}\""));
    }
    SECTION("__STDC__")
    {
        auto ret = preprocessResult("__STDC__");
        CHECK(ret == "1");
    }
    SECTION("__STDC_HOSTED__")
    {
        auto ret = preprocessResult("__STDC_HOSTED__");
        CHECK(ret == "0");
    }
    SECTION("__STDC_MB_MIGHT_NEQ_WC__")
    {
        auto ret = preprocessResult("__STDC_MB_MIGHT_NEQ_WC__");
        CHECK(ret == "1");
    }
    SECTION("__STDC_VERSION__")
    {
        auto ret = preprocessResult("__STDC_VERSION__");
        CHECK(ret == "199901L");
    }
    SECTION("__FILE__")
    {
        auto ret = preprocessResult("__FILE__");
        CHECK(ret == "\"<stdin>\"");
    }
    SECTION("__LINE__")
    {
        auto ret = preprocessResult("__LINE__\n\n__LINE__");
        CHECK(ret == "1\n\n3");
    }
}

TEST_CASE("Function like Macros", "[PP]")
{
    SECTION("Normal")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(5,7);");
        CHECK(ret == "\nint i = ((5) > (7) ? (5) : (7));");
    }
    SECTION("Argument count")
    {
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(5);",
                        ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args("\"max\"", 2, 1)));
        PP_OUTPUTS_WITH(
            "#define max(a, b,...) ((a) > (b) ? (a) : (b))\n"
            "int i = max(5);",
            ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N.args("\"max\"", 2, 1)));
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(5,7,10);",
                        ProducesError(TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args("\"max\"", 2, 3)));
    }
    SECTION("No closing )")
    {
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(5",
                        ProducesError(cld::Errors::Parser::EXPECTED_N.args("1 ')'")));
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max((5,",
                        ProducesError(cld::Errors::Parser::EXPECTED_N.args("2 ')'")));
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max((5,)",
                        ProducesError(cld::Errors::Parser::EXPECTED_N.args("1 ')'")));
    }
    SECTION("Must be a call")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max+(5,7);");
        CHECK(ret == "\nint i = max+(5,7);");
    }
    SECTION("Whitespace between opening parentheses")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max           (5,7);");
        CHECK(ret == "\nint i = ((5) > (7) ? (5) : (7));");
    }
    SECTION("Multi line")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(5,\n"
                                    "7);end");
        CHECK(ret == "\nint i = ((5) > (7) ? (5) : (7));end");
    }
    SECTION("No arguments")
    {
        auto ret = preprocessResult("#define max() ((a) > (b) ? (a) : (b))\n"
                                    "int i = max();");
        CHECK(ret == "\nint i = ((a) > (b) ? (a) : (b));");
    }
    SECTION("Empty arguments")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(,);");
        CHECK(ret == "\nint i = (() > () ? () : ());");
    }
    SECTION("Nested arguments")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max((5,7),7);");
        CHECK(ret == "\nint i = (((5,7)) > (7) ? ((5,7)) : (7));");
    }
    SECTION("Macro replacement in argument")
    {
        auto ret = preprocessResult("#define VALUE 5\n"
                                    "#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(VALUE,7);");
        CHECK(ret == "\n\nint i = ((5) > (7) ? (5) : (7));");
        PP_OUTPUTS_WITH("#define VALUE 5,7\n"
                        "#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(VALUE);",
                        ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args("\"max\"", 2, 1)));
    }
    SECTION("Empty result")
    {
        auto ret = preprocessResult("#define VALUE 5\n"
                                    "#define max(a, b)\n"
                                    "int i = max(VALUE,7);");
        CHECK(ret == "\n\nint i = ;");
    }
    SECTION("__VA_ARGS__")
    {
        auto ret = preprocessResult("#define max(a,...) ((a) > (__VA_ARGS__) ? (a) : (__VA_ARGS__))\n"
                                    "int i = max(5,7,645,,3);");
        CHECK(ret == "\nint i = ((5) > (7,645,,3) ? (5) : (7,645,,3));");
    }
    SECTION("Empty __VA_ARGS__")
    {
        auto ret = preprocessResult("#define max(a,...) ((a) > (__VA_ARGS__) ? (a) : (__VA_ARGS__))\n"
                                    "int i = max(5);");
        CHECK(ret == "\nint i = ((5) > () ? (5) : ());");
    }
    SECTION("Only __VA_ARGS__")
    {
        auto ret = preprocessResult("#define max(...) ((a) > (__VA_ARGS__) ? (a) : (__VA_ARGS__))\n"
                                    "int i = max(5);");
        CHECK(ret == "\nint i = ((a) > (5) ? (a) : (5));");
    }
    SECTION("Macro replacement in replacement list")
    {
        auto ret = preprocessResult("#define OP >\n"
                                    "#define max(a, b) ((a) OP (b) ? (a) : (b))\n"
                                    "int i = max(5,7);");
        CHECK(ret == "\n\nint i = ((5) > (7) ? (5) : (7));");
    }
    SECTION("Macro replacement in replacement list doesn't create arguments")
    {
        auto ret = preprocessResult("#define OP a\n"
                                    "#define max(a, b) ((a) OP (b) ? (a) : (b))\n"
                                    "int i = max(5,7);");
        CHECK(ret == "\n\nint i = ((5) a (7) ? (5) : (7));");
    }
    SECTION("__LINE__ position")
    {
        // This behaviour here is implementation defined
        // Clang does it on the ). GCC does it on the identifier. We follow Clang's behaviour
        auto ret = preprocessResult("#define func() __LINE__\n"
                                    "int i = func(\n"
                                    ");");
        CHECK(ret == "\nint i = 3;");
    }
}

#undef PP_OUTPUTS_WITH
#undef preprocessResult
#undef preprocessTest

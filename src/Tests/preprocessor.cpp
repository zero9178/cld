#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Text.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include "TestConfig.hpp"

#define preprocessResult(source)                                                            \
    [](const std::string& str) {                                                            \
        std::string storage;                                                                \
        llvm::raw_string_ostream ss(storage);                                               \
        auto tokens = cld::Lexer::tokenize(str, cld::LanguageOptions::native(), true, &ss); \
        INFO(ss.str());                                                                     \
        REQUIRE(ss.str().empty());                                                          \
        auto ret = cld::PP::preprocess(tokens, {}, &ss);                                    \
        INFO(ss.str());                                                                     \
        REQUIRE(ss.str().empty());                                                          \
        return cld::Lexer::constructPP(ret, ret.data().begin(), ret.data().end());          \
    }(source)

#define preprocessTest(source)                                                              \
    [](const std::string& str) {                                                            \
        std::string storage;                                                                \
        llvm::raw_string_ostream ss(storage);                                               \
        auto tokens = cld::Lexer::tokenize(str, cld::LanguageOptions::native(), true, &ss); \
        INFO(ss.str());                                                                     \
        REQUIRE(ss.str().empty());                                                          \
        auto ret = cld::PP::preprocess(tokens, {}, &ss);                                    \
        return ret;                                                                         \
    }(source)

#define PP_OUTPUTS_WITH(input, match)                                                         \
    do                                                                                        \
    {                                                                                         \
        std::string s;                                                                        \
        llvm::raw_string_ostream ss(s);                                                       \
        auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), true, &ss); \
        INFO(ss.str());                                                                       \
        REQUIRE(ss.str().empty());                                                            \
        cld::PP::preprocess(tokens, {}, &ss);                                                 \
        CHECK_THAT(s, match);                                                                 \
        if (!s.empty())                                                                       \
        {                                                                                     \
            cld::PP::preprocess(tokens);                                                      \
        }                                                                                     \
    } while (0)

using namespace cld::Errors::PP;
using namespace cld::Warnings::PP;
using namespace cld::Notes;

TEST_CASE("C99 Standard examples", "[PP]")
{
    SECTION("6.10.3.5 'Scope of macro definitions'")
    {
        return;
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
                CHECK(ret == "\n\n\n\n\n\n\n% f(2 * (0)) + t(1);");
            }
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
            auto ret = preprocessResult("#define debug(...) fprintf(stderr, _ _VA_ARGS_ _)\n"
                                        "#define showlist(...) puts(#_ _VA_ARGS_ _)\n"
                                        "#define report(test, ...) ((test)?puts(#test):\\\n"
                                        "printf(_ _VA_ARGS_ _))\n"
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
                          [](auto&& token) { return token.getMacroId() == 1; }));
        CHECK(std::all_of(tokens.data().begin() + 13, tokens.data().end(),
                          [](auto&& token) { return !token.macroInserted(); }));
        CHECK(tokens.getSubstitutions().size() == 1);
        REQUIRE(tokens.getSubstitutions().count(1));
        CHECK(tokens.getSubstitutions().at(1).identifier.getRepresentation() == "FUNC");
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
                          [](auto&& token) { return token.getMacroId() == 2; }));
        CHECK(std::all_of(tokens.data().begin() + 12, tokens.data().begin() + 13,
                          [](auto&& token) { return token.getMacroId() == 1; }));
        CHECK(std::all_of(tokens.data().begin() + 13, tokens.data().begin() + 18,
                          [](auto&& token) { return token.getMacroId() == 3; }));
        CHECK(std::all_of(tokens.data().begin() + 18, tokens.data().end(),
                          [](auto&& token) { return !token.macroInserted(); }));
        CHECK(tokens.getSubstitutions().size() == 3);
        REQUIRE(tokens.getSubstitutions().count(1));
        CHECK(tokens.getSubstitutions().at(1).identifier.getRepresentation() == "NESTED");
        REQUIRE(tokens.getSubstitutions().count(2));
        CHECK(tokens.getSubstitutions().at(2).identifier.getRepresentation() == "FUNC");
        REQUIRE(tokens.getSubstitutions().count(3));
        CHECK(tokens.getSubstitutions().at(3).identifier.getRepresentation() == "FUNC");
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
}

TEST_CASE("Builtin macros")
{
    SECTION("Date")
    {
        auto time = std::time(nullptr);
        auto tm = std::localtime(&time);
        std::string date(30, ' ');
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat"
        auto size = std::strftime(date.data(), date.size(), "%b %e %Y", tm);
#pragma GCC diagnostic pop
        CLD_ASSERT(size > 0);
        date.resize(size);
        auto ret = preprocessResult("const char* date(){ return __DATE__; }");
        CHECK(ret == "const char* date(){ return \"" + date + "\"; }");
    }
    SECTION("Time")
    {
        auto time = std::time(nullptr);
        auto tm = std::localtime(&time);
        std::string timeString(30, ' ');
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat"
        auto size = std::strftime(timeString.data(), timeString.size(), "%H:%M:%S", tm);
#pragma GCC diagnostic pop
        CLD_ASSERT(size > 0);
        timeString.resize(size);
        auto ret = preprocessResult("const char* time(){ return __TIME__; }");
        CHECK_THAT(ret,
                   Catch::Matchers::Matches("const char\\* time\\(\\)\\{ return \"\\d{1,2}:\\d{1,2}:\\d{1,2}\"; \\}"));
    }
}

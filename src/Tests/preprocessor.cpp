#include "catch.hpp"

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include <sstream>

#define preprocessTest(source)                                                           \
    [](const std::string& str) {                                                         \
        std::stringstream ss;                                                            \
        auto tokens = OpenCL::Lexer::tokenize(str, OpenCL::Language::Preprocessor, &ss); \
        INFO(ss.str());                                                                  \
        REQUIRE(ss.str().empty());                                                       \
        auto ret = OpenCL::PP::preprocess(tokens, &ss);                                  \
        return std::pair{OpenCL::Lexer::reconstruct(ret.begin(), ret.end()), ss.str()};  \
    }(source)

TEST_CASE("C99 Standard examples", "[PP]")
{
    SECTION("6.10.3.5 'Scope of macro definitions'")
    {
        SECTION("Example 1")
        {
            auto [ret, error] = preprocessTest("#define TABSIZE 100\nint table[TABSIZE];");
            INFO(error);
            CHECK(error.empty());
            CHECK(ret == "\nint table[100];");
        }
        SECTION("Example 2")
        {
            auto [ret, error] = preprocessTest("#define max(a, b) ((a) > (b) ? (a) : (b))");
            INFO(error);
            CHECK(error.empty());
            CHECK(ret.empty());
        }
        SECTION("Example 3")
        {
            SECTION("Partial")
            {
                auto [ret, error] = preprocessTest("#define x 3\n"
                                                   "#define f(a) f(x * (a))\n"
                                                   "#undef x\n"
                                                   "#define x 2\n"
                                                   "#define g f\n"
                                                   "#define z z[0]\n"
                                                   "#define t(a) a\n"
                                                   "% t(t(g)(0) + t)(1);");
                INFO(error);
                CHECK(error.empty());
                CHECK(ret == "\n\n\n\n\n\n\n% f(2 * (0)) + t(1);");
            }
            return;
            SECTION("Complete")
            {
                auto [ret, error] = preprocessTest("#define x 3\n"
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
                INFO(error);
                CHECK(error.empty());
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
            auto [ret, error] = preprocessTest("#define t(x,y,z) x ## y ## z\n"
                                               "int j[] = { t(1,2,3), t(,4,5), t(6,,7), t(8,9,),\n"
                                               "t(10,,), t(,11,), t(,,12), t(,,) };");
            INFO(error);
            CHECK(error.empty());
            CHECK(ret
                  == "\nint j[] = { 123, 45, 67, 89,\n"
                     "10, 11, 12, };");
        }
        SECTION("Example 7")
        {
            auto [ret, error] = preprocessTest("#define debug(...) fprintf(stderr, _ _VA_ARGS_ _)\n"
                                               "#define showlist(...) puts(#_ _VA_ARGS_ _)\n"
                                               "#define report(test, ...) ((test)?puts(#test):\\\n"
                                               "printf(_ _VA_ARGS_ _))\n"
                                               "debug(\"Flag\");\n"
                                               "debug(\"X = %d\\n\", x);\n"
                                               "showlist(The first, second, and third items.);\n"
                                               "report(x>y, \"x is %d but y is %d\", x, y);");
            INFO(error);
            CHECK(error.empty());
            CHECK(ret
                  == "\n\n\nfprintf(stderr, \"Flag\" );\n"
                     "fprintf(stderr, \"X = %d\\n\", x );\n"
                     "puts( \"The first, second, and third items.\" );\n"
                     "((x>y)?puts(\"x>y\"):\n"
                     "printf(\"x is %d but y is %d\", x, y));");
        }
    }
}

TEST_CASE("Macros", "[PP]")
{
    SECTION("Object like Macros")
    {
        SECTION("Multiline")
        {
            auto [ret, error] =
                preprocessTest("#define FUNC \\\n (1 +\\\n 3) \nint main(void) {\n    return FUNC;\n}\n");
            INFO(error);
            CHECK(error.empty());
            CHECK(ret == "\n\n\nint main(void) {\n    return (1 + 3);\n}");
        }
        SECTION("Empty")
        {
            auto [ret, error] = preprocessTest("#define TABSIZE\nint table[TABSIZE];");
            INFO(error);
            CHECK(error.empty());
            CHECK(ret == "\nint table[];");
            SECTION("Multiline")
            {
                auto [ret, error] = preprocessTest("#define TABSIZE \\\n\nint table[TABSIZE];");
                INFO(error);
                CHECK(error.empty());
                CHECK(ret == "\n\nint table[];");
            }
        }
        SECTION("Concat")
        {
            {
                auto [ret, error] = preprocessTest("#define TABSIZE return\\\na\nint main(void){TABSIZE;}");
                INFO(error);
                CHECK(error.empty());
                CHECK(ret == "\n\nint main(void){returna;}");
            }
            {
                auto [ret, error] = preprocessTest("#define TABSIZE return \\\na\nint main(void){TABSIZE;}");
                INFO(error);
                CHECK(error.empty());
                CHECK(ret == "\n\nint main(void){return a;}");
            }
            {
                auto [ret, error] = preprocessTest("#define TABSIZE return\\\n a\nint main(void){TABSIZE;}");
                INFO(error);
                CHECK(error.empty());
                CHECK(ret == "\n\nint main(void){return a;}");
            }
        }
    }
}

#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Text.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include "TestConfig.hpp"

#define preprocessResult(source)                                                      \
    [](const std::string& str) {                                                      \
        std::string storage;                                                          \
        llvm::raw_string_ostream ss(storage);                                         \
        auto tokens = cld::Lexer::tokenize(str, cld::LanguageOptions::native(), &ss); \
        INFO(ss.str());                                                               \
        REQUIRE(ss.str().empty());                                                    \
        auto ret = cld::PP::preprocess(std::move(tokens), &ss);                       \
        INFO(ss.str());                                                               \
        REQUIRE(ss.str().empty());                                                    \
        return ret;                                                                   \
    }(source)

#define preprocessReconstructsTo(source, resultSource)                                                  \
    do                                                                                                  \
    {                                                                                                   \
        std::string storage;                                                                            \
        llvm::raw_string_ostream ss(storage);                                                           \
        auto tokens = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), &ss);                \
        INFO(ss.str());                                                                                 \
        REQUIRE(ss.str().empty());                                                                      \
        auto ret = cld::PP::preprocess(std::move(tokens), &ss);                                         \
        INFO(ss.str());                                                                                 \
        REQUIRE(ss.str().empty());                                                                      \
        auto str = cld::PP::reconstruct(ret.data().data(), ret.data().data() + ret.data().size(), ret); \
        CHECK_THAT(str, ProducesLines(resultSource));                                                   \
    } while (0)

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

namespace cld::Tests
{
struct ProducesPP : Catch::MatcherBase<cld::PPSourceObject>
{
private:
    std::string m_source;

public:
    ProducesPP(std::string source) : m_source(std::move(source)) {}

    bool match(const cld::PPSourceObject& arg) const override
    {
        bool errors = false;
        auto ret = cld::Lexer::tokenize(m_source, cld::LanguageOptions::native(), nullptr, &errors);
        REQUIRE_FALSE(errors);
        auto withoutNewline = ret.data();
        withoutNewline.erase(std::remove_if(withoutNewline.begin(), withoutNewline.end(),
                                            [](const cld::Lexer::PPToken& token) {
                                                return token.getTokenType() == cld::Lexer::TokenType::Newline;
                                            }),
                             withoutNewline.end());
        return std::equal(withoutNewline.begin(), withoutNewline.end(), arg.data().begin(), arg.data().end(),
                          [&](const cld::Lexer::PPToken& lhs, const cld::Lexer::PPToken& rhs) {
                              return std::tuple(cld::Lexer::normalizeSpelling(lhs.getRepresentation(ret)),
                                                lhs.getTokenType(), lhs.getValue())
                                     == std::tuple(cld::Lexer::normalizeSpelling(rhs.getRepresentation(arg)),
                                                   rhs.getTokenType(), rhs.getValue());
                          });
    }

protected:
    std::string describe() const override
    {
        return "produces pp tokens of:\n" + m_source;
    }
};
} // namespace cld::Tests

using namespace cld::Errors::PP;
using namespace cld::Warnings::PP;
using namespace cld::Notes;

TEST_CASE("PP C99 Standard examples", "[PP]")
{
    SECTION("6.10.3.5 'Scope of macro definitions'")
    {
        SECTION("Example 1")
        {
            auto ret = preprocessResult("#define TABSIZE 100\nint table[TABSIZE];");
            CHECK_THAT(ret, ProducesPP("int table[100];"));
        }
        SECTION("Example 2")
        {
            auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))");
            CHECK(ret.data().empty());
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
                                            "#define t(a) a\n"
                                            "% t(t(g)(0) + t)(1);");
                // the first t in t(t(g)(0) + t) is found
                // it's single argument is then preprocessed as if it's the end of the file
                // therefore we are now pre processing t(g)(0) + t
                // t(g) turns into g
                // g turns into f
                // f(0) + t turns into f(2 * (0)) + t
                // Now preprocessing of the argument is done
                // Now argument substitution takes place as if t(f(2 * (0)) + t)(1) was written
                // after replacement of t we get f(2 * (0)) + t(1) and preprocessing is done
                CHECK_THAT(ret, ProducesPP("% f(2 * (0)) + t(1);"));
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
                CHECK_THAT(ret, ProducesPP("f(2* (y+1))+ f(2* (f(2* (z[0])))) % f(2 * (0)) + t(1);\n"
                                           "f(2 * (2+(3,4)-0,1)) | f(2 * (~ 5)) & f(2 * (0,1))^m(0,1);\n"
                                           "int i[] = { 1, 23, 4, 5, };\n"
                                           "char c[2][6] = { \"hello\", \"\" };"));
            }
        }
        return;
        SECTION("Example 5")
        {
            auto ret = preprocessResult("#define t(x,y,z) x ## y ## z\n"
                                        "int j[] = { t(1,2,3), t(,4,5), t(6,,7), t(8,9,),\n"
                                        "t(10,,), t(,11,), t(,,12), t(,,) };");
            CHECK_THAT(ret, ProducesPP("int j[] = { 123, 45, 67, 89,\n"
                                       "10, 11, 12, };"));
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
            CHECK_THAT(ret, ProducesPP("fprintf(stderr, \"Flag\" );\n"
                                       "fprintf(stderr, \"X = %d\\n\", x );\n"
                                       "puts( \"The first, second, and third items.\" );\n"
                                       "((x>y)?puts(\"x>y\"):\n"
                                       "printf(\"x is %d but y is %d\", x, y));"));
        }
    }
}

TEST_CASE("PP Text line", "[PP]")
{
    SECTION("Backslash Newline")
    {
        auto ret = preprocessResult(R"(Multi\
line output!
Normal line)");
        CHECK_THAT(ret, ProducesPP("Multiline output!\nNormal line"));
    }
    SECTION("Trigraphs Newline")
    {
        auto ret = preprocessResult("struct ?\?< ?\?> a;");
        CHECK_THAT(ret, ProducesPP("struct { } a;"));
    }
}

TEST_CASE("PP Macros", "[PP]")
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

namespace
{
template <class InputIterator>
bool haveMacroID(InputIterator begin, InputIterator end, std::uint64_t macroId)
{
    return std::all_of(begin, end, [macroId](const cld::Lexer::PPToken& token) {
        return token.getMacroId() == cld::Lexer::MacroID(macroId);
    });
}
} // namespace

TEST_CASE("PP Object like Macros", "[PP]")
{
    SECTION("Normal")
    {
        auto ret = preprocessResult("#define FUNC (1 + 3)\nint main(void) {\n    return FUNC;\n}\n");
        CHECK_THAT(ret, ProducesPP("\nint main(void) {\n    return (1 + 3);\n}"));
        REQUIRE(ret.data().size() == 14);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 7, 0));
        CHECK(haveMacroID(ret.data().begin() + 7, ret.data().begin() + 12, 1));
        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().end(), 0));
        REQUIRE(ret.getSubstitutions().size() == 2);
        CHECK(ret.getSubstitutions()[1].macroIdentifier.offset == 8);
        CHECK(ret.getSubstitutions()[1].macroIdentifier.length == 4);
    }
    SECTION("Empty")
    {
        auto ret = preprocessResult("#define TABSIZE\nint table[TABSIZE];");
        CHECK_THAT(ret, ProducesPP("\nint table[];"));
    }
    SECTION("Nested")
    {
        auto ret = preprocessResult(
            R"(#define FUNC (1 + 3)
#define NESTED FUNC * FUNC
int main(void) {
    return NESTED;
})");
        CHECK_THAT(ret, ProducesPP("\n\nint main(void) {\n    return (1 + 3) * (1 + 3);\n}"));
        REQUIRE(ret.data().size() == 20);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 7, 0));
        CHECK(haveMacroID(ret.data().begin() + 7, ret.data().begin() + 12, 2));
        CHECK(haveMacroID(ret.data().begin() + 12, ret.data().begin() + 13, 1));
        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().begin() + 18, 3));
        CHECK(haveMacroID(ret.data().begin() + 18, ret.data().end(), 0));
    }
    SECTION("At beginning of line")
    {
        auto ret = preprocessResult("#define INT int\nINT table[100];");
        CHECK_THAT(ret, ProducesPP("\nint table[100];"));
    }
    SECTION("Succeeding macros")
    {
        auto ret = preprocessResult("#define LONG long\nLONG LONG table[100];");
        CHECK_THAT(ret, ProducesPP("\nlong long table[100];"));
    }
    SECTION("Yielding __LINE__")
    {
        auto ret = preprocessResult("#define VALUE __LINE__\nlong table[VALUE];");
        CHECK_THAT(ret, ProducesPP("\nlong table[2];"));
    }
    SECTION("Recursive")
    {
        auto ret = preprocessResult(
            R"(#define FUNC (1 + 3)
#define NESTED NESTED * FUNC
int main(void) {
    return NESTED;
})");
        CHECK_THAT(ret, ProducesPP(R"(

int main(void) {
    return NESTED * (1 + 3);
})"));
    }
    SECTION("Following trigraphs")
    {
        auto ret = preprocessResult("#define TABSIZE\nint table?\?(TABSIZE?\?);");
        CHECK_THAT(ret, ProducesPP("int table[];"));
        ret = preprocessResult("#define TABSIZE 100\nint table?\?(TABSIZE?\?);");
        CHECK_THAT(ret, ProducesPP("int table[100];"));
    }
}

TEST_CASE("PP Builtin macros", "[PP]")
{
    SECTION("Date")
    {
        auto ret = preprocessResult("__DATE__");
        REQUIRE(ret.data().size() == 1);
        // Make sure to change regex in the year 10000
        CHECK_THAT(cld::to_string(ret.data()[0].getValue()), Catch::Matchers::Matches("\\w+ \\d{1,2} \\d{4}"));
    }
    SECTION("Time")
    {
        auto ret = preprocessResult("__TIME__");
        REQUIRE(ret.data().size() == 1);
        CHECK_THAT(cld::to_string(ret.data()[0].getValue()), Catch::Matchers::Matches("\\d{1,2}:\\d{1,2}:\\d{1,2}"));
    }
    SECTION("__STDC__")
    {
        auto ret = preprocessResult("__STDC__");
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("__STDC_HOSTED__")
    {
        auto ret = preprocessResult("__STDC_HOSTED__");
        CHECK_THAT(ret, ProducesPP("0"));
    }
    SECTION("__STDC_MB_MIGHT_NEQ_WC__")
    {
        auto ret = preprocessResult("__STDC_MB_MIGHT_NEQ_WC__");
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("__STDC_VERSION__")
    {
        auto ret = preprocessResult("__STDC_VERSION__");
        CHECK_THAT(ret, ProducesPP("199901L"));
    }
    SECTION("__FILE__")
    {
        auto ret = preprocessResult("__FILE__");
        CHECK_THAT(ret, ProducesPP("\"<stdin>\""));
    }
    SECTION("__LINE__")
    {
        auto ret = preprocessResult("__LINE__\n\n__LINE__");
        CHECK_THAT(ret, ProducesPP("1 3"));
    }
}

TEST_CASE("PP Function like Macros", "[PP]")
{
    SECTION("Normal")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(5,7);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > (7) ? (5) : (7));"));
        REQUIRE(ret.data().size() >= 21);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 3, 0));
        CHECK(haveMacroID(ret.data().begin() + 3, ret.data().begin() + 5, 1));
        CHECK(haveMacroID(ret.data().begin() + 5, ret.data().begin() + 6, 2)); // a
        CHECK(haveMacroID(ret.data().begin() + 6, ret.data().begin() + 9, 1));
        CHECK(haveMacroID(ret.data().begin() + 9, ret.data().begin() + 10, 3)); // b
        CHECK(haveMacroID(ret.data().begin() + 10, ret.data().begin() + 13, 1));
        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().begin() + 14, 4)); // a
        CHECK(haveMacroID(ret.data().begin() + 14, ret.data().begin() + 17, 1));
        CHECK(haveMacroID(ret.data().begin() + 17, ret.data().begin() + 18, 5)); // b
        CHECK(haveMacroID(ret.data().begin() + 18, ret.data().begin() + 20, 1));
        CHECK(haveMacroID(ret.data().begin() + 20, ret.data().begin() + 21, 0));
        auto& subs = ret.getSubstitutions();
        REQUIRE(subs.size() == 6);
        CHECK(subs[1].macroIdentifier.offset == 8);
        CHECK(subs[1].macroIdentifier.length == 3);
        CHECK(subs[1].replacedIdentifier.offset == 50);
        CHECK(subs[1].replacedIdentifier.length == 8);
        CHECK(subs[1].replacedIdentifier.macroId == cld::Lexer::MacroID(0));

        CHECK(subs[2].macroIdentifier.offset == 12);
        CHECK(subs[2].macroIdentifier.length == 1);
        CHECK(subs[2].replacedIdentifier.offset == 20);
        CHECK(subs[2].replacedIdentifier.length == 1);
        CHECK(subs[2].replacedIdentifier.macroId == cld::Lexer::MacroID(1));

        CHECK(subs[3].macroIdentifier.offset == 15);
        CHECK(subs[3].macroIdentifier.length == 1);
        CHECK(subs[3].replacedIdentifier.offset == 26);
        CHECK(subs[3].replacedIdentifier.length == 1);
        CHECK(subs[3].replacedIdentifier.macroId == cld::Lexer::MacroID(1));

        CHECK(subs[4].macroIdentifier.offset == 12);
        CHECK(subs[4].macroIdentifier.length == 1);
        CHECK(subs[4].replacedIdentifier.offset == 32);
        CHECK(subs[4].replacedIdentifier.length == 1);
        CHECK(subs[4].replacedIdentifier.macroId == cld::Lexer::MacroID(1));

        CHECK(subs[5].macroIdentifier.offset == 15);
        CHECK(subs[5].macroIdentifier.length == 1);
        CHECK(subs[5].replacedIdentifier.offset == 38);
        CHECK(subs[5].replacedIdentifier.length == 1);
        CHECK(subs[5].replacedIdentifier.macroId == cld::Lexer::MacroID(1));
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
        CHECK_THAT(ret, ProducesPP("int i = max+(5,7);"));
    }
    SECTION("Whitespace between opening parentheses")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max           (5,7);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > (7) ? (5) : (7));"));
    }
    SECTION("Multi line")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(5,\n"
                                    "7);end");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > (7) ? (5) : (7));end"));
    }
    SECTION("No arguments")
    {
        auto ret = preprocessResult("#define max() ((a) > (b) ? (a) : (b))\n"
                                    "int i = max();");
        CHECK_THAT(ret, ProducesPP("int i = ((a) > (b) ? (a) : (b));"));
    }
    SECTION("Empty arguments")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(,);");
        CHECK_THAT(ret, ProducesPP("int i = (() > () ? () : ());"));
    }
    SECTION("Nested arguments")
    {
        auto ret = preprocessResult("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max((5,7),7);");
        CHECK_THAT(ret, ProducesPP("int i = (((5,7)) > (7) ? ((5,7)) : (7));"));
    }
    SECTION("Macro replacement in argument")
    {
        auto ret = preprocessResult("#define VALUE 5\n"
                                    "#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                    "int i = max(VALUE,7);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > (7) ? (5) : (7));"));
        auto& subs = ret.getSubstitutions();
        REQUIRE(ret.data().size() >= 21);
        //        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 3, 0));
        //        CHECK(haveMacroID(ret.data().begin() + 3, ret.data().begin() + 5, 1));
        //        CHECK(haveMacroID(ret.data().begin() + 5, ret.data().begin() + 6, 3)); // a
        //        CHECK(haveMacroID(ret.data().begin() + 6, ret.data().begin() + 9, 1));
        //        CHECK(haveMacroID(ret.data().begin() + 9, ret.data().begin() + 10, 4)); // b
        //        CHECK(haveMacroID(ret.data().begin() + 10, ret.data().begin() + 13, 1));
        //        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().begin() + 14, 6)); // a
        //        CHECK(haveMacroID(ret.data().begin() + 14, ret.data().begin() + 17, 1));
        //        CHECK(haveMacroID(ret.data().begin() + 17, ret.data().begin() + 18, 7)); // b
        //        CHECK(haveMacroID(ret.data().begin() + 18, ret.data().begin() + 20, 1));
        //        CHECK(haveMacroID(ret.data().begin() + 20, ret.data().begin() + 21, 0));
        //        REQUIRE(subs.size() == 8);
        //        CHECK(subs[1].replacedIdentifier.macroId == cld::Lexer::MacroID(0));
        //        CHECK(subs[2].replacedIdentifier.macroId == cld::Lexer::MacroID(1));
        //        CHECK(subs[3].replacedIdentifier.macroId == cld::Lexer::MacroID(2));
        //        CHECK(subs[4].replacedIdentifier.macroId == cld::Lexer::MacroID(1));
        //        CHECK(subs[5].replacedIdentifier.macroId == cld::Lexer::MacroID(1));
        //        CHECK(subs[6].replacedIdentifier.macroId == cld::Lexer::MacroID(5));
        //        CHECK(subs[7].replacedIdentifier.macroId == cld::Lexer::MacroID(1));
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
        CHECK_THAT(ret, ProducesPP("int i = ;"));
    }
    SECTION("__VA_ARGS__")
    {
        auto ret = preprocessResult("#define max(a,...) ((a) > (__VA_ARGS__) ? (a) : (__VA_ARGS__))\n"
                                    "int i = max(5,7,645,,3);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > (7,645,,3) ? (5) : (7,645,,3));"));
    }
    SECTION("Empty __VA_ARGS__")
    {
        auto ret = preprocessResult("#define max(a,...) ((a) > (__VA_ARGS__) ? (a) : (__VA_ARGS__))\n"
                                    "int i = max(5);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > () ? (5) : ());"));
    }
    SECTION("Only __VA_ARGS__")
    {
        auto ret = preprocessResult("#define max(...) ((a) > (__VA_ARGS__) ? (a) : (__VA_ARGS__))\n"
                                    "int i = max(5);");
        CHECK_THAT(ret, ProducesPP("\nint i = ((a) > (5) ? (a) : (5));"));
    }
    SECTION("Macro replacement in replacement list")
    {
        auto ret = preprocessResult("#define OP >\n"
                                    "#define max(a, b) ((a) OP (b) ? (a) : (b))\n"
                                    "int i = max(5,7);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) > (7) ? (5) : (7));"));
    }
    SECTION("Macro replacement in replacement list doesn't create arguments")
    {
        auto ret = preprocessResult("#define OP a\n"
                                    "#define max(a, b) ((a) OP (b) ? (a) : (b))\n"
                                    "int i = max(5,7);");
        CHECK_THAT(ret, ProducesPP("int i = ((5) a (7) ? (5) : (7));"));
    }
    SECTION("__LINE__ position")
    {
        // This behaviour here is implementation defined
        // Clang does it on the ). GCC does it on the identifier. We follow Clang's behaviour
        auto ret = preprocessResult("#define func() __LINE__\n"
                                    "int i = func(\n"
                                    ");");
        CHECK_THAT(ret, ProducesPP("int i = 3;"));
    }
    SECTION("Rescanning behaviour")
    {
        auto ret = preprocessResult("#define A B\n"
                                    "#define B(a) a\n"
                                    "A\n"
                                    "(0)");
        CHECK_THAT(ret, ProducesPP("\n\n0"));
    }
    SECTION("Macro replacement in argument")
    {
        auto ret = preprocessResult("#define t(a) a\n"
                                    "#define g f\n"
                                    "#define f(a) f(x * (a))\n"
                                    "#define x 2\n"
                                    "t(g)(0)");
        CHECK_THAT(ret, ProducesPP("f(2 * (0))"));
    }
    SECTION("Identity macro")
    {
        auto ret = preprocessResult("#define I(x) x\n"
                                    "#define A(x) (10+x)\n"
                                    "I(A(A(40)))");
        CHECK_THAT(ret, ProducesPP("(10+(10+40))"));
        auto& subs = ret.getSubstitutions();
        auto& tokens = ret.data();
        REQUIRE(tokens.size() == 9);
        //        CHECK(haveMacroID(tokens.begin(), tokens.begin() + 3, 3));
        //        CHECK(haveMacroID(tokens.begin() + 3, tokens.begin() + 6, 5));
        //        CHECK(haveMacroID(tokens.begin() + 6, tokens.begin() + 7, 6));
        //        CHECK(haveMacroID(tokens.begin() + 7, tokens.begin() + 8, 5));
        //        CHECK(haveMacroID(tokens.begin() + 8, tokens.begin() + 9, 3));
        //        REQUIRE(subs.size() == 7);
        //        CHECK(subs[1].replacedIdentifier.macroId == cld::Lexer::MacroID(0));
        //        CHECK(subs[2].replacedIdentifier.macroId == cld::Lexer::MacroID(1));
        //        CHECK(subs[3].replacedIdentifier.macroId == cld::Lexer::MacroID(2));
        //        CHECK(subs[4].replacedIdentifier.macroId == cld::Lexer::MacroID(3));
        //        CHECK(subs[5].replacedIdentifier.macroId == cld::Lexer::MacroID(4));
        //        CHECK(subs[6].replacedIdentifier.macroId == cld::Lexer::MacroID(5));
    }
    SECTION("Argument substitution after rescan")
    {
        auto ret = preprocessResult("#define I(x) I_(x)\n"
                                    "#define I_(x) 1+x +1\n"
                                    "#define VALUE 5\n"
                                    "I(VALUE)");
        auto& subs = ret.getSubstitutions();
        auto& tokens = ret.data();
        CHECK_THAT(ret, ProducesPP("1+5+1"));
    }
}

TEST_CASE("PP Operator #", "[PP]")
{
    SECTION("Simple")
    {
        auto ret = preprocessResult("#define Q(x) #x\n"
                                    "Q(5)");
        CHECK_THAT(ret, ProducesPP("\"5\""));
    }
    SECTION("Preprocessed arguments")
    {
        auto ret = preprocessResult("#define Y 20\n"
                                    "#define A(x) (10+x+Y)\n"
                                    "#define Q_(x) #x\n"
                                    "#define Q(x) Q_(x)\n"
                                    "Q(A(A(40)))");
        CHECK_THAT(ret, ProducesPP("\"(10+(10+40+20)+20)\""));
        ret = preprocessResult("#define Y 20\n"
                               "#define A(x) (10+x+Y)\n"
                               "#define Q(x) #x\n"
                               "Q(A(A(40)))");
        CHECK_THAT(ret, ProducesPP("\"A(A(40))\""));
    }
}

TEST_CASE("PP Reconstruction", "[PP]")
{
    SECTION("Function macro")
    {
        preprocessReconstructsTo("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                 "int i = max(5,7);",
                                 "int i = ((5) > (7) ? (5) : (7));");
    }
    SECTION("Function macro with whitespace between call")
    {
        preprocessReconstructsTo("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                 "int i = max           (5,7);",
                                 "int i = ((5) > (7) ? (5) : (7));");
    }
    SECTION("Multiline function call")
    {
        preprocessReconstructsTo("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                 "int i = max(5,\n"
                                 "7);end",
                                 "int i = ((5) > (7) ? (5) : (7));end");
    }
    SECTION("Empty argument result")
    {
        preprocessReconstructsTo("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                 "int i = max(,);",
                                 "int i = (() > () ? () : ());");
        preprocessReconstructsTo("#define max(a, b) ((a ) > (b) ? (a) : (b))\n"
                                 "int i = max(,);",
                                 "int i = (( ) > () ? () : ());");
        preprocessReconstructsTo("#define max(a, b) (( a) > (b) ? (a) : (b))\n"
                                 "int i = max(,);",
                                 "int i = (( ) > () ? () : ());");
        preprocessReconstructsTo("#define max(a, b) (( a ) > (b) ? (a) : (b))\n"
                                 "int i = max(,);",
                                 "int i = (( ) > () ? () : ());");
    }
    SECTION("Macro in function call")
    {
        preprocessReconstructsTo("#define VALUE 5\n"
                                 "#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                                 "int i = max(VALUE,7);",
                                 "int i = ((5) > (7) ? (5) : (7));");
    }
    SECTION("Nested")
    {
        preprocessReconstructsTo("#define t(a) a\n"
                                 "#define g f\n"
                                 "#define f(a) f(x * (a))\n"
                                 "#define x 2\n"
                                 "t(g)(0)",
                                 "f(2 * (0))");
    }
}

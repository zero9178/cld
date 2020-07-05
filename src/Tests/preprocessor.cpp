#include "catch.hpp"

#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Compiler/SourceObject.hpp>
#include <Frontend/Common/Text.hpp>
#include <Frontend/Preprocessor/Preprocessor.hpp>

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
        std::vector<cld::Lexer::PPToken> retWithoutNewline;
        std::vector<cld::Lexer::PPToken> argWithoutNewline;
        auto newLineFilter = [](const cld::Lexer::PPToken& token) {
            return token.getTokenType() == cld::Lexer::TokenType::Newline;
        };
        std::remove_copy_if(ret.data().begin(), ret.data().end(), std::back_inserter(retWithoutNewline), newLineFilter);
        std::remove_copy_if(arg.data().begin(), arg.data().end(), std::back_inserter(argWithoutNewline), newLineFilter);
        return std::equal(retWithoutNewline.begin(), retWithoutNewline.end(), argWithoutNewline.begin(),
                          argWithoutNewline.end(), [&](const cld::Lexer::PPToken& lhs, const cld::Lexer::PPToken& rhs) {
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
using namespace cld::Notes::PP;

TEST_CASE("PP C99 Standard examples", "[PP]")
{
    SECTION("6.10.3.5 'Scope of macro definitions'")
    {
        SECTION("Example 3")
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

TEST_CASE("PP Define", "[PP]")
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
    PP_OUTPUTS_WITH("#define A(x) # a x", ProducesError(EXPECTED_AN_ARGUMENT_AFTER_POUND));
    PP_OUTPUTS_WITH("#define A # a x", ProducesNothing());
    PP_OUTPUTS_WITH("#define A(x) ## x",
                    ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST));
    PP_OUTPUTS_WITH("#define A ## x",
                    ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST));
    PP_OUTPUTS_WITH("#define A(x) x ##", ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST));
    PP_OUTPUTS_WITH("#define A x ##", ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST));
}

namespace
{
template <class InputIterator>
bool haveMacroID(InputIterator begin, InputIterator end, std::uint32_t macroId)
{
    return std::all_of(begin, end,
                       [macroId](const cld::Lexer::PPToken& token) { return token.getMacroId() == macroId; });
}
} // namespace

TEST_CASE("PP Object like Macros", "[PP]")
{
    SECTION("Normal")
    {
        auto ret = preprocessResult("#define FUNC (1 + 3)\nint main(void) {\n    return FUNC;\n}\n");
        CHECK_THAT(ret, ProducesPP("int main(void) {\n    return (1 + 3);\n}"));
        REQUIRE(ret.data().size() == 17);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 6, 0));
        CHECK(haveMacroID(ret.data().begin() + 7, ret.data().begin() + 8, 0));
        CHECK(haveMacroID(ret.data().begin() + 8, ret.data().begin() + 13, 1));
        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().end(), 0));
        REQUIRE(ret.getSubstitutions().size() == 2);
        REQUIRE(std::holds_alternative<cld::Source::Substitution>(ret.getSubstitutions()[1]));
        CHECK(cld::get<cld::Source::Substitution>(ret.getSubstitutions()[1]).replacedIdentifier.getRepresentation(ret)
              == "FUNC");
        CHECK(cld::get<cld::Source::Substitution>(ret.getSubstitutions()[1]).macroIdentifier.getRepresentation(ret)
              == "FUNC");
        CHECK(!cld::get<cld::Source::Substitution>(ret.getSubstitutions()[1]).empty);
        CHECK(!cld::get<cld::Source::Substitution>(ret.getSubstitutions()[1]).closeParentheses);
    }
    SECTION("Empty")
    {
        auto ret = preprocessResult("#define TABSIZE\nint table[TABSIZE];");
        CHECK_THAT(ret, ProducesPP("int table[];"));
    }
    SECTION("Nested")
    {
        auto ret = preprocessResult(
            R"(#define FUNC (1 + 3)
#define NESTED FUNC * FUNC
int main(void) {
    return NESTED;
})");
        CHECK_THAT(ret, ProducesPP("int main(void) {\n    return (1 + 3) * (1 + 3);\n}"));
        REQUIRE(ret.data().size() == 23);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 6, 0));
        CHECK(haveMacroID(ret.data().begin() + 7, ret.data().begin() + 8, 0));
        CHECK(haveMacroID(ret.data().begin() + 8, ret.data().begin() + 13, 2));
        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().begin() + 14, 1));
        CHECK(haveMacroID(ret.data().begin() + 14, ret.data().begin() + 19, 3));
        CHECK(haveMacroID(ret.data().begin() + 19, ret.data().end(), 0));
    }
    SECTION("At beginning of line")
    {
        auto ret = preprocessResult("#define INT int\nINT table[100];");
        CHECK_THAT(ret, ProducesPP("int table[100];"));
    }
    SECTION("Succeeding macros")
    {
        auto ret = preprocessResult("#define LONG long\nLONG LONG table[100];");
        CHECK_THAT(ret, ProducesPP("long long table[100];"));
    }
    SECTION("Yielding __LINE__")
    {
        auto ret = preprocessResult("#define VALUE __LINE__\nlong table[VALUE];");
        CHECK_THAT(ret, ProducesPP("long table[2];"));
    }
    SECTION("Recursive")
    {
        auto ret = preprocessResult(
            R"(#define FUNC (1 + 3)
#define NESTED NESTED * FUNC
int main(void) {
    return NESTED;
})");
        CHECK_THAT(ret, ProducesPP(R"(int main(void) {
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
        REQUIRE(ret.data().size() >= 1);
        // Make sure to change regex in the year 10000
        CHECK_THAT(cld::to_string(ret.data()[0].getValue()), Catch::Matchers::Matches("\\w+ \\d{1,2} \\d{4}"));
    }
    SECTION("Time")
    {
        auto ret = preprocessResult("__TIME__");
        REQUIRE(ret.data().size() >= 1);
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
        CHECK_THAT(ret, ProducesPP("1\n\n3"));
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
        REQUIRE(std::holds_alternative<cld::Source::Substitution>(subs[1]));
        CHECK(cld::get<cld::Source::Substitution>(subs[1]).macroIdentifier.getRepresentation(ret) == "max");
        CHECK(cld::get<cld::Source::Substitution>(subs[1]).replacedIdentifier.getRepresentation(ret) == "max");
        CHECK(!cld::get<cld::Source::Substitution>(subs[1]).empty);
        REQUIRE(cld::get<cld::Source::Substitution>(subs[1]).closeParentheses);
        CHECK(cld::get<cld::Source::Substitution>(subs[1]).closeParentheses->getTokenType()
              == cld::Lexer::TokenType::CloseParentheses);
        CHECK(cld::get<cld::Source::Substitution>(subs[1]).replacedIdentifier.getMacroId() == 0);

        REQUIRE(std::holds_alternative<cld::Source::Substitution>(subs[2]));
        CHECK(cld::get<cld::Source::Substitution>(subs[2]).macroIdentifier.getRepresentation(ret) == "a");
        CHECK(cld::get<cld::Source::Substitution>(subs[2]).replacedIdentifier.getRepresentation(ret) == "a");
        CHECK(!cld::get<cld::Source::Substitution>(subs[2]).empty);
        CHECK(!cld::get<cld::Source::Substitution>(subs[2]).closeParentheses);
        CHECK(cld::get<cld::Source::Substitution>(subs[2]).replacedIdentifier.getMacroId() == 1);

        REQUIRE(std::holds_alternative<cld::Source::Substitution>(subs[3]));
        CHECK(cld::get<cld::Source::Substitution>(subs[3]).macroIdentifier.getRepresentation(ret) == "b");
        CHECK(cld::get<cld::Source::Substitution>(subs[3]).replacedIdentifier.getRepresentation(ret) == "b");
        CHECK(!cld::get<cld::Source::Substitution>(subs[3]).empty);
        CHECK(!cld::get<cld::Source::Substitution>(subs[3]).closeParentheses);
        CHECK(cld::get<cld::Source::Substitution>(subs[3]).replacedIdentifier.getMacroId() == 1);

        REQUIRE(std::holds_alternative<cld::Source::Substitution>(subs[4]));
        CHECK(cld::get<cld::Source::Substitution>(subs[4]).macroIdentifier.getRepresentation(ret) == "a");
        CHECK(cld::get<cld::Source::Substitution>(subs[4]).replacedIdentifier.getRepresentation(ret) == "a");
        CHECK(!cld::get<cld::Source::Substitution>(subs[4]).empty);
        CHECK(!cld::get<cld::Source::Substitution>(subs[4]).closeParentheses);
        CHECK(cld::get<cld::Source::Substitution>(subs[4]).replacedIdentifier.getMacroId() == 1);

        REQUIRE(std::holds_alternative<cld::Source::Substitution>(subs[5]));
        CHECK(cld::get<cld::Source::Substitution>(subs[5]).macroIdentifier.getRepresentation(ret) == "b");
        CHECK(cld::get<cld::Source::Substitution>(subs[5]).replacedIdentifier.getRepresentation(ret) == "b");
        CHECK(!cld::get<cld::Source::Substitution>(subs[5]).empty);
        CHECK(!cld::get<cld::Source::Substitution>(subs[5]).closeParentheses);
        CHECK(cld::get<cld::Source::Substitution>(subs[5]).replacedIdentifier.getMacroId() == 1);
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
        CHECK_THAT(ret, ProducesPP("int i = ((a) > (5) ? (a) : (5));"));
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
        CHECK_THAT(ret, ProducesPP("0"));
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
    SECTION("Argument substitution after rescan")
    {
        auto ret = preprocessResult("#define I(x) I_(x)\n"
                                    "#define I_(x) 1+x +1\n"
                                    "#define VALUE 5\n"
                                    "I(VALUE)");
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
        auto& subs = ret.getSubstitutions();
        REQUIRE(subs.size() == 4);
        REQUIRE(std::holds_alternative<cld::Source::Stringification>(subs[3]));
        auto& stringify = cld::get<cld::Source::Stringification>(subs[3]);
        CHECK(stringify.replacedIdentifier.getRepresentation(ret) == "x");
        REQUIRE(stringify.stringified.size() == 1);
        CHECK(stringify.stringified[0].getRepresentation(ret) == "5");
    }
    SECTION("Whitespace")
    {
        auto ret = preprocessResult("#define Q(x) #x\n"
                                    "Q(5 +            5)");
        CHECK_THAT(ret, ProducesPP("\"5 + 5\""));
    }
    SECTION("Literals")
    {
        auto ret = preprocessResult("#define Q(x) #x\n"
                                    "Q(\"text\")");
        CHECK_THAT(ret, ProducesPP("\"\\\"text\\\"\""));
        ret = preprocessResult("#define Q(x) #x\n"
                               "Q('\"')");
        CHECK_THAT(ret, ProducesPP("\"'\\\"'\""));
        ret = preprocessResult("#define Q(x) #x\n"
                               "Q(\"\\n\")");
        CHECK_THAT(ret, ProducesPP("\"\\\"\\\\n\\\"\""));
    }
    SECTION("__VA_ARGS__")
    {
        auto ret = preprocessResult("#define Q(...) #__VA_ARGS__\n"
                                    "Q(5,56,7)");
        CHECK_THAT(ret, ProducesPP("\"5,56,7\""));
    }
    SECTION("Multiline function argument")
    {
        auto ret = preprocessResult("#define Q(x) #x\n"
                                    "Q(5\n"
                                    "5)");
        CHECK_THAT(ret, ProducesPP("\"5 5\""));
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

TEST_CASE("PP Operator ##", "[PP]")
{
    SECTION("Simple")
    {
        auto ret = preprocessResult("#define Q 5 ## 7\n"
                                    "Q");
        CHECK_THAT(ret, ProducesPP("57"));
        auto& subs = ret.getSubstitutions();
        REQUIRE(subs.size() == 3);
        REQUIRE(std::holds_alternative<cld::Source::TokenConcatenation>(subs[2]));
        auto& stringify = cld::get<cld::Source::TokenConcatenation>(subs[2]);
        CHECK(stringify.leftToken.getRepresentation(ret) == "5");
        CHECK(stringify.rightToken.getRepresentation(ret) == "7");
    }
    SECTION("Disabled by substitution")
    {
        auto ret = preprocessResult("#define hash_hash # ## #\n"
                                    "#define Q 5 hash_hash 5\n"
                                    "Q");
        CHECK_THAT(ret, ProducesPP("5 ## 5"));
    }
    SECTION("Function Macro")
    {
        auto ret = preprocessResult("#define Q(x) x ## x\n"
                                    "Q(5)");
        CHECK_THAT(ret, ProducesPP("55"));
    }
    SECTION("Function macro disabled by substitution")
    {
        auto ret = preprocessResult("#define hash_hash # ## #\n"
                                    "#define Q(x) x hash_hash x\n"
                                    "#define str_(x) #x\n"
                                    "#define str(x) str_(x)\n"
                                    "str(Q(5))");
        CHECK_THAT(ret, ProducesPP("\"5 ## 5\""));
    }
    SECTION("Invalid concat")
    {
        PP_OUTPUTS_WITH("#define concat(x,y) x ## y\n"
                        "concat(.,foo)",
                        ProducesWarning(TOKEN_CONCATENATION_RESULTING_IN_AN_INVALID_TOKEN_IS_UB)
                            && ProducesNote(WHEN_CONCATENATING_N_AND_N.args(".", "foo")));
    }
    SECTION("Placemarkers")
    {
        auto ret =
            preprocessResult("#define t(x,y,z) x ## y ## z\n"
                             "int j[] = {t(1,2,3), t(,4,5), t(6,,7), t(8,9,), t(10,,), t(,11,), t(,,12), t(,,)};");
        CHECK_THAT(ret, ProducesPP("int j[] = {123, 45, 67, 89, 10, 11, 12, };"));
    }
    SECTION("No recursion")
    {
        auto ret = preprocessResult("#define TEXT1 TEXT ## 1\n"
                                    "TEXT1");
        CHECK_THAT(ret, ProducesPP("TEXT1"));
    }
    SECTION("Up for rescan")
    {
        auto ret = preprocessResult("#define TEXT2 5\n"
                                    "#define TEXT1 TEXT ## 2\n"
                                    "TEXT1");
        CHECK_THAT(ret, ProducesPP("5"));
    }
    SECTION("Leading whitespace propagation")
    {
        auto ret = preprocessResult("#define Q(x,y) 5 x##y\n"
                                    "#define Q_(...) print(__VA_ARGS__)\n"
                                    "#define print(...) #__VA_ARGS__\n"
                                    "Q_(Q(,7))");
        CHECK_THAT(ret, ProducesPP("\"5 7\""));
        ret = preprocessResult("#define Q(x,y) 5 x ##y\n"
                               "#define Q_(...) print(__VA_ARGS__)\n"
                               "#define print(...) #__VA_ARGS__\n"
                               "Q_(Q(,7))");
        CHECK_THAT(ret, ProducesPP("\"5 7\""));
        ret = preprocessResult("#define Q(x,y) +x##y\n"
                               "#define Q_(...) print(__VA_ARGS__)\n"
                               "#define print(...) #__VA_ARGS__\n"
                               "Q_(Q(,7))");
        CHECK_THAT(ret, ProducesPP("\"+7\""));
    }
}

TEST_CASE("PP C Preprocessor tricks", "[PP]")
{
    // Some preprocessor tricks taken from https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms
    SECTION("IIF")
    {
        auto ret = preprocessResult("#define IIF(cond) IIF_ ## cond\n"
                                    "#define IIF_0(t, f) f\n"
                                    "#define IIF_1(t, f) t\n"
                                    "IIF(1)(true, false)");
        CHECK_THAT(ret, ProducesPP("true"));
        ret = preprocessResult("#define IIF(cond) IIF_ ## cond\n"
                               "#define IIF_0(t, f) f\n"
                               "#define IIF_1(t, f) t\n"
                               "#define A() 1\n"
                               "IIF(A())(true, false)");
        CHECK_THAT(ret, ProducesPP("IIF_A()(true, false)"));
        ret = preprocessResult("#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)\n"
                               "#define PRIMITIVE_CAT(a,...) a ## __VA_ARGS__\n"
                               "#define IIF(cond) PRIMITIVE_CAT(IIF_, cond)\n"
                               "#define IIF_0(t, ...) __VA_ARGS__\n"
                               "#define IIF_1(t, ...) t\n"
                               "#define A() 1\n"
                               "IIF(1)(true,false)\n"
                               "IIF(A())(true, false)");
        CHECK_THAT(ret, ProducesPP("true\n"
                                   "true"));
    }
    SECTION("Detection")
    {
        auto ret = preprocessResult("#define CHECK_N(x, n, ...) n\n"
                                    "#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)\n"
                                    "#define PROBE(x) x,1,\n"
                                    "CHECK(PROBE(~))\n"
                                    "CHECK(xxx)");
        CHECK_THAT(ret, ProducesPP("1 0"));
        ret = preprocessResult("#define CHECK_N(x, n, ...) n\n"
                               "#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)\n"
                               "#define PROBE(x) x,1,\n"
                               "#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)\n"
                               "#define IS_PAREN_PROBE(...) PROBE(~)\n"
                               "IS_PAREN(())\n"
                               "IS_PAREN(xxx)");
        CHECK_THAT(ret, ProducesPP("1 0"));
        ret = preprocessResult("#define CHECK_N(x, n, ...) n\n"
                               "#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)\n"
                               "#define PROBE(x) x,1,\n"
                               "#define NOT(x) CHECK(PRIMITIVE_CAT(NOT_, x))\n"
                               "#define NOT_0 PROBE(~)\n"
                               "#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)\n"
                               "#define PRIMITIVE_CAT(a,...) a ## __VA_ARGS__\n"
                               "#define IIF(cond) PRIMITIVE_CAT(IIF_, cond)\n"
                               "#define IIF_0(t, ...) __VA_ARGS__\n"
                               "#define IIF_1(t, ...) t\n"
                               "#define BOOL(x) COMPL(NOT(x))\n"
                               "#define IF(c) IIF(BOOL(c))\n"
                               "#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)\n"
                               "#define COMPL_0 1\n"
                               "#define COMPL_1 0\n"
                               "#define EAT(...)\n"
                               "#define EXPAND(...) __VA_ARGS__\n"
                               "#define WHEN(c) IF(c)(EXPAND, EAT)\n"
                               "WHEN(1)(5)\n"
                               "WHEN(0)(4)\n");
        CHECK_THAT(ret, ProducesPP("5"));
    }
    SECTION("Recursion")
    {
        auto ret = preprocessResult("#define EMPTY()\n"
                                    "#define DEFER(id) id EMPTY()\n"
                                    "#define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()\n"
                                    "#define EXPAND(...) __VA_ARGS__\n"
                                    "#define A() 123\n"
                                    "A()\n"
                                    "DEFER(A)()\n"
                                    "EXPAND(DEFER(A)())");
        CHECK_THAT(ret, ProducesPP("123 A() 123"));
        ret = preprocessResult("#define CHECK_N(x, n, ...) n\n"
                               "#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)\n"
                               "#define PROBE(x) x,1,\n"
                               "#define NOT(x) CHECK(PRIMITIVE_CAT(NOT_, x))\n"
                               "#define NOT_0 PROBE(~)\n"
                               "#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)\n"
                               "#define PRIMITIVE_CAT(a,...) a ## __VA_ARGS__\n"
                               "#define IIF(cond) PRIMITIVE_CAT(IIF_, cond)\n"
                               "#define IIF_0(t, ...) __VA_ARGS__\n"
                               "#define IIF_1(t, ...) t\n"
                               "#define BOOL(x) COMPL(NOT(x))\n"
                               "#define IF(c) IIF(BOOL(c))\n"
                               "#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)\n"
                               "#define COMPL_0 1\n"
                               "#define COMPL_1 0\n"
                               "#define EAT(...)\n"
                               "#define EXPAND(...) __VA_ARGS__\n"
                               "#define WHEN(c) IF(c)(EXPAND, EAT)\n"
                               "\n"
                               "#define DEC(x) PRIMITIVE_CAT(DEC_, x)\n"
                               "#define DEC_0 0\n"
                               "#define DEC_1 0\n"
                               "#define DEC_2 1\n"
                               "#define DEC_3 2\n"
                               "#define DEC_4 3\n"
                               "#define DEC_5 4\n"
                               "#define DEC_6 5\n"
                               "#define DEC_7 6\n"
                               "#define DEC_8 7\n"
                               "#define DEC_9 8\n"
                               "\n"
                               "#define EMPTY()\n"
                               "#define DEFER(id) id EMPTY()\n"
                               "#define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()\n"
                               "\n"
                               "#define EVAL(...)  EVAL1(EVAL1(EVAL1(__VA_ARGS__)))\n"
                               "#define EVAL1(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))\n"
                               "#define EVAL2(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))\n"
                               "#define EVAL3(...) EVAL4(EVAL4(EVAL4(__VA_ARGS__)))\n"
                               "#define EVAL4(...) EVAL5(EVAL5(EVAL5(__VA_ARGS__)))\n"
                               "#define EVAL5(...) __VA_ARGS__\n"
                               "\n"
                               "#define REPEAT(count, macro, ...) \\\n"
                               "    WHEN(count) \\\n"
                               "    ( \\\n"
                               "        OBSTRUCT(REPEAT_INDIRECT) () \\\n"
                               "        ( \\\n"
                               "            DEC(count), macro, __VA_ARGS__ \\\n"
                               "        ) \\\n"
                               "        OBSTRUCT(macro) \\\n"
                               "        ( \\\n"
                               "            DEC(count), __VA_ARGS__ \\\n"
                               "        ) \\\n"
                               "    )\n"
                               "#define REPEAT_INDIRECT() REPEAT\n"
                               "\n"
                               "#define M(i, _) i\n"
                               "EVAL(REPEAT(8, M, ~))");
        CHECK_THAT(ret, ProducesPP("0 1 2 3 4 5 6 7"));
    }
    SECTION("Comparison")
    {
        auto ret = preprocessResult("#define CHECK_N(x, n, ...) n\n"
                                    "#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)\n"
                                    "#define PROBE(x) x,1,\n"
                                    "#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)\n"
                                    "#define IS_PAREN_PROBE(...) PROBE(~)\n"
                                    "\n"
                                    "#define COMPARE_foo(x) x\n"
                                    "#define COMPARE_bar(x) x\n"
                                    "#define PRIMITIVE_COMPARE(x, y) IS_PAREN \\\n"
                                    "( \\\n"
                                    "COMPARE_ ## x ( COMPARE_ ## y) (())  \\\n"
                                    ")\n"
                                    "PRIMITIVE_COMPARE(foo, bar)\n"
                                    "PRIMITIVE_COMPARE(foo, foo)\n"
                                    "PRIMITIVE_COMPARE(foo, unfoo)");
        CHECK_THAT(ret, ProducesPP("1 0 0"));
        ret = preprocessResult("#define CHECK_N(x, n, ...) n\n"
                               "#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)\n"
                               "#define PROBE(x) x,1,\n"
                               "#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)\n"
                               "#define IS_PAREN_PROBE(...) PROBE(~)\n"
                               "\n"
                               "#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)\n"
                               "#define PRIMITIVE_CAT(a,...) a ## __VA_ARGS__\n"
                               "#define IIF(cond) PRIMITIVE_CAT(IIF_, cond)\n"
                               "#define IIF_0(t, ...) __VA_ARGS__\n"
                               "#define IIF_1(t, ...) t\n"
                               "#define BOOL(x) COMPL(NOT(x))\n"
                               "#define IF(c) IIF(BOOL(c))\n"
                               "#define EAT(...)\n"
                               "\n"
                               "#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)\n"
                               "#define COMPL_0 1\n"
                               "#define COMPL_1 0\n"
                               "\n"
                               "#define BITAND(x) PRIMITIVE_CAT(BITAND_, x)\n"
                               "#define BITAND_0(y) 0\n"
                               "#define BITAND_1(y) y"
                               "\n"
                               "#define COMPARE_foo(x) x\n"
                               "#define COMPARE_bar(x) x\n"
                               "#define PRIMITIVE_COMPARE(x, y) IS_PAREN \\\n"
                               "( \\\n"
                               "COMPARE_ ## x ( COMPARE_ ## y) (())  \\\n"
                               ")\n"
                               "\n"
                               "#define IS_COMPARABLE(x) IS_PAREN( CAT(COMPARE_, x) (()) )\n"
                               "\n"
                               "#define NOT_EQUAL(x, y) \\\n"
                               "IIF(BITAND(IS_COMPARABLE(x))(IS_COMPARABLE(y)) ) \\\n"
                               "( \\\n"
                               "   PRIMITIVE_COMPARE, \\\n"
                               "   1 EAT \\\n"
                               ")(x, y)\n"
                               "\n"
                               "#define EQUAL(x, y) COMPL(NOT_EQUAL(x, y))\n"
                               "\n"
                               "EQUAL(foo,bar)\n"
                               "EQUAL(foo,foo)\n"
                               "EQUAL(foo,unfoo)\n");
        CHECK_THAT(ret, ProducesPP("0 1 0"));
    }
}

TEST_CASE("PP includes", "[PP]")
{
    SECTION("Simple") {}
    SECTION("Empty")
    {
        PP_OUTPUTS_WITH("#include", ProducesError(EXPECTED_A_FILENAME_AFTER_INCLUDE));
    }
    SECTION("Computed")
    {
        PP_OUTPUTS_WITH("#define EMPTY()\n"
                        "#include EMPTY()",
                        ProducesError(EXPECTED_A_FILENAME_AFTER_INCLUDE));
        PP_OUTPUTS_WITH("#define MACRO <TEST\n"
                        "#include MACRO",
                        ProducesError(cld::Errors::Lexer::UNTERMINATED_N.args(cld::Errors::Lexer::INCLUDE_DIRECTIVE)));
        PP_OUTPUTS_WITH("#define MACRO <TEST> w\n"
                        "#include MACRO",
                        ProducesError(EXTRA_TOKENS_AFTER_INCLUDE));
        PP_OUTPUTS_WITH("#define MACRO \"TEST\" w\n"
                        "#include MACRO",
                        ProducesError(EXTRA_TOKENS_AFTER_INCLUDE));
    }
}

TEST_CASE("PP Reconstruction", "[PP]")
{
    SECTION("Object macro")
    {
        preprocessReconstructsTo("#define VALUE 6\n"
                                 "int i = VALUE;",
                                 "int i = 6;");
        preprocessReconstructsTo("#define VALUE 6\n"
                                 "int i = VALUE ;",
                                 "int i = 6 ;");
        preprocessReconstructsTo("#define VALUE 6\n"
                                 "int i =VALUE;",
                                 "int i =6;");
        preprocessReconstructsTo("#define VALUE\n"
                                 "int i = VALUE;",
                                 "int i = ;");
        preprocessReconstructsTo("#define VALUE\n"
                                 "int i =VALUE;",
                                 "int i =;");
    }
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

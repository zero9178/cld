#include <catch.hpp>

#include <llvm/ADT/ScopeExit.h>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>
#include <cld/Support/Filesystem.hpp>
#include <cld/Support/Text.hpp>

#include "TestConfig.hpp"

#define preprocessResult(source)                                                        \
    [](const std::string& str)                                                          \
    {                                                                                   \
        std::string storage;                                                            \
        llvm::raw_string_ostream ss(storage);                                           \
        bool errorsOccurred = false;                                                    \
        auto options = cld::LanguageOptions::native();                                  \
        auto tokens = cld::Lexer::tokenize(str, &options, &ss, &errorsOccurred);        \
        UNSCOPED_INFO(ss.str());                                                        \
        REQUIRE_FALSE(errorsOccurred);                                                  \
        auto result = cld::PP::preprocess(std::move(tokens), {}, &ss, &errorsOccurred); \
        UNSCOPED_INFO(ss.str());                                                        \
        REQUIRE_FALSE(errorsOccurred);                                                  \
        return result;                                                                  \
    }(source)

#define preprocessResultWith(source, option)                                \
    [](const std::string& str, cld::PP::Options options)                    \
    {                                                                       \
        std::string storage;                                                \
        llvm::raw_string_ostream ss(storage);                               \
        auto l = cld::LanguageOptions::native();                            \
        auto tokens = cld::Lexer::tokenize(str, &l, &ss);                   \
        UNSCOPED_INFO(ss.str());                                            \
        REQUIRE_THAT(ss.str(), ProducesNoErrors());                         \
        auto result = cld::PP::preprocess(std::move(tokens), options, &ss); \
        UNSCOPED_INFO(ss.str());                                            \
        REQUIRE_THAT(ss.str(), ProducesNoErrors());                         \
        return result;                                                      \
    }(source, option)

#define preprocessReconstructsTo(source, resultSource)                                                  \
    do                                                                                                  \
    {                                                                                                   \
        std::string storage;                                                                            \
        llvm::raw_string_ostream ss(storage);                                                           \
        auto options = cld::LanguageOptions::native();                                                  \
        auto tokens = cld::Lexer::tokenize(source, &options, &ss);                                      \
        UNSCOPED_INFO(ss.str());                                                                        \
        REQUIRE(ss.str().empty());                                                                      \
        auto ret = cld::PP::preprocess(std::move(tokens), {}, &ss);                                     \
        UNSCOPED_INFO(ss.str());                                                                        \
        REQUIRE(ss.str().empty());                                                                      \
        auto str = cld::PP::reconstruct(ret.data().data(), ret.data().data() + ret.data().size(), ret); \
        CHECK_THAT(str, ProducesLines(resultSource));                                                   \
    } while (0)

#define PP_OUTPUTS_WITH(input, match)                             \
    do                                                            \
    {                                                             \
        std::string s;                                            \
        llvm::raw_string_ostream ss(s);                           \
        auto options = cld::LanguageOptions::native();            \
        auto tokens = cld::Lexer::tokenize(input, &options, &ss); \
        UNSCOPED_INFO(ss.str());                                  \
        REQUIRE(ss.str().empty());                                \
        cld::PP::preprocess(std::move(tokens), {}, &ss);          \
        CHECK_THAT(s, match);                                     \
        if (!s.empty())                                           \
        {                                                         \
            tokens = cld::Lexer::tokenize(input, &options, &ss);  \
            cld::PP::preprocess(std::move(tokens));               \
        }                                                         \
    } while (0)

#define PP_OUTPUTS_WITH_OPTIONS(input, match, options)        \
    do                                                        \
    {                                                         \
        std::string s;                                        \
        llvm::raw_string_ostream ss(s);                       \
        auto l = cld::LanguageOptions::native();              \
        auto tokens = cld::Lexer::tokenize(input, &l, &ss);   \
        UNSCOPED_INFO(ss.str());                              \
        REQUIRE(ss.str().empty());                            \
        cld::PP::preprocess(std::move(tokens), options, &ss); \
        CHECK_THAT(s, match);                                 \
        if (!s.empty())                                       \
        {                                                     \
            tokens = cld::Lexer::tokenize(input, &l, &ss);    \
            cld::PP::preprocess(std::move(tokens), options);  \
        }                                                     \
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
        auto options = cld::LanguageOptions::native();
        auto ret = cld::Lexer::tokenize(m_source, &options, nullptr, &errors);
        REQUIRE_FALSE(errors);
        std::vector<cld::Lexer::PPToken> retWithoutNewline;
        std::vector<cld::Lexer::PPToken> argWithoutNewline;
        auto newLineFilter = [](const cld::Lexer::PPToken& token)
        { return token.getTokenType() == cld::Lexer::TokenType::Newline; };
        std::remove_copy_if(ret.data().begin(), ret.data().end(), std::back_inserter(retWithoutNewline), newLineFilter);
        std::remove_copy_if(arg.data().begin(), arg.data().end(), std::back_inserter(argWithoutNewline), newLineFilter);
        return std::equal(retWithoutNewline.begin(), retWithoutNewline.end(), argWithoutNewline.begin(),
                          argWithoutNewline.end(),
                          [&](const cld::Lexer::PPToken& lhs, const cld::Lexer::PPToken& rhs)
                          {
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

TEST_CASE("PP Unknown and non directives", "[PP]")
{
    SECTION("Non directive")
    {
        PP_OUTPUTS_WITH("#5non directive", ProducesNoErrors());
        PP_OUTPUTS_WITH("#", ProducesNoErrors());
    }
    SECTION("Unknown directive")
    {
        PP_OUTPUTS_WITH("#non directive\n", ProducesError(N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE, "'non'"));
    }
}

TEST_CASE("PP Define", "[PP]")
{
    SECTION("Parsing")
    {
        using namespace cld::Errors::Parser;
        SECTION("Simple")
        {
            PP_OUTPUTS_WITH("#define ID 5\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define 5\n", ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'5'"));
            PP_OUTPUTS_WITH("#define\n", ProducesError(EXPECTED_N_AFTER_N, "identifier", "'define'"));
            PP_OUTPUTS_WITH("#define ID+\n", ProducesError(WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION));
        }
        SECTION("Empty identifier list")
        {
            PP_OUTPUTS_WITH("#define ID()\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID() 5\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(\n", ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        }
        SECTION("Ellipse only")
        {
            PP_OUTPUTS_WITH("#define ID(...)\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(...) 5\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(...\n",
                            ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        }
        SECTION("Simple identifier list")
        {
            PP_OUTPUTS_WITH("#define ID(a)\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(a) 5\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(5\n", ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'5'"));
            PP_OUTPUTS_WITH("#define ID(a\n", ProducesError(EXPECTED_N, "')'") && ProducesNote(TO_MATCH_N_HERE, "'('"));
        }
        SECTION("Multiple identifiers")
        {
            PP_OUTPUTS_WITH("#define ID(a,b,c)\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(a,)\n", ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "')'"));
            PP_OUTPUTS_WITH("#define ID(a,\n", ProducesError(EXPECTED_N, "identifier"));
            PP_OUTPUTS_WITH("#define ID(a,5) 5\n", ProducesError(EXPECTED_N_INSTEAD_OF_N, "identifier", "'5'"));
            PP_OUTPUTS_WITH("#define ID(a,b,c,...)\n", ProducesNoErrors());
            PP_OUTPUTS_WITH("#define ID(a,b,a)\n", ProducesError(REDEFINITION_OF_MACRO_PARAMETER_N, "'a'")
                                                       && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        }
    }
    SECTION("6.10.3.2 Duplicates")
    {
        PP_OUTPUTS_WITH("#define macroName\n#define macroName\n",
                        ProducesWarning(N_REDEFINED, "'macroName'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName()\n#define macroName()\n",
                        ProducesWarning(N_REDEFINED, "'macroName'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName(a)\n#define macroName(a)\n",
                        ProducesWarning(N_REDEFINED, "'macroName'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName(...)\n#define macroName(...)\n",
                        ProducesWarning(N_REDEFINED, "'macroName'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName ad\n#define macroName ad\n",
                        ProducesWarning(N_REDEFINED, "'macroName'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName ad\n#define macroName a\n",
                        ProducesError(REDEFINITION_OF_MACRO_N, "'macroName'")
                            && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName()\n#define macroName\n",
                        ProducesError(REDEFINITION_OF_MACRO_N, "'macroName'")
                            && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        PP_OUTPUTS_WITH("#define macroName(...)\n#define macroName()\n",
                        ProducesError(REDEFINITION_OF_MACRO_N, "'macroName'")
                            && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    }
    SECTION("6.10.3.5 __VA_ARGS__ not allowed")
    {
        PP_OUTPUTS_WITH("#define macroName __VA_ARGS__\n", ProducesError(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST));
        PP_OUTPUTS_WITH("#define macroName() __VA_ARGS__\n", ProducesError(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST));
        PP_OUTPUTS_WITH("#define macroName(a) __VA_ARGS__\n", ProducesError(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST));
        PP_OUTPUTS_WITH("#define macroName(...) __VA_ARGS__\n", ProducesNoErrors());
        PP_OUTPUTS_WITH("#define macroName(a,...) __VA_ARGS__\n", ProducesNoErrors());
    }
    SECTION("6.10.8.4 Defining builtin macros")
    {
        constexpr std::array PREDEFINED_MACRO_NAMES = {
            "__DATE__",         "__FILE__", "__LINE__", "__STDC__", "__STDC_HOSTED__", "__STDC_MB_MIGHT_NEQ_WC__",
            "__STDC_VERSION__", "__TIME__"};
        for (std::string_view iter : PREDEFINED_MACRO_NAMES)
        {
            DYNAMIC_SECTION(iter)
            {
                PP_OUTPUTS_WITH(
                    "#define " + cld::to_string(iter) + "\n",
                    ProducesError(DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED, '\'' + cld::to_string(iter) + '\''));
            }
        }
    }
    SECTION("6.10.8.4 Undefining builtin macros")
    {
        constexpr std::array PREDEFINED_MACRO_NAMES = {
            "__DATE__",         "__FILE__", "__LINE__", "__STDC__", "__STDC_HOSTED__", "__STDC_MB_MIGHT_NEQ_WC__",
            "__STDC_VERSION__", "__TIME__"};
        for (std::string_view iter : PREDEFINED_MACRO_NAMES)
        {
            DYNAMIC_SECTION(iter)
            {
                PP_OUTPUTS_WITH(
                    "#undef " + cld::to_string(iter) + "\n",
                    ProducesError(UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED, '\'' + cld::to_string(iter) + '\''));
            }
        }
    }
    PP_OUTPUTS_WITH("#define defined\n", ProducesError(DEFINED_CANNOT_BE_USED_AS_MACRO_NAME));
    PP_OUTPUTS_WITH("#define A(x) # a x\n", ProducesError(EXPECTED_AN_ARGUMENT_AFTER_POUND));
    PP_OUTPUTS_WITH("#define A # a x\n", ProducesNoErrors());
    PP_OUTPUTS_WITH("#define A(x) ## x\n",
                    ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST));
    PP_OUTPUTS_WITH("#define A ## x\n",
                    ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST));
    PP_OUTPUTS_WITH("#define A(x) x ##\n", ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST));
    PP_OUTPUTS_WITH("#define A x ##\n", ProducesError(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST));
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
        auto ret = preprocessResult("#define FUNC (1 + 3)\nint main(void) {\n    return FUNC;\n}");
        CHECK_THAT(ret, ProducesPP("int main(void) {\n    return (1 + 3);\n}"));
        REQUIRE(ret.data().size() == 14);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 6, 0));
        CHECK(haveMacroID(ret.data().begin() + 6, ret.data().begin() + 7, 0));
        CHECK(haveMacroID(ret.data().begin() + 7, ret.data().begin() + 12, 1));
        CHECK(haveMacroID(ret.data().begin() + 12, ret.data().end(), 0));
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
        REQUIRE(ret.data().size() == 20);
        CHECK(haveMacroID(ret.data().begin(), ret.data().begin() + 6, 0));
        CHECK(haveMacroID(ret.data().begin() + 6, ret.data().begin() + 7, 0));
        CHECK(haveMacroID(ret.data().begin() + 7, ret.data().begin() + 12, 2));
        CHECK(haveMacroID(ret.data().begin() + 12, ret.data().begin() + 13, 1));
        CHECK(haveMacroID(ret.data().begin() + 13, ret.data().begin() + 18, 3));
        CHECK(haveMacroID(ret.data().begin() + 18, ret.data().end(), 0));
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
    SECTION("defined isn't special")
    {
        auto ret = preprocessResult("#define BAR 5\n"
                                    "#define MACRO BAR)\n"
                                    "defined(MACRO");
        CHECK_THAT(ret, ProducesPP("defined(5)"));
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
        CHECK_THAT(ret, ProducesPP("1"));
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
        SECTION("Normal")
        {
            auto ret = preprocessResult("__FILE__");
            CHECK_THAT(ret, ProducesPP("\"<stdin>\""));
        }
        SECTION("Cross file")
        {
            auto scope = createInclude("A.h", "#define MACRO __FILE__\n");
            auto ret = preprocessResult("#include \"A.h\"\n"
                                        "MACRO");
            CHECK_THAT(ret, ProducesPP("\"<stdin>\""));
        }
        SECTION("#line directive")
        {
            auto cwd = cld::fs::current_path();
            auto scope = createInclude("A.h", "__FILE__");
            auto ret = preprocessResult("#line 5 \"main.c\"\n"
                                        "__FILE__\n"
                                        "#include \"A.h\"\n");
            REQUIRE(ret.data().size() == 2);
            CHECK(ret.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
            CHECK(ret.data()[1].getTokenType() == cld::Lexer::TokenType::StringLiteral);
            CHECK(ret.data()[0].getValue() == "main.c");
            CHECK(cld::fs::equivalent(ret.data()[1].getValue().data(), cwd / "A.h"));
        }
    }
    SECTION("__LINE__")
    {
        SECTION("Normal")
        {
            auto ret = preprocessResult("__LINE__\n\n__LINE__");
            CHECK_THAT(ret, ProducesPP("1\n\n3"));
        }
        SECTION("Cross file")
        {
            auto scope = createInclude("A.h", "#define MACRO __LINE__\n");
            auto ret = preprocessResult("#include \"A.h\"\n"
                                        "MACRO");
            CHECK_THAT(ret, ProducesPP("2"));
        }
        SECTION("#line directive")
        {
            auto scope = createInclude("A.h", "__LINE__");
            auto ret = preprocessResult("#line 5\n"
                                        "__LINE__\n"
                                        "#include \"A.h\"\n");
            REQUIRE(ret.data().size() == 2);
            CHECK(ret.data()[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
            CHECK(ret.data()[1].getTokenType() == cld::Lexer::TokenType::PPNumber);
            CHECK(ret.data()[0].getValue() == "5");
            CHECK(ret.data()[1].getValue() == "1");
        }
    }
}

TEST_CASE("PP Function like Macros", "[PP]")
{
    SECTION("Recursive with argument name")
    {
        auto ret = preprocessResult("#define GLOBAL(t, v) v\n"
                                    "#define vfsList GLOBAL(sqlite3_vfs*,vfsList)\n"
                                    "vfsList");
        CHECK_THAT(ret, ProducesPP("vfsList"));
    }
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
        auto subs = ret.getSubstitutions();
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
                        ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N, "'max'", 2, 1));
        PP_OUTPUTS_WITH("#define max(a, b,...) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(5);",
                        ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N, "'max'", 2, 1));
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(5,7,10);",
                        ProducesError(TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N, "'max'", 2, 3));
    }
    SECTION("No closing )")
    {
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max(5",
                        ProducesError(cld::Errors::Parser::EXPECTED_N, "')'")
                            && ProducesNote(cld::Notes::TO_MATCH_N_HERE, "'('"));
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max((5,",
                        ProducesError(cld::Errors::Parser::EXPECTED_N, "')'")
                            && ProducesNote(cld::Notes::TO_MATCH_N_HERE, "'('"));
        PP_OUTPUTS_WITH("#define max(a, b) ((a) > (b) ? (a) : (b))\n"
                        "int i = max((5,)",
                        ProducesError(cld::Errors::Parser::EXPECTED_N, "')'")
                            && ProducesNote(cld::Notes::TO_MATCH_N_HERE, "'('"));
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
                        ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N, "'max'", 2, 1));
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
        auto subs = ret.getSubstitutions();
        REQUIRE(subs.size() == 3);
        REQUIRE(std::holds_alternative<cld::Source::Stringification>(subs[2]));
        auto& stringify = cld::get<cld::Source::Stringification>(subs[2]);
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
        auto subs = ret.getSubstitutions();
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
                            && ProducesNote(WHEN_CONCATENATING_N_AND_N, "'.'", "", "'foo'"));
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

TEST_CASE("PP includes UTF8", "[PP]")
{
    auto scope = createInclude("貓🍌.h", "#define MACRO 1\n");
    CHECK(cld::fs::exists(u8"貓🍌.h"));
    auto ret = preprocessResult("#include \"貓🍌.h\"\n"
                                "MACRO");
    CHECK_THAT(ret, ProducesPP("1"));
}

TEST_CASE("PP includes", "[PP]")
{
    SECTION("Absolute path")
    {
        auto scope = createInclude("Resources/TestInclude.h", "#define MACRO 1\n");
        auto cwd = cld::fs::current_path();
        auto ret = preprocessResult("#include \"" + cwd.string()
                                    + "/Resources/TestInclude.h\"\n"
                                      "MACRO\n");
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("Relative path")
    {
        auto file1 = createInclude("A.h", "#include \"B.h\"\n");
        auto file2 = createInclude("B.h", "#define MACRO 1\n");
        auto ret = preprocessResult("#include \"A.h\"\n"
                                    "MACRO");
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("Quoted before non quoted")
    {
        auto cwd = cld::fs::current_path();
        auto file1 = createInclude("Quoted/A.h", "#define MACRO 1\n");
        auto file2 = createInclude("Unquoted/A.h", "#define MACRO 0\n");
        cld::PP::Options options;
        options.includeQuoteDirectories.push_back((cwd / "Quoted").string());
        options.includeDirectories.push_back((cwd / "Unquoted").string());
        auto ret = preprocessResultWith("#include \"A.h\"\n"
                                        "MACRO",
                                        options);
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("Quoted also looks in non quoted")
    {
        auto cwd = cld::fs::current_path();
        auto file2 = createInclude("Unquoted/A.h", "#define MACRO 1\n");
        cld::PP::Options options;
        options.includeDirectories.push_back((cwd / "Unquoted").string());
        auto ret = preprocessResultWith("#include \"A.h\"\n"
                                        "MACRO",
                                        options);
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("Relative before quoted")
    {
        auto cwd = cld::fs::current_path();
        auto file2 = createInclude("A.h", "#define MACRO 1\n");
        auto file1 = createInclude("Quoted/A.h", "#define MACRO 0\n");
        cld::PP::Options options;
        options.includeQuoteDirectories.push_back((cwd / "Quoted").string());
        auto ret = preprocessResultWith("#include \"A.h\"\n"
                                        "MACRO",
                                        options);
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("Non quoted")
    {
        auto cwd = cld::fs::current_path();
        auto file2 = createInclude("Quoted/A.h", "#define MACRO 1\n");
        cld::PP::Options options;
        options.includeDirectories.push_back((cwd / "Quoted").string());
        auto ret = preprocessResultWith("#include <A.h>\n"
                                        "MACRO",
                                        options);
        CHECK_THAT(ret, ProducesPP("1"));
    }
    SECTION("Non quoted doesn't look in quoted")
    {
        auto cwd = cld::fs::current_path();
        auto file2 = createInclude("Quoted/A.h", "#define MACRO 1\n");
        PP_OUTPUTS_WITH("#include <A.h>\n", ProducesError(FILE_NOT_FOUND, "A.h"));
    }
    SECTION("Changed __FILE__")
    {
        auto cwd = cld::fs::current_path();
        auto file1 = createInclude("A.h", "#include \"B.h\"\n"
                                          "__FILE__");
        auto file2 = createInclude("B.h", "__FILE__");
        auto ret = preprocessResult("#include \"A.h\"\n"
                                    "__FILE__");
        REQUIRE(ret.data().size() == 3);
        CHECK(ret.data()[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        CHECK(ret.data()[1].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        CHECK(ret.data()[2].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        CHECK(cld::fs::equivalent(ret.data()[0].getValue().data(), cwd / "B.h"));
        CHECK(cld::fs::equivalent(ret.data()[1].getValue().data(), cwd / "A.h"));
        CHECK(ret.data()[2].getValue() == "<stdin>");
    }
    SECTION("System include does not emit warnings")
    {
        auto cwd = cld::fs::current_path();
        auto file1 = createInclude("Quoted/A.h", "#define MACRO 1\n");
        cld::PP::Options options;
        options.includeDirectories.push_back((cwd / "Quoted").string());
        PP_OUTPUTS_WITH_OPTIONS("#define MACRO 1\n"
                                "#include <A.h>\n",
                                ProducesWarning(N_REDEFINED, "'MACRO'"), options);
        options.systemDirectories = {(cwd / "Quoted").string()};
        options.includeDirectories.clear();
        PP_OUTPUTS_WITH_OPTIONS("#define MACRO 1\n"
                                "#include <A.h>\n",
                                !ProducesWarning(N_REDEFINED, "'MACRO'"), options);
        auto file2 = createInclude("B.h", "#define MACRO 1\n"
                                          "#include \"./Quoted/A.h\"\n");
        options.systemDirectories = {cwd.string()};
        PP_OUTPUTS_WITH_OPTIONS("#include <A.h>\n", !ProducesWarning(N_REDEFINED, "'MACRO'"), options);
    }
    SECTION("Non existent directory")
    {
        auto cwd = cld::fs::current_path();
        auto file1 = createInclude("Quoted/A.h", "#define MACRO 1\n");
        cld::PP::Options options;
        options.systemDirectories.push_back((cwd / "DirectoryThatHopefullyReallyDoesntExit/ThatdBeAwkward").string());
        options.includeDirectories.push_back((cwd / "Quoted").string());
        PP_OUTPUTS_WITH_OPTIONS("#define MACRO 1\n"
                                "#include <A.h>\n",
                                ProducesWarning(N_REDEFINED, "'MACRO'"), options);
    }
    SECTION("File not found")
    {
        PP_OUTPUTS_WITH("#include \"hello\"\n", ProducesError(FILE_NOT_FOUND, "hello"));
    }
    SECTION("Could not open file")
    {
        auto cwd = cld::fs::current_path();
        PP_OUTPUTS_WITH("#include \"" + cwd.string() + "/FileThatDoesNotExist\"\n",
                        ProducesError(FILE_NOT_FOUND, cwd.string() + "/FileThatDoesNotExist"));
    }
    SECTION("Empty")
    {
        PP_OUTPUTS_WITH("#include\n", ProducesError(EXPECTED_A_FILENAME_AFTER_INCLUDE));
    }
    SECTION("Computed")
    {
        SECTION("Quoted")
        {
            auto file1 = createInclude("A.h", "#define MACRO 1\n");
            auto ret = preprocessResult("#define PATH \"A.h\"\n"
                                        "#include PATH\n"
                                        "MACRO");
            CHECK_THAT(ret, ProducesPP("1"));
        }
        SECTION("Non quoted")
        {
            auto cwd = cld::fs::current_path();
            auto file1 = createInclude("A.h", "#define MACRO 1\n");
            cld::PP::Options options;
            options.includeDirectories.emplace_back(cwd.string());
            auto ret = preprocessResultWith("#define PATH <A.h>\n"
                                            "#include PATH\n"
                                            "MACRO",
                                            options);
            CHECK_THAT(ret, ProducesPP("1"));
        }
        PP_OUTPUTS_WITH("#define EMPTY()\n"
                        "#include EMPTY()\n",
                        ProducesError(EXPECTED_A_FILENAME_AFTER_INCLUDE));
        PP_OUTPUTS_WITH("#define MACRO w\n"
                        "#include MACRO\n",
                        ProducesError(EXPECTED_A_FILENAME_AFTER_INCLUDE_2));
        PP_OUTPUTS_WITH("#define MACRO <TEST\n"
                        "#include MACRO\n",
                        ProducesError(cld::Errors::Lexer::UNTERMINATED_INCLUDE_DIRECTIVE));
        PP_OUTPUTS_WITH("#define MACRO <TEST> w\n"
                        "#include MACRO\n",
                        ProducesError(EXTRA_TOKENS_AFTER_INCLUDE));
        PP_OUTPUTS_WITH("#define MACRO \"TEST\" w\n"
                        "#include MACRO\n",
                        ProducesError(EXTRA_TOKENS_AFTER_INCLUDE));
    }
}

TEST_CASE("PP line directive", "[PP]")
{
    SECTION("With number")
    {
        auto ret = preprocessResult("#line 5\n");
        REQUIRE(ret.getFiles().size() == 2);
        CHECK(ret.getFiles()[1].path == "<stdin>");
        REQUIRE(ret.getFiles()[1].lineAndFileMapping.size() == 1);
        auto& [startLine, fileName, newLine] = ret.getFiles()[1].lineAndFileMapping[0];
        CHECK(startLine == 2);
        CHECK(!fileName);
        CHECK(newLine == 5);
    }
    SECTION("With file too")
    {
        auto ret = preprocessResult("#line 5 \"main.c\"\n");
        REQUIRE(ret.getFiles().size() == 2);
        CHECK(ret.getFiles()[1].path == "<stdin>");
        REQUIRE(ret.getFiles()[1].lineAndFileMapping.size() == 1);
        auto& [startLine, fileName, newLine] = ret.getFiles()[1].lineAndFileMapping[0];
        CHECK(startLine == 2);
        REQUIRE(fileName);
        CHECK(*fileName == "main.c");
        CHECK(newLine == 5);
    }
    SECTION("String is parsed as string literal")
    {
        PP_OUTPUTS_WITH("#line 5 \"\\9\"\n", ProducesError(cld::Errors::Lexer::INVALID_OCTAL_CHARACTER, "9"));
        PP_OUTPUTS_WITH("#line 5 \"as\ndawd\"\n",
                        ProducesError(cld::Errors::Lexer::NEWLINE_IN_STRING_LITERAL_USE_BACKLASH_N));
    }
    SECTION("Non decimal")
    {
        PP_OUTPUTS_WITH("#line 0x5\n", ProducesError(NUMBER_MUST_BE_IN_DECIMAL_IN_LINE_DIRECTIVE));
        PP_OUTPUTS_WITH("#line 0b5\n", ProducesError(NUMBER_MUST_BE_IN_DECIMAL_IN_LINE_DIRECTIVE));
    }
    SECTION("Illegal values")
    {
        PP_OUTPUTS_WITH("#line 0\n", ProducesError(NUMBER_MUST_NOT_BE_ZERO_IN_LINE_DIRECTIVE));
        PP_OUTPUTS_WITH("#line 2147483648\n", ProducesError(NUMBER_MUST_NOT_BE_GREATER_THAN_X_IN_LINE_DIRECTIVE));
        PP_OUTPUTS_WITH("#line 34463423545245325234523423423423523\n",
                        ProducesError(NUMBER_MUST_NOT_BE_GREATER_THAN_X_IN_LINE_DIRECTIVE));
    }
    SECTION("Computed")
    {
        PP_OUTPUTS_WITH("#define EMPTY\n"
                        "#line EMPTY\n",
                        ProducesError(EXPECTED_A_NUMBER_AFTER_LINE));
        PP_OUTPUTS_WITH("#define MACRO w\n"
                        "#line MACRO\n",
                        ProducesError(EXPECTED_A_NUMBER_AFTER_LINE));
        PP_OUTPUTS_WITH("#define MACRO 5 w\n"
                        "#line MACRO\n",
                        ProducesError(EXPECTED_END_OF_LINE_OR_STRING_AFTER_NUMBER_IN_LINE));
        PP_OUTPUTS_WITH("#define MACRO 5 L\"5\"\n"
                        "#line MACRO\n",
                        ProducesError(STRING_MUST_BE_NORMAL_IN_LINE_DIRECTIVE));
    }
}

TEST_CASE("PP conditional inclusion", "[PP]")
{
    SECTION("ifdef/ifndef")
    {
        SECTION("Simple")
        {
            auto ret = preprocessResult("#define MACRO\n"
                                        "#ifdef MACRO\n"
                                        "#define RESULT 1\n"
                                        "#endif\n"
                                        "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            ret = preprocessResult("#define MACRO\n"
                                   "#ifndef MACRO\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("RESULT"));
            ret = preprocessResult("#ifndef MACRO\n"
                                   "#define MACRO 1\n"
                                   "#endif\n"
                                   "MACRO");
            CHECK_THAT(ret, ProducesPP("1"));
        }
        SECTION("else")
        {
            auto ret = preprocessResult("#define MACRO\n"
                                        "#ifndef MACRO\n"
                                        "#define RESULT 0\n"
                                        "#else\n"
                                        "#define RESULT 1\n"
                                        "#endif\n"
                                        "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            ret = preprocessResult("#ifdef MACRO\n"
                                   "#define RESULT 0\n"
                                   "#else\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
        }
    }
    SECTION("if")
    {
        SECTION("Simple")
        {
            auto ret = preprocessResult("#if 1\n"
                                        "#define RESULT 1\n"
                                        "#endif\n"
                                        "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
        }
        SECTION("Identifiers are 0")
        {
            auto ret = preprocessResult("#if !TEST\n"
                                        "#define RESULT 1\n"
                                        "#endif\n"
                                        "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            PP_OUTPUTS_WITH("#if ID.m\n"
                            "#endif\n",
                            ProducesError(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N, "identifier", "'m'"));
        }
        SECTION("Unary defined")
        {
            auto ret = preprocessResult("#define MACRO\n"
                                        "#if defined MACRO\n"
                                        "#define RESULT 1\n"
                                        "#endif\n"
                                        "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            ret = preprocessResult("#define MACRO\n"
                                   "#if defined(MACRO)\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            ret = preprocessResult("#if defined MACRO\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("RESULT"));
            ret = preprocessResult("#if defined(MACRO)\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("RESULT"));
            ret = preprocessResult("#if defined(__FILE__)\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            ret = preprocessResult("#if defined(__LINE__)\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            ret = preprocessResult("#define BAR\n"
                                   "#define FOO defined(BAR)\n"
                                   "#if FOO\n"
                                   "#define RESULT 1\n"
                                   "#endif\n"
                                   "RESULT");
            CHECK_THAT(ret, ProducesPP("1"));
            PP_OUTPUTS_WITH("#define BAR\n"
                            "#define FOO defined(BAR)\n"
                            "#if FOO\n"
                            "#define RESULT 1\n"
                            "#endif\n"
                            "RESULT",
                            ProducesWarning(MACRO_EXPANSION_PRODUCING_DEFINED_IS_NOT_PORTABLE));
        }
        PP_OUTPUTS_WITH("#define BAR\n"
                        "#if BAR\n"
                        "\n"
                        "#endif\n",
                        ProducesError(EXPECTED_AN_EXPRESSION_AFTER_IF));
        PP_OUTPUTS_WITH("#if 0\n"
                        "\n"
                        "#elif\n"
                        "\n"
                        "#endif\n",
                        ProducesError(EXPECTED_AN_EXPRESSION_AFTER_ELIF));
        PP_OUTPUTS_WITH("#define BAR\n"
                        "#if 0\n"
                        "\n"
                        "#elif BAR\n"
                        "\n"
                        "#endif\n",
                        ProducesError(EXPECTED_AN_EXPRESSION_AFTER_ELIF_2));
    }
}

TEST_CASE("PP error", "[PP]")
{
    PP_OUTPUTS_WITH("#error aiwzdbwauzdbwaizadbw\n", ProducesError(ERROR_ENCOUNTERED));
    PP_OUTPUTS_WITH("#if 0\n"
                    "#error aiwzdbwauzdbwaizadbw\n"
                    "#endif\n",
                    ProducesNoErrors());
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
    SECTION("Multiline normal text")
    {
        preprocessReconstructsTo("a\n"
                                 "b\n"
                                 "c",
                                 "a\n"
                                 "b\n"
                                 "c");
    }
    SECTION("Multiline text with macro line")
    {
        preprocessReconstructsTo("#define MACRO b\n"
                                 "a\n"
                                 "MACRO\n"
                                 "c",
                                 "a\n"
                                 "b\n"
                                 "c");
    }
}

#include "catch.hpp"

#include <Frontend/Compiler/Diagnostic.hpp>
#include <Frontend/Compiler/Lexer.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

#include "TestConfig.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"

// TODO: Use __VA_OPT__(,) in C++20
#define CREATE_ERROR(variableName, format, ...)                       \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, ##__VA_ARGS__>(::cld::Severity::Error, "")

#define CREATE_NOTE(variableName, format, ...)                        \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, ##__VA_ARGS__>(::cld::Severity::Note, "")

#define CREATE_WARNING(variableName, cliName, format, ...)            \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, ##__VA_ARGS__>(::cld::Severity::Warning, cliName)

namespace
{
cld::SourceInterface* interface;

[[nodiscard]] cld::Lexer::CTokenIterator lexes(std::string_view code,
                                               const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto temp = cld::Lexer::tokenize(code, options, &ss);
    UNSCOPED_INFO(buffer);
    REQUIRE(buffer.empty());
    static cld::CSourceObject csourceObject;
    csourceObject = cld::Lexer::toCTokens(temp);
    interface = &csourceObject;
    return csourceObject.data().data();
}

[[nodiscard]] cld::Lexer::PPTokenIterator pplexes(std::string_view code,
                                                  const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    static cld::PPSourceObject ppsourceObject;
    ppsourceObject = cld::Lexer::tokenize(code, options, &ss);
    UNSCOPED_INFO(buffer);
    REQUIRE(buffer.empty());
    interface = &ppsourceObject;
    return ppsourceObject.data().data();
}

[[nodiscard]] cld::Lexer::PPTokenIterator pp(std::string_view code,
                                             const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    static cld::PPSourceObject ppsourceObject;
    ppsourceObject = cld::Lexer::tokenize(code, options, &ss);
    UNSCOPED_INFO(buffer);
    REQUIRE(buffer.empty());
    ppsourceObject = cld::PP::preprocess(std::move(ppsourceObject), &ss);
    UNSCOPED_INFO(buffer);
    REQUIRE(buffer.empty());
    interface = &ppsourceObject;
    return ppsourceObject.data().data();
}

} // namespace

namespace
{
CREATE_ERROR(pointLocationTest, "");
CREATE_ERROR(singleArgumentTest, "Here %0");
CREATE_ERROR(englishPluralTest, "I want %0 cake%s0 now");
CREATE_WARNING(warningTest, "warning-test", "A warning");
CREATE_NOTE(noteTest, "A note");
CREATE_ERROR(underline, "Highlight!", cld::Underline<0>);
CREATE_ERROR(twoUnderlines, "Highlight!", cld::Underline<0>, cld::Underline<1, '^'>);
CREATE_ERROR(pointAt, "Highlight!", cld::PointAt<0>);
CREATE_ERROR(insertAfter, "Highlight!", cld::InsertAfter<0>);
CREATE_ERROR(insertAfterText, "Highlight!", cld::InsertAfter<0, 1>);
CREATE_ERROR(threeInsertsWithText, "Highlight!", cld::InsertAfter<0, 1>, cld::InsertAfter<2, 3>,
             cld::InsertAfter<4, 5>);
CREATE_ERROR(annotate, "Highlight!", cld::Annotate<0, 1>);
CREATE_ERROR(annotateWithInsert, "Highlight!", cld::Annotate<0, 1>, cld::InsertAfter<2, 1>);
} // namespace

using namespace Catch::Matchers;

TEST_CASE("Diag in format arguments", "[diag]")
{
    SECTION("Specifying location")
    {
        STATIC_REQUIRE(pointLocationTest.getSize() == 0);
        const auto* tokens = lexes("text text2");
        auto message = pointLocationTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:1: error:"));
        llvm::errs() << message;
        message = pointLocationTest.args(4, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:5: error:"));
        llvm::errs() << message;
        message = pointLocationTest.args(std::pair{*tokens, 5}, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:6: error:"));
        llvm::errs() << message;
        message = pointLocationTest.args(std::pair{9, *tokens}, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:10: error:"));
        llvm::errs() << message;
    }
    SECTION("Simple argument")
    {
        STATIC_REQUIRE(singleArgumentTest.getSize() == 1);
        const auto* tokens = lexes("text");
        auto message = singleArgumentTest.args(*tokens, *interface, std::string_view("we go"));
        CHECK_THAT(message.getText(), Contains("Here we go"));
        llvm::errs() << message;
        message = singleArgumentTest.args(*tokens, *interface, 5);
        CHECK_THAT(message.getText(), Contains("Here 5"));
        llvm::errs() << message;
        std::string_view text = "we go";
        message = singleArgumentTest.args(*tokens, *interface, text);
        CHECK_THAT(message.getText(), Contains("Here we go"));
        llvm::errs() << message;
    }
    SECTION("English plural test")
    {
        STATIC_REQUIRE(englishPluralTest.getSize() == 1);
        const auto* tokens = lexes("text");
        auto number = GENERATE(0, 1, 2, 3, 4, 5);
        auto message = englishPluralTest.args(*tokens, *interface, number);
        if (number == 1)
        {
            CHECK_THAT(message.getText(), Contains("I want 1 cake now"));
            llvm::errs() << message;
        }
        else
        {
            CHECK_THAT(message.getText(), Contains("I want " + std::to_string(number) + " cakes now"));
            llvm::errs() << message;
        }
    }
}

TEST_CASE("Diag severity", "[diag]")
{
    const auto* tokens = lexes("text text2");
    SECTION("Error")
    {
        auto message = pointLocationTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:1: error:"));
        llvm::errs() << message;
    }
    SECTION("Warning")
    {
        auto message = warningTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:1: warning:"));
        auto options = cld::LanguageOptions::native();
        options.disabledWarnings.insert("warning-test");
        tokens = lexes("text text2", options);
        message = warningTest.args(*tokens, *interface);
        CHECK(message.getText().empty());
    }
    SECTION("Note")
    {
        auto message = noteTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:1:1: note:"));
        llvm::errs() << message;
    }
}

TEST_CASE("Diag line printing", "[diag]")
{
    SECTION("Simple")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = pointLocationTest.args(*begin, *interface);
        CHECK_THAT(message.getText(), Contains("1 | A series of"));
        llvm::errs() << message;
        message = pointLocationTest.args(*(begin + 1), *interface);
        CHECK_THAT(message.getText(), Contains("1 | A series of"));
        llvm::errs() << message;
        message = pointLocationTest.args(*(begin + 2), *interface);
        CHECK_THAT(message.getText(), Contains("1 | A series of"));
        llvm::errs() << message;
        message = pointLocationTest.args(*(begin + 3), *interface);
        CHECK_THAT(message.getText(), Contains("2 |  identifiers"));
        llvm::errs() << message;
    }
    SECTION("Multline")
    {
        const auto* begin = lexes("    A\\\nnt     text cont\\\ninues");
        auto message = pointLocationTest.args(*(begin + 1), *interface);
        CHECK_THAT(message.getText(), Contains("2 | nt     text cont"));
        llvm::errs() << message;
        message = pointLocationTest.args(*begin, *interface);
        CHECK_THAT(message.getText(), Contains("1 |     A") && Contains("2 | nt     text cont"));
        llvm::errs() << message;
        message = pointLocationTest.args(std::pair{0, 30}, *interface);
        CHECK_THAT(message.getText(),
                   Contains("1 |     A") && Contains("2 | nt     text cont") && Contains("3 | inues"));
        llvm::errs() << message;
    }
    SECTION("Unicode")
    {
        const auto* begin = lexes("\"\xe3\\\n\x80\xBA\"");
        auto message = pointLocationTest.args(*begin, *interface);
        CHECK_THAT(message.getText(), Contains("1 | \"�\\") && Contains("2 | ��\""));
        llvm::errs() << message;
    }
    SECTION("Newline")
    {
        const auto* begin = pplexes("A series of\n identifiers");
        auto message = pointLocationTest.args(
            std::make_pair(*(begin + 2), (begin + 2)->getOffset() + (begin + 2)->getLength()), *interface);
        CHECK_THAT(message.getText(), Contains(":1:12:") && Contains("1 | A series of") && !Contains("2 |"));
        llvm::errs() << message;
    }
}

TEST_CASE("Diag underline", "[diag]")
{
    SECTION("Word for word")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = underline.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("| ~"));
        llvm::errs() << message;
        message = underline.args(*begin, *interface, *(begin + 1));
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("|   ~~~~~~"));
        llvm::errs() << message;
        message = underline.args(*begin, *interface, *(begin + 2));
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("|          ~~"));
        llvm::errs() << message;
        message = underline.args(*begin, *interface, *(begin + 3));
        CHECK_THAT(message.getText(),
                   Contains("1 | A series of") && Contains("2 |  identifiers") && Contains("|  ~~~~~~~~~~~"));
        llvm::errs() << message;
    }
    SECTION("Range")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = underline.args(*begin, *interface, std::pair{*begin, *(begin + 3)});
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("2 |  identifiers")
                                          && Contains("| ~~~~~~~~~~~") && Contains("| ~~~~~~~~~~~~"));
        llvm::errs() << message;
    }
    SECTION("Unicode")
    {
        const auto* begin = lexes("\"🍌\" text");
        auto message = underline.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("1 | \"🍌\" text") && Contains("| ~~~"));
        llvm::errs() << message;
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        message = underline.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("| ~~~"));
        llvm::errs() << message;
    }
    SECTION("Overlapping")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = twoUnderlines.args(*begin, *interface, std::pair{*begin, *(begin + 3)}, *(begin + 1));
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("2 |  identifiers")
                                          && Contains("| ~~^^^^^^~~~") && Contains("| ~~~~~~~~~~~~"));
        llvm::errs() << message;
    }
}

TEST_CASE("Diag point at", "[diag]")
{
    SECTION("Word for word")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = pointAt.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("| ^"));
        llvm::errs() << message;
        message = pointAt.args(*begin, *interface, *(begin + 1));
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("|   ^^^^^^"));
        llvm::errs() << message;
        message = pointAt.args(*begin, *interface, *(begin + 2));
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("|          ^^"));
        llvm::errs() << message;
        message = pointAt.args(*begin, *interface, *(begin + 3));
        CHECK_THAT(message.getText(),
                   Contains("1 | A series of") && Contains("2 |  identifiers") && Contains("|  ^^^^^^^^^^^"));
        llvm::errs() << message;
    }
    SECTION("Range")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = pointAt.args(*begin, *interface, std::pair{*begin, *(begin + 3)});
        CHECK_THAT(message.getText(), Contains("1 | A series of") && Contains("2 |  identifiers")
                                          && Contains("| ^ ^^^^^^ ^^") && Contains("|  ^^^^^^^^^^^"));
        llvm::errs() << message;
    }
    SECTION("Unicode")
    {
        const auto* begin = lexes("\"🍌\" text");
        auto message = pointAt.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("1 | \"🍌\" text") && Contains("| ^^^"));
        llvm::errs() << message;
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        message = pointAt.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("| ^^^"));
        llvm::errs() << message;
    }
}

TEST_CASE("Diag insertion", "[diag]")
{
    SECTION("Simple non text insertion")
    {
        const auto* begin = lexes("A series of \n identifiers");
        auto message = insertAfter.args(*begin, *interface, *begin);
        CHECK_THAT(message.getText(), Contains("|  ^"));
        llvm::errs() << message;
        message = insertAfter.args(*begin, *interface, *(begin + 1));
        CHECK_THAT(message.getText(), Contains("|         ^"));
        llvm::errs() << message;
        message = insertAfter.args(*begin, *interface, *(begin + 2));
        CHECK_THAT(message.getText(), Contains("|            ^"));
        llvm::errs() << message;
        message = insertAfter.args(*(begin + 3), *interface, *(begin + 3));
        CHECK_THAT(message.getText(), Contains("|             ^"));
        llvm::errs() << message;
    }
    SECTION("After PP Newline")
    {
        const auto* begin = pplexes("A series of\n identifiers");
        auto message = insertAfter.args(*begin, *interface, *(begin + 3));
        CHECK_THAT(message.getText(), Contains("\n     >"));
        llvm::errs() << message;
    }
    SECTION("Simple with text insertion")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = insertAfterText.args(*begin, *interface, *begin, std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("|  ^") && Contains("|  text"));
        llvm::errs() << message;
        message = insertAfterText.args(*begin, *interface, *(begin + 1), std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("|         ^") && Contains("|         text"));
        llvm::errs() << message;
        message = insertAfterText.args(*begin, *interface, *(begin + 2), std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("|            ^") && Contains("|            text"));
        llvm::errs() << message;
        message = insertAfterText.args(*(begin + 3), *interface, *(begin + 3), std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("|             ^") && Contains("|             text"));
        llvm::errs() << message;
    }
    SECTION("After PP Newline with text")
    {
        const auto* begin = pplexes("A series of\n identifiers");
        auto message = insertAfterText.args(*begin, *interface, *(begin + 3), std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("\n     > text"));
        llvm::errs() << message;
    }
    SECTION("Multiple with text insertion")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message =
            threeInsertsWithText.args(*begin, *interface, *begin, std::string_view("Whitespace 1"), *(begin + 1),
                                      std::string_view("Whitespace 2"), *(begin + 2), std::string_view("Whitespace 3"));
        CHECK_THAT(message.getText(), Contains("|  ^      ^  ^") && Contains("|  |      |  Whitespace 3")
                                          && Contains("|  |      Whitespace 2") && Contains("|  Whitespace 1"));
        llvm::errs() << message;
        message = threeInsertsWithText.args(*begin, *interface, *begin, 1, *(begin + 1), 2, *(begin + 2), 3);
        CHECK_THAT(message.getText(), Contains("|  ^      ^  ^") && Contains("|  1      2  3"));
        llvm::errs() << message;
    }
}

TEST_CASE("Diag annotate", "[diag]")
{
    SECTION("Simple annotate")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = annotate.args(*begin, *interface, *begin, std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("| ^") && Contains("| |") && Contains("| text"));
        llvm::errs() << message;
        message = annotate.args(*begin, *interface, *(begin + 1), std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("|   ~~~~~~") && Contains("|      |") && Contains("|      text"));
        llvm::errs() << message;
        message = annotate.args(*begin, *interface, *(begin + 2), std::string_view("text"));
        CHECK_THAT(message.getText(),
                   Contains("|          ~~") && Contains("|           |") && Contains("|           text"));
        llvm::errs() << message;
        message = annotate.args(*(begin + 3), *interface, *(begin + 3), std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("|  ~~~~~~~~~~~") && Contains("|       |") && Contains("|       text"));
        llvm::errs() << message;
    }
    SECTION("Ranges")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = annotate.args(*begin, *interface, std::pair{*begin, *(begin + 2)}, std::string_view("text"));
        CHECK_THAT(message.getText(), Contains("| ~~~~~~~~~~~") && Contains("|      |") && Contains("|      text"));
        llvm::errs() << message;
    }
    SECTION("Mixed with insert")
    {
        const auto* begin = lexes("A series of\n identifiers");
        auto message = annotateWithInsert.args(*begin, *interface, *begin, std::string_view("text"), *(begin + 2));
        CHECK_THAT(message.getText(),
                   Contains("| ^          ^") && Contains("| |          text") && Contains("| text"));
        llvm::errs() << message;
    }
}

TEST_CASE("Diag line and file diretive")
{
    SECTION("Line change")
    {
        const auto* tokens = pp("#line 5\n"
                                "text text2");
        auto message = pointLocationTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), StartsWith("<stdin>:5:1:"));
        llvm::errs() << message;
    }
    SECTION("File change")
    {
        const auto* tokens = pp("#line 5 \"main.c\"\n"
                                "\n"
                                "text text2");
        auto message = pointLocationTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), StartsWith("main.c:6:1:"));
        llvm::errs() << message;
    }
}

#undef CREATE_ERROR
#undef CREATE_WARNING
#undef CREATE_NOTE
#pragma clang diagnostic pop

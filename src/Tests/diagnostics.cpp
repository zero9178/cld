#include "catch.hpp"

#include <Frontend/Compiler/Diagnostic.h>
#include <Frontend/Compiler/Lexer.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

#include "TestConfig.hpp"

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
    INFO(buffer);
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
    INFO(buffer);
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
    INFO(buffer);
    REQUIRE(buffer.empty());
    ppsourceObject = cld::PP::preprocess(std::move(ppsourceObject), &ss);
    INFO(buffer);
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
CREATE_ERROR(pointAt, "Highlight!", cld::PointAt<0>);
CREATE_ERROR(insertAfter, "Highlight!", cld::InsertAfter<0>);
CREATE_ERROR(insertAfterText, "Highlight!", cld::InsertAfter<0, 1>);
CREATE_ERROR(annotate, "Highlight!", cld::Annotate<0, 1>);
} // namespace

TEST_CASE("Diag in format arguments", "[diag]")
{
    SECTION("Specifying location")
    {
        STATIC_REQUIRE(pointLocationTest.getSize() == 0);
        const auto* tokens = lexes("text text2");
        auto message = pointLocationTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:1: error:"));
        message = pointLocationTest.args(4, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:5: error:"));
        message = pointLocationTest.args(std::pair{*(tokens + 1), 0}, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:6: error:"));
        message = pointLocationTest.args(std::pair{4, *(tokens + 1)}, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:10: error:"));
    }
    SECTION("Simple argument")
    {
        STATIC_REQUIRE(singleArgumentTest.getSize() == 1);
        const auto* tokens = lexes("text");
        auto message = singleArgumentTest.args(*tokens, *interface, "we go");
        CHECK_THAT(message.getText(), Catch::Matchers::Contains("Here we go"));
        message = singleArgumentTest.args(*tokens, *interface, 5);
        CHECK_THAT(message.getText(), Catch::Matchers::Contains("Here 5"));
        std::string_view text = "we go";
        message = singleArgumentTest.args(*tokens, *interface, text);
        CHECK_THAT(message.getText(), Catch::Matchers::Contains("Here we go"));
    }
    SECTION("English plural test")
    {
        STATIC_REQUIRE(englishPluralTest.getSize() == 1);
        const auto* tokens = lexes("text");
        auto number = GENERATE(0, 1, 2, 3, 3, 5);
        auto message = englishPluralTest.args(*tokens, *interface, number);
        if (number == 1)
        {
            CHECK_THAT(message.getText(), Catch::Matchers::Contains("I want 1 cake now"));
        }
        else
        {
            CHECK_THAT(message.getText(), Catch::Matchers::Contains("I want " + std::to_string(number) + " cakes now"));
        }
    }
}

TEST_CASE("Diag severity", "[diag]")
{
    const auto* tokens = lexes("text text2");
    SECTION("Error")
    {
        auto message = pointLocationTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:1: error:"));
    }
    SECTION("Warning")
    {
        auto message = warningTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:1: warning:"));
        auto options = cld::LanguageOptions::native();
        options.disabledWarnings.insert("warning-test");
        tokens = lexes("text text2", options);
        message = warningTest.args(*tokens, *interface);
        CHECK(message.getText().empty());
    }
    SECTION("Note")
    {
        auto message = noteTest.args(*tokens, *interface);
        CHECK_THAT(message.getText(), Catch::Matchers::StartsWith("<stdin>:1:1: note:"));
    }
}

TEST_CASE("Diag line printing", "[diag]") {}

#undef CREATE_ERROR

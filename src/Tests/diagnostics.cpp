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
        auto message = singleArgumentTest.args(*tokens, *interface, "we go");
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
}

#undef CREATE_ERROR

#include "catch.hpp"

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Message.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include "TestConfig.hpp"

namespace
{
cld::CSourceObject csourceObject;
cld::PPSourceObject ppsourceObject;

[[nodiscard]] cld::Lexer::CTokenIterator lexes(std::string_view code,
                                               const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto temp = cld::Lexer::tokenize(code, options, &ss);
    INFO(buffer);
    REQUIRE(buffer.empty());
    csourceObject = cld::Lexer::toCTokens(temp);
    return csourceObject.data().cbegin();
}

[[nodiscard]] cld::Lexer::PPTokenIterator pplexes(std::string_view code,
                                                  const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    ppsourceObject = cld::Lexer::tokenize(code, options, &ss);
    INFO(buffer);
    REQUIRE(buffer.empty());
    return ppsourceObject.data().cbegin();
}
} // namespace

template <class... Args>
std::string cmessagePrints(cld::CMessage::Severity severity, Args&&... args)
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto message = [&] {
        switch (severity)
        {
            case cld::CMessage::Error: return cld::CMessage::error(std::forward<Args>(args)...);
            case cld::CMessage::Note: return cld::CMessage::note(std::forward<Args>(args)...);
            case cld::CMessage::Warning: return cld::CMessage::warning(std::forward<Args>(args)...);
        }
        CLD_UNREACHABLE;
    }();
    message.print(ss, csourceObject);
    message.print(llvm::errs(), csourceObject);
    return buffer;
}

template <class... Args>
std::string ppmessagePrints(cld::PPMessage::Severity severity, Args&&... args)
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto message = [&] {
        switch (severity)
        {
            case cld::PPMessage::Error: return cld::PPMessage::error(std::forward<Args>(args)...);
            case cld::PPMessage::Note: return cld::PPMessage::note(std::forward<Args>(args)...);
            case cld::PPMessage::Warning: return cld::PPMessage::warning(std::forward<Args>(args)...);
        }
        CLD_UNREACHABLE;
    }();
    message.print(ss, ppsourceObject);
    message.print(llvm::errs(), ppsourceObject);
    return buffer;
}

TEST_CASE("Simple printing", "[MSG]")
{
    auto begin = lexes("A series of\n identifiers");
    SECTION("Severities")
    {
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin) == R"(1:1: error: Message
   1 | A series of
)");
        CHECK(cmessagePrints(cld::CMessage::Note, "Message", begin) == R"(1:1: note: Message
   1 | A series of
)");
        CHECK(cmessagePrints(cld::CMessage::Warning, "Message", begin) == R"(1:1: warning: Message
   1 | A series of
)");
    }
    SECTION("Single target")
    {
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin + 1) == R"(1:3: error: Message
   1 | A series of
)");
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin + 2) == R"(1:10: error: Message
   1 | A series of
)");
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin + 3) == R"(2:2: error: Message
   2 |  identifiers
)");
    }
    SECTION("Span")
    {
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4) == R"(1:1: error: Message
   1 | A series of
   2 |  identifiers
)");
    }
    SECTION("Cutting newline")
    {
        begin = lexes("    A\\\nnt     text cont\\\ninues");
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin + 1) == R"(2:8: error: Message
   2 | nt     text cont\
)");
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin) == R"(1:5: error: Message
   1 |     A\
   2 | nt     text cont\
)");
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 3) == R"(1:5: error: Message
   1 |     A\
   2 | nt     text cont\
   3 | inues
)");
    }
    SECTION("End of File")
    {
        CHECK(cmessagePrints(cld::CMessage::Error, "Message", begin + 4) == R"(2:13: error: Message
   2 |  identifiers
)");
    }
    SECTION("PP Newlines")
    {
        auto ppbegin = pplexes("#if 0\n");
        CHECK_THAT(ppmessagePrints(cld::PPMessage::Error, "Message", ppbegin + 4), ProducesLines(R"(2:1: error: Message
   1 | #if 0
   2 |
)"));
    }
    SECTION("Unicode")
    {
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin), ProducesLines(R"(1:1: error: Message
   1 | "�\
   2 | ��"
)"));
    }
}

using CModifiers = std::vector<cld::Modifier<cld::Lexer::CToken>>;
using PPModifiers = std::vector<cld::Modifier<cld::Lexer::PPToken>>;

TEST_CASE("Underline", "[MSG]")
{
    SECTION("Underline word for word")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Underline(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Underline(begin + 1)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |   ~~~~~~
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Underline(begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |          ~~
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin + 3, CModifiers{cld::Underline(begin + 3)}),
                   ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |  ~~~~~~~~~~~
)"));
    }
    SECTION("Ranges")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~~~~~~~~~~
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
        begin = lexes("    A\\\nnt     text cont\\\ninues");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Underline(begin)}),
                   ProducesLines(R"(1:5: error: Message
   1 |     A\
     |     ~~
   2 | nt     text cont\
     | ~~
)"));
    }
    SECTION("Unicode")
    {
        auto begin = lexes("\"🍌\" text");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Underline(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | "🍌" text
     | ~~~
)"));
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Underline(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | "�\
     | ~~~
   2 | ��"
     | ~~~
)"));
    }
    SECTION("Overlapping")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::Underline(begin + 1, begin + 3), cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~~~~~~~~~~
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                           CModifiers{cld::Underline(begin + 1, begin + 3, '^'), cld::Underline(begin, begin + 4)}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~^^^^^^^^^
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                           CModifiers{cld::Underline(begin + 1, begin + 3, '^'), cld::Underline(begin, begin + 2)}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~^^^^^^^^^
   2 |  identifiers
)"));
    }
}

TEST_CASE("PointAt", "[MSG]")
{
    SECTION("Underline word for word")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::PointAt(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::PointAt(begin + 1)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |   ^^^^^^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::PointAt(begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |          ^^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin + 3, CModifiers{cld::PointAt(begin + 3)}),
                   ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
    }
    SECTION("Ranges")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::PointAt(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
        begin = lexes("    A\\\nnt     text cont\\\ninues");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::PointAt(begin)}),
                   ProducesLines(R"(1:5: error: Message
   1 |     A\
     |     ^^
   2 | nt     text cont\
     | ^^
)"));
    }
    SECTION("Unicode")
    {
        auto begin = lexes("\"🍌\" text");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::PointAt(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | "🍌" text
     | ^^^
)"));
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::PointAt(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | "�\
     | ^^^
   2 | ��"
     | ^^^
)"));
    }
    SECTION("Overlapping")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::PointAt(begin + 1, begin + 3), cld::PointAt(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::PointAt(begin + 1, begin + 3), cld::PointAt(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::PointAt(begin + 1, begin + 3), cld::PointAt(begin, begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
)"));
    }
    SECTION("With underline")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::PointAt(begin, begin + 4), cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^~^^^^^^~^^
   2 |  identifiers
     | ~^^^^^^^^^^^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, begin + 4,
                                  CModifiers{cld::PointAt(begin + 1), cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~^^^^^^~~~
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
    }
}

TEST_CASE("Insert", "[MSG]")
{
    SECTION("Simple non text insertion")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 1)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |         ^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |            ^
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin + 3, CModifiers{cld::InsertAfter(begin + 3)}),
                   ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |             ^
)"));
    }
    SECTION("After PP Newline")
    {
        auto begin = pplexes("A series of\n identifiers");
        CHECK_THAT(
            ppmessagePrints(cld::PPMessage::Error, "Message", begin + 3, PPModifiers{cld::InsertAfter(begin + 3)}),
            ProducesLines(R"(1:12: error: Message
   1 | A series of
     >
   2 |  identifiers
)"));
    }
    SECTION("Simple with text insertion")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^
     |  text
)"));
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 1, "text")}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     |         ^
     |         text
)"));
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 2, "text")}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     |            ^
     |            text
)"));
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin + 3, CModifiers{cld::InsertAfter(begin + 3, "text")}),
            ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |             ^
     |             text
)"));
    }
    SECTION("After PP Newline with text")
    {
        auto begin = pplexes("A series of\n identifiers");
        CHECK_THAT(ppmessagePrints(cld::PPMessage::Error, "Message", begin + 3,
                                   PPModifiers{cld::InsertAfter(begin + 3, "text")}),
                   ProducesLines(R"(1:12: error: Message
   1 | A series of
     > text
   2 |  identifiers
)"));
    }
    SECTION("Multiple with text insertion")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin,
                                  CModifiers{cld::InsertAfter(begin, "Whitespace 1"),
                                             cld::InsertAfter(begin + 1, "Whitespace 2"),
                                             cld::InsertAfter(begin + 2, "Whitespace 3")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^      ^  ^
     |  |      |  Whitespace 3
     |  |      Whitespace 2
     |  Whitespace 1
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin,
                                  CModifiers{cld::InsertAfter(begin, "1"), cld::InsertAfter(begin + 1, "2"),
                                             cld::InsertAfter(begin + 2, "3")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^      ^  ^
     |  1      2  3
)"));
    }
    SECTION("Pointing at non whitespace")
    {
        auto begin = lexes("5+3=8");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5⍽+3=8
     |  ^
     |  2
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 1, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+⍽3=8
     |   ^
     |   2
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 2, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+3⍽=8
     |    ^
     |    2
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 3, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+3=⍽8
     |     ^
     |     2
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::InsertAfter(begin + 4, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+3=8
     |      ^
     |      2
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin,
                                  CModifiers{cld::InsertAfter(begin, "1"), cld::InsertAfter(begin + 1, "2"),
                                             cld::InsertAfter(begin + 2, "3"), cld::InsertAfter(begin + 3, "4")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5⍽+⍽3⍽=⍽8
     |  ^ ^ ^ ^
     |  1 2 3 4
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin,
                                  CModifiers{cld::InsertAfter(begin, "10"), cld::InsertAfter(begin + 1, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5⍽+⍽3=8
     |  ^ ^
     |  | 2
     |  10
)"));
    }
}

TEST_CASE("Annotate", "[MSG]")
{
    SECTION("Simple annotate")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Annotate(begin, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^
     | |
     | text
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Annotate(begin + 1, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |   ~~~~~~
     |      |
     |      text
)"));
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Annotate(begin + 2, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |          ~~
     |           |
     |           text
)"));
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin + 3, CModifiers{cld::Annotate(begin + 3, "text")}),
            ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |  ~~~~~~~~~~~
     |       |
     |       text
)"));
    }
    SECTION("Ranges")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(
            cmessagePrints(cld::CMessage::Error, "Message", begin, CModifiers{cld::Annotate(begin, begin + 3, "text")}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~~~~~~~~~~
     |      |
     |      text
)"));
    }
    SECTION("Mixed with others")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(cmessagePrints(cld::CMessage::Error, "Message", begin,
                                  CModifiers{cld::Annotate(begin, "text"), cld::InsertAfter(begin + 2, "text2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^          ^
     | |          text2
     | text
)"));
    }
}

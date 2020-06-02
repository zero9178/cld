#include "catch.hpp"

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Message.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include "TestConfig.hpp"

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

template <class... Args>
std::string messagePrints(cld::Message::Severity severity, Args&&... args)
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    auto message = [&] {
        switch (severity)
        {
            case cld::Message::Error: return cld::Message::error(std::forward<Args>(args)...);
            case cld::Message::Note: return cld::Message::note(std::forward<Args>(args)...);
            case cld::Message::Warning: return cld::Message::warning(std::forward<Args>(args)...);
        }
        CLD_UNREACHABLE;
    }();
    message.print(ss, *interface);
    message.print(llvm::errs(), *interface);
    return buffer;
}

} // namespace

TEST_CASE("Simple printing", "[MSG]")
{
    auto begin = lexes("A series of\n identifiers");
    SECTION("Severities")
    {
        CHECK(messagePrints(cld::Message::Error, "Message", begin) == R"(1:1: error: Message
   1 | A series of
)");
        CHECK(messagePrints(cld::Message::Note, "Message", begin) == R"(1:1: note: Message
   1 | A series of
)");
        CHECK(messagePrints(cld::Message::Warning, "Message", begin) == R"(1:1: warning: Message
   1 | A series of
)");
    }
    SECTION("Single target")
    {
        CHECK(messagePrints(cld::Message::Error, "Message", begin + 1) == R"(1:3: error: Message
   1 | A series of
)");
        CHECK(messagePrints(cld::Message::Error, "Message", begin + 2) == R"(1:10: error: Message
   1 | A series of
)");
        CHECK(messagePrints(cld::Message::Error, "Message", begin + 3) == R"(2:2: error: Message
   2 |  identifiers
)");
    }
    SECTION("Span")
    {
        CHECK(messagePrints(cld::Message::Error, "Message", begin, begin + 4) == R"(1:1: error: Message
   1 | A series of
   2 |  identifiers
)");
    }
    SECTION("Cutting newline")
    {
        begin = lexes("    A\\\nnt     text cont\\\ninues");
        CHECK(messagePrints(cld::Message::Error, "Message", begin + 1) == R"(2:8: error: Message
   2 | nt     text cont\
)");
        CHECK(messagePrints(cld::Message::Error, "Message", begin) == R"(1:5: error: Message
   1 |     A\
   2 | nt     text cont\
)");
        CHECK(messagePrints(cld::Message::Error, "Message", begin, begin + 3) == R"(1:5: error: Message
   1 |     A\
   2 | nt     text cont\
   3 | inues
)");
    }
    SECTION("End of File")
    {
        CHECK(messagePrints(cld::Message::Error, "Message", cld::Message::after, begin + 3) == R"(2:13: error: Message
   2 |  identifiers
)");
    }
    SECTION("PP Newlines")
    {
        auto ppbegin = pplexes("#if 0\n");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", cld::Message::after, ppbegin + 3),
                   ProducesLines(R"(2:1: error: Message
   1 | #if 0
   2 |
)"));
    }
    SECTION("Unicode")
    {
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin), ProducesLines(R"(1:1: error: Message
   1 | "�\
   2 | ��"
)"));
    }
}

using Modifiers = std::vector<cld::Modifier>;

TEST_CASE("Underline", "[MSG]")
{
    SECTION("Underline word for word")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Underline(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Underline(begin + 1)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |   ~~~~~~
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Underline(begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |          ~~
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::Underline(begin + 3)}),
                   ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |  ~~~~~~~~~~~
)"));
    }
    SECTION("Ranges")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~~~~~~~~~~
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
        begin = lexes("    A\\\nnt     text cont\\\ninues");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Underline(begin)}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Underline(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | "🍌" text
     | ~~~
)"));
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Underline(begin)}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::Underline(begin + 1, begin + 3), cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~~~~~~~~~~
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
        CHECK_THAT(
            messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                          Modifiers{cld::Underline(begin + 1, begin + 3, '^'), cld::Underline(begin, begin + 4)}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ~~^^^^^^^^^
   2 |  identifiers
     | ~~~~~~~~~~~~
)"));
        CHECK_THAT(
            messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                          Modifiers{cld::Underline(begin + 1, begin + 3, '^'), cld::Underline(begin, begin + 2)}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::PointAt(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::PointAt(begin + 1)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |   ^^^^^^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::PointAt(begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |          ^^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::PointAt(begin + 3)}),
                   ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
    }
    SECTION("Ranges")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(
            messagePrints(cld::Message::Error, "Message", begin, begin + 4, Modifiers{cld::PointAt(begin, begin + 4)}),
            ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
        begin = lexes("    A\\\nnt     text cont\\\ninues");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::PointAt(begin)}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::PointAt(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | "🍌" text
     | ^^^
)"));
        begin = lexes("\"\xe3\\\n\x80\xBA\"");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::PointAt(begin)}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::PointAt(begin + 1, begin + 3), cld::PointAt(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::PointAt(begin + 1, begin + 3), cld::PointAt(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
     |  ^^^^^^^^^^^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::PointAt(begin + 1, begin + 3), cld::PointAt(begin, begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^ ^^^^^^ ^^
   2 |  identifiers
)"));
    }
    SECTION("With underline")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::PointAt(begin, begin + 4), cld::Underline(begin, begin + 4)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^~^^^^^^~^^
   2 |  identifiers
     | ~^^^^^^^^^^^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, begin + 4,
                                 Modifiers{cld::PointAt(begin + 1), cld::Underline(begin, begin + 4)}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 1)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |         ^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 2)}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |            ^
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::InsertAfter(begin + 3)}),
                   ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |             ^
)"));
    }
    SECTION("After PP Newline")
    {
        auto begin = pplexes("A series of\n identifiers");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::InsertAfter(begin + 3)}),
                   ProducesLines(R"(1:12: error: Message
   1 | A series of
     >
   2 |  identifiers
)"));
    }
    SECTION("Simple with text insertion")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^
     |  text
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 1, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |         ^
     |         text
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 2, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |            ^
     |            text
)"));
        CHECK_THAT(
            messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::InsertAfter(begin + 3, "text")}),
            ProducesLines(R"(2:2: error: Message
   2 |  identifiers
     |             ^
     |             text
)"));
    }
    SECTION("After PP Newline with text")
    {
        auto begin = pplexes("A series of\n identifiers");
        CHECK_THAT(
            messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::InsertAfter(begin + 3, "text")}),
            ProducesLines(R"(1:12: error: Message
   1 | A series of
     > text
   2 |  identifiers
)"));
    }
    SECTION("Multiple with text insertion")
    {
        auto begin = lexes("A series of\n identifiers");
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin,
                                 Modifiers{cld::InsertAfter(begin, "Whitespace 1"),
                                           cld::InsertAfter(begin + 1, "Whitespace 2"),
                                           cld::InsertAfter(begin + 2, "Whitespace 3")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |  ^      ^  ^
     |  |      |  Whitespace 3
     |  |      Whitespace 2
     |  Whitespace 1
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin,
                                 Modifiers{cld::InsertAfter(begin, "1"), cld::InsertAfter(begin + 1, "2"),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5⍽+3=8
     |  ^
     |  2
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 1, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+⍽3=8
     |   ^
     |   2
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 2, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+3⍽=8
     |    ^
     |    2
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 3, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+3=⍽8
     |     ^
     |     2
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::InsertAfter(begin + 4, "2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5+3=8
     |      ^
     |      2
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin,
                                 Modifiers{cld::InsertAfter(begin, "1"), cld::InsertAfter(begin + 1, "2"),
                                           cld::InsertAfter(begin + 2, "3"), cld::InsertAfter(begin + 3, "4")}),
                   ProducesLines(R"(1:1: error: Message
   1 | 5⍽+⍽3⍽=⍽8
     |  ^ ^ ^ ^
     |  1 2 3 4
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin,
                                 Modifiers{cld::InsertAfter(begin, "10"), cld::InsertAfter(begin + 1, "2")}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Annotate(begin, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^
     | |
     | text
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Annotate(begin + 1, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |   ~~~~~~
     |      |
     |      text
)"));
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Annotate(begin + 2, "text")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     |          ~~
     |           |
     |           text
)"));
        CHECK_THAT(
            messagePrints(cld::Message::Error, "Message", begin + 3, Modifiers{cld::Annotate(begin + 3, "text")}),
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
            messagePrints(cld::Message::Error, "Message", begin, Modifiers{cld::Annotate(begin, begin + 3, "text")}),
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
        CHECK_THAT(messagePrints(cld::Message::Error, "Message", begin,
                                 Modifiers{cld::Annotate(begin, "text"), cld::InsertAfter(begin + 2, "text2")}),
                   ProducesLines(R"(1:1: error: Message
   1 | A series of
     | ^          ^
     | |          text2
     | text
)"));
    }
}

#include "catch.hpp"

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Message.hpp>
#include <CompilerCore/C/SourceObject.hpp>

namespace
{
cld::SourceObject sourceObject;

[[nodiscard]] cld::Lexer::TokenIterator lexes(std::string_view code,
                                              const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    sourceObject = cld::Lexer::tokenize({code.begin(), code.end()}, options, false, &ss);
    REQUIRE(buffer.empty());
    return sourceObject.data().cbegin();
}

[[nodiscard, maybe_unused]] cld::Lexer::TokenIterator
    pplexes(std::string_view code, const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    sourceObject = cld::Lexer::tokenize({code.begin(), code.end()}, options, true, &ss);
    REQUIRE(buffer.empty());
    return sourceObject.data().cbegin();
}
} // namespace

template <class... Args>
std::string messagePrints(cld::Message::Severity severity, Args&&... args)
{
    std::string buffer;
    llvm::raw_string_ostream ss(buffer);
    switch (severity)
    {
        case cld::Message::Error: cld::Message::error(std::forward<Args>(args)...).print(ss, sourceObject); break;
        case cld::Message::Note: cld::Message::note(std::forward<Args>(args)...).print(ss, sourceObject); break;
        case cld::Message::Warning: cld::Message::warning(std::forward<Args>(args)...).print(ss, sourceObject); break;
    }
    return buffer;
}

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
}

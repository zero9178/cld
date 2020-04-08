#include "catch.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Preprocessor/Parser.hpp>

#include "TestConfig.hpp"

#define treeProduces(source, matches)                                                                      \
    [] {                                                                                                   \
        std::string string;                                                                                \
        llvm::raw_string_ostream ss(string);                                                               \
        static cld::SourceObject tokens;                                                                   \
        REQUIRE_NOTHROW(tokens = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), true, &ss)); \
        ss.flush();                                                                                        \
        REQUIRE(string.empty());                                                                           \
        auto tree = cld::PP::buildTree(tokens, &ss);                                                       \
        CHECK_THAT(string, matches);                                                                       \
        cld::PP::buildTree(tokens);                                                                        \
        if (!string.empty())                                                                               \
        {                                                                                                  \
            llvm::errs() << '\n';                                                                          \
        }                                                                                                  \
        return std::move(tree.first);                                                                      \
    }()

#define functionProduces(parser, source, matches)                                                          \
    [] {                                                                                                   \
        std::string string;                                                                                \
        llvm::raw_string_ostream ss(string);                                                               \
        static cld::SourceObject tokens;                                                                   \
        REQUIRE_NOTHROW(tokens = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), true, &ss)); \
        ss.flush();                                                                                        \
        REQUIRE(string.empty());                                                                           \
        cld::PP::Context context(tokens, &ss);                                                             \
        auto begin = tokens.data().cbegin();                                                               \
        auto ret = parser(begin, tokens.data().cend(), context);                                           \
        CHECK_THAT(string, matches);                                                                       \
        {                                                                                                  \
            auto begin2 = tokens.data().cbegin();                                                          \
            cld::PP::Context context2(tokens);                                                             \
            parser(begin2, tokens.data().cend(), context2);                                                \
            if (!string.empty())                                                                           \
            {                                                                                              \
                llvm::errs() << '\n';                                                                      \
            }                                                                                              \
        }                                                                                                  \
        return ret;                                                                                        \
    }()

using namespace cld::ErrorMessages;
using namespace cld::Notes;
using namespace cld::ErrorMessages::Parser;
using namespace cld::ErrorMessages::PP;

TEST_CASE("Parse Preprocessor Group", "[PPParse]")
{
    SECTION("Text line")
    {
        auto ret = functionProduces(parseGroup, "a line", ProducesNothing());
        REQUIRE(ret.groupPart.size() == 1);
        const auto& part = ret.groupPart[0];
        REQUIRE(std::holds_alternative<std::vector<cld::Lexer::Token>>(part));
        const auto& vector = std::get<std::vector<cld::Lexer::Token>>(part);
        REQUIRE(vector.size() == 2);
        REQUIRE(vector[0].getRepresentation() == "a");
        REQUIRE(vector[1].getRepresentation() == "line");
    }
    SECTION("Only #")
    {
        treeProduces("#", ProducesNothing());
    }
    SECTION("Non directive")
    {
        treeProduces("#5non directive", ProducesNothing());
    }
    SECTION("Invalid directive")
    {
        treeProduces("#non directive", ProducesError(N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE.args("'non'"))
                                           && ProducesNoWarnings() && ProducesNoNotes());
    }
}

TEST_CASE("Parse Preprocessor Control Line", "[PPParse]")
{
    SECTION("Includes")
    {
        SECTION("Normal")
        {
            auto tree = treeProduces("#include <String>", ProducesNothing());
            REQUIRE(tree.groups.size() == 1);
            const auto& parts = tree.groups[0];
            REQUIRE(parts.groupPart.size() == 1);
            const auto& part = parts.groupPart[0];
            REQUIRE(std::holds_alternative<cld::PP::ControlLine>(part));
            const auto& ret = std::get<cld::PP::ControlLine>(part);
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::IncludeTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::IncludeTag>(ret.variant);
            REQUIRE(std::distance(include.begin, include.end) == 1);
            auto begin = include.begin;
            CHECK(begin->getRepresentation() == "<String>");
            CHECK(begin->getTokenType() == cld::Lexer::TokenType::StringLiteral);
        }
        SECTION("Empty")
        {
            treeProduces("#include", ProducesError(EXPECTED_N_AFTER_N.args("Tokens", "'include'")));
        }
    }
    SECTION("Lines")
    {
        SECTION("Normal")
        {
            treeProduces("#line 5", ProducesNothing());
            auto ret = functionProduces(parseControlLine, "line 5", ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::LineTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::LineTag>(ret.variant);
            REQUIRE(std::distance(include.begin, include.end) == 1);
            auto begin = include.begin;
            CHECK(begin->getRepresentation() == "5");
            CHECK(begin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        }
        SECTION("Empty")
        {
            treeProduces("#line", ProducesError(EXPECTED_N_AFTER_N.args("Tokens", "'line'")));
        }
    }
    SECTION("Errors")
    {
        SECTION("Normal")
        {
            treeProduces("#error 5", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "error 5", ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::ErrorTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::ErrorTag>(ret.variant);
            REQUIRE(std::distance(include.begin, include.end) == 1);
            auto begin = include.begin;
            CHECK(begin->getRepresentation() == "5");
            CHECK(begin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        }
        SECTION("Empty")
        {
            treeProduces("#error", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "error", ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::ErrorTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::ErrorTag>(ret.variant);
            REQUIRE(std::distance(include.begin, include.end) == 0);
        }
    }
    SECTION("Pragma")
    {
        SECTION("Normal")
        {
            treeProduces("#pragma 5", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "pragma 5", ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::PragmaTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::PragmaTag>(ret.variant);
            REQUIRE(std::distance(include.begin, include.end) == 1);
            auto begin = include.begin;
            CHECK(begin->getRepresentation() == "5");
            CHECK(begin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        }
        SECTION("Empty")
        {
            treeProduces("#pragma", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "pragma", ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::PragmaTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::PragmaTag>(ret.variant);
            REQUIRE(std::distance(include.begin, include.end) == 0);
        }
    }
    SECTION("undef")
    {
        SECTION("Normal")
        {
            treeProduces("#undef ID", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "undef ID", ProducesNothing());
            REQUIRE(std::holds_alternative<std::string>(ret.variant));
            const auto& text = std::get<std::string>(ret.variant);
            CHECK(text == "ID");
        }
        SECTION("Errors")
        {
            treeProduces("#undef", ProducesError(EXPECTED_N.args("identifier")));
            treeProduces("#undef ID 5", ProducesError(EXPECTED_N.args("newline")));
        }
    }
    SECTION("define")
    {
        treeProduces("#define ID", ProducesNothing());
    }
}

TEST_CASE("Parse Preprocessor Define", "[PPParse]")
{
    SECTION("Simple")
    {
        auto ret = functionProduces(parseDefineDirective, "define ID 5", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        CHECK(!ret.identifierList);
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 1);
        CHECK(ret.replacementBegin->getRepresentation() == "5");
        CHECK(ret.replacementBegin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        ret = functionProduces(parseDefineDirective, "define ID", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        CHECK(!ret.identifierList);
        functionProduces(parseDefineDirective, "define 5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'5'")));
        functionProduces(parseDefineDirective, "define", ProducesError(EXPECTED_N.args("identifier")));
        ret = functionProduces(parseDefineDirective, "define ID (a)", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        CHECK(!ret.identifierList);
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 3);
        auto begin = ret.replacementBegin;
        CHECK(begin->getRepresentation() == "(");
        CHECK(begin->getTokenType() == cld::Lexer::TokenType::OpenParentheses);
        begin++;
        CHECK(begin->getRepresentation() == "a");
        CHECK(begin->getTokenType() == cld::Lexer::TokenType::Identifier);
        begin++;
        CHECK(begin->getRepresentation() == ")");
        CHECK(begin->getTokenType() == cld::Lexer::TokenType::CloseParentheses);
        functionProduces(parseDefineDirective, "define ID+",
                         ProducesError(WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION));
    }
    SECTION("Empty Identifier list")
    {
        auto ret = functionProduces(parseDefineDirective, "define ID()", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        REQUIRE(ret.identifierList);
        CHECK(ret.identifierList->empty());
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 0);
        ret = functionProduces(parseDefineDirective, "define ID() 5", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        REQUIRE(ret.identifierList);
        CHECK(ret.identifierList->empty());
        REQUIRE(std::distance(ret.replacementBegin, ret.replacementEnd) == 1);
        CHECK(ret.replacementBegin->getRepresentation() == "5");
        CHECK(ret.replacementBegin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        functionProduces(parseDefineDirective, "define ID(",
                         ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    }
    SECTION("Ellipse only")
    {
        auto ret = functionProduces(parseDefineDirective, "define ID(...)", ProducesNothing());
        CHECK(ret.hasEllipse == true);
        REQUIRE(ret.identifierList);
        CHECK(ret.identifierList->empty());
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 0);
        ret = functionProduces(parseDefineDirective, "define ID(...) 5", ProducesNothing());
        CHECK(ret.hasEllipse == true);
        REQUIRE(ret.identifierList);
        CHECK(ret.identifierList->empty());
        REQUIRE(std::distance(ret.replacementBegin, ret.replacementEnd) == 1);
        CHECK(ret.replacementBegin->getRepresentation() == "5");
        CHECK(ret.replacementBegin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        functionProduces(parseDefineDirective, "define ID(...",
                         ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    }
    SECTION("Single Identifier list")
    {
        auto ret = functionProduces(parseDefineDirective, "define ID(a)", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        REQUIRE(ret.identifierList);
        REQUIRE(ret.identifierList->size() == 1);
        REQUIRE(ret.identifierList.value()[0] == "a");
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 0);
        ret = functionProduces(parseDefineDirective, "define ID(a) 5", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        REQUIRE(ret.identifierList);
        REQUIRE(ret.identifierList->size() == 1);
        CHECK(ret.identifierList.value()[0] == "a");
        REQUIRE(std::distance(ret.replacementBegin, ret.replacementEnd) == 1);
        CHECK(ret.replacementBegin->getRepresentation() == "5");
        CHECK(ret.replacementBegin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        functionProduces(parseDefineDirective, "define ID(5",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'5'")));
        functionProduces(parseDefineDirective, "define ID(a",
                         ProducesError(EXPECTED_N.args("')'")) && ProducesNote(TO_MATCH_N_HERE.args("'('")));
    }
    SECTION("Multiple identifiers")
    {
        auto ret = functionProduces(parseDefineDirective, "define ID(a,b,c)", ProducesNothing());
        CHECK(ret.hasEllipse == false);
        REQUIRE(ret.identifierList);
        REQUIRE(ret.identifierList->size() == 3);
        CHECK(ret.identifierList.value()[0] == "a");
        CHECK(ret.identifierList.value()[1] == "b");
        CHECK(ret.identifierList.value()[2] == "c");
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 0);
        functionProduces(parseDefineDirective, "define ID(a,)",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "')'")));
        functionProduces(parseDefineDirective, "define ID(a,", ProducesError(EXPECTED_N.args("identifier")));
        ret = functionProduces(parseDefineDirective, "define ID(a,5) 5",
                               ProducesError(EXPECTED_N_INSTEAD_OF_N.args("identifier", "'5'")));
        CHECK(ret.hasEllipse == false);
        REQUIRE(ret.identifierList);
        REQUIRE(ret.identifierList->size() == 1);
        CHECK(ret.identifierList.value()[0] == "a");
        REQUIRE(std::distance(ret.replacementBegin, ret.replacementEnd) == 1);
        CHECK(ret.replacementBegin->getTokenType() == cld::Lexer::TokenType::PPNumber);
        CHECK(ret.replacementBegin->getRepresentation() == "5");
        ret = functionProduces(parseDefineDirective, "define ID(a,b,c,...)", ProducesNothing());
        CHECK(ret.hasEllipse == true);
        REQUIRE(ret.identifierList);
        REQUIRE(ret.identifierList->size() == 3);
        CHECK(ret.identifierList.value()[0] == "a");
        CHECK(ret.identifierList.value()[1] == "b");
        CHECK(ret.identifierList.value()[2] == "c");
        CHECK(std::distance(ret.replacementBegin, ret.replacementEnd) == 0);
        functionProduces(parseDefineDirective, "define ID(a,b,a)",
                         ProducesError(REDEFINITION_OF_MACRO_PARAMETER_N.args("'a'"))
                             && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    }
    treeProduces("#define defined", ProducesError(DEFINED_CANNOT_BE_USED_AS_MACRO_NAME));
}

TEST_CASE("Parse Preprocessor if section", "[PPParse]")
{
    SECTION("if")
    {
        treeProduces("#if 0\n5\n#endif", ProducesNothing());
        auto ret = functionProduces(parseIfGroup, "if 0\n5\n", ProducesNothing());
        CHECK(ret.optionalGroup);
        REQUIRE(std::holds_alternative<std::vector<cld::Lexer::Token>>(ret.ifs));
        const auto& tokens = std::get<std::vector<cld::Lexer::Token>>(ret.ifs);
        REQUIRE(tokens.size() == 1);
        CHECK(tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        CHECK(tokens[0].getRepresentation() == "0");
        functionProduces(parseIfGroup, "if\n5", ProducesError(EXPECTED_N_AFTER_N.args("Tokens", "'if'")));
    }
    SECTION("ifdef")
    {
        treeProduces("#ifdef ID\n5\n#endif", ProducesNothing());
        auto ret = functionProduces(parseIfGroup, "ifdef ID\n5\n", ProducesNothing());
        CHECK(ret.optionalGroup);
        REQUIRE(std::holds_alternative<cld::PP::IfGroup::IfDefTag>(ret.ifs));
        const auto& tokens = std::get<cld::PP::IfGroup::IfDefTag>(ret.ifs);
        CHECK(tokens.identifier == "ID");
        functionProduces(parseIfGroup, "ifdef \n5", ProducesError(EXPECTED_N.args("identifier")));
        functionProduces(parseIfGroup, "ifdef ID 5 \n 5", ProducesError(EXPECTED_N.args("newline")));
    }
    SECTION("ifndef")
    {
        treeProduces("#ifndef ID\n5\n#endif", ProducesNothing());
        auto ret = functionProduces(parseIfGroup, "ifndef ID\n5\n", ProducesNothing());
        CHECK(ret.optionalGroup);
        REQUIRE(std::holds_alternative<cld::PP::IfGroup::IfnDefTag>(ret.ifs));
        const auto& tokens = std::get<cld::PP::IfGroup::IfnDefTag>(ret.ifs);
        CHECK(tokens.identifier == "ID");
        functionProduces(parseIfGroup, "ifndef \n5", ProducesError(EXPECTED_N.args("identifier")));
        functionProduces(parseIfGroup, "ifndef ID 5 \n 5", ProducesError(EXPECTED_N.args("newline")));
    }
    SECTION("elif")
    {
        treeProduces("#if 0\n#elif 1\n5\n#endif", ProducesNothing());
        auto ret = functionProduces(parseIfSection, "if 0\n#elif 1\n5\n#endif", ProducesNothing());
        REQUIRE(ret.elifGroups.size() == 1);
        const auto& elif = ret.elifGroups[0];
        CHECK(elif.optionalGroup);
        const auto& tokens = elif.constantExpression;
        REQUIRE(tokens.size() == 1);
        CHECK(tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        CHECK(tokens[0].getRepresentation() == "1");
        functionProduces(parseIfSection, "if 0\n#elif\n5\n#endif",
                         ProducesError(EXPECTED_N_AFTER_N.args("Tokens", "'elif'")));
    }
    SECTION("Else")
    {
        auto ret = functionProduces(parseIfSection, "if 0\n#else\n5\n#endif", ProducesNothing());
        CHECK(ret.elifGroups.empty());
        REQUIRE(ret.optionalElseGroup);
        const auto& elseGroup = *ret.optionalElseGroup;
        CHECK(elseGroup.optionalGroup);
        ret = functionProduces(parseIfSection, "if 0\n#else\n#endif", ProducesNothing());
        CHECK(ret.elifGroups.empty());
        REQUIRE(ret.optionalElseGroup);
        CHECK(!ret.optionalElseGroup->optionalGroup);
        functionProduces(parseIfSection, "if 0\n#else 5\n#endif",
                         ProducesError(EXPECTED_N_INSTEAD_OF_N.args("newline", "'5'")));
    }
    SECTION("Nested")
    {
        treeProduces("#if 1\n"
                     "  #if 0\n"
                     "      #error This won't error\n"
                     "  #else\n"
                     "      #line 7\n"
                     "  #endif\n"
                     "#endif\n",
                     ProducesNothing());
    }
    SECTION("Empty if")
    {
        treeProduces("#if 0\n#endif", ProducesNothing());
        treeProduces("#if 0\n#else\n#endif", ProducesNothing());
        treeProduces("#if 0\n#elif 1\n#endif", ProducesNothing());
    }
    treeProduces("#if 0\n", ProducesError(EXPECTED_N.args("'#endif'")) && ProducesNote(TO_MATCH_N_HERE.args("'if'")));
    treeProduces("#if 0\n#else\n#else\n", ProducesError(EXPECTED_N_INSTEAD_OF_N.args("'#endif'", "'#else'"))
                                              && ProducesNote(TO_MATCH_N_HERE.args("'if'")));
}

namespace
{
void parse(std::string_view source)
{
    std::string string;
    llvm::raw_string_ostream ss(string);
    static cld::SourceObject tokens;
    REQUIRE_NOTHROW(tokens = cld::Lexer::tokenize(std::string(source.begin(), source.end()),
                                                  cld::LanguageOptions::native(), true, &ss));
    ss.flush();
    REQUIRE(string.empty());
    cld::PP::buildTree(tokens, &llvm::errs());
}
} // namespace

TEST_CASE("Parse Preprocessor Fuzzer discoveries", "[PPParse]")
{
    parse("  #if\n"
          "#if\n"
          "#else  //     \n"
          "\"f \n"
          "p\"\"\n"
          "\"\"\"\"\"\"2f\n"
          "\n"
          "#if\n"
          "de\n"
          "#if\n"
          ".\n"
          "%");
    parse("#elif");
}

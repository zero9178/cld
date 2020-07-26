#include "catch.hpp"

#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Compiler/SourceObject.hpp>
#include <Frontend/Preprocessor/Parser.hpp>

#include "TestConfig.hpp"

namespace
{
cld::PPSourceObject sourceObject;
} // namespace

#define treeProduces(source, matches)                                                     \
    []() mutable {                                                                        \
        std::string string;                                                               \
        llvm::raw_string_ostream ss(string);                                              \
        sourceObject = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), &ss); \
        ss.flush();                                                                       \
        REQUIRE(string.empty());                                                          \
        auto tree = cld::PP::buildTree(sourceObject, &ss);                                \
        CHECK_THAT(string, matches);                                                      \
        cld::PP::buildTree(sourceObject);                                                 \
        if (!string.empty())                                                              \
        {                                                                                 \
            llvm::errs() << '\n';                                                         \
        }                                                                                 \
        return std::move(tree.first);                                                     \
    }()

#define functionProduces(parser, source, offset, matches)                                           \
    []() mutable {                                                                                  \
        std::string string;                                                                         \
        llvm::raw_string_ostream ss(string);                                                        \
        sourceObject = cld::Lexer::tokenize(source, cld::LanguageOptions::native(), &ss);           \
        ss.flush();                                                                                 \
        REQUIRE(string.empty());                                                                    \
        cld::PP::Context context(sourceObject, &ss);                                                \
        auto begin = std::as_const(sourceObject).data().data() + offset;                            \
        auto ret = parser(begin, sourceObject.data().data() + sourceObject.data().size(), context); \
        CHECK_THAT(string, matches);                                                                \
        {                                                                                           \
            auto begin2 = std::as_const(sourceObject).data().data() + offset;                       \
            cld::PP::Context context2(sourceObject);                                                \
            parser(begin2, sourceObject.data().data() + sourceObject.data().size(), context2);      \
            if (!string.empty())                                                                    \
            {                                                                                       \
                llvm::errs() << '\n';                                                               \
            }                                                                                       \
        }                                                                                           \
        return ret;                                                                                 \
    }()

using namespace cld::Errors;
using namespace cld::Notes;
using namespace cld::Errors::Parser;
using namespace cld::Errors::PP;

TEST_CASE("Parse Preprocessor Group", "[PPParse]")
{
    SECTION("Text block")
    {
        auto ret = functionProduces(parseGroup, "a line", 0, ProducesNothing());
        REQUIRE(ret.groupPart.size() == 1);
        const auto& part = ret.groupPart[0];
        REQUIRE(std::holds_alternative<cld::PP::TextBlock>(part));
        const auto& block = std::get<cld::PP::TextBlock>(part);
        REQUIRE(block.tokens.size() == 2);
        REQUIRE(block.tokens[0].getRepresentation(sourceObject) == "a");
        REQUIRE(block.tokens[1].getRepresentation(sourceObject) == "line");
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
        // Error mustn't be handled in the parser
        treeProduces("#non directive", ProducesNothing());
    }
}

TEST_CASE("Parse Preprocessor Control Line", "[PPParse]")
{
    SECTION("Includes")
    {
        SECTION("Normal")
        {
            auto tree = treeProduces("#include <String>\n", ProducesNothing());
            REQUIRE(tree.groups.size() == 1);
            const auto& parts = tree.groups[0];
            REQUIRE(parts.groupPart.size() == 1);
            const auto& part = parts.groupPart[0];
            REQUIRE(std::holds_alternative<cld::PP::ControlLine>(part));
            const auto& ret = std::get<cld::PP::ControlLine>(part);
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::IncludeTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::IncludeTag>(ret.variant);
            REQUIRE(include.tokens.size() == 1);
            CHECK(include.tokens[0].getRepresentation(sourceObject) == "<String>");
            CHECK(include.tokens[0].getTokenType() == cld::Lexer::TokenType::StringLiteral);
        }
        SECTION("Empty")
        {
            treeProduces("#include\n", ProducesNothing());
        }
    }
    SECTION("Lines")
    {
        SECTION("Normal")
        {
            treeProduces("#line 5\n", ProducesNothing());
            auto ret = functionProduces(parseControlLine, "#line 5\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::LineTag>(ret.variant));
            const auto& line = std::get<cld::PP::ControlLine::LineTag>(ret.variant);
            REQUIRE(line.tokens.size() == 1);
            CHECK(line.tokens[0].getRepresentation(sourceObject) == "5");
            CHECK(line.tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        }
        SECTION("Empty")
        {
            // Mustn't be handled in the parser
            treeProduces("#line\n", ProducesNothing());
        }
    }
    SECTION("Define")
    {
        SECTION("Normal")
        {
            treeProduces("#define n\n", ProducesNothing());
            auto ret = functionProduces(parseControlLine, "#define n\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::DefineDirective>(ret.variant));
            const auto& define = std::get<cld::PP::DefineDirective>(ret.variant);
            REQUIRE(define.tokens.size() == 1);
            CHECK(define.tokens[0].getRepresentation(sourceObject) == "n");
            CHECK(define.tokens[0].getTokenType() == cld::Lexer::TokenType::Identifier);
        }
        SECTION("Invalid syntax")
        {
            // Mustn't be handled in the parser
            treeProduces("#define\n", ProducesNothing());
            treeProduces("#define 5\n", ProducesNothing());
            treeProduces("#define a(\n", ProducesNothing());
            treeProduces("#define a()\n", ProducesNothing());
            treeProduces("#define a+\n", ProducesNothing());
        }
    }
    SECTION("Errors")
    {
        SECTION("Normal")
        {
            treeProduces("#error 5\n", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "#error 5\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::ErrorTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::ErrorTag>(ret.variant);
            REQUIRE(include.tokens.size() == 1);
            CHECK(include.tokens[0].getRepresentation(sourceObject) == "5");
            CHECK(include.tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        }
        SECTION("Empty")
        {
            treeProduces("#error\n", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "#error\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::ErrorTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::ErrorTag>(ret.variant);
            REQUIRE(include.tokens.size() == 0);
        }
    }
    SECTION("Pragma")
    {
        SECTION("Normal")
        {
            treeProduces("#pragma 5\n", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "#pragma 5\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::PragmaTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::PragmaTag>(ret.variant);
            REQUIRE(include.tokens.size() == 1);
            CHECK(include.tokens[0].getRepresentation(sourceObject) == "5");
            CHECK(include.tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        }
        SECTION("Empty")
        {
            treeProduces("#pragma\n", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "#pragma\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::PP::ControlLine::PragmaTag>(ret.variant));
            const auto& include = std::get<cld::PP::ControlLine::PragmaTag>(ret.variant);
            CHECK(include.tokens.empty());
        }
    }
    SECTION("undef")
    {
        SECTION("Normal")
        {
            treeProduces("#undef ID\n", ProducesNothing());
            const auto& ret = functionProduces(parseControlLine, "#undef ID\n", 1, ProducesNothing());
            REQUIRE(std::holds_alternative<cld::Lexer::PPTokenIterator>(ret.variant));
            const auto& iter = std::get<cld::Lexer::PPTokenIterator>(ret.variant);
            CHECK(iter->getValue() == "ID");
        }
        SECTION("Errors")
        {
            treeProduces("#undef\n", ProducesError(EXPECTED_N, "identifier"));
            treeProduces("#undef ID 5\n", ProducesError(EXPECTED_N, "newline"));
        }
    }
    SECTION("define")
    {
        treeProduces("#define ID\n", ProducesNothing());
    }
}

TEST_CASE("Parse Preprocessor if section", "[PPParse]")
{
    SECTION("if")
    {
        treeProduces("#if 0\n5\n#endif\n", ProducesNothing());
        auto ret = functionProduces(parseIfGroup, "#if 0\n5\n", 1, ProducesNothing());
        CHECK(ret.optionalGroup);
        REQUIRE(std::holds_alternative<llvm::ArrayRef<cld::Lexer::PPToken>>(ret.ifs));
        const auto& tokens = std::get<llvm::ArrayRef<cld::Lexer::PPToken>>(ret.ifs);
        REQUIRE(tokens.size() == 1);
        CHECK(tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        CHECK(tokens[0].getRepresentation(sourceObject) == "0");
        functionProduces(parseIfGroup, "#if\n5\n", 1, ProducesError(EXPECTED_TOKENS_AFTER_N, "'if'"));
    }
    SECTION("ifdef")
    {
        treeProduces("#ifdef ID\n5\n#endif\n", ProducesNothing());
        auto ret = functionProduces(parseIfGroup, "#ifdef ID\n5\n", 1, ProducesNothing());
        CHECK(ret.optionalGroup);
        REQUIRE(std::holds_alternative<cld::PP::IfGroup::IfDefTag>(ret.ifs));
        const auto& tokens = std::get<cld::PP::IfGroup::IfDefTag>(ret.ifs);
        CHECK(tokens.identifier == "ID");
        functionProduces(parseIfGroup, "#ifdef \n5", 1, ProducesError(EXPECTED_N, "identifier"));
        functionProduces(parseIfGroup, "#ifdef ID 5 \n 5\n", 1, ProducesError(EXPECTED_N, "newline"));
    }
    SECTION("ifndef")
    {
        treeProduces("#ifndef ID\n5\n#endif\n", ProducesNothing());
        auto ret = functionProduces(parseIfGroup, "#ifndef ID\n5\n", 1, ProducesNothing());
        CHECK(ret.optionalGroup);
        REQUIRE(std::holds_alternative<cld::PP::IfGroup::IfnDefTag>(ret.ifs));
        const auto& tokens = std::get<cld::PP::IfGroup::IfnDefTag>(ret.ifs);
        CHECK(tokens.identifier == "ID");
        functionProduces(parseIfGroup, "#ifndef \n5", 1, ProducesError(EXPECTED_N, "identifier"));
        functionProduces(parseIfGroup, "#ifndef ID 5 \n 5", 1, ProducesError(EXPECTED_N, "newline"));
    }
    SECTION("elif")
    {
        treeProduces("#if 0\n#elif 1\n5\n#endif\n", ProducesNothing());
        auto ret = functionProduces(parseIfSection, "#if 0\n#elif 1\n5\n#endif\n", 1, ProducesNothing());
        REQUIRE(ret.elifGroups.size() == 1);
        const auto& elif = ret.elifGroups[0];
        CHECK(elif.optionalGroup);
        const auto& tokens = elif.constantExpression;
        REQUIRE(tokens.size() == 1);
        CHECK(tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        CHECK(tokens[0].getRepresentation(sourceObject) == "1");
        functionProduces(parseIfSection, "#if 0\n#elif\n5\n#endif\n", 1,
                         ProducesError(EXPECTED_TOKENS_AFTER_N, "'elif'"));
    }
    SECTION("Else")
    {
        auto ret = functionProduces(parseIfSection, "#if 0\n#else\n5\n#endif\n", 1, ProducesNothing());
        CHECK(ret.elifGroups.empty());
        REQUIRE(ret.optionalElseGroup);
        const auto& elseGroup = *ret.optionalElseGroup;
        CHECK(elseGroup.optionalGroup);
        ret = functionProduces(parseIfSection, "#if 0\n#else\n#endif\n", 1, ProducesNothing());
        CHECK(ret.elifGroups.empty());
        REQUIRE(ret.optionalElseGroup);
        CHECK(!ret.optionalElseGroup->optionalGroup);
        functionProduces(parseIfSection, "#if 0\n#else 5\n#endif\n", 1,
                         ProducesError(EXPECTED_N_INSTEAD_OF_N, "newline", "'5'"));
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
    treeProduces("#if 0\n", ProducesError(EXPECTED_ENDIF) && ProducesNote(TO_MATCH_N_HERE, "'if'"));
    treeProduces("#if 0\n#else\n#else\n",
                 ProducesError(EXPECTED_ENDIF_INSTEAD_OF_N, "'else'") && ProducesNote(TO_MATCH_N_HERE, "'if'"));
}

namespace
{
void parse(std::string_view source)
{
    std::string string;
    llvm::raw_string_ostream ss(string);
    cld::PPSourceObject tokens;
    tokens = cld::Lexer::tokenize(std::string(source.begin(), source.end()), cld::LanguageOptions::native(), &ss);
    ss.flush();
    if (!string.empty() || tokens.data().empty())
    {
        return;
    }
    cld::PP::buildTree(tokens);
}
} // namespace

TEST_CASE("Parse Preprocessor Fuzzer discoveries", "[PPParse]")
{
    parse("         R$  \\\x0a i\x0a#ifndef\"#>\x0a");
    parse("#define  u0(fine   : \n"
          "z*z\n"
          "#define   (e.fin,e *z\n"
          "#defin \n"
          "z*z\n"
          "#define   ue(efin,e *z\n"
          "#define   ue(efin,p,ne  : \n"
          "z*z\n"
          "#define   ue(efin,e *z\n"
          "#.efin \n"
          "z*z\n"
          "#define   ue(efin,e *z\n"
          "#define   ue(efin,pr\\");
    parse(
        "#define ue(efi, u\\     e  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////// e+efin,pr");
    parse("#if\n"
          "#\n"
          "#else#\\\n"
          "else\n"
          "#\n"
          "\n"
          "");
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

#undef treeProduces
#undef functionProduces

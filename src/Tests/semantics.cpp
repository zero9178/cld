#include "catch.hpp"

#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Compiler/Parser.hpp>
#include <Frontend/Compiler/SemanticAnalysis.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

#include <array>

#include "TestConfig.hpp"

static std::pair<cld::Semantics::TranslationUnit, std::string>
    generateSemantics(const std::string& source, const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    cld::PPSourceObject tokens;
    bool errors = false;
    tokens = cld::Lexer::tokenize(source, options, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    tokens = cld::PP::preprocess(std::move(tokens), &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto ctokens = cld::Lexer::toCTokens(tokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto parsing = cld::Parser::buildTree(ctokens, &ss);
    UNSCOPED_INFO(storage);
    REQUIRE(parsing.second);
    cld::Semantics::SemanticAnalysis analysis(ctokens, &ss);
    auto semantics = analysis.visit(parsing.first);
    return {std::move(semantics), ss.str()};
}

#define SEMA_PRODUCES(source, matcher)               \
    [](std::string input) {                          \
        auto text = generateSemantics(input).second; \
        CHECK_THAT(text, matcher);                   \
        llvm::errs() << text;                        \
    }(source)

using namespace cld::Errors::Semantics;
using namespace cld::Errors;
using namespace cld::Warnings::Semantics;
using namespace cld::Warnings;
using namespace cld::Notes;

TEST_CASE("Semantics declarations", "[semantics]")
{
    SECTION("Primitives")
    {
        auto isConst = GENERATE(false, true);
        auto isVolatile = GENERATE(false, true);
        SECTION("void")
        {
            std::string text = "void ";
            if (isConst)
            {
                text += "const ";
            }
            if (isVolatile)
            {
                text += "volatile ";
            }
            text += " i;";
            SEMA_PRODUCES(text, ProducesError(DECLARATION_MUST_HAVE_A_COMPLETE_TYPE));
        }
        auto options = cld::LanguageOptions::native();
        std::vector<std::pair<cld::Semantics::Type, std::vector<std::string_view>>> table = {
            {cld::Semantics::PrimitiveType::createChar(isConst, isVolatile, options), {"char"}},
            {cld::Semantics::PrimitiveType::createSignedChar(isConst, isVolatile), {"signed", "char"}},
            {cld::Semantics::PrimitiveType::createUnsignedChar(isConst, isVolatile), {"unsigned", "char"}},
            {cld::Semantics::PrimitiveType::createShort(isConst, isVolatile, options), {"short"}},
            {cld::Semantics::PrimitiveType::createShort(isConst, isVolatile, options), {"short", "int"}},
            {cld::Semantics::PrimitiveType::createShort(isConst, isVolatile, options), {"short", "int", "signed"}},
            {cld::Semantics::PrimitiveType::createShort(isConst, isVolatile, options), {"short", "signed"}},
            {cld::Semantics::PrimitiveType::createUnsignedShort(isConst, isVolatile, options), {"short", "unsigned"}},
            {cld::Semantics::PrimitiveType::createUnsignedShort(isConst, isVolatile, options),
             {"short", "unsigned", "int"}},
            {cld::Semantics::PrimitiveType::createInt(isConst, isVolatile, options), {"int"}},
            {cld::Semantics::PrimitiveType::createInt(isConst, isVolatile, options), {"int", "signed"}},
            {cld::Semantics::PrimitiveType::createInt(isConst, isVolatile, options), {"signed"}},
            {cld::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile, options), {"unsigned"}},
            {cld::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile, options), {"unsigned", "int"}},
            {cld::Semantics::PrimitiveType::createLong(isConst, isVolatile, options), {"long"}},
            {cld::Semantics::PrimitiveType::createLong(isConst, isVolatile, options), {"long", "int"}},
            {cld::Semantics::PrimitiveType::createLong(isConst, isVolatile, options), {"long", "int", "signed"}},
            {cld::Semantics::PrimitiveType::createLong(isConst, isVolatile, options), {"long", "signed"}},
            {cld::Semantics::PrimitiveType::createUnsignedLong(isConst, isVolatile, options), {"long", "unsigned"}},
            {cld::Semantics::PrimitiveType::createUnsignedLong(isConst, isVolatile, options),
             {"long", "unsigned", "int"}},
            {cld::Semantics::PrimitiveType::createLongLong(isConst, isVolatile), {"long", "long"}},
            {cld::Semantics::PrimitiveType::createLongLong(isConst, isVolatile), {"long", "long", "int"}},
            {cld::Semantics::PrimitiveType::createLongLong(isConst, isVolatile), {"long", "long", "int", "signed"}},
            {cld::Semantics::PrimitiveType::createLongLong(isConst, isVolatile), {"long", "long", "signed"}},
            {cld::Semantics::PrimitiveType::createUnsignedLongLong(isConst, isVolatile),
             {"long", "long", "int", "unsigned"}},
            {cld::Semantics::PrimitiveType::createUnsignedLongLong(isConst, isVolatile), {"long", "long", "unsigned"}},
            {cld::Semantics::PrimitiveType::createFloat(isConst, isVolatile), {"float"}},
            {cld::Semantics::PrimitiveType::createDouble(isConst, isVolatile), {"double"}},
            {cld::Semantics::PrimitiveType::createLongDouble(isConst, isVolatile, options), {"long", "double"}},
            {cld::Semantics::PrimitiveType::createUnderlineBool(isConst, isVolatile), {"_Bool"}},
        };
        for (auto& [type, variants] : table)
        {
            do
            {
                std::string text(variants[0]);
                for (auto& iter : llvm::ArrayRef(variants).drop_front())
                {
                    text += " ";
                    text += iter;
                }
                if (isConst)
                {
                    text += " const";
                }
                if (isVolatile)
                {
                    text += " volatile";
                }
                auto [translationUnit, errors] = generateSemantics(text + " i;", options);
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getName() == "i");
                CHECK(decl->getType() == type);
                CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
                CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
            } while (std::next_permutation(variants.begin(), variants.end()));
        }
        SEMA_PRODUCES("float int i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'int'"));
        SEMA_PRODUCES("long double long i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'long double'", "'long'"));
        SEMA_PRODUCES("double long long i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'double long'", "'long'"));
    }
    SECTION("Multiple declarations")
    {
        auto [translationUnit, errors] = generateSemantics("int i,f;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 2);
        {
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
            CHECK(decl->getName() == "i");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
            CHECK(decl->getName() == "f");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Linkage")
    {
        SECTION("Objects at file scope are external by default")
        {
            auto [translationUnit, errors] = generateSemantics("int i;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
        }
        SECTION("Internal linkage")
        {
            auto [translationUnit, errors] = generateSemantics("static int i;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::Internal);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
        }
        SEMA_PRODUCES("static extern int i;",
                      ProducesError(ONLY_ONE_STORAGE_SPECIFIER) && ProducesNote(PREVIOUS_STORAGE_SPECIFIER_HERE));
    }
    SECTION("Lifetime")
    {
        SECTION("File scope")
        {
            auto [translationUnit, errors] = generateSemantics("int i;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
            SEMA_PRODUCES("auto int i;", ProducesError(DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO));
            SEMA_PRODUCES("register int i;", ProducesError(DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_REGISTER));
        }
    }
    SECTION("Typedef")
    {
        SECTION("Simple")
        {
            auto [translationUnit, errors] = generateSemantics("typedef long double ld;\n"
                                                               "ld d;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getName() == "d");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createLongDouble(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Stacking const volatile")
        {
            auto [translationUnit, errors] = generateSemantics("typedef long double ld;\n"
                                                               "const volatile ld d;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getName() == "d");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createLongDouble(true, true, cld::LanguageOptions::native()));
        }
        SEMA_PRODUCES("typedef int i;\n"
                      "typedef float i;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'i'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        SEMA_PRODUCES("typedef int i;\n"
                      "typedef int i;",
                      ProducesNothing());
    }
    SEMA_PRODUCES("int;", ProducesError(DECLARATION_DOES_NOT_DECLARE_ANYTHING));
    SEMA_PRODUCES("struct f;", !ProducesError(DECLARATION_DOES_NOT_DECLARE_ANYTHING));
    SEMA_PRODUCES("int i;\n"
                  "float i;",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'i'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
}

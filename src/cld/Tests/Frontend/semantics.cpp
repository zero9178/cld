#include <catch.hpp>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/SemanticAnalysis.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>

#include <array>

#include "TestConfig.hpp"

static std::pair<cld::Semantics::TranslationUnit, std::string>
    generateSemantics(std::string source, const cld::LanguageOptions& options = cld::LanguageOptions::native())
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    cld::PPSourceObject tokens;
    bool errors = false;
    tokens = cld::Lexer::tokenize(std::move(source), options, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    tokens = cld::PP::preprocess(std::move(tokens), &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    static cld::CSourceObject ctokens;
    ctokens = cld::Lexer::toCTokens(tokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto parsing = cld::Parser::buildTree(ctokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    cld::Semantics::SemanticAnalysis analysis(ctokens, &ss);
    auto semantics = analysis.visit(parsing);
    return {std::move(semantics), ss.str()};
}

static std::pair<cld::Semantics::TranslationUnit, std::string> generateSemantics(std::string source, cld::Triple triple)
{
    return generateSemantics(std::move(source), cld::LanguageOptions::fromTriple(triple));
}

#define SEMA_PRODUCES(source, matcher)               \
    [&](std::string input) {                         \
        auto text = generateSemantics(input).second; \
        CHECK_THAT(text, matcher);                   \
        llvm::errs() << text;                        \
    }(source)

using namespace cld::Errors::Semantics;
using namespace cld::Errors;
using namespace cld::Warnings::Semantics;
using namespace cld::Warnings;
using namespace cld::Notes;
using namespace cld::Notes::Semantics;

TEST_CASE("Semantics declarations", "[semantics]")
{
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
            CHECK(decl->getNameToken()->getText() == "i");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
            CHECK(decl->getNameToken()->getText() == "f");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Linkage")
    {
        SECTION("External linkage")
        {
            SECTION("Simple")
            {
                auto [translationUnit, errors] = generateSemantics("extern int i;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
                CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
                CHECK(decl->getKind() == cld::Semantics::Declaration::Kind::DeclarationOnly);
            }
            SECTION("With init")
            {
                auto [translationUnit, errors] = generateSemantics("extern int i = 5;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
                CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
                CHECK(decl->getKind() == cld::Semantics::Declaration::Kind::DeclarationOnly);
            }
            SEMA_PRODUCES("void foo(void) {\n"
                          " extern int i = 0;\n"
                          "}",
                          ProducesError(CANNOT_INITIALIZE_STATIC_OR_EXTERN_VARIABLE_AT_BLOCK_SCOPE));
        }
        SECTION("Prior internal linkage overwrites external")
        {
            auto [translationUnit, errors] = generateSemantics("static int i;\n"
                                                               "extern int i;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit.getGlobals().size() == 2);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::Internal);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
            CHECK(decl->getKind() == cld::Semantics::Declaration::Kind::TentativeDefinition);
            SEMA_PRODUCES("static int i;\n"
                          "int i;",
                          ProducesError(STATIC_VARIABLE_N_REDEFINED_WITHOUT_STATIC, "'i'"));
        }
        SECTION("External definitions with static linkage")
        {
            SECTION("Tentative definition")
            {
                auto [translationUnit, errors] = generateSemantics("static int i;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getLinkage() == cld::Semantics::Linkage::Internal);
                CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
                CHECK(decl->getKind() == cld::Semantics::Declaration::Kind::TentativeDefinition);
            }
            SECTION("Definition")
            {
                auto [translationUnit, errors] = generateSemantics("static int i = 0;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getLinkage() == cld::Semantics::Linkage::Internal);
                CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
                CHECK(decl->getKind() == cld::Semantics::Declaration::Kind::Definition);
            }
            SEMA_PRODUCES("static int i;\n"
                          "static int i;",
                          ProducesNoErrors());
            SEMA_PRODUCES("static int i;\n"
                          "static int i = 0;",
                          ProducesNoErrors());
            SEMA_PRODUCES("static int i;\n"
                          "static int i = 0;",
                          ProducesNoErrors());
            SEMA_PRODUCES("static int i = 0;\n"
                          "static int i = 0;",
                          ProducesError(REDEFINITION_OF_SYMBOL_N, "'i'"));
        }
        SECTION("Objects at function or block scope are None by default")
        {
            SECTION("None")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                    translationUnit.getGlobals()[0]));
                auto& func =
                    *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() == 1);
                auto& var = func.getCompoundStatement().getCompoundItems()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(var));
                auto& decl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(var);
                CHECK(decl.getLinkage() == cld::Semantics::Linkage::None);
            }
            SECTION("extern")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    extern int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                    translationUnit.getGlobals()[0]));
                auto& func =
                    *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() == 1);
                auto& var = func.getCompoundStatement().getCompoundItems()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(var));
                auto& decl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(var);
                CHECK(decl.getLinkage() == cld::Semantics::Linkage::External);
            }
        }
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
        SECTION("Block and function scope")
        {
            SECTION("Default")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                    translationUnit.getGlobals()[0]));
                auto& func =
                    *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() == 1);
                auto& var = func.getCompoundStatement().getCompoundItems()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(var));
                auto& decl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(var);
                CHECK(decl.getLifetime() == cld::Semantics::Lifetime::Automatic);
            }
            SECTION("auto")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    auto int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                    translationUnit.getGlobals()[0]));
                auto& func =
                    *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() == 1);
                auto& var = func.getCompoundStatement().getCompoundItems()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(var));
                auto& decl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(var);
                CHECK(decl.getLifetime() == cld::Semantics::Lifetime::Automatic);
            }
            SECTION("static")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    static int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                    translationUnit.getGlobals()[0]));
                auto& func =
                    *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() == 1);
                auto& var = func.getCompoundStatement().getCompoundItems()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(var));
                auto& decl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(var);
                CHECK(decl.getLifetime() == cld::Semantics::Lifetime::Static);
            }
            SECTION("extern")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    extern int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                    translationUnit.getGlobals()[0]));
                auto& func =
                    *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() == 1);
                auto& var = func.getCompoundStatement().getCompoundItems()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(var));
                auto& decl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(var);
                CHECK(decl.getLifetime() == cld::Semantics::Lifetime::Static);
            }
            SECTION("register")
            {
                SECTION("Param list")
                {
                    auto [translationUnit, errors] = generateSemantics("void foo(register int i)\n"
                                                                       "{}\n");
                    REQUIRE_THAT(errors, ProducesNothing());
                    REQUIRE(translationUnit.getGlobals().size() == 1);
                    REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                        translationUnit.getGlobals()[0]));
                    auto& func =
                        *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                    REQUIRE(func.getParameterDeclarations().size() == 1);
                    auto& decl = *func.getParameterDeclarations()[0];
                    CHECK(decl.getLifetime() == cld::Semantics::Lifetime::Register);
                }
                SECTION("Identifier list")
                {
                    auto [translationUnit, errors] = generateSemantics("void foo(i) register int i;\n"
                                                                       "{}\n");
                    REQUIRE_THAT(errors, ProducesNothing());
                    REQUIRE(translationUnit.getGlobals().size() == 1);
                    REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(
                        translationUnit.getGlobals()[0]));
                    auto& func =
                        *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(translationUnit.getGlobals()[0]);
                    REQUIRE(func.getParameterDeclarations().size() == 1);
                    auto& decl = *func.getParameterDeclarations()[0];
                    CHECK(decl.getLifetime() == cld::Semantics::Lifetime::Register);
                }
            }
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
            CHECK(decl->getNameToken()->getText() == "d");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createLongDouble(false, false, cld::LanguageOptions::native()));
            CHECK(decl->getType().isTypedef());
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
            CHECK(decl->getNameToken()->getText() == "d");
            CHECK(decl->getType()
                  == cld::Semantics::PrimitiveType::createLongDouble(true, true, cld::LanguageOptions::native()));
        }
        SECTION("Variable length array")
        {
            auto [translationUnit, errors] = generateSemantics("void foo(int n) {\n"
                                                               "typedef int r[2 * n][n++];\n"
                                                               "r f;\n"
                                                               "}");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            auto& global = translationUnit.getGlobals()[0];
            REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::FunctionDefinition>>(global));
            auto& def = *cld::get<std::unique_ptr<cld::Semantics::FunctionDefinition>>(global);
            REQUIRE(def.getCompoundStatement().getCompoundItems().size() == 3);
            auto& first = def.getCompoundStatement().getCompoundItems()[0];
            REQUIRE(std::holds_alternative<std::shared_ptr<const cld::Semantics::Expression>>(first));
            auto& firstExpr = *cld::get<std::shared_ptr<const cld::Semantics::Expression>>(first);
            CHECK(std::holds_alternative<cld::Semantics::BinaryOperator>(firstExpr.getVariant()));
            auto& second = def.getCompoundStatement().getCompoundItems()[1];
            REQUIRE(std::holds_alternative<std::shared_ptr<const cld::Semantics::Expression>>(second));
            auto& secondExpr = *cld::get<std::shared_ptr<const cld::Semantics::Expression>>(second);
            CHECK(std::holds_alternative<cld::Semantics::UnaryOperator>(secondExpr.getVariant()));
            auto& third = def.getCompoundStatement().getCompoundItems()[2];
            REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(third));
            auto& thirdDecl = *cld::get<std::unique_ptr<cld::Semantics::Declaration>>(third);
            CHECK(std::holds_alternative<cld::Semantics::ValArrayType>(thirdDecl.getType().getVariant()));
            SEMA_PRODUCES("extern int i;\n"
                          "typedef int n[i];",
                          ProducesError(VARIABLY_MODIFIED_TYPEDEF_NOT_ALLOWED_AT_FILE_SCOPE));
            SEMA_PRODUCES("extern int i;\n"
                          "void foo(void) {\n"
                          "typedef int n[i];\n"
                          "}",
                          ProducesNoErrors());
            SEMA_PRODUCES("extern int i;\n"
                          "void foo(void) {\n"
                          "int n[i] = {5.3034};\n"
                          "}",
                          ProducesError(CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE));
        }
        SEMA_PRODUCES("typedef int i;\n"
                      "typedef float i;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'i'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        SEMA_PRODUCES("typedef int i;\n"
                      "typedef int i;",
                      ProducesNoErrors());
        SEMA_PRODUCES("typedef int i;\n"
                      "i float f;",
                      ProducesError(EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_TYPENAME));
        SEMA_PRODUCES("typedef void V;", ProducesNoErrors());
        SEMA_PRODUCES("typedef struct Point { float x,y; };", ProducesError(TYPEDEF_DECLARATION_DOES_NOT_HAVE_A_NAME));
        SEMA_PRODUCES("typedef struct Point Point;\n"
                      "\n"
                      "struct Point {\n"
                      " float x,y;\n"
                      "};\n"
                      "Point p;",
                      ProducesNoErrors());
    }
    SEMA_PRODUCES("int;", ProducesError(DECLARATION_DOES_NOT_DECLARE_ANYTHING));
    SEMA_PRODUCES("struct f;", !ProducesError(DECLARATION_DOES_NOT_DECLARE_ANYTHING));
    SEMA_PRODUCES("int i;\n"
                  "float i;",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'i'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("inline int f;", ProducesError(INLINE_ONLY_ALLOWED_FOR_FUNCTIONS));
}

TEST_CASE("Semantics primitive declarations", "[semantics]")
{
    auto [isConst,isVolatile] = GENERATE(table<bool,bool>({std::tuple{false,true},std::tuple{false,true}}));
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
        SEMA_PRODUCES("void foo(void) {\n" + text
                          + "\n"
                            "}",
                      ProducesError(DECLARATION_MUST_NOT_BE_VOID));
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
        {cld::Semantics::PrimitiveType::createUnsignedLong(isConst, isVolatile, options), {"long", "unsigned", "int"}},
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
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getNameToken()->getText() == "i");
            CHECK(decl->getType() == type);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
        } while (std::next_permutation(variants.begin(), variants.end()));
    }
    SEMA_PRODUCES("float int i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'int'"));
    SEMA_PRODUCES("float void i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'void'"));
    SEMA_PRODUCES("float char i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'char'"));
    SEMA_PRODUCES("float short i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'short'"));
    SEMA_PRODUCES("float long i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'long'"));
    SEMA_PRODUCES("float float i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'float'"));
    SEMA_PRODUCES("float double i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'double'"));
    SEMA_PRODUCES("float signed i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'signed'"));
    SEMA_PRODUCES("float unsigned i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'unsigned'"));
    SEMA_PRODUCES("float _Bool i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'_Bool'"));
    SEMA_PRODUCES("float enum f i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'enum'"));
    SEMA_PRODUCES("float struct f i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'struct'"));
    SEMA_PRODUCES("float union f i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'float'", "'union'"));
    SEMA_PRODUCES("long double long i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'long double'", "'long'"));
    SEMA_PRODUCES("double long long i;", ProducesError(CANNOT_COMBINE_N_WITH_N, "'double long'", "'long'"));
}

TEST_CASE("Semantics pointer declarations", "[semantics]")
{
    SECTION("Pointers")
    {
        SECTION("Simple")
        {
            auto [translationUnit, errors] = generateSemantics("int *f;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getNameToken()->getText() == "f");
            CHECK(decl->getType()
                  == cld::Semantics::PointerType::create(
                      false, false, false,
                      cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
        }
        SECTION("Element qualified")
        {
            SECTION("West const")
            {
                auto isConst = GENERATE(false, true);
                auto isVolatile = GENERATE(false, true);
                std::string text = "int *f;";
                if (isConst)
                {
                    text = "const " + text;
                }
                if (isVolatile)
                {
                    text = "volatile " + text;
                }
                auto [translationUnit, errors] = generateSemantics(text);
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getNameToken()->getText() == "f");
                CHECK(decl->getType()
                      == cld::Semantics::PointerType::create(false, false, false,
                                                             cld::Semantics::PrimitiveType::createInt(
                                                                 isConst, isVolatile, cld::LanguageOptions::native())));
            }
            SECTION("East const")
            {
                auto isConst = GENERATE(false, true);
                auto isVolatile = GENERATE(false, true);
                std::string text = "int";
                if (isConst)
                {
                    text += " const ";
                }
                if (isVolatile)
                {
                    text += " volatile ";
                }
                text += "*f;";
                auto [translationUnit, errors] = generateSemantics(text);
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                CHECK(decl->getNameToken()->getText() == "f");
                CHECK(decl->getType()
                      == cld::Semantics::PointerType::create(false, false, false,
                                                             cld::Semantics::PrimitiveType::createInt(
                                                                 isConst, isVolatile, cld::LanguageOptions::native())));
            }
        }
        SECTION("Pointer qualified")
        {
            auto isConst = GENERATE(false, true);
            auto isVolatile = GENERATE(false, true);
            auto isRestrict = GENERATE(false, true);
            std::string text = "int * ";
            if (isConst)
            {
                text += "const ";
            }
            if (isVolatile)
            {
                text += "volatile ";
            }
            if (isRestrict)
            {
                text += "restrict ";
            }
            text += "f;";
            auto [translationUnit, errors] = generateSemantics(text);
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getNameToken()->getText() == "f");
            CHECK(decl->getType()
                  == cld::Semantics::PointerType::create(
                      isConst, isVolatile, isRestrict,
                      cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
        }
        SECTION("restrict")
        {
            SEMA_PRODUCES("int (* restrict f)();",
                          ProducesError(ELEMENT_TYPE_OF_POINTER_WITH_RESTRICT_QUALIFIER_MUST_NOT_BE_A_FUNCTION_TYPE));
            SEMA_PRODUCES("restrict int f;", ProducesError(RESTRICT_CAN_ONLY_BE_APPLIED_TO_POINTERS));
        }
        SEMA_PRODUCES("int (*f)(d);", ProducesError(IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION));
    }
}

TEST_CASE("Semantics array declarations", "[semantics]")
{
    SECTION("Arrays")
    {
        SECTION("Simple")
        {
            auto [translationUnit, errors] = generateSemantics("int f[1];");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getNameToken()->getText() == "f");
            CHECK(decl->getType()
                  == cld::Semantics::ArrayType::create(
                      false, false, false, false,
                      cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 1));
        }
        SECTION("Order")
        {
            auto [translationUnit, errors] = generateSemantics("int (*f[1]);");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getNameToken()->getText() == "f");
            CHECK(decl->getType()
                  == cld::Semantics::ArrayType::create(
                      false, false, false, false,
                      cld::Semantics::PointerType::create(
                          false, false, false,
                          cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native())),
                      1));
        }
        SEMA_PRODUCES("int f[0];", ProducesError(ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO));
        SEMA_PRODUCES("int f[-5];", ProducesError(ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO));
        SEMA_PRODUCES("int f[5.0];", ProducesError(ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE));
        SEMA_PRODUCES("int f[1]();", ProducesError(ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION));
        SEMA_PRODUCES("void f[1];", ProducesError(ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE));
        SEMA_PRODUCES("struct {\n"
                      " int r,f[];\n"
                      "} f[1];",
                      ProducesError(ARRAY_ELEMENT_TYPE_MUST_NOT_CONTAIN_A_FLEXIBLE_ARRAY_MEMBER));
        SEMA_PRODUCES("int f[static 1];", ProducesError(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_STATIC));
        SEMA_PRODUCES("int f[const 1];", ProducesError(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED));
        SEMA_PRODUCES("int f[];", ProducesNothing());

        SEMA_PRODUCES("int n = 1;\n"
                      "int f[n];",
                      ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE));
        SEMA_PRODUCES("int n = 1;\n"
                      "int f[n][5];",
                      ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE));
        SEMA_PRODUCES("int f[*];", ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE)
                                       && ProducesError(STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES));
        SEMA_PRODUCES("int f[*][5];",
                      ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE)
                          && ProducesError(STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES));
        SEMA_PRODUCES("int foo(void) {\n"
                      "int f[*];\n"
                      "}",
                      !ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE)
                          && ProducesError(STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES));
        SEMA_PRODUCES("int foo(void) {\n"
                      "int f[*][5];\n"
                      "}",
                      !ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE)
                          && ProducesError(STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES));
        SEMA_PRODUCES("int n = 1;\n"
                      "int f[static n];",
                      ProducesError(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_STATIC));
        SEMA_PRODUCES("int f[const 5];", ProducesError(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED));
        SEMA_PRODUCES("int f[volatile 5];", ProducesError(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED));
        SEMA_PRODUCES("int f[restrict 5];", ProducesError(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED));
    }
}

TEST_CASE("Semantics function prototypes", "[semantics]")
{
    SECTION("Simple")
    {
        auto [translationUnit, errors] = generateSemantics("int f(int,float);");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getNameToken()->getText() == "f");
        CHECK(decl->getType()
              == cld::Semantics::FunctionType::create(
                  cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()),
                  {{cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), ""},
                   {cld::Semantics::PrimitiveType::createFloat(false, false), ""}},
                  false, false));
    }
    SECTION("Parameters")
    {
        SEMA_PRODUCES("int f(int a[]);", ProducesNothing());
        SEMA_PRODUCES("int f(void);", ProducesNothing());
        SEMA_PRODUCES("int f(const void);", ProducesError(VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER));
        SEMA_PRODUCES("int f(int a[*]);", ProducesNothing());
        SEMA_PRODUCES("int f(int a,float a);", ProducesNothing());
        SEMA_PRODUCES("int f(register int a);", ProducesNothing());
        SEMA_PRODUCES("int f(int a[static 6]);", ProducesNothing());
        SEMA_PRODUCES("int f(int a[6][static 5]);", ProducesError(STATIC_ONLY_ALLOWED_IN_OUTERMOST_ARRAY));
    }
    SEMA_PRODUCES("int f(int) = 5;", ProducesError(FUNCTION_PROTOTYPE_MUST_NOT_HAVE_AN_INITIALIZER));
    SEMA_PRODUCES("inline int f(int);", ProducesNothing());
    SEMA_PRODUCES("int f(extern int a);",
                  ProducesError(NO_STORAGE_CLASS_SPECIFIER_ALLOWED_IN_PARAMETER_BESIDES_REGISTER));
    SEMA_PRODUCES("int (f(int a)[10]);", ProducesError(FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY));
    SEMA_PRODUCES("int (f(int a))(float);", ProducesError(FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION));
}

TEST_CASE("Semantics struct and union type", "[semantics]")
{
    SECTION("Simple struct")
    {
        auto [translationUnit, errors] = generateSemantics("struct A{ int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::StructType>(decl->getType().getVariant()));
        CHECK(cld::get<cld::Semantics::StructType>(decl->getType().getVariant()).getName() == "A");
        CHECK(cld::get<cld::Semantics::StructType>(decl->getType().getVariant()).getScopeOrId() == 0);
    }
    SECTION("Simple union")
    {
        auto [translationUnit, errors] = generateSemantics("union A{ int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::UnionType>(decl->getType().getVariant()));
        CHECK(cld::get<cld::Semantics::UnionType>(decl->getType().getVariant()).getName() == "A");
        CHECK(cld::get<cld::Semantics::UnionType>(decl->getType().getVariant()).getScopeOrId() == 0);
    }
    SECTION("Anonymous struct")
    {
        auto [translationUnit, errors] = generateSemantics("struct { int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::AnonymousStructType>(decl->getType().getVariant()));
        auto& anon = cld::get<cld::Semantics::AnonymousStructType>(decl->getType().getVariant());
        CHECK(anon.getFields().size() == 3);
        CHECK(anon.getFields()[0].name == "i");
        CHECK(*anon.getFields()[0].type
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK_FALSE(anon.getFields()[0].bitFieldBounds);
        CHECK(anon.getFields()[1].name == "f");
        CHECK(*anon.getFields()[1].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[1].bitFieldBounds);
        CHECK(anon.getFields()[2].name == "r");
        CHECK(*anon.getFields()[2].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[2].bitFieldBounds);
    }
    SECTION("Anonymous union")
    {
        auto [translationUnit, errors] = generateSemantics("union { int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::AnonymousUnionType>(decl->getType().getVariant()));
        auto& anon = cld::get<cld::Semantics::AnonymousUnionType>(decl->getType().getVariant());
        CHECK(anon.getFields().size() == 3);
        CHECK(anon.getFields()[0].name == "i");
        CHECK(*anon.getFields()[0].type
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK_FALSE(anon.getFields()[0].bitFieldBounds);
        CHECK(anon.getFields()[1].name == "f");
        CHECK(*anon.getFields()[1].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[1].bitFieldBounds);
        CHECK(anon.getFields()[2].name == "r");
        CHECK(*anon.getFields()[2].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[2].bitFieldBounds);
    }
    SECTION("Flexible array member")
    {
        SEMA_PRODUCES("struct A{ int f[]; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "'int[]'"));
        SEMA_PRODUCES("struct A{ int r;int f[],c; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "'int[]'"));
        SEMA_PRODUCES("struct A{ int r,f[]; };", ProducesNothing());
        SEMA_PRODUCES("union A{ int r,f[]; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION, "'int[]'"));
        SEMA_PRODUCES("struct A{ int r;int f[]; };", ProducesNothing());
        SEMA_PRODUCES("union A{ int r;int f[]; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION, "'int[]'"));
        SEMA_PRODUCES("struct A {\n"
                      " int r;\n"
                      " int f[];\n"
                      "};\n"
                      "struct B {\n"
                      " struct A a;\n"
                      "};",
                      ProducesError(STRUCT_WITH_FLEXIBLE_ARRAY_MEMBER_NOT_ALLOWED_IN_STRUCT));
        SEMA_PRODUCES("struct A {\n"
                      " int r;\n"
                      " int f[];\n"
                      "};\n"
                      "union B {\n"
                      " struct A a;\n"
                      "};",
                      ProducesNothing());
        SEMA_PRODUCES(
            "struct A {\n"
            " int r;\n"
            " int f[];\n"
            "};\n"
            "union B {\n"
            " struct A a;\n"
            "};\n"
            "struct C {\n"
            " union B r;\n"
            "};",
            ProducesError(UNION_WITH_STRUCT_OR_UNION_CONTAINING_A_FLEXIBLE_ARRAY_MEMBER_IS_NOT_ALLOWED_IN_STRUCT));
    }
    SECTION("Bitfields")
    {
        SECTION("sizeof")
        {
            SECTION("Discrete bitfields")
            {
                // Windows uses discrete bitfields
                auto [translationUnit, errors] = generateSemantics("struct A\n"
                                                                   "{\n"
                                                                   "    int a : 8;\n"
                                                                   "    int b : 3;\n"
                                                                   "    int c : 1;\n"
                                                                   "    int d : 1;\n"
                                                                   "    int e : 1;\n"
                                                                   "    _Bool f : 1;\n"
                                                                   "};\n"
                                                                   "\n"
                                                                   "int a[sizeof(struct A)];",
                                                                   x64windowsMsvc);
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().getVariant()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().getVariant());
                CHECK(array.getSize() == 8);
            }
            SECTION("System V bitfields")
            {
                auto [translationUnit, errors] = generateSemantics("struct A\n"
                                                                   "{\n"
                                                                   "    int a : 8;\n"
                                                                   "    int b : 3;\n"
                                                                   "    int c : 1;\n"
                                                                   "    int d : 1;\n"
                                                                   "    int e : 1;\n"
                                                                   "    _Bool f : 1;\n"
                                                                   "};\n"
                                                                   "\n"
                                                                   "int a[sizeof(struct A)];",
                                                                   x64linux);
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().getVariant()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().getVariant());
                CHECK(array.getSize() == 4);
            }
        }
        SECTION("Layout and bounds")
        {
            std::string_view source = "struct A\n"
                                      "{\n"
                                      "    int a : 8;\n"
                                      "    int b : 3;\n"
                                      "    int c : 1;\n"
                                      "    int d : 1;\n"
                                      "    int e : 1;\n"
                                      "    _Bool f : 1;\n"
                                      "};";
            bool errors = false;
            auto tokens = cld::Lexer::tokenize(cld::to_string(source), cld::LanguageOptions::fromTriple(x64windowsMsvc),
                                               &llvm::errs(), &errors);
            REQUIRE_FALSE(errors);
            auto ctokens = cld::Lexer::toCTokens(tokens, &llvm::errs(), &errors);
            REQUIRE_FALSE(errors);
            auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
            REQUIRE_FALSE(errors);
            auto program = cld::Semantics::analyse(tree, std::move(ctokens), &llvm::errs(), &errors);
            REQUIRE_FALSE(errors);
            auto* structDef = program.getStructDefinition("A", 0);
            REQUIRE(structDef);
            REQUIRE(structDef->getFields().size() == 6);
            CHECK(structDef->getLayout().size() == 2);

            CHECK(structDef->getFields()[0].layoutIndex == 0);
            REQUIRE(structDef->getFields()[0].bitFieldBounds);
            CHECK(*structDef->getFields()[0].bitFieldBounds == std::pair{0u, 8u});

            CHECK(structDef->getFields()[1].layoutIndex == 0);
            REQUIRE(structDef->getFields()[1].bitFieldBounds);
            CHECK(*structDef->getFields()[1].bitFieldBounds == std::pair{8u, 11u});

            CHECK(structDef->getFields()[2].layoutIndex == 0);
            REQUIRE(structDef->getFields()[2].bitFieldBounds);
            CHECK(*structDef->getFields()[2].bitFieldBounds == std::pair{11u, 12u});

            CHECK(structDef->getFields()[3].layoutIndex == 0);
            REQUIRE(structDef->getFields()[3].bitFieldBounds);
            CHECK(*structDef->getFields()[3].bitFieldBounds == std::pair{12u, 13u});

            CHECK(structDef->getFields()[4].layoutIndex == 0);
            REQUIRE(structDef->getFields()[4].bitFieldBounds);
            CHECK(*structDef->getFields()[4].bitFieldBounds == std::pair{13u, 14u});

            CHECK(structDef->getFields()[5].layoutIndex == 1);
            REQUIRE(structDef->getFields()[5].bitFieldBounds);
            CHECK(*structDef->getFields()[5].bitFieldBounds == std::pair{0u, 1u});
        }
        SEMA_PRODUCES("struct A {\n"
                      " float i :5;\n"
                      "};",
                      ProducesError(BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL));
        SEMA_PRODUCES("struct A {\n"
                      " int i[3] :5;\n"
                      "};",
                      ProducesError(BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL));
        SEMA_PRODUCES("struct A {\n"
                      " int i : -5;\n"
                      "};",
                      ProducesError(BITFIELD_MUST_BE_OF_SIZE_ZERO_OR_GREATER));
        SEMA_PRODUCES("struct A {\n"
                      " int i : 1 << 7;\n"
                      "};",
                      ProducesError(BITFIELD_MUST_NOT_HAVE_A_GREATER_WIDTH_THAN_THE_TYPE));
        SEMA_PRODUCES("struct A {\n"
                      " int i : 0;\n"
                      "};",
                      ProducesError(BITFIELD_WITH_SIZE_ZERO_MAY_NOT_HAVE_A_NAME));
    }
    SEMA_PRODUCES("struct A {\n"
                  " struct B {\n"
                  "     int r;\n"
                  " } b;\n"
                  "};\n"
                  "\n"
                  "struct B {\n"
                  " int r;\n"
                  "};",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'B'"));
    SEMA_PRODUCES("struct A {\n"
                  " struct B {\n"
                  "     int r;\n"
                  " } b;\n"
                  "};\n"
                  "\n"
                  "struct B a;",
                  ProducesNothing());
    SEMA_PRODUCES("struct A{ void f; };", ProducesError(VOID_TYPE_NOT_ALLOWED_IN_STRUCT));
    SEMA_PRODUCES("union A{ void f; };", ProducesError(VOID_TYPE_NOT_ALLOWED_IN_UNION));
    SEMA_PRODUCES("struct A{ struct r f; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "'struct r'"));
    SEMA_PRODUCES("union A{ struct r f; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION, "'struct r'"));
    SEMA_PRODUCES("struct A{ struct A f; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "'struct A'"));
    SEMA_PRODUCES("union A{ union A f; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION, "'union A'"));
    SEMA_PRODUCES("struct A{ int f(); };", ProducesError(FUNCTION_TYPE_NOT_ALLOWED_IN_STRUCT));
    SEMA_PRODUCES("union A{ int f(); };", ProducesError(FUNCTION_TYPE_NOT_ALLOWED_IN_UNION));
    SEMA_PRODUCES("struct A{ int f[*]; };", ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_STRUCT));
    SEMA_PRODUCES("union A{ int f[*]; };", ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_UNION));
}

TEST_CASE("Semantics enums", "[semantics]")
{
    SECTION("Values")
    {
        SECTION("Default values")
        {
            auto [tu, errors] = generateSemantics("enum A {\n"
                                                  "_,a,b,c\n"
                                                  "};\n"
                                                  "\n"
                                                  "int a_[a];\n"
                                                  "int b_[b];\n"
                                                  "int c_[c];");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(tu.getGlobals().size() == 3);
            std::uint64_t i = 1;
            for (auto& iter : tu.getGlobals())
            {
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().getVariant()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().getVariant());
                CHECK(array.getSize() == i++);
            }
        }
        SECTION("Explicit values")
        {
            auto [tu, errors] = generateSemantics("enum A {\n"
                                                  "a = 5,b = 4,c = 3\n"
                                                  "};\n"
                                                  "\n"
                                                  "int a_[a];\n"
                                                  "int b_[b];\n"
                                                  "int c_[c];");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(tu.getGlobals().size() == 3);
            std::uint64_t i = 5;
            for (auto& iter : tu.getGlobals())
            {
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().getVariant()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().getVariant());
                CHECK(array.getSize() == i--);
            }
        }
        SEMA_PRODUCES("enum A {\n"
                      "a = 1ull << 40,\n"
                      "};",
                      ProducesError(VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT));
    }
    SECTION("Anonymous enum")
    {
        SECTION("Default values")
        {
            auto [tu, errors] = generateSemantics("enum {\n"
                                                  "_,a,b,c\n"
                                                  "};\n"
                                                  "\n"
                                                  "int a_[a];\n"
                                                  "int b_[b];\n"
                                                  "int c_[c];");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(tu.getGlobals().size() == 3);
            std::uint64_t i = 1;
            for (auto& iter : tu.getGlobals())
            {
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().getVariant()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().getVariant());
                CHECK(array.getSize() == i++);
            }
        }
        SECTION("Explicit values")
        {
            auto [tu, errors] = generateSemantics("enum {\n"
                                                  "a = 5,b = 4,c = 3\n"
                                                  "};\n"
                                                  "\n"
                                                  "int a_[a];\n"
                                                  "int b_[b];\n"
                                                  "int c_[c];");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(tu.getGlobals().size() == 3);
            std::uint64_t i = 5;
            for (auto& iter : tu.getGlobals())
            {
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().getVariant()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().getVariant());
                CHECK(array.getSize() == i--);
            }
        }
        SEMA_PRODUCES("enum {\n"
                      "a = 1ull << 40,\n"
                      "};",
                      ProducesError(VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT));
    }
    SEMA_PRODUCES("enum A;", ProducesError(FORWARD_DECLARING_AN_ENUM_IS_NOT_ALLOWED));
    SEMA_PRODUCES("enum A {\n"
                  "l,\n"
                  "};\n"
                  "enum A;",
                  !ProducesError(FORWARD_DECLARING_AN_ENUM_IS_NOT_ALLOWED));
    SEMA_PRODUCES("enum A {\n"
                  "l\n"
                  "};\n"
                  "\n"
                  "enum A {\n"
                  "f\n"
                  "};",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'A'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("enum A {\n"
                  "l\n"
                  "};\n"
                  "\n"
                  "enum B {\n"
                  "l\n"
                  "};",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'l'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("enum A {\n"
                  "l\n"
                  "};\n"
                  "\n"
                  "enum {\n"
                  "l\n"
                  "};",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'l'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("enum {\n"
                  "a = (float)5,\n"
                  "};\n"
                  "\n"
                  "int a_[a];\n",
                  ProducesError(CANNOT_CAST_TO_NON_INTEGER_TYPE_IN_INTEGER_CONSTANT_EXPRESSION)
                      && !ProducesError(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE));
}

TEST_CASE("Semantics function definitions")
{
    SEMA_PRODUCES("static int bar(int n,int r[3][5][n]) {}", ProducesNothing());
    SECTION("Identifier list")
    {
        SEMA_PRODUCES("void foo(voi)\n"
                      "{}",
                      ProducesError(PARAMETER_N_IN_IDENTIFIER_LIST_DOES_NOT_HAVE_A_MATCHING_DECLARATION, "'voi'"));
        SEMA_PRODUCES("int foo(a) int a = 5;{}",
                      ProducesError(DECLARATION_OF_IDENTIFIER_LIST_NOT_ALLOWED_TO_HAVE_AN_INITIALIZER));
        SEMA_PRODUCES("int foo(a) int a;float;{}",
                      ProducesError(DECLARATION_OF_IDENTIFIER_LIST_MUST_DECLARE_AT_LEAST_ONE_IDENTIFIER));
        SEMA_PRODUCES("int foo(a) int a;float b;{}",
                      ProducesError(DECLARATION_OF_IDENTIFIER_LIST_NOT_BELONGING_TO_ANY_PARAMETER));
        SEMA_PRODUCES("int foo(a,b,c) int a;{}",
                      ProducesError(PARAMETER_N_IN_IDENTIFIER_LIST_DOES_NOT_HAVE_A_MATCHING_DECLARATION, "'b'")
                          && ProducesError(PARAMETER_N_IN_IDENTIFIER_LIST_DOES_NOT_HAVE_A_MATCHING_DECLARATION, "'c'"));
        SEMA_PRODUCES("int foo(a,b,c) int a,a;{}",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'a'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        SEMA_PRODUCES("int foo(a,b,a) int a,b;{}",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'a'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
        SEMA_PRODUCES("int (*foo(a,b,c))(z) int a,b,c;{}",
                      ProducesError(IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION));
    }
    SEMA_PRODUCES("struct A {\n"
                  " float f;\n"
                  "};\n"
                  "\n"
                  "int foo(struct A {\n"
                  " int r;\n"
                  "} a){}",
                  ProducesNothing());
    SEMA_PRODUCES("struct A {\n"
                  " float f;\n"
                  "};\n"
                  "\n"
                  "int foo(a) struct A { int r; } a; {}",
                  ProducesNothing());
    SEMA_PRODUCES("int foo(int a,int a) {}",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'a'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("int foo(int a,int b) int a; int f; {}",
                  ProducesError(FUNCTION_DEFINITION_WITH_A_PARAMETER_LIST_MUST_NOT_HAVE_DECLARATIONS_FOLLOWING_IT));
    SEMA_PRODUCES("typedef int F(float f);\n"
                  "\n"
                  "F foo\n"
                  "{}",
                  ProducesError(FUNCTION_DEFINITION_MUST_HAVE_A_PARAMETER_LIST));
    SEMA_PRODUCES("auto int foo(){}", ProducesError(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION));
    SEMA_PRODUCES("register int foo(){}", ProducesError(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION));
    SEMA_PRODUCES("typedef int foo(){}", ProducesError(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION));
    SEMA_PRODUCES("int foo{}", ProducesError(FUNCTION_DEFINITION_MUST_HAVE_FUNCTION_TYPE));
    SEMA_PRODUCES("struct R foo(void){}", ProducesError(RETURN_TYPE_OF_FUNCTION_DEFINITION_MUST_BE_A_COMPLETE_TYPE));
}

TEST_CASE("Semantics type compatibility", "[semantics]")
{
    SECTION("Qualifiers")
    {
        SEMA_PRODUCES("int foo;\n"
                      "int foo;",
                      ProducesNothing());
        SEMA_PRODUCES("const int foo;\n"
                      "const int foo;",
                      ProducesNothing());
        SEMA_PRODUCES("volatile int foo;\n"
                      "volatile int foo;",
                      ProducesNothing());
        SEMA_PRODUCES("const volatile int foo;\n"
                      "const int volatile foo;",
                      ProducesNothing());
        SEMA_PRODUCES("const int foo;\n"
                      "int foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("const int foo;\n"
                      "volatile int foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("volatile int foo;\n"
                      "int foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
    }
    SECTION("Arrays")
    {
        SEMA_PRODUCES("int foo[5];\n"
                      "int foo[5];",
                      ProducesNothing());
        SEMA_PRODUCES("int foo[];\n"
                      "int foo[5];",
                      ProducesNothing());
        SEMA_PRODUCES("float foo[5];\n"
                      "int foo[5];",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo[4];\n"
                      "int foo[5];",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo[];\n"
                      "int foo[];",
                      ProducesNothing());
    }
    SECTION("Pointers")
    {
        SEMA_PRODUCES("int* foo;\n"
                      "int* foo;",
                      ProducesNothing());
        SEMA_PRODUCES("int* const foo;\n"
                      "int* const foo;",
                      ProducesNothing());
        SEMA_PRODUCES("int* volatile foo;\n"
                      "int* volatile foo;",
                      ProducesNothing());
        SEMA_PRODUCES("int* restrict foo;\n"
                      "int* restrict foo;",
                      ProducesNothing());
        SEMA_PRODUCES("int* const foo;\n"
                      "int* foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int* volatile foo;\n"
                      "int* foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int* restrict foo;\n"
                      "int* foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("float* foo;\n"
                      "int* foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("const int* foo;\n"
                      "int* foo;",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
    }
    SECTION("Functions")
    {
        SEMA_PRODUCES("int foo();\n"
                      "int foo();",
                      ProducesNothing());
        SEMA_PRODUCES("int foo();\n"
                      "float foo();",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo();\n"
                      "int foo(int a,...);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo();\n"
                      "int foo(int a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo();\n"
                      "int foo(float a);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo();\n"
                      "int foo(const int a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo();\n"
                      "int foo(const float a);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(const float a);\n"
                      "int foo(a) const float a; {}",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(const double a);\n"
                      "int foo(a) const float a; {}",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(int a);\n"
                      "int foo() {}",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(void);\n"
                      "int foo() {}",
                      ProducesNothing());
        SEMA_PRODUCES("int foo();\n"
                      "int foo(a) int a; {}",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a);\n"
                      "int foo(const float a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a);\n"
                      "int foo(int a);",
                      ProducesError(ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'")));
        SEMA_PRODUCES("int foo(float a,...);\n"
                      "int foo(float a,...);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a,...);\n"
                      "int foo(float a);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(float a);\n"
                      "int foo(float a,...);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(float a);\n"
                      "int foo(float a,int);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(float* a);\n"
                      "int foo(float a[]);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a[5]);\n"
                      "int foo(float a[]);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a[const 5]);\n"
                      "int foo(float* const a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a[restrict 5]);\n"
                      "int foo(float* restrict a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float a[volatile 5]);\n"
                      "int foo(float* volatile a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(const float a[static 5]);\n"
                      "int foo(const float* a);",
                      ProducesNothing());
        SEMA_PRODUCES("int foo(float (*a)(int));\n"
                      "int foo(float a(int));",
                      ProducesNothing());
        SEMA_PRODUCES("void foor(int x[restrict 5]);\n"
                      "void foor(int[restrict 5]);",
                      ProducesNothing());
    }
    SECTION("Records")
    {
        SEMA_PRODUCES("void foo(struct A* i) {\n"
                      " struct A {\n"
                      "     int i;\n"
                      " };\n"
                      " i->i;\n"
                      " struct A* f;\n"
                      " i - f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(union A* i) {\n"
                      " union A {\n"
                      "     int i;\n"
                      " };\n"
                      " i->i;\n"
                      " union A* f;\n"
                      " i - f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "struct {\n"
                      " int i;\n"
                      "} *i,*f;\n"
                      " i - f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "struct {\n"
                      " int i;\n"
                      "}* i;\n"
                      "struct {\n"
                      " int i;\n"
                      "}* f;\n"
                      " i - f;\n"
                      "}",
                      !ProducesNoErrors());
    }
}

TEST_CASE("Semantics composite type", "[semantics]")
{
    SECTION("Standard example")
    {
        auto [translationUnit, errors] = generateSemantics("int f(int (*)(),double (*)[3]);\n"
                                                           "int f(int (*)(char *),double (*)[]);");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 2);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[1]);
        CHECK(decl->getNameToken()->getText() == "f");
        CHECK(decl->getType()
              == cld::Semantics::FunctionType::create(
                  cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()),
                  {{cld::Semantics::PointerType::create(
                        false, false, false,
                        cld::Semantics::FunctionType::create(
                            cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()),
                            {{cld::Semantics::PointerType::create(false, false, false,
                                                                  cld::Semantics::PrimitiveType::createChar(
                                                                      false, false, cld::LanguageOptions::native())),
                              ""}},
                            false, false)),
                    ""},
                   {cld::Semantics::PointerType::create(
                        false, false, false,
                        cld::Semantics::ArrayType::create(
                            false, false, false, false, cld::Semantics::PrimitiveType::createDouble(false, false), 3)),
                    ""}},
                  false, false));
    }
}

TEST_CASE("Semantics type printing", "[semantics]")
{
    using namespace cld::Semantics;
    auto toStr = [](const Type& type) {
        cld::CSourceObject object;
        return cld::diag::StringConverter<Type>::inArg(type, object);
    };
    SECTION("Primitives")
    {
        CHECK(toStr(PrimitiveType::createVoid(false, false)) == "void");
        CHECK(toStr(PrimitiveType::createVoid(true, false)) == "const void");
        CHECK(toStr(PrimitiveType::createVoid(false, true)) == "volatile void");
        CHECK(toStr(PrimitiveType::createVoid(true, true)) == "const volatile void");
        CHECK(toStr(PrimitiveType::createChar(false, false, cld::LanguageOptions::native())) == "char");
        CHECK(toStr(PrimitiveType::createSignedChar(false, false)) == "signed char");
        CHECK(toStr(PrimitiveType::createUnsignedChar(false, false)) == "unsigned char");
        CHECK(toStr(PrimitiveType::createShort(false, false, cld::LanguageOptions::native())) == "short");
        CHECK(toStr(PrimitiveType::createUnsignedShort(false, false, cld::LanguageOptions::native()))
              == "unsigned short");
        CHECK(toStr(PrimitiveType::createInt(false, false, cld::LanguageOptions::native())) == "int");
        CHECK(toStr(PrimitiveType::createUnsignedInt(false, false, cld::LanguageOptions::native())) == "unsigned int");
        CHECK(toStr(PrimitiveType::createLong(false, false, cld::LanguageOptions::native())) == "long");
        CHECK(toStr(PrimitiveType::createUnsignedLong(false, false, cld::LanguageOptions::native()))
              == "unsigned long");
        CHECK(toStr(PrimitiveType::createLongLong(false, false)) == "long long");
        CHECK(toStr(PrimitiveType::createUnsignedLongLong(false, false)) == "unsigned long long");
        CHECK(toStr(PrimitiveType::createFloat(false, false)) == "float");
        CHECK(toStr(PrimitiveType::createDouble(false, false)) == "double");
        CHECK(toStr(PrimitiveType::createUnderlineBool(false, false)) == "_Bool");
        CHECK(toStr(PrimitiveType::createLongDouble(false, false, cld::LanguageOptions::native())) == "long double");
    }
    SECTION("Pointers")
    {
        {
            CHECK(toStr(PointerType::create(
                      false, false, false,
                      PointerType::create(false, false, false,
                                          PointerType::create(true, false, false,
                                                              PrimitiveType::createShort(
                                                                  false, false, cld::LanguageOptions::native())))))
                  == "short *const **");
        }
        CHECK(toStr(PointerType::create(
                  false, false, false,
                  ArrayType::create(false, false, false, false, PrimitiveType::createSignedChar(false, false), 5)))
              == "signed char(*)[5]");
        CHECK(toStr(PointerType::create(
                  false, false, false,
                  FunctionType::create(PrimitiveType::createSignedChar(false, false), {}, false, false)))
              == "signed char(*)(void)");
    }
    SECTION("Array")
    {
        CHECK(toStr(ArrayType::create(false, false, false, false,
                                      PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 4))
              == "int[4]");
        CHECK(toStr(ArrayType::create(false, false, false, true,
                                      PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 4))
              == "int[static 4]");
        CHECK(toStr(ArrayType::create(false, false, true, false,
                                      PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 4))
              == "int[restrict 4]");
        CHECK(toStr(ArrayType::create(false, true, false, false,
                                      PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 4))
              == "int[volatile 4]");
        CHECK(toStr(ArrayType::create(true, false, false, false,
                                      PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 4))
              == "int[const 4]");
        CHECK(toStr(ArrayType::create(
                  false, false, false, false,
                  ArrayType::create(false, false, false, false,
                                    PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), 6),
                  4))
              == "int[4][6]");
        CHECK(
            toStr(ArrayType::create(
                false, false, false, false,
                PointerType::create(false, false, false,
                                    FunctionType::create(
                                        PrimitiveType::createInt(false, false, cld::LanguageOptions::native()),
                                        {{PrimitiveType::createChar(false, false, cld::LanguageOptions::native()), ""}},
                                        false, false)),
                5))
            == "int(*[5])(char)");
    }
    SECTION("Functions")
    {
        CHECK(toStr(FunctionType::create(PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), {},
                                         false, false))
              == "int(void)");
        CHECK(toStr(FunctionType::create(PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), {},
                                         false, true))
              == "int()");
        CHECK(toStr(FunctionType::create(PrimitiveType::createInt(false, false, cld::LanguageOptions::native()),
                                         {{PrimitiveType::createInt(false, false, cld::LanguageOptions::native()), ""}},
                                         true, false))
              == "int(int,...)");
        CHECK(toStr(FunctionType::create(
                  PointerType::create(
                      false, false, false,
                      FunctionType::create(PrimitiveType::createInt(false, false, cld::LanguageOptions::native()),
                                           {{PrimitiveType::createFloat(false, false), ""}}, false, false)),
                  {{PrimitiveType::createChar(false, false, cld::LanguageOptions::native()), ""}}, false, false))
              == "int(*(char))(float)");
        CHECK(toStr(AbstractArrayType::create(
                  false, false, false,
                  ArrayType::create(
                      false, false, false, false,
                      PointerType::create(
                          false, false, false,
                          PointerType::create(
                              false, false, false,
                              FunctionType::create(
                                  PointerType::create(
                                      false, false, false,
                                      AbstractArrayType::create(
                                          false, false, false,
                                          PointerType::create(false, false, false,
                                                              PrimitiveType::createChar(
                                                                  false, false, cld::LanguageOptions::native())))),
                                  {}, false, true))),
                      8)))
              == "char *(*(**[][8])())[]");
    }
}

using namespace cld::Semantics;

namespace
{
/***
 * Assume last thing in the translation unit is a function definition and assumes that the last expression
 * inside the function definitions compound statement is a non empty expression statement.
 * Returns the expression of that expression statement
 * @param source Source code to process
 * @param options Language options
 * @return Last expression in the function definition
 */
const Expression& generateExpression(std::string source, cld::LanguageOptions options = cld::LanguageOptions::native())
{
    static TranslationUnit translationUnit({});
    std::string errors;
    std::tie(translationUnit, errors) = generateSemantics(std::move(source), options);
    REQUIRE_THAT(errors, ProducesNoErrors());
    REQUIRE(std::holds_alternative<std::unique_ptr<FunctionDefinition>>(translationUnit.getGlobals().back()));
    auto& funcDef = *cld::get<std::unique_ptr<FunctionDefinition>>(translationUnit.getGlobals().back());
    REQUIRE(
        std::holds_alternative<std::unique_ptr<Statement>>(funcDef.getCompoundStatement().getCompoundItems().back()));
    auto& statement = *cld::get<std::unique_ptr<Statement>>(funcDef.getCompoundStatement().getCompoundItems().back());
    REQUIRE(statement.is<ExpressionStatement>());
    auto& expr = static_cast<ExpressionStatement&>(statement).getExpression();
    REQUIRE(expr);
    return *expr;
}
} // namespace

TEST_CASE("Semantics primary expressions", "[semantics]")
{
    SECTION("Constant")
    {
        SECTION("Normal")
        {
            auto options = cld::LanguageOptions::native();
            auto constant = GENERATE_COPY(values<std::pair<std::string, Type>>({
                {"5", PrimitiveType::createInt(false, false, options)},
                {"5u", PrimitiveType::createUnsignedInt(false, false, options)},
                {"5l", PrimitiveType::createLong(false, false, options)},
                {"5ul", PrimitiveType::createUnsignedLong(false, false, options)},
                {"5LL", PrimitiveType::createLongLong(false, false)},
                {"5uLL", PrimitiveType::createUnsignedLongLong(false, false)},
                {"5.0f", PrimitiveType::createFloat(false, false)},
                {"5.0", PrimitiveType::createDouble(false, false)},
                {"5.0L", PrimitiveType::createLongDouble(false, false, options)},
                {"\"txt\"",
                 ArrayType::create(false, false, false, false, PrimitiveType::createChar(false, false, options), 4)},
            }));
            auto& expr = generateExpression("void foo(void) { " + constant.first + ";}");
            CHECK(expr.getType() == constant.second);
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            CHECK(std::holds_alternative<Constant>(expr.getVariant()));
        }
        SECTION("Wide")
        {
            SECTION("Windows")
            {
                auto options = cld::LanguageOptions::fromTriple(x64windowsMsvc);
                auto& expr = generateExpression("void foo(void) { L\"txt\";}", options);
                CHECK(expr.getType()
                      == ArrayType::create(false, false, false, false,
                                           PrimitiveType::createUnsignedShort(false, false, options), 4));
                CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
                CHECK(std::holds_alternative<Constant>(expr.getVariant()));
            }
            SECTION("Unix")
            {
                auto options = cld::LanguageOptions::fromTriple(x64linux);
                auto& expr = generateExpression("void foo(void) { L\"txt\";}", options);
                CHECK(expr.getType()
                      == ArrayType::create(false, false, false, false, PrimitiveType::createInt(false, false, options),
                                           4));
                CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
                CHECK(std::holds_alternative<Constant>(expr.getVariant()));
            }
        }
    }
    SECTION("Parentheses")
    {
        auto& expr = generateExpression("void foo(void) { (5);}");
        CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
        CHECK(std::holds_alternative<Constant>(expr.getVariant()));
    }
    SECTION("Identifiers")
    {
        SECTION("Declarations")
        {
            auto& expr = generateExpression("void foo(void) { foo;}");
            CHECK(expr.getType() == FunctionType::create(PrimitiveType::createVoid(false, false), {}, false, false));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<DeclarationRead>(expr.getVariant()));
            CHECK(std::holds_alternative<const FunctionDefinition*>(
                cld::get<DeclarationRead>(expr.getVariant()).getDeclRead()));
        }
        SECTION("Enum constants")
        {
            auto& expr = generateExpression("enum A { VALUE = 7};void foo(void) { VALUE;}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(std::holds_alternative<Constant>(expr.getVariant()));
        }
        SEMA_PRODUCES("void foo(void) { bar; }", ProducesError(UNDECLARED_IDENTIFIER_N, "'bar'"));
    }
}

TEST_CASE("Semantics postfix expressions", "[semantics]")
{
    SECTION("Subscript")
    {
        auto& expr = generateExpression("void foo(int *i) {\n"
                                        "    i[5];\n"
                                        "}");

        CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
        REQUIRE(std::holds_alternative<SubscriptOperator>(expr.getVariant()));
        CHECK(cld::get<SubscriptOperator>(expr.getVariant()).getLeftExpression().getType()
              == PointerType::create(false, false, false,
                                     PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
        CHECK(cld::get<SubscriptOperator>(expr.getVariant()).getLeftExpression().getValueCategory()
              == ValueCategory::Rvalue);
        CHECK(cld::get<SubscriptOperator>(expr.getVariant()).getRightExpression().getType()
              == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(cld::get<SubscriptOperator>(expr.getVariant()).getRightExpression().getValueCategory()
              == ValueCategory::Rvalue);
        SEMA_PRODUCES("int foo(void) {\n"
                      " 5[5];\n"
                      "}",
                      ProducesError(EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE));
        SEMA_PRODUCES("int foo(void) {\n"
                      " int *i;\n"
                      " i[5.0];\n"
                      "}",
                      ProducesError(EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE));
        SEMA_PRODUCES("int foo(void) {\n"
                      " int *i;\n"
                      " 5.0[i];\n"
                      "}",
                      ProducesError(EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r *i;\n"
                      " i[5];\n"
                      "}",
                      ProducesError(POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR, "'struct r'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r *i;\n"
                      " 5[i];\n"
                      "}",
                      ProducesError(POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR, "'struct r'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " void (*i)(int);\n"
                      " i[5];\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR));
        SEMA_PRODUCES("int foo(void) {\n"
                      " void (*i)(int);\n"
                      " 5[i];\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR));
    }
    SECTION("Dot member access")
    {
        SECTION("struct")
        {
            auto& expr = generateExpression("struct A {\n"
                                            " int i;\n"
                                            "};\n"
                                            "\n"
                                            "int foo(struct A i) {\n"
                                            " i.i;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getValueCategory()
                  == ValueCategory::Lvalue);
            CHECK(std::holds_alternative<StructType>(
                cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getType().getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getMemberIndex() == 0);
        }
        SECTION("union")
        {
            auto& expr = generateExpression("union A {\n"
                                            " int i;\n"
                                            "};\n"
                                            "\n"
                                            "int foo(union A i) {\n"
                                            " i.i;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getValueCategory()
                  == ValueCategory::Lvalue);
            CHECK(std::holds_alternative<UnionType>(
                cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getType().getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getMemberIndex() == 0);
        }
        SECTION("anonymous struct")
        {
            auto& expr = generateExpression("int foo(struct { int i; } i) {\n"
                                            " i.i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getValueCategory()
                  == ValueCategory::Lvalue);
            CHECK(std::holds_alternative<AnonymousStructType>(
                cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getType().getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getMemberIndex() == 0);
        }
        SECTION("anonymous union")
        {
            auto& expr = generateExpression("int foo(union { int i; } i) {\n"
                                            " i.i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getValueCategory()
                  == ValueCategory::Lvalue);
            CHECK(std::holds_alternative<AnonymousUnionType>(
                cld::get<MemberAccess>(expr.getVariant()).getRecordExpression().getType().getVariant()));
            CHECK(cld::get<MemberAccess>(expr.getVariant()).getMemberIndex() == 0);
        }
        SEMA_PRODUCES("int foo(void) {\n"
                      " '5'.m;\n"
                      "}",
                      ProducesError(EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_DOT_OPERATOR));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r* i;\n"
                      " (*i).m;\n"
                      "}",
                      ProducesError(STRUCT_N_IS_AN_INCOMPLETE_TYPE, "r"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " union r* i;\n"
                      " (*i).m;\n"
                      "}",
                      ProducesError(UNION_N_IS_AN_INCOMPLETE_TYPE, "r"));
        SEMA_PRODUCES("struct A {\n"
                      " int i;\n"
                      "};\n"
                      "\n"
                      "int foo(void) {\n"
                      " struct A i;\n"
                      " i.m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N, "'m'", "A"));
        SEMA_PRODUCES("union A {\n"
                      " int i;\n"
                      "};\n"
                      "\n"
                      "int foo(void) {\n"
                      " union A i;\n"
                      " i.m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_UNION_N, "'m'", "A"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct { int i; } i;\n"
                      " i.m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT, "'m'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " union { int i; } i;\n"
                      " i.m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION, "'m'"));
    }
    SECTION("Arrow member access")
    {
        SECTION("struct")
        {
            auto& expr = generateExpression("struct A {\n"
                                            " int i;\n"
                                            "};\n"
                                            "\n"
                                            "int foo(struct A* i) {\n"
                                            " i->i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            auto& mem = cld::get<MemberAccess>(expr.getVariant());
            CHECK(mem.getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<PointerType>(mem.getRecordExpression().getType().getVariant()));
            CHECK(std::holds_alternative<StructType>(
                cld::get<PointerType>(mem.getRecordExpression().getType().getVariant()).getElementType().getVariant()));
            CHECK(mem.getMemberIndex() == 0);
        }
        SECTION("union")
        {
            auto& expr = generateExpression("union A {\n"
                                            " int i;\n"
                                            "};\n"
                                            "\n"
                                            "int foo(union A* i) {\n"
                                            " i->i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            auto& mem = cld::get<MemberAccess>(expr.getVariant());
            CHECK(mem.getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<PointerType>(mem.getRecordExpression().getType().getVariant()));
            CHECK(std::holds_alternative<UnionType>(
                cld::get<PointerType>(mem.getRecordExpression().getType().getVariant()).getElementType().getVariant()));
            CHECK(mem.getMemberIndex() == 0);
        }
        SECTION("anonymous struct")
        {
            auto& expr = generateExpression("int foo( struct { int i; }* i) {\n"
                                            " i->i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            auto& mem = cld::get<MemberAccess>(expr.getVariant());
            CHECK(mem.getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<PointerType>(mem.getRecordExpression().getType().getVariant()));
            CHECK(std::holds_alternative<AnonymousStructType>(
                cld::get<PointerType>(mem.getRecordExpression().getType().getVariant()).getElementType().getVariant()));
            CHECK(mem.getMemberIndex() == 0);
        }
        SECTION("anonymous union")
        {
            auto& expr = generateExpression("int foo(union { int i; } *i) {\n"
                                            " i->i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<MemberAccess>(expr.getVariant()));
            auto& mem = cld::get<MemberAccess>(expr.getVariant());
            CHECK(mem.getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(std::holds_alternative<PointerType>(mem.getRecordExpression().getType().getVariant()));
            CHECK(std::holds_alternative<AnonymousUnionType>(
                cld::get<PointerType>(mem.getRecordExpression().getType().getVariant()).getElementType().getVariant()));
            CHECK(mem.getMemberIndex() == 0);
        }
        SEMA_PRODUCES("int foo(void) {\n"
                      " 5->m;\n"
                      "}",
                      ProducesError(EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r* i;\n"
                      " i->m;\n"
                      "}",
                      ProducesError(STRUCT_N_IS_AN_INCOMPLETE_TYPE, "r"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " union r* i;\n"
                      " i->m;\n"
                      "}",
                      ProducesError(UNION_N_IS_AN_INCOMPLETE_TYPE, "r"));
        SEMA_PRODUCES("struct A {\n"
                      " int i;\n"
                      "};\n"
                      "\n"
                      "int foo(void) {\n"
                      " struct A* i;\n"
                      " i->m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N, "'m'", "A"));
        SEMA_PRODUCES("union A {\n"
                      " int i;\n"
                      "};\n"
                      "\n"
                      "int foo(void) {\n"
                      " union A* i;\n"
                      " i->m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_UNION_N, "'m'", "A"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct { int i; } *i;\n"
                      " i->m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT, "'m'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " union { int i; } *i;\n"
                      " i->m;\n"
                      "}",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION, "'m'"));
    }
    SECTION("Increment and decrement")
    {
        SECTION("Increment")
        {
            auto& expr = generateExpression("int foo(volatile int i) {\n"
                                            " i++;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(std::holds_alternative<UnaryOperator>(expr.getVariant()));
            auto& unary = cld::get<UnaryOperator>(expr.getVariant());
            CHECK(unary.getKind() == UnaryOperator::PostIncrement);
        }
        SECTION("Decrement")
        {
            auto& expr = generateExpression("int foo(volatile int i) {\n"
                                            " i--;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(std::holds_alternative<UnaryOperator>(expr.getVariant()));
            auto& unary = cld::get<UnaryOperator>(expr.getVariant());
            CHECK(unary.getKind() == UnaryOperator::PostDecrement);
        }
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct { int i; } i;\n"
                      " i++;\n"
                      "}",
                      ProducesError(OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'++'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct { int i; } i;\n"
                      " i--;\n"
                      "}",
                      ProducesError(OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'--'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " int i;\n"
                      " (i++)++;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'++'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " int i;\n"
                      " (i--)--;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'--'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " const int i;\n"
                      " i++;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'++'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " const int i;\n"
                      " i--;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'--'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r* i;\n"
                      " i++;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r* i;\n"
                      " i--;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " void (*i)(int);\n"
                      " i++;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("int foo(void) {\n"
                      " void (*i)(int);\n"
                      " i--;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
    }
}

TEST_CASE("Semantics unary expressions", "[semantics]")
{
    SECTION("Increment and decrement")
    {
        SECTION("Increment")
        {
            auto& expr = generateExpression("int foo(volatile int i) {\n"
                                            " ++i;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(std::holds_alternative<UnaryOperator>(expr.getVariant()));
            auto& unary = cld::get<UnaryOperator>(expr.getVariant());
            CHECK(unary.getKind() == UnaryOperator::PreIncrement);
        }
        SECTION("Decrement")
        {
            auto& expr = generateExpression("int foo(volatile int i) {\n"
                                            " --i;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(std::holds_alternative<UnaryOperator>(expr.getVariant()));
            auto& unary = cld::get<UnaryOperator>(expr.getVariant());
            CHECK(unary.getKind() == UnaryOperator::PreDecrement);
        }
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct { int i; } i;\n"
                      " ++i;\n"
                      "}",
                      ProducesError(OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'++'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct { int i; } i;\n"
                      " --i;\n"
                      "}",
                      ProducesError(OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'--'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " int i;\n"
                      " ++(++i);\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'++'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " int i;\n"
                      " --(--i);\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'--'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " const int i;\n"
                      " ++i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'++'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " const int i;\n"
                      " --i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'--'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r* i;\n"
                      " ++i;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " struct r* i;\n"
                      " --i;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'"));
        SEMA_PRODUCES("int foo(void) {\n"
                      " void (*i)(int);\n"
                      " ++i;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("int foo(void) {\n"
                      " void (*i)(int);\n"
                      " --i;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
    }
    SECTION("Address of")
    {
        SECTION("Following dereference")
        {
            auto& exp = generateExpression("void foo(int *i) {\n"
                                           " &*i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType()
                  == PointerType::create(false, false, false,
                                         PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
        }
        SECTION("Following subscript")
        {
            auto& exp = generateExpression("void foo(int *i) {\n"
                                           " &i[0];\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType()
                  == PointerType::create(false, false, false,
                                         PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
        }
        SEMA_PRODUCES("void foo(register int i) {\n"
                      " &i;\n"
                      "}",
                      ProducesError(CANNOT_TAKE_ADDRESS_OF_DECLARATION_ANNOTATED_WITH_REGISTER));
        SEMA_PRODUCES("void foo(struct { int i : 5; } i) {\n"
                      " &i.i;\n"
                      "}",
                      ProducesError(CANNOT_TAKE_ADDRESS_OF_BITFIELD));
        SEMA_PRODUCES("void foo(void) {\n"
                      " &5;\n"
                      "}",
                      ProducesError(CANNOT_TAKE_ADDRESS_OF_TEMPORARY));
    }
    SECTION("Dereference")
    {
        auto& exp = generateExpression("void foo(int* i) {\n"
                                       "*i;\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Lvalue);
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        REQUIRE(std::holds_alternative<UnaryOperator>(exp.getVariant()));
        CHECK(cld::get<UnaryOperator>(exp.getVariant()).getKind() == UnaryOperator::Dereference);
        SEMA_PRODUCES("void foo(int i) {\n"
                      " *i;\n"
                      "}",
                      ProducesError(CANNOT_DEREFERENCE_NON_POINTER_TYPE_N, "", "'int'"));
    }
    SECTION("Unary + and -")
    {
        SECTION("-")
        {
            auto& exp = generateExpression("void foo(short i) {\n"
                                           "-i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            REQUIRE(std::holds_alternative<UnaryOperator>(exp.getVariant()));
            CHECK(cld::get<UnaryOperator>(exp.getVariant()).getKind() == UnaryOperator::Minus);
        }
        SECTION("+")
        {
            auto& exp = generateExpression("void foo(short i) {\n"
                                           "+i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            REQUIRE(std::holds_alternative<UnaryOperator>(exp.getVariant()));
            CHECK(cld::get<UnaryOperator>(exp.getVariant()).getKind() == UnaryOperator::Plus);
        }
        SEMA_PRODUCES("void foo(float i) {\n"
                      "+i;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "+i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'+'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "-i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'-'"));
    }
    SECTION("Bitwise negate")
    {
        auto& exp = generateExpression("void foo(short i) {\n"
                                       "~i;\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        REQUIRE(std::holds_alternative<UnaryOperator>(exp.getVariant()));
        CHECK(cld::get<UnaryOperator>(exp.getVariant()).getKind() == UnaryOperator::BitwiseNegate);
        SEMA_PRODUCES("void foo(float i) {\n"
                      "~i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'~'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "~i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'~'"));
    }
    SECTION("Logical negate")
    {
        SECTION("Integer")
        {
            auto& exp = generateExpression("void foo(short i) {\n"
                                           "!i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            REQUIRE(std::holds_alternative<UnaryOperator>(exp.getVariant()));
            CHECK(cld::get<UnaryOperator>(exp.getVariant()).getKind() == UnaryOperator::BooleanNegate);
        }
        SECTION("Pointer")
        {
            auto& exp = generateExpression("void foo(int* i) {\n"
                                           "!i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            REQUIRE(std::holds_alternative<UnaryOperator>(exp.getVariant()));
            CHECK(cld::get<UnaryOperator>(exp.getVariant()).getKind() == UnaryOperator::BooleanNegate);
        }
        SEMA_PRODUCES("void foo(float i) {\n"
                      "!i;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(struct { int r; } i) {\n"
                      "!i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'!'"));
    }
    SECTION("sizeof")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "sizeof(int);\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        CHECK(exp.getType() == PrimitiveType::createSizeT(false, false, cld::LanguageOptions::native()));
        REQUIRE(std::holds_alternative<SizeofOperator>(exp.getVariant()));
        SEMA_PRODUCES("void foo(struct r* i) {\n"
                      " sizeof *i;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_IN_SIZE_OF, "'struct r'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      " sizeof(struct r);\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_IN_SIZE_OF, "'struct r'"));
        SEMA_PRODUCES("void foo(int (*i)(void)) {\n"
                      " sizeof *i;\n"
                      "}",
                      ProducesError(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF));
        SEMA_PRODUCES("void foo(void) {\n"
                      " sizeof(void(void));\n"
                      "}",
                      ProducesError(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF));
        SEMA_PRODUCES("void foo(struct { int i : 5; } i) {\n"
                      " sizeof i.i;\n"
                      "}",
                      ProducesError(BITFIELD_NOT_ALLOWED_IN_SIZE_OF));
    }
}

TEST_CASE("Semantics cast expression", "[semantics]")
{
    auto& exp = generateExpression("void foo(int* const i) {\n"
                                   "(const int* const)i;\n"
                                   "}");
    CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
    CHECK(exp.getType()
          == PointerType::create(false, false, false,
                                 PrimitiveType::createInt(true, false, cld::LanguageOptions::native())));
    CHECK(std::holds_alternative<Cast>(exp.getVariant()));
    SEMA_PRODUCES("void foo(int* i) {\n"
                  " (struct r)i;\n"
                  "}",
                  ProducesError(TYPE_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
    SEMA_PRODUCES("void foo(struct r* i) {\n"
                  " (int)*i;\n"
                  "}",
                  ProducesError(EXPRESSION_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
}

TEST_CASE("Semantics term expression", "[semantics]")
{
    SECTION("Multiply")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "5 * 5;\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
        CHECK(binOp.getKind() == BinaryOperator::Multiply);
        SEMA_PRODUCES("void foo(void) {\n"
                      "5.0 * 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "5.0 * 5.f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "5uLL * 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *i) {\n"
                      " i * 5;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'*'"));
        SEMA_PRODUCES("void foo(int *i) {\n"
                      " 5 * i;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'*'"));
    }
    SECTION("Divide")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "5 / 5;\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
        CHECK(binOp.getKind() == BinaryOperator::Divide);
        SEMA_PRODUCES("void foo(void) {\n"
                      "5.0 / 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "5.0 / 5.f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "5uLL / 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *i) {\n"
                      " i / 5;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'/'"));
        SEMA_PRODUCES("void foo(int *i) {\n"
                      " 5 / i;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'/'"));
    }
    SECTION("Modulo")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "5 % 5;\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
        CHECK(binOp.getKind() == BinaryOperator::Modulo);
        SEMA_PRODUCES("void foo(void) {\n"
                      "5uLL % 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "5.0 % 5;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'%'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "5.0 % 5.f;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'%'")
                          && ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'%'"));
        SEMA_PRODUCES("void foo(int *i) {\n"
                      " i % 5;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'%'"));
        SEMA_PRODUCES("void foo(int *i) {\n"
                      " 5 % i;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'%'"));
    }
}

TEST_CASE("Semantics additive expression", "[semantics]")
{
    SECTION("Plus")
    {
        SECTION("Arithmetic")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "5 + 5;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
            auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
            CHECK(binOp.getKind() == BinaryOperator::Addition);
        }
        SECTION("Pointer")
        {
            SECTION("Pointer left")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "i + 5;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType()
                      == PointerType::create(false, false, false,
                                             PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
                REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
                auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
                CHECK(binOp.getKind() == BinaryOperator::Addition);
            }
            SECTION("Pointer right")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "5 + i;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType()
                      == PointerType::create(false, false, false,
                                             PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
                REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
                auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
                CHECK(binOp.getKind() == BinaryOperator::Addition);
            }
        }
        SEMA_PRODUCES("void foo(struct { int i; } i) {\n"
                      " i + 5;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'+'"));
        SEMA_PRODUCES("void foo(struct { int i; } i) {\n"
                      " 5 + i;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'+'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      " 5.f + i;\n"
                      "}",
                      ProducesError(EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE, "'+'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      " i + 5.f;\n"
                      "}",
                      ProducesError(EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE, "'+'"));
        SEMA_PRODUCES("void foo(struct r* i) {\n"
                      " i + 5;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'"));
        SEMA_PRODUCES("void foo(void (*i)(void)) {\n"
                      " i + 5;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
    }
    SECTION("Minus")
    {
        SECTION("Arithmetic")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "5 - 5;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
            auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
            CHECK(binOp.getKind() == BinaryOperator::Subtraction);
        }
        SECTION("Pointer")
        {
            SECTION("Pointer and int")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "i - 5;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType()
                      == PointerType::create(false, false, false,
                                             PrimitiveType::createInt(false, false, cld::LanguageOptions::native())));
                REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
                auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
                CHECK(binOp.getKind() == BinaryOperator::Subtraction);
            }
            SECTION("Pointer and Pointer")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "i - i;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType() == PrimitiveType::createPtrdiffT(false, false, cld::LanguageOptions::native()));
                REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
                auto& binOp = cld::get<BinaryOperator>(exp.getVariant());
                CHECK(binOp.getKind() == BinaryOperator::Subtraction);
            }
        }
        SEMA_PRODUCES("void foo(struct { int i; } i) {\n"
                      " i - 5;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'-'"));
        SEMA_PRODUCES("void foo(struct { int i; } i) {\n"
                      " 5 - i;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'-'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      " 5 - i;\n"
                      "}",
                      ProducesError(CANNOT_SUBTRACT_POINTER_FROM_ARITHMETIC_TYPE));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      " i - 5.f;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE, "'-'"));
        SEMA_PRODUCES("void foo(struct r* i) {\n"
                      " i - 5;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'"));
        SEMA_PRODUCES("void foo(void (*i)(void)) {\n"
                      " i - 5;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int* f,struct r* i) {\n"
                      " f - i;\n"
                      "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct r'")
                          && ProducesError(CANNOT_SUBTRACT_POINTERS_OF_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int* f,void (*i)(void)) {\n"
                      " f - i;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC)
                          && ProducesError(CANNOT_SUBTRACT_POINTERS_OF_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int* f,const int* i) {\n"
                      "i - f;\n"
                      "}",
                      ProducesNoErrors());
    }
}

TEST_CASE("Semantics shift expression", "[semantics]")
{
    SECTION("Left")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5uLL << 1;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createUnsignedLongLong(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::LeftShift);
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5.0 << 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'<<'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5 << 1.0;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'<<'"));
    }
    SECTION("Right")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5uLL >> 1;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createUnsignedLongLong(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::RightShift);
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5.0 >> 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'>>'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5 >> 1.0;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'>>'"));
    }
}

TEST_CASE("Semantics relational expression", "[semantics]")
{
    SECTION("<")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 < 3;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::LessThan);
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " r < 0;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'<'"));
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " 0 < r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'<'"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " 0 < r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'<'"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r < 0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'<'"));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " r < (int*)5;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " (int*)5 < r;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int *r,float *f) {\n"
                      " f < r;\n"
                      "}",
                      ProducesError(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES));
    }
    SECTION(">")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 > 3;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::GreaterThan);
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " r > 0;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'>'"));
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " 0 > r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'>'"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " 0 > r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'>'"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r > 0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'>'"));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " r > (int*)5;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " (int*)5 > r;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int *r,float *f) {\n"
                      " f > r;\n"
                      "}",
                      ProducesError(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES));
    }
    SECTION("<=")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 <= 3;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::LessOrEqual);
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " r <= 0;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'<='"));
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " 0 <= r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'<='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " 0 <= r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'<='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r <= 0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'<='"));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " r <= (int*)5;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " (int*)5 <= r;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int *r,float *f) {\n"
                      " f <= r;\n"
                      "}",
                      ProducesError(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES));
    }
    SECTION(">=")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 >= 3;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::GreaterOrEqual);
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " r >= 0;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'>='"));
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " 0 >= r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'>='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " 0 >= r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'>='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r >= 0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'>='"));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " r >= (int*)5;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int (*r)(void)) {\n"
                      " (int*)5 >= r;\n"
                      "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(int *r,float *f) {\n"
                      " f >= r;\n"
                      "}",
                      ProducesError(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES));
    }
}

TEST_CASE("Semantics equal expressions", "[semantics]")
{
    SECTION("==")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 == 3;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::Equal);
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " r == 0;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'=='"));
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " 0 == r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'=='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " 5 == r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'=='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r == 5.0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'=='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r == 5;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL, "'=='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r == 0;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *r,float *f) {\n"
                      " f == r;\n"
                      "}",
                      ProducesError(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int (*r)(void),int (*f)(void)) {\n"
                      " f == r;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *r,void *f) {\n"
                      " f == r;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *r,void *f) {\n"
                      " r == f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void *r,void *f) {\n"
                      " f == r;\n"
                      "}",
                      ProducesNoErrors());
    }
    SECTION("!=")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 != 3;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::NotEqual);
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " r != 0;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'!='"));
        SEMA_PRODUCES("void foo(struct { int r; } r) {\n"
                      " 0 != r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'!='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " 5 != r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'!='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r != 5.0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'!='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r != 5;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL, "'!='"));
        SEMA_PRODUCES("void foo(int* r) {\n"
                      " r != 0;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *r,float *f) {\n"
                      " f != r;\n"
                      "}",
                      ProducesError(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int (*r)(void),int (*f)(void)) {\n"
                      " f != r;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *r,void *f) {\n"
                      " f != r;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int *r,void *f) {\n"
                      " r != f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void *r,void *f) {\n"
                      " f != r;\n"
                      "}",
                      ProducesNoErrors());
    }
}

TEST_CASE("Semantics bit operators", "[semantics]")
{
    SECTION("&")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 & 1uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createUnsignedLongLong(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::BitAnd);
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5.0 & 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'&'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5 & 1.0;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'&'"));
    }
    SECTION("|")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 | 1uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createUnsignedLongLong(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::BitOr);
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5.0 | 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'|'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5 | 1.0;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'|'"));
    }
    SECTION("^")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 ^ 1uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createUnsignedLongLong(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::BitXor);
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5.0 ^ 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'^'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      " 5 ^ 1.0;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'^'"));
    }
}

TEST_CASE("Semantics logic operators", "[semantics]")
{
    SECTION("&&")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 && 1uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::LogicAnd);
        SEMA_PRODUCES("void foo(struct { int i; } r) {\n"
                      " r && 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'&&'"));
        SEMA_PRODUCES("void foo(struct { int i; } r) {\n"
                      " 5 && r;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'&&'"));
        SEMA_PRODUCES("void foo(int f[5]) {\n"
                      " f && 0;\n"
                      "}",
                      ProducesNoErrors());
    }
    SECTION("||")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       " 5 || 1uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<BinaryOperator>(exp.getVariant()));
        CHECK(cld::get<BinaryOperator>(exp.getVariant()).getKind() == BinaryOperator::LogicOr);
        SEMA_PRODUCES("void foo(struct { int i; } r) {\n"
                      " r || 1;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'||'"));
        SEMA_PRODUCES("void foo(struct { int i; } r) {\n"
                      " 5 || r;\n"
                      "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'||'"));
        SEMA_PRODUCES("void foo(int f[5]) {\n"
                      " f || 0;\n"
                      "}",
                      ProducesNoErrors());
    }
}

TEST_CASE("Semantics conditional expression", "[semantics]")
{
    SECTION("Arithmetic")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "1 ? 5.0 : 3uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createDouble(false, false));
    }
    SECTION("Void")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "1 ? *(void const*)5 : *(void*)3uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createVoid(false, false));
    }
    SECTION("Pointer")
    {
        SECTION("Void")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "1 ? (void const*)5 : (float*)3uLL;\n"
                                           "}");
            CHECK(exp.getType() == PointerType::create(false, false, false, PrimitiveType::createVoid(true, false)));
        }
        SECTION("Merging void")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "1 ? (void const*)5 : (void volatile*)3uLL;\n"
                                           "}");
            CHECK(exp.getType() == PointerType::create(false, false, false, PrimitiveType::createVoid(true, true)));
        }
        SECTION("Composite type")
        {
            auto& exp = generateExpression("void foo(void (*f)(float[]),void (*r)(float[5])) {\n"
                                           "1 ? f : r;\n"
                                           "}");
            CHECK(exp.getType()
                  == PointerType::create(
                      false, false, false,
                      FunctionType::create(
                          PrimitiveType::createVoid(false, false),
                          {{ArrayType::create(false, false, false, false, PrimitiveType::createFloat(false, false), 5),
                            ""}},
                          false, false)));
        }
    }
    SEMA_PRODUCES("void foo(struct { int i; } r) {\n"
                  " r ? 5 : 3;\n"
                  "}",
                  ProducesError(FIRST_OPERAND_OF_CONDITIONAL_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
    SEMA_PRODUCES("void foo(_Bool r) {\n"
                  " r ? 5 : (int*)3;\n"
                  "}",
                  ProducesError(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE));
    SEMA_PRODUCES("void foo(_Bool r) {\n"
                  " r ? *(void*)5 : 3;\n"
                  "}",
                  ProducesError(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_VOID));
    SEMA_PRODUCES("void foo(_Bool r) {\n"
                  " r ? (int*)5 : 3.0;\n"
                  "}",
                  ProducesError(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_A_POINTER_TYPE));
    SEMA_PRODUCES("void foo(_Bool r) {\n"
                  " r ? (int*)5 : 3;\n"
                  "}",
                  ProducesError(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL));
    SEMA_PRODUCES("void foo(_Bool r) {\n"
                  " r ? (int*)5 : 0;\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(_Bool r) {\n"
                  " r ? (int*)5 : (float*)3;\n"
                  "}",
                  ProducesError(POINTER_TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES));
    SEMA_PRODUCES("void foo(_Bool r,struct R* i,struct F* f) {\n"
                  " r ? *i : *f;\n"
                  "}",
                  ProducesError(TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES));
}

TEST_CASE("Semantics assignment expression", "[semantics]")
{
    SECTION("Simple")
    {
        auto& exp = generateExpression("void foo(volatile int i) {\n"
                                       "i = 5;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<Assignment>(exp.getVariant()));
        CHECK(cld::get<Assignment>(exp.getVariant()).getKind() == Assignment::Simple);
        SEMA_PRODUCES("void foo(void) {\n"
                      "5 = 3;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'='"));
        SEMA_PRODUCES("void foo(const int i) {\n"
                      "i = 3;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'='"));
        SEMA_PRODUCES("struct R {\n"
                      "const int i;\n"
                      "};\n"
                      "\n"
                      "void foo(struct R i,struct R f) {\n"
                      "i = f;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'='"));
        SEMA_PRODUCES("void foo(int i) {\n"
                      "i = (int*)3;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'='"));
        SEMA_PRODUCES("void foo(_Bool i) {\n"
                      "i = (int*)3;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("struct R {\n"
                      "int i;\n"
                      "};\n"
                      "void foo(_Bool i) {\n"
                      "struct R r;\n"
                      "i = r;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "'='"));
        SEMA_PRODUCES("struct R;\n"
                      "void foo(struct R* i,struct R* f) {\n"
                      "*i = *f;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_TO_INCOMPLETE_TYPE_N, "'struct R'"));
        SEMA_PRODUCES("void foo(int i) {\n"
                      "int r[5];\n"
                      "r = i;"
                      "}",
                      ProducesError(CANNOT_ASSIGN_TO_ARRAY_TYPE_N, "'int[5]'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "struct { int i; } r,f;\n"
                      "r = f;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "struct { int i; } r;\n"
                      "struct { int i; } f;\n"
                      "r = f;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = 3.0;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE, "'='"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = 3;\n"
                      "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL, "'='"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = (void*)3;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void* i) {\n"
                      "i = (int*)3;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = (const void*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = (volatile void*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(volatile int* i) {\n"
                      "i = (const void*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(const int* i) {\n"
                      "i = (volatile void*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(void* i) {\n"
                      "i = foo;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_FUNCTION_POINTER_TO_VOID_POINTER));
        SEMA_PRODUCES("void foo(void (*i)(void)) {\n"
                      "i = (void*)5;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_VOID_POINTER_TO_FUNCTION_POINTER));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = (const int*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i = (volatile int*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(volatile int* i) {\n"
                      "i = (const int*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(const int* i) {\n"
                      "i = (volatile int*)3;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
    }
    SECTION("Plus and Minus")
    {
        auto operand = GENERATE(as<std::string>(), "+=", "-=");
        auto& exp = generateExpression("void foo(volatile int i) {\n"
                                       "i "
                                       + operand
                                       + " 5;\n"
                                         "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<Assignment>(exp.getVariant()));
        CHECK(cld::get<Assignment>(exp.getVariant()).getKind()
              == (operand == "-=" ? Assignment::Minus : Assignment::Plus));
        SEMA_PRODUCES("void foo(void) {\n"
                      "5 " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(const int i) {\n"
                      "i " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES("struct R {\n"
                      "const int i;\n"
                      "};\n"
                      "\n"
                      "void foo(struct R i,struct R f) {\n"
                      "i " + operand
                          + " f;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES(
            "void foo(int i) {\n"
            "i " + operand
                + " (int*)3;\n"
                  "}",
            ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i " + operand
                          + " 3;\n"
                            "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i " + operand
                          + " 3.0;\n"
                            "}",
                      ProducesError(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int (*i)(void)) {\n"
                      "i " + operand
                          + " 1;\n"
                            "}",
                      ProducesError(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC));
        SEMA_PRODUCES("void foo(struct R *i) {\n"
                      "i " + operand
                          + " 1;\n"
                            "}",
                      ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct R'"));
    }
    SECTION("Divide and Multiply")
    {
        auto operand = GENERATE(as<std::string>(), "*=", "/=");
        auto& exp = generateExpression("void foo(volatile int i) {\n"
                                       "i "
                                       + operand
                                       + " 5;\n"
                                         "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<Assignment>(exp.getVariant()));
        CHECK(cld::get<Assignment>(exp.getVariant()).getKind()
              == (operand == "*=" ? Assignment::Multiply : Assignment::Divide));
        SEMA_PRODUCES("void foo(void) {\n"
                      "5 " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(const int i) {\n"
                      "i " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES("struct R {\n"
                      "const int i;\n"
                      "};\n"
                      "\n"
                      "void foo(struct R i,struct R f) {\n"
                      "i " + operand
                          + " f;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'")
                          && ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'" + operand + "'")
                          && ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int i) {\n"
                      "i " + operand
                          + " (int*)3;\n"
                            "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i " + operand
                          + " 3.0;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "'" + operand + "'"));
    }
    SECTION("Modulo,Shift and Bit")
    {
        auto operand = GENERATE(as<std::string>(), "%=", "<<=", ">>=", "&=", "|=", "^=");
        auto& exp = generateExpression("void foo(volatile int i) {\n"
                                       "i "
                                       + operand
                                       + " 5;\n"
                                         "}");
        CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<Assignment>(exp.getVariant()));
        CHECK(cld::get<Assignment>(exp.getVariant()).getKind() == [&operand] {
            if (operand == "%=")
            {
                return Assignment::Modulo;
            }
            if (operand == "<<=")
            {
                return Assignment::LeftShift;
            }
            if (operand == ">>=")
            {
                return Assignment::RightShift;
            }
            if (operand == "&=")
            {
                return Assignment::BitAnd;
            }
            if (operand == "|=")
            {
                return Assignment::BitOr;
            }
            if (operand == "^=")
            {
                return Assignment::BitXor;
            }
            CLD_UNREACHABLE;
        }());
        SEMA_PRODUCES("void foo(void) {\n"
                      "5 " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(const int i) {\n"
                      "i " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'"));
        SEMA_PRODUCES("struct R {\n"
                      "const int i;\n"
                      "};\n"
                      "\n"
                      "void foo(struct R i,struct R f) {\n"
                      "i " + operand
                          + " f;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'" + operand + "'")
                          && ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'" + operand + "'")
                          && ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int i) {\n"
                      "i " + operand
                          + " (int*)3;\n"
                            "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int* i) {\n"
                      "i " + operand
                          + " 3;\n"
                            "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'" + operand + "'"));
        SEMA_PRODUCES("void foo(int i) {\n"
                      "i " + operand
                          + " 3.0;\n"
                            "}",
                      ProducesError(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'" + operand + "'"));
    }
}

TEST_CASE("Semantics comma expression", "[semantics]")
{
    auto& exp = generateExpression("void foo(void) {\n"
                                   "5.0,3;\n"
                                   "}");
    CHECK(exp.getType() == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
    CHECK(std::holds_alternative<CommaExpression>(exp.getVariant()));
    SEMA_PRODUCES("void foo(int i) {\n"
                  "(5,i) = 3;\n"
                  "}",
                  ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST, "'='"));
}

TEST_CASE("Semantics function call expression", "[semantics]")
{
    SECTION("K & R")
    {
        auto& exp = generateExpression("double bar();\n"
                                       "\n"
                                       "void foo(unsigned short i) {\n"
                                       "bar(5.0f,i);\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createDouble(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<CallExpression>(exp.getVariant()));
        auto& call = cld::get<CallExpression>(exp.getVariant());
        REQUIRE(call.getArgumentExpressions().size() == 2);
        REQUIRE(std::holds_alternative<Conversion>(call.getArgumentExpressions()[0].getVariant()));
        CHECK(cld::get<Conversion>(call.getArgumentExpressions()[0].getVariant()).getKind()
              == Conversion::DefaultArgumentPromotion);
        CHECK(call.getArgumentExpressions()[0].getType() == PrimitiveType::createDouble(false, false));
        REQUIRE(std::holds_alternative<Conversion>(call.getArgumentExpressions()[1].getVariant()));
        CHECK(cld::get<Conversion>(call.getArgumentExpressions()[1].getVariant()).getKind()
              == Conversion::IntegerPromotion);
        CHECK(call.getArgumentExpressions()[1].getType()
              == PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("Prototyped")
    {
        auto& exp = generateExpression("double bar(float,unsigned short);\n"
                                       "\n"
                                       "void foo(unsigned short i) {\n"
                                       "bar(5.0f,i);\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType::createDouble(false, false));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(std::holds_alternative<CallExpression>(exp.getVariant()));
        auto& call = cld::get<CallExpression>(exp.getVariant());
        REQUIRE(call.getArgumentExpressions().size() == 2);
        CHECK(call.getArgumentExpressions()[0].getType() == PrimitiveType::createFloat(false, false));
        CHECK(call.getArgumentExpressions()[1].getType()
              == PrimitiveType::createUnsignedShort(false, false, cld::LanguageOptions::native()));
    }
    SEMA_PRODUCES("void foo(int i) {\n"
                  " i();\n"
                  "}",
                  ProducesError(CANNOT_CALL_NON_FUNCTION_TYPE));
    SEMA_PRODUCES("void bar(int i);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar();\n"
                  "}",
                  ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N, "'bar'", 1, 0));
    SEMA_PRODUCES("void foo(void (*bar)(int)) {\n"
                  " bar();\n"
                  "}",
                  ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N, 1, 0));
    SEMA_PRODUCES("void bar(int i,...);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar();\n"
                  "}",
                  ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_AT_LEAST_N_GOT_N, "'bar'", 1, 0));
    SEMA_PRODUCES("void foo(void (*bar)(int,...)) {\n"
                  " bar();\n"
                  "}",
                  ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_AT_LEAST_N_GOT_N, 1, 0));
    SEMA_PRODUCES("void bar(void);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar(3,5,3);\n"
                  "}",
                  ProducesError(TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N, "'bar'", 0, 3));
    SEMA_PRODUCES("void foo(void (*bar)(void)) {\n"
                  " bar(3,5,3);\n"
                  "}",
                  ProducesError(TOO_MANY_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N, 0, 3));
    SEMA_PRODUCES("void bar(_Bool);\n"
                  "void foo(void) {\n"
                  " struct { int i; } r;\n"
                  " bar(r);\n"
                  "}",
                  ProducesError(EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE, 1));
    SEMA_PRODUCES("void bar(_Bool);\n"
                  "void foo(int* r) {\n"
                  " bar(r);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void bar(int);\n"
                  "void foo(int* r) {\n"
                  " bar(r);\n"
                  "}",
                  ProducesError(EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_TYPE, 1));
    SEMA_PRODUCES("struct R;\n"
                  "\n"
                  "void bar(struct R);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar(5);\n"
                  "}",
                  ProducesError(CANNOT_PASS_ARGUMENT_TO_INCOMPLETE_TYPE_N_OF_PARAMETER_N, "'struct R'", 1));
    SEMA_PRODUCES("void bar(int *);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar(5.0);\n"
                  "}",
                  ProducesError(EXPECTED_ARGUMENT_N_TO_BE_A_POINTER_TYPE, 1));
    SEMA_PRODUCES("void bar(int *);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar(5);\n"
                  "}",
                  ProducesError(EXPECTED_ARGUMENT_N_TO_BE_NULL, 1));
    SEMA_PRODUCES("void bar(int *);\n"
                  "\n"
                  "void foo(void) {\n"
                  " bar(0);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void bar(int*);\n"
                  "\n"
                  "void foo(float* f) {\n"
                  " bar(f);\n"
                  "}",
                  ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_N, 1, "'int *'"));
    SEMA_PRODUCES("void bar(int*);\n"
                  "\n"
                  "void foo(const int* f) {\n"
                  " bar(f);\n"
                  "}",
                  ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_N, 1, "'int *'"));
    SEMA_PRODUCES("void bar(void*);\n"
                  "\n"
                  "void foo(const float* f) {\n"
                  " bar(f);\n"
                  "}",
                  ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_N, 1, "'void *'"));
    SEMA_PRODUCES("void bar(void*);\n"
                  "\n"
                  "void foo(void (*f)(int)) {\n"
                  " bar(f);\n"
                  "}",
                  ProducesError(CANNOT_PASS_FUNCTION_POINTER_TO_VOID_POINTER_PARAMETER));
    SEMA_PRODUCES("void bar(void (*)(int));\n"
                  "\n"
                  "void foo(void *f) {\n"
                  " bar(f);\n"
                  "}",
                  ProducesError(CANNOT_PASS_VOID_POINTER_TO_FUNCTION_POINTER_PARAMETER));
    SEMA_PRODUCES("void bar();\n"
                  "\n"
                  "void foo(void) {\n"
                  "bar(5,3,43,4,3,4);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void bar(int,...);\n"
                  "\n"
                  "void foo(void) {\n"
                  "bar(5,3,43,4,3,4);\n"
                  "}",
                  ProducesNoErrors());
}

TEST_CASE("Semantics simple initializer", "[semantics]")
{
    SECTION("String literal")
    {
        SEMA_PRODUCES("char foo[6] = \"string\";", ProducesNoErrors());
        SEMA_PRODUCES("unsigned char foo[6] = \"string\";", ProducesNoErrors());
        SEMA_PRODUCES("signed char foo[6] = \"string\";", ProducesNoErrors());
        SEMA_PRODUCES("char foo[6] = L\"string\";",
                      ProducesError(CANNOT_INITIALIZE_CHAR_ARRAY_WITH_WIDE_STRING_LITERAL));
        CHECK_THAT(generateSemantics("unsigned short foo[6] = \"string\";", x64windowsGnu).second,
                   ProducesError(CANNOT_INITIALIZE_WCHART_ARRAY_WITH_STRING_LITERAL));
        CHECK_THAT(generateSemantics("int foo[6] = \"string\";", x64linux).second,
                   ProducesError(CANNOT_INITIALIZE_WCHART_ARRAY_WITH_STRING_LITERAL));
    }
    SECTION("Size deduction")
    {
        SECTION("Normal string")
        {
            auto [translationUnit, errors] = generateSemantics("char foo[] = \"string\";");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            auto& global = translationUnit.getGlobals()[0];
            REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
            auto& declaration = *cld::get<std::unique_ptr<Declaration>>(global);
            REQUIRE(std::holds_alternative<ArrayType>(declaration.getType().getVariant()));
            CHECK(cld::get<ArrayType>(declaration.getType().getVariant()).getSize() == 7);
        }
        SECTION("Wide string")
        {
            SECTION("Windows")
            {
                auto [translationUnit, errors] =
                    generateSemantics("unsigned short foo[] = L\"string\";", x64windowsGnu);
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                auto& global = translationUnit.getGlobals()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
                auto& declaration = *cld::get<std::unique_ptr<Declaration>>(global);
                REQUIRE(std::holds_alternative<ArrayType>(declaration.getType().getVariant()));
                CHECK(cld::get<ArrayType>(declaration.getType().getVariant()).getSize() == 7);
            }
            SECTION("Posix")
            {
                auto [translationUnit, errors] = generateSemantics("int foo[] = L\"string\";", x64linux);
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                auto& global = translationUnit.getGlobals()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
                auto& declaration = *cld::get<std::unique_ptr<Declaration>>(global);
                REQUIRE(std::holds_alternative<ArrayType>(declaration.getType().getVariant()));
                CHECK(cld::get<ArrayType>(declaration.getType().getVariant()).getSize() == 7);
            }
        }
    }
    SEMA_PRODUCES("float i[5] = 5.0;", ProducesError(ARRAY_MUST_BE_INITIALIZED_WITH_INITIALIZER_LIST));
    SEMA_PRODUCES("char i[5] = 5.0;", ProducesError(ARRAY_MUST_BE_INITIALIZED_WITH_STRING_OR_INITIALIZER_LIST));
    SEMA_PRODUCES("unsigned char i[5] = 5.0;",
                  ProducesError(ARRAY_MUST_BE_INITIALIZED_WITH_STRING_OR_INITIALIZER_LIST));
    SEMA_PRODUCES("signed char i[5] = 5.0;", ProducesError(ARRAY_MUST_BE_INITIALIZED_WITH_STRING_OR_INITIALIZER_LIST));
    CHECK_THAT(generateSemantics("int i[5] = 5.0;", x64linux).second,
               ProducesError(ARRAY_MUST_BE_INITIALIZED_WITH_WIDE_STRING_OR_INITIALIZER_LIST));
    CHECK_THAT(generateSemantics("unsigned short i[5] = 5.0;", x64windowsGnu).second,
               ProducesError(ARRAY_MUST_BE_INITIALIZED_WITH_WIDE_STRING_OR_INITIALIZER_LIST));
    SEMA_PRODUCES("extern int f;\n"
                  "int i = f;",
                  ProducesError(VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    SEMA_PRODUCES("extern int f;\n"
                  "int* i = &f;",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(void) {\n"
                  "extern int f;\n"
                  "int i = f;\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(void) {\n"
                  "extern int f;\n"
                  "static int i = f;\n"
                  "}",
                  ProducesError(VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    SEMA_PRODUCES("int i = (void*)5;", ProducesError(EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_TYPE));
    SEMA_PRODUCES("struct R { int i; };\n"
                  "struct R getR(void);\n"
                  "\n"
                  "void foo(void) {\n"
                  "_Bool i = getR();\n"
                  "}",
                  ProducesError(EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
    SEMA_PRODUCES(
        "int* i = (const int*)5;",
        ProducesError(CANNOT_INITIALIZE_VARIABLE_OF_TYPE_N_WITH_INCOMPATIBLE_TYPE_N, "'int *'", "'const int *'"));
    SEMA_PRODUCES("const int* i = 5.0;", ProducesError(EXPECTED_INITIALIZER_TO_BE_A_POINTER_TYPE));
    SEMA_PRODUCES("const int* i = 3;", ProducesError(EXPECTED_INITIALIZER_TO_BE_NULL));
    SEMA_PRODUCES("void foo(void);\n"
                  "void* i = foo;",
                  ProducesError(CANNOT_INITIALIZE_VOID_POINTER_WITH_FUNCTION_POINTER));
    SEMA_PRODUCES("extern void* foo;\n"
                  "void (*i)(void) = foo;",
                  ProducesError(CANNOT_INITIALIZE_FUNCTION_POINTER_WITH_VOID_POINTER_PARAMETER));
}

TEST_CASE("Semantics initializer list", "[semantics]")
{
    SECTION("Initializing struct")
    {
        auto [translationUnit, errors] = generateSemantics("typedef struct Point {\n"
                                                           "float x;\n"
                                                           "float y;\n"
                                                           "} Point;\n"
                                                           "\n"
                                                           "Point point = {5.0,3.0};");
        REQUIRE(translationUnit.getGlobals().size() == 1);
        auto& global = translationUnit.getGlobals()[0];
        REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
        auto& decl = cld::get<std::unique_ptr<Declaration>>(global);
        REQUIRE(decl->getInitializer());
        REQUIRE(std::holds_alternative<InitializerList>(*decl->getInitializer()));
        SEMA_PRODUCES("typedef struct Point {\n"
                      "float x;\n"
                      "float y;\n"
                      "} Point;\n"
                      "\n"
                      "Point point = {{5.0},{3.0}};\n",
                      ProducesNoErrors());
        SEMA_PRODUCES("typedef struct Point {\n"
                      "float x;\n"
                      "float y;\n"
                      "} Point;\n"
                      "\n"
                      "Point point = {5.0};\n",
                      ProducesNoErrors());
        SEMA_PRODUCES("typedef struct Point {\n"
                      "float x;\n"
                      "float y;\n"
                      "} Point;\n"
                      "\n"
                      "Point point = {5.0,3.0,2};\n",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
        SEMA_PRODUCES("typedef struct Point {\n"
                      "float x;\n"
                      "float y;\n"
                      "} Point;\n"
                      "\n"
                      "Point point = {5.0,3.0,2};\n",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
        SEMA_PRODUCES("typedef struct Point\n"
                      "{\n"
                      "    float x,y;\n"
                      "} Point;\n"
                      "\n"
                      "typedef struct Line\n"
                      "{\n"
                      "    Point p0,p1;\n"
                      "} Line;\n"
                      "\n"
                      "Line line = {5.0,5.0,5.0,5.0};\n",
                      ProducesNoErrors());
        SEMA_PRODUCES("\n"
                      "struct A\n"
                      "{\n"
                      "    char s[5];\n"
                      "    int r;\n"
                      "};\n"
                      "\n"
                      "struct B\n"
                      "{\n"
                      "    struct A a;\n"
                      "    int r;\n"
                      "};\n"
                      "\n"
                      "struct C\n"
                      "{\n"
                      "    struct B b;\n"
                      "    int r;\n"
                      "};\n"
                      "\n"
                      "struct A getA();\n"
                      "\n"
                      "struct B getB();\n"
                      "\n"
                      "struct C getC();\n"
                      "\n"
                      "void foo(void)\n"
                      "{\n"
                      "    struct C c = {\"d\"};\n"
                      "    struct C c1 = {{{{\"d\"}}}};\n"
                      "    struct C c2 = {{getA()}};\n"
                      "    struct C c3 = {getB()};\n"
                      "}\n",
                      ProducesNothing());
    }
    SECTION("Initializing union")
    {
        SEMA_PRODUCES("union F {\n"
                      "int r;\n"
                      "float f;\n"
                      "} t = {5,3.0};",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
        SEMA_PRODUCES("struct R {\n"
                      " union {\n"
                      "  int r;\n"
                      "  float f;\n"
                      " } u;\n"
                      "} t = {5,3.0};",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
        SEMA_PRODUCES("struct R {\n"
                      " union {\n"
                      "  int r;\n"
                      "  float f;\n"
                      " } u;\n"
                      " float f;\n"
                      "} t = {5,3.0};",
                      ProducesNoErrors());
        SEMA_PRODUCES("struct R {\n"
                      " union {\n"
                      "  int r;\n"
                      "  float f;\n"
                      " } u;\n"
                      " float f;\n"
                      "} t = {.u.f = 5,3.0};",
                      ProducesNoErrors());
    }
    SECTION("Initializing arrays")
    {
        SEMA_PRODUCES("int r[5] = {3,3,53,34,3};", ProducesNoErrors());
        SEMA_PRODUCES("int r[5] = {3,3,53,34,3,5};", ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
        SEMA_PRODUCES("struct Point {\n"
                      " float x,y;\n"
                      "} points[2] = {5.0,2.0,4.0,23.04};",
                      ProducesNoErrors());
        SEMA_PRODUCES("struct Point {\n"
                      " float x,y;\n"
                      "} points[2] = {5.0,2.0,4.0,23.04,7};",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
        SECTION("Size deduction")
        {
            SECTION("array of structs")
            {
                auto [translationUnit, errors] = generateSemantics("struct Point {\n"
                                                                   " float x,y;\n"
                                                                   "} points[] = {5.0,2.0,4.0,23.04,7};");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                auto& global = translationUnit.getGlobals()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
                auto& declaration = *cld::get<std::unique_ptr<Declaration>>(global);
                REQUIRE(std::holds_alternative<ArrayType>(declaration.getType().getVariant()));
                CHECK(cld::get<ArrayType>(declaration.getType().getVariant()).getSize() == 3);
            }
            SECTION("Designators")
            {
                using Catch::Matchers::Equals;
                auto [translationUnit, errors] =
                    generateSemantics("struct Point {\n"
                                      " float x,y;\n"
                                      "} points[] = {[10].x = 5,[10].y = 3,[0] = {3.0,2.0},3.0,3.0,23.025,23.024};");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                auto& global = translationUnit.getGlobals()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
                auto& declaration = *cld::get<std::unique_ptr<Declaration>>(global);
                REQUIRE(std::holds_alternative<ArrayType>(declaration.getType().getVariant()));
                CHECK(cld::get<ArrayType>(declaration.getType().getVariant()).getSize() == 11);
                REQUIRE(declaration.getInitializer());
                REQUIRE(std::holds_alternative<InitializerList>(*declaration.getInitializer()));
                auto& initialization = cld::get<InitializerList>(*declaration.getInitializer());
                REQUIRE(initialization.getFields().size() == 8);
                auto& fields = initialization.getFields();
                CHECK_THAT(fields[0].path, Equals<std::uint32_t>({10, 0}));
                CHECK_THAT(fields[1].path, Equals<std::uint32_t>({10, 1}));
                CHECK_THAT(fields[2].path, Equals<std::uint32_t>({0, 0}));
                CHECK_THAT(fields[3].path, Equals<std::uint32_t>({0, 1}));
                CHECK_THAT(fields[4].path, Equals<std::uint32_t>({1, 0}));
                CHECK_THAT(fields[5].path, Equals<std::uint32_t>({1, 1}));
                CHECK_THAT(fields[6].path, Equals<std::uint32_t>({2, 0}));
                CHECK_THAT(fields[7].path, Equals<std::uint32_t>({2, 1}));
            }
            SECTION("array is declared")
            {
                auto [translationUnit, errors] = generateSemantics("int a[] = {[sizeof(*a) - 1] = 5};");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                auto& global = translationUnit.getGlobals()[0];
                REQUIRE(std::holds_alternative<std::unique_ptr<Declaration>>(global));
                auto& declaration = *cld::get<std::unique_ptr<Declaration>>(global);
                REQUIRE(std::holds_alternative<ArrayType>(declaration.getType().getVariant()));
                CHECK(cld::get<ArrayType>(declaration.getType().getVariant()).getSize() == 4);
            }
        }
    }
    SECTION("Designators")
    {
        SEMA_PRODUCES("struct r { int i; float f; } a[2] = { .i = 3};",
                      ProducesError(EXPECTED_INDEX_DESIGNATOR_FOR_ARRAY_TYPE));
        SEMA_PRODUCES("int a[] = { [0.0] = 3};", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        SEMA_PRODUCES("int a[] = { [-5] = 3};", ProducesError(DESIGNATOR_INDEX_MUST_NOT_BE_NEGATIVE));
        SEMA_PRODUCES("int a[5] = { [5] = 3};",
                      ProducesError(DESIGNATOR_INDEX_OUT_OF_RANGE_FOR_ARRAY_TYPE_N, "'int[5]'"));
        SEMA_PRODUCES("struct r { int i; float f; } a = { [0] = 3};",
                      ProducesError(EXPECTED_MEMBER_DESIGNATOR_FOR_STRUCT_TYPE));
        SEMA_PRODUCES("union r { int i; float f; } a = { [0] = 3};",
                      ProducesError(EXPECTED_MEMBER_DESIGNATOR_FOR_UNION_TYPE));
        SEMA_PRODUCES("struct r { int i; float f; } a = { .r = 3};",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N, "'r'", "r"));
        SEMA_PRODUCES("union r { int i; float f; } a = { .r = 3};",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_UNION_N, "'r'", "r"));
        SEMA_PRODUCES("struct { int i; float f; } a = { .r = 3};",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT, "'r'"));
        SEMA_PRODUCES("union { int i; float f; } a = { .r = 3};",
                      ProducesError(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION, "'r'"));
        SEMA_PRODUCES("struct { int i; float f; int r[]; } a = { .r[0] = 3};",
                      ProducesError(CANNOT_INITIALIZE_FLEXIBLE_ARRAY_MEMBER));
        SEMA_PRODUCES("struct r { int i; float f; } a = { .i[0] = 3};",
                      ProducesError(CANNOT_INDEX_INTO_NON_ARRAY_TYPE_N, "'int'"));
        SEMA_PRODUCES("struct r { int i; float f; } a = { .i.m = 3};",
                      ProducesError(CANNOT_ACCESS_MEMBERS_OF_NON_STRUCT_OR_UNION_TYPE_N, "'int'"));
    }
    SECTION("Single brace")
    {
        SEMA_PRODUCES("int i = {5};", ProducesNoErrors());
        SEMA_PRODUCES("int i = {{5}};",
                      ProducesError(CANNOT_INITIALIZE_ARITHMETIC_OR_POINTER_TYPE_WITH_INITIALIZER_LIST));
        SEMA_PRODUCES("int* i = {0};", ProducesNoErrors());
        SEMA_PRODUCES("int i = {5,};", ProducesNoErrors());
        SEMA_PRODUCES("int* i = {0,};", ProducesNoErrors());
        SEMA_PRODUCES("char foo[6] = {\"string\"};", ProducesNoErrors());
        SEMA_PRODUCES("unsigned char foo[6] = {\"string\"};", ProducesNoErrors());
        SEMA_PRODUCES("signed char foo[6] = {\"string\"};", ProducesNoErrors());
    }
}

TEST_CASE("Semantics compound literal", "[semantics]")
{
    SECTION("Simple")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "(struct { int r; float f; }){.r = 3,.f = 3.53f};\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Lvalue);
        CHECK(std::holds_alternative<CompoundLiteral>(exp.getVariant()));
        CHECK(std::holds_alternative<AnonymousStructType>(exp.getType().getVariant()));
    }
    SECTION("Size deduction")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "(float[]){3,3.53f};\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Lvalue);
        CHECK(std::holds_alternative<CompoundLiteral>(exp.getVariant()));
        REQUIRE(std::holds_alternative<ArrayType>(exp.getType().getVariant()));
        CHECK(cld::get<ArrayType>(exp.getType().getVariant()).getSize() == 2);
    }
    SEMA_PRODUCES("void foo(void) {\n"
                  " (int(void)){5};\n"
                  "}",
                  ProducesError(CANNOT_INITIALIZE_FUNCTION_TYPE));
    SEMA_PRODUCES("void foo(int i) {\n"
                  " (int[i]){5};\n"
                  "}",
                  ProducesError(CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE));
    SEMA_PRODUCES("typedef struct Point {\n"
                  " float x;\n"
                  " float y;\n"
                  "} Point;\n"
                  "\n"
                  "Point p0 = (Point){.y = 3,.x = 5};",
                  ProducesNoErrors());
    SEMA_PRODUCES("typedef struct Point {\n"
                  " float x;\n"
                  " float y;\n"
                  "} Point;\n"
                  "\n"
                  "int getX();\n"
                  "\n"
                  "Point p0 = (Point){.y = 3,.x = getX()};",
                  ProducesError(FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    SEMA_PRODUCES("typedef struct Point {\n"
                  " float x;\n"
                  " float y;\n"
                  "} Point;\n"
                  "\n"
                  "int getX();\n"
                  "\n"
                  "void foo(void) {\n"
                  "static Point p0 = (Point){.y = 3,.x = getX()};\n"
                  "}",
                  ProducesError(FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    SEMA_PRODUCES("typedef struct Point {\n"
                  " float x;\n"
                  " float y;\n"
                  "} Point;\n"
                  "\n"
                  "int getX();\n"
                  "\n"
                  "void foo(void) {\n"
                  "static Point p0 = (Point){.y = 3,.x = 5};\n"
                  "Point p1 = (Point){.y = 3,.x = getX()};\n"
                  "}",
                  ProducesNoErrors());
}

TEST_CASE("Semantics __func__", "[semantics]")
{
    auto& exp = generateExpression("void foo(void) {\n"
                                   "__func__;\n"
                                   "}");
    REQUIRE(std::holds_alternative<Constant>(exp.getVariant()));
    auto& constant = cld::get<Constant>(exp.getVariant());
    REQUIRE(std::holds_alternative<std::string>(constant.getValue()));
    CHECK(cld::get<std::string>(constant.getValue()) == "foo");
    SEMA_PRODUCES("void __func__(void) {}",
                  ProducesError(DEFINING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("int __func__(void);", ProducesError(DECLARING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("int __func__;", ProducesError(DECLARING_VARIABLES_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("typedef int __func__;", ProducesError(DECLARING_TYPEDEFS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("int foo(int __func__) {}",
                  ProducesError(DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("int foo(int __func__);", ProducesNoErrors());
    SEMA_PRODUCES("int foo(__func__) {}",
                  ProducesError(DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("int foo() int __func__; {}",
                  ProducesError(DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
}

TEST_CASE("Semantics return statement", "[semantics]")
{
    SEMA_PRODUCES("int foo(void) {\n"
                  "return;\n"
                  "}",
                  ProducesError(CANNOT_RETURN_NO_VALUE_FROM_FUNCTION_N_WITH_RETURN_TYPE_N, "'foo'", "'int'"));
    SEMA_PRODUCES("void foo(void) {\n"
                  "return 5;\n"
                  "}",
                  ProducesError(CANNOT_RETURN_VALUE_FROM_FUNCTION_N_WITH_VOID_RETURN_TYPE, "'foo'"));
}

TEST_CASE("Semantics if statement", "[semantics]")
{
    SEMA_PRODUCES("struct R { int x; };\n"
                  "void foo(struct R r) {\n"
                  " if(r);\n"
                  "}",
                  ProducesError(CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
}

TEST_CASE("Semantics for statement", "[semantics]")
{
    SEMA_PRODUCES("void foo(int i) {\n"
                  "for(int i;;);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int i) {\n"
                  "for(extern int i;;);\n"
                  "}",
                  ProducesError(ONLY_AUTO_OR_REGISTER_ALLOWED_IN_FOR_STATEMENTS_DECLARATION));
    SEMA_PRODUCES("void foo(int i) {\n"
                  "for(static int i;;);\n"
                  "}",
                  ProducesError(ONLY_AUTO_OR_REGISTER_ALLOWED_IN_FOR_STATEMENTS_DECLARATION));
    SEMA_PRODUCES("void foo(int i) {\n"
                  "for(typedef int i;;);\n"
                  "}",
                  ProducesError(ONLY_AUTO_OR_REGISTER_ALLOWED_IN_FOR_STATEMENTS_DECLARATION));
    SEMA_PRODUCES("void foo(int i) {\n"
                  "for(auto int i;;);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int i) {\n"
                  "for(register int i;;);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("struct R { int x; };\n"
                  "void foo(struct R r) {\n"
                  " for(;r;);\n"
                  "}",
                  ProducesError(CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
}

TEST_CASE("Semantics while statement", "[semantics]")
{
    SEMA_PRODUCES("struct R { int x; };\n"
                  "void foo(struct R r) {\n"
                  " while(r);\n"
                  "}",
                  ProducesError(CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
}

TEST_CASE("Semantics do while statement", "[semantics]")
{
    SEMA_PRODUCES("struct R { int x; };\n"
                  "void foo(struct R r) {\n"
                  " do;while(r);\n"
                  "}",
                  ProducesError(CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE));
}

TEST_CASE("Semantics goto and label statement", "[semantics]")
{
    SEMA_PRODUCES("void foo(void) {\n"
                  "__func__:;\n"
                  "}",
                  ProducesError(DECLARING_LABEL_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR));
    SEMA_PRODUCES("void foo(void) {\n"
                  "text:;\n"
                  "text:;\n"
                  "}",
                  ProducesError(REDEFINITION_OF_LABEL_N, "'text'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("void foo(void) {\n"
                  "goto text;\n"
                  "}",
                  ProducesError(NO_LABEL_CALLED_N_FOUND_IN_FUNCTION_N, "'text'", "'foo'"));
    SEMA_PRODUCES("void foo(void) {\n"
                  "text:\n"
                  "goto text;\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(void) {\n"
                  "goto text;\n"
                  "{\n"
                  " text:;\n"
                  "}\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "goto text;\n"
                  "{\n"
                  " int r[n];\n"
                  " text:;\n"
                  "}\n"
                  "}",
                  ProducesError(GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N, "'text'", "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE, "'r'", "'int[*]'")
                      && ProducesNote(LABEL_N_HERE, "'text'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "goto text;\n"
                  "{\n"
                  " int (*r)[n];\n"
                  " text:;\n"
                  "}\n"
                  "}",
                  ProducesError(GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N, "'text'", "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE, "'r'", "'int(*)[*]'")
                      && ProducesNote(LABEL_N_HERE, "'text'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "goto text;\n"
                  "{\n"
                  " typedef int r[n];\n"
                  " text:;\n"
                  "}\n"
                  "}",
                  ProducesError(GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N, "'text'", "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_TYPEDEF_N_HERE, "'r'") && ProducesNote(LABEL_N_HERE, "'text'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "goto text;\n"
                  "{\n"
                  " text:;\n"
                  " typedef int r[n];\n"
                  "}\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  " goto text;\n"
                  " int r[n];\n"
                  " text:;\n"
                  "}",
                  ProducesError(GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N, "'text'", "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE, "'r'", "'int[*]'")
                      && ProducesNote(LABEL_N_HERE, "'text'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  " goto text;\n"
                  " text:;\n"
                  " typedef int r[n];\n"
                  "}",
                  ProducesNoErrors());
}

TEST_CASE("Semantics switch statement", "[semantics]")
{
    SEMA_PRODUCES("void foo(void) {\n"
                  "switch(5.0);\n"
                  "}",
                  ProducesError(CONTROLLING_EXPRESSION_MUST_BE_AN_INTEGER_TYPE));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " case 5.0:;\n"
                  "}\n"
                  "}",
                  ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
    SEMA_PRODUCES("void foo(int n) {\n"
                  " case 5:;\n"
                  "}",
                  ProducesError(CASE_MUST_BE_WITHIN_A_SWITCH_STATEMENT));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " case 3 + 2:;\n"
                  " case 2 + 3:;\n"
                  "}\n"
                  "}",
                  ProducesError(REDEFINITION_OF_CASE_WITH_VALUE_N, "'5'") && ProducesNote(PREVIOUS_CASE_HERE));
    SEMA_PRODUCES("void foo(int n) {\n"
                  " default:;\n"
                  "}",
                  ProducesError(DEFAULT_MUST_BE_WITHIN_A_SWITCH_STATEMENT));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " default:;\n"
                  " default:;\n"
                  "}\n"
                  "}",
                  ProducesError(REDEFINITION_OF_DEFAULT) && ProducesNote(PREVIOUS_DEFAULT_HERE));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " int r[n];\n"
                  " case 2 + 3:;\n"
                  "}\n"
                  "}",
                  ProducesError(CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N, "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE, "'r'", "'int[*]'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " int r[n];\n"
                  " default:;\n"
                  "}\n"
                  "}",
                  ProducesError(DEFAULT_CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N, "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE, "'r'", "'int[*]'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " typedef int r[n];\n"
                  " case 2 + 3:;\n"
                  "}\n"
                  "}",
                  ProducesError(CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N, "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_TYPEDEF_N_HERE, "'r'"));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " typedef int r[n];\n"
                  " default:;\n"
                  "}\n"
                  "}",
                  ProducesError(DEFAULT_CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N, "'r'")
                      && ProducesNote(VARIABLY_MODIFIED_TYPEDEF_N_HERE, "'r'"));

    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " case 2 + 3:;\n"
                  " int r[n];\n"
                  "}\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " default:;\n"
                  " int r[n];\n"
                  "}\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " case 2 + 3:;\n"
                  " typedef int r[n];\n"
                  "}\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  " default:;\n"
                  " typedef int r[n];\n"
                  "}\n"
                  "}",
                  ProducesNoErrors());
}

TEST_CASE("Semantics break and continue", "[semantics]")
{
    SEMA_PRODUCES("void foo(void) {\n"
                  "break;\n"
                  "}",
                  ProducesError(BREAK_MUST_BE_WITHIN_A_SWITCH_OR_LOOP_STATEMENT));
    SEMA_PRODUCES("void foo(void) {\n"
                  "continue;\n"
                  "}",
                  ProducesError(CONTINUE_MUST_BE_WITHIN_A_LOOP_STATEMENT));
    SEMA_PRODUCES("void foo(int n) {\n"
                  "switch(n) {\n"
                  "case 5:break;\n"
                  "case 10:n += 5;\n"
                  "default:break;\n"
                  "}"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "while(1) break;\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "do break; while(1);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "for(;;) break;\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "while(1) continue;\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "do continue; while(1);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void foo(int n) {\n"
                  "for(;;) continue;\n"
                  "}",
                  ProducesNoErrors());
}

TEST_CASE("Semantics enum type", "[semantics]")
{
    SEMA_PRODUCES("enum E {\n"
                  "\tx,\n"
                  "\ty,\n"
                  "\tz,\n"
                  "};\n"
                  "\n"
                  "int\n"
                  "main()\n"
                  "{\n"
                  "\tenum E e;\n"
                  "\n"
                  "\tif(x != 0)\n"
                  "\t\treturn 1;\n"
                  "\tif(y != 1)\n"
                  "\t\treturn 2;\n"
                  "\tif(z != 2)\n"
                  "\t\treturn 3;\n"
                  "\t\n"
                  "\te = x;\n"
                  "\treturn e;\n"
                  "}\n"
                  "",
                  ProducesNoErrors());
}

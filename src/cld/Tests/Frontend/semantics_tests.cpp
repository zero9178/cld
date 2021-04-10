#include <catch.hpp>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/SemanticAnalysis.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>

#include <array>

#include "TestConfig.hpp"

using namespace cld::Semantics;

static std::pair<const TranslationUnit * CLD_NON_NULL, std::string>
    generateSemantics(std::string_view source, const cld::LanguageOptions& options)
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    cld::PPSourceObject tokens;
    bool errors = false;
    tokens = cld::Lexer::tokenize(cld::to_string(source), &options, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    tokens = cld::PP::preprocess(std::move(tokens), {}, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto ctokens = cld::Lexer::toCTokens(tokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto parsing = cld::Parser::buildTree(ctokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    static Program program;
    program = analyse(parsing, std::move(ctokens), &ss);
    return {&program.getTranslationUnit(), ss.str()};
}

static std::pair<const TranslationUnit * CLD_NON_NULL, std::string>
    generateSemantics(std::string_view source, cld::Triple triple = cld::Triple::native())
{
    return generateSemantics(source, cld::LanguageOptions::fromTriple(triple));
}

static Program generateProgram(std::string_view source, cld::Triple triple = cld::Triple::native())
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    cld::PPSourceObject tokens;
    bool errors = false;
    auto options = cld::LanguageOptions::fromTriple(triple);
    tokens = cld::Lexer::tokenize(cld::to_string(source), &options, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    tokens = cld::PP::preprocess(std::move(tokens), {}, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto ctokens = cld::Lexer::toCTokens(tokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto parsing = cld::Parser::buildTree(ctokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    return analyse(parsing, std::move(ctokens), &ss);
}

#define SEMA_PRODUCES(source, matcher)               \
    [&](std::string input) {                         \
        auto text = generateSemantics(input).second; \
        CHECK_THAT(text, matcher);                   \
        llvm::errs() << text;                        \
    }(source)

#define SEMA_PRODUCES_WITH(source, matcher, tripleOrOption)          \
    [&](std::string input) {                                         \
        auto text = generateSemantics(input, tripleOrOption).second; \
        CHECK_THAT(text, matcher);                                   \
        llvm::errs() << text;                                        \
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
        REQUIRE_THAT(errors, ProducesNoErrors());
        REQUIRE(translationUnit->getGlobals().size() == 2);
        {
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getLinkage() == Linkage::External);
            CHECK(decl->getLifetime() == Lifetime::Static);
            CHECK(decl->getNameToken()->getText() == "i");
            CHECK(decl->getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        }
        {
            REQUIRE(translationUnit->getGlobals()[1]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[1]->as<VariableDeclaration>();
            CHECK(decl->getLinkage() == Linkage::External);
            CHECK(decl->getLifetime() == Lifetime::Static);
            CHECK(decl->getNameToken()->getText() == "f");
            CHECK(decl->getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
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
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(decl->getLinkage() == Linkage::External);
                CHECK(decl->getLifetime() == Lifetime::Static);
                CHECK(decl->getKind() == VariableDeclaration::Kind::DeclarationOnly);
            }
            SECTION("With init")
            {
                auto [translationUnit, errors] = generateSemantics("extern int i = 5;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(decl->getLinkage() == Linkage::External);
                CHECK(decl->getLifetime() == Lifetime::Static);
                CHECK(decl->getKind() == VariableDeclaration::Kind::DeclarationOnly);
            }
            SEMA_PRODUCES("void foo(void) {\n"
                          " extern int i = 0;\n"
                          "}",
                          ProducesError(CANNOT_INITIALIZE_STATIC_OR_EXTERN_VARIABLE_AT_BLOCK_SCOPE));
        }
        SECTION("Prior internal linkage overwrites external")
        {
            SECTION("Variable")
            {
                auto [translationUnit, errors] = generateSemantics("static int i;\n"
                                                                   "extern int i;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 2);
                REQUIRE(translationUnit->getGlobals()[1]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[1]->as<VariableDeclaration>();
                CHECK(decl->getLinkage() == Linkage::Internal);
                CHECK(decl->getLifetime() == Lifetime::Static);
                CHECK(decl->getKind() == VariableDeclaration::Kind::TentativeDefinition);
                SEMA_PRODUCES("static int i;\n"
                              "int i;",
                              ProducesError(STATIC_VARIABLE_N_REDEFINED_WITHOUT_STATIC, "'i'"));
            }
            SECTION("Function")
            {
                auto [translationUnit, errors] = generateSemantics("static int foo(void);\n"
                                                                   "extern int foo(void);\n"
                                                                   "int foo(void) { }");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 3);
                REQUIRE(translationUnit->getGlobals()[2]->is<FunctionDefinition>());
                auto* def = &translationUnit->getGlobals()[2]->as<FunctionDefinition>();
                CHECK(def->getLinkage() == Linkage::Internal);
            }
        }
        SECTION("Static re-definition/declaration")
        {
            SEMA_PRODUCES("extern int foo(void);\n"
                          "static int foo(void);",
                          ProducesError(REDECLARATION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE, "'foo'"));
            SEMA_PRODUCES("int foo(void) { }\n"
                          "static int foo(void);",
                          ProducesError(REDECLARATION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE, "'foo'"));
            SEMA_PRODUCES("int foo(void);\n"
                          "static int foo(void) { }",
                          ProducesError(REDEFINITION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE, "'foo'"));
        }
        SECTION("External definitions with static linkage")
        {
            SECTION("Tentative definition")
            {
                auto [translationUnit, errors] = generateSemantics("static int i;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(decl->getLinkage() == Linkage::Internal);
                CHECK(decl->getLifetime() == Lifetime::Static);
                CHECK(decl->getKind() == VariableDeclaration::Kind::TentativeDefinition);
            }
            SECTION("Definition")
            {
                auto [translationUnit, errors] = generateSemantics("static int i = 0;");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(decl->getLinkage() == Linkage::Internal);
                CHECK(decl->getLifetime() == Lifetime::Static);
                CHECK(decl->getKind() == VariableDeclaration::Kind::Definition);
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
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() >= 1);
                auto& var = func.getCompoundStatement().getCompoundItems().back();
                REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(var));
                auto& decl = *cld::get<cld::IntrVarPtr<Declaration>>(var);
                CHECK(decl.getLinkage() == Linkage::None);
            }
            SECTION("extern")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    extern int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() >= 1);
                auto& var = func.getCompoundStatement().getCompoundItems().back();
                REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(var));
                auto& decl = *cld::get<cld::IntrVarPtr<Declaration>>(var);
                CHECK(decl.getLinkage() == Linkage::External);
            }
        }
        SECTION("Objects at file scope are external by default")
        {
            auto [translationUnit, errors] = generateSemantics("int i;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getLinkage() == Linkage::External);
            CHECK(decl->getLifetime() == Lifetime::Static);
        }
        SECTION("Internal linkage")
        {
            auto [translationUnit, errors] = generateSemantics("static int i;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getLinkage() == Linkage::Internal);
            CHECK(decl->getLifetime() == Lifetime::Static);
        }
        SEMA_PRODUCES("static extern int i;",
                      ProducesError(ONLY_ONE_STORAGE_SPECIFIER) && ProducesNote(PREVIOUS_STORAGE_SPECIFIER_HERE));
    }
    SECTION("Lifetime")
    {
        SECTION("File scope")
        {
            auto [translationUnit, errors] = generateSemantics("int i;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getLifetime() == Lifetime::Static);
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
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() >= 1);
                auto& var = func.getCompoundStatement().getCompoundItems().back();
                REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(var));
                auto& decl = *cld::get<cld::IntrVarPtr<Declaration>>(var);
                auto* varDecl = decl.tryAs<VariableDeclaration>();
                REQUIRE(varDecl);
                CHECK(varDecl->getLifetime() == Lifetime::Automatic);
            }
            SECTION("auto")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    auto int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() >= 1);
                auto& var = func.getCompoundStatement().getCompoundItems().back();
                REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(var));
                auto& decl = *cld::get<cld::IntrVarPtr<Declaration>>(var);
                auto* varDecl = decl.tryAs<VariableDeclaration>();
                REQUIRE(varDecl);
                CHECK(varDecl->getLifetime() == Lifetime::Automatic);
            }
            SECTION("static")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    static int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() >= 1);
                auto& var = func.getCompoundStatement().getCompoundItems().back();
                REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(var));
                auto& decl = *cld::get<cld::IntrVarPtr<Declaration>>(var);
                auto* varDecl = decl.tryAs<VariableDeclaration>();
                REQUIRE(varDecl);
                CHECK(varDecl->getLifetime() == Lifetime::Static);
            }
            SECTION("extern")
            {
                auto [translationUnit, errors] = generateSemantics("void foo(void)\n"
                                                                   "{\n"
                                                                   "    extern int i;\n"
                                                                   "}\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                REQUIRE(func.getCompoundStatement().getCompoundItems().size() >= 1);
                auto& var = func.getCompoundStatement().getCompoundItems().back();
                REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(var));
                auto& decl = *cld::get<cld::IntrVarPtr<Declaration>>(var);
                auto* varDecl = decl.tryAs<VariableDeclaration>();
                REQUIRE(varDecl);
                CHECK(varDecl->getLifetime() == Lifetime::Static);
            }
            SECTION("register")
            {
                SECTION("Param list")
                {
                    auto [translationUnit, errors] = generateSemantics("void foo(register int i)\n"
                                                                       "{}\n");
                    REQUIRE_THAT(errors, ProducesNoErrors());
                    REQUIRE(translationUnit->getGlobals().size() == 1);
                    REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                    auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                    REQUIRE(func.getParameterDeclarations().size() == 1);
                    auto& decl = *func.getParameterDeclarations()[0];
                    CHECK(decl.getLifetime() == Lifetime::Register);
                }
                SECTION("Identifier list")
                {
                    auto [translationUnit, errors] = generateSemantics("void foo(i) register int i;\n"
                                                                       "{}\n");
                    REQUIRE_THAT(errors, ProducesNoErrors());
                    REQUIRE(translationUnit->getGlobals().size() == 1);
                    REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDefinition>());
                    auto& func = translationUnit->getGlobals()[0]->as<FunctionDefinition>();
                    REQUIRE(func.getParameterDeclarations().size() == 1);
                    auto& decl = *func.getParameterDeclarations()[0];
                    CHECK(decl.getLifetime() == Lifetime::Register);
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "d");
            CHECK(decl->getType() == PrimitiveType(PrimitiveType::LongDouble, cld::LanguageOptions::native()));
            CHECK(decl->getType().isTypedef());
        }
        SECTION("Stacking const volatile")
        {
            auto [translationUnit, errors] = generateSemantics("typedef long double ld;\n"
                                                               "const volatile ld d;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "d");
            CHECK(decl->getType()
                  == PrimitiveType(PrimitiveType::LongDouble, cld::LanguageOptions::native(), flag::isConst = true,
                                   flag::isVolatile = true));
        }
        SECTION("Can't remove const volatile")
        {
            auto [translationUnit, errors] = generateSemantics("typedef const volatile long double ld;\n"
                                                               "ld d;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "d");
            CHECK(decl->getType()
                  == PrimitiveType(PrimitiveType::LongDouble, cld::LanguageOptions::native(), flag::isConst = true,
                                   flag::isVolatile = true));
        }
        SECTION("Combine qualifiers")
        {
            auto [translationUnit, errors] = generateSemantics("typedef const long double ld;\n"
                                                               "volatile ld d;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "d");
            CHECK(decl->getType()
                  == PrimitiveType(PrimitiveType::LongDouble, cld::LanguageOptions::native(), flag::isConst = true,
                                   flag::isVolatile = true));
        }
        SECTION("Qualifiers on array types")
        {
            auto [translationUnit, errors] = generateSemantics("typedef long double ld[5];\n"
                                                               "volatile ld d;");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "d");
            auto longDouble =
                PrimitiveType(PrimitiveType::LongDouble, cld::LanguageOptions::native(), flag::isVolatile = true);
            CHECK(decl->getType() == ArrayType(&longDouble, 5));
        }
        SECTION("Variable length array")
        {
            auto [translationUnit, errors] = generateSemantics("void foo(int n) {\n"
                                                               "typedef int r[2 * n][n++];\n"
                                                               "r f;\n"
                                                               "}");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            auto& global = translationUnit->getGlobals()[0];
            REQUIRE(global->is<FunctionDefinition>());
            auto& def = global->as<FunctionDefinition>();
            REQUIRE(def.getCompoundStatement().getCompoundItems().size() >= 4);
            auto& first = def.getCompoundStatement().getCompoundItems()[1];
            REQUIRE(std::holds_alternative<std::shared_ptr<const ExpressionBase>>(first));
            auto& firstExpr = *cld::get<std::shared_ptr<const ExpressionBase>>(first);
            CHECK(firstExpr.is<BinaryOperator>());
            auto& second = def.getCompoundStatement().getCompoundItems()[2];
            REQUIRE(std::holds_alternative<std::shared_ptr<const ExpressionBase>>(second));
            auto& secondExpr = *cld::get<std::shared_ptr<const ExpressionBase>>(second);
            CHECK(secondExpr.is<UnaryOperator>());
            auto& third = def.getCompoundStatement().getCompoundItems()[3];
            REQUIRE(std::holds_alternative<cld::IntrVarPtr<Declaration>>(third));
            auto& thirdDecl = *cld::get<cld::IntrVarPtr<Declaration>>(third);
            REQUIRE(thirdDecl.is<VariableDeclaration>());
            CHECK(thirdDecl.as<VariableDeclaration>().getType().is<ValArrayType>());
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
    SECTION("Incomplete types")
    {
        SEMA_PRODUCES("void foo() { void f;}", ProducesError(DECLARATION_MUST_NOT_BE_VOID));
        SEMA_PRODUCES("void foo() { struct R f; }", ProducesError(DECLARATION_MUST_HAVE_A_COMPLETE_TYPE));
        SEMA_PRODUCES("void f;", ProducesError(TYPE_OF_TENTATIVE_DEFINITION_IS_NEVER_COMPLETED));
        SEMA_PRODUCES("int f[];", ProducesNoErrors());
        SEMA_PRODUCES("struct R f;", ProducesError(TYPE_OF_TENTATIVE_DEFINITION_IS_NEVER_COMPLETED));
        SEMA_PRODUCES("struct R f;\n"
                      "\n"
                      "\n"
                      "struct R { int i; };",
                      ProducesNoErrors());
        SEMA_PRODUCES("int f[];\n"
                      "\n"
                      "\n"
                      "int f[5];",
                      ProducesNoErrors());
        SEMA_PRODUCES("struct R f = {0};", ProducesError(CANNOT_INITIALIZE_VARIABLE_OF_INCOMPLETE_TYPE));
        SEMA_PRODUCES("extern struct R f;", ProducesNoErrors());
        SEMA_PRODUCES("extern struct R f = {0};", ProducesError(CANNOT_INITIALIZE_VARIABLE_OF_INCOMPLETE_TYPE));
    }
    SEMA_PRODUCES("int;", ProducesError(DECLARATION_DOES_NOT_DECLARE_ANYTHING));
    SEMA_PRODUCES("struct f;", !ProducesError(DECLARATION_DOES_NOT_DECLARE_ANYTHING));
    SEMA_PRODUCES("int i;\n"
                  "float i;",
                  ProducesError(REDEFINITION_OF_SYMBOL_N, "'i'") && ProducesNote(PREVIOUSLY_DECLARED_HERE));
    SEMA_PRODUCES("inline int f;", ProducesError(INLINE_ONLY_ALLOWED_FOR_FUNCTIONS));
    SEMA_PRODUCES("static int i;", ProducesWarning(UNUSED_VARIABLE_N, "'i'"));
    SEMA_PRODUCES("int i;", !ProducesWarning(UNUSED_VARIABLE_N, "'i'"));
}

TEST_CASE("Semantics primitive declarations", "[semantics]")
{
    auto [isConst, isVolatile] = GENERATE(table<bool, bool>({std::tuple{false, true}, std::tuple{false, true}}));
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
    auto options = cld::LanguageOptions::fromTriple(x64linux);
    std::vector<std::pair<PrimitiveType::Kind, std::vector<std::string_view>>> table = {
        {PrimitiveType::Char, {"char"}},
        {PrimitiveType::SignedChar, {"signed", "char"}},
        {PrimitiveType::UnsignedChar, {"unsigned", "char"}},
        {PrimitiveType::Short, {"short"}},
        {PrimitiveType::Short, {"short", "int"}},
        {PrimitiveType::Short, {"short", "int", "signed"}},
        {PrimitiveType::Short, {"short", "signed"}},
        {PrimitiveType::UnsignedShort, {"short", "unsigned"}},
        {PrimitiveType::UnsignedShort, {"short", "unsigned", "int"}},
        {PrimitiveType::Int, {"int"}},
        {PrimitiveType::Int, {"int", "signed"}},
        {PrimitiveType::Int, {"signed"}},
        {PrimitiveType::UnsignedInt, {"unsigned"}},
        {PrimitiveType::UnsignedInt, {"unsigned", "int"}},
        {PrimitiveType::Long, {"long"}},
        {PrimitiveType::Long, {"long", "int"}},
        {PrimitiveType::Long, {"long", "int", "signed"}},
        {PrimitiveType::Long, {"long", "signed"}},
        {PrimitiveType::UnsignedLong, {"long", "unsigned"}},
        {PrimitiveType::UnsignedLong, {"long", "unsigned", "int"}},
        {PrimitiveType::LongLong, {"long", "long"}},
        {PrimitiveType::LongLong, {"long", "long", "int"}},
        {PrimitiveType::LongLong, {"long", "long", "int", "signed"}},
        {PrimitiveType::LongLong, {"long", "long", "signed"}},
        {PrimitiveType::UnsignedLongLong, {"long", "long", "int", "unsigned"}},
        {PrimitiveType::UnsignedLongLong, {"long", "long", "unsigned"}},
        {PrimitiveType::Float, {"float"}},
        {PrimitiveType::Double, {"double"}},
        {PrimitiveType::LongDouble, {"long", "double"}},
        {PrimitiveType::Bool, {"_Bool"}},
        {PrimitiveType::Int128, {"__int128"}},
        {PrimitiveType::Int128, {"signed", "__int128"}},
        {PrimitiveType::UnsignedInt128, {"unsigned", "__int128"}},
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "i");
            CHECK(decl->getType()
                  == PrimitiveType(type, options, flag::isConst = isConst, flag::isVolatile = isVolatile));
            CHECK(decl->getLinkage() == Linkage::External);
            CHECK(decl->getLifetime() == Lifetime::Static);
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "f");
            auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
            CHECK(decl->getType() == PointerType(&integer));
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
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(decl->getNameToken()->getText() == "f");
                auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native(),
                                             flag::isConst = isConst, flag::isVolatile = isVolatile);
                CHECK(decl->getType() == PointerType(&integer));
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
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(decl->getNameToken()->getText() == "f");
                auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native(),
                                             flag::isConst = isConst, flag::isVolatile = isVolatile);
                CHECK(decl->getType() == PointerType(&integer));
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "f");
            auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
            CHECK(decl->getType()
                  == PointerType(&integer, flag::isConst = isConst, flag::isVolatile = isVolatile,
                                 flag::isRestricted = isRestrict));
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "f");
            auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
            CHECK(decl->getType() == ArrayType(&integer, 1));
        }
        SECTION("Order")
        {
            auto [translationUnit, errors] = generateSemantics("int (*f[1]);");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(decl->getNameToken()->getText() == "f");
            auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
            auto pointer = PointerType(&integer);
            CHECK(decl->getType() == ArrayType(&pointer, 1));
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
        SEMA_PRODUCES("int f[];", ProducesNoErrors());

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
        REQUIRE_THAT(errors, ProducesNoErrors());
        REQUIRE(translationUnit->getGlobals().size() == 1);
        REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDeclaration>());
        auto* decl = &translationUnit->getGlobals()[0]->as<FunctionDeclaration>();
        CHECK(decl->getNameToken()->getText() == "f");
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
        auto floating = PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native());
        CHECK(decl->getType() == FunctionType(&integer, {{&integer, ""}, {&floating, ""}}));
    }
    SECTION("Parameters")
    {
        SEMA_PRODUCES("int f(int a[]);", ProducesNoErrors());
        SEMA_PRODUCES("int f(void);", ProducesNoErrors());
        SEMA_PRODUCES("int f(const void);", ProducesError(VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER));
        SEMA_PRODUCES("int f(int a[*]);", ProducesNoErrors());
        SEMA_PRODUCES("int f(int a,float a);", ProducesNoErrors());
        SEMA_PRODUCES("int f(register int a);", ProducesNoErrors());
        SEMA_PRODUCES("int f(int a[static 6]);", ProducesNoErrors());
        SEMA_PRODUCES("int f(int a[const 6]);", ProducesNoErrors());
        SEMA_PRODUCES("int f(int (*a)[static 6]);", ProducesError(ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_STATIC));
        SEMA_PRODUCES("int f(int (*a)[const 6]);", ProducesError(ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_QUALIFIED));
        SEMA_PRODUCES("int f(int a[6][static 5]);", ProducesError(STATIC_ONLY_ALLOWED_IN_OUTERMOST_ARRAY));
        SEMA_PRODUCES("int f(int a[6][const 5]);", ProducesError(ARRAY_QUALIFIERS_ONLY_ALLOWED_IN_OUTERMOST_ARRAY));
    }
    SECTION("With calling convention")
    {
        auto [translationUnit, errors] = generateSemantics("#define __cdecl __attribute__((__cdecl))\n"
                                                           "\n"
                                                           "int __cdecl atexit(void (__cdecl *)(void));",
                                                           cld::Tests::x64windowsGnu);
        REQUIRE_THAT(errors, ProducesNoErrors());
        REQUIRE(translationUnit->getGlobals().size() == 1);
        REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDeclaration>());
        auto* decl = &translationUnit->getGlobals()[0]->as<FunctionDeclaration>();
        CHECK(decl->getNameToken()->getText() == "atexit");
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
        auto voidType = PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native());
        auto callBack = FunctionType(&voidType,{});
        auto pointer = PointerType(&callBack);
        CHECK(decl->getType() == FunctionType(&integer, {{&pointer, ""}}));
    }
    SEMA_PRODUCES("int f(int) = 5;", ProducesError(FUNCTION_PROTOTYPE_MUST_NOT_HAVE_AN_INITIALIZER));
    SEMA_PRODUCES("int f(void) {\n"
                  "static int foo(void);\n"
                  "}",
                  ProducesError(FUNCTION_PROTOTYPE_AT_BLOCK_SCOPE_MAY_ONLY_BE_EXTERN));
    SEMA_PRODUCES("static inline int f(int);", ProducesNoErrors());
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
        REQUIRE_THAT(errors, ProducesNoErrors());
        REQUIRE(translationUnit->getGlobals().size() == 1);
        REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
        auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(decl->getType().is<StructType>());
        CHECK(decl->getType().as<StructType>().getStructName() == "A");
    }
    SECTION("Defining two variables with one struct definition")
    {
        SEMA_PRODUCES("struct A{ int i; float f, r; } a,b;", ProducesNoErrors());
        SEMA_PRODUCES("struct A;\n"
                      "typedef struct A{ int i; float f, r; } a,b;",
                      ProducesNoErrors());
    }
    SECTION("Simple union")
    {
        auto [translationUnit, errors] = generateSemantics("union A{ int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNoErrors());
        REQUIRE(translationUnit->getGlobals().size() == 1);
        REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
        auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(decl->getType().is<UnionType>());
        CHECK(decl->getType().as<UnionType>().getUnionName() == "A");
    }
    SECTION("Union alignment")
    {
        auto program = generateProgram("union A{ int i[3]; int* f; } a;");
        auto* unionInfo = program.lookupType<UnionInfo>("A", ProgramInterface::GLOBAL_SCOPE);
        REQUIRE(unionInfo);
        auto* unionDef = std::get_if<UnionDefinition>(&unionInfo->type);
        REQUIRE(unionDef);
        CHECK(unionDef->getAlignOf() == cld::LanguageOptions::native().sizeOfVoidStar);
        CHECK(unionDef->getSizeOf() == 4 * cld::LanguageOptions::native().sizeOfInt);
    }
    SECTION("Anonymous struct")
    {
        auto program = generateProgram("struct { int i; float f, r; } a;");
        REQUIRE(program.getTranslationUnit().getGlobals().size() == 1);
        REQUIRE(program.getTranslationUnit().getGlobals()[0]->is<VariableDeclaration>());
        auto* decl = &program.getTranslationUnit().getGlobals()[0]->as<VariableDeclaration>();
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(decl->getType().is<StructType>());
        CHECK(decl->getType().as<StructType>().isAnonymous());
        auto& fields = getFields(decl->getType());
        CHECK(fields.size() == 3);
        CHECK(fields.values_container()[0].second.name == "i");
        CHECK(*fields.values_container()[0].second.type
              == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK_FALSE(fields.values_container()[0].second.bitFieldBounds);
        CHECK(fields.values_container()[1].second.name == "f");
        CHECK(*fields.values_container()[1].second.type
              == PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native()));
        CHECK_FALSE(fields.values_container()[1].second.bitFieldBounds);
        CHECK(fields.values_container()[2].second.name == "r");
        CHECK(*fields.values_container()[2].second.type
              == PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native()));
        CHECK_FALSE(fields.values_container()[2].second.bitFieldBounds);
    }
    SECTION("Anonymous union")
    {
        auto program = generateProgram("union { int i; float f, r; } a;");
        REQUIRE(program.getTranslationUnit().getGlobals().size() == 1);
        REQUIRE(program.getTranslationUnit().getGlobals()[0]->is<VariableDeclaration>());
        auto* decl = &program.getTranslationUnit().getGlobals()[0]->as<VariableDeclaration>();
        CHECK(decl->getNameToken()->getText() == "a");
        REQUIRE(decl->getType().is<UnionType>());
        CHECK(decl->getType().as<UnionType>().isAnonymous());
        auto& fields = getFields(decl->getType());
        CHECK(fields.size() == 3);
        CHECK(fields.values_container()[0].second.name == "i");
        CHECK(*fields.values_container()[0].second.type
              == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK_FALSE(fields.values_container()[0].second.bitFieldBounds);
        CHECK(fields.values_container()[1].second.name == "f");
        CHECK(*fields.values_container()[1].second.type
              == PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native()));
        CHECK_FALSE(fields.values_container()[1].second.bitFieldBounds);
        CHECK(fields.values_container()[2].second.name == "r");
        CHECK(*fields.values_container()[2].second.type
              == PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native()));
        CHECK_FALSE(fields.values_container()[2].second.bitFieldBounds);
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
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                REQUIRE(decl->getType().is<ArrayType>());
                auto& array = decl->getType().as<ArrayType>();
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
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                REQUIRE(translationUnit->getGlobals()[0]->is<VariableDeclaration>());
                auto* decl = &translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                REQUIRE(decl->getType().is<ArrayType>());
                auto& array = decl->getType().as<ArrayType>();
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
            auto program = generateProgram(source, cld::Tests::x64windowsMsvc);
            auto* structInfo = program.lookupType<StructInfo>("A", 0);
            REQUIRE(structInfo);
            auto* structDef = std::get_if<StructDefinition>(&structInfo->type);
            REQUIRE(structDef);
            REQUIRE(structDef->getFields().size() == 6);
            CHECK(structDef->getMemLayout().size() == 2);

            CHECK_THAT(structDef->getFields().values_container()[0].second.indices,
                       Catch::Equals(std::vector<std::uint64_t>{0}));
            REQUIRE(structDef->getFields().values_container()[0].second.bitFieldBounds);
            CHECK(*structDef->getFields().values_container()[0].second.bitFieldBounds == std::pair{0u, 8u});

            CHECK_THAT(structDef->getFields().values_container()[1].second.indices,
                       Catch::Equals(std::vector<std::uint64_t>{0}));
            REQUIRE(structDef->getFields().values_container()[1].second.bitFieldBounds);
            CHECK(*structDef->getFields().values_container()[1].second.bitFieldBounds == std::pair{8u, 11u});

            CHECK_THAT(structDef->getFields().values_container()[2].second.indices,
                       Catch::Equals(std::vector<std::uint64_t>{0}));
            REQUIRE(structDef->getFields().values_container()[2].second.bitFieldBounds);
            CHECK(*structDef->getFields().values_container()[2].second.bitFieldBounds == std::pair{11u, 12u});

            CHECK_THAT(structDef->getFields().values_container()[3].second.indices,
                       Catch::Equals(std::vector<std::uint64_t>{0}));
            REQUIRE(structDef->getFields().values_container()[3].second.bitFieldBounds);
            CHECK(*structDef->getFields().values_container()[3].second.bitFieldBounds == std::pair{12u, 13u});

            CHECK_THAT(structDef->getFields().values_container()[4].second.indices,
                       Catch::Equals(std::vector<std::uint64_t>{0}));
            REQUIRE(structDef->getFields().values_container()[4].second.bitFieldBounds);
            CHECK(*structDef->getFields().values_container()[4].second.bitFieldBounds == std::pair{13u, 14u});

            CHECK_THAT(structDef->getFields().values_container()[5].second.indices,
                       Catch::Equals(std::vector<std::uint64_t>{1}));
            REQUIRE(structDef->getFields().values_container()[5].second.bitFieldBounds);
            CHECK(*structDef->getFields().values_container()[5].second.bitFieldBounds == std::pair{0u, 1u});
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
    SECTION("Scope")
    {
        SEMA_PRODUCES("struct R;\n"
                      "\n"
                      "void foo(struct R* r) {\n"
                      "  struct R {\n"
                      "    int i;\n"
                      "    float f;\n"
                      "  };\n"
                      "  r->i += 5;\n"
                      "}",
                      ProducesError(STRUCT_N_IS_AN_INCOMPLETE_TYPE, "R"));
        SEMA_PRODUCES("void foo(struct R* r) {\n"
                      "  struct R {\n"
                      "    int i;\n"
                      "    float f;\n"
                      "  };\n"
                      "  r->i += 5;\n"
                      "}",
                      ProducesNoErrors());
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
                  ProducesNoErrors());
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(tu->getGlobals().size() == 3);
            std::uint64_t i = 1;
            for (auto& iter : tu->getGlobals())
            {
                auto* decl = iter->tryAs<VariableDeclaration>();
                REQUIRE(decl);
                REQUIRE(decl->getType().is<ArrayType>());
                auto& array = decl->getType().as<ArrayType>();
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(tu->getGlobals().size() == 3);
            std::uint64_t i = 5;
            for (auto& iter : tu->getGlobals())
            {
                auto* decl = iter->tryAs<VariableDeclaration>();
                REQUIRE(decl);
                REQUIRE(decl->getType().is<ArrayType>());
                auto& array = decl->getType().as<ArrayType>();
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(tu->getGlobals().size() == 3);
            std::uint64_t i = 1;
            for (auto& iter : tu->getGlobals())
            {
                auto* decl = iter->tryAs<VariableDeclaration>();
                REQUIRE(decl);
                REQUIRE(decl->getType().is<ArrayType>());
                auto& array = decl->getType().as<ArrayType>();
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
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(tu->getGlobals().size() == 3);
            std::uint64_t i = 5;
            for (auto& iter : tu->getGlobals())
            {
                auto* decl = iter->tryAs<VariableDeclaration>();
                REQUIRE(decl);
                REQUIRE(decl->getType().is<ArrayType>());
                auto& array = decl->getType().as<ArrayType>();
                CHECK(array.getSize() == i--);
            }
        }
        SEMA_PRODUCES("enum {\n"
                      "a = 1ull << 40,\n"
                      "};",
                      ProducesError(VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT));
    }
    SEMA_PRODUCES("typedef enum _STORAGE_PORT_CODE_SET {\n"
                  "  StoragePortCodeSetReserved   = 0,\n"
                  "  StoragePortCodeSetStorport   = 1,\n"
                  "  StoragePortCodeSetSCSIport   = 2 \n"
                  "} STORAGE_PORT_CODE_SET, *PSTORAGE_PORT_CODE_SET;",
                  ProducesNoErrors());
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
    SEMA_PRODUCES("static int bar(int n,int r[3][5][n]) {}", ProducesNoErrors());
    SECTION("Identifier list")
    {
        auto [translationUnit, errors] = generateSemantics("void test_inflate(compr,comprLen,uncompr,uncomprLen)\n"
                                                           "char* compr,*uncompr;\n"
                                                           "long comprLen,uncomprLen;\n"
                                                           "{}");
        REQUIRE(translationUnit->getGlobals().size() == 1);
        auto& first = translationUnit->getGlobals()[0];
        REQUIRE(first->is<FunctionDefinition>());
        auto& functionDefinition = first->as<FunctionDefinition>();
        REQUIRE(functionDefinition.getParameterDeclarations().size() == 4);
        CHECK(functionDefinition.getParameterDeclarations()[0]->getNameToken()->getText() == "compr");
        auto charType = PrimitiveType(PrimitiveType::Char, cld::LanguageOptions::native());
        CHECK(functionDefinition.getParameterDeclarations()[0]->getType() == PointerType(&charType));
        CHECK(functionDefinition.getParameterDeclarations()[1]->getNameToken()->getText() == "comprLen");
        CHECK(functionDefinition.getParameterDeclarations()[1]->getType()
              == PrimitiveType(PrimitiveType::Long, cld::LanguageOptions::native()));
        CHECK(functionDefinition.getParameterDeclarations()[2]->getNameToken()->getText() == "uncompr");
        CHECK(functionDefinition.getParameterDeclarations()[2]->getType() == PointerType(&charType));
        CHECK(functionDefinition.getParameterDeclarations()[3]->getNameToken()->getText() == "uncomprLen");
        CHECK(functionDefinition.getParameterDeclarations()[3]->getType()
              == PrimitiveType(PrimitiveType::Long, cld::LanguageOptions::native()));
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
                  ProducesNoErrors());
    SEMA_PRODUCES("struct A {\n"
                  " float f;\n"
                  "};\n"
                  "\n"
                  "int foo(a) struct A { int r; } a; {}",
                  ProducesNoErrors());
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
    SEMA_PRODUCES("static int i(void){}", ProducesWarning(UNUSED_FUNCTION_N, "'i'"));
    SEMA_PRODUCES("int i(void){}", !ProducesWarning(UNUSED_FUNCTION_N, "'i'"));
}

TEST_CASE("Semantics type compatibility", "[semantics]")
{
    SECTION("Qualifiers")
    {
        SEMA_PRODUCES("int foo;\n"
                      "int foo;",
                      ProducesNoErrors());
        SEMA_PRODUCES("const int foo;\n"
                      "const int foo;",
                      ProducesNoErrors());
        SEMA_PRODUCES("volatile int foo;\n"
                      "volatile int foo;",
                      ProducesNoErrors());
        SEMA_PRODUCES("const volatile int foo;\n"
                      "const int volatile foo;",
                      ProducesNoErrors());
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
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo[];\n"
                      "int foo[5];",
                      ProducesNoErrors());
        SEMA_PRODUCES("float foo[5];\n"
                      "int foo[5];",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo[4];\n"
                      "int foo[5];",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo[];\n"
                      "int foo[];",
                      ProducesNoErrors());
    }
    SECTION("Pointers")
    {
        SEMA_PRODUCES("int* foo;\n"
                      "int* foo;",
                      ProducesNoErrors());
        SEMA_PRODUCES("int* const foo;\n"
                      "int* const foo;",
                      ProducesNoErrors());
        SEMA_PRODUCES("int* volatile foo;\n"
                      "int* volatile foo;",
                      ProducesNoErrors());
        SEMA_PRODUCES("int* restrict foo;\n"
                      "int* restrict foo;",
                      ProducesNoErrors());
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
        SEMA_PRODUCES(
            "int inflate_table(int type,unsigned short* lens,unsigned codes,int** table,unsigned* bits,unsigned short* work);\n"
            "int foo(type,lens,codes,table,bits,work)\n"
            "int type;\n"
            "unsigned short* lens;\n"
            "unsigned codes;\n"
            "int**table;\n"
            "unsigned * bits;\n"
            "unsigned short *work;\n"
            "{}",
            ProducesNoErrors());
        SEMA_PRODUCES("int foo();\n"
                      "int foo();",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo();\n"
                      "float foo();",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo();\n"
                      "int foo(int a,...);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo();\n"
                      "int foo(int a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo();\n"
                      "int foo(float a);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo();\n"
                      "int foo(const int a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo();\n"
                      "int foo(const float a);",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(const float a);\n"
                      "int foo(a) const float a; {}",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(const double a);\n"
                      "int foo(a) const float a; {}",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(int a);\n"
                      "int foo() {}",
                      ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'"));
        SEMA_PRODUCES("int foo(void);\n"
                      "int foo() {}",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo();\n"
                      "int foo(a) int a; {}",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float a);\n"
                      "int foo(const float a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float a);\n"
                      "int foo(int a);",
                      ProducesError(ProducesError(REDEFINITION_OF_SYMBOL_N, "'foo'")));
        SEMA_PRODUCES("int foo(float a,...);\n"
                      "int foo(float a,...);",
                      ProducesNoErrors());
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
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float a[5]);\n"
                      "int foo(float a[]);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float a[const 5]);\n"
                      "int foo(float* const a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float a[restrict 5]);\n"
                      "int foo(float* restrict a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float a[volatile 5]);\n"
                      "int foo(float* volatile a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(const float a[static 5]);\n"
                      "int foo(const float* a);",
                      ProducesNoErrors());
        SEMA_PRODUCES("int foo(float (*a)(int));\n"
                      "int foo(float a(int));",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foor(int x[restrict 5]);\n"
                      "void foor(int[restrict 5]);",
                      ProducesNoErrors());
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
        REQUIRE_THAT(errors, ProducesNoErrors());
        REQUIRE(translationUnit->getGlobals().size() == 2);
        REQUIRE(translationUnit->getGlobals()[1]->is<FunctionDeclaration>());
        auto* decl = &translationUnit->getGlobals()[1]->as<FunctionDeclaration>();
        CHECK(decl->getNameToken()->getText() == "f");
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
        auto doubleType = PrimitiveType(PrimitiveType::Double, cld::LanguageOptions::native());
        auto charType = PrimitiveType(PrimitiveType::Char, cld::LanguageOptions::native());
        auto charStar = PointerType(&charType);
        auto function = FunctionType(&integer, {{&charStar, ""}});
        auto ptr1 = PointerType(&function);
        auto array = ArrayType(&doubleType, 3);
        auto ptr2 = PointerType(&array);
        CHECK(decl->getType() == FunctionType(&integer, {{&ptr1, ""}, {&ptr2, ""}}));
    }
}

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
const ExpressionBase& generateExpression(std::string source,
                                         cld::LanguageOptions options = cld::LanguageOptions::native())
{
    auto [translationUnit, errors] = generateSemantics(std::move(source), options);
    REQUIRE_THAT(errors, ProducesNoErrors());
    REQUIRE(translationUnit->getGlobals().back()->is<FunctionDefinition>());
    auto& funcDef = translationUnit->getGlobals().back()->as<FunctionDefinition>();
    REQUIRE(
        std::holds_alternative<cld::IntrVarPtr<Statement>>(funcDef.getCompoundStatement().getCompoundItems().back()));
    auto& statement = *cld::get<cld::IntrVarPtr<Statement>>(funcDef.getCompoundStatement().getCompoundItems().back());
    REQUIRE(statement.is<ExpressionStatement>());
    auto* expr = static_cast<ExpressionStatement&>(statement).getExpression();
    REQUIRE(expr);
    return *expr;
}
} // namespace

TEST_CASE("Semantics type printing", "[semantics]")
{
    using namespace cld::Semantics;
    auto toStr = [](const Type& type)
    {
        cld::CSourceObject object;
        return cld::diag::StringConverter<Type>::inArg(type, &object);
    };
    SECTION("Primitives")
    {
        CHECK(toStr(PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native())) == "void");
        CHECK(toStr(PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native(), flag::isConst = true))
              == "const void");
        CHECK(toStr(PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native(), flag::isVolatile = true))
              == "volatile void");
        CHECK(toStr(PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native(), flag::isConst = true,
                                  flag::isVolatile = true))
              == "const volatile void");
        CHECK(toStr(PrimitiveType(PrimitiveType::Char, cld::LanguageOptions::native())) == "char");
        CHECK(toStr(PrimitiveType(PrimitiveType::SignedChar, cld::LanguageOptions::native())) == "signed char");
        CHECK(toStr(PrimitiveType(PrimitiveType::UnsignedChar, cld::LanguageOptions::native())) == "unsigned char");
        CHECK(toStr(PrimitiveType(PrimitiveType::Short, cld::LanguageOptions::native())) == "short");
        CHECK(toStr(PrimitiveType(PrimitiveType::UnsignedShort, cld::LanguageOptions::native())) == "unsigned short");
        CHECK(toStr(PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native())) == "int");
        CHECK(toStr(PrimitiveType(PrimitiveType::UnsignedInt, cld::LanguageOptions::native())) == "unsigned int");
        CHECK(toStr(PrimitiveType(PrimitiveType::Long, cld::LanguageOptions::native())) == "long");
        CHECK(toStr(PrimitiveType(PrimitiveType::UnsignedLong, cld::LanguageOptions::native())) == "unsigned long");
        CHECK(toStr(PrimitiveType(PrimitiveType::LongLong, cld::LanguageOptions::native())) == "long long");
        CHECK(toStr(PrimitiveType(PrimitiveType::UnsignedLongLong, cld::LanguageOptions::native()))
              == "unsigned long long");
        CHECK(toStr(PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native())) == "float");
        CHECK(toStr(PrimitiveType(PrimitiveType::Double, cld::LanguageOptions::native())) == "double");
        CHECK(toStr(PrimitiveType(PrimitiveType::Bool, cld::LanguageOptions::native())) == "_Bool");
        CHECK(toStr(PrimitiveType(PrimitiveType::LongDouble, cld::LanguageOptions::native())) == "long double");
    }
    SECTION("Pointers")
    {
        {
            auto shortType = PrimitiveType(PrimitiveType::Short, cld::LanguageOptions::native());
            auto ptr1 = PointerType(&shortType, flag::isConst = true);
            auto ptr2 = PointerType(&ptr1);
            CHECK(toStr(PointerType(&ptr2)) == "short *const **");
        }
        auto charType = PrimitiveType(PrimitiveType::SignedChar, cld::LanguageOptions::native());
        auto arrayType = ArrayType(&charType, 5);
        CHECK(toStr(PointerType(&arrayType)) == "signed char(*)[5]");
        auto signedChar = PrimitiveType(PrimitiveType::SignedChar, cld::LanguageOptions::native());
        auto function = FunctionType(&signedChar,{});
        CHECK(toStr(PointerType(&function)) == "signed char(*)(void)");
    }
    SECTION("Array")
    {
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
        CHECK(toStr(ArrayType(&integer, 4)) == "int[4]");
        CHECK(toStr(ArrayType(&integer, 4, flag::isStatic = true)) == "int[static 4]");
        CHECK(toStr(ArrayType(&integer, 4, flag::isRestricted = true)) == "int[restrict 4]");
        CHECK(toStr(ArrayType(&integer, 4, flag::isVolatile = true)) == "int[volatile 4]");
        CHECK(toStr(ArrayType(&integer, 4, flag::isConst = true)) == "int[const 4]");
        auto array = ArrayType(&integer, 6);
        CHECK(toStr(ArrayType(&array, 4)) == "int[4][6]");
        auto charType = PrimitiveType(PrimitiveType::Char, cld::LanguageOptions::native());
        auto function = FunctionType(&integer, {{&charType, ""}});
        auto ptr = PointerType(&function);
        CHECK(toStr(ArrayType(&ptr, 5)) == "int(*[5])(char)");
    }
    SECTION("Functions")
    {
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
        CHECK(toStr(FunctionType(&integer,{})) == "int(void)");
        CHECK(toStr(FunctionType(&integer, {}, flag::isKAndR = true)) == "int()");
        CHECK(toStr(FunctionType(&integer, {{&integer, ""}}, flag::isVARArg = true)) == "int(int,...)");
        auto charType = PrimitiveType(PrimitiveType::Char, cld::LanguageOptions::native());
        {
            auto floating = PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native());
            auto function = FunctionType(&integer, {{&floating, ""}}, flag::isKAndR = true);
            auto ptr = PointerType(&function);
            CHECK(toStr(FunctionType(&ptr, {{&charType, ""}})) == "int(*(char))(float)");
        }
        auto charStar = PointerType(&charType);
        auto abstractArray = AbstractArrayType(&charStar);
        auto pointerType = PointerType(&abstractArray);
        auto function = FunctionType(&pointerType, {}, flag::isKAndR = true);
        auto functionPtr = PointerType(&function);
        auto functionPtrPtr = PointerType(&functionPtr);
        auto array = ArrayType(&functionPtrPtr, 8);
        CHECK(toStr(AbstractArrayType(&array)) == "char *(*(**[][8])())[]");
    }
    SECTION("Typedef")
    {
        SECTION("Normal")
        {
            auto [translationUnit, errors] = generateSemantics("typedef int INTEGER;\n"
                                                               "INTEGER i;\n");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(toStr(var.getType()) == "INTEGER");
        }
        SECTION("const typedef")
        {
            {
                auto [translationUnit, errors] = generateSemantics("typedef const int INTEGER;\n"
                                                                   "INTEGER i;\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(toStr(var.getType()) == "INTEGER");
            }
            {
                auto [translationUnit, errors] = generateSemantics("typedef const int INTEGER;\n"
                                                                   "const INTEGER i;\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(toStr(var.getType()) == "INTEGER");
            }
        }
        SECTION("const variable")
        {
            auto [translationUnit, errors] = generateSemantics("typedef int INTEGER;\n"
                                                               "const INTEGER i;\n");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(toStr(var.getType()) == "const INTEGER");
        }
        SECTION("volatile typedef")
        {
            {
                auto [translationUnit, errors] = generateSemantics("typedef volatile int INTEGER;\n"
                                                                   "INTEGER i;\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(toStr(var.getType()) == "INTEGER");
            }
            {
                auto [translationUnit, errors] = generateSemantics("typedef volatile int INTEGER;\n"
                                                                   "volatile INTEGER i;\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(toStr(var.getType()) == "INTEGER");
            }
        }
        SECTION("volatile variable")
        {
            auto [translationUnit, errors] = generateSemantics("typedef int INTEGER;\n"
                                                               "volatile INTEGER i;\n");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(toStr(var.getType()) == "volatile INTEGER");
        }
        SECTION("restrict typedef")
        {
            {
                auto [translationUnit, errors] = generateSemantics("typedef int* restrict INTEGER;\n"
                                                                   "INTEGER i;\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(toStr(var.getType()) == "INTEGER");
            }
            {
                auto [translationUnit, errors] = generateSemantics("typedef int* restrict INTEGER;\n"
                                                                   "restrict INTEGER i;\n");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
                CHECK(toStr(var.getType()) == "INTEGER");
            }
        }
        SECTION("restrict variable")
        {
            auto [translationUnit, errors] = generateSemantics("typedef int* INTEGER;\n"
                                                               "restrict INTEGER i;\n");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            auto& var = translationUnit->getGlobals()[0]->as<VariableDeclaration>();
            CHECK(toStr(var.getType()) == "restrict INTEGER");
        }
        SECTION("Loosing cv qualifiers")
        {
            auto& expr = generateExpression("typedef const int INTEGER;\n"
                                            "void foo(INTEGER i) {\n"
                                            "    +i;\n"
                                            "}\n");
            CHECK(toStr(expr.getType()) == "int");
        }
    }
}

TEST_CASE("Semantics primary expressions", "[semantics]")
{
    SECTION("Constant")
    {
        SECTION("Normal")
        {
            auto options = cld::LanguageOptions::native();
            auto constant = GENERATE_COPY(values<std::pair<std::string, cld::IntrVarValue<Type>>>({
                {"5", PrimitiveType(PrimitiveType::Int, options)},
                {"5u", PrimitiveType(PrimitiveType::UnsignedInt, options)},
                {"5l", PrimitiveType(PrimitiveType::Long, options)},
                {"5ul", PrimitiveType(PrimitiveType::UnsignedLong, options)},
                {"5LL", PrimitiveType(PrimitiveType::LongLong, options)},
                {"5uLL", PrimitiveType(PrimitiveType::UnsignedLongLong, options)},
                {"5.0f", PrimitiveType(PrimitiveType::Float, options)},
                {"5.0", PrimitiveType(PrimitiveType::Double, options)},
                {"5.0L", PrimitiveType(PrimitiveType::LongDouble, options)},
            }));
            auto& expr = generateExpression("void foo(void) { " + constant.first + ";}");
            CHECK(expr.getType() == *constant.second);
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            CHECK(expr.is<Constant>());
        }
        SECTION("String")
        {
            auto& expr = generateExpression("void foo(void) { \"txt\";}");
            auto charType = PrimitiveType(PrimitiveType::Char, cld::LanguageOptions::native());
            CHECK(expr.getType() == ArrayType(&charType, 4));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            CHECK(expr.is<Constant>());
        }
        SECTION("Wide String")
        {
            SECTION("Windows")
            {
                auto options = cld::LanguageOptions::fromTriple(x64windowsMsvc);
                auto& expr = generateExpression("void foo(void) { L\"txt\";}", options);
                auto wcharT = PrimitiveType(PrimitiveType::UnsignedShort, options);
                CHECK(expr.getType() == ArrayType(&wcharT, 4));
                CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
                CHECK(expr.is<Constant>());
            }
            SECTION("Unix")
            {
                auto options = cld::LanguageOptions::fromTriple(x64linux);
                auto& expr = generateExpression("void foo(void) { L\"txt\";}", options);
                auto wcharT = PrimitiveType(PrimitiveType::Int, options);
                CHECK(expr.getType() == ArrayType(&wcharT, 4));
                CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
                CHECK(expr.is<Constant>());
            }
        }
    }
    SECTION("Parentheses")
    {
        auto& expr = generateExpression("void foo(void) { (5);}");
        CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
        CHECK(expr.is<Constant>());
    }
    SECTION("Identifiers")
    {
        SECTION("Declarations")
        {
            auto& expr = generateExpression("void foo(void) { foo;}");
            auto voidType = PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native());
            CHECK(expr.getType() == FunctionType(&voidType,{}));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<DeclarationRead>());
            CHECK(expr.as<DeclarationRead>().getDeclRead().is<FunctionDefinition>());
        }
        SECTION("Enum constants")
        {
            auto& expr = generateExpression("enum A { VALUE = 7};void foo(void) { VALUE;}");
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(expr.is<Constant>());
        }
        SEMA_PRODUCES("void foo(void) { bar; }", ProducesError(UNDECLARED_IDENTIFIER_N, "'bar'"));
    }
}

TEST_CASE("Semantics postfix expressions", "[semantics]")
{
    SECTION("Subscript")
    {
        SECTION("Simple")
        {
            auto& expr = generateExpression("void foo(int *i) {\n"
                                            "    i[5];\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<SubscriptOperator>());
            auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
            CHECK(expr.as<SubscriptOperator>().getLeftExpression().getType() == PointerType(&integer));
            CHECK(expr.as<SubscriptOperator>().getLeftExpression().getValueCategory() == ValueCategory::Rvalue);
            CHECK(expr.as<SubscriptOperator>().getRightExpression().getType()
                  == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.as<SubscriptOperator>().getRightExpression().getValueCategory() == ValueCategory::Rvalue);
        }
        SECTION("CV qualified")
        {
            auto& expr = generateExpression("void foo(void) {\n"
                                            "   const int i[5];\n"
                                            "   i[3];\n"
                                            "}");
            CHECK(expr.getType()
                  == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native(), flag::isConst = true));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
        }
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
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            CHECK(expr.as<MemberAccess>().getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            CHECK(expr.as<MemberAccess>().getRecordExpression().getType().is<StructType>());
            CHECK_THAT(expr.as<MemberAccess>().getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
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
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            CHECK(expr.as<MemberAccess>().getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            CHECK(expr.as<MemberAccess>().getRecordExpression().getType().is<UnionType>());
            CHECK_THAT(expr.as<MemberAccess>().getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
        }
        SECTION("anonymous struct")
        {
            auto& expr = generateExpression("int foo(struct { int i; } i) {\n"
                                            " i.i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            CHECK(expr.as<MemberAccess>().getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            CHECK(expr.as<MemberAccess>().getRecordExpression().getType().is<StructType>());
            CHECK_THAT(expr.as<MemberAccess>().getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
        }
        SECTION("anonymous union")
        {
            auto& expr = generateExpression("int foo(union { int i; } i) {\n"
                                            " i.i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            CHECK(expr.as<MemberAccess>().getRecordExpression().getValueCategory() == ValueCategory::Lvalue);
            CHECK(expr.as<MemberAccess>().getRecordExpression().getType().is<UnionType>());
            CHECK_THAT(expr.as<MemberAccess>().getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
        }
        SEMA_PRODUCES("int foo(void) {\n"
                      " '5'.m;\n"
                      "}",
                      ProducesError(EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_DOT_OPERATOR));
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

            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            auto& mem = expr.as<MemberAccess>();
            REQUIRE(mem.getRecordExpression().getType().is<PointerType>());
            CHECK(mem.getRecordExpression().getType().as<PointerType>().getElementType().is<StructType>());
            CHECK_THAT(mem.getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
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

            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            auto& mem = expr.as<MemberAccess>();
            REQUIRE(mem.getRecordExpression().getType().is<PointerType>());
            CHECK(mem.getRecordExpression().getType().as<PointerType>().getElementType().is<UnionType>());
            CHECK_THAT(mem.getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
        }
        SECTION("anonymous struct")
        {
            auto& expr = generateExpression("int foo( struct { int i; }* i) {\n"
                                            " i->i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            auto& mem = expr.as<MemberAccess>();
            REQUIRE(mem.getRecordExpression().getType().is<PointerType>());
            CHECK(mem.getRecordExpression().getType().as<PointerType>().getElementType().is<StructType>());
            CHECK_THAT(mem.getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
        }
        SECTION("anonymous union")
        {
            auto& expr = generateExpression("int foo(union { int i; } *i) {\n"
                                            " i->i;\n"
                                            "}");

            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Lvalue);
            REQUIRE(expr.is<MemberAccess>());
            auto& mem = expr.as<MemberAccess>();
            REQUIRE(mem.getRecordExpression().getType().is<PointerType>());
            CHECK(mem.getRecordExpression().getType().as<PointerType>().getElementType().is<UnionType>());
            CHECK_THAT(mem.getField().indices, Catch::Equals(std::vector<std::uint64_t>{0}));
        }
        SEMA_PRODUCES("int foo(void) {\n"
                      " 5->m;\n"
                      "}",
                      ProducesError(EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_ARROW_OPERATOR));
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
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(expr.is<UnaryOperator>());
            auto& unary = expr.as<UnaryOperator>();
            CHECK(unary.getKind() == UnaryOperator::PostIncrement);
        }
        SECTION("Decrement")
        {
            auto& expr = generateExpression("int foo(volatile int i) {\n"
                                            " i--;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(expr.is<UnaryOperator>());
            auto& unary = expr.as<UnaryOperator>();
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
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(expr.is<UnaryOperator>());
            auto& unary = expr.as<UnaryOperator>();
            CHECK(unary.getKind() == UnaryOperator::PreIncrement);
        }
        SECTION("Decrement")
        {
            auto& expr = generateExpression("int foo(volatile int i) {\n"
                                            " --i;\n"
                                            "}");
            CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            CHECK(expr.getValueCategory() == ValueCategory::Rvalue);
            REQUIRE(expr.is<UnaryOperator>());
            auto& unary = expr.as<UnaryOperator>();
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
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
        SECTION("Following dereference")
        {
            auto& exp = generateExpression("void foo(int *i) {\n"
                                           " &*i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PointerType(&integer));
        }
        SECTION("Following subscript")
        {
            auto& exp = generateExpression("void foo(int *i) {\n"
                                           " &i[0];\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PointerType(&integer));
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        REQUIRE(exp.is<UnaryOperator>());
        CHECK(exp.as<UnaryOperator>().getKind() == UnaryOperator::Dereference);
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
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<UnaryOperator>());
            CHECK(exp.as<UnaryOperator>().getKind() == UnaryOperator::Minus);
        }
        SECTION("+")
        {
            auto& exp = generateExpression("void foo(short i) {\n"
                                           "+i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<UnaryOperator>());
            CHECK(exp.as<UnaryOperator>().getKind() == UnaryOperator::Plus);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        REQUIRE(exp.is<UnaryOperator>());
        CHECK(exp.as<UnaryOperator>().getKind() == UnaryOperator::BitwiseNegate);
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
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<UnaryOperator>());
            CHECK(exp.as<UnaryOperator>().getKind() == UnaryOperator::BooleanNegate);
        }
        SECTION("Pointer")
        {
            auto& exp = generateExpression("void foo(int* i) {\n"
                                           "!i;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<UnaryOperator>());
            CHECK(exp.as<UnaryOperator>().getKind() == UnaryOperator::BooleanNegate);
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
        CHECK(exp.getType() == PrimitiveType(cld::LanguageOptions::native().sizeTType, cld::LanguageOptions::native()));
        REQUIRE(exp.is<SizeofOperator>());
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
    SECTION("Simple")
    {
        auto& exp = generateExpression("void foo(int* const i) {\n"
                                       "(const int* const)i;\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native(), flag::isConst = true);
        CHECK(exp.getType() == PointerType(&integer));
        CHECK(exp.is<Cast>());
    }
    SEMA_PRODUCES("void foo(int* i) {\n"
                  " (void)i;\n"
                  "}",
                  ProducesNoErrors());
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        REQUIRE(exp.is<BinaryOperator>());
        auto& binOp = exp.as<BinaryOperator>();
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        REQUIRE(exp.is<BinaryOperator>());
        auto& binOp = exp.as<BinaryOperator>();
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
        SECTION("Simple")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "5 % 5;\n"
                                           "}");
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<BinaryOperator>());
            auto& binOp = exp.as<BinaryOperator>();
            CHECK(binOp.getKind() == BinaryOperator::Modulo);
        }
        SECTION("Signed with unsigned")
        {
            auto option = cld::LanguageOptions::fromTriple(x64windowsGnu);
            auto& exp = generateExpression("void foo(void) {\n"
                                           "5l % 5u;\n"
                                           "}",
                                           option);
            CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::UnsignedInt, option));
            REQUIRE(exp.is<BinaryOperator>());
            auto& binOp = exp.as<BinaryOperator>();
            CHECK(binOp.getKind() == BinaryOperator::Modulo);
        }
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
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<BinaryOperator>());
            auto& binOp = exp.as<BinaryOperator>();
            CHECK(binOp.getKind() == BinaryOperator::Addition);
        }
        SECTION("Pointer")
        {
            auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
            SECTION("Pointer left")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "i + 5;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType() == PointerType(&integer));
                REQUIRE(exp.is<BinaryOperator>());
                auto& binOp = exp.as<BinaryOperator>();
                CHECK(binOp.getKind() == BinaryOperator::Addition);
            }
            SECTION("Pointer right")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "5 + i;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType() == PointerType(&integer));
                REQUIRE(exp.is<BinaryOperator>());
                auto& binOp = exp.as<BinaryOperator>();
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
            CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
            REQUIRE(exp.is<BinaryOperator>());
            auto& binOp = exp.as<BinaryOperator>();
            CHECK(binOp.getKind() == BinaryOperator::Subtraction);
        }
        SECTION("Pointer")
        {
            SECTION("Pointer and int")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "i - 5;\n"
                                               "}");
                auto integer = PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native());
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType() == PointerType(&integer));
                REQUIRE(exp.is<BinaryOperator>());
                auto& binOp = exp.as<BinaryOperator>();
                CHECK(binOp.getKind() == BinaryOperator::Subtraction);
            }
            SECTION("Pointer and Pointer")
            {
                auto& exp = generateExpression("void foo(int* i) {\n"
                                               "i - i;\n"
                                               "}");
                CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
                CHECK(exp.getType()
                      == PrimitiveType(cld::LanguageOptions::native().ptrdiffType, cld::LanguageOptions::native()));
                REQUIRE(exp.is<BinaryOperator>());
                auto& binOp = exp.as<BinaryOperator>();
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::UnsignedLongLong, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::LeftShift);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::UnsignedLongLong, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::RightShift);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::LessThan);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::GreaterThan);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::LessOrEqual);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::GreaterOrEqual);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::Equal);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::NotEqual);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::UnsignedLongLong, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::BitAnd);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::UnsignedLongLong, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::BitOr);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::UnsignedLongLong, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::BitXor);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::LogicAnd);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<BinaryOperator>());
        CHECK(exp.as<BinaryOperator>().getKind() == BinaryOperator::LogicOr);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Double, cld::LanguageOptions::native()));
    }
    SECTION("Void")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "1 ? *(void const*)5 : *(void*)3uLL;\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native()));
    }
    SECTION("Pointer")
    {
        SECTION("Void")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "1 ? (void const*)5 : (float*)3uLL;\n"
                                           "}");
            auto voidType = PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native(), flag::isConst = true);
            CHECK(exp.getType() == PointerType(&voidType));
        }
        SECTION("Merging void")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "1 ? (void const*)5 : (void volatile*)3uLL;\n"
                                           "}");
            auto voidType = PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native(), flag::isConst = true,
                                          flag::isVolatile = true);
            CHECK(exp.getType() == PointerType(&voidType));
        }
        SECTION("Composite type")
        {
            auto& exp = generateExpression("void foo(void (*f)(float[]),void (*r)(float[5])) {\n"
                                           "1 ? f : r;\n"
                                           "}");
            auto floating = PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native());
            auto array = ArrayType(&floating, 5);
            auto voidType = PrimitiveType(PrimitiveType::Void, cld::LanguageOptions::native());
            auto function = FunctionType(&voidType, {{&array, ""}});
            CHECK(exp.getType() == PointerType(&function));
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<Assignment>());
        CHECK(exp.as<Assignment>().getKind() == Assignment::Simple);
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<Assignment>());
        CHECK(exp.as<Assignment>().getKind() == (operand == "-=" ? Assignment::Minus : Assignment::Plus));
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<Assignment>());
        CHECK(exp.as<Assignment>().getKind() == (operand == "*=" ? Assignment::Multiply : Assignment::Divide));
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<Assignment>());
        CHECK(exp.as<Assignment>().getKind() ==
              [&operand]
              {
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
    CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
    CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
    CHECK(exp.is<CommaExpression>());
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
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Double, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<CallExpression>());
        auto& call = exp.as<CallExpression>();
        REQUIRE(call.getArgumentExpressions().size() == 2);
        REQUIRE(call.getArgumentExpressions()[0]->is<Conversion>());
        CHECK(call.getArgumentExpressions()[0]->as<Conversion>().getKind() == Conversion::DefaultArgumentPromotion);
        CHECK(call.getArgumentExpressions()[0]->getType()
              == PrimitiveType(PrimitiveType::Double, cld::LanguageOptions::native()));
        REQUIRE(call.getArgumentExpressions()[1]->is<Conversion>());
        CHECK(call.getArgumentExpressions()[1]->as<Conversion>().getKind() == Conversion::IntegerPromotion);
        CHECK(call.getArgumentExpressions()[1]->getType()
              == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
    }
    SECTION("Prototyped")
    {
        auto& exp = generateExpression("double bar(float,unsigned short);\n"
                                       "\n"
                                       "void foo(unsigned short i) {\n"
                                       "bar(5.0f,i);\n"
                                       "}");
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Double, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Rvalue);
        REQUIRE(exp.is<CallExpression>());
        auto& call = exp.as<CallExpression>();
        REQUIRE(call.getArgumentExpressions().size() == 2);
        CHECK(call.getArgumentExpressions()[0]->getType()
              == PrimitiveType(PrimitiveType::Float, cld::LanguageOptions::native()));
        CHECK(call.getArgumentExpressions()[1]->getType()
              == PrimitiveType(PrimitiveType::UnsignedShort, cld::LanguageOptions::native()));
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
            REQUIRE(translationUnit->getGlobals().size() == 1);
            auto& global = translationUnit->getGlobals()[0];
            REQUIRE(global->is<VariableDeclaration>());
            auto& declaration = global->as<VariableDeclaration>();
            REQUIRE(declaration.getType().is<ArrayType>());
            CHECK(declaration.getType().as<ArrayType>().getSize() == 7);
        }
        SECTION("Wide string")
        {
            SECTION("Windows")
            {
                auto [translationUnit, errors] =
                    generateSemantics("unsigned short foo[] = L\"string\";", x64windowsGnu);
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& global = translationUnit->getGlobals()[0];
                REQUIRE(global->is<VariableDeclaration>());
                auto& declaration = global->as<VariableDeclaration>();
                REQUIRE(declaration.getType().is<ArrayType>());
                CHECK(declaration.getType().as<ArrayType>().getSize() == 7);
            }
            SECTION("Posix")
            {
                auto [translationUnit, errors] = generateSemantics("int foo[] = L\"string\";", x64linux);
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& global = translationUnit->getGlobals()[0];
                REQUIRE(global->is<VariableDeclaration>());
                auto& declaration = global->as<VariableDeclaration>();
                REQUIRE(declaration.getType().is<ArrayType>());
                CHECK(declaration.getType().as<ArrayType>().getSize() == 7);
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
        REQUIRE(translationUnit->getGlobals().size() == 1);
        auto& global = translationUnit->getGlobals()[0];
        REQUIRE(global->is<VariableDeclaration>());
        auto* decl = &global->as<VariableDeclaration>();
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
                      ProducesNoErrors());
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
        SEMA_PRODUCES("const char* c[] = {\"test\",\"a\"};", ProducesNoErrors());
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
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& global = translationUnit->getGlobals()[0];
                REQUIRE(global->is<VariableDeclaration>());
                auto& declaration = global->as<VariableDeclaration>();
                REQUIRE(declaration.getType().is<ArrayType>());
                CHECK(declaration.getType().as<ArrayType>().getSize() == 3);
            }
            SECTION("Designators")
            {
                using Catch::Matchers::Equals;
                auto [translationUnit, errors] =
                    generateSemantics("struct Point {\n"
                                      " float x,y;\n"
                                      "} points[] = {[10].x = 5,[10].y = 3,[0] = {3.0,2.0},3.0,3.0,23.025,23.024};");
                REQUIRE_THAT(errors, ProducesNoErrors());
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& global = translationUnit->getGlobals()[0];
                REQUIRE(global->is<VariableDeclaration>());
                auto& declaration = global->as<VariableDeclaration>();
                REQUIRE(declaration.getType().is<ArrayType>());
                CHECK(declaration.getType().as<ArrayType>().getSize() == 11);
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
                REQUIRE(translationUnit->getGlobals().size() == 1);
                auto& global = translationUnit->getGlobals()[0];
                REQUIRE(global->is<VariableDeclaration>());
                auto& declaration = global->as<VariableDeclaration>();
                REQUIRE(declaration.getType().is<ArrayType>());
                CHECK(declaration.getType().as<ArrayType>().getSize() == 4);
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
        CHECK(exp.is<CompoundLiteral>());
        CHECK(exp.getType().is<StructType>());
    }
    SECTION("Size deduction")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "(float[]){3,3.53f};\n"
                                       "}");
        CHECK(exp.getValueCategory() == ValueCategory::Lvalue);
        CHECK(exp.is<CompoundLiteral>());
        REQUIRE(exp.getType().is<ArrayType>());
        CHECK(exp.getType().as<ArrayType>().getSize() == 2);
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
    REQUIRE(exp.is<DeclarationRead>());
    auto& declarationRead = exp.as<DeclarationRead>();
    REQUIRE(declarationRead.getDeclRead().is<VariableDeclaration>());
    auto& decl = declarationRead.getDeclRead().as<VariableDeclaration>();
    REQUIRE(decl.getInitializer());
    REQUIRE(std::holds_alternative<cld::IntrVarPtr<ExpressionBase>>(*decl.getInitializer()));
    REQUIRE(cld::get<cld::IntrVarPtr<ExpressionBase>>(*decl.getInitializer())->is<Constant>());
    auto& constant = cld::get<cld::IntrVarPtr<ExpressionBase>>(*decl.getInitializer())->as<Constant>();
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

TEST_CASE("Semantics varargs", "[semantics]")
{
    auto triple = GENERATE_REF(values({cld::Tests::x64windowsGnu, cld::Tests::x64linux}));
    SECTION("__builtin_va_start")
    {
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list);\n"
                           "}",
                           ProducesError(NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N, 2, 1),
                           triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list,3,5);\n"
                           "}",
                           ProducesError(TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N, 2, 3),
                           triple);
        SEMA_PRODUCES_WITH("void foo(void) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list,5);\n"
                           "}",
                           ProducesError(CANNOT_USE_VA_START_IN_A_FUNCTION_WITH_FIXED_ARGUMENT_COUNT), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_start(5,3);\n"
                           "}",
                           ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST, 1), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list,5);\n"
                           "}",
                           ProducesWarning(SECOND_ARGUMENT_OF_VA_START_SHOULD_BE_THE_LAST_PARAMETER), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list,i);\n"
                           "}",
                           ProducesNoErrors(), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "const __builtin_va_list list;\n"
                           "__builtin_va_start(list,i);\n"
                           "}",
                           ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST, 1), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "void (*f)(int,...) = __builtin_va_start;\n"
                           "}",
                           ProducesError(BUILTIN_FUNCTION_MAY_ONLY_BE_CALLED_DIRECTLY), triple);
    }
    SECTION("__builtin_va_arg")
    {
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_arg(5,int);\n"
                           "}",
                           ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST, 1), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list,i);\n"
                           "__builtin_va_arg(list,int);\n"
                           "}",
                           ProducesNoErrors(), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "const __builtin_va_list list;\n"
                           "__builtin_va_arg(list,int);\n"
                           "}",
                           ProducesError(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST, 1), triple);
        SEMA_PRODUCES_WITH("void foo(int i,...) {\n"
                           "__builtin_va_list list;\n"
                           "__builtin_va_start(list,i);\n"
                           "__builtin_va_arg(list,struct I);\n"
                           "}",
                           ProducesError(INCOMPLETE_TYPE_N_IN_VA_ARG, "'struct I'"), triple);
    }
}

TEST_CASE("Semantics inline functions", "[semantics]")
{
    SECTION("inline main")
    {
        auto options = cld::LanguageOptions::native();
        options.freeStanding = true;
        CHECK_THAT(generateSemantics("inline int main(void) {}", options).second, ProducesNoErrors());
        options.freeStanding = false;
        CHECK_THAT(generateSemantics("inline int main(void);", options).second,
                   ProducesError(INLINE_MAIN_IS_NOT_ALLOWED_IN_A_HOSTED_ENVIRONMENT));
        CHECK_THAT(generateSemantics("inline int main(void) {}", options).second,
                   ProducesError(INLINE_MAIN_IS_NOT_ALLOWED_IN_A_HOSTED_ENVIRONMENT));
    }
    SECTION("References")
    {
        SEMA_PRODUCES(
            "inline void foo(void) {\n"
            "static int i;\n"
            "}",
            ProducesError(
                INLINE_FUNCTION_N_WITH_EXTERNAL_LINKAGE_IS_NOT_ALLOWED_TO_CONTAIN_OR_ACCESS_THE_INTERNAL_IDENTIFIER_N,
                "'foo'", "'i'"));
        SEMA_PRODUCES("inline void foo(void) {\n"
                      "static const int i = 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("static inline void foo(void) {\n"
                      "static int i = 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES(
            "static int i = 0;\n"
            "\n"
            "inline void foo(void) {\n"
            "i;\n"
            "}",
            ProducesError(
                INLINE_FUNCTION_N_WITH_EXTERNAL_LINKAGE_IS_NOT_ALLOWED_TO_CONTAIN_OR_ACCESS_THE_INTERNAL_IDENTIFIER_N,
                "'foo'", "'i'"));
        SEMA_PRODUCES(
            "static int i(void);\n"
            "\n"
            "inline void foo(void) {\n"
            "i;\n"
            "}",
            ProducesError(
                INLINE_FUNCTION_N_WITH_EXTERNAL_LINKAGE_IS_NOT_ALLOWED_TO_CONTAIN_OR_ACCESS_THE_INTERNAL_IDENTIFIER_N,
                "'foo'", "'i'"));
        SEMA_PRODUCES("static int i = 0;\n"
                      "\n"
                      "static inline void foo(void) {\n"
                      "i;\n"
                      "}",
                      ProducesNoErrors());
    }
    SECTION("External linkage inline function")
    {
        SEMA_PRODUCES("inline void foo(void);", ProducesError(NO_DEFINITION_FOR_INLINE_FUNCTION_N_FOUND, "'foo'"));
        SEMA_PRODUCES("static inline void foo(void);", ProducesNoErrors());
    }
}

TEST_CASE("Semantics anonymous inline structs and unions", "[semantics]")
{
    SECTION("Initialization")
    {
        SEMA_PRODUCES("struct __pthread_cond_s {\n"
                      "    __extension__ union {\n"
                      "        unsigned long long int __wsed;\n"
                      "        struct {\n"
                      "            unsigned int __low;\n"
                      "            unsigned int __high;\n"
                      "        };\n"
                      "    };\n"
                      "} s = {.__low = 5,.__high = 3};\n"
                      "\n",
                      ProducesNoErrors());
        SEMA_PRODUCES("__extension__ struct R {\n"
                      "    int f;\n"
                      "    struct {\n"
                      "        float t;\n"
                      "        int r;\n"
                      "    };\n"
                      "};\n"
                      "\n"
                      "struct R r = {.t = 5};\n",
                      ProducesNoErrors());
        SEMA_PRODUCES("__extension__ struct R {\n"
                      "    struct {\n"
                      "        float t;\n"
                      "        int r;\n"
                      "    };\n"
                      "    int f;\n"
                      "};\n"
                      "\n"
                      "struct R r = {{3.0,5,3}};\n",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
    }
    SEMA_PRODUCES("struct __pthread_cond_s {\n"
                  "    __extension__ union {\n"
                  "        unsigned long long int __wsed;\n"
                  "        struct {\n"
                  "            unsigned int __low;\n"
                  "            unsigned int __high;\n"
                  "        };\n"
                  "    };\n"
                  "};\n"
                  "\n"
                  "unsigned int* function(struct __pthread_cond_s* c) {\n"
                  "   return &c->__low;\n"
                  "}\n",
                  ProducesNoErrors());
    SEMA_PRODUCES("struct __pthread_cond_s\n"
                  "{\n"
                  "  __extension__ union\n"
                  "  {\n"
                  "    __extension__ unsigned long long int __wseq;\n"
                  "    struct\n"
                  "    {\n"
                  "      unsigned int __low;\n"
                  "      unsigned int __high;\n"
                  "    } __wseq32;\n"
                  "  };\n"
                  "  __extension__ union\n"
                  "  {\n"
                  "    __extension__ unsigned long long int __g1_start;\n"
                  "    struct\n"
                  "    {\n"
                  "      unsigned int __low;\n"
                  "      unsigned int __high;\n"
                  "    } __g1_start32;\n"
                  "  };\n"
                  "  unsigned int __g_refs[2];\n"
                  "  unsigned int __g_size[2];\n"
                  "  unsigned int __g1_orig_size;\n"
                  "  unsigned int __wrefs;\n"
                  "  unsigned int __g_signals[2];\n"
                  "};",
                  ProducesNoErrors());
    SEMA_PRODUCES("struct __pthread_cond_s {\n"
                  "    union {\n"
                  "        unsigned long long int __wsed;\n"
                  "        struct {\n"
                  "            unsigned int __low;\n"
                  "            unsigned int __high;\n"
                  "        } __wseq32;\n"
                  "    };\n"
                  "};",
                  ProducesError(FIELD_WITHOUT_A_NAME_IS_NOT_ALLOWED));
    SEMA_PRODUCES("__extension__ struct R {\n"
                  "    int i;\n"
                  "    union {\n"
                  "        float i;\n"
                  "        int f;\n"
                  "    };\n"
                  "};",
                  ProducesError(REDEFINITION_OF_FIELD_N, "'i'"));
}

TEST_CASE("Semantics flexible array member", "[semantics]")
{
    SECTION("Layout and offset")
    {
        std::string_view source = "struct A\n"
                                  "{\n"
                                  "    char c;\n"
                                  "    int f[];\n"
                                  "};";
        bool errors = false;
        auto options = cld::LanguageOptions::fromTriple(x64windowsMsvc);
        auto tokens = cld::Lexer::tokenize(cld::to_string(source), &options, &llvm::errs(), &errors);
        REQUIRE_FALSE(errors);
        auto ctokens = cld::Lexer::toCTokens(tokens, &llvm::errs(), &errors);
        REQUIRE_FALSE(errors);
        auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
        REQUIRE_FALSE(errors);
        auto program = analyse(tree, std::move(ctokens), &llvm::errs(), &errors);
        REQUIRE_FALSE(errors);
        auto* structInfo = program.lookupType<StructInfo>("A", ProgramInterface::GLOBAL_SCOPE);
        REQUIRE(structInfo);
        auto* structDef = std::get_if<StructDefinition>(&structInfo->type);
        REQUIRE(structDef);
        REQUIRE(structDef->getFields().size() == 2);
        REQUIRE(structDef->getFieldLayout().size() == 2);
        REQUIRE(structDef->getMemLayout().size() == 2);
        CHECK(structDef->getMemLayout()[0].offset == 0);
        CHECK(structDef->getMemLayout()[1].offset == 4);
    }
    SEMA_PRODUCES("struct A{ int f[]; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "'int[]'"));
    SEMA_PRODUCES("struct A{ int r;int f[],c; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "'int[]'"));
    SEMA_PRODUCES("struct A{ int r,f[]; };", ProducesNoErrors());
    SEMA_PRODUCES("union A{ int r,f[]; };", ProducesError(INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION, "'int[]'"));
    SEMA_PRODUCES("struct A{ int r;int f[]; };", ProducesNoErrors());
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
                  ProducesNoErrors());
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

TEST_CASE("Semantics offsetof", "[semantics]")
{
    auto& expr = generateExpression(
        "\n"
        "typedef unsigned short LogEst;\n"
        "\n"
        "typedef unsigned long long Bitmask;\n"
        "\n"
        "struct WhereInfo\n"
        "{\n"
        "    int* pParse;\n"
        "    int* pTabList;\n"
        "    int* pOrderBy;\n"
        "    int* pResultSet;\n"
        "    int* pWhere;\n"
        "    int aiCurOnePass[2];\n"
        "    int iContinue;\n"
        "    int iBreak;\n"
        "    int savedNQueryLoop;\n"
        "    unsigned short wctrlFlags;\n"
        "    LogEst iLimit;\n"
        "    unsigned char nLevel;\n"
        "    char nOBSat;\n"
        "    unsigned char eOnePass;\n"
        "    unsigned char eDistinct;\n"
        "    unsigned bDeferredSeek:1;\n"
        "    unsigned untestedTerms:1;\n"
        "    unsigned bOrderedInnerLoops:1;\n"
        "    unsigned sorted:1;\n"
        "    LogEst nRowOut;\n"
        "    int iTop;\n"
        "    int* pLoops;\n"
        "    int* pExprMods;\n"
        "    Bitmask revMask;\n"
        "    int f;\n"
        "};\n"
        "\n"
        "int main(void)\n"
        "{\n"
        "    __builtin_offsetof(struct WhereInfo,f);// - __builtin_offsetof(struct WhereInfo,nOBSat);\n"
        "}",
        cld::LanguageOptions::fromTriple(x64linux));
    REQUIRE(expr.is<BuiltinOffsetOf>());
    CHECK(expr.as<BuiltinOffsetOf>().getOffset() == 104);
    SEMA_PRODUCES("struct T {\n"
                  "int i;\n"
                  "int f : 3;\n"
                  "};\n"
                  "\n"
                  "int main(void) { \n"
                  "__builtin_offsetof(struct T,f);\n"
                  "}",
                  ProducesError(BITFIELD_NOT_ALLOWED_IN_OFFSET_OF));
    SEMA_PRODUCES("int main(void) { \n"
                  "__builtin_offsetof(int,f);\n"
                  "}",
                  ProducesError(TYPE_N_IN_OFFSETOF_MUST_BE_A_STRUCT_OR_UNION_TYPE, "'int'"));
    SEMA_PRODUCES("struct T {\n"
                  "int i;\n"
                  "int f : 3;\n"
                  "};\n"
                  "\n"
                  "int main(void) { \n"
                  "__builtin_offsetof(struct T,i.m);\n"
                  "}",
                  ProducesError(EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_DOT_OPERATOR_2));
    SEMA_PRODUCES("struct T {\n"
                  "int* i;\n"
                  "int f : 3;\n"
                  "};\n"
                  "\n"
                  "int main(void) { \n"
                  "__builtin_offsetof(struct T,i[0]);\n"
                  "}",
                  ProducesError(EXPECTED_ARRAY_TYPE_ON_THE_LEFT_SIDE_OF_THE_SUBSCRIPT_OPERATOR));
}

TEST_CASE("Semantics __sync_*", "[semantics]")
{
    auto& expr = generateExpression("void function(void) {\n"
                                    "int i = 5;\n"
                                    "__sync_fetch_and_add(&i,3);\n"
                                    "}");
    CHECK(expr.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
    SEMA_PRODUCES("void function(void) {\n"
                  "int i = 5;\n"
                  "int f = __sync_fetch_and_add(&i,3);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES(
        "void function(void) {\n"
        "int i[3] = {5};\n"
        "__sync_fetch_and_add(&i,5);\n"
        "}",
        ProducesError(POINTER_ELEMENT_TYPE_IN_N_MUST_BE_AN_INTEGER_OR_POINTER_TYPe, "'__sync_fetch_and_add'"));
    SEMA_PRODUCES(
        "void function(void) {\n"
        "float i = 5;\n"
        "__sync_fetch_and_add(&i,5);\n"
        "}",
        ProducesError(POINTER_ELEMENT_TYPE_IN_N_MUST_BE_AN_INTEGER_OR_POINTER_TYPe, "'__sync_fetch_and_add'"));
    SEMA_PRODUCES("void function(void) {\n"
                  "int* i = 0,p;\n"
                  "__sync_fetch_and_add(&i,&p);\n"
                  "}",
                  ProducesNoErrors());
    SEMA_PRODUCES("void function(void) {\n"
                  "__sync_fetch_and_add(5,5);\n"
                  "}",
                  ProducesError(EXPECTED_POINTER_TYPE_AS_FIRST_ARGUMENT_TO_N, "'__sync_fetch_and_add'"));
    SEMA_PRODUCES("void function(void) {\n"
                  "const int i = 5;\n"
                  "__sync_fetch_and_add(&i,5);\n"
                  "}",
                  ProducesError(POINTER_ELEMENT_TYPE_IN_N_MAY_NOT_BE_CONST_QUALIFIED, "'__sync_fetch_and_add'"));
    SEMA_PRODUCES("void function(void) {\n"
                  "_Bool i = 1;\n"
                  "__sync_fetch_and_add(&i,5);\n"
                  "}",
                  ProducesError(POINTER_ELEMENT_TYPE_IN_N_MUST_NOT_BE_BOOL, "'__sync_fetch_and_add'"));
    SEMA_PRODUCES_WITH(
        "void function(void) {\n"
        "__int128 i = 1;\n"
        "__sync_fetch_and_add(&i,5);\n"
        "}",
        ProducesError(POINTER_ELEMENT_TYPE_IN_N_MUST_NOT_HAVE_A_SIZE_GREATER_THAN_8, "'__sync_fetch_and_add'"),
        x64linux);
}

TEST_CASE("Semantics __attribute__", "[semantics]")
{
    SEMA_PRODUCES("int i __attribute__((thisAttributeShouldIdeallyNeverExit));",
                  ProducesWarning(UNKNOWN_ATTRIBUTE_N_IGNORED, "'thisAttributeShouldIdeallyNeverExit'"));
    SECTION("Attribute positioning")
    {
        SECTION("Function return type")
        {
            auto [translationUnit, errors] =
                generateSemantics("static int __attribute__((vector_size(8),used)) foo(void);");
            REQUIRE_THAT(errors, ProducesNoErrors());
            REQUIRE(translationUnit->getGlobals().size() == 1);
            REQUIRE(translationUnit->getGlobals()[0]->is<FunctionDeclaration>());
            auto* decl = &translationUnit->getGlobals()[0]->as<FunctionDeclaration>();
            CHECK(decl->hasAttribute<UsedAttribute>());
            CHECK(decl->getType().as<FunctionType>().getReturnType().is<VectorType>());
            SEMA_PRODUCES("static __attribute__((used)) int foo(void);",
                          ProducesNoErrors() && !ProducesWarning(UNUSED_FUNCTION_N, "'foo'"));
        }
        SECTION("Function parameter")
        {
            SEMA_PRODUCES("static int foo(int i __attribute__((used))) {\n"
                          "\n"
                          "}",
                          ProducesWarning(ATTRIBUTE_USED_ONLY_APPLIES_TO_GLOBAL_VARIABLES_WITH_INTERNAL_LINKAGE)
                              && ProducesWarning(UNUSED_FUNCTION_N, "'foo'"));
        }
    }
}

TEST_CASE("Semantics __attribute__((used))", "[semantics]")
{
    SEMA_PRODUCES("typedef int i __attribute__((used));",
                  ProducesWarning(ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES, "'used'"));
    SEMA_PRODUCES("static int i __attribute__((used));",
                  ProducesNoErrors() && !ProducesWarning(UNUSED_VARIABLE_N, "'i'"));
    SEMA_PRODUCES("static int i __attribute__((used(name)));",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N, "'used'", "0", "1"));
    SEMA_PRODUCES("static int i __attribute__((used()));",
                  ProducesNoErrors() && !ProducesWarning(UNUSED_VARIABLE_N, "'i'"));
    SEMA_PRODUCES("static int foo(void) __attribute__((used));",
                  ProducesNoErrors() && !ProducesWarning(UNUSED_FUNCTION_N, "'foo'"));
    SEMA_PRODUCES("static int foo(void) __attribute__((used)){}",
                  ProducesNoErrors() && !ProducesWarning(UNUSED_FUNCTION_N, "'foo'"));
    SEMA_PRODUCES("int i __attribute__((used));",
                  ProducesWarning(ATTRIBUTE_USED_ONLY_APPLIES_TO_GLOBAL_VARIABLES_WITH_INTERNAL_LINKAGE));
    SEMA_PRODUCES("int foo(void) { int i __attribute__((used)); }",
                  ProducesWarning(ATTRIBUTE_USED_ONLY_APPLIES_TO_GLOBAL_VARIABLES_WITH_INTERNAL_LINKAGE));
    SEMA_PRODUCES("int foo(void) __attribute__((used));",
                  ProducesWarning(ATTRIBUTE_USED_ONLY_APPLIES_TO_FUNCTIONS_WITH_INTERNAL_LINKAGE));
    SEMA_PRODUCES("int foo(void) __attribute__((used)){}",
                  ProducesWarning(ATTRIBUTE_USED_ONLY_APPLIES_TO_FUNCTIONS_WITH_INTERNAL_LINKAGE));
}

TEST_CASE("Semantics __attribute__((vector_size(x)))", "[semantics]")
{
    SEMA_PRODUCES("typedef int i __attribute__((vector_size));",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N, "'vector_size'", 1, 0));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(3,5)));",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N, "'vector_size'", 1, 2));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(x)));", ProducesError(UNDECLARED_IDENTIFIER_N, "'x'"));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(3.5)));",
                  ProducesError(EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_ARGUMENT_TO_VECTOR_SIZE));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(-3)));",
                  ProducesError(ARGUMENT_TO_VECTOR_SIZE_MUST_BE_A_POSITIVE_NUMBER));
    SEMA_PRODUCES("typedef int* i __attribute__((vector_size(3)));",
                  ProducesError(VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_ARITHMETIC_TYPES));
    SEMA_PRODUCES("int* i __attribute__((vector_size(3)));",
                  ProducesError(VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_VARIABLES_OF_ARITHMETIC_TYPES));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(7)));",
                  ProducesError(ARGUMENT_OF_VECTOR_SIZE_MUST_BE_A_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(20)));",
                  ProducesError(ARGUMENT_OF_VECTOR_SIZE_SHOULD_BE_A_POWER_OF_2_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE));
    SEMA_PRODUCES("typedef int i __attribute__((vector_size(8)));", ProducesNoErrors());
    SEMA_PRODUCES("enum\n"
                  "{\n"
                  " Thing = 8,\n"
                  "};\n"
                  "\n"
                  "typedef int i __attribute__((vector_size(Thing)));",
                  ProducesNoErrors());
}

TEST_CASE("Semantics vectors", "[semantics]")
{
    SECTION("Subscript operator")
    {
        auto& exp = generateExpression("void foo(void) {\n"
                                       "__attribute__((vector_size(16))) int i;\n"
                                       "i[0] = 1;\n"
                                       "i[2];"
                                       "}");
        CHECK(exp.getType() == PrimitiveType(PrimitiveType::Int, cld::LanguageOptions::native()));
        CHECK(exp.getValueCategory() == ValueCategory::Lvalue);
        CHECK(exp.is<SubscriptOperator>());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(16))) int i;\n"
                      "i[3.0] = 1;\n"
                      "}",
                      ProducesError(EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(16))) int i;\n"
                      "&i[0];\n"
                      "}",
                      ProducesError(CANNOT_TAKE_ADDRESS_OF_VECTOR_ELEMENT));
    }
    SECTION("Unary minus")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(16))) int i;\n"
                      "-i;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(16))) float i;\n"
                      "-i;\n"
                      "}",
                      ProducesNoErrors());
    }
    SECTION("Unary bitnot")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(16))) int i;\n"
                      "~i;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(16))) float i;\n"
                      "~i;\n"
                      "}",
                      ProducesError(OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'~'"));
    }
    SECTION("Casting")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "(long long)i;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "(int)i;\n"
                      "}",
                      ProducesError(CANNOT_CAST_FROM_VECTOR_TYPE_TO_TYPE_OF_DIFFERING_SIZE));
        SEMA_PRODUCES("void foo(void) {\n"
                      "int i;\n"
                      "(__attribute__((vector_size(8))) int)i;\n"
                      "}",
                      ProducesError(CANNOT_CAST_TO_VECTOR_TYPE_FROM_TYPE_OF_DIFFERING_SIZE));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "(__attribute__((vector_size(8))) short)i;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "(int*)i;\n"
                      "}",
                      ProducesError(CANNOT_CAST_FROM_VECTOR_TYPE_TO_NON_INTEGER_OR_VECTOR_TYPE));
        SEMA_PRODUCES("void foo(void) {\n"
                      "int* i;\n"
                      "(__attribute__((vector_size(8))) int)i;\n"
                      "}",
                      ProducesError(CANNOT_CAST_TO_VECTOR_TYPE_FROM_NON_INTEGER_OR_VECTOR_TYPE));
    }
    SECTION("Scalar conversion")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) short i;\n"
                      "i * 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) short i;\n"
                      "i * 5.0;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) short i;\n"
                      "i * 500000;\n"
                      "}",
                      ProducesError(CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) short i;\n"
                      "i * 5.1;\n"
                      "}",
                      ProducesError(CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i * 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i * 5.0;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i * 5.03;\n"
                      "}",
                      ProducesError(CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i * 4294967295;\n"
                      "}",
                      ProducesError(CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION));
    }
    SECTION("Mul and div")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i / i2;\n"
                      "i * i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i * i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'*'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) unsigned int i2;\n"
                      "i / i2;\n"
                      "i * i2;\n"
                      "}",
                      ProducesNoErrors());
    }
    SECTION("Mod")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i % i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i % i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'%'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i % 12;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'%'"));
    }
    SECTION("Add and Sub")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i + i2;\n"
                      "i - i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i + i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'+'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "int *f;\n"
                      "i + f;\n"
                      "}",
                      ProducesError(POINTER_ARITHMETIC_WITH_VECTORS_IS_NOT_ALLOWED));
    }
    SECTION("Shift")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i << i2;\n"
                      "i >> i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "i << 5;\n"
                      "i >> 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i << i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'<<'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i << 12;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'<<'"));
    }
    SECTION("Relational")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i < i2;\n"
                      "i > i2;\n"
                      "i <= i2;\n"
                      "i >= i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "i < 5;\n"
                      "i > 5;\n"
                      "i <= 5;\n"
                      "i >= 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i < i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'<'"));
        SECTION("Floating point result")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "__attribute__((vector_size(16))) float f;\n"
                                           "f < 5;\n"
                                           "}");
            auto& type = exp.getType();
            REQUIRE(isVector(type));
            auto& elementType = getVectorElementType(type);
            REQUIRE(elementType.is<PrimitiveType>());
            auto& primType = elementType.as<PrimitiveType>();
            CHECK(primType.getBitCount() == 32);
            CHECK(primType.isSigned());
            CHECK_FALSE(primType.isFloatingPoint());
        }
        SECTION("Unsigned integer result")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "__attribute__((vector_size(16))) unsigned long f;\n"
                                           "f < 5;\n"
                                           "}");
            auto& type = exp.getType();
            REQUIRE(isVector(type));
            auto& elementType = getVectorElementType(type);
            REQUIRE(elementType.is<PrimitiveType>());
            auto& primType = elementType.as<PrimitiveType>();
            CHECK(primType.getKind() == PrimitiveType::Long);
            CHECK(primType.isSigned());
            CHECK_FALSE(primType.isFloatingPoint());
        }
    }
    SECTION("Equality")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i == i2;\n"
                      "i != i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "i == 5;\n"
                      "i != 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i == i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'=='"));
        SECTION("Floating point result")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "__attribute__((vector_size(16))) float f;\n"
                                           "f == 5;\n"
                                           "}");
            auto& type = exp.getType();
            REQUIRE(isVector(type));
            auto& elementType = getVectorElementType(type);
            REQUIRE(elementType.is<PrimitiveType>());
            auto& primType = elementType.as<PrimitiveType>();
            CHECK(primType.getBitCount() == 32);
            CHECK(primType.isSigned());
            CHECK_FALSE(primType.isFloatingPoint());
        }
        SECTION("Unsigned integer result")
        {
            auto& exp = generateExpression("void foo(void) {\n"
                                           "__attribute__((vector_size(16))) unsigned long f;\n"
                                           "f == 5;\n"
                                           "}");
            auto& type = exp.getType();
            REQUIRE(isVector(type));
            auto& elementType = getVectorElementType(type);
            REQUIRE(elementType.is<PrimitiveType>());
            auto& primType = elementType.as<PrimitiveType>();
            CHECK(primType.getKind() == PrimitiveType::Long);
            CHECK(primType.isSigned());
            CHECK_FALSE(primType.isFloatingPoint());
        }
    }
    SECTION("Bitops")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "i | i2;\n"
                      "i & i2;\n"
                      "i ^ i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "i | 5;\n"
                      "i & 5;\n"
                      "i ^ 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i | i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'|'"));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) float i;\n"
                      "i | 12;\n"
                      "}",
                      ProducesError(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "'|'"));
    }
    SECTION("Conditional expression")
    {
        SEMA_PRODUCES("void foo(int b) {\n"
                      "__attribute__((vector_size(8))) int i,i2;\n"
                      "b ? i : i2;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int b) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "b ? i : 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(int b) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "b ? i : i2;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_IN_CONDITIONAL_OPERATOR_MUST_MATCH));
    }
    SECTION("Assignment")
    {
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short i2;\n"
                      "i = i2;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
        SEMA_PRODUCES("void foo(void) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "i += 5;\n"
                      "}",
                      ProducesNoErrors());
        SEMA_PRODUCES("void foo(float s) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "i += s;\n"
                      "}",
                      ProducesError(CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION));
        SEMA_PRODUCES("void foo() {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "__attribute__((vector_size(8))) short s;\n"
                      "i += s;\n"
                      "}",
                      ProducesError(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH, "'+='"));
        SEMA_PRODUCES("void foo(int s) {\n"
                      "__attribute__((vector_size(8))) int i;\n"
                      "s |= i;\n"
                      "}",
                      ProducesError(CANNOT_ASSIGN_INCOMPATIBLE_TYPES));
    }
    SECTION("Initialization")
    {
        SEMA_PRODUCES("int r __attribute__((vector_size(sizeof(int) * 4))) = {3,3,53,34};", ProducesNoErrors());
        SEMA_PRODUCES("int r __attribute__((vector_size(sizeof(int) * 4))) = {3,3,53,34,3};",
                      ProducesError(NO_MORE_SUB_OBJECTS_TO_INITIALIZE));
    }
}

TEST_CASE("Semantics __attribute__((aligned))", "[semantics]")
{
    SEMA_PRODUCES("struct __attribute__ ((aligned)) S { short f[3]; };", ProducesNoErrors());
    SEMA_PRODUCES("struct __attribute__ ((aligned(8))) S { short f[3]; };", ProducesNoErrors());
    SEMA_PRODUCES("struct __attribute__ ((aligned(8,3))) S { short f[3]; };",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N, "'aligned'", 1, 2));
    SEMA_PRODUCES("struct __attribute__ ((aligned(3.0))) S { short f[3]; };",
                  ProducesError(EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_ARGUMENT_TO_ALIGNED));
    SEMA_PRODUCES("struct __attribute__ ((aligned(-1))) S { short f[3]; };",
                  ProducesError(ARGUMENT_TO_ALIGNED_MUST_BE_A_POSITIVE_NUMBER));
    SEMA_PRODUCES("struct __attribute__ ((aligned(3))) S { short f[3]; };",
                  ProducesError(ARGUMENT_TO_ALIGNED_MUST_BE_A_POWER_OF_2));
    SECTION("struct")
    {
        SECTION("Simple")
        {
            auto program = generateProgram("struct __attribute__((aligned(8))) S {\n"
                                           "short f[3];\n"
                                           "};\n",
                                           x64linux);
            auto* structInfo = program.lookupType<StructInfo>("S", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(structInfo);
            auto* attribute = structInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 8);
            CHECK(StructType(*structInfo).getSizeOf(program) == 8);
            CHECK(StructType(*structInfo).getAlignOf(program) == 8);
        }
        SECTION("Can't reduce align")
        {
            auto program = generateProgram("struct __attribute__((aligned(1))) S {\n"
                                           "short f[3];\n"
                                           "};\n",
                                           x64linux);
            auto* structInfo = program.lookupType<StructInfo>("S", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(structInfo);
            auto* attribute = structInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 1);
            CHECK(StructType(*structInfo).getSizeOf(program) == 6);
            CHECK(StructType(*structInfo).getAlignOf(program) == 2);
        }
        SECTION("Largest alignment attribute is chosen")
        {
            auto program = generateProgram("struct __attribute__((aligned(4))) S;\n"
                                           "\n"
                                           "struct __attribute__((aligned(2))) S {\n"
                                           "short f[3];\n"
                                           "};\n",
                                           x64linux);
            auto* structInfo = program.lookupType<StructInfo>("S", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(structInfo);
            auto* attribute = structInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 4);
            CHECK(StructType(*structInfo).getSizeOf(program) == 8);
            CHECK(StructType(*structInfo).getAlignOf(program) == 4);
        }
    }
    SECTION("union")
    {
        SECTION("Simple")
        {
            auto program = generateProgram("union __attribute__((aligned(8))) S {\n"
                                           "short f[3];\n"
                                           "char c;\n"
                                           "};\n",
                                           x64linux);
            auto* unionInfo = program.lookupType<UnionInfo>("S", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(unionInfo);
            auto* attribute = unionInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 8);
            CHECK(UnionType(*unionInfo).getSizeOf(program) == 8);
            CHECK(UnionType(*unionInfo).getAlignOf(program) == 8);
        }
        SECTION("Can't reduce align")
        {
            auto program = generateProgram("union __attribute__((aligned(1))) S {\n"
                                           "short f[3];\n"
                                           "char c;\n"
                                           "};\n",
                                           x64linux);
            auto* unionInfo = program.lookupType<UnionInfo>("S", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(unionInfo);
            auto* attribute = unionInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 1);
            CHECK(UnionType(*unionInfo).getSizeOf(program) == 6);
            CHECK(UnionType(*unionInfo).getAlignOf(program) == 2);
        }
        SECTION("Largest alignment attribute is chosen")
        {
            auto program = generateProgram("union __attribute__((aligned(4))) S;\n"
                                           "\n"
                                           "union __attribute__((aligned(2))) S {\n"
                                           "short f[3];\n"
                                           "char c;\n"
                                           "};\n",
                                           x64linux);
            auto* unionInfo = program.lookupType<UnionInfo>("S", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(unionInfo);
            auto* attribute = unionInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 4);
            CHECK(UnionType(*unionInfo).getSizeOf(program) == 8);
            CHECK(UnionType(*unionInfo).getAlignOf(program) == 4);
        }
    }
    SECTION("enum")
    {
        SECTION("stricter")
        {
            auto program = generateProgram("enum __attribute__((aligned(8))) Thing {\n"
                                           "f,\n"
                                           "c\n"
                                           "};\n",
                                           x64linux);
            auto* enumInfo = program.lookupType<EnumInfo>("Thing", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(enumInfo);
            auto* attribute = enumInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 8);
            CHECK(EnumType(*enumInfo).getSizeOf(program) == 4);
            CHECK(EnumType(*enumInfo).getAlignOf(program) == 8);
        }
        SECTION("lenient")
        {
            auto program = generateProgram("enum __attribute__((aligned(1))) Thing {\n"
                                           "f,\n"
                                           "c\n"
                                           "};\n",
                                           x64linux);
            auto* enumInfo = program.lookupType<EnumInfo>("Thing", ProgramInterface::GLOBAL_SCOPE);
            REQUIRE(enumInfo);
            auto* attribute = enumInfo->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 1);
            CHECK(EnumType(*enumInfo).getSizeOf(program) == 4);
            CHECK(EnumType(*enumInfo).getAlignOf(program) == 1);
        }
    }
    SECTION("Typedef")
    {
        SECTION("stricter")
        {
            auto program = generateProgram("struct S {\n"
                                           "short f[3];\n"
                                           "};\n"
                                           "\n"
                                           "typedef __attribute__((aligned(8))) struct S S;",
                                           x64linux);
            auto& decl = program.getScopes()[ProgramInterface::GLOBAL_SCOPE].declarations.at("S");
            REQUIRE(std::holds_alternative<TypedefInfo*>(decl.declared));
            auto* attribute = cld::get<TypedefInfo*>(decl.declared)->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 8);
            CHECK(cld::get<TypedefInfo*>(decl.declared)->type->getAlignOf(program) == 8);
        }
        SECTION("lenient")
        {
            auto program = generateProgram("struct S {\n"
                                           "short f[3];\n"
                                           "};\n"
                                           "\n"
                                           "typedef __attribute__((aligned(1))) struct S S;",
                                           x64linux);
            auto& decl = program.getScopes()[ProgramInterface::GLOBAL_SCOPE].declarations.at("S");
            REQUIRE(std::holds_alternative<TypedefInfo*>(decl.declared));
            auto* attribute = cld::get<TypedefInfo*>(decl.declared)->getAttributeIf<AlignedAttribute>();
            REQUIRE(attribute);
            CHECK(attribute->alignment == 1);
            CHECK(cld::get<TypedefInfo*>(decl.declared)->type->getAlignOf(program) == 1);
        }
    }
    SECTION("Global variable")
    {
        auto program = generateProgram("int i __attribute__((aligned(8))) = 5;", x64linux);
        auto& decl = program.getScopes()[ProgramInterface::GLOBAL_SCOPE].declarations.at("i");
        REQUIRE(std::holds_alternative<VariableDeclaration*>(decl.declared));
        auto* attribute = cld::get<VariableDeclaration*>(decl.declared)->getAttributeIf<AlignedAttribute>();
        REQUIRE(attribute);
        CHECK(attribute->alignment == 8);
    }
    SECTION("Local variable")
    {
        auto& expr = generateExpression("void foo(void) {\n"
                                        "    int i __attribute__((aligned(8))) = 5;\n"
                                        "    i;\n"
                                        "}",
                                        cld::LanguageOptions::fromTriple(x64linux));
        REQUIRE(expr.is<DeclarationRead>());
        auto& decl = expr.as<DeclarationRead>().getDeclRead();
        REQUIRE(decl.is<VariableDeclaration>());
        auto* attribute = decl.as<VariableDeclaration>().getAttributeIf<AlignedAttribute>();
        REQUIRE(attribute);
        CHECK(attribute->alignment == 8);
    }
    SECTION("Function")
    {
        auto program = generateProgram("__attribute__((aligned(8))) void foo(void) {\n"
                                       "    int i = 5;\n"
                                       "    i;\n"
                                       "}",
                                       x64linux);
        auto& globals = program.getTranslationUnit().getGlobals();
        REQUIRE(globals.size() == 1);
        auto& func = globals[0];
        REQUIRE(func->is<FunctionDefinition>());
        auto* attribute = func->as<FunctionDefinition>().getAttributeIf<AlignedAttribute>();
        REQUIRE(attribute);
        CHECK(attribute->alignment == 8);
    }
    SECTION("Type")
    {
        SEMA_PRODUCES("long foo(void) {\n"
                      "    int i = 5;\n"
                      "    return (__attribute__((aligned(8))) long)i;\n"
                      "}",
                      ProducesWarning(ATTRIBUTE_N_IGNORED_WHILE_PARSING_TYPE, "'aligned'"));
    }
}

TEST_CASE("Semantics __attribute__((noinline))", "[semantics]")
{
    SEMA_PRODUCES("int __attribute__ ((noinline(8))) foo(void);",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_NONE_GOT_N, "'noinline'", 1));
    auto program = generateProgram("__attribute__((noinline)) void foo(void) {\n"
                                   "    int i = 5;\n"
                                   "    i;\n"
                                   "}",
                                   x64linux);
    auto& globals = program.getTranslationUnit().getGlobals();
    REQUIRE(globals.size() == 1);
    auto& func = globals[0];
    REQUIRE(func->is<FunctionDefinition>());
    CHECK(func->as<FunctionDefinition>().hasAttribute<NoinlineAttribute>());
}

TEST_CASE("Semantics __attribute__((always_inline))", "[semantics]")
{
    SEMA_PRODUCES("int __attribute__ ((always_inline(8))) foo(void);",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_NONE_GOT_N, "'always_inline'", 1));
    auto program = generateProgram("__attribute__((always_inline)) void foo(void) {\n"
                                   "    int i = 5;\n"
                                   "    i;\n"
                                   "}",
                                   x64linux);
    auto& globals = program.getTranslationUnit().getGlobals();
    REQUIRE(globals.size() == 1);
    auto& func = globals[0];
    REQUIRE(func->is<FunctionDefinition>());
    CHECK(func->as<FunctionDefinition>().hasAttribute<AlwaysInlineAttribute>());
}

TEST_CASE("Semantics __attribute__((gnu_inline))", "[semantics]")
{
    SEMA_PRODUCES("int __attribute__ ((gnu_inline(8))) foo(void);",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_NONE_GOT_N, "'gnu_inline'", 1));
    SEMA_PRODUCES(
        "int __attribute__ ((gnu_inline)) foo(void);",
        ProducesWarning(GNU_INLINE_CAN_NOT_BE_APPLIED_TO_FUNCTION_N_BECAUSE_IT_IS_NOT_DECLARED_INLINE, "'foo'"));
    SEMA_PRODUCES("int foo(void);\n"
                  "\n"
                  "__attribute__((gnu_inline)) inline int foo(void) {\n"
                  "return 5;\n"
                  "}",
                  ProducesNoWarnings());

    auto program = generateProgram("__attribute__((gnu_inline)) inline void foo(void) {\n"
                                   "    int i = 5;\n"
                                   "    i;\n"
                                   "}",
                                   x64linux);
    auto& globals = program.getTranslationUnit().getGlobals();
    REQUIRE(globals.size() == 1);
    auto& func = globals[0];
    REQUIRE(func->is<FunctionDefinition>());
    CHECK(func->as<FunctionDefinition>().hasAttribute<GnuInlineAttribute>());
}

TEST_CASE("Semantics __attribute__((artificial))", "[semantics]")
{
    SEMA_PRODUCES("int __attribute__ ((artificial(8))) foo(void);",
                  ProducesError(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_NONE_GOT_N, "'artificial'", 1));
    auto program = generateProgram("__attribute__((artificial)) void foo(void) {\n"
                                   "    int i = 5;\n"
                                   "    i;\n"
                                   "}",
                                   x64linux);
    auto& globals = program.getTranslationUnit().getGlobals();
    REQUIRE(globals.size() == 1);
    auto& func = globals[0];
    REQUIRE(func->is<FunctionDefinition>());
    CHECK(func->as<FunctionDefinition>().hasAttribute<ArtificialAttribute>());
}

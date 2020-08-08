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
        SECTION("External linkage")
        {
            auto [translationUnit, errors] = generateSemantics("extern int i;");
            REQUIRE_THAT(errors, ProducesNothing());
            REQUIRE(translationUnit.getGlobals().size() == 1);
            REQUIRE(
                std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
            auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
            CHECK(decl->getLinkage() == cld::Semantics::Linkage::External);
            CHECK(decl->getLifetime() == cld::Semantics::Lifetime::Static);
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
        SEMA_PRODUCES("typedef int i;\n"
                      "i float f;",
                      ProducesError(EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_TYPENAME));
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
        SEMA_PRODUCES(text, ProducesError(DECLARATION_MUST_NOT_BE_VOID));
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
            CHECK(decl->getName() == "i");
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
            CHECK(decl->getName() == "f");
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
                CHECK(decl->getName() == "f");
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
                CHECK(decl->getName() == "f");
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
            CHECK(decl->getName() == "f");
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
            CHECK(decl->getName() == "f");
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
            CHECK(decl->getName() == "f");
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
                      ProducesError(VARIABLE_LENGTH_ARRAY_NOT_ALLOWED_AT_FILE_SCOPE));
        SEMA_PRODUCES("int n = 1;\n"
                      "int f[n][5];",
                      ProducesError(VARIABLE_LENGTH_ARRAY_NOT_ALLOWED_AT_FILE_SCOPE));
        SEMA_PRODUCES("int f[*];", ProducesError(VARIABLE_LENGTH_ARRAY_NOT_ALLOWED_AT_FILE_SCOPE));
        SEMA_PRODUCES("int f[*][5];", ProducesError(VARIABLE_LENGTH_ARRAY_NOT_ALLOWED_AT_FILE_SCOPE));
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
        CHECK(decl->getName() == "f");
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
        CHECK(decl->getName() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::StructType>(decl->getType().get()));
        CHECK(cld::get<cld::Semantics::StructType>(decl->getType().get()).getName() == "A");
        CHECK(cld::get<cld::Semantics::StructType>(decl->getType().get()).getScopeOrId() == 0);
    }
    SECTION("Simple union")
    {
        auto [translationUnit, errors] = generateSemantics("union A{ int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getName() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::UnionType>(decl->getType().get()));
        CHECK(cld::get<cld::Semantics::UnionType>(decl->getType().get()).getName() == "A");
        CHECK(cld::get<cld::Semantics::UnionType>(decl->getType().get()).getScopeOrId() == 0);
    }
    SECTION("Anonymous struct")
    {
        auto [translationUnit, errors] = generateSemantics("struct { int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getName() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::AnonymousStructType>(decl->getType().get()));
        auto& anon = cld::get<cld::Semantics::AnonymousStructType>(decl->getType().get());
        CHECK(anon.getFields().size() == 3);
        CHECK(anon.getFields()[0].name == "i");
        CHECK(*anon.getFields()[0].type
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK_FALSE(anon.getFields()[0].bitFieldSize);
        CHECK(anon.getFields()[1].name == "f");
        CHECK(*anon.getFields()[1].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[1].bitFieldSize);
        CHECK(anon.getFields()[2].name == "r");
        CHECK(*anon.getFields()[2].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[2].bitFieldSize);
    }
    SECTION("Anonymous union")
    {
        auto [translationUnit, errors] = generateSemantics("union { int i; float f, r; } a;");
        REQUIRE_THAT(errors, ProducesNothing());
        REQUIRE(translationUnit.getGlobals().size() == 1);
        REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]));
        auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
        CHECK(decl->getName() == "a");
        REQUIRE(std::holds_alternative<cld::Semantics::AnonymousUnionType>(decl->getType().get()));
        auto& anon = cld::get<cld::Semantics::AnonymousUnionType>(decl->getType().get());
        CHECK(anon.getFields().size() == 3);
        CHECK(anon.getFields()[0].name == "i");
        CHECK(*anon.getFields()[0].type
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        CHECK_FALSE(anon.getFields()[0].bitFieldSize);
        CHECK(anon.getFields()[1].name == "f");
        CHECK(*anon.getFields()[1].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[1].bitFieldSize);
        CHECK(anon.getFields()[2].name == "r");
        CHECK(*anon.getFields()[2].type == cld::Semantics::PrimitiveType::createFloat(false, false));
        CHECK_FALSE(anon.getFields()[2].bitFieldSize);
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
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().get()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().get());
                CHECK(array.getSize() == 8);
            }
            SECTION("System V bitfields")
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
                                                                   x64linux);
                REQUIRE_THAT(errors, ProducesNothing());
                REQUIRE(translationUnit.getGlobals().size() == 1);
                REQUIRE(std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(
                    translationUnit.getGlobals()[0]));
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(translationUnit.getGlobals()[0]);
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().get()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().get());
                CHECK(array.getSize() == 4);
            }
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
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().get()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().get());
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
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().get()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().get());
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
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().get()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().get());
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
                REQUIRE(std::holds_alternative<cld::Semantics::ArrayType>(decl->getType().get()));
                auto& array = cld::get<cld::Semantics::ArrayType>(decl->getType().get());
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
                  ProducesError(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION)
                      && !ProducesError(VARIABLE_LENGTH_ARRAY_NOT_ALLOWED_AT_FILE_SCOPE));
}

TEST_CASE("Semantics function definitions")
{
    SECTION("Identifier list")
    {
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
        CHECK(decl->getName() == "f");
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
        std::string result = "void*";
        auto ptrIsConst = GENERATE(false, true);
        auto ptrIsVolatile = GENERATE(false, true);
        auto ptrIsRestricted = GENERATE(false, true);
        auto elemntIsConst = GENERATE(false, true);
        auto elemntIsVolatile = GENERATE(false, true);
        if (elemntIsVolatile)
        {
            result = "volatile " + result;
        }
        if (elemntIsConst)
        {
            result = "const " + result;
        }
        if (ptrIsRestricted)
        {
            result += " restrict";
        }
        if (ptrIsConst)
        {
            result += " const";
        }
        if (ptrIsVolatile)
        {
            result += " volatile";
        }
        CHECK(toStr(PointerType::create(ptrIsConst, ptrIsVolatile, ptrIsRestricted,
                                        PrimitiveType::createVoid(elemntIsConst, elemntIsVolatile)))
              == result);
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
              == "char*(*(**[][8])())[]");
    }
}

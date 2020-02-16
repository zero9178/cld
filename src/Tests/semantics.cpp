#include "catch.hpp"

#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SemanticAnalysis.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include <array>

static std::pair<OpenCL::Semantics::TranslationUnit, std::string>
    generateSemantics(const std::string& source,
                      const OpenCL::LanguageOptions& options = OpenCL::LanguageOptions::native())
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    OpenCL::SourceObject tokens;
    REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(source, options));
    auto parsing = OpenCL::Parser::buildTree(tokens, &ss);
    REQUIRE((ss.str().empty() && parsing.second));
    OpenCL::Semantics::SemanticAnalysis analysis(tokens, &ss);
    auto semantics = analysis.visit(parsing.first);
    return {semantics, ss.str()};
}

TEST_CASE("Function definition semantics", "[semantics]")
{
    SECTION("Standard")
    {
        auto source = "void foo(int i,float f){}";
        auto [semantics, error] = generateSemantics(source);
        INFO(error)
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 2);
        SECTION("Prototype")
        {
            auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
            REQUIRE(prototype);
            auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
            CHECK(prototype->getName() == "foo");
            CHECK(!functionType->isLastVararg());
            SECTION("Arguments")
            {
                CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
                CHECK(functionType->getArguments().size() == 2);
                CHECK(functionType->getArguments()[0].first
                      == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
                CHECK(functionType->getArguments()[0].second == "i");
                CHECK(functionType->getArguments()[1].first
                      == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
                CHECK(functionType->getArguments()[1].second == "f");
            }
        }
        SECTION("Definition")
        {
            const OpenCL::Semantics::FunctionDefinition* definition =
                std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics.getGlobals()[1]);
            REQUIRE(definition);
            CHECK(definition->getName() == "foo");
            CHECK(definition->hasPrototype());
            CHECK(definition->getLinkage() == OpenCL::Semantics::Linkage::External);
            CHECK(!definition->getType().isLastVararg());
            SECTION("Arguments")
            {
                CHECK(definition->getType().getReturnType()
                      == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
                CHECK(definition->getType().getArguments().size() == 2);
                CHECK(definition->getType().getArguments()[0].first
                      == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
                CHECK(definition->getType().getArguments()[0].second == "i");
                CHECK(definition->getType().getArguments()[1].first
                      == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
                CHECK(definition->getType().getArguments()[1].second == "f");
            }
        }
    }
    SECTION("K & R")
    {
        auto source = R"(void foo(i,f) register short i;float f;{})";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        const OpenCL::Semantics::FunctionDefinition* definition =
            std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics.getGlobals()[0]);
        REQUIRE(definition);
        CHECK(definition->getName() == "foo");
        CHECK(!definition->hasPrototype());
        CHECK(definition->getLinkage() == OpenCL::Semantics::Linkage::External);
        SECTION("Arguments")
        {
            CHECK(definition->getType().getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(definition->getType().getArguments().size() == 2);
            CHECK(definition->getType().getArguments()[0].first
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
            CHECK(definition->getParameterDeclarations()[0].getType()
                  == OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native()));
            CHECK(definition->getParameterDeclarations()[0].getLifetime() == OpenCL::Semantics::Lifetime::Register);
            CHECK(definition->getType().getArguments()[1].first
                  == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
            CHECK(definition->getParameterDeclarations()[1].getType()
                  == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(definition->getType().getArguments()[0].second == "i");
            CHECK(definition->getType().getArguments()[1].second == "f");
        }
    }
    SECTION("No argument")
    {
        auto source = R"(void foo(void){})";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 2);
        SECTION("Prototype")
        {
            auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
            REQUIRE(prototype);
            CHECK(prototype->getName() == "foo");
            auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
            REQUIRE(functionType);
            CHECK(!functionType->isLastVararg());
            CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(functionType->getArguments().empty());
        }
        SECTION("Definition")
        {
            const OpenCL::Semantics::FunctionDefinition* definition =
                std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics.getGlobals()[1]);
            REQUIRE(definition);
            CHECK(definition->getName() == "foo");
            CHECK(!definition->getType().isLastVararg());
            CHECK(definition->getType().getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(definition->getType().getArguments().empty());
        }
    }
}

TEST_CASE("Function declaration semantics", "[semantics]")
{
    SECTION("Standard")
    {
        auto source = R"(void foo(register int i,float f);)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(prototype);
        CHECK(prototype->getName() == "foo");
        auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
        REQUIRE(functionType);
        CHECK(!functionType->isLastVararg());
        CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
        SECTION("Arguments")
        {
            REQUIRE(functionType->getArguments().size() == 2);
            CHECK(functionType->getArguments()[0].first
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
            CHECK(functionType->getArguments()[0].second == "i");
            CHECK(functionType->getArguments()[1].first == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(functionType->getArguments()[1].second == "f");
        }
    }
    SECTION("Empty identifier list")
    {
        auto source = R"(double foo();)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(prototype);
        CHECK(prototype->getName() == "foo");
        auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
        REQUIRE(functionType);
        CHECK(!functionType->isLastVararg());
        CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
        CHECK(functionType->getArguments().empty());
    }
    SECTION("Ellipsis")
    {
        auto source = R"(void foo(int i,float f,...);)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(prototype);
        CHECK(prototype->getName() == "foo");
        auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
        REQUIRE(functionType);
        CHECK(functionType->isLastVararg());
        CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
        SECTION("Arguments")
        {
            REQUIRE(functionType->getArguments().size() == 2);
            CHECK(functionType->getArguments()[0].first
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
            CHECK(functionType->getArguments()[0].second == "i");
            CHECK(functionType->getArguments()[1].first == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(functionType->getArguments()[1].second == "f");
        }
    }
    SECTION("Linkage")
    {
        SECTION("internal")
        {
            auto source = R"(static void foo(void);)";
            auto [semantics, error] = generateSemantics(source);
            INFO(error);
            REQUIRE(error.empty());
            REQUIRE(semantics.getGlobals().size() == 1);
            auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
            REQUIRE(prototype);
            CHECK(prototype->getName() == "foo");
            auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
            REQUIRE(functionType);
            CHECK(!functionType->isLastVararg());
            CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(prototype->getLinkage() == OpenCL::Semantics::Linkage::Internal);
        }
        SECTION("External")
        {
            auto source = R"(extern void foo(void);)";
            auto [semantics, error] = generateSemantics(source);
            INFO(error);
            REQUIRE(error.empty());
            REQUIRE(semantics.getGlobals().size() == 1);
            auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
            REQUIRE(prototype);
            CHECK(prototype->getName() == "foo");
            auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
            REQUIRE(functionType);
            CHECK(!functionType->isLastVararg());
            CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(prototype->getLinkage() == OpenCL::Semantics::Linkage::External);
        }
    }
}

TEST_CASE("Function definitions and prototypes that should fail semantics", "[semantics]")
{
    std::array<const char*, 11> sources = {"static extern void foo(void);",
                                           "extern static void foo(void);",
                                           "void foo(void i);",
                                           "void foo(a,f);",
                                           "register void foo(void);",
                                           "auto void foo(void);",
                                           "void foo(const void);",
                                           "void foo(a) void a;{}",
                                           "void foo(static int i);",
                                           "void foo(extern int i);",
                                           "void foo(auto int i);"};
    for (auto& source : sources)
    {
        DYNAMIC_SECTION(source)
        {
            auto [semantics, error] = generateSemantics(source);
            INFO(error);
            REQUIRE_FALSE(error.empty());
        }
    }
}

TEST_CASE("Primitive Declaration semantics", "[semantics]")
{
    SECTION("Multi declarations")
    {
        auto source = R"(int i,f,c;)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 3);
        std::array names = {"i", "f", "c"};
        std::size_t i = 0;
        for (auto& iter : semantics.getGlobals())
        {
            auto* declaration = std::get_if<OpenCL::Semantics::Declaration>(&iter);
            REQUIRE(declaration);
            CHECK(declaration->getType()
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
            CHECK(declaration->getName() == names[i++]);
            CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
            CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
        }
    }
    SECTION("non cv qualified")
    {
        auto source = R"(int i;)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration* declaration =
            std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType()
              == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("const")
    {
        auto source = R"(const int const i;)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration* declaration =
            std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType()
              == OpenCL::Semantics::PrimitiveType::createInt(true, false, OpenCL::LanguageOptions::native()));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("volatile")
    {
        auto source = R"(volatile int i;)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration* declaration =
            std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType()
              == OpenCL::Semantics::PrimitiveType::createInt(false, true, OpenCL::LanguageOptions::native()));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("const volatile")
    {
        auto source = R"(const int volatile i;)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration* declaration =
            std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType()
              == OpenCL::Semantics::PrimitiveType::createInt(true, true, OpenCL::LanguageOptions::native()));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("External linkage")
    {
        auto source = R"(extern int i;)";
        auto [semantics, error] = generateSemantics(source);
        INFO(error);
        REQUIRE(error.empty());
        REQUIRE(semantics.getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration* declaration =
            std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType()
              == OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native()));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::External);
    }
    SECTION("Various primitives")
    {
        std::array results = {
            std::pair{"char i;",
                      OpenCL::Semantics::PrimitiveType::createChar(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed char i;",
                      OpenCL::Semantics::PrimitiveType::createChar(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned i;", OpenCL::Semantics::PrimitiveType::createUnsignedInt(
                                         false, false, OpenCL::LanguageOptions::native())},
            std::pair{"short i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"short int i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"int short i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed short i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"short signed i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed short int i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"short signed int i;",
                      OpenCL::Semantics::PrimitiveType::createShort(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned short i;", OpenCL::Semantics::PrimitiveType::createUnsignedShort(
                                               false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned short int i;", OpenCL::Semantics::PrimitiveType::createUnsignedShort(
                                                   false, false, OpenCL::LanguageOptions::native())},
            std::pair{"int i;",
                      OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed int i;",
                      OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed i;",
                      OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned int i;", OpenCL::Semantics::PrimitiveType::createUnsignedInt(
                                             false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned i;", OpenCL::Semantics::PrimitiveType::createUnsignedInt(
                                         false, false, OpenCL::LanguageOptions::native())},
            std::pair{"long i;",
                      OpenCL::Semantics::PrimitiveType::createLong(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed long i;",
                      OpenCL::Semantics::PrimitiveType::createLong(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"long int i;",
                      OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"signed long int i;",
                      OpenCL::Semantics::PrimitiveType::createInt(false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned long i;", OpenCL::Semantics::PrimitiveType::createUnsignedLong(
                                              false, false, OpenCL::LanguageOptions::native())},
            std::pair{"unsigned long int i;", OpenCL::Semantics::PrimitiveType::createUnsignedLong(
                                                  false, false, OpenCL::LanguageOptions::native())},
            std::pair{"long long i;", OpenCL::Semantics::PrimitiveType::createLongLong(false, false)},
            std::pair{"signed long long i;", OpenCL::Semantics::PrimitiveType::createLongLong(false, false)},
            std::pair{"long long int i;", OpenCL::Semantics::PrimitiveType::createLongLong(false, false)},
            std::pair{"signed long long int i;", OpenCL::Semantics::PrimitiveType::createLongLong(false, false)},
            std::pair{"unsigned long long i;", OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false)},
            std::pair{"unsigned long long int i;",
                      OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false)},
            std::pair{"long unsigned int long i;",
                      OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false)},
            std::pair{"float i;", OpenCL::Semantics::PrimitiveType::createFloat(false, false)},
            std::pair{"double i;", OpenCL::Semantics::PrimitiveType::createDouble(false, false)},
            std::pair{"double i;", OpenCL::Semantics::PrimitiveType::createLongDouble(
                                       false, false, OpenCL::LanguageOptions::native())}};
        for (auto& [source, type] : results)
        {
            DYNAMIC_SECTION("Primitive: " << source)
            {
                auto [semantics, error] = generateSemantics(source);
                INFO(error);
                REQUIRE(error.empty());
                if (semantics.getGlobals().size() != 1)
                {
                    FAIL(source);
                }
                auto* declaration = std::get_if<OpenCL::Semantics::Declaration>(&semantics.getGlobals()[0]);
                REQUIRE(declaration);
                CHECK(declaration->getType() == type);
            }
        }
    }
}

TEST_CASE("Invalid primitive declarations semantics", "[semantics]")
{
    std::array sources = {
        "void i;",         "const void i;",       "static extern int i;", "auto register int i;", "short int long i;",
        "char char i;",    "long short i;",       "long long long i;",    "float int i;",         "signed unsigned i;",
        "restrict int i;", "long double long i;", "long long double i;",
    };
    for (auto& source : sources)
    {
        DYNAMIC_SECTION(source)
        {
            auto [semantics, error] = generateSemantics(source);
            REQUIRE_FALSE(error.empty());
        }
    }
}

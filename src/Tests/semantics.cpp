#include "catch.hpp"
#include <CompilerCore/C/SemanticAnalysis.hpp>
#include <CompilerCore/C/Parser.hpp>

TEST_CASE("Function definition", "[semantics]")
{
    auto source = "void foo(int i,float f){}";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 2);
    SECTION("Prototype")
    {
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
        REQUIRE(prototype);
        auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
        CHECK(prototype->getName() == "foo");
        CHECK(!functionType->isLastVararg());
        SECTION("Arguments")
        {
            CHECK(functionType->getReturnType()
                      == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(functionType->getArguments().size() == 2);
            CHECK(functionType->getArguments()[0].first
                      == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(functionType->getArguments()[0].second == "i");
            CHECK(functionType->getArguments()[1].first
                      == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(functionType->getArguments()[1].second == "f");
        }
    }
    SECTION("Definition")
    {
        const OpenCL::Semantics::FunctionDefinition
            * definition = std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics->getGlobals()[1]);
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
                      == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(definition->getType().getArguments()[0].second == "i");
            CHECK(definition->getType().getArguments()[1].first
                      == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(definition->getType().getArguments()[1].second == "f");
        }
    }
}

TEST_CASE("K&R Function definition", "[semantics]")
{
    auto source = R"(void foo(i,f) short i;float f;{})";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 1);
    const OpenCL::Semantics::FunctionDefinition* definition = std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics->getGlobals()[0]);
    REQUIRE(definition);
    CHECK(definition->getName() == "foo");
    CHECK(!definition->hasPrototype());
    CHECK(definition->getLinkage() == OpenCL::Semantics::Linkage::External);
    SECTION("Arguments")
    {
        CHECK(definition->getType().getReturnType()
                  == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
        CHECK(definition->getType().getArguments().size() == 2);
        CHECK(definition->getType().getArguments()[0].first
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(definition->getParameterDeclarations()[0].getType()
                  == OpenCL::Semantics::PrimitiveType::createShort(false, false));
        CHECK(definition->getType().getArguments()[1].first
                      == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
        CHECK(definition->getParameterDeclarations()[1].getType()
                  == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
        CHECK(definition->getType().getArguments()[0].second == "i");
        CHECK(definition->getType().getArguments()[1].second == "f");
    }
}

TEST_CASE("No argument function prototype","[semantics]")
{
    auto source = R"(void foo(void);)";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 1);
    auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
    REQUIRE(prototype);
    CHECK(prototype->getName() == "foo");
    auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
    REQUIRE(functionType);
    CHECK(!functionType->isLastVararg());
    CHECK(functionType->getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
    CHECK(functionType->getArguments().empty());
}

TEST_CASE("No argument function definition","[semantics]")
{
    auto source = R"(void foo(void){})";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 2);
    SECTION("Prototype")
    {
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
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
        const OpenCL::Semantics::FunctionDefinition
            * definition = std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics->getGlobals()[1]);
        REQUIRE(definition);
        CHECK(definition->getName() == "foo");
        CHECK(!definition->getType().isLastVararg());
        CHECK(definition->getType().getReturnType() == OpenCL::Semantics::PrimitiveType::createVoid(false,false));
        CHECK(definition->getType().getArguments().empty());
    }
}

TEST_CASE("Function prototype", "[semantics]")
{
    auto source = R"(void foo(int i,float f);)";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 1);
    auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
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
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(functionType->getArguments()[0].second == "i");
        CHECK(functionType->getArguments()[1].first
                  == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
        CHECK(functionType->getArguments()[1].second == "f");
    }
}

TEST_CASE("Empty identifier list function prototype","[semantics]")
{
    auto source = R"(double foo();)";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 1);
    auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
    REQUIRE(prototype);
    CHECK(prototype->getName() == "foo");
    auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
    REQUIRE(functionType);
    CHECK(!functionType->isLastVararg());
    CHECK(functionType->getReturnType()
              == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
    CHECK(functionType->getArguments().empty());
}

TEST_CASE("Ellipsis function","[semantics]")
{
    auto source = R"(void foo(int i,float f,...);)";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 1);
    auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
    REQUIRE(prototype);
    CHECK(prototype->getName() == "foo");
    auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
    REQUIRE(functionType);
    CHECK(functionType->isLastVararg());
    CHECK(functionType->getReturnType()
              == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
    SECTION("Arguments")
    {
        REQUIRE(functionType->getArguments().size() == 2);
        CHECK(functionType->getArguments()[0].first
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(functionType->getArguments()[0].second == "i");
        CHECK(functionType->getArguments()[1].first
                  == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
        CHECK(functionType->getArguments()[1].second == "f");
    }
}

TEST_CASE("Function linkage","[semantics]")
{
    SECTION("internal")
    {
        auto source = R"(static void foo(void);)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
        if (!parsing)
        {
            FAIL(parsing.error().getText());
        }

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(*parsing);
        if (!semantics)
        {
            FAIL(semantics.error().getText());
        }
        REQUIRE(semantics->getGlobals().size() == 1);
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
        REQUIRE(prototype);
        CHECK(prototype->getName() == "foo");
        auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
        REQUIRE(functionType);
        CHECK(!functionType->isLastVararg());
        CHECK(functionType->getReturnType()
                  == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
        CHECK(prototype->getLinkage() == OpenCL::Semantics::Linkage::Internal);
    }
    SECTION("External")
    {
        auto source = R"(extern void foo(void);)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
        if (!parsing)
        {
            FAIL(parsing.error().getText());
        }

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(*parsing);
        if (!semantics)
        {
            FAIL(semantics.error().getText());
        }
        REQUIRE(semantics->getGlobals().size() == 1);
        auto* prototype = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
        REQUIRE(prototype);
        CHECK(prototype->getName() == "foo");
        auto* functionType = std::get_if<OpenCL::Semantics::FunctionType>(&prototype->getType().get());
        REQUIRE(functionType);
        CHECK(!functionType->isLastVararg());
        CHECK(functionType->getReturnType()
                  == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
        CHECK(prototype->getLinkage() == OpenCL::Semantics::Linkage::External);
    }
}

TEST_CASE("Function definitions and prototypes that should fail","[semantics]")
{
    std::array sources = {
        "static extern void foo(void);",
        "extern static void foo(void);",
        "void foo(void i);",
        "void foo(a,f);",
        "register void foo(void);",
        "auto void foo(void);",
        "void foo(const void);",
        "void foo(a) void a;{}",
    };
    for(auto& source : sources)
    {
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
        if (!parsing)
        {
            FAIL_CHECK(parsing.error().getText());
        }

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(*parsing);
        if (!semantics)
        {
            continue;
        }
        FAIL_CHECK(source);
    }
}

TEST_CASE("Declaration semantics", "[semantics]")
{
    auto source = R"(int i;)";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
    if (!parsing)
    {
        FAIL(parsing.error().getText());
    }

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(*parsing);
    if (!semantics)
    {
        FAIL(semantics.error().getText());
    }
    REQUIRE(semantics->getGlobals().size() == 1);
    const OpenCL::Semantics::Declaration
        * declaration = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
    REQUIRE(declaration);
    CHECK(declaration->getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
    CHECK(declaration->getName() == "i");
    CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
    CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
}

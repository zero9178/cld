#include "catch.hpp"
#include <CompilerCore/C/SemanticAnalysis.hpp>
#include <CompilerCore/C/Parser.hpp>

TEST_CASE("Function definition", "[semantics]")
{
    auto source = R"(void foo(int i,float f){})";
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
        const OpenCL::Semantics::FunctionPrototype
            * prototype = std::get_if<OpenCL::Semantics::FunctionPrototype>(&semantics->getGlobals()[0]);
        REQUIRE(prototype);
        CHECK(prototype->getName() == "foo");
        CHECK(!prototype->getType().isLastVararg());
        SECTION("Arguments")
        {
            REQUIRE(prototype->getArgumentNames().size() == 2);
            CHECK(prototype->getArgumentNames()[0] == "i");
            CHECK(prototype->getArgumentNames()[1] == "f");
            CHECK(prototype->getType().getReturnType()
                      == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(prototype->getType().getArguments().size() == 2);
            CHECK(prototype->getType().getArguments()[0]
                      == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(prototype->getType().getArguments()[1]
                      == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
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
            REQUIRE(definition->getArgumentNames().size() == 2);
            CHECK(definition->getArgumentNames()[0] == "i");
            CHECK(definition->getArgumentNames()[1] == "f");
            CHECK(definition->getType().getReturnType()
                      == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
            CHECK(definition->getType().getArguments().size() == 2);
            CHECK(definition->getType().getArguments()[0]
                      == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(definition->getType().getArguments()[1]
                      == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
        }
    }
}

TEST_CASE("Function prototype", "[semnatics]")
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
    const OpenCL::Semantics::FunctionPrototype
        * prototype = std::get_if<OpenCL::Semantics::FunctionPrototype>(&semantics->getGlobals()[0]);
    REQUIRE(prototype);
    CHECK(prototype->getName() == "foo");
    CHECK(!prototype->getType().isLastVararg());
    SECTION("Arguments")
    {
        REQUIRE(prototype->getArgumentNames().size() == 2);
        CHECK(prototype->getArgumentNames()[0] == "i");
        CHECK(prototype->getArgumentNames()[1] == "f");
        CHECK(prototype->getType().getReturnType()
                  == OpenCL::Semantics::PrimitiveType::createVoid(false, false));
        CHECK(prototype->getType().getArguments().size() == 2);
        CHECK(prototype->getType().getArguments()[0]
                  == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(prototype->getType().getArguments()[1]
                  == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
    }
}

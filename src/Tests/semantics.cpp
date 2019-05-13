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
    REQUIRE_FALSE(semantics->getGlobals().empty());
    const OpenCL::Semantics::FunctionDefinition
        * definition = std::get_if<OpenCL::Semantics::FunctionDefinition>(&semantics->getGlobals()[0]);
    REQUIRE(definition);
    CHECK(definition->getName() == "foo");
    CHECK(definition->hasPrototype());
    REQUIRE(definition->getArgumentNames().size() == 2);
    CHECK(definition->getArgumentNames()[0] == "i");
    CHECK(definition->getArgumentNames()[1] == "f");
    CHECK(!definition->getType().isLastVararg());
    CHECK(definition->getType().getReturnType()
              == OpenCL::Semantics::PrimitiveType::create(false, false, false, false, 0));
    CHECK(definition->getType().getArguments().size() == 2);
    CHECK(definition->getType().getArguments()[0]
              == OpenCL::Semantics::PrimitiveType::create(false, false, false, true, 32));
    CHECK(definition->getType().getArguments()[1]
              == OpenCL::Semantics::PrimitiveType::create(false, false, true, true, 32));
}

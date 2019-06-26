#include "catch.hpp"

#include <array>
#include <CompilerCore/C/SemanticAnalysis.hpp>
#include <CompilerCore/C/Parser.hpp>

TEST_CASE("Function definition", "[semantics]")
{
    auto source = "void foo(int i,float f){}";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
    auto source = R"(void foo(i,f) register short i;float f;{})";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
        CHECK(definition->getParameterDeclarations()[0].getLifetime() == OpenCL::Semantics::Lifetime::Register);
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
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
    auto source = R"(void foo(register int i,float f);)";
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
    auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
    REQUIRE(parsing.second);

    OpenCL::Semantics::SemanticAnalysis analysis;
    auto semantics = analysis.visit(parsing.first);
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
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
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
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
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
    std::array<const char*, 11> sources = {
        "static extern void foo(void);",
        "extern static void foo(void);",
        "void foo(void i);",
        "void foo(a,f);",
        "register void foo(void);",
        "auto void foo(void);",
        "void foo(const void);",
        "void foo(a) void a;{}",
        "void foo(static int i);",
        "void foo(extern int i);",
        "void foo(auto int i);"
    };
    for(auto& source : sources)
    {
        DYNAMIC_SECTION(source)
        {
            auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
            REQUIRE(parsing.second);

            OpenCL::Semantics::SemanticAnalysis analysis;
            auto semantics = analysis.visit(parsing.first);
            REQUIRE(!semantics);
        }
    }
}

TEST_CASE("Primitive Declaration semantics", "[semantics]")
{
    SECTION("Multi declarations")
    {
        auto source = R"(int i,f,c;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
        if (!semantics)
        {
            FAIL(semantics.error().getText());
        }
        REQUIRE(semantics->getGlobals().size() == 3);
        std::array names = {"i", "f", "c"};
        std::size_t i = 0;
        for (auto& iter : semantics->getGlobals())
        {
            auto* declaration = std::get_if<OpenCL::Semantics::Declaration>(&iter);
            REQUIRE(declaration);
            CHECK(declaration->getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(declaration->getName() == names[i++]);
            CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
            CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
        }
    }
    SECTION("Empty declaration")
    {
        auto source = R"(int;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
        REQUIRE(!semantics);
    }
    SECTION("non cv qualified")
    {
        auto source = R"(int i;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
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
    SECTION("const")
    {
        auto source = R"(const int const i;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
        if (!semantics)
        {
            FAIL(semantics.error().getText());
        }
        REQUIRE(semantics->getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration
            * declaration = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType() == OpenCL::Semantics::PrimitiveType::createInt(true, false));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("volatile")
    {
        auto source = R"(volatile int i;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
        if (!semantics)
        {
            FAIL(semantics.error().getText());
        }
        REQUIRE(semantics->getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration
            * declaration = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType() == OpenCL::Semantics::PrimitiveType::createInt(false, true));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("const volatile")
    {
        auto source = R"(const int volatile i;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
        if (!semantics)
        {
            FAIL(semantics.error().getText());
        }
        REQUIRE(semantics->getGlobals().size() == 1);
        const OpenCL::Semantics::Declaration
            * declaration = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
        REQUIRE(declaration);
        CHECK(declaration->getType() == OpenCL::Semantics::PrimitiveType::createInt(true, true));
        CHECK(declaration->getName() == "i");
        CHECK(declaration->getLifetime() == OpenCL::Semantics::Lifetime::Static);
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::None);
    }
    SECTION("External linkage")
    {
        auto source = R"(extern int i;)";
        auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
        REQUIRE(parsing.second);

        OpenCL::Semantics::SemanticAnalysis analysis;
        auto semantics = analysis.visit(parsing.first);
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
        CHECK(declaration->getLinkage() == OpenCL::Semantics::Linkage::External);
    }
    SECTION("Various primitives")
    {
        std::array results = {
            std::pair{"char i;",OpenCL::Semantics::PrimitiveType::createChar(false,false)},
            std::pair{"signed char i;",OpenCL::Semantics::PrimitiveType::createChar(false,false)},
            std::pair{"unsigned i;",OpenCL::Semantics::PrimitiveType::createUnsignedInt(false,false)},
            std::pair{"short i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"short int i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"int short i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"signed short i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"short signed i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"signed short int i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"short signed int i;",OpenCL::Semantics::PrimitiveType::createShort(false,false)},
            std::pair{"unsigned short i;",OpenCL::Semantics::PrimitiveType::createUnsignedShort(false,false)},
            std::pair{"unsigned short int i;",OpenCL::Semantics::PrimitiveType::createUnsignedShort(false,false)},
            std::pair{"int i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"signed int i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"signed i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"unsigned int i;",OpenCL::Semantics::PrimitiveType::createUnsignedInt(false,false)},
            std::pair{"unsigned i;",OpenCL::Semantics::PrimitiveType::createUnsignedInt(false,false)},
            std::pair{"long i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"signed long i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"long int i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"signed long int i;",OpenCL::Semantics::PrimitiveType::createInt(false,false)},
            std::pair{"unsigned long i;",OpenCL::Semantics::PrimitiveType::createUnsignedInt(false,false)},
            std::pair{"unsigned long int i;",OpenCL::Semantics::PrimitiveType::createUnsignedInt(false,false)},
            std::pair{"long long i;",OpenCL::Semantics::PrimitiveType::createLongLong(false,false)},
            std::pair{"signed long long i;",OpenCL::Semantics::PrimitiveType::createLongLong(false,false)},
            std::pair{"long long int i;",OpenCL::Semantics::PrimitiveType::createLongLong(false,false)},
            std::pair{"signed long long int i;",OpenCL::Semantics::PrimitiveType::createLongLong(false,false)},
            std::pair{"unsigned long long i;",OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false,false)},
            std::pair{"unsigned long long int i;",OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false,false)},
            std::pair{"long unsigned int long i;",OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false,false)},
            std::pair{"float i;",OpenCL::Semantics::PrimitiveType::createFloat(false,false)},
            std::pair{"double i;",OpenCL::Semantics::PrimitiveType::createDouble(false,false)}
        };
        for(auto& [source,type] : results)
        {
            DYNAMIC_SECTION("Primitive: "<<source)
            {
                auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
                REQUIRE(parsing.second);

                OpenCL::Semantics::SemanticAnalysis analysis;
                auto semantics = analysis.visit(parsing.first);
                if (!semantics)
                {
                    FAIL(semantics.error().getText());
                }
                if(semantics->getGlobals().size() != 1)
                {
                    FAIL(source);
                }
                auto* declaration = std::get_if<OpenCL::Semantics::Declaration>(&semantics->getGlobals()[0]);
                REQUIRE(declaration);
                CHECK(declaration->getType() == type);
            }
        }
    }
}

TEST_CASE("Invalid primitive declarations","[semantics]")
{
    std::array sources = {
        "void i;",
        "const void i;",
        "static extern int i;",
        "auto register int i;",
        "short int long i;",
        "char char i;",
        "long short i;",
        "float int i;",
        "signed unsigned i;",
        "restrict int i;",
    };
    for(auto& source : sources)
    {
        DYNAMIC_SECTION(source)
        {
            auto parsing = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source), nullptr);
            REQUIRE(parsing.second);

            OpenCL::Semantics::SemanticAnalysis analysis;
            auto semantics = analysis.visit(parsing.first);
            REQUIRE(!semantics);
        }
    }
}

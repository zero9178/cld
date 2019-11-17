#include "catch.hpp"

#include <CompilerCore/C/ConstantEvaluator.hpp>
#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SemanticAnalysis.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include <array>

#include "TestConfig.hpp"

using namespace OpenCL::ErrorMessages;
using namespace OpenCL::ErrorMessages::Semantics;

namespace
{
    std::pair<OpenCL::Semantics::ConstRetType, std::string> evaluateConstantExpression(
        const std::string& expression,
        OpenCL::Semantics::ConstantEvaluator::Mode mode = OpenCL::Semantics::ConstantEvaluator::Integer)
    {
        std::ostringstream ss;
        OpenCL::SourceObject tokens{{}};
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(expression));
        OpenCL::Parser::Context context(tokens, &ss);
        auto ref = tokens.cbegin();
        auto parsing = OpenCL::Parser::parseConditionalExpression(ref, tokens.cend(), context);
        INFO(ss.str());
        REQUIRE((ss.str().empty()));
        OpenCL::Semantics::SemanticAnalysis analysis(&ss);
        OpenCL::Semantics::ConstantEvaluator evaluator(
            [&analysis](const OpenCL::Syntax::TypeName& typeName) -> OpenCL::Semantics::Type {
                return analysis.declaratorsToType(
                    {typeName.getSpecifierQualifiers().begin(), typeName.getSpecifierQualifiers().end()},
                    typeName.getAbstractDeclarator());
            },
            {},
            [&ss, &tokens](std::string message, std::optional<OpenCL::Modifier> modifier) {
                ss << OpenCL::Message::error(std::move(message), tokens.cbegin(), tokens.cend(), std::move(modifier));
            },
            mode);
        auto ret = evaluator.visit(parsing);
        auto string = ss.str();
        if (OpenCL::colourConsoleOutput && !string.empty())
        {
            OpenCL::Semantics::ConstantEvaluator(
                [&analysis](const OpenCL::Syntax::TypeName& typeName) -> OpenCL::Semantics::Type {
                    return analysis.declaratorsToType(
                        {typeName.getSpecifierQualifiers().begin(), typeName.getSpecifierQualifiers().end()},
                        typeName.getAbstractDeclarator());
                },
                {},
                [&tokens](std::string message, std::optional<OpenCL::Modifier> modifier) {
                    std::cerr << OpenCL::Message::error(std::move(message), tokens.cbegin(), tokens.cend(),
                                                        std::move(modifier))
                              << std::endl;
                },
                mode)
                .visit(parsing);
        }
        return {ret, string};
    }
} // namespace

TEST_CASE("Const eval Primary expression", "[constEval]")
{
    SECTION("int")
    {
        auto [value, error] = evaluateConstantExpression("0");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
        CHECK(std::get<std::int32_t>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(value.getType().getName() == "int");
    }
    SECTION("unsigned int")
    {
        auto [value, error] = evaluateConstantExpression("0u");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::uint32_t>(value.getValue()));
        CHECK(std::get<std::uint32_t>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createUnsignedInt(false, false));
        CHECK(value.getType().getName() == "unsigned int");
    }
    SECTION("long long")
    {
        auto [value, error] = evaluateConstantExpression("0ll");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::int64_t>(value.getValue()));
        CHECK(std::get<std::int64_t>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createLongLong(false, false));
        CHECK(value.getType().getName() == "long long");
    }
    SECTION("unsigned long long")
    {
        auto [value, error] = evaluateConstantExpression("0ull");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::uint64_t>(value.getValue()));
        CHECK(std::get<std::uint64_t>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
        CHECK(value.getType().getName() == "unsigned long long");
    }
    SECTION("float")
    {
        auto [value, error] = evaluateConstantExpression(".0f");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<float>(value.getValue()));
        CHECK(std::get<float>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
        CHECK(value.getType().getName() == "float");
    }
    SECTION("double")
    {
        auto [value, error] = evaluateConstantExpression(".0");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<double>(value.getValue()));
        CHECK(std::get<double>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
        CHECK(value.getType().getName() == "double");
    }
    SECTION("String literal")
    {
        auto [value, error] = evaluateConstantExpression("\"test\"");
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("String literals")));
        CHECK(value.isUndefined());
    }
    SECTION("Parenthese")
    {
        auto [value, error] = evaluateConstantExpression("(((((0)))))");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
        CHECK(std::get<std::int32_t>(value.getValue()) == 0);
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(value.getType().getName() == "int");
    }
    SECTION("Identifier")
    {
        auto [value, error] = evaluateConstantExpression("i");
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("variable access")));
        CHECK(value.isUndefined());
    }
}

TEST_CASE("Const eval postfix expression", "[constEval]")
{
    SECTION("Function call")
    {
        auto [value, error] = evaluateConstantExpression("i()");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("function call")));
    }
    SECTION("Increment")
    {
        auto [value, error] = evaluateConstantExpression("i++");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'++'")));
    }
    SECTION("Decrement")
    {
        auto [value, error] = evaluateConstantExpression("i--");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'--'")));
    }
    SECTION("Initializer")
    {
        auto [value, error] = evaluateConstantExpression("(int){0}");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("initializer")));
    }
}

TEST_CASE("Const eval unary expression", "[constEval]")
{
    SECTION("Increment")
    {
        auto [value, error] = evaluateConstantExpression("++0");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'++'")));
    }
    SECTION("Decrement")
    {
        auto [value, error] = evaluateConstantExpression("--0");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'--'")));
    }
    SECTION("Plus")
    {
        SECTION("Integer")
        {
            {
                auto [value, error] = evaluateConstantExpression("+0");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("+(char)0");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("+0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] =
                    evaluateConstantExpression("+.0", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
        }
        SECTION("Pointer")
        {
            auto [value, error] =
                evaluateConstantExpression("+(void*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args("+", "'void*'")));
        }
    }
    SECTION("Minus")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("-1");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == -1);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("-0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] =
                    evaluateConstantExpression("-.1", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == Approx(-0.1));
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
        }
        SECTION("Pointer")
        {
            auto [value, error] =
                evaluateConstantExpression("-(void*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args("-", "'void*'")));
        }
    }
    SECTION("Bitnot")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("~1u");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::uint32_t>(value.getValue()));
            CHECK(std::get<std::uint32_t>(value.getValue()) == ~1u);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createUnsignedInt(false, false));
            CHECK(value.getType().getName() == "unsigned int");
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("~0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] =
                    evaluateConstantExpression("~0.0", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error,
                           Catch::Contains(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args("~", "'double'")));
            }
        }
        SECTION("Pointer")
        {
            auto [value, error] =
                evaluateConstantExpression("~(void*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args("~", "'void*'")));
        }
    }
    SECTION("Logical not")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("!1");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("!0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] =
                    evaluateConstantExpression("!.1", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Pointer")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("!(void*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("!(void*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
    }
    SECTION("Sizeof")
    {
        SECTION("Type")
        {
            SECTION("Successful")
            {
                struct Test
                {
                    int f;
                    float r[5];
                    char c[24];
                };
                union TestU {
                    int f;
                    float r[5];
                    char c[24];
                };
                std::array sizes = {std::pair{"char", 1ull},
                                    std::pair{"unsigned char", 1ull},
                                    std::pair{"short", 2ull},
                                    std::pair{"unsigned short", 2ull},
                                    std::pair{"int", 4ull},
                                    std::pair{"long", 4ull},
                                    std::pair{"unsigned int", 4ull},
                                    std::pair{"unsigned long", 4ull},
                                    std::pair{"long long", 8ull},
                                    std::pair{"unsigned long long", 8ull},
                                    std::pair{"float", 4ull},
                                    std::pair{"double", 8ull},
                                    std::pair{"float[0]", 0ull},
                                    std::pair{"int[5]", 20ull},
                                    std::pair{"struct Test"
                                              "{"
                                              "int f;"
                                              "float r[5];"
                                              "char c[24];"
                                              "}",
                                              static_cast<unsigned long long>(sizeof(Test))},
                                    std::pair{"union Test"
                                              "{"
                                              "int f;"
                                              "float r[5];"
                                              "char c[24];"
                                              "}",
                                              static_cast<unsigned long long>(sizeof(TestU))},
                                    std::pair{"void*", 8ull},
                                    std::pair{"struct u*", 8ull}};
                for (auto& [name, size] : sizes)
                {
                    DYNAMIC_SECTION(name)
                    {
                        auto [value, error] = evaluateConstantExpression("sizeof(" + std::string(name) + ")");
                        REQUIRE(error.empty());
                        REQUIRE(!value.isUndefined());
                        REQUIRE(std::holds_alternative<std::uint64_t>(value.getValue()));
                        CHECK(std::get<std::uint64_t>(value.getValue()) == size);
                        CHECK(value.getType()
                              == OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
                        CHECK(value.getType().getName() == "unsigned long long");
                    }
                }
            }
            SECTION("Failing")
            {
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(float[])");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, Catch::Contains(INCOMPLETE_TYPE_N_IN_SIZE_OF.args("'float[]'")));
                }
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(int())");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, Catch::Contains(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF));
                }
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(int[*])");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, Catch::Contains(SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION));
                }
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(struct i)");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, Catch::Contains(INCOMPLETE_TYPE_N_IN_SIZE_OF.args("'struct i'")));
                }
            }
        }
    }
}

TEST_CASE("Const eval cast expression", "[constEval]")
{
    SECTION("Integer")
    {
        {
            auto [value, error] = evaluateConstantExpression("(unsigned long long)0");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::uint64_t>(value.getValue()));
            CHECK(std::get<std::uint64_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
            CHECK(value.getType().getName() == "unsigned long long");
        }
        {
            auto [value, error] = evaluateConstantExpression("(unsigned long long)0.5");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::uint64_t>(value.getValue()));
            CHECK(std::get<std::uint64_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
            CHECK(value.getType().getName() == "unsigned long long");
        }
    }
    SECTION("Float")
    {
        SECTION("Integer constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(float)0");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Arithmetic constant expressions")
        {
            auto [value, error] =
                evaluateConstantExpression("(float)0", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<float>(value.getValue()));
            CHECK(std::get<float>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(value.getType().getName() == "float");
        }
        SECTION("Initializer constant expressions")
        {
            auto [value, error] =
                evaluateConstantExpression("(float)(int*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args("'int*'", "'float'")));
        }
    }
    SECTION("Pointers")
    {
        SECTION("Integer constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(void*)0");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Arithmetic constant expressions")
        {
            auto [value, error] =
                evaluateConstantExpression("(void*)0", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Initializer constant expressions")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<OpenCL::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<OpenCL::Semantics::VoidStar>(value.getValue()).address == 0);
                CHECK(value.getType()
                      == OpenCL::Semantics::PointerType::create(
                          false, false, false, OpenCL::Semantics::PrimitiveType::createVoid(false, false)));
                CHECK(value.getType().getName() == "void*");
            }
            {
                auto [value, error] = evaluateConstantExpression("(float*)(void*)0",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<OpenCL::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<OpenCL::Semantics::VoidStar>(value.getValue()).address == 0);
                CHECK(value.getType()
                      == OpenCL::Semantics::PointerType::create(
                          false, false, false, OpenCL::Semantics::PrimitiveType::createFloat(false, false)));
                CHECK(value.getType().getName() == "float*");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)0.0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args("'double'", "'void*'")));
            }
        }
    }
}

TEST_CASE("Const eval term", "[constEval]")
{
    SECTION("Integer")
    {
        SECTION("Multiply")
        {
            auto [value, error] = evaluateConstantExpression("5 * 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 20);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Divide")
        {
            auto [value, error] = evaluateConstantExpression("5 / 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 1);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Rest")
        {
            auto [value, error] = evaluateConstantExpression("5 % 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 1);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Float")
    {
        SECTION("Multiply")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 * 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 * .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 * .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == 3 * .55);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
        }
        SECTION("Divide")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 / 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 / .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 / .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == 3 / .55);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
        }
        SECTION("Rest")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 % 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 % .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 % .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "%", "'int'", "'double'")));
            }
        }
    }
    SECTION("Pointer")
    {
        SECTION("Multiply")
        {
            auto [value, error] =
                evaluateConstantExpression("3 * (void*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "*", "'int'", "'void*'")));
        }
        SECTION("Divide")
        {
            auto [value, error] =
                evaluateConstantExpression("3 / (void*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "/", "'int'", "'void*'")));
        }
        SECTION("Rest")
        {
            auto [value, error] =
                evaluateConstantExpression("3 % (void*)6", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "%", "'int'", "'void*'")));
        }
    }
}

TEST_CASE("Const eval additive", "[constEval]")
{
    SECTION("Integer")
    {
        SECTION("Plus")
        {
            auto [value, error] = evaluateConstantExpression("5 + 4");
            UNSCOPED_INFO(error);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 9);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Minus")
        {
            auto [value, error] = evaluateConstantExpression("5 - 2");
            UNSCOPED_INFO(error);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 3);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Float")
    {
        SECTION("Plus")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 + 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 + .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 + .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == Approx(3.55));
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
        }
        SECTION("Minus")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 - 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 - .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 - .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == Approx(3 - .55));
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
        }
    }
    SECTION("Pointer")
    {
        SECTION("Plus")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("3 + (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<OpenCL::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<OpenCL::Semantics::VoidStar>(value.getValue()).address == 17);
                CHECK(value.getType()
                      == OpenCL::Semantics::PointerType::create(
                          false, false, false, OpenCL::Semantics::PrimitiveType::createInt(false, false)));
                CHECK(value.getType().getName() == "int*");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int*)5 + 3", OpenCL::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<OpenCL::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<OpenCL::Semantics::VoidStar>(value.getValue()).address == 17);
                CHECK(value.getType()
                      == OpenCL::Semantics::PointerType::create(
                          false, false, false, OpenCL::Semantics::PrimitiveType::createInt(false, false)));
                CHECK(value.getType().getName() == "int*");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3.0 + (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "+", "'double'", "'int*'")));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 + (int*)3",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "+", "'int*'", "'int*'")));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int*)5 + 3.0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "+", "'int*'", "'double'")));
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)5 + 3",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args("'struct i'")));
            }
        }
        SECTION("Minus")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("3 - (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "-", "'int'", "'int*'")));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int*)5 - 3", OpenCL::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<OpenCL::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<OpenCL::Semantics::VoidStar>(value.getValue()).address == -7);
                CHECK(value.getType()
                      == OpenCL::Semantics::PointerType::create(
                          false, false, false, OpenCL::Semantics::PrimitiveType::createInt(false, false)));
                CHECK(value.getType().getName() == "int*");
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)20 - (int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int64_t>(value.getValue()));
                CHECK(std::get<std::int64_t>(value.getValue()) == 4);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createLongLong(false, false));
                CHECK(value.getType().getName() == "long long");
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)3 - (struct i*)5",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args("'struct i'")));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3.0 - (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "-", "'double'", "'int*'")));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int*)5 - 3.0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "-", "'int*'", "'double'")));
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)5 - 3",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args("'struct i'")));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int* const)20 - (int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int64_t>(value.getValue()));
                CHECK(std::get<std::int64_t>(value.getValue()) == 4);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createLongLong(false, false));
                CHECK(value.getType().getName() == "long long");
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)20 - (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int64_t>(value.getValue()));
                CHECK(std::get<std::int64_t>(value.getValue()) == 4);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createLongLong(false, false));
                CHECK(value.getType().getName() == "long long");
            }
        }
    }
}

TEST_CASE("Const eval shift", "[constEval]")
{
    SECTION("Integer")
    {
        SECTION("Left")
        {
            auto [value, error] = evaluateConstantExpression("5 << 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 5 << 4);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Right")
        {
            auto [value, error] = evaluateConstantExpression("5 >> 2");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 5 >> 2);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Float")
    {
        SECTION("Left")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 << 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 << .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 << .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "<<", "'int'", "'double'")));
            }
        }
        SECTION("Right")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 >> 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 >> .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 >> .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      ">>", "'int'", "'double'")));
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Left")
        {
            auto [value, error] =
                evaluateConstantExpression("3 << (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "<<", "'int'", "'int*'")));
        }
        SECTION("Right")
        {
            auto [value, error] =
                evaluateConstantExpression("3 >> (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  ">>", "'int'", "'int*'")));
        }
    }
}

TEST_CASE("Const eval bitand", "[constEval]")
{
    SECTION("Integer")
    {
        auto [value, error] = evaluateConstantExpression("5 & 4");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
        CHECK(std::get<std::int32_t>(value.getValue()) == (5 & 4));
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(value.getType().getName() == "int");
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 & 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 & .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] =
                evaluateConstantExpression("3 & .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "&", "'int'", "'double'")));
        }
    }
    SECTION("Pointers")
    {
        auto [value, error] =
            evaluateConstantExpression("3 & (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(
                              CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args("&", "'int'", "'int*'")));
    }
}

TEST_CASE("Const eval bitxor", "[constEval]")
{
    SECTION("Integer")
    {
        auto [value, error] = evaluateConstantExpression("5 ^ 4");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
        CHECK(std::get<std::int32_t>(value.getValue()) == (5 ^ 4));
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(value.getType().getName() == "int");
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 ^ 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 ^ .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] =
                evaluateConstantExpression("3 ^ .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "^", "'int'", "'double'")));
        }
    }
    SECTION("Pointers")
    {
        auto [value, error] =
            evaluateConstantExpression("3 ^ (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(
                              CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args("^", "'int'", "'int*'")));
    }
}

TEST_CASE("Const eval bitor", "[constEval]")
{
    SECTION("Integer")
    {
        auto [value, error] = evaluateConstantExpression("5 | 4");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
        CHECK(std::get<std::int32_t>(value.getValue()) == (5 | 4));
        CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
        CHECK(value.getType().getName() == "int");
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 | 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 | .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] =
                evaluateConstantExpression("3 | .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "|", "'int'", "'double'")));
        }
    }
    SECTION("Pointers")
    {
        auto [value, error] =
            evaluateConstantExpression("3 | (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(
                              CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args("|", "'int'", "'int*'")));
    }
}

TEST_CASE("Const eval and", "[constEval]")
{
    SECTION("Integer")
    {
        {
            auto [value, error] = evaluateConstantExpression("5 && 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] = evaluateConstantExpression("5 && 0");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 && 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 && .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] =
                evaluateConstantExpression("3 && .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] =
                evaluateConstantExpression("0 && .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Pointers")
    {
        {
            auto [value, error] =
                evaluateConstantExpression("3 && (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] =
                evaluateConstantExpression("0 && (int*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
}

TEST_CASE("Const eval or", "[constEval]")
{
    SECTION("Integer")
    {
        {
            auto [value, error] = evaluateConstantExpression("5 || 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || 0");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 || 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 || .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] =
                evaluateConstantExpression("3 || .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] =
                evaluateConstantExpression("0 || .0", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Pointer")
    {
        {
            auto [value, error] =
                evaluateConstantExpression("3 || (void*)5", OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] =
                evaluateConstantExpression("0 || (void*)0", OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
}

TEST_CASE("Const eval comparison", "[constEval]")
{
    SECTION("Integer")
    {
        SECTION("Less than")
        {
            auto [value, error] = evaluateConstantExpression("5 < 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Greater than")
        {
            auto [value, error] = evaluateConstantExpression("5 > 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Less than or equal")
        {
            auto [value, error] = evaluateConstantExpression("5 <= 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("5 >= 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
    SECTION("Float")
    {
        SECTION("Less than")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 < 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 < .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 < .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Greater than")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 > 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 > .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 > .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Less than or equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 <= 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 <= .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 <= .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Greater than or equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 >= 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 >= .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 >= .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Less than")
        {
            auto [value, error] = evaluateConstantExpression("(void*)3 < (void*)55",
                                                             OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Greater than")
        {
            auto [value, error] = evaluateConstantExpression("(int* const)3 > (const int*)55",
                                                             OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Less than or equal")
        {
            auto [value, error] = evaluateConstantExpression("(struct i*)3 <= (struct i*)55",
                                                             OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("(float*)3 >= (float*)55",
                                                             OpenCL::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
    }
}

TEST_CASE("Const eval equal", "[constEval]")
{
    SECTION("Integer")
    {
        SECTION("Equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("5 == 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("4 == 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("5 != 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("4 != 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
    }
    SECTION("Float")
    {
        SECTION("Equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 == 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 == .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 == .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression(".55 == .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 != 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 != .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("3 != .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression(".55 != .55", OpenCL::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("(int* const)5 == (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(int* const)4 == (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)5 == (struct i*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(int* const)5 == (void*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)5 == (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(float*)5 == (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error,
                           Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N.args(
                               "==", "'float*'", "'int const*'")));
            }
            {
                auto [value, error] = evaluateConstantExpression("0 == (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)5 == 0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("5 == (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER));
            }
            {
                auto [value, error] = evaluateConstantExpression("(const int*)5 == 4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER));
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("(int* const)5 != (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(int* const)4 != (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)5 != (struct i*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(int* const)5 != (void*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)5 != (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("(float*)5 != (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error,
                           Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N.args(
                               "!=", "'float*'", "'int const*'")));
            }
            {
                auto [value, error] = evaluateConstantExpression("0 != (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)5 != 0", OpenCL::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression("5 != (const int*)4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER));
            }
            {
                auto [value, error] = evaluateConstantExpression("(const int*)5 != 4",
                                                                 OpenCL::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER));
            }
        }
    }
}

TEST_CASE("Const eval expression", "[constEval]")
{
    auto [value, error] = evaluateConstantExpression("(.55 , 3)");
    CHECK(value.isUndefined());
    CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("','")));
}

TEST_CASE("Const eval assignments", "[constEval]")
{
    std::array assignmentOperators = {"=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="};
    for (auto& iter : assignmentOperators)
    {
        auto [value, error] = evaluateConstantExpression("(.55 " + std::string(iter) + " 3)");
        CHECK(value.isUndefined());
        CHECK_THAT(error, Catch::Contains(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args('\'' + std::string(iter) + '\'')));
    }
}

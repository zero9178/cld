#include "catch.hpp"

#include <CompilerCore/C/ConstantEvaluator.hpp>
#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SemanticAnalysis.hpp>

#include <array>

#include "TestConfig.hpp"

using namespace OpenCL::ErrorMessages;
using namespace OpenCL::ErrorMessages::Semantics;

namespace
{
    enum ConstantExpression
    {
        Integer,
        Arithmetic,
        Initialization
    };

    std::pair<OpenCL::Semantics::ConstRetType, std::string>
        evaluateConstantExpression(const std::string& expression, ConstantExpression constantExpression = Integer)
    {
        std::ostringstream ss;
        std::vector<OpenCL::Lexer::Token> tokens;
        REQUIRE_NOTHROW(tokens = OpenCL::Lexer::tokenize(expression));
        OpenCL::Parser::ParsingContext context(&ss);
        auto ref = tokens.cbegin();
        auto ds = context.setDiagnosticStart(ref);
        auto parsing = OpenCL::Parser::parseConditionalExpression(ref, tokens.cend(), context);
        INFO(ss.str());
        REQUIRE((ss.str().empty() && parsing));
        OpenCL::Semantics::SemanticAnalysis analysis(&ss);
        OpenCL::Semantics::ConstantEvaluator evaluator(
            tokens.begin(), tokens.end(),
            [&analysis](const OpenCL::Syntax::TypeName& typeName) -> OpenCL::Semantics::Type {
                return analysis.declaratorsToType(
                    {typeName.getSpecifierQualifiers().begin(), typeName.getSpecifierQualifiers().end()},
                    typeName.getAbstractDeclarator());
            },
            {}, [&ss](const OpenCL::Message& message) { ss << message; }, constantExpression == Integer);
        auto ret = evaluator.visit(*parsing);
        auto string = ss.str();
        if (OpenCL::colourConsoleOutput && !string.empty())
        {
            OpenCL::Semantics::ConstantEvaluator(
                tokens.begin(), tokens.end(),
                [&analysis](const OpenCL::Syntax::TypeName& typeName) -> OpenCL::Semantics::Type {
                    return analysis.declaratorsToType(
                        {typeName.getSpecifierQualifiers().begin(), typeName.getSpecifierQualifiers().end()},
                        typeName.getAbstractDeclarator());
                },
                {}, [](const OpenCL::Message& message) { std::cerr << message; }, constantExpression == Integer)
                .visit(*parsing);
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
                auto [value, error] = evaluateConstantExpression("+.0", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
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
                auto [value, error] = evaluateConstantExpression("-.1", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == Approx(-0.1));
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
            }
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
            auto [value, error] = evaluateConstantExpression("~0.0", Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args("~", "'double'")));
        }
    }
    SECTION("Minus")
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
                auto [value, error] = evaluateConstantExpression("!.1", Arithmetic);
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
                std::array<std::pair<const char*, std::uint64_t>, 16> sizes = {{{"char", 1},
                                                                                {"unsigned char", 1},
                                                                                {"short", 2},
                                                                                {"unsigned short", 2},
                                                                                {"int", 4},
                                                                                {"long", 4},
                                                                                {"unsigned int", 4},
                                                                                {"unsigned long", 4},
                                                                                {"long long", 8},
                                                                                {"unsigned long long", 8},
                                                                                {"float", 4},
                                                                                {"double", 8},
                                                                                {"float[0]", 0},
                                                                                {"int[5]", 20},
                                                                                {"struct Test"
                                                                                 "{"
                                                                                 "int f;"
                                                                                 "float r[5];"
                                                                                 "char c[24];"
                                                                                 "}",
                                                                                 sizeof(Test)},
                                                                                {"union Test"
                                                                                 "{"
                                                                                 "int f;"
                                                                                 "float r[5];"
                                                                                 "char c[24];"
                                                                                 "}",
                                                                                 sizeof(TestU)}}};
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
        {
            auto [value, error] = evaluateConstantExpression("(float)0");
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        {
            auto [value, error] = evaluateConstantExpression("(float)0", Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<float>(value.getValue()));
            CHECK(std::get<float>(value.getValue()) == 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createFloat(false, false));
            CHECK(value.getType().getName() == "float");
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
                auto [value, error] = evaluateConstantExpression("3 * .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 / .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 % .55", Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "%", "'int'", "'double'")));
            }
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
                auto [value, error] = evaluateConstantExpression("3 + .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 - .55", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<double>(value.getValue()));
                CHECK(std::get<double>(value.getValue()) == Approx(3 - .55));
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createDouble(false, false));
                CHECK(value.getType().getName() == "double");
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
                auto [value, error] = evaluateConstantExpression("3 >> .55", Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      ">>", "'int'", "'double'")));
            }
        }
        SECTION("Right")
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
                auto [value, error] = evaluateConstantExpression("3 << .55", Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                      "<<", "'int'", "'double'")));
            }
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
            auto [value, error] = evaluateConstantExpression("3 & .55", Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "&", "'int'", "'double'")));
        }
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
            auto [value, error] = evaluateConstantExpression("3 ^ .55", Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "^", "'int'", "'double'")));
        }
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
            auto [value, error] = evaluateConstantExpression("3 | .55", Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, Catch::Contains(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "|", "'int'", "'double'")));
        }
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
            auto [value, error] = evaluateConstantExpression("3 && .55", Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] = evaluateConstantExpression("0 && .55", Arithmetic);
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
            auto [value, error] = evaluateConstantExpression("3 || .55", Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
            CHECK(std::get<std::int32_t>(value.getValue()) != 0);
            CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
            CHECK(value.getType().getName() == "int");
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || .0", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 < .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 > .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 <= .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 >= .55", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
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
                auto [value, error] = evaluateConstantExpression("3 == .55", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression(".55 == .55", Arithmetic);
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
                auto [value, error] = evaluateConstantExpression("3 != .55", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) != 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
            }
            {
                auto [value, error] = evaluateConstantExpression(".55 != .55", Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<std::int32_t>(value.getValue()));
                CHECK(std::get<std::int32_t>(value.getValue()) == 0);
                CHECK(value.getType() == OpenCL::Semantics::PrimitiveType::createInt(false, false));
                CHECK(value.getType().getName() == "int");
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

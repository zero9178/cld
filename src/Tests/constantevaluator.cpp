#include "catch.hpp"

#include <Frontend/Compiler/ConstantEvaluator.hpp>
#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Compiler/Parser.hpp>
#include <Frontend/Compiler/SemanticAnalysis.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

#include "TestConfig.hpp"

using namespace cld::Errors;
using namespace cld::Errors::Semantics;
using namespace cld::Warnings::Semantics;

namespace
{
std::pair<cld::Semantics::ConstRetType, std::string> evaluateConstantExpression(
    const std::string& expression, const cld::LanguageOptions& options = cld::LanguageOptions::native(),
    cld::Semantics::ConstantEvaluator::Mode mode = cld::Semantics::ConstantEvaluator::Integer)
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    auto tokens = cld::Lexer::tokenize(expression, options);
    REQUIRE(!tokens.data().empty());
    auto ctokens = cld::Lexer::toCTokens(tokens);
    cld::Parser::Context context(ctokens, &ss);
    const auto* ref = std::as_const(ctokens).data().data();
    auto parsing = cld::Parser::parseConditionalExpression(ref, ctokens.data().data() + ctokens.data().size(), context);
    UNSCOPED_INFO(ss.str());
    REQUIRE((ss.str().empty()));
    cld::Semantics::SemanticAnalysis analysis(ctokens, &ss);
    cld::Semantics::ConstantEvaluator evaluator(
        ctokens, {}, &analysis, [&ss](const cld::Message& message) { ss << message; }, mode);
    auto ret = evaluator.visit(parsing);
    auto string = ss.str();
    if (!string.empty())
    {
        cld::Semantics::ConstantEvaluator(
            ctokens, {}, &analysis, [](const cld::Message& message) { llvm::errs() << message << '\n'; }, mode)
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
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
        CHECK(value.getType()
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("unsigned int")
    {
        auto [value, error] = evaluateConstantExpression("0u");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isUnsigned());
        CHECK(result.getBitWidth() == sizeof(unsigned int) * 8);
        CHECK(value.getType()
              == cld::Semantics::PrimitiveType::createUnsignedInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("long")
    {
        auto [value, error] = evaluateConstantExpression("0l", cld::Tests::x86linux);
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == 32);
        CHECK(value.getType() == cld::Semantics::PrimitiveType::createLong(false, false, cld::Tests::x86linux));
    }
    SECTION("unsigned long")
    {
        auto [value, error] = evaluateConstantExpression("0ul", cld::Tests::x86linux);
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isUnsigned());
        CHECK(result.getBitWidth() == 32);
        CHECK(value.getType() == cld::Semantics::PrimitiveType::createUnsignedLong(false, false, cld::Tests::x86linux));
    }
    SECTION("long long")
    {
        auto [value, error] = evaluateConstantExpression("0ll", cld::Tests::x86linux);
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == 64);
        CHECK(value.getType() == cld::Semantics::PrimitiveType::createLongLong(false, false));
    }
    SECTION("unsigned long long")
    {
        auto [value, error] = evaluateConstantExpression("0ull", cld::Tests::x86linux);
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isUnsigned());
        CHECK(result.getBitWidth() == 64);
        CHECK(value.getType() == cld::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
    }
    SECTION("float")
    {
        auto [value, error] = evaluateConstantExpression(".0f");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
        auto result = std::get<llvm::APFloat>(value.getValue());
        CHECK(result.convertToFloat() == 0.f);
        CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
        CHECK(value.getType() == cld::Semantics::PrimitiveType::createFloat(false, false));
    }
    SECTION("double")
    {
        auto [value, error] = evaluateConstantExpression(".0");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
        auto result = std::get<llvm::APFloat>(value.getValue());
        CHECK(result.convertToDouble() == 0.0);
        CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
        CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
    }
    SECTION("long double")
    {
        SECTION("Msvc")
        {
            auto [value, error] = evaluateConstantExpression(".0l", cld::Tests::x64windowsMsvc);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToDouble() == 0.0);
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createLongDouble(false, false, cld::Tests::x64windowsMsvc));
        }
        SECTION("Gnu")
        {
            auto [value, error] = evaluateConstantExpression(".0l", cld::Tests::x64windowsGnu);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            REQUIRE(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_x87DoubleExtended);
            CHECK(result.compare(llvm::APFloat(llvm::APFloat::x87DoubleExtended(), "0.0")));
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createLongDouble(false, false, cld::Tests::x64windowsGnu));
        }
    }
    SECTION("String literal")
    {
        auto [value, error] = evaluateConstantExpression("\"test\"");
        CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "String literals"));
        CHECK(value.isUndefined());
    }
    SECTION("Parentheses")
    {
        auto [value, error] = evaluateConstantExpression("(((((0)))))");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
        CHECK(value.getType()
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("Identifier")
    {
        auto [value, error] = evaluateConstantExpression("i");
        CHECK_THAT(error, ProducesError(VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
        CHECK(value.isUndefined());
    }
}

TEST_CASE("Const eval postfix expression", "[constEval]")
{
    SECTION("Function call")
    {
        auto [value, error] = evaluateConstantExpression("i()");
        CHECK(value.isUndefined());
        CHECK_THAT(error, ProducesError(FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    }
    SECTION("Increment")
    {
        auto [value, error] = evaluateConstantExpression("i++");
        CHECK(value.isUndefined());
        CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "'++'"));
    }
    SECTION("Decrement")
    {
        auto [value, error] = evaluateConstantExpression("i--");
        CHECK(value.isUndefined());
        CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "'--'"));
    }
    SECTION("Initializer")
    {
        auto [value, error] = evaluateConstantExpression("(int){0}");
        CHECK(value.isUndefined());
        CHECK_THAT(error, ProducesError(INITIALIZER_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    }
}

TEST_CASE("Const eval unary expression", "[constEval]")
{
    SECTION("Increment")
    {
        auto [value, error] = evaluateConstantExpression("++0");
        CHECK(value.isUndefined());
        CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "'++'"));
    }
    SECTION("Decrement")
    {
        auto [value, error] = evaluateConstantExpression("--0");
        CHECK(value.isUndefined());
        CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "'--'"));
    }
    SECTION("Plus")
    {
        SECTION("Integer")
        {
            {
                auto [value, error] = evaluateConstantExpression("+0");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("+(signed char)0");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("+(unsigned char)0");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("+0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("+.0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 0.0);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
        }
        SECTION("Pointer")
        {
            auto [value, error] = evaluateConstantExpression("+(void*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N, "'+'", "'void*'"));
        }
    }
    SECTION("Minus")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("-1");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == -1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("-0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("-.1", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == -.1);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
        }
        SECTION("Pointer")
        {
            auto [value, error] = evaluateConstantExpression("-(void*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N, "'-'", "'void*'"));
        }
    }
    SECTION("Bitnot")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("~1u");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == ~1u);
            CHECK(result.isUnsigned());
            CHECK(result.getBitWidth() == sizeof(unsigned int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createUnsignedInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("~0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("~0.0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N, "'~'", "'double'"));
            }
        }
        SECTION("Pointer")
        {
            auto [value, error] = evaluateConstantExpression("~(void*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N, "'~'", "'void*'"));
        }
    }
    SECTION("Logical not")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("!1");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Floating point")
        {
            SECTION("Integer constant expression")
            {
                auto [value, error] = evaluateConstantExpression("!0.0");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("!.1", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Pointer")
        {
            {
                auto [value, error] = evaluateConstantExpression("!(void*)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("!(void*)5", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
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
                union TestU
                {
                    int f;
                    float r[5];
                    char c[24];
                };
#define SIZEPAIR(type) std::pair{#type, sizeof(type)}
                std::array sizes = {SIZEPAIR(char),
                                    SIZEPAIR(unsigned char),
                                    SIZEPAIR(short),
                                    SIZEPAIR(unsigned short),
                                    SIZEPAIR(int),
                                    SIZEPAIR(long),
                                    SIZEPAIR(unsigned int),
                                    SIZEPAIR(unsigned long),
                                    SIZEPAIR(long long),
                                    SIZEPAIR(unsigned long long),
                                    SIZEPAIR(float),
                                    SIZEPAIR(double),
                                    SIZEPAIR(long double),
                                    SIZEPAIR(int[5]),
                                    std::pair{"struct Test"
                                              "{"
                                              "int f;"
                                              "float r[5];"
                                              "char c[24];"
                                              "}",
                                              sizeof(Test)},
                                    std::pair{"struct"
                                              "{"
                                              "int f;"
                                              "float r[5];"
                                              "char c[24];"
                                              "}",
                                              sizeof(Test)},
                                    std::pair{"union Test"
                                              "{"
                                              "int f;"
                                              "float r[5];"
                                              "char c[24];"
                                              "}",
                                              sizeof(TestU)},
                                    std::pair{"union"
                                              "{"
                                              "int f;"
                                              "float r[5];"
                                              "char c[24];"
                                              "}",
                                              sizeof(TestU)},
                                    SIZEPAIR(void*),
                                    SIZEPAIR(struct u*)};
#undef SIZEPAIR
                for (auto& [name, size] : sizes)
                {
                    DYNAMIC_SECTION(name)
                    {
                        auto [value, error] = evaluateConstantExpression("sizeof(" + std::string(name) + ")");
                        REQUIRE(error.empty());
                        REQUIRE(!value.isUndefined());
                        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                        auto result = std::get<llvm::APSInt>(value.getValue());
                        CHECK(result == size);
                        CHECK(result.isUnsigned());
                        CHECK(result.getBitWidth() == sizeof(unsigned long long) * 8);
                        CHECK(value.getType() == cld::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
                    }
                }
            }
            SECTION("Failing")
            {
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(float[])");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, ProducesError(INCOMPLETE_TYPE_N_IN_SIZE_OF, "'float[]'"));
                }
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(int())");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, ProducesError(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF));
                }
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(int[*])");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error,
                               ProducesError(SIZEOF_VAL_MODIFIED_TYPE_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION));
                }
                {
                    auto [value, error] = evaluateConstantExpression("sizeof(struct i)");
                    CHECK(value.isUndefined());
                    CHECK_THAT(error, ProducesError(INCOMPLETE_TYPE_N_IN_SIZE_OF, "'struct i'"));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isUnsigned());
            CHECK(result.getBitWidth() == sizeof(unsigned long long) * 8);
            CHECK(value.getType() == cld::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
        }
        {
            auto [value, error] = evaluateConstantExpression("(unsigned long long)0.5");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isUnsigned());
            CHECK(result.getBitWidth() == sizeof(unsigned long long) * 8);
            CHECK(value.getType() == cld::Semantics::PrimitiveType::createUnsignedLongLong(false, false));
        }
    }
    SECTION("Float")
    {
        SECTION("Integer constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(float)0");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Arithmetic constant expressions")
        {
            SECTION("From int")
            {
                auto [value, error] = evaluateConstantExpression("(float)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToFloat() == 0.f);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createFloat(false, false));
            }
            SECTION("From float")
            {
                auto [value, error] = evaluateConstantExpression("(float)5.3", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToFloat() == 5.3f);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createFloat(false, false));
            }
        }
        SECTION("Initializer constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(float)(int*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(INVALID_CAST_FROM_TYPE_N_TO_TYPE_N, "'int*'", "'float'"));
        }
        SECTION("To Infinity")
        {
            auto [value, error] = evaluateConstantExpression(
                "(float)" + std::to_string(std::numeric_limits<double>::max()), cld::LanguageOptions::native(),
                cld::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToFloat() == std::numeric_limits<float>::infinity());
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
            CHECK(value.getType() == cld::Semantics::PrimitiveType::createFloat(false, false));
        }
        SECTION("Warnings")
        {
            SECTION("To Integer")
            {
                auto [value, error] =
                    evaluateConstantExpression("(long long)3.40282347E+38f", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Arithmetic);
                CHECK_THAT(error,
                           ProducesWarning(VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N, "3.40282347E+38", "'long long'"));
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Integer constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(void*)0");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Arithmetic constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(void*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Initializer constant expressions")
        {
            {
                auto [value, error] = evaluateConstantExpression("(void*)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 0);
                CHECK(value.getType()
                      == cld::Semantics::PointerType::create(false, false, false,
                                                             cld::Semantics::PrimitiveType::createVoid(false, false)));
            }
            {
                auto [value, error] = evaluateConstantExpression("(float*)(void*)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 0);
                CHECK(value.getType()
                      == cld::Semantics::PointerType::create(false, false, false,
                                                             cld::Semantics::PrimitiveType::createFloat(false, false)));
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)0.0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INVALID_CAST_FROM_TYPE_N_TO_TYPE_N, "'double'", "'void*'"));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 20);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Divide")
        {
            auto [value, error] = evaluateConstantExpression("5 / 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Modulo")
        {
            auto [value, error] = evaluateConstantExpression("5 % 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Float")
    {
        SECTION("Multiply")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 * 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 * .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 * .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3 * .55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
            {
                auto [value, error] = evaluateConstantExpression("3.0f * .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3 * .55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
        }
        SECTION("Divide")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 / 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 / .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 / .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3 / .55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
        }
        SECTION("Rest")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 % 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 % .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 % .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'%'",
                                                "'int'", "'double'"));
            }
        }
    }
    SECTION("Pointer")
    {
        SECTION("Multiply")
        {
            auto [value, error] = evaluateConstantExpression("3 * (void*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'*'", "'int'",
                                            "'void*'"));
        }
        SECTION("Divide")
        {
            auto [value, error] = evaluateConstantExpression("3 / (void*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'/'", "'int'",
                                            "'void*'"));
        }
        SECTION("Rest")
        {
            auto [value, error] = evaluateConstantExpression("3 % (void*)6", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'%'", "'int'",
                                            "'void*'"));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 9);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Minus")
        {
            auto [value, error] = evaluateConstantExpression("5 - 2");
            UNSCOPED_INFO(error);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 3);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Float")
    {
        SECTION("Plus")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 + 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 + .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 + .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3.55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
        }
        SECTION("Minus")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 - 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 - .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 - .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3 - .55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
                CHECK(value.getType() == cld::Semantics::PrimitiveType::createDouble(false, false));
            }
        }
    }
    SECTION("Pointer")
    {
        SECTION("Plus")
        {
            {
                auto [value, error] = evaluateConstantExpression("3 + (int*)5", cld::Tests::x64linux,
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 17);
                CHECK(value.getType()
                      == cld::Semantics::PointerType::create(
                          false, false, false,
                          cld::Semantics::PrimitiveType::createInt(false, false, cld::Tests::x64linux)));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 + 3", cld::Tests::x64linux,
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 17);
                CHECK(value.getType()
                      == cld::Semantics::PointerType::create(
                          false, false, false,
                          cld::Semantics::PrimitiveType::createInt(false, false, cld::Tests::x64linux)));
            }
            {
                auto [value, error] = evaluateConstantExpression("3.0 + (int*)5", cld::Tests::x64linux,
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'+'",
                                                "'double'", "'int*'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 + (int*)3", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'+'",
                                                "'int*'", "'int*'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 + 3.0", cld::Tests::x64linux,
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'+'",
                                                "'int*'", "'double'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)5 + 3", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct i'"));
            }
        }
        SECTION("Minus")
        {
            SECTION("64 bit")
            {
                {
                    auto [value, error] = evaluateConstantExpression("(int*)20 - (int*)4", cld::Tests::x64linux,
                                                                     cld::Semantics::ConstantEvaluator::Initialization);
                    UNSCOPED_INFO(error);
                    REQUIRE(error.empty());
                    REQUIRE(!value.isUndefined());
                    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                    auto result = std::get<llvm::APSInt>(value.getValue());
                    CHECK(result == 4);
                    CHECK(result.isSigned());
                    CHECK(result.getBitWidth() == 64);
                    CHECK(value.getType()
                          == cld::Semantics::PrimitiveType::createLong(false, false, cld::Tests::x64linux));
                }
                {
                    auto [value, error] = evaluateConstantExpression("(int* const)20 - (int*)4", cld::Tests::x64linux,
                                                                     cld::Semantics::ConstantEvaluator::Initialization);
                    UNSCOPED_INFO(error);
                    REQUIRE(error.empty());
                    REQUIRE(!value.isUndefined());
                    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                    auto result = std::get<llvm::APSInt>(value.getValue());
                    CHECK(result == 4);
                    CHECK(result.isSigned());
                    CHECK(result.getBitWidth() == 64);
                    CHECK(value.getType()
                          == cld::Semantics::PrimitiveType::createLong(false, false, cld::Tests::x64linux));
                }
                {
                    auto [value, error] = evaluateConstantExpression("(int*)20 - (const int*)4", cld::Tests::x64linux,
                                                                     cld::Semantics::ConstantEvaluator::Initialization);
                    UNSCOPED_INFO(error);
                    REQUIRE(error.empty());
                    REQUIRE(!value.isUndefined());
                    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                    auto result = std::get<llvm::APSInt>(value.getValue());
                    CHECK(result == 4);
                    CHECK(result.isSigned());
                    CHECK(result.getBitWidth() == 64);
                    CHECK(value.getType()
                          == cld::Semantics::PrimitiveType::createLong(false, false, cld::Tests::x64linux));
                }
            }
            SECTION("32 bit")
            {
                [] {
                    auto [value, error] = evaluateConstantExpression("(int*)20 - (int*)4", cld::Tests::x86linux,
                                                                     cld::Semantics::ConstantEvaluator::Initialization);
                    UNSCOPED_INFO(error);
                    REQUIRE(error.empty());
                    REQUIRE(!value.isUndefined());
                    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                    auto result = std::get<llvm::APSInt>(value.getValue());
                    CHECK(result == 4);
                    CHECK(result.isSigned());
                    CHECK(result.getBitWidth() == 32);
                    CHECK(value.getType()
                          == cld::Semantics::PrimitiveType::createInt(false, false, cld::Tests::x86linux));
                }();
                {
                    auto [value, error] = evaluateConstantExpression("(int* const)20 - (int*)4", cld::Tests::x86linux,
                                                                     cld::Semantics::ConstantEvaluator::Initialization);
                    UNSCOPED_INFO(error);
                    REQUIRE(error.empty());
                    REQUIRE(!value.isUndefined());
                    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                    auto result = std::get<llvm::APSInt>(value.getValue());
                    CHECK(result == 4);
                    CHECK(result.isSigned());
                    CHECK(result.getBitWidth() == 32);
                    CHECK(value.getType()
                          == cld::Semantics::PrimitiveType::createInt(false, false, cld::Tests::x86linux));
                }
                {
                    auto [value, error] = evaluateConstantExpression("(int*)20 - (const int*)4", cld::Tests::x86linux,
                                                                     cld::Semantics::ConstantEvaluator::Initialization);
                    UNSCOPED_INFO(error);
                    REQUIRE(error.empty());
                    REQUIRE(!value.isUndefined());
                    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                    auto result = std::get<llvm::APSInt>(value.getValue());
                    CHECK(result == 4);
                    CHECK(result.isSigned());
                    CHECK(result.getBitWidth() == 32);
                    CHECK(value.getType()
                          == cld::Semantics::PrimitiveType::createInt(false, false, cld::Tests::x86linux));
                }
            }
            {
                auto [value, error] = evaluateConstantExpression("3 - (int*)5", cld::Tests::x64linux,
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'-'",
                                                "'int'", "'int*'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 - 3", cld::Tests::x64linux,
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == std::uint64_t(-7));
                CHECK(value.getType()
                      == cld::Semantics::PointerType::create(
                          false, false, false,
                          cld::Semantics::PrimitiveType::createInt(false, false, cld::Tests::x64linux)));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(struct i*)3 - (struct i*)5", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct i'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("3.0 - (int*)5", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'-'",
                                                "'double'", "'int*'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 - 3.0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'-'",
                                                "'int*'", "'double'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("(struct i*)5 - 3", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "'struct i'"));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 5 << 4);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Right")
        {
            auto [value, error] = evaluateConstantExpression("5 >> 2");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 5 >> 2);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Float")
    {
        SECTION("Left")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 << 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 << .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 << .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'<<'",
                                                "'int'", "'double'"));
            }
        }
        SECTION("Right")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 >> 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 >> .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 >> .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'>>'",
                                                "'int'", "'double'"));
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Left")
        {
            auto [value, error] = evaluateConstantExpression("3 << (int*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'<<'", "'int'",
                                            "'int*'"));
        }
        SECTION("Right")
        {
            auto [value, error] = evaluateConstantExpression("3 >> (int*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'>>'", "'int'",
                                            "'int*'"));
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
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == (5 & 4));
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
        CHECK(value.getType()
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 & 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 & .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 & .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'&'", "'int'",
                                            "'double'"));
        }
    }
    SECTION("Pointers")
    {
        auto [value, error] = evaluateConstantExpression("3 & (int*)5", cld::LanguageOptions::native(),
                                                         cld::Semantics::ConstantEvaluator::Initialization);
        CHECK(value.isUndefined());
        CHECK_THAT(error,
                   ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'&'", "'int'", "'int*'"));
    }
}

TEST_CASE("Const eval bitxor", "[constEval]")
{
    SECTION("Integer")
    {
        auto [value, error] = evaluateConstantExpression("5 ^ 4");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == (5 ^ 4));
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
        CHECK(value.getType()
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 ^ 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 ^ .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 ^ .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'^'", "'int'",
                                            "'double'"));
        }
    }
    SECTION("Pointers")
    {
        auto [value, error] = evaluateConstantExpression("3 ^ (int*)5", cld::LanguageOptions::native(),
                                                         cld::Semantics::ConstantEvaluator::Initialization);
        CHECK(value.isUndefined());
        CHECK_THAT(error,
                   ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'^'", "'int'", "'int*'"));
    }
}

TEST_CASE("Const eval bitor", "[constEval]")
{
    SECTION("Integer")
    {
        auto [value, error] = evaluateConstantExpression("5 | 4");
        REQUIRE(error.empty());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == (5 | 4));
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
        CHECK(value.getType()
              == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 | 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 | .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 | .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'|'", "'int'",
                                            "'double'"));
        }
    }
    SECTION("Pointers")
    {
        auto [value, error] = evaluateConstantExpression("3 | (int*)5", cld::LanguageOptions::native(),
                                                         cld::Semantics::ConstantEvaluator::Initialization);
        CHECK(value.isUndefined());
        CHECK_THAT(error,
                   ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'|'", "'int'", "'int*'"));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            auto [value, error] = evaluateConstantExpression("5 && 0");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 && 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 && .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 && .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            auto [value, error] = evaluateConstantExpression("0 && .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Pointers")
    {
        {
            auto [value, error] = evaluateConstantExpression("3 && (int*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            auto [value, error] = evaluateConstantExpression("0 && (int*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || 0");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression(".55 || 3");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 || .55");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        {
            auto [value, error] = evaluateConstantExpression("3 || .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || .0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Arithmetic);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Pointer")
    {
        {
            auto [value, error] = evaluateConstantExpression("3 || (void*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || (void*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
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
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Greater than")
        {
            auto [value, error] = evaluateConstantExpression("5 > 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Less than or equal")
        {
            auto [value, error] = evaluateConstantExpression("5 <= 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("5 >= 4");
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
    }
    SECTION("Float")
    {
        SECTION("Less than")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 < 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 < .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 < .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Greater than")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 > 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 > .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 > .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Less than or equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 <= 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 <= .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 <= .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Greater than or equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 >= 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 >= .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 >= .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Less than")
        {
            auto [value, error] = evaluateConstantExpression("(void*)3 < (void*)55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Greater than")
        {
            auto [value, error] =
                evaluateConstantExpression("(int* const)3 > (const int*)55", cld::LanguageOptions::native(),
                                           cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Less than or equal")
        {
            auto [value, error] =
                evaluateConstantExpression("(struct i*)3 <= (struct i*)55", cld::LanguageOptions::native(),
                                           cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("(float*)3 >= (float*)55", cld::LanguageOptions::native(),
                                                             cld::Semantics::ConstantEvaluator::Initialization);
            REQUIRE(error.empty());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            CHECK(value.getType()
                  == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
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
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("4 == 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("5 != 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("4 != 4");
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
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
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 == .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 == .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression(".55 == .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression(".55 != 3");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 != .55");
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
            }
            {
                auto [value, error] = evaluateConstantExpression("3 != .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression(".55 != .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Arithmetic);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Equal")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 == (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)4 == (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(struct i*)5 == (struct i*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 == (void*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)5 == (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(float*)5 == (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'=='",
                                                "'float*'", "'const int*'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("0 == (const int*)4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)5 == 0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("5 == (const int*)4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER));
            }
            {
                auto [value, error] = evaluateConstantExpression("(const int*)5 == 4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER));
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)4 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(struct i*)5 != (struct i*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 != (void*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)5 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(float*)5 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N, "'!='",
                                                "'float*'", "'const int*'"));
            }
            {
                auto [value, error] = evaluateConstantExpression("0 != (const int*)4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)5 != 0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                REQUIRE(error.empty());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
                CHECK(value.getType()
                      == cld::Semantics::PrimitiveType::createInt(false, false, cld::LanguageOptions::native()));
            }
            {
                auto [value, error] = evaluateConstantExpression("5 != (const int*)4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER));
            }
            {
                auto [value, error] = evaluateConstantExpression("(const int*)5 != 4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::ConstantEvaluator::Initialization);
                CHECK(value.isUndefined());
                CHECK_THAT(error, ProducesError(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER));
            }
        }
    }
}

TEST_CASE("Const eval expression", "[constEval]")
{
    auto [value, error] = evaluateConstantExpression("(.55 , 3)");
    CHECK(value.isUndefined());
    CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "','"));
}

TEST_CASE("Const eval assignments", "[constEval]")
{
    auto iter = GENERATE(as<std::string>{}, "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=");
    auto [value, error] = evaluateConstantExpression("(.55 " + iter + " 3)");
    CHECK(value.isUndefined());
    CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, '\'' + iter + '\''));
}

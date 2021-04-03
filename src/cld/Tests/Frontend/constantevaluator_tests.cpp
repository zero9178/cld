#include <catch.hpp>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/SemanticAnalysis.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>

#include "TestConfig.hpp"

using namespace cld::Errors;
using namespace cld::Errors::Semantics;
using namespace cld::Warnings::Semantics;

namespace
{
std::pair<cld::Semantics::ConstValue, std::string>
    evaluateConstantExpression(std::string expression,
                               const cld::LanguageOptions& options = cld::LanguageOptions::native(),
                               cld::Semantics::SemanticAnalysis::Mode mode = cld::Semantics::SemanticAnalysis::Integer)
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    auto tokens = cld::Lexer::tokenize(std::move(expression), &options);
    REQUIRE(!tokens.data().empty());
    auto ctokens = cld::Lexer::toCTokens(tokens);
    cld::Parser::Context context(ctokens, &ss);
    const auto* ref = std::as_const(ctokens).data().data();
    auto parsing = cld::Parser::parseConditionalExpression(ref, ctokens.data().data() + ctokens.data().size(), context);
    UNSCOPED_INFO(ss.str());
    REQUIRE_THAT(ss.str(), ProducesNoErrors());
    REQUIRE(parsing);
    cld::Semantics::SemanticAnalysis analysis(ctokens, &ss);
    auto expr = analysis.visit(*parsing);
    UNSCOPED_INFO(ss.str());
    REQUIRE_THAT(ss.str(), ProducesNoErrors());
    auto ret = analysis.evaluateConstantExpression(*expr, mode);
    if (!ret)
    {
        for (auto& iter : ret.error())
        {
            ss << iter;
            llvm::errs() << iter;
        }
        ret = cld::Semantics::ConstValue{};
    }
    return {*ret, ss.str()};
}

std::pair<cld::Semantics::ConstValue, std::string>
    evaluateConstantExpression(std::string expression, const cld::Triple triple,
                               cld::Semantics::SemanticAnalysis::Mode mode = cld::Semantics::SemanticAnalysis::Integer)
{
    return evaluateConstantExpression(std::move(expression), cld::LanguageOptions::fromTriple(triple), mode);
}

std::pair<cld::Semantics::ConstValue, std::string>
    evaluateProgram(std::string source, const cld::LanguageOptions& options = cld::LanguageOptions::native(),
                    cld::Semantics::SemanticAnalysis::Mode mode = cld::Semantics::SemanticAnalysis::Integer)
{
    std::string storage;
    llvm::raw_string_ostream ss(storage);
    cld::PPSourceObject tokens;
    bool errors = false;
    tokens = cld::Lexer::tokenize(std::move(source), &options, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    tokens = cld::PP::preprocess(std::move(tokens), {}, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    static cld::CSourceObject ctokens;
    ctokens = cld::Lexer::toCTokens(tokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    auto parsing = cld::Parser::buildTree(ctokens, &ss, &errors);
    UNSCOPED_INFO(storage);
    REQUIRE_FALSE(errors);
    cld::Semantics::SemanticAnalysis analysis(ctokens, &ss);
    auto translationUnit = analysis.visit(parsing);
    REQUIRE_THAT(ss.str(), ProducesNoErrors());
    REQUIRE(translationUnit.getGlobals().back()->is<cld::Semantics::FunctionDefinition>());
    auto& funcDef = translationUnit.getGlobals().back()->as<cld::Semantics::FunctionDefinition>();
    REQUIRE(std::holds_alternative<cld::IntrVarPtr<cld::Semantics::Statement>>(
        funcDef.getCompoundStatement().getCompoundItems().back()));
    auto& statement =
        cld::get<cld::IntrVarPtr<cld::Semantics::Statement>>(funcDef.getCompoundStatement().getCompoundItems().back());
    REQUIRE(statement->is<cld::Semantics::ExpressionStatement>());
    auto* expr = static_cast<cld::Semantics::ExpressionStatement&>(*statement).getExpression();
    REQUIRE(expr);
    auto ret = analysis.evaluateConstantExpression(*expr, mode);
    if (!ret)
    {
        for (auto& iter : ret.error())
        {
            ss << iter;
            llvm::errs() << iter;
        }
        ret = cld::Semantics::ConstValue{};
    }
    return {*ret, ss.str()};
}

std::pair<cld::Semantics::ConstValue, std::string>
    evaluateProgram(std::string source, cld::Triple triple,
                    cld::Semantics::SemanticAnalysis::Mode mode = cld::Semantics::SemanticAnalysis::Integer)
{
    return evaluateProgram(std::move(source), cld::LanguageOptions::fromTriple(triple), mode);
}
} // namespace

#define INT_EVAL_PRODUCES(text, matcher)                    \
    [&](std::string source) {                               \
        auto [_, str] = evaluateConstantExpression(source); \
        CHECK_THAT(str, matcher);                           \
    }(text)

#define ARITH_EVAL_PRODUCES(text, matcher)                                                        \
    [&](std::string source) {                                                                     \
        auto [_, str] = evaluateConstantExpression(source, cld::LanguageOptions::native(),        \
                                                   cld::Semantics::SemanticAnalysis::Arithmetic); \
        CHECK_THAT(str, matcher);                                                                 \
    }(text)

#define INIT_EVAL_PRODUCES(text, matcher)                                                             \
    [&](std::string source) {                                                                         \
        auto [_, str] = evaluateConstantExpression(source, cld::LanguageOptions::native(),            \
                                                   cld::Semantics::SemanticAnalysis::Initialization); \
        CHECK_THAT(str, matcher);                                                                     \
    }(text)

#define INT_EVAL_PROG_PRODUCES(text, matcher)    \
    [&](std::string source) {                    \
        auto [_, str] = evaluateProgram(source); \
        CHECK_THAT(str, matcher);                \
    }(text)

#define ARITH_EVAL_PROG_PRODUCES(text, matcher)                                                                    \
    [&](std::string source) {                                                                                      \
        auto [_, str] =                                                                                            \
            evaluateProgram(source, cld::LanguageOptions::native(), cld::Semantics::SemanticAnalysis::Arithmetic); \
        CHECK_THAT(str, matcher);                                                                                  \
    }(text)

#define INIT_EVAL_PROG_PRODUCES(text, matcher)                                                                         \
    [&](std::string source) {                                                                                          \
        auto [_, str] =                                                                                                \
            evaluateProgram(source, cld::LanguageOptions::native(), cld::Semantics::SemanticAnalysis::Initialization); \
        CHECK_THAT(str, matcher);                                                                                      \
    }(text)

TEST_CASE("Const eval Primary expression", "[constEval]")
{
    SECTION("int")
    {
        auto [value, error] = evaluateConstantExpression("0");
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
    }
    SECTION("unsigned int")
    {
        auto [value, error] = evaluateConstantExpression("0u");
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isUnsigned());
        CHECK(result.getBitWidth() == sizeof(unsigned int) * 8);
    }
    SECTION("long")
    {
        auto [value, error] = evaluateConstantExpression("0l", cld::Tests::x86linux);
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == 32);
    }
    SECTION("unsigned long")
    {
        auto [value, error] = evaluateConstantExpression("0ul", cld::Tests::x86linux);
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isUnsigned());
        CHECK(result.getBitWidth() == 32);
    }
    SECTION("long long")
    {
        auto [value, error] = evaluateConstantExpression("0ll", cld::Tests::x86linux);
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == 64);
    }
    SECTION("unsigned long long")
    {
        auto [value, error] = evaluateConstantExpression("0ull", cld::Tests::x86linux);
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 0);
        CHECK(result.isUnsigned());
        CHECK(result.getBitWidth() == 64);
    }
    SECTION("float")
    {
        auto [value, error] = evaluateConstantExpression(".0f", cld::LanguageOptions::native(),
                                                         cld::Semantics::SemanticAnalysis::Arithmetic);
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
        auto result = std::get<llvm::APFloat>(value.getValue());
        CHECK(result.convertToFloat() == 0.f);
        CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
    }
    SECTION("double")
    {
        auto [value, error] = evaluateConstantExpression(".0", cld::LanguageOptions::native(),
                                                         cld::Semantics::SemanticAnalysis::Arithmetic);
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
        auto result = std::get<llvm::APFloat>(value.getValue());
        CHECK(result.convertToDouble() == 0.0);
        CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
    }
    SECTION("long double")
    {
        SECTION("Msvc")
        {
            auto [value, error] = evaluateConstantExpression(".0l", cld::Tests::x64windowsMsvc,
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToDouble() == 0.0);
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
        }
        SECTION("Gnu")
        {
            auto [value, error] = evaluateConstantExpression(".0l", cld::Tests::x64windowsGnu,
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            REQUIRE(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_x87DoubleExtended);
            CHECK(result.compare(llvm::APFloat(llvm::APFloat::x87DoubleExtended(), "0.0")));
        }
    }
    SECTION("String literal")
    {
        auto [value, error] = evaluateConstantExpression("\"test\"");
        CHECK_THAT(error, ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "String literals"));
        CHECK(value.isUndefined());
    }
}

TEST_CASE("Const eval postfix expression", "[constEval]")
{
    SECTION("Function call")
    {
        INT_EVAL_PRODUCES("((void(*)(void))5)()", ProducesError(FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    }
    SECTION("Initializer")
    {
        INT_EVAL_PRODUCES("(int){0}", ProducesError(COMPOUND_LITERAL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    }
}

TEST_CASE("Const eval unary expression", "[constEval]")
{
    SECTION("Plus")
    {
        SECTION("Integer")
        {
            {
                auto [value, error] = evaluateConstantExpression("+0");
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("+(signed char)0");
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("+(unsigned char)0");
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
        SECTION("Floating point")
        {
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("+.0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 0.0);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            }
            INT_EVAL_PRODUCES("+.0", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
    }
    SECTION("Minus")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("-1");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == -1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Floating point")
        {
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("-.1", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == -.1);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            }
            INT_EVAL_PRODUCES("-.1", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
    }
    SECTION("Bitnot")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("~1u");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == ~1u);
            CHECK(result.isUnsigned());
            CHECK(result.getBitWidth() == sizeof(unsigned int) * 8);
        }
    }
    SECTION("Logical not")
    {
        SECTION("Integer")
        {
            auto [value, error] = evaluateConstantExpression("!1");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Floating point")
        {
            SECTION("Arithmetic constant expression")
            {
                auto [value, error] = evaluateConstantExpression("!.1", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            INT_EVAL_PRODUCES("!.1", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        SECTION("Pointer")
        {
            {
                auto [value, error] = evaluateConstantExpression("!(void*)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("!(void*)5", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
    }
    SECTION("Sizeof")
    {
        INT_EVAL_PROG_PRODUCES("void foo(int i) {\n"
                               "int r[i * 2];\n"
                               "sizeof r;\n"
                               "}",
                               ProducesError(SIZEOF_VAL_MODIFIED_TYPE_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION));
    }
}

TEST_CASE("Const eval cast expression", "[constEval]")
{
    SECTION("Integer")
    {
        {
            auto [value, error] = evaluateConstantExpression("(unsigned long long)0");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isUnsigned());
            CHECK(result.getBitWidth() == sizeof(unsigned long long) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("(unsigned long long)0.5");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isUnsigned());
            CHECK(result.getBitWidth() == sizeof(unsigned long long) * 8);
        }
    }
    SECTION("Float")
    {
        SECTION("Integer constant expressions")
        {
            auto [value, error] = evaluateConstantExpression("(float)0");
            CHECK(value.isUndefined());
            CHECK_THAT(error, ProducesError(CANNOT_CAST_TO_NON_INTEGER_TYPE_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Arithmetic constant expressions")
        {
            SECTION("From int")
            {
                auto [value, error] = evaluateConstantExpression("(float)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToFloat() == 0.f);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
            }
            SECTION("From float")
            {
                auto [value, error] = evaluateConstantExpression("(float)5.3", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToFloat() == 5.3f);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
            }
        }
        SECTION("To Infinity")
        {
            auto [value, error] = evaluateConstantExpression(
                "(float)" + std::to_string(std::numeric_limits<double>::max()), cld::LanguageOptions::native(),
                cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToFloat() == std::numeric_limits<float>::infinity());
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEsingle);
        }
        SECTION("Warnings")
        {
            SECTION("To Integer")
            {
                auto [value, error] =
                    evaluateConstantExpression("(long long)3.40282347E+38f", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Arithmetic);
                CHECK_THAT(error, ProducesWarning(VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N, "'3.40282347E+38'",
                                                  "'long long'"));
            }
        }
    }
    SECTION("Pointers")
    {
        SECTION("Integer constant expressions")
        {
            INT_EVAL_PRODUCES("(void*)0",
                              ProducesError(CANNOT_CAST_TO_NON_INTEGER_TYPE_IN_INTEGER_CONSTANT_EXPRESSION));
        }
        SECTION("Arithmetic constant expressions")
        {
            ARITH_EVAL_PRODUCES("(void*)0",
                                ProducesError(CANNOT_CAST_TO_NON_ARITHMETIC_TYPE_IN_ARITHMETIC_CONSTANT_EXPRESSION));
        }
        SECTION("Initializer constant expressions")
        {
            {
                auto [value, error] = evaluateConstantExpression("(void*)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 0);
            }
            {
                auto [value, error] = evaluateConstantExpression("(float*)(void*)0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 0);
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
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 20);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Divide")
        {
            auto [value, error] = evaluateConstantExpression("5 / 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
            INT_EVAL_PRODUCES("0/0", ProducesError(INTEGER_DIVISION_BY_ZERO_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
        }
        SECTION("Modulo")
        {
            auto [value, error] = evaluateConstantExpression("5 % 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
    SECTION("Float")
    {
        SECTION("Multiply")
        {
            {
                auto [value, error] = evaluateConstantExpression("3 * .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3 * .55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            }
            {
                auto [value, error] = evaluateConstantExpression("3.0f * .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
                auto result = std::get<llvm::APFloat>(value.getValue());
                CHECK(result.convertToDouble() == 3 * .55);
                CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
            }
            INT_EVAL_PRODUCES("3.0f * .55", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        }
        SECTION("Divide")
        {
            auto [value, error] = evaluateConstantExpression("3 / .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToDouble() == 3 / .55);
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);

            INT_EVAL_PRODUCES("3.0f / .55", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
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
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 9);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Minus")
        {
            auto [value, error] = evaluateConstantExpression("5 - 2");
            UNSCOPED_INFO(error);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 3);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
    SECTION("Float")
    {
        SECTION("Plus")
        {
            auto [value, error] = evaluateConstantExpression("3 + .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            UNSCOPED_INFO(error);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToDouble() == 3.55);
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
        }
        SECTION("Minus")
        {
            auto [value, error] = evaluateConstantExpression("3 - .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            UNSCOPED_INFO(error);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APFloat>(value.getValue()));
            auto result = std::get<llvm::APFloat>(value.getValue());
            CHECK(result.convertToDouble() == 3 - .55);
            CHECK(llvm::APFloat::SemanticsToEnum(result.getSemantics()) == llvm::APFloat::S_IEEEdouble);
        }
    }
    SECTION("Pointer")
    {
        SECTION("Plus")
        {
            {
                auto [value, error] = evaluateConstantExpression("3 + (int*)5", cld::Tests::x64linux,
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 17);
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 + 3", cld::Tests::x64linux,
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == 17);
            }
        }
        SECTION("Minus")
        {
            SECTION("64 bit")
            {
                auto [value, error] = evaluateConstantExpression("(int*)20 - (int*)4", cld::Tests::x64linux,
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 4);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == 64);
            }
            SECTION("32 bit")
            {
                auto [value, error] = evaluateConstantExpression("(int*)20 - (int*)4", cld::Tests::x86linux,
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 4);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == 32);
            }
            {
                auto [value, error] = evaluateConstantExpression("(int*)5 - 3", cld::Tests::x64linux,
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                UNSCOPED_INFO(error);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
                CHECK(std::get<cld::Semantics::VoidStar>(value.getValue()).address == std::uint64_t(-7));
            }
        }
    }
}

TEST_CASE("Const eval shift", "[constEval]")
{
    SECTION("Left")
    {
        auto [value, error] = evaluateConstantExpression("5 << 4");
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 5 << 4);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
    }
    SECTION("Right")
    {
        auto [value, error] = evaluateConstantExpression("5 >> 2");
        REQUIRE_THAT(error, ProducesNoErrors());
        REQUIRE(!value.isUndefined());
        REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
        auto result = std::get<llvm::APSInt>(value.getValue());
        CHECK(result == 5 >> 2);
        CHECK(result.isSigned());
        CHECK(result.getBitWidth() == sizeof(int) * 8);
    }
}

TEST_CASE("Const eval bitand", "[constEval]")
{
    auto [value, error] = evaluateConstantExpression("5 & 4");
    REQUIRE_THAT(error, ProducesNoErrors());
    REQUIRE(!value.isUndefined());
    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
    auto result = std::get<llvm::APSInt>(value.getValue());
    CHECK(result == (5 & 4));
    CHECK(result.isSigned());
    CHECK(result.getBitWidth() == sizeof(int) * 8);
}

TEST_CASE("Const eval bitxor", "[constEval]")
{
    auto [value, error] = evaluateConstantExpression("5 ^ 4");
    REQUIRE_THAT(error, ProducesNoErrors());
    REQUIRE(!value.isUndefined());
    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
    auto result = std::get<llvm::APSInt>(value.getValue());
    CHECK(result == (5 ^ 4));
    CHECK(result.isSigned());
    CHECK(result.getBitWidth() == sizeof(int) * 8);
}

TEST_CASE("Const eval bitor", "[constEval]")
{
    auto [value, error] = evaluateConstantExpression("5 | 4");
    REQUIRE_THAT(error, ProducesNoErrors());
    REQUIRE(!value.isUndefined());
    REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
    auto result = std::get<llvm::APSInt>(value.getValue());
    CHECK(result == (5 | 4));
    CHECK(result.isSigned());
    CHECK(result.getBitWidth() == sizeof(int) * 8);
}

TEST_CASE("Const eval and", "[constEval]")
{
    SECTION("Integer")
    {
        {
            auto [value, error] = evaluateConstantExpression("5 && 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("5 && 0");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression("3 && .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("0 && .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        INT_EVAL_PRODUCES("3 && .55", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
        INT_EVAL_PRODUCES("0 && .55", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
    }
    SECTION("Pointers")
    {
        {
            auto [value, error] = evaluateConstantExpression("3 && (int*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("0 && (int*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
}

TEST_CASE("Const eval or", "[constEval]")
{
    SECTION("Integer")
    {
        {
            auto [value, error] = evaluateConstantExpression("5 || 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || 0");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
    SECTION("Float")
    {
        {
            auto [value, error] = evaluateConstantExpression("3 || .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || .0", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        INT_EVAL_PRODUCES("1 || .55", ProducesError(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS));
    }
    SECTION("Pointer")
    {
        {
            auto [value, error] = evaluateConstantExpression("3 || (void*)5", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        {
            auto [value, error] = evaluateConstantExpression("0 || (void*)0", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
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
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Greater than")
        {
            auto [value, error] = evaluateConstantExpression("5 > 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Less than or equal")
        {
            auto [value, error] = evaluateConstantExpression("5 <= 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("5 >= 4");
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
    SECTION("Float")
    {
        SECTION("Less than")
        {
            auto [value, error] = evaluateConstantExpression("3 < .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Greater than")
        {
            auto [value, error] = evaluateConstantExpression("3 > .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Less than or equal")
        {
            auto [value, error] = evaluateConstantExpression("3 <= .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("3 >= .55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Arithmetic);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
    }
    SECTION("Pointers")
    {
        SECTION("Less than")
        {
            auto [value, error] = evaluateConstantExpression("(void*)3 < (void*)55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 1);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Greater than")
        {
            auto [value, error] =
                evaluateConstantExpression("(int* const)3 > (const int*)55", cld::LanguageOptions::native(),
                                           cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Less than or equal")
        {
            auto [value, error] =
                evaluateConstantExpression("(struct i*)3 <= (struct i*)55", cld::LanguageOptions::native(),
                                           cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result != 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
        }
        SECTION("Greater than or equal")
        {
            auto [value, error] = evaluateConstantExpression("(float*)3 >= (float*)55", cld::LanguageOptions::native(),
                                                             cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(!value.isUndefined());
            REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
            auto result = std::get<llvm::APSInt>(value.getValue());
            CHECK(result == 0);
            CHECK(result.isSigned());
            CHECK(result.getBitWidth() == sizeof(int) * 8);
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
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("4 == 4");
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("5 != 4");
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("4 != 4");
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
    }
    SECTION("Float")
    {
        SECTION("Equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("3 == .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression(".55 == .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] = evaluateConstantExpression("3 != .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression(".55 != .55", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Arithmetic);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
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
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)4 == (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(struct i*)5 == (struct i*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 == (void*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)5 == (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("0 == (const int*)4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)5 == 0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
        SECTION("Not equal")
        {
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)4 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result == 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(struct i*)5 != (struct i*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(int* const)5 != (void*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] =
                    evaluateConstantExpression("(void*)5 != (const int*)4", cld::LanguageOptions::native(),
                                               cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("0 != (const int*)4", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
            {
                auto [value, error] = evaluateConstantExpression("(void*)5 != 0", cld::LanguageOptions::native(),
                                                                 cld::Semantics::SemanticAnalysis::Initialization);
                REQUIRE_THAT(error, ProducesNoErrors());
                REQUIRE(!value.isUndefined());
                REQUIRE(std::holds_alternative<llvm::APSInt>(value.getValue()));
                auto result = std::get<llvm::APSInt>(value.getValue());
                CHECK(result != 0);
                CHECK(result.isSigned());
                CHECK(result.getBitWidth() == sizeof(int) * 8);
            }
        }
    }
}

TEST_CASE("Const eval expression", "[constEval]")
{
    INT_EVAL_PRODUCES("(.55 , 3)", ProducesError(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "','"));
}

TEST_CASE("Const eval initialization", "[constEval]")
{
    SECTION("Ampersand")
    {
        SECTION("Lvalue")
        {
            auto [value, error] =
                evaluateProgram("int i;\n"
                                "void foo(void) {\n"
                                "&i;\n"
                                "}",
                                cld::LanguageOptions::native(), cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            CHECK(std::holds_alternative<cld::Semantics::AddressConstant>(value.getValue()));
        }
        SECTION("Following dereference")
        {
            auto [value, error] = evaluateProgram("void foo(void) {\n"
                                                  "&*((int*)5 + 3);\n"
                                                  "}",
                                                  x64linux, cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
            CHECK(cld::get<cld::Semantics::VoidStar>(value.getValue()).address == 17);
        }
        SECTION("Following subscript")
        {
            auto [value, error] = evaluateProgram("void foo(void) {\n"
                                                  "&((int*)5)[3];\n"
                                                  "}",
                                                  x64linux, cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            REQUIRE(std::holds_alternative<cld::Semantics::VoidStar>(value.getValue()));
            CHECK(cld::get<cld::Semantics::VoidStar>(value.getValue()).address == 17);
        }
    }
    SECTION("Lvalue conversion")
    {
        SECTION("Function")
        {
            auto [value, error] =
                evaluateProgram("int i(void);\n"
                                "void foo(void) {\n"
                                "i;\n"
                                "}",
                                cld::LanguageOptions::native(), cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            CHECK(std::holds_alternative<cld::Semantics::AddressConstant>(value.getValue()));
        }
        SECTION("Array")
        {
            auto [value, error] =
                evaluateProgram("int i[5];\n"
                                "void foo(void) {\n"
                                "i + 1;\n"
                                "}",
                                cld::LanguageOptions::native(), cld::Semantics::SemanticAnalysis::Initialization);
            REQUIRE_THAT(error, ProducesNoErrors());
            CHECK(std::holds_alternative<cld::Semantics::AddressConstant>(value.getValue()));
        }
        INIT_EVAL_PROG_PRODUCES("int i;\n"
                                "void foo(void) {\n"
                                "i + 1;\n"
                                "}",
                                ProducesError(VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION));
    }
}

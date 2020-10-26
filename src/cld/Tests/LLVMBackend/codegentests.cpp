#define __USE_MINGW_ANSI_STDIO 1
#include <catch.hpp>

#include <llvm/IR/Verifier.h>

#include <cld/LLVMBackend/Codegen.hpp>

#include <numeric>

#include <TestTargets.hpp>
#include <stdarg.h>

#include "Common.hpp"

using namespace Catch::Matchers;

TEST_CASE("LLVM codegen functions", "[LLVM]")
{
    llvm::LLVMContext context;
    llvm::Module module("", context);
    SECTION("Normal function")
    {
        auto program = generateProgram("void foo(void);");
        cld::CGLLVM::generateLLVM(module, program);
        CAPTURE(module);
        REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
        auto* function = module.getFunction("foo");
        REQUIRE(function);
        CHECK(function->getLinkage() == llvm::GlobalValue::ExternalLinkage);
        CHECK(function->getReturnType()->isVoidTy());
        CHECK(function->getNumOperands() == 0);
    }
    SECTION("Signed type")
    {
        auto program = generateProgram("char f0(void){ return 0;}\n"
                                       "short f1(void){return 0;}\n"
                                       "int f2(void){return 0;}\n");
        cld::CGLLVM::generateLLVM(module, program);
        CAPTURE(module);
        REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
        auto* function = module.getFunction("f0");
        REQUIRE(function);
        CHECK(function->getReturnType()->isIntegerTy());
        CHECK(function->hasAttribute(0, llvm::Attribute::SExt));
        function = module.getFunction("f1");
        REQUIRE(function);
        CHECK(function->getReturnType()->isIntegerTy());
        CHECK(function->hasAttribute(0, llvm::Attribute::SExt));
        function = module.getFunction("f2");
        REQUIRE(function);
        CHECK(function->getReturnType()->isIntegerTy());
    }
}

TEST_CASE("LLVM codegen cdecl", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("x64 Windows")
    {
        SECTION("Arguments")
        {
            SECTION("8")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->arg_size() == 1);
                CHECK(function->getArg(0)->getType() == llvm::IntegerType::getIntNTy(context, 8));
            }
            SECTION("16")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "short c;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->arg_size() == 1);
                CHECK(function->getArg(0)->getType() == llvm::IntegerType::getIntNTy(context, 16));
            }
            SECTION("32")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c[4];\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->arg_size() == 1);
                CHECK(function->getArg(0)->getType() == llvm::IntegerType::getIntNTy(context, 32));
            }
            SECTION("64")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "int c;\n"
                                                          "float f;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->arg_size() == 1);
                CHECK(function->getArg(0)->getType() == llvm::IntegerType::getIntNTy(context, 64));
            }
            SECTION("Not power of two")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c[3];\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isStructTy());
            }
            SECTION("> 64")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "float c;\n"
                                                          "short r[3];\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isStructTy());
            }
            SECTION("long double")
            {
                SECTION("GNU")
                {
                    auto program = generateProgramWithOptions("void foo(long double);", x64windowsGnu);
                    cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                    CAPTURE(*module);
                    REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                    auto* function = module->getFunction("foo");
                    REQUIRE(function);
                    REQUIRE(function->arg_size() == 1);
                    REQUIRE(function->getArg(0)->getType()->isPointerTy());
                    CHECK(function->getArg(0)->getType()->getPointerElementType()->isX86_FP80Ty());
                }
                SECTION("MSVC")
                {
                    auto program = generateProgramWithOptions("void foo(long double);", x64windowsMsvc);
                    cld::CGLLVM::generateLLVM(*module, program, x64windowsMsvc);
                    CAPTURE(*module);
                    REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                    auto* function = module->getFunction("foo");
                    REQUIRE(function);
                    REQUIRE(function->arg_size() == 1);
                    CHECK(function->getArg(0)->getType()->isDoubleTy());
                }
            }
        }
        SECTION("Return type")
        {
            SECTION("8")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c;\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->arg_size() == 0);
                CHECK(function->getReturnType()->isIntegerTy(8));
            }
            SECTION("16")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c;\n"
                                                          "_Bool r;\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->arg_size() == 0);
                CHECK(function->getReturnType()->isIntegerTy(16));
            }
            SECTION("32")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "float f;\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->arg_size() == 0);
                CHECK(function->getReturnType()->isIntegerTy(32));
            }
            SECTION("64")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c[2];\n"
                                                          "short r;\n"
                                                          "float f;\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->arg_size() == 0);
                CHECK(function->getReturnType()->isIntegerTy(64));
            }
            SECTION("> 64")
            {
                auto program = generateProgramWithOptions("long double foo(void);", x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isX86_FP80Ty());
                CHECK(function->hasAttribute(1, llvm::Attribute::StructRet));
                CHECK(function->hasAttribute(1, llvm::Attribute::NoAlias));
            }
            SECTION("Not power of 2")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "short r[3];\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64windowsGnu);
                cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isStructTy());
                CHECK(function->hasAttribute(1, llvm::Attribute::StructRet));
                CHECK(function->hasAttribute(1, llvm::Attribute::NoAlias));
            }
        }
        SECTION("Execution")
        {
#ifdef _WIN64
            struct R
            {
                short r[3];
            };
            struct Input
            {
                short f;
                unsigned char c[2];
            };
            auto program = generateProgram("struct R {\n"
                                           "short r[3];\n"
                                           "};\n"
                                           "struct Input {\n"
                                           "short f;\n"
                                           "unsigned char c[2];\n"
                                           "};\n"
                                           "struct R foo(struct Input i) {\n"
                                           "struct R r;\n"
                                           "r.r[0] = i.f;\n"
                                           "r.r[1] = i.c[0] + i.c[1];\n"
                                           "r.r[2] = i.c[0] - i.c[1];\n"
                                           "return r;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            Input input = {72, {'5', '0'}};
            auto result = computeInJIT<R(Input)>(std::move(module), "foo", input);
            CHECK(result.r[0] == 72);
            CHECK(result.r[1] == '5' + '0');
            CHECK(result.r[2] == 5);
#endif
        }
    }
    SECTION("x64 Unix")
    {
        SECTION("Arguments")
        {
            SECTION("> 128")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "int r[5];\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isStructTy());
                CHECK(function->hasAttribute(1, llvm::Attribute::ByVal));
                CHECK(function->hasAttribute(1, llvm::Attribute::Alignment));
                CHECK(function->getAttribute(1, llvm::Attribute::Alignment).getValueAsInt() == 8);
            }
            SECTION("Fits in integers")
            {
                SECTION("64")
                {
                    auto program = generateProgramWithOptions("struct R {\n"
                                                              "int r[2];\n"
                                                              "};\n"
                                                              "void foo(struct R);",
                                                              x64linux);
                    cld::CGLLVM::generateLLVM(*module, program, x64linux);
                    CAPTURE(*module);
                    REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                    auto* function = module->getFunction("foo");
                    REQUIRE(function);
                    CHECK(function->getReturnType()->isVoidTy());
                    REQUIRE(function->arg_size() == 1);
                    CHECK(function->getArg(0)->getType()->isIntegerTy(64));
                }
                SECTION("128")
                {
                    auto program = generateProgramWithOptions("struct R {\n"
                                                              "int r[4];\n"
                                                              "};\n"
                                                              "void foo(struct R);",
                                                              x64linux);
                    cld::CGLLVM::generateLLVM(*module, program, x64linux);
                    CAPTURE(*module);
                    REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                    auto* function = module->getFunction("foo");
                    REQUIRE(function);
                    CHECK(function->getReturnType()->isVoidTy());
                    REQUIRE(function->arg_size() == 2);
                    CHECK(function->getArg(0)->getType()->isIntegerTy(64));
                    CHECK(function->getArg(1)->getType()->isIntegerTy(64));
                }
            }
            SECTION("fp80")
            {
                auto program = generateProgramWithOptions("void foo(long double r);", x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isX86_FP80Ty());
            }
            SECTION("struct of fp80")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "long double r;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isStructTy());
                CHECK(function->hasAttribute(1, llvm::Attribute::ByVal));
                CHECK(function->hasAttribute(1, llvm::Attribute::Alignment));
                CHECK(function->getAttribute(1, llvm::Attribute::Alignment).getValueAsInt() == 16);
            }
            SECTION("Integer mixed with floats")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "int r;\n"
                                                          "float f;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                CHECK(function->getArg(0)->getType()->isIntegerTy(64));
            }
            SECTION("Two floats are squeezed into one xmm")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "float r;\n"
                                                          "float f;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(llvm::isa<llvm::FixedVectorType>(function->getArg(0)->getType()));
                auto* vector = llvm::cast<llvm::FixedVectorType>(function->getArg(0)->getType());
                CHECK(vector->getNumElements() == 2);
                CHECK(vector->getElementType()->isFloatTy());
            }
            SECTION("Normal register passing")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c;\n"
                                                          "double d;\n"
                                                          "};\n"
                                                          "void foo(struct R);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 2);
                CHECK(function->getArg(0)->getType()->isIntegerTy(8));
                CHECK(function->getArg(1)->getType()->isDoubleTy());
            }
            SECTION("Not enough integer registers")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "long c[2];\n"
                                                          "};\n"
                                                          "void foo(struct R,struct R,long,struct R,float,long);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 8);
                CHECK(function->getArg(0)->getType()->isIntegerTy(64));
                CHECK(function->getArg(1)->getType()->isIntegerTy(64));
                CHECK(function->getArg(2)->getType()->isIntegerTy(64));
                CHECK(function->getArg(3)->getType()->isIntegerTy(64));
                CHECK(function->getArg(4)->getType()->isIntegerTy(64));
                REQUIRE(function->getArg(5)->getType()->isPointerTy());
                CHECK(function->getArg(5)->getType()->getPointerElementType()->isStructTy());
                CHECK(function->hasAttribute(6, llvm::Attribute::ByVal));
                CHECK(function->getArg(6)->getType()->isFloatTy());
                CHECK(function->getArg(7)->getType()->isIntegerTy(64));
            }
            SECTION("Large struct")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "float f;\n"
                                                          "int r[2];\n"
                                                          "};\n"
                                                          "\n"
                                                          "void foo(struct R);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 2);
                CHECK(function->getArg(0)->getType()->isIntegerTy(64));
                CHECK(function->getArg(1)->getType()->isIntegerTy(32));
            }
        }
        SECTION("Return type")
        {
            SECTION("Normal register passing")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "char c;\n"
                                                          "double d;\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                REQUIRE(function->getReturnType()->isStructTy());
                REQUIRE(function->getReturnType()->getStructNumElements() == 2);
                CHECK(function->getReturnType()->getStructElementType(0)->isIntegerTy(8));
                CHECK(function->getReturnType()->getStructElementType(1)->isDoubleTy());
                CHECK(function->arg_size() == 0);
            }
            SECTION("Two floats are squeezed into one xmm")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "float r;\n"
                                                          "float f;\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->arg_size() == 0);
                REQUIRE(llvm::isa<llvm::FixedVectorType>(function->getReturnType()));
                auto* vector = llvm::cast<llvm::FixedVectorType>(function->getReturnType());
                CHECK(vector->getNumElements() == 2);
                CHECK(vector->getElementType()->isFloatTy());
            }
            SECTION("> 128")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "int r[5];\n"
                                                          "};\n"
                                                          "struct R foo(void);",
                                                          x64linux);
                cld::CGLLVM::generateLLVM(*module, program, x64linux);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                auto* function = module->getFunction("foo");
                REQUIRE(function);
                CHECK(function->getReturnType()->isVoidTy());
                REQUIRE(function->arg_size() == 1);
                REQUIRE(function->getArg(0)->getType()->isPointerTy());
                CHECK(function->getArg(0)->getType()->getPointerElementType()->isStructTy());
                CHECK(function->hasAttribute(1, llvm::Attribute::StructRet));
                CHECK(function->hasAttribute(1, llvm::Attribute::NoAlias));
            }
        }
        SECTION("Execution")
        {
#if (defined(__unix__) || defined(__APPLE__)) && defined(__x86_64__)
            struct R
            {
                short r[3];
            };
            struct Input
            {
                double f;
                unsigned char c[2];
            };
            auto program = generateProgram("struct R {\n"
                                           "short r[3];\n"
                                           "};\n"
                                           "struct Input {\n"
                                           "double f;\n"
                                           "unsigned char c[2];\n"
                                           "};\n"
                                           "struct R foo(struct Input i) {\n"
                                           "struct R r;\n"
                                           "r.r[0] = i.f;\n"
                                           "r.r[1] = i.c[0] + i.c[1];\n"
                                           "r.r[2] = i.c[0] - i.c[1];\n"
                                           "return r;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            Input input = {72, {'5', '0'}};
            auto result = computeInJIT<R(Input)>(std::move(module), "foo", input);
            CHECK(result.r[0] == 72);
            CHECK(result.r[1] == '5' + '0');
            CHECK(result.r[2] == 5);
#endif
        }
    }
}

TEST_CASE("LLVM codegen global variables", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Tentative definition")
    {
        SECTION("Single")
        {
            auto program = generateProgram("int foo;");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(module->getGlobalList().size() == 1);
            auto* variable = module->getGlobalVariable("foo");
            REQUIRE(variable);
            CHECK(variable->getLinkage() == llvm::GlobalValue::CommonLinkage);
            CHECK(!variable->isDeclaration());
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                            * 8));
        }
        SECTION("Multiple")
        {
            auto program = generateProgram("int foo;\n"
                                           "int foo;");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(module->getGlobalList().size() == 1);
            auto* variable = module->getGlobalVariable("foo");
            REQUIRE(variable);
            CHECK(variable->getLinkage() == llvm::GlobalValue::CommonLinkage);
            CHECK(!variable->isDeclaration());
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                            * 8));
        }
        SECTION("Using multiple")
        {
            auto program = generateProgram("int foo[];\n"
                                           "\n"
                                           "int* bar(void) {\n"
                                           "return foo;\n"
                                           "}\n"
                                           "int foo[5] = {3,3,3,3,3};\n"
                                           "\n"
                                           "int function(void) {\n"
                                           "return bar() == foo;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* variable = module->getGlobalVariable("foo");
            REQUIRE(variable);
            CHECK(variable->getLinkage() == llvm::GlobalValue::ExternalLinkage);
            CHECK_FALSE(variable->isDeclaration());
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isArrayTy());
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
        SECTION("Internal linkage")
        {
            SECTION("Single")
            {
                auto program = generateProgram("static int foo;");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(module->getGlobalList().size() == 1);
                auto* variable = module->getGlobalVariable("foo", true);
                REQUIRE(variable);
                CHECK(variable->getLinkage() == llvm::GlobalValue::InternalLinkage);
                CHECK(!variable->isDeclaration());
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                                * 8));
            }
            SECTION("Multiple")
            {
                auto program = generateProgram("static int foo;\n"
                                               "static int foo;");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(module->getGlobalList().size() == 1);
                auto* variable = module->getGlobalVariable("foo", true);
                REQUIRE(variable);
                CHECK(variable->getLinkage() == llvm::GlobalValue::InternalLinkage);
                CHECK(!variable->isDeclaration());
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                                * 8));
            }
        }
    }
    SECTION("Declaration")
    {
        SECTION("Normal")
        {
            auto program = generateProgram("extern int foo;");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(module->getGlobalList().size() == 1);
            auto* variable = module->getGlobalVariable("foo");
            REQUIRE(variable);
            CHECK(variable->getLinkage() == llvm::GlobalValue::ExternalLinkage);
            CHECK(variable->isDeclaration());
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                            * 8));
        }
        SECTION("With ignored init")
        {
            auto program = generateProgram("extern int foo = 5;");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(module->getGlobalList().size() == 1);
            auto* variable = module->getGlobalVariable("foo");
            REQUIRE(variable);
            CHECK(variable->getLinkage() == llvm::GlobalValue::ExternalLinkage);
            CHECK(variable->isDeclaration());
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                            * 8));
        }
    }
    SECTION("Definitions")
    {
        SECTION("External definitions")
        {
            SECTION("Single")
            {
                auto program = generateProgram("int foo = 5;");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(module->getGlobalList().size() == 1);
                auto* variable = module->getGlobalVariable("foo");
                REQUIRE(variable);
                CHECK(variable->getLinkage() == llvm::GlobalValue::ExternalLinkage);
                CHECK(!variable->isDeclaration());
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                                * 8));
            }
            SECTION("Multiple")
            {
                auto program = generateProgram("int foo;\n"
                                               "int foo = 3;");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(module->getGlobalList().size() == 1);
                auto* variable = module->getGlobalVariable("foo");
                REQUIRE(variable);
                CHECK(variable->getLinkage() == llvm::GlobalValue::ExternalLinkage);
                CHECK(!variable->isDeclaration());
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                                * 8));
            }
        }
        SECTION("External definitions")
        {
            SECTION("Single")
            {
                auto program = generateProgram("static int foo = 5;");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(module->getGlobalList().size() == 1);
                auto* variable = module->getGlobalVariable("foo", true);
                REQUIRE(variable);
                CHECK(variable->getLinkage() == llvm::GlobalValue::InternalLinkage);
                CHECK(!variable->isDeclaration());
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                                * 8));
            }
            SECTION("Multiple")
            {
                auto program = generateProgram("static int foo;\n"
                                               "static int foo = 3;");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(module->getGlobalList().size() == 1);
                auto* variable = module->getGlobalVariable("foo", true);
                REQUIRE(variable);
                CHECK(variable->getLinkage() == llvm::GlobalValue::InternalLinkage);
                CHECK(!variable->isDeclaration());
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                                * 8));
            }
        }
    }
}

TEST_CASE("LLVM codegen types", "[LLVM]")
{
    llvm::LLVMContext context;
    llvm::Module module("", context);
    SECTION("Primitives")
    {
        SECTION("chars")
        {
            SECTION("char")
            {
                auto program = generateProgram("char foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(8));
            }
            SECTION("signed char")
            {
                auto program = generateProgram("signed char foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(8));
            }
            SECTION("unsigned char")
            {
                auto program = generateProgram("unsigned char foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(8));
            }
        }
        SECTION("_Bool")
        {
            auto program = generateProgram("_Bool foo;");
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
            REQUIRE(variable);
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isIntegerTy(
                cld::LanguageOptions::native().sizeOfUnderlineBool * 8));
        }
        SECTION("shorts")
        {
            auto size = cld::LanguageOptions::native().sizeOfShort * 8;
            SECTION("short")
            {
                auto program = generateProgram("short foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(size));
            }
            SECTION("unsigned short")
            {
                auto program = generateProgram("unsigned short foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(size));
            }
        }
        SECTION("ints")
        {
            auto size = cld::LanguageOptions::native().sizeOfInt * 8;
            SECTION("int")
            {
                auto program = generateProgram("int foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(size));
            }
            SECTION("unsigned int")
            {
                auto program = generateProgram("unsigned int foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(size));
            }
        }
        SECTION("longs")
        {
            auto size = cld::LanguageOptions::native().sizeOfLong * 8;
            SECTION("long")
            {
                auto program = generateProgram("long foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(size));
            }
            SECTION("unsigned long")
            {
                auto program = generateProgram("unsigned long foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(size));
            }
        }
        SECTION("long longs")
        {
            SECTION("long long")
            {
                auto program = generateProgram("long long foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(64));
            }
            SECTION("unsigned long long")
            {
                auto program = generateProgram("unsigned long long foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isIntegerTy(64));
            }
        }
        SECTION("float")
        {
            auto program = generateProgram("float foo;");
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
            REQUIRE(variable);
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isFloatTy());
        }
        SECTION("double")
        {
            auto program = generateProgram("double foo;");
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
            REQUIRE(variable);
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isDoubleTy());
        }
        SECTION("long double")
        {
            SECTION("MSVC")
            {
                auto program = generateProgramWithOptions("long double foo;", cld::Tests::x64windowsMsvc);
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isDoubleTy());
            }
            SECTION("GNU")
            {
                auto program = generateProgramWithOptions("long double foo;", cld::Tests::x64linux);
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                CHECK(variable->getType()->getPointerElementType()->isX86_FP80Ty());
            }
        }
    }
    SECTION("Arrays")
    {
        auto program = generateProgram("float foo[5];");
        cld::CGLLVM::generateLLVM(module, program);
        CAPTURE(module);
        REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
        CHECK(module.getGlobalList().size() == 1);
        auto* variable = module.getGlobalVariable("foo");
        REQUIRE(variable);
        REQUIRE(variable->getType()->isPointerTy());
        REQUIRE(variable->getType()->getPointerElementType()->isArrayTy());
        CHECK(variable->getType()->getPointerElementType()->getArrayElementType()->isFloatTy());
        CHECK(variable->getType()->getPointerElementType()->getArrayNumElements() == 5);
    }
    SECTION("Struct")
    {
        SECTION("Definition")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f;\n"
                                           "int n;\n"
                                           "};\n"
                                           "\n"
                                           "struct R foo;");
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
            REQUIRE(variable);
            REQUIRE(variable->getType()->isPointerTy());
            auto* structType = variable->getType()->getPointerElementType();
            REQUIRE(structType->isStructTy());
            REQUIRE(structType->getStructNumElements() == 2);
            CHECK(structType->getStructElementType(0)->isFloatTy());
            CHECK(structType->getStructElementType(1)->isIntegerTy(cld::LanguageOptions::native().sizeOfInt * 8));
        }
        SECTION("Opaque")
        {
            auto program = generateProgram("struct R;\n"
                                           "struct R* foo;");
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
            REQUIRE(variable);
            REQUIRE(variable->getType()->isPointerTy());
            REQUIRE(variable->getType()->getPointerElementType()->isPointerTy());
            auto* structType = variable->getType()->getPointerElementType()->getPointerElementType();
            REQUIRE(structType->isStructTy());
            CHECK(structType->getStructNumElements() == 0);
        }
        SECTION("Bitfields")
        {
            SECTION("Discrete bitfields")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "int a : 8, b : 3, c : 1, d : 1, e : 1;\n"
                                                          "_Bool f : 1;\n"
                                                          "};\n"
                                                          "\n"
                                                          "struct R foo;",
                                                          cld::Tests::x64windowsMsvc);
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                auto* structType = variable->getType()->getPointerElementType();
                REQUIRE(structType->isStructTy());
                REQUIRE(structType->getStructNumElements() == 2);
                CHECK(structType->getStructElementType(0)->isIntegerTy(cld::LanguageOptions::native().sizeOfInt * 8));
                CHECK(structType->getStructElementType(1)->isIntegerTy(
                    cld::LanguageOptions::native().sizeOfUnderlineBool * 8));
            }
            SECTION("System V bitfields")
            {
                auto program = generateProgramWithOptions("struct R {\n"
                                                          "int a : 8, b : 3, c : 1, d : 1, e : 1;\n"
                                                          "_Bool f : 1;\n"
                                                          "};\n"
                                                          "\n"
                                                          "struct R foo;",
                                                          cld::Tests::x64linux);
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
                REQUIRE(variable);
                REQUIRE(variable->getType()->isPointerTy());
                auto* structType = variable->getType()->getPointerElementType();
                REQUIRE(structType->isStructTy());
                REQUIRE(structType->getStructNumElements() == 1);
                CHECK(structType->getStructElementType(0)->isIntegerTy(cld::LanguageOptions::native().sizeOfInt * 8));
            }
        }
    }
}

TEST_CASE("LLVM codegen literals", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("String literal")
    {
        auto program = generateProgram("int function(void) {\n"
                                       "char * s;\n"
                                       "s = \"abc\" \"def\";\n"
                                       "if (s[0] != 'a') return 1;\n"
                                       "if (s[1] != 'b') return 2;\n"
                                       "if (s[2] != 'c') return 3;\n"
                                       "if (s[3] != 'd') return 4;\n"
                                       "if (s[4] != 'e') return 5;\n"
                                       "if (s[5] != 'f') return 6;\n"
                                       "if (s[6] != 0) return 7;\n"
                                       "\n"
                                       "return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "function") == 0);
    }
}

TEST_CASE("LLVM codegen unary expressions", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Address of")
    {
        SECTION("Simple")
        {
            auto program = generateProgram("int* foo(void) {\n"
                                           "int i;\n"
                                           "return &i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int*(void)>(std::move(module), "foo") != nullptr);
        }
        SECTION("Address of dereference")
        {
            auto program = generateProgram("int* foo(void) {\n"
                                           "int* i = 0;\n"
                                           "return &*i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int*(void)>(std::move(module), "foo") == nullptr);
        }
        SECTION("Address of subscript")
        {
            auto program = generateProgram("int* foo(void) {\n"
                                           "int* i = 0;\n"
                                           "return &i[0];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int*(void)>(std::move(module), "foo") == nullptr);
        }
    }
    SECTION("Dereference")
    {
        auto program = generateProgram("int foo(void) {\n"
                                       "int i,f;\n"
                                       "i = 5,f = 13;\n"
                                       "int* iPtr;\n"
                                       "iPtr = &i;\n"
                                       "*iPtr = *iPtr + f;\n"
                                       "return i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 18);
    }
    SECTION("Post increment")
    {
        SECTION("Integers")
        {
            SECTION("Signed")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "int i;\n"
                                               "i = 5;\n"
                                               "return i++;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("add nsw i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
            }
            SECTION("Unsigned")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "unsigned int i;\n"
                                               "i = 5;\n"
                                               "return i++;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("add i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
            }
        }
        SECTION("Floating point")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float i;\n"
                                           "i = 5;\n"
                                           "return i++;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 5.0f);
        }
        SECTION("Pointers")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float array[2];\n"
                                           "array[0] = 1;\n"
                                           "array[1] = 2;\n"
                                           "float* pointer;\n"
                                           "pointer = array;\n"
                                           "return *(pointer++);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 1.0f);
        }
    }
    SECTION("Post decrement")
    {
        SECTION("Integers")
        {
            SECTION("Signed")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "int i;\n"
                                               "i = 5;\n"
                                               "return i--;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("sub nsw i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
            }
            SECTION("Unsigned")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "unsigned int i;\n"
                                               "i = 5;\n"
                                               "return i--;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("sub i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
            }
        }
        SECTION("Floating point")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float i;\n"
                                           "i = 5;\n"
                                           "return i--;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 5.0f);
        }
        SECTION("Pointers")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float array[2];\n"
                                           "array[0] = 1;\n"
                                           "array[1] = 2;\n"
                                           "float* pointer;\n"
                                           "pointer = array;\n"
                                           "pointer++;\n"
                                           "return *(pointer--);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 2.0f);
        }
    }
    SECTION("Pre increment")
    {
        SECTION("Integers")
        {
            SECTION("Signed")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "int i;\n"
                                               "i = 5;\n"
                                               "return ++i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("add nsw i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 6);
            }
            SECTION("Unsigned")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "unsigned int i;\n"
                                               "i = 5;\n"
                                               "return ++i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("add i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 6);
            }
        }
        SECTION("Floating point")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float i;\n"
                                           "i = 5;\n"
                                           "return ++i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 6.0f);
        }
        SECTION("Pointers")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float array[2];\n"
                                           "array[0] = 1;\n"
                                           "array[1] = 2;\n"
                                           "float* pointer;\n"
                                           "pointer = array;\n"
                                           "return *(++pointer);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 2.0f);
        }
    }
    SECTION("Pre decrement")
    {
        SECTION("Integers")
        {
            SECTION("Signed")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "int i;\n"
                                               "i = 5;\n"
                                               "return --i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("sub nsw i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 4);
            }
            SECTION("Unsigned")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "unsigned int i;\n"
                                               "i = 5;\n"
                                               "return --i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("sub i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 4);
            }
        }
        SECTION("Floating point")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float i;\n"
                                           "i = 5;\n"
                                           "return --i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 4.0f);
        }
        SECTION("Pointers")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float array[2];\n"
                                           "array[0] = 1;\n"
                                           "array[1] = 2;\n"
                                           "float* pointer;\n"
                                           "pointer = array;\n"
                                           "pointer++;\n"
                                           "return *(--pointer);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 1.0f);
        }
    }
    SECTION("Minus")
    {
        SECTION("Integers")
        {
            SECTION("Signed")
            {
                auto program = generateProgram("int foo(void) {\n"
                                               "int i;\n"
                                               "i = 5;\n"
                                               "return -i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK_THAT(*module, ContainsIR("sub nsw i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == -5);
            }
            SECTION("Unsigned")
            {
                auto program = generateProgram("unsigned int foo(void) {\n"
                                               "unsigned int i;\n"
                                               "i = 5;\n"
                                               "return -i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                CHECK_THAT(*module, ContainsIR("sub i\\d+ .*, .*"));
                CHECK(cld::Tests::computeInJIT<unsigned int(void)>(std::move(module), "foo")
                      == static_cast<unsigned int>(-5));
            }
        }
        SECTION("Floating point")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float i;\n"
                                           "i = 5;\n"
                                           "return -i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == -5);
        }
    }
    SECTION("Negate")
    {
        auto program = generateProgram("unsigned int foo(void) {\n"
                                       "unsigned int i;\n"
                                       "i = 5;\n"
                                       "return ~i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        CHECK(cld::Tests::computeInJIT<unsigned int(void)>(std::move(module), "foo") == ~5u);
    }
    SECTION("Boolean negate")
    {
        SECTION("Int")
        {
            SECTION("False")
            {
                auto program = generateProgram("int foo(unsigned i) {\n"
                                               " return !i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                CHECK(cld::Tests::computeInJIT<int(unsigned)>(std::move(module), "foo", 1) == 0);
            }
            SECTION("True")
            {
                auto program = generateProgram("int foo(unsigned i) {\n"
                                               " return !i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                CHECK(cld::Tests::computeInJIT<int(unsigned)>(std::move(module), "foo", 0) == 1);
            }
        }
        SECTION("Float")
        {
            SECTION("False")
            {
                auto program = generateProgram("int foo(float i) {\n"
                                               " return !i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                CHECK(cld::Tests::computeInJIT<int(float)>(std::move(module), "foo", 1) == 0);
            }
            SECTION("True")
            {
                auto program = generateProgram("int foo(float i) {\n"
                                               " return !i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                CHECK(cld::Tests::computeInJIT<int(float)>(std::move(module), "foo", 0) == 1);
            }
        }
        SECTION("Pointer")
        {
            SECTION("False")
            {
                auto program = generateProgram("int foo(float* i) {\n"
                                               " return !i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                float f;
                CHECK(cld::Tests::computeInJIT<int(float*)>(std::move(module), "foo", &f) == 0);
            }
            SECTION("True")
            {
                auto program = generateProgram("int foo(float* i) {\n"
                                               " return !i;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                CHECK(cld::Tests::computeInJIT<int(float*)>(std::move(module), "foo", nullptr) == 1);
            }
        }
    }
}

TEST_CASE("LLVM codegen member access", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Struct")
    {
        SECTION("Simple struct")
        {
            auto program = generateProgram("typedef struct Point {\n"
                                           "float x,y;\n"
                                           "} Point;\n"
                                           "\n"
                                           "float foo(void) {\n"
                                           "    Point point;\n"
                                           "    point.x = 5;\n"
                                           "    point.y = 3;\n"
                                           "    return point.x + point.y;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 8.0f);
        }
        SECTION("Simple anonymous struct")
        {
            auto program = generateProgram("typedef struct{\n"
                                           "float x,y;\n"
                                           "} Point;\n"
                                           "\n"
                                           "float foo(void) {\n"
                                           "    Point point;\n"
                                           "    point.x = 5;\n"
                                           "    point.y = 3;\n"
                                           "    return point.x + point.y;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 8.0f);
        }
        SECTION("Bitfield")
        {
            auto program = generateProgram("struct A {\n"
                                           "unsigned int f:13,r:13;\n"
                                           "};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "struct A a;\n"
                                           "a.r = 0xFFFF;\n"
                                           "return a.r;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 0x1FFF);
        }
        SECTION("As pointer")
        {
            SECTION("Simple struct")
            {
                auto program = generateProgram("typedef struct Point {\n"
                                               "float x,y;\n"
                                               "} Point;\n"
                                               "\n"
                                               "float foo(void) {\n"
                                               "    Point point;\n"
                                               "    Point* ptr;\n"
                                               "    ptr = &point;"
                                               "    ptr->x = 5;\n"
                                               "    ptr->y = 3;\n"
                                               "    return ptr->x + ptr->y;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 8.0f);
            }
            SECTION("Simple anonymous struct")
            {
                auto program = generateProgram("typedef struct{\n"
                                               "float x,y;\n"
                                               "} Point;\n"
                                               "\n"
                                               "float foo(void) {\n"
                                               "    Point point;\n"
                                               "    Point* ptr;\n"
                                               "    ptr = &point;"
                                               "    ptr->x = 5;\n"
                                               "    ptr->y = 3;\n"
                                               "    return ptr->x + ptr->y;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 8.0f);
            }
            SECTION("Bitfield")
            {
                auto program = generateProgram("struct A {\n"
                                               "unsigned int f:13,r:13;\n"
                                               "};\n"
                                               "\n"
                                               "int foo(void) {\n"
                                               "struct A a;\n"
                                               "struct A* ptr;\n"
                                               "ptr = &a;\n"
                                               "ptr->r = 0xFFFF;\n"
                                               "return ptr->r;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 0x1FFF);
            }
        }
    }
    SECTION("Union")
    {
        SECTION("Simple Union")
        {
            auto program = generateProgram("typedef union U {\n"
                                           "float x;\n"
                                           "char storage[sizeof(float)];\n"
                                           "} U;\n"
                                           "\n"
                                           "char foo(void) {\n"
                                           "    U u;\n"
                                           "    u.x = 5;\n"
                                           "    return u.storage[3];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<char(void)>(std::move(module), "foo") > 0);
        }
        SECTION("Simple anonymous union")
        {
            auto program = generateProgram("typedef union {\n"
                                           "float x;\n"
                                           "char storage[sizeof(float)];\n"
                                           "} U;\n"
                                           "\n"
                                           "char foo(void) {\n"
                                           "    U u;\n"
                                           "    u.x = 5;\n"
                                           "    return u.storage[3];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<char(void)>(std::move(module), "foo") > 0);
        }
        SECTION("Bitfield")
        {
            auto program = generateProgram("union A {\n"
                                           "unsigned int f:13,r:13;\n"
                                           "};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "union A a;\n"
                                           "a.r = 0xFFFF;\n"
                                           "return a.r;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 0x1FFF);
        }
        SECTION("As pointer")
        {
            SECTION("Simple Union")
            {
                auto program = generateProgram("typedef union U {\n"
                                               "float x;\n"
                                               "char storage[sizeof(float)];\n"
                                               "} U;\n"
                                               "\n"
                                               "char foo(void) {\n"
                                               "    U u;\n"
                                               "    U* ptr;\n"
                                               "    ptr = &u;\n"
                                               "    ptr->x = 5;\n"
                                               "    return ptr->storage[3];\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(cld::Tests::computeInJIT<char(void)>(std::move(module), "foo") > 0);
            }
            SECTION("Simple anonymous union")
            {
                auto program = generateProgram("typedef union {\n"
                                               "float x;\n"
                                               "char storage[sizeof(float)];\n"
                                               "} U;\n"
                                               "\n"
                                               "char foo(void) {\n"
                                               "    U u;\n"
                                               "    U* ptr;\n"
                                               "    ptr = &u;\n"
                                               "    ptr->x = 5;\n"
                                               "    return ptr->storage[3];\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(cld::Tests::computeInJIT<char(void)>(std::move(module), "foo") > 0);
            }
            SECTION("Bitfield")
            {
                auto program = generateProgram("union A {\n"
                                               "unsigned int f:13,r:13;\n"
                                               "};\n"
                                               "\n"
                                               "int foo(void) {\n"
                                               "union A a;\n"
                                               "union A* ptr;\n"
                                               "ptr = &a;\n"
                                               "ptr->r = 0xFFFF;\n"
                                               "return ptr->r;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 0x1FFF);
            }
        }
    }
    SECTION("From function return")
    {
        auto program = generateProgram("typedef struct Point {\n"
                                       "float x,y;\n"
                                       "} Point;\n"
                                       "\n"
                                       "Point getPoint(void) {\n"
                                       "Point p;\n"
                                       "p.x = 5;\n"
                                       "p.y = 3;\n"
                                       "return p;\n"
                                       "}\n"
                                       "\n"
                                       "float foo(void) {\n"
                                       "return getPoint().x + getPoint().y;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<float(void)>(std::move(module), "foo") == 8.0f);
    }
}

TEST_CASE("LLVM codegen lvalues", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Primitives")
    {
        auto program = generateProgram("int foo(void) {\n"
                                       "int r;\n"
                                       "r = 5;\n"
                                       "return r;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
    }
    SECTION("Array")
    {
        auto program = generateProgram("void* foo(void) {\n"
                                       "int r[3];\n"
                                       "return r;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<void*()>(std::move(module), "foo") != nullptr);
    }
    SECTION("Function")
    {
        auto program = generateProgram("void bar(void) {return;}\n"
                                       "\n"
                                       "void (*foo(void))(void) {\n"
                                       "return bar;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<void (*())()>(std::move(module), "foo") != nullptr);
    }
}

TEST_CASE("LLVM codegen binary expressions", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Addition")
    {
        SECTION("Integers")
        {
            auto program = generateProgram("int add(int r,int f) {\n"
                                           "return r + f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("add nsw"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "add", 5, 10) == 15);
        }
        SECTION("Unsigned integers")
        {
            auto program = generateProgram("unsigned int add(unsigned int r,unsigned int f) {\n"
                                           "return r + f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, !ContainsIR("add nsw") && ContainsIR("add"));
            CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "add", 5, 10) == 15);
        }
        SECTION("Floating point type")
        {
            auto program = generateProgram("float add(float r,float f) {\n"
                                           "return r + f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(float, float)>(std::move(module), "add", 5, 10) == 15);
        }
        SECTION("Pointer types")
        {
            SECTION("Pointer left")
            {
                auto program = generateProgram("float add(float* r,int f) {\n"
                                               "return *(r + f);\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                std::array<float, 2> a = {1, 2};
                CHECK(cld::Tests::computeInJIT<float(float*, int)>(std::move(module), "add", a.data(), 1) == 2);
            }
            SECTION("Pointer right")
            {
                auto program = generateProgram("float add(float* r,int f) {\n"
                                               "return *(f + r);\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                std::array<float, 2> a = {1, 2};
                CHECK(cld::Tests::computeInJIT<float(float*, int)>(std::move(module), "add", a.data(), 1) == 2);
            }
        }
    }
    SECTION("Subtraction")
    {
        SECTION("Integers")
        {
            auto program = generateProgram("int sub(int r,int f) {\n"
                                           "return r - f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("sub nsw"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "sub", 10, 5) == 5);
        }
        SECTION("Unsigned Integers")
        {
            auto program = generateProgram("unsigned sub(unsigned r,unsigned f) {\n"
                                           "return r - f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, !ContainsIR("sub nsw") && ContainsIR("sub"));
            CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "sub", 10, 5) == 5);
        }
        SECTION("Floating point type")
        {
            auto program = generateProgram("float sub(float r,float f) {\n"
                                           "return r - f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(float, float)>(std::move(module), "sub", 5, 10) == -5.0);
        }
        SECTION("Pointer types")
        {
            SECTION("With integer")
            {
                auto program = generateProgram("float sub(float* r,int f) {\n"
                                               "return *(r - f);\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                std::array<float, 2> a = {1, 2};
                CHECK(cld::Tests::computeInJIT<float(float*, int)>(std::move(module), "sub", a.data() + 2, 1) == 2);
            }
            SECTION("With pointer")
            {
                auto program = generateProgram("long long sub(float* r,float* f) {\n"
                                               "return r - f;\n"
                                               "}");
                cld::CGLLVM::generateLLVM(*module, program);
                CAPTURE(*module);
                REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
                std::array<float, 2> a = {1, 2};
                CHECK(cld::Tests::computeInJIT<long long(float*, float*)>(std::move(module), "sub", a.data() + 2,
                                                                          a.data())
                      == 2);
            }
        }
    }
    SECTION("Multiply")
    {
        SECTION("Integers")
        {
            auto program = generateProgram("int mul(int r,int f) {\n"
                                           "return r * f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("mul nsw"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "mul", 3, 5) == 15);
        }
        SECTION("Unsigned Integers")
        {
            auto program = generateProgram("unsigned mul(unsigned r,unsigned f) {\n"
                                           "return r * f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, !ContainsIR("mul nsw") && ContainsIR("mul"));
            CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "mul", 3, 5) == 15);
        }
        SECTION("Floats")
        {
            auto program = generateProgram("float mul(float r,float f) {\n"
                                           "return r * f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(float, float)>(std::move(module), "mul", 3, 5) == 15);
        }
    }
    SECTION("Divide")
    {
        SECTION("Integers")
        {
            auto program = generateProgram("int div(int r,int f) {\n"
                                           "return r / f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("sdiv"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "div", 30, 5) == 6);
        }
        SECTION("Unsigned Integers")
        {
            auto program = generateProgram("unsigned div(unsigned r,unsigned f) {\n"
                                           "return r / f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("udiv"));
            CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "div", 30, 5) == 6);
        }
        SECTION("Floats")
        {
            auto program = generateProgram("float div(float r,float f) {\n"
                                           "return r / f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(float, float)>(std::move(module), "div", 3, 5) == 3.0f / 5.0f);
        }
    }
    SECTION("Modulo")
    {
        SECTION("Integers")
        {
            auto program = generateProgram("int mod(int r,int f) {\n"
                                           "return r % f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("srem"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "mod", 30, 8) == 6);
        }
        SECTION("Unsigned Integers")
        {
            auto program = generateProgram("unsigned mod(unsigned r,unsigned f) {\n"
                                           "return r % f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("urem"));
            CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "mod", 30, 8) == 6);
        }
        SECTION("With arithmetic conversion")
        {
            auto program = generateProgramWithOptions("unsigned int foo(long i) {\n"
                                                      "return i % 5u;\n"
                                                      "}",
                                                      x64windowsGnu);
            cld::CGLLVM::generateLLVM(*module, program, x64windowsGnu);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("urem"));
        }
    }
    SECTION("Left shift")
    {
        auto program = generateProgram("int lshift(int r,int f) {\n"
                                       "return r << f;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "lshift", 1, 7) == 128);
    }
    SECTION("Right shift")
    {
        SECTION("signed")
        {
            auto program = generateProgram("int rshift(int r,int f) {\n"
                                           "return r >> f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("ashr"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "rshift", 127, 4) == 7);
        }
        SECTION("unsigned")
        {
            auto program = generateProgram("unsigned int rshift(unsigned int r,unsigned int f) {\n"
                                           "return r >> f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("lshr"));
            CHECK(
                cld::Tests::computeInJIT<unsigned int(unsigned int, unsigned int)>(std::move(module), "rshift", 127, 4)
                == 7);
        }
    }
    SECTION("Greater than")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int cmp(int r,int f) {\n"
                                           "return r > f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp sgt"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "cmp", 4, 4) == 0);
        }
        SECTION("Unsigned")
        {
            auto program = generateProgram("int cmp(unsigned int r,unsigned int f) {\n"
                                           "return r > f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp ugt"));
            CHECK(cld::Tests::computeInJIT<int(unsigned, unsigned)>(std::move(module), "cmp", 127, 4) == 1);
        }
        SECTION("Float")
        {
            auto program = generateProgram("int cmp(float r,float f) {\n"
                                           "return r > f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float, float)>(std::move(module), "cmp", 127, 4) == 1);
        }
    }
    SECTION("Less or equal")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int cmp(int r,int f) {\n"
                                           "return r <= f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp sle"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "cmp", 4, 4) == 1);
        }
        SECTION("Unsigned")
        {
            auto program = generateProgram("int cmp(unsigned int r,unsigned int f) {\n"
                                           "return r <= f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp ule"));
            CHECK(cld::Tests::computeInJIT<int(unsigned, unsigned)>(std::move(module), "cmp", 127, 4) == 0);
        }
        SECTION("Float")
        {
            auto program = generateProgram("int cmp(float r,float f) {\n"
                                           "return r <= f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float, float)>(std::move(module), "cmp", 127, 4) == 0);
        }
    }
    SECTION("Greater or equal")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int cmp(int r,int f) {\n"
                                           "return r >= f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp sge"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "cmp", 4, 127) == 0);
        }
        SECTION("Unsigned")
        {
            auto program = generateProgram("int cmp(unsigned int r,unsigned int f) {\n"
                                           "return r >= f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp uge"));
            CHECK(cld::Tests::computeInJIT<int(unsigned, unsigned)>(std::move(module), "cmp", 127, 4) == 1);
        }
        SECTION("Float")
        {
            auto program = generateProgram("int cmp(float r,float f) {\n"
                                           "return r >= f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float, float)>(std::move(module), "cmp", 127, 4) == 1);
        }
    }
    SECTION("Equal")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int cmp(int r,unsigned int f) {\n"
                                           "return r == f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(int, unsigned)>(std::move(module), "cmp", -1, -1) == 1);
        }
        SECTION("Float")
        {
            auto program = generateProgram("int cmp(float r,float f) {\n"
                                           "return r == f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float, float)>(std::move(module), "cmp", 127, 4) == 0);
        }
        SECTION("non void pointers")
        {
            auto program = generateProgram("int cmp(int* r,int* f) {\n"
                                           "return r == f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i;
            CHECK(cld::Tests::computeInJIT<int(int*, int*)>(std::move(module), "cmp", &i, &i) == 1);
        }
        SECTION("Null pointer constants")
        {
            auto program = generateProgram("int cmp(int* r) {\n"
                                           "return r == 0;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i;
            CHECK(cld::Tests::computeInJIT<int(int*)>(std::move(module), "cmp", &i) == 0);
        }
        SECTION("void pointers")
        {
            auto program = generateProgram("int cmp(int* r,void* f) {\n"
                                           "return r == f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i;
            CHECK(cld::Tests::computeInJIT<int(int*, int*)>(std::move(module), "cmp", &i, &i) == 1);
        }
    }
    SECTION("Not Equal")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int cmp(int r,unsigned int f) {\n"
                                           "return r != f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(int, unsigned)>(std::move(module), "cmp", -1, -1) == 0);
        }
        SECTION("Float")
        {
            auto program = generateProgram("int cmp(float r,float f) {\n"
                                           "return r != f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float, float)>(std::move(module), "cmp", 127, 4) == 1);
        }
        SECTION("non void pointers")
        {
            auto program = generateProgram("int cmp(int* r,int* f) {\n"
                                           "return r != f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i;
            CHECK(cld::Tests::computeInJIT<int(int*, int*)>(std::move(module), "cmp", &i, &i) == 0);
        }
        SECTION("Null pointer constants")
        {
            auto program = generateProgram("int cmp(int* r) {\n"
                                           "return r != 0;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i;
            CHECK(cld::Tests::computeInJIT<int(int*)>(std::move(module), "cmp", &i) == 1);
        }
        SECTION("void pointers")
        {
            auto program = generateProgram("int cmp(int* r,void* f) {\n"
                                           "return r != f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i;
            CHECK(cld::Tests::computeInJIT<int(int*, int*)>(std::move(module), "cmp", &i, &i) == 0);
        }
    }
    SECTION("Less than")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int cmp(int r,int f) {\n"
                                           "return r < f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp slt"));
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "cmp", 4, 4) == 0);
        }
        SECTION("Unsigned")
        {
            auto program = generateProgram("int cmp(unsigned int r,unsigned int f) {\n"
                                           "return r < f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK_THAT(*module, ContainsIR("icmp ult"));
            CHECK(cld::Tests::computeInJIT<int(unsigned, unsigned)>(std::move(module), "cmp", 4, 127) == 1);
        }
        SECTION("Float")
        {
            auto program = generateProgram("int cmp(float r,float f) {\n"
                                           "return r < f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float, float)>(std::move(module), "cmp", 127, 4) == 0);
        }
    }
    SECTION("Bit or")
    {
        auto program = generateProgram("unsigned or(unsigned r,unsigned f) {\n"
                                       "return r | f;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "or", 5, 2) == 7);
    }
    SECTION("Bit and")
    {
        auto program = generateProgram("unsigned or(unsigned r,unsigned f) {\n"
                                       "return r & f;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "or", 5, 3) == 1);
    }
    SECTION("Bit xor")
    {
        auto program = generateProgram("unsigned or(unsigned r,unsigned f) {\n"
                                       "return r ^ f;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<unsigned(unsigned, unsigned)>(std::move(module), "or", 5, 3) == 6);
    }
    SECTION("&&")
    {
        SECTION("False")
        {
            auto program = generateProgram("void and(unsigned value,unsigned* check) {\n"
                                           "value && ++*check;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned number = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*)>(std::move(module), "and", 0, &number);
            CHECK(number == 0);
        }
        SECTION("True")
        {
            auto program = generateProgram("void and(unsigned value,unsigned* check) {\n"
                                           "value && ++*check;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned number = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*)>(std::move(module), "and", 1, &number);
            CHECK(number == 1);
        }
    }
    SECTION("||")
    {
        SECTION("False")
        {
            auto program = generateProgram("void or(unsigned value,unsigned* check) {\n"
                                           "value || ++*check;\n"

                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned number = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*)>(std::move(module), "or", 0, &number);
            CHECK(number == 1);
        }
        SECTION("True")
        {
            auto program = generateProgram("void or(unsigned value,unsigned* check) {\n"
                                           "value || ++*check;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned number = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*)>(std::move(module), "or", 1, &number);
            CHECK(number == 0);
        }
    }
}

TEST_CASE("LLVM codegen casts", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Integer to pointer")
    {
        auto program = generateProgram("int* bool(int i) {\n"
                                       "return (int*)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int*(int)>(std::move(module), "bool", 5) == (int*)5);
    }
    SECTION("Pointer to pointer")
    {
        auto program = generateProgram("int* bool(float* i) {\n"
                                       "return (int*)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int*(float*)>(std::move(module), "bool", (float*)5) == (int*)5);
    }
    SECTION("Int to bool")
    {
        auto program = generateProgram("int bool(int i) {\n"
                                       "return (_Bool)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "bool", 5) == 1);
    }
    SECTION("Float to bool")
    {
        auto program = generateProgram("int bool(float i) {\n"
                                       "return (_Bool)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(float)>(std::move(module), "bool", 0) == 0);
    }
    SECTION("Pointer to bool")
    {
        auto program = generateProgram("int bool(float* i) {\n"
                                       "return (_Bool)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        float f;
        CHECK(cld::Tests::computeInJIT<int(float*)>(std::move(module), "bool", &f) == 1);
    }
    SECTION("Int to int")
    {
        auto program = generateProgram("long bool(char i) {\n"
                                       "return (long)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<long(char)>(std::move(module), "bool", 3) == 3);
    }
    SECTION("Int to float")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("float bool(int i) {\n"
                                           "return (float)i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(int)>(std::move(module), "bool", -3) == -3.0f);
        }
        SECTION("Unsigned")
        {
            auto program = generateProgram("float bool(unsigned i) {\n"
                                           "return (float)i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float(unsigned int)>(std::move(module), "bool", 3) == 3.0f);
        }
    }
    SECTION("Float to int")
    {
        SECTION("Signed")
        {
            auto program = generateProgram("int bool(float i) {\n"
                                           "return (int)i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(float)>(std::move(module), "bool", -3.534) == -3);
        }
        SECTION("Unsigned")
        {
            auto program = generateProgram("unsigned bool(float i) {\n"
                                           "return (unsigned)i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<unsigned(float)>(std::move(module), "bool", 3.534) == 3);
        }
    }
    SECTION("Pointer to int")
    {
        auto program = generateProgram("long bool(int* i) {\n"
                                       "return (long)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<long(int*)>(std::move(module), "bool", (int*)5) == 5);
    }
    SECTION("Floating point cast")
    {
        auto program = generateProgram("double bool(float i) {\n"
                                       "return (double)i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<double(float)>(std::move(module), "bool", 5.0) == 5.0);
    }
}

TEST_CASE("LLVM codegen conditional expressions", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("True")
    {
        auto program = generateProgram("void or(unsigned value,unsigned* true,unsigned int* false) {\n"
                                       "value ? ++*true : ++*false;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        unsigned first = 0, second = 0;
        cld::Tests::computeInJIT<void(unsigned, unsigned*, unsigned*)>(std::move(module), "or", 1, &first, &second);
        CHECK(first == 1);
        CHECK(second == 0);
    }
    SECTION("False")
    {
        auto program = generateProgram("void or(unsigned value,unsigned* true,unsigned int* false) {\n"
                                       "value ? ++*true : ++*false;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        unsigned first = 0, second = 0;
        cld::Tests::computeInJIT<void(unsigned, unsigned*, unsigned*)>(std::move(module), "or", 0, &first, &second);
        CHECK(first == 0);
        CHECK(second == 1);
    }
}

namespace
{
void varargTest1(int* success, ...)
{
    struct R
    {
        int c;
        float f;
    };

    va_list args;
    va_start(args, success);
    auto r = va_arg(args, R);
    if (r.c == 5 && r.f == 3)
    {
        *success = true;
    }
    va_end(args);
}

void varargTest2(int* success, ...)
{
    struct R
    {
        float f[2];
        double r;
    };
    va_list args;
    va_start(args, success);
    auto r = va_arg(args, R);
    va_end(args);
    if (r.r == 3.5 && r.f[0] == 3456.34f && r.f[1] == 4356.2134f)
    {
        *success = true;
    }
}

void varargTest3(int* success, ...)
{
    struct R
    {
        float f[8];
    };
    va_list args;
    va_start(args, success);
    auto r = va_arg(args, R);
    va_end(args);
    for (std::size_t i = 0; i < 8; i++)
    {
        if (r.f[i] != i)
        {
            return;
        }
    }
    *success = true;
}
} // namespace

TEST_CASE("LLVM codegen function call", "[LLVM]")
{
    // This test case uses lambdas which according to the C++ standard don't have C linkage but Compilers allow it
    // so we'll use it anyways
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Functions without prototypes")
    {
        SECTION("Passing in struct of size 64")
        {
            struct R
            {
                int c;
                float f;
            };
            auto program = generateProgram("struct R {\n"
                                           "int c;\n"
                                           "float f;\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(),int* success) {\n"
                                           "struct R r;\n"
                                           "r.c = 5;\n"
                                           "r.f = 3;\n"
                                           "toCall(r,success);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* func = +[](struct R r, int* success) {
                if (r.c == 5 && r.f == 3)
                {
                    *success = true;
                }
            };
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(R, int*), int*)>(std::move(module), "function", func, &success);
            CHECK(success);
        }
        SECTION("Passing in struct of size 128")
        {
            struct R
            {
                float f[2];
                double r;
            };
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "double r;\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(),int* success) {\n"
                                           "struct R r;\n"
                                           "r.r = 3.5;\n"
                                           "r.f[0] = 3456.34;\n"
                                           "r.f[1] = 4356.2134;\n"
                                           "toCall(r,success);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* func = +[](struct R r, int* success) {
                if (r.r == 3.5 && r.f[0] == 3456.34f && r.f[1] == 4356.2134f)
                {
                    *success = true;
                }
            };
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(R, int*), int*)>(std::move(module), "function", func, &success);
            CHECK(success);
        }
        SECTION("Passing in large struct")
        {
            struct R
            {
                float f[8];
            };
            auto program = generateProgram("struct R {\n"
                                           "float f[8];\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(),int* success) {\n"
                                           "struct R r;\n"
                                           "r.f[0] = 0;\n"
                                           "r.f[1] = 1;\n"
                                           "r.f[2] = 2;\n"
                                           "r.f[3] = 3;\n"
                                           "r.f[4] = 4;\n"
                                           "r.f[5] = 5;\n"
                                           "r.f[6] = 6;\n"
                                           "r.f[7] = 7;\n"
                                           "toCall(r,success);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* func = +[](struct R r, int* success) {
                for (std::size_t i = 0; i < 8; i++)
                {
                    if (r.f[i] != i)
                    {
                        return;
                    }
                }
                *success = true;
            };
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(R, int*), int*)>(std::move(module), "function", func, &success);
            CHECK(success);
        }
    }
    SECTION("Functions with vararg")
    {
        SECTION("Passing in struct of size 64")
        {
            auto program = generateProgram("struct R {\n"
                                           "int c;\n"
                                           "float f;\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(int*,...),int* success) {\n"
                                           "struct R r;\n"
                                           "r.c = 5;\n"
                                           "r.f = 3;\n"
                                           "toCall(success,r);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));

            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(int*, ...), int*)>(std::move(module), "function", varargTest1,
                                                                      &success);
            CHECK(success);
        }
        SECTION("Passing in struct of size 128")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "double r;\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(int*,...),int* success) {\n"
                                           "struct R r;\n"
                                           "r.r = 3.5;\n"
                                           "r.f[0] = 3456.34;\n"
                                           "r.f[1] = 4356.2134;\n"
                                           "toCall(success,r);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(int*, ...), int*)>(std::move(module), "function", varargTest2,
                                                                      &success);
            CHECK(success);
        }
        SECTION("Passing in large struct")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f[8];\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(int*,...),int* success) {\n"
                                           "struct R r;\n"
                                           "r.f[0] = 0;\n"
                                           "r.f[1] = 1;\n"
                                           "r.f[2] = 2;\n"
                                           "r.f[3] = 3;\n"
                                           "r.f[4] = 4;\n"
                                           "r.f[5] = 5;\n"
                                           "r.f[6] = 6;\n"
                                           "r.f[7] = 7;\n"
                                           "toCall(success,r);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(int*, ...), int*)>(std::move(module), "function", varargTest3,
                                                                      &success);
            CHECK(success);
        }
    }
    SECTION("Functions with prototypes")
    {
        SECTION("Passing in struct of size 64")
        {
            struct R
            {
                int c;
                float f;
            };
            auto program = generateProgram("struct R {\n"
                                           "int c;\n"
                                           "float f;\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(struct R,int* i),int* success) {\n"
                                           "struct R r;\n"
                                           "r.c = 5;\n"
                                           "r.f = 3;\n"
                                           "toCall(r,success);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* func = +[](struct R r, int* success) {
                if (r.c == 5 && r.f == 3)
                {
                    *success = true;
                }
            };
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(R, int*), int*)>(std::move(module), "function", func, &success);
            CHECK(success);
        }
        SECTION("Passing in struct of size 128")
        {
            struct R
            {
                float f[2];
                double r;
            };
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "double r;\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(struct R,int* i),int* success) {\n"
                                           "struct R r;\n"
                                           "r.r = 3.5;\n"
                                           "r.f[0] = 3456.34;\n"
                                           "r.f[1] = 4356.2134;\n"
                                           "toCall(r,success);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* func = +[](struct R r, int* success) {
                if (r.r == 3.5 && r.f[0] == 3456.34f && r.f[1] == 4356.2134f)
                {
                    *success = true;
                }
            };
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(R, int*), int*)>(std::move(module), "function", func, &success);
            CHECK(success);
        }
        SECTION("Passing in large struct")
        {
            struct R
            {
                float f[8];
            };
            auto program = generateProgram("struct R {\n"
                                           "float f[8];\n"
                                           "};\n"
                                           "\n"
                                           "void function(void (*toCall)(struct R,int* i),int* success) {\n"
                                           "struct R r;\n"
                                           "r.f[0] = 0;\n"
                                           "r.f[1] = 1;\n"
                                           "r.f[2] = 2;\n"
                                           "r.f[3] = 3;\n"
                                           "r.f[4] = 4;\n"
                                           "r.f[5] = 5;\n"
                                           "r.f[6] = 6;\n"
                                           "r.f[7] = 7;\n"
                                           "toCall(r,success);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            auto* func = +[](struct R r, int* success) {
                for (std::size_t i = 0; i < 8; i++)
                {
                    if (r.f[i] != i)
                    {
                        return;
                    }
                }
                *success = true;
            };
            int success = 0;
            cld::Tests::computeInJIT<void(void (*)(R, int*), int*)>(std::move(module), "function", func, &success);
            CHECK(success);
        }
    }
    SECTION("Returning struct of size 64")
    {
        struct R
        {
            int c;
            float f;
        };
        auto program = generateProgram("struct R {\n"
                                       "int c;\n"
                                       "float f;\n"
                                       "};\n"
                                       "\n"
                                       "int function(struct R (*toCall)(void)) {\n"
                                       "struct R r = toCall();\n"
                                       "return r.c == 5 && r.f == 3;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        auto* func = +[]() -> struct R
        {
            return {5, 3};
        };
        CHECK(cld::Tests::computeInJIT<int(R(*)())>(std::move(module), "function", func) == 1);
    }
    SECTION("Returning large struct")
    {
        struct R
        {
            float f[8];
        };
        auto program =
            generateProgram("struct R {\n"
                            "float f[8];\n"
                            "};\n"
                            "\n"
                            "int function(struct R (*toCall)(void)) {\n"
                            "struct R r = toCall();\n"
                            "return r.f[0] == 0 && r.f[1] == 1 && r.f[2] == 2 && r.f[3] == 3 && r.f[4] == 4\n"
                            "&& r.f[5] == 5 && r.f[6] == 6 && r.f[7] == 7;\n"
                            "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        auto* func = +[]() -> struct R
        {
            struct R r;
            std::iota(std::begin(r.f), std::end(r.f), 0);
            return r;
        };
        CHECK(cld::Tests::computeInJIT<int(R(*)())>(std::move(module), "function", func) == 1);
    }
}

TEST_CASE("LLVM codegen assignment", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Simple")
    {
        SECTION("Without operator")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 5;\n"
                                           "bar = 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 7);
        }
        SECTION("+=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "char bar;\n"
                                           "bar = 5;\n"
                                           "bar += 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 12);
        }
        SECTION("-=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "float bar;\n"
                                           "bar = 5;\n"
                                           "bar -= 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == -2);
        }
        SECTION("/=")
        {
            auto program = generateProgram("float foo(void) {\n"
                                           "float bar;\n"
                                           "bar = 5;\n"
                                           "bar /= 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float()>(std::move(module), "foo") == 5.f / 7);
        }
        SECTION("*=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "float bar;\n"
                                           "bar = 5;\n"
                                           "bar *= 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 35);
        }
        SECTION("%=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 50;\n"
                                           "bar %= 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 50 % 7);
        }
        SECTION("<<=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 1;\n"
                                           "bar <<= 7;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 1 << 7);
        }
        SECTION(">>=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 100;\n"
                                           "bar >>= 2;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 25);
        }
        SECTION("&=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 100;\n"
                                           "bar &= ~32;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 68);
        }
        SECTION("|=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 100;\n"
                                           "bar |= 8;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 108);
        }
        SECTION("^=")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int bar;\n"
                                           "bar = 100;\n"
                                           "bar ^= 12;\n"
                                           "return bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 104);
        }
    }
    SECTION("Bitfields")
    {
        SECTION("Without operator")
        {
            auto program = generateProgram("struct R {\n"
                                           "unsigned int f : 13;\n"
                                           "unsigned int bar : 13;\n"
                                           "};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "struct R r;\n"
                                           "r.bar = 5;\n"
                                           "r.f = 5;\n"
                                           "r.bar = 0xFFFF;\n"
                                           "return r.bar == 0x1FFF && r.f == 5;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") != 0);
        }
        SECTION("+=")
        {
            auto program = generateProgram("struct R {\n"
                                           "int bar : 13;\n"
                                           "};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "struct R r;\n"
                                           "r.bar = 5;\n"
                                           "r.bar += 7;\n"
                                           "return r.bar;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 12);
        }
    }
}

TEST_CASE("LLVM codegen if statement", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("if")
    {
        SECTION("True")
        {
            auto program = generateProgram("void or(unsigned value,unsigned* true) {\n"
                                           "if(value) {\n"
                                           "++*true;\n"
                                           "}\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned first = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*)>(std::move(module), "or", 1, &first);
            CHECK(first == 1);
        }
        SECTION("False")
        {
            auto program = generateProgram("void or(unsigned value,unsigned* true) {\n"
                                           "if(value) {\n"
                                           "++*true;\n"
                                           "}\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned first = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*)>(std::move(module), "or", 0, &first);
            CHECK(first == 0);
        }
    }
    SECTION("if else")
    {
        SECTION("True")
        {
            auto program = generateProgram("void or(unsigned value,unsigned* true,unsigned int* false) {\n"
                                           "if(value) {\n"
                                           "++*true;\n"
                                           "} else {\n"
                                           "++*false;\n"
                                           "}\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned first = 0, second = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*, unsigned*)>(std::move(module), "or", 1, &first, &second);
            CHECK(first == 1);
            CHECK(second == 0);
        }
        SECTION("False")
        {
            auto program = generateProgram("void or(unsigned value,unsigned* true,unsigned int* false) {\n"
                                           "if(value) {\n"
                                           "++*true;\n"
                                           "} else {\n"
                                           "++*false;\n"
                                           "}\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            unsigned first = 0, second = 0;
            cld::Tests::computeInJIT<void(unsigned, unsigned*, unsigned*)>(std::move(module), "or", 0, &first, &second);
            CHECK(first == 0);
            CHECK(second == 1);
        }
    }
    SECTION("Control flow")
    {
        SECTION("Without dead code")
        {
            auto program = generateProgram("int threeOrFive(int value) {\n"
                                           "    if(value) {\n"
                                           "        return 5;\n"
                                           "    } else {\n"
                                           "        return 3;\n"
                                           "    }\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        }
        SECTION("Dead code")
        {
            auto program = generateProgram("int threeOrFive(int value) {\n"
                                           "    if(value) {\n"
                                           "        return 5;\n"
                                           "    } else {\n"
                                           "        return 3;\n"
                                           "    }\n"
                                           "    value *= 2; // Dead code \n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        }
    }
}

TEST_CASE("LLVM codegen for loop", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Initial declaration")
    {
        auto program = generateProgram("int incrementLoop(int n) {\n"
                                       "int i;\n"
                                       "for (i = 0;i < n; i++);\n"
                                       "return i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "incrementLoop", 5) == 5);
    }
    SECTION("Initial expression")
    {
        auto program = generateProgram("int incrementLoop(int n) {\n"
                                       "int r = 0;\n"
                                       "for (int i = 0;i < n; i++) {\n"
                                       "    r++;\n"
                                       "}\n"
                                       "return r;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "incrementLoop", 5) == 5);
    }
    SECTION("Without controlling")
    {
        auto program = generateProgram("int incrementLoop(int n) {\n"
                                       "int i;\n"
                                       "for (i = 0;; i++) {\n"
                                       "    if (i >= n) return i;\n"
                                       "}\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "incrementLoop", 5) == 5);
    }
    SECTION("Without iteration")
    {
        auto program = generateProgram("int incrementLoop(int n) {\n"
                                       "int i;\n"
                                       "for (i = 0;i < n;) {\n"
                                       "     i++;\n"
                                       "}\n"
                                       "return i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "incrementLoop", 5) == 5);
    }
}

TEST_CASE("LLVM codegen while loop", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    auto program = generateProgram("int incrementLoop(int n) {\n"
                                   "int i = 0;\n"
                                   "while (i < n) {\n"
                                   "     i++;\n"
                                   "}\n"
                                   "return i;\n"
                                   "}");
    cld::CGLLVM::generateLLVM(*module, program);
    CAPTURE(*module);
    REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
    CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "incrementLoop", 5) == 5);
}

TEST_CASE("LLVM codegen do while loop", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    auto program = generateProgram("int incrementLoop(int n) {\n"
                                   "int i = 0;\n"
                                   "do {\n"
                                   "     i++;\n"
                                   "} while (i < n);\n"
                                   "return i;\n"
                                   "}");
    cld::CGLLVM::generateLLVM(*module, program);
    CAPTURE(*module);
    REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
    CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "incrementLoop", 5) == 5);
}

TEST_CASE("LLVM codegen switch, case and default", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Simple")
    {
        auto program = generateProgram("static int switchFunc(int n) {\n"
                                       "switch (n) {\n"
                                       "case 3: return 5;\n"
                                       "case 5: return 3;\n"
                                       "default: return 7;\n"
                                       "}\n"
                                       "}"
                                       "\n"
                                       "int intoSwitch(void) {\n"
                                       "int first,second,third;\n"
                                       "first = switchFunc(3);\n"
                                       "second = switchFunc(5);\n"
                                       "third = switchFunc(300);\n"
                                       "return first == 5 && second == 3 && third == 7;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "intoSwitch") != 0);
    }
    SECTION("Fallthrough")
    {
        auto program = generateProgram("static int switchFunc(int n) {\n"
                                       "switch (n) {\n"
                                       "case 3: n += 5;\n"
                                       "case 5: n += 3;\n"
                                       "default: n += 7;\n"
                                       "}\n"
                                       "return n;\n"
                                       "}"
                                       "\n"
                                       "int intoSwitch(void) {\n"
                                       "int first,second,third;\n"
                                       "first = switchFunc(3);\n"
                                       "second = switchFunc(5);\n"
                                       "third = switchFunc(300);\n"
                                       "return first == 18 && second == 15 && third == 307;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "intoSwitch") != 0);
    }
    SECTION("In dead code")
    {
        auto program = generateProgram("static int switchFunc(int n) {\n"
                                       "return 0;\n"
                                       "switch (n) {\n"
                                       "case 3: return 5;\n"
                                       "case 5: return 3;\n"
                                       "default: return 7;\n"
                                       "}\n"
                                       "}\n"
                                       "\n"
                                       "int intoSwitch(void) {\n"
                                       "int first,second,third;\n"
                                       "first = switchFunc(3);\n"
                                       "second = switchFunc(5);\n"
                                       "third = switchFunc(300);\n"
                                       "return first == 0 && second == 0 && third == 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "intoSwitch") != 0);
    }
}

TEST_CASE("LLVM Codegen goto and labels", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Simple")
    {
        auto program = generateProgram("static int switchFunc(int n) {\n"
                                       "if (n == 3)\n"
                                       "    goto three;\n"
                                       "else if (n == 5) \n"
                                       "    goto five;\n"
                                       "else goto defaultCase;\n"
                                       "three:\n"
                                       "    return 5;\n"
                                       "five:\n"
                                       "    return 3;\n"
                                       "defaultCase:\n"
                                       "    return 7;\n"
                                       "}\n"
                                       "\n"
                                       "int intoSwitch(void) {\n"
                                       "int first,second,third;\n"
                                       "first = switchFunc(3);\n"
                                       "second = switchFunc(5);\n"
                                       "third = switchFunc(300);\n"
                                       "return first == 5 && second == 3 && third == 7;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "intoSwitch") != 0);
    }
}

TEST_CASE("LLVM Codegen initialization", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("static lifetime")
    {
        SECTION("Simple")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "double r;\n"
                                           "};\n"
                                           "\n"
                                           "float function(void) {\n"
                                           "static struct R r = {.r = 3.5,.f = {3456.34,4356.2134}};\n"
                                           "return r.r + r.f[0] + r.f[1];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            using namespace Catch::literals;
            CHECK(cld::Tests::computeInJIT<float()>(std::move(module), "function") == 7816.0534_a);
        }
        SECTION("Union")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "};\n"
                                           "\n"
                                           "union U {\n"
                                           "int i[10];\n"
                                           "struct R r;\n"
                                           "};\n"
                                           "\n"
                                           "float function(void) {\n"
                                           "static union U u = {.r.f = {3456.34,4356.2134}};\n"
                                           "return u.r.f[0] + u.r.f[1];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            using namespace Catch::literals;
            CHECK(cld::Tests::computeInJIT<float()>(std::move(module), "function") == 7812.5534_a);
        }
        SECTION("Bitfields")
        {
            auto program = generateProgram("struct A {\n"
                                           "unsigned int f:13,r:13;\n"
                                           "};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "static struct A a = {.r = 0xFFFF,.f = 12};\n"
                                           "return a.r == 0x1FFF && a.f == 12;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") != 0);
        }
        SECTION("String literals")
        {
            auto program = generateProgram("char c[] = \"text\";\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "return sizeof(c);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
        }
        SECTION("Nested string literal")
        {
            auto program = generateProgram("struct S {\n"
                                           "char c[50];\n"
                                           "};\n"
                                           "struct S s = {.c = \"text\",.c[0] = '4'};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "return s.c[2] == 'x' && s.c[0] == '4';\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") != 0);
        }
    }
    SECTION("automatic lifetime")
    {
        SECTION("Simple")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "double r;\n"
                                           "};\n"
                                           "\n"
                                           "float function(void) {\n"
                                           "struct R r = {.r = 3.5,.f = {3456.34,4356.2134}};\n"
                                           "return r.r + r.f[0] + r.f[1];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            using namespace Catch::literals;
            CHECK(cld::Tests::computeInJIT<float()>(std::move(module), "function") == 7816.0534_a);
        }
        SECTION("Union")
        {
            auto program = generateProgram("struct R {\n"
                                           "float f[2];\n"
                                           "};\n"
                                           "\n"
                                           "union U {\n"
                                           "int i[10];\n"
                                           "struct R r;\n"
                                           "};\n"
                                           "\n"
                                           "float function(void) {\n"
                                           "union U u = {.r.f = {3456.34,4356.2134}};\n"
                                           "return u.r.f[0] + u.r.f[1];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            using namespace Catch::literals;
            CHECK(cld::Tests::computeInJIT<float()>(std::move(module), "function") == 7812.5534_a);
        }
        SECTION("Bitfields")
        {
            auto program = generateProgram("struct A {\n"
                                           "unsigned int f:13,r:13;\n"
                                           "};\n"
                                           "\n"
                                           "int foo(void) {\n"
                                           "struct A a = {.r = 0xFFFF,.f = 12};\n"
                                           "return a.r == 0x1FFF && a.f == 12;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") != 0);
        }
        SECTION("String literals")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "char c[] = \"text\";\n"
                                           "return sizeof(c);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 5);
        }
        SECTION("Nested string literal")
        {
            auto program = generateProgram("struct S {\n"
                                           "char c[50];\n"
                                           "};"
                                           "\n"
                                           "int foo(void) {\n"
                                           "struct S s = {.c = \"text\"};"
                                           "return s.c[2];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int(void)>(std::move(module), "foo") == 'x');
        }
    }
}

TEST_CASE("LLVM Codegen variably modified types", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Variably modified type declaration")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    int (*r[n])[n*2];\n"
                                       "    return (int)r;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 3) != 0);
    }
    SECTION("array of a variable length array declaration")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    int r[n][5][3];\n"
                                       "    return (int)r;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 3) != 0);
    }
    SECTION("variable length array of an array declaration")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    int r[3][5][n][4];\n"
                                       "    return (int)r;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 3) != 0);
    }
    SECTION("Indexing")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    int r[3][5][n],counter = 0;\n"
                                       "    for (int i = 0; i < 3; i++)\n"
                                       "    {\n"
                                       "        for (int i2 = 0; i2 < 5; i2++)\n"
                                       "        {\n"
                                       "            for (int i3 = 0; i3 < n; i3++)\n"
                                       "            {\n"
                                       "                r[i][i2][i3] = counter++;\n"
                                       "            }\n"
                                       "        }\n"
                                       "    }\n"
                                       "    return r[2][3][4];\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 5) == 69);
    }
    SECTION("Pointer arithmetic")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    int r[3][5][n],counter = 0;\n"
                                       "    for (int i = 0; i < 3; i++)\n"
                                       "    {\n"
                                       "        for (int i2 = 0; i2 < 5; i2++)\n"
                                       "        {\n"
                                       "            for (int i3 = 0; i3 < n; i3++)\n"
                                       "            {\n"
                                       "                *(*(*(r + i) + i2)+i3) = counter++;\n"
                                       "            }\n"
                                       "        }\n"
                                       "    }\n"
                                       "    return *(*(*(r + 2)+3)+4);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 5) == 69);
    }
    SECTION("Sizeof")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    int r[3][5][n];\n"
                                       "    return sizeof(r);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 5) == 75 * sizeof(int));
    }
    SECTION("Function parameter")
    {
        auto program = generateProgram("static int bar(int n,int r[3][5][n]) {\n"
                                       "    return r[n/2][2][2];\n"
                                       "}\n"
                                       "\n"
                                       "int function(int n) {\n"
                                       "    int r[3][5][n],counter = 0;\n"
                                       "    r[n/2][2][2] = 5;\n"
                                       "    return bar(n,r);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 5) == 5);
    }
    SECTION("After dead code")
    {
        auto program = generateProgram("int function(int n) {\n"
                                       "    return n;\n"
                                       "    int r[3][5][n];\n"
                                       "    return sizeof(r);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int)>(std::move(module), "function", 5) == 5);
    }
    SECTION("Typedef evaluation")
    {
        auto program = generateProgram("int function(void) {\n"
                                       "    int count = 0;\n"
                                       "    for (int i = 0; i < 10; i++,count++) {\n"
                                       "        typedef int (*f)[i++];\n"
                                       "    }\n"
                                       "    return count;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 5);
    }
}

namespace
{
std::string text;

int printfCallback(const char* format, ...)
{
    va_list list;
    va_start(list, format);
    va_list copy;
    va_copy(copy, list);
    std::size_t size = std::vsnprintf(nullptr, 0, format, copy);
    va_end(copy);
    std::string buffer(size + 1, '\0');
    auto ret = vsnprintf(buffer.data(), buffer.size() + 1, format, list);
    buffer.resize(ret);
    va_end(list);
    text += buffer;
    return ret;
}
} // namespace

TEST_CASE("LLVM codegen c-testsuite", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("0019.c")
    {
        auto program = generateProgram("int\n"
                                       "function()\n"
                                       "{\n"
                                       "struct S { struct S *p; int x; } s;\n"
                                       "\n"
                                       "s.x = 0;\n"
                                       "s.p = &s;\n"
                                       "return s.p->p->p->p->p->x;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00033.c")
    {
        auto program = generateProgram("int g;\n"
                                       "\n"
                                       "int\n"
                                       "effect()\n"
                                       "{\n"
                                       "\tg = 1;\n"
                                       "\treturn 1;\n"
                                       "}\n"
                                       "\n"
                                       "int\n"
                                       "function()\n"
                                       "{\n"
                                       "    int x;\n"
                                       "    \n"
                                       "    g = 0;\n"
                                       "    x = 0;\n"
                                       "    if(x && effect())\n"
                                       "    \treturn 1;\n"
                                       "    if(g)\n"
                                       "    \treturn 2;\n"
                                       "    x = 1;\n"
                                       "    if(x && effect()) {\n"
                                       "    \tif(g != 1)\n"
                                       "    \t\treturn 3;\n"
                                       "    } else {\n"
                                       "    \treturn 4;\n"
                                       "    }\n"
                                       "    g = 0;\n"
                                       "    x = 1;\n"
                                       "    if(x || effect()) {\n"
                                       "    \tif(g)\n"
                                       "    \t\treturn 5;\n"
                                       "    } else {\n"
                                       "    \treturn 6;\n"
                                       "    }\n"
                                       "    x = 0;\n"
                                       "    if(x || effect()) {\n"
                                       "    \tif(g != 1)\n"
                                       "    \t\treturn 7;\n"
                                       "    } else {\n"
                                       "    \treturn 8;\n"
                                       "    } \n"
                                       "    return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00034.c")
    {
        auto program = generateProgram("int function() {\n"
                                       "  int x;\n"
                                       "\n"
                                       "  x = 0;\n"
                                       "  while (1) break;\n"
                                       "  while (1) {\n"
                                       "    if (x == 5) {\n"
                                       "      break;\n"
                                       "    }\n"
                                       "    x = x + 1;\n"
                                       "    continue;\n"
                                       "  }\n"
                                       "  for (;;) {\n"
                                       "    if (x == 10) {\n"
                                       "      break;\n"
                                       "    }\n"
                                       "    x = x + 1;\n"
                                       "    continue;\n"
                                       "  }\n"
                                       "  do {\n"
                                       "    if (x == 15) {\n"
                                       "      break;\n"
                                       "    }\n"
                                       "    x = x + 1;\n"
                                       "    continue;\n"
                                       "  } while (1);\n"
                                       "  return x - 15;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00072.c")
    {
        auto program = generateProgram("int function(void) {\n"
                                       "    int arr[2];\n"
                                       "    int* p;\n"
                                       "    p = &arr[0];\n"
                                       "    p += 1;\n"
                                       "    *p = 123;\n"
                                       "    if(arr[1] != 123)\n"
                                       "        return 1;\n"
                                       "    return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00077.c")
    {
        auto program = generateProgram(
            "int foo(int x[100])\n{"
            "\n"
            "    int y[100];\n"
            "    int* p;\n"
            "    y[0] = 2000;\n\n    if (x[0] != 1000)\n    {\n        return 1;\n    }\n\n    p = x;\n\n    if (p[0] != 1000)\n    {\n        return 2;\n    }\n\n    p = y;\n\n    if (p[0] != 2000)\n    {\n        return 3;\n    }\n\n    if (sizeof(x) != sizeof(void*))\n    {\n        return 4;\n    }\n\n    if (sizeof(y) <= sizeof(x))\n    {\n        return 5;\n    }\n\n    return 0;\n}\n\nint function(void)\n{\n    int x[100];\n    x[0] = 1000;\n\n    return foo(x);\n}"
            "");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00078.c")
    {
        auto program = generateProgram("int f1(char* p)\n"
                                       "{\n"
                                       "    return *p + 1;\n"
                                       "}\n"
                                       "\n"
                                       "int function(void) {\n"
                                       "    char s = 1;\n"
                                       "    int v[1000];\n"
                                       "    int f1(char*);\n"
                                       "    if (f1(&s) != 2)\n"
                                       "        return 1;\n"
                                       "    return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00089.c")
    {
        auto program = generateProgram("int zero()\n"
                                       "{\n"
                                       "return 0;\n"
                                       "}\n"
                                       "\n"
                                       "struct S {\n"
                                       "int (*zerofunc)();\n"
                                       "} s = { &zero };\n"
                                       "\n"
                                       "struct S *\n"
                                       "anon()\n"
                                       "{\n"
                                       "return &s;\n"
                                       "}\n"
                                       "\n"
                                       "typedef struct S* (*fty)();\n"
                                       "\n"
                                       "fty\n"
                                       "go()\n"
                                       "{\n"
                                       "return &anon;\n"
                                       "}\n"
                                       "\n"
                                       "int\n"
                                       "function()\n"
                                       "{\n"
                                       "return go()()->zerofunc();\n"
                                       "}\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00091.c")
    {
        auto program = generateProgram("typedef struct {\n"
                                       "int v;\n"
                                       "int sub[2];\n"
                                       "} S;\n"
                                       "\n"
                                       "S a[1] = {{1,{2,3}}};\n"
                                       "\n"
                                       "int function()\n"
                                       "{\n"
                                       "if (a[0].v != 1)\n"
                                       "    return 1;\n"
                                       "if (a[0].sub[0] != 2)\n"
                                       "    return 2;\n"
                                       "if (a[0].sub[1] != 3)\n"
                                       "    return 3;\n"
                                       "return 0;\n"
                                       "}\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00104.c")
    {
        auto program = generateProgram("int\n"
                                       "function()\n"
                                       "{\n"
                                       "    int x;\n"
                                       "    long long l;\n"
                                       "    \n"
                                       "    x = 0;\n"
                                       "    l = 0;\n"
                                       "    \n"
                                       "    x = ~x;\n"
                                       "    if (x != 0xffffffff) \n"
                                       "        return 1;\n"
                                       "    \n"
                                       "    l = ~l;\n"
                                       "    if (x != 0xffffffffffffffff) \n"
                                       "        return 2;\n"
                                       "    \n"
                                       "    return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00115.c")
    {
        auto program = generateProgram("#define B \"b\"\n"
                                       "\n"
                                       "char s[] = \"a\" B \"c\";\n"
                                       "\n"
                                       "int\n"
                                       "function()\n"
                                       "{\n"
                                       "    if(s[0] != 'a')\n"
                                       "        return 1;\n"
                                       "    if(s[1] != 'b')\n"
                                       "        return 2;\n"
                                       "    if(s[2] != 'c')\n"
                                       "        return 3;\n"
                                       "    if(s[3] != '\\0')\n"
                                       "        return 4;\n"
                                       "    return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00121.c")
    {
        auto program = generateProgram("int f(int a), g(int a), a;\n"
                                       "\n"
                                       "\n"
                                       "int function() \n"
                                       "{\n"
                                       "return f(1) - g(1);\n"
                                       "}\n"
                                       "\n"
                                       "int f(int a) { return a; }\n"
                                       "int g(int a) { return a; }\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00124.c")
    {
        auto program = generateProgram("int f2(int c, int b) {\n"
                                       "   return c - b;\n"
                                       "}\n"
                                       "\n"
                                       "int (*f1(int a,int b))(int c,int b) \n"
                                       "{\n"
                                       "   if (a != b) return f2;\n"
                                       "   return 0;\n"
                                       "}\n"
                                       "\n"
                                       "int\n"
                                       "function()\n"
                                       "{\n"
                                       "   int (* (*p)(int a, int b))(int c, int d) = f1;\n"
                                       "   \n"
                                       "   return (*(*p)(0,2))(2,2);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00138.c")
    {
        auto program = generateProgram("#define M(x) x\n"
                                       "#define A(a,b) a(b)\n"
                                       "\n"
                                       "int function(void) {\n"
                                       "char *a = A(M,\"hi\");\n"
                                       "\n"
                                       "return (a[1] == 'i') ? 0 : 1;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00145.c")
    {
        auto program = generateProgram("#if 0 != (0 && (0/0))\n"
                                       "#error 0 != (0 && (0/0))\n"
                                       "#endif\n"
                                       "\n"
                                       "#if 1 != (-1 || (0/0))\n"
                                       "#error 1 != (-1 || (0/0))\n"
                                       "#endif\n"
                                       "\n"
                                       "#if 3 != (-1 ? 3 : (0/0))\n"
                                       "#error 3 != (-1 ? 3 : (0/0))\n"
                                       "#endif\n"
                                       "\n"
                                       "int function(void) { return 0; }");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 0);
    }
    SECTION("00204.c")
    {
        auto program =
            generateProgram("\n"
                            "int (*print)(const char*,...);\n"
                            "\n"
                            "struct s1 { char x[1]; } s1 = { \"0\" };\n"
                            "struct s2 { char x[2]; } s2 = { \"12\" };\n"
                            "struct s3 { char x[3]; } s3 = { \"345\" };\n"
                            "struct s4 { char x[4]; } s4 = { \"6789\" };\n"
                            "struct s5 { char x[5]; } s5 = { \"abcde\" };\n"
                            "struct s6 { char x[6]; } s6 = { \"fghijk\" };\n"
                            "struct s7 { char x[7]; } s7 = { \"lmnopqr\" };\n"
                            "struct s8 { char x[8]; } s8 = { \"stuvwxyz\" };\n"
                            "struct s9 { char x[9]; } s9 = { \"ABCDEFGHI\" };\n"
                            "struct s10 { char x[10]; } s10 = { \"JKLMNOPQRS\" };\n"
                            "struct s11 { char x[11]; } s11 = { \"TUVWXYZ0123\" };\n"
                            "struct s12 { char x[12]; } s12 = { \"456789abcdef\" };\n"
                            "struct s13 { char x[13]; } s13 = { \"ghijklmnopqrs\" };\n"
                            "struct s14 { char x[14]; } s14 = { \"tuvwxyzABCDEFG\" };\n"
                            "struct s15 { char x[15]; } s15 = { \"HIJKLMNOPQRSTUV\" };\n"
                            "struct s16 { char x[16]; } s16 = { \"WXYZ0123456789ab\" };\n"
                            "struct s17 { char x[17]; } s17 = { \"cdefghijklmnopqrs\" };\n"
                            "\n"
                            "struct hfa11 { float a; } hfa11 = { 11.1 };\n"
                            "struct hfa12 { float a, b; } hfa12 = { 12.1, 12.2 };\n"
                            "struct hfa13 { float a, b, c; } hfa13 = { 13.1, 13.2, 13.3 };\n"
                            "struct hfa14 { float a, b, c, d; } hfa14 = { 14.1, 14.2, 14.3, 14.4 };\n"
                            "\n"
                            "struct hfa21 { double a; } hfa21 = { 21.1 };\n"
                            "struct hfa22 { double a, b; } hfa22 = { 22.1, 22.2 };\n"
                            "struct hfa23 { double a, b, c; } hfa23 = { 23.1, 23.2, 23.3 };\n"
                            "struct hfa24 { double a, b, c, d; } hfa24 = { 24.1, 24.2, 24.3, 24.4 };\n"
                            "\n"
                            "struct hfa31 { long double a; } hfa31 = { 31.1 };\n"
                            "struct hfa32 { long double a, b; } hfa32 = { 32.1, 32.2 };\n"
                            "struct hfa33 { long double a, b, c; } hfa33 = { 33.1, 33.2, 33.3 };\n"
                            "struct hfa34 { long double a, b, c, d; } hfa34 = { 34.1, 34.2, 34.3, 34.4 };\n"
                            "\n"
                            "void fa_s1(struct s1 a) { print(\"%.1s\\n\", a.x); }\n"
                            "void fa_s2(struct s2 a) { print(\"%.2s\\n\", a.x); }\n"
                            "void fa_s3(struct s3 a) { print(\"%.3s\\n\", a.x); }\n"
                            "void fa_s4(struct s4 a) { print(\"%.4s\\n\", a.x); }\n"
                            "void fa_s5(struct s5 a) { print(\"%.5s\\n\", a.x); }\n"
                            "void fa_s6(struct s6 a) { print(\"%.6s\\n\", a.x); }\n"
                            "void fa_s7(struct s7 a) { print(\"%.7s\\n\", a.x); }\n"
                            "void fa_s8(struct s8 a) { print(\"%.8s\\n\", a.x); }\n"
                            "void fa_s9(struct s9 a) { print(\"%.9s\\n\", a.x); }\n"
                            "void fa_s10(struct s10 a) { print(\"%.10s\\n\", a.x); }\n"
                            "void fa_s11(struct s11 a) { print(\"%.11s\\n\", a.x); }\n"
                            "void fa_s12(struct s12 a) { print(\"%.12s\\n\", a.x); }\n"
                            "void fa_s13(struct s13 a) { print(\"%.13s\\n\", a.x); }\n"
                            "void fa_s14(struct s14 a) { print(\"%.14s\\n\", a.x); }\n"
                            "void fa_s15(struct s15 a) { print(\"%.15s\\n\", a.x); }\n"
                            "void fa_s16(struct s16 a) { print(\"%.16s\\n\", a.x); }\n"
                            "void fa_s17(struct s17 a) { print(\"%.17s\\n\", a.x); }\n"
                            "\n"
                            "void fa_hfa11(struct hfa11 a)\n"
                            "{ print(\"%.1f\\n\", a.a); }\n"
                            "void fa_hfa12(struct hfa12 a)\n"
                            "{ print(\"%.1f %.1f\\n\", a.a, a.a); }\n"
                            "void fa_hfa13(struct hfa13 a)\n"
                            "{ print(\"%.1f %.1f %.1f\\n\", a.a, a.b, a.c); }\n"
                            "void fa_hfa14(struct hfa14 a)\n"
                            "{ print(\"%.1f %.1f %.1f %.1f\\n\", a.a, a.b, a.c, a.d); }\n"
                            "\n"
                            "void fa_hfa21(struct hfa21 a)\n"
                            "{ print(\"%.1f\\n\", a.a); }\n"
                            "void fa_hfa22(struct hfa22 a)\n"
                            "{ print(\"%.1f %.1f\\n\", a.a, a.a); }\n"
                            "void fa_hfa23(struct hfa23 a)\n"
                            "{ print(\"%.1f %.1f %.1f\\n\", a.a, a.b, a.c); }\n"
                            "void fa_hfa24(struct hfa24 a)\n"
                            "{ print(\"%.1f %.1f %.1f %.1f\\n\", a.a, a.b, a.c, a.d); }\n"
                            "\n"
                            "void fa_hfa31(struct hfa31 a)\n"
                            "{ print(\"%.1Lf\\n\", a.a); }\n"
                            "void fa_hfa32(struct hfa32 a)\n"
                            "{ print(\"%.1Lf %.1Lf\\n\", a.a, a.a); }\n"
                            "void fa_hfa33(struct hfa33 a)\n"
                            "{ print(\"%.1Lf %.1Lf %.1Lf\\n\", a.a, a.b, a.c); }\n"
                            "void fa_hfa34(struct hfa34 a)\n"
                            "{ print(\"%.1Lf %.1Lf %.1Lf %.1Lf\\n\", a.a, a.b, a.c, a.d); }\n"
                            "\n"
                            "void fa1(struct s8 a, struct s9 b, struct s10 c, struct s11 d,\n"
                            "         struct s12 e, struct s13 f)\n"
                            "{\n"
                            "    print(\"%.3s %.3s %.3s %.3s %.3s %.3s\\n\", a.x, b.x, c.x, d.x, e.x, f.x);\n"
                            "}\n"
                            "\n"
                            "void fa2(struct s9 a, struct s10 b, struct s11 c, struct s12 d,\n"
                            "         struct s13 e, struct s14 f)\n"
                            "{\n"
                            "    print(\"%.3s %.3s %.3s %.3s %.3s %.3s\\n\", a.x, b.x, c.x, d.x, e.x, f.x);\n"
                            "}\n"
                            "\n"
                            "void fa3(struct hfa14 a, struct hfa23 b, struct hfa32 c)\n"
                            "{\n"
                            "    print(\"%.1f %.1f %.1f %.1f %.1Lf %.1Lf\\n\",\n"
                            "           a.a, a.d, b.a, b.c, c.a, c.b);\n"
                            "}\n"
                            "\n"
                            "void fa4(struct s1 a, struct hfa14 b, struct s2 c, struct hfa24 d,\n"
                            "         struct s3 e, struct hfa34 f)\n"
                            "{\n"
                            "    print(\"%.1s %.1f %.1f %.2s %.1f %.1f %.3s %.1Lf %.1Lf\\n\",\n"
                            "           a.x, b.a, b.d, c.x, d.a, d.d, e.x, f.a, f.d);\n"
                            "}\n"
                            "\n"
                            "void arg(void)\n"
                            "{\n"
                            "    print(\"Arguments:\\n\");\n"
                            "    fa_s1(s1);\n"
                            "    fa_s2(s2);\n"
                            "    fa_s3(s3);\n"
                            "    fa_s4(s4);\n"
                            "    fa_s5(s5);\n"
                            "    fa_s6(s6);\n"
                            "    fa_s7(s7);\n"
                            "    fa_s8(s8);\n"
                            "    fa_s9(s9);\n"
                            "    fa_s10(s10);\n"
                            "    fa_s11(s11);\n"
                            "    fa_s12(s12);\n"
                            "    fa_s13(s13);\n"
                            "    fa_s14(s14);\n"
                            "    fa_s15(s15);\n"
                            "    fa_s16(s16);\n"
                            "    fa_s17(s17);\n"
                            "    fa_hfa11(hfa11);\n"
                            "    fa_hfa12(hfa12);\n"
                            "    fa_hfa13(hfa13);\n"
                            "    fa_hfa14(hfa14);\n"
                            "    fa_hfa21(hfa21);\n"
                            "    fa_hfa22(hfa22);\n"
                            "    fa_hfa23(hfa23);\n"
                            "    fa_hfa24(hfa24);\n"
                            "    fa_hfa31(hfa31);\n"
                            "    fa_hfa32(hfa32);\n"
                            "    fa_hfa33(hfa33);\n"
                            "    fa_hfa34(hfa34);\n"
                            "    fa1(s8, s9, s10, s11, s12, s13);\n"
                            "    fa2(s9, s10, s11, s12, s13, s14);\n"
                            "    fa3(hfa14, hfa23, hfa32);\n"
                            "    fa4(s1, hfa14, s2, hfa24, s3, hfa34);\n"
                            "}\n"
                            "\n"
                            "struct s1 fr_s1(void) { return s1; }\n"
                            "struct s2 fr_s2(void) { return s2; }\n"
                            "struct s3 fr_s3(void) { return s3; }\n"
                            "struct s4 fr_s4(void) { return s4; }\n"
                            "struct s5 fr_s5(void) { return s5; }\n"
                            "struct s6 fr_s6(void) { return s6; }\n"
                            "struct s7 fr_s7(void) { return s7; }\n"
                            "struct s8 fr_s8(void) { return s8; }\n"
                            "struct s9 fr_s9(void) { return s9; }\n"
                            "struct s10 fr_s10(void) { return s10; }\n"
                            "struct s11 fr_s11(void) { return s11; }\n"
                            "struct s12 fr_s12(void) { return s12; }\n"
                            "struct s13 fr_s13(void) { return s13; }\n"
                            "struct s14 fr_s14(void) { return s14; }\n"
                            "struct s15 fr_s15(void) { return s15; }\n"
                            "struct s16 fr_s16(void) { return s16; }\n"
                            "struct s17 fr_s17(void) { return s17; }\n"
                            "\n"
                            "struct hfa11 fr_hfa11(void) { return hfa11; }\n"
                            "struct hfa12 fr_hfa12(void) { return hfa12; }\n"
                            "struct hfa13 fr_hfa13(void) { return hfa13; }\n"
                            "struct hfa14 fr_hfa14(void) { return hfa14; }\n"
                            "\n"
                            "struct hfa21 fr_hfa21(void) { return hfa21; }\n"
                            "struct hfa22 fr_hfa22(void) { return hfa22; }\n"
                            "struct hfa23 fr_hfa23(void) { return hfa23; }\n"
                            "struct hfa24 fr_hfa24(void) { return hfa24; }\n"
                            "\n"
                            "struct hfa31 fr_hfa31(void) { return hfa31; }\n"
                            "struct hfa32 fr_hfa32(void) { return hfa32; }\n"
                            "struct hfa33 fr_hfa33(void) { return hfa33; }\n"
                            "struct hfa34 fr_hfa34(void) { return hfa34; }\n"
                            "\n"
                            "void ret(void)\n"
                            "{\n"
                            "    struct s1 t1 = fr_s1();\n"
                            "    struct s2 t2 = fr_s2();\n"
                            "    struct s3 t3 = fr_s3();\n"
                            "    struct s4 t4 = fr_s4();\n"
                            "    struct s5 t5 = fr_s5();\n"
                            "    struct s6 t6 = fr_s6();\n"
                            "    struct s7 t7 = fr_s7();\n"
                            "    struct s8 t8 = fr_s8();\n"
                            "    struct s9 t9 = fr_s9();\n"
                            "    struct s10 t10 = fr_s10();\n"
                            "    struct s11 t11 = fr_s11();\n"
                            "    struct s12 t12 = fr_s12();\n"
                            "    struct s13 t13 = fr_s13();\n"
                            "    struct s14 t14 = fr_s14();\n"
                            "    struct s15 t15 = fr_s15();\n"
                            "    struct s16 t16 = fr_s16();\n"
                            "    struct s17 t17 = fr_s17();\n"
                            "    print(\"Return values:\\n\");\n"
                            "    print(\"%.1s\\n\", t1.x);\n"
                            "    print(\"%.2s\\n\", t2.x);\n"
                            "    print(\"%.3s\\n\", t3.x);\n"
                            "    print(\"%.4s\\n\", t4.x);\n"
                            "    print(\"%.5s\\n\", t5.x);\n"
                            "    print(\"%.6s\\n\", t6.x);\n"
                            "    print(\"%.7s\\n\", t7.x);\n"
                            "    print(\"%.8s\\n\", t8.x);\n"
                            "    print(\"%.9s\\n\", t9.x);\n"
                            "    print(\"%.10s\\n\", t10.x);\n"
                            "    print(\"%.11s\\n\", t11.x);\n"
                            "    print(\"%.12s\\n\", t12.x);\n"
                            "    print(\"%.13s\\n\", t13.x);\n"
                            "    print(\"%.14s\\n\", t14.x);\n"
                            "    print(\"%.15s\\n\", t15.x);\n"
                            "    print(\"%.16s\\n\", t16.x);\n"
                            "    print(\"%.17s\\n\", t17.x);\n"
                            "    print(\"%.1f\\n\", fr_hfa11().a);\n"
                            "    print(\"%.1f %.1f\\n\", fr_hfa12().a, fr_hfa12().b);\n"
                            "    print(\"%.1f %.1f\\n\", fr_hfa13().a, fr_hfa13().c);\n"
                            "    print(\"%.1f %.1f\\n\", fr_hfa14().a, fr_hfa14().d);\n"
                            "    print(\"%.1f\\n\", fr_hfa21().a);\n"
                            "    print(\"%.1f %.1f\\n\", fr_hfa22().a, fr_hfa22().b);\n"
                            "    print(\"%.1f %.1f\\n\", fr_hfa23().a, fr_hfa23().c);\n"
                            "    print(\"%.1f %.1f\\n\", fr_hfa24().a, fr_hfa24().d);\n"
                            "    print(\"%.1Lf\\n\", fr_hfa31().a);\n"
                            "    print(\"%.1Lf %.1Lf\\n\", fr_hfa32().a, fr_hfa32().b);\n"
                            "    print(\"%.1Lf %.1Lf\\n\", fr_hfa33().a, fr_hfa33().c);\n"
                            "    print(\"%.1Lf %.1Lf\\n\", fr_hfa34().a, fr_hfa34().d);\n"
                            "}\n"
                            "\n"
                            "int match(const char **s, const char *f)\n"
                            "{\n"
                            "    const char *p = *s;\n"
                            "    for (p = *s; *f && *f == *p; f++, p++)\n"
                            "        ;\n"
                            "    if (!*f) {\n"
                            "        *s = p - 1;\n"
                            "        return 1;\n"
                            "    }\n"
                            "    return 0;\n"
                            "}\n"
                            "\n"
                            "void myprintf(const char *format, ...)\n"
                            "{\n"
                            "    const char *s;\n"
                            "    __builtin_va_list ap;\n"
                            "    __builtin_va_start(ap, format);\n"
                            "    for (s = format; *s; s++) {\n"
                            "        if (match(&s, \"%7s\")) {\n"
                            "            struct s7 t7 = __builtin_va_arg(ap, struct s7);\n"
                            "            print(\"%.7s\", t7.x);\n"
                            "        }\n"
                            "        else if (match(&s, \"%9s\")) {\n"
                            "            struct s9 t9 = __builtin_va_arg(ap, struct s9);\n"
                            "            print(\"%.9s\", t9.x);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa11\")) {\n"
                            "            struct hfa11 x = __builtin_va_arg(ap, struct hfa11);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.a);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa12\")) {\n"
                            "            struct hfa12 x = __builtin_va_arg(ap, struct hfa12);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.b);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa13\")) {\n"
                            "            struct hfa13 x = __builtin_va_arg(ap, struct hfa13);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.c);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa14\")) {\n"
                            "            struct hfa14 x = __builtin_va_arg(ap, struct hfa14);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.d);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa21\")) {\n"
                            "            struct hfa21 x = __builtin_va_arg(ap, struct hfa21);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.a);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa22\")) {\n"
                            "            struct hfa22 x = __builtin_va_arg(ap, struct hfa22);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.b);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa23\")) {\n"
                            "            struct hfa23 x = __builtin_va_arg(ap, struct hfa23);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.c);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa24\")) {\n"
                            "            struct hfa24 x = __builtin_va_arg(ap, struct hfa24);\n"
                            "            print(\"%.1f,%.1f\", x.a, x.d);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa31\")) {\n"
                            "            struct hfa31 x = __builtin_va_arg(ap, struct hfa31);\n"
                            "            print(\"%.1Lf,%.1Lf\", x.a, x.a);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa32\")) {\n"
                            "            struct hfa32 x = __builtin_va_arg(ap, struct hfa32);\n"
                            "            print(\"%.1Lf,%.1Lf\", x.a, x.b);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa33\")) {\n"
                            "            struct hfa33 x = __builtin_va_arg(ap, struct hfa33);\n"
                            "            print(\"%.1Lf,%.1Lf\", x.a, x.c);\n"
                            "        }\n"
                            "        else if (match(&s, \"%hfa34\")) {\n"
                            "            struct hfa34 x = __builtin_va_arg(ap, struct hfa34);\n"
                            "            print(\"%.1Lf,%.1Lf\", x.a, x.d);\n"
                            "        }\n"
                            "        else\n"
                            "            print(\"%c\",*s);\n"
                            "    }\n"
                            "    print(\"\\n\");\n"
                            "}\n"
                            "\n"
                            "void stdarg(void)\n"
                            "{\n"
                            "    print(\"stdarg:\\n\");\n"
                            "    myprintf(\"%9s %9s %9s %9s %9s %9s\", s9, s9, s9, s9, s9, s9);\n"
                            "    myprintf(\"%7s %9s %9s %9s %9s %9s\", s7, s9, s9, s9, s9, s9);\n"
                            "\n"
                            "    myprintf(\"HFA long double:\");\n"
                            "    myprintf(\"%hfa34 %hfa34 %hfa34 %hfa34\", hfa34, hfa34, hfa34, hfa34);\n"
                            "    myprintf(\"%hfa33 %hfa34 %hfa34 %hfa34\", hfa33, hfa34, hfa34, hfa34);\n"
                            "    myprintf(\"%hfa32 %hfa34 %hfa34 %hfa34\", hfa32, hfa34, hfa34, hfa34);\n"
                            "    myprintf(\"%hfa31 %hfa34 %hfa34 %hfa34\", hfa31, hfa34, hfa34, hfa34);\n"
                            "\n"
                            "    myprintf(\"%hfa32 %hfa33 %hfa33 %hfa33 %hfa33\",\n"
                            "             hfa32, hfa33, hfa33, hfa33, hfa33);\n"
                            "    myprintf(\"%hfa31 %hfa33 %hfa33 %hfa33 %hfa33\",\n"
                            "             hfa31, hfa33, hfa33, hfa33, hfa33);\n"
                            "    myprintf(\"%hfa33 %hfa33 %hfa33 %hfa33\",\n"
                            "             hfa33, hfa33, hfa33, hfa33);\n"
                            "\n"
                            "    myprintf(\"%hfa34 %hfa32 %hfa32 %hfa32 %hfa32\",\n"
                            "             hfa34, hfa32, hfa32, hfa32, hfa32);\n"
                            "    myprintf(\"%hfa33 %hfa32 %hfa32 %hfa32 %hfa32\",\n"
                            "             hfa33, hfa32, hfa32, hfa32, hfa32);\n"
                            "\n"
                            "    myprintf(\"%hfa34 %hfa32 %hfa31 %hfa31 %hfa31 %hfa31\",\n"
                            "             hfa34, hfa32, hfa31, hfa31, hfa31, hfa31);\n"
                            "\n"
                            "    myprintf(\"HFA double:\");\n"
                            "    myprintf(\"%hfa24 %hfa24 %hfa24 %hfa24\", hfa24, hfa24, hfa24, hfa24);\n"
                            "    myprintf(\"%hfa23 %hfa24 %hfa24 %hfa24\", hfa23, hfa24, hfa24, hfa24);\n"
                            "    myprintf(\"%hfa22 %hfa24 %hfa24 %hfa24\", hfa22, hfa24, hfa24, hfa24);\n"
                            "    myprintf(\"%hfa21 %hfa24 %hfa24 %hfa24\", hfa21, hfa24, hfa24, hfa24);\n"
                            "\n"
                            "    myprintf(\"%hfa22 %hfa23 %hfa23 %hfa23 %hfa23\",\n"
                            "             hfa22, hfa23, hfa23, hfa23, hfa23);\n"
                            "    myprintf(\"%hfa21 %hfa23 %hfa23 %hfa23 %hfa23\",\n"
                            "             hfa21, hfa23, hfa23, hfa23, hfa23);\n"
                            "    myprintf(\"%hfa23 %hfa23 %hfa23 %hfa23\",\n"
                            "             hfa23, hfa23, hfa23, hfa23);\n"
                            "\n"
                            "    myprintf(\"%hfa24 %hfa22 %hfa22 %hfa22 %hfa22\",\n"
                            "             hfa24, hfa22, hfa22, hfa22, hfa22);\n"
                            "    myprintf(\"%hfa23 %hfa22 %hfa22 %hfa22 %hfa22\",\n"
                            "             hfa23, hfa22, hfa22, hfa22, hfa22);\n"
                            "\n"
                            "    myprintf(\"%hfa24 %hfa22 %hfa21 %hfa21 %hfa21 %hfa21\",\n"
                            "             hfa24, hfa22, hfa21, hfa21, hfa21, hfa21);\n"
                            "\n"
                            "    myprintf(\"HFA float:\");\n"
                            "    myprintf(\"%hfa14 %hfa14 %hfa14 %hfa14\", hfa14, hfa14, hfa14, hfa14);\n"
                            "    myprintf(\"%hfa13 %hfa14 %hfa14 %hfa14\", hfa13, hfa14, hfa14, hfa14);\n"
                            "    myprintf(\"%hfa12 %hfa14 %hfa14 %hfa14\", hfa12, hfa14, hfa14, hfa14);\n"
                            "    myprintf(\"%hfa11 %hfa14 %hfa14 %hfa14\", hfa11, hfa14, hfa14, hfa14);\n"
                            "\n"
                            "    myprintf(\"%hfa12 %hfa13 %hfa13 %hfa13 %hfa13\",\n"
                            "             hfa12, hfa13, hfa13, hfa13, hfa13);\n"
                            "    myprintf(\"%hfa11 %hfa13 %hfa13 %hfa13 %hfa13\",\n"
                            "             hfa11, hfa13, hfa13, hfa13, hfa13);\n"
                            "    myprintf(\"%hfa13 %hfa13 %hfa13 %hfa13\",\n"
                            "             hfa13, hfa13, hfa13, hfa13);\n"
                            "\n"
                            "    myprintf(\"%hfa14 %hfa12 %hfa12 %hfa12 %hfa12\",\n"
                            "             hfa14, hfa12, hfa12, hfa12, hfa12);\n"
                            "    myprintf(\"%hfa13 %hfa12 %hfa12 %hfa12 %hfa12\",\n"
                            "             hfa13, hfa12, hfa12, hfa12, hfa12);\n"
                            "\n"
                            "    myprintf(\"%hfa14 %hfa12 %hfa11 %hfa11 %hfa11 %hfa11\",\n"
                            "             hfa14, hfa12, hfa11, hfa11, hfa11, hfa11);\n"
                            "}\n"
                            "\n"
                            "void pll(unsigned long long x)\n"
                            "{\n"
                            "    print(\"%llx\\n\", x);\n"
                            "}\n"
                            "\n"
                            "void movi(void)\n"
                            "{\n"
                            "    print(\"MOVI:\\n\");\n"
                            "    pll(0);\n"
                            "    pll(0xabcd);\n"
                            "    pll(0xabcd0000);\n"
                            "    pll(0xabcd00000000);\n"
                            "    pll(0xabcd000000000000);\n"
                            "    pll(0xffffabcd);\n"
                            "    pll(0xabcdffff);\n"
                            "    pll(0xffffffffffffabcd);\n"
                            "    pll(0xffffffffabcdffff);\n"
                            "    pll(0xffffabcdffffffff);\n"
                            "    pll(0xabcdffffffffffff);\n"
                            "    pll(0xaaaaaaaa);\n"
                            "    pll(0x5555555555555555);\n"
                            "    pll(0x77777777);\n"
                            "    pll(0x3333333333333333);\n"
                            "    pll(0xf8f8f8f8);\n"
                            "    pll(0x1e1e1e1e1e1e1e1e);\n"
                            "    pll(0x3f803f80);\n"
                            "    pll(0x01ff01ff01ff01ff);\n"
                            "    pll(0x007fffc0);\n"
                            "    pll(0x03fff80003fff800);\n"
                            "    pll(0x0007fffffffffe00);\n"
                            "\n"
                            "    pll(0xabcd1234);\n"
                            "    pll(0xabcd00001234);\n"
                            "    pll(0xabcd000000001234);\n"
                            "    pll(0xabcd12340000);\n"
                            "    pll(0xabcd000012340000);\n"
                            "    pll(0xabcd123400000000);\n"
                            "    pll(0xffffffffabcd1234);\n"
                            "    pll(0xffffabcdffff1234);\n"
                            "    pll(0xabcdffffffff1234);\n"
                            "    pll(0xffffabcd1234ffff);\n"
                            "    pll(0xabcdffff1234ffff);\n"
                            "    pll(0xabcd1234ffffffff);\n"
                            "\n"
                            "    pll(0xffffef0123456789);\n"
                            "    pll(0xabcdef012345ffff);\n"
                            "\n"
                            "    pll(0xabcdef0123456789);\n"
                            "}\n"
                            "\n"
                            "static unsigned addip0(unsigned x) { return x + 0; }\n"
                            "static unsigned long long sublp0(unsigned long long x) { return x - 0; }\n"
                            "static unsigned addip123(unsigned x) { return x + 123; }\n"
                            "static unsigned long long addlm123(unsigned long long x) { return x + -123; }\n"
                            "static unsigned long long sublp4095(unsigned long long x) { return x - 4095; }\n"
                            "static unsigned subim503808(unsigned x) { return x - -503808; }\n"
                            "static unsigned long long addp12345(unsigned long long x) { return x + 12345; }\n"
                            "static unsigned subp12345(unsigned x) { return x - 12345; }\n"
                            "\n"
                            "static unsigned mvni(unsigned x) { return 0xffffffff - x; }\n"
                            "static unsigned long long negl(unsigned long long x) { return 0 - x; }\n"
                            "static unsigned rsbi123(unsigned x) { return 123 - x; }\n"
                            "static unsigned long long rsbl123(unsigned long long x) { return 123 - x; }\n"
                            "\n"
                            "static unsigned andi0(unsigned x) { return x & 0; }\n"
                            "static unsigned long long andlm1(unsigned long long x) { return x & -1; }\n"
                            "static unsigned long long orrl0(unsigned long long x) { return x | 0; }\n"
                            "static unsigned orrim1(unsigned x) { return x | -1; }\n"
                            "static unsigned eori0(unsigned x) { return x ^ 0; }\n"
                            "static unsigned long long eorlm1(unsigned long long x) { return x ^ -1; }\n"
                            "static unsigned and0xf0(unsigned x) { return x & 0xf0; }\n"
                            "static unsigned long long orr0xf0(unsigned long long x) { return x | 0xf0; }\n"
                            "static unsigned long long eor0xf0(unsigned long long x) { return x ^ 0xf0; }\n"
                            "\n"
                            "static unsigned lsli0(unsigned x) { return x << 0; }\n"
                            "static unsigned lsri0(unsigned x) { return x >> 0; }\n"
                            "static long long asrl0(long long x) { return x >> 0; }\n"
                            "static unsigned lsli1(unsigned x) { return x << 1; }\n"
                            "static unsigned lsli31(unsigned x) { return x << 31; }\n"
                            "static unsigned long long lsll1(unsigned long long x) { return x << 1; }\n"
                            "static unsigned long long lsll63(unsigned long long x) { return x << 63; }\n"
                            "static unsigned lsri1(unsigned x) { return x >> 1; }\n"
                            "static unsigned lsri31(unsigned x) { return x >> 31; }\n"
                            "static unsigned long long lsrl1(unsigned long long x) { return x >> 1; }\n"
                            "static unsigned long long lsrl63(unsigned long long x) { return x >> 63; }\n"
                            "static int asri1(int x) { return x >> 1; }\n"
                            "static int asri31(int x) { return x >> 31; }\n"
                            "static long long asrl1(long long x) { return x >> 1; }\n"
                            "static long long asrl63(long long x) { return x >> 63; }\n"
                            "\n"
                            "void opi(void)\n"
                            "{\n"
                            "    int x = 1000;\n"
                            "    pll(addip0(x));\n"
                            "    pll(sublp0(x));\n"
                            "    pll(addip123(x));\n"
                            "    pll(addlm123(x));\n"
                            "    pll(sublp4095(x));\n"
                            "    pll(subim503808(x));\n"
                            "    pll(addp12345(x));\n"
                            "    pll(subp12345(x));\n"
                            "    pll(mvni(x));\n"
                            "    pll(negl(x));\n"
                            "    pll(rsbi123(x));\n"
                            "    pll(rsbl123(x));\n"
                            "    pll(andi0(x));\n"
                            "    pll(andlm1(x));\n"
                            "    pll(orrl0(x));\n"
                            "    pll(orrim1(x));\n"
                            "    pll(eori0(x));\n"
                            "    pll(eorlm1(x));\n"
                            "    pll(and0xf0(x));\n"
                            "    pll(orr0xf0(x));\n"
                            "    pll(eor0xf0(x));\n"
                            "    pll(lsli0(x));\n"
                            "    pll(lsri0(x));\n"
                            "    pll(asrl0(x));\n"
                            "    pll(lsli1(x));\n"
                            "    pll(lsli31(x));\n"
                            "    pll(lsll1(x));\n"
                            "    pll(lsll63(x));\n"
                            "    pll(lsri1(x));\n"
                            "    pll(lsri31(x));\n"
                            "    pll(lsrl1(x));\n"
                            "    pll(lsrl63(x));\n"
                            "    pll(asri1(x));\n"
                            "    pll(asri31(x));\n"
                            "    pll(asrl1(x));\n"
                            "    pll(asrl63(x));\n"
                            "}\n"
                            "\n"
                            "void pcs(void)\n"
                            "{\n"
                            "    arg();\n"
                            "    ret();\n"
                            "    stdarg();\n"
                            "    movi();\n"
                            "    opi();\n"
                            "}\n"
                            "\n"
                            "int function(int(*printF)(const char*,...))\n"
                            "{\n"
                            "    print = printF;\n"
                            "    pcs();\n"
                            "    return 0;\n"
                            "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        text.clear();
        CHECK(cld::Tests::computeInJIT<int(int (*)(const char*, ...))>(std::move(module), "function", printfCallback)
              == 0);
        CHECK(text
              == "Arguments:\n"
                 "0\n"
                 "12\n"
                 "345\n"
                 "6789\n"
                 "abcde\n"
                 "fghijk\n"
                 "lmnopqr\n"
                 "stuvwxyz\n"
                 "ABCDEFGHI\n"
                 "JKLMNOPQRS\n"
                 "TUVWXYZ0123\n"
                 "456789abcdef\n"
                 "ghijklmnopqrs\n"
                 "tuvwxyzABCDEFG\n"
                 "HIJKLMNOPQRSTUV\n"
                 "WXYZ0123456789ab\n"
                 "cdefghijklmnopqrs\n"
                 "11.1\n"
                 "12.1 12.1\n"
                 "13.1 13.2 13.3\n"
                 "14.1 14.2 14.3 14.4\n"
                 "21.1\n"
                 "22.1 22.1\n"
                 "23.1 23.2 23.3\n"
                 "24.1 24.2 24.3 24.4\n"
                 "31.1\n"
                 "32.1 32.1\n"
                 "33.1 33.2 33.3\n"
                 "34.1 34.2 34.3 34.4\n"
                 "stu ABC JKL TUV 456 ghi\n"
                 "ABC JKL TUV 456 ghi tuv\n"
                 "14.1 14.4 23.1 23.3 32.1 32.2\n"
                 "0 14.1 14.4 12 24.1 24.4 345 34.1 34.4\n"
                 "Return values:\n"
                 "0\n"
                 "12\n"
                 "345\n"
                 "6789\n"
                 "abcde\n"
                 "fghijk\n"
                 "lmnopqr\n"
                 "stuvwxyz\n"
                 "ABCDEFGHI\n"
                 "JKLMNOPQRS\n"
                 "TUVWXYZ0123\n"
                 "456789abcdef\n"
                 "ghijklmnopqrs\n"
                 "tuvwxyzABCDEFG\n"
                 "HIJKLMNOPQRSTUV\n"
                 "WXYZ0123456789ab\n"
                 "cdefghijklmnopqrs\n"
                 "11.1\n"
                 "12.1 12.2\n"
                 "13.1 13.3\n"
                 "14.1 14.4\n"
                 "21.1\n"
                 "22.1 22.2\n"
                 "23.1 23.3\n"
                 "24.1 24.4\n"
                 "31.1\n"
                 "32.1 32.2\n"
                 "33.1 33.3\n"
                 "34.1 34.4\n"
                 "stdarg:\n"
                 "ABCDEFGHI ABCDEFGHI ABCDEFGHI ABCDEFGHI ABCDEFGHI ABCDEFGHI\n"
                 "lmnopqr ABCDEFGHI ABCDEFGHI ABCDEFGHI ABCDEFGHI ABCDEFGHI\n"
                 "HFA long double:\n"
                 "34.1,34.4 34.1,34.4 34.1,34.4 34.1,34.4\n"
                 "33.1,33.3 34.1,34.4 34.1,34.4 34.1,34.4\n"
                 "32.1,32.2 34.1,34.4 34.1,34.4 34.1,34.4\n"
                 "31.1,31.1 34.1,34.4 34.1,34.4 34.1,34.4\n"
                 "32.1,32.2 33.1,33.3 33.1,33.3 33.1,33.3 33.1,33.3\n"
                 "31.1,31.1 33.1,33.3 33.1,33.3 33.1,33.3 33.1,33.3\n"
                 "33.1,33.3 33.1,33.3 33.1,33.3 33.1,33.3\n"
                 "34.1,34.4 32.1,32.2 32.1,32.2 32.1,32.2 32.1,32.2\n"
                 "33.1,33.3 32.1,32.2 32.1,32.2 32.1,32.2 32.1,32.2\n"
                 "34.1,34.4 32.1,32.2 31.1,31.1 31.1,31.1 31.1,31.1 31.1,31.1\n"
                 "HFA double:\n"
                 "24.1,24.4 24.1,24.4 24.1,24.4 24.1,24.4\n"
                 "23.1,23.3 24.1,24.4 24.1,24.4 24.1,24.4\n"
                 "22.1,22.2 24.1,24.4 24.1,24.4 24.1,24.4\n"
                 "21.1,21.1 24.1,24.4 24.1,24.4 24.1,24.4\n"
                 "22.1,22.2 23.1,23.3 23.1,23.3 23.1,23.3 23.1,23.3\n"
                 "21.1,21.1 23.1,23.3 23.1,23.3 23.1,23.3 23.1,23.3\n"
                 "23.1,23.3 23.1,23.3 23.1,23.3 23.1,23.3\n"
                 "24.1,24.4 22.1,22.2 22.1,22.2 22.1,22.2 22.1,22.2\n"
                 "23.1,23.3 22.1,22.2 22.1,22.2 22.1,22.2 22.1,22.2\n"
                 "24.1,24.4 22.1,22.2 21.1,21.1 21.1,21.1 21.1,21.1 21.1,21.1\n"
                 "HFA float:\n"
                 "14.1,14.4 14.1,14.4 14.1,14.4 14.1,14.4\n"
                 "13.1,13.3 14.1,14.4 14.1,14.4 14.1,14.4\n"
                 "12.1,12.2 14.1,14.4 14.1,14.4 14.1,14.4\n"
                 "11.1,11.1 14.1,14.4 14.1,14.4 14.1,14.4\n"
                 "12.1,12.2 13.1,13.3 13.1,13.3 13.1,13.3 13.1,13.3\n"
                 "11.1,11.1 13.1,13.3 13.1,13.3 13.1,13.3 13.1,13.3\n"
                 "13.1,13.3 13.1,13.3 13.1,13.3 13.1,13.3\n"
                 "14.1,14.4 12.1,12.2 12.1,12.2 12.1,12.2 12.1,12.2\n"
                 "13.1,13.3 12.1,12.2 12.1,12.2 12.1,12.2 12.1,12.2\n"
                 "14.1,14.4 12.1,12.2 11.1,11.1 11.1,11.1 11.1,11.1 11.1,11.1\n"
                 "MOVI:\n"
                 "0\n"
                 "abcd\n"
                 "abcd0000\n"
                 "abcd00000000\n"
                 "abcd000000000000\n"
                 "ffffabcd\n"
                 "abcdffff\n"
                 "ffffffffffffabcd\n"
                 "ffffffffabcdffff\n"
                 "ffffabcdffffffff\n"
                 "abcdffffffffffff\n"
                 "aaaaaaaa\n"
                 "5555555555555555\n"
                 "77777777\n"
                 "3333333333333333\n"
                 "f8f8f8f8\n"
                 "1e1e1e1e1e1e1e1e\n"
                 "3f803f80\n"
                 "1ff01ff01ff01ff\n"
                 "7fffc0\n"
                 "3fff80003fff800\n"
                 "7fffffffffe00\n"
                 "abcd1234\n"
                 "abcd00001234\n"
                 "abcd000000001234\n"
                 "abcd12340000\n"
                 "abcd000012340000\n"
                 "abcd123400000000\n"
                 "ffffffffabcd1234\n"
                 "ffffabcdffff1234\n"
                 "abcdffffffff1234\n"
                 "ffffabcd1234ffff\n"
                 "abcdffff1234ffff\n"
                 "abcd1234ffffffff\n"
                 "ffffef0123456789\n"
                 "abcdef012345ffff\n"
                 "abcdef0123456789\n"
                 "3e8\n"
                 "3e8\n"
                 "463\n"
                 "36d\n"
                 "fffffffffffff3e9\n"
                 "7b3e8\n"
                 "3421\n"
                 "ffffd3af\n"
                 "fffffc17\n"
                 "fffffffffffffc18\n"
                 "fffffc93\n"
                 "fffffffffffffc93\n"
                 "0\n"
                 "3e8\n"
                 "3e8\n"
                 "ffffffff\n"
                 "3e8\n"
                 "fffffffffffffc17\n"
                 "e0\n"
                 "3f8\n"
                 "318\n"
                 "3e8\n"
                 "3e8\n"
                 "3e8\n"
                 "7d0\n"
                 "0\n"
                 "7d0\n"
                 "0\n"
                 "1f4\n"
                 "0\n"
                 "1f4\n"
                 "0\n"
                 "1f4\n"
                 "0\n"
                 "1f4\n"
                 "0\n");
    }
}

TEST_CASE("LLVM codegen miscellaneous programs", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("vararg issue")
    {
        auto program = generateProgram("int (*print)(const char*,...);\n"
                                       "\n"
                                       "struct hfa12 {\n"
                                       " float a,b;"
                                       "} hfa12 = { 12.1,12.2};\n"
                                       "\n"
                                       "struct hfa13 {\n"
                                       " float a,b,c;\n"
                                       "} hfa13 = { 13.1,13.2,13.3 };\n"
                                       "\n"
                                       "void foo(void* p,...) {\n"
                                       "    __builtin_va_list list;\n"
                                       "    __builtin_va_start(list,p);\n"
                                       "    struct hfa12 x = __builtin_va_arg(list,struct hfa12);\n"
                                       "    print(\"%.1f,%.1f \", x.a, x.b);\n"
                                       "    for (int i = 0; i < 4; i++) {\n"
                                       "        struct hfa13 x = __builtin_va_arg(list,struct hfa13);\n"
                                       "        print(\"%.1f,%.1f \", x.a,x.c);\n"
                                       "    }\n"
                                       "    __builtin_va_end(list);\n"
                                       "}\n"
                                       "\n"
                                       "void function(int (*printF)(const char*,...)) {\n"
                                       "print = printF;\n"
                                       "foo(0,hfa12,hfa13,hfa13,hfa13,hfa13);\n"
                                       "}\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        text.clear();
        cld::Tests::computeInJIT<void(int (*)(const char*, ...))>(std::move(module), "function", printfCallback);
        CHECK(text == "12.1,12.2 13.1,13.3 13.1,13.3 13.1,13.3 13.1,13.3 ");
    }
    SECTION("Member access shenanigans")
    {
        auto program = generateProgram("struct T {\n"
                                       "   int i;\n"
                                       "};\n"
                                       "\n"
                                       "struct S {\n"
                                       "   struct T i;\n"
                                       "};\n"
                                       "\n"
                                       "void function(void) {\n"
                                       "struct S p[1] = {5};\n"
                                       "(&p->i)->i;\n"
                                       "}\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
    }
    SECTION("variably modified types shenanigans")
    {
        auto program = generateProgram("struct A {\n"
                                       "    double (*arr)[];\n"
                                       "    int length;\n"
                                       "    int width;\n"
                                       "};\n"
                                       "\n"
                                       "void print_mat(int l,int w,double (*arr)[w],int (*printF)(const char*,...))\n"
                                       "{\n"
                                       "    for(int i = 0; i < l; i++)\n"
                                       "    {\n"
                                       "        for (int j = 0; j < w; j++)\n"
                                       "        {\n"
                                       "            printF(\"%6.6f, \",arr[i][j]);\n"
                                       "        }\n"
                                       "    printF(\"\\n\");\n"
                                       "    }\n"
                                       "}\n"
                                       "\n"
                                       "void function(int (*printF)(const char*,...))\n"
                                       "{\n"
                                       "    double a[3][2] = {{1,2,},{3,4},{5,6}};\n"
                                       "    struct A as = {\n"
                                       "        .length = 3,\n"
                                       "        .width = 2,\n"
                                       "        .arr = a\n"
                                       "    };\n"
                                       "\n"
                                       "    print_mat(as.length,as.width,as.arr,printF);\n"
                                       "    double b[3][3] = {{1.}};\n"
                                       "    as.width = 3;\n"
                                       "    as.arr = b;\n"
                                       "    \n"
                                       "    print_mat(as.length,as.width,as.arr,printF);\n"
                                       "}\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        cld::Tests::computeInJIT<void(int (*)(const char*, ...))>(std::move(module), "function", printf);
    }
}

TEST_CASE("LLVM codegen var arg", "[LLVM]")
{
    auto triple = GENERATE_REF(values({cld::Triple::native(), cld::Tests::x64windowsGnu, cld::Tests::x64linux}));
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Inside of vararg function")
    {
        SECTION("integer register")
        {
            auto program = generateProgram("int function(void*p,...) {\n"
                                           "__builtin_va_list list;\n"
                                           "__builtin_va_start(list,p);\n"
                                           "int i = __builtin_va_arg(list,int);\n"
                                           "__builtin_va_end(list);\n"
                                           "return i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            if (triple == cld::Triple::native())
            {
                CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, 5) == 5);
            }
        }
        SECTION("pointer in integer register")
        {
            auto program = generateProgram("int function(void*p,...) {\n"
                                           "__builtin_va_list list;\n"
                                           "__builtin_va_start(list,p);\n"
                                           "int* i = __builtin_va_arg(list,int*);\n"
                                           "__builtin_va_end(list);\n"
                                           "return *i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            int i = 5;
            if (triple == cld::Triple::native())
            {
                CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, &i) == 5);
            }
        }
        SECTION("small simple struct")
        {
            struct T
            {
                float f;
                int r;
            };
            auto program = generateProgram("struct T {\n"
                                           "float f;\n"
                                           "int r;\n"
                                           "};\n"
                                           "\n"
                                           "int function(void*p,...) {\n"
                                           "__builtin_va_list list;\n"
                                           "__builtin_va_start(list,p);\n"
                                           "int i = __builtin_va_arg(list,struct T).r;\n"
                                           "__builtin_va_end(list);\n"
                                           "return i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            if (triple == cld::Triple::native())
            {
                CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, 5})
                      == 5);
            }
        }
        SECTION("less than 128 bytes struct")
        {
            struct T
            {
                double f;
                int r[2];
            };
            auto program = generateProgram("struct T {\n"
                                           "double f;\n"
                                           "int r[2];\n"
                                           "};\n"
                                           "\n"
                                           "int function(void*p,...) {\n"
                                           "__builtin_va_list list;\n"
                                           "__builtin_va_start(list,p);\n"
                                           "struct T i = __builtin_va_arg(list,struct T);\n"
                                           "__builtin_va_end(list);\n"
                                           "return i.r[0] + i.r[1];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            if (triple == cld::Triple::native())
            {
                CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, {3, 2}})
                      == 5);
            }
        }
        SECTION("less than 128 bytes struct all in integers")
        {
            struct T
            {
                char c[9];
            };
            auto program = generateProgram(
                "struct T {\n"
                "char r[9];\n"
                "};\n"
                "\n"
                "int function(void*p,...) {\n"
                "__builtin_va_list list;\n"
                "__builtin_va_start(list,p);\n"
                "struct T i = __builtin_va_arg(list,struct T);\n"
                "__builtin_va_end(list);\n"
                "return i.r[0] + i.r[1] + i.r[2] + i.r[3] + i.r[4] + i.r[5] + i.r[6] + i.r[7] + i.r[8];\n"
                "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            if (triple == cld::Triple::native())
            {
                CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr,
                                                                T{1, 2, 3, 4, 5, 6, 7, 8, 9})
                      == 45);
            }
        }
        SECTION("less than 128 bytes struct")
        {
            struct T
            {
                float r[2];
            };
            auto program = generateProgram("struct T {\n"
                                           "float r[2];\n"
                                           "};\n"
                                           "\n"
                                           "int function(void*p,...) {\n"
                                           "__builtin_va_list list;\n"
                                           "__builtin_va_start(list,p);\n"
                                           "struct T i = __builtin_va_arg(list,struct T);\n"
                                           "__builtin_va_end(list);\n"
                                           "return i.r[0] + i.r[1];\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            {
                if (triple == cld::Triple::native())
                    CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, 5.0})
                          == 8);
            }
        }
        SECTION("larger struct")
        {
            struct T
            {
                float f;
                int r[5];
            };
            auto program = generateProgram("struct T {\n"
                                           "float f;\n"
                                           "int r[5];\n"
                                           "};\n"
                                           "\n"
                                           "int function(void*p,...) {\n"
                                           "__builtin_va_list list;\n"
                                           "__builtin_va_start(list,p);\n"
                                           "int i = __builtin_va_arg(list,struct T).r[2];\n"
                                           "__builtin_va_end(list);\n"
                                           "return i;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            if (triple == cld::Triple::native())
            {
                CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr,
                                                                T{3.0, {3, 4, 5, 6, 7}})
                      == 5);
            }
        }
        SECTION("single fp80 in struct")
        {
            auto program = generateProgram("int (*print)(const char*,...);\n"
                                           "\n"
                                           "struct hfa31 { long double a; } s2 = { 31.1 };\n"
                                           "\n"
                                           "void fa_s2(void*p,...) {\n"
                                           "    __builtin_va_list list;\n"
                                           "    __builtin_va_start(list,p);\n"
                                           "    struct hfa31 a = __builtin_va_arg(list,struct hfa31);\n"
                                           "    __builtin_va_end(list);\n"
                                           "    print(\"%.1Lf,%.1Lf\",a.a,a.a);\n"
                                           "}\n"
                                           "\n"
                                           "void function(int (*printF)(const char*,...)) {\n"
                                           "print = printF;\n"
                                           "fa_s2(0,s2);\n"
                                           "}\n");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            if (triple == cld::Triple::native())
            {
                text.clear();
                cld::Tests::computeInJIT<void(int (*)(const char*, ...))>(std::move(module), "function",
                                                                          printfCallback);
                CHECK(text == "31.1,31.1");
            }
        }
    }
    SECTION("As function parameter")
    {
        auto program = generateProgram("static int foo(__builtin_va_list list)\n"
                                       "{\n"
                                       "return __builtin_va_arg(list,int);\n"
                                       "}\n"
                                       "\n"
                                       "int function(void*p,...) {\n"
                                       "__builtin_va_list list;\n"
                                       "__builtin_va_start(list,p);\n"
                                       "int i = foo(list);\n"
                                       "__builtin_va_end(list);\n"
                                       "return i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        if (triple == cld::Triple::native())
        {
            CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, 5) == 5);
        }
    }
}

TEST_CASE("LLVM codegen inline functions", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("InlineDefinition")
    {
        auto program = generateProgram("inline int foo(void) {\n"
                                       "return 5;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        auto* function = module->getFunction("foo");
        REQUIRE(function);
        CHECK(function->isDeclaration());
    }
    SECTION("InlineDefinition with internal linkage")
    {
        auto program = generateProgram("inline static int foo(void) {\n"
                                       "return 5;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        auto* function = module->getFunction("foo");
        REQUIRE(function);
        CHECK_FALSE(function->isDeclaration());
    }
    SECTION("extern inline")
    {
        SECTION("extern in definition")
        {
            auto program = generateProgram("extern inline int foo(void) {\n"
                                           "return 5;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            auto* function = module->getFunction("foo");
            REQUIRE(function);
            CHECK_FALSE(function->isDeclaration());
        }
        SECTION("extern in declaration")
        {
            auto program = generateProgram("inline int foo(void) {\n"
                                           "return 5;\n"
                                           "}\n"
                                           "\n"
                                           "extern inline int foo(void);\n");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            auto* function = module->getFunction("foo");
            REQUIRE(function);
            CHECK_FALSE(function->isDeclaration());
        }
    }
    SECTION("Inline in declaration only")
    {
        auto program = generateProgram("int foo(void) {\n"
                                       "return 5;\n"
                                       "}\n"
                                       "\n"
                                       "inline int foo(void);\n");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        auto* function = module->getFunction("foo");
        REQUIRE(function);
        CHECK_FALSE(function->isDeclaration());
    }
}

TEST_CASE("LLVM codegen nameless anonymous struct or union fields", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Simple member access and auto initializer")
    {
        auto program = generateProgram("struct __pthread_cond_s {\n"
                                       "    __extension__ union {\n"
                                       "        unsigned long long int __wsed;\n"
                                       "        struct {\n"
                                       "            unsigned int __low;\n"
                                       "            unsigned int __high;\n"
                                       "        };\n"
                                       "    };\n"
                                       "};\n"
                                       "\n"
                                       "static unsigned int* foo(struct __pthread_cond_s* c) {\n"
                                       "   return &c->__low;\n"
                                       "}\n"
                                       "\n"
                                       "unsigned int function(void) {\n"
                                       "    struct __pthread_cond_s s = {.__low = 5,.__high = 3};\n"
                                       "    return *foo(&s);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        CHECK(cld::Tests::computeInJIT<unsigned int()>(std::move(module), "function") == 5);
    }
    SECTION("Simple member access and static initializer")
    {
        auto program = generateProgram("struct __pthread_cond_s {\n"
                                       "    __extension__ union {\n"
                                       "        unsigned long long int __wsed;\n"
                                       "        struct {\n"
                                       "            unsigned int __low;\n"
                                       "            unsigned int __high;\n"
                                       "        };\n"
                                       "    };\n"
                                       "};\n"
                                       "\n"
                                       "static unsigned int* foo(struct __pthread_cond_s* c) {\n"
                                       "   return &c->__low;\n"
                                       "}\n"
                                       "\n"
                                       "struct __pthread_cond_s s = {.__low = 5,.__high = 3};\n"
                                       "\n"
                                       "unsigned int function(void) {\n"
                                       "    return *foo(&s);\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        CHECK(cld::Tests::computeInJIT<unsigned int()>(std::move(module), "function") == 5);
    }
    SECTION("Simple member access and static array initializer")
    {
        auto program =
            generateProgram("struct __pthread_cond_s {\n"
                            "    __extension__ union {\n"
                            "        unsigned long long int __wsed;\n"
                            "        struct {\n"
                            "            unsigned int __low;\n"
                            "            unsigned int __high;\n"
                            "        };\n"
                            "    };\n"
                            "};\n"
                            "\n"
                            "static unsigned int* foo(struct __pthread_cond_s* c) {\n"
                            "   return &c->__low;\n"
                            "}\n"
                            "\n"
                            "struct __pthread_cond_s s[2] = {[0] = {.__low = 5,.__high = 3},[1].__wsed = 5};\n"
                            "\n"
                            "unsigned int function(void) {\n"
                            "    return *foo(s);\n"
                            "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        CHECK(cld::Tests::computeInJIT<unsigned int()>(std::move(module), "function") == 5);
    }
}

TEST_CASE("LLVM codegen miscellaneous builtins", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("__builtin_*abs*")
    {
        SECTION("abs")
        {
            auto program = generateProgram("int function(void) {\n"
                                           "return __builtin_abs(5) == 5 && __builtin_abs(-5) == 5;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
        SECTION("llabs")
        {
            auto program = generateProgram("int function(void) {\n"
                                           "return __builtin_llabs(5ll) == 5ll && __builtin_llabs(-5ll) == 5ll;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
        SECTION("labs")
        {
            auto program = generateProgram("int function(void) {\n"
                                           "return __builtin_labs(5L) == 5L && __builtin_abs(-5L) == 5L;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
        SECTION("fabs")
        {
            auto program = generateProgram("int function(void) {\n"
                                           "return __builtin_fabs(5.) == 5. && __builtin_abs(-5.) == 5.;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
        SECTION("fabsf")
        {
            auto program = generateProgram("int function(void) {\n"
                                           "return __builtin_fabsf(5.f) == 5.f && __builtin_abs(-5.f) == 5.f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
        SECTION("fabsl")
        {
            auto program = generateProgram("int function(void) {\n"
                                           "return __builtin_fabsl(5.l) == 5.l && __builtin_abs(-5.l) == 5.l;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "function") == 1);
        }
    }
    SECTION("__builtin_inf*")
    {
        SECTION("inf")
        {
            auto program = generateProgram("double function(void) {\n"
                                           "return __builtin_inf();\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<double()>(std::move(module), "function")
                  == std::numeric_limits<double>::infinity());
        }
        SECTION("inff")
        {
            auto program = generateProgram("float function(void) {\n"
                                           "return __builtin_inff();\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<float()>(std::move(module), "function")
                  == std::numeric_limits<float>::infinity());
        }
        SECTION("infl")
        {
            auto program = generateProgram("long double function(void) {\n"
                                           "return __builtin_infl();\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<long double()>(std::move(module), "function")
                  == std::numeric_limits<long double>::infinity());
        }
    }
}

TEST_CASE("LLVM codegen flexible array members", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Accessing members")
    {
        struct S
        {
            char c;
            int f[5];
        };
        auto program = generateProgram("struct S {\n"
                                       "    char c;\n"
                                       "    int f[];\n"
                                       "};\n"
                                       "\n"
                                       "int function(struct S* s) {\n"
                                       "    for (int i = 0; i < 5; i++) {\n"
                                       "        if (s->f[i] != i) {\n"
                                       "            return 1;\n"
                                       "        }\n"
                                       "    }\n"
                                       "    return 0;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        struct S s = {0, {0, 1, 2, 3, 4}};
        CHECK(cld::Tests::computeInJIT<int(struct S*)>(std::move(module), "function", &s) == 0);
    }
}

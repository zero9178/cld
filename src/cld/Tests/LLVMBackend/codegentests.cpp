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
        CHECK(function->hasAttribute(0, llvm::Attribute::SExt));
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
    llvm::Module module("", context);
    SECTION("Tentative definition")
    {
        SECTION("Single")
        {
            auto program = generateProgram("int foo;");
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
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
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
            REQUIRE(variable);
            CHECK(variable->getLinkage() == llvm::GlobalValue::CommonLinkage);
            CHECK(!variable->isDeclaration());
            REQUIRE(variable->getType()->isPointerTy());
            CHECK(variable->getType()->getPointerElementType()->isIntegerTy(cld::LanguageOptions::native().sizeOfInt
                                                                            * 8));
        }
        SECTION("Internal linkage")
        {
            SECTION("Single")
            {
                auto program = generateProgram("static int foo;");
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo", true);
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
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo", true);
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
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
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
            cld::CGLLVM::generateLLVM(module, program);
            CAPTURE(module);
            REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
            CHECK(module.getGlobalList().size() == 1);
            auto* variable = module.getGlobalVariable("foo");
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
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
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
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo");
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
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo", true);
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
                cld::CGLLVM::generateLLVM(module, program);
                CAPTURE(module);
                REQUIRE_FALSE(llvm::verifyModule(module, &llvm::errs()));
                CHECK(module.getGlobalList().size() == 1);
                auto* variable = module.getGlobalVariable("foo", true);
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
        auto program = generateProgram("int* foo(void) {\n"
                                       "int i;\n"
                                       "return &i;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int*(void)>(std::move(module), "foo") != nullptr);
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
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "add", 5, 10) == 15);
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
            auto program = generateProgram("float add(float* r,int f) {\n"
                                           "return *(r + f);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            std::array<float, 2> a = {1, 2};
            CHECK(cld::Tests::computeInJIT<float(float*, int)>(std::move(module), "add", a.data(), 1) == 2);
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
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "sub", 10, 5) == 5);
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
            auto program = generateProgram("float sub(float* r,int f) {\n"
                                           "return *(r - f);\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            std::array<float, 2> a = {1, 2};
            CHECK(cld::Tests::computeInJIT<float(float*, int)>(std::move(module), "sub", a.data() + 2, 1) == 2);
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
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "mul", 3, 5) == 15);
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
            CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "div", 30, 5) == 6);
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
        auto program = generateProgram("int mod(int r,int f) {\n"
                                       "return r % f;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "mod", 30, 8) == 6);
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
        auto program = generateProgram("int rshift(int r,int f) {\n"
                                       "return r >> f;\n"
                                       "}");
        cld::CGLLVM::generateLLVM(*module, program);
        CAPTURE(*module);
        REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
        CHECK(cld::Tests::computeInJIT<int(int, int)>(std::move(module), "rshift", 127, 4) == 7);
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
}

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
}

TEST_CASE("LLVM codegen miscellaneous programs", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
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
                                       "        .arr = a"
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
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
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
        CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, 5) == 5);
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
        CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, 5}) == 5);
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
        CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, {3, 2}}) == 5);
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
        CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, 5.0}) == 8);
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
        CHECK(cld::Tests::computeInJIT<int(void*, ...)>(std::move(module), "function", nullptr, T{3.0, {3, 4, 5, 6, 7}})
              == 5);
    }
}

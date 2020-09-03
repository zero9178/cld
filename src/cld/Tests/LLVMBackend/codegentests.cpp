#include <catch.hpp>

#include <llvm/IR/Verifier.h>

#include <cld/LLVMBackend/Codegen.hpp>

#include <TestTargets.hpp>

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
            }
        }
#ifdef _WIN64
        SECTION("Execution")
        {
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
        }
#endif
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
        // TODO: Check initializers once those are implemented
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
    SECTION("Multiply")
    {
        SECTION("Integers")
        {
            auto program = generateProgram("int foo(void) {\n"
                                           "int r;\n"
                                           "r = 5;\n"
                                           "int f;\n"
                                           "f = 3;\n"
                                           "return r * f;\n"
                                           "}");
            cld::CGLLVM::generateLLVM(*module, program);
            CAPTURE(*module);
            REQUIRE_FALSE(llvm::verifyModule(*module, &llvm::errs()));
            CHECK(cld::Tests::computeInJIT<int()>(std::move(module), "foo") == 15);
        }
    }
}

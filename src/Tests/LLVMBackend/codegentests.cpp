#include <catch.hpp>

#include <llvm/IR/Verifier.h>

#include <LLVMBackend/Codegen.hpp>
#include <TestTargets.hpp>

#include "Common.hpp"

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

TEST_CASE("LLVM codegen expressions", "[LLVM]")
{
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("", context);
    SECTION("Lvalue conversion")
    {
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

#include <catch.hpp>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/cldmain.hpp>

#include <TestConfig.hpp>

#define CLD_MAIN_PRODUCES(matcher, ...)       \
    [](std::vector<std::string_view> ref) {   \
        std::string storage;                  \
        llvm::raw_string_ostream ss(storage); \
        cld::main(ref, &ss);                  \
        ss.flush();                           \
        CHECK_THAT(storage, matcher);         \
        if (!storage.empty())                 \
        {                                     \
            llvm::errs() << storage << '\n';  \
        }                                     \
    }(__VA_ARGS__)

using namespace cld::Errors::CLI;

TEST_CASE("Commandline options", "[cld]")
{
    SECTION("Compile")
    {
        auto dummyFile = createInclude("main.c", "int main(void) { return 0; }");
        SECTION("Normal")
        {
            std::vector<std::string_view> vector = {"-c", "main.c"};
            auto ret = cld::main(vector);
            CHECK(ret == 0);
            CHECK(cld::fs::exists(cld::fs::u8path("main.o")));
        }
        SECTION("LLVM")
        {
            std::vector<std::string_view> vector = {"-c", "main.c", "-emit-llvm"};
            auto ret = cld::main(vector);
            CHECK(ret == 0);
            CHECK(cld::fs::exists(cld::fs::u8path("main.bc")));
        }
        CLD_MAIN_PRODUCES(ProducesError(CANNOT_COMPILE_TO_OBJECT_FILE_AND_ASSEMBLY_AT_THE_SAME_TIME),
                          {"-c", "main.c", "-S"});
        CLD_MAIN_PRODUCES(ProducesError(CANNOT_COMPILE_TO_OBJECT_FILE_AND_PREPROCESS_AT_THE_SAME_TIME),
                          {"-c", "main.c", "-E"});
    }
    SECTION("Assembly")
    {
        auto dummyFile = createInclude("main.c", "int main(void) { return 0; }");
        SECTION("Normal")
        {
            std::vector<std::string_view> vector = {"-S", "main.c"};
            auto ret = cld::main(vector);
            CHECK(ret == 0);
            CHECK(cld::fs::exists(cld::fs::u8path("main.s")));
        }
        SECTION("LLVM")
        {
            std::vector<std::string_view> vector = {"-S", "main.c", "-emit-llvm"};
            auto ret = cld::main(vector);
            CHECK(ret == 0);
            CHECK(cld::fs::exists(cld::fs::u8path("main.ll")));
        }
        CLD_MAIN_PRODUCES(ProducesError(CANNOT_COMPILE_TO_ASSEMBLY_AND_PREPROCESS_AT_THE_SAME_TIME),
                          {"-S", "main.c", "-E"});
    }
    SECTION("Includes")
    {
        auto dummyFile = createInclude("main.c", "#include \"test.h\"\n"
                                                 "int main(void) { return 0; }");
        auto dummyHeader = createInclude("headers/test.h", "");
        std::vector<std::string_view> vector = {"-c", "main.c"};
        CHECK(cld::main(vector) != 0);
        CLD_MAIN_PRODUCES(ProducesNoErrors(), {"-c", "main.c", "-Iheaders"});
    }
    SECTION("output file")
    {
        auto dummyFile = createInclude("main.c", "int main(void) { return 0; }");
        cld::fs::remove(cld::fs::u8path("test.o"));
        std::vector<std::string_view> vector = {"-c", "main.c", "-o", "test.o"};
        CHECK(cld::main(vector) == 0);
        CHECK(cld::fs::exists(cld::fs::u8path("test.o")));
    }
    SECTION("disabling warnings")
    {
        auto dummyFile = createInclude("main.c", "#define foo 0\n"
                                                 "#define foo 0\n"
                                                 "int main(void) { return foo; }");
        CLD_MAIN_PRODUCES(ProducesNoWarnings(), {"-c", "main.c", "-Wno-macro-redefined"});
    }
    auto dummyFile = createInclude("main.c", "int main(void) { return 0; }");
    CLD_MAIN_PRODUCES(ProducesError(UNKNOWN_LANGUAGE_STANDARD_N, "java8"), {"-std=java8", "-c", "main.c"});
    CLD_MAIN_PRODUCES(ProducesError(NO_SOURCE_FILES_SPECIFIED), {"-c"});
    CLD_MAIN_PRODUCES(ProducesError(FAILED_TO_OPEN_C_SOURCE_FILE_N, "missing.c"), {"missing.c", "-c"});
    CLD_MAIN_PRODUCES(ProducesError(FAILED_TO_OPEN_FILE_N_FOR_OUTPUT, "/PleaseDoNotExitThatWouldBeSuperAwkward/main.o"),
                      {"main.c", "-c", "-o", "/PleaseDoNotExitThatWouldBeSuperAwkward/main.o"});
}

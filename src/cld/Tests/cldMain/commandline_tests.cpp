#include <catch.hpp>

#include <cld/cldMain/CommandLine.hpp>

namespace
{
CLD_CLI_OPT(COMPILE_ONLY, ("-c", "--compile"))("Stop after compiling object files");
CLD_CLI_OPT(INCLUDES, ("-I<dir>", "--include-directory <dir>", "--include-directory=<dir>"), (std::string_view, dir))
("Additional include directories", cld::CLIMultiArg::List);

CLD_CLI_OPT(STRING_ARG, ("--prefix=<arg>", "--prefix <arg>"), (std::string, arg))("A std::string arg");
CLD_CLI_OPT(STRING_VIEW_ARG, ("--prefix=<arg>", "--prefix <arg>"), (std::string_view, arg))("A std::string_view arg");
CLD_CLI_OPT(OPT, ("-O<level>", "-O", "--optimize", "--optimize=<level>"), (std::uint8_t, level))("Optimization level");
CLD_CLI_OPT(PIE, ("-f[no-]pie"))("Position independent executable");
} // namespace

TEST_CASE("Commandline without args", "[cli]")
{
    SECTION("Appears once")
    {
        SECTION("-c alternative")
        {
            std::vector<std::string_view> elements = {"awdawd", "-c", "wadawdaw"};
            auto cli = cld::parseCommandLine<COMPILE_ONLY>(elements);
            CHECK(cli.get<COMPILE_ONLY>());
            CHECK_THAT(cli.getUnrecognized(), Catch::Equals(std::vector<std::string_view>{"awdawd", "wadawdaw"}));
        }
        SECTION("--compile alternative")
        {
            std::vector<std::string_view> elements = {"awdawd", "--compile", "wadawdaw"};
            auto cli = cld::parseCommandLine<COMPILE_ONLY>(elements);
            CHECK(cli.get<COMPILE_ONLY>());
            CHECK_THAT(cli.getUnrecognized(), Catch::Equals(std::vector<std::string_view>{"awdawd", "wadawdaw"}));
        }
    }
    SECTION("Appears 0 times")
    {
        std::vector<std::string_view> elements = {"awdawd", "wadawdaw"};
        auto cli = cld::parseCommandLine<COMPILE_ONLY>(elements);
        CHECK_FALSE(cli.get<COMPILE_ONLY>());
        CHECK_THAT(cli.getUnrecognized(), Catch::Equals(std::vector<std::string_view>{"awdawd", "wadawdaw"}));
    }
    SECTION("Appears multiple times")
    {
        std::vector<std::string_view> elements = {"awdawd", "-c", "wadawdaw", "-c", "--compile"};
        auto cli = cld::parseCommandLine<COMPILE_ONLY>(elements);
        CHECK(cli.get<COMPILE_ONLY>());
        CHECK_THAT(cli.getUnrecognized(), Catch::Equals(std::vector<std::string_view>{"awdawd", "wadawdaw"}));
    }
}

TEST_CASE("Commandline arg types", "[cli]")
{
    SECTION("Don't match against whitespace")
    {
        std::vector<std::string_view> elements = {"-I", "text"};
        auto cli = cld::parseCommandLine<INCLUDES>(elements);
        CHECK(cli.get<INCLUDES>().empty());
        CHECK_THAT(cli.getUnrecognized(), Catch::Equals(std::vector<std::string_view>{"-I", "text"}));
    }
    SECTION("Strings")
    {
        SECTION("std::string")
        {
            std::vector<std::string_view> elements = {"--prefix", "text"};
            auto cli = cld::parseCommandLine<STRING_ARG>(elements);
            REQUIRE(cli.get<STRING_ARG>());
            CHECK(*cli.get<STRING_ARG>() == "text");
        }
        SECTION("std::string_view")
        {
            std::vector<std::string_view> elements = {"--prefix", "text"};
            auto cli = cld::parseCommandLine<STRING_VIEW_ARG>(elements);
            REQUIRE(cli.get<STRING_VIEW_ARG>());
            CHECK(*cli.get<STRING_VIEW_ARG>() == "text");
        }
    }
    SECTION("Integral types")
    {
        std::vector<std::string_view> elements = {"-O3"};
        auto cli = cld::parseCommandLine<OPT>(elements);
        REQUIRE(cli.get<OPT>());
        CHECK(*cli.get<OPT>() == 3);
    }
}

TEST_CASE("Commandline with multi arg", "[cli]")
{
    SECTION("List")
    {
        SECTION("Appears 0 times")
        {
            std::vector<std::string_view> elements = {"awdawd", "wadawdaw", "test"};
            auto cli = cld::parseCommandLine<INCLUDES>(elements);
            CHECK_THAT(cli.get<INCLUDES>(), Catch::Equals(std::vector<std::string_view>{}));
        }
        SECTION("Appears multiple times")
        {
            std::vector<std::string_view> elements = {"-Iawdawd", "--include-directory", "wadawdaw",
                                                      "--include-directory=test"};
            auto cli = cld::parseCommandLine<INCLUDES>(elements);
            CHECK_THAT(cli.get<INCLUDES>(), Catch::Equals(std::vector<std::string_view>{"awdawd", "wadawdaw", "test"}));
        }
    }
}

TEST_CASE("Commandline negate", "[cli]")
{
    std::vector<std::string_view> elements = {"awdawd", "wadawdaw", "test"};
    auto cli = cld::parseCommandLine<PIE>(elements);
    CHECK_FALSE(cli.get<PIE>());
    elements = {"awdawd", "-fpie", "test"};
    cli = cld::parseCommandLine<PIE>(elements);
    CHECK(cli.get<PIE>());
    elements = {"-fpie", "-fpie", "-fpie"};
    cli = cld::parseCommandLine<PIE>(elements);
    CHECK(cli.get<PIE>());
    elements = {"-fpie", "-fpie", "-fno-pie"};
    cli = cld::parseCommandLine<PIE>(elements);
    CHECK_FALSE(cli.get<PIE>());
    elements = {"-fpie", "-fno-pie", "-fpie"};
    cli = cld::parseCommandLine<PIE>(elements);
    CHECK(cli.get<PIE>());
}

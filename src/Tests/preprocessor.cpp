#include "catch.hpp"

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>

#include <sstream>

TEST_CASE("Preprocess skipped sections", "[preprocess]")
{
    std::stringstream ss;
    auto source = OpenCL::Lexer::tokenize("#if VERSION == 1\n"
                                          "#define INCFILE \"vers1.h\"\n"
                                          "#elif VERSION == 2\n"
                                          "#define INCFILE \"vers2.h\" // and so on\n"
                                          "#else\n"
                                          "#define INCFILE \"versN.h\"\n"
                                          "#endif\n"
                                          "#include INCFILE",
                                          &ss);
    REQUIRE(ss.str().empty());
    OpenCL::PP::preprocess(source, &ss);
    CHECK(ss.str().empty());
}

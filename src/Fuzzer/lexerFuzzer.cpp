#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string>

extern "C" int LLVMFuzzerTestOneInput(const std::uint8_t* data, std::size_t size)
{
    if (size == 0)
    {
        return 0;
    }
    std::string input(size, ' ');
    std::transform(data, data + size, input.begin(), [](std::uint8_t byte) -> char {
        char result;
        std::memcpy(&result, &byte, 1);
        return result;
    });

    std::string output;
    llvm::raw_string_ostream ss(output);
    OpenCL::Lexer::tokenize(input, OpenCL::LanguageOptions::native(), false, &ss);
    return 0;
}

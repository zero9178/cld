
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Preprocessor/Parser.hpp>

#include <algorithm>
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
    auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), true, &ss);
    if (!output.empty() || tokens.data().empty())
    {
        return 0;
    }
    cld::PP::buildTree(tokens, &ss);
    return 0;
}

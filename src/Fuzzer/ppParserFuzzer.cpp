
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

    std::string input(size, '\0');
    std::memcpy(input.data(), data, size);

    bool errors;
    auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), true, &llvm::nulls(), &errors);
    if (errors || tokens.data().empty())
    {
        return 0;
    }
    cld::PP::buildTree(tokens, &llvm::nulls());
    return 0;
}

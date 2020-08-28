#include <llvm/Support/raw_ostream.h>

#include <Frontend/Compiler/Lexer.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

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

    bool errors = false;
    auto tokens = cld::Lexer::tokenize(std::move(input), cld::LanguageOptions::native(), &llvm::nulls(), &errors);
    if (errors)
    {
        return 0;
    }
    cld::Lexer::toCTokens(tokens, &llvm::nulls());
    return 0;
}

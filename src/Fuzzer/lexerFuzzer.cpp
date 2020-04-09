#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/SourceObject.hpp>

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

    cld::Lexer::tokenize(input, cld::LanguageOptions::native(), false, &llvm::nulls());
    return 0;
}

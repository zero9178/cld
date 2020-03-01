#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SourceObject.hpp>

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
    std::memcpy(input.data(), data, size);

    std::string output;
    llvm::raw_string_ostream ss(output);
    auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), false, &ss);
    if (!output.empty() || tokens.data().empty())
    {
        return 0;
    }

    cld::Parser::Context context(tokens, &ss);
    context.setBracketMax(64);
    auto begin = tokens.data().cbegin();
    parseTranslationUnit(begin, tokens.data().cend(), context);

    return 0;
}

#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Parser.hpp>

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
    auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &llvm::nulls(), &errors);
    if (errors || tokens.data().empty())
    {
        return 0;
    }
    auto ctokens = cld::Lexer::toCTokens(tokens,&llvm::nulls(),&errors);
    if (errors || tokens.data().empty())
    {
        return 0;
    }

    cld::Parser::Context context(ctokens, &llvm::nulls());
    context.setBracketMax(64);
    auto begin = ctokens.data().cbegin();
    parseTranslationUnit(begin, ctokens.data().cend(), context);

    return 0;
}

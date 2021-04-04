#include <llvm/Support/raw_ostream.h>

#include <cld/Frontend/Compiler/Lexer.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>

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
    auto options = cld::LanguageOptions::native();
    auto tokens = cld::Lexer::tokenize(std::move(input), &options, &llvm::nulls(), &errors);
    if (errors || tokens.data().empty())
    {
        return 0;
    }
    auto ctokens = cld::Lexer::toCTokens(tokens, &llvm::nulls(), &errors);
    if (errors || tokens.data().empty())
    {
        return 0;
    }

    cld::Parser::Context context(ctokens, &llvm::nulls());
    context.setBracketMax(64);
    const auto* begin = std::as_const(ctokens).data().data();
    parseTranslationUnit(begin, ctokens.data().data() + ctokens.data().size(), context);

    return 0;
}

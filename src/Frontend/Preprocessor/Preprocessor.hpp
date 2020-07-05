#pragma once

#include <llvm/Support/raw_ostream.h>

#include <Frontend/Compiler/SourceObject.hpp>

#include <string>
#include <vector>

namespace cld
{
struct LanguageOptions;

namespace PP
{
PPSourceObject preprocess(PPSourceObject&& sourceObject, llvm::raw_ostream* reporter = &llvm::errs()) noexcept;

template <class T>
std::string reconstruct(const T* begin, const T* end, const SourceInterface& sourceInterface) noexcept
{
    static_assert(std::is_base_of_v<Lexer::TokenBase, T>);
    if (begin == end)
    {
        return {};
    }
    std::string result = Lexer::normalizeSpelling(begin->getRepresentation(sourceInterface));
    const auto* prev = begin++;
    while (begin != end)
    {
        if (begin->getTokenType() == cld::Lexer::TokenType::Newline)
        {
            result += '\n';
        }
        else if (begin->hasLeadingWhitespace()
                 || Lexer::needsWhitespaceInBetween(prev->getTokenType(), begin->getTokenType()))
        {
            result += " " + Lexer::normalizeSpelling(begin->getRepresentation(sourceInterface));
        }
        else
        {
            result += Lexer::normalizeSpelling(begin->getRepresentation(sourceInterface));
        }
        prev = begin++;
    }
    return result;
}

} // namespace PP
} // namespace cld

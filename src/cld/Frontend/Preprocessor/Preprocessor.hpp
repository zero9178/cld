#pragma once

#include <llvm/Support/raw_ostream.h>

#include <cld/Frontend/Compiler/SourceObject.hpp>

#include <string>
#include <vector>

namespace cld
{
struct LanguageOptions;

namespace PP
{
PPSourceObject preprocess(PPSourceObject&& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(),
                          bool* errorsOccurred = nullptr) noexcept;

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
        std::uint64_t newLines = 0;
        auto toMacroId0 = [&](std::uint32_t macroId, std::uint32_t fileId, std::uint64_t offset, bool isPrev) {
            auto tuple = std::make_tuple(macroId, fileId, offset);
            while (std::get<0>(tuple) != 0)
            {
                tuple = cld::match(
                    sourceInterface.getSubstitutions()[std::get<0>(tuple)],
                    [](std::monostate) -> std::tuple<std::uint32_t, std::uint32_t, std::uint64_t> { CLD_UNREACHABLE; },
                    [isPrev](const Source::Substitution& substitution) {
                        if (isPrev && substitution.closeParentheses)
                        {
                            return std::make_tuple(substitution.closeParentheses->getMacroId(),
                                                   substitution.closeParentheses->getFileId(),
                                                   substitution.closeParentheses->getOffset());
                        }
                        return std::make_tuple(substitution.replacedIdentifier.getMacroId(),
                                               substitution.replacedIdentifier.getFileId(),
                                               substitution.replacedIdentifier.getOffset());
                    },
                    [](const Source::Stringification& stringification) {
                        return std::make_tuple(stringification.replacedIdentifier.getMacroId(),
                                               stringification.replacedIdentifier.getFileId(),
                                               stringification.replacedIdentifier.getOffset());
                    },
                    [isPrev](const Source::TokenConcatenation& concat) {
                        if (isPrev)
                        {
                            return std::make_tuple(concat.rightToken.getMacroId(), concat.rightToken.getFileId(),
                                                   concat.rightToken.getOffset());
                        }
                        return std::make_tuple(concat.leftToken.getMacroId(), concat.leftToken.getFileId(),
                                               concat.leftToken.getOffset());
                    });
            }
            return tuple;
        };
        auto beginValues = toMacroId0(begin->getMacroId(), begin->getFileId(), begin->getOffset(), false);
        auto prevValues = toMacroId0(prev->getMacroId(), prev->getFileId(), prev->getOffset(), true);
        // Both prev and begin are part of a function like macro then it can happen that prev will end up
        // pointing at the ) of the macro call while begin points at the identifier. For that case as
        // they are part of the same macro there cannot be a newline in between
        if (std::get<2>(beginValues) >= std::get<2>(prevValues))
        {
            newLines = std::min<std::size_t>(
                sourceInterface.getLineNumber(std::get<1>(beginValues), std::get<2>(beginValues))
                    - sourceInterface.getLineNumber(std::get<1>(prevValues), std::get<2>(prevValues)),
                1);
        }
        result.insert(result.size(), newLines, '\n');
        if (newLines == 0
            && (begin->hasLeadingWhitespace()
                || Lexer::needsWhitespaceInBetween(prev->getTokenType(), begin->getTokenType())))
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

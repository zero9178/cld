#pragma once

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <string>
#include <type_traits>
#include <vector>

#include "LanguageOptions.hpp"
#include "Lexer.hpp"
#include "SourceInterface.hpp"

namespace cld
{
namespace Source
{
struct File
{
    std::string path;
    std::string source;
    std::vector<std::uint64_t> starts;
    std::vector<Lexer::PPToken> ppTokens;
};

struct Substitution
{
    Lexer::PPToken macroIdentifier;
};
} // namespace Source

template <class T>
class SourceObject final : public SourceInterface
{
    std::vector<T> m_tokens;
    std::vector<Source::File> m_files;
    LanguageOptions m_languageOptions = LanguageOptions::native(LanguageOptions::C99);
    std::vector<Source::Substitution> m_substitutions;
    std::vector<std::uint64_t> m_afterPPStarts;

    [[nodiscard]] const Lexer::TokenBase* inc(const Lexer::TokenBase* ptr) const noexcept override
    {
        return static_cast<const T*>(ptr) + 1;
    }

    [[nodiscard]] const Lexer::TokenBase* dec(const Lexer::TokenBase* ptr) const noexcept override
    {
        return static_cast<const T*>(ptr) - 1;
    }

public:
    SourceObject() = default;

    explicit SourceObject(std::vector<T> tokens, std::vector<Source::File> files, LanguageOptions languageOptions,
                          std::vector<Source::Substitution> substitutions,
                          std::vector<std::uint64_t> afterPPStarts = {})
        : m_tokens(std::move(tokens)),
          m_files(std::move(files)),
          m_languageOptions(std::move(languageOptions)),
          m_substitutions(substitutions),
          m_afterPPStarts(std::move(afterPPStarts))
    {
    }

    [[nodiscard]] std::uint64_t getLineNumber(Lexer::FileID fileID, std::uint64_t offset) const noexcept override
    {
        CLD_ASSERT((std::uint64_t)fileID < m_files.size());
        auto result = std::lower_bound(m_files[(std::uint64_t)fileID].starts.begin(),
                                       m_files[(std::uint64_t)fileID].starts.end(), offset);
        CLD_ASSERT(result != m_files[(std::uint64_t)fileID].starts.end());
        return std::distance(m_files[(std::uint64_t)fileID].starts.begin(), result) + (*result == offset ? 1 : 0);
    }

    [[nodiscard]] std::uint64_t getLineStartOffset(Lexer::FileID fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT((std::uint64_t)fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[(std::uint64_t)fileID].starts.size());
        return m_files[(std::uint64_t)fileID].starts[line - 1];
    }

    [[nodiscard]] std::uint64_t getLineEndOffset(Lexer::FileID fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT((std::uint64_t)fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[(std::uint64_t)fileID].starts.size());
        return line == m_files[(std::uint64_t)fileID].starts.size() ?
                   m_files[(std::uint64_t)fileID].ppTokens.back().getOffset()
                       + m_files[(std::uint64_t)fileID].ppTokens.back().getLength() :
                   m_files[(std::uint64_t)fileID].starts[line];
    }

    [[nodiscard]] const std::vector<T>& data() const noexcept
    {
        return m_tokens;
    }

    [[nodiscard]] std::vector<T>& data() noexcept
    {
        return m_tokens;
    }

    [[nodiscard]] const LanguageOptions& getLanguageOptions() const noexcept
    {
        return m_languageOptions;
    }

    [[nodiscard]] std::uint64_t getPPLineNumber(std::uint64_t offset) const noexcept override
    {
        auto result = std::lower_bound(m_afterPPStarts.begin(), m_afterPPStarts.end(), offset);
        CLD_ASSERT(result != m_afterPPStarts.end());
        return std::distance(m_afterPPStarts.begin(), result) + (*result == offset ? 1 : 0);
    }

    [[nodiscard]] std::uint64_t getPPLineStartOffset(std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(line - 1 < m_afterPPStarts.size());
        return m_afterPPStarts[line - 1];
    }

    [[nodiscard]] std::uint64_t getPPLineEndOffset(std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(line - 1 < m_afterPPStarts.size());
        return line == m_afterPPStarts.size() ? m_tokens.back().getPPOffset() + m_tokens.back().getLength() :
                                                m_afterPPStarts[line];
    }

    [[nodiscard]] const std::vector<Source::Substitution>& getSubstitutions() const noexcept
    {
        return m_substitutions;
    }

    [[nodiscard]] const std::vector<Source::File>& getFiles() const noexcept override
    {
        return m_files;
    }

    [[nodiscard]] std::vector<Source::File>& getFiles() noexcept
    {
        return m_files;
    }
};

using CSourceObject = SourceObject<Lexer::CToken>;
using PPSourceObject = SourceObject<Lexer::PPToken>;

extern template class SourceObject<Lexer::CToken>;

extern template class SourceObject<Lexer::PPToken>;

} // namespace cld

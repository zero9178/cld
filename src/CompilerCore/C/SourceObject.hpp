#pragma once

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <string>
#include <type_traits>
#include <vector>

#include "LanguageOptions.hpp"

namespace cld
{
namespace Lexer
{
class CToken;
class PPToken;
} // namespace Lexer

namespace Source
{
struct File
{
    std::string path;
    std::string source;
    std::vector<std::uint64_t> starts;
    std::vector<std::uint64_t> afterPPStarts;
};

struct Substitution
{
    std::uint64_t identifierPos;
    std::uint64_t identifierLength;
};
} // namespace Source

template <class T>
class SourceObject
{
public:
private:
    std::vector<T> m_tokens;
    std::vector<Source::File> m_files;
    LanguageOptions m_languageOptions = LanguageOptions::native(LanguageOptions::C99);
    std::vector<Source::Substitution> m_substitutions;

public:
    SourceObject() = default;

    explicit SourceObject(std::vector<T> tokens, std::vector<Source::File> files, LanguageOptions languageOptions,
                          std::vector<Source::Substitution> substitutions)
        : m_tokens(std::move(tokens)),
          m_files(files),
          m_languageOptions(std::move(languageOptions)),
          m_substitutions(substitutions)
    {
    }

    [[nodiscard]] std::uint64_t getLineNumber(std::uint64_t fileID, std::uint64_t offset) const noexcept
    {
        CLD_ASSERT(fileID < m_files.size());
        auto result = std::lower_bound(m_files[fileID].starts.begin(), m_files[fileID].starts.end(), offset);
        return std::distance(m_files[fileID].starts.begin(), result) + (*result == offset ? 1 : 0);
    }

    [[nodiscard]] std::uint64_t getLineStartOffset(std::uint64_t fileID, std::uint64_t line) const noexcept
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].starts.size());
        return m_files[fileID].starts[line - 1];
    }

    [[nodiscard]] std::uint64_t getLineEndOffset(std::uint64_t fileID, std::uint64_t line) const noexcept
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].starts.size());
        return line == m_files[fileID].starts.size() ? m_tokens.back().getOffset() + m_tokens.back().getLength() :
                                                       m_files[fileID].starts[line];
    }

    [[nodiscard]] const std::vector<T>& data() const noexcept
    {
        return m_tokens;
    }

    [[nodiscard]] const LanguageOptions& getLanguageOptions() const noexcept
    {
        return m_languageOptions;
    }

    [[nodiscard]] std::uint64_t getPPLineNumber(std::uint64_t fileID, std::uint64_t offset) const noexcept
    {
        CLD_ASSERT(fileID < m_files.size());
        auto result =
            std::lower_bound(m_files[fileID].afterPPStarts.begin(), m_files[fileID].afterPPStarts.end(), offset);
        return std::distance(m_files[fileID].afterPPStarts.begin(), result) + (*result == offset ? 1 : 0);
    }

    [[nodiscard]] std::uint64_t getPPLineStartOffset(std::uint64_t fileID, std::uint64_t line) const noexcept
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].afterPPStarts.size());
        return m_files[fileID].afterPPStarts[line - 1];
    }

    [[nodiscard]] std::uint64_t getPPLineEndOffset(std::uint64_t fileID, std::uint64_t line) const noexcept
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].afterPPStarts.size());
        return line == m_files[fileID].afterPPStarts.size() ?
                   m_tokens.back().getPPOffset() + m_tokens.back().getLength() :
                   m_files[fileID].afterPPStarts[line];
    }

    const std::vector<Source::Substitution>& getSubstitutions() const noexcept
    {
        return m_substitutions;
    }

    const std::vector<Source::File>& getFiles() const noexcept
    {
        return m_files;
    }
};

using CSourceObject = SourceObject<Lexer::CToken>;
using PPSourceObject = SourceObject<Lexer::PPToken>;

extern template class SourceObject<Lexer::CToken>;

extern template class SourceObject<Lexer::PPToken>;

} // namespace cld

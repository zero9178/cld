#pragma once

#include <Frontend/Common/Util.hpp>

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
    std::vector<std::tuple<std::uint64_t, std::optional<std::string>, std::uint64_t>> lineAndFileMapping{};
};

struct Substitution
{
    Lexer::PPToken macroIdentifier;
    Lexer::PPToken replacedIdentifier;
    std::optional<Lexer::PPToken> closeParentheses;
    bool empty;
};

struct Stringification
{
    std::vector<Lexer::PPToken> stringified;
    Lexer::PPToken replacedIdentifier;
};

struct TokenConcatenation
{
    Lexer::PPToken leftToken;
    Lexer::PPToken rightToken;
};
} // namespace Source

namespace detail
{
template <class T>
class SourceObjectStorage;

template <>
class SourceObjectStorage<Lexer::CToken>
{
};

template <>
class SourceObjectStorage<Lexer::PPToken> : public virtual PPSourceInterface
{
    std::vector<Lexer::IntervalMap> m_intervalMap;

public:
    SourceObjectStorage() = default;

    SourceObjectStorage(std::vector<Lexer::IntervalMap> intervalMap) : m_intervalMap(std::move(intervalMap)) {}

    llvm::ArrayRef<Lexer::IntervalMap> getIntervalMaps() const noexcept
    {
        return m_intervalMap;
    }

    std::vector<Lexer::IntervalMap>& getIntervalMap()
    {
        return m_intervalMap;
    }
};
} // namespace detail

template <class T>
class SourceObject final : virtual public SourceInterface, public detail::SourceObjectStorage<T>
{
    std::vector<T> m_tokens;
    std::vector<Source::File> m_files;
    LanguageOptions m_languageOptions = LanguageOptions::native(LanguageOptions::C99);
    std::vector<Source::PPRecord> m_substitutions;

public:
    SourceObject() = default;

    template <class U = T, std::enable_if_t<!std::is_same_v<Lexer::PPToken, U>>* = nullptr>
    SourceObject(std::vector<T> tokens, std::vector<Source::File> files, LanguageOptions languageOptions,
                 std::vector<Source::PPRecord> substitutions)
        : m_tokens(std::move(tokens)),
          m_files(std::move(files)),
          m_languageOptions(std::move(languageOptions)),
          m_substitutions(std::move(substitutions))
    {
    }

    template <class U = T, std::enable_if_t<std::is_same_v<Lexer::PPToken, U>>* = nullptr>
    SourceObject(std::vector<T> tokens, std::vector<Source::File> files, LanguageOptions languageOptions,
                 std::vector<Source::PPRecord> substitutions, std::vector<Lexer::IntervalMap> intervalMaps)
        : detail::SourceObjectStorage<T>(std::move(intervalMaps)),
          m_tokens(std::move(tokens)),
          m_files(std::move(files)),
          m_languageOptions(std::move(languageOptions)),
          m_substitutions(std::move(substitutions))
    {
    }

    [[nodiscard]] std::uint64_t getLineNumber(std::uint32_t fileID, std::uint64_t offset) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        auto result = std::lower_bound(m_files[fileID].starts.begin(), m_files[fileID].starts.end(), offset);
        CLD_ASSERT(result != m_files[fileID].starts.end());
        return std::distance(m_files[fileID].starts.begin(), result) + (*result == offset ? 1 : 0);
    }

    [[nodiscard]] std::uint64_t getLineStartOffset(std::uint32_t fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].starts.size());
        return m_files[fileID].starts[line - 1];
    }

    [[nodiscard]] std::uint64_t getLineEndOffset(std::uint32_t fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].starts.size());
        if (line != m_files[fileID].starts.size())
        {
            return m_files[fileID].starts[line] - 1;
        }
        return m_files[fileID].ppTokens.back().getOffset() + m_files[fileID].ppTokens.back().getLength();
    }

    [[nodiscard]] const std::vector<T>& data() const noexcept
    {
        return m_tokens;
    }

    [[nodiscard]] std::vector<T>& data() noexcept
    {
        return m_tokens;
    }

    [[nodiscard]] const LanguageOptions& getLanguageOptions() const noexcept override
    {
        return m_languageOptions;
    }

    [[nodiscard]] llvm::ArrayRef<Source::PPRecord> getSubstitutions() const noexcept override
    {
        return m_substitutions;
    }

    [[nodiscard]] llvm::ArrayRef<Source::File> getFiles() const noexcept override
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

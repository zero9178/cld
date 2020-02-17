#include "SourceObject.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <cassert>

cld::SourceObject::SourceObject(std::vector<std::uint64_t> starts, std::vector<Lexer::Token> tokens,
                                LanguageOptions languageOptions, SubstitutionMap substitutions)
    : m_starts(std::move(starts)),
      m_tokens(std::move(tokens)),
      m_languageOptions(languageOptions),
      m_substitutions(std::move(substitutions))
{
    assert(std::is_sorted(m_starts.begin(), m_starts.end()));
}

cld::LanguageOptions cld::SourceObject::getLanguageOptions() const
{
    return m_languageOptions;
}

std::uint64_t cld::SourceObject::getLineNumber(std::uint64_t offset) const noexcept
{
    auto result = std::lower_bound(m_starts.begin(), m_starts.end(), offset);
    return std::distance(m_starts.begin(), result) + (*result == offset ? 1 : 0);
}

std::uint64_t cld::SourceObject::getLineStartOffset(std::uint64_t line) const noexcept
{
    assert(line - 1 < m_starts.size());
    return m_starts[line - 1];
}

std::uint64_t cld::SourceObject::getLineEndOffset(std::uint64_t line) const noexcept
{
    assert(line - 1 < m_starts.size());
    return line == m_starts.size() ? m_tokens.back().getOffset() + m_tokens.back().getLength() : m_starts[line];
}

const std::vector<cld::Lexer::Token>& cld::SourceObject::data() const
{
    return m_tokens;
}

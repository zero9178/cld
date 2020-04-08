#include "SourceObject.hpp"

#include <CompilerCore/Common/Util.hpp>

cld::SourceObject::SourceObject(std::vector<std::uint64_t> starts, std::vector<Lexer::Token> tokens,
                                LanguageOptions languageOptions)
    : m_starts(std::move(starts)), m_languageOptions(languageOptions), m_tokens(std::move(tokens))
{
    CLD_ASSERT(std::is_sorted(m_starts.begin(), m_starts.end()));
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
    CLD_ASSERT(line - 1 < m_starts.size());
    return m_starts[line - 1];
}

std::uint64_t cld::SourceObject::getLineEndOffset(std::uint64_t line) const noexcept
{
    CLD_ASSERT(line - 1 < m_starts.size());
    return line == m_starts.size() ? m_tokens.back().getOffset() + m_tokens.back().getLength() : m_starts[line];
}

const std::vector<cld::Lexer::Token>& cld::SourceObject::data() const
{
    return m_tokens;
}

bool cld::SourceObject::isPreprocessed() const
{
    return false;
}

cld::PPSourceObject::PPSourceObject(const SourceObject& sourceObject, std::vector<Lexer::Token> tokens,
                                    const SubstitutionMap& substitutions, const std::vector<std::uint64_t>& ppstarts)
    : SourceObject(sourceObject), m_substitutions(substitutions), m_starts(ppstarts)
{
    m_tokens = std::move(tokens);
}

bool cld::PPSourceObject::isPreprocessed() const
{
    return true;
}

std::uint64_t cld::PPSourceObject::getPPLineNumber(std::uint64_t offset) const noexcept
{
    auto result = std::lower_bound(m_starts.begin(), m_starts.end(), offset);
    return std::distance(m_starts.begin(), result) + (*result == offset ? 1 : 0);
}

std::uint64_t cld::PPSourceObject::getPPLineStartOffset(std::uint64_t line) const noexcept
{
    CLD_ASSERT(line - 1 < m_starts.size());
    return m_starts[line - 1];
}

std::uint64_t cld::PPSourceObject::getPPLineEndOffset(std::uint64_t line) const noexcept
{
    CLD_ASSERT(line - 1 < m_starts.size());
    return line == m_starts.size() ? m_tokens.back().getPPOffset() + m_tokens.back().getLength() : m_starts[line];
}

#include "SourceObject.hpp"

cld::SourceObject::SourceObject(std::string source, std::vector<std::uint64_t> starts, std::vector<Lexer::Token> tokens,
                                LanguageOptions languageOptions)
    : m_source(std::move(source)),
      m_starts(std::move(starts)),
      m_languageOptions(std::move(languageOptions)),
      m_tokens(std::move(tokens))
{
    CLD_ASSERT(std::is_sorted(m_starts.begin(), m_starts.end()));
    for (auto& iter : m_tokens)
    {
        iter.setOriginalSource(m_source.data());
    }
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

const std::string& cld::SourceObject::getSource() const
{
    return m_source;
}

cld::SourceObject::SourceObject(const cld::SourceObject& rhs)
    : SourceObject(rhs.getSource(), rhs.m_starts, rhs.m_tokens, rhs.getLanguageOptions())
{
}

cld::SourceObject& cld::SourceObject::operator=(const cld::SourceObject& rhs)
{
    m_source = rhs.m_source;
    m_starts = rhs.m_starts;
    m_tokens.clear();
    m_tokens.reserve(rhs.m_tokens.size());
    std::transform(rhs.m_tokens.begin(), rhs.m_tokens.end(), std::back_inserter(m_tokens), [this](Lexer::Token token) {
        token.setOriginalSource(m_source.data());
        return token;
    });
    m_languageOptions = rhs.m_languageOptions;
    return *this;
}

cld::SourceObject::SourceObject(cld::SourceObject&& rhs) noexcept
    : SourceObject(std::move(rhs.m_source), std::move(rhs.m_starts), std::move(rhs.m_tokens),
                   std::move(rhs.m_languageOptions))
{
}

cld::SourceObject& cld::SourceObject::operator=(cld::SourceObject&& rhs) noexcept
{
    m_source = std::move(rhs.m_source);
    m_starts = std::move(rhs.m_starts);
    m_tokens = std::move(rhs.m_tokens);
    for (auto& iter : m_tokens)
    {
        iter.setOriginalSource(m_source.data());
    }
    m_languageOptions = std::move(rhs.m_languageOptions);
    return *this;
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

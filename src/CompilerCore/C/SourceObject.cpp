#include "SourceObject.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <cassert>

#include "ParserUtil.hpp"

OpenCL::SourceObject::SourceObject(std::vector<Lexer::Token> tokens, LanguageOptions languageOptions,
                                   SubstitutionMap substitutions)
    : m_tokens(std::move(tokens)), m_languageOptions(languageOptions), m_substitutions(std::move(substitutions))
{
    constructLineMap();
}

void OpenCL::SourceObject::constructLineMap()
{
    m_lines.clear();
    auto sourceBegin = this->m_tokens.cbegin();
    while (sourceBegin != this->m_tokens.cend())
    {
        if (this->m_lines.size() != sourceBegin->getLine() - 1)
        {
            this->m_lines.emplace_back(sourceBegin, sourceBegin);
            continue;
        }
        assert(m_lines.size() >= sourceBegin->getLine() - 1);
        auto eol = OpenCL::findEOL(sourceBegin, this->m_tokens.cend());
        this->m_lines.emplace_back(sourceBegin, eol);
        sourceBegin = eol;
    }
}

OpenCL::SourceObject::const_iterator OpenCL::SourceObject::getLineStart(OpenCL::SourceObject::const_iterator iter) const
{
    if (m_lines.empty())
    {
        return m_tokens.cbegin();
    }
    if (iter == m_lines.back().second)
    {
        return m_lines.back().first;
    }
    assert(iter->getLine() - 1 < m_lines.size());
    return m_lines[iter->getLine() - 1].first;
}

OpenCL::SourceObject::const_iterator OpenCL::SourceObject::getLineEnd(OpenCL::SourceObject::const_iterator iter) const
{
    if (m_lines.empty() || iter == m_lines.back().second)
    {
        return m_tokens.cend();
    }
    assert(iter->getLine() - 1 < m_lines.size());
    return m_lines[iter->getLine() - 1].second;
}

const std::vector<OpenCL::Lexer::Token>& OpenCL::SourceObject::data() const
{
    return m_tokens;
}

OpenCL::SourceObject::const_iterator OpenCL::SourceObject::begin() const
{
    return m_tokens.begin();
}

OpenCL::SourceObject::const_iterator OpenCL::SourceObject::end() const
{
    return m_tokens.end();
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::SourceObject::cbegin() const
{
    return m_tokens.begin();
}

OpenCL::SourceObject::const_iterator OpenCL::SourceObject::cend() const
{
    return m_tokens.end();
}

OpenCL::SourceObject::SourceObject(const OpenCL::SourceObject& sourceObject)
    : SourceObject(sourceObject.data(), sourceObject.getLanguageOptions(), sourceObject.m_substitutions)
{
}

OpenCL::SourceObject& OpenCL::SourceObject::operator=(const OpenCL::SourceObject& sourceObject)
{
    m_tokens = sourceObject.data();
    m_languageOptions = sourceObject.getLanguageOptions();
    m_substitutions = sourceObject.m_substitutions;
    constructLineMap();
    return *this;
}

OpenCL::SourceObject::SourceObject(OpenCL::SourceObject&& sourceObject) noexcept
    : SourceObject(std::move(sourceObject.m_tokens), sourceObject.getLanguageOptions(),
                   std::move(sourceObject.m_substitutions))
{
}

OpenCL::SourceObject& OpenCL::SourceObject::operator=(OpenCL::SourceObject&& sourceObject) noexcept
{
    m_tokens = std::move(sourceObject.m_tokens);
    m_languageOptions = sourceObject.getLanguageOptions();
    m_substitutions = std::move(sourceObject.m_substitutions);
    constructLineMap();
    return *this;
}

OpenCL::LanguageOptions OpenCL::SourceObject::getLanguageOptions() const
{
    return m_languageOptions;
}

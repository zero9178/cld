#include "SourceObject.hpp"

#include "ParserUtil.hpp"

OpenCL::SourceObject::SourceObject(std::vector<OpenCL::Lexer::Token> tokens, Language language)
    : m_tokens(std::move(tokens)), m_language(language)
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
        auto eol = OpenCL::findEOL(sourceBegin, this->m_tokens.cend());
        this->m_lines.emplace_back(sourceBegin, eol);
        sourceBegin = eol;
    }
}

std::vector<OpenCL::Lexer::Token>::const_iterator
    OpenCL::SourceObject::getLineStart(std::vector<OpenCL::Lexer::Token>::const_iterator iter) const
{
    if (m_lines.empty())
    {
        return m_tokens.cbegin();
    }
    if (iter == m_lines.back().second)
    {
        return m_lines.back().first;
    }
    return m_lines[iter->getLine() - 1].first;
}

std::vector<OpenCL::Lexer::Token>::const_iterator
    OpenCL::SourceObject::getLineEnd(std::vector<OpenCL::Lexer::Token>::const_iterator iter) const
{
    if (m_lines.empty() || iter == m_lines.back().second)
    {
        return m_tokens.cend();
    }
    return m_lines[iter->getLine() - 1].second;
}

const std::vector<OpenCL::Lexer::Token>& OpenCL::SourceObject::data() const
{
    return m_tokens;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::SourceObject::begin() const
{
    return m_tokens.begin();
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::SourceObject::end() const
{
    return m_tokens.end();
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::SourceObject::cbegin() const
{
    return m_tokens.begin();
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::SourceObject::cend() const
{
    return m_tokens.end();
}

OpenCL::SourceObject::SourceObject(const OpenCL::SourceObject& sourceObject) : SourceObject(sourceObject.data()) {}

OpenCL::SourceObject& OpenCL::SourceObject::operator=(const OpenCL::SourceObject& sourceObject)
{
    m_tokens = sourceObject.data();
    constructLineMap();
    return *this;
}

OpenCL::SourceObject::SourceObject(OpenCL::SourceObject&& sourceObject) noexcept
    : SourceObject(std::move(sourceObject.m_tokens))
{
}
OpenCL::SourceObject& OpenCL::SourceObject::operator=(OpenCL::SourceObject&& sourceObject) noexcept
{
    m_tokens = std::move(sourceObject.m_tokens);
    constructLineMap();
    return *this;
}

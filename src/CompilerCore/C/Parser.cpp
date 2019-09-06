#include "Parser.hpp"

#include <algorithm>
#include <cassert>

#include "ParserUtil.hpp"

std::pair<OpenCL::Syntax::TranslationUnit, bool> OpenCL::Parser::buildTree(const std::vector<Lexer::Token>& tokens,
                                                                           std::ostream* reporter)
{
    Context context(tokens.cbegin(), tokens.cend(), reporter);
    auto begin = tokens.cbegin();
    return {parseTranslationUnit(begin, tokens.cend(), context), context.getCurrentErrorCount() == 0};
}

void OpenCL::Parser::Context::addTypedef(const std::string& name, DeclarationLocation declarator)
{
    auto [iter, inserted] = m_currentScope.back().emplace(name, Declaration{declarator, true});
    if (!inserted && iter->second.isTypedef)
    {
        log({Message::error(ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + name + '\''),
                            getLineStart(declarator.begin), getLineEnd(declarator.end),
                            Modifier(declarator.identifier, declarator.identifier + 1)),
             Message::note(Notes::PREVIOUSLY_DECLARED_HERE, getLineStart(iter->second.location.begin),
                           getLineEnd(iter->second.location.end),
                           Modifier(iter->second.location.identifier, iter->second.location.identifier + 1))});
    }
}

bool OpenCL::Parser::Context::isTypedef(const std::string& name) const
{
    for (auto& iter : m_currentScope)
    {
        if (auto result = iter.find(name); result != iter.end() && result->second.isTypedef)
        {
            return true;
        }
    }
    return false;
}

void OpenCL::Parser::Context::log(std::vector<Message> messages)
{
    for (auto& iter : messages)
    {
        if (iter.getSeverity() == Message::Error)
        {
            m_errorCount++;
        }
        if (this->m_reporter)
        {
            *this->m_reporter << iter;
        }
    }
}

void OpenCL::Parser::Context::addToScope(const std::string& name, DeclarationLocation declarator)
{
    assert(!name.empty());
    auto [iter, inserted] = m_currentScope.back().emplace(name, Declaration{declarator, false});
    if (!inserted && iter->second.isTypedef)
    {
        log({Message::error(ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + name + '\''),
                            getLineStart(declarator.begin), getLineEnd(declarator.end),
                            Modifier(declarator.identifier, declarator.identifier + 1)),
             Message::note(Notes::PREVIOUSLY_DECLARED_HERE, getLineStart(iter->second.location.begin),
                           getLineEnd(iter->second.location.end),
                           Modifier(iter->second.location.identifier, iter->second.location.identifier + 1))});
    }
}

void OpenCL::Parser::Context::pushScope()
{
    m_currentScope.emplace_back();
}

void OpenCL::Parser::Context::popScope()
{
    m_currentScope.pop_back();
}

std::size_t OpenCL::Parser::Context::getCurrentErrorCount() const
{
    return m_errorCount;
}

const OpenCL::Parser::Context::DeclarationLocation*
    OpenCL::Parser::Context::getLocationOf(const std::string& name) const
{
    for (auto iter = m_currentScope.rbegin(); iter != m_currentScope.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end())
        {
            return &result->second.location;
        }
    }
    return nullptr;
}

bool OpenCL::Parser::Context::isTypedefInScope(const std::string& name) const
{
    for (auto iter = m_currentScope.rbegin(); iter != m_currentScope.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end())
        {
            return result->second.isTypedef;
        }
    }
    return false;
}

OpenCL::Parser::Context::Context(Tokens::const_iterator sourceBegin, Tokens::const_iterator sourceEnd,
                                 std::ostream* reporter)
    : m_reporter(reporter)
{
    while (sourceBegin != sourceEnd)
    {
        auto eol = findEOL(sourceBegin, sourceEnd);
        m_lines.insert({*sourceBegin, {sourceBegin, eol}});
        sourceBegin = eol;
    }
}

bool OpenCL::Parser::Context::tokenCompare(const OpenCL::Lexer::Token& lhs, const OpenCL::Lexer::Token& rhs)
{
    return lhs.getLine() < rhs.getLine();
}

std::vector<OpenCL::Lexer::Token>::const_iterator
    OpenCL::Parser::Context::getLineStart(std::vector<OpenCL::Lexer::Token>::const_iterator iter) const
{
    if (m_lines.empty())
    {
        return iter;
    }
    auto& second = m_lines.rbegin()->second;
    if (iter == second.second)
    {
        return second.first;
    }
    else
    {
        return m_lines.at(*iter).first;
    }
}

std::vector<OpenCL::Lexer::Token>::const_iterator
    OpenCL::Parser::Context::getLineEnd(std::vector<OpenCL::Lexer::Token>::const_iterator iter) const
{
    if (m_lines.empty())
    {
        return iter;
    }
    if (iter == m_lines.rbegin()->second.second)
    {
        return iter;
    }
    else
    {
        return m_lines.at(*iter).second;
    }
}

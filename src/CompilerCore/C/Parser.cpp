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
        if (this->m_branches.empty() || (this->m_branches.size() == 1 && this->m_branches.back().empty()))
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
        else if (this->m_branches.back().empty())
        {
            this->m_branches[this->m_branches.size() - 2].back()->m_messages.push_back(std::move(iter));
        }
        else
        {
            this->m_branches.back().back()->m_messages.push_back(std::move(iter));
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

std::unique_ptr<OpenCL::Parser::Context::Branch>
    OpenCL::Parser::Context::createBranch(Tokens::const_iterator& begin, Branch::CriteriaFunction&& criteria)
{
    return std::make_unique<Branch>(*this, begin, std::move(criteria));
}

std::size_t OpenCL::Parser::Context::getCurrentErrorCount() const
{
    if (!m_branches.empty())
    {
        if (!m_branches.back().empty())
        {
            return std::count_if(m_branches.back().back()->m_messages.begin(),
                                 m_branches.back().back()->m_messages.end(),
                                 [](const Message& message) { return message.getSeverity() == Message::Error; });
        }
    }
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
    if (iter == m_lines.rbegin()->second.second)
    {
        return iter;
    }
    else
    {
        return m_lines.at(*iter).second;
    }
}

OpenCL::Parser::Context::Branch::Branch(Context& context, std::vector<Lexer::Token>::const_iterator& begin,
                                        CriteriaFunction&& criteria)
    : context(context), m_begin(begin), m_curr(begin), m_criteria(std::move(criteria))
{
    context.m_branches.back().push_back(this);
}

std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Parser::Context::Branch::getCurrent()
{
    return m_curr;
}

OpenCL::Parser::Context::Branch::operator bool() const
{
    return std::none_of(m_messages.begin(), m_messages.end(),
                        [](const Message& message) { return message.getSeverity() == Message::Error; });
}

OpenCL::Parser::Context::Branch::~Branch()
{
    if (!context.m_branches.back().empty())
    {
        if (*this)
        {
            m_begin = m_curr;
            context.m_branches.back().clear();
        }
        else
        {
            Branch* result = nullptr;
            for (auto& iter : context.m_branches.back())
            {
                if (iter->m_criteria && iter->m_criteria(iter->m_begin, iter->m_curr))
                {
                    result = iter;
                    break;
                }
            }
            if (!result)
            {
                auto alternative = std::find_if(context.m_branches.back().begin(), context.m_branches.back().end(),
                                                [](const Branch* ptr) { return !ptr->m_criteria; });
                result =
                    alternative != context.m_branches.back().end() ? *alternative : context.m_branches.back().front();
            }
            context.m_branches.back().clear();
            m_begin = result->m_curr;
            context.log(std::move(result->m_messages));
        }
    }
}

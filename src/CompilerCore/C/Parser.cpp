#include "Parser.hpp"

#include <algorithm>

std::pair<OpenCL::Syntax::TranslationUnit, bool> OpenCL::Parser::buildTree(const std::vector<Lexer::Token>& tokens,
                                                                           std::ostream* reporter)
{
    ParsingContext context(reporter);
    auto begin = tokens.cbegin();
    return {parseTranslationUnit(begin, tokens.cend(), context), !context.isErrorsOccured()};
}

void OpenCL::Parser::ParsingContext::addTypedef(const std::string& name)
{
    m_typedefs.back().insert(name);
}

bool OpenCL::Parser::ParsingContext::isTypedef(const std::string& name) const
{
    for (auto& iter : m_typedefs)
    {
        if (iter.count(name))
        {
            return true;
        }
    }
    return false;
}

void OpenCL::Parser::ParsingContext::logError(std::string message,
                                              Tokens::const_iterator end,
                                              std::optional<Modifier> modifier,
                                              std::vector<Message::Note> notes)
{
    logImpl(Message(std::move(message), m_start.back(), end, std::move(modifier), std::move(notes)));
}

void OpenCL::Parser::ParsingContext::logImpl(Message&& error)
{
    if (this->m_branches.empty() || (this->m_branches.size() == 1 && this->m_branches.back().empty()))
    {
        this->m_errorsOccured = true;
        m_errorCount++;
        if (this->m_reporter)
        {
            *this->m_reporter << error;
        }
    }
    else if (this->m_branches.back().empty())
    {
        this->m_branches[this->m_branches.size() - 2].back()->m_errors.push_back(std::move(error));
    }
    else
    {
        this->m_branches.back().back()->m_errors.push_back(std::move(error));
    }
}

void OpenCL::Parser::ParsingContext::addToScope(std::string name, DeclarationLocation declarator)
{
    m_currentScope.back().emplace(std::move(name), declarator);
}

bool OpenCL::Parser::ParsingContext::isInScope(const std::string& name) const
{
    for (auto& iter : m_currentScope)
    {
        if (iter.count(name))
        {
            return true;
        }
    }
    return false;
}

void OpenCL::Parser::ParsingContext::pushScope()
{
    m_currentScope.emplace_back();
    m_typedefs.emplace_back();
}

void OpenCL::Parser::ParsingContext::popScope()
{
    m_currentScope.pop_back();
    m_typedefs.pop_back();
}

bool OpenCL::Parser::ParsingContext::isErrorsOccured() const
{
    return m_errorsOccured;
}

std::unique_ptr<OpenCL::Parser::ParsingContext::Branch> OpenCL::Parser::ParsingContext::createBranch(Tokens::const_iterator& begin,
                                                                                                     Branch::CriteriaFunction&& criteria)
{
    return std::make_unique<Branch>(*this, begin, std::move(criteria));
}

std::size_t OpenCL::Parser::ParsingContext::getCurrentErrorCount() const
{
    if (!m_branches.empty())
    {
        if (!m_branches.back().empty())
        {
            return m_branches.back().back()->m_errors.size();
        }
    }
    return m_errorCount;
}

const OpenCL::Parser::ParsingContext::DeclarationLocation* OpenCL::Parser::ParsingContext::getLocationOf(const std::string& name) const
{
    for (auto& iter : m_currentScope)
    {
        if (auto result = iter.find(name);result != iter.end())
        {
            return &result->second;
        }
    }
    return nullptr;
}

OpenCL::Parser::ParsingContext::Branch::Branch(ParsingContext& context,
                                               std::vector<Lexer::Token>::const_iterator& begin,
                                               CriteriaFunction&& criteria)
    : context(context), m_begin(begin), m_curr(begin), m_criteria(std::move(criteria))
{
    context.m_branches.back().push_back(this);
}

std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Parser::ParsingContext::Branch::getCurrent()
{
    return m_curr;
}

OpenCL::Parser::ParsingContext::Branch::operator bool() const
{
    return m_errors.empty();
}

OpenCL::Parser::ParsingContext::Branch::~Branch()
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
                auto alternative = std::find_if(context.m_branches.back().begin(),
                                                context.m_branches.back().end(),
                                                [](const Branch* ptr)
                                                {
                                                    return !ptr->m_criteria;
                                                });
                result = alternative != context.m_branches.back().end() ? *alternative : context.m_branches.back()
                                                                                                .front();
            }
            context.m_branches.back().clear();
            m_begin = result->m_curr;
            for (auto& iter : result->m_errors)
            {
                context.logImpl(std::move(iter));
            }
        }
    }
}

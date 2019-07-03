#include "Parser.hpp"

#include <algorithm>

std::pair<OpenCL::Syntax::TranslationUnit, bool> OpenCL::Parser::buildTree(const std::vector<Lexer::Token>& tokens,
                                                                           std::ostream* reporter)
{
    ParsingContext context(reporter);
    auto begin = tokens.cbegin();
    return {parseTranslationUnit(begin, tokens.cend(), context), !context.isErrorsOccured()};
}

void OpenCL::Parser::ParsingContext::addTypedef(const std::string& name, DeclarationLocation declarator)
{
    auto[iter, inserted] = m_currentScope.back().emplace(name, Declaration{declarator, true});
    if (!inserted && iter->second.isTypedef)
    {
        logError(ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + name + '\''),
                 declarator.end,
                 Modifier(declarator.identifier, declarator.identifier + 1, Modifier::Underline),
                 {{Notes::PREVIOUSLY_DECLARED_HERE, iter->second.location.begin, iter->second.location.end, Modifier(
                     iter->second.location.identifier,
                     iter->second.location.identifier + 1,
                     Modifier::Underline)}});
    }
}

bool OpenCL::Parser::ParsingContext::isTypedef(const std::string& name) const
{
    for (auto& iter : m_currentScope)
    {
        if (auto result = iter.find(name);result != iter.end() && result->second.isTypedef)
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

void OpenCL::Parser::ParsingContext::addToScope(const std::string& name, DeclarationLocation declarator)
{
    auto[iter, inserted] = m_currentScope.back().emplace(name, Declaration{declarator, false});
    if (!inserted && iter->second.isTypedef)
    {
        logError(ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + name + '\''),
                 declarator.end,
                 Modifier(declarator.identifier, declarator.identifier + 1, Modifier::Underline),
                 {{Notes::PREVIOUSLY_DECLARED_HERE, iter->second.location.begin, iter->second.location.end, Modifier(
                     iter->second.location.identifier,
                     iter->second.location.identifier + 1,
                     Modifier::Underline)}});
    }
}

void OpenCL::Parser::ParsingContext::pushScope()
{
    m_currentScope.emplace_back();
}

void OpenCL::Parser::ParsingContext::popScope()
{
    m_currentScope.pop_back();
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
    for (auto iter = m_currentScope.rbegin(); iter != m_currentScope.rend(); iter++)
    {
        if (auto result = iter->find(name);result != iter->end())
        {
            return &result->second.location;
        }
    }
    return nullptr;
}

bool OpenCL::Parser::ParsingContext::isTypedefInScope(const std::string& name) const
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

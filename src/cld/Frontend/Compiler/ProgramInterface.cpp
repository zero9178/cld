#include "ProgramInterface.hpp"

cld::Semantics::ProgramInterface::DeclContainer::DeclContainer(const cld::Semantics::ProgramInterface* interface,
                                                               cld::Semantics::ScopePoint from, std::size_t endScope)
    : m_interface(interface),
      m_startScope(from.getScopeId()),
      m_declCounts(std::move(from).getDeclCounts()),
      m_endScope(endScope)
{
    std::size_t distance = 0;
    {
        std::size_t currentScope = m_startScope;
        while (currentScope != m_endScope)
        {
            currentScope = m_interface->getScopes()[currentScope].previousScope;
            distance++;
        }
    }

    auto iter = m_declCounts.begin();
    for (; iter != m_declCounts.end() && *iter == 0 && m_startScope != m_endScope; iter++)
    {
        m_startScope = m_interface->getScopes()[m_startScope].previousScope;
    }

    while (m_endScope != END_OF_SCOPES && m_interface->getScopes()[m_endScope].declarations.size() == 0)
    {
        distance++;
        m_endScope = m_interface->getScopes()[m_endScope].previousScope;
    }

    std::size_t redundant = iter - m_declCounts.begin();
    m_declCounts.erase(m_declCounts.begin(), iter);
    m_declCounts.erase(m_declCounts.begin() + (distance - redundant + 1), m_declCounts.end());
}

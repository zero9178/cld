
#pragma once

#include <utility>

namespace cld
{
template <class F>
class ScopeExit
{
    F m_exit;
    bool m_engaged = true;

public:
    explicit ScopeExit(F&& t) : m_exit(std::move(t)) {}

    ~ScopeExit()
    {
        if (m_engaged)
        {
            m_exit();
        }
    }

    ScopeExit(const ScopeExit&) = delete;

    ScopeExit(ScopeExit&& rhs) noexcept : m_exit(std::move(rhs.m_exit)), m_engaged(std::exchange(rhs.m_engaged, false))
    {
    }

    ScopeExit& operator=(const ScopeExit&) = delete;

    ScopeExit& operator=(ScopeExit&& rhs) noexcept
    {
        m_exit = std::move(rhs.m_exit);
        m_engaged = std::exchange(rhs.m_engaged, false);
    }
};
} // namespace cld

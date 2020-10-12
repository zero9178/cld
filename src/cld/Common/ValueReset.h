#pragma once

#include <utility>

namespace cld
{
template <class T>
class ValueReset
{
    T m_valueAfter;
    T* m_assignedTo;

public:
    ValueReset(T& assignedTo, T valueAfter) : m_valueAfter(valueAfter), m_assignedTo(&assignedTo) {}

    ~ValueReset()
    {
        if (m_assignedTo)
        {
            *m_assignedTo = m_valueAfter;
        }
    }

    ValueReset(const ValueReset&) = delete;
    ValueReset& operator=(const ValueReset&) = delete;

    ValueReset(ValueReset&& rhs) noexcept
    {
        m_assignedTo = std::exchange(rhs.m_assignedTo, nullptr);
        m_valueAfter = rhs.m_valueAfter;
    }

    ValueReset& operator=(ValueReset&& rhs) noexcept
    {
        m_assignedTo = std::exchange(rhs.m_assignedTo, nullptr);
        m_valueAfter = rhs.m_valueAfter;
        return *this;
    }
};
} // namespace cld

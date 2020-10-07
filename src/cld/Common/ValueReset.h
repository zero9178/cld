#pragma once

namespace cld
{
template <class T>
class ValueReset
{
    T m_valueAfter;
    T& m_assignedTo;

public:
    ValueReset(T& assignedTo, T valueAfter) : m_valueAfter(valueAfter), m_assignedTo(assignedTo) {}

    ~ValueReset()
    {
        m_assignedTo = m_valueAfter;
    }

    ValueReset(const ValueReset&) = delete;
    ValueReset& operator=(const ValueReset&) = delete;
    ValueReset(ValueReset&&) = delete;
    ValueReset& operator=(ValueReset&&) = delete;
};
} // namespace cld

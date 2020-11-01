#pragma once

#include <array>

#include "Constexpr.hpp"
#include "Util.hpp"

namespace cld
{
template <class T, std::size_t N>
class MaxVector
{
    std::array<T, N> m_array = {};
    suitableUInt<N> m_size{};

public:
    constexpr MaxVector() = default;

    constexpr MaxVector(std::initializer_list<T> initializerList) noexcept : m_size(initializerList.size())
    {
        CLD_ASSERT(m_size <= N);
        for (std::size_t i = 0; i < m_size; i++)
        {
            m_array[i] = initializerList[i];
        }
    }

    template <std::size_t N2>
    constexpr MaxVector(const MaxVector<T, N2>& rhs) noexcept : m_size(rhs.size())
    {
        CLD_ASSERT(m_size <= N);
        for (std::size_t i = 0; i < m_size; i++)
        {
            m_array[i] = rhs[i];
        }
    }

    constexpr MaxVector& operator=(std::initializer_list<T> initializerList) noexcept
    {
        m_size = initializerList.size();
        CLD_ASSERT(m_size <= N);
        for (std::size_t i = 0; i < m_size; i++)
        {
            m_array[i] = initializerList[i];
        }
    }

    template <class Iter>
    constexpr MaxVector(Iter first, Iter last) : m_size(last - first)
    {
        CLD_ASSERT(m_size <= N);
        for (auto iter = first; iter != last; iter++)
        {
            m_array[iter - first] = *iter;
        }
    }

    [[nodiscard]] constexpr std::size_t size() const noexcept
    {
        return m_size;
    }

    [[nodiscard]] constexpr T* begin() noexcept
    {
        return m_array.data();
    }

    [[nodiscard]] constexpr const T* begin() const noexcept
    {
        return m_array.data();
    }

    [[nodiscard]] constexpr T* end() noexcept
    {
        return m_array.data() + m_size;
    }

    [[nodiscard]] constexpr const T* end() const noexcept
    {
        return m_array.data() + m_size;
    }

    [[nodiscard]] constexpr const T* cbegin() const noexcept
    {
        return begin();
    }

    [[nodiscard]] constexpr const T* cend() const noexcept
    {
        return end();
    }

    constexpr void resize(std::size_t size) noexcept
    {
        m_size = size;
        CLD_ASSERT(m_size <= N);
    }

    constexpr void resize(std::size_t size, const T& value) noexcept
    {
        for (std::size_t i = m_size; i < size; i++)
        {
            m_array[i] = value;
        }
        m_size = size;
        CLD_ASSERT(m_size <= N);
    }

    [[nodiscard]] constexpr bool empty() const noexcept
    {
        return m_size;
    }

    [[nodiscard]] constexpr T& operator[](std::size_t n) noexcept
    {
        CLD_ASSERT(n < m_size);
        return m_array[n];
    }

    [[nodiscard]] constexpr const T& operator[](std::size_t n) const noexcept
    {
        CLD_ASSERT(n < m_size);
        return m_array[n];
    }

    [[nodiscard]] constexpr T& at(std::size_t n) noexcept
    {
        CLD_ASSERT(n < m_size);
        return m_array[n];
    }

    [[nodiscard]] constexpr const T& at(std::size_t n) const noexcept
    {
        CLD_ASSERT(n < m_size);
        return m_array[n];
    }

    [[nodiscard]] constexpr T& front() noexcept
    {
        return at(0);
    }

    [[nodiscard]] constexpr const T& front() const noexcept
    {
        return at(0);
    }

    [[nodiscard]] constexpr T& back() noexcept
    {
        CLD_ASSERT(!empty());
        return at(m_size - 1);
    }

    [[nodiscard]] constexpr const T& back() const noexcept
    {
        CLD_ASSERT(!empty());
        return at(m_size - 1);
    }

    [[nodiscard]] constexpr T* data() noexcept
    {
        return m_array.data();
    }

    [[nodiscard]] constexpr const T* data() const noexcept
    {
        return m_array.data();
    }

    constexpr void push_back(const T& value) noexcept
    {
        m_array[m_size++] = value;
    }

    constexpr void push_back(T&& value) noexcept
    {
        m_array[m_size++] = std::move(value);
    }

    constexpr void pop_back() noexcept
    {
        m_size--;
    }
};
} // namespace cld

#pragma once

#include <variant>

#include "Util.hpp"

namespace cld
{
namespace detail
{
template <class T, std::uint8_t i, class First, class... Rest>
[[nodiscard]] constexpr std::uint8_t getIndex() noexcept
{
    if constexpr (std::is_same_v<T, First>)
    {
        return i;
    }
    else if constexpr (sizeof...(Rest) == 0)
    {
        CLD_UNREACHABLE;
    }
    else
    {
        return getIndex<T, i + 1, Rest...>();
    }
}

template <class T, std::uint8_t i, class Ret, class Curr, class... Rest>
[[nodiscard]] constexpr Ret cast(T* pointer, std::uint8_t index) noexcept
{
    if (index == i)
    {
        return static_cast<std::conditional_t<std::is_const_v<T>, const Curr*, Curr*>>(pointer);
    }
    else if constexpr (sizeof...(Rest) == 0)
    {
        CLD_UNREACHABLE;
    }
    else
    {
        return cast<T, i + 1, Ret, Rest...>(pointer, index);
    }
}
} // namespace detail

template <class... Args>
class MonadicInheritance
{
    std::uint8_t m_index;

public:
    template <class T>
    constexpr MonadicInheritance(std::in_place_type_t<T>)
        // Forces constant evaluation
        : m_index(std::integral_constant<std::uint8_t, detail::getIndex<T, 0, Args...>()>::value)
    {
        static_assert((std::is_same_v<T, Args> || ...), "T must be one of the subclasses specified in Args...");
    }

    [[nodiscard]] constexpr std::variant<const Args * CLD_NON_NULL...> getVariant() const&
    {
        return detail::cast<std::remove_reference_t<decltype(*this)>, 0, std::variant<const Args*...>, Args...>(
            this, m_index);
    }

    [[nodiscard]] constexpr std::variant<Args * CLD_NON_NULL...> getVariant() &&
    {
        return detail::cast<std::remove_reference_t<decltype(*this)>, 0, std::variant<Args*...>, Args...>(this,
                                                                                                          m_index);
    }

    [[nodiscard]] constexpr std::size_t index() const noexcept
    {
        return m_index;
    }

    template <class T>
    [[nodiscard]] constexpr bool is() const noexcept
    {
        constexpr auto var = detail::getIndex<T, 0, Args...>();
        return index() == var;
    }
};

} // namespace cld

#pragma once

#include <array>
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
        constexpr std::array<std::variant<const Args * CLD_NON_NULL...> (*)(decltype(this)), sizeof...(Args)>
            getterFuncs = {{+[](decltype(this) ptr) -> std::variant<const Args * CLD_NON_NULL...> {
                return static_cast<const Args*>(ptr);
            }...}};
        return getterFuncs[index()](this);
    }

    [[nodiscard]] constexpr std::variant<Args * CLD_NON_NULL...> getVariant() &&
    {
        constexpr std::array<std::variant<Args * CLD_NON_NULL...> (*)(decltype(this)), sizeof...(Args)> getterFuncs = {
            {+[](decltype(this) ptr) -> std::variant<Args * CLD_NON_NULL...> { return static_cast<Args*>(ptr); }...}};
        return getterFuncs[index()](this);
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

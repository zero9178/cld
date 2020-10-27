#pragma once

#include <array>
#include <variant>

#include "Constexpr.hpp"
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
        static_assert(always_false<T>, "Template type was not specified in the sets of possible base classes");
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

    template <class T>
    [[nodiscard]] constexpr const T& get() const noexcept
    {
        CLD_ASSERT(index() == detail::getIndex<T, 0, Args...>());
        return *static_cast<const T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr T& get() noexcept
    {
        CLD_ASSERT(index() == detail::getIndex<T, 0, Args...>());
        return *static_cast<T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr const T* get_if() const noexcept
    {
        if (index() != detail::getIndex<T, 0, Args...>())
        {
            return nullptr;
        }
        return static_cast<const T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr T* get_if() noexcept
    {
        if (index() != detail::getIndex<T, 0, Args...>())
        {
            return nullptr;
        }
        return static_cast<T*>(this);
    }

    template <class F>
    decltype(auto) match(F&& f)
    {
        constexpr std::array<std::common_type_t<decltype(f(std::declval<Args>()))...> (*)(MonadicInheritance&, F&),
                             sizeof...(Args)>
            calling = {
                {+[](MonadicInheritance& base, F& f) -> decltype(auto) { return f(static_cast<Args&>(base)); }...}};
        return calling[index()](*this, f);
    }

    template <class F>
    decltype(auto) match(F&& f) const
    {
        constexpr std::array<std::common_type_t<decltype(f(std::declval<Args>()))...> (*)(const MonadicInheritance&,
                                                                                          F&),
                             sizeof...(Args)>
            calling = {{+[](const MonadicInheritance& base, F& f) -> decltype(auto) {
                return f(static_cast<const Args&>(base));
            }...}};
        return calling[index()](*this, f);
    }

    template <class T = bool>
    [[nodiscard]] friend std::enable_if_t<(always_true<T> && ... && cld::IsEqualComparable<Args, Args>{}), T>
        operator==(const MonadicInheritance& lhs, const MonadicInheritance& rhs) noexcept
    {
        if (lhs.index() != rhs.index())
        {
            return false;
        }
        constexpr std::array<bool (*)(const MonadicInheritance&, const MonadicInheritance&), sizeof...(Args)> equality =
            {{+[](const MonadicInheritance& lhs, const MonadicInheritance& rhs) -> bool {
                return static_cast<const Args&>(lhs) == static_cast<const Args&>(rhs);
            }...}};
        return equality[lhs.index()](lhs, rhs);
    }

    template <class T = bool>
    [[nodiscard]] friend std::enable_if_t<(always_true<T> && ... && cld::IsEqualComparable<Args, Args>{}), T>
        operator!=(const MonadicInheritance& lhs, const MonadicInheritance& rhs) noexcept
    {
        return !(lhs == rhs);
    }
};

} // namespace cld

#pragma once

#include <array>
#include <memory>

#include "Constexpr.hpp"
#include "Util.hpp"

namespace cld
{
namespace detail
{
template <class T, std::size_t i, class First, class... Rest>
[[nodiscard]] constexpr std::size_t getIndex() noexcept
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
class AbstractIntrusiveVariant
{
    template <class F, class First, class... Rest>
    static decltype(auto) matchHelper(F&& f, First&& first, Rest&&... rest)
    {
        return f(std::forward<First>(first));
        if constexpr (sizeof...(Rest) > 0)
        {
            return matchHelper(f, std::forward<Rest>(rest)...);
        }
    }

    template <class F>
    using MatchReturn = decltype(matchHelper(std::declval<F>(), std::declval<Args>()...));

    cld::suitableUInt<sizeof...(Args) - 1> m_index;

public:
    template <class T>
    constexpr AbstractIntrusiveVariant(std::in_place_type_t<T>)
        // Forces constant evaluation
        : m_index(std::integral_constant<std::uint8_t, detail::getIndex<T, 0, Args...>()>::value)
    {
        static_assert((std::is_same_v<T, Args> || ...), "T must be one of the subclasses specified in Args...");
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
    [[nodiscard]] constexpr const T& cast() const noexcept
    {
        CLD_ASSERT(index() == detail::getIndex<T, 0, Args...>());
        return *static_cast<const T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr T& cast() noexcept
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

    template <class... F>
    decltype(auto) match(F&&... f)
    {
        using Callable = decltype(detail::overload{std::forward<F>(f)...});
        constexpr std::array<MatchReturn<Callable> (*)(AbstractIntrusiveVariant&, Callable &&), sizeof...(Args)>
            calling = {{+[](AbstractIntrusiveVariant& base, Callable&& callable) -> decltype(auto) {
                return callable(static_cast<Args&>(base));
            }...}};
        return calling[index()](*this, Callable{std::forward<F>(f)...});
    }

    template <class... F>
    decltype(auto) match(F&&... f) const
    {
        using Callable = decltype(detail::overload{std::forward<F>(f)...});
        constexpr std::array<MatchReturn<Callable> (*)(const AbstractIntrusiveVariant&, Callable&&), sizeof...(Args)>
            calling = {{+[](const AbstractIntrusiveVariant& base, Callable&& callable) -> decltype(auto) {
                return callable(static_cast<const Args&>(base));
            }...}};
        return calling[index()](*this, Callable{std::forward<F>(f)...});
    }

    template <class T = bool>
    [[nodiscard]] friend std::enable_if_t<(always_true<T> && ... && cld::IsEqualComparable<Args, Args>{}), T>
        operator==(const AbstractIntrusiveVariant& lhs, const AbstractIntrusiveVariant& rhs) noexcept
    {
        if (lhs.index() != rhs.index())
        {
            return false;
        }
        constexpr std::array<bool (*)(const AbstractIntrusiveVariant&, const AbstractIntrusiveVariant&),
                             sizeof...(Args)>
            equality = {{+[](const AbstractIntrusiveVariant& lhs, const AbstractIntrusiveVariant& rhs) -> bool {
                return static_cast<const Args&>(lhs) == static_cast<const Args&>(rhs);
            }...}};
        return equality[lhs.index()](lhs, rhs);
    }

    template <class T = bool>
    [[nodiscard]] friend std::enable_if_t<(always_true<T> && ... && cld::IsEqualComparable<Args, Args>{}), T>
        operator!=(const AbstractIntrusiveVariant& lhs, const AbstractIntrusiveVariant& rhs) noexcept
    {
        return !(lhs == rhs);
    }
};

namespace detail::AbstractIntrusiveVariant
{
template <class... Args>
auto deduceArgs(::cld::AbstractIntrusiveVariant<Args...>*) -> ::cld::AbstractIntrusiveVariant<Args...>;
} // namespace detail::AbstractIntrusiveVariant

template <class T, class U = decltype(detail::AbstractIntrusiveVariant::deduceArgs(std::declval<T*>()))>
class IntrusiveVariantDeleter
{
    static_assert(always_false<T>, "Can't delete pointer that isn't a subclass of AbstractIntrusiveVariant");
};

template <class Base, class... SubClasses>
class IntrusiveVariantDeleter<Base, AbstractIntrusiveVariant<SubClasses...>>
{
public:
    IntrusiveVariantDeleter() = default;

    template <class T, std::enable_if_t<std::disjunction_v<std::is_same<T, SubClasses>...>>* = nullptr>
    IntrusiveVariantDeleter(std::default_delete<T>&&) noexcept
    {
    }

    template <class U, std::enable_if_t<std::is_base_of_v<Base, U>>* = nullptr>
    IntrusiveVariantDeleter(IntrusiveVariantDeleter<U>&&) noexcept
    {
    }

    void operator()(Base* pointer) const noexcept
    {
        constexpr std::array<void (*)(Base*), sizeof...(SubClasses)> deleteFuncs = {
            {+[](Base* ptr)
             {
                 if constexpr (std::is_base_of_v<Base, SubClasses>)
                 {
                     delete static_cast<SubClasses*>(ptr);
                 }
                 else
                 {
                     CLD_UNREACHABLE;
                 }
             }...}};
        deleteFuncs[pointer->index()](pointer);
    }
};

template <class T>
using IntrVarPtr = std::unique_ptr<T, ::cld::IntrusiveVariantDeleter<T>>;

} // namespace cld

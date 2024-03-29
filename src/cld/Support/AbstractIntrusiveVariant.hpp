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

template <class Base, class... Args>
class AbstractIntrusiveVariant
{
    cld::suitableUInt<sizeof...(Args) - 1> m_index;

    template <class First, class...>
    static First first()
    {
        CLD_UNREACHABLE;
    }

public:
    template <class T>
    constexpr AbstractIntrusiveVariant(std::in_place_type_t<T>)
        : m_index(std::integral_constant<index_type, indexOf<T>()>::value)
    {
        static_assert((std::is_same_v<T, Args> || ...), "T must be one of the subclasses specified in Args...");
    }

    using index_type = decltype(m_index);
    using base_type = Base;

    template <class T>
    constexpr static index_type indexOf() noexcept
    {
        return detail::getIndex<T, 0, Args...>();
    }

    [[nodiscard]] constexpr index_type index() const noexcept
    {
        return m_index;
    }

    template <class T>
    [[nodiscard]] constexpr bool is() const noexcept
    {
        constexpr auto var = indexOf<T>();
        return index() == var;
    }

    template <class T>
    [[nodiscard]] constexpr const T& as() const noexcept
    {
        CLD_ASSERT(index() == std::integral_constant<index_type, indexOf<T>()>::value);
        return *static_cast<const T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr T& as() noexcept
    {
        CLD_ASSERT(index() == std::integral_constant<index_type, indexOf<T>()>::value);
        return *static_cast<T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr const T* tryAs() const noexcept
    {
        constexpr auto var = indexOf<T>();
        if (index() != var)
        {
            return nullptr;
        }
        return static_cast<const T*>(this);
    }

    template <class T>
    [[nodiscard]] constexpr T* tryAs() noexcept
    {
        constexpr auto var = indexOf<T>();
        if (index() != var)
        {
            return nullptr;
        }
        return static_cast<T*>(this);
    }

    template <class... F>
    decltype(auto) match(F&&... f)
    {
        using Callable = decltype(detail::overload{std::forward<F>(f)...});
        constexpr std::array<std::invoke_result_t<Callable, decltype(first<Args...>())&> (*)(AbstractIntrusiveVariant&,
                                                                                             Callable &&),
                             sizeof...(Args)>
            calling = {{+[](AbstractIntrusiveVariant& base, Callable&& callable) -> decltype(auto)
                        { return callable(static_cast<Args&>(base)); }...}};
        return calling[index()](*this, Callable{std::forward<F>(f)...});
    }

    template <class... F>
    decltype(auto) match(F&&... f) const
    {
        using Callable = decltype(detail::overload{std::forward<F>(f)...});
        constexpr std::array<std::invoke_result_t<Callable, const decltype(first<Args...>())&> (*)(
                                 const AbstractIntrusiveVariant&, Callable&&),
                             sizeof...(Args)>
            calling = {{+[](const AbstractIntrusiveVariant& base, Callable&& callable) -> decltype(auto)
                        { return callable(static_cast<const Args&>(base)); }...}};
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
            equality = {{+[](const AbstractIntrusiveVariant& lhs, const AbstractIntrusiveVariant& rhs) -> bool
                         { return static_cast<const Args&>(lhs) == static_cast<const Args&>(rhs); }...}};
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
template <class Base, class... Args>
auto deduceArgs(::cld::AbstractIntrusiveVariant<Base, Args...>*) -> ::cld::AbstractIntrusiveVariant<Base, Args...>;
} // namespace detail::AbstractIntrusiveVariant

template <class T, class U = decltype(detail::AbstractIntrusiveVariant::deduceArgs(std::declval<T*>()))>
class IntrusiveVariantDeleter
{
    static_assert(always_false<T>, "Can't delete pointer that isn't a subclass of AbstractIntrusiveVariant");
};

template <class Base, class Top, class... SubClasses>
class IntrusiveVariantDeleter<Base, AbstractIntrusiveVariant<Top, SubClasses...>>
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

#define CLD_GEN_INTR_VAR_WARNING_FUNCS(ThisClass)                                                             \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Function always returns false")                                                        \
    constexpr CLD_ALWAYS_INLINE std::enable_if_t<!std::is_same_v<T, ThisClass>, bool> is() const noexcept     \
    {                                                                                                         \
        return false;                                                                                         \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Function always returns true")                                                         \
    constexpr CLD_ALWAYS_INLINE std::enable_if_t<std::is_same_v<T, ThisClass>, bool> is() const noexcept      \
    {                                                                                                         \
        return true;                                                                                          \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_ERROR_ON_USAGE("Cast will never succeed")                                                             \
    CLD_ALWAYS_INLINE std::enable_if_t<!std::is_same_v<T, ThisClass>, T&> as() noexcept                       \
    {                                                                                                         \
        return AbstractIntrusiveVariant::as<T>();                                                             \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_ERROR_ON_USAGE("Cast will never succeed")                                                             \
    CLD_ALWAYS_INLINE std::enable_if_t<!std::is_same_v<T, ThisClass>, const T&> as() const noexcept           \
    {                                                                                                         \
        return AbstractIntrusiveVariant::as<T>();                                                             \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Class is already of type " #ThisClass)                                                 \
    CLD_ALWAYS_INLINE std::enable_if_t<std::is_same_v<T, ThisClass>, ThisClass&> as() noexcept                \
    {                                                                                                         \
        return *this;                                                                                         \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Class is already of type " #ThisClass)                                                 \
    CLD_ALWAYS_INLINE std::enable_if_t<std::is_same_v<T, ThisClass>, const ThisClass&> as() const noexcept    \
    {                                                                                                         \
        return *this;                                                                                         \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Function will always return nullptr")                                                  \
    CLD_ALWAYS_INLINE std::enable_if_t<!std::is_same_v<T, ThisClass>, T*> tryAs() noexcept                    \
    {                                                                                                         \
        return nullptr;                                                                                       \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Function will always return nullptr")                                                  \
    CLD_ALWAYS_INLINE std::enable_if_t<!std::is_same_v<T, ThisClass>, const T*> tryAs() const noexcept        \
    {                                                                                                         \
        return nullptr;                                                                                       \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Class is already of type " #ThisClass)                                                 \
    CLD_ALWAYS_INLINE std::enable_if_t<std::is_same_v<T, ThisClass>, ThisClass*> tryAs() noexcept             \
    {                                                                                                         \
        return this;                                                                                          \
    }                                                                                                         \
                                                                                                              \
    template <class T>                                                                                        \
    CLD_WARN_ON_USAGE("Class is already of type " #ThisClass)                                                 \
    CLD_ALWAYS_INLINE std::enable_if_t<std::is_same_v<T, ThisClass>, const ThisClass*> tryAs() const noexcept \
    {                                                                                                         \
        return this;                                                                                          \
    }                                                                                                         \
                                                                                                              \
    template <class... F>                                                                                     \
    CLD_WARN_ON_USAGE("Class type " #ThisClass " known ahead of time already")                                \
    CLD_ALWAYS_INLINE decltype(auto) match(F&&... f)                                                          \
    {                                                                                                         \
        return AbstractIntrusiveVariant::match(std::forward<F>(f)...);                                        \
    }                                                                                                         \
                                                                                                              \
    template <class... F>                                                                                     \
    CLD_WARN_ON_USAGE("Class type " #ThisClass " known ahead of time already")                                \
    CLD_ALWAYS_INLINE decltype(auto) match(F&&... f) const                                                    \
    {                                                                                                         \
        return AbstractIntrusiveVariant::match(std::forward<F>(f)...);                                        \
    }

#pragma once

#include "Constexpr.hpp"
#include "MonadicInheritance.h"

namespace cld
{
namespace detail
{
template <class... Args>
struct DeduceArgs
{
    MonadicInheritance<Args...>* p;

    using type = MonadicInheritance<Args...>;
};

template <class... Args>
DeduceArgs(MonadicInheritance<Args...>*) -> DeduceArgs<Args...>;
} // namespace detail

template <class T, class U = typename decltype(detail::DeduceArgs{std::declval<T*>()})::type>
class MonadicStorage
{
    static_assert(always_false<T>, "MonadicStorage can only store a class inheriting from MonadicInheritance");
};

template <class Base, class... SubClasses>
class MonadicStorage<Base, MonadicInheritance<SubClasses...>>
{
    alignas(SubClasses...) std::byte m_storage[std::max({sizeof(SubClasses)...})];

public:
    template <class T>
    MonadicStorage(T&& value) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>, T&&>)
    {
        static_assert((std::is_same_v<std::decay_t<T>, SubClasses> || ...));
        new (m_storage) std::decay_t<T>(std::forward<T>(value));
    }

    template <class T, class... Args>
    MonadicStorage(std::in_place_type_t<T>, Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>)
    {
        static_assert((std::is_same_v<T, SubClasses> || ...));
        new (m_storage) T(std::forward<Args>(args)...);
    }

    ~MonadicStorage()
    {
        constexpr std::array<void (*)(void*), sizeof...(SubClasses)> destructorFuncs = {
            {*[](void* ptr) { reinterpret_cast<SubClasses*>(ptr)->~SubClasses(); }...}};
        destructorFuncs[get().index()](m_storage);
    }

    operator Base&() noexcept
    {
        return get();
    }

    operator const Base&() const noexcept
    {
        return get();
    }

    [[nodiscard]] Base& get() noexcept
    {
        return *reinterpret_cast<Base*>(m_storage);
    }

    [[nodiscard]] const Base& get() const noexcept
    {
        return *reinterpret_cast<const Base*>(m_storage);
    }
};

} // namespace cld

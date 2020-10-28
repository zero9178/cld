#pragma once

#include <cstddef>
#include <variant>

#include "Constexpr.hpp"
#include "MonadicInheritance.h"

namespace cld
{
namespace detail::MonadicStorage
{
template <class... Args>
struct DeduceArgs
{
    MonadicInheritance<Args...>* p;

    using type = MonadicInheritance<Args...>;
};

template <class... Args>
DeduceArgs(MonadicInheritance<Args...>*) -> DeduceArgs<Args...>;

template <class Base, class... SubClasses>
class MonadicStorageBase
{
protected:
    alignas(SubClasses...) std::byte m_storage[std::max({sizeof(SubClasses)...})];

    [[nodiscard]] Base& get() noexcept
    {
        return *reinterpret_cast<Base*>(this->m_storage);
    }

    [[nodiscard]] const Base& get() const noexcept
    {
        return *reinterpret_cast<const Base*>(this->m_storage);
    }
};

enum class SpecialMem
{
    Trivial,
    Exists,
    Deleted
};

template <class Base, SpecialMem status, class... SubClasses>
struct MonadicStorageDestruct;

template <class Base, class... SubClasses>
struct MonadicStorageDestruct<Base, SpecialMem::Trivial, SubClasses...> : MonadicStorageBase<Base, SubClasses...>
{
    MonadicStorageDestruct() = default;

    ~MonadicStorageDestruct() = default;

    MonadicStorageDestruct(const MonadicStorageDestruct&) = default;
    MonadicStorageDestruct& operator=(const MonadicStorageDestruct&) = default;
    MonadicStorageDestruct(MonadicStorageDestruct&&) noexcept = default;
    MonadicStorageDestruct& operator=(MonadicStorageDestruct&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageDestruct<Base, SpecialMem::Exists, SubClasses...> : MonadicStorageBase<Base, SubClasses...>
{
protected:
    void destruct()
    {
        constexpr std::array<void (*)(void*), sizeof...(SubClasses)> destructorFuncs = {
            {*[](void* ptr) { reinterpret_cast<SubClasses*>(ptr)->~SubClasses(); }...}};
        destructorFuncs[this->get().index()](this->m_storage);
    }

public:
    MonadicStorageDestruct() = default;

    ~MonadicStorageDestruct() noexcept((std::is_nothrow_destructible_v<SubClasses> && ...))
    {
        destruct();
    }

    MonadicStorageDestruct(const MonadicStorageDestruct&) = default;
    MonadicStorageDestruct& operator=(const MonadicStorageDestruct&) = default;
    MonadicStorageDestruct(MonadicStorageDestruct&&) noexcept = default;
    MonadicStorageDestruct& operator=(MonadicStorageDestruct&&) noexcept = default;
};

template <class T>
constexpr SpecialMem dtor()
{
    return std::is_trivially_destructible_v<T> ? SpecialMem::Trivial : SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct MonadicStorageCopy;

template <class Base, class... SubClasses>
struct MonadicStorageCopy<Base, SpecialMem::Trivial, SubClasses...>
    : MonadicStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const MonadicStorageCopy&) noexcept {}

public:
    MonadicStorageCopy() = default;

    MonadicStorageCopy(const MonadicStorageCopy&) noexcept = default;

    MonadicStorageCopy& operator=(const MonadicStorageCopy&) noexcept = default;
    MonadicStorageCopy(MonadicStorageCopy&&) noexcept = default;
    MonadicStorageCopy& operator=(MonadicStorageCopy&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageCopy<Base, SpecialMem::Deleted, SubClasses...>
    : MonadicStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const MonadicStorageCopy&) noexcept {}

public:
    MonadicStorageCopy() = default;

    MonadicStorageCopy(const MonadicStorageCopy&) noexcept = delete;

    MonadicStorageCopy& operator=(const MonadicStorageCopy&) noexcept = default;
    MonadicStorageCopy(MonadicStorageCopy&&) noexcept = default;
    MonadicStorageCopy& operator=(MonadicStorageCopy&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageCopy<Base, SpecialMem::Exists, SubClasses...>
    : MonadicStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void
        copyConstruct(const MonadicStorageCopy& rhs) noexcept((std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        constexpr std::array<void (*)(std::byte*, void*), sizeof...(SubClasses)> copyFuncs = {
            {*[](std::byte* storage, void* ptr) { new (storage) SubClasses(*reinterpret_cast<SubClasses*>(ptr)); }...}};
        copyFuncs[rhs.get().index()](this->m_storage, rhs.m_storage);
    }

public:
    MonadicStorageCopy() = default;

    MonadicStorageCopy(const MonadicStorageCopy& rhs) noexcept(
        (std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        copyConstruct(rhs);
    }

    MonadicStorageCopy& operator=(const MonadicStorageCopy&) = default;

    MonadicStorageCopy(MonadicStorageCopy&&) noexcept = default;
    MonadicStorageCopy& operator=(MonadicStorageCopy&&) noexcept = default;
};

template <class T>
constexpr SpecialMem copyCtor()
{
    if constexpr (!std::is_copy_constructible_v<T>)
    {
        return SpecialMem::Deleted;
    }
    else if constexpr (std::is_trivially_copy_constructible_v<T>)
    {
        return SpecialMem::Trivial;
    }
    return SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct MonadicStorageCopyAss;

template <class Base, class... SubClasses>
struct MonadicStorageCopyAss<Base, SpecialMem::Deleted, SubClasses...>
    : MonadicStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    MonadicStorageCopyAss() = default;

    MonadicStorageCopyAss& operator=(const MonadicStorageCopyAss&) noexcept = delete;

    ~MonadicStorageCopyAss() = default;
    MonadicStorageCopyAss(const MonadicStorageCopyAss&) noexcept = default;
    MonadicStorageCopyAss(MonadicStorageCopyAss&&) noexcept = default;
    MonadicStorageCopyAss& operator=(MonadicStorageCopyAss&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageCopyAss<Base, SpecialMem::Exists, SubClasses...>
    : MonadicStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    MonadicStorageCopyAss() = default;

    MonadicStorageCopyAss&
        operator=(const MonadicStorageCopyAss& rhs) noexcept((std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        if (this->get().index() == rhs.get().index())
        {
            constexpr std::array<void (*)(std::byte*, void*), sizeof...(SubClasses)> copyFuncs = {
                {*[](std::byte* storage, void* ptr) {
                    *reinterpret_cast<SubClasses*>(storage) = *(reinterpret_cast<SubClasses*>(ptr));
                }...}};
            copyFuncs[rhs.get().index()](this->m_storage, rhs.m_storage);
        }
        else
        {
            if constexpr (!(std::is_trivially_destructible_v<SubClasses> && ...))
            {
                this->destruct();
            }
            this->copyConstruct(rhs);
        }
        return *this;
    }

    ~MonadicStorageCopyAss() = default;
    MonadicStorageCopyAss(const MonadicStorageCopyAss&) noexcept = default;
    MonadicStorageCopyAss(MonadicStorageCopyAss&&) noexcept = default;
    MonadicStorageCopyAss& operator=(MonadicStorageCopyAss&&) noexcept = default;
};

template <class T>
constexpr SpecialMem copyAss()
{
    if constexpr (!std::is_copy_assignable_v<T> || !std::is_copy_constructible_v<T>)
    {
        return SpecialMem::Deleted;
    }
    return SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct MonadicStorageMove;

template <class Base, class... SubClasses>
struct MonadicStorageMove<Base, SpecialMem::Trivial, SubClasses...>
    : MonadicStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(MonadicStorageMove&&) noexcept {}

public:
    MonadicStorageMove() = default;

    MonadicStorageMove(const MonadicStorageMove&) noexcept = default;
    MonadicStorageMove& operator=(const MonadicStorageMove&) noexcept = default;
    MonadicStorageMove(MonadicStorageMove&&) noexcept = default;
    MonadicStorageMove& operator=(MonadicStorageMove&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageMove<Base, SpecialMem::Deleted, SubClasses...>
    : MonadicStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(MonadicStorageMove&&) noexcept {}

public:
    MonadicStorageMove() = default;

    MonadicStorageMove(MonadicStorageMove&&) = default;

    MonadicStorageMove& operator=(const MonadicStorageMove&) noexcept = default;
    MonadicStorageMove(const MonadicStorageMove&) noexcept = default;
    MonadicStorageMove& operator=(MonadicStorageMove&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageMove<Base, SpecialMem::Exists, SubClasses...>
    : MonadicStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(MonadicStorageMove&& rhs) noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
    {
        constexpr std::array<void (*)(std::byte*, void*), sizeof...(SubClasses)> moveFuncs = {
            {*[](std::byte* storage, void* ptr) {
                new (storage) SubClasses(std::move(*reinterpret_cast<SubClasses*>(ptr)));
            }...}};
        moveFuncs[rhs.get().index()](this->m_storage, rhs.m_storage);
    }

public:
    MonadicStorageMove() = default;

    MonadicStorageMove(MonadicStorageMove&& rhs) noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
    {
        moveConstruct(std::move(rhs));
    }

    MonadicStorageMove(const MonadicStorageMove&) noexcept = default;
    MonadicStorageMove& operator=(const MonadicStorageMove&) = default;
    MonadicStorageMove& operator=(MonadicStorageMove&&) noexcept = default;
};

template <class T>
constexpr SpecialMem moveCtor()
{
    if constexpr (!std::is_move_constructible_v<T>)
    {
        return SpecialMem::Deleted;
    }
    else if constexpr (std::is_trivially_move_constructible_v<T>)
    {
        return SpecialMem::Trivial;
    }
    return SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct MonadicStorageMoveAss;

template <class Base, class... SubClasses>
struct MonadicStorageMoveAss<Base, SpecialMem::Deleted, SubClasses...>
    : MonadicStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    MonadicStorageMoveAss() = default;

    MonadicStorageMoveAss& operator=(const MonadicStorageMoveAss&) noexcept = delete;

    ~MonadicStorageMoveAss() = default;
    MonadicStorageMoveAss(const MonadicStorageMoveAss&) noexcept = default;
    MonadicStorageMoveAss(MonadicStorageMoveAss&&) noexcept = default;
    MonadicStorageMoveAss& operator=(MonadicStorageMoveAss&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct MonadicStorageMoveAss<Base, SpecialMem::Exists, SubClasses...>
    : MonadicStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    MonadicStorageMoveAss() = default;

    MonadicStorageMoveAss&
        operator=(MonadicStorageMoveAss&& rhs) noexcept((std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        if (this->get().index() == rhs.get().index())
        {
            constexpr std::array<void (*)(std::byte*, void*), sizeof...(SubClasses)> moveFuncs = {
                {*[](std::byte* storage, void* ptr) {
                    *reinterpret_cast<SubClasses*>(storage) = std::move(*reinterpret_cast<SubClasses*>(ptr));
                }...}};
            moveFuncs[rhs.get().index()](this->m_storage, rhs.m_storage);
        }
        else
        {
            if constexpr (!(std::is_trivially_destructible_v<SubClasses> && ...))
            {
                this->destruct();
            }
            this->moveConstruct(std::move(rhs));
        }
        return *this;
    }

    ~MonadicStorageMoveAss() = default;
    MonadicStorageMoveAss(const MonadicStorageMoveAss&) noexcept = default;
    MonadicStorageMoveAss(MonadicStorageMoveAss&&) noexcept = default;
    MonadicStorageMoveAss& operator=(const MonadicStorageMoveAss&) noexcept = default;
};

template <class T>
constexpr SpecialMem moveAss()
{
    if constexpr (!std::is_move_assignable_v<T> || !std::is_move_constructible_v<T>)
    {
        return SpecialMem::Deleted;
    }
    return SpecialMem::Exists;
}

template <class T>
struct isInPlaceT
{
    constexpr static bool value = false;
};

template <class T>
struct isInPlaceT<std::in_place_type_t<T>>
{
    constexpr static bool value = true;
};
} // namespace detail::MonadicStorage

template <class T, class U = typename decltype(detail::MonadicStorage::DeduceArgs{std::declval<T*>()})::type>
class MonadicStorage
{
    static_assert(always_false<T>, "MonadicStorage can only store a class inheriting from MonadicInheritance");
};

template <class Base, class... SubClasses>
class MonadicStorage<Base, MonadicInheritance<SubClasses...>>
    : public detail::MonadicStorage::MonadicStorageMoveAss<
          Base, std::max({detail::MonadicStorage::moveAss<SubClasses>()...}), SubClasses...>
{
    using detail::MonadicStorage::MonadicStorageBase<Base, SubClasses...>::get;

public:
    template <class T, std::enable_if_t<(std::is_same_v<std::decay_t<T>, SubClasses> || ...)>* = nullptr>
    MonadicStorage(T&& value) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>, T&&>)
    {
        static_assert((std::is_same_v<std::decay_t<T>, SubClasses> || ...));
        new (this->m_storage) std::decay_t<T>(std::forward<T>(value));
    }

    template <class T, class... Args>
    MonadicStorage(std::in_place_type_t<T>, Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>)
    {
        static_assert((std::is_same_v<T, SubClasses> || ...));
        new (this->m_storage) T(std::forward<Args>(args)...);
    }

    template <class T, class... Args>
    T& emplace(Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>)
    {
        *this = MonadicStorage(std::in_place_type<T>, std::forward<Args>(args)...);
        return *reinterpret_cast<T*>(this->m_storage);
    }

    operator Base&() noexcept
    {
        return get();
    }

    operator const Base&() const noexcept
    {
        return get();
    }

    Base* CLD_NON_NULL operator->() noexcept
    {
        return &get();
    }

    const Base* CLD_NON_NULL operator->() const noexcept
    {
        return &get();
    }

    Base& operator*() noexcept
    {
        return get();
    }

    const Base& operator*() const noexcept
    {
        return get();
    }

    std::unique_ptr<Base> toUniquePtr() && noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
    {
        return get().match([](auto&& value) -> std::unique_ptr<Base> {
            return std::make_unique<std::decay_t<decltype(value)>>(std::move(value));
        });
    }
};

} // namespace cld

#pragma once

#include <algorithm>
#include <cstddef>
#include <variant>

#include "AbstractIntrusiveVariant.h"
#include "Constexpr.hpp"

namespace cld
{
namespace detail::InstrusiveVariantStorage
{
template <class... Args>
auto deduceArgs(::cld::AbstractIntrusiveVariant<Args...>*) -> ::cld::AbstractIntrusiveVariant<Args...>;

template <class Base, class... SubClasses>
class InstrusiveVariantStorageBase
{
protected:
    alignas(SubClasses...) std::byte m_storage[std::max({sizeof(SubClasses)...})];

    [[nodiscard]] Base& get()
    {
        return *reinterpret_cast<Base*>(this->m_storage);
    }

    [[nodiscard]] const Base& get() const
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
struct InstrusiveVariantStorageDestruct;

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageDestruct<Base, SpecialMem::Trivial, SubClasses...>
    : InstrusiveVariantStorageBase<Base, SubClasses...>
{
protected:
    void destruct() {}

public:
    InstrusiveVariantStorageDestruct() = default;

    ~InstrusiveVariantStorageDestruct() = default;

    InstrusiveVariantStorageDestruct(const InstrusiveVariantStorageDestruct&) = default;
    InstrusiveVariantStorageDestruct& operator=(const InstrusiveVariantStorageDestruct&) = default;
    InstrusiveVariantStorageDestruct(InstrusiveVariantStorageDestruct&&) = default;
    InstrusiveVariantStorageDestruct& operator=(InstrusiveVariantStorageDestruct&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageDestruct<Base, SpecialMem::Exists, SubClasses...>
    : InstrusiveVariantStorageBase<Base, SubClasses...>
{
protected:
    void destruct()
    {
        constexpr std::array<void (*)(Base*), sizeof...(SubClasses)> destructorFuncs = {
            {+[](Base* ptr) { std::destroy_at(static_cast<SubClasses*>(ptr)); }...}};
        destructorFuncs[this->get().index()](&this->get());
    }

public:
    InstrusiveVariantStorageDestruct() = default;

    ~InstrusiveVariantStorageDestruct()
    {
        destruct();
    }

    InstrusiveVariantStorageDestruct(const InstrusiveVariantStorageDestruct&) = default;
    InstrusiveVariantStorageDestruct& operator=(const InstrusiveVariantStorageDestruct&) = default;
    InstrusiveVariantStorageDestruct(InstrusiveVariantStorageDestruct&&) = default;
    InstrusiveVariantStorageDestruct& operator=(InstrusiveVariantStorageDestruct&&) = default;
};

template <class T>
constexpr SpecialMem dtor()
{
    return std::is_trivially_destructible_v<T> ? SpecialMem::Trivial : SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct InstrusiveVariantStorageCopy;

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageCopy<Base, SpecialMem::Trivial, SubClasses...>
    : InstrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const InstrusiveVariantStorageCopy& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> copyFuncs = {
            {+[](std::byte* storage, Base* ptr) { new (storage) SubClasses(*static_cast<SubClasses*>(ptr)); }...}};
        copyFuncs[rhs.cast().index()](this->m_storage, &rhs.get());
    }

public:
    InstrusiveVariantStorageCopy() = default;

    InstrusiveVariantStorageCopy(const InstrusiveVariantStorageCopy&) = default;

    InstrusiveVariantStorageCopy& operator=(const InstrusiveVariantStorageCopy&) = default;
    InstrusiveVariantStorageCopy(InstrusiveVariantStorageCopy&&) = default;
    InstrusiveVariantStorageCopy& operator=(InstrusiveVariantStorageCopy&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageCopy<Base, SpecialMem::Deleted, SubClasses...>
    : InstrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const InstrusiveVariantStorageCopy&) {}

public:
    InstrusiveVariantStorageCopy() = default;

    InstrusiveVariantStorageCopy(const InstrusiveVariantStorageCopy&) = delete;

    InstrusiveVariantStorageCopy& operator=(const InstrusiveVariantStorageCopy&) = default;
    InstrusiveVariantStorageCopy(InstrusiveVariantStorageCopy&&) = default;
    InstrusiveVariantStorageCopy& operator=(InstrusiveVariantStorageCopy&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageCopy<Base, SpecialMem::Exists, SubClasses...>
    : InstrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const InstrusiveVariantStorageCopy& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> copyFuncs = {
            {+[](std::byte* storage, Base* ptr) { new (storage) SubClasses(*static_cast<SubClasses*>(ptr)); }...}};
        copyFuncs[rhs.cast().index()](this->m_storage, &rhs.get());
    }

public:
    InstrusiveVariantStorageCopy() = default;

    InstrusiveVariantStorageCopy(const InstrusiveVariantStorageCopy& rhs)
    {
        copyConstruct(rhs);
    }

    InstrusiveVariantStorageCopy& operator=(const InstrusiveVariantStorageCopy&) = default;

    InstrusiveVariantStorageCopy(InstrusiveVariantStorageCopy&&) = default;
    InstrusiveVariantStorageCopy& operator=(InstrusiveVariantStorageCopy&&) = default;
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
struct InstrusiveVariantStorageCopyAss;

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageCopyAss<Base, SpecialMem::Deleted, SubClasses...>
    : InstrusiveVariantStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    InstrusiveVariantStorageCopyAss() = default;

    InstrusiveVariantStorageCopyAss& operator=(const InstrusiveVariantStorageCopyAss&) = delete;

    ~InstrusiveVariantStorageCopyAss() = default;
    InstrusiveVariantStorageCopyAss(const InstrusiveVariantStorageCopyAss&) = default;
    InstrusiveVariantStorageCopyAss(InstrusiveVariantStorageCopyAss&&) = default;
    InstrusiveVariantStorageCopyAss& operator=(InstrusiveVariantStorageCopyAss&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageCopyAss<Base, SpecialMem::Exists, SubClasses...>
    : InstrusiveVariantStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    InstrusiveVariantStorageCopyAss() = default;

    InstrusiveVariantStorageCopyAss& operator=(const InstrusiveVariantStorageCopyAss& rhs)
    {
        if (this->get().index() == rhs.cast().index())
        {
            constexpr std::array<void (*)(Base*, Base*), sizeof...(SubClasses)> copyFuncs = {
                {+[](Base* storage, Base* ptr) {
                    *static_cast<SubClasses*>(storage) = *static_cast<SubClasses*>(ptr);
                }...}};
            copyFuncs[rhs.cast().index()](&this->get(), &rhs.get());
        }
        else
        {
            this->destruct();
            this->copyConstruct(rhs);
        }
        return *this;
    }

    ~InstrusiveVariantStorageCopyAss() = default;
    InstrusiveVariantStorageCopyAss(const InstrusiveVariantStorageCopyAss&) = default;
    InstrusiveVariantStorageCopyAss(InstrusiveVariantStorageCopyAss&&) = default;
    InstrusiveVariantStorageCopyAss& operator=(InstrusiveVariantStorageCopyAss&&) = default;
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
struct InstrusiveVariantStorageMove;

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageMove<Base, SpecialMem::Trivial, SubClasses...>
    : InstrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(InstrusiveVariantStorageMove&& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> moveFuncs = {
            {+[](std::byte* storage, Base* ptr) {
                new (storage) SubClasses(std::move(*static_cast<SubClasses*>(ptr)));
            }...}};
        moveFuncs[rhs.cast().index()](this->m_storage, &rhs.get());
    }

public:
    InstrusiveVariantStorageMove() = default;

    InstrusiveVariantStorageMove(const InstrusiveVariantStorageMove&) = default;
    InstrusiveVariantStorageMove& operator=(const InstrusiveVariantStorageMove&) = default;
    InstrusiveVariantStorageMove(InstrusiveVariantStorageMove&&) = default;
    InstrusiveVariantStorageMove& operator=(InstrusiveVariantStorageMove&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageMove<Base, SpecialMem::Deleted, SubClasses...>
    : InstrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(InstrusiveVariantStorageMove&&) {}

public:
    InstrusiveVariantStorageMove() = default;

    InstrusiveVariantStorageMove(InstrusiveVariantStorageMove&&) = default;

    InstrusiveVariantStorageMove& operator=(const InstrusiveVariantStorageMove&) = default;
    InstrusiveVariantStorageMove(const InstrusiveVariantStorageMove&) = default;
    InstrusiveVariantStorageMove& operator=(InstrusiveVariantStorageMove&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageMove<Base, SpecialMem::Exists, SubClasses...>
    : InstrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(InstrusiveVariantStorageMove&& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> moveFuncs = {
            {+[](std::byte* storage, Base* ptr) {
                new (storage) SubClasses(std::move(*static_cast<SubClasses*>(ptr)));
            }...}};
        moveFuncs[rhs.get().index()](this->m_storage, &rhs.get());
    }

public:
    InstrusiveVariantStorageMove() = default;

    InstrusiveVariantStorageMove(InstrusiveVariantStorageMove&& rhs)
#if !defined(_MSC_VER) || defined(__clang__) || _MSC_VER >= 1928
        noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
#endif
    {
        moveConstruct(std::move(rhs));
    }

    InstrusiveVariantStorageMove(const InstrusiveVariantStorageMove&) = default;
    InstrusiveVariantStorageMove& operator=(const InstrusiveVariantStorageMove&) = default;
    InstrusiveVariantStorageMove& operator=(InstrusiveVariantStorageMove&&) = default;
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
struct InstrusiveVariantStorageMoveAss;

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageMoveAss<Base, SpecialMem::Deleted, SubClasses...>
    : InstrusiveVariantStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    InstrusiveVariantStorageMoveAss() = default;

    InstrusiveVariantStorageMoveAss& operator=(const InstrusiveVariantStorageMoveAss&) = delete;

    ~InstrusiveVariantStorageMoveAss() = default;
    InstrusiveVariantStorageMoveAss(const InstrusiveVariantStorageMoveAss&) = default;
    InstrusiveVariantStorageMoveAss(InstrusiveVariantStorageMoveAss&&) = default;
    InstrusiveVariantStorageMoveAss& operator=(InstrusiveVariantStorageMoveAss&&) = default;
};

template <class Base, class... SubClasses>
struct InstrusiveVariantStorageMoveAss<Base, SpecialMem::Exists, SubClasses...>
    : InstrusiveVariantStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    InstrusiveVariantStorageMoveAss() = default;

    InstrusiveVariantStorageMoveAss& operator=(InstrusiveVariantStorageMoveAss&& rhs)
    {
        if (this->get().index() == rhs.get().index())
        {
            constexpr std::array<void (*)(Base*, Base*), sizeof...(SubClasses)> moveFuncs = {
                {+[](Base* storage, Base* ptr) {
                    *static_cast<SubClasses*>(storage) = std::move(*static_cast<SubClasses*>(ptr));
                }...}};
            moveFuncs[rhs.get().index()](&this->get(), &rhs.get());
        }
        else
        {
            this->destruct();
            this->moveConstruct(std::move(rhs));
        }
        return *this;
    }

    ~InstrusiveVariantStorageMoveAss() = default;
    InstrusiveVariantStorageMoveAss(const InstrusiveVariantStorageMoveAss&) = default;
    InstrusiveVariantStorageMoveAss(InstrusiveVariantStorageMoveAss&&) = default;
    InstrusiveVariantStorageMoveAss& operator=(const InstrusiveVariantStorageMoveAss&) = default;
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

} // namespace detail::InstrusiveVariantStorage

template <class T, class U = decltype(detail::InstrusiveVariantStorage::deduceArgs(std::declval<T*>()))>
class InstrusiveVariantStorage
{
    static_assert(always_false<T>,
                  "InstrusiveVariantStorage can only store a class inheriting from AbstractIntrusiveVariant");
};

template <class Base, class... SubClasses>
class InstrusiveVariantStorage<Base, AbstractIntrusiveVariant<SubClasses...>>
    : public detail::InstrusiveVariantStorage::InstrusiveVariantStorageMoveAss<
          Base, std::max({detail::InstrusiveVariantStorage::moveAss<SubClasses>()...}), SubClasses...>
{
    using detail::InstrusiveVariantStorage::InstrusiveVariantStorageBase<Base, SubClasses...>::get;

public:
    template <class T, std::enable_if_t<(std::is_same_v<std::decay_t<T>, SubClasses> || ...)>* = nullptr>
    InstrusiveVariantStorage(T&& value)
    {
        static_assert((std::is_same_v<std::decay_t<T>, SubClasses> || ...));
        new (this->m_storage) std::decay_t<T>(std::forward<T>(value));
    }

    template <class T, class... Args>
    InstrusiveVariantStorage(std::in_place_type_t<T>, Args&&... args)
    {
        static_assert((std::is_same_v<T, SubClasses> || ...));
        new (this->m_storage) T(std::forward<Args>(args)...);
    }

    template <class T, class... Args>
    T& emplace(Args&&... args)
    {
        *this = InstrusiveVariantStorage(std::in_place_type<T>, std::forward<Args>(args)...);
        return this->get().template cast<T>();
    }

    operator Base&()
    {
        return get();
    }

    operator const Base&() const
    {
        return get();
    }

    Base* CLD_NON_NULL operator->()
    {
        return &get();
    }

    const Base* CLD_NON_NULL operator->() const
    {
        return &get();
    }

    Base& operator*()
    {
        return get();
    }

    const Base& operator*() const
    {
        return get();
    }

    std::unique_ptr<Base> toUniquePtr() &&
    {
        return get().match([](auto&& value) -> std::unique_ptr<Base> {
            return std::make_unique<std::decay_t<decltype(value)>>(std::move(value));
        });
    }
};

} // namespace cld

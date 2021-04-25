#pragma once

#include <algorithm>
#include <cstddef>
#include <variant>

#include "AbstractIntrusiveVariant.hpp"
#include "Constexpr.hpp"

namespace cld
{
namespace detail::IntrusiveVariantStorage
{
template <class... Args>
auto deduceArgs(::cld::AbstractIntrusiveVariant<Args...>*) -> ::cld::AbstractIntrusiveVariant<Args...>;

template <class Base, class... SubClasses>
class IntrusiveVariantStorageBase
{
protected:
    alignas(SubClasses...) std::byte m_storage[std::max({sizeof(SubClasses)...})];
    cld::suitableUInt<sizeof...(SubClasses)> m_index;

    [[nodiscard]] Base& get()
    {
        constexpr std::array<Base& (*)(std::byte*), sizeof...(SubClasses)> converters = {
            +[](std::byte* ptr) -> Base& { return *std::launder(reinterpret_cast<SubClasses*>(ptr)); }...};
        return converters[m_index](m_storage);
    }

    [[nodiscard]] const Base& get() const
    {
        constexpr std::array<const Base& (*)(const std::byte*), sizeof...(SubClasses)> converters = {
            +[](const std::byte* ptr) -> const Base& {
                return *std::launder(reinterpret_cast<const SubClasses*>(ptr));
            }...};
        return converters[m_index](m_storage);
    }
};

enum class SpecialMem
{
    Trivial,
    Exists,
    Deleted
};

template <class Base, SpecialMem status, class... SubClasses>
struct IntrusiveVariantStorageDestruct;

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageDestruct<Base, SpecialMem::Trivial, SubClasses...>
    : IntrusiveVariantStorageBase<Base, SubClasses...>
{
protected:
    void destruct() {}

public:
    IntrusiveVariantStorageDestruct() noexcept = default;

    ~IntrusiveVariantStorageDestruct() noexcept = default;

    IntrusiveVariantStorageDestruct(const IntrusiveVariantStorageDestruct&) noexcept = default;
    IntrusiveVariantStorageDestruct& operator=(const IntrusiveVariantStorageDestruct&) noexcept = default;
    IntrusiveVariantStorageDestruct(IntrusiveVariantStorageDestruct&&) noexcept = default;
    IntrusiveVariantStorageDestruct& operator=(IntrusiveVariantStorageDestruct&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageDestruct<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageBase<Base, SubClasses...>
{
protected:
    void destruct()
    {
        constexpr std::array<void (*)(Base*), sizeof...(SubClasses)> destructorFuncs = {
            {+[](Base* ptr) { std::destroy_at(static_cast<SubClasses*>(ptr)); }...}};
        destructorFuncs[this->m_index](&this->get());
    }

public:
    IntrusiveVariantStorageDestruct() = default;

    ~IntrusiveVariantStorageDestruct()
    {
        destruct();
    }

    IntrusiveVariantStorageDestruct(const IntrusiveVariantStorageDestruct&) noexcept = default;
    IntrusiveVariantStorageDestruct& operator=(const IntrusiveVariantStorageDestruct&) noexcept = default;
    IntrusiveVariantStorageDestruct(IntrusiveVariantStorageDestruct&&) noexcept = default;
    IntrusiveVariantStorageDestruct& operator=(IntrusiveVariantStorageDestruct&&) noexcept = default;
};

template <class T>
constexpr SpecialMem dtor()
{
    return std::is_trivially_destructible_v<T> ? SpecialMem::Trivial : SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct IntrusiveVariantStorageCopy;

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopy<Base, SpecialMem::Trivial, SubClasses...>
    : IntrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const IntrusiveVariantStorageCopy& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> copyFuncs = {
            {+[](std::byte* storage, Base* ptr) { new (storage) SubClasses(*static_cast<SubClasses*>(ptr)); }...}};
        copyFuncs[rhs.m_index](this->m_storage, &rhs.get());
        this->m_storage = rhs.m_index;
    }

public:
    IntrusiveVariantStorageCopy() noexcept = default;

    IntrusiveVariantStorageCopy(const IntrusiveVariantStorageCopy&) noexcept = default;

    IntrusiveVariantStorageCopy& operator=(const IntrusiveVariantStorageCopy&) noexcept = default;
    IntrusiveVariantStorageCopy(IntrusiveVariantStorageCopy&&) noexcept = default;
    IntrusiveVariantStorageCopy& operator=(IntrusiveVariantStorageCopy&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopy<Base, SpecialMem::Deleted, SubClasses...>
    : IntrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const IntrusiveVariantStorageCopy&) {}

public:
    IntrusiveVariantStorageCopy() noexcept = default;

    IntrusiveVariantStorageCopy(const IntrusiveVariantStorageCopy&) = delete;

    IntrusiveVariantStorageCopy& operator=(const IntrusiveVariantStorageCopy&) noexcept = default;
    IntrusiveVariantStorageCopy(IntrusiveVariantStorageCopy&&) noexcept = default;
    IntrusiveVariantStorageCopy& operator=(IntrusiveVariantStorageCopy&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopy<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const IntrusiveVariantStorageCopy& rhs)
    {
        constexpr std::array<void (*)(std::byte*, const Base*), sizeof...(SubClasses)> copyFuncs = {
            {+[](std::byte* storage, const Base* ptr)
             { new (storage) SubClasses(*static_cast<const SubClasses*>(ptr)); }...}};
        copyFuncs[rhs.m_index](this->m_storage, &rhs.get());
        this->m_index = rhs.m_index;
    }

public:
    IntrusiveVariantStorageCopy() = default;

    IntrusiveVariantStorageCopy(const IntrusiveVariantStorageCopy& rhs) noexcept(
        (std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        copyConstruct(rhs);
    }

    IntrusiveVariantStorageCopy& operator=(const IntrusiveVariantStorageCopy&) noexcept = default;

    IntrusiveVariantStorageCopy(IntrusiveVariantStorageCopy&&) noexcept = default;
    IntrusiveVariantStorageCopy& operator=(IntrusiveVariantStorageCopy&&) noexcept = default;
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
struct IntrusiveVariantStorageCopyAss;

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopyAss<Base, SpecialMem::Deleted, SubClasses...>
    : IntrusiveVariantStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageCopyAss() noexcept = default;

    IntrusiveVariantStorageCopyAss& operator=(const IntrusiveVariantStorageCopyAss&) = delete;

    ~IntrusiveVariantStorageCopyAss() = default;
    IntrusiveVariantStorageCopyAss(const IntrusiveVariantStorageCopyAss&) noexcept = default;
    IntrusiveVariantStorageCopyAss(IntrusiveVariantStorageCopyAss&&) noexcept = default;
    IntrusiveVariantStorageCopyAss& operator=(IntrusiveVariantStorageCopyAss&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopyAss<Base, SpecialMem::Trivial, SubClasses...>
    : IntrusiveVariantStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageCopyAss() noexcept = default;

    IntrusiveVariantStorageCopyAss& operator=(const IntrusiveVariantStorageCopyAss&) noexcept = default;

    ~IntrusiveVariantStorageCopyAss() = default;
    IntrusiveVariantStorageCopyAss(const IntrusiveVariantStorageCopyAss&) noexcept = default;
    IntrusiveVariantStorageCopyAss(IntrusiveVariantStorageCopyAss&&) noexcept = default;
    IntrusiveVariantStorageCopyAss& operator=(IntrusiveVariantStorageCopyAss&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopyAss<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageCopyAss() noexcept = default;

    IntrusiveVariantStorageCopyAss& operator=(const IntrusiveVariantStorageCopyAss& rhs) noexcept(
        (std::is_nothrow_copy_constructible_v<SubClasses> && ...)
        && (std::is_nothrow_copy_assignable_v<SubClasses> && ...))
    {
        if (this->m_index == rhs.m_index)
        {
            constexpr std::array<void (*)(Base*, const Base*), sizeof...(SubClasses)> copyFuncs = {
                {+[](Base* storage, const Base* ptr)
                 { *static_cast<SubClasses*>(storage) = *static_cast<const SubClasses*>(ptr); }...}};
            copyFuncs[rhs.m_index](&this->get(), &rhs.get());
        }
        else
        {
            this->destruct();
            this->copyConstruct(rhs);
        }
        return *this;
    }

    ~IntrusiveVariantStorageCopyAss() noexcept = default;
    IntrusiveVariantStorageCopyAss(const IntrusiveVariantStorageCopyAss&) noexcept = default;
    IntrusiveVariantStorageCopyAss(IntrusiveVariantStorageCopyAss&&) noexcept = default;
    IntrusiveVariantStorageCopyAss& operator=(IntrusiveVariantStorageCopyAss&&) noexcept = default;
};

template <class T>
constexpr SpecialMem copyAss()
{
    if constexpr (std::is_trivially_copy_assignable_v<T> && std::is_trivially_destructible_v<T>)
    {
        return SpecialMem::Trivial;
    }
    else if constexpr (!std::is_copy_assignable_v<T> || !std::is_copy_constructible_v<T>)
    {
        return SpecialMem::Deleted;
    }
    return SpecialMem::Exists;
}

template <class Base, SpecialMem status, class... SubClasses>
struct IntrusiveVariantStorageMove;

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMove<Base, SpecialMem::Trivial, SubClasses...>
    : IntrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(IntrusiveVariantStorageMove&& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> moveFuncs = {
            {+[](std::byte* storage, Base* ptr)
             { new (storage) SubClasses(std::move(*static_cast<SubClasses*>(ptr))); }...}};
        moveFuncs[rhs.m_index](this->m_storage, &rhs.get());
        this->m_index = rhs.m_index;
    }

public:
    IntrusiveVariantStorageMove() noexcept = default;

    IntrusiveVariantStorageMove(const IntrusiveVariantStorageMove&) noexcept = default;
    IntrusiveVariantStorageMove& operator=(const IntrusiveVariantStorageMove&) noexcept = default;
    IntrusiveVariantStorageMove(IntrusiveVariantStorageMove&&) noexcept = default;
    IntrusiveVariantStorageMove& operator=(IntrusiveVariantStorageMove&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMove<Base, SpecialMem::Deleted, SubClasses...>
    : IntrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(IntrusiveVariantStorageMove&&) {}

public:
    IntrusiveVariantStorageMove() noexcept = default;

    IntrusiveVariantStorageMove(IntrusiveVariantStorageMove&&) = delete;

    IntrusiveVariantStorageMove& operator=(const IntrusiveVariantStorageMove&) noexcept = default;
    IntrusiveVariantStorageMove(const IntrusiveVariantStorageMove&) noexcept = default;
    IntrusiveVariantStorageMove& operator=(IntrusiveVariantStorageMove&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMove<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(IntrusiveVariantStorageMove&& rhs)
    {
        constexpr std::array<void (*)(std::byte*, Base*), sizeof...(SubClasses)> moveFuncs = {
            {+[](std::byte* storage, Base* ptr)
             { new (storage) SubClasses(std::move(*static_cast<SubClasses*>(ptr))); }...}};
        moveFuncs[rhs.m_index](this->m_storage, &rhs.get());
        this->m_index = rhs.m_index;
    }

public:
    IntrusiveVariantStorageMove() = default;

    IntrusiveVariantStorageMove(IntrusiveVariantStorageMove&& rhs) noexcept(
        (std::is_nothrow_move_constructible_v<SubClasses> && ...))
    {
        moveConstruct(std::move(rhs));
    }

    IntrusiveVariantStorageMove(const IntrusiveVariantStorageMove&) noexcept = default;
    IntrusiveVariantStorageMove& operator=(const IntrusiveVariantStorageMove&) noexcept = default;
    IntrusiveVariantStorageMove& operator=(IntrusiveVariantStorageMove&&) noexcept = default;
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
struct IntrusiveVariantStorageMoveAss;

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMoveAss<Base, SpecialMem::Deleted, SubClasses...>
    : IntrusiveVariantStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageMoveAss() noexcept = default;

    ~IntrusiveVariantStorageMoveAss() = default;

    IntrusiveVariantStorageMoveAss& operator=(IntrusiveVariantStorageMoveAss&&) = delete;

    IntrusiveVariantStorageMoveAss(const IntrusiveVariantStorageMoveAss&) noexcept = default;
    IntrusiveVariantStorageMoveAss& operator=(const IntrusiveVariantStorageMoveAss&) noexcept = default;
    IntrusiveVariantStorageMoveAss(IntrusiveVariantStorageMoveAss&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMoveAss<Base, SpecialMem::Trivial, SubClasses...>
    : IntrusiveVariantStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageMoveAss() noexcept = default;

    IntrusiveVariantStorageMoveAss& operator=(IntrusiveVariantStorageMoveAss&&) noexcept = default;

    ~IntrusiveVariantStorageMoveAss() = default;
    IntrusiveVariantStorageMoveAss(const IntrusiveVariantStorageMoveAss&) noexcept = default;
    IntrusiveVariantStorageMoveAss& operator=(const IntrusiveVariantStorageMoveAss&) noexcept = default;
    IntrusiveVariantStorageMoveAss(IntrusiveVariantStorageMoveAss&&) noexcept = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMoveAss<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageMoveAss() noexcept = default;

    IntrusiveVariantStorageMoveAss& operator=(IntrusiveVariantStorageMoveAss&& rhs) noexcept(
        (std::is_nothrow_move_constructible_v<SubClasses> && ...)
        && (std::is_nothrow_move_assignable_v<SubClasses> && ...))
    {
        if (this->m_index == rhs.m_index)
        {
            constexpr std::array<void (*)(Base*, Base*), sizeof...(SubClasses)> moveFuncs = {
                {+[](Base* storage, Base* ptr)
                 { *static_cast<SubClasses*>(storage) = std::move(*static_cast<SubClasses*>(ptr)); }...}};
            moveFuncs[rhs.m_index](&this->get(), &rhs.get());
        }
        else
        {
            this->destruct();
            this->moveConstruct(std::move(rhs));
        }
        return *this;
    }

    ~IntrusiveVariantStorageMoveAss() noexcept = default;
    IntrusiveVariantStorageMoveAss(const IntrusiveVariantStorageMoveAss&) noexcept = default;
    IntrusiveVariantStorageMoveAss(IntrusiveVariantStorageMoveAss&&) noexcept = default;
    IntrusiveVariantStorageMoveAss& operator=(const IntrusiveVariantStorageMoveAss&) noexcept = default;
};

template <class T>
constexpr SpecialMem moveAss()
{
    if constexpr (std::is_trivially_move_assignable_v<T> && std::is_trivially_destructible_v<T>)
    {
        return SpecialMem::Trivial;
    }
    else if constexpr (!std::is_move_assignable_v<T> || !std::is_move_constructible_v<T>)
    {
        return SpecialMem::Deleted;
    }
    return SpecialMem::Exists;
}

} // namespace detail::IntrusiveVariantStorage

template <class T, class U = decltype(detail::IntrusiveVariantStorage::deduceArgs(std::declval<T*>()))>
class IntrVarValue
{
    static_assert(always_false<T>,
                  "InstrusiveVariantStorage can only store a class inheriting from AbstractIntrusiveVariant");

public:
    // DEDUCTION ONLY

    template <class V>
    IntrVarValue(const V&)
    {
        CLD_UNREACHABLE;
    }

    template <class V>
    IntrVarValue(V&&) noexcept
    {
        CLD_UNREACHABLE;
    }

    template <class V, class... Args>
    IntrVarValue(std::in_place_type_t<V>, Args&&...) noexcept
    {
        CLD_UNREACHABLE;
    }
};

template <class V>
IntrVarValue(const V&) -> IntrVarValue<typename V::base_type>;

template <class V>
IntrVarValue(V&&) -> IntrVarValue<typename V::base_type>;

template <class V, class... Args>
IntrVarValue(std::in_place_type_t<V>, Args&&...) -> IntrVarValue<typename V::base_type>;

template <class Base, class... SubClasses>
class IntrVarValue<Base, AbstractIntrusiveVariant<Base, SubClasses...>>
    : public detail::IntrusiveVariantStorage::IntrusiveVariantStorageMoveAss<
          Base, std::max({detail::IntrusiveVariantStorage::moveAss<SubClasses>()...}), SubClasses...>
{
    using detail::IntrusiveVariantStorage::IntrusiveVariantStorageBase<Base, SubClasses...>::get;

public:
    template <class T, class... Args, std::enable_if_t<std::disjunction_v<std::is_same<T, SubClasses>...>>* = nullptr>
    IntrVarValue(std::in_place_type_t<T>, Args&&... args)
    {
        new (this->m_storage) T(std::forward<Args>(args)...);
        constexpr auto var = Base::template indexOf<T>();
        this->m_index = var;
    }

    template <class T, std::enable_if_t<std::disjunction_v<std::is_same<T, SubClasses>...>>* = nullptr>
    IntrVarValue(T&& value) noexcept(std::is_nothrow_move_constructible_v<T>)
    {
        new (this->m_storage) T(std::move(value));
        constexpr auto var = Base::template indexOf<T>();
        this->m_index = var;
    }

    template <class T, std::enable_if_t<std::disjunction_v<std::is_same<T, SubClasses>...>>* = nullptr>
    IntrVarValue&
        operator=(T&& value) noexcept(std::is_nothrow_move_assignable_v<T>&& std::is_nothrow_move_constructible_v<T>)
    {
        constexpr auto var = Base::template indexOf<T>();
        if (this->m_index == var)
        {
            this->get().template as<T>() = std::move(value);
        }
        else
        {
            this->destruct();
            new (this->m_storage) T(std::move(value));
            this->m_index = var;
        }
        return *this;
    }

    template <class T, std::enable_if_t<std::disjunction_v<std::is_same<T, SubClasses>...>>* = nullptr>
    IntrVarValue(const T& value) noexcept(std::is_nothrow_copy_constructible_v<T>)
    {
        new (this->m_storage) T(value);
        constexpr auto var = Base::template indexOf<T>();
        this->m_index = var;
    }

    template <class T, std::enable_if_t<std::disjunction_v<std::is_same<T, SubClasses>...>>* = nullptr>
    IntrVarValue& operator=(const T& value) noexcept(
        std::is_nothrow_copy_assignable_v<T>&& std::is_nothrow_copy_constructible_v<T>)
    {
        constexpr auto var = Base::template indexOf<T>();
        if (this->m_index == var)
        {
            this->get().template as<T>() = value;
        }
        else
        {
            this->destruct();
            new (this->m_storage) T(value);
            this->m_index = var;
        }
        return *this;
    }

    template <class U = Base,
              std::enable_if_t<
                  std::is_same_v<U, Base> && std::conjunction_v<std::is_copy_constructible<SubClasses>...>>* = nullptr>
    IntrVarValue(const U& value) noexcept((std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        this->m_index = value.index();
        constexpr std::array<void (*)(std::byte*, const Base&), sizeof...(SubClasses)> copyConstructors = {
            +[](std::byte* storage, const Base& value)
            { new (storage) SubClasses(static_cast<const SubClasses&>(value)); }...};
        copyConstructors[this->m_index](this->m_storage, value);
    }

    template <class U = Base,
              std::enable_if_t<
                  std::is_same_v<U, Base> && std::conjunction_v<std::is_move_constructible<SubClasses>...>>* = nullptr>
    IntrVarValue(U&& value) noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
    {
        this->m_index = value.index();
        constexpr std::array<void (*)(std::byte*, Base &&), sizeof...(SubClasses)> moveConstructors = {
            +[](std::byte* storage, Base&& value) { new (storage) SubClasses(static_cast<SubClasses&&>(value)); }...};
        moveConstructors[this->m_index](this->m_storage, std::move(value));
    }

    template <class T, class... Args, std::enable_if_t<(std::is_same_v<T, SubClasses> || ...)>* = nullptr>
    T& emplace(Args&&... args)
    {
        this->destruct();
        auto* object = new (this->m_storage) T(std::forward<Args>(args)...);
        constexpr auto var = Base::template indexOf<T>();
        this->m_index = var;
        return *object;
    }

    Base* CLD_NON_NULL operator->()
    {
        return &get();
    }

    const Base* CLD_NON_NULL operator->() const
    {
        return &get();
    }

    Base* CLD_NON_NULL data()
    {
        return &get();
    }

    const Base* CLD_NON_NULL data() const
    {
        return &get();
    }

    operator const Base&() const&
    {
        return get();
    }

    Base& operator*() &
    {
        return get();
    }

    const Base& operator*() const&
    {
        return get();
    }

    Base&& operator*() &&
    {
        return std::move(get());
    }
};

template <class T, class U, std::enable_if_t<IsEqualComparable<T, U>{}>* = nullptr>
bool operator==(const IntrVarValue<T>& lhs, const IntrVarValue<U>& rhs)
{
    return *lhs == *rhs;
}

template <class T, class U, std::enable_if_t<IsEqualComparable<T, U>{}>* = nullptr>
bool operator!=(const IntrVarValue<T>& lhs, const IntrVarValue<U>& rhs)
{
    return !(lhs == rhs);
}

template <class T, class U, std::enable_if_t<IsEqualComparable<T, U>{}>* = nullptr>
bool operator==(const IntrVarValue<T>& lhs, const U& rhs)
{
    return *lhs == rhs;
}

template <class T, class U, std::enable_if_t<IsEqualComparable<T, U>{}>* = nullptr>
bool operator==(const T& lhs, const IntrVarValue<U>& rhs)
{
    return lhs == *rhs;
}

template <class T, class U, std::enable_if_t<IsEqualComparable<T, U>{}>* = nullptr>
bool operator!=(const T& lhs, const IntrVarValue<U>& rhs)
{
    return !(lhs == rhs);
}

template <class T, class U, std::enable_if_t<IsEqualComparable<T, U>{}>* = nullptr>
bool operator!=(const IntrVarValue<T>& lhs, const U& rhs)
{
    return !(lhs == rhs);
}

} // namespace cld

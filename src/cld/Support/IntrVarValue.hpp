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
    IntrusiveVariantStorageDestruct() = default;

    ~IntrusiveVariantStorageDestruct() = default;

    IntrusiveVariantStorageDestruct(const IntrusiveVariantStorageDestruct&) = default;
    IntrusiveVariantStorageDestruct& operator=(const IntrusiveVariantStorageDestruct&) = default;
    IntrusiveVariantStorageDestruct(IntrusiveVariantStorageDestruct&&) = default;
    IntrusiveVariantStorageDestruct& operator=(IntrusiveVariantStorageDestruct&&) = default;
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
        destructorFuncs[this->get().index()](&this->get());
    }

public:
    IntrusiveVariantStorageDestruct() = default;

    ~IntrusiveVariantStorageDestruct()
    {
        destruct();
    }

    IntrusiveVariantStorageDestruct(const IntrusiveVariantStorageDestruct&) = default;
    IntrusiveVariantStorageDestruct& operator=(const IntrusiveVariantStorageDestruct&) = default;
    IntrusiveVariantStorageDestruct(IntrusiveVariantStorageDestruct&&) = default;
    IntrusiveVariantStorageDestruct& operator=(IntrusiveVariantStorageDestruct&&) = default;
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
        copyFuncs[rhs.get().index()](this->m_storage, &rhs.get());
        this->m_storage = rhs.get().index();
    }

public:
    IntrusiveVariantStorageCopy() = default;

    IntrusiveVariantStorageCopy(const IntrusiveVariantStorageCopy&) = default;

    IntrusiveVariantStorageCopy& operator=(const IntrusiveVariantStorageCopy&) = default;
    IntrusiveVariantStorageCopy(IntrusiveVariantStorageCopy&&) = default;
    IntrusiveVariantStorageCopy& operator=(IntrusiveVariantStorageCopy&&) = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopy<Base, SpecialMem::Deleted, SubClasses...>
    : IntrusiveVariantStorageDestruct<Base, std::max({dtor<SubClasses>()...}), SubClasses...>
{
protected:
    void copyConstruct(const IntrusiveVariantStorageCopy&) {}

public:
    IntrusiveVariantStorageCopy() = default;

    IntrusiveVariantStorageCopy(const IntrusiveVariantStorageCopy&) = delete;

    IntrusiveVariantStorageCopy& operator=(const IntrusiveVariantStorageCopy&) = default;
    IntrusiveVariantStorageCopy(IntrusiveVariantStorageCopy&&) = default;
    IntrusiveVariantStorageCopy& operator=(IntrusiveVariantStorageCopy&&) = default;
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
        copyFuncs[rhs.get().index()](this->m_storage, &rhs.get());
        this->m_index = rhs.get().index();
    }

public:
    IntrusiveVariantStorageCopy() = default;

    IntrusiveVariantStorageCopy(const IntrusiveVariantStorageCopy& rhs)
    {
        copyConstruct(rhs);
    }

    IntrusiveVariantStorageCopy& operator=(const IntrusiveVariantStorageCopy&) = default;

    IntrusiveVariantStorageCopy(IntrusiveVariantStorageCopy&&) = default;
    IntrusiveVariantStorageCopy& operator=(IntrusiveVariantStorageCopy&&) = default;
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
    IntrusiveVariantStorageCopyAss() = default;

    IntrusiveVariantStorageCopyAss& operator=(const IntrusiveVariantStorageCopyAss&) = delete;

    ~IntrusiveVariantStorageCopyAss() = default;
    IntrusiveVariantStorageCopyAss(const IntrusiveVariantStorageCopyAss&) = default;
    IntrusiveVariantStorageCopyAss(IntrusiveVariantStorageCopyAss&&) = default;
    IntrusiveVariantStorageCopyAss& operator=(IntrusiveVariantStorageCopyAss&&) = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageCopyAss<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageCopy<Base, std::max({copyCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageCopyAss() = default;

    IntrusiveVariantStorageCopyAss& operator=(const IntrusiveVariantStorageCopyAss& rhs)
    {
        if (this->get().index() == rhs.get().index())
        {
            constexpr std::array<void (*)(Base*, const Base*), sizeof...(SubClasses)> copyFuncs = {
                {+[](Base* storage, const Base* ptr)
                 { *static_cast<SubClasses*>(storage) = *static_cast<const SubClasses*>(ptr); }...}};
            copyFuncs[rhs.get().index()](&this->get(), &rhs.get());
        }
        else
        {
            this->destruct();
            this->copyConstruct(rhs);
        }
        return *this;
    }

    ~IntrusiveVariantStorageCopyAss() = default;
    IntrusiveVariantStorageCopyAss(const IntrusiveVariantStorageCopyAss&) = default;
    IntrusiveVariantStorageCopyAss(IntrusiveVariantStorageCopyAss&&) = default;
    IntrusiveVariantStorageCopyAss& operator=(IntrusiveVariantStorageCopyAss&&) = default;
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
        moveFuncs[rhs.get().index()](this->m_storage, &rhs.get());
        this->m_index = rhs.get().index();
    }

public:
    IntrusiveVariantStorageMove() = default;

    IntrusiveVariantStorageMove(const IntrusiveVariantStorageMove&) = default;
    IntrusiveVariantStorageMove& operator=(const IntrusiveVariantStorageMove&) = default;
    IntrusiveVariantStorageMove(IntrusiveVariantStorageMove&&) = default;
    IntrusiveVariantStorageMove& operator=(IntrusiveVariantStorageMove&&) = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMove<Base, SpecialMem::Deleted, SubClasses...>
    : IntrusiveVariantStorageCopyAss<Base, std::max({copyAss<SubClasses>()...}), SubClasses...>
{
protected:
    void moveConstruct(IntrusiveVariantStorageMove&&) {}

public:
    IntrusiveVariantStorageMove() = default;

    IntrusiveVariantStorageMove(IntrusiveVariantStorageMove&&) = default;

    IntrusiveVariantStorageMove& operator=(const IntrusiveVariantStorageMove&) = default;
    IntrusiveVariantStorageMove(const IntrusiveVariantStorageMove&) = default;
    IntrusiveVariantStorageMove& operator=(IntrusiveVariantStorageMove&&) = default;
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
        moveFuncs[rhs.get().index()](this->m_storage, &rhs.get());
        this->m_index = rhs.get().index();
    }

public:
    IntrusiveVariantStorageMove() = default;

    IntrusiveVariantStorageMove(IntrusiveVariantStorageMove&& rhs)
#if !defined(_MSC_VER) || defined(__clang__) || _MSC_VER >= 1928
        noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
#endif
    {
        moveConstruct(std::move(rhs));
    }

    IntrusiveVariantStorageMove(const IntrusiveVariantStorageMove&) = default;
    IntrusiveVariantStorageMove& operator=(const IntrusiveVariantStorageMove&) = default;
    IntrusiveVariantStorageMove& operator=(IntrusiveVariantStorageMove&&) = default;
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
    IntrusiveVariantStorageMoveAss() = default;

    IntrusiveVariantStorageMoveAss& operator=(const IntrusiveVariantStorageMoveAss&) = delete;

    ~IntrusiveVariantStorageMoveAss() = default;
    IntrusiveVariantStorageMoveAss(const IntrusiveVariantStorageMoveAss&) = default;
    IntrusiveVariantStorageMoveAss(IntrusiveVariantStorageMoveAss&&) = default;
    IntrusiveVariantStorageMoveAss& operator=(IntrusiveVariantStorageMoveAss&&) = default;
};

template <class Base, class... SubClasses>
struct IntrusiveVariantStorageMoveAss<Base, SpecialMem::Exists, SubClasses...>
    : IntrusiveVariantStorageMove<Base, std::max({moveCtor<SubClasses>()...}), SubClasses...>
{
    IntrusiveVariantStorageMoveAss() = default;

    IntrusiveVariantStorageMoveAss& operator=(IntrusiveVariantStorageMoveAss&& rhs)
    {
        if (this->get().index() == rhs.get().index())
        {
            constexpr std::array<void (*)(Base*, Base*), sizeof...(SubClasses)> moveFuncs = {
                {+[](Base* storage, Base* ptr)
                 { *static_cast<SubClasses*>(storage) = std::move(*static_cast<SubClasses*>(ptr)); }...}};
            moveFuncs[rhs.get().index()](&this->get(), &rhs.get());
        }
        else
        {
            this->destruct();
            this->moveConstruct(std::move(rhs));
        }
        return *this;
    }

    ~IntrusiveVariantStorageMoveAss() = default;
    IntrusiveVariantStorageMoveAss(const IntrusiveVariantStorageMoveAss&) = default;
    IntrusiveVariantStorageMoveAss(IntrusiveVariantStorageMoveAss&&) = default;
    IntrusiveVariantStorageMoveAss& operator=(const IntrusiveVariantStorageMoveAss&) = default;
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
};

template <class V>
IntrVarValue(const V&) -> IntrVarValue<typename V::base_type>;

template <class V>
IntrVarValue(V&&) -> IntrVarValue<typename V::base_type>;

template <class Base, class... SubClasses>
class IntrVarValue<Base, AbstractIntrusiveVariant<Base, SubClasses...>>
    : public detail::IntrusiveVariantStorage::IntrusiveVariantStorageMoveAss<
          Base, std::max({detail::IntrusiveVariantStorage::moveAss<SubClasses>()...}), SubClasses...>
{
    using detail::IntrusiveVariantStorage::IntrusiveVariantStorageBase<Base, SubClasses...>::get;

public:
    template <class T, std::enable_if_t<(std::is_same_v<std::decay_t<T>, SubClasses> || ...)>* = nullptr>
    IntrVarValue(T&& value) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>, T>)
    {
        static_assert((std::is_same_v<std::decay_t<T>, SubClasses> || ...));
        auto* object = new (this->m_storage) std::decay_t<T>(std::forward<T>(value));
        this->m_index = object->index();
    }

    template <class T, std::enable_if_t<(std::is_same_v<std::decay_t<T>, SubClasses> || ...)>* = nullptr>
    IntrVarValue& operator=(T&& value) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>, T>)
    {
        *this = IntrVarValue(std::forward<T>(value));
        return *this;
    }

    template <class T, class... Args, std::enable_if_t<(std::is_same_v<T, SubClasses> || ...)>* = nullptr>
    IntrVarValue(std::in_place_type_t<T>, Args&&... args)
    {
        auto* object = new (this->m_storage) T(std::forward<Args>(args)...);
        this->m_index = object->index();
    }

    template <class U = Base,
              std::enable_if_t<std::conjunction_v<std::is_copy_constructible<SubClasses>...>>* = nullptr>
    IntrVarValue(const Base& value) noexcept((std::is_nothrow_copy_constructible_v<SubClasses> && ...))
    {
        this->m_index = value.index();
        constexpr std::array<void (*)(std::byte*, const Base&), sizeof...(SubClasses)> copyConstructors = {
            +[](std::byte* storage, const Base& value)
            { new (storage) SubClasses(static_cast<const SubClasses&>(value)); }...};
        copyConstructors[this->m_index](this->m_storage, value);
    }

    template <class U = Base,
              std::enable_if_t<std::conjunction_v<std::is_move_constructible<SubClasses>...>>* = nullptr>
    IntrVarValue(Base&& value) noexcept((std::is_nothrow_move_constructible_v<SubClasses> && ...))
    {
        this->m_index = value.index();
        constexpr std::array<void (*)(std::byte*, Base &&), sizeof...(SubClasses)> moveConstructors = {
            +[](std::byte* storage, Base&& value) { new (storage) SubClasses(static_cast<SubClasses&&>(value)); }...};
        moveConstructors[this->m_index](this->m_storage, std::move(value));
    }

    template <class T, class... Args, std::enable_if_t<(std::is_same_v<T, SubClasses> || ...)>* = nullptr>
    T& emplace(Args&&... args)
    {
        *this = IntrVarValue(std::in_place_type<T>, std::forward<Args>(args)...);
        return this->get().template as<T>();
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


#pragma once

#include <llvm/Support/Compiler.h>

#include <cstring>
#include <type_traits>
#include <unordered_set>
#include <vector>

#include "AbstractIntrusiveVariant.hpp"
#include "IntrVarValue.hpp"
#include "Util.hpp"

namespace cld
{
template <class T, class U = decltype(detail::AbstractIntrusiveVariant::deduceArgs(std::declval<T*>()))>
class IntrVarAllocator
{
    static_assert(always_false<T>, "Allocator only works for allocating subclasses of AbstractIntrusiveVariant");
};

namespace detail::IntrVarAllocator
{
constexpr std::size_t SLAB_MAX_SIZE = 4096;

template <std::size_t allocSize, std::size_t align, std::size_t count, bool withMetadata>
struct Slab;

template <std::size_t allocSize, std::size_t align, std::size_t count>
struct Slab<allocSize, align, count, false>
{
    static constexpr std::size_t size = SLAB_MAX_SIZE / allocSize * allocSize;
    alignas(align) std::byte storage[size]{};

    Slab()
    {
        __asan_poison_memory_region(storage, size);
    }
};

constexpr std::size_t log2n(std::size_t n)
{
    std::size_t r = 0;
    if (n >> 16)
    {
        r += 16;
        n >>= 16;
    }
    if (n >> 8)
    {
        r += 8;
        n >>= 8;
    }
    if (n >> 4)
    {
        r += 4;
        n >>= 4;
    }
    if (n >> 2)
    {
        r += 2;
        n >>= 2;
    }
    if (n - 1)
        ++r;
    return r;
}

template <std::size_t allocSize, std::size_t align, std::size_t count>
struct Slab<allocSize, align, count, true>
{
    static constexpr std::size_t size = SLAB_MAX_SIZE / allocSize * allocSize;
    alignas(align) std::byte storage[size]{};
    struct
    {
        cld::suitableUInt<count> index : log2n(count) + 1;
        cld::suitableUInt<count> destroyed : 1;
    } metadata[size / allocSize]{};

    Slab()
    {
        __asan_poison_memory_region(storage, size);
    }
};

template <class T>
class Deleter
{
    ::cld::IntrVarAllocator<typename T::base_type>* m_allocator;

    template <class U>
    friend class Deleter;

public:
    Deleter() = default;

    Deleter(::cld::IntrVarAllocator<typename T::base_type>* allocator) : m_allocator(allocator) {}

    template <class U, std::enable_if_t<std::is_same_v<typename T::base_type, typename U::base_type>>* = nullptr>
    Deleter(Deleter<U>&& rhs) noexcept : m_allocator(rhs.m_allocator)
    {
    }

    void operator()(T* pointer) const noexcept
    {
        CLD_ASSERT(m_allocator);
        m_allocator->destroy(pointer);
    }
};

} // namespace detail::IntrVarAllocator

template <class T>
using IVAUniquePtr = std::unique_ptr<T, detail::IntrVarAllocator::Deleter<T>>;

template <class Base, class... Subclasses>
class IntrVarAllocator<Base, AbstractIntrusiveVariant<Base, Subclasses...>>
{
    constexpr static std::size_t largestAlign = std::max({alignof(Subclasses)...});
    constexpr static std::size_t largestSize = std::max({sizeof(Subclasses)...});
    constexpr static std::size_t allocSize = cld::roundUpTo(largestSize, largestAlign);

    constexpr static bool needsDestructor = (!std::is_trivially_destructible_v<Subclasses> || ...);

    static_assert(allocSize >= sizeof(std::byte*),
                  "Allocator only supports allocating for classes equal to or larger than a pointer");

    using index_type = typename Base::index_type;

    using Slab = detail::IntrVarAllocator::Slab<allocSize, largestAlign, sizeof...(Subclasses), needsDestructor>;

    std::vector<std::unique_ptr<Slab>> m_slabs;
    std::vector<std::byte*> m_heads;

    static bool slabFull(std::byte* slabHeader)
    {
        return reinterpret_cast<std::uintptr_t>(slabHeader) & 1;
    }

    std::vector<std::byte*>::iterator getFreeHead()
    {
        auto head = std::find_if_not(m_heads.begin(), m_heads.end(), &slabFull);
        if (head == m_heads.end())
        {
            auto newSlab = std::make_unique<Slab>();
            auto insertPoint = std::lower_bound(m_slabs.begin(), m_slabs.end(), newSlab);
            auto index = insertPoint - m_slabs.begin();
            auto* storage = (*m_slabs.insert(insertPoint, std::move(newSlab)))->storage;
            return m_heads.insert(m_heads.begin() + index, storage);
        }
        return head;
    }

    void advance(std::vector<std::byte*>::iterator head)
    {
        *head += allocSize;
        if (*head == std::end(m_slabs[head - m_heads.begin()]->storage))
        {
            *head = reinterpret_cast<std::byte*>(reinterpret_cast<std::uintptr_t>(*head) | 1);
        }
    }

    void destroyAt(index_type index, std::byte* memory)
    {
        constexpr std::array<void (*)(std::byte*), sizeof...(Subclasses)> destructors = {
            +[](std::byte* memory) { std::destroy_at(std::launder(reinterpret_cast<Subclasses*>(memory))); }...};
        destructors[index](memory);
    }

    void destroySlab(std::byte* head, Slab& slab)
    {
        if (slabFull(head))
        {
            for (std::byte* start = slab.storage; start != std::end(slab.storage); start += allocSize)
            {
                auto& metadata = slab.metadata[(start - slab.storage) / allocSize];
                if (metadata.destroyed)
                {
                    continue;
                }
                metadata.destroyed = true;
                destroyAt(metadata.index, start);
                __asan_poison_memory_region(start, allocSize);
            }
            return;
        }
        std::byte* current = head;
        std::byte* last = std::end(slab.storage);
        std::unordered_set<std::byte*> holes;
        while (current < std::end(slab.storage))
        {
            std::byte* next;
            __asan_unpoison_memory_region(current, sizeof(std::byte*));
            std::memcpy(&next, current, sizeof(std::byte*));
            if (!next)
            {
                last = current;
                break;
            }
            holes.insert(current);
            current = next;
        }
        for (std::byte* start = slab.storage; start != last; start += allocSize)
        {
            auto& metadata = slab.metadata[(start - slab.storage) / allocSize];
            if (holes.count(start) || metadata.destroyed)
            {
                continue;
            }
            metadata.destroyed = true;
            destroyAt(metadata.index, start);
            __asan_poison_memory_region(start, allocSize);
        }
    }

public:
    IntrVarAllocator() = default;

    IntrVarAllocator(const IntrVarAllocator&) = delete;
    IntrVarAllocator& operator=(const IntrVarAllocator&) = delete;

    ~IntrVarAllocator()
    {
        if constexpr (needsDestructor)
        {
            for (std::size_t i = 0; i < m_slabs.size(); i++)
            {
                destroySlab(m_heads[i], *m_slabs[i]);
            }
        }
    }

    IntrVarAllocator(IntrVarAllocator&&) noexcept = default;

    IntrVarAllocator& operator=(IntrVarAllocator&& rhs) noexcept
    {
        if constexpr (needsDestructor)
        {
            for (std::size_t i = 0; i < m_slabs.size(); i++)
            {
                destroySlab(m_heads[i], *m_slabs[i]);
            }
        }
        m_slabs = std::move(rhs.m_slabs);
        m_heads = std::move(rhs.m_heads);
        return *this;
    }

    void destroy(cld::not_null<const Base> object)
    {
        Base* casted = const_cast<Base*>(object.get());
        std::byte* pointerIntoObject = reinterpret_cast<std::byte*>(casted);
        auto slab = std::lower_bound(m_slabs.begin(), m_slabs.end(), pointerIntoObject,
                                     [](const std::unique_ptr<Slab>& slab, std::byte* value)
                                     { return std::end(slab->storage) < value; });
        CLD_ASSERT(slab != m_slabs.end());
        CLD_ASSERT((*slab)->storage <= pointerIntoObject && pointerIntoObject < std::end((*slab)->storage));
        if constexpr (needsDestructor)
        {
            auto& metadata = (*slab)->metadata[(pointerIntoObject - (*slab)->storage) / allocSize];
            if (metadata.destroyed)
            {
                return;
            }
            metadata.destroyed = true;
        }
        auto index = casted->index();
        constexpr std::array<void (*)(Base*), sizeof...(Subclasses)> destructors = {
            +[](Base* base) { std::destroy_at(static_cast<Subclasses*>(base)); }...};
        destructors[index](casted);
        constexpr std::array<std::byte* (*)(Base*), sizeof...(Subclasses)> pointerAdjust = {
            +[](Base* base) { return reinterpret_cast<std::byte*>(static_cast<Subclasses*>(base)); }...};
        auto* pointerToStart = pointerAdjust[index](casted);
        auto& head = m_heads[slab - m_slabs.begin()];
        __asan_unpoison_memory_region(pointerToStart, sizeof(std::byte*));
        std::memcpy(pointerToStart, &head, sizeof(std::byte*));
        // Poison the whole thing. No one but the allocator should access the emplaced free list. Allocator unpoisons
        // first
        __asan_poison_memory_region(pointerToStart, allocSize);
        head = pointerToStart;
    }

    template <class U = Base,
              std::enable_if_t<std::is_same_v<U, Base> && (std::is_copy_constructible_v<Subclasses> && ...)>* = nullptr>
    [[nodiscard]] cld::not_null<Base> alloc(const Base& value)
    {
        constexpr std::array<Base* (*)(IntrVarAllocator*, const Base&), sizeof...(Subclasses)> copyConstructors = {
            +[](IntrVarAllocator* self, const Base& value) -> Base* {
                return self->alloc<Subclasses>(static_cast<const Subclasses&>(value));
            }...};
        return copyConstructors[value.index()](this, value);
    }

    template <class U = Base,
              std::enable_if_t<std::is_same_v<U, Base> && (std::is_move_constructible_v<Subclasses> && ...)>* = nullptr>
    [[nodiscard]] cld::not_null<Base> alloc(Base&& value)
    {
        constexpr std::array<Base* (*)(IntrVarAllocator*, Base &&), sizeof...(Subclasses)> moveConstructors = {
            +[](IntrVarAllocator* self, Base&& value) -> Base* {
                return self->alloc<Subclasses>(static_cast<Subclasses&&>(std::move(value)));
            }...};
        return moveConstructors[value.index()](this, std::move(value));
    }

    template <class U, class... Args>
    [[nodiscard]] auto alloc(Args&&... args)
        -> std::enable_if_t<(std::is_same_v<U, Subclasses> || ...), cld::not_null<U>>
    {
        auto head = getFreeHead();
        __asan_unpoison_memory_region(*head, std::max(sizeof(U), sizeof(std::byte*)));
        std::byte* previouslyContained;
        std::memcpy(&previouslyContained, *head, sizeof(std::byte*));
        __asan_poison_memory_region(*head + sizeof(U), allocSize - sizeof(U));
        auto* object = new (*head) U(std::forward<Args>(args)...);
        if constexpr (needsDestructor)
        {
            auto& slab = *m_slabs[head - m_heads.begin()];
            auto& metadata = slab.metadata[(*head - slab.storage) / allocSize];
            metadata.index = object->index();
            metadata.destroyed = false;
        }
        if (previouslyContained != 0)
        {
            *head = previouslyContained;
        }
        else
        {
            advance(head);
        }
        return object;
    }

    template <class U, class... Args>
    [[nodiscard]] IVAUniquePtr<U> allocUnique(Args&&... args)
    {
        return IVAUniquePtr<U>{alloc<U>(std::forward<Args>(args)...), this};
    }
};

} // namespace cld

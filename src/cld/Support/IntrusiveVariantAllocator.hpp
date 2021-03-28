
#pragma once

#include <llvm/Support/Compiler.h>

#include <cstring>
#include <type_traits>
#include <unordered_set>
#include <vector>

#include "AbstractIntrusiveVariant.hpp"
#include "Util.hpp"

namespace cld
{
template <class T, class U = decltype(detail::AbstractIntrusiveVariant::deduceArgs(std::declval<T*>()))>
class IntrusiveVariantAllocator
{
    static_assert(always_false<T>, "Allocator only works for allocating subclasses of AbstractIntrusiveVariant");
};

namespace detail::IntrusiveVariantAllocator
{
template <std::size_t allocSize, std::size_t align, std::size_t count, bool withMetadata>
struct Slab;

template <std::size_t allocSize, std::size_t align, std::size_t count>
struct Slab<allocSize, align, count, false>
{
    static constexpr std::size_t size = 4096 / allocSize * allocSize;
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
    static constexpr std::size_t size = 4096 / allocSize * allocSize;
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

template <class Base>
class Deleter
{
    ::cld::IntrusiveVariantAllocator<Base>* m_alloc;

public:
    Deleter() = default;

    Deleter(::cld::IntrusiveVariantAllocator<Base>* mAlloc) : m_alloc(mAlloc) {}

    void operator()(Base* pointer) const noexcept;
};

} // namespace detail::IntrusiveVariantAllocator

template <class Base, class PreciseClass = Base>
using IVAUniquePtr = std::unique_ptr<PreciseClass, detail::IntrusiveVariantAllocator::Deleter<Base>>;

template <class Base, class... Subclasses>
class IntrusiveVariantAllocator<Base, AbstractIntrusiveVariant<Subclasses...>>
{
    constexpr static std::size_t largestAlign = std::max({alignof(Subclasses)...});
    constexpr static std::size_t largestSize = std::max({sizeof(Subclasses)...});
    constexpr static std::size_t allocSize = cld::roundUpTo(largestSize, largestAlign);

    constexpr static bool needsDestructor = (!std::is_trivially_destructible_v<Subclasses> || ...);

    static_assert(allocSize >= sizeof(std::byte*),
                  "Allocator only supports allocating for classes equal to or larger than a pointer");

    using index_type = typename Base::index_type;

    using Slab =
        detail::IntrusiveVariantAllocator::Slab<allocSize, largestAlign, sizeof...(Subclasses), needsDestructor>;

    std::vector<Slab> m_slabs;
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
            return m_heads.insert(m_heads.end(), m_slabs.emplace_back().storage);
        }
        return head;
    }

    void advance(std::vector<std::byte*>::iterator head)
    {
        *head += allocSize;
        if (*head == std::end(m_slabs[head - m_heads.begin()].storage))
        {
            *head = reinterpret_cast<std::byte*>(reinterpret_cast<std::uintptr_t>(*head) | 1);
        }
    }

    void destroyAt(index_type index, std::byte* memory)
    {
        constexpr std::array<void (*)(std::byte*), sizeof...(Subclasses)> destructors = {
            +[](std::byte* memory) { std::destroy_at(reinterpret_cast<Subclasses*>(memory)); }...};
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
    IntrusiveVariantAllocator() : m_slabs(1), m_heads{m_slabs.front().storage} {}

    IntrusiveVariantAllocator(const IntrusiveVariantAllocator&) = delete;
    IntrusiveVariantAllocator& operator=(const IntrusiveVariantAllocator&) = delete;

    IntrusiveVariantAllocator(IntrusiveVariantAllocator&&) noexcept = default;
    IntrusiveVariantAllocator& operator=(IntrusiveVariantAllocator&&) noexcept = default;

    ~IntrusiveVariantAllocator()
    {
        if constexpr (needsDestructor)
        {
            for (std::size_t i = 0; i < m_slabs.size(); i++)
            {
                destroySlab(m_heads[i], m_slabs[i]);
            }
        }
    }

    void destroy(const Base* object)
    {
        Base* casted = const_cast<Base*>(object);
        std::byte* pointerIntoObject = reinterpret_cast<std::byte*>(casted);
        auto slab =
            std::find_if(m_slabs.begin(), m_slabs.end(),
                         [pointerIntoObject](const Slab& slab)
                         { return pointerIntoObject >= slab.storage && pointerIntoObject < std::end(slab.storage); });
        CLD_ASSERT(slab != m_slabs.end());
        if (slab->metadata[(pointerIntoObject - slab->storage) / allocSize].destroyed)
        {
            return;
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
        __asan_poison_memory_region(pointerToStart + sizeof(std::byte*), allocSize - sizeof(std::byte*));
        head = pointerToStart;
    }

    template <class U, class... Args>
    [[nodiscard]] U* alloc(Args&&... args)
    {
        static_assert((std::is_same_v<U, Subclasses> || ...),
                      "Class to allocate has to be one of the listed Subclasses");
        auto head = getFreeHead();
        __asan_unpoison_memory_region(*head, std::max(sizeof(U), sizeof(std::byte*)));
        std::byte* previouslyContained;
        std::memcpy(&previouslyContained, *head, sizeof(std::byte*));
        __asan_poison_memory_region(*head + sizeof(U), allocSize - sizeof(U));
        auto* object = new (*head) U(std::forward<Args>(args)...);
        if constexpr (needsDestructor)
        {
            auto& slab = m_slabs[head - m_heads.begin()];
            slab.metadata[(*head - slab.storage) / allocSize].index = object->index();
            slab.metadata[(*head - slab.storage) / allocSize].destroyed = false;
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
    [[nodiscard]] IVAUniquePtr<Base, U> allocUnique(Args&&... args)
    {
        return IVAUniquePtr<Base, U>{alloc<U>(std::forward<Args>(args)...),
                                     detail::IntrusiveVariantAllocator::Deleter<Base>{this}};
    }
};

namespace detail::IntrusiveVariantAllocator
{
template <class Base>
void Deleter<Base>::operator()(Base* pointer) const noexcept
{
    CLD_ASSERT(m_alloc);
    m_alloc->destroy(pointer);
}
} // namespace detail::IntrusiveVariantAllocator

} // namespace cld

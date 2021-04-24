#pragma once

#include <variant>

#include "Lexer.hpp"

namespace cld::Semantics
{
class Useable;

struct AlignedAttribute
{
    // Except inside of the apply method for AlignedAttribute, it's never an empty optional
    std::optional<std::uint64_t> alignment;

    constexpr static std::size_t count = 1;
};

struct DeprecatedAttribute
{
    std::optional<std::string_view> optionalMessage;

    constexpr static std::size_t count = 1;
};

struct CleanupAttribute
{
    const Useable* cleanupFunction;

    constexpr static std::size_t count = 1;
};

struct UsedAttribute
{
    constexpr static std::size_t count = 0;
};

struct NoinlineAttribute
{
    constexpr static std::size_t count = 0;
};

struct AlwaysInlineAttribute
{
    constexpr static std::size_t count = 0;
};

struct GnuInlineAttribute
{
    constexpr static std::size_t count = 0;
};

struct ArtificialAttribute
{
    constexpr static std::size_t count = 0;
};

struct DllImportAttribute
{
    constexpr static std::size_t count = 0;
};

struct VectorSizeAttribute
{
    std::uint64_t size;
    constexpr static std::size_t count = 1;
};

struct NothrowAttribute
{
    constexpr static std::size_t count = 0;
};

using FunctionAttribute =
    std::variant<AlignedAttribute, DeprecatedAttribute, UsedAttribute, NoinlineAttribute, AlwaysInlineAttribute,
                 GnuInlineAttribute, ArtificialAttribute, DllImportAttribute, NothrowAttribute>;

using TypeAttribute = std::variant<AlignedAttribute, DeprecatedAttribute>;

using VariableAttribute =
    std::variant<AlignedAttribute, DeprecatedAttribute, CleanupAttribute, UsedAttribute, DllImportAttribute>;

using AllAttributes =
    VariantUnion<std::variant<VectorSizeAttribute>, FunctionAttribute, TypeAttribute, VariableAttribute>;

template <class T>
class AttributeHolder
{
    std::vector<T> m_attributes;

    template <class U, std::size_t i = 0, class First, class... Args>
    constexpr static std::size_t indexFromType(const std::variant<First, Args...>*)
    {
        if constexpr (std::is_same_v<U, First>)
        {
            return i;
        }
        else if constexpr (sizeof...(Args) > 0)
        {
            return indexFromType<U, i + 1>((std::variant<Args...>*)nullptr);
        }
        else
        {
            CLD_UNREACHABLE;
        }
    }

public:
    [[nodiscard]] const std::vector<T>& getAttributes() const&
    {
        return m_attributes;
    }

    [[nodiscard]] std::vector<T>&& getAttributes() &&
    {
        return std::move(m_attributes);
    }

    template <class U>
    [[nodiscard]] const U& getAttribute() const
    {
        constexpr std::size_t index = indexFromType<U>((T*)nullptr);
        auto result = std::lower_bound(m_attributes.begin(), m_attributes.end(), index,
                                       [](const T& value, std::size_t index) { return value.index() < index; });
        CLD_ASSERT(result != m_attributes.end() && result->index() == index);
        return cld::get<U>(*result);
    }

    template <class U>
    [[nodiscard]] const U* getAttributeIf() const
    {
        constexpr std::size_t index = indexFromType<U>((T*)nullptr);
        auto result = std::lower_bound(m_attributes.begin(), m_attributes.end(), index,
                                       [](const T& value, std::size_t index) { return value.index() < index; });
        if (result != m_attributes.end() && result->index() == index)
        {
            return &cld::get<U>(*result);
        }
        return nullptr;
    }

    template <class U>
    [[nodiscard]] U* getAttributeIf()
    {
        return const_cast<U*>(static_cast<const AttributeHolder<T>*>(this)->getAttributeIf<U>());
    }

    template <class U>
    [[nodiscard]] bool hasAttribute() const
    {
        return getAttributeIf<U>();
    }

    template <class U>
    std::optional<U> removeAttribute()
    {
        constexpr std::size_t index = indexFromType<U>((T*)nullptr);
        auto result = std::lower_bound(m_attributes.begin(), m_attributes.end(), index,
                                       [](const T& value, std::size_t index) { return value.index() < index; });
        if (result != m_attributes.end() && result->index() == index)
        {
            auto copy = cld::get<U>(*result);
            m_attributes.erase(result);
            return copy;
        }
        return {};
    }

    T& addAttribute(T&& attribute)
    {
        auto result = std::lower_bound(m_attributes.begin(), m_attributes.end(), attribute.index(),
                                       [](const T& value, std::size_t index) { return value.index() < index; });
        if (result != m_attributes.end() && result->index() == attribute.index())
        {
            return *result = std::move(attribute);
        }
        auto iter = m_attributes.insert(result, std::move(attribute));
        return *iter;
    }

    template <class U>
    void tryAddFromOther(AttributeHolder<U>&& other)
    {
        auto attributes = std::move(other).getAttributes();
        m_attributes.reserve(m_attributes.size() + attributes.size());
        for (auto& iter : attributes)
        {
            cld::match(std::move(iter),
                       [&](auto&& value)
                       {
                           using W = std::decay_t<decltype(value)>;
                           static_assert(std::is_move_constructible_v<W>);
                           if constexpr (std::is_constructible_v<T, W&&>)
                           {
                               addAttribute(std::move(value));
                           }
                       });
        }
    }

    template <class U>
    void tryAddFromOther(const AttributeHolder<U>& other)
    {
        auto& attributes = other.getAttributes();
        m_attributes.reserve(m_attributes.size() + attributes.size());
        for (auto& iter : attributes)
        {
            cld::match(iter,
                       [&](auto&& value)
                       {
                           using W = std::decay_t<decltype(value)>;
                           static_assert(std::is_copy_constructible_v<W>);
                           if constexpr (std::is_constructible_v<T, W>)
                           {
                               addAttribute(std::move(value));
                           }
                       });
        }
    }
};

} // namespace cld::Semantics

#pragma once

#include <variant>

namespace cld::Semantics
{
class Useable;

struct AlignmentAttribute
{
    std::optional<std::size_t> optionalAlignment;
};

struct DeprecatedAttribute
{
    std::optional<std::string_view> optionalMessage;
};

struct CleanupAttribute
{
    const Useable* cleanupFunction;
};

struct UsedAttribute
{
};

using FunctionAttribute = std::variant<AlignmentAttribute, DeprecatedAttribute, UsedAttribute>;

using TypeAttribute = std::variant<AlignmentAttribute, DeprecatedAttribute>;

using VariableAttribute = std::variant<AlignmentAttribute, DeprecatedAttribute, CleanupAttribute, UsedAttribute>;

template <class T>
class AttributeHolder
{
    std::vector<T> m_attributes;

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
    const U& getAttribute() const
    {
        auto result = std::find_if(m_attributes.begin(), m_attributes.end(),
                                   [](auto&& value) { return std::holds_alternative<U>(value); });
        CLD_ASSERT(result != m_attributes.end());
        return cld::get<U>(*result);
    }

    template <class U>
    const U* getAttributeIf() const
    {
        auto result = std::find_if(m_attributes.begin(), m_attributes.end(),
                                   [](auto&& value) { return std::holds_alternative<U>(value); });
        if (result == m_attributes.end())
        {
            return nullptr;
        }
        return &cld::get<U>(*result);
    }

    template <class U>
    bool hasAttribute() const
    {
        return std::find_if(m_attributes.begin(), m_attributes.end(),
                            [](auto&& value) { return std::holds_alternative<U>(value); })
               != m_attributes.end();
    }

    T& addAttribute(T&& attribute)
    {
        m_attributes.push_back(std::move(attribute));
        return m_attributes.back();
    }

    template <class U>
    void tryAddFromOther(AttributeHolder<U>&& other)
    {
        auto attributes = std::move(other).getAttributes();
        m_attributes.reserve(m_attributes.size() + attributes.size());
        for (auto& iter : attributes)
        {
            cld::match(std::move(iter), [&](auto&& value) {
                using W = std::decay_t<decltype(value)>;
                static_assert(std::is_move_constructible_v<W>);
                if constexpr (std::is_constructible_v<T, W&&>)
                {
                    m_attributes.emplace_back(std::move(value));
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
            cld::match(iter, [&](auto&& value) {
                using W = std::decay_t<decltype(value)>;
                static_assert(std::is_copy_constructible_v<W>);
                if constexpr (std::is_constructible_v<T, W>)
                {
                    m_attributes.emplace_back(value);
                }
            });
        }
    }
};

} // namespace cld::Semantics

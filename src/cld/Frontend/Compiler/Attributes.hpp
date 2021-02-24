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
    [[nodiscard]] const std::vector<T>& getAttributes() const
    {
        return m_attributes;
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

    void addAttribute(T&& attribute)
    {
        m_attributes.push_back(std::move(attribute));
    }
};

} // namespace cld::Semantics

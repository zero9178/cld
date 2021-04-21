#pragma once

#include <cld/Support/Util.hpp>

#include <variant>

#include "Semantics.hpp"
#include "Syntax.hpp"

namespace cld::Semantics
{
template <class StartType, class Callable>
class RecursiveVisitor
{
    const StartType& m_start;
    Callable m_nextFunc;

    using ValueType = std::decay_t<std::remove_pointer_t<std::invoke_result_t<Callable, const StartType&>>>;

    class Iterator
    {
        const ValueType* m_curr;
        const Callable* m_nextFunc;

    public:
        using reference = const ValueType&;
        using value_type = const ValueType;
        using pointer = const ValueType*;
        using iterator_category = std::forward_iterator_tag;
        using difference_type = void;

        Iterator() = default;

        Iterator(const ValueType* curr, const Callable* nextFunc) : m_curr(curr), m_nextFunc(nextFunc) {}

        bool operator==(const Iterator& rhs) const noexcept
        {
            return m_curr == rhs.m_curr;
        }

        bool operator!=(const Iterator& rhs) const noexcept
        {
            return !(*this == rhs);
        }

        const ValueType& operator*() const noexcept
        {
            CLD_ASSERT(m_curr);
            return *m_curr;
        }

        const ValueType* operator->() const noexcept
        {
            return m_curr;
        }

        Iterator& operator++(int) noexcept
        {
            CLD_ASSERT(m_curr && m_nextFunc);
            m_curr = (*m_nextFunc)(*m_curr);
            return *this;
        }

        Iterator operator++() noexcept
        {
            auto before = *this;
            CLD_ASSERT(m_curr && m_nextFunc);
            m_curr = (*m_nextFunc)(*m_curr);
            return before;
        }
    };

public:
    RecursiveVisitor(const StartType& start, Callable nextFunc) : m_start(start), m_nextFunc(std::move(nextFunc))
    {
        static_assert(std::is_convertible_v<const StartType&, const ValueType&>);
        static_assert(std::is_invocable_r_v<const ValueType*, Callable, const ValueType&>);
    }

    using value_type = const ValueType;
    using reference = const ValueType&;
    using const_reference = const ValueType&;
    using const_iterator = Iterator;
    using iterator = Iterator;

    const_iterator begin() const
    {
        return Iterator(&static_cast<const ValueType&>(m_start), &m_nextFunc);
    }

    const_iterator cbegin() const
    {
        return begin();
    }

    const_iterator end() const
    {
        return Iterator(nullptr, &m_nextFunc);
    }

    const_iterator cend() const
    {
        return end();
    }
};

constexpr auto DIRECT_DECL_NEXT_FN = [](const Syntax::DirectDeclarator& value) -> const Syntax::DirectDeclarator*
{
    return cld::match(
        value,
        [](const Syntax::DirectDeclaratorParentheses& parentheses) -> const Syntax::DirectDeclarator* {
            return &parentheses.getDeclarator().getDirectDeclarator();
        },
        [](const Syntax::DirectDeclaratorIdentifier&) -> const Syntax::DirectDeclarator* { return nullptr; },
        [](const auto& value) -> const Syntax::DirectDeclarator* { return &value.getDirectDeclarator(); });
};

constexpr auto ARRAY_TYPE_NEXT_FN = [](const Type& type) -> const Type*
{
    return type.match(
        [](auto&& value) -> const Type*
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                if (isArray(value.getType()))
                {
                    return &value.getType();
                }
            }
            return nullptr;
        });
};

constexpr auto TYPE_NEXT_FN = [](const Type& type) -> const Type*
{
    return type.match(
        [](const auto& value) -> const Type*
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return &value.getType();
            }
            else
            {
                return nullptr;
            }
        },
        [](const PointerType& pointerType) -> const Type* { return &pointerType.getElementType(); },
        [](const FunctionType& functionType) -> const Type* { return &functionType.getReturnType(); });
};
} // namespace cld::Semantics

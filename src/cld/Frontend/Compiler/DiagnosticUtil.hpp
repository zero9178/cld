#pragma once

#include "Lexer.hpp"

namespace cld::diag
{
namespace detail
{
template <class T, class = void>
struct IsIterable : std::false_type
{
};

template <class T>
struct IsIterable<T, std::void_t<decltype(std::begin(std::declval<T>()), std::end(std::declval<T>()))>> : std::true_type
{
};
} // namespace detail

struct PointLocation
{
    std::uint64_t offset;
    std::uint32_t fileId;
    std::uint32_t macroId;
};

using PointRange = std::pair<PointLocation, PointLocation>;

template <class T>
PointRange getPointRange(const T& arg)
{
    using U = std::decay_t<T>;
    if constexpr (std::is_same_v<PointRange, U>)
    {
        return arg;
    }
    else if constexpr (std::is_base_of_v<Lexer::TokenBase, U>)
    {
        return {{arg.getOffset(), arg.getFileId(), arg.getMacroId()},
                {arg.getOffset() + arg.getLength(), arg.getFileId(), arg.getMacroId()}};
    }
    else if constexpr (std::is_convertible_v<U, std::uint64_t>)
    {
        return {{static_cast<std::uint64_t>(arg), 0, 0}, {static_cast<std::uint64_t>(arg) + 1, 0, 0}};
    }
    else if constexpr (detail::IsIterable<T>{})
    {
        CLD_ASSERT(std::begin(arg) != std::end(arg));
        auto& first = *std::begin(arg);
        auto& last = *(std::end(arg) - 1);
        if constexpr (std::is_pointer_v<std::decay_t<decltype(first)>> || IsSmartPtr<std::decay_t<decltype(first)>>{})
        {
            return {getPointRange(*first).first, getPointRange(*last).second};
        }
        else
        {
            return {getPointRange(first).first, getPointRange(last).second};
        }
    }
    else if constexpr (IsVariant<U>{})
    {
        return cld::match(arg,
                          [](auto&& value)
                          {
                              using V = std::decay_t<decltype(value)>;
                              if constexpr (std::is_pointer_v<V> || IsSmartPtr<V>{})
                              {
                                  CLD_ASSERT(value);
                                  return getPointRange(*value);
                              }
                              else
                              {
                                  return getPointRange(value);
                              }
                          });
    }
    else if constexpr (std::tuple_size_v<U> == 2)
    {
        using T1 = std::decay_t<std::tuple_element_t<0, U>>;
        using T2 = std::decay_t<std::tuple_element_t<1, U>>;
        if constexpr (std::is_base_of_v<
                          Lexer::TokenBase,
                          std::remove_pointer_t<T1>> && std::is_base_of_v<Lexer::TokenBase, std::remove_pointer_t<T2>>)
        {
            auto& [arg1, arg2] = arg;
            PointLocation first, second;
            if constexpr (std::is_pointer_v<T1>)
            {
                CLD_ASSERT(arg1);
                first = {arg1->getOffset(), arg1->getFileId(), arg1->getMacroId()};
            }
            else
            {
                first = {arg1.getOffset(), arg1.getFileId(), arg1.getMacroId()};
            }
            if constexpr (std::is_pointer_v<T2>)
            {
                CLD_ASSERT(arg2);
                second = {arg2->getOffset() + arg2->getLength(), arg2->getFileId(), arg2->getMacroId()};
            }
            else
            {
                second = {arg2.getOffset() + arg2.getLength(), arg2.getFileId(), arg2.getMacroId()};
            }
            return {first, second};
        }
        else if constexpr (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_convertible_v<T2, std::uint64_t>)
        {
            auto& [arg1, arg2] = arg;
            return {{static_cast<std::uint64_t>(arg2), arg1.getFileId(), arg1.getMacroId()},
                    {static_cast<std::uint64_t>(arg2) + 1, arg1.getFileId(), arg1.getMacroId()}};
        }
        else if constexpr (std::is_base_of_v<Lexer::TokenBase, T2> && std::is_convertible_v<T1, std::uint64_t>)
        {
            auto& [arg1, arg2] = arg;
            return {{static_cast<std::uint64_t>(arg1), arg2.getFileId(), arg2.getMacroId()},
                    {static_cast<std::uint64_t>(arg1) + 1, arg2.getFileId(), arg2.getMacroId()}};
        }
        else if constexpr (std::is_convertible_v<T1, std::uint64_t> && std::is_convertible_v<T2, std::uint64_t>)
        {
            auto& [arg1, arg2] = arg;
            return {{static_cast<std::uint64_t>(arg1), 0, 0}, {static_cast<std::uint64_t>(arg2), 0, 0}};
        }
        else
        {
            auto& [arg1, arg2] = arg;
            auto first = getPointRange(arg1);
            auto second = getPointRange(arg2);
            return {first.first, second.second};
        }
    }
    else
    {
        using T1 = std::decay_t<std::tuple_element_t<0, U>>;
        if constexpr (std::is_base_of_v<Lexer::TokenBase, T1>)
        {
            auto& [arg1, arg2, arg3] = arg;
            return {{static_cast<std::uint64_t>(arg2), arg1.getFileId(), arg1.getMacroId()},
                    {static_cast<std::uint64_t>(arg3), arg1.getFileId(), arg1.getMacroId()}};
        }
        else
        {
            auto& [arg1, arg2, arg3] = arg;
            return {{static_cast<std::uint64_t>(arg1), arg3.getFileId(), arg3.getMacroId()},
                    {static_cast<std::uint64_t>(arg2), arg3.getFileId(), arg3.getMacroId()}};
        }
    }
}

} // namespace cld::diag

#pragma once

#include <optional>
#include <tuple>
#include <variant>

namespace cld
{
template <std::size_t... ints>
constexpr static auto integerSequenceToTuple(std::index_sequence<ints...>)
{
    return std::make_tuple(std::integral_constant<std::size_t, ints>{}...);
}

template <class T, typename = void>
struct IsTupleLike : std::false_type
{
};

template <class T>
struct IsTupleLike<T, std::void_t<typename std::tuple_size<T>::type>> : std::true_type
{
};

template <class T>
struct IsTuple : std::false_type
{
};

template <class... T>
struct IsTuple<std::tuple<T...>> : std::true_type
{
};

template <class T>
struct IsVariant : std::false_type
{
};

template <class... T>
struct IsVariant<std::variant<T...>> : std::true_type
{
};

template <class T>
struct IsUniquePtr : std::false_type
{
};

template <class T>
struct IsUniquePtr<std::unique_ptr<T>> : std::true_type
{
};

template <class T>
struct IsOptional : std::false_type
{
};

template <class T>
struct IsOptional<std::optional<T>> : std::true_type
{
};

template <class T>
struct IsSharedPtr : std::false_type
{
};

template <class T>
struct IsSharedPtr<std::shared_ptr<T>> : std::true_type
{
};

template <class T>
struct IsSmartPtr : std::disjunction<IsUniquePtr<T>, IsSharedPtr<T>>
{
};

template <class T, class U, typename = void>
struct IsEqualComparable : std::false_type
{
};

template <class T, class U>
struct IsEqualComparable<T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>> : std::true_type
{
};

template <typename, typename = void>
constexpr bool IsTypeCompleteV = false;

template <typename T>
constexpr bool IsTypeCompleteV<T, std::void_t<decltype(sizeof(T))>> = true;

} // namespace cld

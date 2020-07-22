#pragma once

#include <Frontend/Common/Text.hpp>
#include <Frontend/Compiler/SourceInterface.hpp>

#include <type_traits>

namespace cld::diag
{
namespace detail
{
template <class T, class = void>
struct StringConstraintCheck : std::false_type
{
};

template <class T>
struct StringConstraintCheck<T, std::void_t<decltype(::cld::to_string(std::declval<std::decay_t<T>>()))>>
    : std::true_type
{
};
} // namespace detail

namespace StringConverters
{
template <class T>
std::enable_if_t<(detail::StringConstraintCheck<T>{}), std::string> inFormat(const T& arg, const SourceInterface&)
{
    return cld::to_string(arg);
}

template <class T>
std::enable_if_t<(detail::StringConstraintCheck<T>{}), std::string> inArg(const T& arg, const SourceInterface&)
{
    return cld::to_string(arg);
}
} // namespace StringConverters

template <char32_t... name>
struct CustomFormat;

template <>
struct CustomFormat<U's'>
{
    // Greedily accept everything so we can properly error
    template <class T>
    std::string operator()(T integer) const
    {
        static_assert(std::is_integral_v<std::decay_t<T>>, "Argument to %s must be an integer type");
        if (integer != 1)
        {
            return "s";
        }
        return "";
    }
};
} // namespace cld::diag

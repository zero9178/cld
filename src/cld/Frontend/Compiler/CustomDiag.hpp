#pragma once

#include <cld/Common/Text.hpp>

#include <type_traits>

namespace cld
{
class SourceInterface;

namespace diag
{
template <class T>
struct StringConverter
{
    static std::string inFormat(const T& arg, const SourceInterface&)
    {
        return cld::to_string(arg);
    }

    static std::string inArg(const T& arg, const SourceInterface&)
    {
        return cld::to_string(arg);
    }
};

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
} // namespace diag
} // namespace cld

#pragma once

#include <cld/Support/IntrVarValue.hpp>
#include <cld/Support/Text.hpp>

#include <type_traits>

namespace cld
{
class SourceInterface;

namespace diag
{
template <class T>
struct StringConverter
{
    static std::string inFormat(const T& arg, const SourceInterface*)
    {
        return cld::to_string(arg);
    }

    static std::string inArg(const T& arg, const SourceInterface*)
    {
        return cld::to_string(arg);
    }
};

template <class T>
struct StringConverter<IntrVarValue<T>>
{
    static std::string inFormat(const IntrVarValue<T>& arg, const SourceInterface* sourceInterface)
    {
        return StringConverter<T>::inFormat(arg, sourceInterface);
    }

    static std::string inArg(const IntrVarValue<T>& arg, const SourceInterface* sourceInterface)
    {
        return StringConverter<T>::inArg(arg, sourceInterface);
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

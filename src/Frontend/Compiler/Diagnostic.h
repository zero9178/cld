#pragma once

#include <llvm/Support/Unicode.h>

#include <ctre.hpp>

#include "Message.hpp"

namespace cld
{
namespace detail
{
constexpr static auto DIAG_ARG_PATTERN = ctll::fixed_string{"%[^\\s\\\\]*(\\d)"};

#pragma clang diagnostic push
#pragma ide diagnostic ignored "llvm-qualified-auto"
template <class... Args>
constexpr std::int64_t getBiggestPercentArg(std::u32string_view text)
{
    std::int64_t max = -1;
    while (auto result = ctre::search<DIAG_ARG_PATTERN>(text))
    {
        max = std::max<std::int64_t>(max, result.template get<1>().view()[0] - '0');
        const auto end = result.get_end_position();
        text.remove_prefix(end - text.begin());
    }
    std::int64_t sizes[] = {max, *std::max_element(Args::indices.begin(), Args::indices.end())...};
    return *std::max_element(std::begin(sizes), std::end(sizes)) + 1;
}

template <std::size_t N, class... Args>
constexpr bool checkForNoHoles(std::u32string_view text)
{
    if constexpr (N == 0)
    {
        return true;
    }
    else
    {
        bool seen[N] = {false};
        while (auto result = ctre::search<DIAG_ARG_PATTERN>(text))
        {
            auto index = result.template get<1>().view()[0] - '0';
            seen[index] = true;
            const auto end = result.get_end_position();
            text.remove_prefix(end - text.begin());
        }
        (([&seen](auto&& array) {
             for (auto iter : array)
             {
                 seen[iter] = true;
             }
         }(Args::indices)),
         ...);
        for (auto iter : seen)
        {
            if (!iter)
            {
                return false;
            }
        }
        return true;
    }
}
#pragma clang diagnostic pop

} // namespace detail

template <std::size_t n, char character = '~'>
struct Underline
{
    constexpr static std::array<std::size_t, 1> indices = {n};
    constexpr static std::size_t affects = n;
};

template <std::size_t n>
struct PointAt
{
    constexpr static std::array<std::size_t, 1> indices = {n};
    constexpr static std::size_t affects = n;
};

template <std::size_t token, std::int64_t text = -1>
struct InsertAfter
{
private:
    constexpr static std::array<std::size_t, 1 + (text >= 0 ? 1 : 0)> getIndices()
    {
        if constexpr (text >= 0)
        {
            return {token, text};
        }
        else
        {
            return {token};
        }
    }

public:
    constexpr static std::array<std::size_t, 1 + (text >= 0 ? 1 : 0)> indices = getIndices();
    constexpr static std::size_t affects = token;
};

template <std::size_t token, std::size_t text>
struct Annotate
{
    constexpr static std::array<std::size_t, 2> indices = {token, text};
    constexpr static std::size_t affects = token;
};

template <std::size_t N, auto& format, class... Mods>
class Diagnostic
{
    Severity m_category;
    std::string_view m_name;

public:
    constexpr Diagnostic(Severity mCategory, std::string_view name) : m_category(mCategory), m_name(name) {}

    constexpr static std::size_t getSize()
    {
        return N;
    }

    constexpr static std::u32string_view getFormat()
    {
        return {format.begin(), format.size()};
    }

    template <class... Args>
    Message args(const SourceInterface& sourceInterface, Args&&... args)
    {
        static_assert(sizeof...(Args) == N, "Not the same amount of argument as needed by the format");
    }
};

template <const auto& text, class... Args>
constexpr auto makeDiagnostic(Severity category, std::string_view name)
{
    constexpr auto n = detail::getBiggestPercentArg<Args...>({text.begin(), text.size()});
    constexpr bool tmp = detail::checkForNoHoles<n, Args...>({text.begin(), text.size()});
    static_assert(tmp, "Not allowed to have any holes between % indices");
    return Diagnostic<n, text, Args...>(category, name);
}

} // namespace cld

#pragma once

#include <llvm/Support/Unicode.h>

#include <Frontend/Common/Text.hpp>
#include <Frontend/Common/Util.hpp>

#include <optional>

#include <ctre.hpp>

#include "Message.hpp"

namespace cld
{
namespace detail
{
constexpr static auto DIAG_ARG_PATTERN = ctll::fixed_string{"%([^\\s\\\\]*)(\\d)"};

#pragma clang diagnostic push
#pragma ide diagnostic ignored "llvm-qualified-auto"
template <class... Args>
constexpr std::int64_t getBiggestPercentArg(std::u32string_view text)
{
    std::int64_t max = -1;
    while (auto result = ctre::search<DIAG_ARG_PATTERN>(text))
    {
        max = std::max<std::int64_t>(max, result.template get<2>().view().back() - '0');
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
            auto index = result.template get<2>().view().back() - '0';
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

template <template <auto...> class Template, typename T>
struct IsSpecializationOf : std::false_type
{
};

template <template <auto...> class Template, auto... Args>
struct IsSpecializationOf<Template, Template<Args...>> : std::true_type
{
};

class DiagnosticBase
{
    Severity m_severity;

public:
    struct PointLocation
    {
        std::uint64_t offset;
        std::uint32_t fileId;
        std::uint32_t macroId;
    };

protected:
    struct Argument
    {
        std::optional<PointLocation> location;
        std::optional<std::string> text;
        std::optional<std::uint64_t> integral;
    };

    cld::Message print(PointLocation location, std::string_view message, llvm::ArrayRef<Argument> arguments,
                       const SourceInterface& sourceInterface) const;

public:
    constexpr DiagnosticBase(Severity severity) : m_severity(severity) {}

    constexpr Severity getSeverity() const noexcept
    {
        return m_severity;
    }
};

} // namespace detail

template <std::size_t n, char character = '~'>
struct Underline
{
    constexpr static std::array<std::size_t, 1> indices = {n};
    constexpr static std::size_t affects = n;
};

template <std::size_t n>
using PointAt = Underline<n, '^'>;

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
class Diagnostic : public detail::DiagnosticBase
{
    enum Constraint : std::uint8_t
    {
        LocationConstraint = 0b1,
        StringConstraint = 0b10,
        IntegerConstraint = 0b100,
    };

    constexpr static std::array<std::underlying_type_t<Constraint>, N> getConstraints();

    template <class T>
    static std::string getString(const T& arg)
    {
        return cld::to_string(arg);
    }

    // TODO: Replace all those things with Concepts once the Codebase is C++20
    template <class T, class = void>
    struct StringConstraintCheck : std::false_type
    {
    };

    template <class T>
    struct StringConstraintCheck<T, std::void_t<decltype(getString(std::declval<T>()))>> : std::true_type
    {
    };

    template <typename T, typename = void>
    struct IsIterable : std::false_type
    {
    };

    template <typename T>
    struct IsIterable<T, std::void_t<decltype(std::begin(std::declval<T>())), decltype(std::end(std::declval<T>()))>>
        : std::true_type
    {
    };

    template <class T, std::enable_if_t<IsIterable<T>{}>* = nullptr>
    constexpr static bool iteratesOverTokens()
    {
        return std::is_base_of_v<cld::Lexer::TokenBase,
                                 typename std::iterator_traits<decltype(std::begin(std::declval<T>()))>::value_type>;
    }

    template <class T, std::enable_if_t<!IsIterable<T>{}>* = nullptr>
    constexpr static bool iteratesOverTokens()
    {
        return false;
    }

    template <class T, std::enable_if_t<IsIterable<T>{}>* = nullptr>
    constexpr static bool iteratesOverIntegrals()
    {
        return std::is_convertible_v<typename std::iterator_traits<decltype(std::begin(std::declval<T>()))>::value_type,
                                     std::int64_t>;
    }

    template <class T, std::enable_if_t<!IsIterable<T>{}>* = nullptr>
    constexpr static bool iteratesOverIntegrals()
    {
        return false;
    }

    template <class>
    struct IsPair : std::false_type
    {
    };

    template <class T, class U>
    struct IsPair<std::pair<T, U>> : std::true_type
    {
    };

    template <class T, std::enable_if_t<IsPair<T>{}>* = nullptr>
    constexpr static bool isPairComboOfTheseThings()
    {
        using T1 = std::tuple_element_t<0, T>;
        using T2 = std::tuple_element_t<1, T>;
        return (std::is_base_of_v<cld::Lexer::TokenBase, T1> && iteratesOverIntegrals<T2>())
               || (std::is_base_of_v<cld::Lexer::TokenBase, T2> && iteratesOverIntegrals<T1>());
    }

    template <class T, std::enable_if_t<!IsPair<T>{}>* = nullptr>
    constexpr static bool isPairComboOfTheseThings()
    {
        return false;
    }

    template <class T>
    constexpr static bool locationConstraintCheck()
    {
        return std::is_base_of_v<cld::Lexer::TokenBase, T> || iteratesOverTokens<T>() || iteratesOverIntegrals<T>()
               || isPairComboOfTheseThings<T>();
    }

    template <std::size_t argumentIndex, std::underlying_type_t<Constraint> constraint, class T>
    struct ConstraintCheck
    {
        static_assert(!(constraint & StringConstraint) || StringConstraintCheck<T>{},
                      "Argument must be convertible to string");
        static_assert(!(constraint & IntegerConstraint) || std::is_convertible_v<T, std::int64_t>,
                      "Argument must be an integer type");
        static_assert(!(constraint & LocationConstraint) || locationConstraintCheck<T>(),
                      "Argument must denote a location or location range");
    };

    template <class T, std::enable_if_t<IsPair<T>{}>* = nullptr>
    constexpr static bool isPointLocationPair()
    {
        using T1 = std::tuple_element_t<0, T>;
        using T2 = std::tuple_element_t<1, T>;
        return (std::is_base_of_v<cld::Lexer::TokenBase, T1> && std::is_convertible_v<T2, std::int64_t>)
               || (std::is_base_of_v<cld::Lexer::TokenBase, T2> && std::is_convertible_v<T1, std::int64_t>);
    }

    template <class T, std::enable_if_t<!IsPair<T>{}>* = nullptr>
    constexpr static bool isPointLocationPair()
    {
        return false;
    }

    template <class T>
    static PointLocation getPointLocation(const T& arg)
    {
        if constexpr (std::is_convertible_v<T, std::uint64_t>)
        {
            return {static_cast<std::uint64_t>(arg), 0, 0};
        }
        else if constexpr (std::is_base_of_v<Lexer::TokenBase, T>)
        {
            return {arg.getOffset(), arg.getFileId(), arg.getMacroId()};
        }
        else
        {
            static_assert(
                isPointLocationPair<T>(),
                "Location must be an integer type, a subtype of Lexer::TokenBase or a std::pair of the previous two");
            if constexpr (std::is_integral_v<std::tuple_element_t<0, T>>)
            {
                return {static_cast<std::uint64_t>(arg.second.getOffset() + arg.first), arg.second.getFileId(),
                        arg.second.getMacroId()};
            }
            else
            {
                return {static_cast<std::uint64_t>(arg.first.getOffset() + arg.second), arg.first.getFileId(),
                        arg.first.getMacroId()};
            }
        }
    }

    template <auto&, std::size_t>
    constexpr static void checkConstraints()
    {
    }

    template <auto& array, std::size_t i, class Curr, class... Args>
    constexpr static void checkConstraints()
    {
        [[maybe_unused]] auto v = ConstraintCheck<i, array[i], Curr>{};
        checkConstraints<array, i + 1, Args...>();
    }

    template <class Tuple, std::size_t... ints>
    static std::array<Argument, N> createArgumentArray(Tuple&& args, std::index_sequence<ints...>);

    std::string_view m_name;

public:
    constexpr Diagnostic(Severity severity, std::string_view name) : detail::DiagnosticBase(severity), m_name(name) {}

    constexpr static std::size_t getSize()
    {
        return N;
    }

    constexpr static std::u32string_view getFormat()
    {
        return {format.begin(), format.size()};
    }

    template <class T, class... Args>
    Message args(const T& location, const SourceInterface& sourceInterface, Args&&... args) const;

private:
    constexpr static auto constraints = getConstraints();
};

template <const auto& text, class... Args>
constexpr auto makeDiagnostic(Severity category, std::string_view name)
{
    constexpr auto n = detail::getBiggestPercentArg<Args...>({text.begin(), text.size()});
    constexpr bool tmp = detail::checkForNoHoles<n, Args...>({text.begin(), text.size()});
    static_assert(tmp, "Not allowed to have any holes between % indices");
    return Diagnostic<n, text, Args...>(category, name);
}

template <std::size_t N, auto& format, class... Mods>
template <class T, class... Args>
Message Diagnostic<N, format, Mods...>::args(const T& location, const SourceInterface& sourceInterface,
                                             Args&&... args) const
{
    static_assert(sizeof...(Args) == N, "Not the same amount of argument as needed by the format");
    this->checkConstraints<constraints, 0, Args...>();
    if (sourceInterface.getLanguageOptions().disabledWarnings.count(cld::to_string(m_name)))
    {
        return {};
    }
    constexpr auto u32string = getFormat();
    const char32_t* start = u32string.data();
    std::array<char, u32string.size() * 4> result;
    char* resStart = result.data();
    auto ret =
        llvm::ConvertUTF32toUTF8(reinterpret_cast<const llvm::UTF32**>(&start),
                                 reinterpret_cast<const llvm::UTF32*>(u32string.data() + u32string.size()),
                                 reinterpret_cast<llvm::UTF8**>(&resStart),
                                 reinterpret_cast<llvm::UTF8*>(result.data() + result.size()), llvm::strictConversion);
    (void)ret;
    CLD_ASSERT(ret == llvm::conversionOK);
    auto array =
        createArgumentArray(std::forward_as_tuple(std::forward<Args>(args)...), std::index_sequence_for<Args...>{});
    return print(getPointLocation(location), {result.data(), static_cast<std::size_t>(resStart - result.data())}, array,
                 sourceInterface);
}

template <std::size_t N, auto& format, class... Mods>
constexpr auto Diagnostic<N, format, Mods...>::getConstraints() -> std::array<std::underlying_type_t<Constraint>, N>
{
    std::array<std::underlying_type_t<Constraint>, N> result{};
    auto text = getFormat();
    while (auto search = ctre::search<detail::DIAG_ARG_PATTERN>(text))
    {
        auto index = search.template get<2>().view().back() - '0';
        auto mods = search.template get<1>().view();
        if (mods.empty())
        {
            result[index] |= Constraint::StringConstraint;
        }
        else if (mods == U"s")
        {
            result[index] |= Constraint::IntegerConstraint;
        }
        else
        {
            CLD_UNREACHABLE;
        }
        const auto end = search.get_end_position();
        text.remove_prefix(end - text.begin());
    }
    (
        [&result](auto&& value) {
            using T = std::decay_t<decltype(value)>;
            if constexpr (detail::IsSpecializationOf<Underline, T>{})
            {
                result[T::affects] |= Constraint::LocationConstraint;
            }
            else if constexpr (detail::IsSpecializationOf<InsertAfter, T>{})
            {
                result[T::affects] |= Constraint::LocationConstraint;
                if constexpr (T::indices.size() > 1)
                {
                    result[T::indices[1]] |= Constraint::StringConstraint;
                }
            }
            else if constexpr (detail::IsSpecializationOf<Annotate, T>{})
            {
                result[T::indices[0]] |= Constraint::LocationConstraint;
                result[T::indices[1]] |= Constraint::StringConstraint;
            }
        }(Mods{}),
        ...);
    return result;
}

template <std::size_t N, auto& format, class... Mods>
template <class Tuple, std::size_t... ints>
auto Diagnostic<N, format, Mods...>::createArgumentArray(Tuple&& args, std::index_sequence<ints...>)
    -> std::array<Argument, N>
{
    std::array<Argument, N> result;
    (
        [&result, &args](auto integer) {
            constexpr auto i = decltype(integer)::value;
            if constexpr ((bool)(constraints[i] & Constraint::LocationConstraint))
            {
                // TODO:
            }
            if constexpr ((bool)(constraints[i] & Constraint::StringConstraint))
            {
                result[i].text = getString(std::get<i>(args));
            }
            if constexpr ((bool)(constraints[i] & Constraint::IntegerConstraint))
            {
                result[i].integral = static_cast<std::int64_t>(std::get<i>(args));
            }
        }(std::integral_constant<std::size_t, ints>{}),
        ...);
    return result;
}

} // namespace cld

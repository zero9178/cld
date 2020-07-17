#pragma once

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/Unicode.h>

#include <Frontend/Common/Text.hpp>
#include <Frontend/Common/Util.hpp>

#include <optional>

#include <ctre.hpp>

#include "Message.hpp"

namespace cld
{
template <std::size_t index, std::int64_t text = -1>
struct InsertAfter;

template <std::size_t index, std::size_t text>
struct Annotate;

template <std::size_t index, char character = '~', bool continuous = true>
struct Underline;

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
        std::array<bool, N> seen = {{false}};
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

// Got these due to a bug in VS not allowing to make a IsSpecializationOf struct

template <class T>
struct IsUnderline : std::false_type
{
};

template <auto... Args>
struct IsUnderline<Underline<Args...>> : std::true_type
{
};

template <class T>
struct IsInsertAfter : std::false_type
{
};

template <auto... Args>
struct IsInsertAfter<InsertAfter<Args...>> : std::true_type
{
};

template <class T>
struct IsAnnotate : std::false_type
{
};

template <auto... Args>
struct IsAnnotate<Annotate<Args...>> : std::true_type
{
};

template <std::size_t N, class... Args>
constexpr bool checkForNoDuplicates()
{
    if constexpr (N == 0)
    {
        return true;
    }
    else
    {
        std::array<bool, N> seen = {{false}};
        return (([&seen](auto value) {
                    using T = decltype(value);
                    if constexpr (IsInsertAfter<T>{})
                    {
                        return true;
                    }
                    constexpr auto index = T::affects;
                    if (seen[index])
                    {
                        return false;
                    }
                    seen[index] = true;
                    return true;
                }(Args{}))
                && ...);
    }
}
#pragma clang diagnostic pop

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
    struct Underline
    {
        std::size_t index;
        char characters;
        bool continuous;
    };

    struct InsertAfter
    {
        std::size_t index;
        std::int64_t text;
    };

    struct Annotate
    {
        std::size_t index;
        std::size_t text;
    };

    using Modifiers = std::variant<Underline, InsertAfter, Annotate>;

    struct Argument
    {
        std::optional<std::pair<PointLocation, PointLocation>> range;
        std::optional<std::string> text;
        std::optional<std::uint64_t> integral;
    };

    Message print(std::pair<PointLocation, PointLocation> location, std::string_view message,
                  llvm::MutableArrayRef<Argument> arguments, llvm::ArrayRef<Modifiers> modifiers,
                  const SourceInterface& sourceInterface) const;

public:
    constexpr DiagnosticBase(Severity severity) : m_severity(severity) {}

    constexpr Severity getSeverity() const noexcept
    {
        return m_severity;
    }
};

} // namespace detail

// Move the non-type template parameters to become members in C++20

template <std::size_t index, char character, bool continuous>
struct Underline
{
    constexpr static std::array<std::size_t, 1> indices = {index};
    constexpr static std::size_t affects = index;

    [[nodiscard]] constexpr static std::size_t getIndex() noexcept
    {
        return index;
    }

    [[nodiscard]] constexpr static char getCharacter() noexcept
    {
        return character;
    }

    [[nodiscard]] constexpr static bool isContinuous() noexcept
    {
        return continuous;
    }
};

template <std::size_t index>
using PointAt = Underline<index, '^', false>;

template <std::size_t index, std::int64_t text>
struct InsertAfter
{
private:
    constexpr static std::array<std::size_t, 1 + (text >= 0 ? 1 : 0)> getIndices()
    {
        if constexpr (text >= 0)
        {
            return {index, text};
        }
        else
        {
            return {index};
        }
    }

public:
    constexpr static std::array<std::size_t, 1 + (text >= 0 ? 1 : 0)> indices = getIndices();
    constexpr static std::size_t affects = index;

    [[nodiscard]] constexpr static std::size_t getIndex() noexcept
    {
        return index;
    }

    [[nodiscard]] constexpr static std::int64_t getText() noexcept
    {
        return text;
    }
};

template <std::size_t index, std::size_t text>
struct Annotate
{
    constexpr static std::array<std::size_t, 2> indices = {index, text};
    constexpr static std::size_t affects = index;

    [[nodiscard]] constexpr static std::size_t getIndex() noexcept
    {
        return index;
    }

    [[nodiscard]] constexpr static std::size_t getText() noexcept
    {
        return text;
    }
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

    template <std::size_t... ints>
    constexpr static std::array<Modifiers, sizeof...(Mods)> getModifiers(std::index_sequence<ints...>);

    template <class T>
    static std::string getString(const T& arg)
    {
        return to_string(arg);
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

    template <class T, typename = void>
    struct IsTupleLike : std::false_type
    {
    };

    template <class T>
    struct IsTupleLike<T, std::void_t<typename std::tuple_size<T>::type>> : std::true_type
    {
    };

    template <class T>
    constexpr static bool locationConstraintCheck()
    {
        if constexpr (IsTupleLike<T>{})
        {
            if constexpr (std::tuple_size_v<T> == 2)
            {
                using T1 = std::tuple_element_t<0, T>;
                using T2 = std::tuple_element_t<1, T>;
                return (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_convertible_v<T2, std::uint64_t>)
                       || (std::is_base_of_v<Lexer::TokenBase, T2> && std::is_convertible_v<T1, std::uint64_t>)
                       || (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_base_of_v<Lexer::TokenBase, T2>)
                       || (std::is_convertible_v<T1, std::uint64_t> && std::is_convertible_v<T2, std::uint64_t>);
            }
            else if constexpr (std::tuple_size_v<T> == 3)
            {
                using T1 = std::tuple_element_t<0, T>;
                using T2 = std::tuple_element_t<1, T>;
                using T3 = std::tuple_element_t<2, T>;
                return (std::is_base_of_v<
                            Lexer::TokenBase,
                            T1> && std::is_convertible_v<T2, std::uint64_t> && std::is_convertible_v<T3, std::uint64_t>)
                       || (std::is_base_of_v<
                               Lexer::TokenBase,
                               T3> && std::is_convertible_v<T2, std::uint64_t> && std::is_convertible_v<T1, std::uint64_t>);
            }
            else
            {
                return false;
            }
        }
        else
        {
            return std::is_base_of_v<Lexer::TokenBase, T> || std::is_convertible_v<T, std::uint64_t>;
        }
    }

    template <std::size_t argumentIndex, std::underlying_type_t<Constraint> constraint, class T>
    struct ConstraintCheck
    {
        static_assert(!(constraint & StringConstraint) || StringConstraintCheck<T>{},
                      "Argument must be convertible to string");
        static_assert(!(constraint & IntegerConstraint) || std::is_convertible_v<T, std::int64_t>,
                      "Argument must be an integer type");
        static_assert(!(constraint & LocationConstraint) || locationConstraintCheck<T>(),
                      "Argument must denote a location range");
    };

    template <class T>
    static std::pair<PointLocation, PointLocation> getPointRange(const T& arg)
    {
        if constexpr (std::is_base_of_v<Lexer::TokenBase, T>)
        {
            return {{arg.getOffset(), arg.getFileId(), arg.getMacroId()},
                    {arg.getOffset() + arg.getLength(), arg.getFileId(), arg.getMacroId()}};
        }
        else if constexpr (std::is_convertible_v<T, std::uint64_t>)
        {
            return {{static_cast<std::uint64_t>(arg), 0, 0}, {static_cast<std::uint64_t>(arg) + 1, 0, 0}};
        }
        else if constexpr (std::tuple_size_v<T> == 2)
        {
            using T1 = std::tuple_element_t<0, T>;
            using T2 = std::tuple_element_t<1, T>;
            if constexpr (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_base_of_v<Lexer::TokenBase, T2>)
            {
                auto& [arg1, arg2] = arg;
                return {{arg1.getOffset(), arg1.getFileId(), arg1.getMacroId()},
                        {arg2.getOffset() + arg2.getLength(), arg2.getFileId(), arg2.getMacroId()}};
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
        }
        else
        {
            using T1 = std::tuple_element_t<0, T>;
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

    constexpr static auto modifiers = getModifiers(std::index_sequence_for<Mods...>{});
};

template <const auto& text, class... Args>
constexpr auto makeDiagnostic(Severity category, std::string_view name)
{
    constexpr auto n = detail::getBiggestPercentArg<Args...>({text.begin(), text.size()});
    constexpr bool tmp = detail::checkForNoHoles<n, Args...>({text.begin(), text.size()});
    static_assert(tmp, "Not allowed to have any holes between % indices");
    constexpr bool duplicates = detail::checkForNoDuplicates<n, Args...>();
    static_assert(duplicates, "Not allowed to have any duplicate indices in Modifiers");
    return Diagnostic<n, text, Args...>(category, name);
}

template <std::size_t N, auto& format, class... Mods>
template <class T, class... Args>
Message Diagnostic<N, format, Mods...>::args(const T& location, const SourceInterface& sourceInterface,
                                             Args&&... args) const
{
    static_assert(sizeof...(Args) == N, "Not the same amount of argument as needed by the format");
    this->checkConstraints<constraints, 0, Args...>();
    static_assert(locationConstraintCheck<T>(), "First argument must denote a location");
    if (sourceInterface.getLanguageOptions().disabledWarnings.count(to_string(m_name)))
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
    return print(getPointRange(location), {result.data(), static_cast<std::size_t>(resStart - result.data())}, array,
                 modifiers, sourceInterface);
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
        [&result](auto value) {
            using T = decltype(value);
            if constexpr (detail::IsUnderline<T>{})
            {
                result[T::affects] |= Constraint::LocationConstraint;
            }
            else if constexpr (detail::IsAnnotate<T>{})
            {
                result[T::indices[0]] |= Constraint::LocationConstraint;
                result[T::indices[1]] |= Constraint::StringConstraint;
            }
            else if constexpr (detail::IsInsertAfter<T>{})
            {
                result[T::affects] |= Constraint::LocationConstraint;
                if constexpr (T::indices.size() > 1)
                {
                    result[T::indices[1]] |= Constraint::StringConstraint;
                }
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
                result[i].range = getPointRange(std::get<i>(args));
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

template <std::size_t N, auto& format, class... Mods>
template <std::size_t... ints>
constexpr auto Diagnostic<N, format, Mods...>::getModifiers(std::index_sequence<ints...>)
    -> std::array<Modifiers, sizeof...(Mods)>
{
    std::array<Modifiers, sizeof...(Mods)> result = {};
    (
        [&result](auto value) {
            constexpr std::size_t i = decltype(value)::value;
            using T = std::tuple_element_t<i, std::tuple<Mods...>>;
            if constexpr (detail::IsUnderline<T>{})
            {
                result[i] = Modifiers{DiagnosticBase::Underline{T::getIndex(), T::getCharacter(), T::isContinuous()}};
            }
            else if constexpr (detail::IsAnnotate<T>{})
            {
                result[i] = Modifiers{DiagnosticBase::Annotate{T::getIndex(), T::getText()}};
            }
            else if constexpr (detail::IsInsertAfter<T>{})
            {
                result[i] = Modifiers{DiagnosticBase::InsertAfter{T::getIndex(), T::getText()}};
            }
        }(std::integral_constant<std::size_t, ints>{}),
        ...);
    return result;
}

} // namespace cld

#pragma once

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/Unicode.h>

#include <Frontend/Common/Text.hpp>
#include <Frontend/Common/Util.hpp>

#include <array>
#include <optional>
#include <unordered_map>

#include <ctre.hpp>

#include "CustomDiag.hpp"
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

template <std::size_t index, char character, bool continuous>
struct IsUnderline<Underline<index, character, continuous>> : std::true_type
{
};

template <class T>
struct IsInsertAfter : std::false_type
{
};

template <std::size_t index, std::int64_t text>
struct IsInsertAfter<InsertAfter<index, text>> : std::true_type
{
};

template <class T>
struct IsAnnotate : std::false_type
{
};

template <std::size_t index, std::size_t text>
struct IsAnnotate<Annotate<index, text>> : std::true_type
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
        char character;
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
        std::optional<std::string> inFormatText;
        std::optional<std::string> inArgText;
        std::unordered_map<std::u32string_view, std::string> customModifiers;
    };

    Message print(std::pair<PointLocation, PointLocation> location, std::string_view message,
                  llvm::MutableArrayRef<Argument> arguments, llvm::ArrayRef<Modifiers> modifiers,
                  const SourceInterface& sourceInterface) const;

    static std::string stringFromToken(const SourceInterface& sourceInterface, const Lexer::TokenBase& token);

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
public:
    enum Constraint : std::uint8_t
    {
        LocationConstraint = 0b1,
        StringConstraint = 0b10,
        CustomConstraint = 0b1000,
    };

private:
    template <std::size_t... ints>
    constexpr static auto integerSequenceToTuple(std::index_sequence<ints...>)
    {
        return std::make_tuple(std::integral_constant<std::size_t, ints>{}...);
    }

    constexpr static std::array<std::underlying_type_t<Constraint>, N> getConstraints();

    template <std::size_t... ints>
    constexpr static std::array<Modifiers, sizeof...(Mods)> getModifiers(std::index_sequence<ints...>);

    // TODO: Replace all those things with Concepts once the Codebase is C++20

    template <class T, class = void>
    struct IsIterable : std::false_type
    {
    };

    template <class T>
    struct IsIterable<T, std::void_t<decltype(std::begin(std::declval<T>()), std::end(std::declval<T>()))>>
        : std::true_type
    {
    };

    template <class T>
    constexpr static bool locationConstraintCheck()
    {
        using U = std::decay_t<T>;
        if constexpr (IsVariant<U>{})
        {
            constexpr auto size = std::variant_size_v<U>;
            return std::apply(
                [](auto&&... values) {
                    return ([](auto indexT) {
                        constexpr auto index = decltype(indexT)::value;
                        // Don't usually allow pointers except in variants.
                        using V = std::remove_pointer_t<std::variant_alternative_t<index, U>>;
                        return locationConstraintCheck<V>();
                    }(values)
                            && ...);
                },
                integerSequenceToTuple(std::make_index_sequence<size>{}));
        }
        else if constexpr (IsTupleLike<U>{})
        {
            if constexpr (std::tuple_size_v<U> == 2)
            {
                using T1 = std::decay_t<std::tuple_element_t<0, U>>;
                using T2 = std::decay_t<std::tuple_element_t<1, U>>;
                return (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_convertible_v<T2, std::uint64_t>)
                       || (std::is_base_of_v<Lexer::TokenBase, T2> && std::is_convertible_v<T1, std::uint64_t>)
                       || (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_base_of_v<Lexer::TokenBase, T2>)
                       || (std::is_convertible_v<T1, std::uint64_t> && std::is_convertible_v<T2, std::uint64_t>);
            }
            else if constexpr (std::tuple_size_v<U> == 3)
            {
                using T1 = std::decay_t<std::tuple_element_t<0, U>>;
                using T2 = std::decay_t<std::tuple_element_t<1, U>>;
                using T3 = std::decay_t<std::tuple_element_t<2, U>>;
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
        else if constexpr (IsIterable<U>{})
        {
            using ValueType = typename std::iterator_traits<decltype(std::begin(std::declval<U>()))>::value_type;
            return locationConstraintCheck<std::remove_pointer_t<ValueType>>();
        }
        else
        {
            return std::is_base_of_v<Lexer::TokenBase, U> || std::is_convertible_v<U, std::uint64_t>;
        }
    }

    template <class T, class = void>
    struct HasStringConverters : std::false_type
    {
    };

    template <class T>
    struct HasStringConverters<
        T, std::void_t<decltype(
               diag::StringConverter<T>::inFormat(std::declval<T>(), std::declval<const SourceInterface&>()),
               diag::StringConverter<T>::inArg(std::declval<T>(), std::declval<const SourceInterface&>()))>>
        : std::true_type
    {
    };

    template <std::size_t argumentIndex, std::underlying_type_t<Constraint> constraint, class T>
    struct ConstraintCheck
    {
        static_assert(!(constraint & StringConstraint) || HasStringConverters<std::decay_t<T>>{},
                      "Argument must be convertible to string");
        static_assert(!(constraint & LocationConstraint) || locationConstraintCheck<T>(),
                      "Argument must denote a location range");
    };

    template <class T>
    static std::pair<PointLocation, PointLocation> getPointRange(const T& arg)
    {
        using U = std::decay_t<T>;
        if constexpr (std::is_base_of_v<Lexer::TokenBase, U>)
        {
            return {{arg.getOffset(), arg.getFileId(), arg.getMacroId()},
                    {arg.getOffset() + arg.getLength(), arg.getFileId(), arg.getMacroId()}};
        }
        else if constexpr (std::is_convertible_v<U, std::uint64_t>)
        {
            return {{static_cast<std::uint64_t>(arg), 0, 0}, {static_cast<std::uint64_t>(arg) + 1, 0, 0}};
        }
        else if constexpr (IsIterable<T>{})
        {
            CLD_ASSERT(std::begin(arg) != std::end(arg));
            auto& first = *std::begin(arg);
            auto& last = *(std::end(arg) - 1);
            if constexpr (std::is_pointer_v<std::decay_t<decltype(first)>>)
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
            return cld::match(arg, [](auto&& value) {
                using V = std::decay_t<decltype(value)>;
                if constexpr (std::is_pointer_v<V>)
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
            else
            {
                CLD_UNREACHABLE;
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
    static std::array<Argument, N> createArgumentArray(const SourceInterface& sourceInterface, Tuple&& args,
                                                       std::index_sequence<ints...>);

    std::string_view m_name;

    template <std::size_t i>
    constexpr static auto customModifiersFor()
    {
        constexpr std::size_t amountOfCustomModifiers = [] {
            std::size_t count = 0;
            auto text = getFormat();
            while (auto result = ctre::search<detail::DIAG_ARG_PATTERN>(text))
            {
                const auto end = result.get_end_position();
                text.remove_prefix(end - text.begin());
                auto index = result.template get<2>().view().back() - '0';
                auto mods = result.template get<1>().view();
                if (index != i || mods.empty())
                {
                    continue;
                }
                count++;
            }
            return count;
        }();
        std::array<std::u32string_view, amountOfCustomModifiers> result{};
        std::size_t count = 0;
        auto text = getFormat();
        while (auto search = ctre::search<detail::DIAG_ARG_PATTERN>(text))
        {
            const auto end = search.get_end_position();
            text.remove_prefix(end - text.begin());
            auto index = search.template get<2>().view().back() - '0';
            auto mods = search.template get<1>().view();
            if (index != i || mods.empty())
            {
                continue;
            }
            result[count++] = mods;
        }
        return result;
    }

    template <std::size_t i1, std::size_t i2, class Tuple>
    constexpr static void convertCustomModifier(std::array<detail::DiagnosticBase::Argument, N>& result, Tuple& args)
    {
        constexpr std::tuple tuple = std::apply(
            [](auto... stringIndices) {
                return std::make_tuple(
                    std::integral_constant<char32_t,
                                           std::get<i1>(allFormatModifiers)[i2][decltype(stringIndices)::value]>{}...);
            },
            integerSequenceToTuple(std::make_index_sequence<std::get<i1>(allFormatModifiers)[i2].size()>{}));
        result[i1].customModifiers[std::get<i1>(allFormatModifiers)[i2]] = std::apply(
            [&args](auto... chars) -> std::string {
                static_assert(
                    IsTypeCompleteV<diag::CustomFormat<decltype(chars)::value...>>,
                    "No template specialization of cld::diag::CustomModifier exists for a given format modifier");
                static_assert(
                    std::is_invocable_r_v<std::string, diag::CustomFormat<decltype(chars)::value...>,
                                          std::tuple_element_t<i1, Tuple>>,
                    "No operator() found in cld::diag::CustomModifier for the given arg that returns a type convertible to std::string");
                return diag::CustomFormat<decltype(chars)::value...>{}(std::get<i1>(args));
            },
            tuple);
    }

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

    constexpr static auto constraints = getConstraints();

    std::string_view getName() const
    {
        return m_name;
    }

private:
    constexpr static auto modifiers = getModifiers(std::index_sequence_for<Mods...>{});

    constexpr static auto allFormatModifiers =
        std::apply([](auto... indices) { return std::make_tuple(customModifiersFor<decltype(indices)::value>()...); },
                   integerSequenceToTuple(std::make_index_sequence<N>{}));
};

namespace diag
{
std::tuple<const Lexer::TokenBase&, std::uint64_t> after(const Lexer::TokenBase& token);
} // namespace diag

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
    auto array = createArgumentArray(sourceInterface, std::forward_as_tuple(std::forward<Args>(args)...),
                                     std::index_sequence_for<Args...>{});
    return print(getPointRange(location), {result.data(), static_cast<std::size_t>(resStart - result.data())}, array,
                 modifiers, sourceInterface);
}

template <std::size_t N, auto& format, class... Mods>
constexpr auto Diagnostic<N, format, Mods...>::getConstraints() -> std::array<std::underlying_type_t<Constraint>, N>
{
    std::array<std::underlying_type_t<Constraint>, N> result{};
    auto text = getFormat();
    std::uint8_t i = 0;
    while (auto search = ctre::search<detail::DIAG_ARG_PATTERN>(text))
    {
        auto index = search.template get<2>().view().back() - '0';
        auto mods = search.template get<1>().view();
        if (mods.empty())
        {
            result[index] |= Constraint::StringConstraint;
        }
        else
        {
            result[index] |= Constraint::CustomConstraint;
        }
        const auto end = search.get_end_position();
        text.remove_prefix(end - text.begin());
        i++;
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
            else
            {
                CLD_UNREACHABLE;
            }
        }(Mods{}),
        ...);
    return result;
}

template <std::size_t N, auto& format, class... Mods>
template <class Tuple, std::size_t... ints>
auto Diagnostic<N, format, Mods...>::createArgumentArray(const SourceInterface& sourceInterface, Tuple&& args,
                                                         std::index_sequence<ints...>) -> std::array<Argument, N>
{
    std::array<Argument, N> result;
    (
        [&result, &args, &sourceInterface](auto integer) {
            using IntegerTy = decltype(integer);
            using ArgTy = std::decay_t<std::tuple_element_t<IntegerTy::value, Tuple>>;
            static_assert(constraints[IntegerTy::value]);
            static_assert(!(constraints[IntegerTy::value] & Constraint::StringConstraint)
                              || !std::is_same_v<ArgTy, const char*>,
                          "Using string literals as arguments is heavily discouraged. Make a new Diagnostic."
                          "If you are mean and want to circumvent this error, convert to std::string_view");
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::LocationConstraint))
            {
                result[IntegerTy::value].range = getPointRange(std::get<IntegerTy::value>(args));
            }
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::StringConstraint))
            {
                result[IntegerTy::value].inFormatText =
                    ::cld::diag::StringConverter<ArgTy>::inFormat(std::get<IntegerTy::value>(args), sourceInterface);
                result[IntegerTy::value].inArgText =
                    ::cld::diag::StringConverter<ArgTy>::inArg(std::get<IntegerTy::value>(args), sourceInterface);
            }
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::CustomConstraint))
            {
                auto apply = [&result, &args](auto... values) {
                    (convertCustomModifier<IntegerTy::value, decltype(values)::value>(result, args), ...);
                };
                std::apply(apply,
                           integerSequenceToTuple(
                               std::make_index_sequence<std::get<IntegerTy::value>(allFormatModifiers).size()>{}));
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
            else
            {
                CLD_UNREACHABLE;
            }
        }(std::integral_constant<std::size_t, ints>{}),
        ...);
    return result;
}

} // namespace cld

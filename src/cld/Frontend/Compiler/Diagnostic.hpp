#pragma once

#include <cld/Support/Constexpr.hpp>
#include <cld/Support/Text.hpp>
#include <cld/Support/Util.hpp>

#include <array>
#include <optional>
#include <unordered_map>
#include <unordered_set>

#include <tcb/span.hpp>

#include "CustomDiag.hpp"
#include "DiagnosticUtil.hpp"
#include "Message.hpp"

namespace cld
{
namespace Lexer
{
class TokenBase;
} // namespace Lexer

template <std::size_t index, std::int64_t text = -1>
struct InsertAfter;

template <std::size_t index, std::size_t text>
struct Annotate;

template <std::size_t index>
struct AnnotateExpr;

template <std::size_t index, char character = '~', bool continuous = true>
struct Underline;

namespace detail::Diagnostic
{
struct Arg
{
    std::string_view modifier;
    std::size_t index;
    std::string_view view;
};

constexpr std::optional<Arg> nextArg(std::string_view& text)
{
    auto result = text.find('%');
    if (result == text.npos)
    {
        return {};
    }
    std::size_t modifierEnd = result + 1;
    for (; modifierEnd != text.size() && !(text[modifierEnd] >= '0' && text[modifierEnd] <= '9'); modifierEnd++)
        ;
    CLD_ASSERT(modifierEnd != text.size());
    auto view = text.substr(result, modifierEnd - result + 1);
    auto modifier = view;
    modifier.remove_prefix(1);
    modifier.remove_suffix(1);
    auto index = static_cast<std::size_t>(text[modifierEnd] - '0');
    text.remove_prefix(modifierEnd + 1);
    return Arg{modifier, index, view};
}

template <class... Args>
constexpr std::int64_t getBiggestPercentArg(std::string_view text)
{
    std::int64_t max = -1;
    while (auto result = nextArg(text))
    {
        max = std::max<std::int64_t>(max, result->index);
    }
    std::int64_t sizes[] = {
        max, static_cast<std::int64_t>(*std::max_element(Args::indices.begin(), Args::indices.end()))...};
    return *std::max_element(std::begin(sizes), std::end(sizes)) + 1;
}

template <std::size_t N, class... Args>
constexpr bool checkForNoHoles(std::string_view text)
{
    if constexpr (N == 0)
    {
        return true;
    }
    else
    {
        std::array<bool, N> seen = {{false}};
        while (auto result = nextArg(text))
        {
            seen[result->index] = true;
        }
        ((
             [&seen](auto&& array)
             {
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

template <class T>
struct IsAnnotateExpr : std::false_type
{
};

template <std::size_t index>
struct IsAnnotateExpr<AnnotateExpr<index>> : std::true_type
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
        return ((
                    [&seen](auto value)
                    {
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

class WarningRegistrar
{
public:
    explicit WarningRegistrar(std::string_view name);
};

class DiagnosticBase
{
    Severity m_severity;
    std::string_view m_name;

public:
    static std::unordered_set<std::string_view>& allWarnings();

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
        std::optional<std::pair<diag::PointLocation, diag::PointLocation>> range;
        std::optional<std::string> inFormatText;
        std::optional<std::string> inArgText;
        std::unordered_map<std::string_view, std::string> customModifiers;
    };

    Message print(std::pair<diag::PointLocation, diag::PointLocation> location, std::string_view message,
                  tcb::span<Argument> arguments, tcb::span<const Modifiers> modifiers,
                  const SourceInterface& sourceInterface) const;

    Message print(std::string_view message, tcb::span<Argument> arguments) const;

    void evaluateFormatsInMessage(std::string_view message, tcb::span<Argument> arguments,
                                  llvm::raw_string_ostream& ss) const;

public:
    constexpr DiagnosticBase(Severity severity, std::string_view name) : m_severity(severity), m_name(name) {}

    constexpr Severity getSeverity() const noexcept
    {
        return m_severity;
    }

    constexpr std::string_view getName() const
    {
        return m_name;
    }
};

} // namespace detail::Diagnostic

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

template <std::size_t index>
struct AnnotateExpr
{
    constexpr static std::array<std::size_t, 1> indices = {index};
    constexpr static std::size_t affects = index;

    [[nodiscard]] constexpr static std::size_t getIndex() noexcept
    {
        return index;
    }
};

template <std::size_t N, auto& format, class... Mods>
class Diagnostic : public detail::Diagnostic::DiagnosticBase
{
public:
    enum Constraint : std::uint8_t
    {
        LocationConstraint = 0b1,
        StringConstraint = 0b10,
        TypeConstraint = 0b100,
        CustomConstraint = 0b1000,
    };

private:
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
        if constexpr (std::is_same_v<diag::PointRange, U>)
        {
            return true;
        }
        else if constexpr (IsVariant<U>{})
        {
            constexpr auto size = std::variant_size_v<U>;
            return std::apply(
                [](auto&&... values)
                {
                    return (
                        [](auto indexT)
                        {
                            constexpr auto index = decltype(indexT)::value;
                            // Don't usually allow pointers except in variants.
                            using V = std::remove_pointer_t<std::variant_alternative_t<index, U>>;
                            if constexpr (IsSmartPtr<V>{})
                            {
                                return locationConstraintCheck<typename V::element_type>();
                            }
                            else
                            {
                                return locationConstraintCheck<V>();
                            }
                        }(values)
                        && ...);
                },
                Constexpr::integerSequenceToTuple(std::make_index_sequence<size>{}));
        }
        else if constexpr (IsTupleLike<U>{})
        {
            if constexpr (std::tuple_size_v<U> == 2)
            {
                using T1 = std::decay_t<std::tuple_element_t<0, U>>;
                using T2 = std::decay_t<std::tuple_element_t<1, U>>;
                return (std::is_base_of_v<Lexer::TokenBase, T1> && std::is_convertible_v<T2, std::uint64_t>)
                       || (std::is_base_of_v<Lexer::TokenBase, T2> && std::is_convertible_v<T1, std::uint64_t>)
                       || (std::is_convertible_v<T1, std::uint64_t> && std::is_convertible_v<T2, std::uint64_t>)
                       || (std::is_base_of_v<Lexer::TokenBase,
                                             std::remove_pointer_t<
                                                 T1>> && std::is_base_of_v<Lexer::TokenBase, std::remove_pointer_t<T2>>)
                       || (locationConstraintCheck<T1>() && locationConstraintCheck<T2>());
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
        else if constexpr (IsIterable<U>{} && !std::is_same_v<std::string, U> && !std::is_same_v<std::string_view, U>)
        {
            using ValueType = typename std::iterator_traits<decltype(std::begin(std::declval<U>()))>::value_type;
            if constexpr (IsSmartPtr<ValueType>{})
            {
                return locationConstraintCheck<typename ValueType::element_type>();
            }
            else
            {
                return locationConstraintCheck<std::remove_pointer_t<ValueType>>();
            }
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
        T, std::void_t<
               decltype(diag::StringConverter<T>::inFormat(std::declval<T>(), std::declval<const SourceInterface*>()),
                        diag::StringConverter<T>::inArg(std::declval<T>(), std::declval<const SourceInterface*>()))>>
        : std::true_type
    {
    };

    template <class T, class = void>
    struct HasType : std::false_type
    {
    };

    template <class T>
    struct HasType<T, std::void_t<decltype(std::declval<T>().getType())>> : std::true_type
    {
    };

    template <std::size_t argumentIndex, std::underlying_type_t<Constraint> constraint, class T>
    struct ConstraintCheck
    {
        static_assert(!(constraint & StringConstraint) || HasStringConverters<std::decay_t<T>>{},
                      "Argument must be convertible to string");
        static_assert(!(constraint & LocationConstraint) || locationConstraintCheck<T>(),
                      "Argument must denote a location range");
        static_assert(!(constraint & TypeConstraint) || HasType<T>{}, "Argument must have a type");
    };

    template <auto&, std::size_t>
    constexpr static void checkConstraints()
    {
    }

    template <auto& array, std::size_t i, class Curr, class... Args>
    constexpr static void checkConstraints()
    {
        [[maybe_unused]] constexpr auto v = ConstraintCheck<i, array[i], Curr>{};
        checkConstraints<array, i + 1, Args...>();
    }

    template <class Tuple, std::size_t... ints>
    static std::array<Argument, N> createArgumentArray(const SourceInterface* sourceInterface, Tuple&& args,
                                                       std::index_sequence<ints...>);

    constexpr static cld::MaxVector<std::string_view, N> customModifiersFor(std::size_t i)
    {
        cld::MaxVector<std::string_view, N> result{};
        auto text = getFormat();
        while (auto arg = detail::Diagnostic::nextArg(text))
        {
            if (arg->index != i || arg->modifier.empty())
            {
                continue;
            }
            result.push_back(arg->modifier);
        }
        return result;
    }

    template <std::size_t i, class Tuple, std::size_t... ints>
    constexpr static void convertAllCustomModifier(std::array<Argument, N>& result, Tuple& args,
                                                   std::index_sequence<ints...>)
    {
        (convertCustomModifier<i, ints>(result, args), ...);
    }

    template <std::size_t i1, std::size_t i2, class Tuple>
    constexpr static void convertCustomModifier(std::array<detail::Diagnostic::DiagnosticBase::Argument, N>& result,
                                                Tuple& args)
    {
        constexpr std::tuple tuple = std::apply(
            [](auto... stringIndices)
            {
                return std::make_tuple(
                    std::integral_constant<char, allFormatModifiers[i1][i2][decltype(stringIndices)::value]>{}...);
            },
            Constexpr::integerSequenceToTuple(std::make_index_sequence<allFormatModifiers[i1][i2].size()>{}));
        result[i1].customModifiers[allFormatModifiers[i1][i2]] = std::apply(
            [&args](auto... chars) -> std::string
            {
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
    constexpr Diagnostic(Severity severity, std::string_view name) : detail::Diagnostic::DiagnosticBase(severity, name)
    {
    }

    constexpr static std::size_t getSize()
    {
        return N;
    }

    constexpr static std::string_view getFormat()
    {
        return {format.begin(), format.size()};
    }

    template <class T, class... Args>
    Message args(const T& location, const SourceInterface& sourceInterface, Args&&... args) const;

    template <class... Args>
    Message argsCLI(Args&&... args) const;

    constexpr static auto constraints = getConstraints();

private:
    constexpr static auto modifiers = getModifiers(std::index_sequence_for<Mods...>{});

    constexpr static auto getAllFormatModifiers()
    {
        if constexpr (N != 0)
        {
            std::array<cld::MaxVector<std::string_view, N>, N> result;
            for (std::size_t i = 0; i < N; i++)
            {
                result[i] = customModifiersFor(i);
            }
            return result;
        }
        else
        {
            return 0;
        }
    }

    constexpr static auto allFormatModifiers = getAllFormatModifiers();
};

namespace diag
{
std::tuple<const Lexer::TokenBase&, std::uint64_t> after(const Lexer::TokenBase& token);

const std::unordered_set<std::string_view>& getAllWarnings();
} // namespace diag

template <const auto& text, class... Args>
constexpr auto makeDiagnostic(Severity category, std::string_view name)
{
    constexpr auto n = detail::Diagnostic::getBiggestPercentArg<Args...>({text.begin(), text.size()});
    constexpr bool tmp = detail::Diagnostic::checkForNoHoles<n, Args...>({text.begin(), text.size()});
    static_assert(tmp, "Not allowed to have any holes between % indices");
    constexpr bool duplicates = detail::Diagnostic::checkForNoDuplicates<n, Args...>();
    static_assert(duplicates, "Not allowed to have any duplicate indices in Modifiers");
    return Diagnostic<n, text, Args...>(category, name);
}

template <std::size_t N, auto& format, class... Mods>
template <class T, class... Args>
Message Diagnostic<N, format, Mods...>::args(const T& location, const SourceInterface& sourceInterface,
                                             Args&&... args) const
{
    static_assert(sizeof...(Args) == N, "Not the same amount of argument as needed by the format");
    [[maybe_unused]] auto* v = this->checkConstraints<constraints, 0, Args...>; // Instantiate but don't call the
                                                                                // function to improve debug perf a lil
    static_assert(locationConstraintCheck<T>(), "First argument must denote a location");
    auto array = createArgumentArray(&sourceInterface, std::forward_as_tuple(std::forward<Args>(args)...),
                                     std::index_sequence_for<Args...>{});
    return print(diag::getPointRange(location), getFormat(), array, modifiers, sourceInterface);
}

template <std::size_t N, auto& format, class... Mods>
template <class... Args>
Message Diagnostic<N, format, Mods...>::argsCLI(Args&&... args) const
{
    static_assert(sizeof...(Args) == N, "Not the same amount of argument as needed by the format");
    [[maybe_unused]] auto* v = this->checkConstraints<constraints, 0, Args...>; // Instantiate but don't call the
    // function to improve debug perf a lil
    // TODO: Instead of passing nullptr make it so that StringConverter may use const SourceInterface& as a second
    //  argument but make it a compiler error if such a string converter is used when using argsCLI instead of args
    auto array = createArgumentArray(nullptr, std::forward_as_tuple(std::forward<Args>(args)...),
                                     std::index_sequence_for<Args...>{});
    return print(getFormat(), array);
}

template <std::size_t N, auto& format, class... Mods>
constexpr auto Diagnostic<N, format, Mods...>::getConstraints() -> std::array<std::underlying_type_t<Constraint>, N>
{
    std::array<std::underlying_type_t<Constraint>, N> result{};
    auto text = getFormat();
    while (auto search = detail::Diagnostic::nextArg(text))
    {
        if (search->modifier.empty())
        {
            result[search->index] |= Constraint::StringConstraint;
        }
        else
        {
            result[search->index] |= Constraint::CustomConstraint;
        }
    }
    (
        [&result](auto value)
        {
            using T = decltype(value);
            if constexpr (detail::Diagnostic::IsUnderline<T>{})
            {
                result[T::affects] |= Constraint::LocationConstraint;
            }
            else if constexpr (detail::Diagnostic::IsAnnotate<T>{})
            {
                result[T::indices[0]] |= Constraint::LocationConstraint;
                result[T::indices[1]] |= Constraint::StringConstraint;
            }
            else if constexpr (detail::Diagnostic::IsAnnotateExpr<T>{})
            {
                result[T::affects] |= Constraint::LocationConstraint | Constraint::TypeConstraint;
            }
            else if constexpr (detail::Diagnostic::IsInsertAfter<T>{})
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
auto Diagnostic<N, format, Mods...>::createArgumentArray(const SourceInterface* sourceInterface, Tuple&& args,
                                                         std::index_sequence<ints...>) -> std::array<Argument, N>
{
    (void)sourceInterface;
    std::array<Argument, N> result;
    (
        [&result, &args, sourceInterface](auto integer)
        {
            using IntegerTy = decltype(integer);
            using ArgTy = std::decay_t<std::tuple_element_t<IntegerTy::value, Tuple>>;
            static_assert(constraints[IntegerTy::value]);
            static_assert(!(constraints[IntegerTy::value] & Constraint::StringConstraint)
                              || !std::is_same_v<ArgTy, const char*>,
                          "Using string literals as arguments is heavily discouraged. Make a new Diagnostic."
                          "If you are mean and want to circumvent this error, convert to std::string_view");
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::LocationConstraint))
            {
                result[IntegerTy::value].range = ::cld::diag::getPointRange(std::get<IntegerTy::value>(args));
            }
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::StringConstraint))
            {
                result[IntegerTy::value].inFormatText =
                    ::cld::diag::StringConverter<ArgTy>::inFormat(std::get<IntegerTy::value>(args), sourceInterface);
                result[IntegerTy::value].inArgText =
                    ::cld::diag::StringConverter<ArgTy>::inArg(std::get<IntegerTy::value>(args), sourceInterface);
            }
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::TypeConstraint))
            {
                using Type = std::decay_t<decltype(std::declval<ArgTy>().getType())>;
                result[IntegerTy::value].inArgText = ::cld::diag::StringConverter<Type>::inArg(
                    std::get<IntegerTy::value>(args).getType(), sourceInterface);
            }
            if constexpr ((bool)(constraints[IntegerTy::value] & Constraint::CustomConstraint))
            {
                convertAllCustomModifier<IntegerTy::value>(
                    result, args, std::make_index_sequence<allFormatModifiers[IntegerTy::value].size()>{});
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
        [&result](auto value)
        {
            constexpr std::size_t i = decltype(value)::value;
#if __has_builtin(__type_pack_element)
            using T = __type_pack_element<i, Mods...>;
#else
            using T = std::tuple_element_t<i, std::tuple<Mods...>>;
#endif
            if constexpr (detail::Diagnostic::IsUnderline<T>{})
            {
                result[i] = Modifiers{DiagnosticBase::Underline{T::getIndex(), T::getCharacter(), T::isContinuous()}};
            }
            else if constexpr (detail::Diagnostic::IsAnnotate<T>{})
            {
                result[i] = Modifiers{DiagnosticBase::Annotate{T::getIndex(), T::getText()}};
            }
            else if constexpr (detail::Diagnostic::IsAnnotateExpr<T>{})
            {
                result[i] = Modifiers{DiagnosticBase::Annotate{T::getIndex(), T::getIndex()}};
            }
            else if constexpr (detail::Diagnostic::IsInsertAfter<T>{})
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

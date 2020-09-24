#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <charconv>
#include <optional>
#include <string_view>
#include <tuple>
#include <variant>

#include <ctre.hpp>

#include "Constexpr.hpp"
#include "Util.hpp"

namespace cld
{
enum class CLIMultiArg
{
    Overwrite,
    List,
    BitwiseMerge
};

template <class ReturnType, std::size_t args, class... Alternatives>
class CommandLineOption
{
    std::tuple<Alternatives...> m_alternatives;
    std::array<std::u32string_view, args> m_argNameToRetTypePos;
    std::string_view m_description;
    CLIMultiArg m_multiArg;

public:
    constexpr CommandLineOption(type_identity<ReturnType>, std::tuple<Alternatives...> alternatives,
                                std::array<std::u32string_view, args> argNameToRetTypePos, std::string_view description,
                                CLIMultiArg multiArg)
        : m_alternatives(std::move(alternatives)),
          m_argNameToRetTypePos(argNameToRetTypePos),
          m_description(description),
          m_multiArg(multiArg)
    {
    }

    using value_type = ReturnType;

    constexpr const std::array<std::u32string_view, args>& getArgNames() const
    {
        return m_argNameToRetTypePos;
    }

    constexpr const std::tuple<Alternatives...>& getAlternatives() const
    {
        return m_alternatives;
    }

    constexpr CLIMultiArg getMultiArg() const
    {
        return m_multiArg;
    }

    constexpr std::string_view getDescription() const
    {
        return m_description;
    }
};

namespace detail::CommandLine
{
struct Whitespace
{
};

struct Arg
{
    std::u32string_view value;
};

struct Text
{
    std::u32string_view value;
};

struct Ellipsis
{
};

struct NegationOption
{
    std::u32string_view value;
};

template <class... Args>
constexpr std::array<std::variant<Whitespace, Arg, Text, Ellipsis, NegationOption>, sizeof...(Args)>
    tupleToArray(const std::tuple<Args...>& tuple)
{
    return std::apply(
        [](auto&&... values) {
            return std::array<std::variant<Whitespace, Arg, Text, Ellipsis, NegationOption>, sizeof...(Args)>{
                values...};
        },
        tuple);
}

constexpr std::optional<std::u32string_view> findArg(std::u32string_view arg)
{
    std::size_t start = 0;
    for (; start < arg.size() && arg[start] != U'<'; start++)
        ;
    if (start == arg.size())
    {
        return std::nullopt;
    }
    std::size_t end = start + 1;
    for (; end < arg.size() && arg[end] != U'>'; end++)
        ;
    if (end == arg.size())
    {
        return std::nullopt;
    }
    return arg.substr(start, end - start + 1);
}

constexpr const char32_t* skipWhitespace(std::u32string_view arg)
{
    const char32_t* start = arg.data();
    while (start != arg.data() + arg.size() && *start == U' ')
    {
        start++;
    }
    return start;
}

constexpr const char32_t* skipLetters(std::u32string_view arg)
{
    const char32_t* start = arg.data();
    while (start != arg.data() + arg.size() && (*start != U'<' && *start != U' ' && *start != U'[' && *start != U'.'))
    {
        start++;
    }
    return start;
}

constexpr const char32_t* findClosingSquareBracket(std::u32string_view arg)
{
    const char32_t* start = arg.data();
    while (start != arg.data() + arg.size() && *start != U']')
    {
        start++;
    }
    return start;
}

template <auto& arg, std::size_t offset>
constexpr auto parseOption()
{
    static_assert(arg.size() > 0, "Cannot parse empty string");
    if constexpr (offset >= arg.size())
    {
        return std::make_tuple();
    }
    else
    {
        constexpr std::u32string_view view{(arg.begin() + offset), arg.size() - offset};
        if constexpr (*(arg.begin() + offset) == U'<')
        {
            constexpr auto optArg = findArg(view);
            static_assert(optArg, "Failed to parse arg. Did you terminate with '>'?");
            return std::tuple_cat(std::make_tuple(Arg{{optArg->data() + 1, optArg->size() - 2}}),
                                  parseOption<arg, optArg->data() + optArg->size() - arg.begin()>());
        }
        else if constexpr (*(arg.begin() + offset) == U' ')
        {
            constexpr auto nextNonWhitespace = skipWhitespace(view);
            return std::tuple_cat(std::make_tuple(Whitespace{}), parseOption<arg, nextNonWhitespace - arg.begin()>());
        }
        else if constexpr (*(arg.begin() + offset) == U'[')
        {
            constexpr auto closing = findClosingSquareBracket(view);
            return std::tuple_cat(std::make_tuple(NegationOption{{view.substr(1, closing - view.data() - 1)}}),
                                  parseOption<arg, closing - arg.begin() + 1>());
        }
        else if constexpr (*(arg.begin() + offset) == U'.' && offset + 2 < arg.size()
                           && *(arg.begin() + offset + 1) == U'.' && *(arg.begin() + offset + 2) == U'.')
        {
            return std::tuple_cat(std::make_tuple(Ellipsis{}), parseOption<arg, offset + 3>());
        }
        else
        {
            constexpr auto nextNonLetter = skipLetters(view);
            return std::tuple_cat(std::make_tuple(Text{{view.data(), nextNonLetter - view.data()}}),
                                  parseOption<arg, nextNonLetter - arg.begin()>());
        }
    }
}

template <class First, class... Args>
constexpr auto filterOutArgs(First first, Args&&... args)
{
    if constexpr (std::is_same_v<Arg, First>)
    {
        if constexpr (sizeof...(Args) == 0)
        {
            return std::make_tuple(first);
        }
        else
        {
            return std::tuple_cat(std::make_tuple(first), filterOutArgs(args...));
        }
    }
    else if constexpr (sizeof...(Args) != 0)
    {
        return filterOutArgs(args...);
    }
    else
    {
        return std::make_tuple();
    }
}

template <bool sizeOrArray, std::size_t inputSize, std::size_t maxOutputSize = inputSize>
constexpr auto unique(std::array<std::u32string_view, inputSize> array)
{
    std::size_t currentResultSize = 0;
    std::array<std::u32string_view, maxOutputSize> result;
    for (std::size_t i = 0; i < inputSize; i++)
    {
        bool contains = false;
        for (std::size_t i2 = 0; i2 < currentResultSize; i2++)
        {
            if (array[i] == result[i2])
            {
                contains = true;
                break;
            }
        }
        if (contains)
        {
            continue;
        }
        result[currentResultSize++] = array[i];
    }
    if constexpr (!sizeOrArray)
    {
        return currentResultSize;
    }
    else
    {
        return result;
    }
}

template <std::size_t size, class Tuple>
constexpr std::array<std::pair<std::u32string_view, bool>, size>
    evaluateOptional(std::array<std::u32string_view, size> args, const Tuple& tuple)
{
    std::array<std::pair<std::u32string_view, bool>, size> result;
    for (std::size_t i = 0; i < size; i++)
    {
        result[i].first = args[i];
        result[i].second = !std::apply(
            [arg = args[i]](auto&&... args) {
                return (std::apply(
                            [arg](auto&&... tokens) {
                                auto argsFiltered = filterOutArgs(tokens...);
                                return std::apply(
                                    [arg](auto&&... inputArgs) { return ((arg == inputArgs.value) || ...); },
                                    argsFiltered);
                            },
                            args)
                        && ...);
            },
            tuple);
    }
    return result;
}

template <std::size_t curr = 0, class... Args>
constexpr void validateTokens(std::tuple<Args...> tuple)
{
    if constexpr (curr == 0)
    {
        static_assert(std::is_same_v<std::tuple_element_t<0, std::tuple<Args...>>, Text>,
                      "First element in command line option must be a text");
    }
    if constexpr (std::is_same_v<std::tuple_element_t<curr, std::tuple<Args...>>, Ellipsis>)
    {
        static_assert(curr >= 2, "Expected structure of \"<Arg> <Separator> ...\"");
        static_assert(std::is_same_v<std::tuple_element_t<curr - 2, std::tuple<Args...>>, Arg>,
                      "Expected structure of \"<Arg> <Separator> ...\"");
    }
    if constexpr (curr + 1 < sizeof...(Args))
    {
        validateTokens<curr + 1>(tuple);
    }
}

template <auto* t>
struct Pointer
{
    constexpr static auto pointer = t;
};

template <auto& first, auto& second, auto&... rest>
constexpr auto toPair()
{
    if constexpr (sizeof...(rest) == 0)
    {
        return std::make_tuple(std::pair{Pointer<&first>{}, second});
    }
    else
    {
        return std::tuple_cat(std::make_tuple(std::pair{Pointer<&first>{}, second}), toPair<rest...>());
    }
}

template <const auto&... args>
struct Pack
{
    constexpr static auto parseOptionsImpl()
    {
        constexpr auto tuple = std::make_tuple(parseOption<args, 0>()...);
        static_assert((std::apply([](auto&&... input) { (validateTokens(input), ...); }, tuple), true));
        constexpr auto allArgsTuple = std::apply(
            [](auto&&... input) {
                return std::apply([](auto&&... input) { return filterOutArgs(input...); }, std::tuple_cat(input...));
            },
            tuple);
        // libc++ until either version LLVM 11 or 12 does not support constexpr std::array<T,0>
        if constexpr (std::tuple_size_v<std::decay_t<decltype(allArgsTuple)>> != 0)
        {
            constexpr auto allArgsSVArray = std::apply(
                [](auto&&... input) { return std::array<std::u32string_view, sizeof...(input)>{input.value...}; },
                allArgsTuple);
            constexpr std::size_t setSize = unique<false>(allArgsSVArray);
            constexpr auto uniqueSet = unique<true, allArgsSVArray.size(), setSize>(allArgsSVArray);

            constexpr auto foundOptionals = evaluateOptional(uniqueSet, tuple);
            return std::pair{tuple, foundOptionals};
        }
        else
        {
            return std::pair{tuple, std::make_tuple()};
        }
    }
};

template <class T>
type_identity<T> unpack(const std::in_place_type_t<T>&)
{
    return {};
}

template <std::size_t i>
constexpr bool isOptional(std::u32string_view text, std::array<std::pair<std::u32string_view, bool>, i> array)
{
    for (std::size_t i2 = 0; i2 < i; i2++)
    {
        if (array[i2].first == text)
        {
            return array[i2].second;
        }
    }
    return true;
}

constexpr bool isOptional(std::u32string_view, std::tuple<>)
{
    return true;
}

template <class T, auto&... args>
constexpr auto parseOptions(std::string_view description, CLIMultiArg multiArg = CLIMultiArg::Overwrite)
{
    constexpr auto pair = T::parseOptionsImpl();
    constexpr auto lexems = pair.first;
    constexpr auto arguments = pair.second;
    constexpr auto tuple = [] {
        if constexpr (sizeof...(args) == 0)
        {
            return std::make_tuple();
        }
        else
        {
            return toPair<args...>();
        }
    }();
    constexpr auto value = std::apply(
        [&](auto&&... values) {
#if defined(_MSC_VER) && !defined(__clang__)
            constexpr auto arguments = T::parseOptionsImpl().second;
#endif
            using Tuple =
                std::tuple<std::conditional_t<isOptional(std::u32string_view(
                                                             std::decay_t<decltype(values.first)>::pointer->begin(),
                                                             std::decay_t<decltype(values.first)>::pointer->size()),
                                                         arguments),
                                              std::optional<typename decltype(unpack(values.second))::type>,
                                              typename decltype(unpack(values.second))::type>...>;
            if constexpr (std::tuple_size_v<Tuple> == 1)
            {
                return type_identity<std::tuple_element_t<0, Tuple>>{};
            }
            else
            {
                return type_identity<Tuple>{};
            }
        },
        tuple);

    return CommandLineOption(value, lexems,
                             std::apply(
                                 [](auto&&... values) {
                                     return std::array<std::u32string_view, sizeof...(values)>{
                                         std::u32string_view(std::decay_t<decltype(values.first)>::pointer->begin(),
                                                             std::decay_t<decltype(values.first)>::pointer->size())...};
                                 },
                                 tuple),
                             description, multiArg);
}

template <bool sizeOrArray, std::size_t maxSize>
constexpr auto utf32ToUtf8(std::u32string_view stringView)
{
    std::size_t resultSize = 0;
    std::array<char, maxSize> result = {};
    for (std::size_t i = 0; i < stringView.size(); i++)
    {
        char32_t code = stringView[i];
        if (code <= 0x7F)
        {
            result[resultSize++] = code;
        }
        else if (code <= 0x7FF)
        {
            result[resultSize++] = 0xC0 | (code >> 6);
            result[resultSize++] = 0x80 | (code & 0x3F);
        }
        else if (code <= 0xFFFF)
        {
            result[resultSize++] = 0xE0 | (code >> 12);
            result[resultSize++] = 0x80 | ((code >> 6) & 0x3F);
            result[resultSize++] = 0x80 | (code & 0x3F);
        }
        else if (code <= 0x10FFFF)
        {
            result[resultSize++] = 0xF0 | (code >> 18);
            result[resultSize++] = 0x80 | ((code >> 12) & 0x3F);
            result[resultSize++] = 0x80 | ((code >> 6) & 0x3F);
            result[resultSize++] = 0x80 | (code & 0x3F);
        }
    }
    if constexpr (!sizeOrArray)
    {
        return resultSize;
    }
    else
    {
        return result;
    }
}

template <std::size_t size>
constexpr static std::size_t indexOf(std::array<std::u32string_view, size> array, std::u32string_view text)
{
    for (std::size_t i = 0; i < size; i++)
    {
        if (array[i] == text)
        {
            return i;
        }
    }
    CLD_UNREACHABLE;
}

template <class T>
struct ValueType
{
    using type = typename T::value_type;
};

template <auto* cliOption, std::size_t i, class Storage>
static bool checkAlternative(llvm::MutableArrayRef<std::string_view>& commandLine, Storage& storage)
{
    auto thisElement = [](auto& storage) -> decltype(auto) {
        if constexpr (cliOption->getMultiArg() == CLIMultiArg::List)
        {
            return storage.back();
        }
        else
        {
            return *storage;
        }
    };

    constexpr auto alternative = std::get<i>(cliOption->getAlternatives());
    constexpr static auto array = detail::CommandLine::tupleToArray(alternative);
    auto storageCopy = [&] {
        if constexpr (cliOption->getMultiArg() == CLIMultiArg::List)
        {
            return 0;
        }
        else
        {
            return storage;
        }
    }();
    if constexpr (cliOption->getMultiArg() == CLIMultiArg::List)
    {
        storage.emplace_back();
    }
    else
    {
        storage.emplace();
    }
    auto copy = commandLine;
    std::vector<std::string_view> contents(commandLine.begin(),
                                           commandLine.begin() + std::min(array.size(), commandLine.size()));
    bool success = YComb{[&, &storage = storage](auto&& self, auto indexT) -> bool {
        constexpr std::size_t index = decltype(indexT)::value;
        constexpr auto curr = array[index];
        if constexpr (std::holds_alternative<detail::CommandLine::Text>(curr))
        {
            if (commandLine.empty())
            {
                return false;
            }
            constexpr auto utf32 = cld::get<detail::CommandLine::Text>(curr).value;
            constexpr std::size_t utf8Size = detail::CommandLine::utf32ToUtf8<false, utf32.size() * 4>(utf32);
            constexpr static auto utf8Array = detail::CommandLine::utf32ToUtf8<true, utf8Size>(utf32);
            constexpr std::string_view prefix(utf8Array.data(), utf8Array.size());
            if (commandLine.front().substr(0, prefix.size()) == prefix)
            {
                commandLine.front().remove_prefix(prefix.size());
            }
            else
            {
                return false;
            }
        }
        else if constexpr (std::holds_alternative<detail::CommandLine::NegationOption>(curr))
        {
            static_assert(!std::holds_alternative<detail::CommandLine::NegationOption>(curr)
                              || cliOption->getMultiArg() == CLIMultiArg::Overwrite,
                          "Negate option requires 'Overwrite' as multi arg option");
            if (commandLine.empty())
            {
                return true;
            }
            constexpr auto utf32 = cld::get<detail::CommandLine::NegationOption>(curr).value;
            constexpr std::size_t utf8Size = detail::CommandLine::utf32ToUtf8<false, utf32.size() * 4>(utf32);
            constexpr static auto utf8Array = detail::CommandLine::utf32ToUtf8<true, utf8Size>(utf32);
            constexpr std::string_view prefix(utf8Array.data(), utf8Array.size());
            if (commandLine.front().substr(0, prefix.size()) == prefix)
            {
                commandLine.front().remove_prefix(prefix.size());
                if constexpr (cliOption->getMultiArg() == CLIMultiArg::Overwrite)
                {
                    storage.reset();
                }
            }
        }
        else if constexpr (std::holds_alternative<detail::CommandLine::Arg>(curr))
        {
            constexpr std::size_t storageIndex =
                indexOf(cliOption->getArgNames(), cld::get<detail::CommandLine::Arg>(curr).value);
            using ArgTypeT = typename std::conditional_t<
                IsTuple<std::decay_t<decltype(thisElement(storage))>>{},
                std::tuple_element<storageIndex, std::decay_t<decltype(thisElement(storage))>>,
                std::decay<decltype(thisElement(storage))>>::type;
            using ArgType =
                typename std::conditional_t<IsOptional<ArgTypeT>{}, ValueType<ArgTypeT>, type_identity<ArgTypeT>>::type;
            std::string_view text;
            if (commandLine.empty())
            {
                return false;
            }
            if constexpr (index + 1 == array.size())
            {
                text = commandLine.front();
                commandLine.front().remove_prefix(commandLine.front().size());
            }
            else if constexpr (std::holds_alternative<detail::CommandLine::Whitespace>(array[index + 1]))
            {
                text = commandLine.front();
                commandLine.front().remove_prefix(commandLine.front().size());
            }
            else if constexpr (std::holds_alternative<detail::CommandLine::Text>(array[index + 1]))
            {
                auto end = std::find_if(commandLine.front().begin(), commandLine.front().end(),
                                        [separator = cld::get<detail::CommandLine::Text>(array[index + 1]).value[0]](
                                            char c) { return c == (char)separator; });
                text = commandLine.front().substr(0, end - commandLine.front().begin());
                commandLine.front().remove_prefix(text.size());
            }
            else
            {
                CLD_UNREACHABLE;
            }
            auto& element = [&]() -> decltype(auto) {
                if constexpr (IsTuple<std::decay_t<decltype(thisElement(storage))>>{})
                {
                    // MSVC
#if defined(_MSC_VER) && !defined(__clang__)
                    constexpr std::size_t index = decltype(indexT)::value;
                    constexpr auto curr = array[index];
                    constexpr std::size_t storageIndex =
                        indexOf(cliOption->getArgNames(), cld::get<detail::CommandLine::Arg>(curr).value);
#endif
                    return std::get<storageIndex>(thisElement(storage));
                }
                else
                {
                    return thisElement(storage);
                }
            }();
            if constexpr (std::is_same_v<std::string, ArgType> || std::is_same_v<std::string_view, ArgType>)
            {
                element = text;
            }
            else if constexpr (std::is_integral_v<ArgType>)
            {
                element = 0;
                if constexpr (IsOptional<std::decay_t<decltype(element)>>{})
                {
                    std::from_chars(text.begin(), text.end(), *element);
                }
                else
                {
                    std::from_chars(text.begin(), text.end(), element);
                }
            }
        }
        else if constexpr (std::holds_alternative<detail::CommandLine::Whitespace>(curr))
        {
            if (!commandLine.empty() && !commandLine.front().empty())
            {
                return false;
            }
            else if (!commandLine.empty())
            {
                commandLine = commandLine.drop_front();
            }
        }

        if constexpr (index + 1 != array.size())
        {
            return self(std::integral_constant<std::size_t, index + 1>{});
        }
        else
        {
            if (commandLine.empty())
            {
                return true;
            }
            if (commandLine.front().empty())
            {
                commandLine = commandLine.drop_front();
                return true;
            }
            return false;
        }
    }}(std::integral_constant<std::size_t, 0>{});
    if (success)
    {
        return true;
    }
    if constexpr (cliOption->getMultiArg() == CLIMultiArg::List)
    {
        storage.pop_back();
    }
    else
    {
        storage = storageCopy;
    }
    commandLine = copy;
    std::move(contents.begin(), contents.end(), commandLine.begin());
    return false;
}

template <auto* cliOption, class Storage>
static bool checkAllAlternatives(llvm::MutableArrayRef<std::string_view>& commandLine, Storage& storage)
{
    using AllAlternatives = std::decay_t<decltype(cliOption->getAlternatives())>;
    return std::apply(
        [&](auto&&... indices) -> bool {
            return ([&](auto index) -> bool {
                using Index = decltype(index);
                constexpr std::size_t i = Index::value;
                constexpr auto alternative = std::get<i>(cliOption->getAlternatives());
                using Tuple = std::decay_t<decltype(alternative)>;
                constexpr std::u32string_view u32prefix = std::get<0>(alternative).value;
                constexpr std::size_t utf8Size =
                    detail::CommandLine::utf32ToUtf8<false, u32prefix.size() * 4>(u32prefix);
                constexpr static auto utf8Prefix = detail::CommandLine::utf32ToUtf8<true, utf8Size>(u32prefix);

                constexpr std::string_view prefix(utf8Prefix.data(), utf8Prefix.size());
                if (commandLine.front() == prefix)
                {
                    return checkAlternative<cliOption, i>(commandLine, storage);
                }
                else if constexpr (std::tuple_size_v<Tuple> >= 2)
                {
                    if constexpr (!std::is_same_v<std::tuple_element_t<1, Tuple>, detail::CommandLine::Whitespace>)
                    {
                        if (commandLine.front().substr(0, prefix.size()) == prefix)
                        {
                            return checkAlternative<cliOption, i>(commandLine, storage);
                        }
                    }
                }
                return false;
            }(indices) || ...);
        },
        integerSequenceToTuple(std::make_index_sequence<std::tuple_size_v<AllAlternatives>>{}));
}

} // namespace detail::CommandLine

template <class Storage>
class CommandLine
{
    Storage m_storage;
    std::vector<std::string_view> m_unrecognized;

public:
    CommandLine(Storage storage, std::vector<std::string_view> unrecognized)
        : m_storage(std::move(storage)), m_unrecognized(std::move(unrecognized))
    {
    }

    [[nodiscard]] const std::vector<std::string_view>& getUnrecognized() const noexcept
    {
        return m_unrecognized;
    }

    template <auto& option>
    [[nodiscard]] const auto& get() const noexcept
    {
        return std::get<
                   std::pair<detail::CommandLine::Pointer<&option>,
                             std::conditional_t<option.getMultiArg() == CLIMultiArg::List,
                                                std::vector<typename std::decay_t<decltype(option)>::value_type>,
                                                std::optional<typename std::decay_t<decltype(option)>::value_type>>>>(
                   m_storage)
            .second;
    }
};

template <auto&... options>
auto parseCommandLine(llvm::MutableArrayRef<std::string_view> commandLine)
{
    constexpr auto tuple = std::make_tuple(detail::CommandLine::Pointer<&options>{}...);
    auto storage = std::make_tuple(
        std::pair{detail::CommandLine::Pointer<&options>{},
                  std::conditional_t<options.getMultiArg() == CLIMultiArg::List,
                                     std::vector<typename std::decay_t<decltype(options)>::value_type>,
                                     std::optional<typename std::decay_t<decltype(options)>::value_type>>{}}...);
    std::vector<std::string_view> unrecognized;
    while (!commandLine.empty())
    {
        if (!std::apply(
                [&](auto&&... indices) -> bool {
                    return ([&](auto index) -> bool {
                        constexpr std::size_t i = decltype(index)::value;
                        return detail::CommandLine::checkAllAlternatives<
                            std::tuple_element_t<i, decltype(tuple)>::pointer>(commandLine,
                                                                               std::get<i>(storage).second);
                    }(indices) || ...);
                },
                integerSequenceToTuple(std::make_index_sequence<sizeof...(options)>{})))
        {
            unrecognized.push_back(commandLine.front());
            commandLine = commandLine.drop_front();
        }
    }
    return CommandLine(std::move(storage), std::move(unrecognized));
}
} // namespace cld

#define CLD_MACRO_NULL_SEP(NAME, i, REC, RES) REC RES

#define CLD_MACRO_GEN_STRING(NAME, STRING, INDEX) constexpr static auto NAME##arg##INDEX = ::ctll::fixed_string{STRING};

#define CLD_MACRO_GEN_STRINGS(NAME, ...) \
    P99_FOR(NAME, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__), CLD_MACRO_NULL_SEP, CLD_MACRO_GEN_STRING, __VA_ARGS__)

#define CLD_MACRO_REMOVE_PAREN(...) __VA_ARGS__

#define CLD_MACRO_GEN_STRINGS2(NAME, PACK) CLD_MACRO_GEN_STRINGS(NAME, CLD_MACRO_REMOVE_PAREN PACK)

#define CLD_MACRO_COMMA_SEP(NAME, i, REC, RES) REC, RES

#define CLD_MACRO_LIST_STRING(NAME, STRING, INDEX) detail::NAME##arg##INDEX

#define CLD_MACRO_LIST_STRINGS(NAME, ...) \
    P99_FOR(NAME, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__), CLD_MACRO_COMMA_SEP, CLD_MACRO_LIST_STRING, __VA_ARGS__)

#define CLD_MACRO_LIST_STRINGS2(NAME, PACK) CLD_MACRO_LIST_STRINGS(NAME, CLD_MACRO_REMOVE_PAREN PACK)

#define CLD_MACRO_STRINGIFY_SECOND(x, y) #y

#define CLD_MACRO_GEN_BIND(NAME, STRING, INDEX) \
    constexpr static auto NAME##bind##INDEX = ::ctll::fixed_string{CLD_MACRO_STRINGIFY_SECOND STRING};

#define CLD_MACRO_GEN_BINDS(NAME, ...) \
    P99_FOR(NAME, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__), CLD_MACRO_NULL_SEP, CLD_MACRO_GEN_BIND, __VA_ARGS__)

#define CLD_MACRO_LIST_BIND_DEFER(index, type, name) detail::name##bind##index, ::std::in_place_type<type>

#define CLD_MACRO_GET_FIRST(x, y) x

#define CLD_MACRO_LIST_BIND(NAME, ARG, INDEX) CLD_MACRO_LIST_BIND_DEFER(INDEX, CLD_MACRO_GET_FIRST ARG, NAME)

#define CLD_MACRO_LIST_BINDS(NAME, ...) \
    P99_FOR(NAME, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__), CLD_MACRO_COMMA_SEP, CLD_MACRO_LIST_BIND, __VA_ARGS__)

#define CLD_MACRO_CLI_OPT_2(NAME, PACK)                                    \
    namespace detail                                                       \
    {                                                                      \
    CLD_MACRO_GEN_STRINGS2(NAME, PACK)                                     \
    }                                                                      \
    constexpr static auto NAME = ::cld::detail::CommandLine::parseOptions< \
        ::cld::detail::CommandLine::Pack<CLD_MACRO_LIST_STRINGS2(NAME, PACK)>>

#define CLD_MACRO_CLI_OPT_3(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)
#define CLD_MACRO_CLI_OPT_4(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)
#define CLD_MACRO_CLI_OPT_5(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)
#define CLD_MACRO_CLI_OPT_6(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)
#define CLD_MACRO_CLI_OPT_7(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)
#define CLD_MACRO_CLI_OPT_8(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)
#define CLD_MACRO_CLI_OPT_9(...) CLD_CLI_OPT_VARARG(__VA_ARGS__)

#define CLD_CLI_OPT_VARARG(NAME, PACK, ...)                                    \
    namespace detail                                                           \
    {                                                                          \
    CLD_MACRO_GEN_STRINGS2(NAME, PACK)                                         \
    CLD_MACRO_GEN_BINDS(NAME, __VA_ARGS__)                                     \
    }                                                                          \
    constexpr static auto NAME = ::cld::detail::CommandLine::parseOptions<     \
        ::cld::detail::CommandLine::Pack<CLD_MACRO_LIST_STRINGS2(NAME, PACK)>, \
        CLD_MACRO_LIST_BINDS(NAME, __VA_ARGS__)>

#define CLD_CLI_OPT(...) P99_PASTE2(CLD_MACRO_CLI_OPT_, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__))(__VA_ARGS__)

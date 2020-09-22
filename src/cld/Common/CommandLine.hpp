#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <charconv>
#include <optional>
#include <string_view>
#include <tuple>
#include <variant>

#include "Constexpr.hpp"
#include "Util.hpp"

namespace cld
{
// "-D<macro>=<value>","-D<macro>","--define-macro <macro>","--define-macro=<macro>"
// pair{"<macro>",std::in_place_type<std::string_view>},pair{"<value>",std::in_place_type<std::string_view>}
// Return type -> std::optional<std::tuple<std::string_view,std::optional<std::string_view>>>

template <class ReturnType, std::size_t args, class... Alternatives>
class CommandLineOption
{
    std::tuple<Alternatives...> m_alternatives;
    std::array<std::u32string_view, args> m_argNameToRetTypePos;

public:
    constexpr CommandLineOption(type_identity<ReturnType>, std::tuple<Alternatives...> alternatives,
                                std::array<std::u32string_view, args> argNameToRetTypePos)
        : m_alternatives(std::move(alternatives)), m_argNameToRetTypePos(argNameToRetTypePos)
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

template <class... Args>
constexpr std::array<std::variant<Whitespace, Arg, Text, Ellipsis>, sizeof...(Args)>
    tupleToArray(const std::tuple<Args...>& tuple)
{
    return std::apply(
        [](auto&&... values) {
            return std::array<std::variant<Whitespace, Arg, Text, Ellipsis>, sizeof...(Args)>{values...};
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
    while (start != arg.data() + arg.size() && (*start != U'<' && *start != U' '))
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
        constexpr auto allArgsSVArray = std::apply(
            [](auto&&... input) { return std::array<std::u32string_view, sizeof...(input)>{input.value...}; },
            allArgsTuple);
        constexpr std::size_t setSize = unique<false>(allArgsSVArray);
        constexpr auto uniqueSet = unique<true, allArgsSVArray.size(), setSize>(allArgsSVArray);

        constexpr auto foundOptionals = evaluateOptional(uniqueSet, tuple);
        return std::pair{tuple, foundOptionals};
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

template <class T, auto&... args>
constexpr auto parseOptions()
{
    constexpr auto lexems = T::parseOptionsImpl().first;
    constexpr auto tuple = toPair<args...>();
    constexpr auto value = std::apply(
        [](auto&&... values) {
            constexpr auto arguments = T::parseOptionsImpl().second;
            using Tuple =
                std::tuple<std::conditional_t<isOptional(std::u32string_view(
                                                             std::decay_t<decltype(values.first)>::pointer->begin(),
                                                             std::decay_t<decltype(values.first)>::pointer->size()),
                                                         arguments),
                                              std::optional<typename decltype(unpack(values.second))::type>,
                                              typename decltype(unpack(values.second))::type>...>;
            return type_identity<Tuple>{};
        },
        tuple);

    return CommandLineOption(value, lexems,
                             std::apply(
                                 [](auto&&... values) {
                                     return std::array{
                                         std::u32string_view(std::decay_t<decltype(values.first)>::pointer->begin(),
                                                             std::decay_t<decltype(values.first)>::pointer->size())...};
                                 },
                                 tuple));
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

} // namespace detail::CommandLine

class CommandLine
{
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

    template <auto* cliOption, std::size_t i, class Storage>
    static bool checkAlternative(llvm::MutableArrayRef<std::string_view>& commandLine, Storage& storage)
    {
        constexpr auto alternative = std::get<i>(cliOption->getAlternatives());
        constexpr static auto array = detail::CommandLine::tupleToArray(alternative);
        storage.emplace();
        auto copy = commandLine;
        std::vector<std::string_view> contents(commandLine.begin(),
                                               commandLine.begin() + std::min(array.size(), commandLine.size()));
        bool success = YComb{[&, &storage = storage](auto&& self, auto indexT) -> bool {
            constexpr std::size_t index = decltype(indexT)::value;
            constexpr auto curr = array[index];
            if constexpr (std::holds_alternative<detail::CommandLine::Text>(curr))
            {
                constexpr auto utf32 = cld::get<detail::CommandLine::Text>(curr).value;
                constexpr std::size_t utf8Size = detail::CommandLine::utf32ToUtf8<false, utf32.size() * 4>(utf32);
                constexpr static auto utf8Array = detail::CommandLine::utf32ToUtf8<true, utf8Size>(utf32);
                constexpr std::string_view prefix(utf8Array.data(), utf8Array.size());
                if (commandLine.empty())
                {
                    return false;
                }
                if (commandLine.front().substr(0, prefix.size()) == prefix)
                {
                    commandLine.front().remove_prefix(prefix.size());
                }
                else
                {
                    return false;
                }
            }
            else if constexpr (std::holds_alternative<detail::CommandLine::Arg>(curr))
            {
                constexpr std::size_t storageIndex =
                    indexOf(cliOption->getArgNames(), cld::get<detail::CommandLine::Arg>(curr).value);
                using ArgType = std::tuple_element_t<storageIndex, std::decay_t<decltype(*storage)>>;
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
                    auto end =
                        std::find_if(commandLine.front().begin(), commandLine.front().end(),
                                     [separator = cld::get<detail::CommandLine::Text>(array[index + 1]).value[0]](
                                         char c) { return c == (char)separator; });
                    text = commandLine.front().substr(0, end - commandLine.front().begin());
                    commandLine.front().remove_prefix(text.size());
                }
                else
                {
                    CLD_UNREACHABLE;
                }
                if constexpr (std::is_same_v<std::string, ArgType> || std::is_same_v<std::string_view, ArgType>)
                {
                    std::get<storageIndex>(*storage) = text;
                }
                else if constexpr (std::is_integral_v<ArgType>)
                {
                    std::get<storageIndex>(*storage) = 0;
                    if constexpr (IsOptional<std::decay_t<decltype(std::get<storageIndex>(*storage))>>{})
                    {
                        std::from_chars(text.begin(), text.end(), &*std::get<storageIndex>(*storage));
                    }
                    else
                    {
                        std::from_chars(text.begin(), text.end(), std::get<storageIndex>(*storage));
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
        storage.reset();
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
                        if constexpr (std::is_same_v<std::tuple_element_t<1, Tuple>, detail::CommandLine::Arg>)
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

public:
    template <auto&... options>
    static CommandLine parse(llvm::MutableArrayRef<std::string_view> commandLine);
};

template <auto&... options>
CommandLine CommandLine::parse(llvm::MutableArrayRef<std::string_view> commandLine)
{
    constexpr auto tuple = std::make_tuple(detail::CommandLine::Pointer<&options>{}...);
    auto storage =
        std::make_tuple(std::pair{options, std::optional<typename std::decay_t<decltype(options)>::value_type>{}}...);
    std::vector<std::string_view> unrecognized;
    while (!commandLine.empty())
    {
        if (!std::apply(
                [&](auto&&... indices) -> bool {
                    return ([&](auto index) -> bool {
                        constexpr std::size_t i = decltype(index)::value;
                        return checkAllAlternatives<std::tuple_element_t<i, decltype(tuple)>::pointer>(
                            commandLine, std::get<i>(storage).second);
                    }(indices) || ...);
                },
                integerSequenceToTuple(std::make_index_sequence<sizeof...(options)>{})))
        {
            unrecognized.push_back(commandLine.front());
            commandLine = commandLine.drop_front();
        }
    }
    return CommandLine();
}
} // namespace cld

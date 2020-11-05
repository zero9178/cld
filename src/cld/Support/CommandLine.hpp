#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <cld/Frontend/Compiler/Message.hpp>
#include <cld/Support/Constexpr.hpp>
#include <cld/Support/Text.hpp>
#include <cld/Support/Util.hpp>

#include <optional>
#include <string_view>
#include <tuple>
#include <unordered_set>
#include <variant>
#include <vector>

#include <ctre.hpp>

namespace cld
{
enum class CLIMultiArg
{
    Overwrite,
    List,
    BitwiseMerge
};

struct CommandLineOptionBase
{
};

template <class ReturnType, std::size_t args, class... Alternatives>
class CommandLineOption : public CommandLineOptionBase
{
    std::string_view m_firstOptionString;
    std::tuple<Alternatives...> m_alternatives;
    std::array<std::u32string_view, args> m_argNameToRetTypePos;
    std::string_view m_description;
    CLIMultiArg m_multiArg;

public:
    constexpr CommandLineOption(type_identity<ReturnType>, std::string_view firstOptionString,
                                std::tuple<Alternatives...> alternatives,
                                std::array<std::u32string_view, args> argNameToRetTypePos, std::string_view description,
                                CLIMultiArg multiArg)
        : m_firstOptionString(firstOptionString),
          m_alternatives(std::move(alternatives)),
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

    constexpr std::string_view getFirstOptionString() const
    {
        return {m_firstOptionString.data(), m_firstOptionString.size()};
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

template <std::size_t inputSize, std::size_t maxOutputSize = inputSize>
constexpr auto unique(std::array<std::u32string_view, inputSize> array)
{
    MaxVector<std::u32string_view, maxOutputSize> result;
    for (std::size_t i = 0; i < inputSize; i++)
    {
        bool contains = false;
        for (std::size_t i2 = 0; i2 < result.size(); i2++)
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
        result.push_back(array[i]);
    }
    return result;
}

template <std::size_t size, class Tuple>
constexpr std::array<std::pair<std::u32string_view, bool>, size>
    evaluateOptional(MaxVector<std::u32string_view, size> args, const Tuple& tuple)
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

template <class... Args>
constexpr bool containsNegate(std::tuple<Args...> tuple)
{
    auto array = tupleToArray(tuple);
    for (std::size_t i = 0; i < array.size(); i++)
    {
        if (std::holds_alternative<NegationOption>(array[i]))
        {
            return true;
        }
    }
    return false;
}

template <auto& option>
constexpr bool containsNegate()
{
    return std::apply([](auto... alternatives) -> bool { return (containsNegate(alternatives) || ...); },
                      option.getAlternatives());
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
        constexpr auto firstString = std::get<0>(std::tuple(args...));
        constexpr auto u8String = Constexpr::utf32ToUtf8<firstString.size()>({firstString.begin(), firstString.size()});
        // libc++ until either version LLVM 11 or 12 does not support constexpr std::array<T,0>
        if constexpr (std::tuple_size_v<std::decay_t<decltype(allArgsTuple)>> != 0)
        {
            constexpr auto allArgsSVArray = std::apply(
                [](auto&&... input) { return std::array<std::u32string_view, sizeof...(input)>{input.value...}; },
                allArgsTuple);
            constexpr auto uniqueSet = unique(allArgsSVArray);
            constexpr auto foundOptionals = evaluateOptional<uniqueSet.size()>(uniqueSet, tuple);
            return std::tuple{tuple, foundOptionals, u8String};
        }
        else
        {
            return std::tuple{tuple, std::make_tuple(), u8String};
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

template <class T>
constexpr auto parsedTuple = T::parseOptionsImpl();

template <class T, auto&... args>
constexpr auto parseOptions(std::string_view description, CLIMultiArg multiArg = CLIMultiArg::Overwrite)
{
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
            constexpr auto arguments = std::get<1>(parsedTuple<T>);
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

    return CommandLineOption(value, {std::get<2>(parsedTuple<T>).data(), std::get<2>(parsedTuple<T>).size()},
                             std::get<0>(parsedTuple<T>),
                             std::apply(
                                 [](auto&&... values) {
                                     return std::array<std::u32string_view, sizeof...(values)>{
                                         std::u32string_view(std::decay_t<decltype(values.first)>::pointer->begin(),
                                                             std::decay_t<decltype(values.first)>::pointer->size())...};
                                 },
                                 tuple),
                             description, multiArg);
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

template <>
struct ValueType<bool>
{
    using type = bool;
};

template <class T>
class SmallFunction;

template <class Ret, class... Params>
class SmallFunction<Ret(Params...)>
{
    alignas(std::max_align_t) std::byte m_storage[32];
    Ret (*m_call)(std::byte*, Params...) = nullptr;

    template <class F>
    static Ret call(std::byte* storage, Params... args)
    {
        return (*reinterpret_cast<F*>(storage))(std::forward<Params>(args)...);
    }

public:
    SmallFunction() = default;

    template <class F>
    SmallFunction(F&& f) : m_call(call<std::remove_reference_t<F>>)
    {
        static_assert(std::is_trivially_destructible_v<
                          std::remove_reference_t<F>> && sizeof(std::remove_reference_t<F>) <= sizeof(m_storage));
        constexpr std::size_t alignOfLambda = alignof(std::remove_reference_t<F>);
        static_assert(alignOfLambda <= alignof(std::max_align_t));
        new (m_storage) std::remove_reference_t<F>(std::forward<F>(f));
    }

    template <class... Args>
    Ret operator()(Args&&... args)
    {
        CLD_ASSERT(m_call);
        return (*m_call)(m_storage, std::forward<Args>(args)...);
    }
};

template <class T>
struct tupleSizeOr1
{
    constexpr static std::size_t value = 1;
};

template <class... Args>
struct tupleSizeOr1<std::tuple<Args...>>
{
    constexpr static std::size_t value = sizeof...(Args);
};

template <auto* cliOption, std::size_t i, std::size_t index>
constexpr auto arg = std::get<index>(std::get<i>(cliOption->getAlternatives()));

template <auto* cliOption, std::size_t size, class Storage>
class Mutator
{
    MaxVector<SmallFunction<void()>, size * 3> m_queue;
    std::size_t m_currentIndex{};
    std::size_t m_currentPos{};
    llvm::MutableArrayRef<std::string_view>& m_commandLine;
    Storage& m_storage;
    bool m_removeFromStorage{};
    bool m_firstArg = true;

    template <std::size_t i, std::size_t index>
    constexpr static bool isLast()
    {
        using Tuple = std::decay_t<decltype(cliOption->getAlternatives())>;
        using TupleArg = std::tuple_element_t<i, Tuple>;
        return index + 1 == std::tuple_size_v<TupleArg>;
    }

public:
    Mutator(llvm::MutableArrayRef<std::string_view>& commandLine, Storage& storage)
        : m_commandLine(commandLine), m_storage(storage)
    {
        if constexpr (std::is_same_v<bool, Storage>)
        {
            m_queue.push_back([this] { m_storage = true; });
        }
    }

    bool tryConsume(std::string_view text)
    {
        if (m_currentIndex >= m_commandLine.size())
        {
            return false;
        }
        if (m_commandLine[m_currentIndex].substr(m_currentPos, text.size()) != text)
        {
            return false;
        }
        m_queue.push_back([this, size = text.size()] { m_commandLine.front().remove_prefix(size); });
        m_currentPos += text.size();
        return true;
    }

    bool tryNegate(std::string_view text)
    {
        if (!tryConsume(text))
        {
            return true;
        }
        if constexpr (cliOption->getMultiArg() == CLIMultiArg::Overwrite)
        {
            m_queue.push_back([this] { m_storage = {}; });
        }
        else if constexpr (cliOption->getMultiArg() == CLIMultiArg::List)
        {
            m_removeFromStorage = true;
        }
        return true;
    }

    template <std::size_t i, std::size_t index>
    bool tryParseArg()
    {
        if (m_currentIndex >= m_commandLine.size() || m_commandLine[m_currentIndex].size() <= m_currentPos)
        {
            return false;
        }
        std::string_view text;
        if constexpr (isLast<i, index>())
        {
            text = m_commandLine[m_currentIndex].substr(m_currentPos);
            m_queue.push_back([this, size = text.size()] { m_commandLine.front().remove_prefix(size); });
            m_currentPos += text.size();
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(arg<cliOption, i, index + 1>)>, Whitespace>)
        {
            text = m_commandLine[m_currentIndex].substr(m_currentPos);
            m_queue.push_back([this, size = text.size()] { m_commandLine.front().remove_prefix(size); });
            m_currentPos += text.size();
        }
        else if constexpr (std::is_same_v<std::decay_t<decltype(arg<cliOption, i, index + 1>)>, Text>)
        {
            constexpr auto utf32 = arg<cliOption, i, index + 1>.value;
            constexpr auto utf8Array = Constexpr::utf32ToUtf8<utf32.size() * 4>(utf32);
            auto end = m_commandLine[m_currentIndex].find_first_of({utf8Array.data(), utf8Array.size()}, m_currentPos);

            text = m_commandLine[m_currentIndex].substr(m_currentPos, end - m_currentPos);
            m_queue.push_back([this, size = text.size()] { m_commandLine.front().remove_prefix(size); });
            m_currentPos += text.size();
        }
        else
        {
            static_assert(always_false<std::integral_constant<std::size_t, i>>);
        }

        constexpr std::size_t storageIndex = indexOf(cliOption->getArgNames(), arg<cliOption, i, index>.value);
        constexpr bool isTupleVector = IsTuple<std::decay_t<typename ValueType<Storage>::type>>{};
        using ArgTypeT = typename std::conditional_t<
            isTupleVector, std::tuple_element<storageIndex, std::decay_t<typename ValueType<Storage>::type>>,
            std::decay<typename ValueType<Storage>::type>>::type;
        using ArgType =
            typename std::conditional_t<IsOptional<ArgTypeT>{}, ValueType<ArgTypeT>, type_identity<ArgTypeT>>::type;
        auto assign = [=](auto&& value) {
            if constexpr (cliOption->getMultiArg() == CLIMultiArg::List)
            {
                if constexpr (containsNegate<*cliOption>())
                {
                    if (m_removeFromStorage)
                    {
                        m_queue.push_back([this, value] { m_storage.erase(value); });
                    }
                    else
                    {
                        m_queue.push_back([this, value] { m_storage.insert(value); });
                    }
                }
                else
                {
#if defined(_MSC_VER) && !defined(__clang__)
                    constexpr bool isTupleVector = IsTuple<std::decay_t<typename ValueType<Storage>::type>>{};
#endif
                    if constexpr (isTupleVector)
                    {
                        if (m_firstArg)
                        {
                            m_queue.push_back([this] { m_storage.emplace_back(); });
                        }
                        m_queue.push_back([=] {
#if defined(_MSC_VER) && !defined(__clang__)
                            constexpr std::size_t storageIndex =
                                indexOf(cliOption->getArgNames(), arg<cliOption, i, index>.value);
#endif
                            std::get<storageIndex>(m_storage.back()) = value;
                        });
                    }
                    else
                    {
                        m_queue.push_back([this, value] {
                            m_storage.emplace_back();
                            m_storage.back() = value;
                        });
                    }
                }
            }
            else
            {
                m_queue.push_back([this, value] { m_storage = value; });
            }
        };
        if constexpr (std::is_same_v<std::string, ArgType> || std::is_same_v<std::string_view, ArgType>)
        {
            assign(text);
        }
        else if constexpr (std::is_integral_v<ArgType>)
        {
            if constexpr (std::is_signed_v<ArgType>)
            {
                assign(std::stoll(cld::to_string(text)));
            }
            else
            {
                assign(std::stoull(cld::to_string(text)));
            }
        }

        if (m_firstArg)
        {
            m_firstArg = false;
        }
        return true;
    }

    bool tryWhitespace()
    {
        if (m_currentIndex < m_commandLine.size() && m_currentPos < m_commandLine[m_currentIndex].size())
        {
            return false;
        }
        if (m_currentIndex < m_commandLine.size())
        {
            m_queue.push_back([this] { m_commandLine = m_commandLine.drop_front(); });
            m_currentIndex++;
            m_currentPos = 0;
        }
        return true;
    }

    bool tryEnd()
    {
        if (m_currentIndex >= m_commandLine.size())
        {
            return true;
        }
        if (m_commandLine[m_currentIndex].size() <= m_currentPos)
        {
            m_queue.push_back([this] { m_commandLine = m_commandLine.drop_front(); });
            m_currentIndex++;
            m_currentPos = 0;
            return true;
        }
        return false;
    }

    void exec()
    {
        for (auto& iter : m_queue)
        {
            iter();
        }
    }
};

template <auto* cliOption, std::size_t i, std::size_t index, class Storage, std::size_t size>
bool evaluateArg(Mutator<cliOption, size, Storage>& mutator)
{
    using ArgType = std::decay_t<decltype(arg<cliOption, i, index>)>;
    if constexpr (std::is_same_v<ArgType, Text>)
    {
        constexpr auto utf32 = arg<cliOption, i, index>.value;
        constexpr auto utf8Array = Constexpr::utf32ToUtf8<utf32.size() * 4>(utf32);
        if (!mutator.tryConsume({utf8Array.data(), utf8Array.size()}))
        {
            return false;
        }
    }
    else if constexpr (std::is_same_v<ArgType, NegationOption>)
    {
        constexpr auto utf32 = arg<cliOption, i, index>.value;
        constexpr auto utf8Array = Constexpr::utf32ToUtf8<utf32.size() * 4>(utf32);
        if (!mutator.tryNegate({utf8Array.data(), utf8Array.size()}))
        {
            return false;
        }
    }
    else if constexpr (std::is_same_v<ArgType, Arg>)
    {
        if (!mutator.template tryParseArg<i, index>())
        {
            return false;
        }
    }
    else if constexpr (std::is_same_v<ArgType, Whitespace>)
    {
        if (!mutator.tryWhitespace())
        {
            return false;
        }
    }

    return true;
}

struct Failure
{
    std::size_t failedIndex;
    std::optional<cld::Message> message{};
};

template <auto* cliOption, std::size_t i, class Storage>
std::optional<Failure> checkAlternative2(llvm::MutableArrayRef<std::string_view>& commandLine, Storage& storage)
{
    using AlternativeType = std::tuple_element_t<i, std::decay_t<decltype(cliOption->getAlternatives())>>;
    Mutator<cliOption, std::tuple_size_v<AlternativeType>, Storage> mutator(commandLine, storage);
    auto failure = YComb{[&](auto&& self, auto indexT) -> std::optional<std::size_t> {
        constexpr std::size_t index = std::decay_t<decltype(indexT)>::value;
        if (!evaluateArg<cliOption, i, index>(mutator))
        {
            return index;
        }
        if constexpr (index + 1 < std::tuple_size_v<AlternativeType>)
        {
            return self(std::integral_constant<std::size_t, index + 1>{});
        }
        else
        {
            if (!mutator.tryEnd())
            {
                return index;
            }
            return std::nullopt;
        }
    }}(std::integral_constant<std::size_t, 0>{});
    if (failure)
    {
        return Failure{*failure};
    }

    mutator.exec();
    return std::nullopt;
}

template <auto* cliOption, class Storage>
bool checkAllAlternatives(llvm::MutableArrayRef<std::string_view>& commandLine, Storage& storage)
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
                constexpr auto utf8Prefix = Constexpr::utf32ToUtf8<u32prefix.size() * 4>(u32prefix);
                if (commandLine.front() == std::string_view(utf8Prefix.data(), utf8Prefix.size()))
                {
                    return !checkAlternative2<cliOption, i>(commandLine, storage).has_value();
                }
                if constexpr (std::tuple_size_v<Tuple> >= 2)
                {
                    if constexpr (!std::is_same_v<std::tuple_element_t<1, Tuple>, detail::CommandLine::Whitespace>)
                    {
                        if (commandLine.front().substr(0, utf8Prefix.size())
                            == std::string_view(utf8Prefix.data(), utf8Prefix.size()))
                        {
                            return !checkAlternative2<cliOption, i>(commandLine, storage).has_value();
                        }
                    }
                }
                return false;
            }(indices) || ...);
        },
        Constexpr::integerSequenceToTuple(std::make_index_sequence<std::tuple_size_v<AllAlternatives>>{}));
}

template <auto& option>
using OptionStorage =
    std::conditional_t<option.getMultiArg() == CLIMultiArg::List,
                       std::conditional_t<containsNegate<option>(),
                                          std::unordered_set<typename std::decay_t<decltype(option)>::value_type>,
                                          std::vector<typename std::decay_t<decltype(option)>::value_type>>,
                       std::conditional_t<tupleSizeOr1<typename std::decay_t<decltype(option)>::value_type>::value == 0,
                                          bool, std::optional<typename std::decay_t<decltype(option)>::value_type>>>;

void printHelp(llvm::raw_ostream& os, std::vector<std::pair<std::string_view, std::string_view>> options);

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
        return std::get<1>(std::get<std::tuple<detail::CommandLine::Pointer<&option>,
                                               detail::CommandLine::OptionStorage<option>, int>>(m_storage));
    }

    template <auto& option>
    [[nodiscard]] int pos() const noexcept
    {
        return std::get<2>(std::get<std::tuple<detail::CommandLine::Pointer<&option>,
                                               detail::CommandLine::OptionStorage<option>, int>>(m_storage));
    }

    template <auto&... options>
    [[nodiscard]] const CommandLineOptionBase* lastSpecified() const noexcept
    {
        static_assert(sizeof...(options) > 0);
        std::array<std::pair<const CommandLineOptionBase*, int>, sizeof...(options)> temp = {
            std::pair{&options, pos<options>()}...};
        auto result = std::max_element(temp.begin(), temp.end(),
                                       [](const auto& lhs, const auto& rhs) { return lhs.second < rhs.second; });
        if (result->second < 0)
        {
            return nullptr;
        }
        return result->first;
    }

    void printHelp(llvm::raw_ostream& os) const
    {
        std::vector<std::pair<std::string_view, std::string_view>> textAndDesc;
        std::apply(
            [&](auto&&... values) {
                (
                    [&](auto&& tuple) {
                        using Tuple = std::decay_t<decltype(tuple)>;
                        using Pointer = std::tuple_element_t<0, Tuple>;
                        textAndDesc.emplace_back(Pointer::pointer->getFirstOptionString(),
                                                 Pointer::pointer->getDescription());
                    }(values),
                    ...);
            },
            m_storage);
        ::cld::detail::CommandLine::printHelp(os, std::move(textAndDesc));
    }
};

namespace cli
{
template <auto& option>
struct InitialStorage
{
    static detail::CommandLine::OptionStorage<option> getInitial()
    {
        return {};
    }
};
} // namespace cli

template <auto&... options>
auto parseCommandLine(llvm::MutableArrayRef<std::string_view> commandLine)
{
    constexpr auto tuple = std::make_tuple(detail::CommandLine::Pointer<&options>{}...);
    auto storage = std::make_tuple(std::tuple{
        detail::CommandLine::Pointer<&options>{},
        detail::CommandLine::OptionStorage<options>{::cld::cli::InitialStorage<options>::getInitial()}, -1}...);
    std::vector<std::string_view> unrecognized;
    int counter = 0;
    while (!commandLine.empty())
    {
        if (!std::apply(
                [&](auto&&... indices) -> bool {
                    return ([&](auto index) -> bool {
                        constexpr std::size_t i = decltype(index)::value;
                        if (detail::CommandLine::checkAllAlternatives<
                                std::tuple_element_t<i, decltype(tuple)>::pointer>(commandLine,
                                                                                   std::get<1>(std::get<i>(storage))))
                        {
                            std::get<2>(std::get<i>(storage)) = counter;
                            return true;
                        }
                        return false;
                    }(indices) || ...);
                },
                Constexpr::integerSequenceToTuple(std::make_index_sequence<sizeof...(options)>{})))
        {
            unrecognized.push_back(commandLine.front());
            commandLine = commandLine.drop_front();
        }
        counter++;
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

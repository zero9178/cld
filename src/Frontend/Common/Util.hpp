#pragma once

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <variant>

#ifdef NDEBUG

    #ifndef CLD_USE_ASSERTS

        #ifdef __clang__

            #define CLD_UNREACHABLE          \
                do                           \
                    __builtin_unreachable(); \
                while (0)

            #define CLD_ASSERT(x) __builtin_assume(bool(x))

        #elif defined(__GNUC__)

            #define CLD_UNREACHABLE          \
                do                           \
                    __builtin_unreachable(); \
                while (0)

            #define CLD_ASSERT(x) \
                if (x)            \
                    ;             \
                else              \
                    __builtin_unreachable()

        #elif defined(_MSC_VER)

            #define CLD_UNREACHABLE  \
                do                   \
                    __assume(false); \
                while (0)

            #define CLD_ASSERT(x) __assume((bool)(x))

        #else

            #define CLD_UNREACHABLE

            #define CLD_ASSERT(x)

        #endif

    #else

        #define CLD_UNREACHABLE \
            do                  \
                std::abort();   \
            while (0)

        #define CLD_ASSERT(x)                                            \
            do                                                           \
            {                                                            \
                if (!(x))                                                \
                {                                                        \
                    fprintf(stderr, __FILE__ ":%d: " #x "\n", __LINE__); \
                    std::abort();                                        \
                }                                                        \
            } while (0)

    #endif

#else

    #define CLD_ASSERT(x) assert(x)

    #define CLD_UNREACHABLE                                                              \
        do                                                                               \
        {                                                                                \
            assert(false);                                                               \
            std::abort(); /* So that the compiler sees code afterwards is unreachable */ \
        } while (0)

#endif

namespace cld
{
template <typename T, typename Variant>
decltype(auto) get(Variant&& variant) noexcept
{
    CLD_ASSERT(!variant.valueless_by_exception() && std::holds_alternative<T>(variant));
    auto* value = std::get_if<T>(&variant);
    CLD_ASSERT(value);
    if constexpr (std::is_lvalue_reference_v<Variant>)
    {
        return *value;
    }
    else
    {
        return std::move(*value);
    }
}

template <std::size_t i, typename Variant>
decltype(auto) get(Variant&& variant) noexcept
{
    CLD_ASSERT(!variant.valueless_by_exception() && variant.index() == i);
    auto* value = std::get_if<i>(&variant);
    CLD_ASSERT(value);
    if constexpr (std::is_lvalue_reference_v<Variant>)
    {
        return *value;
    }
    else
    {
        return std::move(*value);
    }
}

namespace detail
{
template <class... Ts>
struct overload : Ts...
{
    using Ts::operator()...;
};
template <class... Ts>
overload(Ts...) -> overload<Ts...>;

template <typename G>
struct Y
{
    template <typename... X>
    decltype(auto) operator()(X&&... x) const&
    {
        return g(*this, std::forward<X>(x)...);
    }

    G g;
};

template <typename G>
Y(G) -> Y<G>;

template <class T>
struct Identity
{
    using type = T;
};

template <typename G, class Ret>
struct YWithRet
{
    template <typename... X>
    Ret operator()(X&&... x) const&
    {
        if constexpr (std::is_void_v<std::decay_t<Ret>>)
        {
            g(*this, std::forward<X>(x)...);
        }
        else
        {
            return g(*this, std::forward<X>(x)...);
        }
    }

    G g;
    Identity<Ret> id;
};

template <typename G, class T>
YWithRet(G, Identity<T>) -> YWithRet<G, T>;

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 0 || i > 12)>* = nullptr)
{
    if (variant.index() == i)
    {
        return std::forward<Callable>(callable)(cld::get<i>(std::forward<Variant>(variant)));
    }
    else if constexpr (i != 0)
    {
        return visitImpl<i - 1>(std::forward<Callable>(callable), std::forward<Variant>(variant));
    }
    else
    {
        CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 1)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 2)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 3)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 4)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 5)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 6)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 7)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        case 7: return std::forward<Callable>(callable)(cld::get<7>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 8)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        case 7: return std::forward<Callable>(callable)(cld::get<7>(std::forward<Variant>(variant)));
        case 8: return std::forward<Callable>(callable)(cld::get<8>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 9)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        case 7: return std::forward<Callable>(callable)(cld::get<7>(std::forward<Variant>(variant)));
        case 8: return std::forward<Callable>(callable)(cld::get<8>(std::forward<Variant>(variant)));
        case 9: return std::forward<Callable>(callable)(cld::get<9>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 10)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        case 7: return std::forward<Callable>(callable)(cld::get<7>(std::forward<Variant>(variant)));
        case 8: return std::forward<Callable>(callable)(cld::get<8>(std::forward<Variant>(variant)));
        case 9: return std::forward<Callable>(callable)(cld::get<9>(std::forward<Variant>(variant)));
        case 10: return std::forward<Callable>(callable)(cld::get<10>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 11)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        case 7: return std::forward<Callable>(callable)(cld::get<7>(std::forward<Variant>(variant)));
        case 8: return std::forward<Callable>(callable)(cld::get<8>(std::forward<Variant>(variant)));
        case 9: return std::forward<Callable>(callable)(cld::get<9>(std::forward<Variant>(variant)));
        case 10: return std::forward<Callable>(callable)(cld::get<10>(std::forward<Variant>(variant)));
        case 11: return std::forward<Callable>(callable)(cld::get<11>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 12)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        case 2: return std::forward<Callable>(callable)(cld::get<2>(std::forward<Variant>(variant)));
        case 3: return std::forward<Callable>(callable)(cld::get<3>(std::forward<Variant>(variant)));
        case 4: return std::forward<Callable>(callable)(cld::get<4>(std::forward<Variant>(variant)));
        case 5: return std::forward<Callable>(callable)(cld::get<5>(std::forward<Variant>(variant)));
        case 6: return std::forward<Callable>(callable)(cld::get<6>(std::forward<Variant>(variant)));
        case 7: return std::forward<Callable>(callable)(cld::get<7>(std::forward<Variant>(variant)));
        case 8: return std::forward<Callable>(callable)(cld::get<8>(std::forward<Variant>(variant)));
        case 9: return std::forward<Callable>(callable)(cld::get<9>(std::forward<Variant>(variant)));
        case 10: return std::forward<Callable>(callable)(cld::get<10>(std::forward<Variant>(variant)));
        case 11: return std::forward<Callable>(callable)(cld::get<11>(std::forward<Variant>(variant)));
        case 12: return std::forward<Callable>(callable)(cld::get<12>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <class Callable, class Variant>
decltype(auto) visit(Callable&& callable, Variant&& variant)
{
    return visitImpl<std::variant_size_v<std::decay_t<Variant>> - 1>(std::forward<Callable>(callable),
                                                                     std::forward<Variant>(variant));
}
} // namespace detail

template <typename Variant, typename... Matchers>
decltype(auto) match(Variant&& variant, Matchers&&... matchers)
{
    return detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
}

template <class Ret, typename Variant, typename... Matchers>
Ret match(Variant&& variant, Matchers&&... matchers)
{
    if constexpr (std::is_void_v<std::decay_t<Ret>>)
    {
        detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
    }
    else
    {
        return detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
    }
}

template <typename Variant, typename... Matchers>
decltype(auto) matchWithSelf(Variant&& variant, Matchers&&... matchers)
{
    return detail::visit(detail::Y{detail::overload{std::forward<Matchers>(matchers)...}},
                         std::forward<Variant>(variant));
}

template <class Ret, typename Variant, typename... Matchers>
Ret matchWithSelf(Variant&& variant, Matchers&&... matchers)
{
    if constexpr (std::is_void_v<std::decay_t<Ret>>)
    {
        detail::visit(detail::YWithRet{detail::overload{std::forward<Matchers>(matchers)...}, detail::Identity<Ret>{}},
                      std::forward<Variant>(variant));
    }
    else
    {
        return detail::visit(detail::Y{detail::overload{std::forward<Matchers>(matchers)...}, detail::Identity<Ret>{}},
                             std::forward<Variant>(variant));
    }
}

template <typename T, typename... Ts>
constexpr size_t getIndex(const std::variant<Ts...>&) noexcept
{
    size_t r = 0;
    auto test = [&](bool b) {
        if (!b)
            ++r;
        return b;
    };
    (test(std::is_same_v<T, Ts>) || ...);
    return r;
}

template <class T, typename = void>
struct IsTupleLike : std::false_type
{
};

template <class T>
struct IsTupleLike<T, std::void_t<typename std::tuple_size<T>::type>> : std::true_type
{
};

template <class T>
struct IsVariant : std::false_type
{
};

template <class... T>
struct IsVariant<std::variant<T...>> : std::true_type
{
};

template <class T, class U, typename = void>
struct IsEqualComparable : std::false_type
{
};

template <class T, class U>
struct IsEqualComparable<T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>> : std::true_type
{
};

template <typename, typename = void>
constexpr bool IsTypeCompleteV = false;

template <typename T>
constexpr bool IsTypeCompleteV<T, std::void_t<decltype(sizeof(T))>> = true;

} // namespace cld

#pragma once

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <memory>
#include <variant>

#ifdef NDEBUG

    #ifndef CLD_USE_ASSERTS

        #ifdef __clang__

            #define CLD_UNREACHABLE          \
                do                           \
                    __builtin_unreachable(); \
                while (0)

            #define CLD_ASSERT(...) __builtin_assume(bool(__VA_ARGS__))

        #elif defined(__GNUC__)

            #define CLD_UNREACHABLE          \
                do                           \
                    __builtin_unreachable(); \
                while (0)

            #define CLD_ASSERT(...) \
                if (__VA_ARGS__)    \
                    ;               \
                else                \
                    __builtin_unreachable()

        #elif defined(_MSC_VER)

            #define CLD_UNREACHABLE  \
                do                   \
                    __assume(false); \
                while (0)

            #define CLD_ASSERT(...) __assume((bool)(__VA_ARGS__))

        #else

            #define CLD_UNREACHABLE

            #define CLD_ASSERT(...)

        #endif

    #else

        #define CLD_UNREACHABLE \
            do                  \
                std::abort();   \
            while (0)

        #define CLD_ASSERT(...)                                                    \
            do                                                                     \
            {                                                                      \
                if (!(__VA_ARGS__))                                                \
                {                                                                  \
                    fprintf(stderr, __FILE__ ":%d: " #__VA_ARGS__ "\n", __LINE__); \
                    std::abort();                                                  \
                }                                                                  \
            } while (0)

    #endif

#else

    #define CLD_ASSERT(...)                                                    \
        do                                                                     \
        {                                                                      \
            if (!(__VA_ARGS__))                                                \
            {                                                                  \
                fprintf(stderr, __FILE__ ":%d: " #__VA_ARGS__ "\n", __LINE__); \
                std::abort();                                                  \
            }                                                                  \
        } while (0)

    #define CLD_UNREACHABLE                                                              \
        do                                                                               \
        {                                                                                \
            std::abort(); /* So that the compiler sees code afterwards is unreachable */ \
        } while (0)

#endif

#ifdef __clang__
    #define CLD_NON_NULL _Nonnull
    #define CLD_NULLABLE _Nullable
#else
    #define CLD_NON_NULL
    #define CLD_NULLABLE
#endif

namespace cld
{
template <typename T, typename Variant>
constexpr decltype(auto) get(Variant&& variant) noexcept
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
constexpr decltype(auto) get(Variant&& variant) noexcept
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

template <typename G>
struct YComb
{
    template <typename... X>
    constexpr decltype(auto) operator()(X&&... x) const
    {
        return g(*this, std::forward<X>(x)...);
    }

    template <typename... X>
    constexpr decltype(auto) operator()(X&&... x)
    {
        return g(*this, std::forward<X>(x)...);
    }

    G g;
};

template <typename G>
YComb(G) -> YComb<G>;

template <class T>
struct type_identity
{
    using type = T;
};

namespace detail
{
template <class... Ts>
struct overload : Ts...
{
    using Ts::operator()...;
};
template <class... Ts>
overload(Ts...) -> overload<Ts...>;

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
    type_identity<Ret> id;
};

template <typename G, class T>
YWithRet(G, type_identity<T>) -> YWithRet<G, T>;

template <std::size_t i, class Callable, class Variant>
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant,
                                   std::enable_if_t<(i == 0 || i > 12)>* = nullptr)
{
    if (variant.index() == i)
    {
        return std::forward<Callable>(callable)(cld::get<i>(std::forward<Variant>(variant)));
    }
    if constexpr (i != 0)
    {
        return visitImpl<i - 1>(std::forward<Callable>(callable), std::forward<Variant>(variant));
    }
    else
    {
        CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 1)>* = nullptr)
{
    switch (variant.index())
    {
        case 0: return std::forward<Callable>(callable)(cld::get<0>(std::forward<Variant>(variant)));
        case 1: return std::forward<Callable>(callable)(cld::get<1>(std::forward<Variant>(variant)));
        default: CLD_UNREACHABLE;
    }
}

template <std::size_t i, class Callable, class Variant>
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 2)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 3)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 4)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 5)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 6)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 7)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 8)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 9)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 10)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 11)>* = nullptr)
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
constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 12)>* = nullptr)
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
constexpr decltype(auto) visit(Callable&& callable, Variant&& variant)
{
    CLD_ASSERT(!variant.valueless_by_exception());
    return visitImpl<std::variant_size_v<std::decay_t<Variant>> - 1>(std::forward<Callable>(callable),
                                                                     std::forward<Variant>(variant));
}
} // namespace detail

template <typename Variant, typename... Matchers>
constexpr decltype(auto) match(Variant&& variant, Matchers&&... matchers)
{
    CLD_ASSERT(!variant.valueless_by_exception());
    return detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
}

template <class Ret, typename Variant, typename... Matchers>
constexpr Ret match(Variant&& variant, Matchers&&... matchers)
{
    static_assert(std::is_void_v<Ret>, "Only explicit return type of void allowed");
    detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
}

template <typename Variant, typename... Matchers>
constexpr decltype(auto) matchWithSelf(Variant&& variant, Matchers&&... matchers)
{
    return detail::visit(YComb{detail::overload{std::forward<Matchers>(matchers)...}}, std::forward<Variant>(variant));
}

template <class Ret, typename Variant, typename... Matchers>
constexpr Ret matchWithSelf(Variant&& variant, Matchers&&... matchers)
{
    static_assert(std::is_void_v<Ret>, "Only explicit return type of void allowed");
    detail::visit(detail::YWithRet{detail::overload{std::forward<Matchers>(matchers)...}, type_identity<Ret>{}},
                  std::forward<Variant>(variant));
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

template <class... Args>
constexpr std::size_t hashCombine(const Args&... args)
{
    std::size_t seed = 0;
    ((seed ^= std::hash<std::decay_t<Args>>{}(args) + 0x9e3779b9 + (seed << 6) + (seed >> 2)), ...);
    return seed;
}

template <class... Args>
constexpr std::size_t rawHashCombine(Args... args)
{
    static_assert((std::is_integral_v<Args> && ...));
    std::size_t seed = 0;
    ((seed ^= args + 0x9e3779b9 + (seed << 6) + (seed >> 2)), ...);
    return seed;
}

template <class T1, class T2>
constexpr T1 roundUpTo(T1 number, T2 multiple)
{
    static_assert(std::is_integral_v<T1> && std::is_integral_v<T2>);
    if (multiple == 0)
    {
        return number;
    }

    auto remainder = number % multiple;
    if (remainder == 0)
    {
        return number;
    }

    return number + multiple - remainder;
}

namespace details
{
template <class F, class... Args>
class BindFrontImpl
{
    F f;
    std::tuple<Args...> front;

public:
    template <class F2, class... Args2>
    explicit BindFrontImpl(F2&& f2, Args2&&... args2) : f(std::forward<F2>(f2)), front(std::forward<Args2>(args2)...)
    {
    }

    template <class... Last>
    decltype(auto) operator()(Last&&... last) & noexcept(std::is_nothrow_invocable_v<F, Args..., Last...>)
    {
        return std::apply(
            [&](auto&&... members) -> decltype(auto) {
                return std::invoke(f, std::forward<decltype(members)>(members)..., std::forward<Last>(last)...);
            },
            front);
    }

    template <class... Last>
    decltype(auto) operator()(Last&&... last) const& noexcept(std::is_nothrow_invocable_v<F, Args..., Last...>)
    {
        return std::apply(
            [&](auto&&... members) -> decltype(auto) {
                return std::invoke(f, std::forward<decltype(members)>(members)..., std::forward<Last>(last)...);
            },
            front);
    }

    template <class... Last>
    decltype(auto) operator()(Last&&... last) && noexcept(std::is_nothrow_invocable_v<F, Args..., Last...>)
    {
        return std::apply(
            [&](auto&&... members) -> decltype(auto) {
                return std::invoke(std::move(f), std::forward<decltype(members)>(members)...,
                                   std::forward<Last>(last)...);
            },
            std::move(front));
    }

    template <class... Last>
    decltype(auto) operator()(Last&&... last) const&& noexcept(std::is_nothrow_invocable_v<F, Args..., Last...>)
    {
        return std::apply(
            [&](auto&&... members) -> decltype(auto) {
                return std::invoke(std::move(f), std::forward<decltype(members)>(members)...,
                                   std::forward<Last>(last)...);
            },
            std::move(front));
    }
};

} // namespace details

template <class F, class... Args>
auto bind_front(F&& f, Args&&... args)
{
    return details::BindFrontImpl<std::decay_t<F>, std::decay_t<Args>...>(std::forward<F>(f),
                                                                          std::forward<Args>(args)...);
}

} // namespace cld

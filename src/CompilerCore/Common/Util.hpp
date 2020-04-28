#pragma once

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <variant>

#ifdef NDEBUG

    #ifndef CLD_USE_ASSERTS

        #ifndef _MSC_VER

            #define CLD_UNREACHABLE          \
                do                           \
                    __builtin_unreachable(); \
                while (0)

            #define CLD_ASSERT(x) \
                if (x)            \
                    ;             \
                else              \
                    __builtin_unreachable()

        #else

            #define CLD_UNREACHABLE  \
                do                   \
                    __assume(false); \
                while (0)

            #define CLD_ASSERT(x) __assume((bool)(x))

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

template <std::size_t i = 0, class Callable, class Variant>
decltype(auto) visitImpl(Callable&& callable, Variant&& variant)
{
    if (variant.index() == i)
    {
        return std::forward<Callable>(callable)(cld::get<i>(std::forward<Variant>(variant)));
    }
    else if constexpr (i + 1 != std::variant_size_v<std::decay_t<Variant>>)
    {
        return visitImpl<i + 1>(std::forward<Callable>(callable), std::forward<Variant>(variant));
    }
    else
    {
        CLD_UNREACHABLE;
    }
}

template <class Callable, class Variant>
decltype(auto) visit(Callable&& callable, Variant&& variant)
{
    return visitImpl(std::forward<Callable>(callable), std::forward<Variant>(variant));
}
} // namespace detail

template <typename Variant, typename... Matchers>
decltype(auto) match(Variant&& variant, Matchers&&... matchers)
{
    return detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
}

template <typename Variant, typename... Matchers>
decltype(auto) matchWithSelf(Variant&& variant, Matchers&&... matchers)
{
    return detail::visit(detail::Y{detail::overload{std::forward<Matchers>(matchers)...}},
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
} // namespace cld

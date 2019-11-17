#ifndef OPENCLPARSER_UTIL_HPP
#define OPENCLPARSER_UTIL_HPP

#include <variant>

namespace OpenCL
{
    namespace detail
    {
        template <class... Ts>
        struct overload : Ts...
        {
            using Ts::operator()...;
        };
        template <class... Ts>
        overload(Ts...)->overload<Ts...>;

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
        Y(G)->Y<G>;
    } // namespace detail

    template <typename Variant, typename... Matchers>
    auto match(Variant&& variant, Matchers&&... matchers)
    {
        return std::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
    }

    template <typename Variant, typename... Matchers>
    auto stateMachine(Variant& variant, Matchers&&... matchers)
    {
        variant = std::visit(detail::overload{std::forward<Matchers>(matchers)...}, variant);
    }

    template <typename Variant, typename... Matchers>
    auto matchWithSelf(Variant&& variant, Matchers&&... matchers)
    {
        return std::visit(detail::Y{detail::overload{std::forward<Matchers>(matchers)...}},
                          std::forward<Variant>(variant));
    }
} // namespace OpenCL

#ifdef NDEBUG

    #ifndef _MSC_VER

        #define OPENCL_UNREACHABLE       \
            do                           \
                __builtin_unreachable(); \
            while (0)

    #else

        #define OPENCL_UNREACHABLE \
            do                     \
                __assume(false);   \
            while (0)

    #endif

#else

    #include <cstdlib>

    #define OPENCL_UNREACHABLE \
        do                     \
            std::abort();      \
        while (0)

#endif

#endif // OPENCLPARSER_UTIL_HPP

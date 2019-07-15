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
    auto matchWithSelf(Variant&& variant, Matchers&&... matchers)
    {
        return std::visit(detail::Y{detail::overload{std::forward<Matchers>(matchers)...}},
                          std::forward<Variant>(variant));
    }
} // namespace OpenCL

#endif // OPENCLPARSER_UTIL_HPP

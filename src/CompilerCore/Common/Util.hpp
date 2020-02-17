#ifndef OPENCLPARSER_UTIL_HPP
#define OPENCLPARSER_UTIL_HPP

#pragma warning(push, 0)
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Unicode.h>
#pragma warning(pop)

#include <cstdlib>
#include <string>
#include <variant>

namespace cld
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
    return std::visit(detail::Y{detail::overload{std::forward<Matchers>(matchers)...}}, std::forward<Variant>(variant));
}

template <typename T, typename... Ts>
constexpr size_t getIndex(const std::variant<Ts...>&)
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

inline std::string stringOfSameWidth(std::string_view original, char characterToReplace)
{
    auto utf8Width = llvm::sys::unicode::columnWidthUTF8({original.data(), original.size()});
    return std::string(utf8Width < 0 ? original.size() : utf8Width, characterToReplace);
}
} // namespace cld

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

    #define OPENCL_UNREACHABLE \
        do                     \
            std::abort();      \
        while (0)

#endif

#endif // OPENCLPARSER_UTIL_HPP

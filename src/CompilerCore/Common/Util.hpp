#pragma once

#pragma warning(push, 0)
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Unicode.h>
#pragma warning(pop)

#include <cassert>
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
} // namespace detail

template <typename Variant, typename... Matchers>
decltype(auto) match(Variant&& variant, Matchers&&... matchers)
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

        #define CLD_ASSERT(x) __assume(x)

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

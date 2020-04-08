#pragma once

#include <variant>

namespace cld::Semantics
{
template <class T, class F, template <class...> class Variant, class... Args>
const T* findRecursively(const Variant<Args...>& variant, F&& getNextFunc)
{
    static_assert(std::disjunction_v<std::is_same<Args, T>...>, "Variant does not contain T");
    const T* result = nullptr;
    matchWithSelf(variant, [&result, getNextFunc = std::forward<F>(getNextFunc)](auto&& self, auto&& value) -> void {
        using ValueType = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<ValueType, T>)
        {
            result = &value;
        }
        auto* next = getNextFunc(value);
        if (next)
        {
            std::visit([&self](auto&& value) { return self(value); }, *next);
        }
    });
    return result;
}

template <class T, class F, template <class...> class Variant, class... Args>
std::pair<const T*, std::uint64_t> findRecursivelyWithDepth(const Variant<Args...>& variant, F&& getNextFunc)
{
    static_assert(std::disjunction_v<std::is_same<Args, T>...>, "Variant does not contain T");
    const T* result = nullptr;
    std::uint64_t resultDepth = 0;
    std::uint64_t currentDepth = 0;
    matchWithSelf(variant,
                  [&result, &currentDepth, &resultDepth,
                   getNextFunc = std::forward<F>(getNextFunc)](auto&& self, auto&& value) -> void {
                      using ValueType = std::decay_t<decltype(value)>;
                      currentDepth++;
                      if constexpr (std::is_same_v<ValueType, T>)
                      {
                          resultDepth = currentDepth;
                          result = &value;
                      }
                      auto* next = getNextFunc(value);
                      if (next)
                      {
                          std::visit([&self](auto&& value) { return self(value); }, *next);
                      }
                  });
    return {result, resultDepth};
}
} // namespace cld::Semantics

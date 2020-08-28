
#pragma once

#include <type_traits>

#include "Util.hpp"

namespace cld
{
template <class Fn>
class function_ref;

template <class Ret, class... Params>
class function_ref<Ret(Params...)>
{
    Ret (*m_callback)(void*, Params... params){};
    void* m_callable{};

public:
    function_ref() = default;

    template <class Callable, std::enable_if_t<!std::is_same_v<std::decay_t<Callable>, function_ref>>* = nullptr>
    function_ref(Callable&& callable)
        : m_callback(+[](void* callable, Params... params) -> Ret {
              return (*reinterpret_cast<std::remove_reference_t<Callable>*>(callable))(std::forward<Params>(params)...);
          }),
          m_callable(&callable)
    {
    }

    template <class... Args>
    Ret operator()(Args&&... args) const
    {
        CLD_ASSERT(m_callable);
        return m_callback(m_callable, std::forward<Args>(args)...);
    }

    explicit operator bool() const
    {
        return m_callable;
    }
};
} // namespace cld

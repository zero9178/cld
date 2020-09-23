#pragma once

#include <optional>
#include <tuple>
#include <variant>

namespace cld
{
template <std::size_t... ints>
constexpr static auto integerSequenceToTuple(std::index_sequence<ints...>)
{
    return std::make_tuple(std::integral_constant<std::size_t, ints>{}...);
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
struct IsTuple : std::false_type
{
};

template <class... T>
struct IsTuple<std::tuple<T...>> : std::true_type
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

template <class T>
struct IsUniquePtr : std::false_type
{
};

template <class T>
struct IsUniquePtr<std::unique_ptr<T>> : std::true_type
{
};

template <class T>
struct IsOptional : std::false_type
{
};

template <class T>
struct IsOptional<std::optional<T>> : std::true_type
{
};

template <class T>
struct IsSharedPtr : std::false_type
{
};

template <class T>
struct IsSharedPtr<std::shared_ptr<T>> : std::true_type
{
};

template <class T>
struct IsSmartPtr : std::disjunction<IsUniquePtr<T>, IsSharedPtr<T>>
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

// See https://gitlab.inria.fr/gustedt/p99/-/tree/master. Code was copy pasted out of multiple header files without
// modifications to any macros that start with the P99_ or P00_ prefix. Copyright notice follows:

/* This may look like nonsense, but it really is -*- mode: C; coding: utf-8 -*- */
/*                                                                              */
/* Except for parts copied from previous work and as explicitly stated below,   */
/* the authors and copyright holders for this work are as follows:              */
/* (C) copyright  2010-2013 Jens Gustedt, INRIA, France                         */
/* (C) copyright  2013 Pierre-Nicolas Clauss                                    */
/* (C) copyright  2012 William Morris                                           */
/*                                                                              */
/* This file is free software; it is part of the P99 project.                   */
/*                                                                              */
/* Licensed under the Apache License, Version 2.0 (the "License");              */
/* you may not use this file except in compliance with the License.             */
/* You may obtain a copy of the License at                                      */
/*                                                                              */
/*     http://www.apache.org/licenses/LICENSE-2.0                               */
/*                                                                              */
/* Unless required by applicable law or agreed to in writing, software          */
/* distributed under the License is distributed on an "AS IS" BASIS,            */
/* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.     */
/* See the License for the specific language governing permissions and          */
/* limitations under the License.                                               */
/*                                                                              */

#define P99_PRED(N) P00_PRED(N)
#define P00_PRED(N) P00__PRED(P00_PRED_, N)
#define P00__PRED(P, N) P##N

#define P00_PRED_1 0
#define P00_PRED_2 1
#define P00_PRED_3 2
#define P00_PRED_4 3
#define P00_PRED_5 4
#define P00_PRED_6 5

#define P99_LAST(...) P99_CHS(P99_PRED(P00_NARG(__VA_ARGS__)), __VA_ARGS__, )
#define P99_ALLBUTLAST(...) P99_PASTE2(P00_PRE, P99_PRED(P00_NARG(__VA_ARGS__)))(__VA_ARGS__, )

#define P99_CHS(N, ...) P00_CHS(P99_SKP(N, __VA_ARGS__))
#define P00_CHS(...) P00_CHS_(__VA_ARGS__, )
#define P00_CHS_(X, ...) X

#define P99_SKP(N, ...) P99_PASTE2(P00_SKP, N)(__VA_ARGS__)
#define P00_SKP0(...) __VA_ARGS__
#define P00_SKP1(_0, ...) __VA_ARGS__
#define P00_SKP2(_0, ...) P00_SKP1(__VA_ARGS__)
#define P00_SKP3(_0, ...) P00_SKP2(__VA_ARGS__)
#define P00_SKP4(_0, ...) P00_SKP3(__VA_ARGS__)
#define P00_SKP5(_0, ...) P00_SKP4(__VA_ARGS__)

#define P00_NARG(...) P00_NARG_1(__VA_ARGS__)
#define P00_ARG(_1, _2, _3, _4, _5, _6, ...) _6
#define P00_NARG_1(...) P00_ARG(__VA_ARGS__, 5, 4, 3, 2, 1, 0, )
#define P00_NARG_2(...) P00_ARG(__VA_ARGS__, P00_INV(2), 2, P00_INV(2), 1, P00_INV(2), 0, )
#define P00_NARG_3(...) P00_ARG(__VA_ARGS__, P00_INV(3), P00_INV(3), 1, P00_INV(3), P00_INV(3), 0, )
#define P00_NARG_4(...) P00_ARG(__VA_ARGS__, P00_INV(4), 1, P00_INV(4), P00_INV(4), P00_INV(4), 0, )

#define P00_PRE0(...)
#define P00_PRE1(_0, ...) _0
#define P00_PRE2(_0, ...) _0, P00_PRE1(__VA_ARGS__)
#define P00_PRE3(_0, ...) _0, P00_PRE2(__VA_ARGS__)
#define P00_PRE4(_0, ...) _0, P00_PRE3(__VA_ARGS__)
#define P00_PRE5(_0, ...) _0, P00_PRE4(__VA_ARGS__)

#define P00_FOR0(NAME, OP, FUNC, ...)
#define P00_FOR1(NAME, OP, FUNC, ...) FUNC(NAME, P00_PRE1(__VA_ARGS__, ), 0)
#define P00_FOR2(NAME, OP, FUNC, ...) \
    OP(NAME, 1, P00_FOR1(NAME, OP, FUNC, P99_ALLBUTLAST(__VA_ARGS__)), FUNC(NAME, P99_LAST(__VA_ARGS__), 1))
#define P00_FOR3(NAME, OP, FUNC, ...) \
    OP(NAME, 2, P00_FOR2(NAME, OP, FUNC, P99_ALLBUTLAST(__VA_ARGS__)), FUNC(NAME, P99_LAST(__VA_ARGS__), 2))
#define P00_FOR4(NAME, OP, FUNC, ...) \
    OP(NAME, 3, P00_FOR3(NAME, OP, FUNC, P99_ALLBUTLAST(__VA_ARGS__)), FUNC(NAME, P99_LAST(__VA_ARGS__), 3))
#define P00_FOR5(NAME, OP, FUNC, ...) \
    OP(NAME, 4, P00_FOR4(NAME, OP, FUNC, P99_ALLBUTLAST(__VA_ARGS__)), FUNC(NAME, P99_LAST(__VA_ARGS__), 4))
#define P00_FOR6(NAME, OP, FUNC, ...) \
    OP(NAME, 5, P00_FOR5(NAME, OP, FUNC, P99_ALLBUTLAST(__VA_ARGS__)), FUNC(NAME, P99_LAST(__VA_ARGS__), 5))
#define P99_CAT2(_1, _2) _1##_2
#define P99_PASTE2(_1, _2) P99_CAT2(_1, _2)
#define P99_FOR(NAME, N, OP, FUNC, ...) P99_PASTE2(P00_FOR, N)(NAME, OP, FUNC, __VA_ARGS__)

// End of code copied from P99
// Code starting here is subject to the license as seen in the LICENSE file of the root directory of this project

#define CLD_MACRO_COUNT_ARGUMENTS(...) CLD_MACRO_COUNT_ARGUMENTS_(__VA_ARGS__, CLD_MACRO_RSEQ_N())
#define CLD_MACRO_COUNT_ARGUMENTS_(...) CLD_MACRO_ARGUMENTS_N(__VA_ARGS__)
#define CLD_MACRO_ARGUMENTS_N(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, \
                              _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36,  \
                              _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53,  \
                              _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, N, ...)                             \
    N
#define CLD_MACRO_RSEQ_N()                                                                                            \
    63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36,   \
        35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, \
        7, 6, 5, 4, 3, 2, 1, 0

} // namespace cld

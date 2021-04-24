#pragma once

#include <cld/Frontend/Compiler/Semantics.hpp>

#include <algorithm>
#include <numeric>

namespace cld::CGLLVM
{
// Default std::hash and operator== of Semantics::Type and subclasses make use of equality the way C defines it. This
// does not equal the type system a backend may care about however. The LLVM backend eg, does not care about any
// qualifiers, making non qualified, const qualified or volatile qualified types equal.

template <class T>
struct TypeHasher
{
    std::size_t operator()(const T& arg) const noexcept
    {
        return std::hash<T>{}(arg);
    }
};

template <>
struct TypeHasher<Semantics::Type>
{
    std::size_t operator()(const Semantics::Type& arg) const noexcept;
};

template <>
struct TypeHasher<Semantics::PrimitiveType>
{
    std::size_t operator()(const Semantics::PrimitiveType& arg) const noexcept
    {
        return cld::hashCombine(arg.getBitCount(), arg.isFloatingPoint());
    }
};

template <>
struct TypeHasher<Semantics::ArrayType>
{
    std::size_t operator()(const Semantics::ArrayType& arg) const noexcept
    {
        return cld::hashCombine<TypeHasher>(arg.getSize(), arg.getType());
    }
};

template <>
struct TypeHasher<Semantics::AbstractArrayType>
{
    std::size_t operator()(const Semantics::AbstractArrayType& arg) const noexcept
    {
        return TypeHasher<Semantics::Type>{}(arg.getType());
    }
};

template <>
struct TypeHasher<Semantics::ValArrayType>
{
    std::size_t operator()(const Semantics::ValArrayType& arg) const noexcept
    {
        return TypeHasher<Semantics::Type>{}(arg.getType());
    }
};

template <>
struct TypeHasher<Semantics::FunctionType>
{
    std::size_t operator()(const Semantics::FunctionType& arg) const noexcept
    {
        std::size_t hash =
            std::accumulate(arg.getParameters().begin(), arg.getParameters().end(), 0,
                            [](std::size_t hash, const Semantics::FunctionType::Parameter& param)
                            { return cld::rawHashCombine(hash, TypeHasher<Semantics::Type>{}(*param.type)); });
        return cld::rawHashCombine(hash, cld::hashCombine<TypeHasher>(arg.isLastVararg(), arg.getReturnType()));
    }
};

template <>
struct TypeHasher<Semantics::PointerType>
{
    std::size_t operator()(const Semantics::PointerType& arg) const noexcept
    {
        return TypeHasher<Semantics::Type>{}(arg.getElementType());
    }
};

template <>
struct TypeHasher<Semantics::VectorType>
{
    std::size_t operator()(const Semantics::VectorType& arg) const noexcept
    {
        return cld::hashCombine<TypeHasher>(arg.getSize(), arg.getType());
    }
};

inline std::size_t TypeHasher<Semantics::Type>::operator()(const Semantics::Type& arg) const noexcept
{
    return cld::rawHashCombine(std::hash<std::size_t>{}(arg.index()), arg.match(
                                                                          [](const auto& arg) -> std::size_t
                                                                          {
                                                                              using T = std::decay_t<decltype(arg)>;
                                                                              return TypeHasher<T>{}(arg);
                                                                          }));
}

template <class T>
struct TypeEqual;

template <class... Args>
struct TypeEqual<std::variant<Args...>>
{
    bool operator()(const std::variant<Args...>& lhs, const std::variant<Args...>& rhs) const noexcept
    {
        if (lhs.index() != rhs.index())
        {
            return false;
        }
        return cld::match(lhs,
                          [&](auto&& value)
                          {
                              using T = std::decay_t<decltype(value)>;
                              return TypeEqual<T>{}(value, cld::get<T>(rhs));
                          });
    }
};

template <>
struct TypeEqual<Semantics::Type>
{
    bool operator()(const Semantics::Type& lhs, const Semantics::Type& rhs) const noexcept;
};

template <>
struct TypeEqual<Semantics::PrimitiveType>
{
    bool operator()(const Semantics::PrimitiveType& lhs, const Semantics::PrimitiveType& rhs) const noexcept
    {
        return std::tuple(lhs.getBitCount(), lhs.isFloatingPoint())
               == std::tuple(rhs.getBitCount(), rhs.isFloatingPoint());
    }
};

template <>
struct TypeEqual<Semantics::ArrayType>
{
    bool operator()(const Semantics::ArrayType& lhs, const Semantics::ArrayType& rhs) const noexcept
    {
        if (lhs.getSize() != rhs.getSize())
        {
            return false;
        }
        return TypeEqual<Semantics::Type>{}(lhs.getType(), rhs.getType());
    }
};

template <>
struct TypeEqual<Semantics::AbstractArrayType>
{
    bool operator()(const Semantics::AbstractArrayType& lhs, const Semantics::AbstractArrayType& rhs) const noexcept
    {
        return TypeEqual<Semantics::Type>{}(lhs.getType(), rhs.getType());
    }
};

template <>
struct TypeEqual<Semantics::ValArrayType>
{
    bool operator()(const Semantics::ValArrayType& lhs, const Semantics::ValArrayType& rhs) const noexcept
    {
        return TypeEqual<Semantics::Type>{}(lhs.getType(), rhs.getType());
    }
};

template <>
struct TypeEqual<Semantics::PointerType>
{
    bool operator()(const Semantics::PointerType& lhs, const Semantics::PointerType& rhs) const noexcept
    {
        return TypeEqual<Semantics::Type>{}(lhs.getElementType(), rhs.getElementType());
    }
};

template <>
struct TypeEqual<Semantics::FunctionType>
{
    bool operator()(const Semantics::FunctionType& lhs, const Semantics::FunctionType& rhs) const noexcept
    {
        if (lhs.isLastVararg() != rhs.isLastVararg())
        {
            return false;
        }
        if (!TypeEqual<Semantics::Type>{}(lhs.getReturnType(), rhs.getReturnType()))
        {
            return false;
        }
        return std::equal(
            lhs.getParameters().begin(), lhs.getParameters().end(), rhs.getParameters().begin(),
            rhs.getParameters().end(),
            [](const Semantics::FunctionType::Parameter& lhs, const Semantics::FunctionType::Parameter& rhs)
            { return TypeEqual<Semantics::Type>{}(*lhs.type, *rhs.type); });
    }
};

template <>
struct TypeEqual<Semantics::StructType>
{
    bool operator()(const Semantics::StructType& lhs, const Semantics::StructType& rhs) const noexcept
    {
        return &lhs.getInfo() == &rhs.getInfo();
    }
};

template <>
struct TypeEqual<Semantics::UnionType>
{
    bool operator()(const Semantics::UnionType& lhs, const Semantics::UnionType& rhs) const noexcept
    {
        return &lhs.getInfo() == &rhs.getInfo();
    }
};

template <>
struct TypeEqual<Semantics::EnumType>
{
    bool operator()(const Semantics::EnumType& lhs, const Semantics::EnumType& rhs) const noexcept
    {
        return &lhs.getInfo() == &rhs.getInfo();
    }
};

template <>
struct TypeEqual<Semantics::VectorType>
{
    bool operator()(const Semantics::VectorType& lhs, const Semantics::VectorType& rhs) const noexcept
    {
        if (lhs.getSize() != rhs.getSize())
        {
            return false;
        }
        return TypeEqual<Semantics::Type>{}(lhs.getType(), rhs.getType());
    }
};

template <>
struct TypeEqual<Semantics::ErrorType>
{
    bool operator()(const Semantics::ErrorType&, const Semantics::ErrorType&) const noexcept
    {
        return true;
    }
};

inline bool TypeEqual<Semantics::Type>::operator()(const Semantics::Type& lhs,
                                                   const Semantics::Type& rhs) const noexcept
{
    if (lhs.index() != rhs.index())
    {
        return false;
    }
    return lhs.match(
        [&](auto&& value) -> bool
        {
            using T = std::decay_t<decltype(value)>;
            return TypeEqual<T>{}(value, rhs.template as<T>());
        });
}

} // namespace cld::CGLLVM

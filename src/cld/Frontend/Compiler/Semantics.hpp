
#pragma once

#include <cld/Support/AbstractIntrusiveVariant.hpp>
#include <cld/Support/IntrVarValue.hpp>

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <tsl/ordered_map.h>

#include "Attributes.hpp"
#include "DiagnosticUtil.hpp"
#include "LanguageOptions.hpp"
#include "Lexer.hpp"

namespace cld
{
namespace Syntax
{
class Declarator;
class Node;
class TranslationUnit;
} // namespace Syntax

} // namespace cld

namespace cld::Semantics
{
struct StructInfo;
struct UnionInfo;
struct EnumInfo;
struct TypedefInfo;

class StructDefinition;

class UnionDefinition;

class EnumDefinition;

class ProgramInterface;

class Type : public AbstractIntrusiveVariant<Type, class PrimitiveType, class ArrayType, class AbstractArrayType,
                                             class ValArrayType, class FunctionType, class StructType, class UnionType,
                                             class EnumType, class PointerType, class VectorType, class ErrorType>
{
public:
    enum TypeFlags : std::uint8_t
    {
        Nothing = 0,
        Const = 0b1,
        Volatile = 0b10,
        Restricted = 0b100,
        Static = 0b1000,
        VARArg = 0b10000,
        KAndR = 0b100000,
    };

private:
    const TypedefInfo* m_info = nullptr;

protected:
    TypeFlags m_flags;

    template <class T>
    Type(std::in_place_type_t<T>, TypeFlags flags)
        : AbstractIntrusiveVariant(std::in_place_type<T>), m_flags(static_cast<TypeFlags>(flags))
    {
    }

    Type(const Type&) = default;
    Type& operator=(const Type&) = default;
    Type(Type&&) noexcept = default;
    Type& operator=(Type&&) noexcept = default;

public:
    [[nodiscard]] bool isConst() const
    {
        return m_flags & Const;
    }

    [[nodiscard]] bool isVolatile() const
    {
        return m_flags & Volatile;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_flags & Restricted;
    }

    [[nodiscard]] TypeFlags getFlags() const
    {
        return m_flags;
    }

    void setConst(bool isConst);

    void setVolatile(bool isVolatile);

    void setRestricted(bool isRestricted);

    [[nodiscard]] std::string_view getTypedefName() const;

    [[nodiscard]] const TypedefInfo* getTypedefInfo() const
    {
        return m_info;
    }

    void setTypedef(const TypedefInfo* info)
    {
        m_info = info;
    }

    [[nodiscard]] bool isTypedef() const
    {
        return m_info;
    }

    [[nodiscard]] bool isUndefined() const
    {
        return is<class ErrorType>();
    }

    [[nodiscard]] bool operator==(const Type& rhs) const;

    [[nodiscard]] bool operator!=(const Type& rhs) const;

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;
};

inline Type::TypeFlags operator|(Type::TypeFlags lhs, Type::TypeFlags rhs)
{
    using UT = std::underlying_type_t<Type::TypeFlags>;
    return static_cast<Type::TypeFlags>(static_cast<UT>(lhs) | static_cast<UT>(rhs));
}

inline Type::TypeFlags operator&(Type::TypeFlags lhs, Type::TypeFlags rhs)
{
    using UT = std::underlying_type_t<Type::TypeFlags>;
    return static_cast<Type::TypeFlags>(static_cast<UT>(lhs) & static_cast<UT>(rhs));
}

inline Type::TypeFlags operator~(Type::TypeFlags lhs)
{
    using UT = std::underlying_type_t<Type::TypeFlags>;
    return static_cast<Type::TypeFlags>(~static_cast<UT>(lhs));
}

namespace detail
{
template <Type::TypeFlags flag>
struct FlagParameter
{
    bool value;

    Type::TypeFlags result() const
    {
        return value ? flag : Type::Nothing;
    }

    constexpr inline static auto kind = flag;
};

template <>
struct FlagParameter<Type::Nothing>
{
    Type::TypeFlags value;

    Type::TypeFlags result() const
    {
        return value;
    }

    constexpr inline static auto kind = Type::Nothing;
};

template <Type::TypeFlags flag>
auto isFlagParameter(const FlagParameter<flag>&) -> std::true_type;

template <class T>
auto isFlagParameter(const T&) -> std::false_type;

template <Type::TypeFlags flag>
struct FlagFactory
{
    FlagParameter<flag> operator=(bool value) const
    {
        return FlagParameter<flag>{value};
    }
};

template <>
struct FlagFactory<Type::Nothing>
{
    FlagParameter<Type::Nothing> operator=(Type::TypeFlags value) const
    {
        return FlagParameter<Type::Nothing>{value};
    }
};

template <class... Args>
constexpr bool areFlags()
{
    return (decltype(isFlagParameter(std::declval<Args>())){} && ...);
}

template <Type::TypeFlags flag, class... Args>
constexpr bool contains()
{
    return (false || ... || (Args::kind == flag));
}

} // namespace detail

namespace flag
{
constexpr static detail::FlagFactory<Type::Const> isConst;
constexpr static detail::FlagFactory<Type::Volatile> isVolatile;
constexpr static detail::FlagFactory<Type::Restricted> isRestricted;
constexpr static detail::FlagFactory<Type::Static> isStatic;
constexpr static detail::FlagFactory<Type::VARArg> isVARArg;
constexpr static detail::FlagFactory<Type::KAndR> isKAndR;
constexpr static detail::FlagFactory<Type::Nothing> useFlags;
} // namespace flag

class PrimitiveType final : public Type
{
    std::uint8_t m_bitCount;
    std::uint8_t m_alignOf : 5;
    bool m_isFloatingPoint : 1;
    bool m_isSigned : 1;

public:
    enum Kind : std::uint8_t
    {
        Char,
        SignedChar,
        UnsignedChar,
        Bool,
        Short,
        UnsignedShort,
        Int,
        UnsignedInt,
        Long,
        UnsignedLong,
        LongLong,
        UnsignedLongLong,
        Float,
        Double,
        LongDouble,
        Void,
        Int128,
        UnsignedInt128
    };

private:
    Kind m_kind;

    PrimitiveType(Kind kind, const LanguageOptions& options, TypeFlags flags);

    PrimitiveType(LanguageOptions::UnderlyingType underlyingType, const LanguageOptions& options, TypeFlags flags);

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface&) const
    {
        return getByteCount();
    }

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface&) const
    {
        return m_alignOf;
    }

    [[nodiscard]] bool equalsImpl(const PrimitiveType& rhs) const
    {
        return m_kind == rhs.m_kind;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    PrimitiveType(Kind kind, const LanguageOptions& options, Flags&&... flags)
        : PrimitiveType(kind, options, (Nothing | ... | flags.result()))
    {
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "PrimitiveType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "PrimitiveType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "PrimitiveType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "PrimitiveType can't be k&r");
    }

    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    PrimitiveType(LanguageOptions::UnderlyingType underlyingType, const LanguageOptions& options, Flags&&... flags)
        : PrimitiveType(underlyingType, options, (Nothing | ... | flags.result()))
    {
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "PrimitiveType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "PrimitiveType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "PrimitiveType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "PrimitiveType can't be k&r");
    }

    [[nodiscard]] bool isFloatingPoint() const
    {
        return m_isFloatingPoint;
    }

    [[nodiscard]] bool isSigned() const
    {
        return m_isSigned;
    }

    [[nodiscard]] std::uint8_t getByteCount() const
    {
        return cld::roundUpTo(m_bitCount, m_alignOf * 8) / 8;
    }

    [[nodiscard]] std::uint8_t getBitCount() const
    {
        return m_bitCount;
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }
};

class ArrayType final : public Type
{
    const Type* m_type;
    std::uint64_t m_size;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface& program) const
    {
        return m_type->getSizeOf(program) * m_size;
    }

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const
    {
        return m_type->getAlignOf(program);
    }

    [[nodiscard]] bool equalsImpl(const ArrayType& rhs) const
    {
        return std::tie(*m_type, m_size) == std::tie(*rhs.m_type, rhs.m_size);
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    ArrayType(cld::not_null<const Type> type, std::uint64_t size, Flags&&... flags)
        : Type(std::in_place_type<ArrayType>, (Nothing | ... | flags.result()) & ~(VARArg | KAndR)),
          m_type(type),
          m_size(size)
    {
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "ArrayType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "ArrayType can't be k&r");
    }

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] std::uint64_t getSize() const
    {
        return m_size;
    }

    [[nodiscard]] bool isStatic() const
    {
        return m_flags & Static;
    }
};

class AbstractArrayType final : public Type
{
    const Type* m_type;

    friend class Type;

    std::uint64_t getSizeOfImpl(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const
    {
        return m_type->getAlignOf(program);
    }

    [[nodiscard]] bool equalsImpl(const AbstractArrayType& rhs) const
    {
        return *m_type == *rhs.m_type;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    AbstractArrayType(cld::not_null<const Type> type, Flags&&... flags)
        : Type(std::in_place_type<AbstractArrayType>, (Nothing | ... | flags.result()) & ~(Static | VARArg | KAndR)),
          m_type(type)
    {
        static_assert(!detail::contains<Type::Static, Flags...>(), "ArrayType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "ArrayType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "ArrayType can't be k&r");
    }

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }
};

class ExpressionBase;

class ValArrayType final : public Type
{
    const Type* m_type;
    std::shared_ptr<const ExpressionBase> m_expression;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const
    {
        return m_type->getAlignOf(program);
    }

    [[nodiscard]] bool equalsImpl(const ValArrayType& rhs) const
    {
        return std::tie(*m_type, m_expression) == std::tie(*rhs.m_type, rhs.m_expression);
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    ValArrayType(cld::not_null<const Type> type, std::shared_ptr<const ExpressionBase> expression, Flags&&... flags)
        : Type(std::in_place_type<ValArrayType>, (Nothing | ... | flags.result()) & ~(VARArg | KAndR)),
          m_type(type),
          m_expression(std::move(expression))
    {
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "ValArrayType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "ValArrayType can't be k&r");
    }

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] bool isStatic() const
    {
        return m_flags & Static;
    }

    // NULLABLE
    [[nodiscard]] const std::shared_ptr<const ExpressionBase>& getExpression() const
    {
        return m_expression;
    }
};

class FunctionType final : public Type
{
public:
    struct Parameter
    {
        const Type* CLD_NON_NULL type;
        std::string_view name;
    };

private:
    const Type* m_returnType;
    std::vector<Parameter> m_parameters;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] bool equalsImpl(const FunctionType& rhs) const
    {
        if (*m_returnType != *rhs.m_returnType)
        {
            return false;
        }
        return std::equal(m_parameters.begin(), m_parameters.end(), rhs.m_parameters.begin(), rhs.m_parameters.end(),
                          [](const auto& lhs, const auto& rhs) { return *lhs.type == *rhs.type; });
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    explicit FunctionType(cld::not_null<const Type> returnType, std::vector<Parameter>&& parameters, Flags&&... flags)
        : Type(std::in_place_type<FunctionType>,
               (Nothing | ... | flags.result()) & ~(Const | Volatile | Restricted | Static)),
          m_returnType(returnType),
          m_parameters(std::move(parameters))
    {
        static_assert(!detail::contains<Type::Const, Flags...>(), "FunctionType can't be const");
        static_assert(!detail::contains<Type::Volatile, Flags...>(), "FunctionType can't be volatile");
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "FunctionType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "PrimitiveType can't be static");
    }

    [[nodiscard]] const Type& getReturnType() const
    {
        return *m_returnType;
    }

    [[nodiscard]] const std::vector<Parameter>& getParameters() const
    {
        return m_parameters;
    }

    [[nodiscard]] bool isLastVararg() const
    {
        return m_flags & VARArg;
    }

    [[nodiscard]] bool isKandR() const
    {
        return m_flags & KAndR;
    }
};

class StructType final : public Type
{
    const StructInfo* m_info;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] bool equalsImpl(const StructType& rhs) const
    {
        return m_info == rhs.m_info;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    explicit StructType(const StructInfo& info, Flags&&... flags)
        : Type(std::in_place_type<StructType>,
               (Nothing | ... | flags.result()) & ~(Restricted | Static | VARArg | KAndR)),
          m_info(&info)
    {
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "StructType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "StructType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "StructType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "StructType can't be k&r");
    }

    [[nodiscard]] std::string_view getStructName() const;

    [[nodiscard]] bool isAnonymous() const;

    [[nodiscard]] const StructInfo& getInfo() const
    {
        return *m_info;
    }
};

class UnionType final : public Type
{
    const UnionInfo* m_info;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] bool equalsImpl(const UnionType& rhs) const
    {
        return m_info == rhs.m_info;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    explicit UnionType(const UnionInfo& info, Flags&&... flags)
        : Type(std::in_place_type<UnionType>,
               (Nothing | ... | flags.result()) & ~(Restricted | Static | VARArg | KAndR)),
          m_info(&info)
    {
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "UnionType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "UnionType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "UnionType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "UnionType can't be k&r");
    }

    [[nodiscard]] std::string_view getUnionName() const;

    [[nodiscard]] bool isAnonymous() const;

    [[nodiscard]] const UnionInfo& getInfo() const
    {
        return *m_info;
    }
};

class EnumType final : public Type
{
    const EnumInfo* m_info;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] bool equalsImpl(const EnumType& rhs) const
    {
        return m_info == rhs.m_info;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    explicit EnumType(const EnumInfo& info, Flags&&... flags)
        : Type(std::in_place_type<EnumType>,
               (Nothing | ... | flags.result()) & ~(Restricted | Static | VARArg | KAndR)),
          m_info(&info)
    {
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "EnumType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "EnumType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "EnumType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "EnumType can't be k&r");
    }

    [[nodiscard]] std::string_view getEnumName() const;

    [[nodiscard]] bool isAnonymous() const;

    [[nodiscard]] const EnumInfo& getInfo() const
    {
        return *m_info;
    }
};

class PointerType final : public Type
{
    const Type* m_elementType;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const;

    [[nodiscard]] bool equalsImpl(const PointerType& rhs) const
    {
        return *m_elementType == *rhs.m_elementType;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    explicit PointerType(cld::not_null<const Type> elementType, Flags&&... flags)
        : Type(std::in_place_type<PointerType>, (Nothing | ... | flags.result()) & ~(Static | VARArg | KAndR)),
          m_elementType(elementType)
    {
        static_assert(!detail::contains<Type::Static, Flags...>(), "PointerType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "PointerType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "PointerType can't be k&r");
    }

    [[nodiscard]] const Type& getElementType() const
    {
        return *m_elementType;
    }
};

class VectorType final : public Type
{
    const Type* m_elementType;
    std::uint64_t m_size;

    friend class Type;

    [[nodiscard]] std::uint64_t getSizeOfImpl(const ProgramInterface& program) const
    {
        return m_elementType->getSizeOf(program) * m_size;
    }

    [[nodiscard]] std::uint64_t getAlignOfImpl(const ProgramInterface& program) const
    {
        return getSizeOf(program);
    }

    [[nodiscard]] bool equalsImpl(const VectorType& rhs) const
    {
        return std::tie(*m_elementType, m_size) == std::tie(*rhs.m_elementType, rhs.m_size);
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    VectorType(cld::not_null<const Type> elementType, std::uint64_t size, Flags&&... flags)
        : Type(std::in_place_type<VectorType>,
               (Nothing | ... | flags.result()) & ~(Restricted | Static | VARArg | KAndR)),
          m_elementType(elementType),
          m_size(size)
    {
        static_assert(!detail::contains<Type::Restricted, Flags...>(), "VectorType can't be restricted");
        static_assert(!detail::contains<Type::Static, Flags...>(), "VectorType can't be static");
        static_assert(!detail::contains<Type::VARArg, Flags...>(), "VectorType can't be vararg");
        static_assert(!detail::contains<Type::KAndR, Flags...>(), "VectorType can't be k&r");
    }

    [[nodiscard]] const Type& getType() const
    {
        return *m_elementType;
    }

    [[nodiscard]] std::uint64_t getSize() const
    {
        return m_size;
    }
};

class ErrorType : public Type
{
    friend class Type;

    std::size_t getSizeOfImpl(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    std::size_t getAlignOfImpl(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    bool equalsImpl(const ErrorType&) const
    {
        return true;
    }

public:
    template <class... Flags, std::enable_if_t<detail::areFlags<Flags...>()>* = nullptr>
    ErrorType(Flags&&... flags) : Type(std::in_place_type<ErrorType>, (Nothing | ... | flags.result()))
    {
    }
};

enum class ValueCategory : std::uint8_t
{
    Lvalue,
    Rvalue
};

class ExpressionBase
    : public AbstractIntrusiveVariant<
          ExpressionBase, class Constant, class DeclarationRead, class Conversion, class MemberAccess,
          class BinaryOperator, class Cast, class UnaryOperator, class SizeofOperator, class SubscriptOperator,
          class Conditional, class Assignment, class CommaExpression, class CallExpression, class CompoundLiteral,
          class BuiltinVAArg, class BuiltinOffsetOf, class ErrorExpression>
{
    IntrVarValue<Type> m_type;
    ValueCategory m_valueCategory;

protected:
    template <class T>
    ExpressionBase(std::in_place_type_t<T>, IntrVarValue<Type> type, ValueCategory valueCategory)
        : AbstractIntrusiveVariant(std::in_place_type<T>), m_type(std::move(type)), m_valueCategory(valueCategory)
    {
    }

public:
    ExpressionBase(const ExpressionBase&) = delete;
    ExpressionBase& operator=(const ExpressionBase&) = delete;

    // Currently MSVC deletes the move constructor and assign operator if I mark them noexcept
    // This is due to llvm::APSInt not being noexcept. Other compilers will believe me that Expression is noexcept
    // if I mark it as such but Microsoft instead punishes me by deleting it.

    ExpressionBase(ExpressionBase&&)
#if !defined(_MSC_VER) || defined(__clang__) || _MSC_VER >= 1928
        noexcept
#endif
        = default;
    ExpressionBase& operator=(ExpressionBase&&)
#if !defined(_MSC_VER) || defined(__clang__) || _MSC_VER >= 1928
        noexcept
#endif
        = default;

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] ValueCategory getValueCategory() const
    {
        return m_valueCategory;
    }

    [[nodiscard]] bool isUndefined() const
    {
        return is<ErrorExpression>();
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

bool isStringLiteralExpr(const ExpressionBase& expression);

bool isBitfieldAccess(const ExpressionBase& expression);

class Constant final : public ExpressionBase
{
public:
    using Variant = std::variant<llvm::APSInt, llvm::APFloat, std::string, Lexer::NonCharString>;

private:
    Variant m_value;
    Lexer::CTokenIterator m_valueBegin;
    Lexer::CTokenIterator m_valueEnd;

public:
    Constant(IntrVarValue<Type> type, Variant value, Lexer::CTokenIterator valueBegin, Lexer::CTokenIterator valueEnd)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_value(std::move(value)),
          m_valueBegin(valueBegin),
          m_valueEnd(valueEnd)
    {
    }

    [[nodiscard]] const Variant& getValue() const
    {
        return m_value;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_valueBegin;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_valueEnd;
    }
};

class FunctionDefinition;

class Declaration;

class FunctionDeclaration;

class VariableDeclaration;

class BuiltinFunction;

class Useable;

class DeclarationRead final : public ExpressionBase
{
    const Useable* CLD_NON_NULL m_declRead;
    Lexer::CTokenIterator m_identifierToken;

public:
    explicit DeclarationRead(IntrVarValue<Type> type, const Useable& useable, Lexer::CTokenIterator identifierToken)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Lvalue),
          m_declRead(&useable),
          m_identifierToken(identifierToken)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getIdentifierToken() const
    {
        return m_identifierToken;
    }

    [[nodiscard]] const Useable& getDeclRead() const
    {
        return *m_declRead;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_identifierToken;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_identifierToken + 1;
    }
};

class Conversion final : public ExpressionBase
{
public:
    enum Kind
    {
        LValue,
        IntegerPromotion,
        ArithmeticConversion,
        DefaultArgumentPromotion,
        Implicit,
    };

private:
    Kind m_kind;
    IntrVarPtr<ExpressionBase> m_expression;

public:
    Conversion(IntrVarValue<Type> type, Kind kind, IntrVarPtr<ExpressionBase>&& expression)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_kind(kind),
          m_expression(std::move(expression))
    {
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] const ExpressionBase& getExpression() const
    {
        return *m_expression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class Cast final : public ExpressionBase
{
    Lexer::CTokenIterator m_openParentheses;
    Lexer::CTokenIterator m_closeParentheses;
    IntrVarPtr<ExpressionBase> m_expression;

public:
    Cast(IntrVarValue<Type> type, Lexer::CTokenIterator openParentheses, Lexer::CTokenIterator closeParentheses,
         IntrVarPtr<ExpressionBase>&& expression)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, type, ValueCategory::Rvalue),
          m_openParentheses(openParentheses),
          m_closeParentheses(closeParentheses),
          m_expression(std::move(expression))
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const
    {
        return m_closeParentheses;
    }

    [[nodiscard]] const ExpressionBase& getExpression() const
    {
        return *m_expression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

struct Field
{
    const Type* CLD_NON_NULL type; // NOT NULL
    std::string_view name;
    const Lexer::CToken* nameToken;
    std::vector<std::size_t> indices;
    std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
    std::vector<const Type*> parentTypes;
};

class MemberAccess final : public ExpressionBase
{
    IntrVarPtr<ExpressionBase> m_recordExpr;
    const Field* m_field;
    Lexer::CTokenIterator m_memberIdentifier;

public:
    MemberAccess(IntrVarValue<Type> type, ValueCategory valueCategory, IntrVarPtr<ExpressionBase>&& recordExpr,
                 const Field& field, Lexer::CTokenIterator memberIdentifier)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), valueCategory),
          m_recordExpr(std::move(recordExpr)),
          m_field(&field),
          m_memberIdentifier(memberIdentifier)
    {
    }

    [[nodiscard]] const ExpressionBase& getRecordExpression() const
    {
        return *m_recordExpr;
    }

    [[nodiscard]] const Field& getField() const
    {
        return *m_field;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_memberIdentifier + 1;
    }
};

class SubscriptOperator final : public ExpressionBase
{
    bool m_leftIsPointer;
    IntrVarPtr<ExpressionBase> m_leftExpr;
    Lexer::CTokenIterator m_openBracket;
    IntrVarPtr<ExpressionBase> m_rightExpr;
    Lexer::CTokenIterator m_closeBracket;

public:
    SubscriptOperator(IntrVarValue<Type> type, bool leftIsPointer, IntrVarPtr<ExpressionBase>&& leftExpr,
                      Lexer::CTokenIterator openBracket, IntrVarPtr<ExpressionBase>&& rightExpr,
                      Lexer::CTokenIterator closeBracket)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Lvalue),
          m_leftIsPointer(leftIsPointer),
          m_leftExpr(std::move(leftExpr)),
          m_openBracket(openBracket),
          m_rightExpr(std::move(rightExpr)),
          m_closeBracket(closeBracket)
    {
    }

    [[nodiscard]] const ExpressionBase& getLeftExpression() const
    {
        return *m_leftExpr;
    }

    [[nodiscard]] const ExpressionBase& getRightExpression() const
    {
        return *m_rightExpr;
    }

    [[nodiscard]] const ExpressionBase& getPointerExpression() const
    {
        return m_leftIsPointer ? *m_leftExpr : *m_rightExpr;
    }

    [[nodiscard]] const ExpressionBase& getIntegerExpression() const
    {
        return m_leftIsPointer ? *m_rightExpr : *m_leftExpr;
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenBracket() const
    {
        return m_openBracket;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseBracket() const
    {
        return m_closeBracket;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_closeBracket + 1;
    }
};

class BinaryOperator final : public ExpressionBase
{
    IntrVarPtr<ExpressionBase> m_leftOperand;

public:
    enum Kind
    {
        Addition,
        Subtraction,
        Multiply,
        Divide,
        Modulo,
        LeftShift,
        RightShift,
        LessThan,
        GreaterThan,
        LessOrEqual,
        GreaterOrEqual,
        Equal,
        NotEqual,
        BitOr,
        BitAnd,
        BitXor,
        LogicAnd,
        LogicOr
    };

private:
    Kind m_kind;
    Lexer::CTokenIterator m_operatorToken;
    IntrVarPtr<ExpressionBase> m_rightOperand;

public:
    BinaryOperator(IntrVarValue<Type> type, IntrVarPtr<ExpressionBase>&& leftOperand, Kind kind,
                   Lexer::CTokenIterator operatorToken, IntrVarPtr<ExpressionBase>&& rightOperand)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_leftOperand(std::move(leftOperand)),
          m_kind(kind),
          m_operatorToken(operatorToken),
          m_rightOperand(std::move(rightOperand))
    {
    }

    [[nodiscard]] const ExpressionBase& getLeftExpression() const
    {
        return *m_leftOperand;
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] Lexer::CTokenIterator getOperatorToken() const
    {
        return m_operatorToken;
    }

    [[nodiscard]] const ExpressionBase& getRightExpression() const
    {
        return *m_rightOperand;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class UnaryOperator final : public ExpressionBase
{
public:
    enum Kind
    {
        AddressOf,
        Dereference,
        PostIncrement,
        PostDecrement,
        PreIncrement,
        PreDecrement,
        Plus,
        Minus,
        BooleanNegate,
        BitwiseNegate,
    };

private:
    Kind m_kind;
    Lexer::CTokenIterator m_operatorToken;
    IntrVarPtr<ExpressionBase> m_operand;

public:
    UnaryOperator(IntrVarValue<Type> type, ValueCategory valueCategory, Kind kind, Lexer::CTokenIterator operatorToken,
                  IntrVarPtr<ExpressionBase>&& operand)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), valueCategory),
          m_kind(kind),
          m_operatorToken(operatorToken),
          m_operand(std::move(operand))
    {
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] Lexer::CTokenIterator getOperatorToken() const
    {
        return m_operatorToken;
    }

    [[nodiscard]] const ExpressionBase& getOperand() const
    {
        return *m_operand;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class SizeofOperator final : public ExpressionBase
{
public:
    struct TypeVariant
    {
        Lexer::CTokenIterator openParentheses;
        IntrVarValue<Type> type;
        Lexer::CTokenIterator closeParentheses;
    };

    using Variant = std::variant<IntrVarPtr<ExpressionBase>, TypeVariant>;

private:
    Lexer::CTokenIterator m_sizeOfToken;
    std::optional<std::uint64_t> m_size;
    Variant m_variant;

public:
    SizeofOperator(const LanguageOptions& options, Lexer::CTokenIterator sizeOfToken, std::optional<std::uint64_t> size,
                   Variant variant)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, PrimitiveType(options.sizeTType, options),
                         ValueCategory::Rvalue),
          m_sizeOfToken(sizeOfToken),
          m_size(size),
          m_variant(std::move(variant))
    {
    }

    [[nodiscard]] const std::optional<std::uint64_t>& getSize() const
    {
        return m_size;
    }

    [[nodiscard]] const Variant& getVariant() const
    {
        return m_variant;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_sizeOfToken;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class Conditional final : public ExpressionBase
{
    IntrVarPtr<ExpressionBase> m_boolExpression;
    Lexer::CTokenIterator m_questionMark;
    IntrVarPtr<ExpressionBase> m_trueExpression;
    Lexer::CTokenIterator m_colon;
    IntrVarPtr<ExpressionBase> m_falseExpression;

public:
    Conditional(IntrVarValue<Type> type, IntrVarPtr<ExpressionBase>&& boolExpression,
                Lexer::CTokenIterator questionMark, IntrVarPtr<ExpressionBase>&& trueExpression,
                Lexer::CTokenIterator colon, IntrVarPtr<ExpressionBase>&& falseExpression)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_boolExpression(std::move(boolExpression)),
          m_questionMark(questionMark),
          m_trueExpression(std::move(trueExpression)),
          m_colon(colon),
          m_falseExpression(std::move(falseExpression))
    {
    }

    [[nodiscard]] const ExpressionBase& getBoolExpression() const
    {
        return *m_boolExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator getQuestionMark() const
    {
        return m_questionMark;
    }

    [[nodiscard]] const ExpressionBase& getTrueExpression() const
    {
        return *m_trueExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator getColon() const
    {
        return m_colon;
    }

    [[nodiscard]] const ExpressionBase& getFalseExpression() const
    {
        return *m_falseExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class Assignment final : public ExpressionBase
{
    IntrVarPtr<ExpressionBase> m_leftOperand;
    IntrVarValue<Type> m_leftCalcType;

public:
    enum Kind
    {
        Simple,
        Plus,
        Minus,
        Divide,
        Multiply,
        Modulo,
        LeftShift,
        RightShift,
        BitAnd,
        BitOr,
        BitXor
    };

private:
    Kind m_kind;
    Lexer::CTokenIterator m_operatorToken;
    IntrVarPtr<ExpressionBase> m_rightOperand;

public:
    Assignment(IntrVarValue<Type> type, IntrVarPtr<ExpressionBase>&& leftOperand, IntrVarValue<Type> leftCalcType,
               Kind kind, Lexer::CTokenIterator operatorToken, IntrVarPtr<ExpressionBase>&& rightOperand)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_leftOperand(std::move(leftOperand)),
          m_leftCalcType(std::move(leftCalcType)),
          m_kind(kind),
          m_operatorToken(operatorToken),
          m_rightOperand(std::move(rightOperand))
    {
    }

    [[nodiscard]] const ExpressionBase& getLeftExpression() const
    {
        return *m_leftOperand;
    }

    [[nodiscard]] const Type& getLeftCalcType() const
    {
        return *m_leftCalcType;
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] Lexer::CTokenIterator getOperatorToken() const
    {
        return m_operatorToken;
    }

    [[nodiscard]] const ExpressionBase& getRightExpression() const
    {
        return *m_rightOperand;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class CommaExpression final : public ExpressionBase
{
    std::vector<std::pair<IntrVarPtr<ExpressionBase>, Lexer::CTokenIterator>> m_commaExpressions;
    IntrVarPtr<ExpressionBase> m_lastExpression;

public:
    CommaExpression(IntrVarValue<Type> type,
                    std::vector<std::pair<IntrVarPtr<ExpressionBase>, Lexer::CTokenIterator>>&& commaExpressions,
                    IntrVarPtr<ExpressionBase>&& lastExpression)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_commaExpressions(std::move(commaExpressions)),
          m_lastExpression(std::move(lastExpression))
    {
        CLD_ASSERT(m_commaExpressions.size() >= 1);
    }

    [[nodiscard]] const std::vector<std::pair<IntrVarPtr<ExpressionBase>, Lexer::CTokenIterator>>&
        getCommaExpressions() const
    {
        return m_commaExpressions;
    }

    [[nodiscard]] const ExpressionBase& getLastExpression() const
    {
        return *m_lastExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class CallExpression final : public ExpressionBase
{
    IntrVarPtr<ExpressionBase> m_functionExpression;
    Lexer::CTokenIterator m_openParentheses;
    std::vector<IntrVarPtr<ExpressionBase>> m_argumentExpressions;
    Lexer::CTokenIterator m_closeParentheses;

public:
    CallExpression(IntrVarValue<Type> type, IntrVarPtr<ExpressionBase>&& functionExpression,
                   Lexer::CTokenIterator openParentheses, std::vector<IntrVarPtr<ExpressionBase>>&& argumentExpressions,
                   Lexer::CTokenIterator closeParentheses)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_functionExpression(std::move(functionExpression)),
          m_openParentheses(openParentheses),
          m_argumentExpressions(std::move(argumentExpressions)),
          m_closeParentheses(closeParentheses)
    {
    }

    [[nodiscard]] const ExpressionBase& getFunctionExpression() const
    {
        return *m_functionExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] const std::vector<IntrVarPtr<ExpressionBase>>& getArgumentExpressions() const
    {
        return m_argumentExpressions;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const
    {
        return m_closeParentheses;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class InitializerList;

using Initializer = std::variant<InitializerList, IntrVarPtr<ExpressionBase>>;

class CompoundLiteral final : public ExpressionBase
{
    Lexer::CTokenIterator m_openParentheses;
    std::unique_ptr<Initializer> m_initializer;
    Lexer::CTokenIterator m_closeParentheses;
    Lexer::CTokenIterator m_initEnd;
    bool m_staticLifetime;

public:
    CompoundLiteral(IntrVarValue<Type> type, Lexer::CTokenIterator openParentheses, Initializer initializer,
                    Lexer::CTokenIterator closeParentheses, Lexer::CTokenIterator initEnd, bool staticLifetime);

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const Initializer& getInitializer() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;

    [[nodiscard]] bool hasStaticLifetime() const
    {
        return m_staticLifetime;
    }
};

class BuiltinVAArg final : public ExpressionBase
{
    Lexer::CTokenIterator m_builtinToken;
    Lexer::CTokenIterator m_openParentheses;
    IntrVarPtr<ExpressionBase> m_expression;
    Lexer::CTokenIterator m_closeParentheses;

public:
    BuiltinVAArg(IntrVarValue<Type> type, Lexer::CTokenIterator builtinToken, Lexer::CTokenIterator openParentheses,
                 IntrVarPtr<ExpressionBase>&& expression, Lexer::CTokenIterator closeParentheses)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
          m_builtinToken(builtinToken),
          m_openParentheses(openParentheses),
          m_expression(std::move(expression)),
          m_closeParentheses(closeParentheses)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getBuiltinToken() const
    {
        return m_builtinToken;
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] const ExpressionBase& getExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const
    {
        return m_closeParentheses;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_builtinToken;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_closeParentheses + 1;
    }
};

class BuiltinOffsetOf final : public ExpressionBase
{
    Lexer::CTokenIterator m_builtinToken;
    Lexer::CTokenIterator m_openParentheses;

public:
    struct RuntimeEval
    {
        IntrVarValue<Type> type;
        std::vector<std::variant<IntrVarPtr<ExpressionBase>, const Field*>> path;
        std::optional<diag::PointRange> failedConstExpr;
    };

    using OffsetType = std::variant<std::uint64_t, RuntimeEval>;

private:
    OffsetType m_offset;
    Lexer::CTokenIterator m_closeParentheses;

public:
    BuiltinOffsetOf(const LanguageOptions& options, Lexer::CTokenIterator builtinToken,
                    Lexer::CTokenIterator openParentheses, OffsetType offset, Lexer::CTokenIterator closeParentheses)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, PrimitiveType(options.sizeTType, options),
                         ValueCategory::Rvalue),
          m_builtinToken(builtinToken),
          m_openParentheses(openParentheses),
          m_offset(std::move(offset)),
          m_closeParentheses(closeParentheses)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getBuiltinToken() const
    {
        return m_builtinToken;
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] const OffsetType& getOffset() const
    {
        return m_offset;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const
    {
        return m_closeParentheses;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_builtinToken;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_closeParentheses + 1;
    }
};

class ErrorExpression final : public ExpressionBase
{
    Lexer::CTokenIterator m_begin;
    Lexer::CTokenIterator m_end;

public:
    explicit ErrorExpression(const Syntax::Node& node) : ErrorExpression(ErrorType{}, node) {}

    ErrorExpression(IntrVarValue<Type> type, const Syntax::Node& node);

    ErrorExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end)
        : ExpressionBase(std::in_place_type<ErrorExpression>, ErrorType{}, {}), m_begin(begin), m_end(end)
    {
    }

    ErrorExpression(IntrVarValue<Type> type, Lexer::CTokenIterator begin, Lexer::CTokenIterator end)
        : ExpressionBase(std::in_place_type<ErrorExpression>, std::move(type), {}), m_begin(begin), m_end(end)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_begin;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_end;
    }
};

class CompoundStatement;

class LabelStatement;

class CaseStatement;

class DefaultStatement;

class IfStatement;

class SwitchStatement;

class ForStatement;

class HeadWhileStatement;

class FootWhileStatement;

class BreakStatement;

class ContinueStatement;

class ReturnStatement;

class ExpressionStatement;

class GotoStatement;

class GNUASMStatement;

class Statement
    : public AbstractIntrusiveVariant<Statement, ForStatement, ReturnStatement, ExpressionStatement, IfStatement,
                                      CompoundStatement, HeadWhileStatement, FootWhileStatement, BreakStatement,
                                      ContinueStatement, SwitchStatement, DefaultStatement, CaseStatement,
                                      GotoStatement, LabelStatement, GNUASMStatement>
{
    std::size_t m_scope;

protected:
    template <class T>
    Statement(std::size_t scope, std::in_place_type_t<T>)
        : AbstractIntrusiveVariant(std::in_place_type<T>), m_scope(scope)
    {
    }

public:
    /**
     * Scope of the statement. If the statement starts a new scope
     * (Currently only Compound statement and for loop with declaration) it's the scope it started
     * @return Scope of the statement
     */
    [[nodiscard]] std::size_t getScope() const
    {
        return m_scope;
    }
};

class ReturnStatement final : public Statement
{
    IntrVarPtr<ExpressionBase> m_expression;

public:
    explicit ReturnStatement(std::int64_t scope, IntrVarPtr<ExpressionBase>&& expression)
        : Statement(scope, std::in_place_type<ReturnStatement>), m_expression(std::move(expression))
    {
    }

    const IntrVarPtr<ExpressionBase>& getExpression() const
    {
        return m_expression;
    }
};

class ExpressionStatement final : public Statement
{
    IntrVarPtr<ExpressionBase> m_expression;

public:
    explicit ExpressionStatement(std::size_t scope, IntrVarPtr<ExpressionBase>&& expression)
        : Statement(scope, std::in_place_type<ExpressionStatement>), m_expression(std::move(expression))
    {
    }

    const ExpressionBase* getExpression() const
    {
        return m_expression.get();
    }
};

class GotoStatement final : public Statement
{
    const LabelStatement* m_label;

public:
    explicit GotoStatement(std::size_t scope, const LabelStatement* label)
        : Statement(scope, std::in_place_type<GotoStatement>), m_label(label)
    {
    }

    const LabelStatement* getLabel() const
    {
        return m_label;
    }
};

using BreakableStatements = std::variant<const ForStatement*, const FootWhileStatement * CLD_NON_NULL,
                                         const HeadWhileStatement * CLD_NON_NULL, const SwitchStatement * CLD_NON_NULL>;

class BreakStatement final : public Statement
{
    BreakableStatements m_statement;

public:
    explicit BreakStatement(std::size_t scope, BreakableStatements statements)
        : Statement(scope, std::in_place_type<BreakStatement>), m_statement(statements)
    {
    }

    [[nodiscard]] const BreakableStatements& getBreakableStatement() const
    {
        return m_statement;
    }
};

using LoopStatements =
    std::variant<const ForStatement*, const FootWhileStatement * CLD_NON_NULL, const HeadWhileStatement * CLD_NON_NULL>;

class ContinueStatement final : public Statement
{
    LoopStatements m_loopStatement;

public:
    explicit ContinueStatement(std::size_t scope, LoopStatements loopStatement)
        : Statement(scope, std::in_place_type<ContinueStatement>), m_loopStatement(loopStatement)
    {
    }

    [[nodiscard]] const LoopStatements& getLoopStatement() const
    {
        return m_loopStatement;
    }
};

class IfStatement final : public Statement
{
    IntrVarPtr<ExpressionBase> m_expression;
    IntrVarPtr<Statement> m_trueBranch;
    IntrVarPtr<Statement> m_falseBranch;

public:
    IfStatement(std::size_t scope, IntrVarPtr<ExpressionBase>&& expression, IntrVarPtr<Statement>&& trueBranch,
                IntrVarPtr<Statement>&& falseBranch)
        : Statement(scope, std::in_place_type<IfStatement>),
          m_expression(std::move(expression)),
          m_trueBranch(std::move(trueBranch)),
          m_falseBranch(std::move(falseBranch))
    {
    }

    [[nodiscard]] const ExpressionBase& getExpression() const
    {
        return *m_expression;
    }

    [[nodiscard]] const Statement& getTrueBranch() const;

    [[nodiscard]] const Statement* getFalseBranch() const
    {
        return m_falseBranch.get();
    }
};

class Useable : public AbstractIntrusiveVariant<Useable, class VariableDeclaration, class FunctionDefinition,
                                                class BuiltinFunction, class FunctionDeclaration>
{
    std::uint64_t m_uses{};

protected:
    template <class T>
    Useable(std::in_place_type_t<T>) : AbstractIntrusiveVariant(std::in_place_type<T>)
    {
    }

    void setUses(std::uint64_t uses) noexcept
    {
        m_uses = uses;
    }

public:
    [[nodiscard]] std::uint64_t getUses() const noexcept
    {
        return m_uses;
    }

    void incrementUsage() noexcept
    {
        m_uses++;
    }

    [[nodiscard]] bool isUsed() const noexcept
    {
        return m_uses;
    }
};

enum class Linkage : std::uint8_t
{
    Internal,
    External,
    None
};

class Declaration : public Useable
{
    Linkage m_linkage;
    Lexer::CTokenIterator m_nameToken;

protected:
    template <class T>
    Declaration(std::in_place_type_t<T>, Linkage linkage, Lexer::CTokenIterator nameToken)
        : Useable(std::in_place_type<T>), m_linkage(linkage), m_nameToken(nameToken)
    {
    }

public:
    [[nodiscard]] Linkage getLinkage() const
    {
        return m_linkage;
    }

    [[nodiscard]] Lexer::CTokenIterator getNameToken() const
    {
        return m_nameToken;
    }
};

class CompoundStatement final : public Statement
{
public:
    using Variant = std::variant<IntrVarPtr<Statement>, IntrVarPtr<Declaration>, std::shared_ptr<const ExpressionBase>>;

private:
    Lexer::CTokenIterator m_openBrace;
    std::vector<Variant> m_compoundItems;
    Lexer::CTokenIterator m_closeBrace;

public:
    CompoundStatement(std::size_t scope, Lexer::CTokenIterator openBrace, std::vector<Variant>&& compoundItems,
                      Lexer::CTokenIterator closeBrace)
        : Statement(scope, std::in_place_type<CompoundStatement>),
          m_openBrace(openBrace),
          m_compoundItems(std::move(compoundItems)),
          m_closeBrace(closeBrace)
    {
    }

    CompoundStatement(const CompoundStatement&) = delete;
    CompoundStatement& operator=(const CompoundStatement&) = delete;

    CompoundStatement(CompoundStatement&&) noexcept = default;
    CompoundStatement& operator=(CompoundStatement&&) noexcept = default;

    void prependItem(Variant&& variant)
    {
        m_compoundItems.insert(m_compoundItems.begin(), std::move(variant));
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenBrace() const
    {
        return m_openBrace;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseBrace() const
    {
        return m_closeBrace;
    }

    [[nodiscard]] const std::vector<Variant>& getCompoundItems() const
    {
        return m_compoundItems;
    }
};

class ForStatement final : public Statement
{
public:
    using Variant = std::variant<std::monostate, IntrVarPtr<ExpressionBase>, std::vector<IntrVarPtr<Declaration>>>;

private:
    Lexer::CTokenIterator m_forToken;
    Variant m_initial;
    IntrVarPtr<ExpressionBase> m_controlling;
    IntrVarPtr<ExpressionBase> m_iteration;
    IntrVarPtr<Statement> m_statement;

public:
    ForStatement(std::size_t scope, Lexer::CTokenIterator forToken, Variant&& initial,
                 IntrVarPtr<ExpressionBase>&& controlling, IntrVarPtr<ExpressionBase>&& iteration,
                 IntrVarPtr<Statement>&& statement)
        : Statement(scope, std::in_place_type<ForStatement>),
          m_forToken(forToken),
          m_initial(std::move(initial)),
          m_controlling(std::move(controlling)),
          m_iteration(std::move(iteration)),
          m_statement(std::move(statement))
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getForToken() const
    {
        return m_forToken;
    }

    [[nodiscard]] const Variant& getInitial() const
    {
        return m_initial;
    }

    [[nodiscard]] const ExpressionBase* getControlling() const
    {
        return m_controlling.get();
    }

    [[nodiscard]] const ExpressionBase* getIteration() const
    {
        return m_iteration.get();
    }

    [[nodiscard]] const Statement& getStatement() const;
};

class HeadWhileStatement final : public Statement
{
    IntrVarPtr<ExpressionBase> m_expression;
    IntrVarPtr<Statement> m_statement;

public:
    HeadWhileStatement(std::size_t scope, IntrVarPtr<ExpressionBase>&& expression, IntrVarPtr<Statement>&& statement)
        : Statement(scope, std::in_place_type<HeadWhileStatement>),
          m_expression(std::move(expression)),
          m_statement(std::move(statement))
    {
    }

    [[nodiscard]] const ExpressionBase& getExpression() const
    {
        return *m_expression;
    }

    [[nodiscard]] const Statement& getStatement() const;
};

class FootWhileStatement final : public Statement
{
    IntrVarPtr<Statement> m_statement;
    IntrVarPtr<ExpressionBase> m_expression;

public:
    FootWhileStatement(std::size_t scope, IntrVarPtr<Statement>&& statement, IntrVarPtr<ExpressionBase>&& expression)
        : Statement(scope, std::in_place_type<FootWhileStatement>),
          m_statement(std::move(statement)),
          m_expression(std::move(expression))
    {
    }

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const ExpressionBase& getExpression() const
    {
        return *m_expression;
    }
};

class SwitchStatement final : public Statement
{
    IntrVarPtr<ExpressionBase> m_expression;
    IntrVarPtr<Statement> m_statement;
    std::map<llvm::APSInt, const CaseStatement * CLD_NON_NULL> m_cases;
    const DefaultStatement* CLD_NULLABLE m_default;

public:
    SwitchStatement(std::size_t scope, IntrVarPtr<ExpressionBase>&& expression, IntrVarPtr<Statement>&& statement,
                    std::map<llvm::APSInt, const CaseStatement* CLD_NON_NULL> cases = {},
                    const DefaultStatement* CLD_NULLABLE defaultStmt = nullptr)
        : Statement(scope, std::in_place_type<SwitchStatement>),
          m_expression(std::move(expression)),
          m_statement(std::move(statement)),
          m_cases(std::move(cases)),
          m_default(defaultStmt)
    {
    }

    [[nodiscard]] const ExpressionBase& getExpression() const&
    {
        return *m_expression;
    }

    [[nodiscard]] IntrVarPtr<ExpressionBase>&& getExpression() &&
    {
        return std::move(m_expression);
    }

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const std::map<llvm::APSInt, const CaseStatement * CLD_NON_NULL>& getCases() const
    {
        return m_cases;
    }

    [[nodiscard]] const DefaultStatement* getDefaultStatement() const
    {
        return m_default;
    }
};

class DefaultStatement final : public Statement
{
    Lexer::CTokenIterator m_defaultToken;
    Lexer::CTokenIterator m_colonToken;
    IntrVarPtr<Statement> m_statement;
    const SwitchStatement* CLD_NON_NULL m_switchStmt;

public:
    DefaultStatement(std::size_t scope, Lexer::CTokenIterator defaultToken, Lexer::CTokenIterator colonToken,
                     IntrVarPtr<Statement>&& statement, const SwitchStatement& switchStmt)
        : Statement(scope, std::in_place_type<DefaultStatement>),
          m_defaultToken(defaultToken),
          m_colonToken(colonToken),
          m_statement(std::move(statement)),
          m_switchStmt(&switchStmt)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getDefaultToken() const
    {
        return m_defaultToken;
    }

    [[nodiscard]] Lexer::CTokenIterator getColonToken() const
    {
        return m_colonToken;
    }

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const SwitchStatement& getSwitchStatement() const
    {
        return *m_switchStmt;
    }
};

class CaseStatement final : public Statement
{
    Lexer::CTokenIterator m_caseToken;
    llvm::APSInt m_constant;
    Lexer::CTokenIterator m_colonToken;
    IntrVarPtr<Statement> m_statement;
    const SwitchStatement* CLD_NON_NULL m_switchStmt;

public:
    CaseStatement(std::size_t scope, Lexer::CTokenIterator caseToken, llvm::APSInt constant,
                  Lexer::CTokenIterator colonToken, IntrVarPtr<Statement>&& statement,
                  const SwitchStatement& switchStmt)
        : Statement(scope, std::in_place_type<CaseStatement>),
          m_caseToken(caseToken),
          m_constant(std::move(constant)),
          m_colonToken(colonToken),
          m_statement(std::move(statement)),
          m_switchStmt(&switchStmt)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getCaseToken() const
    {
        return m_caseToken;
    }

    [[nodiscard]] const llvm::APSInt& getConstant() const
    {
        return m_constant;
    }

    [[nodiscard]] Lexer::CTokenIterator getColonToken() const
    {
        return m_colonToken;
    }

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const SwitchStatement& getSwitchStatement() const
    {
        return *m_switchStmt;
    }
};

class LabelStatement final : public Statement
{
    Lexer::CTokenIterator m_identifier;
    std::size_t m_sizeOfCurrentScope;
    IntrVarPtr<Statement> m_statement;

public:
    LabelStatement(std::size_t scope, Lexer::CTokenIterator identifier, std::size_t sizeOfCurrentScope,
                   IntrVarPtr<Statement>&& statement)
        : Statement(scope, std::in_place_type<LabelStatement>),
          m_identifier(identifier),
          m_sizeOfCurrentScope(sizeOfCurrentScope),
          m_statement(std::move(statement))
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getIdentifier() const
    {
        return m_identifier;
    }

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] std::size_t getSizeOfCurrentScope() const
    {
        return m_sizeOfCurrentScope;
    }
};

class GNUASMStatement final : public Statement
{
public:
    GNUASMStatement(std::int64_t scope) : Statement(scope, std::in_place_type<GNUASMStatement>) {}
};

struct FieldInLayout
{
    const Type* CLD_NON_NULL type;
    std::size_t layoutIndex; /// Index into the memory layout. Due to bitfields, multiple fields may have the same
                             /// layout index as bitfields share a common underlying fields. This field
                             /// is redundant in unions
    std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
};

using FieldMap = tsl::ordered_map<std::string_view, Field, std::hash<std::string_view>, std::equal_to<std::string_view>,
                                  std::allocator<std::pair<std::string_view, Field>>,
                                  std::vector<std::pair<std::string_view, Field>>>;

struct MemoryLayout
{
    const Type* CLD_NON_NULL type;
    std::size_t offset;
};

class StructDefinition
{
    std::string_view m_name;
    FieldMap m_fields;
    std::vector<FieldInLayout> m_fieldLayout;
    std::vector<MemoryLayout> m_memLayout;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

public:
    StructDefinition(std::string_view name, FieldMap fields, std::vector<FieldInLayout> fieldLayout,
                     std::vector<MemoryLayout> memLayout, std::uint64_t sizeOf, std::uint64_t alignOf)
        : m_name(name),
          m_fields(std::move(fields)),
          m_fieldLayout(std::move(fieldLayout)),
          m_memLayout(std::move(memLayout)),
          m_sizeOf(sizeOf),
          m_alignOf(alignOf)
    {
    }

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] bool isAnonymous() const
    {
        return m_name.empty();
    }

    [[nodiscard]] const FieldMap& getFields() const
    {
        return m_fields;
    }

    [[nodiscard]] const std::vector<FieldInLayout>& getFieldLayout() const
    {
        return m_fieldLayout;
    }

    [[nodiscard]] const std::vector<MemoryLayout>& getMemLayout() const
    {
        return m_memLayout;
    }

    [[nodiscard]] std::uint64_t getSizeOf() const
    {
        return m_sizeOf;
    }

    [[nodiscard]] std::uint64_t getAlignOf() const
    {
        return m_alignOf;
    }
};

class UnionDefinition
{
    std::string_view m_name;
    FieldMap m_fields;
    std::vector<FieldInLayout> m_fieldLayout;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

public:
    UnionDefinition(std::string_view name, FieldMap fields, std::vector<FieldInLayout> fieldLayout,
                    std::uint64_t sizeOf, std::uint64_t alignOf)
        : m_name(name),
          m_fields(std::move(fields)),
          m_fieldLayout(std::move(fieldLayout)),
          m_sizeOf(sizeOf),
          m_alignOf(alignOf)
    {
    }

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] bool isAnonymous() const
    {
        return m_name.empty();
    }

    [[nodiscard]] const FieldMap& getFields() const
    {
        return m_fields;
    }

    [[nodiscard]] const std::vector<FieldInLayout>& getFieldLayout() const
    {
        return m_fieldLayout;
    }

    [[nodiscard]] std::uint64_t getSizeOf() const
    {
        return m_sizeOf;
    }

    [[nodiscard]] std::uint64_t getAlignOf() const
    {
        return m_alignOf;
    }
};

const FieldMap& getFields(const Type& recordType);

llvm::ArrayRef<MemoryLayout> getMemoryLayout(const Type& structType);

llvm::ArrayRef<FieldInLayout> getFieldLayout(const Type& recordType);

class EnumDefinition
{
    std::string_view m_name;
    PrimitiveType m_type;
    std::vector<std::pair<std::string_view, llvm::APSInt>> m_values;

public:
    EnumDefinition(std::string_view name, PrimitiveType type,
                   std::vector<std::pair<std::string_view, llvm::APSInt>> values)
        : m_name(name), m_type(std::move(type)), m_values(std::move(values))
    {
    }

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] const PrimitiveType& getType() const
    {
        return m_type;
    }

    [[nodiscard]] const std::vector<std::pair<std::string_view, llvm::APSInt>>& getValues() const
    {
        return m_values;
    }
};

class InitializerList final
{
public:
    struct Initialization
    {
        std::vector<std::uint32_t> path;
        IntrVarPtr<ExpressionBase> expression;
    };

private:
    std::vector<Initialization> m_fields;

public:
    explicit InitializerList(std::vector<Initialization>&& fields) : m_fields(std::move(fields)) {}

    [[nodiscard]] const std::vector<Initialization>& getFields() const&
    {
        return m_fields;
    }

    [[nodiscard]] std::vector<Initialization>&& getFields() &&
    {
        return std::move(m_fields);
    }
};

template <class ValueType, class... Participants>
class DeclarationGroup
{
    const ValueType* m_first;

    class Iterator
    {
        const ValueType* m_curr;

    public:
        using value_type = const ValueType;
        using reference = const ValueType&;
        using pointer = const ValueType*;
        using iterator_category = std::forward_iterator_tag;
        using difference_type = void;

        Iterator() : m_curr(nullptr) {}

        explicit Iterator(const ValueType* curr) : m_curr(curr) {}

        bool operator==(const Iterator& rhs) const noexcept
        {
            return m_curr == rhs.m_curr;
        }

        bool operator!=(const Iterator& rhs) const noexcept
        {
            return !(*this == rhs);
        }

        reference operator*() const noexcept
        {
            CLD_ASSERT(m_curr);
            return *m_curr;
        }

        pointer operator->() const noexcept
        {
            return m_curr;
        }

        Iterator& operator++() noexcept
        {
            CLD_ASSERT(m_curr);
            m_curr = m_curr->match(
                [](const auto& value) -> const ValueType*
                {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr ((std::is_same_v<T, Participants> || ...))
                    {
                        return value.getNext();
                    }
                    CLD_UNREACHABLE;
                });
            return *this;
        }

        Iterator operator++(int) noexcept
        {
            auto before = *this;
            this->operator++();
            return before;
        }
    };

protected:
    explicit DeclarationGroup(cld::not_null<const ValueType> first) : m_first(first) {}

    using value_type = const ValueType;
    using reference = const ValueType&;
    using const_reference = const ValueType&;
    using iterator = Iterator;
    using const_iterator = iterator;

    [[nodiscard]] const_iterator begin() const
    {
        return Iterator(m_first);
    }

    [[nodiscard]] const_iterator end() const
    {
        return Iterator{};
    }

    [[nodiscard]] const_iterator cbegin() const
    {
        return begin();
    }

    [[nodiscard]] const_iterator cend() const
    {
        return end();
    }
};

enum class InlineKind : std::uint8_t
{
    InlineDefinition,
    Inline,
    None,
};

class FunctionGroup;

class FunctionDeclaration final : public Declaration, public AttributeHolder<FunctionAttribute>
{
    FunctionType m_type;
    InlineKind m_inlineKind;
    const Useable* m_next = nullptr;
    const Useable* m_first = nullptr;

    friend class FunctionDefinition;

public:
    FunctionDeclaration(FunctionType type, Linkage linkage, Lexer::CTokenIterator nameToken, InlineKind inlineKind,
                        Useable* previous);

    [[nodiscard]] const FunctionType& getType() const noexcept
    {
        return m_type;
    }

    [[nodiscard]] InlineKind getInlineKind() const noexcept
    {
        return m_inlineKind;
    }

    FunctionGroup getFunctionGroup() const noexcept;

    const Useable* getNext() const
    {
        return m_next;
    }

    const Useable& getFirst() const
    {
        return m_first ? *m_first : *this;
    }
};

enum class Lifetime : std::uint8_t
{
    Automatic,
    Static,
    Register
};

class VariableGroup;

class VariableDeclaration final : public Declaration, public AttributeHolder<VariableAttribute>
{
public:
    enum Kind : std::uint8_t
    {
        DeclarationOnly,     // If a declaration is not at file scope it's a "DeclarationOnly" if "extern"
        TentativeDefinition, // Only possible at file scope
        Definition
    };

private:
    IntrVarValue<Type> m_type;
    std::vector<VariableAttribute> m_variableAttributes;
    std::optional<Initializer> m_initializer;
    Lifetime m_lifetime;
    Kind m_kind;
    const VariableDeclaration* m_next = nullptr;
    const VariableDeclaration* m_first = nullptr;

public:
    VariableDeclaration(IntrVarValue<Type> type, Linkage linkage, Lifetime lifetime, Lexer::CTokenIterator nameToken,
                        Kind kind, std::optional<Initializer> initializer = {}, VariableDeclaration* previous = nullptr)
        : Declaration(std::in_place_type<VariableDeclaration>, linkage, nameToken),
          m_type(std::move(type)),
          m_initializer(std::move(initializer)),
          m_lifetime(lifetime),
          m_kind(kind),
          m_first(previous ? &previous->getFirst() : nullptr)
    {
        if (previous)
        {
            previous->m_next = this;
            setUses(previous->getUses());
        }
    }

    [[nodiscard]] const Type& getType() const noexcept
    {
        return *m_type;
    }

    void setType(IntrVarValue<Type> type) noexcept
    {
        m_type = std::move(type);
    }

    [[nodiscard]] Lifetime getLifetime() const
    {
        return m_lifetime;
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] const std::optional<Initializer>& getInitializer() const noexcept
    {
        return m_initializer;
    }

    void setInitializer(std::optional<Initializer> initializer)
    {
        m_initializer = std::move(initializer);
    }

    [[nodiscard]] VariableGroup getVariableGroup() const;

    [[nodiscard]] const VariableDeclaration* getNext() const
    {
        return m_next;
    }

    [[nodiscard]] const VariableDeclaration& getFirst() const
    {
        return m_first ? *m_first : *this;
    }
};

class VariableGroup final : public DeclarationGroup<VariableDeclaration, VariableDeclaration>
{
public:
    explicit VariableGroup(cld::not_null<const VariableDeclaration> decl) : DeclarationGroup(&decl->getFirst()) {}
};

class FunctionDefinition final : public Useable, public AttributeHolder<FunctionAttribute>
{
    FunctionType m_type;
    Lexer::CTokenIterator m_nameToken;
    std::vector<std::unique_ptr<VariableDeclaration>> m_parameterDeclarations;
    Linkage m_linkage;
    InlineKind m_inlineKind;
    CompoundStatement m_compoundStatement;
    const FunctionDeclaration* m_next = nullptr;
    const FunctionDeclaration* m_first = nullptr;

    friend class FunctionDeclaration;

public:
    FunctionDefinition(FunctionType type, Lexer::CTokenIterator nameToken,
                       std::vector<std::unique_ptr<VariableDeclaration>> parameterDeclarations, Linkage linkage,
                       InlineKind inlineKind, CompoundStatement compoundStatement, FunctionDeclaration* previous)
        : Useable(std::in_place_type<FunctionDefinition>),
          m_type(std::move(type)),
          m_nameToken(nameToken),
          m_parameterDeclarations(std::move(parameterDeclarations)),
          m_linkage(linkage),
          m_inlineKind(inlineKind),
          m_compoundStatement(std::move(compoundStatement)),
          m_first(previous ? &previous->getFirst().as<FunctionDeclaration>() : nullptr)
    {
        if (previous)
        {
            previous->m_next = this;
            setUses(previous->getUses());
        }
    }

    [[nodiscard]] Lexer::CTokenIterator getNameToken() const
    {
        return m_nameToken;
    }

    [[nodiscard]] const FunctionType& getType() const
    {
        return m_type;
    }

    [[nodiscard]] const std::vector<std::unique_ptr<VariableDeclaration>>& getParameterDeclarations() const&
    {
        return m_parameterDeclarations;
    }

    [[nodiscard]] std::vector<std::unique_ptr<VariableDeclaration>>&& getParameterDeclarations() &&
    {
        return std::move(m_parameterDeclarations);
    }

    [[nodiscard]] bool isKandR() const
    {
        return m_type.isKandR();
    }

    [[nodiscard]] Linkage getLinkage() const
    {
        return m_linkage;
    }

    [[nodiscard]] const CompoundStatement& getCompoundStatement() const
    {
        return m_compoundStatement;
    }

    void setCompoundStatement(CompoundStatement&& compoundStatement)
    {
        m_compoundStatement = std::move(compoundStatement);
    }

    [[nodiscard]] InlineKind getInlineKind() const noexcept
    {
        return m_inlineKind;
    }

    FunctionGroup getFunctionGroup() const noexcept;

    const FunctionDeclaration* getNext() const
    {
        return m_next;
    }

    const Useable& getFirst() const
    {
        return m_first ? static_cast<const Useable&>(*m_first) : static_cast<const Useable&>(*this);
    }
};

class BuiltinFunction final : public Useable
{
    FunctionType m_type;

public:
    enum Kind
    {
        VAStart,
        VAEnd,
        VACopy,
        LLAbs,
        LAbs,
        Abs,
        FAbs,
        FAbsf,
        FAbsl,
        Inf,
        Inff,
        Infl,
        SyncFetchAndAdd,
        SyncFetchAndSub,
        SyncFetchAndOr,
        SyncFetchAndAnd,
        SyncFetchAndXor,
        SyncFetchAndNand,
        SyncAddAndFetch,
        SyncSubAndFetch,
        SyncOrAndFetch,
        SyncAndAndFetch,
        SyncXorAndFetch,
        SyncNandAndFetch,
        SyncBoolCompareAndSwap,
        SyncValCompareAndSwap,
        SyncSynchronize,
        SyncLockTestAndSet,
        SyncLockRelease,
        ReturnAddress,
        ExtractReturnAddr,
        FRobReturnAddr,
        FrameAddress,
        Expect,
        ExpectWithProbability,
        Prefetch,
        ClearCache,
        Trap,
        Unreachable,
        x86CpuInit,
        x86CpuIs,
        x86CpuSupports,
    };

private:
    Kind m_kind;

public:
    BuiltinFunction(FunctionType type, Kind kind)
        : Useable(std::in_place_type<BuiltinFunction>), m_type(std::move(type)), m_kind(kind)
    {
    }

    [[nodiscard]] const FunctionType& getType() const noexcept
    {
        return m_type;
    }

    [[nodiscard]] Kind getKind() const noexcept
    {
        return m_kind;
    }
};

class FunctionGroup final : public DeclarationGroup<Useable, FunctionDefinition, FunctionDeclaration>
{
public:
    explicit FunctionGroup(cld::not_null<const FunctionDeclaration> decl) : DeclarationGroup(&decl->getFirst()) {}

    explicit FunctionGroup(cld::not_null<const FunctionDefinition> decl) : DeclarationGroup(&decl->getFirst()) {}

    [[nodiscard]] bool isInline() const
    {
        return std::none_of(
            begin(), end(),
            [](const Useable& useable)
            {
                return useable.match([](const FunctionDefinition& def) -> InlineKind { return def.getInlineKind(); },
                                     [](const FunctionDeclaration& decl) -> InlineKind { return decl.getInlineKind(); },
                                     [](const auto&) -> InlineKind { CLD_UNREACHABLE; })
                       == InlineKind::None;
            });
    }

    [[nodiscard]] bool isExternInline() const
    {
        for (auto& useable : *this)
        {
            auto kind =
                useable.match([](const FunctionDefinition& def) -> InlineKind { return def.getInlineKind(); },
                              [](const FunctionDeclaration& decl) -> InlineKind { return decl.getInlineKind(); },
                              [](const auto&) -> InlineKind { CLD_UNREACHABLE; });
            switch (kind)
            {
                case InlineKind::None: return false;
                case InlineKind::Inline: return true;
                default: continue;
            }
        }
        return false;
    }

    [[nodiscard]] bool isDefined() const
    {
        return getDefinition();
    }

    [[nodiscard]] const FunctionDefinition* getDefinition() const
    {
        auto result = std::find_if(begin(), end(), std::mem_fn(&Useable::is<FunctionDefinition>));
        if (result == end())
        {
            return nullptr;
        }
        return &result->as<FunctionDefinition>();
    }
};

class TranslationUnit final
{
    std::vector<IntrVarPtr<Useable>> m_globals;

public:
    TranslationUnit() = default;

    explicit TranslationUnit(std::vector<IntrVarPtr<Useable>> globals) : m_globals(std::move(globals)) {}

    [[nodiscard]] const std::vector<IntrVarPtr<Useable>>& getGlobals() const
    {
        return m_globals;
    }
};

class Program;

Program analyse(const Syntax::TranslationUnit& parseTree, CSourceObject&& cTokens,
                llvm::raw_ostream* reporter = &llvm::errs(), bool* errors = nullptr);

[[nodiscard]] Lexer::CTokenIterator declaratorToLoc(const cld::Syntax::Declarator& declarator);

[[nodiscard]] bool isCompleteType(const Type& type);

[[nodiscard]] bool isVoid(const Type& type);

[[nodiscard]] bool isArray(const Type& type);

[[nodiscard]] bool isCharArray(const Type& type, const LanguageOptions& options);

[[nodiscard]] const Type& getArrayElementType(const Type& type);

[[nodiscard]] const Type& getVectorElementType(const Type& type);

[[nodiscard]] const Type& getPointerElementType(const Type& type);

[[nodiscard]] IntrVarValue<Type> adjustParameterType(const Type& type);

[[nodiscard]] bool isInteger(const Type& type);

[[nodiscard]] bool isArithmetic(const Type& type);

[[nodiscard]] bool isScalar(const Type& type);

[[nodiscard]] bool isRecord(const Type& type);

[[nodiscard]] bool isAnonymous(const Type& type);

[[nodiscard]] bool isBool(const Type& type);

[[nodiscard]] bool isCharType(const Type& type);

[[nodiscard]] bool isAggregate(const Type& type);

[[nodiscard]] bool isVariablyModified(const Type& type);

[[nodiscard]] bool isVariableLengthArray(const Type& type);

[[nodiscard]] bool isCharacterLikeType(const Type& type, const LanguageOptions& options);

[[nodiscard]] IntrVarValue<Type> removeQualifiers(const Type& type);

} // namespace cld::Semantics

namespace cld::diag
{
template <>
struct StringConverter<Semantics::Type>
{
    static std::string inFormat(const Semantics::Type& arg, const SourceInterface* sourceInterface)
    {
        return "'" + inArg(arg, sourceInterface) + "'";
    }

    static std::string inArg(const Semantics::Type& arg, const SourceInterface*);
};

template <>
struct CustomFormat<U'f', U'u', U'l', U'l'>
{
    std::string operator()(const Semantics::Type& arg);
};

template <>
struct CustomFormat<U't', U'y', U'p', U'e'>
{
    std::string operator()(const Semantics::ExpressionBase& arg);
};

template <>
struct CustomFormat<U'f', U'u', U'l', U'l', U'T', U'y', U'p', U'e'>
{
    std::string operator()(const Semantics::ExpressionBase& arg);
};

} // namespace cld::diag

namespace std
{
template <>
struct hash<cld::Semantics::Type>
{
    std::size_t operator()(const cld::Semantics::Type& type) const noexcept;
};

template <>
struct hash<cld::Semantics::PrimitiveType>
{
    std::size_t operator()(const cld::Semantics::PrimitiveType& type) const noexcept
    {
        return cld::hashCombine(type.getKind());
    }
};

template <>
struct hash<cld::Semantics::ArrayType>
{
    std::size_t operator()(const cld::Semantics::ArrayType& type) const noexcept
    {
        return cld::hashCombine(type.isRestricted(), type.isStatic(), type.getSize(), type.getType());
    }
};

template <>
struct hash<cld::Semantics::AbstractArrayType>
{
    std::size_t operator()(const cld::Semantics::AbstractArrayType& type) const noexcept
    {
        return cld::hashCombine(type.isRestricted(), type.getType());
    }
};

template <>
struct hash<cld::Semantics::ValArrayType>
{
    std::size_t operator()(const cld::Semantics::ValArrayType& type) const noexcept
    {
        return cld::hashCombine(type.isRestricted(), type.isStatic(), type.getExpression(), type.getType());
    }
};

template <>
struct hash<cld::Semantics::FunctionType>
{
    std::size_t operator()(const cld::Semantics::FunctionType& type) const noexcept
    {
        std::size_t hash = 0;
        for (auto& [type, name] : type.getParameters())
        {
            hash = cld::rawHashCombine(hash, std::hash<cld::Semantics::Type>{}(*type));
        }
        return cld::rawHashCombine(cld::hashCombine(type.isLastVararg(), type.isKandR(), type.getReturnType()), hash);
    }
};

template <>
struct hash<cld::Semantics::StructType>
{
    std::size_t operator()(const cld::Semantics::StructType& type) const noexcept
    {
        return std::hash<const cld::Semantics::StructInfo*>{}(&type.getInfo());
    }
};

template <>
struct hash<cld::Semantics::UnionType>
{
    std::size_t operator()(const cld::Semantics::UnionType& type) const noexcept
    {
        return std::hash<const cld::Semantics::UnionInfo*>{}(&type.getInfo());
    }
};

template <>
struct hash<cld::Semantics::EnumType>
{
    std::size_t operator()(const cld::Semantics::EnumType& type) const noexcept
    {
        return std::hash<const cld::Semantics::EnumInfo*>{}(&type.getInfo());
    }
};

template <>
struct hash<cld::Semantics::PointerType>
{
    std::size_t operator()(const cld::Semantics::PointerType& type) const noexcept
    {
        return cld::hashCombine(type.isRestricted(), type.getElementType());
    }
};

template <>
struct hash<cld::Semantics::VectorType>
{
    std::size_t operator()(const cld::Semantics::VectorType& type) const noexcept
    {
        return cld::hashCombine(type.getSize(), type.getType());
    }
};

template <>
struct hash<cld::Semantics::ErrorType>
{
    std::size_t operator()(const cld::Semantics::ErrorType&) const noexcept
    {
        return 0;
    }
};

inline std::size_t std::hash<cld::Semantics::Type>::operator()(const cld::Semantics::Type& type) const noexcept
{
    return cld::rawHashCombine(type.match(
                                   [](const auto& type) -> std::size_t
                                   {
                                       using T = std::decay_t<decltype(type)>;
                                       return std::hash<T>{}(type);
                                   }),
                               cld::hashCombine(type.isConst(), type.isVolatile(), type.index()));
}
} // namespace std

inline bool cld::Semantics::isVoid(const cld::Semantics::Type& type)
{
    auto* primitive = type.tryAs<PrimitiveType>();
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Void;
}

inline bool cld::Semantics::isArray(const Type& type)
{
    return type.is<ArrayType>() || type.is<AbstractArrayType>() || type.is<ValArrayType>();
}

inline bool cld::Semantics::isCharArray(const Type& type, const LanguageOptions& options)
{
    if (!isArray(type))
    {
        return false;
    }
    return isCharacterLikeType(getArrayElementType(type), options);
}

inline const cld::Semantics::Type& cld::Semantics::getArrayElementType(const Type& type)
{
    return type.match(
        [](const auto&) -> const Type& { CLD_UNREACHABLE; },
        [](const ArrayType& arrayType) -> const Type& { return arrayType.getType(); },
        [](const AbstractArrayType& abstractArrayType) -> const Type& { return abstractArrayType.getType(); },
        [](const ValArrayType& arrayType) -> const Type& { return arrayType.getType(); });
}

inline const cld::Semantics::Type& cld::Semantics::getVectorElementType(const Type& type)
{
    return type.as<VectorType>().getType();
}

inline const cld::Semantics::Type& cld::Semantics::getPointerElementType(const Type& type)
{
    return type.as<PointerType>().getElementType();
}

inline bool cld::Semantics::isInteger(const Type& type)
{
    return type.is<PrimitiveType>() && !type.as<PrimitiveType>().isFloatingPoint()
           && type.as<PrimitiveType>().getBitCount() != 0;
}

inline bool cld::Semantics::isArithmetic(const Type& type)
{
    return (type.is<PrimitiveType>() && type.as<PrimitiveType>().getBitCount() != 0) || type.is<EnumType>();
}

inline bool cld::Semantics::isScalar(const Type& type)
{
    return isArithmetic(type) || type.is<PointerType>();
}

inline bool cld::Semantics::isRecord(const cld::Semantics::Type& type)
{
    return type.is<UnionType>() || type.is<StructType>();
}

inline bool cld::Semantics::isAnonymous(const Type& type)
{
    return (type.is<EnumType>() && type.as<EnumType>().isAnonymous())
           || (type.is<StructType>() && type.as<StructType>().isAnonymous())
           || (type.is<UnionType>() && type.as<UnionType>().isAnonymous());
}

inline bool cld::Semantics::isBool(const cld::Semantics::Type& type)
{
    auto* primitive = type.tryAs<PrimitiveType>();
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Bool;
}

inline bool cld::Semantics::isCharType(const cld::Semantics::Type& type)
{
    auto* primitive = type.tryAs<PrimitiveType>();
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Char
           || primitive->getKind() == PrimitiveType::Kind::UnsignedChar
           || primitive->getKind() == PrimitiveType::Kind::SignedChar;
}

inline bool cld::Semantics::isCharacterLikeType(const Type& type, const LanguageOptions& options)
{
    if (isCharType(type))
    {
        return true;
    }
    return removeQualifiers(type) == PrimitiveType(options.wcharUnderlyingType, options);
}

inline bool cld::Semantics::isAggregate(const Type& type)
{
    return isRecord(type) || isArray(type) || type.is<VectorType>();
}

inline cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::removeQualifiers(const Type& type)
{
    if (type.isConst() || type.isVolatile() || type.isRestricted())
    {
        IntrVarValue copy = type;
        copy->setConst(false);
        copy->setVolatile(false);
        copy->setRestricted(false);
        return copy;
    }
    return type;
}

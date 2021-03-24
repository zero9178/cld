#pragma clang diagnostic push
#pragma ide diagnostic ignored "HidingNonVirtualFunction"
#pragma once

#include <cld/Support/AbstractIntrusiveVariant.h>

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <tsl/ordered_map.h>

#include "Attributes.hpp"
#include "LanguageOptions.hpp"
#include "Lexer.hpp"

namespace cld
{
class Message;

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

class Type;

class StructDefinition;

class UnionDefinition;

class EnumDefinition;

class ProgramInterface;

namespace details
{
enum class TypeFlags : std::uint8_t
{
    Nothing = 0,
    Const = 0b1,
    Volatile = 0b10,
    Restricted = 0b100,
    Static = 0b1000,
};

using TypeFlagsUT = std::underlying_type_t<TypeFlags>;

template <class Left, class Right>
struct OrExpr
{
    Left left;
    Right right;

    constexpr operator TypeFlags() const
    {
        return static_cast<TypeFlags>(static_cast<TypeFlagsUT>(left) | static_cast<TypeFlagsUT>(right));
    }
};

template <class T>
struct IsOrExpr : std::false_type
{
};

template <class Left, class Right>
struct IsOrExpr<OrExpr<Left, Right>> : std::true_type
{
};

template <class Left, class Right>
struct AndExpr
{
    Left left;
    Right right;

    constexpr operator TypeFlags() const
    {
        return static_cast<TypeFlags>(static_cast<TypeFlagsUT>(left) & static_cast<TypeFlagsUT>(right));
    }
};

template <class T>
struct IsAndExpr : std::false_type
{
};

template <class Left, class Right>
struct IsAndExpr<AndExpr<Left, Right>> : std::true_type
{
};

template <TypeFlags value>
struct Constant
{
    constexpr operator TypeFlags() const
    {
        return value;
    }
};

template <class T>
auto isConstant(const T&) -> std::false_type;

template <TypeFlags value>
auto isConstant(const Constant<value>&) -> std::true_type;

struct Value
{
    TypeFlags value;

    constexpr operator TypeFlags() const
    {
        return value;
    }
};

template <class T>
constexpr bool typeAllowedInExpr()
{
    return std::is_same_v<T, TypeFlags> || std::is_same_v<T, Value> || IsOrExpr<T>{} || IsAndExpr<T>{}
           || decltype(isConstant(std::declval<T>())){};
}

template <class T>
constexpr auto toValue(const T& value)
{
    if constexpr (std::is_same_v<T, TypeFlags>)
    {
        return Value{value};
    }
    else
    {
        return value;
    }
}

template <class Left, class Right,
          std::enable_if_t<(typeAllowedInExpr<Left>() && typeAllowedInExpr<Right>())>* = nullptr>
constexpr OrExpr<Left, Right> operator|(const Left& left, const Right& right)
{
    return OrExpr{toValue(left), toValue(right)};
}

template <TypeFlags left, TypeFlags right>
constexpr auto operator|(const Constant<left>&, const Constant<right>&)
    -> Constant<static_cast<TypeFlags>(static_cast<TypeFlagsUT>(left) | static_cast<TypeFlagsUT>(right))>
{
    return {};
}

template <class Left, class Right,
          std::enable_if_t<(typeAllowedInExpr<Left>() && typeAllowedInExpr<Right>())>* = nullptr>
constexpr AndExpr<Left, Right> operator&(const Left& left, const Right& right)
{
    return AndExpr{toValue(left), toValue(right)};
}

template <TypeFlags left, TypeFlags right>
constexpr auto operator&(const Constant<left>&, const Constant<right>&)
    -> Constant<static_cast<TypeFlags>(static_cast<TypeFlagsUT>(left) & static_cast<TypeFlagsUT>(right))>
{
    return {};
}

template <TypeFlags... allowedValues, class T>
constexpr auto containsOnlyAllowed(const T& arg) -> std::enable_if_t<typeAllowedInExpr<T>(), bool>
{
    struct MaskValue
    {
        TypeFlagsUT knownBitMask;
        TypeFlagsUT knownBitValues;
    };
    MaskValue result = YComb{[&](auto&& self, const auto& arg) -> MaskValue {
        using U = std::decay_t<decltype(arg)>;
        if constexpr (IsOrExpr<U>{})
        {
            MaskValue left = self(arg.left);
            MaskValue right = self(arg.right);
            return {static_cast<TypeFlagsUT>((left.knownBitValues & left.knownBitMask)
                                             | (right.knownBitValues & right.knownBitValues)
                                             | (left.knownBitMask & right.knownBitMask)),
                    static_cast<TypeFlagsUT>((left.knownBitValues & left.knownBitMask)
                                             | (right.knownBitValues & right.knownBitValues))};
        }
        else if constexpr (IsAndExpr<U>{})
        {
            MaskValue left = self(arg.left);
            MaskValue right = self(arg.right);
            auto knownBits = (~(~left.knownBitMask | left.knownBitValues))
                             | (~(~right.knownBitMask | right.knownBitValues))
                             | (left.knownBitMask & right.knownBitMask);
            return {static_cast<TypeFlagsUT>(knownBits),
                    static_cast<TypeFlagsUT>((left.knownBitValues & left.knownBitMask)
                                             & (right.knownBitValues & right.knownBitValues))};
        }
        else if constexpr (std::is_same_v<U, Value>)
        {
            return {0, 0};
        }
        else if constexpr (decltype(isConstant(std::declval<U>())){})
        {
            return {static_cast<TypeFlagsUT>(-1), static_cast<TypeFlagsUT>(static_cast<TypeFlags>(arg))};
        }
        else
        {
            CLD_UNREACHABLE;
        }
    }}(arg);
    auto mask = (static_cast<TypeFlagsUT>(allowedValues) | ...);
    return (result.knownBitValues & result.knownBitMask & (~mask)) == 0;
}

inline TypeFlags& operator|=(TypeFlags& left, const TypeFlags right)
{
    left = static_cast<TypeFlags>(static_cast<TypeFlagsUT>(left) | static_cast<TypeFlagsUT>(right));
    return left;
}

inline TypeFlags& operator&=(TypeFlags& left, const TypeFlags right)
{
    left = static_cast<TypeFlags>(static_cast<TypeFlagsUT>(left) & static_cast<TypeFlagsUT>(right));
    return left;
}

} // namespace details

using details::TypeFlags;

constexpr auto NothingFlag = details::Constant<TypeFlags::Nothing>{};
constexpr auto ConstFlag = details::Constant<TypeFlags::Const>{};
constexpr auto VolatileFlag = details::Constant<TypeFlags::Volatile>{};
constexpr auto RestrictedFlag = details::Constant<TypeFlags::Restricted>{};
constexpr auto StaticFlag = details::Constant<TypeFlags::Static>{};

class PrimitiveType final
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

    PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount, std::uint8_t alignOf, Kind kind);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, bool isFloatingPoint, bool isSigned,
                                     std::uint8_t bitCount, std::uint8_t alignOf, Kind kind);

    [[nodiscard]] static Type createChar(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createSignedChar(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createUnderlineBool(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createUnsignedChar(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createShort(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createUnsignedShort(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createInt(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createUnsignedInt(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createLong(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createUnsignedLong(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createLongLong(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createUnsignedLongLong(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createFloat(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createDouble(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createLongDouble(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createVoid(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createInt128(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createUnsignedInt128(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createPtrdiffT(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createSizeT(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createWcharT(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type fromUnderlyingType(bool isConst, bool isVolatile,
                                                 LanguageOptions::UnderlyingType underlyingType,
                                                 const LanguageOptions& options);

    [[nodiscard]] bool isFloatingPoint() const
    {
        return m_isFloatingPoint;
    }

    [[nodiscard]] bool isSigned() const
    {
        return m_isSigned;
    }

    [[nodiscard]] std::uint8_t getByteCount() const;

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface&) const
    {
        return getByteCount();
    }

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface&) const
    {
        return m_alignOf;
    }

    [[nodiscard]] std::uint8_t getBitCount() const
    {
        return m_bitCount;
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] bool operator==(const PrimitiveType& rhs) const;

    [[nodiscard]] bool operator!=(const PrimitiveType& rhs) const;
};

class ArrayType final
{
    std::shared_ptr<const Type> m_type;
    std::uint64_t m_size;
    bool m_restricted : 1;
    bool m_static : 1;

    ArrayType(bool isRestricted, bool isStatic, std::shared_ptr<Type>&& type, std::uint64_t size);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type type,
                                     std::uint64_t size);

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] std::uint64_t getSize() const
    {
        return m_size;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_restricted;
    }

    [[nodiscard]] bool isStatic() const
    {
        return m_static;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const ArrayType& rhs) const;

    [[nodiscard]] bool operator!=(const ArrayType& rhs) const;
};

class AbstractArrayType final
{
    std::shared_ptr<const Type> m_type;
    bool m_restricted;

    AbstractArrayType(bool isRestricted, std::shared_ptr<Type>&& type);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, bool isRestricted, Type type);

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_restricted;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const AbstractArrayType& rhs) const;

    [[nodiscard]] bool operator!=(const AbstractArrayType& rhs) const;
};

class ExpressionBase;

class ValArrayType final
{
    std::shared_ptr<const Type> m_type;
    bool m_restricted : 1;
    bool m_static : 1;
    std::shared_ptr<const ExpressionBase> m_expression;

    ValArrayType(bool isRestricted, bool isStatic, std::shared_ptr<cld::Semantics::Type>&& type,
                 std::shared_ptr<const ExpressionBase>&& expression);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type type,
                                     std::shared_ptr<const ExpressionBase> expression);

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_restricted;
    }

    [[nodiscard]] bool isStatic() const
    {
        return m_static;
    }

    // NULLABLE
    [[nodiscard]] const std::shared_ptr<const ExpressionBase>& getExpression() const
    {
        return m_expression;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const ValArrayType& rhs) const;

    [[nodiscard]] bool operator!=(const ValArrayType& rhs) const;
};

class FunctionType final
{
    std::shared_ptr<const Type> m_returnType;
    std::vector<std::pair<Type, std::string_view>> m_arguments;
    bool m_lastIsVararg : 1;
    bool m_isKandR : 1;

    FunctionType(std::shared_ptr<const Type>&& returnType, std::vector<std::pair<Type, std::string_view>> arguments,
                 bool lastIsVararg, bool isKandR)
        : m_returnType(std::move(returnType)),
          m_arguments(std::move(arguments)),
          m_lastIsVararg(lastIsVararg),
          m_isKandR(isKandR)
    {
    }

public:
    [[nodiscard]] static Type create(cld::Semantics::Type returnType,
                                     std::vector<std::pair<Type, std::string_view>>&& arguments, bool lastIsVararg,
                                     bool isKandR);

    [[nodiscard]] const Type& getReturnType() const
    {
        return *m_returnType;
    }

    [[nodiscard]] const std::vector<std::pair<Type, std::string_view>>& getArguments() const
    {
        return m_arguments;
    }

    [[nodiscard]] bool isLastVararg() const
    {
        return m_lastIsVararg;
    }

    [[nodiscard]] bool isKandR() const
    {
        return m_isKandR;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] bool operator==(const FunctionType& rhs) const;

    [[nodiscard]] bool operator!=(const FunctionType& rhs) const;
};

class StructType final
{
    std::string_view m_name;
    const StructInfo* m_info;

    StructType(std::string_view name, const StructInfo& info);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, std::string_view name, const StructInfo& info);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] bool isAnonymous() const
    {
        return m_name.empty();
    }

    [[nodiscard]] const StructInfo& getInfo() const
    {
        return *m_info;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const StructType& rhs) const
    {
        return m_info == rhs.m_info;
    }

    [[nodiscard]] bool operator!=(const StructType& rhs) const
    {
        return !(rhs == *this);
    }
};

class UnionType final
{
    std::string_view m_name;
    const UnionInfo* m_info;

    UnionType(std::string_view name, const UnionInfo& info);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, const UnionInfo& info);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] bool isAnonymous() const
    {
        return m_name.empty();
    }

    [[nodiscard]] const UnionInfo& getInfo() const
    {
        return *m_info;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const UnionType& rhs) const
    {
        return m_info == rhs.m_info;
    }

    [[nodiscard]] bool operator!=(const UnionType& rhs) const
    {
        return !(rhs == *this);
    }
};

class EnumType final
{
    std::string_view m_name;
    const EnumInfo* m_info;

    EnumType(std::string_view name, const EnumInfo& info);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, const EnumInfo& info);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] bool isAnonymous() const
    {
        return m_name.empty();
    }

    [[nodiscard]] const EnumInfo& getInfo() const
    {
        return *m_info;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const EnumType& rhs) const
    {
        return m_info == rhs.m_info;
    }

    [[nodiscard]] bool operator!=(const EnumType& rhs) const
    {
        return !(rhs == *this);
    }
};

class PointerType final
{
    std::shared_ptr<const Type> m_elementType;
    bool m_restricted;

    PointerType(bool isRestricted, std::shared_ptr<Type>&& elementType);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, Type elementType);

    [[nodiscard]] const Type& getElementType() const&
    {
        return *m_elementType;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_restricted;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const PointerType& rhs) const;

    [[nodiscard]] bool operator!=(const PointerType& rhs) const;
};

class VectorType final
{
    std::shared_ptr<const Type> m_elementType;
    std::uint64_t m_size;

    VectorType(std::shared_ptr<Type>&& type, std::uint64_t size);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, Type type, std::uint64_t size);

    [[nodiscard]] const Type& getType() const
    {
        return *m_elementType;
    }

    [[nodiscard]] std::uint64_t getSize() const
    {
        return m_size;
    }

    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const VectorType& rhs) const;

    [[nodiscard]] bool operator!=(const VectorType& rhs) const;
};

class Type final : public AttributeHolder<TypeAttribute>
{
public:
    using Variant = std::variant<std::monostate, PrimitiveType, ArrayType, AbstractArrayType, ValArrayType,
                                 FunctionType, StructType, UnionType, EnumType, PointerType, VectorType>;

private:
    Variant m_type;
    std::string_view m_name;
    bool m_isConst : 1;
    bool m_isVolatile : 1;

public:
    explicit Type(bool isConst = false, bool isVolatile = false, Variant type = std::monostate{})
        : m_type(std::move(type)), m_isConst(isConst), m_isVolatile(isVolatile)
    {
    }

    [[nodiscard]] const Variant& getVariant() const&
    {
        return m_type;
    }

    [[nodiscard]] Variant&& getVariant() &&
    {
        return std::move(m_type);
    }

    [[nodiscard]] bool isConst() const
    {
        return m_isConst;
    }

    [[nodiscard]] bool isVolatile() const
    {
        return m_isVolatile;
    }

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    void setName(std::string_view name)
    {
        m_name = name;
    }

    [[nodiscard]] bool isTypedef() const
    {
        return !m_name.empty();
    }

    [[nodiscard]] bool operator==(const Type& rhs) const;

    [[nodiscard]] bool operator!=(const Type& rhs) const;

    [[nodiscard]] bool isUndefined() const
    {
        return std::holds_alternative<std::monostate>(m_type);
    }

    // Likely replaced with an interface soon?
    [[nodiscard]] std::uint64_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::uint64_t getAlignOf(const ProgramInterface& program) const;
};

enum class ValueCategory : std::uint8_t
{
    Lvalue,
    Rvalue
};

class ExpressionBase
    : public AbstractIntrusiveVariant<class Constant, class DeclarationRead, class Conversion, class MemberAccess,
                                      class BinaryOperator, class Cast, class UnaryOperator, class SizeofOperator,
                                      class SubscriptOperator, class Conditional, class Assignment,
                                      class CommaExpression, class CallExpression, class CompoundLiteral,
                                      class BuiltinVAArg, class BuiltinOffsetOf, class ErrorExpression>
{
    Type m_type;
    ValueCategory m_valueCategory;

protected:
    template <class T>
    ExpressionBase(std::in_place_type_t<T>, Type type, ValueCategory valueCategory)
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
        return m_type;
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
    Constant(Type type, Variant value, Lexer::CTokenIterator valueBegin, Lexer::CTokenIterator valueEnd)
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
    explicit DeclarationRead(Type type, const Useable& useable, Lexer::CTokenIterator identifierToken)
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
    Conversion(Type type, Kind kind, IntrVarPtr<ExpressionBase>&& expression)
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
    Cast(Type type, Lexer::CTokenIterator openParentheses, Lexer::CTokenIterator closeParentheses,
         IntrVarPtr<ExpressionBase>&& expression)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>, std::move(type), ValueCategory::Rvalue),
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
    std::shared_ptr<const Type> type; // NOT NULL
    std::string_view name;
    const Lexer::CToken* nameToken;
    std::vector<std::size_t> indices;
    std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
    std::vector<std::shared_ptr<const Type>> parentTypes;
};

class MemberAccess final : public ExpressionBase
{
    IntrVarPtr<ExpressionBase> m_recordExpr;
    const Field* m_field;
    Lexer::CTokenIterator m_memberIdentifier;

public:
    MemberAccess(Type type, ValueCategory valueCategory, IntrVarPtr<ExpressionBase>&& recordExpr, const Field& field,
                 Lexer::CTokenIterator memberIdentifier)
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
    SubscriptOperator(Type type, bool leftIsPointer, IntrVarPtr<ExpressionBase>&& leftExpr,
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
    BinaryOperator(Type type, IntrVarPtr<ExpressionBase>&& leftOperand, Kind kind, Lexer::CTokenIterator operatorToken,
                   IntrVarPtr<ExpressionBase>&& rightOperand)
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
    UnaryOperator(Type type, ValueCategory valueCategory, Kind kind, Lexer::CTokenIterator operatorToken,
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
        Type type;
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
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>,
                         PrimitiveType::createSizeT(false, false, options), ValueCategory::Rvalue),
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
    Conditional(Type type, IntrVarPtr<ExpressionBase>&& boolExpression, Lexer::CTokenIterator questionMark,
                IntrVarPtr<ExpressionBase>&& trueExpression, Lexer::CTokenIterator colon,
                IntrVarPtr<ExpressionBase>&& falseExpression)
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
    Type m_leftCalcType;

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
    Assignment(Type type, IntrVarPtr<ExpressionBase>&& leftOperand, Type leftCalcType, Kind kind,
               Lexer::CTokenIterator operatorToken, IntrVarPtr<ExpressionBase>&& rightOperand)
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
        return m_leftCalcType;
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
    CommaExpression(Type type,
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
    CallExpression(Type type, IntrVarPtr<ExpressionBase>&& functionExpression, Lexer::CTokenIterator openParentheses,
                   std::vector<IntrVarPtr<ExpressionBase>>&& argumentExpressions,
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
    explicit CompoundLiteral(Type type, Lexer::CTokenIterator openParentheses, Initializer initializer,
                             Lexer::CTokenIterator closeParentheses, Lexer::CTokenIterator initEnd,
                             bool staticLifetime);

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
    BuiltinVAArg(Type type, Lexer::CTokenIterator builtinToken, Lexer::CTokenIterator openParentheses,
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
    std::uint64_t m_offset;
    Lexer::CTokenIterator m_closeParentheses;

public:
    BuiltinOffsetOf(const LanguageOptions& options, Lexer::CTokenIterator builtinToken,
                    Lexer::CTokenIterator openParentheses, std::uint64_t offset, Lexer::CTokenIterator closeParentheses)
        : ExpressionBase(std::in_place_type<std::decay_t<decltype(*this)>>,
                         PrimitiveType::createSizeT(false, false, options), ValueCategory::Rvalue),
          m_builtinToken(builtinToken),
          m_openParentheses(openParentheses),
          m_offset(offset),
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

    [[nodiscard]] std::uint64_t getOffset() const
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
    explicit ErrorExpression(const Syntax::Node& node) : ErrorExpression(Type{}, node) {}

    ErrorExpression(Type type, const Syntax::Node& node);

    ErrorExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end)
        : ExpressionBase(std::in_place_type<ErrorExpression>, Type{}, {}), m_begin(begin), m_end(end)
    {
    }

    ErrorExpression(Type type, Lexer::CTokenIterator begin, Lexer::CTokenIterator end)
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

class Statement : public AbstractIntrusiveVariant<ForStatement, ReturnStatement, ExpressionStatement, IfStatement,
                                                  CompoundStatement, HeadWhileStatement, FootWhileStatement,
                                                  BreakStatement, ContinueStatement, SwitchStatement, DefaultStatement,
                                                  CaseStatement, GotoStatement, LabelStatement, GNUASMStatement>
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

class Useable
    : public AbstractIntrusiveVariant<VariableDeclaration, FunctionDefinition, BuiltinFunction, FunctionDeclaration>
{
    std::uint64_t m_uses{};

protected:
    template <class T>
    Useable(std::in_place_type_t<T>) : AbstractIntrusiveVariant(std::in_place_type<T>)
    {
    }

public:
    [[nodiscard]] std::uint64_t getUses() const noexcept
    {
        return m_uses;
    }

    void setUses(std::uint64_t uses) noexcept
    {
        m_uses = uses;
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
    Type m_type;
    Linkage m_linkage;
    Lexer::CTokenIterator m_nameToken;

protected:
    template <class T>
    Declaration(std::in_place_type_t<T>, Type type, Linkage linkage, Lexer::CTokenIterator nameToken)
        : Useable(std::in_place_type<T>), m_type(std::move(type)), m_linkage(linkage), m_nameToken(nameToken)
    {
    }

public:
    [[nodiscard]] const Type& getType() const
    {
        return m_type;
    }

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
    std::shared_ptr<const Type> type;
    std::size_t layoutIndex;
    std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
};

using FieldMap = tsl::ordered_map<std::string_view, Field, std::hash<std::string_view>, std::equal_to<std::string_view>,
                                  std::allocator<std::pair<std::string_view, Field>>,
                                  std::vector<std::pair<std::string_view, Field>>>;

struct MemoryLayout
{
    Type type;
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
    Type m_type;
    std::vector<std::pair<std::string_view, llvm::APSInt>> m_values;

public:
    EnumDefinition(std::string_view name, Type type, std::vector<std::pair<std::string_view, llvm::APSInt>> values)
        : m_name(name), m_type(std::move(type)), m_values(std::move(values))
    {
    }

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] const Type& getType() const
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

enum class InlineKind : std::uint8_t
{
    InlineDefinition,
    Inline,
    None,
};

class FunctionDeclaration final : public Declaration, public AttributeHolder<FunctionAttribute>
{
    InlineKind m_inlineKind;

public:
    FunctionDeclaration(Type type, Linkage linkage, Lexer::CTokenIterator nameToken, InlineKind inlineKind)
        : Declaration(std::in_place_type<FunctionDeclaration>, std::move(type), linkage, nameToken),
          m_inlineKind(inlineKind)
    {
    }

    [[nodiscard]] bool isInline() const noexcept
    {
        return m_inlineKind != InlineKind::None;
    }

    [[nodiscard]] InlineKind getInlineKind() const noexcept
    {
        return m_inlineKind;
    }
};

enum class Lifetime : std::uint8_t
{
    Automatic,
    Static,
    Register
};

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
    std::vector<VariableAttribute> m_variableAttributes;
    std::optional<Initializer> m_initializer;
    Lifetime m_lifetime;
    Kind m_kind;

public:
    VariableDeclaration(Type type, Linkage linkage, Lifetime lifetime, Lexer::CTokenIterator nameToken, Kind kind,
                        std::optional<Initializer> initializer = {})
        : Declaration(std::in_place_type<VariableDeclaration>, std::move(type), linkage, nameToken),
          m_initializer(std::move(initializer)),
          m_lifetime(lifetime),
          m_kind(kind)
    {
    }

    [[nodiscard]] Lifetime getLifetime() const
    {
        return m_lifetime;
    }

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] const std::optional<Initializer>& getInitializer() const&
    {
        return m_initializer;
    }

    [[nodiscard]] std::optional<Initializer>&& getInitializer() &&
    {
        return std::move(m_initializer);
    }
};

class FunctionDefinition final : public Useable, public AttributeHolder<FunctionAttribute>
{
    Type m_type;
    Lexer::CTokenIterator m_nameToken;
    std::vector<std::unique_ptr<VariableDeclaration>> m_parameterDeclarations;
    Linkage m_linkage;
    InlineKind m_inlineKind;
    CompoundStatement m_compoundStatement;

public:
    FunctionDefinition(Type type, Lexer::CTokenIterator nameToken,
                       std::vector<std::unique_ptr<VariableDeclaration>> parameterDeclarations, Linkage linkage,
                       InlineKind inlineKind, CompoundStatement compoundStatement)
        : Useable(std::in_place_type<FunctionDefinition>),
          m_type(std::move(type)),
          m_nameToken(nameToken),
          m_parameterDeclarations(std::move(parameterDeclarations)),
          m_linkage(linkage),
          m_inlineKind(inlineKind),
          m_compoundStatement(std::move(compoundStatement))
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getNameToken() const
    {
        return m_nameToken;
    }

    [[nodiscard]] const Type& getType() const&
    {
        return m_type;
    }

    [[nodiscard]] Type&& getType() &&
    {
        return std::move(m_type);
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
        return cld::get<FunctionType>(m_type.getVariant()).isKandR();
    }

    [[nodiscard]] Linkage getLinkage() const
    {
        return m_linkage;
    }

    [[nodiscard]] const CompoundStatement& getCompoundStatement() const&
    {
        return m_compoundStatement;
    }

    [[nodiscard]] CompoundStatement&& getCompoundStatement() &&
    {
        return std::move(m_compoundStatement);
    }

    void setCompoundStatement(CompoundStatement&& compoundStatement)
    {
        m_compoundStatement = std::move(compoundStatement);
    }

    [[nodiscard]] bool isInline() const noexcept
    {
        return m_inlineKind != InlineKind::None;
    }

    [[nodiscard]] InlineKind getInlineKind() const noexcept
    {
        return m_inlineKind;
    }
};

class BuiltinFunction final : public Useable
{
    Type m_type;

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
    BuiltinFunction(Type type, Kind kind)
        : Useable(std::in_place_type<BuiltinFunction>), m_type(std::move(type)), m_kind(kind)
    {
    }

    [[nodiscard]] const Type& getType() const noexcept
    {
        return m_type;
    }

    [[nodiscard]] Kind getKind() const noexcept
    {
        return m_kind;
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

[[nodiscard]] bool isArrayType(const Type& type);

[[nodiscard]] bool isAbstractArray(const Type& type);

[[nodiscard]] bool isCharArray(const Type& type, const LanguageOptions& options);

[[nodiscard]] const Type& getArrayElementType(const Type& type);

[[nodiscard]] const Type& getVectorElementType(const Type& type);

[[nodiscard]] const Type& getPointerElementType(const Type& type);

[[nodiscard]] Type adjustParameterType(Type type);

[[nodiscard]] bool isInteger(const Type& type);

[[nodiscard]] bool isArithmetic(const Type& type);

[[nodiscard]] bool isScalar(const Type& type);

[[nodiscard]] bool isPointer(const Type& type);

[[nodiscard]] bool isRecord(const Type& type);

[[nodiscard]] bool isStruct(const Type& type);

[[nodiscard]] bool isUnion(const Type& type);

[[nodiscard]] bool isAnonymous(const Type& type);

[[nodiscard]] bool isEnum(const Type& type);

[[nodiscard]] bool isBool(const Type& type);

[[nodiscard]] bool isCharType(const Type& type);

[[nodiscard]] bool isAggregate(const Type& type);

[[nodiscard]] bool isVector(const Type& type);

[[nodiscard]] bool isFunctionType(const Type& type);

[[nodiscard]] bool isVariablyModified(const Type& type);

[[nodiscard]] bool isVariableLengthArray(const Type& type);

[[nodiscard]] bool isCharacterLikeType(const Type& type, const LanguageOptions& options);

[[nodiscard]] Type removeQualifiers(Type type);

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
        return cld::hashCombine(type.getBitCount(), type.isFloatingPoint(), type.isSigned(), type.getKind());
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
        for (auto& [type, name] : type.getArguments())
        {
            hash = cld::rawHashCombine(hash, std::hash<cld::Semantics::Type>{}(type));
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

} // namespace std

inline bool cld::Semantics::isVoid(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.getVariant());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Void;
}

inline bool cld::Semantics::isArray(const Type& type)
{
    return std::holds_alternative<ArrayType>(type.getVariant())
           || std::holds_alternative<ValArrayType>(type.getVariant())
           || std::holds_alternative<AbstractArrayType>(type.getVariant());
}

inline bool cld::Semantics::isArrayType(const Type& type)
{
    return std::holds_alternative<ArrayType>(type.getVariant());
}

inline bool cld::Semantics::isAbstractArray(const Type& type)
{
    return std::holds_alternative<AbstractArrayType>(type.getVariant());
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
    return cld::match(
        type.getVariant(), [](const auto&) -> const Type& { CLD_UNREACHABLE; },
        [](const ArrayType& arrayType) -> const Type& { return arrayType.getType(); },
        [](const AbstractArrayType& abstractArrayType) -> const Type& { return abstractArrayType.getType(); },
        [](const ValArrayType& arrayType) -> const Type& { return arrayType.getType(); });
}

inline const cld::Semantics::Type& cld::Semantics::getVectorElementType(const Type& type)
{
    return cld::get<VectorType>(type.getVariant()).getType();
}

inline const cld::Semantics::Type& cld::Semantics::getPointerElementType(const Type& type)
{
    return cld::get<PointerType>(type.getVariant()).getElementType();
}

inline bool cld::Semantics::isInteger(const Type& type)
{
    return std::holds_alternative<PrimitiveType>(type.getVariant())
           && !cld::get<PrimitiveType>(type.getVariant()).isFloatingPoint()
           && cld::get<PrimitiveType>(type.getVariant()).getBitCount() != 0;
}

inline bool cld::Semantics::isArithmetic(const Type& type)
{
    return (std::holds_alternative<PrimitiveType>(type.getVariant())
            && cld::get<PrimitiveType>(type.getVariant()).getBitCount() != 0)
           || std::holds_alternative<EnumType>(type.getVariant());
}

inline bool cld::Semantics::isScalar(const Type& type)
{
    return isArithmetic(type) || std::holds_alternative<PointerType>(type.getVariant());
}

inline bool cld::Semantics::isPointer(const cld::Semantics::Type& type)
{
    return std::holds_alternative<PointerType>(type.getVariant());
}

inline bool cld::Semantics::isRecord(const cld::Semantics::Type& type)
{
    return isUnion(type) || isStruct(type);
}

inline bool cld::Semantics::isStruct(const cld::Semantics::Type& type)
{
    return std::holds_alternative<StructType>(type.getVariant());
}

inline bool cld::Semantics::isUnion(const cld::Semantics::Type& type)
{
    return std::holds_alternative<UnionType>(type.getVariant());
}

inline bool cld::Semantics::isAnonymous(const Type& type)
{
    return (std::holds_alternative<EnumType>(type.getVariant()) && cld::get<EnumType>(type.getVariant()).isAnonymous())
           || (std::holds_alternative<StructType>(type.getVariant())
               && cld::get<StructType>(type.getVariant()).isAnonymous())
           || (std::holds_alternative<UnionType>(type.getVariant())
               && cld::get<UnionType>(type.getVariant()).isAnonymous());
}

inline bool cld::Semantics::isEnum(const Type& type)
{
    return std::holds_alternative<EnumType>(type.getVariant());
}

inline bool cld::Semantics::isBool(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.getVariant());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Bool;
}

inline bool cld::Semantics::isCharType(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.getVariant());
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
    return removeQualifiers(type) == PrimitiveType::createWcharT(false, false, options);
}

inline bool cld::Semantics::isAggregate(const Type& type)
{
    return isRecord(type) || isArray(type) || isVector(type);
}

inline bool cld::Semantics::isVector(const Type& type)
{
    return std::holds_alternative<VectorType>(type.getVariant());
}

inline bool cld::Semantics::isFunctionType(const Type& type)
{
    return std::holds_alternative<FunctionType>(type.getVariant());
}

inline cld::Semantics::Type cld::Semantics::removeQualifiers(Type type)
{
    if (type.isConst() || type.isVolatile()
        || (isPointer(type) && cld::get<PointerType>(type.getVariant()).isRestricted()))
    {
        if (!isPointer(type) || !cld::get<PointerType>(type.getVariant()).isRestricted())
        {
            return Type(false, false, std::move(type).getVariant());
        }
        return PointerType::create(false, false, false, getPointerElementType(type));
    }
    return type;
}

#pragma clang diagnostic pop

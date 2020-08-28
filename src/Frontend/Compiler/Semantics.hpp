#pragma once

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

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

class Type;

class StructDefinition;

class UnionDefinition;

class EnumDefinition;

class ProgramInterface;

class PrimitiveType final
{
    std::uint8_t m_bitCount;
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
        Void
    };

private:
    Kind m_kind;

    PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount, Kind kind);

public:
    [[nodiscard]] static cld::Semantics::Type create(bool isConst, bool isVolatile, bool isFloatingPoint, bool isSigned,
                                                     std::uint8_t bitCount, Kind kind);

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

    [[nodiscard]] static Type createLongLong(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createUnsignedLongLong(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createFloat(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createDouble(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createLongDouble(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createVoid(bool isConst, bool isVolatile);

    [[nodiscard]] static Type createPtrdiffT(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createSizeT(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] static Type createWcharT(bool isConst, bool isVolatile, const LanguageOptions& options);

    [[nodiscard]] bool isFloatingPoint() const
    {
        return m_isFloatingPoint;
    }

    [[nodiscard]] bool isSigned() const
    {
        return m_isSigned;
    }

    [[nodiscard]] std::uint8_t getByteCount() const;

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface&) const
    {
        return getByteCount();
    }

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface&) const
    {
        return getByteCount();
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
    std::size_t m_size;
    bool m_restricted : 1;
    bool m_static : 1;

    ArrayType(bool isRestricted, bool isStatic, std::shared_ptr<Type>&& type, std::size_t size);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type type,
                                     std::size_t size);

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] std::size_t getSize() const
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

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

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

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const AbstractArrayType& rhs) const;

    [[nodiscard]] bool operator!=(const AbstractArrayType& rhs) const;
};

class Expression;

class ValArrayType final
{
    std::shared_ptr<const Type> m_type;
    bool m_restricted : 1;
    bool m_static : 1;
    std::shared_ptr<const Expression> m_expression;

    ValArrayType(bool isRestricted, bool isStatic, std::shared_ptr<cld::Semantics::Type>&& type,
                 std::shared_ptr<const Expression>&& expression);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type type,
                                     std::shared_ptr<const Expression> expression);

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
    [[nodiscard]] const std::shared_ptr<const Expression>& getExpression() const
    {
        return m_expression;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

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

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface&) const
    {
        CLD_UNREACHABLE;
    }

    [[nodiscard]] bool operator==(const FunctionType& rhs) const;

    [[nodiscard]] bool operator!=(const FunctionType& rhs) const;
};

class StructType final
{
    std::string_view m_name;
    std::uint64_t m_scopeOrId;

    StructType(std::string_view name, std::int64_t scope);

public:
    [[nodiscard]] static Type create(bool isConst, bool isVolatile, std::string_view name, std::int64_t scope);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] std::uint64_t getScopeOrId() const
    {
        return m_scopeOrId;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const StructType&) const
    {
        return false;
    }

    [[nodiscard]] bool operator!=(const StructType& rhs) const
    {
        return !(rhs == *this);
    }
};

class UnionType final
{
    std::string_view m_name;
    std::uint64_t m_scopeOrId;

    UnionType(std::string_view name, std::uint64_t scopeOrId);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, std::uint64_t scopeOrId);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] std::uint64_t getScopeOrId() const
    {
        return m_scopeOrId;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const UnionType&) const
    {
        return false;
    }

    [[nodiscard]] bool operator!=(const UnionType& rhs) const
    {
        return !(rhs == *this);
    }
};

struct Field
{
    std::shared_ptr<const Type> type;
    std::string_view name;
    std::optional<std::uint8_t> bitFieldSize;

    [[nodiscard]] bool operator==(const Field& rhs) const;

    [[nodiscard]] bool operator!=(const Field& rhs) const;
};

class AnonymousStructType final
{
    std::uint64_t m_id;
    std::vector<Field> m_fields;
    std::uint32_t m_sizeOf;
    std::uint32_t m_alignOf;

    AnonymousStructType(std::uint64_t id, std::vector<Field>&& fields, std::uint32_t sizeOf, std::uint32_t alignOf)
        : m_id(id), m_fields(std::move(fields)), m_sizeOf(sizeOf), m_alignOf(alignOf)
    {
    }

public:
    static Type create(bool isConst, bool isVolatile, std::uint64_t id, std::vector<Field> fields, std::uint32_t sizeOf,
                       std::uint32_t alignOf);

    [[nodiscard]] std::uint64_t getId() const
    {
        return m_id;
    }

    [[nodiscard]] const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const AnonymousStructType& rhs) const;

    [[nodiscard]] bool operator!=(const AnonymousStructType& rhs) const;
};

class AnonymousUnionType final
{
    std::uint64_t m_id;
    std::vector<Field> m_fields;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

    AnonymousUnionType(std::uint64_t id, std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf);

public:
    static Type create(bool isConst, bool isVolatile, std::uint64_t id, std::vector<Field> fields, std::uint64_t sizeOf,
                       std::uint64_t alignOf);

    [[nodiscard]] std::uint64_t getId() const
    {
        return m_id;
    }

    [[nodiscard]] const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const AnonymousUnionType& rhs) const;

    [[nodiscard]] bool operator!=(const AnonymousUnionType& rhs) const;
};

class EnumType final
{
    std::string_view m_name;
    std::uint64_t m_scopeOrId;

    EnumType(std::string_view name, std::uint64_t scopeOrId);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, std::uint64_t scopeOrId);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    [[nodiscard]] std::uint64_t getScopeOrId() const
    {
        return m_scopeOrId;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const EnumType&) const
    {
        return false;
    }

    [[nodiscard]] bool operator!=(const EnumType& rhs) const
    {
        return !(rhs == *this);
    }
};

class AnonymousEnumType
{
    std::shared_ptr<const Type> m_type;
    std::uint64_t m_id;

    AnonymousEnumType(std::uint64_t id, std::shared_ptr<const Type> type);

public:
    static Type create(bool isConst, bool isVolatile, std::uint64_t id, Type type);

    [[nodiscard]] std::uint64_t getId() const
    {
        return m_id;
    }

    [[nodiscard]] bool operator==(const AnonymousEnumType& rhs) const;

    [[nodiscard]] bool operator!=(const AnonymousEnumType& rhs) const;

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;
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

    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;

    [[nodiscard]] bool operator==(const PointerType& rhs) const;

    [[nodiscard]] bool operator!=(const PointerType& rhs) const;
};

class Type final
{
public:
    using Variant = std::variant<std::monostate, PrimitiveType, ArrayType, AbstractArrayType, ValArrayType,
                                 FunctionType, StructType, UnionType, EnumType, PointerType, AnonymousEnumType,
                                 AnonymousStructType, AnonymousUnionType>;

private:
    Variant m_type;
    std::string m_name;
    bool m_isConst : 1;
    bool m_isVolatile : 1;

public:
    explicit Type(bool isConst = false, bool isVolatile = false, Variant type = std::monostate{})
        : m_type(std::move(type)), m_isConst(isConst), m_isVolatile(isVolatile)
    {
    }

    [[nodiscard]] const Variant& get() const&
    {
        return m_type;
    }

    [[nodiscard]] Variant&& get() &&
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
    [[nodiscard]] std::size_t getSizeOf(const ProgramInterface& program) const;

    [[nodiscard]] std::size_t getAlignOf(const ProgramInterface& program) const;
};

class Constant final
{
public:
    using Variant = std::variant<llvm::APSInt, llvm::APFloat, std::string, Lexer::NonCharString>;

private:
    Variant m_value;
    Lexer::CTokenIterator m_valueBegin;
    Lexer::CTokenIterator m_valueEnd;

public:
    Constant(Variant value, Lexer::CTokenIterator valueBegin, Lexer::CTokenIterator valueEnd)
        : m_value(std::move(value)), m_valueBegin(valueBegin), m_valueEnd(valueEnd)
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

class DeclarationRead final
{
public:
    using Variant = std::variant<const Declaration * CLD_NON_NULL, const FunctionDefinition * CLD_NON_NULL>;

private:
    Variant m_declRead;
    Lexer::CTokenIterator m_identifierToken;

public:
    explicit DeclarationRead(Variant declRead, Lexer::CTokenIterator identifierToken)
        : m_declRead(declRead), m_identifierToken(identifierToken)
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getIdentifierToken() const
    {
        return m_identifierToken;
    }

    [[nodiscard]] Variant getDeclRead() const
    {
        return m_declRead;
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

class Conversion final
{
public:
    enum Kind
    {
        LValue,
        IntegerPromotion,
        ArithmeticConversion,
        DefaultArgumentPromotion,
    };

private:
    Kind m_kind;
    std::unique_ptr<Expression> m_expression;

public:
    Conversion(Kind kind, std::unique_ptr<Expression> expression) : m_kind(kind), m_expression(std::move(expression)) {}

    [[nodiscard]] Kind getKind() const
    {
        return m_kind;
    }

    [[nodiscard]] const Expression& getExpression() const
    {
        return *m_expression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class Cast final
{
    Lexer::CTokenIterator m_openParentheses;
    Type m_newType;
    Lexer::CTokenIterator m_closeParentheses;
    std::unique_ptr<Expression> m_expression;

public:
    Cast(Lexer::CTokenIterator openParentheses, Type newType, Lexer::CTokenIterator closeParentheses,
         std::unique_ptr<Expression> expression)
        : m_openParentheses(openParentheses),
          m_newType(std::move(newType)),
          m_closeParentheses(closeParentheses),
          m_expression(std::move(expression))
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] const Type& getNewType() const
    {
        return m_newType;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const
    {
        return m_closeParentheses;
    }

    [[nodiscard]] const Expression& getExpression() const
    {
        return *m_expression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class MemberAccess final
{
    std::unique_ptr<Expression> m_recordExpr;
    std::uint64_t m_memberIndex;
    Lexer::CTokenIterator m_memberIdentifier;

public:
    MemberAccess(std::unique_ptr<Expression> recordExpr, std::uint64_t memberIndex,
                 Lexer::CTokenIterator memberIdentifier)
        : m_recordExpr(std::move(recordExpr)), m_memberIndex(memberIndex), m_memberIdentifier(memberIdentifier)
    {
    }

    [[nodiscard]] const Expression& getRecordExpression() const
    {
        return *m_recordExpr;
    }

    [[nodiscard]] std::uint64_t getMemberIndex() const
    {
        return m_memberIndex;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return m_memberIdentifier + 1;
    }
};

class SubscriptOperator final
{
    std::unique_ptr<Expression> m_leftExpr;
    Lexer::CTokenIterator m_openBracket;
    std::unique_ptr<Expression> m_rightExpr;
    Lexer::CTokenIterator m_closeBracket;

public:
    SubscriptOperator(std::unique_ptr<Expression> leftExpr, Lexer::CTokenIterator openBracket,
                      std::unique_ptr<Expression> rightExpr, Lexer::CTokenIterator closeBracket)
        : m_leftExpr(std::move(leftExpr)),
          m_openBracket(openBracket),
          m_rightExpr(std::move(rightExpr)),
          m_closeBracket(closeBracket)
    {
    }

    [[nodiscard]] const Expression& getLeftExpression() const
    {
        return *m_leftExpr;
    }

    [[nodiscard]] const Expression& getRightExpression() const
    {
        return *m_rightExpr;
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

class BinaryOperator final
{
    std::unique_ptr<Expression> m_leftOperand;

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
    std::unique_ptr<Expression> m_rightOperand;

public:
    BinaryOperator(std::unique_ptr<Expression> leftOperand, Kind kind, Lexer::CTokenIterator operatorToken,
                   std::unique_ptr<Expression> rightOperand)
        : m_leftOperand(std::move(leftOperand)),
          m_kind(kind),
          m_operatorToken(operatorToken),
          m_rightOperand(std::move(rightOperand))
    {
    }

    [[nodiscard]] const Expression& getLeftExpression() const
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

    [[nodiscard]] const Expression& getRightExpression() const
    {
        return *m_rightOperand;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class UnaryOperator final
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
    std::unique_ptr<Expression> m_operand;

public:
    UnaryOperator(Kind kind, Lexer::CTokenIterator operatorToken, std::unique_ptr<Expression> operand)
        : m_kind(kind), m_operatorToken(operatorToken), m_operand(std::move(operand))
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

    [[nodiscard]] const Expression& getOperand() const
    {
        return *m_operand;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class SizeofOperator final
{
public:
    struct TypeVariant
    {
        Lexer::CTokenIterator openParentheses;
        Type type;
        Lexer::CTokenIterator closeParentheses;
    };

    using Variant = std::variant<std::unique_ptr<Expression>, TypeVariant>;

private:
    Lexer::CTokenIterator m_sizeOfToken;
    std::optional<std::uint64_t> m_size;
    Variant m_variant;

public:
    explicit SizeofOperator(Lexer::CTokenIterator sizeOfToken, std::optional<std::uint64_t> size, Variant variant)
        : m_sizeOfToken(sizeOfToken), m_size(size), m_variant(std::move(variant))
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

class Conditional final
{
    std::unique_ptr<Expression> m_boolExpression;
    Lexer::CTokenIterator m_questionMark;
    std::unique_ptr<Expression> m_trueExpression;
    Lexer::CTokenIterator m_colon;
    std::unique_ptr<Expression> m_falseExpression;

public:
    Conditional(std::unique_ptr<Expression> boolExpression, Lexer::CTokenIterator questionMark,
                std::unique_ptr<Expression> trueExpression, Lexer::CTokenIterator colon,
                std::unique_ptr<Expression> falseExpression)
        : m_boolExpression(std::move(boolExpression)),
          m_questionMark(questionMark),
          m_trueExpression(std::move(trueExpression)),
          m_colon(colon),
          m_falseExpression(std::move(falseExpression))
    {
    }

    [[nodiscard]] const Expression& getBoolExpression() const
    {
        return *m_boolExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator getQuestionMark() const
    {
        return m_questionMark;
    }

    [[nodiscard]] const Expression& getTrueExpression() const
    {
        return *m_trueExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator getColon() const
    {
        return m_colon;
    }

    [[nodiscard]] const Expression& getFalseExpression() const
    {
        return *m_falseExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class Assignment final
{
    std::unique_ptr<Expression> m_leftOperand;

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
    std::unique_ptr<Expression> m_rightOperand;

public:
    Assignment(std::unique_ptr<Expression> leftOperand, Kind kind, Lexer::CTokenIterator operatorToken,
               std::unique_ptr<Expression> rightOperand)
        : m_leftOperand(std::move(leftOperand)),
          m_kind(kind),
          m_operatorToken(operatorToken),
          m_rightOperand(std::move(rightOperand))
    {
    }

    [[nodiscard]] const Expression& getLeftExpression() const
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

    [[nodiscard]] const Expression& getRightExpression() const
    {
        return *m_rightOperand;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class CommaExpression final
{
    std::vector<std::pair<Expression, Lexer::CTokenIterator>> m_commaExpressions;
    std::unique_ptr<Expression> m_lastExpression;

public:
    CommaExpression(std::vector<std::pair<Expression, Lexer::CTokenIterator>>&& commaExpressions,
                    std::unique_ptr<Expression>&& lastExpression)
        : m_commaExpressions(std::move(commaExpressions)), m_lastExpression(std::move(lastExpression))
    {
        CLD_ASSERT(m_commaExpressions.size() >= 1);
    }

    [[nodiscard]] const std::vector<std::pair<Expression, Lexer::CTokenIterator>>& getCommaExpressions() const
    {
        return m_commaExpressions;
    }

    [[nodiscard]] const Expression& getLastExpression() const
    {
        return *m_lastExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

class CallExpression final
{
    std::unique_ptr<Expression> m_functionExpression;
    Lexer::CTokenIterator m_openParentheses;
    std::vector<Expression> m_argumentExpressions;
    Lexer::CTokenIterator m_closeParentheses;

public:
    CallExpression(std::unique_ptr<Expression>&& functionExpression, Lexer::CTokenIterator openParentheses,
                   std::vector<Expression>&& argumentExpressions, Lexer::CTokenIterator closeParentheses)
        : m_functionExpression(std::move(functionExpression)),
          m_openParentheses(openParentheses),
          m_argumentExpressions(std::move(argumentExpressions)),
          m_closeParentheses(closeParentheses)
    {
    }

    [[nodiscard]] const Expression& getFunctionExpression() const
    {
        return *m_functionExpression;
    }

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const
    {
        return m_openParentheses;
    }

    [[nodiscard]] const std::vector<Expression>& getArgumentExpressions() const
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

using Initializer = std::variant<InitializerList, Expression>;

class CompoundLiteral final
{
    Lexer::CTokenIterator m_openParentheses;
    std::unique_ptr<Initializer> m_initializer;
    Lexer::CTokenIterator m_closeParentheses;
    Lexer::CTokenIterator m_initEnd;

public:
    explicit CompoundLiteral(Lexer::CTokenIterator openParentheses, Initializer initializer,
                             Lexer::CTokenIterator closeParentheses, Lexer::CTokenIterator initEnd);

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const Initializer& getInitializer() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

enum class ValueCategory : std::uint8_t
{
    Lvalue,
    Rvalue
};

class Expression final
{
    Type m_type;
    ValueCategory m_valueCategory;

public:
    using Variant =
        std::variant<std::pair<Lexer::CTokenIterator, Lexer::CTokenIterator>, Constant, DeclarationRead, Conversion,
                     MemberAccess, BinaryOperator, Cast, UnaryOperator, SizeofOperator, SubscriptOperator, Conditional,
                     Assignment, CommaExpression, CallExpression, CompoundLiteral>;

private:
    Variant m_expression;

public:
    explicit Expression(const Syntax::Node& node);

    Expression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end) : m_expression(std::pair{begin, end}) {}

    Expression(Type type, ValueCategory valueCategory, Variant expression)
        : m_type(std::move(type)), m_valueCategory(valueCategory), m_expression(std::move(expression))
    {
    }

    Expression(const Expression&) = delete;
    Expression& operator=(const Expression&) = delete;

    // Currently MSVC deletes the move constructor and assign operator if I mark them noexcept
    // This is due to llvm::APSInt not being noexcept. Other compilers will believe me that Expression is noexcept
    // if I mark it as such but Microsoft instead punishes me by deleting it.

    Expression(Expression&&)
#if !defined(_MSC_VER) || defined(__clang__)
        noexcept
#endif
        = default;
    Expression& operator=(Expression&&)
#if !defined(_MSC_VER) || defined(__clang__)
        noexcept
#endif
        = default;

    [[nodiscard]] const Variant& get() const
    {
        return m_expression;
    }

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
        return std::holds_alternative<std::pair<Lexer::CTokenIterator, Lexer::CTokenIterator>>(m_expression);
    }

    [[nodiscard]] Lexer::CTokenIterator begin() const
    {
        return cld::match(
            m_expression, [](std::pair<Lexer::CTokenIterator, Lexer::CTokenIterator> pair) { return pair.first; },
            [](const auto& value) { return value.begin(); });
    }

    [[nodiscard]] Lexer::CTokenIterator end() const
    {
        return cld::match(
            m_expression, [](std::pair<Lexer::CTokenIterator, Lexer::CTokenIterator> pair) { return pair.second; },
            [](const auto& value) { return value.end(); });
    }
};

bool isStringLiteralExpr(const Expression& expression);

class ReturnStatement final
{
    std::optional<Expression> m_expression;

public:
    explicit ReturnStatement(std::optional<Expression>&& expression) : m_expression(std::move(expression)) {}

    const std::optional<Expression>& getExpression() const
    {
        return m_expression;
    }
};

class ExpressionStatement final
{
    std::optional<Expression> m_expression;

public:
    explicit ExpressionStatement(std::optional<Expression>&& expression) : m_expression(std::move(expression)) {}

    const std::optional<Expression>& getExpression() const
    {
        return m_expression;
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

class GotoStatement final
{
    const LabelStatement* m_label;

public:
    explicit GotoStatement(const LabelStatement* label) : m_label(label) {}

    const LabelStatement* getLabel() const
    {
        return m_label;
    }
};

using BreakableStatements = std::variant<const ForStatement*, const FootWhileStatement * CLD_NON_NULL,
                                         const HeadWhileStatement * CLD_NON_NULL, const SwitchStatement * CLD_NON_NULL>;

class BreakStatement final
{
    BreakableStatements m_statement;

public:
    explicit BreakStatement(BreakableStatements statements) : m_statement(statements) {}

    [[nodiscard]] const BreakableStatements& getBreakableStatement() const
    {
        return m_statement;
    }
};

using LoopStatements =
    std::variant<const ForStatement*, const FootWhileStatement * CLD_NON_NULL, const HeadWhileStatement * CLD_NON_NULL>;

class ContinueStatement final
{
    LoopStatements m_loopStatement;

public:
    explicit ContinueStatement(LoopStatements loopStatement) : m_loopStatement(loopStatement) {}

    [[nodiscard]] const LoopStatements& getLoopStatement() const
    {
        return m_loopStatement;
    }
};

using Statement =
    std::variant<std::unique_ptr<ForStatement>, ReturnStatement, ExpressionStatement, IfStatement, CompoundStatement,
                 std::unique_ptr<HeadWhileStatement>, std::unique_ptr<FootWhileStatement>, BreakStatement,
                 ContinueStatement, std::unique_ptr<SwitchStatement>, std::unique_ptr<DefaultStatement>,
                 std::unique_ptr<CaseStatement>, std::unique_ptr<GotoStatement>, std::unique_ptr<LabelStatement>>;

class IfStatement final
{
    Expression m_expression;
    std::unique_ptr<Statement> m_trueBranch;
    std::unique_ptr<Statement> m_falseBranch;

public:
    IfStatement(Expression&& expression, Statement&& trueBranch, std::unique_ptr<Statement>&& falseBranch);

    [[nodiscard]] const Expression& getExpression() const
    {
        return m_expression;
    }

    [[nodiscard]] const Statement& getTrueBranch() const;

    [[nodiscard]] const Statement* getFalseBranch() const
    {
        return m_falseBranch.get();
    }
};

class CompoundStatement final
{
public:
    using Variant = std::variant<Statement, std::unique_ptr<Declaration>, std::shared_ptr<const Expression>>;

private:
    std::vector<Variant> m_compoundItems;

public:
    explicit CompoundStatement(std::vector<Variant>&& compoundItems) : m_compoundItems(std::move(compoundItems)) {}

    CompoundStatement(const CompoundStatement&) = delete;
    CompoundStatement& operator=(const CompoundStatement&) = delete;

    CompoundStatement(CompoundStatement&&) noexcept = default;
    CompoundStatement& operator=(CompoundStatement&&) noexcept = default;

    [[nodiscard]] const std::vector<Variant>& getCompoundItems() const
    {
        return m_compoundItems;
    }
};

class ForStatement final
{
public:
    using Variant = std::variant<std::monostate, Expression, std::vector<std::unique_ptr<Declaration>>>;

private:
    Variant m_initial;
    std::optional<Expression> m_controlling;
    std::optional<Expression> m_iteration;
    std::unique_ptr<Statement> m_statement;

public:
    ForStatement(Variant initial, std::optional<Expression> controlling, std::optional<Expression> iteration,
                 Statement&& statement);

    [[nodiscard]] const Variant& getInitial() const
    {
        return m_initial;
    }

    [[nodiscard]] const std::optional<Expression>& getControlling() const
    {
        return m_controlling;
    }

    [[nodiscard]] const std::optional<Expression>& getIteration() const
    {
        return m_iteration;
    }

    [[nodiscard]] const Statement& getStatement() const;
};

class HeadWhileStatement final
{
    Expression m_expression;
    std::unique_ptr<Statement> m_statement;

public:
    HeadWhileStatement(Expression&& expression, Statement&& statement);

    [[nodiscard]] const Expression& getExpression() const
    {
        return m_expression;
    }

    [[nodiscard]] const Statement& getStatement() const;
};

class FootWhileStatement final
{
    std::unique_ptr<Statement> m_statement;
    Expression m_expression;

public:
    FootWhileStatement(Statement&& statement, Expression&& expression);

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const Expression& getExpression() const
    {
        return m_expression;
    }
};

class SwitchStatement final
{
    Expression m_expression;
    std::unique_ptr<Statement> m_statement;
    std::int64_t m_scope;
    std::map<llvm::APSInt, const CaseStatement * CLD_NON_NULL> m_cases;
    const DefaultStatement* CLD_NULLABLE m_default;

public:
    SwitchStatement(Expression&& expression, Statement&& statement, std::int64_t scope,
                    std::map<llvm::APSInt, const CaseStatement* CLD_NON_NULL> cases = {},
                    const DefaultStatement* CLD_NULLABLE defaultStmt = nullptr);

    [[nodiscard]] const Expression& getExpression() const&
    {
        return m_expression;
    }

    [[nodiscard]] Expression&& getExpression() &&
    {
        return std::move(m_expression);
    }

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] std::int64_t getScope() const
    {
        return m_scope;
    }

    [[nodiscard]] const std::map<llvm::APSInt, const CaseStatement * CLD_NON_NULL>& getCases() const
    {
        return m_cases;
    }

    [[nodiscard]] const DefaultStatement* getDefaultStatement() const
    {
        return m_default;
    }
};

class DefaultStatement final
{
    Lexer::CTokenIterator m_defaultToken;
    Lexer::CTokenIterator m_colonToken;
    std::unique_ptr<Statement> m_statement;
    const SwitchStatement* CLD_NON_NULL m_switchStmt;

public:
    DefaultStatement(Lexer::CTokenIterator defaultToken, Lexer::CTokenIterator colonToken, Statement&& statement,
                     const SwitchStatement& switchStmt);

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

class CaseStatement final
{
    Lexer::CTokenIterator m_caseToken;
    llvm::APSInt m_constant;
    Lexer::CTokenIterator m_colonToken;
    std::unique_ptr<Statement> m_statement;
    const SwitchStatement* CLD_NON_NULL m_switchStmt;

public:
    CaseStatement(Lexer::CTokenIterator caseToken, llvm::APSInt constant, Lexer::CTokenIterator colonToken,
                  Statement&& statement, const SwitchStatement& switchStmt);

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

class LabelStatement final
{
    Lexer::CTokenIterator m_identifier;
    std::int64_t m_scope;
    std::size_t m_sizeOfCurrentScope;
    std::unique_ptr<Statement> m_statement;

public:
    LabelStatement(Lexer::CTokenIterator identifier, std::int64_t scope, std::size_t sizeOfCurrentScope,
                   Statement&& statement);

    [[nodiscard]] Lexer::CTokenIterator getIdentifier() const
    {
        return m_identifier;
    }

    [[nodiscard]] std::int64_t getScope() const
    {
        return m_scope;
    }

    [[nodiscard]] std::size_t getSizeOfCurrentScope() const
    {
        return m_sizeOfCurrentScope;
    }

    [[nodiscard]] const Statement& getStatement() const;
};

class StructDefinition
{
    std::string_view m_name;
    std::vector<Field> m_fields;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

public:
    StructDefinition(std::string_view name, std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf)
        : m_name(name), m_fields(std::move(fields)), m_sizeOf(sizeOf), m_alignOf(alignOf)
    {
    }

    std::string_view getName() const
    {
        return m_name;
    }

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    std::uint64_t getSizeOf() const
    {
        return m_sizeOf;
    }

    std::uint64_t getAlignOf() const
    {
        return m_alignOf;
    }

    bool operator==(const StructDefinition& rhs) const;

    bool operator!=(const StructDefinition& rhs) const;
};

class UnionDefinition
{
    std::string_view m_name;
    std::vector<Field> m_fields;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

public:
    UnionDefinition(std::string_view name, std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf)
        : m_name(name), m_fields(std::move(fields)), m_sizeOf(sizeOf), m_alignOf(alignOf)
    {
    }

    std::string_view getName() const
    {
        return m_name;
    }

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    std::uint64_t getSizeOf() const
    {
        return m_sizeOf;
    }

    std::uint64_t getAlignOf() const
    {
        return m_alignOf;
    }

    bool operator==(const UnionDefinition& rhs) const;

    bool operator!=(const UnionDefinition& rhs) const;
};

class EnumDefinition
{
    std::string_view m_name;
    Type m_type;

public:
    EnumDefinition(std::string_view name, Type type) : m_name(name), m_type(std::move(type)) {}

    std::string_view getName() const
    {
        return m_name;
    }

    const Type& getType() const
    {
        return m_type;
    }
};

class InitializerList final
{
public:
    struct Initialization
    {
        std::vector<std::size_t> path;
        Expression expression;
    };

private:
    std::vector<Initialization> m_fields;

public:
    explicit InitializerList(std::vector<Initialization> fields) : m_fields(std::move(fields)) {}

    [[nodiscard]] const std::vector<Initialization>& getFields() const&
    {
        return m_fields;
    }

    [[nodiscard]] std::vector<Initialization>&& getFields() &&
    {
        return std::move(m_fields);
    }
};

enum class Linkage : std::uint8_t
{
    Internal,
    External,
    None
};

enum class Lifetime : std::uint8_t
{
    Automatic,
    Static,
    Register
};

class Declaration final
{
    Type m_type;
    Linkage m_linkage;
    Lifetime m_lifetime;
    Lexer::CTokenIterator m_nameToken;
    std::optional<Initializer> m_initializer;

public:
    Declaration(Type type, Linkage linkage, Lifetime lifetime, Lexer::CTokenIterator nameToken,
                std::optional<Initializer> initializer = {})
        : m_type(std::move(type)),
          m_linkage(linkage),
          m_lifetime(lifetime),
          m_nameToken(nameToken),
          m_initializer(std::move(initializer))
    {
    }

    [[nodiscard]] const Type& getType() const
    {
        return m_type;
    }

    [[nodiscard]] Linkage getLinkage() const
    {
        return m_linkage;
    }

    [[nodiscard]] Lifetime getLifetime() const
    {
        return m_lifetime;
    }

    [[nodiscard]] Lexer::CTokenIterator getNameToken() const
    {
        return m_nameToken;
    }

    [[nodiscard]] const std::optional<Initializer>& getInitializer() const
    {
        return m_initializer;
    }
};

class FunctionDefinition final
{
    Type m_type;
    Lexer::CTokenIterator m_nameToken;
    std::vector<std::unique_ptr<Declaration>> m_parameterDeclarations;
    Linkage m_linkage;
    CompoundStatement m_compoundStatement;

public:
    FunctionDefinition(Type type, Lexer::CTokenIterator nameToken,
                       std::vector<std::unique_ptr<Declaration>> parameterDeclarations, Linkage linkage,
                       CompoundStatement compoundStatement)
        : m_type(std::move(type)),
          m_nameToken(nameToken),
          m_parameterDeclarations(std::move(parameterDeclarations)),
          m_linkage(linkage),
          m_compoundStatement(std::move(compoundStatement))
    {
    }

    [[nodiscard]] Lexer::CTokenIterator getNameToken() const
    {
        return m_nameToken;
    }

    [[nodiscard]] const Type& getType() const
    {
        return m_type;
    }

    [[nodiscard]] const std::vector<std::unique_ptr<Declaration>>& getParameterDeclarations() const
    {
        return m_parameterDeclarations;
    }

    [[nodiscard]] bool isKandR() const
    {
        return cld::get<FunctionType>(m_type.get()).isKandR();
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
};

class TranslationUnit final
{
public:
    using Variant = std::variant<std::unique_ptr<FunctionDefinition>, std::unique_ptr<Declaration>>;

private:
    std::vector<Variant> m_globals;

public:
    explicit TranslationUnit(std::vector<Variant> globals);

    [[nodiscard]] const std::vector<Variant>& getGlobals() const
    {
        return m_globals;
    }
};

class Program;

Program analyse(const Syntax::TranslationUnit& parseTree, CSourceObject&& ctokens,
                llvm::raw_ostream* reporter = &llvm::errs(), bool* errors = nullptr);

Lexer::CTokenIterator declaratorToLoc(const cld::Syntax::Declarator& declarator);

bool isVoid(const Type& type);

bool isArray(const Type& type);

const Type& getArrayElementType(const Type& type);

bool isInteger(const Type& type);

bool isArithmetic(const Type& type);

bool isScalar(const Type& type);

bool isRecord(const Type& type);

bool isBool(const Type& type);

bool isCharType(const Type& type);

bool isAggregate(const Type& type);

} // namespace cld::Semantics

namespace cld::diag
{
template <>
struct StringConverter<Semantics::Type>
{
    static std::string inFormat(const Semantics::Type& arg, const SourceInterface& sourceInterface)
    {
        return "'" + inArg(arg, sourceInterface) + "'";
    }

    static std::string inArg(const Semantics::Type& arg, const SourceInterface&);
};

template <>
struct CustomFormat<U'f', U'u', U'l', U'l'>
{
    std::string operator()(const Semantics::Type& arg);
};

template <>
struct CustomFormat<U't', U'y', U'p', U'e'>
{
    std::string operator()(const Semantics::Expression& arg);
};

template <>
struct CustomFormat<U'f', U'u', U'l', U'l', U'T', U'y', U'p', U'e'>
{
    std::string operator()(const Semantics::Expression& arg);
};

} // namespace cld::diag

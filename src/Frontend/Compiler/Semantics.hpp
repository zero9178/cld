#pragma once

#include <Frontend/Common/Expected.hpp>

#include <memory>
#include <string>
#include <vector>

#include "Syntax.hpp"

namespace cld
{
class Message;
} // namespace cld

namespace cld::Semantics
{
class Type;

class CompoundStatement;

class LabelStatement;

class CaseStatement;

class DefaultStatement;

class IfStatement;

class SwitchStatement;

class ForStatement;

class HeadWhileStatement;

class FootWhileStatement;

class GotoStatement;

class ContinueStatement;

class BreakStatement;

class ReturnStatement;

class ExpressionStatement;

using Statement = std::variant<ReturnStatement, ExpressionStatement, IfStatement, CompoundStatement, ForStatement,
                               HeadWhileStatement, FootWhileStatement, BreakStatement, ContinueStatement,
                               SwitchStatement, DefaultStatement, CaseStatement, GotoStatement, LabelStatement>;

class PrimitiveType final
{
    bool m_isFloatingPoint;
    bool m_isSigned;
    std::uint8_t m_bitCount;

public:
    enum class Kind
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
    static Type create(bool isConst, bool isVolatile, bool isFloatingPoint, bool isSigned, std::uint8_t bitCount,
                       std::string name, Kind kind);

    static Type createChar(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createSignedChar(bool isConst, bool isVolatile);

    static Type createUnderlineBool(bool isConst, bool isVolatile);

    static Type createUnsignedChar(bool isConst, bool isVolatile);

    static Type createShort(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createUnsignedShort(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createInt(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createUnsignedInt(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createLong(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createUnsignedLong(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createLongLong(bool isConst, bool isVolatile);

    static Type createUnsignedLongLong(bool isConst, bool isVolatile);

    static Type createFloat(bool isConst, bool isVolatile);

    static Type createDouble(bool isConst, bool isVolatile);

    static Type createLongDouble(bool isConst, bool isVolatile, const LanguageOptions& options);

    static Type createVoid(bool isConst, bool isVolatile);

    [[nodiscard]] bool isFloatingPoint() const;

    [[nodiscard]] bool isSigned() const;

    [[nodiscard]] std::uint8_t getByteCount() const;

    [[nodiscard]] std::uint8_t getBitCount() const;

    Kind getKind() const;

    bool operator==(const PrimitiveType& rhs) const;

    bool operator!=(const PrimitiveType& rhs) const;
};

class ArrayType final
{
    bool m_restricted;
    bool m_static;
    std::shared_ptr<const Type> m_type;
    std::size_t m_size;

    ArrayType(bool isRestricted,bool isStatic, std::shared_ptr<Type>&& type, std::size_t size);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type&& type, std::size_t size);

    [[nodiscard]] const Type& getType() const;

    [[nodiscard]] std::size_t getSize() const;

    [[nodiscard]] bool isRestricted() const;

    [[nodiscard]] bool isStatic() const;

    bool operator==(const ArrayType& rhs) const;

    bool operator!=(const ArrayType& rhs) const;
};

class AbstractArrayType final
{
    bool m_restricted;
    std::shared_ptr<const Type> m_type;

    AbstractArrayType(bool isRestricted, std::shared_ptr<Type>&& type);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

    [[nodiscard]] const Type& getType() const;

    [[nodiscard]] bool isRestricted() const;

    bool operator==(const AbstractArrayType& rhs) const;

    bool operator!=(const AbstractArrayType& rhs) const;
};

class ValArrayType final
{
    bool m_restricted;
    bool m_static;
    std::shared_ptr<const Type> m_type;

    ValArrayType(bool isRestricted,bool isStatic, std::shared_ptr<cld::Semantics::Type>&& type);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted,bool isStatic, Type&& type);

    [[nodiscard]] const Type& getType() const;

    [[nodiscard]] bool isRestricted() const;

    [[nodiscard]] bool isStatic() const;

    bool operator==(const ValArrayType& rhs) const;

    bool operator!=(const ValArrayType& rhs) const;
};

class FunctionType final
{
    std::shared_ptr<const Type> m_returnType;
    std::vector<std::pair<Type, std::string>> m_arguments;
    bool m_lastIsVararg;
    bool m_isKandR;

    FunctionType(std::shared_ptr<Type>&& returnType, std::vector<std::pair<Type, std::string>> arguments,
                 bool lastIsVararg, bool isKandR);

public:
    static Type create(cld::Semantics::Type&& returnType, std::vector<std::pair<Type, std::string>>&& arguments,
                       bool lastIsVararg, bool isKandR);

    [[nodiscard]] const Type& getReturnType() const;

    [[nodiscard]] const std::vector<std::pair<Type, std::string>>& getArguments() const;

    [[nodiscard]] bool isLastVararg() const;

    [[nodiscard]] bool isKandR() const;

    bool operator==(const FunctionType& rhs) const;

    bool operator!=(const FunctionType& rhs) const;
};

class StructType final
{
    std::string m_name;
    std::uint64_t m_scope;

    StructType(std::string_view name, std::uint64_t scope);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, std::uint64_t scope);

    [[nodiscard]] std::string_view getName() const;

    std::uint64_t getScope() const;

    bool operator==(const StructType& rhs) const;

    bool operator!=(const StructType& rhs) const;
};

class UnionType final
{
    std::string m_name;
    std::uint64_t m_scope;

    UnionType(std::string_view name, std::uint64_t scope);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, std::uint64_t scope);

    [[nodiscard]] std::string_view getName() const;

    std::uint64_t getScope() const;

    bool operator==(const UnionType& rhs) const;

    bool operator!=(const UnionType& rhs) const;
};

struct Field
{
    std::shared_ptr<const Type> type;
    std::string name;
    std::optional<std::uint64_t> bitFieldSize;

    bool operator==(const Field& rhs) const;

    bool operator!=(const Field& rhs) const;
};

class AnonymousStructType final
{
    std::vector<Field> m_fields;

    AnonymousStructType(std::vector<Field>&& fields);

public:
    static Type create(bool isConst, bool isVolatile, std::vector<Field> fields);

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    bool operator==(const AnonymousStructType& rhs) const;

    bool operator!=(const AnonymousStructType& rhs) const;
};

class AnonymousUnionType final
{
    std::vector<Field> m_fields;

    AnonymousUnionType(std::vector<Field>&& fields);

public:
    static Type create(bool isConst, bool isVolatile, std::vector<Field> fields);

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    bool operator==(const AnonymousUnionType& rhs) const;

    bool operator!=(const AnonymousUnionType& rhs) const;
};

class EnumType final
{
    std::string m_name;
    std::uint64_t m_scope;

    EnumType(std::string name, std::uint64_t scope);

public:
    static Type create(bool isConst, bool isVolatile, const std::string& name, std::uint64_t scope);

    [[nodiscard]] const std::string& getName() const;

    std::uint64_t getScope() const;

    bool operator==(const EnumType& rhs) const;

    bool operator!=(const EnumType& rhs) const;
};

class AnonymousEnumType
{
    std::shared_ptr<const Type> m_type;

    AnonymousEnumType(std::shared_ptr<const Type> type);

public:
    static Type create(bool isConst, bool isVolatile, Type&& type);

    bool operator==(const AnonymousEnumType& rhs) const;

    bool operator!=(const AnonymousEnumType& rhs) const;

    const Type& getType() const
    {
        return *m_type;
    }
};

class PointerType final
{
    bool m_restricted;
    std::shared_ptr<const Type> m_elementType;

    PointerType(bool isRestricted, std::shared_ptr<Type>&& elementType);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& elementType);

    [[nodiscard]] const Type& getElementType() const;

    [[nodiscard]] bool isRestricted() const;

    bool operator==(const PointerType& rhs) const;

    bool operator!=(const PointerType& rhs) const;
};

class Type final
{
    bool m_isConst;
    bool m_isVolatile;
    std::string m_name;

public:
    using Variant = std::variant<std::monostate, PrimitiveType, ArrayType, AbstractArrayType, ValArrayType,
                                 FunctionType, StructType, UnionType, EnumType, PointerType, AnonymousEnumType,
                                 AnonymousStructType, AnonymousUnionType>;

private:
    Variant m_type;

public:
    explicit Type(bool isConst = false, bool isVolatile = false, Variant type = std::monostate{});

    [[nodiscard]] const Variant& get() const;

    [[nodiscard]] bool isConst() const;

    [[nodiscard]] bool isVolatile() const;

    [[nodiscard]] std::string_view getName() const;

    void setName(std::string_view name);

    [[nodiscard]] bool isTypedef() const;

    bool operator==(const Type& rhs) const;

    bool operator!=(const Type& rhs) const;

    [[nodiscard]] bool isUndefined() const;
};

enum class Linkage
{
    Internal,
    External,
    None
};

enum class Lifetime
{
    Automatic,
    Static
};

class Declaration final
{
    Type m_type;
    Linkage m_linkage;
    Lifetime m_lifetime;
    std::string m_name;
    // initializer

public:
    Declaration(Type type, Linkage linkage, Lifetime lifetime, std::string name);

    [[nodiscard]] const Type& getType() const;

    [[nodiscard]] Linkage getLinkage() const;

    [[nodiscard]] Lifetime getLifetime() const;

    [[nodiscard]] const std::string& getName() const;
};

class ReturnStatement final
{
};

class ExpressionStatement final
{
};

class IfStatement final
{
};

class CompoundStatement final
{
    std::vector<std::variant<Statement, Declaration>> m_compoundItems;

public:
    explicit CompoundStatement(std::vector<std::variant<Statement, Declaration>> compoundItems);

    [[nodiscard]] const std::vector<std::variant<Statement, Declaration>>& getCompoundItems() const;
};

class ForStatement final
{
};

class HeadWhileStatement final
{
};

class FootWhileStatement final
{
};

class BreakStatement final
{
};

class ContinueStatement final
{
};

class SwitchStatement final
{
};

class DefaultStatement final
{
};

class CaseStatement final
{
};

class GotoStatement final
{
};

class LabelStatement final
{
};

class StructDefinition
{
    std::string m_name;
    std::vector<Field> m_fields;

public:
    StructDefinition(std::string_view name, std::vector<Field>&& fields);

    std::string_view getName() const
    {
        return m_name;
    }

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    bool operator==(const StructDefinition& rhs) const;

    bool operator!=(const StructDefinition& rhs) const;
};

class UnionDefinition
{
    std::string m_name;
    std::vector<Field> m_fields;

public:
    UnionDefinition(std::string_view name, std::vector<Field>&& fields);

    std::string_view getName() const
    {
        return m_name;
    }

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    bool operator==(const UnionDefinition& rhs) const;
    bool operator!=(const UnionDefinition& rhs) const;
};

class EnumDefinition
{
    std::string m_name;
    Type m_type;

public:
    EnumDefinition(std::string_view name, Type type) : m_name(cld::to_string(name)), m_type(std::move(type)) {}

    std::string_view getName() const
    {
        return m_name;
    }

    const Type& getType() const
    {
        return m_type;
    }
};

class FunctionDefinition final
{
    Type m_type;
    std::string m_name;
    std::vector<Declaration> m_parameterDeclarations;
    Linkage m_linkage;
    CompoundStatement m_compoundStatement;

public:
    FunctionDefinition(Type type, std::string name, std::vector<Declaration> parameterDeclarations,
                       Linkage linkage, CompoundStatement&& compoundStatement);

    [[nodiscard]] const std::string& getName() const;

    [[nodiscard]] const Type& getType() const;

    [[nodiscard]] const std::vector<Declaration>& getParameterDeclarations() const;

    [[nodiscard]] bool isKandR() const;

    [[nodiscard]] Linkage getLinkage() const;
};

class TranslationUnit final
{
public:
    using Variant = std::variant<std::unique_ptr<FunctionDefinition>, std::unique_ptr<Declaration>>;

private:
    std::vector<Variant> m_globals;

public:
    explicit TranslationUnit(std::vector<Variant> globals);

    [[nodiscard]] const std::vector<Variant>& getGlobals() const;
};

std::string_view declaratorToName(const cld::Syntax::Declarator& declarator);

Lexer::CTokenIterator declaratorToLoc(const cld::Syntax::Declarator& declarator);

bool isVoid(const Type& type);

bool isArray(const Type& type);
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

} // namespace cld::diag

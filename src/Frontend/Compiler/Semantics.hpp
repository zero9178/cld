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
class SemanticAnalysis;

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

// using Statement = std::variant<ReturnStatement, ExpressionStatement, IfStatement, CompoundStatement, ForStatement,
//                               HeadWhileStatement, FootWhileStatement, BreakStatement, ContinueStatement,
//                               SwitchStatement, DefaultStatement, CaseStatement, GotoStatement, LabelStatement>;

// Temporary dummy
class Statement
{
};

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
    static cld::Semantics::Type create(bool isConst, bool isVolatile, bool isFloatingPoint, bool isSigned,
                                       std::uint8_t bitCount, Kind kind);

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

    [[nodiscard]] bool isFloatingPoint() const
    {
        return m_isFloatingPoint;
    }

    [[nodiscard]] bool isSigned() const
    {
        return m_isSigned;
    }

    [[nodiscard]] std::uint8_t getByteCount() const;

    [[nodiscard]] std::size_t getSizeOf(const SemanticAnalysis&) const
    {
        return getByteCount();
    }

    [[nodiscard]] std::size_t getAlignOf(const SemanticAnalysis&) const
    {
        return getByteCount();
    }

    [[nodiscard]] std::uint8_t getBitCount() const
    {
        return m_bitCount;
    }

    Kind getKind() const
    {
        return m_kind;
    }

    bool operator==(const PrimitiveType& rhs) const;

    bool operator!=(const PrimitiveType& rhs) const;
};

class ArrayType final
{
    bool m_restricted;
    bool m_static;
    std::shared_ptr<const Type> m_type;
    std::size_t m_size;

    ArrayType(bool isRestricted, bool isStatic, std::shared_ptr<Type>&& type, std::size_t size);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type&& type, std::size_t size);

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

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

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

    [[nodiscard]] const Type& getType() const
    {
        return *m_type;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_restricted;
    }

    std::size_t getSizeOf(const SemanticAnalysis&) const
    {
        CLD_UNREACHABLE;
    }

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

    bool operator==(const AbstractArrayType& rhs) const;

    bool operator!=(const AbstractArrayType& rhs) const;
};

class ValArrayType final
{
    bool m_restricted;
    bool m_static;
    std::shared_ptr<const Type> m_type;

    ValArrayType(bool isRestricted, bool isStatic, std::shared_ptr<cld::Semantics::Type>&& type);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic, Type&& type);

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

    std::size_t getSizeOf(const SemanticAnalysis&) const
    {
        CLD_UNREACHABLE;
    }

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

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

    [[nodiscard]] const Type& getReturnType() const
    {
        return *m_returnType;
    }

    [[nodiscard]] const std::vector<std::pair<Type, std::string>>& getArguments() const
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

    std::size_t getSizeOf(const SemanticAnalysis&) const
    {
        CLD_UNREACHABLE;
    }

    std::size_t getAlignOf(const SemanticAnalysis&) const
    {
        CLD_UNREACHABLE;
    }

    bool operator==(const FunctionType& rhs) const;

    bool operator!=(const FunctionType& rhs) const;
};

class StructType final
{
    std::string m_name;
    std::int64_t m_scopeOrId;

    StructType(std::string_view name, std::int64_t scope);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, std::int64_t scope);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    std::int64_t getScopeOrId() const
    {
        return m_scopeOrId;
    }

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

    bool operator==(const StructType& rhs) const;

    bool operator!=(const StructType& rhs) const;
};

class UnionType final
{
    std::string m_name;
    std::int64_t m_scopeOrId;

    UnionType(std::string_view name, std::int64_t scopeOrId);

public:
    static Type create(bool isConst, bool isVolatile, std::string_view name, std::int64_t scopeOrId);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    std::int64_t getScopeOrId() const
    {
        return m_scopeOrId;
    }

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

    bool operator==(const UnionType& rhs) const;

    bool operator!=(const UnionType& rhs) const;
};

struct Field
{
    std::shared_ptr<const Type> type;
    std::string name;
    std::optional<std::uint8_t> bitFieldSize;

    bool operator==(const Field& rhs) const;

    bool operator!=(const Field& rhs) const;
};

class AnonymousStructType final
{
    std::vector<Field> m_fields;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

    AnonymousStructType(std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf);

public:
    static Type create(bool isConst, bool isVolatile, std::vector<Field> fields, std::uint64_t sizeOf,
                       std::uint64_t alignOf);

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

    bool operator==(const AnonymousStructType& rhs) const;

    bool operator!=(const AnonymousStructType& rhs) const;
};

class AnonymousUnionType final
{
    std::vector<Field> m_fields;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

    AnonymousUnionType(std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf);

public:
    static Type create(bool isConst, bool isVolatile, std::vector<Field> fields, std::uint64_t sizeOf,
                       std::uint64_t alignOf);

    const std::vector<Field>& getFields() const
    {
        return m_fields;
    }

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

    bool operator==(const AnonymousUnionType& rhs) const;

    bool operator!=(const AnonymousUnionType& rhs) const;
};

class EnumType final
{
    std::string m_name;
    std::int64_t m_scopeOrId;

    EnumType(std::string name, std::int64_t scopeOrId);

public:
    static Type create(bool isConst, bool isVolatile, const std::string& name, std::int64_t scopeOrId);

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }

    std::int64_t getScopeOrId() const
    {
        return m_scopeOrId;
    }

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

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

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;
};

class PointerType final
{
    bool m_restricted;
    std::shared_ptr<const Type> m_elementType;

    PointerType(bool isRestricted, std::shared_ptr<Type>&& elementType);

public:
    static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& elementType);

    [[nodiscard]] const Type& getElementType() const
    {
        return *m_elementType;
    }

    [[nodiscard]] bool isRestricted() const
    {
        return m_restricted;
    }

    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;

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
    explicit Type(bool isConst = false, bool isVolatile = false, Variant type = std::monostate{})
        : m_isConst(isConst), m_isVolatile(isVolatile), m_type(std::move(type))
    {
    }

    [[nodiscard]] const Variant& get() const
    {
        return m_type;
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

    bool operator==(const Type& rhs) const;

    bool operator!=(const Type& rhs) const;

    [[nodiscard]] bool isUndefined() const
    {
        return std::holds_alternative<std::monostate>(m_type);
    }

    // Likely replaced with an interface soon?
    std::size_t getSizeOf(const SemanticAnalysis& analysis) const;

    std::size_t getAlignOf(const SemanticAnalysis& analysis) const;
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
    Static,
    Register
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

    [[nodiscard]] std::string_view getName() const
    {
        return m_name;
    }
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
public:
    using Variant = std::variant<Statement, std::unique_ptr<Declaration>>;

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
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

public:
    StructDefinition(std::string_view name, std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf);

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
    std::string m_name;
    std::vector<Field> m_fields;
    std::uint64_t m_sizeOf;
    std::uint64_t m_alignOf;

public:
    UnionDefinition(std::string_view name, std::vector<Field>&& fields, std::uint64_t sizeOf, std::uint64_t alignOf);

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
    std::vector<std::unique_ptr<Declaration>> m_parameterDeclarations;
    Linkage m_linkage;
    CompoundStatement m_compoundStatement;

public:
    FunctionDefinition(Type type, std::string_view name,
                       std::vector<std::unique_ptr<Declaration>> parameterDeclarations, Linkage linkage,
                       CompoundStatement compoundStatement)
        : m_type(std::move(type)),
          m_name(cld::to_string(name)),
          m_parameterDeclarations(std::move(parameterDeclarations)),
          m_linkage(linkage),
          m_compoundStatement(std::move(compoundStatement))
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

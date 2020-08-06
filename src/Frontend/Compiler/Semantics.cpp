#include "Semantics.hpp"

#include <algorithm>
#include <optional>
#include <utility>

#include "ErrorMessages.hpp"
#include "SemanticUtil.hpp"

const cld::Semantics::Type& cld::Semantics::ArrayType::getType() const
{
    return *m_type;
}

std::size_t cld::Semantics::ArrayType::getSize() const
{
    return m_size;
}

cld::Semantics::ArrayType::ArrayType(bool isRestricted, bool isStatic, std::shared_ptr<cld::Semantics::Type>&& type,
                                     std::size_t size)
    : m_restricted(isRestricted), m_static(isStatic), m_type(std::move(type)), m_size(size)
{
}

bool cld::Semantics::ArrayType::isRestricted() const
{
    return m_restricted;
}

cld::Semantics::Type cld::Semantics::ArrayType::create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic,
                                                       cld::Semantics::Type&& type, std::size_t size)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                ArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type)), size));
}

bool cld::Semantics::ArrayType::operator==(const cld::Semantics::ArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type, m_size) == std::tie(rhs.m_restricted, *rhs.m_type, rhs.m_size);
}

bool cld::Semantics::ArrayType::operator!=(const cld::Semantics::ArrayType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::ArrayType::isStatic() const
{
    return m_static;
}

bool cld::Semantics::Type::isConst() const
{
    return m_isConst;
}

bool cld::Semantics::Type::isVolatile() const
{
    return m_isVolatile;
}

std::string_view cld::Semantics::Type::getName() const
{
    return m_name;
}

void cld::Semantics::Type::setName(std::string_view name)
{
    m_name = name;
}

const cld::Semantics::Type::Variant& cld::Semantics::Type::get() const
{
    return m_type;
}

cld::Semantics::Type::Type(bool isConst, bool isVolatile, cld::Semantics::Type::Variant type)
    : m_isConst(isConst), m_isVolatile(isVolatile), m_type(std::move(type))
{
}

bool cld::Semantics::Type::operator==(const cld::Semantics::Type& rhs) const
{
    if (std::tie(m_isConst, m_isVolatile) != std::tie(rhs.m_isConst, rhs.m_isVolatile))
    {
        return false;
    }
    if (m_type.index() != rhs.m_type.index())
    {
        return false;
    }
    return cld::match(m_type, [&](auto&& value) -> bool {
        using T = std::decay_t<decltype(value)>;
        return value == cld::get<T>(rhs.m_type);
    });
}

bool cld::Semantics::Type::operator!=(const cld::Semantics::Type& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::Type::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_type);
}

bool cld::Semantics::Type::isTypedef() const
{
    return !m_name.empty();
}

cld::Semantics::PointerType::PointerType(bool isRestricted, std::shared_ptr<cld::Semantics::Type>&& elementType)
    : m_restricted(isRestricted), m_elementType(std::move(elementType))
{
}

const cld::Semantics::Type& cld::Semantics::PointerType::getElementType() const
{
    return *m_elementType;
}

bool cld::Semantics::PointerType::isRestricted() const
{
    return m_restricted;
}

cld::Semantics::Type cld::Semantics::PointerType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                         cld::Semantics::Type&& elementType)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                PointerType(isRestricted, std::make_shared<Type>(std::move(elementType))));
}

bool cld::Semantics::PointerType::operator==(const cld::Semantics::PointerType& rhs) const
{
    return std::tie(m_restricted, *m_elementType) == std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool cld::Semantics::PointerType::operator!=(const cld::Semantics::PointerType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::EnumType::EnumType(std::string name, std::uint64_t scope) : m_name(std::move(name)), m_scope(scope) {}

cld::Semantics::Type cld::Semantics::EnumType::create(bool isConst, bool isVolatile, const std::string& name,
                                                      std::uint64_t scope)
{
    return cld::Semantics::Type(isConst, isVolatile, EnumType(name, scope));
}

bool cld::Semantics::EnumType::operator==(const cld::Semantics::EnumType& rhs) const
{
    return m_name == rhs.m_name;
}

bool cld::Semantics::EnumType::operator!=(const cld::Semantics::EnumType& rhs) const
{
    return !(rhs == *this);
}

const std::string& cld::Semantics::EnumType::getName() const
{
    return m_name;
}

std::uint64_t cld::Semantics::EnumType::getScope() const
{
    return m_scope;
}

cld::Semantics::PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount, Kind kind)
    : m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned), m_bitCount(bitCount), m_kind(kind)
{
}

bool cld::Semantics::PrimitiveType::isFloatingPoint() const
{
    return m_isFloatingPoint;
}

bool cld::Semantics::PrimitiveType::isSigned() const
{
    return m_isSigned;
}

std::uint8_t cld::Semantics::PrimitiveType::getByteCount() const
{
    int remainder = m_bitCount % 8;
    if (!remainder)
    {
        return m_bitCount / 8;
    }
    return (m_bitCount + 8 - remainder) / 8;
}

std::uint8_t cld::Semantics::PrimitiveType::getBitCount() const
{
    return m_bitCount;
}

cld::Semantics::Type cld::Semantics::PrimitiveType::create(bool isConst, bool isVolatile, bool isFloatingPoint,
                                                           bool isSigned, std::uint8_t bitCount, Kind kind)
{
    return cld::Semantics::Type(isConst, isVolatile, PrimitiveType(isFloatingPoint, isSigned, bitCount, kind));
}

bool cld::Semantics::PrimitiveType::operator==(const cld::Semantics::PrimitiveType& rhs) const
{
    if (m_bitCount == 0 && rhs.m_bitCount == 0)
    {
        return true;
    }
    return std::tie(m_isFloatingPoint, m_isSigned, m_bitCount, m_kind)
           == std::tie(rhs.m_isFloatingPoint, rhs.m_isSigned, rhs.m_bitCount, rhs.m_kind);
}

bool cld::Semantics::PrimitiveType::operator!=(const cld::Semantics::PrimitiveType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createChar(bool isConst, bool isVolatile,
                                                               const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, options.charIsSigned, 8, Kind::Char);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createSignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 8, Kind::SignedChar);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 8, Kind::UnsignedChar);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnderlineBool(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 1, Kind::Bool);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createShort(bool isConst, bool isVolatile,
                                                                const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfShort * 8, Kind::Short);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedShort(bool isConst, bool isVolatile,
                                                                        const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfShort * 8, Kind::UnsignedShort);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createInt(bool isConst, bool isVolatile,
                                                              const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfInt * 8, Kind::Int);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedInt(bool isConst, bool isVolatile,
                                                                      const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfInt * 8, Kind::UnsignedInt);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLong(bool isConst, bool isVolatile,
                                                               const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfLong * 8, Kind::Long);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLong(bool isConst, bool isVolatile,
                                                                       const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfLong * 8, Kind::UnsignedLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 64, Kind::LongLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 64, Kind::UnsignedLongLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createFloat(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 32, Kind::Float);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createDouble(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 64, Kind::Double);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLongDouble(bool isConst, bool isVolatile,
                                                                     const LanguageOptions& options)
{
    return create(isConst, isVolatile, true, true, options.sizeOfLongDoubleBits, Kind::LongDouble);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createVoid(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 0, Kind::Void);
}

cld::Semantics::PrimitiveType::Kind cld::Semantics::PrimitiveType::getKind() const
{
    return m_kind;
}

cld::Semantics::ValArrayType::ValArrayType(bool isRestricted, bool isStatic,
                                           std::shared_ptr<cld::Semantics::Type>&& type)
    : m_restricted(isRestricted), m_static(isStatic), m_type(std::move(type))
{
}

const cld::Semantics::Type& cld::Semantics::ValArrayType::getType() const
{
    return *m_type;
}

bool cld::Semantics::ValArrayType::isRestricted() const
{
    return m_restricted;
}

bool cld::Semantics::ValArrayType::isStatic() const
{
    return m_static;
}

cld::Semantics::Type cld::Semantics::ValArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                          bool isStatic, cld::Semantics::Type&& type)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                ValArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type))));
}

bool cld::Semantics::ValArrayType::operator==(const cld::Semantics::ValArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool cld::Semantics::ValArrayType::operator!=(const cld::Semantics::ValArrayType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::FunctionType::FunctionType(std::shared_ptr<Type>&& returnType,
                                           std::vector<std::pair<Type, std::string>> arguments, bool lastIsVararg,
                                           bool isKandR)
    : m_returnType(std::move(returnType)),
      m_arguments(std::move(arguments)),
      m_lastIsVararg(lastIsVararg),
      m_isKandR(isKandR)
{
}

const cld::Semantics::Type& cld::Semantics::FunctionType::getReturnType() const
{
    return *m_returnType;
}

const std::vector<std::pair<cld::Semantics::Type, std::string>>& cld::Semantics::FunctionType::getArguments() const
{
    return m_arguments;
}

bool cld::Semantics::FunctionType::isLastVararg() const
{
    return m_lastIsVararg;
}

cld::Semantics::Type cld::Semantics::FunctionType::create(cld::Semantics::Type&& returnType,
                                                          std::vector<std::pair<Type, std::string>>&& arguments,
                                                          bool lastIsVararg, bool isKandR)
{
    return cld::Semantics::Type(
        false, false,
        FunctionType(std::make_shared<Type>(std::move(returnType)), std::move(arguments), lastIsVararg, isKandR));
}

bool cld::Semantics::FunctionType::operator==(const cld::Semantics::FunctionType& rhs) const
{
    if (*m_returnType != *rhs.m_returnType)
    {
        return false;
    }
    if (!std::equal(m_arguments.begin(), m_arguments.end(), rhs.m_arguments.begin(), rhs.m_arguments.end(),
                    [](const auto& lhs, const auto& rhs) { return lhs.first == rhs.first; }))
    {
        return false;
    }
    return m_lastIsVararg == rhs.m_lastIsVararg;
}

bool cld::Semantics::FunctionType::operator!=(const cld::Semantics::FunctionType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::FunctionType::isKandR() const
{
    return m_isKandR;
}

cld::Semantics::AbstractArrayType::AbstractArrayType(bool isRestricted, std::shared_ptr<cld::Semantics::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
{
}

const cld::Semantics::Type& cld::Semantics::AbstractArrayType::getType() const
{
    return *m_type;
}

bool cld::Semantics::AbstractArrayType::isRestricted() const
{
    return m_restricted;
}

cld::Semantics::Type cld::Semantics::AbstractArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                               cld::Semantics::Type&& type)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                AbstractArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool cld::Semantics::AbstractArrayType::operator==(const cld::Semantics::AbstractArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool cld::Semantics::AbstractArrayType::operator!=(const cld::Semantics::AbstractArrayType& rhs) const
{
    return !(rhs == *this);
}

std::string_view cld::Semantics::declaratorToName(const cld::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> std::string_view {
            return cld::get<std::string>(name.getIdentifierLoc()->getValue());
        },
        [](auto&& self, const Syntax::DirectDeclaratorParentheses& declarator) -> std::string_view {
            return cld::match(declarator.getDeclarator().getDirectDeclarator(),
                              [&self](auto&& value) -> std::string_view { return self(value); });
        },
        [](auto&& self, auto&& value) -> std::string_view {
            return cld::match(value.getDirectDeclarator(),
                              [&self](auto&& value) -> std::string_view { return self(value); });
        });
}

cld::Lexer::CTokenIterator cld::Semantics::declaratorToLoc(const cld::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> cld::Lexer::CTokenIterator {
            return name.getIdentifierLoc();
        },
        [](auto&& self, const Syntax::DirectDeclaratorParentheses& declarator) -> cld::Lexer::CTokenIterator {
            return cld::match(declarator.getDeclarator().getDirectDeclarator(),
                              [&self](auto&& value) -> cld::Lexer::CTokenIterator { return self(value); });
        },
        [](auto&& self, auto&& value) -> cld::Lexer::CTokenIterator {
            return cld::match(value.getDirectDeclarator(),
                              [&self](auto&& value) -> cld::Lexer::CTokenIterator { return self(value); });
        });
}

cld::Semantics::StructType::StructType(std::string_view name, std::uint64_t scope)
    : m_name(cld::to_string(name)), m_scope(scope)
{
}

cld::Semantics::Type cld::Semantics::StructType::create(bool isConst, bool isVolatile, std::string_view name,
                                                        std::uint64_t scope)
{
    return cld::Semantics::Type(isConst, isVolatile, StructType(name, scope));
}

bool cld::Semantics::StructType::operator==(const cld::Semantics::StructType& rhs) const
{
    return std::tie(m_name, m_scope) == std::tie(rhs.m_name, rhs.m_scope);
}

bool cld::Semantics::StructType::operator!=(const cld::Semantics::StructType& rhs) const
{
    return !(rhs == *this);
}

std::string_view cld::Semantics::StructType::getName() const
{
    return m_name;
}

std::uint64_t cld::Semantics::StructType::getScope() const
{
    return m_scope;
}

cld::Semantics::UnionType::UnionType(std::string_view name, std::uint64_t scope)
    : m_name(cld::to_string(name)), m_scope(scope)
{
}

cld::Semantics::Type cld::Semantics::UnionType::create(bool isConst, bool isVolatile, std::string_view name,
                                                       std::uint64_t scope)
{
    return cld::Semantics::Type(isConst, isVolatile, UnionType(name, scope));
}

bool cld::Semantics::UnionType::operator==(const cld::Semantics::UnionType& rhs) const
{
    return std::tie(m_name, m_scope) == std::tie(rhs.m_name, rhs.m_scope);
}

bool cld::Semantics::UnionType::operator!=(const cld::Semantics::UnionType& rhs) const
{
    return !(rhs == *this);
}

std::string_view cld::Semantics::UnionType::getName() const
{
    return m_name;
}

std::uint64_t cld::Semantics::UnionType::getScope() const
{
    return m_scope;
}

bool cld::Semantics::isVoid(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.get());
    if (!primitive)
    {
        return false;
    }
    return primitive->getBitCount() == 0;
}

bool cld::Semantics::isArray(const cld::Semantics::Type& type)
{
    return std::holds_alternative<ArrayType>(type.get()) || std::holds_alternative<ValArrayType>(type.get())
           || std::holds_alternative<AbstractArrayType>(type.get());
}

cld::Semantics::FunctionDefinition::FunctionDefinition(Type type, std::string name,
                                                       std::vector<Declaration> parameterDeclarations, Linkage linkage,
                                                       CompoundStatement&& compoundStatement)
    : m_type(std::move(type)),
      m_name(std::move(name)),
      m_parameterDeclarations(std::move(parameterDeclarations)),
      m_linkage(linkage),
      m_compoundStatement(std::move(compoundStatement))
{
}

const cld::Semantics::Type& cld::Semantics::FunctionDefinition::getType() const
{
    return m_type;
}

bool cld::Semantics::FunctionDefinition::isKandR() const
{
    return cld::get<FunctionType>(m_type.get()).isKandR();
}

const std::string& cld::Semantics::FunctionDefinition::getName() const
{
    return m_name;
}

cld::Semantics::Linkage cld::Semantics::FunctionDefinition::getLinkage() const
{
    return m_linkage;
}

const std::vector<cld::Semantics::Declaration>& cld::Semantics::FunctionDefinition::getParameterDeclarations() const
{
    return m_parameterDeclarations;
}

cld::Semantics::TranslationUnit::TranslationUnit(std::vector<TranslationUnit::Variant> globals)
    : m_globals(std::move(globals))
{
}

const std::vector<cld::Semantics::TranslationUnit::Variant>& cld::Semantics::TranslationUnit::getGlobals() const
{
    return m_globals;
}

cld::Semantics::Declaration::Declaration(cld::Semantics::Type type, cld::Semantics::Linkage linkage,
                                         cld::Semantics::Lifetime lifetime, std::string name)
    : m_type(std::move(type)), m_linkage(linkage), m_lifetime(lifetime), m_name(std::move(name))
{
}

const cld::Semantics::Type& cld::Semantics::Declaration::getType() const
{
    return m_type;
}

cld::Semantics::Linkage cld::Semantics::Declaration::getLinkage() const
{
    return m_linkage;
}

cld::Semantics::Lifetime cld::Semantics::Declaration::getLifetime() const
{
    return m_lifetime;
}

const std::string& cld::Semantics::Declaration::getName() const
{
    return m_name;
}

cld::Semantics::CompoundStatement::CompoundStatement(std::vector<std::variant<Statement, Declaration>> compoundItems)
    : m_compoundItems(std::move(compoundItems))
{
}

const std::vector<std::variant<cld::Semantics::Statement, cld::Semantics::Declaration>>&
    cld::Semantics::CompoundStatement::getCompoundItems() const
{
    return m_compoundItems;
}

cld::Semantics::StructDefinition::StructDefinition(std::string_view name, std::vector<Field>&& fields)
    : m_name(cld::to_string(name)), m_fields(std::move(fields))
{
}

bool cld::Semantics::StructDefinition::operator==(const cld::Semantics::StructDefinition& rhs) const
{
    return std::tie(m_name, m_fields) == std::tie(rhs.m_name, rhs.m_fields);
}

bool cld::Semantics::StructDefinition::operator!=(const cld::Semantics::StructDefinition& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::UnionDefinition::UnionDefinition(std::string_view name, std::vector<Field>&& fields)
    : m_name(cld::to_string(name)), m_fields(std::move(fields))
{
}

bool cld::Semantics::UnionDefinition::operator==(const cld::Semantics::UnionDefinition& rhs) const
{
    return std::tie(m_name, m_fields) == std::tie(rhs.m_name, rhs.m_fields);
}

bool cld::Semantics::UnionDefinition::operator!=(const cld::Semantics::UnionDefinition& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::AnonymousStructType::AnonymousStructType(std::vector<Field>&& fields) : m_fields(std::move(fields)) {}

bool cld::Semantics::AnonymousStructType::operator==(const cld::Semantics::AnonymousStructType& rhs) const
{
    return m_fields == rhs.m_fields;
}

bool cld::Semantics::AnonymousStructType::operator!=(const cld::Semantics::AnonymousStructType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::AnonymousStructType::create(bool isConst, bool isVolatile,
                                                                 std::vector<Field> fields)
{
    return cld::Semantics::Type(isConst, isVolatile, AnonymousStructType(std::move(fields)));
}

cld::Semantics::AnonymousUnionType::AnonymousUnionType(std::vector<Field>&& fields) : m_fields(std::move(fields)) {}

bool cld::Semantics::AnonymousUnionType::operator==(const cld::Semantics::AnonymousUnionType& rhs) const
{
    return m_fields == rhs.m_fields;
}

bool cld::Semantics::AnonymousUnionType::operator!=(const cld::Semantics::AnonymousUnionType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::AnonymousUnionType::create(bool isConst, bool isVolatile,
                                                                std::vector<Field> fields)
{
    return cld::Semantics::Type(isConst, isVolatile, AnonymousUnionType(std::move(fields)));
}

cld::Semantics::AnonymousEnumType::AnonymousEnumType(std::shared_ptr<const Type> type) : m_type(std::move(type)) {}

bool cld::Semantics::AnonymousEnumType::operator==(const cld::Semantics::AnonymousEnumType& rhs) const
{
    return *m_type == *rhs.m_type;
}

bool cld::Semantics::AnonymousEnumType::operator!=(const cld::Semantics::AnonymousEnumType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::AnonymousEnumType::create(bool isConst, bool isVolatile, Type&& type)
{
    return Type(isConst, isVolatile, AnonymousEnumType(std::make_shared<Type>(std::move(type))));
}

bool cld::Semantics::Field::operator==(const cld::Semantics::Field& rhs) const
{
    return std::tie(type, name, bitFieldSize) == std::tie(rhs.type, rhs.name, rhs.bitFieldSize);
}

bool cld::Semantics::Field::operator!=(const cld::Semantics::Field& rhs) const
{
    return !(rhs == *this);
}

namespace
{
using namespace cld::Semantics;

std::string typeToString(const Type& arg)
{
    if (arg.isTypedef())
    {
        return cld::to_string(arg.getName());
    }
    std::string qualifiersAndSpecifiers;
    std::string declarators;
    std::optional<Type> maybeCurr = arg;
    while (maybeCurr)
    {
        maybeCurr = cld::match(
            maybeCurr->get(),
            [&](const PrimitiveType& primitiveType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                switch (primitiveType.getKind())
                {
                    case PrimitiveType::Kind::Char: qualifiersAndSpecifiers += "char"; break;
                    case PrimitiveType::Kind::SignedChar: qualifiersAndSpecifiers += "signed char"; break;
                    case PrimitiveType::Kind::UnsignedChar: qualifiersAndSpecifiers += "unsigned char"; break;
                    case PrimitiveType::Kind::Bool: qualifiersAndSpecifiers += "_Bool"; break;
                    case PrimitiveType::Kind::Short: qualifiersAndSpecifiers += "short"; break;
                    case PrimitiveType::Kind::UnsignedShort: qualifiersAndSpecifiers += "unsigned short"; break;
                    case PrimitiveType::Kind::Int: qualifiersAndSpecifiers += "int"; break;
                    case PrimitiveType::Kind::UnsignedInt: qualifiersAndSpecifiers += "unsigned int"; break;
                    case PrimitiveType::Kind::Long: qualifiersAndSpecifiers += "long"; break;
                    case PrimitiveType::Kind::UnsignedLong: qualifiersAndSpecifiers += "unsigned long"; break;
                    case PrimitiveType::Kind::LongLong: qualifiersAndSpecifiers += "long long"; break;
                    case PrimitiveType::Kind::UnsignedLongLong: qualifiersAndSpecifiers += "unsigned long long"; break;
                    case PrimitiveType::Kind::Float: qualifiersAndSpecifiers += "float"; break;
                    case PrimitiveType::Kind::Double: qualifiersAndSpecifiers += "double"; break;
                    case PrimitiveType::Kind::LongDouble: qualifiersAndSpecifiers += "long double"; break;
                    case PrimitiveType::Kind::Void: qualifiersAndSpecifiers += "void"; break;
                }
                return {};
            },
            [&](const PointerType&) -> std::optional<Type> {
                std::string temp;
                auto curr = *maybeCurr;
                while (auto* pointerType = std::get_if<PointerType>(&curr.get()))
                {
                    temp += "*";
                    if (pointerType->isRestricted())
                    {
                        temp += " restrict";
                    }
                    if (curr.isConst())
                    {
                        temp += " const";
                    }
                    if (maybeCurr->isVolatile())
                    {
                        temp += " volatile";
                    }
                    curr = pointerType->getElementType();
                }
                if (std::holds_alternative<AbstractArrayType>(curr.get())
                    || std::holds_alternative<ValArrayType>(curr.get()) || std::holds_alternative<ArrayType>(curr.get())
                    || std::holds_alternative<FunctionType>(curr.get()))
                {
                    declarators = "(" + temp + declarators + ")";
                    return {std::move(curr)};
                }
                else
                {
                    qualifiersAndSpecifiers = typeToString(curr) + temp;
                    return {};
                }
            },
            [&](const ValArrayType& valArrayType) -> std::optional<Type> {
                declarators += "[";
                if (valArrayType.isStatic())
                {
                    declarators += "static ";
                }
                if (valArrayType.isRestricted())
                {
                    declarators += "restrict ";
                }
                if (maybeCurr->isConst())
                {
                    declarators += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    declarators += "volatile ";
                }
                declarators += "*]";
                return valArrayType.getType();
            },
            [&](const ArrayType& arrayType) -> std::optional<Type> {
                declarators += "[";
                if (arrayType.isStatic())
                {
                    declarators += "static ";
                }
                if (arrayType.isRestricted())
                {
                    declarators += "restrict ";
                }
                if (maybeCurr->isConst())
                {
                    declarators += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    declarators += "volatile ";
                }
                declarators += cld::to_string(arrayType.getSize()) + "]";
                return arrayType.getType();
            },
            [&](const AbstractArrayType& arrayType) -> std::optional<Type> {
                declarators += "[";
                if (arrayType.isRestricted())
                {
                    declarators += "restrict ";
                }
                if (maybeCurr->isConst())
                {
                    declarators += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    declarators += "volatile ";
                }
                declarators += "]";
                return arrayType.getType();
            },
            [&](const FunctionType& functionType) -> std::optional<Type> {
                declarators += "(";
                if (functionType.getArguments().empty())
                {
                    if (!functionType.isKandR())
                    {
                        declarators += "void";
                    }
                    declarators += ")";
                    return functionType.getReturnType();
                }
                declarators += typeToString(functionType.getArguments()[0].first);
                for (auto& iter : llvm::ArrayRef(functionType.getArguments()).drop_front())
                {
                    declarators += ", " + typeToString(iter.first);
                }
                if (functionType.isLastVararg())
                {
                    declarators += ",...";
                }
                declarators += ")";
                return functionType.getReturnType();
            },
            [&](const StructType& structType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "struct " + cld::to_string(structType.getName());
                return {};
            },
            [&](const UnionType& unionType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "union " + cld::to_string(unionType.getName());
                return {};
            },
            [&](const EnumType& enumType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "struct " + cld::to_string(enumType.getName());
                return {};
            },
            [&](const AnonymousStructType&) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "struct <anonymous>";
                return {};
            },
            [&](const AnonymousUnionType&) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "union <anonymous>";
                return {};
            },
            [&](const AnonymousEnumType&) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "enum <anonymous>";
                return {};
            },
            [&](std::monostate) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "<undefined>";
                return {};
            });
    }
    return qualifiersAndSpecifiers + declarators;
}
} // namespace

std::string cld::diag::StringConverter<cld::Semantics::Type>::inArg(const Semantics::Type& arg, const SourceInterface&)
{
    return typeToString(arg);
}

std::string cld::diag::CustomFormat<U'f', U'u', U'l', U'l'>::operator()(const Semantics::Type& arg)
{
    auto result = typeToString(arg);
    if (arg.isTypedef())
    {
        return "'" + cld::to_string(arg.getName()) + "' (aka '" + result + "')";
    }
    return "'" + result + "'";
}

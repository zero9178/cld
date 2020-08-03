#include "Semantics.hpp"

#include <algorithm>
#include <optional>
#include <utility>

#include "ErrorMessages.hpp"

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
    auto name = cld::to_string(type.getTypeName()) + "[" + std::to_string(size) + "]";
    return cld::Semantics::Type(isConst, isVolatile, std::move(name),
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

cld::Semantics::Type::Type(bool isConst, bool isVolatile, std::string name, cld::Semantics::Type::Variant type)
    : m_isConst(isConst),
      m_isVolatile(isVolatile),
      m_name([name = std::move(name), isConst, isVolatile]() mutable {
          if (isConst)
          {
              name += " const";
          }
          if (isVolatile)
          {
              name += " volatile";
          }
          return name;
      }()),
      m_typeName(m_name),
      m_type(std::move(type))
{
}

bool cld::Semantics::Type::operator==(const cld::Semantics::Type& rhs) const
{
    return std::tie(m_isConst, m_isVolatile, m_type) == std::tie(rhs.m_isConst, rhs.m_isVolatile, rhs.m_type);
}

bool cld::Semantics::Type::operator!=(const cld::Semantics::Type& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::Type::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_type);
}

std::string_view cld::Semantics::Type::getTypeName() const
{
    return m_typeName;
}

bool cld::Semantics::Type::isTypedef() const
{
    return m_typeName != m_name;
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
    std::string name;
    if (!elementType.isTypedef() && std::holds_alternative<FunctionType>(elementType.get()))
    {
        auto openParenthese = elementType.getName().find('(');
        CLD_ASSERT(openParenthese != std::string_view::npos);
        name = cld::to_string(elementType.getName().substr(0, openParenthese)) + "(*)"
               + cld::to_string(elementType.getName().substr(openParenthese));
    }
    else if (!elementType.isTypedef()
             && (std::holds_alternative<ArrayType>(elementType.get())
                 || std::holds_alternative<ValArrayType>(elementType.get())
                 || std::holds_alternative<AbstractArrayType>(elementType.get())))
    {
        auto openBracket = elementType.getName().find('[');
        CLD_ASSERT(openBracket != std::string_view::npos);
        name = cld::to_string(elementType.getName().substr(0, openBracket)) + "(*)"
               + cld::to_string(elementType.getName().substr(openBracket));
    }
    else
    {
        name = cld::to_string(elementType.getTypeName()) + "*";
    }
    if (isRestricted)
    {
        name += " restricted";
    }
    return cld::Semantics::Type(isConst, isVolatile, std::move(name),
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
    return cld::Semantics::Type(isConst, isVolatile, "enum " + name, EnumType(name, scope));
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
    // Round up to the next highest power 2
    std::uint8_t temp = m_bitCount - 1;
    temp |= temp >> 1;
    temp |= temp >> 2;
    temp |= temp >> 4;
    return (temp + 1) / 8;
}

std::uint8_t cld::Semantics::PrimitiveType::getBitCount() const
{
    return m_bitCount;
}

cld::Semantics::Type cld::Semantics::PrimitiveType::create(bool isConst, bool isVolatile, bool isFloatingPoint,
                                                           bool isSigned, std::uint8_t bitCount, std::string name,
                                                           Kind kind)
{
    return cld::Semantics::Type(isConst, isVolatile, std::move(name),
                                PrimitiveType(isFloatingPoint, isSigned, bitCount, kind));
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
    return create(isConst, isVolatile, false, options.charIsSigned, 8, "char", Kind::Char);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createSignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 8, "signed char", Kind::SignedChar);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 8, "unsigned char", Kind::UnsignedChar);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnderlineBool(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 1, "_Bool", Kind::Bool);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createShort(bool isConst, bool isVolatile,
                                                                const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfShort * 8, "short", Kind::Short);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedShort(bool isConst, bool isVolatile,
                                                                        const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfShort * 8, "unsigned short", Kind::UnsignedShort);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createInt(bool isConst, bool isVolatile,
                                                              const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfInt * 8, "int", Kind::Int);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedInt(bool isConst, bool isVolatile,
                                                                      const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfInt * 8, "unsigned int", Kind::UnsignedInt);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLong(bool isConst, bool isVolatile,
                                                               const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfLong * 8, "long", Kind::Long);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLong(bool isConst, bool isVolatile,
                                                                       const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfLong * 8, "unsigned long", Kind::UnsignedLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 64, "long long", Kind::LongLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 64, "unsigned long long", Kind::UnsignedLongLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createFloat(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 32, "float", Kind::Float);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createDouble(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 64, "double", Kind::Double);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLongDouble(bool isConst, bool isVolatile,
                                                                     const LanguageOptions& options)
{
    return create(isConst, isVolatile, true, true, options.sizeOfLongDoubleBits, "long double", Kind::LongDouble);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createVoid(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 0, "void", Kind::Void);
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
    auto name = cld::to_string(type.getName()) + "[*]";
    return cld::Semantics::Type(isConst, isVolatile, std::move(name),
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
    std::string argumentsNames;
    for (std::size_t i = 0; i < arguments.size(); i++)
    {
        argumentsNames += arguments[i].first.getTypeName();
        if (i + 1 < arguments.size())
        {
            argumentsNames += ", ";
        }
    }
    argumentsNames = cld::to_string(returnType.getTypeName()) + "(" + argumentsNames + ")";
    return cld::Semantics::Type(
        false, false, std::move(argumentsNames),
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
    auto name = cld::to_string(type.getTypeName()) + "[]";
    return cld::Semantics::Type(isConst, isVolatile, std::move(name),
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
    return cld::Semantics::Type(isConst, isVolatile, "struct " + cld::to_string(name), StructType(name, scope));
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
    return cld::Semantics::Type(isConst, isVolatile, "struct " + cld::to_string(name), UnionType(name, scope));
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
    return cld::Semantics::Type(isConst, isVolatile, "struct <undefined>", AnonymousStructType(std::move(fields)));
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
    return cld::Semantics::Type(isConst, isVolatile, "union <undefined>", AnonymousUnionType(std::move(fields)));
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
    return Type(isConst, isVolatile, "enum <undefined>", AnonymousEnumType(std::make_shared<Type>(std::move(type))));
}

bool cld::Semantics::Field::operator==(const cld::Semantics::Field& rhs) const
{
    return std::tie(type, name, bitFieldSize) == std::tie(rhs.type, rhs.name, rhs.bitFieldSize);
}

bool cld::Semantics::Field::operator!=(const cld::Semantics::Field& rhs) const
{
    return !(rhs == *this);
}

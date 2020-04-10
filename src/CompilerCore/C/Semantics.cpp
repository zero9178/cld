#include "Semantics.hpp"

#include <algorithm>
#include <optional>
#include <utility>

#include "CompilerCore/Common/Util.hpp"

#include "ErrorMessages.hpp"

const cld::Semantics::Type& cld::Semantics::ArrayType::getType() const
{
    return *m_type;
}

std::size_t cld::Semantics::ArrayType::getSize() const
{
    return m_size;
}

cld::Semantics::ArrayType::ArrayType(bool isRestricted, std::shared_ptr<cld::Semantics::Type>&& type, std::size_t size)
    : m_restricted(isRestricted), m_type(std::move(type)), m_size(size)
{
}

bool cld::Semantics::ArrayType::isRestricted() const
{
    return m_restricted;
}

cld::Semantics::Type cld::Semantics::ArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                       cld::Semantics::Type&& type, std::size_t size)
{
    auto name = type.getName() + "[" + std::to_string(size) + "]";
    return cld::Semantics::Type(isConst, isVolatile, name,
                                ArrayType(isRestricted, std::make_shared<Type>(std::move(type)), size));
}

bool cld::Semantics::ArrayType::operator==(const cld::Semantics::ArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type, m_size) == std::tie(rhs.m_restricted, *rhs.m_type, rhs.m_size);
}

bool cld::Semantics::ArrayType::operator!=(const cld::Semantics::ArrayType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::Type::isConst() const
{
    return m_isConst;
}

bool cld::Semantics::Type::isVolatile() const
{
    return m_isVolatile;
}

const std::string& cld::Semantics::Type::getName() const
{
    return m_name;
}

void cld::Semantics::Type::setName(const std::string& name)
{
    m_name = name;
}

const cld::Semantics::Type::variant& cld::Semantics::Type::get() const
{
    return m_type;
}

cld::Semantics::Type::Type(bool isConst, bool isVolatile, std::string name, cld::Semantics::Type::variant&& type)
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
const std::string& cld::Semantics::Type::getTypeName() const
{
    return m_typeName;
}

bool cld::Semantics::Type::isTypedef() const
{
    return m_typeName != m_name;
}
std::string cld::Semantics::Type::getFullFormattedTypeName() const
{
    return '\'' + m_name + '\'' + (isTypedef() ? "(aka '" + m_typeName + "')" : "");
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
    if (std::holds_alternative<FunctionType>(elementType.get()))
    {
        auto openParenthese = elementType.getName().find('(');
        name = elementType.getName().substr(0, openParenthese) + "(*)" + elementType.getName().substr(openParenthese);
    }
    else if (std::holds_alternative<ArrayType>(elementType.get())
             || std::holds_alternative<ValArrayType>(elementType.get())
             || std::holds_alternative<AbstractArrayType>(elementType.get()))
    {
        auto openBracket = elementType.getName().find('[');
        name = elementType.getName().substr(0, openBracket) + "(*)" + elementType.getName().substr(openBracket);
    }
    else
    {
        name = elementType.getName() + "*";
    }
    if (isRestricted)
    {
        name += " restricted";
    }
    return cld::Semantics::Type(isConst, isVolatile, name,
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

cld::Semantics::EnumType::EnumType(std::string name, std::vector<std::pair<std::string, std::int32_t>> values)
    : m_name(std::move(name)), m_values(std::move(values))
{
}

const std::vector<std::pair<std::string, int32_t>>& cld::Semantics::EnumType::getValues() const
{
    return m_values;
}

bool cld::Semantics::EnumType::isDefinition() const
{
    return !m_values.empty();
}

bool cld::Semantics::EnumType::isAnonymous() const
{
    return m_name.empty();
}

cld::Semantics::Type cld::Semantics::EnumType::create(bool isConst, bool isVolatile, const std::string& name,
                                                      std::vector<std::pair<std::string, std::int32_t>> values)
{
    return cld::Semantics::Type(isConst, isVolatile, "enum " + name, EnumType(name, std::move(values)));
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
    return create(isConst, isVolatile, false, options.isCharSigned(), 8, "char", Kind::Char);
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
    return create(isConst, isVolatile, false, true, options.getSizeOfShort() * 8, "short", Kind::Short);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedShort(bool isConst, bool isVolatile,
                                                                        const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.getSizeOfShort() * 8, "unsigned short",
                  Kind::UnsignedShort);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createInt(bool isConst, bool isVolatile,
                                                              const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.getSizeOfInt() * 8, "int", Kind::Int);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedInt(bool isConst, bool isVolatile,
                                                                      const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.getSizeOfInt() * 8, "unsigned int", Kind::UnsignedInt);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLong(bool isConst, bool isVolatile,
                                                               const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.getSizeOfLong() * 8, "long", Kind::Long);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLong(bool isConst, bool isVolatile,
                                                                       const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.getSizeOfLong() * 8, "unsigned long", Kind::UnsignedLong);
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
    return create(isConst, isVolatile, true, true, options.getSizeOfLongDoubleBits(), "long double", Kind::LongDouble);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createVoid(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 0, "void", Kind::Void);
}

cld::Semantics::ValArrayType::ValArrayType(bool isRestricted, std::shared_ptr<cld::Semantics::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
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

cld::Semantics::Type cld::Semantics::ValArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                          cld::Semantics::Type&& type)
{
    auto name = type.getName() + "[*]";
    return cld::Semantics::Type(isConst, isVolatile, name,
                                ValArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
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
                                           bool hasPrototype)
    : m_returnType(std::move(returnType)),
      m_arguments(std::move(arguments)),
      m_lastIsVararg(lastIsVararg),
      m_hasPrototype(hasPrototype)
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
                                                          bool lastIsVararg, bool hasPrototype)
{
    std::string argumentsNames;
    for (std::size_t i = 0; i < arguments.size(); i++)
    {
        argumentsNames += arguments[i].first.getName();
        if (i + 1 < arguments.size())
        {
            argumentsNames += ", ";
        }
    }
    argumentsNames = returnType.getName() + "(" + argumentsNames + ")";
    return cld::Semantics::Type(
        false, false, argumentsNames,
        FunctionType(std::make_shared<Type>(std::move(returnType)), std::move(arguments), lastIsVararg, hasPrototype));
}

bool cld::Semantics::FunctionType::operator==(const cld::Semantics::FunctionType& rhs) const
{
    std::vector<Type> thisTypes, rhsTypes;
    auto pairFirst = [](const auto& pair) { return pair.first; };
    std::transform(m_arguments.begin(), m_arguments.end(), std::back_inserter(thisTypes), pairFirst);
    std::transform(rhs.m_arguments.begin(), rhs.m_arguments.end(), std::back_inserter(rhsTypes), pairFirst);
    return std::tie(*m_returnType, thisTypes, m_lastIsVararg)
           == std::tie(*rhs.m_returnType, rhsTypes, rhs.m_lastIsVararg);
}

bool cld::Semantics::FunctionType::operator!=(const cld::Semantics::FunctionType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::FunctionType::hasPrototype() const
{
    return m_hasPrototype;
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
    auto name = type.getName() + "[]";
    return cld::Semantics::Type(isConst, isVolatile, name,
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

std::string cld::Semantics::declaratorToName(const cld::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> std::string { return name.getIdentifier(); },
        [](auto&& self, const Syntax::DirectDeclaratorParenthese& declarator) -> std::string {
            return cld::match(declarator.getDeclarator().getDirectDeclarator(),
                              [&self](auto&& value) -> std::string { return self(value); });
        },
        [](auto&& self, auto&& value) -> std::string {
            return cld::match(value.getDirectDeclarator(),
                              [&self](auto&& value) -> std::string { return self(value); });
        });
}

std::vector<cld::Lexer::Token>::const_iterator
    cld::Semantics::declaratorToLoc(const cld::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> std::vector<cld::Lexer::Token>::const_iterator {
            return name.getIdentifierLoc();
        },
        [](auto&& self,
           const Syntax::DirectDeclaratorParenthese& declarator) -> std::vector<cld::Lexer::Token>::const_iterator {
            return cld::match(
                declarator.getDeclarator().getDirectDeclarator(),
                [&self](auto&& value) -> std::vector<cld::Lexer::Token>::const_iterator { return self(value); });
        },
        [](auto&& self, auto&& value) -> std::vector<cld::Lexer::Token>::const_iterator {
            return cld::match(
                value.getDirectDeclarator(),
                [&self](auto&& value) -> std::vector<cld::Lexer::Token>::const_iterator { return self(value); });
        });
}

cld::Semantics::RecordType::RecordType(std::string name, bool isUnion,
                                       std::vector<std::tuple<Type, std::string, std::int64_t>>&& names)
    : m_name(std::move(name)), m_isUnion(isUnion), m_members(std::move(names))
{
}

bool cld::Semantics::RecordType::isUnion() const
{
    return m_isUnion;
}

const std::vector<std::tuple<cld::Semantics::Type, std::string, std::int64_t>>&
    cld::Semantics::RecordType::getMembers() const
{
    return m_members;
}

bool cld::Semantics::RecordType::isDefinition() const
{
    return !m_members.empty();
}

cld::Semantics::Type
    cld::Semantics::RecordType::create(bool isConst, bool isVolatile, bool isUnion, const std::string& name,
                                       std::vector<std::tuple<cld::Semantics::Type, std::string, int64_t>>&& members)
{
    return cld::Semantics::Type(isConst, isVolatile, (isUnion ? "union " : "struct ") + name,
                                RecordType(name, isUnion, std::move(members)));
}

bool cld::Semantics::RecordType::operator==(const cld::Semantics::RecordType& rhs) const
{
    return std::tie(m_isUnion, m_name) == std::tie(rhs.m_isUnion, rhs.m_name);
}

bool cld::Semantics::RecordType::operator!=(const cld::Semantics::RecordType& rhs) const
{
    return !(rhs == *this);
}

const std::string& cld::Semantics::RecordType::getName() const
{
    return m_name;
}

cld::Expected<std::size_t, std::string> cld::Semantics::alignmentOf(const cld::Semantics::Type& type,
                                                                    const LanguageOptions& options)
{
    return match(
        type.get(),
        [](const PrimitiveType& primitiveType) -> Expected<std::size_t, std::string> {
            return primitiveType.getByteCount();
        },
        [&options](const ArrayType& arrayType) -> Expected<std::size_t, std::string> {
            return alignmentOf(arrayType.getType(), options);
        },
        [&type](const AbstractArrayType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(type.getFullFormattedTypeName());
        },
        [&options](const ValArrayType& valArrayType) -> Expected<std::size_t, std::string> {
            return alignmentOf(valArrayType.getType(), options);
        },
        [](const FunctionType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF;
        },
        [&type, &options](const RecordType& recordType) -> Expected<std::size_t, std::string> {
            if (recordType.getMembers().empty())
            {
                return ErrorMessages::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(
                    type.getFullFormattedTypeName());
            }
            if (!recordType.isUnion())
            {
                std::size_t currentAlignment = 0;
                for (auto& [subtype, name, bits] : recordType.getMembers())
                {
                    (void)name;
                    (void)bits;
                    auto result = alignmentOf(subtype, options);
                    if (!result)
                    {
                        return result;
                    }
                    currentAlignment = std::max(currentAlignment, *result);
                }
                return currentAlignment;
            }
            else
            {
                std::optional<std::string> failure;
                auto result = std::max_element(recordType.getMembers().begin(), recordType.getMembers().end(),
                                               [&failure, &options](const auto& lhs, const auto& rhs) {
                                                   auto lhsSize = sizeOf(std::get<0>(lhs), options);
                                                   if (!lhsSize)
                                                   {
                                                       failure = lhsSize.error();
                                                   }
                                                   auto rhsSize = sizeOf(std::get<0>(rhs), options);
                                                   if (!rhsSize)
                                                   {
                                                       failure = rhsSize.error();
                                                   }
                                                   return (lhsSize ? *lhsSize : 0) < (rhsSize ? *rhsSize : 0);
                                               });
                if (failure)
                {
                    return *failure;
                }
                return alignmentOf(std::get<0>(*result), options);
            }
        },
        [&options](const EnumType&) -> Expected<std::size_t, std::string> {
            return std::size_t{options.getSizeOfInt()};
        },
        [&options](const PointerType&) -> Expected<std::size_t, std::string> {
            return std::size_t{options.getSizeOfVoidStar()};
        },
        [](std::monostate) -> Expected<std::size_t, std::string> { CLD_UNREACHABLE; });
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

cld::Expected<std::size_t, std::string> cld::Semantics::sizeOf(const cld::Semantics::Type& type,
                                                               const LanguageOptions& options)
{
    return match(
        type.get(),
        [](const PrimitiveType& primitiveType) -> Expected<std::size_t, std::string> {
            return primitiveType.getByteCount();
        },
        [&options](const ArrayType& arrayType) -> Expected<std::size_t, std::string> {
            auto result = sizeOf(arrayType.getType(), options);
            if (!result)
            {
                return result;
            }
            return *result * arrayType.getSize();
        },
        [&type](const AbstractArrayType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(type.getFullFormattedTypeName());
        },
        [](const ValArrayType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION;
        },
        [](const FunctionType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF;
        },
        [&type, &options](const RecordType& recordType) -> Expected<std::size_t, std::string> {
            if (recordType.getMembers().empty())
            {
                return ErrorMessages::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(type.getFullFormattedTypeName());
            }
            if (!recordType.isUnion())
            {
                std::size_t currentSize = 0;
                for (auto& [subtype, name, bits] : recordType.getMembers())
                {
                    (void)name;
                    (void)bits;
                    auto alignment = alignmentOf(subtype, options);
                    if (!alignment)
                    {
                        return alignment;
                    }
                    auto rest = currentSize % *alignment;
                    if (rest != 0)
                    {
                        currentSize += *alignment - rest;
                    }
                    auto subSize = sizeOf(subtype, options);
                    if (!subSize)
                    {
                        return subSize;
                    }
                    currentSize += *subSize;
                }
                return currentSize;
            }
            else
            {
                std::size_t maxSize = 0;
                for (auto& [subtype, name, bits] : recordType.getMembers())
                {
                    (void)name;
                    (void)bits;
                    auto subSize = sizeOf(subtype, options);
                    if (!subSize)
                    {
                        return subSize;
                    }
                    maxSize = std::max(maxSize, *subSize);
                }
                return maxSize;
            }
        },
        [&options](const EnumType&) -> Expected<std::size_t, std::string> {
            return std::size_t{options.getSizeOfInt()};
        },
        [&options](const PointerType&) -> Expected<std::size_t, std::string> {
            return std::size_t{options.getSizeOfVoidStar()};
        },
        [](std::monostate) -> Expected<std::size_t, std::string> { CLD_UNREACHABLE; });
}

cld::Semantics::FunctionDefinition::FunctionDefinition(FunctionType type, std::string name,
                                                       std::vector<Declaration> parameterDeclarations, Linkage linkage,
                                                       CompoundStatement&& compoundStatement)
    : m_type(std::move(type)),
      m_name(std::move(name)),
      m_parameterDeclarations(std::move(parameterDeclarations)),
      m_linkage(linkage),
      m_compoundStatement(std::move(compoundStatement))
{
}

const cld::Semantics::FunctionType& cld::Semantics::FunctionDefinition::getType() const
{
    return m_type;
}

bool cld::Semantics::FunctionDefinition::hasPrototype() const
{
    return m_type.hasPrototype();
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

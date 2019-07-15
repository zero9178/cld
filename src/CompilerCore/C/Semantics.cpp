#include "Semantics.hpp"

#include <algorithm>
#include <map>
#include <optional>
#include <utility>

#include "CompilerCore/Common/Util.hpp"

#include "ErrorMessages.hpp"
#include "Syntax.hpp"

const OpenCL::Semantics::Type& OpenCL::Semantics::ArrayType::getType() const
{
    return *m_type;
}

std::size_t OpenCL::Semantics::ArrayType::getSize() const
{
    return m_size;
}

OpenCL::Semantics::ArrayType::ArrayType(bool isRestricted, std::shared_ptr<OpenCL::Semantics::Type>&& type,
                                        std::size_t size)
    : m_restricted(isRestricted), m_type(std::move(type)), m_size(size)
{
}

bool OpenCL::Semantics::ArrayType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Semantics::Type OpenCL::Semantics::ArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                             OpenCL::Semantics::Type&& type, std::size_t size)
{
    std::ostringstream ss;
    ss << size;
    auto name = type.getName() + "[" + ss.str() + "]";
    return OpenCL::Semantics::Type(isConst, isVolatile, name,
                                   ArrayType(isRestricted, std::make_shared<Type>(std::move(type)), size));
}

bool OpenCL::Semantics::ArrayType::operator==(const OpenCL::Semantics::ArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type, m_size) == std::tie(rhs.m_restricted, *rhs.m_type, rhs.m_size);
}

bool OpenCL::Semantics::ArrayType::operator!=(const OpenCL::Semantics::ArrayType& rhs) const
{
    return !(rhs == *this);
}

bool OpenCL::Semantics::Type::isConst() const
{
    return m_isConst;
}

bool OpenCL::Semantics::Type::isVolatile() const
{
    return m_isVolatile;
}

const std::string& OpenCL::Semantics::Type::getName() const
{
    return m_name;
}

void OpenCL::Semantics::Type::setName(const std::string& name)
{
    m_name = name;
}

const OpenCL::Semantics::Type::variant& OpenCL::Semantics::Type::get() const
{
    return m_type;
}

OpenCL::Semantics::Type::Type(bool isConst, bool isVolatile, std::string name, OpenCL::Semantics::Type::variant&& type)
    : m_isConst(isConst), m_isVolatile(isVolatile), m_name(std::move(name)), m_typeName(m_name), m_type(std::move(type))
{
}

bool OpenCL::Semantics::Type::operator==(const OpenCL::Semantics::Type& rhs) const
{
    return std::tie(m_isConst, m_isVolatile, m_type) == std::tie(rhs.m_isConst, rhs.m_isVolatile, rhs.m_type);
}

bool OpenCL::Semantics::Type::operator!=(const OpenCL::Semantics::Type& rhs) const
{
    return !(rhs == *this);
}

bool OpenCL::Semantics::Type::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_type);
}
const std::string& OpenCL::Semantics::Type::getTypeName() const
{
    return m_typeName;
}

bool OpenCL::Semantics::Type::isTypedef() const
{
    return m_typeName != m_name;
}
std::string OpenCL::Semantics::Type::getFullFormattedTypeName() const
{
    return '\'' + m_name + '\'' + (isTypedef() ? "(aka '" + m_typeName + "')" : "");
}

OpenCL::Semantics::PointerType::PointerType(bool isRestricted, std::shared_ptr<OpenCL::Semantics::Type>&& elementType)
    : m_restricted(isRestricted), m_elementType(std::move(elementType))
{
}

const OpenCL::Semantics::Type& OpenCL::Semantics::PointerType::getElementType() const
{
    return *m_elementType;
}

bool OpenCL::Semantics::PointerType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Semantics::Type OpenCL::Semantics::PointerType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                               OpenCL::Semantics::Type&& elementType)
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
    return OpenCL::Semantics::Type(isConst, isVolatile, name,
                                   PointerType(isRestricted, std::make_shared<Type>(std::move(elementType))));
}

bool OpenCL::Semantics::PointerType::operator==(const OpenCL::Semantics::PointerType& rhs) const
{
    return std::tie(m_restricted, *m_elementType) == std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool OpenCL::Semantics::PointerType::operator!=(const OpenCL::Semantics::PointerType& rhs) const
{
    return !(rhs == *this);
}

OpenCL::Semantics::EnumType::EnumType(std::string name, std::vector<std::pair<std::string, std::int32_t>> values)
    : m_name(std::move(name)), m_values(std::move(values))
{
}

const std::vector<std::pair<std::string, int32_t>>& OpenCL::Semantics::EnumType::getValues() const
{
    return m_values;
}

bool OpenCL::Semantics::EnumType::isDefinition() const
{
    return !m_values.empty();
}

bool OpenCL::Semantics::EnumType::isAnonymous() const
{
    return m_name.empty();
}

OpenCL::Semantics::Type OpenCL::Semantics::EnumType::create(bool isConst, bool isVolatile, const std::string& name,
                                                            std::vector<std::pair<std::string, std::int32_t>> values)
{
    return OpenCL::Semantics::Type(isConst, isVolatile, "enum " + name, EnumType(name, std::move(values)));
}

bool OpenCL::Semantics::EnumType::operator==(const OpenCL::Semantics::EnumType& rhs) const
{
    return m_name == rhs.m_name;
}

bool OpenCL::Semantics::EnumType::operator!=(const OpenCL::Semantics::EnumType& rhs) const
{
    return !(rhs == *this);
}

const std::string& OpenCL::Semantics::EnumType::getName() const
{
    return m_name;
}

OpenCL::Semantics::PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount)
    : m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned), m_bitCount(bitCount)
{
}

bool OpenCL::Semantics::PrimitiveType::isFloatingPoint() const
{
    return m_isFloatingPoint;
}

bool OpenCL::Semantics::PrimitiveType::isSigned() const
{
    return m_isSigned;
}

std::uint8_t OpenCL::Semantics::PrimitiveType::getBitCount() const
{
    return m_bitCount;
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::create(bool isConst, bool isVolatile, bool isFloatingPoint,
                                                                 bool isSigned, std::uint8_t bitCount)
{
    return OpenCL::Semantics::Type(
        isConst, isVolatile,
        [=]() -> const char* {
            if (isFloatingPoint)
            {
                if (bitCount == 32)
                {
                    return "float";
                }
                else if (bitCount == 64)
                {
                    return "double";
                }
                return "";
            }
            switch (bitCount)
            {
                case 8: return isSigned ? "char" : "unsigned char";
                case 16: return isSigned ? "short" : "unsigned short";
                case 32: return isSigned ? "int" : "unsigned int";
                case 64: return isSigned ? "long long" : "unsigned long long";
                default: return "void";
            }
            return "";
        }(),
        PrimitiveType(isFloatingPoint, isSigned, bitCount));
}

bool OpenCL::Semantics::PrimitiveType::operator==(const OpenCL::Semantics::PrimitiveType& rhs) const
{
    if (m_bitCount == 0 && rhs.m_bitCount == 0)
    {
        return true;
    }
    return std::tie(m_isFloatingPoint, m_isSigned, m_bitCount)
           == std::tie(rhs.m_isFloatingPoint, rhs.m_isSigned, rhs.m_bitCount);
}

bool OpenCL::Semantics::PrimitiveType::operator!=(const OpenCL::Semantics::PrimitiveType& rhs) const
{
    return !(rhs == *this);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 8);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createUnsignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 8);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createShort(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 16);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createUnsignedShort(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 16);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createInt(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 32);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createUnsignedInt(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 32);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 64);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 64);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createFloat(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 32);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createDouble(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 64);
}

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::createVoid(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 0);
}

OpenCL::Semantics::ValArrayType::ValArrayType(bool isRestricted, std::shared_ptr<OpenCL::Semantics::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
{
}

const OpenCL::Semantics::Type& OpenCL::Semantics::ValArrayType::getType() const
{
    return *m_type;
}

bool OpenCL::Semantics::ValArrayType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Semantics::Type OpenCL::Semantics::ValArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                                OpenCL::Semantics::Type&& type)
{
    auto name = type.getName() + "[*]";
    return OpenCL::Semantics::Type(isConst, isVolatile, name,
                                   ValArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool OpenCL::Semantics::ValArrayType::operator==(const OpenCL::Semantics::ValArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool OpenCL::Semantics::ValArrayType::operator!=(const OpenCL::Semantics::ValArrayType& rhs) const
{
    return !(rhs == *this);
}

OpenCL::Semantics::FunctionType::FunctionType(std::shared_ptr<Type>&& returnType,
                                              std::vector<std::pair<Type, std::string>> arguments, bool lastIsVararg,
                                              bool hasPrototype)
    : m_returnType(std::move(returnType)),
      m_arguments(std::move(arguments)),
      m_lastIsVararg(lastIsVararg),
      m_hasPrototype(hasPrototype)
{
}

const OpenCL::Semantics::Type& OpenCL::Semantics::FunctionType::getReturnType() const
{
    return *m_returnType;
}

const std::vector<std::pair<OpenCL::Semantics::Type, std::string>>&
    OpenCL::Semantics::FunctionType::getArguments() const
{
    return m_arguments;
}

bool OpenCL::Semantics::FunctionType::isLastVararg() const
{
    return m_lastIsVararg;
}

OpenCL::Semantics::Type OpenCL::Semantics::FunctionType::create(OpenCL::Semantics::Type&& returnType,
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
    return OpenCL::Semantics::Type(
        false, false, argumentsNames,
        FunctionType(std::make_shared<Type>(std::move(returnType)), std::move(arguments), lastIsVararg, hasPrototype));
}

bool OpenCL::Semantics::FunctionType::operator==(const OpenCL::Semantics::FunctionType& rhs) const
{
    std::vector<Type> thisTypes, rhsTypes;
    auto pairFirst = [](const auto& pair) { return pair.first; };
    std::transform(m_arguments.begin(), m_arguments.end(), std::back_inserter(thisTypes), pairFirst);
    std::transform(rhs.m_arguments.begin(), rhs.m_arguments.end(), std::back_inserter(rhsTypes), pairFirst);
    return std::tie(*m_returnType, thisTypes, m_lastIsVararg)
           == std::tie(*rhs.m_returnType, rhsTypes, rhs.m_lastIsVararg);
}

bool OpenCL::Semantics::FunctionType::operator!=(const OpenCL::Semantics::FunctionType& rhs) const
{
    return !(rhs == *this);
}

bool OpenCL::Semantics::FunctionType::hasPrototype() const
{
    return m_hasPrototype;
}

OpenCL::Semantics::AbstractArrayType::AbstractArrayType(bool isRestricted,
                                                        std::shared_ptr<OpenCL::Semantics::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
{
}

const OpenCL::Semantics::Type& OpenCL::Semantics::AbstractArrayType::getType() const
{
    return *m_type;
}

bool OpenCL::Semantics::AbstractArrayType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Semantics::Type OpenCL::Semantics::AbstractArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                                     OpenCL::Semantics::Type&& type)
{
    auto name = type.getName() + "[]";
    return OpenCL::Semantics::Type(isConst, isVolatile, name,
                                   AbstractArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool OpenCL::Semantics::AbstractArrayType::operator==(const OpenCL::Semantics::AbstractArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool OpenCL::Semantics::AbstractArrayType::operator!=(const OpenCL::Semantics::AbstractArrayType& rhs) const
{
    return !(rhs == *this);
}

std::string OpenCL::Semantics::declaratorToName(const OpenCL::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> std::string { return name.getIdentifier(); },
        [](auto&& self, const Syntax::DirectDeclaratorParenthese& declarator) -> std::string {
            return std::visit([&self](auto&& value) -> std::string { return self(value); },
                              declarator.getDeclarator().getDirectDeclarator());
        },
        [](auto&& self, auto&& value) -> std::string {
            return std::visit([&self](auto&& value) -> std::string { return self(value); },
                              value.getDirectDeclarator());
        });
}

std::vector<OpenCL::Lexer::Token>::const_iterator
    OpenCL::Semantics::declaratorToLoc(const OpenCL::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name)
            -> std::vector<OpenCL::Lexer::Token>::const_iterator { return name.getIdentifierLoc(); },
        [](auto&& self,
           const Syntax::DirectDeclaratorParenthese& declarator) -> std::vector<OpenCL::Lexer::Token>::const_iterator {
            return std::visit(
                [&self](auto&& value) -> std::vector<OpenCL::Lexer::Token>::const_iterator { return self(value); },
                declarator.getDeclarator().getDirectDeclarator());
        },
        [](auto&& self, auto&& value) -> std::vector<OpenCL::Lexer::Token>::const_iterator {
            return std::visit(
                [&self](auto&& value) -> std::vector<OpenCL::Lexer::Token>::const_iterator { return self(value); },
                value.getDirectDeclarator());
        });
}

OpenCL::Semantics::RecordType::RecordType(std::string name, bool isUnion,
                                          std::vector<std::tuple<Type, std::string, std::int64_t>>&& names)
    : m_name(std::move(name)), m_isUnion(isUnion), m_members(std::move(names))
{
}

bool OpenCL::Semantics::RecordType::isUnion() const
{
    return m_isUnion;
}

const std::vector<std::tuple<OpenCL::Semantics::Type, std::string, std::int64_t>>&
    OpenCL::Semantics::RecordType::getMembers() const
{
    return m_members;
}

bool OpenCL::Semantics::RecordType::isDefinition() const
{
    return !m_members.empty();
}

OpenCL::Semantics::Type OpenCL::Semantics::RecordType::create(
    bool isConst, bool isVolatile, bool isUnion, const std::string& name,
    std::vector<std::tuple<OpenCL::Semantics::Type, std::string, int64_t>>&& members)
{
    return OpenCL::Semantics::Type(isConst, isVolatile, (isUnion ? "union " : "struct ") + name,
                                   RecordType(name, isUnion, std::move(members)));
}

bool OpenCL::Semantics::RecordType::operator==(const OpenCL::Semantics::RecordType& rhs) const
{
    return std::tie(m_isUnion, m_name) == std::tie(rhs.m_isUnion, rhs.m_name);
}

bool OpenCL::Semantics::RecordType::operator!=(const OpenCL::Semantics::RecordType& rhs) const
{
    return !(rhs == *this);
}

const std::string& OpenCL::Semantics::RecordType::getName() const
{
    return m_name;
}

OpenCL::Expected<std::size_t, std::string> OpenCL::Semantics::alignmentOf(const OpenCL::Semantics::Type& type)
{
    return match(
        type.get(),
        [](const PrimitiveType& primitiveType) -> Expected<std::size_t, std::string> {
            return primitiveType.getBitCount() / 8;
        },
        [](const ArrayType& arrayType) -> Expected<std::size_t, std::string> {
            return alignmentOf(arrayType.getType());
        },
        [&type](const AbstractArrayType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(type.getFullFormattedTypeName());
        },
        [](const ValArrayType& valArrayType) -> Expected<std::size_t, std::string> {
            return alignmentOf(valArrayType.getType());
        },
        [](const FunctionType&) -> Expected<std::size_t, std::string> {
            return ErrorMessages::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF;
        },
        [&type](const RecordType& recordType) -> Expected<std::size_t, std::string> {
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
                    auto result = alignmentOf(subtype);
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
                                               [&failure](const auto& lhs, const auto& rhs) {
                                                   auto lhsSize = sizeOf(std::get<0>(lhs));
                                                   if (!lhsSize)
                                                   {
                                                       failure = lhsSize.error();
                                                   }
                                                   auto rhsSize = sizeOf(std::get<0>(rhs));
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
                return alignmentOf(std::get<0>(*result));
            }
        },
        [](const EnumType&) -> Expected<std::size_t, std::string> { return 4; },
        [](const PointerType&) -> Expected<std::size_t, std::string> { return 8; },
        [](std::monostate) -> Expected<std::size_t, std::string> { return 0; });
}

bool OpenCL::Semantics::isVoid(const OpenCL::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.get());
    if (!primitive)
    {
        return false;
    }
    return primitive->getBitCount() == 0;
}

OpenCL::Expected<std::size_t, std::string> OpenCL::Semantics::sizeOf(const OpenCL::Semantics::Type& type)
{
    return match(
        type.get(),
        [](const PrimitiveType& primitiveType) -> Expected<std::size_t, std::string> {
            return primitiveType.getBitCount() / 8;
        },
        [](const ArrayType& arrayType) -> Expected<std::size_t, std::string> {
            auto result = sizeOf(arrayType.getType());
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
        [&type](const RecordType& recordType) -> Expected<std::size_t, std::string> {
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
                    auto alignment = alignmentOf(subtype);
                    if (!alignment)
                    {
                        return alignment;
                    }
                    auto rest = currentSize % *alignment;
                    if (rest != 0)
                    {
                        currentSize += *alignment - rest;
                    }
                    auto subSize = sizeOf(subtype);
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
                    auto subSize = sizeOf(subtype);
                    if (!subSize)
                    {
                        return subSize;
                    }
                    maxSize = std::max(maxSize, *subSize);
                }
                return maxSize;
            }
        },
        [](const EnumType&) -> Expected<std::size_t, std::string> { return 4; },
        [](const PointerType&) -> Expected<std::size_t, std::string> { return 8; },
        [](std::monostate) -> Expected<std::size_t, std::string> { return 0; });
}

OpenCL::Semantics::FunctionDefinition::FunctionDefinition(FunctionType type, std::string name,
                                                          std::vector<Declaration> parameterDeclarations,
                                                          Linkage linkage, CompoundStatement&& compoundStatement)
    : m_type(std::move(type)),
      m_name(std::move(name)),
      m_parameterDeclarations(std::move(parameterDeclarations)),
      m_linkage(linkage),
      m_compoundStatement(std::move(compoundStatement))
{
}

const OpenCL::Semantics::FunctionType& OpenCL::Semantics::FunctionDefinition::getType() const
{
    return m_type;
}

bool OpenCL::Semantics::FunctionDefinition::hasPrototype() const
{
    return m_type.hasPrototype();
}

const std::string& OpenCL::Semantics::FunctionDefinition::getName() const
{
    return m_name;
}

OpenCL::Semantics::Linkage OpenCL::Semantics::FunctionDefinition::getLinkage() const
{
    return m_linkage;
}

const std::vector<OpenCL::Semantics::Declaration>&
    OpenCL::Semantics::FunctionDefinition::getParameterDeclarations() const
{
    return m_parameterDeclarations;
}

OpenCL::Semantics::TranslationUnit::TranslationUnit(std::vector<TranslationUnit::Variant> globals)
    : m_globals(std::move(globals))
{
}

const std::vector<OpenCL::Semantics::TranslationUnit::Variant>& OpenCL::Semantics::TranslationUnit::getGlobals() const
{
    return m_globals;
}

OpenCL::Semantics::Declaration::Declaration(OpenCL::Semantics::Type type, OpenCL::Semantics::Linkage linkage,
                                            OpenCL::Semantics::Lifetime lifetime, std::string name)
    : m_type(std::move(type)), m_linkage(linkage), m_lifetime(lifetime), m_name(std::move(name))
{
}

const OpenCL::Semantics::Type& OpenCL::Semantics::Declaration::getType() const
{
    return m_type;
}

OpenCL::Semantics::Linkage OpenCL::Semantics::Declaration::getLinkage() const
{
    return m_linkage;
}

OpenCL::Semantics::Lifetime OpenCL::Semantics::Declaration::getLifetime() const
{
    return m_lifetime;
}

const std::string& OpenCL::Semantics::Declaration::getName() const
{
    return m_name;
}

OpenCL::Semantics::CompoundStatement::CompoundStatement(std::vector<std::variant<Statement, Declaration>> compoundItems)
    : m_compoundItems(std::move(compoundItems))
{
}

const std::vector<std::variant<OpenCL::Semantics::Statement, OpenCL::Semantics::Declaration>>&
    OpenCL::Semantics::CompoundStatement::getCompoundItems() const
{
    return m_compoundItems;
}

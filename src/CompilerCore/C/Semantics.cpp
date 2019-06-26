#include "Semantics.hpp"

#include "ConstantEvaluator.hpp"
#include "Syntax.hpp"
#include <algorithm>
#include <map>
#include <optional>
#include <sstream>
#include <utility>
#include <cassert>
#include <array>

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

OpenCL::Semantics::Type OpenCL::Semantics::ArrayType::create(bool isConst, bool isVolatile,
                                                             bool isRestricted,
                                                             OpenCL::Semantics::Type&& type,
                                                             std::size_t size)
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

OpenCL::Semantics::Type::Type(bool isConst, bool isVolatile, std::string name,
                              OpenCL::Semantics::Type::variant&& type)
    : m_isConst(isConst), m_isVolatile(isVolatile), m_name(std::move(name)), m_type(std::move(type))
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

bool OpenCL::Semantics::Type::isCompatibleWith(const OpenCL::Semantics::Type& rhs) const
{
    return m_type == rhs.m_type;
}

OpenCL::Semantics::PointerType::PointerType(bool isRestricted,
                                            std::shared_ptr<OpenCL::Semantics::Type>&& elementType)
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

OpenCL::Semantics::Type OpenCL::Semantics::PointerType::create(bool isConst, bool isVolatile,
                                                               bool isRestricted,
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

OpenCL::Semantics::EnumType::EnumType(std::string name,
                                      std::vector<std::pair<std::string, std::int32_t>> values)
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

OpenCL::Semantics::Type
OpenCL::Semantics::EnumType::create(bool isConst, bool isVolatile, const std::string& name,
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

OpenCL::Semantics::Type OpenCL::Semantics::PrimitiveType::create(bool isConst, bool isVolatile,
                                                                 bool isFloatingPoint, bool isSigned,
                                                                 std::uint8_t bitCount)
{
    return OpenCL::Semantics::Type(isConst, isVolatile,
                                   [=]() -> const char*
                                   {
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
                                       case 8:return isSigned ? "char" : "unsigned char";
                                       case 16:return isSigned ? "short" : "unsigned short";
                                       case 32:return isSigned ? "int" : "unsigned int";
                                       case 64:return isSigned ? "long long" : "unsigned long long";
                                       default:return "void";
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

OpenCL::Semantics::ValArrayType::ValArrayType(bool isRestricted,
                                              std::shared_ptr<OpenCL::Semantics::Type>&& type)
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

OpenCL::Semantics::Type OpenCL::Semantics::ValArrayType::create(bool isConst, bool isVolatile,
                                                                bool isRestricted,
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
                                              std::vector<std::pair<Type, std::string>> arguments,
                                              bool lastIsVararg,
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

const std::vector<std::pair<OpenCL::Semantics::Type,
                            std::string>>& OpenCL::Semantics::FunctionType::getArguments() const
{
    return m_arguments;
}

bool OpenCL::Semantics::FunctionType::isLastVararg() const
{
    return m_lastIsVararg;
}

OpenCL::Semantics::Type
OpenCL::Semantics::FunctionType::create(OpenCL::Semantics::Type&& returnType,
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
    auto pairFirst = [](const auto& pair)
    { return pair.first; };
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

OpenCL::Semantics::Type OpenCL::Semantics::AbstractArrayType::create(bool isConst, bool isVolatile,
                                                                     bool isRestricted,
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

namespace
{
    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&& ... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G)->Y<G>;

    template <class... Ts>
    struct overload : Ts ...
    {
        using Ts::operator()...;
    };
    template <class... Ts>
    overload(Ts...)->overload<Ts...>;

    OpenCL::Expected<std::vector<std::pair<OpenCL::Semantics::Type, std::string>>,
                     OpenCL::FailureReason> parameterListToArguments(
        const std::vector<OpenCL::Syntax::ParameterDeclaration>& parameterDeclarations,
        const std::map<std::string, std::reference_wrapper<const OpenCL::Semantics::Type>>& typedefs,
        const std::vector<OpenCL::Syntax::Declaration>& declarations,
        const std::map<std::string, OpenCL::Semantics::RecordType>& structOrUnions)
    {
        std::vector<std::pair<OpenCL::Semantics::Type, std::string>> arguments;
        for (auto& pair :
            parameterDeclarations)
        {
            std::vector<OpenCL::Semantics::SpecifierQualifierRef>
                specifierQualifiers;
            for (auto& iter : pair.first)
            {
                auto optional = std::visit(overload{
                    [&specifierQualifiers](const OpenCL::Syntax::TypeSpecifier& typeSpecifier) -> std::optional<OpenCL::FailureReason>
                    {
                        specifierQualifiers.emplace_back(typeSpecifier);
                        return {};
                    },
                    [&specifierQualifiers](const OpenCL::Syntax::TypeQualifier& typeQualifier) -> std::optional<OpenCL::FailureReason>
                    {
                        specifierQualifiers.emplace_back(typeQualifier);
                        return {};
                    },
                    [](const OpenCL::Syntax::StorageClassSpecifier& storageClassSpecifier) -> std::optional<OpenCL::FailureReason>
                    {
                        if (storageClassSpecifier.getSpecifier() != OpenCL::Syntax::StorageClassSpecifier::Register)
                        {
                            return OpenCL::FailureReason(
                                "No storage class specifiers except register allowed for function argument");
                        }
                        return {};
                    },
                    [](OpenCL::Syntax::FunctionSpecifier) -> std::optional<OpenCL::FailureReason>
                    {
                        return OpenCL::FailureReason("Inline not allowed in paramter type list");
                    }}, iter);
                if (optional)
                {
                    return *optional;
                }
            }
            auto result = declaratorsToType(
                specifierQualifiers,
                std::visit(
                    overload{
                        [](const std::unique_ptr<OpenCL::Syntax::Declarator>& directDeclarator) -> OpenCL::Semantics::PossiblyAbstractQualifierRef
                        {
                            return std::cref(*directDeclarator);
                        },
                        [](const std::unique_ptr<OpenCL::Syntax::AbstractDeclarator>& abstractDeclarator) -> OpenCL::Semantics::PossiblyAbstractQualifierRef
                        {
                            return abstractDeclarator.get();
                        }},
                    pair.second),
                typedefs, declarations,
                structOrUnions);
            if (!result)
            {
                return result.error();
            }
            if (parameterDeclarations.size() == 1
                && *result == OpenCL::Semantics::PrimitiveType::createVoid(false, false)
                && !std::visit([](const auto& uniquePtr) -> bool
                               { return uniquePtr.get(); }, pair.second))
            {
                break;
            }
            if (isVoid(*result))
            {
                return OpenCL::FailureReason("Parameter is not allowed to have void datatype");
            }
            arguments.emplace_back(std::move(*result),
                                   std::holds_alternative<std::unique_ptr<OpenCL::Syntax::Declarator>>(pair.second)
                                   ? OpenCL::Semantics::declaratorToName(*std::get<std::unique_ptr<OpenCL::Syntax::Declarator>>(
                                       pair.second)) : "");
        }
        return arguments;
    }

    OpenCL::Expected<OpenCL::Semantics::Type,
                     OpenCL::FailureReason> primitivesToType(std::vector<OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier> primitives,
                                                             bool isConst,
                                                             bool isVolatile)
    {
        enum
        {
            Void = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void),
            Char = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Char),
            Short = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Short),
            Int = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int),
            Long = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long),
            Float = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Float),
            Double = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Double),
            Signed = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Signed),
            Unsigned = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Unsigned),
        };
        std::array<std::size_t, 9> primitivesCount = {0};
        for (auto& iter : primitives)
        {
            primitivesCount[static_cast<std::size_t>(iter)]++;
        }
        if (primitivesCount[Void] > 1)
        {
            return OpenCL::FailureReason("void appearing more than once");
        }
        if (primitivesCount[Char] > 1)
        {
            return OpenCL::FailureReason("char appearing more than once");
        }
        if (primitivesCount[Short] > 1)
        {
            return OpenCL::FailureReason("short appearing more than once");
        }
        if (primitivesCount[Int] > 1)
        {
            return OpenCL::FailureReason("int appearing more than once");
        }
        if (primitivesCount[Float] > 1)
        {
            return OpenCL::FailureReason("float appearing more than once");
        }
        if (primitivesCount[Double] > 1)
        {
            return OpenCL::FailureReason("double appearing more than once");
        }
        if (primitivesCount[Signed] > 1)
        {
            return OpenCL::FailureReason("signed appearing more than once");
        }
        if (primitivesCount[Unsigned] > 1)
        {
            return OpenCL::FailureReason("unsigned appearing more than once");
        }
        if (primitivesCount[Long] > 2)
        {
            return OpenCL::FailureReason("long appearing more than twice");
        }
        bool hasSigned = primitivesCount[Signed];
        bool hasUnsigned = primitivesCount[Unsigned];
        if (hasSigned && hasUnsigned)
        {
            return OpenCL::FailureReason("Can't have both signed and unsigned");
        }
        if (primitivesCount[Void])
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i++ == Void)
                {
                    return false;
                }
                return count;
            }))
            {
                return OpenCL::FailureReason("Can't combine void with any other primitives");
            }
            return OpenCL::Semantics::PrimitiveType::createVoid(isConst, isVolatile);
        }
        if (primitivesCount[Float])
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i++ == Float)
                {
                    return false;
                }
                return count;
            }))
            {
                return OpenCL::FailureReason("Can't combine float with any other primitives");
            }
            return OpenCL::Semantics::PrimitiveType::createFloat(isConst, isVolatile);
        }
        if (primitivesCount[Double])
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i++ == Double)
                {
                    return false;
                }
                return count;
            }))
            {
                return OpenCL::FailureReason("Can't combine double with any other primitives");
            }
            return OpenCL::Semantics::PrimitiveType::createDouble(isConst, isVolatile);
        }
        if (primitivesCount[Char])
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i == Char || i == Signed || i == Unsigned)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
            {
                return OpenCL::FailureReason("Can only combine char with signed or unsigned");
            }
            if (hasUnsigned)
            {
                return OpenCL::Semantics::PrimitiveType::createUnsignedChar(isConst, isVolatile);
            }
            else
            {
                return OpenCL::Semantics::PrimitiveType::createChar(isConst, isVolatile);
            }
        }
        if (primitivesCount[Short])
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i == Short || i == Signed || i == Unsigned || i == Int)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
            {
                return OpenCL::FailureReason("Can only combine short with signed,unsigned or int");
            }
            if (hasUnsigned)
            {
                return OpenCL::Semantics::PrimitiveType::createUnsignedShort(isConst, isVolatile);
            }
            else
            {
                return OpenCL::Semantics::PrimitiveType::createShort(isConst, isVolatile);
            }
        }
        if (primitivesCount[Long] == 1)
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i == Signed || i == Unsigned || i == Int || i == Long)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
            {
                return OpenCL::FailureReason("Can only combine long with long, signed, unsigned and int");
            }
            if (hasUnsigned)
            {
                return OpenCL::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
            }
            else
            {
                return OpenCL::Semantics::PrimitiveType::createInt(isConst, isVolatile);
            }
        }
        if (primitivesCount[Long] == 2)
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i == Signed || i == Unsigned || i == Int || i == Long)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
            {
                return OpenCL::FailureReason("Can only combine long with long, signed, unsigned and int");
            }
            if (hasUnsigned)
            {
                return OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(isConst, isVolatile);
            }
            else
            {
                return OpenCL::Semantics::PrimitiveType::createLongLong(isConst, isVolatile);
            }
        }
        if (primitivesCount[Int])
        {
            std::size_t i = 0;
            if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool
            {
                if (i == Signed || i == Unsigned || i == Int)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
            {
                return OpenCL::FailureReason("Can only combine int with signed or unsigned");
            }
            if (hasUnsigned)
            {
                return OpenCL::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
            }
            else
            {
                return OpenCL::Semantics::PrimitiveType::createInt(isConst, isVolatile);
            }
        }
        if (hasSigned)
        {
            return OpenCL::Semantics::PrimitiveType::createInt(isConst, isVolatile);
        }
        else if (hasUnsigned)
        {
            return OpenCL::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
        }
        return OpenCL::FailureReason("Internal compiler error");
    }
} // namespace

OpenCL::Expected<OpenCL::Semantics::Type, OpenCL::FailureReason> OpenCL::Semantics::declaratorsToType(
    std::vector<SpecifierQualifierRef> specifierQualifiers, PossiblyAbstractQualifierRef declarator,
    const std::map<std::string, std::reference_wrapper<const Type>>& typedefs,
    const std::vector<Syntax::Declaration>& declarations,
    const std::map<std::string, Semantics::RecordType>& structOrUnions)
{
    using ThisReturnType = OpenCL::Expected<Type, OpenCL::FailureReason>;

    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for (auto& iter : specifierQualifiers)
    {
        if (auto* typeQualifier = std::get_if<std::reference_wrapper<const Syntax::TypeQualifier>>(&iter))
        {
            switch (typeQualifier->get().getQualifier())
            {
            case Syntax::TypeQualifier::Const:
            {
                isConst = true;
                break;
            }
            case Syntax::TypeQualifier::Restrict:
            {
                isRestricted = true;
                break;
            }
            case Syntax::TypeQualifier::Volatile:
            {
                isVolatile = true;
                break;
            }
            }
        }
    }

    std::vector<const Syntax::TypeSpecifier*> typeSpecifiers;
    std::transform(specifierQualifiers.begin(), specifierQualifiers.end(), std::back_inserter(typeSpecifiers),
                   [](const auto& value) -> const Syntax::TypeSpecifier*
                   {
                       auto* refPointer = std::get_if<std::reference_wrapper<const Syntax::TypeSpecifier>>(&value);
                       if (!refPointer)
                       {
                           return nullptr;
                       }
                       else
                       {
                           return &refPointer->get();
                       }
                   });
    typeSpecifiers.erase(std::remove(typeSpecifiers.begin(), typeSpecifiers.end(), nullptr), typeSpecifiers.end());

    if (typeSpecifiers.empty())
    {
        return FailureReason("At least one type specifier must be present");
    }
    auto primitiveResult = std::visit(
        overload{
            [&typeSpecifiers, isConst,
                isVolatile](Syntax::TypeSpecifier::PrimitiveTypeSpecifier) -> ThisReturnType
            {
                if (!std::all_of(typeSpecifiers.begin(), typeSpecifiers.end(), [](const auto pointer)
                {
                    return std::holds_alternative<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                        pointer->getVariant());
                }))
                {
                    return FailureReason(
                        "Primitive type specifiers mixed with struct, union, enum and typedef names");
                }
                std::vector<Syntax::TypeSpecifier::PrimitiveTypeSpecifier> primitiveTypeSpecifier;
                std::transform(typeSpecifiers.begin(), typeSpecifiers.end(),
                               std::back_inserter(primitiveTypeSpecifier), [](const auto pointer)
                               {
                                   return std::get<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                                       pointer->getVariant());
                               });

                return primitivesToType(std::move(primitiveTypeSpecifier), isConst, isVolatile);
            },
            [&typeSpecifiers, isConst, isVolatile, &typedefs, &declarations](
                const std::unique_ptr<Syntax::StructOrUnionSpecifier>& structOrUnion) -> ThisReturnType
            {
                if (typeSpecifiers.size() != 1)
                {
                    return FailureReason(std::string("Expected no further type specifiers after ")
                                             + (structOrUnion->isUnion() ? "union" : "struct") + " specifier");
                }
                if (structOrUnion->getStructDeclarations().empty())
                {
                    return RecordType::create(isConst, isVolatile, structOrUnion->isUnion(),
                                              structOrUnion->getIdentifier());
                }
                std::vector<std::tuple<Type, std::string, std::int64_t>> members;
                for (auto&[structSpecifiers, structDecls] : structOrUnion->getStructDeclarations())
                {
                    std::vector<SpecifierQualifierRef> refs;
                    for (auto& iter : structSpecifiers)
                    {
                        std::visit([&refs](auto&& value)
                                   { refs.push_back(value); }, iter);
                    }
                    for (auto& iter : structDecls)
                    {
                        auto type = declaratorsToType(refs,
                                                      [&]() -> PossiblyAbstractQualifierRef
                                                      {
                                                          if (iter.first)
                                                          {
                                                              return *iter.first;
                                                          }
                                                          else
                                                          {
                                                              return nullptr;
                                                          }
                                                      }(),
                                                      typedefs, declarations);
                        if (!type)
                        {
                            return type;
                        }
                        std::string name = iter.first ? declaratorToName(*iter.first) : "";
                        members.emplace_back(std::move(*type), std::move(name), iter.second);
                    }
                }
                return RecordType::create(isConst, isVolatile, structOrUnion->isUnion(),
                                          structOrUnion->getIdentifier(), std::move(members));
            },
            [&typeSpecifiers, isConst,
                isVolatile](const std::unique_ptr<Syntax::EnumSpecifier>& enumSpecifier) -> ThisReturnType
            {
                if (typeSpecifiers.size() != 1)
                {
                    return FailureReason("Expected no further type specifiers after enum");
                }
                std::vector<std::pair<std::string, std::int32_t>> values;
                auto name = std::visit(overload{[](const std::string& name)
                                                { return name; },
                                                [&values](const Syntax::EnumDeclaration& declaration)
                                                {
                                                    values = declaration.getValues();
                                                    return declaration.getName();
                                                }},
                                       enumSpecifier->getVariant());
                return EnumType::create(isConst, isVolatile, name, std::move(values));
            },
            [&](const std::string& typedefName) -> ThisReturnType
            {
                auto result = typedefs.find(typedefName);
                if (result == typedefs.end())
                {
                    return FailureReason("Could not find typedef of name " + typedefName);
                }
                Type copy = result->second;
                copy.setName(typedefName);
                return copy;
            }},
        typeSpecifiers[0]->getVariant());
    if (!primitiveResult)
    {
        return primitiveResult;
    }
    auto baseType = *primitiveResult;
    if (isRestricted)
    {
        return FailureReason("Only pointers can be restricted");
    }

    auto getQualifiers = [](const std::vector<Syntax::TypeQualifier>& typeQualifiers) -> std::tuple<bool, bool, bool>
    {
        bool isConst = false;
        bool isVolatile = false;
        bool isRestricted = false;
        for (auto& typeQual : typeQualifiers)
        {
            switch (typeQual.getQualifier())
            {
            case Syntax::TypeQualifier::Const: isConst = true;
                break;
            case Syntax::TypeQualifier::Restrict: isRestricted = true;
                break;
            case Syntax::TypeQualifier::Volatile: isVolatile = true;
                break;
            default: break;
            }
        }
        return std::tie(isConst, isVolatile, isRestricted);
    };

    auto result = std::visit(
        Y{overload{[&](auto&& self,
                       const Syntax::AbstractDeclarator* abstractDeclarator) -> std::optional<FailureReason>
                   {
                       if (!abstractDeclarator)
                       {
                           return {};
                       }
                       for (auto& iter : abstractDeclarator->getPointers())
                       {
                           auto[isConst, isVolatile, isRestricted] = getQualifiers(iter.getTypeQualifiers());
                           baseType = PointerType::create(isConst, isVolatile, isRestricted, std::move(baseType));
                       }
                       return std::visit(
                           Y{overload{
                               [&](auto&&, const Syntax::DirectAbstractDeclaratorParenthese& abstractDeclarator)
                                   -> std::optional<FailureReason>
                               {
                                   // Might need to watch out for 6.7.5.3.11 not sure yet
                                   return self(&abstractDeclarator.getAbstractDeclarator());
                               },
                               [&](auto&& directSelf,
                                   const Syntax::DirectAbstractDeclaratorAssignmentExpression&
                                   directAbstractDeclaratorAssignmentExpression) -> std::optional<FailureReason>
                               {
                                   if (!directAbstractDeclaratorAssignmentExpression.getAssignmentExpression())
                                   {
                                       baseType = AbstractArrayType::create(false, false, false, std::move(baseType));
                                   }
                                   else
                                   {
                                       Semantics::ConstantEvaluator evaluator(structOrUnions, typedefs);
                                       auto result = evaluator.visit(
                                           *directAbstractDeclaratorAssignmentExpression.getAssignmentExpression());
                                       if (!result)
                                       {
                                           baseType = ValArrayType::create(false, false, false, std::move(baseType));
                                       }
                                       else
                                       {
                                           auto size = std::visit(
                                               [](auto&& value) -> std::optional<std::size_t>
                                               {
                                                   using T = std::decay_t<decltype(value)>;
                                                   if constexpr (std::is_convertible_v<T, std::size_t>)
                                                   {
                                                       return value;
                                                   }
                                                   return {};
                                               },
                                               *result);
                                           if (!size)
                                           {
                                               return FailureReason("Invalid type in result of constant expression");
                                           }
                                           baseType =
                                               ArrayType::create(false, false, false, std::move(baseType), *size);
                                       }
                                   }
                                   if (directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator())
                                   {
                                       return std::visit(
                                           [&directSelf](auto&& value) -> std::optional<FailureReason>
                                           {
                                               return directSelf(value);
                                           },
                                           *directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator());
                                   }
                                   return {};
                               },
                               [&](auto&& directSelf,
                                   const Syntax::DirectAbstractDeclaratorAsterisk& directAbstractDeclarator)
                                   -> std::optional<FailureReason>
                               {
                                   baseType = ValArrayType::create(false, false, false, std::move(baseType));
                                   if (directAbstractDeclarator.getDirectAbstractDeclarator())
                                   {
                                       return std::visit(
                                           [&directSelf](auto&& value) -> std::optional<FailureReason>
                                           {
                                               return directSelf(value);
                                           },
                                           *directAbstractDeclarator.getDirectAbstractDeclarator());
                                   }
                                   return {};
                               },
                               [&](auto&& directSelf,
                                   const Syntax::DirectAbstractDeclaratorParameterTypeList& parameterTypeList)
                                   -> std::optional<FailureReason>
                               {
                                   std::vector<std::pair<Type, std::string>> arguments;
                                   if (parameterTypeList.getParameterTypeList())
                                   {
                                       auto argumentsResult = parameterListToArguments(parameterTypeList
                                                                                           .getParameterTypeList()
                                                                                           ->getParameterList()
                                                                                           .getParameterDeclarations(),
                                                                                       typedefs,
                                                                                       declarations,
                                                                                       structOrUnions);
                                       if (!argumentsResult)
                                       {
                                           return argumentsResult.error();
                                       }
                                       arguments = std::move(*argumentsResult);
                                   }
                                   baseType = FunctionType::create(
                                       std::move(baseType), std::move(arguments),
                                       !parameterTypeList.getParameterTypeList()
                                           || parameterTypeList.getParameterTypeList()->hasEllipse(),
                                       true);
                                   if (parameterTypeList.getDirectAbstractDeclarator())
                                   {
                                       return std::visit(
                                           [&directSelf](auto&& value) -> std::optional<FailureReason>
                                           {
                                               return directSelf(value);
                                           },
                                           *parameterTypeList.getDirectAbstractDeclarator());
                                   }
                                   return {};
                               }}},
                           abstractDeclarator->getDirectAbstractDeclarator());
                   },
                   [&](auto&& self, std::reference_wrapper<const Syntax::Declarator> declarator) -> std::optional<
                       FailureReason>
                   {
                       for (auto& iter : declarator.get().getPointers())
                       {
                           auto[isConst, isVolatile, isRestricted] =
                           getQualifiers(iter.getTypeQualifiers());
                           baseType = PointerType::create(isConst, isVolatile,
                                                          isRestricted,
                                                          std::move(baseType));
                       }
                       return std::visit(Y{overload{
                                             [&](auto&&, const Syntax::DirectDeclaratorIdentifier&) -> std::optional<FailureReason>
                                             {
                                                 return {};
                                             },
                                             [&](auto&&, const Syntax::DirectDeclaratorParenthese& declarator)
                                                 -> std::optional<FailureReason>
                                             {
                                                 // Might need to watch out
                                                 // for 6.7.5.3.11 not sure yet
                                                 return self(std::cref(declarator.getDeclarator()));
                                             },
                                             [&](auto&& directSelf,
                                                 const Syntax::DirectDeclaratorNoStaticOrAsterisk& dirWithoutStaticOrAsterisk)
                                                 -> std::optional<FailureReason>
                                             {
                                                 auto[isConst, isVolatile,
                                                 isRestricted] = getQualifiers(dirWithoutStaticOrAsterisk.getTypeQualifiers());
                                                 if (!dirWithoutStaticOrAsterisk.getAssignmentExpression())
                                                 {
                                                     baseType = AbstractArrayType::create(isConst, isVolatile, isRestricted,
                                                                                          std::move(baseType));
                                                 }
                                                 else
                                                 {
                                                     Semantics::ConstantEvaluator evaluator(structOrUnions, typedefs);
                                                     auto result = evaluator.visit(*dirWithoutStaticOrAsterisk.getAssignmentExpression());
                                                     if (!result)
                                                     {
                                                         baseType = ValArrayType::create(isConst,
                                                                                         isVolatile,
                                                                                         isRestricted,
                                                                                         std::move(baseType));
                                                     }
                                                     else
                                                     {
                                                         auto size = std::visit([](auto&& value) -> std::optional<std::size_t>
                                                                                {
                                                                                    using T = std::decay_t<decltype(value)>;
                                                                                    if constexpr (std::is_convertible_v<T, std::size_t>)
                                                                                    {
                                                                                        return value;
                                                                                    }
                                                                                    return {};
                                                                                },
                                                                                *result);
                                                         if (!size)
                                                         {
                                                             return FailureReason("Invalid type in result of constant expression");
                                                         }
                                                         baseType = ArrayType::create(isConst,
                                                                                      isVolatile,
                                                                                      isRestricted,
                                                                                      std::move(baseType),
                                                                                      *size);
                                                     }
                                                 }
                                                 return std::visit([&directSelf](auto&& value) -> std::optional<FailureReason>
                                                                   {
                                                                       return directSelf(value);
                                                                   },
                                                                   dirWithoutStaticOrAsterisk.getDirectDeclarator());
                                             },
                                             [&](auto&& directSelf,
                                                 const Syntax::DirectDeclaratorStatic& directDeclaratorStatic) ->
                                                 std::optional<FailureReason>
                                             {
                                                 auto[isConst, isVolatile, isRestricted] =getQualifiers(directDeclaratorStatic
                                                                                                            .getTypeQualifiers());
                                                 Semantics::ConstantEvaluator evaluator(structOrUnions, typedefs);
                                                 auto result = evaluator.visit(directDeclaratorStatic.getAssignmentExpression());
                                                 if (!result)
                                                 {
                                                     baseType = ValArrayType::create(isConst,
                                                                                     isVolatile,
                                                                                     isRestricted,
                                                                                     std::move(baseType));
                                                 }
                                                 else
                                                 {
                                                     auto size = std::visit([](auto&& value) -> std::optional<std::size_t>
                                                                            {
                                                                                using T =
                                                                                std::decay_t<decltype(value)>;
                                                                                if constexpr (std::is_convertible_v<T, std::size_t>)
                                                                                {
                                                                                    return value;
                                                                                }
                                                                                return {};
                                                                            },
                                                                            *result);
                                                     if (!size)
                                                     {
                                                         return FailureReason("Invalid type in result of constant expression");
                                                     }
                                                     baseType = ArrayType::create(isConst,
                                                                                  isVolatile,
                                                                                  isRestricted,
                                                                                  std::move(baseType),
                                                                                  *size);
                                                 }
                                                 return std::visit([&directSelf](auto&& value) -> std::optional<FailureReason>
                                                                   {
                                                                       return directSelf(value);
                                                                   },
                                                                   directDeclaratorStatic.getDirectDeclarator());
                                             },
                                             [&](auto&& directSelf, const Syntax::DirectDeclaratorAsterisk& directDeclaratorAsterisk)
                                                 -> std::optional<FailureReason>
                                             {
                                                 auto[isConst, isVolatile, isRestricted] =getQualifiers(directDeclaratorAsterisk
                                                                                                            .getTypeQualifiers());
                                                 baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
                                                 return std::visit([&directSelf](auto&& value) -> std::optional<FailureReason>
                                                                   {
                                                                       return directSelf(value);
                                                                   },
                                                                   directDeclaratorAsterisk.getDirectDeclarator());
                                             },
                                             [&](auto&& directSelf,
                                                 const Syntax::DirectDeclaratorParentheseParameters& parentheseParameters)
                                                 -> std::optional<FailureReason>
                                             {
                                                 std::vector<std::pair<Type, std::string>> arguments;
                                                 auto& parameterDeclarations = parentheseParameters
                                                     .getParameterTypeList()
                                                     .getParameterList()
                                                     .getParameterDeclarations();
                                                 auto argumentResult = parameterListToArguments(parameterDeclarations,
                                                                                                typedefs,
                                                                                                declarations,
                                                                                                structOrUnions);
                                                 if (!argumentResult)
                                                 {
                                                     return argumentResult.error();
                                                 }
                                                 arguments = std::move(*argumentResult);
                                                 baseType = FunctionType::create(std::move(baseType),
                                                                                 std::move(arguments),
                                                                                 parentheseParameters.getParameterTypeList().hasEllipse(),
                                                                                 true);
                                                 return std::visit([&directSelf](auto&& value) -> std::optional<FailureReason>
                                                                   {
                                                                       return directSelf(value);
                                                                   },
                                                                   parentheseParameters.getDirectDeclarator());
                                             },
                                             [&](auto&& directSelf, const Syntax::DirectDeclaratorParentheseIdentifiers& identifiers)
                                                 -> std::optional<FailureReason>
                                             {
                                                 std::vector<std::pair<Type, std::string>> arguments(identifiers.getIdentifiers().size(),
                                                                                                     {PrimitiveType::create(false,
                                                                                                                            false,
                                                                                                                            false,
                                                                                                                            true,
                                                                                                                            32), ""});
                                                 std::map<std::string, Type> declarationMap;
                                                 for (auto& iter :declarations)
                                                 {
                                                     std::vector<SpecifierQualifierRef> refs;
                                                     for (auto& specifiers :iter.getDeclarationSpecifiers())
                                                     {
                                                         auto result = std::visit(
                                                             overload{
                                                                 [](Syntax::StorageClassSpecifier storageClassSpecifier)
                                                                     -> std::optional<FailureReason>
                                                                 {
                                                                     if (storageClassSpecifier.getSpecifier()
                                                                         != Syntax::StorageClassSpecifier::Register)
                                                                     {
                                                                         return OpenCL::FailureReason(
                                                                             "No storage class specifiers except register allowed for function argument");
                                                                     }
                                                                     return {};
                                                                 },
                                                                 [&refs](const Syntax::TypeSpecifier& typeSpecifier)
                                                                     -> std::optional<FailureReason>
                                                                 {
                                                                     refs.emplace_back(typeSpecifier);
                                                                     return {};
                                                                 },
                                                                 [&refs](const Syntax::TypeQualifier& typeQualifier)
                                                                     -> std::optional<FailureReason>
                                                                 {
                                                                     refs.emplace_back(typeQualifier);
                                                                     return {};
                                                                 },
                                                                 [](Syntax::FunctionSpecifier)
                                                                     -> std::optional<FailureReason>
                                                                 {
                                                                     return FailureReason("inline keyword not allowed in this context");
                                                                 }},
                                                             specifiers);
                                                         if (result)
                                                         {
                                                             return result;
                                                         }
                                                     }
                                                     for (auto& pair :iter.getInitDeclarators())
                                                     {
                                                         if (pair.second)
                                                         {
                                                             return FailureReason(
                                                                 "Declarations in function definitions are not allowed to have initializers");
                                                         }
                                                         auto name = Semantics::declaratorToName(*pair.first);
                                                         auto result = Semantics::declaratorsToType(refs,
                                                                                                    *pair.first,
                                                                                                    typedefs,
                                                                                                    {},
                                                                                                    structOrUnions);
                                                         if (!result)
                                                         {
                                                             return result.error();
                                                         }
                                                         declarationMap.emplace(name, *result);
                                                     }
                                                 }

                                                 for (std::size_t i = 0; i < identifiers.getIdentifiers().size(); i++)
                                                 {
                                                     auto result = declarationMap
                                                         .find(identifiers.getIdentifiers()[i].first);
                                                     if (result == declarationMap.end())
                                                     {
                                                         continue;
                                                     }
                                                     if (auto* primitive = std::get_if<PrimitiveType>(&result->second
                                                                                                             .get()))
                                                     {
                                                         if (primitive->isFloatingPoint())
                                                         {
                                                             arguments[i] = {PrimitiveType::createDouble(result->second.isConst(),
                                                                                                         result->second.isVolatile()),
                                                                             result->first};
                                                         }
                                                         else if (primitive->getBitCount() == 0)
                                                         {
                                                             return FailureReason("Declaration can't have void");
                                                         }
                                                         else if (primitive->getBitCount() < 32)
                                                         {
                                                             arguments[i] = {PrimitiveType::createInt(result->second.isConst(),
                                                                                                      result->second.isVolatile()),
                                                                             result->first};
                                                         }
                                                         else
                                                         {
                                                             arguments[i] = {result->second, result->first};
                                                         }
                                                     }
                                                     else
                                                     {
                                                         arguments[i] = {result->second, result->first};
                                                     }
                                                     declarationMap.erase(result);
                                                 }

                                                 std::string error;
                                                 for (auto& iter :declarationMap)
                                                 {
                                                     error += iter.first;
                                                 }
                                                 if (!error.empty())
                                                 {
                                                     return FailureReason(
                                                         error + " named in declaration but not as parameter in identifier list");
                                                 }

                                                 baseType = FunctionType::create(std::move(baseType), std::move(arguments), false, false);
                                                 return std::visit(directSelf, identifiers.getDirectDeclarator());
                                             }}},
                                         declarator.get().getDirectDeclarator());
                   }}}, declarator);
    if (result)
    {
        return *result;
    }
    return baseType;
}

std::string OpenCL::Semantics::declaratorToName(const OpenCL::Syntax::Declarator& declarator)
{
    return std::visit(
        Y{overload{[](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> std::string
                   { return name.getIdentifier(); },
                   [](auto&& self, const Syntax::DirectDeclaratorParenthese& declarator) -> std::string
                   {
                       return std::visit([&self](auto&& value) -> std::string
                                         { return self(value); },
                                         declarator.getDeclarator().getDirectDeclarator());
                   },
                   [](auto&& self, auto&& value) -> std::string
                   {
                       return std::visit([&self](auto&& value) -> std::string
                                         { return self(value); },
                                         value.getDirectDeclarator());
                   }}},
        declarator.getDirectDeclarator());
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::Semantics::declaratorToLoc(const OpenCL::Syntax::Declarator& declarator)
{
    return std::visit(
        Y{overload{[](auto&&,
                      const Syntax::DirectDeclaratorIdentifier& name) -> std::vector<OpenCL::Lexer::Token>::const_iterator
                   { return name.getIdentifierLoc(); },
                   [](auto&& self,
                      const Syntax::DirectDeclaratorParenthese& declarator) -> std::vector<OpenCL::Lexer::Token>::const_iterator
                   {
                       return std::visit([&self](auto&& value) -> std::vector<OpenCL::Lexer::Token>::const_iterator
                                         { return self(value); },
                                         declarator.getDeclarator().getDirectDeclarator());
                   },
                   [](auto&& self, auto&& value) -> std::vector<OpenCL::Lexer::Token>::const_iterator
                   {
                       return std::visit([&self](auto&& value) -> std::vector<OpenCL::Lexer::Token>::const_iterator
                                         { return self(value); },
                                         value.getDirectDeclarator());
                   }}},
        declarator.getDirectDeclarator());
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

OpenCL::Expected<std::size_t, OpenCL::FailureReason>
OpenCL::Semantics::alignmentOf(const OpenCL::Semantics::Type& type)
{
    return std::visit(overload{[](const PrimitiveType& primitiveType) -> Expected<std::size_t, FailureReason>
                               {
                                   return primitiveType.getBitCount() / 8;
                               },
                               [](const ArrayType& arrayType) -> Expected<std::size_t, FailureReason>
                               {
                                   return alignmentOf(arrayType.getType());
                               },
                               [](const AbstractArrayType&) -> Expected<std::size_t, FailureReason>
                               {
                                   return FailureReason("Incomplete type in sizeof");
                               },
                               [](const ValArrayType& valArrayType) -> Expected<std::size_t, FailureReason>
                               {
                                   return alignmentOf(valArrayType.getType());
                               },
                               [](const FunctionType&) -> Expected<std::size_t, FailureReason>
                               {
                                   return FailureReason("Function type not allowed in sizeof operator");
                               },
                               [](const RecordType& recordType) -> Expected<std::size_t, FailureReason>
                               {
                                   if (recordType.getMembers().empty())
                                   {
                                       return FailureReason("Incomplete type in sizeof");
                                   }
                                   if (!recordType.isUnion())
                                   {
                                       std::size_t currentAlignment = 0;
                                       for (auto&[subtype, name, bits] : recordType.getMembers())
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
                                       std::optional<FailureReason> failure;
                                       auto result = std::max_element(
                                           recordType.getMembers().begin(), recordType.getMembers().end(),
                                           [&failure](const auto& lhs, const auto& rhs)
                                           {
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
                               [](const EnumType&) -> Expected<std::size_t, FailureReason>
                               { return 4; },
                               [](const PointerType&) -> Expected<std::size_t, FailureReason>
                               { return 8; }},
                      type.get());
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

OpenCL::Expected<std::size_t, OpenCL::FailureReason>
OpenCL::Semantics::sizeOf(const OpenCL::Semantics::Type& type)
{
    return std::visit(overload{[](const PrimitiveType& primitiveType) -> Expected<std::size_t, FailureReason>
                               {
                                   return primitiveType.getBitCount() / 8;
                               },
                               [](const ArrayType& arrayType) -> Expected<std::size_t, FailureReason>
                               {
                                   auto result = sizeOf(arrayType.getType());
                                   if (!result)
                                   {
                                       return result;
                                   }
                                   return *result * arrayType.getSize();
                               },
                               [](const AbstractArrayType&) -> Expected<std::size_t, FailureReason>
                               {
                                   return FailureReason("Incomplete type in sizeof");
                               },
                               [](const ValArrayType&) -> Expected<std::size_t, FailureReason>
                               {
                                   return FailureReason("sizeof Val array cannot be determined in constant expression");
                               },
                               [](const FunctionType&) -> Expected<std::size_t, FailureReason>
                               {
                                   return FailureReason("Function type not allowed in sizeof operator");
                               },
                               [](const RecordType& recordType) -> Expected<std::size_t, FailureReason>
                               {
                                   std::size_t currentSize = 0;
                                   if (recordType.getMembers().empty())
                                   {
                                       return FailureReason("Incomplete type in sizeof");
                                   }
                                   for (auto&[subtype, name, bits] : recordType.getMembers())
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
                               },
                               [](const EnumType&) -> Expected<std::size_t, FailureReason>
                               { return 4; },
                               [](const PointerType&) -> Expected<std::size_t, FailureReason>
                               { return 8; }},
                      type.get());
}

OpenCL::Semantics::FunctionDefinition::FunctionDefinition(FunctionType type,
                                                          std::string name,
                                                          std::vector<Declaration> parameterDeclarations,
                                                          Linkage linkage)
    : m_type(std::move(type)), m_name(std::move(name)), m_parameterDeclarations(std::move(parameterDeclarations)),
      m_linkage(linkage)
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

const std::vector<OpenCL::Semantics::Declaration>& OpenCL::Semantics::FunctionDefinition::getParameterDeclarations() const
{
    return m_parameterDeclarations;
}

OpenCL::Semantics::TranslationUnit::TranslationUnit(std::vector<TranslationUnit::variant> globals)
    : m_globals(std::move(globals))
{}

const std::vector<OpenCL::Semantics::TranslationUnit::variant>& OpenCL::Semantics::TranslationUnit::getGlobals() const
{
    return m_globals;
}

OpenCL::Semantics::Declaration::Declaration(OpenCL::Semantics::Type type,
                                            OpenCL::Semantics::Linkage linkage,
                                            OpenCL::Semantics::Lifetime lifetime,
                                            std::string name)
    : m_type(std::move(type)), m_linkage(linkage), m_lifetime(lifetime), m_name(std::move(name))
{}

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

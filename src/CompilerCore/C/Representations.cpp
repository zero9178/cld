#include "Representations.hpp"

#include "ConstantEvaluator.hpp"
#include "Syntax.hpp"

#include <algorithm>
#include <map>
#include <sstream>
#include <utility>

const OpenCL::Representations::Type& OpenCL::Representations::ArrayType::getType() const
{
    return *m_type;
}

std::size_t OpenCL::Representations::ArrayType::getSize() const
{
    return m_size;
}

OpenCL::Representations::ArrayType::ArrayType(bool isRestricted, std::shared_ptr<OpenCL::Representations::Type>&& type,
                                              std::size_t size)
    : m_restricted(isRestricted), m_type(std::move(type)), m_size(size)
{
}

bool OpenCL::Representations::ArrayType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Representations::Type OpenCL::Representations::ArrayType::create(bool isConst, bool isVolatile,
                                                                         bool isRestricted,
                                                                         OpenCL::Representations::Type&& type,
                                                                         std::size_t size)
{
    std::ostringstream ss;
    ss << size;
    auto name = type.getName() + "[" + ss.str() + "]";
    return OpenCL::Representations::Type(isConst, isVolatile, name,
                                         ArrayType(isRestricted, std::make_shared<Type>(std::move(type)), size));
}

bool OpenCL::Representations::ArrayType::operator==(const OpenCL::Representations::ArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type, m_size) == std::tie(rhs.m_restricted, *rhs.m_type, rhs.m_size);
}

bool OpenCL::Representations::ArrayType::operator!=(const OpenCL::Representations::ArrayType& rhs) const
{
    return !(rhs == *this);
}

bool OpenCL::Representations::Type::isConst() const
{
    return m_isConst;
}

bool OpenCL::Representations::Type::isVolatile() const
{
    return m_isVolatile;
}

const std::string& OpenCL::Representations::Type::getName() const
{
    return m_name;
}

void OpenCL::Representations::Type::setName(const std::string& name)
{
    m_name = name;
}

const OpenCL::Representations::Type::variant& OpenCL::Representations::Type::getType() const
{
    return m_type;
}

OpenCL::Representations::Type::Type(bool isConst, bool isVolatile, std::string name,
                                    OpenCL::Representations::Type::variant&& type)
    : m_isConst(isConst), m_isVolatile(isVolatile), m_name(std::move(name)), m_type(std::move(type))
{
}

bool OpenCL::Representations::Type::operator==(const OpenCL::Representations::Type& rhs) const
{
    return std::tie(m_isConst, m_isVolatile, m_type) == std::tie(rhs.m_isConst, rhs.m_isVolatile, rhs.m_type);
}

bool OpenCL::Representations::Type::operator!=(const OpenCL::Representations::Type& rhs) const
{
    return !(rhs == *this);
}

bool OpenCL::Representations::Type::isCompatibleWith(const OpenCL::Representations::Type& rhs) const
{
    return m_type == rhs.m_type;
}

OpenCL::Representations::PointerType::PointerType(bool isRestricted,
                                                  std::shared_ptr<OpenCL::Representations::Type>&& elementType)
    : m_restricted(isRestricted), m_elementType(std::move(elementType))
{
}

const OpenCL::Representations::Type& OpenCL::Representations::PointerType::getElementType() const
{
    return *m_elementType;
}

bool OpenCL::Representations::PointerType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Representations::Type OpenCL::Representations::PointerType::create(bool isConst, bool isVolatile,
                                                                           bool isRestricted,
                                                                           OpenCL::Representations::Type&& elementType)
{
    std::string name;
    if (std::holds_alternative<FunctionType>(elementType.getType()))
    {
        auto openParenthese = elementType.getName().find('(');
        name = elementType.getName().substr(0, openParenthese) + "(*)" + elementType.getName().substr(openParenthese);
    }
    else if (std::holds_alternative<ArrayType>(elementType.getType())
             || std::holds_alternative<ValArrayType>(elementType.getType())
             || std::holds_alternative<AbstractArrayType>(elementType.getType()))
    {
        auto openBracket = elementType.getName().find('[');
        name = elementType.getName().substr(0, openBracket) + "(*)" + elementType.getName().substr(openBracket);
    }
    else
    {
        name = elementType.getName() + "*";
    }
    return OpenCL::Representations::Type(isConst, isVolatile, name,
                                         PointerType(isRestricted, std::make_shared<Type>(std::move(elementType))));
}

bool OpenCL::Representations::PointerType::operator==(const OpenCL::Representations::PointerType& rhs) const
{
    return std::tie(m_restricted, *m_elementType) == std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool OpenCL::Representations::PointerType::operator!=(const OpenCL::Representations::PointerType& rhs) const
{
    return !(rhs == *this);
}

OpenCL::Representations::EnumType::EnumType(const std::string& name,
                                            std::vector<std::pair<std::string, std::int32_t>> values)
    : m_name(name), m_values(std::move(values))
{
}

const std::vector<std::pair<std::string, int32_t>>& OpenCL::Representations::EnumType::getValues() const
{
    return m_values;
}

bool OpenCL::Representations::EnumType::isDefinition() const
{
    return !m_values.empty();
}

bool OpenCL::Representations::EnumType::isAnonymous() const
{
    return m_name.empty();
}

OpenCL::Representations::Type
    OpenCL::Representations::EnumType::create(bool isConst, bool isVolatile, const std::string& name,
                                              std::vector<std::pair<std::string, std::int32_t>> values)
{
    return OpenCL::Representations::Type(isConst, isVolatile, "enum " + name, EnumType(name, std::move(values)));
}

bool OpenCL::Representations::EnumType::operator==(const OpenCL::Representations::EnumType& rhs) const
{
    return m_name == rhs.m_name;
}

bool OpenCL::Representations::EnumType::operator!=(const OpenCL::Representations::EnumType& rhs) const
{
    return !(rhs == *this);
}

const std::string& OpenCL::Representations::EnumType::getName() const
{
    return m_name;
}

OpenCL::Representations::PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount)
    : m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned), m_bitCount(bitCount)
{
}

bool OpenCL::Representations::PrimitiveType::isFloatingPoint() const
{
    return m_isFloatingPoint;
}

bool OpenCL::Representations::PrimitiveType::isSigned() const
{
    return m_isSigned;
}

std::uint8_t OpenCL::Representations::PrimitiveType::getBitCount() const
{
    return m_bitCount;
}

OpenCL::Representations::Type OpenCL::Representations::PrimitiveType::create(bool isConst, bool isVolatile,
                                                                             bool isFloatingPoint, bool isSigned,
                                                                             std::uint8_t bitCount)
{
    return OpenCL::Representations::Type(isConst, isVolatile,
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
                                             }
                                             else
                                             {
                                                 switch (bitCount)
                                                 {
                                                     case 8:
                                                         if (isSigned)
                                                         {
                                                             return "char";
                                                         }
                                                         else
                                                         {
                                                             return "unsigned char";
                                                         }
                                                         break;
                                                     case 16:
                                                         if (isSigned)
                                                         {
                                                             return "short";
                                                         }
                                                         else
                                                         {
                                                             return "unsigned short";
                                                         }
                                                         break;
                                                     case 32:
                                                         if (isSigned)
                                                         {
                                                             return "int";
                                                         }
                                                         else
                                                         {
                                                             return "unsigned int";
                                                         }
                                                         break;
                                                     case 64:
                                                         if (isSigned)
                                                         {
                                                             return "long long";
                                                         }
                                                         else
                                                         {
                                                             return "unsigned long long";
                                                         }
                                                         break;
                                                 }
                                             }
                                             return "";
                                         }(),
                                         PrimitiveType(isFloatingPoint, isSigned, bitCount));
}

bool OpenCL::Representations::PrimitiveType::operator==(const OpenCL::Representations::PrimitiveType& rhs) const
{
    return std::tie(m_isFloatingPoint, m_isSigned, m_bitCount)
           == std::tie(rhs.m_isFloatingPoint, rhs.m_isSigned, rhs.m_bitCount);
}

bool OpenCL::Representations::PrimitiveType::operator!=(const OpenCL::Representations::PrimitiveType& rhs) const
{
    return !(rhs == *this);
}

OpenCL::Representations::ValArrayType::ValArrayType(bool isRestricted,
                                                    std::shared_ptr<OpenCL::Representations::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
{
}

const OpenCL::Representations::Type& OpenCL::Representations::ValArrayType::getType() const
{
    return *m_type;
}

bool OpenCL::Representations::ValArrayType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Representations::Type OpenCL::Representations::ValArrayType::create(bool isConst, bool isVolatile,
                                                                            bool isRestricted,
                                                                            OpenCL::Representations::Type&& type)
{
    auto name = type.getName() + "[*]";
    return OpenCL::Representations::Type(isConst, isVolatile, name,
                                         ValArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool OpenCL::Representations::ValArrayType::operator==(const OpenCL::Representations::ValArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool OpenCL::Representations::ValArrayType::operator!=(const OpenCL::Representations::ValArrayType& rhs) const
{
    return !(rhs == *this);
}

OpenCL::Representations::FunctionType::FunctionType(std::shared_ptr<Type>&& returnType, std::vector<Type> arguments,
                                                    bool lastIsVararg, bool hasPrototype)
    : m_returnType(std::move(returnType)),
      m_arguments(std::move(arguments)),
      m_lastIsVararg(lastIsVararg),
      m_hasPrototype(hasPrototype)
{
}

const OpenCL::Representations::Type& OpenCL::Representations::FunctionType::getReturnType() const
{
    return *m_returnType;
}

const std::vector<OpenCL::Representations::Type>& OpenCL::Representations::FunctionType::getArguments() const
{
    return m_arguments;
}

bool OpenCL::Representations::FunctionType::isLastVararg() const
{
    return m_lastIsVararg;
}

OpenCL::Representations::Type
    OpenCL::Representations::FunctionType::create(OpenCL::Representations::Type&& returnType,
                                                  std::vector<OpenCL::Representations::Type>&& arguments,
                                                  bool lastIsVararg, bool hasPrototype)
{
    std::string argumentsNames;
    for (std::size_t i = 0; i < arguments.size(); i++)
    {
        argumentsNames += arguments[i].getName();
        if (i + 1 < arguments.size())
        {
            argumentsNames += ", ";
        }
    }
    argumentsNames = returnType.getName() + "(" + argumentsNames + ")";
    return OpenCL::Representations::Type(
        false, false, argumentsNames,
        FunctionType(std::make_shared<Type>(std::move(returnType)), std::move(arguments), lastIsVararg, hasPrototype));
}

bool OpenCL::Representations::FunctionType::operator==(const OpenCL::Representations::FunctionType& rhs) const
{
    return std::tie(*m_returnType, m_arguments, m_lastIsVararg)
           == std::tie(*rhs.m_returnType, rhs.m_arguments, rhs.m_lastIsVararg);
}

bool OpenCL::Representations::FunctionType::operator!=(const OpenCL::Representations::FunctionType& rhs) const
{
    return !(rhs == *this);
}
bool OpenCL::Representations::FunctionType::hasPrototype() const
{
    return m_hasPrototype;
}

OpenCL::Representations::AbstractArrayType::AbstractArrayType(bool isRestricted,
                                                              std::shared_ptr<OpenCL::Representations::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
{
}

const OpenCL::Representations::Type& OpenCL::Representations::AbstractArrayType::getType() const
{
    return *m_type;
}

bool OpenCL::Representations::AbstractArrayType::isRestricted() const
{
    return m_restricted;
}

OpenCL::Representations::Type OpenCL::Representations::AbstractArrayType::create(bool isConst, bool isVolatile,
                                                                                 bool isRestricted,
                                                                                 OpenCL::Representations::Type&& type)
{
    auto name = type.getName() + "[]";
    return OpenCL::Representations::Type(isConst, isVolatile, name,
                                         AbstractArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool OpenCL::Representations::AbstractArrayType::operator==(const OpenCL::Representations::AbstractArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool OpenCL::Representations::AbstractArrayType::operator!=(const OpenCL::Representations::AbstractArrayType& rhs) const
{
    return !(rhs == *this);
}

namespace
{
    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&&... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G)->Y<G>;

    template <class... Ts>
    struct overload : Ts...
    {
        using Ts::operator()...;
    };
    template <class... Ts>
    overload(Ts...)->overload<Ts...>;
} // namespace

OpenCL::Expected<OpenCL::Representations::Type, OpenCL::FailureReason> OpenCL::Representations::declaratorsToType(
    std::vector<SpecifierQualifierRef> specifierQualifiers, PossiblyAbstractQualifierRef declarator,
    const std::map<std::string, std::reference_wrapper<const Type>>& typedefs,
    const std::vector<Syntax::Declaration>& declarations,
    const std::map<std::string, Representations::RecordType>& structOrUnions)
{
    using ThisReturnType = OpenCL::Expected<Type, OpenCL::FailureReason>;

    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for (auto& iter : specifierQualifiers)
    {
        if (auto* typeQualifier = std::get_if<std::reference_wrapper<const Syntax::TypeQualifier>>(&iter))
        {
            switch (*typeQualifier)
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
                   [](const auto& value) -> const Syntax::TypeSpecifier* {
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

    Type baseType = PrimitiveType::create(isConst, isVolatile, false, true, 32);
    if (!typeSpecifiers.empty())
    {
        auto result = std::visit(
            overload{
                [&typeSpecifiers, isConst,
                 isVolatile](Syntax::TypeSpecifier::PrimitiveTypeSpecifier) -> ThisReturnType {
                    if (!std::all_of(typeSpecifiers.begin(), typeSpecifiers.end(), [](const auto pointer) {
                            return std::holds_alternative<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                                pointer->getVariant());
                        }))
                    {
                        return FailureReason(
                            "Primitive type specifiers mixed with struct, union, enum and typedef names");
                    }
                    std::vector<Syntax::TypeSpecifier::PrimitiveTypeSpecifier> primitiveTypeSpecifier;
                    std::transform(typeSpecifiers.begin(), typeSpecifiers.end(),
                                   std::back_inserter(primitiveTypeSpecifier), [](const auto pointer) {
                                       return std::get<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                                           pointer->getVariant());
                                   });

                    auto convert = [isConst, isVolatile](auto&& self, auto begin, auto end,
                                                         bool isSigned) -> ThisReturnType {
                        switch (*begin)
                        {
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void:
                            {
                                if (begin + 1 < end)
                                {
                                    return FailureReason("Can't combine void with other primitive type specifier");
                                }
                                return PrimitiveType::create(isConst, isVolatile, false, isSigned, 0);
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Char:
                            {
                                if (begin + 1 < end)
                                {
                                    return FailureReason("Can't combine char with other primitive type specifier");
                                }
                                return PrimitiveType::create(isConst, isVolatile, false, isSigned, 8);
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Short:
                            {
                                if (begin + 1 < end)
                                {
                                    if (*(begin + 1) != Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int)
                                    {
                                        return FailureReason(
                                            "Only int is allowed to follow short in primitive type specifiers");
                                    }
                                }
                                return PrimitiveType::create(isConst, isVolatile, false, isSigned, 16);
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int:
                            {
                                if (begin + 1 < end)
                                {
                                    return FailureReason("Can't combine int with other primitive type specifier");
                                }
                                return PrimitiveType::create(isConst, isVolatile, false, isSigned, 32);
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long:
                            {
                                if (begin + 1 == end)
                                {
                                    return PrimitiveType::create(isConst, isVolatile, false, isSigned, 32);
                                }
                                switch (*(begin + 1))
                                {
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int:
                                    {
                                        if (begin + 2 < end)
                                        {
                                            return FailureReason(
                                                "Can't combine long int with any other primitive type specifiers");
                                        }
                                        return PrimitiveType::create(isConst, isVolatile, false, isSigned, 32);
                                    }
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long:
                                    {
                                        if (begin + 3 < end
                                            || (begin + 2 < end
                                                && *(begin + 2) != Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long))
                                        {
                                            return FailureReason(
                                                "Can't combine long long int with any other primitive type specifiers");
                                        }
                                        return PrimitiveType::create(isConst, isVolatile, false, isSigned, 64);
                                    }
                                    default:
                                        return FailureReason(
                                            "Can't combine long int with any other primitive type specifiers");
                                }
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Float:
                            {
                                if (begin + 1 < end)
                                {
                                    return FailureReason(
                                        "Can't combine float with any other primitive type specifiers");
                                }
                                return PrimitiveType::create(isConst, isVolatile, true, isSigned, 32);
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Double:
                            {
                                if (begin + 1 < end)
                                {
                                    return FailureReason(
                                        "Can't combine double with any other primitive type specifiers");
                                }
                                return PrimitiveType::create(isConst, isVolatile, true, isSigned, 64);
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Signed:
                            {
                                auto result = self(self, begin + 1, end, true);
                                if (!result)
                                {
                                    return result;
                                }
                                return *result;
                            }
                            case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Unsigned:
                            {
                                auto result = self(self, begin + 1, end, false);
                                if (!result)
                                {
                                    return result;
                                }
                                return *result;
                            }
                        }
                        return FailureReason("Invalid primitive type specifiers");
                    };
                    return convert(convert, primitiveTypeSpecifier.begin(), primitiveTypeSpecifier.end(), true);
                },
                [&typeSpecifiers, isConst, isVolatile, &typedefs, &declarations](
                    const std::unique_ptr<Syntax::StructOrUnionSpecifier>& structOrUnion) -> ThisReturnType {
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
                    for (auto& [structSpecifiers, structDecls] : structOrUnion->getStructDeclarations())
                    {
                        std::vector<SpecifierQualifierRef> refs;
                        for (auto& iter : structSpecifiers)
                        {
                            std::visit([&refs](auto&& value) { refs.push_back(value); }, iter);
                        }
                        for (auto& iter : structDecls)
                        {
                            auto type = declaratorsToType(refs,
                                                          [&]() -> PossiblyAbstractQualifierRef {
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
                 isVolatile](const std::unique_ptr<Syntax::EnumSpecifier>& enumSpecifier) -> ThisReturnType {
                    if (typeSpecifiers.size() != 1)
                    {
                        return FailureReason("Expected no further type specifiers after enum");
                    }
                    std::vector<std::pair<std::string, std::int32_t>> values;
                    auto name = std::visit(overload{[](const std::string& name) { return name; },
                                                    [&values](const Syntax::EnumDeclaration& declaration) {
                                                        values = declaration.getValues();
                                                        return declaration.getName();
                                                    }},
                                           enumSpecifier->getVariant());
                    return EnumType::create(isConst, isVolatile, name, std::move(values));
                },
                [&](const std::string& typedefName) -> ThisReturnType {
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
        if (!result)
        {
            return result;
        }
        baseType = *result;
    }
    if (isRestricted)
    {
        return FailureReason("Only pointers can be restricted");
    }

    auto getQualifiers = [](const std::vector<Syntax::TypeQualifier>& typeQualifiers) -> std::tuple<bool, bool, bool> {
        bool isConst = false;
        bool isVolatile = false;
        bool isRestricted = false;
        for (auto& typeQual : typeQualifiers)
        {
            switch (typeQual)
            {
                case Syntax::TypeQualifier::Const: isConst = true; break;
                case Syntax::TypeQualifier::Restrict: isRestricted = true; break;
                case Syntax::TypeQualifier::Volatile: isVolatile = true; break;
                default: break;
            }
        }
        return std::tie(isConst, isVolatile, isRestricted);
    };

    auto result = std::visit(
        Y{overload{[&](auto&& self,
                       const Syntax::AbstractDeclarator* abstractDeclarator) -> std::optional<FailureReason> {
                       if (!abstractDeclarator)
                       {
                           return {};
                       }
                       for (auto& iter : abstractDeclarator->getPointers())
                       {
                           auto [isConst, isVolatile, isRestricted] = getQualifiers(iter.getTypeQualifiers());
                           baseType = PointerType::create(isConst, isVolatile, isRestricted, std::move(baseType));
                       }
                       return std::visit(
                           Y{overload{
                               [&](auto&&, const std::unique_ptr<Syntax::AbstractDeclarator>& abstractDeclarator)
                                   -> std::optional<FailureReason> {
                                   // Might need to watch out for 6.7.5.3.11 not sure yet
                                   return self(abstractDeclarator.get());
                               },
                               [&](auto&& directSelf,
                                   const Syntax::DirectAbstractDeclaratorAssignmentExpression&
                                       directAbstractDeclaratorAssignmentExpression) -> std::optional<FailureReason> {
                                   if (!directAbstractDeclaratorAssignmentExpression.getAssignmentExpression())
                                   {
                                       baseType = AbstractArrayType::create(false, false, false, std::move(baseType));
                                   }
                                   else
                                   {
                                       Constant::ConstantEvaluator evaluator(structOrUnions, typedefs);
                                       auto result = evaluator.visit(
                                           *directAbstractDeclaratorAssignmentExpression.getAssignmentExpression());
                                       if (!result)
                                       {
                                           baseType = ValArrayType::create(false, false, false, std::move(baseType));
                                       }
                                       else
                                       {
                                           auto size = std::visit(
                                               [](auto&& value) -> std::optional<std::size_t> {
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
                                           [&directSelf](auto&& value) -> std::optional<FailureReason> {
                                               return directSelf(value);
                                           },
                                           directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator()
                                               ->getVariant());
                                   }
                                   return {};
                               },
                               [&](auto&& directSelf,
                                   const std::unique_ptr<Syntax::DirectAbstractDeclarator>& directAbstractDeclarator)
                                   -> std::optional<FailureReason> {
                                   baseType = ValArrayType::create(false, false, false, std::move(baseType));
                                   if (directAbstractDeclarator)
                                   {
                                       return std::visit(
                                           [&directSelf](auto&& value) -> std::optional<FailureReason> {
                                               return directSelf(value);
                                           },
                                           directAbstractDeclarator->getVariant());
                                   }
                                   return {};
                               },
                               [&](auto&& directSelf,
                                   const Syntax::DirectAbstractDeclaratorParameterTypeList& parameterTypeList)
                                   -> std::optional<FailureReason> {
                                   std::vector<Type> arguments;
                                   if (parameterTypeList.getParameterTypeList())
                                   {
                                       for (auto& pair : parameterTypeList.getParameterTypeList()
                                                             ->getParameterList()
                                                             .getParameterDeclarations())
                                       {
                                           std::vector<SpecifierQualifierRef> specifierQualifiers;
                                           for (auto& iter : pair.first)
                                           {
                                               auto optional = std::visit(
                                                   overload{
                                                       [&specifierQualifiers](
                                                           const Syntax::TypeSpecifier& typeSpecifier)
                                                           -> std::optional<FailureReason> {
                                                           specifierQualifiers.emplace_back(typeSpecifier);
                                                           return {};
                                                       },
                                                       [&specifierQualifiers](
                                                           const Syntax::TypeQualifier& typeQualifier)
                                                           -> std::optional<FailureReason> {
                                                           specifierQualifiers.emplace_back(typeQualifier);
                                                           return {};
                                                       },
                                                       [](const Syntax::StorageClassSpecifier&)
                                                           -> std::optional<FailureReason> { return {}; },
                                                       [](Syntax::FunctionSpecifier) -> std::optional<FailureReason> {
                                                           return FailureReason(
                                                               "Inline not allowed in paramter type list");
                                                       }},
                                                   iter);
                                               if (optional)
                                               {
                                                   return optional;
                                               }
                                           }
                                           auto result = declaratorsToType(
                                               specifierQualifiers,
                                               std::visit(
                                                   overload{
                                                       [](const std::unique_ptr<Syntax::Declarator>& directDeclarator)
                                                           -> PossiblyAbstractQualifierRef {
                                                           return std::cref(*directDeclarator);
                                                       },
                                                       [](const std::unique_ptr<Syntax::AbstractDeclarator>&
                                                              abstractDeclarator) -> PossiblyAbstractQualifierRef {
                                                           return abstractDeclarator.get();
                                                       }},
                                                   pair.second),
                                               typedefs, declarations, structOrUnions);
                                           if (!result)
                                           {
                                               return result.error();
                                           }
                                           arguments.push_back(std::move(*result));
                                       }
                                   }
                                   baseType = FunctionType::create(
                                       std::move(baseType), std::move(arguments),
                                       !parameterTypeList.getParameterTypeList()
                                           || parameterTypeList.getParameterTypeList()->hasEllipse(),
                                       true);
                                   if (parameterTypeList.getDirectAbstractDeclarator())
                                   {
                                       return std::visit(
                                           [&directSelf](auto&& value) -> std::optional<FailureReason> {
                                               return directSelf(value);
                                           },
                                           parameterTypeList.getDirectAbstractDeclarator()->getVariant());
                                   }
                                   return {};
                               }}},
                           abstractDeclarator->getDirectAbstractDeclarator().getVariant());
                   },
                   [&](auto&& self, std::
                                        reference_wrapper<const Syntax::Declarator>
                                            declarator) -> std::
                                                            optional<FailureReason> {
                                                                for (auto& iter : declarator.get().getPointers())
                                                                {
                                                                    auto [isConst, isVolatile, isRestricted] =
                                                                        getQualifiers(iter.getTypeQualifiers());
                                                                    baseType = PointerType::create(isConst, isVolatile,
                                                                                                   isRestricted,
                                                                                                   std::move(baseType));
                                                                }
                                                                return std::visit(
                                                                    Y{
                                                                        overload{
                                                                            [&](auto&&, const std::string&)
                                                                                -> std::optional<FailureReason> {
                                                                                return {};
                                                                            },
                                                                            [&](auto&&,
                                                                                const std::unique_ptr<
                                                                                    Syntax::Declarator>& declarator)
                                                                                -> std::optional<FailureReason> {
                                                                                // Might need to watch out
                                                                                // for 6.7.5.3.11 not sure yet
                                                                                return self(std::cref(*declarator));
                                                                            },
                                                                            [&](auto&& directSelf,
                                                                                const Syntax::
                                                                                    DirectDeclaratorNoStaticOrAsterisk&
                                                                                        dirWithoutStaticOrAsterisk)
                                                                                -> std::optional<FailureReason> {
                                                                                auto [isConst, isVolatile,
                                                                                      isRestricted] =
                                                                                    getQualifiers(
                                                                                        dirWithoutStaticOrAsterisk
                                                                                            .getTypeQualifiers());
                                                                                if (!dirWithoutStaticOrAsterisk
                                                                                         .getAssignmentExpression())
                                                                                {
                                                                                    baseType =
                                                                                        AbstractArrayType::create(
                                                                                            isConst, isVolatile,
                                                                                            isRestricted,
                                                                                            std::move(baseType));
                                                                                }
                                                                                else
                                                                                {
                                                                                    Constant::ConstantEvaluator
                                                                                        evaluator(structOrUnions,
                                                                                                  typedefs);
                                                                                    auto result = evaluator.visit(
                                                                                        *dirWithoutStaticOrAsterisk
                                                                                             .getAssignmentExpression());
                                                                                    if (!result)
                                                                                    {
                                                                                        baseType = ValArrayType::create(
                                                                                            isConst, isVolatile,
                                                                                            isRestricted,
                                                                                            std::move(baseType));
                                                                                    }
                                                                                    else
                                                                                    {
                                                                                        auto size = std::visit(
                                                                                            [](auto&& value)
                                                                                                -> std::optional<
                                                                                                    std::size_t> {
                                                                                                using T = std::decay_t<
                                                                                                    decltype(value)>;
                                                                                                if constexpr (
                                                                                                    std::
                                                                                                        is_convertible_v<
                                                                                                            T,
                                                                                                            std::
                                                                                                                size_t>)
                                                                                                {
                                                                                                    return value;
                                                                                                }
                                                                                                return {};
                                                                                            },
                                                                                            *result);
                                                                                        if (!size)
                                                                                        {
                                                                                            return FailureReason(
                                                                                                "Invalid type in result of constant expression");
                                                                                        }
                                                                                        baseType = ArrayType::create(
                                                                                            isConst, isVolatile,
                                                                                            isRestricted,
                                                                                            std::move(baseType), *size);
                                                                                    }
                                                                                }
                                                                                return std::visit(
                                                                                    [&directSelf](auto&& value)
                                                                                        -> std::optional<
                                                                                            FailureReason> {
                                                                                        return directSelf(value);
                                                                                    },
                                                                                    dirWithoutStaticOrAsterisk
                                                                                        .getDirectDeclarator()
                                                                                        .getVariant());
                                                                            },
                                                                            [&](auto&& directSelf,
                                                                                const Syntax::DirectDeclaratorStatic&
                                                                                    directDeclaratorStatic)
                                                                                -> std::optional<FailureReason> {
                                                                                auto [isConst, isVolatile,
                                                                                      isRestricted] =
                                                                                    getQualifiers(
                                                                                        directDeclaratorStatic
                                                                                            .getTypeQualifiers());
                                                                                Constant::ConstantEvaluator evaluator(
                                                                                    structOrUnions, typedefs);
                                                                                auto result = evaluator.visit(
                                                                                    directDeclaratorStatic
                                                                                        .getAssignmentExpression());
                                                                                if (!result)
                                                                                {
                                                                                    baseType = ValArrayType::create(
                                                                                        isConst, isVolatile,
                                                                                        isRestricted,
                                                                                        std::move(baseType));
                                                                                }
                                                                                else
                                                                                {
                                                                                    auto size = std::visit(
                                                                                        [](auto&& value)
                                                                                            -> std::optional<
                                                                                                std::size_t> {
                                                                                            using T =
                                                                                                std::decay_t<decltype(
                                                                                                    value)>;
                                                                                            if constexpr (
                                                                                                std::is_convertible_v<
                                                                                                    T, std::size_t>)
                                                                                            {
                                                                                                return value;
                                                                                            }
                                                                                            return {};
                                                                                        },
                                                                                        *result);
                                                                                    if (!size)
                                                                                    {
                                                                                        return FailureReason(
                                                                                            "Invalid type in result of constant expression");
                                                                                    }
                                                                                    baseType = ArrayType::create(
                                                                                        isConst, isVolatile,
                                                                                        isRestricted,
                                                                                        std::move(baseType), *size);
                                                                                }
                                                                                return std::visit(
                                                                                    [&directSelf](auto&& value)
                                                                                        -> std::optional<
                                                                                            FailureReason> {
                                                                                        return directSelf(value);
                                                                                    },
                                                                                    directDeclaratorStatic
                                                                                        .getDirectDeclarator()
                                                                                        .getVariant());
                                                                            },
                                                                            [&](auto&& directSelf,
                                                                                const Syntax::DirectDeclaratorAsterisk&
                                                                                    directDeclaratorAsterisk)
                                                                                -> std::optional<FailureReason> {
                                                                                auto [isConst, isVolatile,
                                                                                      isRestricted] =
                                                                                    getQualifiers(
                                                                                        directDeclaratorAsterisk
                                                                                            .getTypeQualifiers());
                                                                                baseType = ValArrayType::create(
                                                                                    isConst, isVolatile, isRestricted,
                                                                                    std::move(baseType));
                                                                                return std::visit(
                                                                                    [&directSelf](auto&& value)
                                                                                        -> std::optional<
                                                                                            FailureReason> {
                                                                                        return directSelf(value);
                                                                                    },
                                                                                    directDeclaratorAsterisk
                                                                                        .getDirectDeclarator()
                                                                                        .getVariant());
                                                                            },
                                                                            [&](auto&& directSelf,
                                                                                const Syntax::
                                                                                    DirectDeclaratorParentheseParameters&
                                                                                        parentheseParameters)
                                                                                -> std::optional<FailureReason> {
                                                                                std::vector<Type> arguments;
                                                                                for (auto& pair :
                                                                                     parentheseParameters
                                                                                         .getParameterTypeList()
                                                                                         .getParameterList()
                                                                                         .getParameterDeclarations())
                                                                                {
                                                                                    std::vector<SpecifierQualifierRef>
                                                                                        specifierQualifiers;
                                                                                    for (auto& iter : pair.first)
                                                                                    {
                                                                                        auto optional = std::visit(
                                                                                            overload{
                                                                                                [&specifierQualifiers](
                                                                                                    const Syntax::
                                                                                                        TypeSpecifier&
                                                                                                            typeSpecifier)
                                                                                                    -> std::optional<
                                                                                                        FailureReason> {
                                                                                                    specifierQualifiers
                                                                                                        .emplace_back(
                                                                                                            typeSpecifier);
                                                                                                    return {};
                                                                                                },
                                                                                                [&specifierQualifiers](
                                                                                                    const Syntax::
                                                                                                        TypeQualifier&
                                                                                                            typeQualifier)
                                                                                                    -> std::optional<
                                                                                                        FailureReason> {
                                                                                                    specifierQualifiers
                                                                                                        .emplace_back(
                                                                                                            typeQualifier);
                                                                                                    return {};
                                                                                                },
                                                                                                [](const Syntax::
                                                                                                       StorageClassSpecifier&)
                                                                                                    -> std::optional<
                                                                                                        FailureReason> {
                                                                                                    return {};
                                                                                                },
                                                                                                [](Syntax::
                                                                                                       FunctionSpecifier)
                                                                                                    -> std::optional<
                                                                                                        FailureReason> {
                                                                                                    return FailureReason(
                                                                                                        "Inline not allowed in paramter type list");
                                                                                                }},
                                                                                            iter);
                                                                                        if (optional)
                                                                                        {
                                                                                            return optional;
                                                                                        }
                                                                                    }
                                                                                    auto result = declaratorsToType(
                                                                                        specifierQualifiers,
                                                                                        std::visit(
                                                                                            overload{
                                                                                                [](const std::unique_ptr<
                                                                                                    Syntax::Declarator>&
                                                                                                       directDeclarator)
                                                                                                    -> PossiblyAbstractQualifierRef {
                                                                                                    return std::cref(
                                                                                                        *directDeclarator);
                                                                                                },
                                                                                                [](const std::unique_ptr<
                                                                                                    Syntax::
                                                                                                        AbstractDeclarator>&
                                                                                                       abstractDeclarator)
                                                                                                    -> PossiblyAbstractQualifierRef {
                                                                                                    return abstractDeclarator
                                                                                                        .get();
                                                                                                }},
                                                                                            pair.second),
                                                                                        typedefs, declarations,
                                                                                        structOrUnions);
                                                                                    if (!result)
                                                                                    {
                                                                                        return result.error();
                                                                                    }
                                                                                    arguments.push_back(
                                                                                        std::move(*result));
                                                                                }
                                                                                baseType = FunctionType::create(
                                                                                    std::move(baseType),
                                                                                    std::move(arguments),
                                                                                    parentheseParameters
                                                                                        .getParameterTypeList()
                                                                                        .hasEllipse(),
                                                                                    true);
                                                                                return std::visit(
                                                                                    [&directSelf](auto&& value)
                                                                                        -> std::optional<
                                                                                            FailureReason> {
                                                                                        return directSelf(value);
                                                                                    },
                                                                                    parentheseParameters
                                                                                        .getDirectDeclarator()
                                                                                        .getVariant());
                                                                            },
                                                                            [&](auto&& directSelf, const Syntax::DirectDeclaratorParentheseIdentifiers&
                                                                                                       identifiers) -> std::
                                                                                                                        optional<
                                                                                                                            FailureReason> {
                                                                                                                            std::vector<Type> arguments(
                                                                                                                                identifiers
                                                                                                                                    .getIdentifiers()
                                                                                                                                    .size(),
                                                                                                                                PrimitiveType::create(
                                                                                                                                    false,
                                                                                                                                    false,
                                                                                                                                    false,
                                                                                                                                    true,
                                                                                                                                    32));
                                                                                                                            std::map<
                                                                                                                                std::
                                                                                                                                    string,
                                                                                                                                Type>
                                                                                                                                declarationMap;
                                                                                                                            for (
                                                                                                                                auto&
                                                                                                                                    iter :
                                                                                                                                declarations)
                                                                                                                            {
                                                                                                                                std::vector<
                                                                                                                                    SpecifierQualifierRef>
                                                                                                                                    refs;
                                                                                                                                for (
                                                                                                                                    auto&
                                                                                                                                        specifiers :
                                                                                                                                    iter.getDeclarationSpecifiers())
                                                                                                                                {
                                                                                                                                    auto result =
                                                                                                                                        std::visit(
                                                                                                                                            overload{[](Syntax::StorageClassSpecifier
                                                                                                                                                            storageClassSpecifier)
                                                                                                                                                         -> std::optional<
                                                                                                                                                             FailureReason> {
                                                                                                                                                         if (storageClassSpecifier
                                                                                                                                                             == Syntax::
                                                                                                                                                                 StorageClassSpecifier::
                                                                                                                                                                     Register)
                                                                                                                                                         {
                                                                                                                                                             return {};
                                                                                                                                                         }
                                                                                                                                                         else
                                                                                                                                                         {
                                                                                                                                                             return FailureReason(
                                                                                                                                                                 "Storage class specifiers not allowed in declarations of function parameters");
                                                                                                                                                         }
                                                                                                                                                     },
                                                                                                                                                     [&refs](
                                                                                                                                                         const Syntax::TypeSpecifier& typeSpecifier) -> std::
                                                                                                                                                                                                         optional<
                                                                                                                                                                                                             FailureReason> {
                                                                                                                                                                                                             refs.emplace_back(
                                                                                                                                                                                                                 typeSpecifier);
                                                                                                                                                                                                             return {};
                                                                                                                                                                                                         },
                                                                                                                                                     [&refs](
                                                                                                                                                         const Syntax::
                                                                                                                                                             TypeQualifier&
                                                                                                                                                                 typeQualifier)
                                                                                                                                                         -> std::optional<
                                                                                                                                                             FailureReason> {
                                                                                                                                                         refs.emplace_back(
                                                                                                                                                             typeQualifier);
                                                                                                                                                         return {};
                                                                                                                                                     },
                                                                                                                                                     [](Syntax::
                                                                                                                                                            FunctionSpecifier)
                                                                                                                                                         -> std::optional<
                                                                                                                                                             FailureReason> {
                                                                                                                                                         return FailureReason(
                                                                                                                                                             "inline keyword not allowed in this context");
                                                                                                                                                     }},
                                                                                                                                            specifiers);
                                                                                                                                    if (result)
                                                                                                                                    {
                                                                                                                                        return result;
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                for (
                                                                                                                                    auto&
                                                                                                                                        pair :
                                                                                                                                    iter.getInitDeclarators())
                                                                                                                                {
                                                                                                                                    if (pair.second)
                                                                                                                                    {
                                                                                                                                        return FailureReason(
                                                                                                                                            "Declarations in function definitions are not allowed to have initializers");
                                                                                                                                    }
                                                                                                                                    auto name = Representations::
                                                                                                                                        declaratorToName(
                                                                                                                                            *pair.first);
                                                                                                                                    auto result = Representations::declaratorsToType(
                                                                                                                                        refs,
                                                                                                                                        *pair.first,
                                                                                                                                        typedefs,
                                                                                                                                        {},
                                                                                                                                        structOrUnions);
                                                                                                                                    if (!result)
                                                                                                                                    {
                                                                                                                                        return result
                                                                                                                                            .error();
                                                                                                                                    }
                                                                                                                                    declarationMap
                                                                                                                                        .emplace(
                                                                                                                                            name,
                                                                                                                                            *result);
                                                                                                                                }
                                                                                                                            }

                                                                                                                            for (
                                                                                                                                std::size_t
                                                                                                                                    i = 0;
                                                                                                                                i
                                                                                                                                < identifiers
                                                                                                                                      .getIdentifiers()
                                                                                                                                      .size();
                                                                                                                                i++)
                                                                                                                            {
                                                                                                                                auto result =
                                                                                                                                    declarationMap
                                                                                                                                        .find(
                                                                                                                                            identifiers
                                                                                                                                                .getIdentifiers()
                                                                                                                                                    [i]);
                                                                                                                                if (result
                                                                                                                                    == declarationMap
                                                                                                                                           .end())
                                                                                                                                {
                                                                                                                                    continue;
                                                                                                                                }
                                                                                                                                if (auto* primitive =
                                                                                                                                        std::get_if<
                                                                                                                                            PrimitiveType>(
                                                                                                                                            &result
                                                                                                                                                 ->second
                                                                                                                                                 .getType()))
                                                                                                                                {
                                                                                                                                    if (primitive
                                                                                                                                            ->isFloatingPoint())
                                                                                                                                    {
                                                                                                                                        arguments[i] = PrimitiveType::create(
                                                                                                                                            result
                                                                                                                                                ->second
                                                                                                                                                .isConst(),
                                                                                                                                            result
                                                                                                                                                ->second
                                                                                                                                                .isVolatile(),
                                                                                                                                            true,
                                                                                                                                            true,
                                                                                                                                            64);
                                                                                                                                    }
                                                                                                                                    else if (
                                                                                                                                        primitive
                                                                                                                                            ->getBitCount()
                                                                                                                                        < 32)
                                                                                                                                    {
                                                                                                                                        arguments[i] = PrimitiveType::create(
                                                                                                                                            result
                                                                                                                                                ->second
                                                                                                                                                .isConst(),
                                                                                                                                            result
                                                                                                                                                ->second
                                                                                                                                                .isVolatile(),
                                                                                                                                            false,
                                                                                                                                            true,
                                                                                                                                            32);
                                                                                                                                    }
                                                                                                                                    else
                                                                                                                                    {
                                                                                                                                        arguments
                                                                                                                                            [i] =
                                                                                                                                                result
                                                                                                                                                    ->second;
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                else
                                                                                                                                {
                                                                                                                                    arguments
                                                                                                                                        [i] =
                                                                                                                                            result
                                                                                                                                                ->second;
                                                                                                                                }
                                                                                                                                declarationMap
                                                                                                                                    .erase(
                                                                                                                                        result);
                                                                                                                            }

                                                                                                                            std::string
                                                                                                                                error;
                                                                                                                            for (
                                                                                                                                auto&
                                                                                                                                    iter :
                                                                                                                                declarationMap)
                                                                                                                            {
                                                                                                                                error +=
                                                                                                                                    iter.first;
                                                                                                                            }
                                                                                                                            if (!error
                                                                                                                                     .empty())
                                                                                                                            {
                                                                                                                                return FailureReason(
                                                                                                                                    error
                                                                                                                                    + " named in declaration but not as parameter in identifier list");
                                                                                                                            }

                                                                                                                            baseType = FunctionType::create(
                                                                                                                                std::move(
                                                                                                                                    baseType),
                                                                                                                                std::move(
                                                                                                                                    arguments),
                                                                                                                                false,
                                                                                                                                false);
                                                                                                                            return std::visit(
                                                                                                                                directSelf,
                                                                                                                                identifiers
                                                                                                                                    .getDirectDeclarator()
                                                                                                                                    .getVariant());
                                                                                                                        }}},
                                                                    declarator.get()
                                                                        .getDirectDeclarator()
                                                                        .getVariant());
                                                            }}},
        declarator);
    return baseType;
}

std::string OpenCL::Representations::declaratorToName(const OpenCL::Syntax::Declarator& declarator)
{
    return std::visit(
        Y{overload{[](auto&&, const std::string& name) -> std::string { return name; },
                   [](auto&& self, const std::unique_ptr<OpenCL::Syntax::Declarator>& declarator) -> std::string {
                       return std::visit([&self](auto&& value) -> std::string { return self(value); },
                                         declarator->getDirectDeclarator().getVariant());
                   },
                   [](auto&& self, auto&& value) -> std::string {
                       return std::visit([&self](auto&& value) -> std::string { return self(value); },
                                         value.getDirectDeclarator().getVariant());
                   }}},
        declarator.getDirectDeclarator().getVariant());
}

OpenCL::Representations::RecordType::RecordType(std::string name, bool isUnion,
                                                std::vector<std::tuple<Type, std::string, std::int64_t>>&& names)
    : m_name(std::move(name)), m_isUnion(isUnion), m_members(std::move(names))
{
}

bool OpenCL::Representations::RecordType::isUnion() const
{
    return m_isUnion;
}

const std::vector<std::tuple<OpenCL::Representations::Type, std::string, std::int64_t>>&
    OpenCL::Representations::RecordType::getMembers() const
{
    return m_members;
}

bool OpenCL::Representations::RecordType::isDefinition() const
{
    return !m_members.empty();
}

OpenCL::Representations::Type OpenCL::Representations::RecordType::create(
    bool isConst, bool isVolatile, bool isUnion, const std::string& name,
    std::vector<std::tuple<OpenCL::Representations::Type, std::string, int64_t>>&& members)
{
    return OpenCL::Representations::Type(isConst, isVolatile, (isUnion ? "union " : "struct ") + name,
                                         RecordType(name, isUnion, std::move(members)));
}

bool OpenCL::Representations::RecordType::operator==(const OpenCL::Representations::RecordType& rhs) const
{
    return std::tie(m_isUnion, m_name) == std::tie(rhs.m_isUnion, rhs.m_name);
}

bool OpenCL::Representations::RecordType::operator!=(const OpenCL::Representations::RecordType& rhs) const
{
    return !(rhs == *this);
}

const std::string& OpenCL::Representations::RecordType::getName() const
{
    return m_name;
}

OpenCL::Expected<std::size_t, OpenCL::FailureReason>
    OpenCL::Representations::alignmentOf(const OpenCL::Representations::Type& type)
{
    return std::visit(overload{[](const PrimitiveType& primitiveType) -> Expected<std::size_t, FailureReason> {
                                   return primitiveType.getBitCount() / 8;
                               },
                               [](const ArrayType& arrayType) -> Expected<std::size_t, FailureReason> {
                                   return alignmentOf(arrayType.getType());
                               },
                               [](const AbstractArrayType&) -> Expected<std::size_t, FailureReason> {
                                   return FailureReason("Incomplete type in sizeof");
                               },
                               [](const ValArrayType& valArrayType) -> Expected<std::size_t, FailureReason> {
                                   return alignmentOf(valArrayType.getType());
                               },
                               [](const FunctionType&) -> Expected<std::size_t, FailureReason> {
                                   return FailureReason("Function type not allowed in sizeof operator");
                               },
                               [](const RecordType& recordType) -> Expected<std::size_t, FailureReason> {
                                   if (recordType.getMembers().empty())
                                   {
                                       return FailureReason("Incomplete type in sizeof");
                                   }
                                   if (!recordType.isUnion())
                                   {
                                       std::size_t currentAlignment = 0;
                                       for (auto& [type, name, bits] : recordType.getMembers())
                                       {
                                           (void)name;
                                           (void)bits;
                                           auto result = alignmentOf(type);
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
                               [](const EnumType&) -> Expected<std::size_t, FailureReason> { return 4; },
                               [](const PointerType&) -> Expected<std::size_t, FailureReason> { return 8; }},
                      type.getType());
}

OpenCL::Expected<std::size_t, OpenCL::FailureReason>
    OpenCL::Representations::sizeOf(const OpenCL::Representations::Type& type)
{
    return std::visit(overload{[](const PrimitiveType& primitiveType) -> Expected<std::size_t, FailureReason> {
                                   return primitiveType.getBitCount() / 8;
                               },
                               [](const ArrayType& arrayType) -> Expected<std::size_t, FailureReason> {
                                   auto result = sizeOf(arrayType.getType());
                                   if (!result)
                                   {
                                       return result;
                                   }
                                   return *result * arrayType.getSize();
                               },
                               [](const AbstractArrayType&) -> Expected<std::size_t, FailureReason> {
                                   return FailureReason("Incomplete type in sizeof");
                               },
                               [](const ValArrayType&) -> Expected<std::size_t, FailureReason> {
                                   return FailureReason("sizeof Val array cannot be determined in constant expression");
                               },
                               [](const FunctionType&) -> Expected<std::size_t, FailureReason> {
                                   return FailureReason("Function type not allowed in sizeof operator");
                               },
                               [](const RecordType& recordType) -> Expected<std::size_t, FailureReason> {
                                   std::size_t currentSize = 0;
                                   if (recordType.getMembers().empty())
                                   {
                                       return FailureReason("Incomplete type in sizeof");
                                   }
                                   for (auto& [type, name, bits] : recordType.getMembers())
                                   {
                                       (void)name;
                                       (void)bits;
                                       auto alignment = alignmentOf(type);
                                       if (!alignment)
                                       {
                                           return alignment;
                                       }
                                       auto rest = currentSize % *alignment;
                                       if (rest != 0)
                                       {
                                           currentSize += *alignment - rest;
                                       }
                                       auto subSize = sizeOf(type);
                                       if (!subSize)
                                       {
                                           return subSize;
                                       }
                                       currentSize += *subSize;
                                   }
                                   return currentSize;
                               },
                               [](const EnumType&) -> Expected<std::size_t, FailureReason> { return 4; },
                               [](const PointerType&) -> Expected<std::size_t, FailureReason> { return 8; }},
                      type.getType());
}

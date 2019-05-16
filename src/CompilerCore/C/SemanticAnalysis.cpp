#include <utility>
#include <algorithm>

#include "SemanticAnalysis.hpp"

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

    template <class... Args>
    struct variant_cast_proxy
    {
        std::variant<Args...> v;

        template <class... ToArgs>
        operator std::variant<ToArgs...>() const
        {
            return std::visit([](auto&& arg) -> std::variant<ToArgs...>
                              { return arg; },
                              v);
        }
    };

    template <class... Args>
    auto variant_cast(const std::variant<Args...>& v) -> variant_cast_proxy<Args...>
    {
        return {v};
    }

    bool canCastTo(const OpenCL::Semantics::Type& sourceType,
                   const OpenCL::Semantics::Type& destinationType,
                   bool explicitConversion)
    {
        if (sourceType.isCompatibleWith(destinationType))
        {
            return true;
        }
        return std::visit(
            overload{
                [&](const OpenCL::Semantics::PrimitiveType& primitiveType) -> bool
                {
                    return std::visit(
                        overload{[&](const OpenCL::Semantics::PrimitiveType&) -> bool
                                 {
                                     return true;
                                 },
                                 [&](const OpenCL::Semantics::PointerType& pointerType) -> bool
                                 {
                                     if (primitiveType.isFloatingPoint())
                                     {
                                         return false;
                                     }
                                     if (explicitConversion)
                                     {
                                         return !std::holds_alternative<OpenCL::Semantics::FunctionType>(
                                             pointerType.getElementType().getType());
                                     }
                                     else
                                     {
                                         return true;
                                     }
                                 },
                                 [&](const OpenCL::Semantics::EnumType&) -> bool
                                 {
                                     return true;
                                 },
                                 [](auto&&) -> bool
                                 { return false; }},
                        destinationType.getType());
                },
                [&](const OpenCL::Semantics::PointerType& pointerType) -> bool
                {
                    return std::visit(overload{[&](const OpenCL::Semantics::PrimitiveType& primitiveType) -> bool
                                               {
                                                   return !(primitiveType.isFloatingPoint() || !explicitConversion);
                                               },
                                               [&](const OpenCL::Semantics::PointerType& otherPointer) -> bool
                                               {
                                                   if (!explicitConversion)
                                                   {
                                                       auto* primitive = std::get_if<OpenCL::Semantics::PrimitiveType>(
                                                           &pointerType.getElementType().getType());
                                                       auto* otherPrimitive
                                                           = std::get_if<OpenCL::Semantics::PrimitiveType>(
                                                               &otherPointer.getElementType().getType());
                                                       if ((!primitive || primitive->getBitCount() == 0)
                                                           && (!otherPrimitive || otherPrimitive->getBitCount() == 0))
                                                       {
                                                           return false;
                                                       }
                                                   }
                                                   return true;
                                               },
                                               [&](const OpenCL::Semantics::EnumType&) -> bool
                                               {
                                                   return true;
                                               },
                                               [](auto&&) -> bool
                                               { return false; }},
                                      destinationType.getType());
                },
                [&](const OpenCL::Semantics::FunctionType& functionType) -> bool
                {
                    return std::visit(overload{[&](const OpenCL::Semantics::PointerType& pointerType) -> bool
                                               {
                                                   if (auto* function = std::get_if<OpenCL::Semantics::FunctionType>(
                                                           &pointerType.getElementType().getType());
                                                       function && *function == functionType)
                                                   {
                                                       return true;
                                                   }
                                                   return false;
                                               },
                                               [](auto&&) -> bool
                                               { return false; }},
                                      destinationType.getType());
                },
                [&](const OpenCL::Semantics::EnumType&) -> bool
                {
                    return std::visit(overload{[&](const OpenCL::Semantics::PrimitiveType&) -> bool
                                               {
                                                   return true;
                                               },
                                               [&](const OpenCL::Semantics::PointerType&) -> bool
                                               {
                                                   return true;
                                               },
                                               [&](const OpenCL::Semantics::EnumType&) -> bool
                                               { return true; },
                                               [](auto&&) -> bool
                                               { return false; }},
                                      destinationType.getType());
                },
                [&](const OpenCL::Semantics::ArrayType& arrayType) -> bool
                {
                    return canCastTo(OpenCL::Semantics::PointerType::create(false, false, false,
                                                                            OpenCL::Semantics::Type(arrayType
                                                                                                        .getType())),
                                     destinationType,
                                     explicitConversion);
                },
                [](auto&&) -> bool
                { return false; }},
            sourceType.getType());
    }

    OpenCL::Semantics::Type integerPromotion(const OpenCL::Semantics::Type& type)
    {
        if (auto* primitive = std::get_if<OpenCL::Semantics::PrimitiveType>(&type.getType()))
        {
            if (!primitive->isFloatingPoint() && primitive->getBitCount() < 32)
            {
                return OpenCL::Semantics::PrimitiveType::create(type.isConst(), type.isVolatile(), false, true, 32);
            }
        }
        return type;
    }
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    return std::visit([this](auto&& value)
                      { return visit(value); }, node.getVariant());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PrimaryExpressionIdentifier& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
{
    return std::visit(
        overload{[](std::int32_t) -> Semantics::Type
                 {
                     return Semantics::PrimitiveType::create(false, false, false, true, 32);
                 },
                 [](std::uint32_t) -> Semantics::Type
                 {
                     return Semantics::PrimitiveType::create(false, false, false, false, 32);
                 },
                 [](std::int64_t) -> Semantics::Type
                 {
                     return Semantics::PrimitiveType::create(false, false, false, true, 64);
                 },
                 [](std::uint64_t) -> Semantics::Type
                 {
                     return Semantics::PrimitiveType::create(false, false, false, false, 64);
                 },
                 [](float) -> Semantics::Type
                 {
                     return Semantics::PrimitiveType::create(false, false, true, true, 32);
                 },
                 [](double) -> Semantics::Type
                 {
                     return Semantics::PrimitiveType::create(false, false, true, true, 64);
                 },
                 [](const std::string& s) -> Semantics::Type
                 {
                     return Semantics::ArrayType::create(
                         false, false, false,
                         Semantics::PrimitiveType::create(false, false, false, true, 8),
                         s.size() + 1);
                 }}, node.getValue());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PrimaryExpressionParenthese& node)
{
    return visit(node.getExpression());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    return std::visit([this](auto&& value)
                      {
                          return visit(value);
                      }, node.getVariant());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionSubscript& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    auto index = visit(node.getExpression());
    if (!index)
    {
        return index;
    }
    auto* ptr = std::get_if<Semantics::PointerType>(&result->getType());
    if (!ptr)
    {
        return FailureReason("[] operator can only be applied to pointers and arrays");
    }
    return ptr->getElementType();
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionIncrement& node)
{
    return visit(node.getPostFixExpression());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionDecrement& node)
{
    return visit(node.getPostFixExpression());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionDot& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    const Semantics::RecordType* structType = std::get_if<Semantics::RecordType>(&result->getType());
    if (!structType)
    {
        return FailureReason("Can only apply . to struct or union type");
    }
    auto member = std::find_if(structType->getMembers().begin(), structType->getMembers().end(),
                               [&node](const auto& tuple)
                               { return std::get<1>(tuple) == node.getIdentifier(); });
    if (member == structType->getMembers().end())
    {
        return FailureReason("Could not find member " + node.getIdentifier() + " in " + structType->getName());
    }
    return std::get<0>(*member);
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionArrow& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    const Semantics::PointerType* pointerType =
        std::get_if<Semantics::PointerType>(&result->getType());
    if (!pointerType)
    {
        return FailureReason("Can only apply -> to pointer types");
    }
    const Semantics::RecordType* structType =
        std::get_if<Semantics::RecordType>(&pointerType->getElementType().getType());
    if (!structType)
    {
        return FailureReason("Can only apply -> to pointer to struct or union type");
    }
    auto member = std::find_if(structType->getMembers().begin(), structType->getMembers().end(),
                               [&node](const auto& tuple)
                               { return std::get<1>(tuple) == node.getIdentifier(); });
    if (member == structType->getMembers().end())
    {
        return FailureReason("Could not find member " + node.getIdentifier() + " in " + structType->getName());
    }
    return std::get<0>(*member);
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionFunctionCall& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    auto type = *result;
    auto* function = std::get_if<Semantics::FunctionType>(&type.getType());
    if (!function)
    {
        return FailureReason("Function call only possible on function type");
    }
    std::size_t i = 0;
    for (auto& iter : node.getOptionalAssignmentExpressions())
    {
        auto argType = visit(*iter);
        if (!argType)
        {
            return argType;
        }
        if (!canCastTo(*argType, function->getArguments()[i++].first, false))
        {
            return FailureReason(
                "Argument " + std::to_string(i - 1) + " in function call can not be cast from " + argType->getName()
                    + " to " + function->getArguments()[i - 1].first.getName());
        }
    }
    return function->getReturnType();
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer& node)
{
    std::vector<Semantics::SpecifierQualifierRef> specifierQualifierRefs;
    for (auto& iter : node.getTypeName().getSpecifierQualifiers())
    {
        std::visit([&specifierQualifierRefs](auto&& value)
                   {
                       specifierQualifierRefs.emplace_back(value);
                   }, iter);
    }
    return Semantics::declaratorsToType(specifierQualifierRefs,
                                        node.getTypeName().getAbstractDeclarator(),
                                        gatherTypedefs(),
                                        {},
                                        gatherStructsAndUnions());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    return std::visit([this](auto&& value)
                      {
                          return visit(value);
                      }, node.getVariant());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression& node)
{
    return visit(node.getPostFixExpression());
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& node)
{
    return Semantics::PrimitiveType::create(false, false, false, false, 64);
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
{
    auto result = visit(node.getUnaryExpression());
    if (!result)
    {
        return result;
    }
    auto type = *result;
    switch (node.getAnOperator())
    {
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:return type;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        return Semantics::PointerType::create(false,
                                              false,
                                              false,
                                              std::move(type));
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
    {
        auto* pointer = std::get_if<Semantics::PointerType>(&type.getType());
        if (!pointer)
        {
            return FailureReason("Can only dereference pointer type");
        }
        return pointer->getElementType();
    }
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:break;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:break;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:break;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:break;
    }
    return result;
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::CastExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::Term& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::AdditiveExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::ShiftExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::RelationalExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::EqualityExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::BitAndExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::BitXorExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::BitOrExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::LogicalAndExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::LogicalOrExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::ConditionalExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::AssignmentExpression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::AssignmentExpressionAssignment& node)
{

}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::Expression& node)
{

}

OpenCL::Expected<OpenCL::Semantics::TranslationUnit, OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(
    const Syntax::TranslationUnit& node)
{
    std::vector<TranslationUnit::variant> globals;
    for (auto& iter : node.getGlobals())
    {
        auto result = std::visit(overload{
            [this, &globals](const Syntax::FunctionDefinition& function) -> Expected<TranslationUnit::variant,
                                                                                     FailureReason>
            {
                auto result = visit(function);
                if (!result)
                {
                    return result;
                }
                if (result->hasPrototype())
                {
                    globals.emplace_back(FunctionPrototype(result->getType(),
                                                           result->getName(), result->getLinkage()));
                }
                return *result;
            },
            [this, &globals](const Syntax::Declaration& declaration) -> Expected<TranslationUnit::variant,
                                                                                 FailureReason>
            {
                auto result = visit(declaration);
                if (!result)
                {
                    return result;
                }
                if (result->size() == 1)
                {
                    return variant_cast((*result)[0]);
                }
                std::transform(result->begin(),
                               result->end() - 1,
                               std::back_inserter(globals),
                               [](const auto& variant) -> TranslationUnit::variant
                               {
                                   return variant_cast(variant);
                               });
                return TranslationUnit::variant(variant_cast(result->back()));
            }
        }, iter.getVariant());
        if (!result)
        {
            return result;
        }
        globals.push_back(std::move(*result));
    }
    return TranslationUnit(std::move(globals));
}

namespace
{
    template <class T, class InputIterator>
    bool declarationSpecifierHas(InputIterator&& begin, InputIterator&& end, const T& value)
    {
        return std::any_of(begin, end, [&value](const OpenCL::Syntax::DeclarationSpecifier& declarationSpecifier)
        {
            auto* t = std::get_if<T>(&declarationSpecifier);
            if (!t)
            {
                return false;
            }
            return *t == value;
        });
    }

    template <class T, class InputIterator, class Predicate>
    bool declarationSpecifierHasIf(InputIterator&& begin, InputIterator&& end, Predicate&& predicate)
    {
        return std::any_of(begin, end, [&predicate](const OpenCL::Syntax::DeclarationSpecifier& declarationSpecifier)
        {
            auto* t = std::get_if<T>(&declarationSpecifier);
            if (!t)
            {
                return false;
            }
            return predicate(*t);
        });
    }
} // namespace

OpenCL::Expected<OpenCL::Semantics::FunctionDefinition,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::FunctionDefinition& node)
{
    auto name = declaratorToName(node.getDeclarator());
    std::vector<Semantics::SpecifierQualifierRef> specifierQualifiers;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        std::visit(
            overload{[&specifierQualifiers](const Syntax::TypeSpecifier& typeSpecifier)
                     {
                         specifierQualifiers.emplace_back(typeSpecifier);
                     },
                     [&specifierQualifiers](const Syntax::TypeQualifier& typeQualifier)
                     {
                         specifierQualifiers.emplace_back(typeQualifier);
                     },
                     [](auto&&)
                     {}},
            iter);
    }
    if (std::count_if(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                      [](const Syntax::DeclarationSpecifier& specifier)
                      {
                          return std::holds_alternative<Syntax::StorageClassSpecifier>(specifier);
                      })
        > 1)
    {
        return FailureReason("A maximum of one storage class specifier allowed in declaration");
    }
    auto type = Semantics::declaratorsToType(specifierQualifiers, node.getDeclarator(), gatherTypedefs(),
                                             node.getDeclarations(), gatherStructsAndUnions());
    if (!std::holds_alternative<Semantics::FunctionType>(type->getType()))
    {
        return FailureReason("Expected parameter list in function definition");
    }
    auto functionRP = std::get<Semantics::FunctionType>(type->getType());
    if (std::any_of(functionRP.getArguments().begin(), functionRP.getArguments().end(), [](const auto& pair)
    {
        return pair.second.empty();
    }))
    {
        return FailureReason("Parameter name omitted");
    }
    bool internalLinkage =
        declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                Syntax::StorageClassSpecifier::Extern);
    bool externalLinkage =
        declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                Syntax::StorageClassSpecifier::Static);
    if (internalLinkage && externalLinkage)
    {
        return FailureReason("Can't combine static with extern");
    }
    if (auto[prev, success] = m_typesOfNamedValues.back().insert({name, std::move(*type)}); !success)
    {
        return FailureReason("Redefinition of symbol " + prev->first);
    }

    auto* paramterTypeList = std::get_if<Syntax::DirectDeclaratorParentheseParameters>(
        &node.getDeclarator().getDirectDeclarator().getVariant());
    auto* identifierList = std::get_if<Syntax::DirectDeclaratorParentheseIdentifiers>(
        &node.getDeclarator().getDirectDeclarator().getVariant());
    std::map<std::string, Semantics::Type> declarationMap;
    for (auto& iter : node.getDeclarations())
    {
        std::vector<Semantics::SpecifierQualifierRef> refs;
        for (auto& specifiers : iter.getDeclarationSpecifiers())
        {
            auto result = std::visit(
                overload{[](Syntax::StorageClassSpecifier storageClassSpecifier) -> std::optional<FailureReason>
                         {
                             if (storageClassSpecifier == Syntax::StorageClassSpecifier::Register)
                             {
                                 return {};
                             }
                             else
                             {
                                 return FailureReason(
                                     "Storage class specifiers not allowed in declarations of function parameters");
                             }
                         },
                         [&refs](const Syntax::TypeSpecifier& typeSpecifier) -> std::optional<FailureReason>
                         {
                             refs.emplace_back(typeSpecifier);
                             return {};
                         },
                         [&refs](const Syntax::TypeQualifier& typeQualifier) -> std::optional<FailureReason>
                         {
                             refs.emplace_back(typeQualifier);
                             return {};
                         },
                         [](Syntax::FunctionSpecifier) -> std::optional<FailureReason>
                         {
                             return FailureReason("inline keyword not allowed in this context");
                         }},
                specifiers);
            if (result)
            {
                return *result;
            }
        }
        for (auto& pair : iter.getInitDeclarators())
        {
            if (pair.second)
            {
                return FailureReason("Declarations in function definitions are not allowed to have initializers");
            }
            auto result =
                Semantics::declaratorsToType(refs, *pair.first, gatherTypedefs(), {}, gatherStructsAndUnions());
            if (!result)
            {
                return result.error();
            }
            declarationMap.emplace(Semantics::declaratorToName(*pair.first), *result);
        }
    }
    if (!identifierList && !declarationMap.empty())
    {
        return FailureReason("Declarations even though function has parameter type list");
    }

    pushScope();
    std::vector<std::string> argumentNames;
    std::vector<Type> realTypes;
    for (std::size_t i = 0; i < functionRP.getArguments().size(); i++)
    {
        if (paramterTypeList)
        {
            auto* declarator = std::get_if<std::unique_ptr<Syntax::Declarator>>(&paramterTypeList
                ->getParameterTypeList().getParameterList().getParameterDeclarations()[i].second);
            if (!declarator)
            {
                return FailureReason("Parameter name omitted");
            }
            auto argName = declaratorToName(**declarator);
            if (!m_typesOfNamedValues.back().emplace(argName, functionRP.getArguments()[i].first).second)
            {
                return FailureReason("Parameter with name " + argName + " already exists");
            }
            argumentNames.push_back(argName);
        }
        else
        {
            auto result = declarationMap.find(identifierList->getIdentifiers()[i]);
            if (result == declarationMap.end())
            {
                if (!m_typesOfNamedValues.back()
                                         .emplace(identifierList->getIdentifiers()[i],
                                                  functionRP.getArguments()[i].first)
                                         .second)
                {
                    return FailureReason(
                        "Parameter with name " + identifierList->getIdentifiers()[i] + " already exists");
                }
                realTypes.push_back(functionRP.getArguments()[i].first);
            }
            else
            {
                if (!m_typesOfNamedValues.back().emplace(identifierList->getIdentifiers()[i], result->second).second)
                {
                    return FailureReason(
                        "Parameter with name " + identifierList->getIdentifiers()[i] + " already exists");
                }
                realTypes.push_back(result->second);
            }
            argumentNames.push_back(identifierList->getIdentifiers()[i]);
        }
    }


    //    auto result = visit(node.getCompoundStatement(),false);
    //    if(result)
    //    {
    //        return result;
    //    }

    popScope();

    return FunctionDefinition(functionRP,
                              name,
                              std::move(realTypes),
                              internalLinkage ? Linkage::Internal : Linkage::External);
}

std::map<std::string, OpenCL::Semantics::RecordType> OpenCL::Semantics::SemanticAnalysis::gatherStructsAndUnions() const
{
    std::map<std::string, OpenCL::Semantics::RecordType> result;
    for (auto iter = m_structsUnions.rbegin(); iter != m_structsUnions.rend(); iter++)
    {
        for (auto&[key, value] : *iter)
        {
            result.emplace(key, value);
        }
    }
    return result;
}

std::map<std::string, std::reference_wrapper<const OpenCL::Semantics::Type>>
OpenCL::Semantics::SemanticAnalysis::gatherTypedefs() const
{
    std::map<std::string, std::reference_wrapper<const OpenCL::Semantics::Type>> result;
    for (auto iter = m_typedefs.rbegin(); iter != m_typedefs.rend(); iter++)
    {
        result.insert(iter->begin(), iter->end());
    }
    return result;
}

OpenCL::Expected<std::vector<std::variant<OpenCL::Semantics::FunctionPrototype, OpenCL::Semantics::Declaration>>,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::Declaration& node)
{
    std::vector<std::variant<OpenCL::Semantics::FunctionPrototype, OpenCL::Semantics::Declaration>> decls;
    std::vector<SpecifierQualifierRef> refs;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        std::visit(overload{[&refs](const Syntax::TypeSpecifier& typeSpecifier)
                            {
                                refs.emplace_back(typeSpecifier);
                            },
                            [&refs](const Syntax::TypeQualifier& typeQualifier)
                            {
                                refs.emplace_back(typeQualifier);
                            },
                            [](auto&&)
                            {}}, iter);
        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter);m_typesOfNamedValues.size() == 1
            && storage)
        {
            if (*storage == Syntax::StorageClassSpecifier::Auto
                || *storage == Syntax::StorageClassSpecifier::Register)
            {
                return FailureReason("auto and register not allowed in declaration in file scope");
            }
        }
    }

    bool hasStatic = declarationSpecifierHas<Syntax::StorageClassSpecifier>(node.getDeclarationSpecifiers().begin(),
                                                                            node.getDeclarationSpecifiers().end(),
                                                                            Syntax::StorageClassSpecifier::Static);
    bool hasExtern = declarationSpecifierHas<Syntax::StorageClassSpecifier>(node.getDeclarationSpecifiers().begin(),
                                                                            node.getDeclarationSpecifiers().end(),
                                                                            Syntax::StorageClassSpecifier::Extern);
    if (hasExtern && hasStatic)
    {
        return FailureReason("static and extern can't appear in the same declaration");
    }

    for (auto&[declarator, initializer] : node.getInitDeclarators())
    {
        auto name = declaratorToName(*declarator);
        auto result = declaratorsToType(refs, *declarator, gatherTypedefs(), {}, gatherStructsAndUnions());
        if (!result)
        {
            return result;
        }
        if (auto* functionType = std::get_if<FunctionType>(&result->getType()))
        {
            if (declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(node.getDeclarationSpecifiers().begin(),
                                                                         node.getDeclarationSpecifiers().end(),
                                                                         [](Syntax::StorageClassSpecifier storageClassSpecifier)
                                                                         {
                                                                             return storageClassSpecifier
                                                                                 != Syntax::StorageClassSpecifier::Static
                                                                                 && storageClassSpecifier
                                                                                     != Syntax::StorageClassSpecifier::Extern;
                                                                         }))
            {
                return FailureReason("Only static and extern are allowed storage specifiers for function declarations");
            }
            if (initializer)
            {
                return FailureReason("Initializer not allowed for function prototype");
            }
            if (m_typesOfNamedValues.size() > 1 && hasStatic)
            {
                return FailureReason("static at function prototype only allowed at file scope");
            }
            if (!functionType->hasPrototype() && !functionType->getArguments().empty())
            {
                return FailureReason("Identifier list not allowed in function prototype");
            }
            decls.emplace_back(FunctionPrototype(*functionType,
                                                 name,
                                                 hasStatic ? Linkage::Internal : Linkage::External));
        }
        else
        {
            []{}();
        }
    }
    return decls;
}

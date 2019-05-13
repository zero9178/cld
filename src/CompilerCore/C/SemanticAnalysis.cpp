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
    auto result = m_typesOfNamedValues.find(node.getIdentifier());
    if (result == m_typesOfNamedValues.end())
    {
        return FailureReason("Undefined reference to " + node.getIdentifier());
    }
    return result->second;
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
                      },node.getVariant());
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
    for(auto& iter : node.getOptionalAssignmentExpressions())
    {
        auto argType = visit(*iter);
        if(!argType)
        {
            return argType;
        }
        if(!canCastTo(*argType,function->getArguments()[i++],false))
        {
            return FailureReason("Argument " + std::to_string(i-1) + " in function call can not be cast from " + argType->getName() + " to " + function->getArguments()[i-1].getName());
        }
    }
    return function->getReturnType();
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer& node)
{
    std::vector<Semantics::SpecifierQualifierRef> specifierQualifierRefs;
    for(auto& iter : node.getTypeName().getSpecifierQualifiers())
    {
        std::visit([&specifierQualifierRefs](auto&& value)
                   {
            specifierQualifierRefs.emplace_back(value);
                   },iter);
    }
    return Semantics::declaratorsToType(specifierQualifierRefs,
                                        node.getTypeName().getAbstractDeclarator(),
                                        m_typedefs,
                                        {},
                                        m_structOrUnions);
}

OpenCL::Expected<OpenCL::Semantics::Type,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    return std::visit([this](auto&& value)
                     {
        return visit(value);
                     },node.getVariant());
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
    if(!result)
    {
        return result;
    }
    auto type = *result;
    switch(node.getAnOperator())
    {
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
        return type;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        return Semantics::PointerType::create(false,
                                              false,
                                              false,
                                              std::move(type));
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
    {
        auto* pointer = std::get_if<Semantics::PointerType>(&type.getType());
        if(!pointer)
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

                }
                return *result;
            },
            [this](const Syntax::Declaration& declaration) -> Expected<TranslationUnit::variant, FailureReason>
            {

            }
        }, iter.getVariant());
    }
    return TranslationUnit(std::move(globals));
}

OpenCL::Expected<OpenCL::Semantics::FunctionDefinition,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::FunctionDefinition& node)
{

}

#include <utility>
#include <algorithm>
#include <optional>

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
                                             pointerType.getElementType().get());
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
                        destinationType.get());
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
                                                           &pointerType.getElementType().get());
                                                       auto* otherPrimitive
                                                           = std::get_if<OpenCL::Semantics::PrimitiveType>(
                                                               &otherPointer.getElementType().get());
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
                                      destinationType.get());
                },
                [&](const OpenCL::Semantics::FunctionType& functionType) -> bool
                {
                    return std::visit(overload{[&](const OpenCL::Semantics::PointerType& pointerType) -> bool
                                               {
                                                   auto* function = std::get_if<OpenCL::Semantics::FunctionType>(
                                                       &pointerType.getElementType().get());
                                                   return function && *function == functionType;
                                               },
                                               [](auto&&) -> bool
                                               { return false; }},
                                      destinationType.get());
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
                                      destinationType.get());
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
            sourceType.get());
    }

    [[maybe_unused]] OpenCL::Semantics::Type integerPromotion(const OpenCL::Semantics::Type& type)
    {
        if (auto* primitive = std::get_if<OpenCL::Semantics::PrimitiveType>(&type.get()))
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
                      { return visit(value); }, node);
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
                      }, node);
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
    auto* ptr = std::get_if<Semantics::PointerType>(&result->get());
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
    const Semantics::RecordType* structType = std::get_if<Semantics::RecordType>(&result->get());
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
        std::get_if<Semantics::PointerType>(&result->get());
    if (!pointerType)
    {
        return FailureReason("Can only apply -> to pointer types");
    }
    const Semantics::RecordType* structType =
        std::get_if<Semantics::RecordType>(&pointerType->getElementType().get());
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
    auto* function = std::get_if<Semantics::FunctionType>(&type.get());
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
                      }, node);
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
        auto* pointer = std::get_if<Semantics::PointerType>(&type.get());
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
            [this, &globals](const Syntax::FunctionDefinition& function) -> Expected<std::optional<TranslationUnit::variant>,
                                                                                     FailureReason>
            {
                auto result = visit(function);
                if (!result)
                {
                    return result;
                }
                if (result->hasPrototype())
                {
                    globals.emplace_back(Declaration(Type(false, false, "", result->getType()),
                                                     result->getLinkage(),
                                                     Lifetime::Static,
                                                     result->getName()));
                }
                return *result;
            },
            [this, &globals](const Syntax::Declaration& declaration) -> Expected<std::optional<TranslationUnit::variant>,
                                                                                 FailureReason>
            {
                auto result = visit(declaration);
                if (!result)
                {
                    return result;
                }
                globals.insert(globals.end(), result->begin(), result->end());
                return std::optional<TranslationUnit::variant>{};
            }
        }, iter);
        if (!result)
        {
            return result;
        }
        if (*result)
        {
            globals.push_back(std::move(**result));
        }
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
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
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
        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter))
        {
            if (storageClassSpecifier)
            {
                return FailureReason("A maximum of one storage class specifier allowed in function definition");
            }
            if (storage->getSpecifier() != Syntax::StorageClassSpecifier::Static
                && storage->getSpecifier() != Syntax::StorageClassSpecifier::Extern)
            {
                return FailureReason("Only static or extern are allowed storage class specifiers in function definition");
            }
            storageClassSpecifier = storage;
        }
    }
    auto type = Semantics::declaratorsToType(specifierQualifiers, node.getDeclarator(), gatherTypedefs(),
                                             node.getDeclarations(), gatherStructsAndUnions());
    if (!type)
    {
        return type;
    }
    if (!std::holds_alternative<Semantics::FunctionType>(type->get()))
    {
        return FailureReason("Expected parameter list in function definition");
    }
    auto functionRP = std::get<Semantics::FunctionType>(type->get());
    if (std::any_of(functionRP.getArguments().begin(), functionRP.getArguments().end(), [](const auto& pair)
    {
        return pair.second.empty();
    }))
    {
        return FailureReason("Parameter name omitted");
    }
    if (!m_definedFunctions.insert(name).second)
    {
        return FailureReason("Redefinition of function " + name);
    }

    auto* paramterTypeList = std::get_if<Syntax::DirectDeclaratorParentheseParameters>(
        &node.getDeclarator().getDirectDeclarator());
    auto* identifierList = std::get_if<Syntax::DirectDeclaratorParentheseIdentifiers>(
        &node.getDeclarator().getDirectDeclarator());
    std::map<std::string, Semantics::Type> declarationMap;
    for (auto& iter : node.getDeclarations())
    {
        std::vector<Semantics::SpecifierQualifierRef> refs;
        for (auto& specifiers : iter.getDeclarationSpecifiers())
        {
            auto result = std::visit(
                overload{[](Syntax::StorageClassSpecifier storageClassSpecifier) -> std::optional<FailureReason>
                         {
                             if (storageClassSpecifier.getSpecifier() == Syntax::StorageClassSpecifier::Register)
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
    std::vector<Declaration> declarations;
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
            auto& specifiers = paramterTypeList->getParameterTypeList().getParameterList().getParameterDeclarations()[i]
                .first;
            declarations.emplace_back(functionRP.getArguments()[i].first,
                                      Linkage::None,
                                      declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(specifiers.begin(),
                                                                                               specifiers.end(),
                                                                                               [](const Syntax::StorageClassSpecifier& specifier)
                                                                                               {
                                                                                                   return specifier
                                                                                                       .getSpecifier()
                                                                                                       == Syntax::StorageClassSpecifier::Register;
                                                                                               })
                                      ? Lifetime::Register : Lifetime::Automatic,
                                      functionRP.getArguments()[i].second);
        }
        else
        {
            auto result = declarationMap.find(identifierList->getIdentifiers()[i].first);
            if (result == declarationMap.end())
            {
                declarations.emplace_back(functionRP.getArguments()[i].first,
                                          Linkage::None,
                                          Lifetime::Automatic,
                                          functionRP.getArguments()[i].second);
            }
            else
            {
                declarations.emplace_back(result->second,
                                          Linkage::None,
                                          declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(node.getDeclarations()[i]
                                                                                                     .getDeclarationSpecifiers()
                                                                                                     .begin(),
                                                                                                 node.getDeclarations()[i]
                                                                                                     .getDeclarationSpecifiers()
                                                                                                     .end(),
                                                                                                 [](const Syntax::StorageClassSpecifier& specifier)
                                                                                                 {
                                                                                                     return specifier
                                                                                                         .getSpecifier()
                                                                                                         == Syntax::StorageClassSpecifier::Register;
                                                                                                 })
                                          ? Lifetime::Register : Lifetime::Automatic,
                                          functionRP.getArguments()[i].second);
            }
        }
        if (!m_typesOfNamedValues.back().emplace(declarations.back().getName(), declarations.back()).second)
        {
            return FailureReason("Parameter with name " + declarations.back().getName() + " already exists");
        }
    }

    //TODO:
    //    auto result = visit(node.getCompoundStatement(),false);
    //    if(result)
    //    {
    //        return result;
    //    }

    popScope();

    return FunctionDefinition(functionRP,
                              name,
                              std::move(declarations),
                              storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static
                              ? Linkage::Internal : Linkage::External);
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

OpenCL::Expected<std::vector<OpenCL::Semantics::Declaration>,
                 OpenCL::FailureReason> OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::Declaration& node)
{
    std::vector<OpenCL::Semantics::Declaration> decls;
    std::vector<SpecifierQualifierRef> refs;
    if (std::count_if(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                      [](const Syntax::DeclarationSpecifier& specifier)
                      {
                          return std::holds_alternative<Syntax::StorageClassSpecifier>(specifier);
                      })
        > 1)
    {
        return FailureReason("A maximum of one storage class specifier allowed in declaration");
    }
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
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
            storageClassSpecifier = storage;
            if (storage->getSpecifier() == Syntax::StorageClassSpecifier::Auto
                || storage->getSpecifier() == Syntax::StorageClassSpecifier::Register)
            {
                return FailureReason("auto and register not allowed in declaration in file scope");
            }
        }
    }
    if (node.getInitDeclarators().empty())
    {
        if (std::none_of(refs.begin(), refs.end(), [](const SpecifierQualifierRef& ref)
        {
            return std::visit(overload{
                [](const Syntax::TypeSpecifier& typeSpecifier) -> bool
                {
                    return std::visit(overload{
                        [](const Syntax::StructOrUnionSpecifier&)
                        {
                            return true;
                        },
                        [](const Syntax::EnumDeclaration&)
                        {
                            return true;
                        },
                        [](auto&&) -> bool
                        {
                            return false;
                        }
                    }, typeSpecifier.getVariant());
                },
                [](auto&&) -> bool
                {
                    return false;
                }
            }, ref);
        }))
        {
            return FailureReason("There must be at least one declaration");
        }
        return decls;
    }
    for (auto&[declarator, initializer] : node.getInitDeclarators())
    {
        auto name = declaratorToName(*declarator);
        auto result = declaratorsToType(refs, *declarator, gatherTypedefs(), {}, gatherStructsAndUnions());
        if (!result)
        {
            return result;
        }
        if (isVoid(*result))
        {
            return FailureReason("Type in declaration is not allowed to be void");
        }
        if (auto* functionType = std::get_if<FunctionType>(&result->get()))
        {
            if (declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(node.getDeclarationSpecifiers().begin(),
                                                                         node.getDeclarationSpecifiers().end(),
                                                                         [](Syntax::StorageClassSpecifier storageClassSpecifier)
                                                                         {
                                                                             return storageClassSpecifier.getSpecifier()
                                                                                 != Syntax::StorageClassSpecifier::Static
                                                                                 && storageClassSpecifier.getSpecifier()
                                                                                     != Syntax::StorageClassSpecifier::Extern;
                                                                         }))
            {
                return FailureReason("Only static and extern are allowed storage specifiers for function declarations");
            }
            if (initializer)
            {
                return FailureReason("Initializer not allowed for function prototype");
            }
            if (m_typesOfNamedValues.size() > 1 && storageClassSpecifier
                && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                return FailureReason("static at function prototype only allowed at file scope");
            }
            if (!functionType->hasPrototype() && !functionType->getArguments().empty())
            {
                return FailureReason("Identifier list not allowed in function prototype");
            }
            decls.emplace_back(Declaration(std::move(*result),
                                           storageClassSpecifier
                                               && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static
                                           ? Linkage::Internal : Linkage::External, Lifetime::Static, std::move(name)));
            m_typesOfNamedValues.back().emplace(decls.back().getName(), decls.back());
        }
        else
        {
            Linkage linkage = Linkage::None;
            Lifetime lifetime = m_typesOfNamedValues.size() > 1 ? Lifetime::Automatic : Lifetime::Static;
            if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                if (m_typesOfNamedValues.size() > 1)
                {
                    lifetime = Lifetime::Static;
                }
                else
                {
                    linkage = Linkage::Internal;
                }
            }
            else if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Extern)
            {
                linkage = Linkage::External;
            }
            else if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Register)
            {
                lifetime = Lifetime::Register;
            }
            auto declaration = Declaration(std::move(*result), linkage, lifetime, name);
            if (auto[prev, success] = m_typesOfNamedValues.back().emplace(name, declaration);!success
                && (prev->second.getLinkage() == Linkage::None || linkage == Linkage::None))
            {
                return FailureReason("Redeclaration of " + prev->first);
            }
            decls.emplace_back(std::move(declaration));
        }
    }
    return decls;
}

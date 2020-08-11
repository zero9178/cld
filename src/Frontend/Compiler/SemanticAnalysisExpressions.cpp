#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::Expression& node)
{
    Expression expression;
    for (auto& iter : node.getAssignmentExpressions())
    {
        expression = visit(iter);
    }
    return expression;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::AssignmentExpression& node)
{
    auto lvalue = visit(node.getConditionalExpression());
    if (node.getAssignments().empty())
    {
        return lvalue;
    }
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpression& node)
{
    return cld::match(node, [&](auto&& value) { return visit(value); });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionConstant& node)
{
    if (std::holds_alternative<llvm::APSInt>(node.getValue()) || std::holds_alternative<llvm::APFloat>(node.getValue()))
    {
        switch (node.getType())
        {
            case Lexer::CToken::Type::None: CLD_UNREACHABLE;
            case Lexer::CToken::Type::Int:
                return Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue, Constant(node.getValue()));
            case Lexer::CToken::Type::UnsignedInt:
                return Expression(
                    PrimitiveType::createUnsignedInt(false, false, m_sourceInterface.getLanguageOptions()),
                    ValueCategory::Rvalue, Constant(node.getValue()));
            case Lexer::CToken::Type::Long:
                return Expression(PrimitiveType::createLong(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue, Constant(node.getValue()));
            case Lexer::CToken::Type::UnsignedLong:
                return Expression(
                    PrimitiveType::createUnsignedLong(false, false, m_sourceInterface.getLanguageOptions()),
                    ValueCategory::Rvalue, Constant(node.getValue()));
            case Lexer::CToken::Type::LongLong:
                return Expression(PrimitiveType::createLongLong(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue()));
            case Lexer::CToken::Type::UnsignedLongLong:
                return Expression(PrimitiveType::createUnsignedLongLong(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue()));
            case Lexer::CToken::Type::Float:
                return Expression(PrimitiveType::createFloat(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue()));
            case Lexer::CToken::Type::Double:
                return Expression(PrimitiveType::createDouble(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue()));
            case Lexer::CToken::Type::LongDouble:
                return Expression(PrimitiveType::createLongDouble(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue, Constant(node.getValue()));
        }
    }
    else if (std::holds_alternative<std::string>(node.getValue()))
    {
        auto& string = cld::get<std::string>(node.getValue());
        return Expression(
            ArrayType::create(false, false, false, false,
                              PrimitiveType::createChar(false, false, m_sourceInterface.getLanguageOptions()),
                              string.size()),
            ValueCategory::Rvalue, node.getValue());
    }
    else if (std::holds_alternative<Lexer::NonCharString>(node.getValue()))
    {
        auto& string = cld::get<Lexer::NonCharString>(node.getValue());
        if (string.type == Lexer::NonCharString::Wide)
        {
            switch (m_sourceInterface.getLanguageOptions().wcharUnderlyingType)
            {
                case LanguageOptions::WideCharType::UnsignedShort:
                    return Expression(ArrayType::create(false, false, false, false,
                                                        PrimitiveType::createUnsignedShort(
                                                            false, false, m_sourceInterface.getLanguageOptions()),
                                                        string.characters.size()),
                                      ValueCategory::Rvalue, node.getValue());
                case LanguageOptions::WideCharType::Int:
                    return Expression(ArrayType::create(false, false, false, false,
                                                        PrimitiveType::createInt(
                                                            false, false, m_sourceInterface.getLanguageOptions()),
                                                        string.characters.size()),
                                      ValueCategory::Rvalue, node.getValue());
            }
            CLD_UNREACHABLE;
        }
    }
    CLD_UNREACHABLE;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionIdentifier& node)
{
    auto* result = lookupDecl(cld::get<std::string>(node.getIdentifier()->getValue()));
    if (!result || std::holds_alternative<Type>(*result))
    {
        log(Errors::Semantics::UNDECLARED_IDENTIFIER_N.args(*node.getIdentifier(), m_sourceInterface,
                                                            *node.getIdentifier()));
        return {};
    }
    if (std::holds_alternative<ConstRetType>(*result))
    {
        auto& value = cld::get<ConstRetType>(*result);
        return Expression(value.getType(), ValueCategory::Rvalue,
                          Constant(cld::match(value.getValue(), [](auto&& value) -> Constant::Variant {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr (std::is_constructible_v<Constant::Variant, T>)
                              {
                                  return value;
                              }
                              CLD_UNREACHABLE;
                          })));
    }
    auto& type = std::holds_alternative<const Declaration*>(*result) ?
                     cld::get<const Declaration*>(*result)->getType() :
                     cld::get<const FunctionDefinition*>(*result)->getType();
    return Expression(type, ValueCategory::Lvalue,
                      DeclarationRead(cld::match(*result, [](auto&& value) -> DeclarationRead::Variant {
                          using T = std::decay_t<decltype(value)>;
                          if constexpr (std::is_constructible_v<DeclarationRead::Variant, T>)
                          {
                              return value;
                          }
                          CLD_UNREACHABLE;
                      })));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionParentheses& node)
{
    return visit(node.getExpression());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpression& node)
{
    return cld::match(node, [&](auto&& value) { return visit(value); });
}

cld::Semantics::Expression
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionSubscript& node)
{
    auto first = lvalueConversion(visit(node.getPostFixExpression()));
    auto second = lvalueConversion(visit(node.getExpression()));
    if (first.isUndefined() || second.isUndefined())
    {
        return {};
    }
    if (!std::holds_alternative<PointerType>(first.getType().get())
        || !std::holds_alternative<PointerType>(second.getType().get()))
    {
        log(Errors::Semantics::EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), first.getType(),
            node.getExpression(), second.getType()));
        return {};
    }
    auto& pointerExpr = std::holds_alternative<PointerType>(first.getType().get()) ? first : second;
    auto& intExpr = &pointerExpr == &first ? second : first;
    if (!std::holds_alternative<PrimitiveType>(intExpr.getType().get())
        || cld::get<PrimitiveType>(intExpr.getType().get()).isFloatingPoint())
    {
        if (&pointerExpr == &first)
        {
            log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(
                node.getExpression(), m_sourceInterface, node.getExpression(), intExpr.getType()));
        }
        else
        {
            log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(
                node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), intExpr.getType()));
        }
        return {};
    }
    auto elementType = cld::get<PointerType>(pointerExpr.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        if (&pointerExpr == &first)
        {
            log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                node.getExpression(), m_sourceInterface, elementType, node.getExpression(), pointerExpr.getType()));
        }
        else
        {
            log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                node.getPostFixExpression(), m_sourceInterface, elementType, node.getPostFixExpression(),
                pointerExpr.getType()));
        }
        return {};
    }
    return Expression(std::move(elementType), ValueCategory::Lvalue,
                      Dereference(std::make_shared<Expression>(
                          elementType, ValueCategory::Rvalue,
                          BinaryOperator(std::make_shared<Expression>(std::move(pointerExpr)), BinaryOperator::Addition,
                                         std::make_shared<Expression>(std::move(intExpr))))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDot& node)
{
    auto structOrUnion = visit(node.getPostFixExpression());
    if (structOrUnion.isUndefined())
    {
        return {};
    }
    if (!std::holds_alternative<StructType>(structOrUnion.getType().get())
        && !std::holds_alternative<UnionType>(structOrUnion.getType().get())
        && !std::holds_alternative<AnonymousStructType>(structOrUnion.getType().get())
        && !std::holds_alternative<AnonymousUnionType>(structOrUnion.getType().get()))
    {
        log(Errors::Semantics::EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_DOT_OPERATOR.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), structOrUnion.getType()));
        return {};
    }
    const std::vector<Field>* fields;
    if (std::holds_alternative<AnonymousUnionType>(structOrUnion.getType().get()))
    {
        fields = &cld::get<AnonymousUnionType>(structOrUnion.getType().get()).getFields();
    }
    if (std::holds_alternative<AnonymousUnionType>(structOrUnion.getType().get()))
    {
        fields = &cld::get<AnonymousUnionType>(structOrUnion.getType().get()).getFields();
    }
    if (std::holds_alternative<StructType>(structOrUnion.getType().get()))
    {
        auto& structType = cld::get<StructType>(structOrUnion.getType().get());
        auto* structDef = getStructDefinition(structType.getName(), structType.getScopeOrId());
        if (!structDef)
        {
            log(Errors::Semantics::STRUCT_N_IS_AN_INCOMPLETE_TYPE.args(
                node.getPostFixExpression(), m_sourceInterface, structType.getName(), node.getPostFixExpression()));
            return {};
        }
        fields = &structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(structOrUnion.getType().get()))
    {
        auto& unionType = cld::get<UnionType>(structOrUnion.getType().get());
        auto* unionDef = getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
        if (!unionDef)
        {
            log(Errors::Semantics::UNION_N_IS_AN_INCOMPLETE_TYPE.args(
                node.getPostFixExpression(), m_sourceInterface, unionType.getName(), node.getPostFixExpression()));
            return {};
        }
        fields = &unionDef->getFields();
    }
    auto result = std::find_if(fields->begin(), fields->end(), [&](const Field& field) {
        return field.name == cld::get<std::string>(node.getIdentifier()->getValue());
    });
    if (result == fields->end())
    {
        if (std::holds_alternative<AnonymousUnionType>(structOrUnion.getType().get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier()));
        }
        if (std::holds_alternative<AnonymousStructType>(structOrUnion.getType().get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier()));
        }
        if (std::holds_alternative<UnionType>(structOrUnion.getType().get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier(),
                cld::get<UnionType>(structOrUnion.getType().get()).getName()));
        }
        if (std::holds_alternative<StructType>(structOrUnion.getType().get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier(),
                cld::get<StructType>(structOrUnion.getType().get()).getName()));
        }
        return {};
    }
    Type type;
    if (result->type->isConst() == structOrUnion.getType().isConst()
        && result->type->isVolatile() == structOrUnion.getType().isVolatile())
    {
        type = *result->type;
    }
    else
    {
        type = Type(structOrUnion.getType().isConst(), structOrUnion.getType().isVolatile(), result->type->get());
    }
    auto category = structOrUnion.getValueCategory();
    return Expression(
        std::move(type), category,
        MemberAccess(std::make_shared<const Expression>(std::move(structOrUnion)), result - fields->begin()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionArrow& node)
{
    auto structOrUnionPtr = visit(node.getPostFixExpression());
    if (structOrUnionPtr.isUndefined())
    {
        return {};
    }
    if (!std::holds_alternative<PointerType>(structOrUnionPtr.getType().get()))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), structOrUnionPtr.getType()));
        return {};
    }
    auto& structOrUnion = cld::get<PointerType>(structOrUnionPtr.getType().get()).getElementType();
    if (!std::holds_alternative<StructType>(structOrUnion.get())
        && !std::holds_alternative<UnionType>(structOrUnion.get())
        && !std::holds_alternative<AnonymousStructType>(structOrUnion.get())
        && !std::holds_alternative<AnonymousUnionType>(structOrUnion.get()))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), structOrUnionPtr.getType()));
        return {};
    }
    const std::vector<Field>* fields;
    if (std::holds_alternative<AnonymousUnionType>(structOrUnion.get()))
    {
        fields = &cld::get<AnonymousUnionType>(structOrUnion.get()).getFields();
    }
    if (std::holds_alternative<AnonymousUnionType>(structOrUnion.get()))
    {
        fields = &cld::get<AnonymousUnionType>(structOrUnion.get()).getFields();
    }
    if (std::holds_alternative<StructType>(structOrUnion.get()))
    {
        auto& structType = cld::get<StructType>(structOrUnion.get());
        auto* structDef = getStructDefinition(structType.getName(), structType.getScopeOrId());
        if (!structDef)
        {
            log(Errors::Semantics::STRUCT_N_IS_AN_INCOMPLETE_TYPE.args(
                node.getPostFixExpression(), m_sourceInterface, structType.getName(), node.getPostFixExpression()));
            return {};
        }
        fields = &structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(structOrUnion.get()))
    {
        auto& unionType = cld::get<UnionType>(structOrUnion.get());
        auto* unionDef = getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
        if (!unionDef)
        {
            log(Errors::Semantics::UNION_N_IS_AN_INCOMPLETE_TYPE.args(
                node.getPostFixExpression(), m_sourceInterface, unionType.getName(), node.getPostFixExpression()));
            return {};
        }
        fields = &unionDef->getFields();
    }
    auto result = std::find_if(fields->begin(), fields->end(), [&](const Field& field) {
        return field.name == cld::get<std::string>(node.getIdentifier()->getValue());
    });
    if (result == fields->end())
    {
        if (std::holds_alternative<AnonymousUnionType>(structOrUnion.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier()));
        }
        if (std::holds_alternative<AnonymousStructType>(structOrUnion.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier()));
        }
        if (std::holds_alternative<UnionType>(structOrUnion.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier(),
                cld::get<UnionType>(structOrUnion.get()).getName()));
        }
        if (std::holds_alternative<StructType>(structOrUnion.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                *node.getIdentifier(), m_sourceInterface, *node.getIdentifier(),
                cld::get<StructType>(structOrUnion.get()).getName()));
        }
        return {};
    }
    Type type;
    if (result->type->isConst() == structOrUnion.isConst() && result->type->isVolatile() == structOrUnion.isVolatile())
    {
        type = *result->type;
    }
    else
    {
        type = Type(structOrUnion.isConst(), structOrUnion.isVolatile(), result->type->get());
    }
    return Expression(std::move(type), ValueCategory::Lvalue,
                      MemberAccess(std::make_shared<const Expression>(
                                       structOrUnion, ValueCategory::Lvalue,
                                       Dereference(std::make_shared<const Expression>(std::move(structOrUnionPtr)))),
                                   result - fields->begin()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionFunctionCall& node) {}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionIncrement& node)
{
    auto value = visit(node.getPostFixExpression());
    if (value.isUndefined())
    {
        return {};
    }
    if (!std::holds_alternative<PrimitiveType>(value.getType().get())
        || !std::holds_alternative<PointerType>(value.getType().get()))
    {
        log(Errors::Semantics::OPERAND_OF_INCREMENT_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), value.getType()));
        return {};
    }
    if (value.getValueCategory() != ValueCategory::Lvalue)
    {
        log(Errors::Semantics::OPERAND_OF_INCREMENT_MUST_BE_AN_LVALUE.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression()));
        return Expression(value.getType(), ValueCategory::Rvalue, {});
    }
    if (std::holds_alternative<PrimitiveType>(value.getType().get()))
    {
        auto type = value.getType();
        return Expression(
            std::move(type), ValueCategory::Rvalue,
            UnaryOperator(UnaryOperator::PostIncrement, std::make_shared<const Expression>(std::move(value))));
    }
    auto& elementType = cld::get<PointerType>(value.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
            node.getPostFixExpression(), m_sourceInterface, elementType, node.getPostFixExpression(), value.getType()));
    }
    auto type = value.getType();
    return Expression(
        std::move(type), ValueCategory::Rvalue,
        UnaryOperator(UnaryOperator::PostIncrement, std::make_shared<const Expression>(std::move(value))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDecrement& node)
{
    auto value = visit(node.getPostFixExpression());
    if (value.isUndefined())
    {
        return {};
    }
    if (!std::holds_alternative<PrimitiveType>(value.getType().get())
        || !std::holds_alternative<PointerType>(value.getType().get()))
    {
        log(Errors::Semantics::OPERAND_OF_DECREMENT_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), value.getType()));
        return {};
    }
    if (value.getValueCategory() != ValueCategory::Lvalue)
    {
        log(Errors::Semantics::OPERAND_OF_DECREMENT_MUST_BE_AN_LVALUE.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression()));
        return Expression(value.getType(), ValueCategory::Rvalue, {});
    }
    if (std::holds_alternative<PrimitiveType>(value.getType().get()))
    {
        auto type = value.getType();
        return Expression(
            std::move(type), ValueCategory::Rvalue,
            UnaryOperator(UnaryOperator::PostDecrement, std::make_shared<const Expression>(std::move(value))));
    }
    auto& elementType = cld::get<PointerType>(value.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
            node.getPostFixExpression(), m_sourceInterface, elementType, node.getPostFixExpression(), value.getType()));
    }
    auto type = value.getType();
    return Expression(
        std::move(type), ValueCategory::Rvalue,
        UnaryOperator(UnaryOperator::PostDecrement, std::make_shared<const Expression>(std::move(value))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionTypeInitializer& node)
{
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpression& node)
{
    return cld::match(node, [&](auto&& value) { return visit(value); });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionPostFixExpression& node)
{
    return visit(node.getPostFixExpression());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionUnaryOperator& node)
{
    auto value = visit(node.getCastExpression());
    switch (node.getOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot: break;
    }
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionSizeOf& node) {}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionDefined& node)
{
    CLD_UNREACHABLE;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::CastExpression& node)
{
    return cld::match(
        node.getVariant(),
        [&](const Syntax::UnaryExpression& unaryExpression) -> Expression { return visit(unaryExpression); },
        [&](const std::pair<Syntax::TypeName, std::unique_ptr<Syntax::CastExpression>>&) -> Expression {
            CLD_UNREACHABLE;
        });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::Term& node)
{
    auto value = visit(node.getCastExpression());
    for (auto& iter : node.getOptionalCastExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::AdditiveExpression& node)
{
    auto value = visit(node.getTerm());
    for (auto& iter : node.getOptionalTerms())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::ShiftExpression& node)
{
    auto value = visit(node.getAdditiveExpression());
    for (auto& iter : node.getOptionalAdditiveExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::RelationalExpression& node)
{
    auto value = visit(node.getShiftExpression());
    for (auto& iter : node.getOptionalShiftExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::EqualityExpression& node)
{
    auto value = visit(node.getRelationalExpression());
    for (auto& iter : node.getOptionalRelationalExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitAndExpression& node)
{
    auto value = visit(node.getEqualityExpressions()[0]);
    for (auto& iter : llvm::ArrayRef(node.getEqualityExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitXorExpression& node)
{
    auto value = visit(node.getBitAndExpressions()[0]);
    for (auto& iter : llvm::ArrayRef(node.getBitAndExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitOrExpression& node)
{
    auto value = visit(node.getBitXorExpressions()[0]);
    for (auto& iter : llvm::ArrayRef(node.getBitXorExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalAndExpression& node)
{
    auto value = visit(node.getBitOrExpressions()[0]);
    for (auto& iter : llvm::ArrayRef(node.getBitOrExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalOrExpression& node)
{
    auto value = visit(node.getAndExpressions()[0]);
    for (auto& iter : llvm::ArrayRef(node.getAndExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::ConditionalExpression& node)
{
    auto condition = visit(node.getLogicalOrExpression());
    if (!node.getOptionalConditionalExpression() && !node.getOptionalExpression())
    {
        return condition;
    }
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::lvalueConversion(Expression expression)
{
    if (std::holds_alternative<ArrayType>(expression.getType().get())
        || std::holds_alternative<ValArrayType>(expression.getType().get()))
    {
        auto elementType = std::holds_alternative<ArrayType>(expression.getType().get()) ?
                               cld::get<ArrayType>(expression.getType().get()).getType() :
                               cld::get<ValArrayType>(expression.getType().get()).getType();
        return Expression(
            PointerType::create(false, false, false, elementType), ValueCategory::Rvalue,
            Conversion(elementType, Conversion::LValue, std::make_shared<Expression>(std::move(expression))));
    }
    if (std::holds_alternative<FunctionType>(expression.getType().get()))
    {
        return Expression(
            PointerType::create(false, false, false, expression.getType()), ValueCategory::Rvalue,
            Conversion(expression.getType(), Conversion::LValue, std::make_shared<Expression>(std::move(expression))));
    }
    // If the expression isn't an lvalue and not qualified then conversion is redundant
    if (expression.getValueCategory() != ValueCategory::Lvalue && !expression.getType().isVolatile()
        && !expression.getType().isConst()
        && (!std::holds_alternative<PointerType>(expression.getType().get())
            || !cld::get<PointerType>(expression.getType().get()).isRestricted()))
    {
        return expression;
    }
    auto newType = Type(false, false, expression.getType().get());
    return Expression(newType, ValueCategory::Rvalue,
                      Conversion(newType, Conversion::LValue, std::make_shared<Expression>(std::move(expression))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::integerPromotion(Expression expression) const
{
    if (!std::holds_alternative<PrimitiveType>(expression.getType().get()))
    {
        return expression;
    }
    auto& prim = cld::get<PrimitiveType>(expression.getType().get());
    if (prim.isFloatingPoint() || prim.getBitCount() == 0
        || prim.getBitCount() >= m_sourceInterface.getLanguageOptions().sizeOfInt * 8)
    {
        return expression;
    }
    auto newType = PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions());
    return Expression(
        newType, ValueCategory::Rvalue,
        Conversion(newType, Conversion::IntegerPromotion, std::make_shared<Expression>(std::move(expression))));
}

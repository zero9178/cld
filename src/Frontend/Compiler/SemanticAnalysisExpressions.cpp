#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

namespace
{
cld::Semantics::Type removeQualifiers(cld::Semantics::Type type)
{
    if (type.isConst() || type.isVolatile()
        || (std::holds_alternative<cld::Semantics::PointerType>(type.get())
            && cld::get<cld::Semantics::PointerType>(type.get()).isRestricted()))
    {
        if (!std::holds_alternative<cld::Semantics::PointerType>(type.get())
            || !cld::get<cld::Semantics::PointerType>(type.get()).isRestricted())
        {
            return cld::Semantics::Type(false, false, std::move(type).get());
        }
        return cld::Semantics::PointerType::create(false, false, false,
                                                   cld::get<cld::Semantics::PointerType>(type.get()).getElementType());
    }
    return type;
}

} // namespace

llvm::ArrayRef<cld::Semantics::Field>
    cld::Semantics::SemanticAnalysis::getFields(const cld::Semantics::Type& recordType) const
{
    if (std::holds_alternative<AnonymousUnionType>(recordType.get()))
    {
        return cld::get<AnonymousUnionType>(recordType.get()).getFields();
    }
    if (std::holds_alternative<AnonymousStructType>(recordType.get()))
    {
        return cld::get<AnonymousStructType>(recordType.get()).getFields();
    }
    if (std::holds_alternative<StructType>(recordType.get()))
    {
        auto& structType = cld::get<StructType>(recordType.get());
        auto* structDef = getStructDefinition(structType.getName(), structType.getScopeOrId());
        CLD_ASSERT(structDef);
        return structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(recordType.get()))
    {
        auto& unionType = cld::get<UnionType>(recordType.get());
        auto* unionDef = getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
        CLD_ASSERT(unionDef);
        return unionDef->getFields();
    }
    CLD_UNREACHABLE;
}

bool cld::Semantics::SemanticAnalysis::isBitfieldAccess(const Expression& expression) const
{
    if (!std::holds_alternative<MemberAccess>(expression.get()))
    {
        return false;
    }
    auto& mem = cld::get<MemberAccess>(expression.get());
    auto& expr = mem.getRecordExpression();
    auto& recordType = expr.getType();
    auto fields = getFields(recordType);
    return static_cast<bool>(fields[mem.getMemberIndex()].bitFieldSize);
}

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
            case Lexer::CToken::Type::UnsignedShort:
                return Expression(
                    PrimitiveType::createUnsignedShort(false, false, m_sourceInterface.getLanguageOptions()),
                    ValueCategory::Rvalue, Constant(node.getValue()));
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
                              string.size() + 1),
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
                                                        string.characters.size() + 1),
                                      ValueCategory::Rvalue, node.getValue());
                case LanguageOptions::WideCharType::Int:
                    return Expression(ArrayType::create(false, false, false, false,
                                                        PrimitiveType::createInt(
                                                            false, false, m_sourceInterface.getLanguageOptions()),
                                                        string.characters.size() + 1),
                                      ValueCategory::Rvalue, node.getValue());
            }
            CLD_UNREACHABLE;
        }
    }
    CLD_UNREACHABLE;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionIdentifier& node)
{
    auto* result = lookupDecl(node.getIdentifier()->getText());
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
        && !std::holds_alternative<PointerType>(second.getType().get()))
    {
        log(Errors::Semantics::EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE.args(
            node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), first.getType(),
            node.getExpression(), second.getType()));
        return {};
    }
    auto& pointerExpr = std::holds_alternative<PointerType>(first.getType().get()) ? first : second;
    auto& intExpr = &pointerExpr == &first ? second : first;
    if (!isInteger(intExpr.getType()))
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
            log(Errors::Semantics::POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
                node.getPostFixExpression(), m_sourceInterface, elementType, node.getPostFixExpression(),
                pointerExpr.getType()));
        }
        else
        {
            log(Errors::Semantics::POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
                node.getExpression(), m_sourceInterface, elementType, node.getExpression(), pointerExpr.getType()));
        }
        return {};
    }
    else if (std::holds_alternative<FunctionType>(elementType.get()))
    {
        if (&pointerExpr == &first)
        {
            log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
                node.getPostFixExpression(), m_sourceInterface, node.getPostFixExpression(), pointerExpr.getType()));
        }
        else
        {
            log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
                node.getExpression(), m_sourceInterface, node.getExpression(), pointerExpr.getType()));
        }
        return {};
    }
    auto pointerType = pointerExpr.getType();
    return Expression(
        std::move(elementType), ValueCategory::Lvalue,
        UnaryOperator(UnaryOperator::Dereference,
                      std::make_shared<Expression>(pointerType, ValueCategory::Rvalue,
                                                   BinaryOperator(std::make_shared<Expression>(std::move(pointerExpr)),
                                                                  BinaryOperator::Addition,
                                                                  std::make_shared<Expression>(std::move(intExpr))))));
}

std::optional<std::pair<cld::Semantics::Type, std::uint64_t>> cld::Semantics::SemanticAnalysis::checkMemberAccess(
    const Type& recordType, const Syntax::PostFixExpression& postFixExpr, const Lexer::CToken& identifier)
{
    const std::vector<Field>* fields;
    if (std::holds_alternative<AnonymousUnionType>(recordType.get()))
    {
        fields = &cld::get<AnonymousUnionType>(recordType.get()).getFields();
    }
    if (std::holds_alternative<AnonymousStructType>(recordType.get()))
    {
        fields = &cld::get<AnonymousStructType>(recordType.get()).getFields();
    }
    if (std::holds_alternative<StructType>(recordType.get()))
    {
        auto& structType = cld::get<StructType>(recordType.get());
        auto* structDef = getStructDefinition(structType.getName(), structType.getScopeOrId());
        if (!structDef)
        {
            log(Errors::Semantics::STRUCT_N_IS_AN_INCOMPLETE_TYPE.args(postFixExpr, m_sourceInterface,
                                                                       structType.getName(), postFixExpr));
            return {};
        }
        fields = &structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(recordType.get()))
    {
        auto& unionType = cld::get<UnionType>(recordType.get());
        auto* unionDef = getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
        if (!unionDef)
        {
            log(Errors::Semantics::UNION_N_IS_AN_INCOMPLETE_TYPE.args(postFixExpr, m_sourceInterface,
                                                                      unionType.getName(), postFixExpr));
            return {};
        }
        fields = &unionDef->getFields();
    }
    CLD_ASSERT(fields);
    auto result = std::find_if(fields->begin(), fields->end(),
                               [&](const Field& field) { return field.name == identifier.getText(); });
    if (result == fields->end())
    {
        if (std::holds_alternative<AnonymousUnionType>(recordType.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(identifier, m_sourceInterface,
                                                                                    identifier));
        }
        if (std::holds_alternative<AnonymousStructType>(recordType.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(identifier, m_sourceInterface,
                                                                                     identifier));
        }
        if (std::holds_alternative<UnionType>(recordType.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(
                identifier, m_sourceInterface, identifier, cld::get<UnionType>(recordType.get()).getName()));
        }
        if (std::holds_alternative<StructType>(recordType.get()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                identifier, m_sourceInterface, identifier, cld::get<StructType>(recordType.get()).getName()));
        }
        return {};
    }
    Type type;
    if (result->type->isConst() == recordType.isConst() && result->type->isVolatile() == recordType.isVolatile())
    {
        type = *result->type;
    }
    else
    {
        type = Type(recordType.isConst(), recordType.isVolatile(), result->type->get());
    }
    return std::pair(std::move(type), result - fields->begin());
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
    auto result = checkMemberAccess(structOrUnion.getType(), node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return {};
    }
    auto& [type, index] = *result;
    if (type.isUndefined())
    {
        return {};
    }
    auto category = structOrUnion.getValueCategory();
    return Expression(std::move(type), category,
                      MemberAccess(std::make_shared<Expression>(std::move(structOrUnion)), index));
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
    auto result = checkMemberAccess(structOrUnion, node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return {};
    }
    auto& [type, index] = *result;
    return Expression(std::move(type), ValueCategory::Lvalue,
                      MemberAccess(std::make_shared<Expression>(
                                       structOrUnion, ValueCategory::Lvalue,
                                       UnaryOperator(UnaryOperator::Dereference,
                                                     std::make_shared<Expression>(std::move(structOrUnionPtr)))),
                                   index));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionFunctionCall& node) {}

template <class Expr>
cld::Semantics::Expression cld::Semantics::SemanticAnalysis::checkIncrementAndDecrement(UnaryOperator::Kind kind,
                                                                                        Expression&& value,
                                                                                        const Lexer::TokenBase& opToken,
                                                                                        const Expr& syntaxExpr)
{
    if (value.isUndefined())
    {
        return {};
    }
    if (!isScalar(value.getType()))
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            syntaxExpr, m_sourceInterface, opToken, syntaxExpr, value.getType()));
        return {};
    }
    if (value.getValueCategory() != ValueCategory::Lvalue)
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_BE_AN_LVALUE.args(syntaxExpr, m_sourceInterface, opToken, syntaxExpr));
    }
    else if (value.getType().isConst())
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_NOT_BE_CONST.args(syntaxExpr, m_sourceInterface, opToken, syntaxExpr,
                                                                   value.getType()));
    }
    if (isArithmetic(value.getType()))
    {
        auto type = removeQualifiers(value.getType());
        return Expression(std::move(type), ValueCategory::Rvalue,
                          UnaryOperator(kind, std::make_shared<Expression>(std::move(value))));
    }
    auto& elementType = cld::get<PointerType>(value.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
            syntaxExpr, m_sourceInterface, elementType, syntaxExpr, value.getType()));
    }
    else if (std::holds_alternative<FunctionType>(elementType.get()))
    {
        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
            syntaxExpr, m_sourceInterface, syntaxExpr, value.getType()));
        return {};
    }
    auto type = removeQualifiers(value.getType());
    return Expression(std::move(type), ValueCategory::Rvalue,
                      UnaryOperator(kind, std::make_shared<Expression>(std::move(value))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionIncrement& node)
{
    return checkIncrementAndDecrement(UnaryOperator::PostIncrement, visit(node.getPostFixExpression()),
                                      node.getIncrementToken(), node.getPostFixExpression());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDecrement& node)
{
    return checkIncrementAndDecrement(UnaryOperator::PostDecrement, visit(node.getPostFixExpression()),
                                      node.getDecrementToken(), node.getPostFixExpression());
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
    if (value.isUndefined())
    {
        return {};
    }
    switch (node.getOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
            return checkIncrementAndDecrement(UnaryOperator::PreIncrement, visit(node.getCastExpression()),
                                              node.getUnaryToken(), node.getCastExpression());
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
            return checkIncrementAndDecrement(UnaryOperator::PreDecrement, visit(node.getCastExpression()),
                                              node.getUnaryToken(), node.getCastExpression());
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        {
            if (std::holds_alternative<UnaryOperator>(value.get())
                && cld::get<UnaryOperator>(value.get()).getKind() == UnaryOperator::Dereference)
            {
                auto& dereference = cld::get<UnaryOperator>(value.get());
                auto& exp = dereference.getOperand();
                return Expression(exp.getType(), ValueCategory::Rvalue, exp.get());
            }
            // TODO: Call Expression
            if (std::holds_alternative<DeclarationRead>(value.get()))
            {
                auto& declRead = cld::get<DeclarationRead>(value.get());
                if (std::holds_alternative<const Declaration*>(declRead.getDeclRead())
                    && cld::get<const Declaration*>(declRead.getDeclRead())->getLifetime() == Lifetime::Register)
                {
                    log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_DECLARATION_ANNOTATED_WITH_REGISTER.args(
                        node.getCastExpression(), m_sourceInterface, node.getUnaryToken(), node.getCastExpression()));
                }
            }
            if (isBitfieldAccess(value))
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_BITFIELD.args(
                    node.getCastExpression(), m_sourceInterface, node.getUnaryToken(), node.getCastExpression()));
            }
            if (value.getValueCategory() != ValueCategory::Lvalue)
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_TEMPORARY.args(
                    node.getCastExpression(), m_sourceInterface, node.getUnaryToken(), node.getCastExpression()));
            }
            auto type = value.getType();
            return Expression(PointerType::create(false, false, false, std::move(type)), ValueCategory::Rvalue,
                              UnaryOperator(UnaryOperator::AddressOf, std::make_shared<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        {
            value = lvalueConversion(std::move(value));
            if (!std::holds_alternative<PointerType>(value.getType().get()))
            {
                log(Errors::Semantics::CANNOT_DEREFERENCE_NON_POINTER_TYPE_N.args(
                    node.getCastExpression(), m_sourceInterface, value.getType(), node.getCastExpression(),
                    node.getUnaryToken()));
                return {};
            }
            auto elementType = cld::get<PointerType>(value.getType().get()).getElementType();
            return Expression(
                std::move(elementType), ValueCategory::Lvalue,
                UnaryOperator(UnaryOperator::Dereference, std::make_shared<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            value = integerPromotion(std::move(value));
            if (!isArithmetic(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                    node.getUnaryToken(), m_sourceInterface, node.getUnaryToken(), node.getCastExpression(),
                    value.getType()));
                return {};
            }
            auto type = value.getType();
            return Expression(
                std::move(type), ValueCategory::Rvalue,
                UnaryOperator(node.getOperator() == Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus ?
                                  UnaryOperator::Minus :
                                  UnaryOperator::Plus,
                              std::make_shared<Expression>(std::move(value))));
        }
        break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            value = integerPromotion(std::move(value));
            if (!isInteger(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                    node.getUnaryToken(), m_sourceInterface, node.getUnaryToken(), node.getCastExpression(),
                    value.getType()));
                return {};
            }
            auto type = value.getType();
            return Expression(
                std::move(type), ValueCategory::Rvalue,
                UnaryOperator(UnaryOperator::BitwiseNegate, std::make_shared<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            value = integerPromotion(std::move(value));
            if (!isScalar(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    node.getUnaryToken(), m_sourceInterface, node.getUnaryToken(), node.getCastExpression(),
                    value.getType()));
                return {};
            }
            auto type = value.getType();
            return Expression(
                std::move(type), ValueCategory::Rvalue,
                UnaryOperator(UnaryOperator::BooleanNegate, std::make_shared<Expression>(std::move(value))));
        }
    }
    CLD_UNREACHABLE;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionSizeOf& node)
{
    return cld::match(
        node.getVariant(),
        [&](const std::unique_ptr<Syntax::UnaryExpression>& unaryExpression) -> Expression {
            auto exp = visit(*unaryExpression);
            if (exp.isUndefined())
            {
                return {};
            }
            auto& type = exp.getType();
            if (!isCompleteType(type))
            {
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(*unaryExpression, m_sourceInterface, type,
                                                                         *unaryExpression));
                return {};
            }
            else if (std::holds_alternative<FunctionType>(type.get()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(*unaryExpression, m_sourceInterface,
                                                                                 *unaryExpression, type));
                return {};
            }
            if (isBitfieldAccess(exp))
            {
                log(Errors::Semantics::BITFIELD_NOT_ALLOWED_IN_SIZE_OF.args(*unaryExpression, m_sourceInterface,
                                                                            *unaryExpression));
            }
            auto size = exp.getType().getSizeOf(*this);
            return Expression(PrimitiveType::createUnsignedLongLong(false, false), ValueCategory::Rvalue,
                              SizeofOperator(size, std::make_shared<Expression>(std::move(exp))));
        },
        [&](const std::unique_ptr<Syntax::TypeName>& typeName) -> Expression {
            auto type = declaratorsToType(typeName->getSpecifierQualifiers(), typeName->getAbstractDeclarator());
            if (type.isUndefined())
            {
                return {};
            }
            if (!isCompleteType(type))
            {
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(*typeName, m_sourceInterface, type,
                                                                         *typeName));
                return {};
            }
            else if (std::holds_alternative<FunctionType>(type.get()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(*typeName, m_sourceInterface,
                                                                                 *typeName, type));
                return {};
            }
            auto size = type.getSizeOf(*this);
            return Expression(PrimitiveType::createUnsignedLongLong(false, false), ValueCategory::Rvalue,
                              SizeofOperator(size, std::move(type)));
        });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionDefined&)
{
    CLD_UNREACHABLE;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::CastExpression& node)
{
    return cld::match(
        node.getVariant(),
        [&](const Syntax::UnaryExpression& unaryExpression) -> Expression { return visit(unaryExpression); },
        [&](const std::pair<Syntax::TypeName, std::unique_ptr<Syntax::CastExpression>>& cast) -> Expression {
            auto type = declaratorsToType(cast.first.getSpecifierQualifiers(), cast.first.getAbstractDeclarator());
            if (type.isUndefined())
            {
                visit(*cast.second);
                return {};
            }
            auto value = visit(*cast.second);
            if (value.isUndefined())
            {
                return {};
            }
            value = lvalueConversion(std::move(value));
            if (!isScalar(type))
            {
                log(Errors::Semantics::TYPE_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    cast.first, m_sourceInterface, cast.first, type));
                return {};
            }
            type = lvalueConversion(std::move(type));
            if (!isScalar(value.getType()))
            {
                log(Errors::Semantics::EXPRESSION_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    *cast.second, m_sourceInterface, *cast.second, value.getType()));
                return {};
            }
            return Expression(type, ValueCategory::Rvalue,
                              Conversion(type, Conversion::Explicit, std::make_shared<Expression>(std::move(value))));
        });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::Term& node)
{
    auto value = visit(node.getCastExpression());
    if (node.getOptionalCastExpressions().empty())
    {
        return value;
    }
    for (auto& iter : node.getOptionalCastExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::AdditiveExpression& node)
{
    auto value = visit(node.getTerm());
    if (node.getOptionalTerms().empty())
    {
        return value;
    }
    for (auto& iter : node.getOptionalTerms())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::ShiftExpression& node)
{
    auto value = visit(node.getAdditiveExpression());
    if (node.getOptionalAdditiveExpressions().empty())
    {
        return value;
    }
    for (auto& iter : node.getOptionalAdditiveExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::RelationalExpression& node)
{
    auto value = visit(node.getShiftExpression());
    if (node.getOptionalShiftExpressions().empty())
    {
        return value;
    }
    for (auto& iter : node.getOptionalShiftExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::EqualityExpression& node)
{
    auto value = visit(node.getRelationalExpression());
    if (node.getOptionalRelationalExpressions().empty())
    {
        return value;
    }
    for (auto& iter : node.getOptionalRelationalExpressions())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitAndExpression& node)
{
    auto value = visit(node.getEqualityExpressions()[0]);
    if (node.getEqualityExpressions().size() == 1)
    {
        return value;
    }
    for (auto& iter : llvm::ArrayRef(node.getEqualityExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitXorExpression& node)
{
    auto value = visit(node.getBitAndExpressions()[0]);
    if (node.getBitAndExpressions().size() == 1)
    {
        return value;
    }
    for (auto& iter : llvm::ArrayRef(node.getBitAndExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitOrExpression& node)
{
    auto value = visit(node.getBitXorExpressions()[0]);
    if (node.getBitXorExpressions().size() == 1)
    {
        return value;
    }
    for (auto& iter : llvm::ArrayRef(node.getBitXorExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalAndExpression& node)
{
    auto value = visit(node.getBitOrExpressions()[0]);
    if (node.getBitOrExpressions().size() == 1)
    {
        return value;
    }
    for (auto& iter : llvm::ArrayRef(node.getBitOrExpressions()).drop_front())
    {
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalOrExpression& node)
{
    auto value = visit(node.getAndExpressions()[0]);
    if (node.getAndExpressions().size() == 1)
    {
        return value;
    }
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

cld::Semantics::Type cld::Semantics::SemanticAnalysis::lvalueConversion(Type type)
{
    if (std::holds_alternative<ArrayType>(type.get()) || std::holds_alternative<ValArrayType>(type.get()))
    {
        auto elementType = std::holds_alternative<ArrayType>(type.get()) ? cld::get<ArrayType>(type.get()).getType() :
                                                                           cld::get<ValArrayType>(type.get()).getType();
        return PointerType::create(false, false, false, elementType);
    }
    if (std::holds_alternative<FunctionType>(type.get()))
    {
        return PointerType::create(false, false, false, type);
    }
    // If the expression isn't an lvalue and not qualified then conversion is redundant
    if (!type.isVolatile() && !type.isConst()
        && (!std::holds_alternative<PointerType>(type.get()) || !cld::get<PointerType>(type.get()).isRestricted()))
    {
        return type;
    }
    return Type(false, false, type.get());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::integerPromotion(Expression expression) const
{
    expression = lvalueConversion(std::move(expression));
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

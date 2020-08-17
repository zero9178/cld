#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

cld::Semantics::Type cld::Semantics::SemanticAnalysis::removeQualifiers(Type type)
{
    if (type.isConst() || type.isVolatile()
        || (std::holds_alternative<PointerType>(type.get()) && cld::get<PointerType>(type.get()).isRestricted()))
    {
        if (!std::holds_alternative<PointerType>(type.get()) || !cld::get<PointerType>(type.get()).isRestricted())
        {
            return Type(false, false, std::move(type).get());
        }
        return PointerType::create(false, false, false,
                                   cld::get<cld::Semantics::PointerType>(type.get()).getElementType());
    }
    return type;
}

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
    auto& recordType = std::holds_alternative<PointerType>(expr.getType().get()) ?
                           cld::get<PointerType>(expr.getType().get()).getElementType() :
                           expr.getType();
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
                    ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::Int:
                return Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::UnsignedInt:
                return Expression(
                    PrimitiveType::createUnsignedInt(false, false, m_sourceInterface.getLanguageOptions()),
                    ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::Long:
                return Expression(PrimitiveType::createLong(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::UnsignedLong:
                return Expression(
                    PrimitiveType::createUnsignedLong(false, false, m_sourceInterface.getLanguageOptions()),
                    ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::LongLong:
                return Expression(PrimitiveType::createLongLong(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::UnsignedLongLong:
                return Expression(PrimitiveType::createUnsignedLongLong(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::Float:
                return Expression(PrimitiveType::createFloat(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::Double:
                return Expression(PrimitiveType::createDouble(false, false), ValueCategory::Rvalue,
                                  Constant(node.getValue(), node.begin(), node.end()));
            case Lexer::CToken::Type::LongDouble:
                return Expression(PrimitiveType::createLongDouble(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
        }
    }
    else if (std::holds_alternative<std::string>(node.getValue()))
    {
        auto& string = cld::get<std::string>(node.getValue());
        return Expression(
            ArrayType::create(false, false, false, false,
                              PrimitiveType::createChar(false, false, m_sourceInterface.getLanguageOptions()),
                              string.size() + 1),
            ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
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
                                      ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
                case LanguageOptions::WideCharType::Int:
                    return Expression(ArrayType::create(false, false, false, false,
                                                        PrimitiveType::createInt(
                                                            false, false, m_sourceInterface.getLanguageOptions()),
                                                        string.characters.size() + 1),
                                      ValueCategory::Rvalue, Constant(node.getValue(), node.begin(), node.end()));
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
                          Constant(cld::match(value.getValue(),
                                              [](auto&& value) -> Constant::Variant {
                                                  using T = std::decay_t<decltype(value)>;
                                                  if constexpr (std::is_constructible_v<Constant::Variant, T>)
                                                  {
                                                      return value;
                                                  }
                                                  CLD_UNREACHABLE;
                                              }),
                                   node.getIdentifier(), node.getIdentifier() + 1));
    }
    auto& type = std::holds_alternative<const Declaration*>(*result) ?
                     cld::get<const Declaration*>(*result)->getType() :
                     cld::get<const FunctionDefinition*>(*result)->getType();
    return Expression(type, ValueCategory::Lvalue,
                      DeclarationRead(cld::match(*result,
                                                 [](auto&& value) -> DeclarationRead::Variant {
                                                     using T = std::decay_t<decltype(value)>;
                                                     if constexpr (std::is_constructible_v<DeclarationRead::Variant, T>)
                                                     {
                                                         return value;
                                                     }
                                                     CLD_UNREACHABLE;
                                                 }),
                                      node.getIdentifier()));
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
        log(Errors::Semantics::EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE.args(node.getPostFixExpression(),
                                                                               m_sourceInterface, first, second));
        return {};
    }
    auto& pointerExpr = std::holds_alternative<PointerType>(first.getType().get()) ? first : second;
    auto& intExpr = &pointerExpr == &first ? second : first;
    if (!isInteger(intExpr.getType()))
    {
        log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(intExpr, m_sourceInterface, intExpr));
        return {};
    }
    auto elementType = cld::get<PointerType>(pointerExpr.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            pointerExpr, m_sourceInterface, elementType, pointerExpr));
        return {};
    }
    else if (std::holds_alternative<FunctionType>(elementType.get()))
    {
        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            pointerExpr, m_sourceInterface, pointerExpr));
        return {};
    }
    auto pointerType = pointerExpr.getType();
    return Expression(std::move(elementType), ValueCategory::Lvalue,
                      SubscriptOperator(std::make_unique<Expression>(std::move(first)), node.getOpenBracket(),
                                        std::make_unique<Expression>(std::move(second)), node.getCloseBracket()));
}

std::optional<std::pair<cld::Semantics::Type, std::uint64_t>> cld::Semantics::SemanticAnalysis::checkMemberAccess(
    const Type& recordType, const Syntax::PostFixExpression& postFixExpr, const Lexer::CToken& identifier)
{
    const std::vector<Field>* fields = nullptr;
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
    if (!isRecord(structOrUnion.getType()))
    {
        log(Errors::Semantics::EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_DOT_OPERATOR.args(
            structOrUnion, m_sourceInterface, structOrUnion));
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
    return Expression(
        std::move(type), category,
        MemberAccess(std::make_unique<Expression>(std::move(structOrUnion)), index, node.getIdentifier()));
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
            structOrUnionPtr, m_sourceInterface, structOrUnionPtr));
        return {};
    }
    auto& structOrUnion = cld::get<PointerType>(structOrUnionPtr.getType().get()).getElementType();
    if (!isRecord(structOrUnion))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR.args(
            structOrUnionPtr, m_sourceInterface, structOrUnionPtr));
        return {};
    }
    auto result = checkMemberAccess(structOrUnion, node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return {};
    }
    auto& [type, index] = *result;
    return Expression(
        std::move(type), ValueCategory::Lvalue,
        MemberAccess(std::make_unique<Expression>(std::move(structOrUnionPtr)), index, node.getIdentifier()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionFunctionCall& node) {}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::checkIncrementAndDecrement(UnaryOperator::Kind kind,
                                                                                        Expression&& value,
                                                                                        Lexer::CTokenIterator opToken)
{
    if (value.isUndefined())
    {
        return {};
    }
    if (!isScalar(value.getType()))
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(value, m_sourceInterface,
                                                                                       *opToken, value));
        return {};
    }
    if (value.getValueCategory() != ValueCategory::Lvalue)
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_BE_AN_LVALUE.args(value, m_sourceInterface, *opToken, value));
    }
    else if (value.getType().isConst())
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_NOT_BE_CONST.args(value, m_sourceInterface, *opToken, value));
    }
    if (isArithmetic(value.getType()))
    {
        auto type = removeQualifiers(value.getType());
        return Expression(std::move(type), ValueCategory::Rvalue,
                          UnaryOperator(kind, opToken, std::make_unique<Expression>(std::move(value))));
    }
    auto& elementType = cld::get<PointerType>(value.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(value, m_sourceInterface, elementType,
                                                                                 value, value.getType()));
    }
    else if (std::holds_alternative<FunctionType>(elementType.get()))
    {
        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(value, m_sourceInterface,
                                                                                               value));
        return {};
    }
    auto type = removeQualifiers(value.getType());
    return Expression(std::move(type), ValueCategory::Rvalue,
                      UnaryOperator(kind, opToken, std::make_unique<Expression>(std::move(value))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionIncrement& node)
{
    return checkIncrementAndDecrement(UnaryOperator::PostIncrement, visit(node.getPostFixExpression()),
                                      node.getIncrementToken());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDecrement& node)
{
    return checkIncrementAndDecrement(UnaryOperator::PostDecrement, visit(node.getPostFixExpression()),
                                      node.getDecrementToken());
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
                                              node.getUnaryToken());
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
            return checkIncrementAndDecrement(UnaryOperator::PreDecrement, visit(node.getCastExpression()),
                                              node.getUnaryToken());
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        {
            // TODO: Call Expression
            if (std::holds_alternative<DeclarationRead>(value.get()))
            {
                auto& declRead = cld::get<DeclarationRead>(value.get());
                if (std::holds_alternative<const Declaration*>(declRead.getDeclRead())
                    && cld::get<const Declaration*>(declRead.getDeclRead())->getLifetime() == Lifetime::Register)
                {
                    log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_DECLARATION_ANNOTATED_WITH_REGISTER.args(
                        value, m_sourceInterface, *node.getUnaryToken(), value));
                }
            }
            if (isBitfieldAccess(value))
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_BITFIELD.args(value, m_sourceInterface,
                                                                            *node.getUnaryToken(), value));
            }
            if (value.getValueCategory() != ValueCategory::Lvalue)
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_TEMPORARY.args(value, m_sourceInterface,
                                                                             *node.getUnaryToken(), value));
            }
            auto type = value.getType();
            return Expression(PointerType::create(false, false, false, std::move(type)), ValueCategory::Rvalue,
                              UnaryOperator(UnaryOperator::AddressOf, node.getUnaryToken(),
                                            std::make_unique<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        {
            value = lvalueConversion(std::move(value));
            if (!std::holds_alternative<PointerType>(value.getType().get()))
            {
                log(Errors::Semantics::CANNOT_DEREFERENCE_NON_POINTER_TYPE_N.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), value));
                return {};
            }
            auto elementType = cld::get<PointerType>(value.getType().get()).getElementType();
            return Expression(std::move(elementType), ValueCategory::Lvalue,
                              UnaryOperator(UnaryOperator::Dereference, node.getUnaryToken(),
                                            std::make_unique<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            value = integerPromotion(std::move(value));
            if (!isArithmetic(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), value));
                return {};
            }
            auto type = value.getType();
            return Expression(
                std::move(type), ValueCategory::Rvalue,
                UnaryOperator(node.getOperator() == Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus ?
                                  UnaryOperator::Minus :
                                  UnaryOperator::Plus,
                              node.getUnaryToken(), std::make_unique<Expression>(std::move(value))));
        }
        break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            value = integerPromotion(std::move(value));
            if (!isInteger(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), value));
                return {};
            }
            auto type = value.getType();
            return Expression(std::move(type), ValueCategory::Rvalue,
                              UnaryOperator(UnaryOperator::BitwiseNegate, node.getUnaryToken(),
                                            std::make_unique<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            value = integerPromotion(std::move(value));
            if (!isScalar(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), value));
                return {};
            }
            auto type = value.getType();
            return Expression(std::move(type), ValueCategory::Rvalue,
                              UnaryOperator(UnaryOperator::BooleanNegate, node.getUnaryToken(),
                                            std::make_unique<Expression>(std::move(value))));
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
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(exp, m_sourceInterface, type, exp));
                return {};
            }
            else if (std::holds_alternative<FunctionType>(type.get()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(exp, m_sourceInterface, exp, type));
                return {};
            }
            if (isBitfieldAccess(exp))
            {
                log(Errors::Semantics::BITFIELD_NOT_ALLOWED_IN_SIZE_OF.args(exp, m_sourceInterface, exp));
            }
            auto size = exp.getType().getSizeOf(*this);
            return Expression(
                getSizeT(m_sourceInterface.getLanguageOptions()), ValueCategory::Rvalue,
                SizeofOperator(node.getSizeOfToken(), size, std::make_unique<Expression>(std::move(exp))));
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
            return Expression(getSizeT(m_sourceInterface.getLanguageOptions()), ValueCategory::Rvalue,
                              SizeofOperator(node.getSizeOfToken(), size,
                                             SizeofOperator::TypeVariant{node.getSizeOfToken() + 1, std::move(type),
                                                                         node.end() - 1}));
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
        [&](const Syntax::CastExpression::CastVariant& cast) -> Expression {
            auto type =
                declaratorsToType(cast.typeName.getSpecifierQualifiers(), cast.typeName.getAbstractDeclarator());
            if (type.isUndefined())
            {
                visit(*cast.cast);
                return {};
            }
            auto value = visit(*cast.cast);
            if (value.isUndefined())
            {
                return {};
            }
            value = lvalueConversion(std::move(value));
            if (!isScalar(type))
            {
                log(Errors::Semantics::TYPE_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    cast.typeName, m_sourceInterface, cast.typeName, type));
                return {};
            }
            type = lvalueConversion(std::move(type));
            if (!isScalar(value.getType()))
            {
                log(Errors::Semantics::EXPRESSION_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    value, m_sourceInterface, value));
                return {};
            }
            return Expression(type, ValueCategory::Rvalue,
                              Cast(cast.openParentheses, type, cast.closeParentheses,
                                   std::make_unique<Expression>(std::move(value))));
        });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::Term& node)
{
    auto value = visit(node.getCastExpression());
    if (node.getOptionalCastExpressions().empty())
    {
        return value;
    }
    for (auto& [kind, token, rhs] : node.getOptionalCastExpressions())
    {
        auto rhsValue = visit(rhs);
        switch (kind)
        {
            case Syntax::Term::BinaryDivide:
            case Syntax::Term::BinaryMultiply:
            {
                arithmeticConversion(value, rhsValue);
                bool errors = false;
                if (!value.isUndefined() && !isArithmetic(value.getType()))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        value, m_sourceInterface, *token, value));
                    errors = true;
                }
                if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                    errors = true;
                }
                if (value.isUndefined() || rhsValue.isUndefined() || errors)
                {
                    value = {};
                    continue;
                }
                auto type = value.getType();
                value = Expression(std::move(type), ValueCategory::Rvalue,
                                   BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                  kind == Syntax::Term::BinaryDivide ? BinaryOperator::Divide :
                                                                                       BinaryOperator::Multiply,
                                                  token, std::make_unique<Expression>(std::move(rhsValue))));
                continue;
            }
            case Syntax::Term::BinaryModulo:
            {
                arithmeticConversion(value, rhsValue);
                bool errors = false;
                if (!value.isUndefined() && !isInteger(value.getType()))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        value, m_sourceInterface, *token, value));
                    errors = true;
                }
                if (!rhsValue.isUndefined() && !isInteger(rhsValue.getType()))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                    errors = true;
                }
                if (value.isUndefined() || rhsValue.isUndefined() || errors)
                {
                    value = {};
                    continue;
                }
                auto type = value.getType();
                value =
                    Expression(std::move(type), ValueCategory::Rvalue,
                               BinaryOperator(std::make_unique<Expression>(std::move(value)), BinaryOperator::Modulo,
                                              token, std::make_unique<Expression>(std::move(rhsValue))));
                continue;
            }
        }
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
    for (auto& [kind, token, rhs] : node.getOptionalTerms())
    {
        auto rhsValue = visit(rhs);
        if (isArithmetic(value.getType()) && isArithmetic(rhsValue.getType()))
        {
            arithmeticConversion(value, rhsValue);
        }
        else
        {
            value = lvalueConversion(std::move(value));
            rhsValue = lvalueConversion(std::move(rhsValue));
        }
        bool errors = false;
        if (!value.isUndefined() && !isScalar(value.getType()))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                value, m_sourceInterface, *token, value));
            errors = true;
        }
        if (!rhsValue.isUndefined() && !isScalar(rhsValue.getType()))
        {
            log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                rhsValue, m_sourceInterface, *token, rhsValue));
            errors = true;
        }
        switch (kind)
        {
            case Syntax::AdditiveExpression::BinaryMinus:
            {
                if (std::holds_alternative<PrimitiveType>(value.getType().get())
                    && std::holds_alternative<PrimitiveType>(rhsValue.getType().get()))
                {
                    auto type = value.getType();
                    value = Expression(std::move(type), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Subtraction, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                    continue;
                }
                if (value.isUndefined() || rhsValue.isUndefined() || errors)
                {
                    value = {};
                    continue;
                }
                if (std::holds_alternative<PrimitiveType>(value.getType().get())
                    && std::holds_alternative<PointerType>(rhsValue.getType().get()))
                {
                    log(Errors::Semantics::CANNOT_SUBTRACT_POINTER_FROM_ARITHMETIC_TYPE.args(value, m_sourceInterface,
                                                                                             value, *token, rhsValue));
                    auto type = rhsValue.getType();
                    value = Expression(std::move(type), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Subtraction, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                    continue;
                }
                // value is guaranteed to be a pointer type now, rhsValue is a scalar
                auto valueElementType = cld::get<PointerType>(value.getType().get()).getElementType();
                if (!isCompleteType(valueElementType))
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                        value, m_sourceInterface, valueElementType, value, value.getType()));
                }
                else if (std::holds_alternative<FunctionType>(valueElementType.get()))
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        value, m_sourceInterface, value));
                }
                if (std::holds_alternative<PointerType>(rhsValue.getType().get()))
                {
                    auto rhsElementType = cld::get<PointerType>(rhsValue.getType().get()).getElementType();
                    if (!isCompleteType(rhsElementType))
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            rhsValue, m_sourceInterface, rhsElementType, rhsValue, rhsValue.getType()));
                    }
                    else if (std::holds_alternative<FunctionType>(rhsElementType.get()))
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            rhsValue, m_sourceInterface, rhsValue));
                    }

                    valueElementType = removeQualifiers(std::move(valueElementType));
                    rhsElementType = removeQualifiers(std::move(rhsElementType));
                    if (!typesAreCompatible(valueElementType, rhsElementType))
                    {
                        log(Errors::Semantics::CANNOT_SUBTRACT_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                            value, m_sourceInterface, value, *token, rhsValue));
                    }
                    value = Expression(getPtrdiffT(m_sourceInterface.getLanguageOptions()), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Subtraction, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                }
                else
                {
                    if (!isInteger(rhsValue.getType()))
                    {
                        log(Errors::Semantics::EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                    auto type = value.getType();
                    value = Expression(std::move(type), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Subtraction, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                }
                continue;
            }
            case Syntax::AdditiveExpression::BinaryPlus:
            {
                if (std::holds_alternative<PrimitiveType>(value.getType().get())
                    && std::holds_alternative<PrimitiveType>(rhsValue.getType().get()))
                {
                    auto type = value.getType();
                    value = Expression(std::move(type), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Addition, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                    continue;
                }
                if (value.isUndefined() || rhsValue.isUndefined() || errors)
                {
                    value = {};
                    continue;
                }
                auto& pointerExpr = std::holds_alternative<PointerType>(value.getType().get()) ? value : rhsValue;
                auto& intExpr = &pointerExpr == &value ? rhsValue : value;
                if (!isInteger(intExpr.getType()))
                {
                    log(Errors::Semantics::EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE.args(
                        intExpr, m_sourceInterface, *token, intExpr));
                }
                auto& elementType = cld::get<PointerType>(pointerExpr.getType().get()).getElementType();
                if (!isCompleteType(elementType))
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                        pointerExpr, m_sourceInterface, elementType, pointerExpr, pointerExpr.getType()));
                }
                else if (std::holds_alternative<FunctionType>(elementType.get()))
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        pointerExpr, m_sourceInterface, pointerExpr));
                }
                auto type = pointerExpr.getType();
                value =
                    Expression(std::move(type), ValueCategory::Rvalue,
                               BinaryOperator(std::make_unique<Expression>(std::move(value)), BinaryOperator::Addition,
                                              token, std::make_unique<Expression>(std::move(rhsValue))));
                continue;
            }
        }
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
    value = integerPromotion(std::move(value));
    for (auto& [kind, token, rhs] : node.getOptionalAdditiveExpressions())
    {
        auto rhsValue = integerPromotion(visit(rhs));
        bool errors = false;
        if (!value.isUndefined() && !isInteger(value.getType()))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(value, m_sourceInterface,
                                                                                           *token, value));
            errors = true;
        }
        if (!rhsValue.isUndefined() && !isInteger(rhsValue.getType()))
        {
            log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(rhsValue, m_sourceInterface,
                                                                                            *token, rhsValue));
            errors = true;
        }
        if (value.isUndefined() || rhsValue.isUndefined() || errors)
        {
            value = {};
            continue;
        }
        auto type = value.getType();
        value = Expression(std::move(type), ValueCategory::Rvalue,
                           BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                          kind == Syntax::ShiftExpression::Left ? BinaryOperator::LeftShift :
                                                                                  BinaryOperator::RightShift,
                                          token, std::make_unique<Expression>(std::move(rhsValue))));
        continue;
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
    for (auto& [kind, token, rhs] : node.getOptionalShiftExpressions())
    {
        auto rhsValue = visit(rhs);
        if (isArithmetic(value.getType()) && isArithmetic(rhsValue.getType()))
        {
            arithmeticConversion(value, rhsValue);
        }
        else
        {
            value = lvalueConversion(std::move(value));
            rhsValue = lvalueConversion(std::move(rhsValue));
        }
        if (!value.isUndefined() && !isScalar(value.getType()))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                value, m_sourceInterface, *token, value));
        }
        else if (isArithmetic(value.getType()))
        {
            if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
            {
                log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
        }
        else if (std::holds_alternative<PointerType>(value.getType().get()))
        {
            if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().get()))
            {
                log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_A_POINTER_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
            else if (!rhsValue.isUndefined())
            {
                auto valueElementType = cld::get<PointerType>(value.getType().get()).getElementType();
                auto rhsElementType = cld::get<PointerType>(rhsValue.getType().get()).getElementType();
                valueElementType = removeQualifiers(std::move(valueElementType));
                if (std::holds_alternative<FunctionType>(valueElementType.get()))
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        value, m_sourceInterface, value));
                    rhsElementType = removeQualifiers(std::move(rhsElementType));
                    if (std::holds_alternative<FunctionType>(rhsElementType.get()))
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            rhsValue, m_sourceInterface, rhsValue));
                    }
                }
                else
                {
                    rhsElementType = removeQualifiers(std::move(rhsElementType));
                    if (std::holds_alternative<FunctionType>(rhsElementType.get()))
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            rhsValue, m_sourceInterface, rhsValue));
                    }
                    else if (!typesAreCompatible(valueElementType, rhsElementType))
                    {
                        log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                            value, m_sourceInterface, value, *token, rhsValue));
                    }
                }
            }
        }
        value = Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                           ValueCategory::Rvalue,
                           BinaryOperator(
                               std::make_unique<Expression>(std::move(value)),
                               [kind = kind] {
                                   switch (kind)
                                   {
                                       case Syntax::RelationalExpression::GreaterThan:
                                           return BinaryOperator::GreaterThan;
                                       case Syntax::RelationalExpression::GreaterThanOrEqual:
                                           return BinaryOperator::GreaterOrEqual;
                                       case Syntax::RelationalExpression::LessThan: return BinaryOperator::LessThan;
                                       case Syntax::RelationalExpression::LessThanOrEqual:
                                           return BinaryOperator::LessOrEqual;
                                   }
                                   CLD_UNREACHABLE;
                               }(),
                               token, std::make_unique<Expression>(std::move(rhsValue))));
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
    for (auto& [kind, token, rhs] : node.getOptionalRelationalExpressions())
    {
        auto rhsValue = visit(rhs);
        if (isArithmetic(value.getType()) && isArithmetic(rhsValue.getType()))
        {
            arithmeticConversion(value, rhsValue);
        }
        else
        {
            value = lvalueConversion(std::move(value));
            rhsValue = lvalueConversion(std::move(rhsValue));
        }
        if (!value.isUndefined() && !isScalar(value.getType()))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                value, m_sourceInterface, *token, value));
        }
        else if (isArithmetic(value.getType()))
        {
            if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
            {
                log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
        }
        else if (std::holds_alternative<PointerType>(value.getType().get()))
        {
            // TODO: Null pointer constant
            if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().get()))
            {
                log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_A_POINTER_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
            else if (!rhsValue.isUndefined())
            {
                auto valueElementType = cld::get<PointerType>(value.getType().get()).getElementType();
                auto rhsElementType = cld::get<PointerType>(rhsValue.getType().get()).getElementType();
                valueElementType = removeQualifiers(std::move(valueElementType));
                rhsElementType = removeQualifiers(std::move(rhsElementType));
                if (!isVoid(valueElementType) && !isVoid(rhsElementType)
                    && !typesAreCompatible(valueElementType, rhsElementType))
                {
                    log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(value, m_sourceInterface,
                                                                                              value, *token, rhsValue));
                }
            }
        }
        value = Expression(
            PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()), ValueCategory::Rvalue,
            BinaryOperator(std::make_unique<Expression>(std::move(value)),
                           kind == Syntax::EqualityExpression::Equal ? BinaryOperator::Equal : BinaryOperator::NotEqual,
                           token, std::make_unique<Expression>(std::move(rhsValue))));
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::doBitOperators(Expression&& lhs, BinaryOperator::Kind kind,
                                                                            Lexer::CTokenIterator token,
                                                                            Expression&& rhs)
{
    if (isArithmetic(lhs.getType()) && isArithmetic(rhs.getType()))
    {
        arithmeticConversion(lhs, rhs);
    }
    if (!lhs.isUndefined() && !isInteger(lhs.getType()))
    {
        log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(lhs, m_sourceInterface, *token,
                                                                                       lhs));
        lhs = {};
    }
    if (!rhs.isUndefined() && !isInteger(rhs.getType()))
    {
        log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(rhs, m_sourceInterface, *token,
                                                                                        rhs));
    }
    auto type = lhs.getType();
    return Expression(std::move(type), ValueCategory::Rvalue,
                      BinaryOperator(std::make_unique<Expression>(std::move(lhs)), kind, token,
                                     std::make_unique<Expression>(std::move(rhs))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitAndExpression& node)
{
    auto value = visit(node.getEqualityExpression());
    if (node.getOptionalEqualityExpressions().empty())
    {
        return value;
    }
    for (auto& [token, rhs] : node.getOptionalEqualityExpressions())
    {
        value = doBitOperators(std::move(value), BinaryOperator::BitAnd, token, visit(rhs));
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitXorExpression& node)
{
    auto value = visit(node.getBitAndExpression());
    if (node.getOptionalBitAndExpressions().empty())
    {
        return value;
    }
    for (auto& [token, rhs] : node.getOptionalBitAndExpressions())
    {
        value = doBitOperators(std::move(value), BinaryOperator::BitXor, token, visit(rhs));
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::BitOrExpression& node)
{
    auto value = visit(node.getBitXorExpression());
    if (node.getOptionalBitXorExpressions().empty())
    {
        return value;
    }
    for (auto& [token, rhs] : node.getOptionalBitXorExpressions())
    {
        value = doBitOperators(std::move(value), BinaryOperator::BitOr, token, visit(rhs));
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::doLogicOperators(Expression&& lhs,
                                                                              BinaryOperator::Kind kind,
                                                                              Lexer::CTokenIterator token,
                                                                              Expression&& rhs)
{
    lhs = lvalueConversion(std::move(lhs));
    rhs = lvalueConversion(std::move(rhs));
    if (!lhs.isUndefined() && !isScalar(lhs.getType()))
    {
        log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            lhs, m_sourceInterface, *token, lhs));
    }
    if (!rhs.isUndefined() && !isScalar(rhs.getType()))
    {
        log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            rhs, m_sourceInterface, *token, rhs));
    }
    return Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                      ValueCategory::Rvalue,
                      BinaryOperator(std::make_unique<Expression>(std::move(lhs)), kind, token,
                                     std::make_unique<Expression>(std::move(rhs))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalAndExpression& node)
{
    auto value = visit(node.getBitOrExpression());
    if (node.getOptionalBitOrExpressions().empty())
    {
        return value;
    }
    for (auto& [token, rhs] : node.getOptionalBitOrExpressions())
    {
        value = doLogicOperators(std::move(value), BinaryOperator::LogicAnd, token, visit(rhs));
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalOrExpression& node)
{
    auto value = visit(node.getAndExpression());
    if (node.getOptionalAndExpressions().empty())
    {
        return value;
    }
    for (auto& [token, rhs] : node.getOptionalAndExpressions())
    {
        value = doLogicOperators(std::move(value), BinaryOperator::LogicOr, token, visit(rhs));
    }
    return value;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::ConditionalExpression& node)
{
    auto condition = visit(node.getLogicalOrExpression());
    if (!node.getOptionalConditionalExpression() && !node.getOptionalExpression() && !node.getOptionalQuestionMark()
        && !node.getOptionalColon())
    {
        return condition;
    }
    if (!condition.isUndefined() && !isScalar(condition.getType()))
    {
        log(Errors::Semantics::FIRST_OPERAND_OF_CONDITIONAL_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            condition, m_sourceInterface, condition, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
    }
    auto second = visit(*node.getOptionalExpression());
    auto third = visit(*node.getOptionalConditionalExpression());
    if (isArithmetic(second.getType()) && isArithmetic(third.getType()))
    {
        arithmeticConversion(second, third);
    }
    else
    {
        second = lvalueConversion(std::move(second));
        third = lvalueConversion(std::move(third));
    }
    Type resultType;
    // TODO: Null pointer constant
    if (isArithmetic(second.getType()))
    {
        if (!third.isUndefined() && !isArithmetic(third.getType()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE.args(
                third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        resultType = second.getType();
    }
    else if (isVoid(second.getType()))
    {
        if (!third.isUndefined() && !isVoid(third.getType()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_VOID.args(
                third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        resultType = second.getType();
    }
    else if (std::holds_alternative<PointerType>(second.getType().get()))
    {
        if (!third.isUndefined() && !std::holds_alternative<PointerType>(third.getType().get()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_A_POINTER_TYPE.args(
                third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        else if (!third.isUndefined())
        {
            auto& secondElementType = cld::get<PointerType>(second.getType().get()).getElementType();
            auto& thirdElementType = cld::get<PointerType>(third.getType().get()).getElementType();
            auto secondWithoutQualifier = removeQualifiers(secondElementType);
            auto thirdWithoutQualifier = removeQualifiers(thirdElementType);
            if (!isVoid(secondWithoutQualifier) && !isVoid(thirdWithoutQualifier)
                && !typesAreCompatible(secondWithoutQualifier, thirdWithoutQualifier))
            {
                log(Errors::Semantics::POINTER_TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES.args(
                    *node.getOptionalQuestionMark(), m_sourceInterface, *node.getOptionalQuestionMark(), second,
                    *node.getOptionalColon(), third));
            }
            else if (isVoid(secondWithoutQualifier) || isVoid(thirdWithoutQualifier))
            {
                resultType = PointerType::create(
                    false, false, false,
                    PrimitiveType::createVoid(secondElementType.isConst() || thirdElementType.isConst(),
                                              secondElementType.isVolatile() || thirdElementType.isVolatile()));
            }
            else
            {
                auto composite = compositeType(secondWithoutQualifier, thirdWithoutQualifier);
                resultType = PointerType::create(false, false, false,
                                                 Type(secondElementType.isConst() || thirdElementType.isConst(),
                                                      secondElementType.isVolatile() || thirdElementType.isVolatile(),
                                                      std::move(composite).get()));
            }
        }
    }
    else if (isRecord(second.getType()) && !third.isUndefined())
    {
        if (!typesAreCompatible(second.getType(), third.getType()))
        {
            log(Errors::Semantics::TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES.args(
                *node.getOptionalQuestionMark(), m_sourceInterface, *node.getOptionalQuestionMark(), second,
                *node.getOptionalColon(), third));
        }
        else
        {
            resultType = second.getType();
        }
    }
    return Expression(std::move(resultType), ValueCategory::Rvalue,
                      Conditional(std::make_unique<Expression>(std::move(condition)), node.getOptionalQuestionMark(),
                                  std::make_unique<Expression>(std::move(second)), node.getOptionalColon(),
                                  std::make_unique<Expression>(std::move(third))));
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
            Conversion(elementType, Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
    }
    if (std::holds_alternative<FunctionType>(expression.getType().get()))
    {
        return Expression(
            PointerType::create(false, false, false, expression.getType()), ValueCategory::Rvalue,
            Conversion(expression.getType(), Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
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
                      Conversion(newType, Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
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
        Conversion(newType, Conversion::IntegerPromotion, std::make_unique<Expression>(std::move(expression))));
}

void cld::Semantics::SemanticAnalysis::arithmeticConversion(cld::Semantics::Expression& lhs,
                                                            cld::Semantics::Expression& rhs) const
{
    lhs = integerPromotion(std::move(lhs));
    rhs = integerPromotion(std::move(rhs));
    if (!isArithmetic(lhs.getType()) || !isArithmetic(rhs.getType()))
    {
        return;
    }
    if (lhs.getType() == rhs.getType())
    {
        return;
    }
    auto& lhsPrim = cld::get<PrimitiveType>(lhs.getType().get());
    auto& rhsPrim = cld::get<PrimitiveType>(rhs.getType().get());
    Type type;
    if (lhsPrim.isFloatingPoint() || rhsPrim.isFloatingPoint())
    {
        auto [floating, biggest] = std::max(std::pair(lhsPrim.isFloatingPoint(), lhsPrim.getKind()),
                                            std::pair(rhsPrim.isFloatingPoint(), rhsPrim.getKind()));
        (void)floating;
        switch (biggest)
        {
            case PrimitiveType::Kind::Float: type = PrimitiveType::createFloat(false, false); break;
            case PrimitiveType::Kind::Double: type = PrimitiveType::createDouble(false, false); break;
            case PrimitiveType::Kind::LongDouble:
                type = PrimitiveType::createLongDouble(false, false, m_sourceInterface.getLanguageOptions());
                break;
            default: CLD_UNREACHABLE;
        }
    }
    else if (rhsPrim.isSigned() == lhsPrim.isSigned() || rhsPrim.getBitCount() != lhsPrim.getBitCount())
    {
        auto [bits, sign, lhsType] = std::max(std::tuple(lhsPrim.getBitCount(), lhsPrim.isSigned(), true),
                                              std::tuple(rhsPrim.getBitCount(), rhsPrim.isSigned(), false));
        type = lhsType ? lhs.getType() : rhs.getType();
    }
    else
    {
        type = !lhsPrim.isSigned() ? lhs.getType() : rhs.getType();
    }
    lhs = Expression(type, ValueCategory::Rvalue,
                     Conversion(type, Conversion::ArithmeticConversion, std::make_unique<Expression>(std::move(lhs))));
    rhs = Expression(type, ValueCategory::Rvalue,
                     Conversion(type, Conversion::ArithmeticConversion, std::make_unique<Expression>(std::move(rhs))));
}

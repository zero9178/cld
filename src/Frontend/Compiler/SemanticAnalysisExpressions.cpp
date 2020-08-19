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
    if (node.getOptionalAssignmentExpressions().empty())
    {
        return visit(node.getAssignmentExpression());
    }
    std::vector<std::pair<Expression, Lexer::CTokenIterator>> expressions;
    expressions.emplace_back(visit(node.getAssignmentExpression()),
                             node.getOptionalAssignmentExpressions().front().first);
    auto ref = llvm::ArrayRef(node.getOptionalAssignmentExpressions()).drop_back();
    for (const auto* iter = ref.begin(); iter != ref.end(); iter++)
    {
        auto& [token, exp] = *iter;
        expressions.emplace_back(visit(exp), (iter + 1)->first);
    }
    auto last = lvalueConversion(visit(node.getOptionalAssignmentExpressions().back().second));
    auto type = last.getType();
    return Expression(std::move(type), ValueCategory::Rvalue,
                      CommaExpression(std::move(expressions), std::make_unique<Expression>(std::move(last))));
}

bool cld::Semantics::SemanticAnalysis::isModifiableLValue(const cld::Semantics::Expression& expression) const
{
    if (expression.getValueCategory() != ValueCategory::Lvalue)
    {
        return false;
    }
    return !isConst(expression.getType());
}

bool cld::Semantics::SemanticAnalysis::isConst(const Type& type) const
{
    if (type.isConst())
    {
        return true;
    }
    if (!isRecord(type) || !isCompleteType(type))
    {
        return false;
    }
    auto fields = getFields(type);
    for (auto& [type, name, size] : fields)
    {
        (void)name;
        (void)size;
        if (isConst(*type))
        {
            return true;
        }
    }
    return false;
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::AssignmentExpression& node)
{
    if (node.getOptionalConditionalExpressions().empty())
    {
        return visit(node.getConditionalExpression());
    }
    auto ref = llvm::ArrayRef(node.getOptionalConditionalExpressions());
    auto rhsValue = lvalueConversion(visit(ref.back().conditionalExpression));
    for (auto iter = ref.rbegin(); iter != ref.rend(); iter++)
    {
        auto& [kind, token, rhs] = *iter;
        auto lhsValue =
            iter + 1 != ref.rend() ? visit((iter + 1)->conditionalExpression) : visit(node.getConditionalExpression());
        if (!lhsValue.isUndefined() && !isModifiableLValue(lhsValue))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST.args(
                lhsValue, m_sourceInterface, *token, lhsValue));
        }
        switch (kind)
        {
            case Syntax::AssignmentExpression::NoOperator:
            {
                if (isArithmetic(lhsValue.getType()))
                {
                    if (!isBool(lhsValue.getType()))
                    {
                        if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
                        {
                            log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                                rhsValue, m_sourceInterface, *token, rhsValue));
                        }
                    }
                    else if (!rhsValue.isUndefined() && !isScalar(rhsValue.getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE
                                .args(rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                }
                else if (isRecord(lhsValue.getType()))
                {
                    if (!isCompleteType(lhsValue.getType()))
                    {
                        log(Errors::Semantics::CANNOT_ASSIGN_TO_INCOMPLETE_TYPE_N.args(lhsValue, m_sourceInterface,
                                                                                       lhsValue, *token));
                    }
                    else if (!rhsValue.isUndefined()
                             && !typesAreCompatible(removeQualifiers(lhsValue.getType()),
                                                    removeQualifiers(rhsValue.getType())))
                    {
                        log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(lhsValue, m_sourceInterface,
                                                                                     lhsValue, *token, rhsValue));
                    }
                }
                else if (std::holds_alternative<PointerType>(lhsValue.getType().get()))
                {
                    if (isInteger(rhsValue.getType()))
                    {
                        auto constant = evaluateConstantExpression(rhsValue);
                        if (!constant)
                        {
                            log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL_2.args(
                                rhsValue, m_sourceInterface, *token, rhsValue));
                        }
                        else if (constant->toUInt() != 0)
                        {
                            log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL.args(
                                rhsValue, m_sourceInterface, *token, rhsValue, *constant));
                        }
                    }
                    else if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().get()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                    else if (!rhsValue.isUndefined())
                    {
                        auto& lhsElementType = cld::get<PointerType>(lhsValue.getType().get()).getElementType();
                        auto& rhsElementType = cld::get<PointerType>(rhsValue.getType().get()).getElementType();
                        if (isVoid(removeQualifiers(lhsElementType)) != isVoid(removeQualifiers(rhsElementType)))
                        {
                            auto& voidExpr = isVoid(removeQualifiers(lhsElementType)) ? lhsValue : rhsValue;
                            auto& otherExpr = &voidExpr == &lhsValue ? rhsValue : lhsValue;
                            auto& elementType = cld::get<PointerType>(otherExpr.getType().get()).getElementType();
                            if (std::holds_alternative<FunctionType>(elementType.get()))
                            {
                                if (&voidExpr == &lhsValue)
                                {
                                    log(Errors::Semantics::CANNOT_ASSIGN_FUNCTION_POINTER_TO_VOID_POINTER.args(
                                        lhsValue, m_sourceInterface, lhsValue, *token, rhsValue));
                                }
                                else
                                {
                                    log(Errors::Semantics::CANNOT_ASSIGN_VOID_POINTER_TO_FUNCTION_POINTER.args(
                                        lhsValue, m_sourceInterface, lhsValue, *token, rhsValue));
                                }
                            }
                            if ((!lhsElementType.isConst() && rhsElementType.isConst())
                                || (!lhsElementType.isVolatile() && rhsElementType.isVolatile()))
                            {
                                log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(
                                    lhsValue, m_sourceInterface, lhsValue, *token, rhsValue));
                            }
                        }
                        else if (!typesAreCompatible(removeQualifiers(lhsElementType),
                                                     removeQualifiers(rhsElementType)))
                        {
                            log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(lhsValue, m_sourceInterface,
                                                                                         lhsValue, *token, rhsValue));
                        }
                        else if ((!lhsElementType.isConst() && rhsElementType.isConst())
                                 || (!lhsElementType.isVolatile() && rhsElementType.isVolatile()))
                        {
                            log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(lhsValue, m_sourceInterface,
                                                                                         lhsValue, *token, rhsValue));
                        }
                    }
                }
                break;
            }
            case Syntax::AssignmentExpression::PlusAssign:
            case Syntax::AssignmentExpression::MinusAssign:
            {
                if (!lhsValue.isUndefined() && !isScalar(lhsValue.getType()))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                        lhsValue, m_sourceInterface, *token, lhsValue));
                }
                if (std::holds_alternative<PointerType>(lhsValue.getType().get()))
                {
                    auto& elementType = cld::get<PointerType>(lhsValue.getType().get()).getElementType();
                    if (std::holds_alternative<FunctionType>(elementType.get()))
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            lhsValue, m_sourceInterface, lhsValue));
                    }
                    else if (!isCompleteType(elementType))
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            lhsValue, m_sourceInterface, elementType, lhsValue, lhsValue.getType()));
                    }
                    if (!rhsValue.isUndefined() && !isInteger(rhsValue.getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                }
                else if (isArithmetic(lhsValue.getType()))
                {
                    if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                }
                break;
            }
                // Doing a simple check is redundant in compound assignments. Either we had a diagnostic saying that at
                // least one operand does not fit the constraints and we don't want further noise or
                // they're both arithmetic types and we already know they can only be compatible
            case Syntax::AssignmentExpression::DivideAssign:
            case Syntax::AssignmentExpression::MultiplyAssign:
            {
                auto lhsType = lhsValue.getType();
                if (isArithmetic(lhsType) && isArithmetic(rhsValue.getType()))
                {
                    arithmeticConversion(lhsType, rhsValue);
                }
                if (!lhsType.isUndefined() && !isArithmetic(lhsType))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        lhsValue, m_sourceInterface, *token, lhsValue));
                }
                if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                }
                break;
            }
            case Syntax::AssignmentExpression::ModuloAssign:
            case Syntax::AssignmentExpression::LeftShiftAssign:
            case Syntax::AssignmentExpression::RightShiftAssign:
            case Syntax::AssignmentExpression::BitAndAssign:
            case Syntax::AssignmentExpression::BitOrAssign:
            case Syntax::AssignmentExpression::BitXorAssign:
            {
                // Originally this would be an arithmetic conversion for the bit operators and modulo. The main point of
                // the arithmetic conversion is to figure out the right result type of the expression. This is redundant
                // here as the resulting type of the assignment is always the left operand and only integer types are
                // allowed. If floating point types were allowed we'd have to do the conversion for that
                auto lhsType = integerPromotion(lhsValue.getType());
                if (!lhsType.isUndefined() && !isInteger(lhsType))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        lhsValue, m_sourceInterface, *token, lhsValue));
                }
                rhsValue = integerPromotion(std::move(rhsValue));
                if (!rhsValue.isUndefined() && !isInteger(rhsValue.getType()))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                }
                break;
            }
        }
        auto type = removeQualifiers(lhsValue.getType());
        rhsValue = Expression(std::move(type), ValueCategory::Rvalue,
                              Assignment(
                                  std::make_unique<Expression>(std::move(lhsValue)),
                                  [kind = kind] {
                                      switch (kind)
                                      {
                                          case Syntax::AssignmentExpression::NoOperator: return Assignment::Simple;
                                          case Syntax::AssignmentExpression::PlusAssign: return Assignment::Plus;
                                          case Syntax::AssignmentExpression::MinusAssign: return Assignment::Minus;
                                          case Syntax::AssignmentExpression::DivideAssign: return Assignment::Divide;
                                          case Syntax::AssignmentExpression::MultiplyAssign:
                                              return Assignment::Multiply;
                                          case Syntax::AssignmentExpression::ModuloAssign: return Assignment::Modulo;
                                          case Syntax::AssignmentExpression::LeftShiftAssign:
                                              return Assignment::LeftShift;
                                          case Syntax::AssignmentExpression::RightShiftAssign:
                                              return Assignment::RightShift;
                                          case Syntax::AssignmentExpression::BitAndAssign: return Assignment::BitAnd;
                                          case Syntax::AssignmentExpression::BitOrAssign: return Assignment::BitOr;
                                          case Syntax::AssignmentExpression::BitXorAssign: return Assignment::BitXor;
                                      }
                                      CLD_UNREACHABLE;
                                  }(),
                                  token, std::make_unique<Expression>(std::move(rhsValue))));
    }
    return rhsValue;
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
        return Expression(node);
    }
    if (std::holds_alternative<std::pair<ConstValue, Type>>(*result))
    {
        auto& value = cld::get<std::pair<ConstValue, Type>>(*result);
        if (value.second.isUndefined())
        {
            return Expression(node);
        }
        if (value.first.isUndefined())
        {
            return Expression(value.second, ValueCategory::Rvalue, {});
        }
        return Expression(value.second, ValueCategory::Rvalue,
                          Constant(cld::match(value.first.getValue(),
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
    auto type = std::holds_alternative<const Declaration*>(*result) ?
                    cld::get<const Declaration*>(*result)->getType() :
                    cld::get<const FunctionDefinition*>(*result)->getType();
    return Expression(std::move(type), ValueCategory::Lvalue,
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
        return Expression(node);
    }
    if (!std::holds_alternative<PointerType>(first.getType().get())
        && !std::holds_alternative<PointerType>(second.getType().get()))
    {
        log(Errors::Semantics::EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE.args(node.getPostFixExpression(),
                                                                               m_sourceInterface, first, second));
        return Expression(node);
    }
    auto& pointerExpr = std::holds_alternative<PointerType>(first.getType().get()) ? first : second;
    auto& intExpr = &pointerExpr == &first ? second : first;
    if (!isInteger(intExpr.getType()))
    {
        log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(intExpr, m_sourceInterface, intExpr));
        return Expression(node);
    }
    auto elementType = cld::get<PointerType>(pointerExpr.getType().get()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            pointerExpr, m_sourceInterface, elementType, pointerExpr));
        return Expression(node);
    }
    else if (std::holds_alternative<FunctionType>(elementType.get()))
    {
        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            pointerExpr, m_sourceInterface, pointerExpr));
        return Expression(node);
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
        return Expression(node);
    }
    if (!isRecord(structOrUnion.getType()))
    {
        log(Errors::Semantics::EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_DOT_OPERATOR.args(
            structOrUnion, m_sourceInterface, structOrUnion));
        return Expression(node);
    }
    auto result = checkMemberAccess(structOrUnion.getType(), node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return Expression(node);
    }
    auto& [type, index] = *result;
    if (type.isUndefined())
    {
        return Expression(node);
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
        return Expression(node);
    }
    if (!std::holds_alternative<PointerType>(structOrUnionPtr.getType().get()))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR.args(
            structOrUnionPtr, m_sourceInterface, structOrUnionPtr));
        return Expression(node);
    }
    auto& structOrUnion = cld::get<PointerType>(structOrUnionPtr.getType().get()).getElementType();
    if (!isRecord(structOrUnion))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR.args(
            structOrUnionPtr, m_sourceInterface, structOrUnionPtr));
        return Expression(node);
    }
    auto result = checkMemberAccess(structOrUnion, node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return Expression(node);
    }
    auto& [type, index] = *result;
    return Expression(
        std::move(type), ValueCategory::Lvalue,
        MemberAccess(std::make_unique<Expression>(std::move(structOrUnionPtr)), index, node.getIdentifier()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionFunctionCall& node)
{
    auto function = lvalueConversion(visit(node.getPostFixExpression()));
    if (!std::holds_alternative<PointerType>(function.getType().get())
        || !std::holds_alternative<FunctionType>(
            cld::get<PointerType>(function.getType().get()).getElementType().get()))
    {
        if (!function.isUndefined())
        {
            log(Errors::Semantics::CANNOT_CALL_NON_FUNCTION_TYPE.args(
                function, m_sourceInterface, function, *node.getOpenParentheses(), *node.getCloseParentheses()));
        }
        for (auto& iter : node.getOptionalAssignmentExpressions())
        {
            CLD_ASSERT(iter);
            visit(*iter);
        }
        return Expression(node);
    }
    auto& ft = cld::get<FunctionType>(cld::get<PointerType>(function.getType().get()).getElementType().get());
    std::vector<Expression> arguments;
    if (ft.isKandR())
    {
        for (auto& iter : node.getOptionalAssignmentExpressions())
        {
            CLD_ASSERT(iter);
            arguments.push_back(defaultArgumentPromotion(visit(*iter)));
        }
    }
    else
    {
        auto callsFunction = [&] {
            if (!std::holds_alternative<Conversion>(function.get()))
            {
                return false;
            }
            auto& conversion = cld::get<Conversion>(function.get());
            if (conversion.getKind() != Conversion::LValue)
            {
                return false;
            }
            if (!std::holds_alternative<DeclarationRead>(conversion.getExpression().get()))
            {
                return false;
            }
            auto& decl = cld::get<DeclarationRead>(conversion.getExpression().get());
            return cld::match(
                decl.getDeclRead(), [](const FunctionDefinition*) { return true; },
                [](const Declaration* declaration) {
                    return std::holds_alternative<FunctionType>(declaration->getType().get());
                });
        }();
        auto& argumentTypes = ft.getArguments();
        if (node.getOptionalAssignmentExpressions().size() < argumentTypes.size())
        {
            if (!ft.isLastVararg())
            {
                if (callsFunction)
                {
                    auto& decl = cld::get<DeclarationRead>(cld::get<Conversion>(function.get()).getExpression().get());
                    log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
                        decl, m_sourceInterface, *decl.getIdentifierToken(), argumentTypes.size(),
                        node.getOptionalAssignmentExpressions().size()));
                }
                else
                {
                    log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N.args(
                        function, m_sourceInterface, argumentTypes.size(),
                        node.getOptionalAssignmentExpressions().size(), function));
                }
            }
            else
            {
                if (callsFunction)
                {
                    auto& decl = cld::get<DeclarationRead>(cld::get<Conversion>(function.get()).getExpression().get());
                    log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_AT_LEAST_N_GOT_N.args(
                        decl, m_sourceInterface, *decl.getIdentifierToken(), argumentTypes.size(),
                        node.getOptionalAssignmentExpressions().size()));
                }
                else
                {
                    log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_AT_LEAST_N_GOT_N.args(
                        function, m_sourceInterface, argumentTypes.size(),
                        node.getOptionalAssignmentExpressions().size(), function));
                }
            }
        }
        else if (!ft.isLastVararg() && node.getOptionalAssignmentExpressions().size() > argumentTypes.size())
        {
            if (callsFunction)
            {
                auto& decl = cld::get<DeclarationRead>(cld::get<Conversion>(function.get()).getExpression().get());
                log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
                    decl, m_sourceInterface, *decl.getIdentifierToken(), argumentTypes.size(),
                    node.getOptionalAssignmentExpressions().size(),
                    llvm::ArrayRef(node.getOptionalAssignmentExpressions()).drop_front(argumentTypes.size())));
            }
            else
            {
                log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N.args(
                    function, m_sourceInterface, argumentTypes.size(), node.getOptionalAssignmentExpressions().size(),
                    function,
                    llvm::ArrayRef(node.getOptionalAssignmentExpressions()).drop_front(argumentTypes.size())));
            }
        }
        else
        {
            std::size_t i = 0;
            for (; i < argumentTypes.size(); i++)
            {
                auto paramType = removeQualifiers(adjustParameterType(argumentTypes[i].first));
                if (paramType.isUndefined())
                {
                    visit(*node.getOptionalAssignmentExpressions()[i]);
                    arguments.emplace_back(*node.getOptionalAssignmentExpressions()[i]);
                    continue;
                }
                auto expression = lvalueConversion(visit(*node.getOptionalAssignmentExpressions()[i]));
                if (expression.isUndefined())
                {
                    arguments.push_back(std::move(expression));
                    continue;
                }
                if (isArithmetic(paramType))
                {
                    if (!isBool(paramType))
                    {
                        if (!isArithmetic(expression.getType()))
                        {
                            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                                expression, m_sourceInterface, i + 1, expression));
                        }
                    }
                    else
                    {
                        if (!isScalar(expression.getType()))
                        {
                            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                                expression, m_sourceInterface, i + 1, expression));
                        }
                    }
                }
                else if (isRecord(paramType))
                {
                    if (!typesAreCompatible(paramType, expression.getType()))
                    {
                        log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_ARGUMENT_N_OF_TYPE_N.args(
                            expression, m_sourceInterface, i + 1, paramType, expression));
                    }
                }
                else if (std::holds_alternative<PointerType>(paramType.get()))
                {
                    if (isInteger(expression.getType()))
                    {
                        auto constant = evaluateConstantExpression(expression);
                        if (!constant)
                        {
                            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_NULL_2.args(expression, m_sourceInterface,
                                                                                         i + 1, expression));
                        }
                        else
                        {
                            if (constant->toUInt() != 0)
                            {
                                log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_NULL.args(
                                    expression, m_sourceInterface, i + 1, expression, *constant));
                            }
                        }
                    }
                    else if (!std::holds_alternative<PointerType>(expression.getType().get()))
                    {
                        log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_A_POINTER_TYPE.args(
                            expression, m_sourceInterface, i + 1, expression));
                    }
                    else
                    {
                        auto& paramElementType = cld::get<PointerType>(paramType.get()).getElementType();
                        auto& expElementType = cld::get<PointerType>(expression.getType().get()).getElementType();
                        if (isVoid(removeQualifiers(paramElementType)) != isVoid(removeQualifiers(expElementType)))
                        {
                            bool paramIsVoid = isVoid(removeQualifiers(paramElementType));
                            auto& nonVoidType = paramIsVoid ? expElementType : paramElementType;
                            if (std::holds_alternative<FunctionType>(nonVoidType.get()))
                            {
                                if (paramIsVoid)
                                {
                                    log(Errors::Semantics::CANNOT_PASS_FUNCTION_POINTER_TO_VOID_POINTER_ARGUMENT.args(
                                        expression, m_sourceInterface, expression));
                                }
                                else
                                {
                                    log(Errors::Semantics::CANNOT_PASS_VOID_POINTER_TO_FUNCTION_POINTER_ARGUMENT.args(
                                        expression, m_sourceInterface, expression));
                                }
                            }
                            if ((!paramElementType.isConst() && expElementType.isConst())
                                || (!paramElementType.isVolatile() && expElementType.isVolatile()))
                            {
                                log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_ARGUMENT_N_OF_TYPE_N.args(
                                    expression, m_sourceInterface, i + 1, paramType, expression));
                            }
                        }
                        else if (!typesAreCompatible(removeQualifiers(paramElementType),
                                                     removeQualifiers(expElementType)))
                        {
                            log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_ARGUMENT_N_OF_TYPE_N.args(
                                expression, m_sourceInterface, i + 1, paramType, expression));
                        }
                        else if ((!paramElementType.isConst() && expElementType.isConst())
                                 || (!paramElementType.isVolatile() && expElementType.isVolatile()))
                        {
                            log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_ARGUMENT_N_OF_TYPE_N.args(
                                expression, m_sourceInterface, i + 1, paramType, expression));
                        }
                    }
                }
                arguments.push_back(std::move(expression));
            }
            for (; i < node.getOptionalAssignmentExpressions().size(); i++)
            {
                arguments.push_back(defaultArgumentPromotion(visit(*node.getOptionalAssignmentExpressions()[i])));
            }
        }
    }

    auto type = ft.getReturnType();
    return Expression(std::move(type), ValueCategory::Rvalue,
                      CallExpression(std::make_unique<Expression>(std::move(function)), node.getOpenParentheses(),
                                     std::move(arguments), node.getCloseParentheses()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::checkIncrementAndDecrement(const Syntax::Node& node,
                                                                                        UnaryOperator::Kind kind,
                                                                                        Expression&& value,
                                                                                        Lexer::CTokenIterator opToken)
{
    if (value.isUndefined())
    {
        return Expression(node);
    }
    if (!isScalar(value.getType()))
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(value, m_sourceInterface,
                                                                                       *opToken, value));
        return Expression(node);
    }
    if (value.getValueCategory() != ValueCategory::Lvalue || value.getType().isConst())
    {
        log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST.args(value, m_sourceInterface,
                                                                                           *opToken, value));
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
        return Expression(node);
    }
    auto type = removeQualifiers(value.getType());
    return Expression(std::move(type), ValueCategory::Rvalue,
                      UnaryOperator(kind, opToken, std::make_unique<Expression>(std::move(value))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionIncrement& node)
{
    return checkIncrementAndDecrement(node, UnaryOperator::PostIncrement, visit(node.getPostFixExpression()),
                                      node.getIncrementToken());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDecrement& node)
{
    return checkIncrementAndDecrement(node, UnaryOperator::PostDecrement, visit(node.getPostFixExpression()),
                                      node.getDecrementToken());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionTypeInitializer& node)
{
    CLD_UNREACHABLE;
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
        return Expression(node);
    }
    switch (node.getOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
            return checkIncrementAndDecrement(node, UnaryOperator::PreIncrement, visit(node.getCastExpression()),
                                              node.getUnaryToken());
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
            return checkIncrementAndDecrement(node, UnaryOperator::PreDecrement, visit(node.getCastExpression()),
                                              node.getUnaryToken());
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        {
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
            if (!std::holds_alternative<CallExpression>(value.get())
                && value.getValueCategory() != ValueCategory::Lvalue)
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
                return Expression(node);
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
                return Expression(node);
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
                return Expression(node);
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
                return Expression(node);
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
                return Expression(node);
            }
            auto& type = exp.getType();
            if (!isCompleteType(type))
            {
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(exp, m_sourceInterface, type, exp));
                return Expression(node);
            }
            else if (std::holds_alternative<FunctionType>(type.get()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(exp, m_sourceInterface, exp, type));
                return Expression(node);
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
                return Expression(node);
            }
            if (!isCompleteType(type))
            {
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(*typeName, m_sourceInterface, type,
                                                                         *typeName));
                return Expression(node);
            }
            else if (std::holds_alternative<FunctionType>(type.get()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(*typeName, m_sourceInterface,
                                                                                 *typeName, type));
                return Expression(node);
            }
            auto size = type.getSizeOf(*this);
            return Expression(getSizeT(m_sourceInterface.getLanguageOptions()), ValueCategory::Rvalue,
                              SizeofOperator(node.getSizeOfToken(), size,
                                             SizeofOperator::TypeVariant{node.getSizeOfToken() + 1, std::move(type),
                                                                         node.end() - 1}));
        });
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionDefined& node)
{
    CLD_ASSERT(m_definedCallback);
    if (m_definedCallback(node.getIdentifier()))
    {
        return Expression(PrimitiveType::createLongLong(false, false), ValueCategory::Rvalue,
                          Constant(llvm::APSInt(llvm::APInt(64, 1), false), node.begin(), node.end()));
    }
    else
    {
        return Expression(PrimitiveType::createLongLong(false, false), ValueCategory::Rvalue,
                          Constant(llvm::APSInt(64, false), node.begin(), node.end()));
    }
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
                return Expression(node);
            }
            auto value = visit(*cast.cast);
            if (value.isUndefined())
            {
                return Expression(node);
            }
            value = lvalueConversion(std::move(value));
            if (!isScalar(type) && !isVoid(removeQualifiers(type)))
            {
                log(Errors::Semantics::TYPE_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    cast.typeName, m_sourceInterface, cast.typeName, type));
                return Expression(node);
            }
            if (!isScalar(value.getType()))
            {
                log(Errors::Semantics::EXPRESSION_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    value, m_sourceInterface, value));
                return Expression(node);
            }
            type = lvalueConversion(std::move(type));
            if (std::holds_alternative<PointerType>(type.get()))
            {
                if (!std::holds_alternative<PointerType>(value.getType().get()) && !isInteger(value.getType()))
                {
                    log(Errors::Semantics::CANNOT_CAST_NON_INTEGER_AND_POINTER_TYPE_N_TO_POINTER_TYPE.args(
                        value, m_sourceInterface, value));
                    return Expression(node);
                }
            }
            else if (std::holds_alternative<PointerType>(value.getType().get()))
            {
                if (!std::holds_alternative<PointerType>(type.get()) && !isInteger(type))
                {
                    log(Errors::Semantics::CANNOT_CAST_POINTER_TYPE_TO_NON_INTEGER_AND_POINTER_TYPE.args(
                        cast.typeName, m_sourceInterface, cast.typeName, value));
                    return Expression(node);
                }
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
                    value = Expression(value.begin(), rhsValue.end());
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
                    value = Expression(value.begin(), rhsValue.end());
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
                    value = Expression(value.begin(), rhsValue.end());
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
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE.args(
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
                    value = Expression(value.begin(), rhsValue.end());
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
            value = Expression(value.begin(), rhsValue.end());
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
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
        }
        else if (std::holds_alternative<PointerType>(value.getType().get()))
        {
            if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().get()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
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
            if (std::holds_alternative<PointerType>(rhsValue.getType().get()) && isInteger(value.getType()))
            {
                auto constant = evaluateConstantExpression(value);
                if (!constant || constant->toUInt() != 0)
                {
                    log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                }
            }
            else if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
        }
        else if (std::holds_alternative<PointerType>(value.getType().get()))
        {
            if (isInteger(rhsValue.getType()))
            {
                auto constant = evaluateConstantExpression(rhsValue);
                if (!constant)
                {
                    log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL_2.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                }
                else
                {
                    if (constant->toUInt() != 0)
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL.args(
                            rhsValue, m_sourceInterface, *token, rhsValue, *constant));
                    }
                }
            }
            else if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().get()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
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
    }
    if (!rhs.isUndefined() && !isInteger(rhs.getType()))
    {
        log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(rhs, m_sourceInterface, *token,
                                                                                        rhs));
    }
    auto type = isInteger(lhs.getType()) ? lhs.getType() : Type{};
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
    if (isArithmetic(second.getType()))
    {
        if (std::holds_alternative<PointerType>(third.getType().get()) && isInteger(second.getType()))
        {
            auto constant = evaluateConstantExpression(second);
            if (!constant || constant->toUInt() != 0)
            {
                log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE.args(
                    third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
            }
        }
        else if (!third.isUndefined() && !isArithmetic(third.getType()))
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
        if (isInteger(third.getType()))
        {
            auto constant = evaluateConstantExpression(third);
            if (!constant)
            {
                log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL_2.args(
                    third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
            }
            else if (constant->toUInt() != 0)
            {
                log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL.args(
                    third, m_sourceInterface, third, *constant, *node.getOptionalQuestionMark(),
                    *node.getOptionalColon()));
            }
            else
            {
                resultType = second.getType();
            }
        }
        else if (!third.isUndefined() && !std::holds_alternative<PointerType>(third.getType().get()))
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
        auto newType = PointerType::create(false, false, false, std::move(elementType));
        return Expression(std::move(newType), ValueCategory::Rvalue,
                          Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
    }
    if (std::holds_alternative<FunctionType>(expression.getType().get()))
    {
        auto newType = PointerType::create(false, false, false, expression.getType());
        return Expression(std::move(newType), ValueCategory::Rvalue,
                          Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
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
    return Expression(std::move(newType), ValueCategory::Rvalue,
                      Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
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

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::defaultArgumentPromotion(Expression expression) const
{
    expression = integerPromotion(std::move(expression));
    if (!std::holds_alternative<PrimitiveType>(expression.getType().get()))
    {
        return expression;
    }
    auto& prim = cld::get<PrimitiveType>(expression.getType().get());
    if (prim.getKind() != PrimitiveType::Kind::Float)
    {
        return expression;
    }
    return Expression(
        PrimitiveType::createDouble(false, false), ValueCategory::Rvalue,
        Conversion(Conversion::DefaultArgumentPromotion, std::make_unique<Expression>(std::move(expression))));
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
    return Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                      ValueCategory::Rvalue,
                      Conversion(Conversion::IntegerPromotion, std::make_unique<Expression>(std::move(expression))));
}

void cld::Semantics::SemanticAnalysis::arithmeticConversion(Expression& lhs, Expression& rhs) const
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
                     Conversion(Conversion::ArithmeticConversion, std::make_unique<Expression>(std::move(lhs))));
    rhs = Expression(type, ValueCategory::Rvalue,
                     Conversion(Conversion::ArithmeticConversion, std::make_unique<Expression>(std::move(rhs))));
}

void cld::Semantics::SemanticAnalysis::arithmeticConversion(Type& lhs, Expression& rhs) const
{
    lhs = integerPromotion(std::move(lhs));
    rhs = integerPromotion(std::move(rhs));
    if (!isArithmetic(lhs) || !isArithmetic(rhs.getType()))
    {
        return;
    }
    if (lhs == rhs.getType())
    {
        return;
    }
    auto& lhsPrim = cld::get<PrimitiveType>(lhs.get());
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
        type = lhsType ? lhs : rhs.getType();
    }
    else
    {
        type = !lhsPrim.isSigned() ? lhs : rhs.getType();
    }
    lhs = type;
    rhs = Expression(std::move(type), ValueCategory::Rvalue,
                     Conversion(Conversion::ArithmeticConversion, std::make_unique<Expression>(std::move(rhs))));
}

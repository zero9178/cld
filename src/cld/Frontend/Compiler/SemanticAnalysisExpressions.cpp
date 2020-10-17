#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

cld::Semantics::Type cld::Semantics::SemanticAnalysis::removeQualifiers(Type type)
{
    if (type.isConst() || type.isVolatile()
        || (std::holds_alternative<PointerType>(type.getVariant())
            && cld::get<PointerType>(type.getVariant()).isRestricted()))
    {
        if (!std::holds_alternative<PointerType>(type.getVariant())
            || !cld::get<PointerType>(type.getVariant()).isRestricted())
        {
            return Type(false, false, std::move(type).getVariant());
        }
        return PointerType::create(false, false, false,
                                   cld::get<cld::Semantics::PointerType>(type.getVariant()).getElementType());
    }
    return type;
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
    auto& fields = getFields(type);
    for (auto& iter : fields)
    {
        if (isConst(*iter.second.type))
        {
            return true;
        }
    }
    return false;
}

bool cld::Semantics::SemanticAnalysis::doAssignmentLikeConstraints(
    const Type& lhsType, Expression& rhsValue, cld::function_ref<void()> mustBeArithmetic,
    cld::function_ref<void()> mustBeArithmeticOrPointer, cld::function_ref<void()> incompleteType,
    cld::function_ref<void()> incompatibleTypes, cld::function_ref<void()> notICE,
    cld::function_ref<void(const ConstValue&)> notNull, cld::function_ref<void()> mustBePointer,
    cld::function_ref<void()> voidFunctionPointers)
{
    if (isArithmetic(lhsType))
    {
        if (!isBool(lhsType))
        {
            if (!rhsValue.getType().isUndefined() && !isArithmetic(rhsValue.getType()))
            {
                mustBeArithmetic();
                return false;
            }
        }
        else if (!rhsValue.getType().isUndefined() && !isScalar(rhsValue.getType()))
        {
            mustBeArithmeticOrPointer();
            return false;
        }
        if (lhsType != rhsValue.getType())
        {
            rhsValue = Expression(removeQualifiers(lhsType), ValueCategory::Rvalue,
                                  Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(rhsValue))));
        }
        return true;
    }

    if (isRecord(lhsType))
    {
        if (!isCompleteType(lhsType))
        {
            incompleteType();
            return false;
        }

        if (!rhsValue.getType().isUndefined()
            && !typesAreCompatible(removeQualifiers(lhsType), removeQualifiers(rhsValue.getType())))
        {
            incompatibleTypes();
            return false;
        }
        return true;
    }

    if (!std::holds_alternative<PointerType>(lhsType.getVariant()))
    {
        return true;
    }

    if (isInteger(rhsValue.getType()))
    {
        auto constant = evaluateConstantExpression(rhsValue);
        if (!constant)
        {
            notICE();
            return false;
        }
        if (constant->toUInt() != 0)
        {
            notNull(*constant);
            return false;
        }
        rhsValue = Expression(removeQualifiers(lhsType), ValueCategory::Rvalue,
                              Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(rhsValue))));
        return true;
    }

    if (!rhsValue.getType().isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().getVariant()))
    {
        mustBePointer();
        return false;
    }

    if (rhsValue.getType().isUndefined())
    {
        return true;
    }

    auto& lhsElementType = cld::get<PointerType>(lhsType.getVariant()).getElementType();
    auto& rhsElementType = cld::get<PointerType>(rhsValue.getType().getVariant()).getElementType();
    if (isVoid(lhsElementType) != isVoid(rhsElementType))
    {
        bool leftIsVoid = isVoid(lhsElementType);
        auto& nonVoidType = leftIsVoid ? rhsElementType : lhsElementType;
        if (std::holds_alternative<FunctionType>(nonVoidType.getVariant()))
        {
            voidFunctionPointers();
            return false;
        }
        if ((!lhsElementType.isConst() && rhsElementType.isConst())
            || (!lhsElementType.isVolatile() && rhsElementType.isVolatile()))
        {
            incompatibleTypes();
            return false;
        }
        if (removeQualifiers(lhsElementType) != removeQualifiers(rhsElementType))
        {
            rhsValue = Expression(removeQualifiers(lhsType), ValueCategory::Rvalue,
                                  Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(rhsValue))));
        }
        return true;
    }

    if (!typesAreCompatible(removeQualifiers(lhsElementType), removeQualifiers(rhsElementType)))
    {
        incompatibleTypes();
        return false;
    }
    if ((!lhsElementType.isConst() && rhsElementType.isConst())
        || (!lhsElementType.isVolatile() && rhsElementType.isVolatile()))
    {
        incompatibleTypes();
        return false;
    }
    if (removeQualifiers(lhsElementType) != removeQualifiers(rhsElementType))
    {
        rhsValue = Expression(removeQualifiers(lhsType), ValueCategory::Rvalue,
                              Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(rhsValue))));
    }
    return true;
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
                if (isArray(lhsValue.getType()))
                {
                    log(Errors::Semantics::CANNOT_ASSIGN_TO_ARRAY_TYPE_N.args(lhsValue, m_sourceInterface, lhsValue,
                                                                              *token));
                }
                doAssignmentLikeConstraints(
                    lhsValue.getType(), rhsValue,
                    [&, token = token] {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    },
                    [&, token = token] {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE
                                .args(rhsValue, m_sourceInterface, *token, rhsValue));
                    },
                    [&, token = token] {
                        log(Errors::Semantics::CANNOT_ASSIGN_TO_INCOMPLETE_TYPE_N.args(lhsValue, m_sourceInterface,
                                                                                       lhsValue, *token));
                    },
                    [&, token = token] {
                        log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(lhsValue, m_sourceInterface,
                                                                                     lhsValue, *token, rhsValue));
                    },
                    [&, token = token] {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL_2.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    },
                    [&, token = token](const ConstValue& constant) {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL.args(
                            rhsValue, m_sourceInterface, *token, rhsValue, constant));
                    },
                    [&, token = token] {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    },
                    [&, token = token] {
                        if (isVoid(cld::get<PointerType>(lhsValue.getType().getVariant()).getElementType()))
                        {
                            log(Errors::Semantics::CANNOT_ASSIGN_FUNCTION_POINTER_TO_VOID_POINTER.args(
                                lhsValue, m_sourceInterface, lhsValue, *token, rhsValue));
                        }
                        else
                        {
                            log(Errors::Semantics::CANNOT_ASSIGN_VOID_POINTER_TO_FUNCTION_POINTER.args(
                                lhsValue, m_sourceInterface, lhsValue, *token, rhsValue));
                        }
                    });
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
                if (std::holds_alternative<PointerType>(lhsValue.getType().getVariant()))
                {
                    auto& elementType = cld::get<PointerType>(lhsValue.getType().getVariant()).getElementType();
                    if (std::holds_alternative<FunctionType>(elementType.getVariant()))
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            lhsValue, m_sourceInterface, lhsValue));
                    }
                    else if (!isCompleteType(elementType))
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            lhsValue, m_sourceInterface, elementType, lhsValue, lhsValue.getType()));
                    }
                    if (!rhsValue.getType().isUndefined() && !isInteger(rhsValue.getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                }
                else if (isArithmetic(lhsValue.getType()))
                {
                    if (!rhsValue.getType().isUndefined() && !isArithmetic(rhsValue.getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                            rhsValue, m_sourceInterface, *token, rhsValue));
                    }
                    else if (isArithmetic(rhsValue.getType()))
                    {
                        auto lhsType = lhsValue.getType();
                        arithmeticConversion(lhsType, rhsValue);
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
                if (!rhsValue.getType().isUndefined() && !isArithmetic(rhsValue.getType()))
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
    if (node.getIdentifier()->getText() == "__func__" && inFunction())
    {
        auto functionName = getCurrentFunctionScope()->currentFunction->getNameToken()->getText();
        auto type = ArrayType::create(false, false, false, false,
                                      PrimitiveType::createChar(false, false, m_sourceInterface.getLanguageOptions()),
                                      functionName.size() + 1);
        return Expression(std::move(type), ValueCategory::Lvalue,
                          Constant(cld::to_string(functionName), node.begin(), node.end()));
    }
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
    auto type = cld::match(
        *result, [](const std::pair<ConstValue, Type>&) -> Type { CLD_UNREACHABLE; },
        [](const Type&) -> Type { CLD_UNREACHABLE; },
        [](const BuiltinFunction* builtinFunction) { return builtinFunction->getType(); },
        [&](const auto* ptr) {
            if (getCurrentFunctionScope() && getCurrentFunctionScope()->currentFunction->isInline()
                && getCurrentFunctionScope()->currentFunction->getLinkage() == Linkage::External
                && ptr->getLinkage() == Linkage::Internal)
            {
                log(Errors::Semantics::
                        INLINE_FUNCTION_N_WITH_EXTERNAL_LINKAGE_IS_NOT_ALLOWED_TO_CONTAIN_OR_ACCESS_THE_INTERNAL_IDENTIFIER_N
                            .args(*ptr->getNameToken(), m_sourceInterface,
                                  *getCurrentFunctionScope()->currentFunction->getNameToken(), *ptr->getNameToken()));
            }
            return ptr->getType();
        });
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

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionBuiltinVAArg& vaArg)
{
    auto expression = visit(vaArg.getAssignmentExpression());
    auto& vaList = *getTypedef("__builtin_va_list");
    if (isArray(expression.getType()))
    {
        // Do Pointer decay
        expression = lvalueConversion(std::move(expression));
    }
    if (!expression.getType().isUndefined() && !typesAreCompatible(expression.getType(), adjustParameterType(vaList)))
    {
        log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST.args(
            expression, m_sourceInterface, 1, expression));
    }
    auto type =
        declaratorsToType(vaArg.getTypeName().getSpecifierQualifiers(), vaArg.getTypeName().getAbstractDeclarator());
    if (!isCompleteType(type))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_VA_ARG.args(vaArg.getTypeName(), m_sourceInterface, type,
                                                                vaArg.getTypeName()));
        return Expression(vaArg);
    }
    type = removeQualifiers(std::move(type));
    return Expression(std::move(type), ValueCategory::Rvalue,
                      BuiltinVAArg(vaArg.getBuiltinToken(), vaArg.getOpenParentheses(),
                                   std::make_unique<Expression>(std::move(expression)), vaArg.getCloseParentheses()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionBuiltinOffsetOf& node)
{
    return Expression(node);
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
    if (first.getType().isUndefined() || second.getType().isUndefined())
    {
        return Expression(node);
    }
    if (!std::holds_alternative<PointerType>(first.getType().getVariant())
        && !std::holds_alternative<PointerType>(second.getType().getVariant()))
    {
        log(Errors::Semantics::EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE.args(node.getPostFixExpression(),
                                                                               m_sourceInterface, first, second));
        return Expression(node);
    }
    auto& pointerExpr = std::holds_alternative<PointerType>(first.getType().getVariant()) ? first : second;
    auto& intExpr = &pointerExpr == &first ? second : first;
    if (!isInteger(intExpr.getType()))
    {
        log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(intExpr, m_sourceInterface, intExpr));
        return Expression(node);
    }
    auto elementType = cld::get<PointerType>(pointerExpr.getType().getVariant()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            pointerExpr, m_sourceInterface, elementType, pointerExpr));
        return Expression(node);
    }
    if (std::holds_alternative<FunctionType>(elementType.getVariant()))
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

std::optional<std::pair<cld::Semantics::Type, const cld::Semantics::Field * CLD_NON_NULL>>
    cld::Semantics::SemanticAnalysis::checkMemberAccess(const Type& recordType,
                                                        const Syntax::PostFixExpression& postFixExpr,
                                                        const Lexer::CToken& identifier)
{
    const FieldMap* fields = nullptr;
    if (std::holds_alternative<AnonymousUnionType>(recordType.getVariant()))
    {
        fields = &cld::get<AnonymousUnionType>(recordType.getVariant()).getFields();
    }
    if (std::holds_alternative<AnonymousStructType>(recordType.getVariant()))
    {
        fields = &cld::get<AnonymousStructType>(recordType.getVariant()).getFields();
    }
    if (std::holds_alternative<StructType>(recordType.getVariant()))
    {
        auto& structType = cld::get<StructType>(recordType.getVariant());
        auto* structDef = getStructDefinition(structType.getName(), structType.getScopeOrId());
        if (!structDef)
        {
            log(Errors::Semantics::STRUCT_N_IS_AN_INCOMPLETE_TYPE.args(postFixExpr, m_sourceInterface,
                                                                       structType.getName(), postFixExpr));
            return {};
        }
        fields = &structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(recordType.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(recordType.getVariant());
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
    auto result = fields->find(identifier.getText());
    if (result == fields->end())
    {
        if (std::holds_alternative<AnonymousUnionType>(recordType.getVariant()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(identifier, m_sourceInterface,
                                                                                    identifier));
        }
        if (std::holds_alternative<AnonymousStructType>(recordType.getVariant()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(identifier, m_sourceInterface,
                                                                                     identifier));
        }
        if (std::holds_alternative<UnionType>(recordType.getVariant()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(
                identifier, m_sourceInterface, identifier, cld::get<UnionType>(recordType.getVariant()).getName()));
        }
        if (std::holds_alternative<StructType>(recordType.getVariant()))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                identifier, m_sourceInterface, identifier, cld::get<StructType>(recordType.getVariant()).getName()));
        }
        return {};
    }
    Type type;
    if (std::pair{result->second.type->isConst(), result->second.type->isVolatile()}
        >= std::pair{recordType.isConst(), recordType.isVolatile()})
    {
        type = *result->second.type;
    }
    else
    {
        type = Type(recordType.isConst(), recordType.isVolatile(), result->second.type->getVariant());
    }
    return std::pair(std::move(type), &result->second);
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
    auto& [type, field] = *result;
    if (type.isUndefined())
    {
        return Expression(node);
    }
    auto category = structOrUnion.getValueCategory();
    return Expression(
        std::move(type), category,
        MemberAccess(std::make_unique<Expression>(std::move(structOrUnion)), *field, node.getIdentifier()));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionArrow& node)
{
    auto structOrUnionPtr = visit(node.getPostFixExpression());
    if (structOrUnionPtr.isUndefined())
    {
        return Expression(node);
    }
    if (!std::holds_alternative<PointerType>(structOrUnionPtr.getType().getVariant()))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_ARROW_OPERATOR.args(
            structOrUnionPtr, m_sourceInterface, structOrUnionPtr));
        return Expression(node);
    }
    auto& structOrUnion = cld::get<PointerType>(structOrUnionPtr.getType().getVariant()).getElementType();
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
    auto& [type, field] = *result;
    if (type.isUndefined())
    {
        return Expression(node);
    }
    return Expression(
        std::move(type), ValueCategory::Lvalue,
        MemberAccess(std::make_unique<Expression>(std::move(structOrUnionPtr)), *field, node.getIdentifier()));
}

namespace
{
bool isBuiltinVAStart(const cld::Semantics::Expression& expression)
{
    auto* declRead = std::get_if<cld::Semantics::DeclarationRead>(&expression.getVariant());
    if (!declRead)
    {
        return false;
    }
    auto* builtin = std::get_if<const cld::Semantics::BuiltinFunction*>(&declRead->getDeclRead());
    if (!builtin)
    {
        return false;
    }
    return (*builtin)->getKind() == cld::Semantics::BuiltinFunction::VAStart;
}

bool isBuiltinFunction(const cld::Semantics::Expression& expression)
{
    auto* declRead = std::get_if<cld::Semantics::DeclarationRead>(&expression.getVariant());
    if (!declRead)
    {
        return false;
    }
    return std::holds_alternative<const cld::Semantics::BuiltinFunction*>(declRead->getDeclRead());
}
} // namespace

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionFunctionCall& node)
{
    auto function = visit(node.getPostFixExpression());
    if (isBuiltinVAStart(function))
    {
        std::vector<Expression> arguments;
        if (node.getOptionalAssignmentExpressions().size() < 2)
        {
            log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N.args(
                function, m_sourceInterface, 2, node.getOptionalAssignmentExpressions().size(), function));
        }
        else if (node.getOptionalAssignmentExpressions().size() > 2)
        {
            log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N.args(
                function, m_sourceInterface, 2, node.getOptionalAssignmentExpressions().size(), function,
                llvm::ArrayRef(node.getOptionalAssignmentExpressions()).drop_front(2)));
        }
        else
        {
            arguments.push_back(visit(node.getOptionalAssignmentExpressions()[0]));
            auto& vaList = *getTypedef("__builtin_va_list");
            if (!arguments[0].getType().isUndefined() && !typesAreCompatible(arguments[0].getType(), vaList))
            {
                log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST.args(
                    arguments[0], m_sourceInterface, 1, arguments[0]));
            }
            // even if the call is done using "__builtin_va_start(list,...)" the list is actually passed as a pointer.
            // In the case of x86_64 ABI __builtin_va_list is an array type causing pointer decay, in the case
            // of Windows x64 it's basically as if &list was written. Since this is a builtin call the backend will
            // have to do special handling either way but we'll insert an lvalueConversion now to force array to pointer
            // decay
            arguments.back() = lvalueConversion(std::move(arguments.back()));
            auto expression = visit(node.getOptionalAssignmentExpressions()[1]);
            if (!getCurrentFunctionScope())
            {
                log(Errors::Semantics::CANNOT_USE_VA_START_OUTSIDE_OF_A_FUNCTION.args(function, m_sourceInterface,
                                                                                      function));
            }
            else if (!cld::get<FunctionType>(getCurrentFunctionScope()->currentFunction->getType().getVariant())
                          .isLastVararg())
            {
                log(Errors::Semantics::CANNOT_USE_VA_START_IN_A_FUNCTION_WITH_FIXED_ARGUMENT_COUNT.args(
                    function, m_sourceInterface, function));
            }
            else if (auto* declRead = std::get_if<DeclarationRead>(&expression.getVariant());
                     !declRead
                     || (!getCurrentFunctionScope()->currentFunction->getParameterDeclarations().empty()
                         && declRead->getDeclRead()
                                != DeclarationRead::Variant(getCurrentFunctionScope()
                                                                ->currentFunction->getParameterDeclarations()
                                                                .back()
                                                                .get())))
            {
                log(Warnings::Semantics::SECOND_ARGUMENT_OF_VA_START_SHOULD_BE_THE_LAST_PARAMETER.args(
                    expression, m_sourceInterface, expression));
            }
            arguments.push_back(lvalueConversion(std::move(expression)));
        }
        return Expression(
            PrimitiveType::createVoid(false, false), ValueCategory::Rvalue,
            CallExpression(std::make_unique<Expression>(
                               PointerType::create(false, false, false, function.getType()), ValueCategory::Rvalue,
                               Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(function)))),
                           node.getOpenParentheses(), std::move(arguments), node.getCloseParentheses()));
    }
    if (!isBuiltinFunction(function))
    {
        function = lvalueConversion(std::move(function));
        if (!std::holds_alternative<PointerType>(function.getType().getVariant())
            || !std::holds_alternative<FunctionType>(
                cld::get<PointerType>(function.getType().getVariant()).getElementType().getVariant()))
        {
            if (!function.getType().isUndefined())
            {
                log(Errors::Semantics::CANNOT_CALL_NON_FUNCTION_TYPE.args(
                    function, m_sourceInterface, function, *node.getOpenParentheses(), *node.getCloseParentheses()));
            }
            for (auto& iter : node.getOptionalAssignmentExpressions())
            {
                visit(iter);
            }
            return Expression(node);
        }
    }
    else
    {
        auto type = function.getType();
        function = Expression(PointerType::create(false, false, false, std::move(type)), ValueCategory::Rvalue,
                              Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(function))));
    }
    auto& ft =
        cld::get<FunctionType>(cld::get<PointerType>(function.getType().getVariant()).getElementType().getVariant());
    std::vector<Expression> arguments;
    if (ft.isKandR())
    {
        for (auto& iter : node.getOptionalAssignmentExpressions())
        {
            arguments.push_back(defaultArgumentPromotion(visit(iter)));
        }
    }
    else
    {
        auto callsFunction = [&] {
            if (!std::holds_alternative<Conversion>(function.getVariant()))
            {
                return false;
            }
            auto& conversion = cld::get<Conversion>(function.getVariant());
            if (conversion.getKind() != Conversion::LValue)
            {
                return false;
            }
            if (!std::holds_alternative<DeclarationRead>(conversion.getExpression().getVariant()))
            {
                return false;
            }
            auto& decl = cld::get<DeclarationRead>(conversion.getExpression().getVariant());
            return cld::match(
                decl.getDeclRead(), [](const FunctionDefinition*) { return true; },
                [](const Declaration* declaration) {
                    return std::holds_alternative<FunctionType>(declaration->getType().getVariant());
                },
                [](const BuiltinFunction*) { return true; });
        }();
        auto& argumentTypes = ft.getArguments();
        if (node.getOptionalAssignmentExpressions().size() < argumentTypes.size())
        {
            if (!ft.isLastVararg())
            {
                if (callsFunction)
                {
                    auto& decl = cld::get<DeclarationRead>(
                        cld::get<Conversion>(function.getVariant()).getExpression().getVariant());
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
                    auto& decl = cld::get<DeclarationRead>(
                        cld::get<Conversion>(function.getVariant()).getExpression().getVariant());
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
                auto& decl =
                    cld::get<DeclarationRead>(cld::get<Conversion>(function.getVariant()).getExpression().getVariant());
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
                    visit(node.getOptionalAssignmentExpressions()[i]);
                    arguments.emplace_back(node.getOptionalAssignmentExpressions()[i]);
                    continue;
                }
                auto expression = lvalueConversion(visit(node.getOptionalAssignmentExpressions()[i]));
                if (expression.isUndefined())
                {
                    arguments.push_back(std::move(expression));
                    continue;
                }
                doAssignmentLikeConstraints(
                    paramType, expression,
                    [&] {
                        log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                            expression, m_sourceInterface, i + 1, expression));
                    },
                    [&] {
                        log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                            expression, m_sourceInterface, i + 1, expression));
                    },
                    [&] {
                        log(Errors::Semantics::CANNOT_PASS_ARGUMENT_TO_INCOMPLETE_TYPE_N_OF_PARAMETER_N.args(
                            expression, m_sourceInterface, paramType, i + 1, expression));
                    },
                    [&] {
                        log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_N.args(
                            expression, m_sourceInterface, i + 1, paramType, expression));
                    },
                    [&] {
                        log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_NULL_2.args(expression, m_sourceInterface,
                                                                                     i + 1, expression));
                    },
                    [&](const ConstValue& constant) {
                        log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_NULL.args(expression, m_sourceInterface, i + 1,
                                                                                   expression, constant));
                    },
                    [&] {
                        log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_A_POINTER_TYPE.args(
                            expression, m_sourceInterface, i + 1, expression));
                    },
                    [&] {
                        if (isVoid(cld::get<PointerType>(paramType.getVariant()).getElementType()))
                        {
                            log(Errors::Semantics::CANNOT_PASS_FUNCTION_POINTER_TO_VOID_POINTER_PARAMETER.args(
                                expression, m_sourceInterface, expression));
                        }
                        else
                        {
                            log(Errors::Semantics::CANNOT_PASS_VOID_POINTER_TO_FUNCTION_POINTER_PARAMETER.args(
                                expression, m_sourceInterface, expression));
                        }
                    });
                arguments.push_back(std::move(expression));
            }
            for (; i < node.getOptionalAssignmentExpressions().size(); i++)
            {
                arguments.push_back(defaultArgumentPromotion(visit(node.getOptionalAssignmentExpressions()[i])));
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
    auto& elementType = cld::get<PointerType>(value.getType().getVariant()).getElementType();
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(value, m_sourceInterface, elementType,
                                                                                 value, value.getType()));
    }
    else if (std::holds_alternative<FunctionType>(elementType.getVariant()))
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
    auto type =
        declaratorsToType(node.getTypeName().getSpecifierQualifiers(), node.getTypeName().getAbstractDeclarator());
    if (type.isUndefined())
    {
        visit(node.getInitializerList(), type, !inFunction() || m_inStaticInitializer);
        return Expression(node);
    }
    if (std::holds_alternative<FunctionType>(type.getVariant()))
    {
        log(Errors::Semantics::CANNOT_INITIALIZE_FUNCTION_TYPE.args(node.getTypeName(), m_sourceInterface,
                                                                    node.getTypeName(), type));
        visit(node.getInitializerList(), Type{}, !inFunction() || m_inStaticInitializer);
        return Expression(node);
    }
    if (isVariableLengthArray(type))
    {
        log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE.args(node.getTypeName(), m_sourceInterface,
                                                                                 node.getTypeName(), type));
        visit(node.getInitializerList(), Type{}, !inFunction() || m_inStaticInitializer);
        return Expression(node);
    }
    Initializer value{Expression(node)};
    if (std::holds_alternative<AbstractArrayType>(type.getVariant()))
    {
        std::size_t size = 0;
        value = visit(node.getInitializerList(), type, !inFunction() || m_inStaticInitializer, &size);
        auto& abstractArrayType = cld::get<AbstractArrayType>(type.getVariant());
        type = ArrayType::create(type.isConst(), type.isVolatile(), abstractArrayType.isRestricted(), false,
                                 abstractArrayType.getType(), size);
    }
    else
    {
        value = visit(node.getInitializerList(), type, !inFunction() || m_inStaticInitializer);
    }
    return Expression(std::move(type), ValueCategory::Lvalue,
                      CompoundLiteral(node.getOpenParentheses(), std::move(value), node.getCloseParentheses(),
                                      node.getInitializerList().end(), m_inStaticInitializer));
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
            if (std::holds_alternative<DeclarationRead>(value.getVariant()))
            {
                auto& declRead = cld::get<DeclarationRead>(value.getVariant());
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
            if (!std::holds_alternative<CallExpression>(value.getVariant())
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
            if (!value.getType().isUndefined() && !std::holds_alternative<PointerType>(value.getType().getVariant()))
            {
                log(Errors::Semantics::CANNOT_DEREFERENCE_NON_POINTER_TYPE_N.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), value));
                return Expression(node);
            }
            if (value.getType().isUndefined())
            {
                return Expression(node);
            }
            auto elementType = cld::get<PointerType>(value.getType().getVariant()).getElementType();
            return Expression(std::move(elementType), ValueCategory::Lvalue,
                              UnaryOperator(UnaryOperator::Dereference, node.getUnaryToken(),
                                            std::make_unique<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            value = integerPromotion(std::move(value));
            if (!value.getType().isUndefined() && !isArithmetic(value.getType()))
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
            if (!value.getType().isUndefined() && !isInteger(value.getType()))
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
            if (!value.getType().isUndefined() && !isScalar(value.getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), value));
            }
            value = toBool(std::move(value));
            return Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                              ValueCategory::Rvalue,
                              UnaryOperator(UnaryOperator::BooleanNegate, node.getUnaryToken(),
                                            std::make_unique<Expression>(std::move(value))));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator ::GNUExtension: return value;
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
            if (std::holds_alternative<FunctionType>(type.getVariant()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(exp, m_sourceInterface, exp, type));
                return Expression(node);
            }
            if (isBitfieldAccess(exp))
            {
                log(Errors::Semantics::BITFIELD_NOT_ALLOWED_IN_SIZE_OF.args(exp, m_sourceInterface, exp));
            }
            if (!isVariableLengthArray(type))
            {
                auto size = exp.getType().getSizeOf(*this);
                return Expression(
                    PrimitiveType::createSizeT(false, false, m_sourceInterface.getLanguageOptions()),
                    ValueCategory::Rvalue,
                    SizeofOperator(node.getSizeOfToken(), size, std::make_unique<Expression>(std::move(exp))));
            }

            return Expression(PrimitiveType::createSizeT(false, false, m_sourceInterface.getLanguageOptions()),
                              ValueCategory::Rvalue,
                              SizeofOperator(node.getSizeOfToken(), {}, std::make_unique<Expression>(std::move(exp))));
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
            if (std::holds_alternative<FunctionType>(type.getVariant()))
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(*typeName, m_sourceInterface,
                                                                                 *typeName, type));
                return Expression(node);
            }
            if (!isVariableLengthArray(type))
            {
                auto size = type.getSizeOf(*this);
                return Expression(PrimitiveType::createSizeT(false, false, m_sourceInterface.getLanguageOptions()),
                                  ValueCategory::Rvalue,
                                  SizeofOperator(node.getSizeOfToken(), size,
                                                 SizeofOperator::TypeVariant{node.getSizeOfToken() + 1, std::move(type),
                                                                             node.end() - 1}));
            }

            return Expression(PrimitiveType::createSizeT(false, false, m_sourceInterface.getLanguageOptions()),
                              ValueCategory::Rvalue,
                              SizeofOperator(node.getSizeOfToken(), {},
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

    return Expression(PrimitiveType::createLongLong(false, false), ValueCategory::Rvalue,
                      Constant(llvm::APSInt(64, false), node.begin(), node.end()));
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
            if (isVoid(type))
            {
                return Expression(
                    removeQualifiers(type), ValueCategory::Rvalue,
                    Cast(cast.openParentheses, cast.closeParentheses, std::make_unique<Expression>(std::move(value))));
            }
            if (!isScalar(type))
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
            if (std::holds_alternative<PointerType>(type.getVariant()))
            {
                if (!std::holds_alternative<PointerType>(value.getType().getVariant()) && !isInteger(value.getType()))
                {
                    log(Errors::Semantics::CANNOT_CAST_NON_INTEGER_AND_POINTER_TYPE_N_TO_POINTER_TYPE.args(
                        value, m_sourceInterface, value));
                    return Expression(node);
                }
            }
            else if (std::holds_alternative<PointerType>(value.getType().getVariant()))
            {
                if (!std::holds_alternative<PointerType>(type.getVariant()) && !isInteger(type))
                {
                    log(Errors::Semantics::CANNOT_CAST_POINTER_TYPE_TO_NON_INTEGER_AND_POINTER_TYPE.args(
                        cast.typeName, m_sourceInterface, cast.typeName, value));
                    return Expression(node);
                }
            }
            return Expression(
                type, ValueCategory::Rvalue,
                Cast(cast.openParentheses, cast.closeParentheses, std::make_unique<Expression>(std::move(value))));
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
        if (!value.getType().isUndefined() && !isScalar(value.getType()))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                value, m_sourceInterface, *token, value));
            errors = true;
        }
        if (!rhsValue.getType().isUndefined() && !isScalar(rhsValue.getType()))
        {
            log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                rhsValue, m_sourceInterface, *token, rhsValue));
            errors = true;
        }
        switch (kind)
        {
            case Syntax::AdditiveExpression::BinaryMinus:
            {
                if (std::holds_alternative<PrimitiveType>(value.getType().getVariant())
                    && std::holds_alternative<PrimitiveType>(rhsValue.getType().getVariant()))
                {
                    auto type = value.getType();
                    value = Expression(std::move(type), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Subtraction, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                    continue;
                }
                if (value.getType().isUndefined() || rhsValue.getType().isUndefined() || errors)
                {
                    value = Expression(value.begin(), rhsValue.end());
                    continue;
                }
                if (std::holds_alternative<PrimitiveType>(value.getType().getVariant())
                    && std::holds_alternative<PointerType>(rhsValue.getType().getVariant()))
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
                auto valueElementType = cld::get<PointerType>(value.getType().getVariant()).getElementType();
                if (!isCompleteType(valueElementType))
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                        value, m_sourceInterface, valueElementType, value, value.getType()));
                }
                else if (std::holds_alternative<FunctionType>(valueElementType.getVariant()))
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        value, m_sourceInterface, value));
                }
                if (std::holds_alternative<PointerType>(rhsValue.getType().getVariant()))
                {
                    auto rhsElementType = cld::get<PointerType>(rhsValue.getType().getVariant()).getElementType();
                    if (!isCompleteType(rhsElementType))
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            rhsValue, m_sourceInterface, rhsElementType, rhsValue, rhsValue.getType()));
                    }
                    else if (std::holds_alternative<FunctionType>(rhsElementType.getVariant()))
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
                    value = Expression(
                        PrimitiveType::createPtrdiffT(false, false, m_sourceInterface.getLanguageOptions()),
                        ValueCategory::Rvalue,
                        BinaryOperator(std::make_unique<Expression>(std::move(value)), BinaryOperator::Subtraction,
                                       token, std::make_unique<Expression>(std::move(rhsValue))));
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
                if (std::holds_alternative<PrimitiveType>(value.getType().getVariant())
                    && std::holds_alternative<PrimitiveType>(rhsValue.getType().getVariant()))
                {
                    auto type = value.getType();
                    value = Expression(std::move(type), ValueCategory::Rvalue,
                                       BinaryOperator(std::make_unique<Expression>(std::move(value)),
                                                      BinaryOperator::Addition, token,
                                                      std::make_unique<Expression>(std::move(rhsValue))));
                    continue;
                }
                if (value.getType().isUndefined() || rhsValue.getType().isUndefined() || errors)
                {
                    value = Expression(value.begin(), rhsValue.end());
                    continue;
                }
                auto& pointerExpr =
                    std::holds_alternative<PointerType>(value.getType().getVariant()) ? value : rhsValue;
                auto& intExpr = &pointerExpr == &value ? rhsValue : value;
                if (!isInteger(intExpr.getType()))
                {
                    log(Errors::Semantics::EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE.args(
                        intExpr, m_sourceInterface, *token, intExpr));
                }
                auto& elementType = cld::get<PointerType>(pointerExpr.getType().getVariant()).getElementType();
                if (!isCompleteType(elementType))
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                        pointerExpr, m_sourceInterface, elementType, pointerExpr, pointerExpr.getType()));
                }
                else if (std::holds_alternative<FunctionType>(elementType.getVariant()))
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
        else if (std::holds_alternative<PointerType>(value.getType().getVariant()))
        {
            if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().getVariant()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
            else if (!rhsValue.isUndefined())
            {
                auto valueElementType = cld::get<PointerType>(value.getType().getVariant()).getElementType();
                auto rhsElementType = cld::get<PointerType>(rhsValue.getType().getVariant()).getElementType();
                valueElementType = removeQualifiers(std::move(valueElementType));
                if (std::holds_alternative<FunctionType>(valueElementType.getVariant()))
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        value, m_sourceInterface, value));
                    rhsElementType = removeQualifiers(std::move(rhsElementType));
                    if (std::holds_alternative<FunctionType>(rhsElementType.getVariant()))
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            rhsValue, m_sourceInterface, rhsValue));
                    }
                }
                else
                {
                    rhsElementType = removeQualifiers(std::move(rhsElementType));
                    if (std::holds_alternative<FunctionType>(rhsElementType.getVariant()))
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
            if (std::holds_alternative<PointerType>(rhsValue.getType().getVariant()) && isInteger(value.getType()))
            {
                auto constant = evaluateConstantExpression(value);
                if (!constant || constant->toUInt() != 0)
                {
                    log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                        rhsValue, m_sourceInterface, *token, rhsValue));
                }
                value = Expression(rhsValue.getType(), ValueCategory::Rvalue,
                                   Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(value))));
            }
            else if (!rhsValue.isUndefined() && !isArithmetic(rhsValue.getType()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
        }
        else if (std::holds_alternative<PointerType>(value.getType().getVariant()))
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
                rhsValue =
                    Expression(value.getType(), ValueCategory::Rvalue,
                               Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(rhsValue))));
            }
            else if (!rhsValue.isUndefined() && !std::holds_alternative<PointerType>(rhsValue.getType().getVariant()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                    rhsValue, m_sourceInterface, *token, rhsValue));
            }
            else if (!rhsValue.isUndefined())
            {
                auto valueElementType = cld::get<PointerType>(value.getType().getVariant()).getElementType();
                auto rhsElementType = cld::get<PointerType>(rhsValue.getType().getVariant()).getElementType();
                valueElementType = removeQualifiers(std::move(valueElementType));
                rhsElementType = removeQualifiers(std::move(rhsElementType));
                if (!isVoid(valueElementType) && !isVoid(rhsElementType)
                    && !typesAreCompatible(valueElementType, rhsElementType))
                {
                    log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(value, m_sourceInterface,
                                                                                              value, *token, rhsValue));
                }
                if (valueElementType != rhsElementType)
                {
                    if (isVoid(valueElementType))
                    {
                        value = Expression(
                            rhsValue.getType(), ValueCategory::Rvalue,
                            Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(value))));
                    }
                    else
                    {
                        rhsValue = Expression(
                            value.getType(), ValueCategory::Rvalue,
                            Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(rhsValue))));
                    }
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
    lhs = toBool(std::move(lhs));
    rhs = toBool(std::move(rhs));
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
    condition = lvalueConversion(std::move(condition));
    if (!condition.isUndefined() && !isScalar(condition.getType()))
    {
        log(Errors::Semantics::FIRST_OPERAND_OF_CONDITIONAL_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            condition, m_sourceInterface, condition, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
    }
    condition = toBool(std::move(condition));
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
        if (std::holds_alternative<PointerType>(third.getType().getVariant()) && isInteger(second.getType()))
        {
            auto constant = evaluateConstantExpression(second);
            if (!constant || constant->toUInt() != 0)
            {
                log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE.args(
                    third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
            }
            second = Expression(third.getType(), ValueCategory::Rvalue,
                                Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(second))));
        }
        else if (!third.isUndefined() && !isArithmetic(third.getType()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE.args(
                third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        resultType = third.getType();
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
    else if (std::holds_alternative<PointerType>(second.getType().getVariant()))
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
            third = Expression(second.getType(), ValueCategory::Rvalue,
                               Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(third))));
            resultType = second.getType();
        }
        else if (!third.isUndefined() && !std::holds_alternative<PointerType>(third.getType().getVariant()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_A_POINTER_TYPE.args(
                third, m_sourceInterface, third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        else if (!third.isUndefined())
        {
            auto& secondElementType = cld::get<PointerType>(second.getType().getVariant()).getElementType();
            auto& thirdElementType = cld::get<PointerType>(third.getType().getVariant()).getElementType();
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
                                                      std::move(composite).getVariant()));
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

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::lvalueConversion(Expression&& expression)
{
    if (expression.getValueCategory() != ValueCategory::Lvalue)
    {
        // It's a lil weird to not be doing an lvalue conversion here but pretty much every user of lvalueConversion
        // probably expects it to also handle array to pointer decay even if the array is an rvalue
        if (isArray(expression.getType()))
        {
            auto& elementType = getArrayElementType(expression.getType());
            auto newType = PointerType::create(false, false, false, elementType);
            return Expression(std::move(newType), ValueCategory::Rvalue,
                              Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(expression))));
        }
        return std::move(expression);
    }
    if (isArray(expression.getType()))
    {
        auto& elementType = getArrayElementType(expression.getType());
        auto newType = PointerType::create(false, false, false, elementType);
        return Expression(std::move(newType), ValueCategory::Rvalue,
                          Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
    }
    if (std::holds_alternative<FunctionType>(expression.getType().getVariant()))
    {
        if (isBuiltinFunction(expression))
        {
            log(Errors::Semantics::BUILTIN_FUNCTION_MAY_ONLY_BE_CALLED_DIRECTLY.args(expression, m_sourceInterface,
                                                                                     expression));
        }
        auto newType = PointerType::create(false, false, false, expression.getType());
        return Expression(std::move(newType), ValueCategory::Rvalue,
                          Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
    }
    auto newType = Type(false, false, expression.getType().getVariant());
    return Expression(std::move(newType), ValueCategory::Rvalue,
                      Conversion(Conversion::LValue, std::make_unique<Expression>(std::move(expression))));
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::lvalueConversion(Type type)
{
    if (isArray(type))
    {
        auto& elementType = getArrayElementType(type);
        return PointerType::create(false, false, false, elementType);
    }
    if (std::holds_alternative<FunctionType>(type.getVariant()))
    {
        return PointerType::create(false, false, false, type);
    }
    // If the expression isn't an lvalue and not qualified then conversion is redundant
    if (!type.isVolatile() && !type.isConst()
        && (!std::holds_alternative<PointerType>(type.getVariant())
            || !cld::get<PointerType>(type.getVariant()).isRestricted()))
    {
        return type;
    }
    return Type(false, false, type.getVariant());
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::defaultArgumentPromotion(Expression&& expression)
{
    expression = integerPromotion(std::move(expression));
    if (!std::holds_alternative<PrimitiveType>(expression.getType().getVariant()))
    {
        return std::move(expression);
    }
    auto& prim = cld::get<PrimitiveType>(expression.getType().getVariant());
    if (prim.getKind() != PrimitiveType::Kind::Float)
    {
        return std::move(expression);
    }
    return Expression(
        PrimitiveType::createDouble(false, false), ValueCategory::Rvalue,
        Conversion(Conversion::DefaultArgumentPromotion, std::make_unique<Expression>(std::move(expression))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::integerPromotion(Expression&& expression)
{
    expression = lvalueConversion(std::move(expression));
    if (isEnum(expression.getType()))
    {
        if (std::holds_alternative<AnonymousEnumType>(expression.getType().getVariant()))
        {
            auto type = cld::get<AnonymousEnumType>(expression.getType().getVariant()).getType();
            return Expression(
                std::move(type), ValueCategory::Rvalue,
                Conversion(Conversion::IntegerPromotion, std::make_unique<Expression>(std::move(expression))));
        }
        auto& enumType = cld::get<EnumType>(expression.getType().getVariant());
        auto* enumDef = getEnumDefinition(enumType.getName(), enumType.getScopeOrId());
        CLD_ASSERT(enumDef);
        return Expression(
            enumDef->getType(), ValueCategory::Rvalue,
            Conversion(Conversion::IntegerPromotion, std::make_unique<Expression>(std::move(expression))));
    }
    if (!std::holds_alternative<PrimitiveType>(expression.getType().getVariant()))
    {
        return std::move(expression);
    }
    auto& prim = cld::get<PrimitiveType>(expression.getType().getVariant());
    if (prim.isFloatingPoint() || prim.getBitCount() == 0
        || prim.getBitCount() >= m_sourceInterface.getLanguageOptions().sizeOfInt * 8)
    {
        return std::move(expression);
    }
    return Expression(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                      ValueCategory::Rvalue,
                      Conversion(Conversion::IntegerPromotion, std::make_unique<Expression>(std::move(expression))));
}

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::toBool(Expression&& expression)
{
    return Expression(PrimitiveType::createUnderlineBool(false, false), ValueCategory::Rvalue,
                      Conversion(Conversion::Implicit, std::make_unique<Expression>(std::move(expression))));
}

void cld::Semantics::SemanticAnalysis::arithmeticConversion(Expression& lhs, Expression& rhs)
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
    auto& lhsPrim = cld::get<PrimitiveType>(lhs.getType().getVariant());
    auto& rhsPrim = cld::get<PrimitiveType>(rhs.getType().getVariant());
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

void cld::Semantics::SemanticAnalysis::arithmeticConversion(Type& lhs, Expression& rhs)
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
    auto& lhsPrim = cld::get<PrimitiveType>(lhs.getVariant());
    auto& rhsPrim = cld::get<PrimitiveType>(rhs.getType().getVariant());
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

cld::Semantics::Expression cld::Semantics::SemanticAnalysis::doSingleElementInitialization(
    const Syntax::Node& node, const Type& type, Expression&& expression, bool staticLifetime, std::size_t* size)
{
    CLD_ASSERT(!type.isUndefined() && !expression.isUndefined());

    if (isArray(type))
    {
        auto& elementType = getArrayElementType(type);
        if (!isCharType(elementType)
            && removeQualifiers(elementType)
                   != PrimitiveType::createWcharT(false, false, m_sourceInterface.getLanguageOptions()))
        {
            log(Errors::Semantics::ARRAY_MUST_BE_INITIALIZED_WITH_INITIALIZER_LIST.args(expression, m_sourceInterface,
                                                                                        expression));
            return Expression(node);
        }
        if (!isStringLiteralExpr(expression))
        {
            if (isCharType(elementType))
            {
                log(Errors::Semantics::ARRAY_MUST_BE_INITIALIZED_WITH_STRING_OR_INITIALIZER_LIST.args(
                    expression, m_sourceInterface, expression));
            }
            else
            {
                log(Errors::Semantics::ARRAY_MUST_BE_INITIALIZED_WITH_WIDE_STRING_OR_INITIALIZER_LIST.args(
                    expression, m_sourceInterface, expression));
            }
            return Expression(node);
        }
        auto& str = cld::get<Constant>(expression.getVariant()).getValue();
        if (std::holds_alternative<std::string>(str) && !isCharType(elementType))
        {
            log(Errors::Semantics::CANNOT_INITIALIZE_WCHART_ARRAY_WITH_STRING_LITERAL.args(
                expression, m_sourceInterface, expression));
            return Expression(node);
        }
        if (isCharType(elementType) && std::holds_alternative<Lexer::NonCharString>(str))
        {
            log(Errors::Semantics::CANNOT_INITIALIZE_CHAR_ARRAY_WITH_WIDE_STRING_LITERAL.args(
                expression, m_sourceInterface, expression));
            return Expression(node);
        }
    }
    else
    {
        expression = lvalueConversion(std::move(expression));
    }

    if (staticLifetime)
    {
        auto result = evaluateConstantExpression(expression, Mode::Initialization);
        if (!result)
        {
            for (auto& iter : result.error())
            {
                log(iter);
            }
        }
    }

    doAssignmentLikeConstraints(
        type, expression,
        [&] {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_TYPE.args(expression, m_sourceInterface,
                                                                                      expression));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                expression, m_sourceInterface, expression));
        },
        [&] { CLD_UNREACHABLE; },
        [&] {
            log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_OF_TYPE_N_WITH_INCOMPATIBLE_TYPE_N.args(
                expression, m_sourceInterface, type, expression));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_NULL_2.args(expression, m_sourceInterface, expression));
        },
        [&](const ConstValue& constant) {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_NULL.args(expression, m_sourceInterface, expression,
                                                                        constant));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_A_POINTER_TYPE.args(expression, m_sourceInterface,
                                                                                  expression));
        },
        [&] {
            if (isVoid(cld::get<PointerType>(type.getVariant()).getElementType()))
            {
                log(Errors::Semantics::CANNOT_INITIALIZE_VOID_POINTER_WITH_FUNCTION_POINTER.args(
                    expression, m_sourceInterface, expression));
            }
            else
            {
                log(Errors::Semantics::CANNOT_INITIALIZE_FUNCTION_POINTER_WITH_VOID_POINTER_PARAMETER.args(
                    expression, m_sourceInterface, expression));
            }
        });

    if (std::holds_alternative<AbstractArrayType>(type.getVariant()) && size)
    {
        auto& elementType = cld::get<AbstractArrayType>(type.getVariant()).getType();
        if (isCharType(elementType))
        {
            if (std::holds_alternative<Constant>(expression.getVariant())
                && std::holds_alternative<std::string>(cld::get<Constant>(expression.getVariant()).getValue()))
            {
                *size = cld::get<std::string>(cld::get<Constant>(expression.getVariant()).getValue()).size() + 1;
            }
        }
        else if (std::holds_alternative<PrimitiveType>(elementType.getVariant())
                 && removeQualifiers(elementType)
                        == PrimitiveType::createWcharT(false, false, m_sourceInterface.getLanguageOptions()))
        {
            if (std::holds_alternative<Constant>(expression.getVariant())
                && std::holds_alternative<Lexer::NonCharString>(cld::get<Constant>(expression.getVariant()).getValue()))
            {
                *size = cld::get<Lexer::NonCharString>(cld::get<Constant>(expression.getVariant()).getValue())
                            .characters.size()
                        + 1;
            }
        }
    }

    return std::move(expression);
}

cld::Semantics::Initializer cld::Semantics::SemanticAnalysis::visit(const Syntax::Initializer& node, const Type& type,
                                                                    bool staticLifetime, std::size_t* size)
{
    return cld::match(
        node.getVariant(),
        [&](const Syntax::AssignmentExpression& assignmentExpression) -> Initializer {
            auto value = visit(assignmentExpression);
            if (type.isUndefined() || value.isUndefined())
            {
                return Expression(assignmentExpression);
            }

            if (!isStringLiteralExpr(value))
            {
                value = lvalueConversion(std::move(value));
            }
            return doSingleElementInitialization(assignmentExpression, type, std::move(value), staticLifetime, size);
        },
        [&](const Syntax::InitializerList& initializerList) -> Initializer {
            return visit(initializerList, type, staticLifetime, size);
        });
}

cld::Semantics::Initializer cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::InitializerList& node,
                                                                    const cld::Semantics::Type& type,
                                                                    bool staticLifetime, std::size_t* size)
{
    if (!type.isUndefined() && !isArray(type) && !isRecord(type) && node.getNonCommaExpressionsAndBlocks().size() == 1
        && node.getNonCommaExpressionsAndBlocks()[0].second.empty()
        && std::holds_alternative<Syntax::AssignmentExpression>(
            node.getNonCommaExpressionsAndBlocks()[0].first.getVariant()))
    {
        return visit(node.getNonCommaExpressionsAndBlocks()[0].first, type, staticLifetime, size);
    }
    if (!type.isUndefined() && !isArray(type) && !isRecord(type))
    {
        log(Errors::Semantics::CANNOT_INITIALIZE_ARITHMETIC_OR_POINTER_TYPE_WITH_INITIALIZER_LIST.args(
            node, m_sourceInterface, node));
        return Expression(node);
    }
    std::size_t otherSize = 0;
    if (!size)
    {
        size = &otherSize;
    }
    *size = 1;

    class Node;

    class INode
    {
    public:
        const Type* CLD_NON_NULL type;
        std::size_t index;
        const INode* CLD_NULLABLE parent;

        virtual INode& at(std::size_t index) = 0;

        virtual const INode& at(std::size_t index) const = 0;

        virtual std::size_t size() const = 0;

        virtual void resize(std::size_t size) = 0;

        virtual ~INode() = default;

        virtual void push_back(Node&& node) = 0;
    };

    class Node final : public INode
    {
        std::vector<Node> m_children;

    public:
        Node& at(std::size_t index) override
        {
            CLD_ASSERT(index < m_children.size());
            return m_children[index];
        }

        const Node& at(std::size_t index) const override
        {
            CLD_ASSERT(index < m_children.size());
            return m_children[index];
        }

        std::size_t size() const override
        {
            return m_children.size();
        }

        void resize(std::size_t size) override
        {
            m_children.resize(size);
        }

        void push_back(Node&& node) override
        {
            m_children.push_back(std::move(node));
        }

        Node() = default;
        Node(const Node&) = delete;
        Node& operator=(const Node&) = delete;
        Node(Node&&) noexcept = default;
        Node& operator=(Node&&) noexcept = default;

        Node(const Type& type, std::size_t index, const INode& parent)
        {
            this->type = &type;
            this->index = index;
            this->parent = &parent;
        }
    };
    class Top final : public INode
    {
        std::vector<std::unique_ptr<Node>> m_children;

    public:
        explicit Top(const Type& type)
        {
            this->type = &type;
            index = static_cast<std::size_t>(-1);
            parent = nullptr;
        }

        Node& at(std::size_t index) override
        {
            CLD_ASSERT(index < m_children.size());
            return *m_children[index];
        }

        const Node& at(std::size_t index) const override
        {
            CLD_ASSERT(index < m_children.size());
            return *m_children[index];
        }

        std::size_t size() const override
        {
            return m_children.size();
        }

        void resize(std::size_t size) override
        {
            auto prevSize = m_children.size();
            m_children.resize(size);
            for (std::size_t i = prevSize; i < m_children.size(); i++)
            {
                m_children[i] = std::make_unique<Node>();
            }
        }

        void push_back(Node&& node) override
        {
            m_children.push_back(std::make_unique<Node>(std::move(node)));
        }

    } top(type);

    auto assignChildren = YComb{[&](auto&& self, const Type& type, INode& parent) -> void {
        if (isRecord(type))
        {
            auto ref = getFieldLayout(type);
            if (std::holds_alternative<AbstractArrayType>(ref.back().type->getVariant()))
            {
                ref = ref.drop_back();
            }
            parent.resize(ref.size());
            for (std::size_t i = 0; i < ref.size(); i++)
            {
                parent.at(i).type = ref[i].type.get();
                parent.at(i).index = i;
                parent.at(i).parent = &parent;
                if (isAggregate(*ref[i].type))
                {
                    self(*ref[i].type, parent.at(i));
                }
            }
        }
        if (std::holds_alternative<ArrayType>(type.getVariant()))
        {
            auto& arrayType = cld::get<ArrayType>(type.getVariant());
            parent.resize(arrayType.getSize());
            for (std::size_t i = 0; i < arrayType.getSize(); i++)
            {
                parent.at(i).type = &arrayType.getType();
                parent.at(i).index = i;
                parent.at(i).parent = &parent;
                if (isAggregate(arrayType.getType()))
                {
                    self(arrayType.getType(), parent.at(i));
                }
            }
        }
    }};
    bool isAbstractArray = false;
    if (std::holds_alternative<AbstractArrayType>(type.getVariant()))
    {
        isAbstractArray = true;
        top.resize(1);
        top.at(0).type = &cld::get<AbstractArrayType>(type.getVariant()).getType();
        top.at(0).index = 0;
        top.at(0).parent = &top;
        if (isAggregate(*top.at(0).type))
        {
            assignChildren(*top.at(0).type, top.at(0));
        }
    }
    else
    {
        assignChildren(type, top);
    }

    auto finishRest = YComb{[&](auto&& self, Syntax::InitializerList::vector::const_iterator begin,
                                Syntax::InitializerList::vector::const_iterator end) -> void {
        for (auto iter = begin; iter != end; iter++)
        {
            auto& [initializer, designationList] = *iter;
            if (!designationList.empty())
            {
                // We can check if they're proper integer constant expressions but that's about it
                for (auto& desig : designationList)
                {
                    if (!std::holds_alternative<Syntax::ConstantExpression>(desig))
                    {
                        continue;
                    }
                    auto exp = visit(cld::get<Syntax::ConstantExpression>(desig));
                }
            }
            if (std::holds_alternative<Syntax::InitializerList>(initializer.getVariant()))
            {
                auto& initializerList = cld::get<Syntax::InitializerList>(initializer.getVariant());
                self(initializerList.getNonCommaExpressionsAndBlocks().begin(),
                     initializerList.getNonCommaExpressionsAndBlocks().end());
            }
            else
            {
                visit(cld::get<Syntax::AssignmentExpression>(initializer.getVariant()));
            }
        }
    }};

    if (type.isUndefined())
    {
        finishRest(node.getNonCommaExpressionsAndBlocks().begin(), node.getNonCommaExpressionsAndBlocks().end());
        return Expression(node);
    }

    std::vector<InitializerList::Initialization> initializations;
    const INode* CLD_NULLABLE current = &top;
    std::size_t currentIndex = 0;
    std::vector<std::size_t> path;
    for (auto iter = node.getNonCommaExpressionsAndBlocks().begin();
         iter != node.getNonCommaExpressionsAndBlocks().end(); iter++)
    {
        auto& [initializer, designationList] = *iter;
        if (!designationList.empty())
        {
            current = &top;
            for (auto& desig : designationList)
            {
                if (&desig != &designationList.front())
                {
                    current = &current->at(currentIndex);
                }
                if (isArray(*current->type))
                {
                    if (!std::holds_alternative<Syntax::ConstantExpression>(desig))
                    {
                        log(Errors::Semantics::EXPECTED_INDEX_DESIGNATOR_FOR_ARRAY_TYPE.args(
                            *cld::get<Lexer::CTokenIterator>(desig), m_sourceInterface,
                            *cld::get<Lexer::CTokenIterator>(desig)));
                        finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    auto exp = visit(cld::get<Syntax::ConstantExpression>(desig));
                    auto constant = evaluateConstantExpression(exp);
                    if (!constant)
                    {
                        for (auto& mes : constant.error())
                        {
                            log(mes);
                        }
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    if (!isInteger(exp.getType()))
                    {
                        log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                            exp, m_sourceInterface, exp));
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    if (cld::get<PrimitiveType>(exp.getType().getVariant()).isSigned() && constant->toInt() < 0)
                    {
                        log(Errors::Semantics::DESIGNATOR_INDEX_MUST_NOT_BE_NEGATIVE.args(exp, m_sourceInterface, exp,
                                                                                          constant->toInt()));
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    if (!(current == &top && isAbstractArray)
                        && constant->toUInt() >= cld::get<ArrayType>(current->type->getVariant()).getSize())
                    {
                        log(Errors::Semantics::DESIGNATOR_INDEX_OUT_OF_RANGE_FOR_ARRAY_TYPE_N.args(
                            exp, m_sourceInterface, *current->type, exp, constant->toUInt()));
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    if (current == &top && isAbstractArray)
                    {
                        *size = std::max(*size, constant->toUInt() + 1);
                        auto prevSize = top.size();
                        for (std::size_t i = prevSize; i < *size; i++)
                        {
                            top.push_back({cld::get<AbstractArrayType>(top.type->getVariant()).getType(), i, top});
                            assignChildren(*top.at(i).type, top.at(i));
                        }
                    }
                    currentIndex = constant->toUInt();
                }
                else if (isRecord(*current->type))
                {
                    if (!std::holds_alternative<Lexer::CTokenIterator>(desig))
                    {
                        if (std::holds_alternative<StructType>(current->type->getVariant())
                            || std::holds_alternative<AnonymousStructType>(current->type->getVariant()))
                        {
                            log(Errors::Semantics::EXPECTED_MEMBER_DESIGNATOR_FOR_STRUCT_TYPE.args(
                                desig, m_sourceInterface, desig));
                        }
                        else
                        {
                            log(Errors::Semantics::EXPECTED_MEMBER_DESIGNATOR_FOR_UNION_TYPE.args(
                                desig, m_sourceInterface, desig));
                        }
                        finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    const auto* token = cld::get<Lexer::CTokenIterator>(desig);
                    auto& fields = getFields(*current->type);
                    auto result =
                        std::find_if(fields.begin(), fields.end(),
                                     [name = token->getText()](const auto& pair) { return pair.second.name == name; });
                    if (result == fields.end())
                    {
                        cld::match(
                            current->type->getVariant(),
                            [&](const StructType& structType) {
                                log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                                    *token, m_sourceInterface, *token, structType.getName()));
                            },
                            [&](const AnonymousStructType&) {
                                log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(
                                    *token, m_sourceInterface, *token));
                            },
                            [&](const UnionType& unionType) {
                                log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(
                                    *token, m_sourceInterface, *token, unionType.getName()));
                            },
                            [&](const AnonymousUnionType&) {
                                log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(
                                    *token, m_sourceInterface, *token));
                            },
                            [&](const auto&) { CLD_UNREACHABLE; });
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    if (std::holds_alternative<AbstractArrayType>(result->second.type->getVariant()))
                    {
                        log(Errors::Semantics::CANNOT_INITIALIZE_FLEXIBLE_ARRAY_MEMBER.args(*token, m_sourceInterface,
                                                                                            *token));
                        finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
                        return Expression(node);
                    }
                    if (result->second.parentTypes.empty())
                    {
                        currentIndex = result - fields.begin();
                    }
                    else
                    {
                        for (auto index : llvm::ArrayRef(result->second.indices).drop_back())
                        {
                            current = &current->at(index);
                        }
                        currentIndex = result->second.indices.back();
                    }
                }
                else if (!current->type->isUndefined())
                {
                    if (std::holds_alternative<Syntax::ConstantExpression>(desig))
                    {
                        log(Errors::Semantics::CANNOT_INDEX_INTO_NON_ARRAY_TYPE_N.args(desig, m_sourceInterface,
                                                                                       *current->type, desig));
                    }
                    else
                    {
                        log(Errors::Semantics::CANNOT_ACCESS_MEMBERS_OF_NON_STRUCT_OR_UNION_TYPE_N.args(
                            desig, m_sourceInterface, *current->type, desig));
                    }
                    finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
                    return Expression(node);
                }
            }
        }
        while (current && currentIndex >= current->size())
        {
            if (current == &top && isAbstractArray)
            {
                top.push_back({cld::get<AbstractArrayType>(top.type->getVariant()).getType(), top.size(), top});
                *size = currentIndex + 1;
                assignChildren(*top.at(currentIndex).type, top.at(currentIndex));
            }
            else
            {
                currentIndex = current->index + 1;
                current = current->parent;
            }
        }
        if (!current)
        {
            log(Errors::Semantics::NO_MORE_SUB_OBJECTS_TO_INITIALIZE.args(initializer, m_sourceInterface, initializer));
            finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
            return Expression(node);
        }
        path.clear();
        {
            auto index = currentIndex;
            const auto* curr = current;
            while (true)
            {
                path.push_back(index);
                if (!curr->parent)
                {
                    break;
                }
                index = curr->index;
                curr = curr->parent;
            }
        }
        if (std::holds_alternative<Syntax::InitializerList>(initializer.getVariant()))
        {
            if (current->at(currentIndex).type->isUndefined())
            {
                finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
                return Expression(node);
            }
            auto merge = visit(cld::get<Syntax::InitializerList>(initializer.getVariant()),
                               *current->at(currentIndex).type, staticLifetime, nullptr);
            if (std::holds_alternative<InitializerList>(merge))
            {
                auto& initializerList = cld::get<InitializerList>(merge);
                initializations.reserve(initializations.size() + initializerList.getFields().size());
                std::transform(
                    std::move_iterator(std::move(initializerList).getFields().begin()),
                    std::move_iterator(std::move(initializerList).getFields().end()),
                    std::back_inserter(initializations),
                    [&](InitializerList::Initialization&& initialization) -> InitializerList::Initialization&& {
                        initialization.path.insert(initialization.path.begin(), path.rbegin(), path.rend());
                        return std::move(initialization);
                    });
            }
            else
            {
                auto& expr = cld::get<Expression>(merge);
                initializations.push_back({{path.rbegin(), path.rend()}, std::move(expr)});
            }
        }
        else
        {
            auto expression = visit(cld::get<Syntax::AssignmentExpression>(initializer.getVariant()));
            if (expression.getType().isUndefined())
            {
                finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
            }
            if (!isStringLiteralExpr(expression))
            {
                expression = lvalueConversion(std::move(expression));
            }
            else if (currentIndex == 0 && &top == current && isArray(*top.type)
                     && node.getNonCommaExpressionsAndBlocks().size() == 1 && designationList.empty())
            {
                // Handles the case of a string literal being used to initialize the top level object with optional
                // braces surrounding it
                return doSingleElementInitialization(initializer, type, std::move(expression), staticLifetime, size);
            }
            while (isAggregate(*current->at(currentIndex).type))
            {
                if (typesAreCompatible(removeQualifiers(expression.getType()), *current->at(currentIndex).type)
                    || (isArray(*current->at(currentIndex).type) && isStringLiteralExpr(expression)))
                {
                    break;
                }
                current = &current->at(currentIndex);
                currentIndex = 0;
                path.insert(path.begin(), 0);
            }
            if (current->at(currentIndex).type->isUndefined())
            {
                finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                return Expression(node);
            }
            expression = doSingleElementInitialization(initializer, *current->at(currentIndex).type,
                                                       std::move(expression), staticLifetime, nullptr);
            initializations.push_back({{path.rbegin(), path.rend()}, std::move(expression)});
        }
        if (std::holds_alternative<UnionType>(current->type->getVariant())
            || std::holds_alternative<AnonymousUnionType>(current->type->getVariant()))
        {
            currentIndex = current->index + 1;
            current = current->parent;
        }
        else
        {
            currentIndex++;
        }
    }
    return InitializerList(std::move(initializations));
}

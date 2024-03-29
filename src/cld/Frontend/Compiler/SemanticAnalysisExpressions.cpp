#include "SemanticAnalysis.hpp"

#include <cld/Support/Expected.hpp>
#include <cld/Support/Util.hpp>

#include "ConstValue.hpp"
#include "ErrorMessages.hpp"
#include "SourceInterface.hpp"

cld::IntrVarPtr<cld::Semantics::ExpressionBase> cld::Semantics::SemanticAnalysis::visit(const Syntax::Expression& node)
{
    if (node.getOptionalAssignmentExpressions().empty())
    {
        return visit(node.getAssignmentExpression());
    }
    std::vector<std::pair<IntrVarPtr<ExpressionBase>, Lexer::CTokenIterator>> expressions;
    expressions.emplace_back(visit(node.getAssignmentExpression()),
                             node.getOptionalAssignmentExpressions().front().first);
    auto ref =
        tcb::span(node.getOptionalAssignmentExpressions().data(), node.getOptionalAssignmentExpressions().size() - 1);
    for (const auto* iter = ref.begin(); iter != ref.end(); iter++)
    {
        auto& [token, exp] = *iter;
        expressions.emplace_back(visit(exp), (iter + 1)->first);
    }
    auto last = lvalueConversion(visit(node.getOptionalAssignmentExpressions().back().second));
    IntrVarValue type = last->getType();
    return std::make_unique<CommaExpression>(std::move(type), std::move(expressions), std::move(last));
}

bool cld::Semantics::SemanticAnalysis::isModifiableLValue(const cld::Semantics::ExpressionBase& expression) const
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
    const Type& lhsType, IntrVarPtr<ExpressionBase>& rhsValue, cld::function_ref<void()> mustBeArithmetic,
    cld::function_ref<void()> mustBeArithmeticOrPointer, cld::function_ref<void()> incompleteType,
    cld::function_ref<void()> incompatibleTypes, cld::function_ref<void()> notICE,
    cld::function_ref<void(const ConstValue&)> notNull, cld::function_ref<void()> mustBePointer,
    cld::function_ref<void()> voidFunctionPointers)
{
    if (isArithmetic(lhsType))
    {
        if (!isBool(lhsType))
        {
            if (!rhsValue->getType().isUndefined() && !isArithmetic(rhsValue->getType()))
            {
                mustBeArithmetic();
                return false;
            }
        }
        else if (!rhsValue->getType().isUndefined() && !isScalar(rhsValue->getType()))
        {
            mustBeArithmeticOrPointer();
            return false;
        }
        if (lhsType != rhsValue->getType())
        {
            rhsValue =
                std::make_unique<Conversion>(*removeQualifiers(lhsType), Conversion::Implicit, std::move(rhsValue));
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

        if (!rhsValue->getType().isUndefined()
            && !typesAreCompatible(*removeQualifiers(lhsType), *removeQualifiers(rhsValue->getType())))
        {
            incompatibleTypes();
            return false;
        }
        return true;
    }

    if (lhsType.is<VectorType>())
    {
        if (!rhsValue->getType().isUndefined()
            && !typesAreCompatible(*removeQualifiers(lhsType), *removeQualifiers(rhsValue->getType())))
        {
            incompatibleTypes();
            return false;
        }
        return true;
    }

    if (!lhsType.is<PointerType>())
    {
        return true;
    }

    if (auto npc = checkPointerOperandsForNPC(*rhsValue, lhsType); !npc || *npc != NPCCheck::WrongType)
    {
        if (!npc)
        {
            notNull(npc.error());
            return false;
        }
        switch (*npc)
        {
            case NPCCheck::Success:
                rhsValue =
                    std::make_unique<Conversion>(removeQualifiers(lhsType), Conversion::Implicit, std::move(rhsValue));
                return true;
            case NPCCheck::NotConstExpr: notICE(); return false;
            default: CLD_UNREACHABLE;
        }
    }

    if (!rhsValue->getType().isUndefined() && !rhsValue->getType().is<PointerType>())
    {
        mustBePointer();
        return false;
    }

    if (rhsValue->getType().isUndefined())
    {
        return true;
    }

    auto& lhsElementType = getPointerElementType(lhsType);
    auto& rhsElementType = getPointerElementType(rhsValue->getType());
    if (isVoid(lhsElementType) != isVoid(rhsElementType))
    {
        bool leftIsVoid = isVoid(lhsElementType);
        auto& nonVoidType = leftIsVoid ? rhsElementType : lhsElementType;
        if (nonVoidType.is<FunctionType>())
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
            rhsValue =
                std::make_unique<Conversion>(removeQualifiers(lhsType), Conversion::Implicit, std::move(rhsValue));
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
        rhsValue = std::make_unique<Conversion>(removeQualifiers(lhsType), Conversion::Implicit, std::move(rhsValue));
    }
    return true;
}

void cld::Semantics::SemanticAnalysis::checkVectorCompoundAssign(const IntrVarPtr<ExpressionBase>& lhs,
                                                                 const Type& lhsType, Lexer::CTokenIterator token,
                                                                 const IntrVarPtr<ExpressionBase>& rhs)
{
    if (lhsType.is<VectorType>() && rhs->getType().is<VectorType>())
    {
        if (rhs->getType() != lhsType)
        {
            log(Errors::Semantics::TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH.args(
                *token, m_sourceInterface, *token, *lhs, *rhs));
        }
    }
    else
    {
        log(Errors::Semantics::CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION.args(
            *token, m_sourceInterface, *lhs, *rhs));
    }
    if (!lhs->getType().is<VectorType>() && lhsType.is<VectorType>())
    {
        log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(*token, m_sourceInterface, *lhs, *token, *rhs));
    }
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::AssignmentExpression& node)
{
    if (node.getOptionalConditionalExpressions().empty())
    {
        return visit(node.getConditionalExpression());
    }
    auto ref = tcb::span(node.getOptionalConditionalExpressions());
    auto rhsValue = lvalueConversion(visit(ref.back().conditionalExpression));
    for (auto iter = ref.rbegin(); iter != ref.rend(); iter++)
    {
        auto& [kind, token, rhs] = *iter;
        auto lhsValue =
            iter + 1 != ref.rend() ? visit((iter + 1)->conditionalExpression) : visit(node.getConditionalExpression());
        if (!lhsValue->isUndefined() && !isModifiableLValue(*lhsValue))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST.args(
                *lhsValue, m_sourceInterface, *token, *lhsValue));
        }
        auto resultType = removeQualifiers(lhsValue->getType());
        IntrVarValue lhsType = lhsValue->getType();
        switch (kind)
        {
            case Syntax::AssignmentExpression::NoOperator:
            {
                if (isArray(lhsValue->getType()))
                {
                    log(Errors::Semantics::CANNOT_ASSIGN_TO_ARRAY_TYPE_N.args(*lhsValue, m_sourceInterface, *lhsValue,
                                                                              *token));
                }
                doAssignmentLikeConstraints(
                    lhsValue->getType(), rhsValue,
                    [&, token = token]
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue));
                    },
                    [&, token = token]
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE
                                .args(*rhsValue, m_sourceInterface, *token, *rhsValue));
                    },
                    [&, token = token] {
                        log(Errors::Semantics::CANNOT_ASSIGN_TO_INCOMPLETE_TYPE_N.args(*lhsValue, m_sourceInterface,
                                                                                       *lhsValue, *token));
                    },
                    [&, token = token]
                    {
                        log(Errors::Semantics::CANNOT_ASSIGN_INCOMPATIBLE_TYPES.args(*lhsValue, m_sourceInterface,
                                                                                     *lhsValue, *token, *rhsValue));
                    },
                    [&, token = token]
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL_2.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue));
                    },
                    [&, token = token](const ConstValue& constant)
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue, constant));
                    },
                    [&, token = token]
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue));
                    },
                    [&, token = token]
                    {
                        if (isVoid(getPointerElementType(lhsValue->getType())))
                        {
                            log(Errors::Semantics::CANNOT_ASSIGN_FUNCTION_POINTER_TO_VOID_POINTER.args(
                                *lhsValue, m_sourceInterface, *lhsValue, *token, *rhsValue));
                        }
                        else
                        {
                            log(Errors::Semantics::CANNOT_ASSIGN_VOID_POINTER_TO_FUNCTION_POINTER.args(
                                *lhsValue, m_sourceInterface, *lhsValue, *token, *rhsValue));
                        }
                    });
                break;
            }
            case Syntax::AssignmentExpression::PlusAssign:
            case Syntax::AssignmentExpression::MinusAssign:
            {
                if (!lhsValue->isUndefined() && !isScalar(lhsValue->getType()) && !lhsValue->getType().is<VectorType>())
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                        *lhsValue, m_sourceInterface, *token, *lhsValue));
                }
                if (lhsValue->getType().is<PointerType>())
                {
                    auto& elementType = getPointerElementType(lhsValue->getType());
                    if (elementType.is<FunctionType>())
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            *lhsValue, m_sourceInterface, *lhsValue));
                    }
                    else if (!isCompleteType(elementType))
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            *lhsValue, m_sourceInterface, elementType, *lhsValue, lhsValue->getType()));
                    }
                    if (!rhsValue->getType().isUndefined() && !isInteger(rhsValue->getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue));
                    }
                }
                else if (isArithmetic(lhsValue->getType()))
                {
                    if (!rhsValue->getType().isUndefined() && !isArithmetic(rhsValue->getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue));
                    }
                    else if (isArithmetic(rhsValue->getType()))
                    {
                        arithmeticConversion(&lhsType, rhsValue);
                    }
                }
                else if (lhsValue->getType().is<VectorType>())
                {
                    arithmeticConversion(&lhsType, rhsValue);
                    checkVectorCompoundAssign(lhsValue, lhsType, token, rhsValue);
                }
                break;
            }
                // Doing a simple check is redundant in compound assignments. Either we had a diagnostic saying that at
                // least one operand does not fit the constraints and we don't want further noise or
                // they're both arithmetic types and we already know they can only be compatible
            case Syntax::AssignmentExpression::DivideAssign:
            case Syntax::AssignmentExpression::MultiplyAssign:
            {
                arithmeticConversion(&lhsType, rhsValue);
                if (!lhsType->isUndefined() && !isArithmetic(lhsType) && !lhsType->is<VectorType>())
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        *lhsValue, m_sourceInterface, *token, *lhsValue));
                }
                if (!rhsValue->getType().isUndefined() && !isArithmetic(rhsValue->getType())
                    && !rhsValue->getType().is<VectorType>())
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue));
                }
                if (lhsType->is<VectorType>() || rhsValue->getType().is<VectorType>())
                {
                    checkVectorCompoundAssign(lhsValue, lhsType, token, rhsValue);
                }
                break;
            }
            case Syntax::AssignmentExpression::BitAndAssign:
            case Syntax::AssignmentExpression::BitOrAssign:
            case Syntax::AssignmentExpression::BitXorAssign:
            case Syntax::AssignmentExpression::ModuloAssign:
            {
                arithmeticConversion(&lhsType, rhsValue);
                if (!lhsType->isUndefined() && !isInteger(lhsType)
                    && (!lhsType->is<VectorType>() || !isInteger(getVectorElementType(lhsType))))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        *lhsValue, m_sourceInterface, *token, *lhsValue));
                }
                if (!rhsValue->isUndefined() && !isInteger(rhsValue->getType())
                    && (!rhsValue->getType().is<VectorType>() || !isInteger(getVectorElementType(rhsValue->getType()))))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue));
                }
                if (lhsType->is<VectorType>() || rhsValue->getType().is<VectorType>())
                {
                    checkVectorCompoundAssign(lhsValue, lhsType, token, rhsValue);
                }
                break;
            }
            case Syntax::AssignmentExpression::LeftShiftAssign:
            case Syntax::AssignmentExpression::RightShiftAssign:
            {
                if (lhsType->is<VectorType>() || rhsValue->getType().is<VectorType>())
                {
                    arithmeticConversion(&lhsType, rhsValue);
                }
                else
                {
                    lhsType = integerPromotion(std::move(lhsType));
                    rhsValue = integerPromotion(std::move(rhsValue));
                }
                if (!lhsType->isUndefined() && !isInteger(lhsType)
                    && (!lhsType->is<VectorType>() || !isInteger(getVectorElementType(lhsType))))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        *lhsValue, m_sourceInterface, *token, *lhsValue));
                }
                if (!rhsValue->isUndefined() && !isInteger(rhsValue->getType())
                    && (!rhsValue->getType().is<VectorType>() || !isInteger(getVectorElementType(rhsValue->getType()))))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue));
                }
                if (lhsType->is<VectorType>() || rhsValue->getType().is<VectorType>())
                {
                    checkVectorCompoundAssign(lhsValue, lhsType, token, rhsValue);
                }
                break;
            }
        }
        rhsValue = std::make_unique<Assignment>(
            std::move(resultType), std::move(lhsValue), std::move(lhsType),
            [kind = kind]
            {
                switch (kind)
                {
                    case Syntax::AssignmentExpression::NoOperator: return Assignment::Simple;
                    case Syntax::AssignmentExpression::PlusAssign: return Assignment::Plus;
                    case Syntax::AssignmentExpression::MinusAssign: return Assignment::Minus;
                    case Syntax::AssignmentExpression::DivideAssign: return Assignment::Divide;
                    case Syntax::AssignmentExpression::MultiplyAssign: return Assignment::Multiply;
                    case Syntax::AssignmentExpression::ModuloAssign: return Assignment::Modulo;
                    case Syntax::AssignmentExpression::LeftShiftAssign: return Assignment::LeftShift;
                    case Syntax::AssignmentExpression::RightShiftAssign: return Assignment::RightShift;
                    case Syntax::AssignmentExpression::BitAndAssign: return Assignment::BitAnd;
                    case Syntax::AssignmentExpression::BitOrAssign: return Assignment::BitOr;
                    case Syntax::AssignmentExpression::BitXorAssign: return Assignment::BitXor;
                }
                CLD_UNREACHABLE;
            }(),
            token, std::move(rhsValue));
    }
    return rhsValue;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpression& node)
{
    return cld::match(node, [&](auto&& value) -> IntrVarPtr<ExpressionBase> { return visit(value); });
}

std::unique_ptr<cld::Semantics::Constant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionConstant& node)
{
    const auto& options = getLanguageOptions();
    if (std::holds_alternative<llvm::APSInt>(node.getValue()) || std::holds_alternative<llvm::APFloat>(node.getValue()))
    {
        switch (node.getType())
        {
            case Lexer::CToken::Type::None: CLD_UNREACHABLE;
            case Lexer::CToken::Type::UnsignedShort:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::UnsignedShort, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::Int:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::Int, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::UnsignedInt:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::UnsignedInt, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::Long:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::Long, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::UnsignedLong:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::UnsignedLong, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::LongLong:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::LongLong, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::UnsignedLongLong:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::UnsignedLongLong, options),
                                                  node.getValue(), node.begin(), node.end());
            case Lexer::CToken::Type::Float:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::Float, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::Double:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::Double, options), node.getValue(),
                                                  node.begin(), node.end());
            case Lexer::CToken::Type::LongDouble:
                return std::make_unique<Constant>(PrimitiveType(PrimitiveType::LongDouble, options), node.getValue(),
                                                  node.begin(), node.end());
        }
    }
    else if (std::holds_alternative<std::string>(node.getValue()))
    {
        auto& string = cld::get<std::string>(node.getValue());
        return std::make_unique<Constant>(
            ArrayType(typeAlloc<PrimitiveType>(PrimitiveType::Char, options), string.size() + 1), node.getValue(),
            node.begin(), node.end());
    }
    else if (std::holds_alternative<Lexer::NonCharString>(node.getValue()))
    {
        auto& string = cld::get<Lexer::NonCharString>(node.getValue());
        if (string.type == Lexer::NonCharString::Wide)
        {
            return std::make_unique<Constant>(
                ArrayType(typeAlloc<PrimitiveType>(options.wcharUnderlyingType, options), string.characters.size() + 1),
                node.getValue(), node.begin(), node.end());
        }
    }
    CLD_UNREACHABLE;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionIdentifier& node)
{
    auto* result = lookupDecl(node.getIdentifier()->getText());
    if (!result || std::holds_alternative<TypedefInfo*>(*result))
    {
        log(Errors::Semantics::UNDECLARED_IDENTIFIER_N.args(*node.getIdentifier(), m_sourceInterface,
                                                            *node.getIdentifier()));
        return std::make_unique<ErrorExpression>(node);
    }
    if (auto* value = std::get_if<std::pair<ConstValue, IntrVarValue<Type>>>(result))
    {
        if (value->second->isUndefined())
        {
            return std::make_unique<ErrorExpression>(node);
        }
        if (value->first.isUndefined())
        {
            return std::make_unique<ErrorExpression>(value->second, node);
        }
        return std::make_unique<Constant>(value->second,
                                          cld::match(value->first.getValue(),
                                                     [](auto&& value) -> Constant::Variant
                                                     {
                                                         using T = std::decay_t<decltype(value)>;
                                                         if constexpr (std::is_constructible_v<Constant::Variant, T>)
                                                         {
                                                             return value;
                                                         }
                                                         CLD_UNREACHABLE;
                                                     }),
                                          node.getIdentifier(), node.getIdentifier() + 1);
    }
    auto& type = cld::match(
        *result, [](const std::pair<ConstValue, IntrVarValue<Type>>&) -> const Type& { CLD_UNREACHABLE; },
        [](const TypedefInfo*) -> const Type& { CLD_UNREACHABLE; },
        [](const BuiltinFunction* builtinFunction) -> const Type& { return builtinFunction->getType(); },
        [&](const auto* ptr) -> const Type&
        {
            if (getCurrentFunctionScope() && getCurrentFunctionScope()->currentFunction->getFunctionGroup().isInline()
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
    auto& useable = cld::match(*result,
                               [](auto&& value) -> Useable&
                               {
                                   using T = std::remove_pointer_t<std::decay_t<decltype(value)>>;
                                   if constexpr (std::is_base_of_v<Useable, T>)
                                   {
                                       return *value;
                                   }
                                   CLD_UNREACHABLE;
                               });
    useable.incrementUsage();
    if (auto* deprecatedAttribute = useable.match(
            [](const auto& value) -> const DeprecatedAttribute* {
                return value.template getAttributeIf<DeprecatedAttribute>();
            },
            [](const BuiltinFunction&) -> const DeprecatedAttribute* { return nullptr; }))
    {
        auto* nameToken = useable.match([](const auto& value) -> Lexer::CTokenIterator { return value.getNameToken(); },
                                        [](const BuiltinFunction&) -> Lexer::CTokenIterator { CLD_UNREACHABLE; });
        if (deprecatedAttribute->optionalMessage)
        {
            if (useable.is<FunctionDeclaration>() || useable.is<FunctionDefinition>())
            {
                if (log(Warnings::Semantics::FUNCTION_N_IS_DEPRECATED_N.args(*node.getIdentifier(), m_sourceInterface,
                                                                             *node.getIdentifier(),
                                                                             *deprecatedAttribute->optionalMessage)))
                {
                    log(Notes::Semantics::MARKED_DEPRECATED_HERE.args(*nameToken, m_sourceInterface, *nameToken));
                }
            }
            else if (useable.is<VariableDeclaration>())
            {
                if (log(Warnings::Semantics::VARIABLE_N_IS_DEPRECATED_N.args(*node.getIdentifier(), m_sourceInterface,
                                                                             *node.getIdentifier(),
                                                                             *deprecatedAttribute->optionalMessage)))
                {
                    log(Notes::Semantics::MARKED_DEPRECATED_HERE.args(*nameToken, m_sourceInterface, *nameToken));
                }
            }
        }
        else
        {
            if (useable.is<FunctionDeclaration>() || useable.is<FunctionDefinition>())
            {
                if (log(Warnings::Semantics::FUNCTION_N_IS_DEPRECATED.args(*node.getIdentifier(), m_sourceInterface,
                                                                           *node.getIdentifier())))
                {
                    log(Notes::Semantics::MARKED_DEPRECATED_HERE.args(*nameToken, m_sourceInterface, *nameToken));
                }
            }
            else if (useable.is<VariableDeclaration>())
            {
                if (log(Warnings::Semantics::VARIABLE_N_IS_DEPRECATED.args(*node.getIdentifier(), m_sourceInterface,
                                                                           *node.getIdentifier())))
                {
                    log(Notes::Semantics::MARKED_DEPRECATED_HERE.args(*nameToken, m_sourceInterface, *nameToken));
                }
            }
        }
    }
    return std::make_unique<DeclarationRead>(type, useable, node.getIdentifier());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionParentheses& node)
{
    return visit(node.getExpression());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionBuiltinVAArg& vaArg)
{
    auto expression = visit(vaArg.getAssignmentExpression());
    auto [isConst, isVolatile] = std::pair{expression->getType().isConst(), expression->getType().isVolatile()};
    expression = lvalueConversion(std::move(expression));
    auto& vaList = *getTypedef("__builtin_va_list");
    if (!expression->getType().isUndefined()
        && (!typesAreCompatible(expression->getType(), *adjustParameterType(vaList.type)) || isConst || isVolatile))
    {
        log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST.args(
            *expression, m_sourceInterface, 1, *expression));
    }
    auto type =
        declaratorsToType(vaArg.getTypeName().getSpecifierQualifiers(), vaArg.getTypeName().getAbstractDeclarator());
    if (!isCompleteType(type))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_VA_ARG.args(vaArg.getTypeName(), m_sourceInterface, type,
                                                                vaArg.getTypeName()));
        return std::make_unique<ErrorExpression>(vaArg);
    }
    type = removeQualifiers(type);
    return std::make_unique<BuiltinVAArg>(std::move(type), vaArg.getBuiltinToken(), vaArg.getOpenParentheses(),
                                          std::move(expression), vaArg.getCloseParentheses());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PrimaryExpressionBuiltinOffsetOf& node)
{
    auto retType = PrimitiveType(getLanguageOptions().sizeTType, getLanguageOptions());
    auto type =
        declaratorsToType(node.getTypeName().getSpecifierQualifiers(), node.getTypeName().getAbstractDeclarator());
    if (type->isUndefined() || !isRecord(type))
    {
        if (!isRecord(type))
        {
            log(Errors::Semantics::TYPE_N_IN_OFFSETOF_MUST_BE_A_STRUCT_OR_UNION_TYPE.args(
                node.getTypeName(), m_sourceInterface, type, node.getTypeName()));
        }
        return std::make_unique<ErrorExpression>(std::move(retType), node);
    }
    auto& fields = getFields(type);
    auto result = fields.find(node.getMemberName()->getText());
    if (result == fields.end())
    {
        reportNoMember(type, *node.getMemberName());
        return std::make_unique<ErrorExpression>(std::move(retType), node);
    }
    if (result->second.bitFieldBounds)
    {
        log(Errors::Semantics::BITFIELD_NOT_ALLOWED_IN_OFFSET_OF.args(*node.getMemberName(), m_sourceInterface,
                                                                      *node.getMemberName()));
        return std::make_unique<ErrorExpression>(std::move(retType), node);
    }
    tcb::span<const Lexer::CToken> range(node.getMemberName(), node.getMemberName() + 1);
    std::uint64_t currentOffset = 0;

    const Type* currentType = type.data();
    for (auto& iter : result->second.indices)
    {
        if (currentType->is<UnionType>())
        {
            auto fieldLayout = getFieldLayout(*currentType);
            currentType = fieldLayout[iter].type;
            continue;
        }
        auto memLayout = getMemoryLayout(*currentType);
        currentOffset += memLayout[iter].offset;
        currentType = memLayout[iter].type;
    }
    bool runtimeEvaluated = false;
    std::vector<std::variant<IntrVarPtr<ExpressionBase>, const Field*>> runtimeVariant;
    runtimeVariant.emplace_back(&result->second);
    std::optional<diag::PointRange> failedConstExpr;
    for (auto& iter : node.getMemberSuffix())
    {
        if (std::holds_alternative<Lexer::CTokenIterator>(iter))
        {
            if (!isRecord(*currentType))
            {
                log(Errors::Semantics::EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_DOT_OPERATOR_2.args(
                    range, m_sourceInterface, range, *currentType));
                return std::make_unique<ErrorExpression>(std::move(retType), node);
            }
            auto& subFields = getFields(*currentType);
            auto subResult = subFields.find(cld::get<Lexer::CTokenIterator>(iter)->getText());
            if (subResult == subFields.end())
            {
                reportNoMember(*currentType, *cld::get<Lexer::CTokenIterator>(iter));
                return std::make_unique<ErrorExpression>(std::move(retType), node);
            }
            if (subResult->second.bitFieldBounds)
            {
                log(Errors::Semantics::BITFIELD_NOT_ALLOWED_IN_OFFSET_OF.args(
                    *cld::get<Lexer::CTokenIterator>(iter), m_sourceInterface, *cld::get<Lexer::CTokenIterator>(iter)));
                return std::make_unique<ErrorExpression>(std::move(retType), node);
            }
            runtimeVariant.emplace_back(&subResult->second);
            range = tcb::span(node.getMemberName(), cld::get<Lexer::CTokenIterator>(iter) + 1);
            for (auto& index : subResult->second.indices)
            {
                if (currentType->is<UnionType>())
                {
                    auto fieldLayout = getFieldLayout(*currentType);
                    currentType = fieldLayout[index].type;
                    continue;
                }
                auto memLayout = getMemoryLayout(*currentType);
                currentOffset += memLayout[index].offset;
                currentType = memLayout[index].type;
            }
        }
        else
        {
            if (!isArray(*currentType))
            {
                log(Errors::Semantics::EXPECTED_ARRAY_TYPE_ON_THE_LEFT_SIDE_OF_THE_SUBSCRIPT_OPERATOR.args(
                    range, m_sourceInterface, range, *currentType));
                return std::make_unique<ErrorExpression>(std::move(retType), node);
            }
            currentType = &getArrayElementType(*currentType);
            auto& subscript = cld::get<Syntax::PrimaryExpressionBuiltinOffsetOf::Subscript>(iter);
            cld::ScopeExit exit{[&] { range = tcb::span(node.getMemberName(), subscript.closeBracket + 1); }};
            auto expression = lvalueConversion(visit(*subscript.expression));
            if (expression->isUndefined())
            {
                return std::make_unique<ErrorExpression>(std::move(retType), node);
            }
            if (!isInteger(expression->getType()))
            {
                log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(*expression, m_sourceInterface,
                                                                                         *expression));
                return std::make_unique<ErrorExpression>(std::move(retType), node);
            }
            if (runtimeEvaluated)
            {
                runtimeVariant.emplace_back(std::move(expression));
                continue;
            }
            auto constant = evaluateConstantExpression(*expression);
            runtimeVariant.emplace_back(std::move(expression));
            if (!constant)
            {
                failedConstExpr = diag::getPointRange(*cld::get<IntrVarPtr<ExpressionBase>>(runtimeVariant.back()));
                runtimeEvaluated = true;
                continue;
            }
            auto size = currentType->getSizeOf(*this);
            if (constant->getInteger().isSigned())
            {
                currentOffset += size * constant->getInteger().getSExtValue();
            }
            else
            {
                currentOffset += size * constant->getInteger().getZExtValue();
            }
        }
    }
    if (runtimeEvaluated)
    {
        return std::make_unique<BuiltinOffsetOf>(
            getLanguageOptions(), node.getBuiltinToken(), node.getOpenParentheses(),
            BuiltinOffsetOf::RuntimeEval{std::move(type), std::move(runtimeVariant), std::move(failedConstExpr)},
            node.getCloseParentheses());
    }
    return std::make_unique<BuiltinOffsetOf>(getLanguageOptions(), node.getBuiltinToken(), node.getOpenParentheses(),
                                             currentOffset, node.getCloseParentheses());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpression& node)
{
    return cld::match(node, [&](auto&& value) { return visit(value); });
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionSubscript& node)
{
    auto first = lvalueConversion(visit(node.getPostFixExpression()));
    auto second = lvalueConversion(visit(node.getExpression()));
    if (first->getType().isUndefined() || second->getType().isUndefined())
    {
        return std::make_unique<ErrorExpression>(node);
    }
    if (!first->getType().is<PointerType>() && !second->getType().is<PointerType>()
        && !first->getType().is<VectorType>() && !second->getType().is<VectorType>())
    {
        log(Errors::Semantics::EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE.args(node.getPostFixExpression(),
                                                                               m_sourceInterface, *first, *second));
        return std::make_unique<ErrorExpression>(node);
    }
    if (first->getType().is<VectorType>() || second->getType().is<VectorType>())
    {
        auto& vectorExpr = first->getType().is<VectorType>() ? first : second;
        auto& intExpr = &vectorExpr == &first ? second : first;
        if (!isInteger(intExpr->getType()))
        {
            log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(*intExpr, m_sourceInterface,
                                                                                     *intExpr));
            return std::make_unique<ErrorExpression>(node);
        }
        IntrVarValue elementType = getVectorElementType(vectorExpr->getType());
        return std::make_unique<SubscriptOperator>(std::move(elementType), &vectorExpr == &first, std::move(first),
                                                   node.getOpenBracket(), std::move(second), node.getCloseBracket());
    }

    auto& pointerExpr = first->getType().is<PointerType>() ? first : second;
    auto& intExpr = &pointerExpr == &first ? second : first;
    if (!isInteger(intExpr->getType()))
    {
        log(Errors::Semantics::EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE.args(*intExpr, m_sourceInterface,
                                                                                 *intExpr));
        return std::make_unique<ErrorExpression>(node);
    }
    IntrVarValue elementType = getPointerElementType(pointerExpr->getType());
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            *pointerExpr, m_sourceInterface, elementType, *pointerExpr));
        return std::make_unique<ErrorExpression>(node);
    }
    if (elementType->is<FunctionType>())
    {
        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR.args(
            *pointerExpr, m_sourceInterface, *pointerExpr));
        return std::make_unique<ErrorExpression>(node);
    }
    return std::make_unique<SubscriptOperator>(std::move(elementType), &pointerExpr == &first, std::move(first),
                                               node.getOpenBracket(), std::move(second), node.getCloseBracket());
}

void cld::Semantics::SemanticAnalysis::reportNoMember(const Type& recordType, const Lexer::CToken& identifier)
{
    if (recordType.is<UnionType>())
    {
        if (isAnonymous(recordType))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(identifier, m_sourceInterface,
                                                                                    identifier));
        }
        else
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(identifier, m_sourceInterface, identifier,
                                                                            recordType.as<UnionType>().getUnionName()));
        }
    }
    if (recordType.is<StructType>())
    {
        if (isAnonymous(recordType))
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(identifier, m_sourceInterface,
                                                                                     identifier));
        }
        else
        {
            log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                identifier, m_sourceInterface, identifier, recordType.as<StructType>().getStructName()));
        }
    }
}

std::optional<std::pair<cld::IntrVarValue<cld::Semantics::Type>, const cld::Semantics::Field * CLD_NON_NULL>>
    cld::Semantics::SemanticAnalysis::checkMemberAccess(const Type& recordType,
                                                        const Syntax::PostFixExpression& postFixExpr,
                                                        const Lexer::CToken& identifier)
{
    const FieldMap* fields = nullptr;
    if (auto* structType = recordType.tryAs<StructType>())
    {
        auto* structDef = std::get_if<StructDefinition>(&structType->getInfo().type);
        if (!structDef)
        {
            log(Errors::Semantics::STRUCT_N_IS_AN_INCOMPLETE_TYPE.args(postFixExpr, m_sourceInterface,
                                                                       structType->getStructName(), postFixExpr));
            return {};
        }
        fields = &structDef->getFields();
    }
    if (auto* unionType = recordType.tryAs<UnionType>())
    {
        auto* unionDef = std::get_if<UnionDefinition>(&unionType->getInfo().type);
        if (!unionDef)
        {
            log(Errors::Semantics::UNION_N_IS_AN_INCOMPLETE_TYPE.args(postFixExpr, m_sourceInterface,
                                                                      unionType->getUnionName(), postFixExpr));
            return {};
        }
        fields = &unionDef->getFields();
    }
    CLD_ASSERT(fields);
    auto result = fields->find(identifier.getText());
    if (result == fields->end())
    {
        reportNoMember(recordType, identifier);
        return {};
    }
    IntrVarValue type = *result->second.type;
    if (std::pair{recordType.isConst(), recordType.isVolatile()}
        > std::pair{result->second.type->isConst(), result->second.type->isVolatile()})
    {
        type->setConst(recordType.isConst());
        type->setVolatile(recordType.isVolatile());
    }
    return std::pair(std::move(type), &result->second);
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDot& node)
{
    auto structOrUnion = visit(node.getPostFixExpression());
    if (structOrUnion->isUndefined())
    {
        return std::make_unique<ErrorExpression>(node);
    }
    if (!isRecord(structOrUnion->getType()))
    {
        log(Errors::Semantics::EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_DOT_OPERATOR.args(
            *structOrUnion, m_sourceInterface, *structOrUnion));
        return std::make_unique<ErrorExpression>(node);
    }
    auto result = checkMemberAccess(structOrUnion->getType(), node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return std::make_unique<ErrorExpression>(node);
    }
    auto& [type, field] = *result;
    if (type->isUndefined())
    {
        return std::make_unique<ErrorExpression>(node);
    }
    auto category = structOrUnion->getValueCategory();
    return std::make_unique<MemberAccess>(type, category, std::move(structOrUnion), *field, node.getIdentifier());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionArrow& node)
{
    auto structOrUnionPtr = lvalueConversion(visit(node.getPostFixExpression()));
    if (structOrUnionPtr->isUndefined())
    {
        return std::make_unique<ErrorExpression>(node);
    }
    if (!structOrUnionPtr->getType().is<PointerType>())
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_ARROW_OPERATOR.args(
            *structOrUnionPtr, m_sourceInterface, *structOrUnionPtr));
        return std::make_unique<ErrorExpression>(node);
    }
    auto& structOrUnion = getPointerElementType(structOrUnionPtr->getType());
    if (!isRecord(structOrUnion))
    {
        log(Errors::Semantics::EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_ARROW_OPERATOR.args(
            *structOrUnionPtr, m_sourceInterface, *structOrUnionPtr));
        return std::make_unique<ErrorExpression>(node);
    }
    auto result = checkMemberAccess(structOrUnion, node.getPostFixExpression(), *node.getIdentifier());
    if (!result)
    {
        return std::make_unique<ErrorExpression>(node);
    }
    auto& [type, field] = *result;
    if (type->isUndefined())
    {
        return std::make_unique<ErrorExpression>(node);
    }
    return std::make_unique<MemberAccess>(type, ValueCategory::Lvalue, std::move(structOrUnionPtr), *field,
                                          node.getIdentifier());
}

namespace
{
bool isBuiltinKind(const cld::Semantics::ExpressionBase& expression, cld::Semantics::BuiltinFunction::Kind kind)
{
    auto* declRead = expression.tryAs<cld::Semantics::DeclarationRead>();
    if (!declRead)
    {
        return false;
    }
    auto* builtin = declRead->getDeclRead().tryAs<cld::Semantics::BuiltinFunction>();
    if (!builtin)
    {
        return false;
    }
    return builtin->getKind() == kind;
}

bool isSyncBuiltinWithType(const cld::Semantics::ExpressionBase& expression)
{
    auto* declRead = expression.tryAs<cld::Semantics::DeclarationRead>();
    if (!declRead)
    {
        return false;
    }
    auto* builtin = declRead->getDeclRead().tryAs<cld::Semantics::BuiltinFunction>();
    if (!builtin)
    {
        return false;
    }
    switch (builtin->getKind())
    {
        case cld::Semantics::BuiltinFunction::SyncFetchAndAdd:
        case cld::Semantics::BuiltinFunction::SyncFetchAndSub:
        case cld::Semantics::BuiltinFunction::SyncFetchAndOr:
        case cld::Semantics::BuiltinFunction::SyncFetchAndAnd:
        case cld::Semantics::BuiltinFunction::SyncFetchAndXor:
        case cld::Semantics::BuiltinFunction::SyncFetchAndNand:
        case cld::Semantics::BuiltinFunction::SyncAddAndFetch:
        case cld::Semantics::BuiltinFunction::SyncSubAndFetch:
        case cld::Semantics::BuiltinFunction::SyncOrAndFetch:
        case cld::Semantics::BuiltinFunction::SyncAndAndFetch:
        case cld::Semantics::BuiltinFunction::SyncXorAndFetch:
        case cld::Semantics::BuiltinFunction::SyncNandAndFetch:
        case cld::Semantics::BuiltinFunction::SyncBoolCompareAndSwap:
        case cld::Semantics::BuiltinFunction::SyncValCompareAndSwap:
        case cld::Semantics::BuiltinFunction::SyncLockTestAndSet:
        case cld::Semantics::BuiltinFunction::SyncLockRelease: return true;
        default: return false;
    }
}

bool isBuiltinFunction(const cld::Semantics::ExpressionBase& expression)
{
    auto* declRead = expression.tryAs<cld::Semantics::DeclarationRead>();
    if (!declRead)
    {
        return false;
    }
    return declRead->getDeclRead().is<cld::Semantics::BuiltinFunction>();
}
} // namespace

std::unique_ptr<cld::Semantics::CallExpression>
    cld::Semantics::SemanticAnalysis::visitVAStart(const Syntax::PostFixExpressionFunctionCall& node,
                                                   IntrVarPtr<ExpressionBase>&& function)
{
    std::vector<IntrVarPtr<ExpressionBase>> arguments;
    if (node.getOptionalAssignmentExpressions().size() < 2)
    {
        log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N.args(
            *function, m_sourceInterface, 2, node.getOptionalAssignmentExpressions().size(), *function));
    }
    else if (node.getOptionalAssignmentExpressions().size() > 2)
    {
        log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N.args(
            *function, m_sourceInterface, 2, node.getOptionalAssignmentExpressions().size(), *function,
            tcb::span(node.getOptionalAssignmentExpressions()).subspan(2)));
    }
    else
    {
        arguments.push_back(visit(node.getOptionalAssignmentExpressions()[0]));
        auto& vaList = *getTypedef("__builtin_va_list");
        if (!arguments[0]->getType().isUndefined() && !typesAreCompatible(arguments[0]->getType(), vaList.type))
        {
            log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST.args(
                *arguments[0], m_sourceInterface, 1, *arguments[0]));
        }
        // even if the call is done using "__builtin_va_start(list,...)" the list is actually passed as a pointer.
        // In the case of x86_64 ABI __builtin_va_list is an array type causing pointer decay, in the case
        // of Windows x64 it's basically as if &list was written. Since this is a builtin call the backend will
        // have to do special handling either way but we'll insert an lvalueConversion now to force array to pointer
        // decay
        if (isArray(vaList.type))
        {
            arguments.back() = lvalueConversion(std::move(arguments.back()));
        }
        auto expression = visit(node.getOptionalAssignmentExpressions()[1]);
        if (!getCurrentFunctionScope())
        {
            log(Errors::Semantics::CANNOT_USE_VA_START_OUTSIDE_OF_A_FUNCTION.args(*function, m_sourceInterface,
                                                                                  *function));
        }
        else if (!getCurrentFunctionScope()->currentFunction->getType().isLastVararg())
        {
            log(Errors::Semantics::CANNOT_USE_VA_START_IN_A_FUNCTION_WITH_FIXED_ARGUMENT_COUNT.args(
                *function, m_sourceInterface, *function));
        }
        else if (auto* declRead = expression->tryAs<DeclarationRead>();
                 !declRead
                 || (!getCurrentFunctionScope()->currentFunction->getParameterDeclarations().empty()
                     && &declRead->getDeclRead()
                            != getCurrentFunctionScope()->currentFunction->getParameterDeclarations().back().get()))
        {
            log(Warnings::Semantics::SECOND_ARGUMENT_OF_VA_START_SHOULD_BE_THE_LAST_PARAMETER.args(
                *expression, m_sourceInterface, *expression));
        }
        arguments.push_back(lvalueConversion(std::move(expression)));
    }
    auto* functionType = &function->getType();
    return std::make_unique<CallExpression>(
        PrimitiveType(PrimitiveType::Void, getLanguageOptions()),
        std::make_unique<Conversion>(PointerType(functionType), Conversion::LValue, std::move(function)),
        node.getOpenParentheses(), std::move(arguments), node.getCloseParentheses());
}

std::unique_ptr<cld::Semantics::CallExpression>
    cld::Semantics::SemanticAnalysis::visitPrefetch(const Syntax::PostFixExpressionFunctionCall& node,
                                                    IntrVarPtr<ExpressionBase>&& function)
{
    std::vector<IntrVarPtr<ExpressionBase>> arguments;
    auto& ft = function->getType().as<FunctionType>();
    if (node.getOptionalAssignmentExpressions().size() == 0)
    {
        auto& decl = function->as<DeclarationRead>();
        log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_AT_LEAST_N_GOT_N.args(
            decl, m_sourceInterface, *decl.getIdentifierToken(), 1, 0));
    }
    else if (node.getOptionalAssignmentExpressions().size() > 3)
    {
        auto& decl = function->as<DeclarationRead>();
        log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
            decl, m_sourceInterface, *decl.getIdentifierToken(), 3, node.getOptionalAssignmentExpressions().size(),
            tcb::span(node.getOptionalAssignmentExpressions()).subspan(3)));
    }
    else
    {
        arguments.push_back(
            checkFunctionArg(1, *ft.getParameters()[0].type, visit(node.getOptionalAssignmentExpressions()[0])));
        if (node.getOptionalAssignmentExpressions().size() > 1)
        {
            auto expression = visit(node.getOptionalAssignmentExpressions()[1]);
            if (!expression->isUndefined())
            {
                auto constant = evaluateConstantExpression(*expression);
                if (!constant || !isInteger(expression->getType()))
                {
                    log(Errors::Semantics::EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_SECOND_ARGUMENT_TO_BUILTIN_PREFETCH
                            .args(*expression, m_sourceInterface, *expression));
                }
                else if (constant->getInteger() != 0 && constant->getInteger() != 1)
                {
                    log(Errors::Semantics::EXPECTED_A_VALUE_OF_0_OR_1_AS_SECOND_ARGUMENT_TO_BUILTIN_PREFETCH.args(
                        *expression, m_sourceInterface, *expression, *constant));
                }
            }
            arguments.push_back(std::move(expression));
        }
        if (node.getOptionalAssignmentExpressions().size() > 2)
        {
            auto expression = visit(node.getOptionalAssignmentExpressions()[2]);
            if (!expression->isUndefined())
            {
                auto constant = evaluateConstantExpression(*expression);
                if (!constant || !isInteger(expression->getType()))
                {
                    log(Errors::Semantics::EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_THIRD_ARGUMENT_TO_BUILTIN_PREFETCH
                            .args(*expression, m_sourceInterface, *expression));
                }
                else if (constant->getInteger() < 0 || constant->getInteger() > 3)
                {
                    log(Errors::Semantics::EXPECTED_A_VALUE_OF_0_TO_3_AS_THIRD_ARGUMENT_TO_BUILTIN_PREFETCH.args(
                        *expression, m_sourceInterface, *expression, *constant));
                }
            }
            arguments.push_back(std::move(expression));
        }
    }
    return std::make_unique<CallExpression>(
        PrimitiveType(PrimitiveType::Void, getLanguageOptions()),
        std::make_unique<Conversion>(PointerType(&ft), Conversion::LValue, std::move(function)),
        node.getOpenParentheses(), std::move(arguments), node.getCloseParentheses());
}

std::unique_ptr<cld::Semantics::CallExpression>
    cld::Semantics::SemanticAnalysis::visitExpectWithProbability(const Syntax::PostFixExpressionFunctionCall& node,
                                                                 IntrVarPtr<ExpressionBase>&& function)
{
    std::vector<IntrVarPtr<ExpressionBase>> arguments;
    auto& ft = function->getType().as<FunctionType>();
    if (node.getOptionalAssignmentExpressions().size() < 3)
    {
        auto& decl = function->as<DeclarationRead>();
        log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
            decl, m_sourceInterface, *decl.getIdentifierToken(), 3, node.getOptionalAssignmentExpressions().size()));
    }
    else if (node.getOptionalAssignmentExpressions().size() > 3)
    {
        auto& decl = function->as<DeclarationRead>();
        log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
            decl, m_sourceInterface, *decl.getIdentifierToken(), 3, node.getOptionalAssignmentExpressions().size(),
            tcb::span(node.getOptionalAssignmentExpressions()).subspan(3)));
    }
    else
    {
        arguments.push_back(
            checkFunctionArg(1, *ft.getParameters()[0].type, visit(node.getOptionalAssignmentExpressions()[0])));
        arguments.push_back(
            checkFunctionArg(2, *ft.getParameters()[1].type, visit(node.getOptionalAssignmentExpressions()[1])));
        arguments.push_back(
            checkFunctionArg(3, *ft.getParameters()[2].type, visit(node.getOptionalAssignmentExpressions()[2])));
        if (arguments.back() && !arguments.back()->isUndefined())
        {
            auto constant = evaluateConstantExpression(*arguments.back(), Arithmetic);
            if (!constant || !isArithmetic(arguments.back()->getType()))
            {
                log(Errors::Semantics::
                        EXPECTED_ARITHMETIC_CONSTANT_EXPRESSION_AS_THIRD_ARGUMENT_TO_BUILTIN_EXPECT_WITH_PROBABILITY
                            .args(*arguments.back(), m_sourceInterface, *arguments.back()));
            }
            else if (constant->getFloating() < llvm::APFloat(constant->getFloating().getSemantics(), 0)
                     || constant->getFloating() > llvm::APFloat(constant->getFloating().getSemantics(), 1))
            {
                log(Errors::Semantics::EXPECTED_A_VALUE_OF_0_TO_1_AS_THIRD_ARGUMENT_TO_BUILTIN_EXPECT_WITH_PROBABILITY
                        .args(*arguments.back(), m_sourceInterface, *arguments.back(), *constant));
            }
        }
    }
    return std::make_unique<CallExpression>(
        PrimitiveType(PrimitiveType::Void, getLanguageOptions()),
        std::make_unique<Conversion>(PointerType(&ft), Conversion::LValue, std::move(function)),
        node.getOpenParentheses(), std::move(arguments), node.getCloseParentheses());
}

std::unique_ptr<cld::Semantics::CallExpression>
    cld::Semantics::SemanticAnalysis::visitSyncBuiltinWithT(const Syntax::PostFixExpressionFunctionCall& node,
                                                            IntrVarPtr<ExpressionBase>&& function)
{
    auto& type = function->getType();
    function = std::make_unique<Conversion>(PointerType(&type), Conversion::LValue, std::move(function));
    auto& ft = getPointerElementType(function->getType()).as<FunctionType>();

    IntrVarValue placeholder = ErrorType{};
    std::vector<IntrVarPtr<ExpressionBase>> arguments;
    if (checkFunctionCount(*function, ft, node))
    {
        auto& decl = function->as<Conversion>().getExpression().as<DeclarationRead>();
        arguments.push_back(lvalueConversion(visit(node.getOptionalAssignmentExpressions()[0])));
        if (!arguments.back()->getType().is<PointerType>())
        {
            log(Errors::Semantics::EXPECTED_POINTER_TYPE_AS_FIRST_ARGUMENT_TO_N.args(
                *arguments.back(), m_sourceInterface, *decl.getIdentifierToken(), *arguments.back()));
        }
        else
        {
            auto& elementType = getPointerElementType(arguments.back()->getType());
            if (elementType.isConst())
            {
                log(Errors::Semantics::POINTER_ELEMENT_TYPE_IN_N_MAY_NOT_BE_CONST_QUALIFIED.args(
                    *arguments.back(), m_sourceInterface, *decl.getIdentifierToken(), *arguments.back()));
            }
            if (!isInteger(elementType) && !elementType.is<PointerType>())
            {
                log(Errors::Semantics::POINTER_ELEMENT_TYPE_IN_N_MUST_BE_AN_INTEGER_OR_POINTER_TYPe.args(
                    *arguments.back(), m_sourceInterface, *decl.getIdentifierToken(), *arguments.back()));
            }
            else if (isBool(elementType))
            {
                log(Errors::Semantics::POINTER_ELEMENT_TYPE_IN_N_MUST_NOT_BE_BOOL.args(
                    *arguments.back(), m_sourceInterface, *decl.getIdentifierToken(), *arguments.back()));
            }
            else if (elementType.getSizeOf(*this) > 8)
            {
                log(Errors::Semantics::POINTER_ELEMENT_TYPE_IN_N_MUST_NOT_HAVE_A_SIZE_GREATER_THAN_8.args(
                    *arguments.back(), m_sourceInterface, *decl.getIdentifierToken(), *arguments.back()));
            }
            placeholder = removeQualifiers(elementType);
        }
        std::size_t i = 1;
        for (; i < ft.getParameters().size(); i++)
        {
            if (ft.getParameters()[i].type->isUndefined())
            {
                arguments.push_back(
                    checkFunctionArg(i + 1, placeholder, visit(node.getOptionalAssignmentExpressions()[i])));
            }
            else
            {
                arguments.push_back(checkFunctionArg(i + 1, *ft.getParameters()[i].type,
                                                     visit(node.getOptionalAssignmentExpressions()[i])));
            }
        }
        for (; i < node.getOptionalAssignmentExpressions().size(); i++)
        {
            visit(node.getOptionalAssignmentExpressions()[i]);
        }
    }
    if (ft.getReturnType().isUndefined())
    {
        return std::make_unique<CallExpression>(std::move(placeholder), std::move(function), node.getOpenParentheses(),
                                                std::move(arguments), node.getCloseParentheses());
    }
    return std::make_unique<CallExpression>(ft.getReturnType(), std::move(function), node.getOpenParentheses(),
                                            std::move(arguments), node.getCloseParentheses());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::checkFunctionArg(std::size_t i, IntrVarValue<Type> paramType,
                                                       IntrVarPtr<ExpressionBase>&& expression)
{
    paramType = removeQualifiers(adjustParameterType(paramType));
    if (paramType->isUndefined())
    {
        return {};
    }
    expression = lvalueConversion(std::move(expression));
    if (expression->isUndefined())
    {
        return {std::move(expression)};
    }
    doAssignmentLikeConstraints(
        paramType, expression,
        [&]
        {
            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_TYPE.args(*expression, m_sourceInterface, i,
                                                                                     *expression));
        },
        [&]
        {
            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *expression, m_sourceInterface, i, *expression));
        },
        [&]
        {
            log(Errors::Semantics::CANNOT_PASS_ARGUMENT_TO_INCOMPLETE_TYPE_N_OF_PARAMETER_N.args(
                *expression, m_sourceInterface, paramType, i, *expression));
        },
        [&]
        {
            log(Errors::Semantics::CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_N.args(
                *expression, m_sourceInterface, i, paramType, *expression));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_NULL_2.args(*expression, m_sourceInterface, i,
                                                                         *expression));
        },
        [&](const ConstValue& constant)
        {
            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_NULL.args(*expression, m_sourceInterface, i, *expression,
                                                                       constant));
        },
        [&]
        {
            log(Errors::Semantics::EXPECTED_ARGUMENT_N_TO_BE_A_POINTER_TYPE.args(*expression, m_sourceInterface, i,
                                                                                 *expression));
        },
        [&]
        {
            if (isVoid(getPointerElementType(paramType)))
            {
                log(Errors::Semantics::CANNOT_PASS_FUNCTION_POINTER_TO_VOID_POINTER_PARAMETER.args(
                    *expression, m_sourceInterface, *expression));
            }
            else
            {
                log(Errors::Semantics::CANNOT_PASS_VOID_POINTER_TO_FUNCTION_POINTER_PARAMETER.args(
                    *expression, m_sourceInterface, *expression));
            }
        });
    return {std::move(expression)};
}

bool cld::Semantics::SemanticAnalysis::checkFunctionCount(const ExpressionBase& function, const FunctionType& ft,
                                                          const Syntax::PostFixExpressionFunctionCall& node)
{
    auto callsFunction = [&]
    {
        if (!function.is<Conversion>())
        {
            return false;
        }
        auto& conversion = function.as<Conversion>();
        if (conversion.getKind() != Conversion::LValue)
        {
            return false;
        }
        if (!conversion.getExpression().is<DeclarationRead>())
        {
            return false;
        }
        auto& decl = conversion.getExpression().as<DeclarationRead>();
        return decl.getDeclRead().match(
            [](const FunctionDefinition&) { return true; }, [](const FunctionDeclaration&) { return true; },
            [](const VariableDeclaration&) { return false; }, [](const BuiltinFunction&) { return true; });
    }();
    auto& argumentTypes = ft.getParameters();
    if (node.getOptionalAssignmentExpressions().size() < argumentTypes.size())
    {
        if (!ft.isLastVararg())
        {
            if (callsFunction)
            {
                auto& decl = function.as<Conversion>().getExpression().as<DeclarationRead>();
                log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
                    decl, m_sourceInterface, *decl.getIdentifierToken(), argumentTypes.size(),
                    node.getOptionalAssignmentExpressions().size()));
            }
            else
            {
                log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N.args(
                    function, m_sourceInterface, argumentTypes.size(), node.getOptionalAssignmentExpressions().size(),
                    function));
            }
        }
        else
        {
            if (callsFunction)
            {
                auto& decl = function.as<Conversion>().getExpression().as<DeclarationRead>();
                log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_AT_LEAST_N_GOT_N.args(
                    decl, m_sourceInterface, *decl.getIdentifierToken(), argumentTypes.size(),
                    node.getOptionalAssignmentExpressions().size()));
            }
            else
            {
                log(Errors::Semantics::NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_AT_LEAST_N_GOT_N.args(
                    function, m_sourceInterface, argumentTypes.size(), node.getOptionalAssignmentExpressions().size(),
                    function));
            }
        }
        return false;
    }
    if (!ft.isLastVararg() && node.getOptionalAssignmentExpressions().size() > argumentTypes.size())
    {
        if (callsFunction)
        {
            auto& decl = function.as<Conversion>().getExpression().as<DeclarationRead>();
            log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N.args(
                decl, m_sourceInterface, *decl.getIdentifierToken(), argumentTypes.size(),
                node.getOptionalAssignmentExpressions().size(),
                tcb::span(node.getOptionalAssignmentExpressions()).subspan(argumentTypes.size())));
        }
        else
        {
            log(Errors::Semantics::TOO_MANY_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N.args(
                function, m_sourceInterface, argumentTypes.size(), node.getOptionalAssignmentExpressions().size(),
                function, tcb::span(node.getOptionalAssignmentExpressions()).subspan(argumentTypes.size())));
        }
        return false;
    }
    return true;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionFunctionCall& node)
{
    auto function = visit(node.getPostFixExpression());
    if (isBuiltinKind(*function, BuiltinFunction::VAStart))
    {
        return visitVAStart(node, std::move(function));
    }
    if (isBuiltinKind(*function, BuiltinFunction::Prefetch))
    {
        return visitPrefetch(node, std::move(function));
    }
    if (isBuiltinKind(*function, BuiltinFunction::ExpectWithProbability))
    {
        return visitExpectWithProbability(node, std::move(function));
    }
    if (isSyncBuiltinWithType(*function))
    {
        return visitSyncBuiltinWithT(node, std::move(function));
    }
    if (!isBuiltinFunction(*function))
    {
        function = lvalueConversion(std::move(function));
        if (!function->getType().is<PointerType>() || !getPointerElementType(function->getType()).is<FunctionType>())
        {
            if (!function->getType().isUndefined())
            {
                log(Errors::Semantics::CANNOT_CALL_NON_FUNCTION_TYPE.args(
                    *function, m_sourceInterface, *function, *node.getOpenParentheses(), *node.getCloseParentheses()));
            }
            for (auto& iter : node.getOptionalAssignmentExpressions())
            {
                visit(iter);
            }
            return std::make_unique<ErrorExpression>(node);
        }
    }
    else
    {
        auto& type = function->getType();
        function = std::make_unique<Conversion>(PointerType(&type), Conversion::LValue, std::move(function));
    }
    auto& ft = getPointerElementType(function->getType()).as<FunctionType>();
    std::vector<IntrVarPtr<ExpressionBase>> arguments;
    if (ft.isKandR())
    {
        for (auto& iter : node.getOptionalAssignmentExpressions())
        {
            arguments.push_back(defaultArgumentPromotion(visit(iter)));
        }
    }
    else
    {
        if (checkFunctionCount(*function, ft, node))
        {
            std::size_t i = 0;
            for (; i < ft.getParameters().size(); i++)
            {
                arguments.push_back(checkFunctionArg(i + 1, *ft.getParameters()[i].type,
                                                     visit(node.getOptionalAssignmentExpressions()[i])));
            }
            for (; i < node.getOptionalAssignmentExpressions().size(); i++)
            {
                arguments.push_back(defaultArgumentPromotion(visit(node.getOptionalAssignmentExpressions()[i])));
            }
        }
    }

    return std::make_unique<CallExpression>(ft.getReturnType(), std::move(function), node.getOpenParentheses(),
                                            std::move(arguments), node.getCloseParentheses());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::checkIncrementAndDecrement(const Syntax::Node& node, UnaryOperator::Kind kind,
                                                                 IntrVarPtr<ExpressionBase>&& value,
                                                                 Lexer::CTokenIterator opToken)
{
    if (value->isUndefined())
    {
        return {std::move(value)};
    }
    if (!isScalar(value->getType()))
    {
        log(Errors::Semantics::OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(*value, m_sourceInterface,
                                                                                       *opToken, *value));
        return std::make_unique<ErrorExpression>(node);
    }
    if (value->getValueCategory() != ValueCategory::Lvalue || value->getType().isConst())
    {
        log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST.args(*value, m_sourceInterface,
                                                                                           *opToken, *value));
    }
    if (isArithmetic(value->getType()))
    {
        auto type = removeQualifiers(value->getType());
        return std::make_unique<UnaryOperator>(std::move(type), ValueCategory::Rvalue, kind, opToken, std::move(value));
    }
    auto& elementType = getPointerElementType(value->getType());
    if (!isCompleteType(elementType))
    {
        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(*value, m_sourceInterface, elementType,
                                                                                 *value, value->getType()));
    }
    else if (elementType.is<FunctionType>())
    {
        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
            *value, m_sourceInterface, *value));
        return std::make_unique<ErrorExpression>(node);
    }
    auto type = removeQualifiers(value->getType());
    return std::make_unique<UnaryOperator>(std::move(type), ValueCategory::Rvalue, kind, opToken, std::move(value));
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionIncrement& node)
{
    return checkIncrementAndDecrement(node, UnaryOperator::PostIncrement, visit(node.getPostFixExpression()),
                                      node.getIncrementToken());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionDecrement& node)
{
    return checkIncrementAndDecrement(node, UnaryOperator::PostDecrement, visit(node.getPostFixExpression()),
                                      node.getDecrementToken());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::PostFixExpressionTypeInitializer& node)
{
    std::vector<ParsedAttribute<>> attributes;
    auto type = declaratorsToType(node.getTypeName().getSpecifierQualifiers(),
                                  node.getTypeName().getAbstractDeclarator(), &attributes);
    if (type->isUndefined())
    {
        visit(node.getInitializerList(), type, !inFunction() || m_inStaticInitializer);
        return std::make_unique<ErrorExpression>(node);
    }
    attributes = applyAttributes(std::pair{&type, diag::getPointRange(node.getTypeName())}, std::move(attributes));
    reportNotApplicableAttributes(attributes);
    if (type->is<FunctionType>())
    {
        log(Errors::Semantics::CANNOT_INITIALIZE_FUNCTION_TYPE.args(node.getTypeName(), m_sourceInterface,
                                                                    node.getTypeName(), type));
        visit(node.getInitializerList(), ErrorType{}, !inFunction() || m_inStaticInitializer);
        return std::make_unique<ErrorExpression>(node);
    }
    if (isVariableLengthArray(type))
    {
        log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE.args(node.getTypeName(), m_sourceInterface,
                                                                                 node.getTypeName(), type));
        visit(node.getInitializerList(), ErrorType{}, !inFunction() || m_inStaticInitializer);
        return std::make_unique<ErrorExpression>(node);
    }
    Initializer value{std::make_unique<ErrorExpression>(node)};
    if (type->is<AbstractArrayType>())
    {
        std::size_t size = 0;
        value = visit(node.getInitializerList(), type, !inFunction() || m_inStaticInitializer, &size);
        auto& abstractArrayType = type->as<AbstractArrayType>();
        type = ArrayType(&abstractArrayType.getType(), size, flag::useFlags = type->getFlags());
    }
    else
    {
        value = visit(node.getInitializerList(), type, !inFunction() || m_inStaticInitializer);
    }
    return std::make_unique<CompoundLiteral>(std::move(type), node.getOpenParentheses(), std::move(value),
                                             node.getCloseParentheses(), node.getInitializerList().end(),
                                             m_inStaticInitializer);
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpression& node)
{
    return cld::match(node, [&](auto&& value) -> IntrVarPtr<ExpressionBase> { return visit(value); });
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionPostFixExpression& node)
{
    return visit(node.getPostFixExpression());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionUnaryOperator& node)
{
    auto value = visit(node.getCastExpression());
    if (value->isUndefined())
    {
        return std::make_unique<ErrorExpression>(node);
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
            if (auto* declRead = value->tryAs<DeclarationRead>())
            {
                if (auto* decl = declRead->getDeclRead().tryAs<VariableDeclaration>();
                    decl && decl->getLifetime() == Lifetime::Register)
                {
                    log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_DECLARATION_ANNOTATED_WITH_REGISTER.args(
                        *value, m_sourceInterface, *node.getUnaryToken(), *value));
                }
            }
            if (auto* subscript = value->tryAs<SubscriptOperator>();
                subscript && subscript->getPointerExpression().getType().is<VectorType>())
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_VECTOR_ELEMENT.args(*value, m_sourceInterface,
                                                                                  *node.getUnaryToken(), *value));
            }
            if (isBitfieldAccess(*value))
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_BITFIELD.args(*value, m_sourceInterface,
                                                                            *node.getUnaryToken(), *value));
            }
            if (!value->is<CallExpression>() && value->getValueCategory() != ValueCategory::Lvalue)
            {
                log(Errors::Semantics::CANNOT_TAKE_ADDRESS_OF_TEMPORARY.args(*value, m_sourceInterface,
                                                                             *node.getUnaryToken(), *value));
            }
            auto element = typeAlloc(value->getType());
            return std::make_unique<UnaryOperator>(PointerType(element), ValueCategory::Rvalue,
                                                   UnaryOperator::AddressOf, node.getUnaryToken(), std::move(value));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        {
            value = lvalueConversion(std::move(value));
            if (!value->getType().isUndefined() && !value->getType().is<PointerType>())
            {
                log(Errors::Semantics::CANNOT_DEREFERENCE_NON_POINTER_TYPE_N.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), *value));
                return std::make_unique<ErrorExpression>(node);
            }
            if (value->getType().isUndefined())
            {
                return std::make_unique<ErrorExpression>(node);
            }
            auto& elementType = getPointerElementType(value->getType());
            return std::make_unique<UnaryOperator>(elementType, ValueCategory::Lvalue, UnaryOperator::Dereference,
                                                   node.getUnaryToken(), std::move(value));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            value = integerPromotion(std::move(value));
            if (!value->getType().isUndefined() && !isArithmetic(value->getType())
                && !value->getType().is<VectorType>())
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), *value));
                return std::make_unique<ErrorExpression>(node);
            }
            IntrVarValue type = value->getType();
            return std::make_unique<UnaryOperator>(
                type, ValueCategory::Rvalue,
                node.getOperator() == Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus ?
                    UnaryOperator::Minus :
                    UnaryOperator::Plus,
                node.getUnaryToken(), std::move(value));
        }
        break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            value = integerPromotion(std::move(value));
            if (!value->getType().isUndefined() && !isInteger(value->getType())
                && (!value->getType().is<VectorType>() || !isInteger(getVectorElementType(value->getType()))))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), *value));
                return std::make_unique<ErrorExpression>(node);
            }
            IntrVarValue type = value->getType();
            return std::make_unique<UnaryOperator>(type, ValueCategory::Rvalue, UnaryOperator::BitwiseNegate,
                                                   node.getUnaryToken(), std::move(value));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            value = integerPromotion(std::move(value));
            if (!value->getType().isUndefined() && !isScalar(value->getType()))
            {
                log(Errors::Semantics::OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    *node.getUnaryToken(), m_sourceInterface, *node.getUnaryToken(), *value));
            }
            value = toBool(std::move(value));
            return std::make_unique<UnaryOperator>(PrimitiveType(PrimitiveType::Int, getLanguageOptions()),
                                                   ValueCategory::Rvalue, UnaryOperator::BooleanNegate,
                                                   node.getUnaryToken(), std::move(value));
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator ::GNUExtension: return value;
    }
    CLD_UNREACHABLE;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionSizeOf& node)
{
    return cld::match(
        node.getVariant(),
        [&](const std::unique_ptr<Syntax::UnaryExpression>& unaryExpression) -> IntrVarPtr<ExpressionBase>
        {
            auto exp = visit(*unaryExpression);
            if (exp->isUndefined())
            {
                return std::make_unique<ErrorExpression>(node);
            }
            auto& type = exp->getType();
            if (!isCompleteType(type))
            {
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(*exp, m_sourceInterface, type, *exp));
                return std::make_unique<ErrorExpression>(node);
            }
            if (type.is<FunctionType>())
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(*exp, m_sourceInterface, *exp, type));
                return std::make_unique<ErrorExpression>(node);
            }
            if (isBitfieldAccess(*exp))
            {
                log(Errors::Semantics::BITFIELD_NOT_ALLOWED_IN_SIZE_OF.args(*exp, m_sourceInterface, *exp));
            }
            if (!isVariableLengthArray(type))
            {
                auto size = exp->getType().getSizeOf(*this);
                return std::make_unique<SizeofOperator>(getLanguageOptions(), node.getSizeOfToken(), size,
                                                        std::move(exp));
            }

            return std::make_unique<SizeofOperator>(getLanguageOptions(), node.getSizeOfToken(), std::nullopt,
                                                    std::move(exp));
        },
        [&](const std::unique_ptr<Syntax::TypeName>& typeName) -> IntrVarPtr<ExpressionBase>
        {
            std::vector<ParsedAttribute<>> attributes;
            auto type =
                declaratorsToType(typeName->getSpecifierQualifiers(), typeName->getAbstractDeclarator(), &attributes);
            if (type->isUndefined())
            {
                return std::make_unique<ErrorExpression>(node);
            }
            reportNotApplicableAttributes(attributes);
            if (!isCompleteType(type))
            {
                log(Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(*typeName, m_sourceInterface, type,
                                                                         *typeName));
                return std::make_unique<ErrorExpression>(node);
            }
            if (type->is<FunctionType>())
            {
                log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(*typeName, m_sourceInterface,
                                                                                 *typeName, type));
                return std::make_unique<ErrorExpression>(node);
            }
            if (!isVariableLengthArray(type))
            {
                auto size = type->getSizeOf(*this);
                return std::make_unique<SizeofOperator>(
                    getLanguageOptions(), node.getSizeOfToken(), size,
                    SizeofOperator::TypeVariant{node.getSizeOfToken() + 1, std::move(type), node.end() - 1});
            }

            return std::make_unique<SizeofOperator>(
                getLanguageOptions(), node.getSizeOfToken(), std::nullopt,
                SizeofOperator::TypeVariant{node.getSizeOfToken() + 1, std::move(type), node.end() - 1});
        });
}

std::unique_ptr<cld::Semantics::Constant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::UnaryExpressionDefined& node)
{
    CLD_ASSERT(m_definedCallback);
    if (m_definedCallback(node.getIdentifier()))
    {
        return std::make_unique<Constant>(PrimitiveType(PrimitiveType::LongLong, getLanguageOptions()),
                                          llvm::APSInt(llvm::APInt(64, 1), false), node.begin(), node.end());
    }

    return std::make_unique<Constant>(PrimitiveType(PrimitiveType::LongLong, getLanguageOptions()),
                                      llvm::APSInt(64, false), node.begin(), node.end());
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::CastExpression& node)
{
    return cld::match(
        node.getVariant(),
        [&](const Syntax::UnaryExpression& unaryExpression) -> IntrVarPtr<ExpressionBase> {
            return visit(unaryExpression);
        },
        [&](const Syntax::CastExpression::CastVariant& cast) -> IntrVarPtr<ExpressionBase>
        {
            std::vector<ParsedAttribute<>> attributes;
            auto type = declaratorsToType(cast.typeName.getSpecifierQualifiers(), cast.typeName.getAbstractDeclarator(),
                                          &attributes);
            if (type->isUndefined())
            {
                visit(*cast.cast);
                return std::make_unique<ErrorExpression>(node);
            }
            reportNotApplicableAttributes(attributes);
            auto value = visit(*cast.cast);
            if (value->isUndefined())
            {
                return std::make_unique<ErrorExpression>(node);
            }
            value = lvalueConversion(std::move(value));
            if (isVoid(type))
            {
                return std::make_unique<Cast>(removeQualifiers(type), cast.openParentheses, cast.closeParentheses,
                                              std::move(value));
            }
            if (!isScalar(type) && !type->is<VectorType>())
            {
                log(Errors::Semantics::TYPE_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    cast.typeName, m_sourceInterface, cast.typeName, type));
                return std::make_unique<ErrorExpression>(node);
            }
            if (!isScalar(value->getType()) && !value->getType().is<VectorType>())
            {
                log(Errors::Semantics::EXPRESSION_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                    *value, m_sourceInterface, *value));
                return std::make_unique<ErrorExpression>(node);
            }
            type = lvalueConversion(std::move(type));
            if (type->is<VectorType>() || value->getType().is<VectorType>())
            {
                if (type->is<VectorType>() != value->getType().is<VectorType>())
                {
                    // Only one of them is a vector
                    if (!isInteger(type) && !isInteger(value->getType()))
                    {
                        if (value->getType().is<VectorType>())
                        {
                            log(Errors::Semantics::CANNOT_CAST_FROM_VECTOR_TYPE_TO_NON_INTEGER_OR_VECTOR_TYPE.args(
                                *value, m_sourceInterface, cast.typeName, type, *value));
                        }
                        else
                        {
                            log(Errors::Semantics::CANNOT_CAST_TO_VECTOR_TYPE_FROM_NON_INTEGER_OR_VECTOR_TYPE.args(
                                *value, m_sourceInterface, cast.typeName, type, *value));
                        }
                        return std::make_unique<ErrorExpression>(node);
                    }
                }
                auto toSize = type->getSizeOf(*this);
                auto fromSize = value->getType().getSizeOf(*this);
                if (toSize != fromSize)
                {
                    auto toString = "sizeof(" + diag::StringConverter<Type>::inArg(type, &m_sourceInterface)
                                    + ") = " + std::to_string(toSize);
                    auto fromString = "sizeof("
                                      + diag::StringConverter<Type>::inArg(value->getType(), &m_sourceInterface)
                                      + ") = " + std::to_string(fromSize);
                    if (type->is<VectorType>())
                    {
                        log(Errors::Semantics::CANNOT_CAST_TO_VECTOR_TYPE_FROM_TYPE_OF_DIFFERING_SIZE.args(
                            *value, m_sourceInterface, cast.typeName, toString, *value, fromString));
                    }
                    else
                    {
                        log(Errors::Semantics::CANNOT_CAST_FROM_VECTOR_TYPE_TO_TYPE_OF_DIFFERING_SIZE.args(
                            *value, m_sourceInterface, cast.typeName, toString, *value, fromString));
                    }
                    return std::make_unique<ErrorExpression>(node);
                }
            }
            else if (type->is<PointerType>())
            {
                if (!value->getType().is<PointerType>() && !isInteger(value->getType()))
                {
                    log(Errors::Semantics::CANNOT_CAST_NON_INTEGER_AND_POINTER_TYPE_N_TO_POINTER_TYPE.args(
                        *value, m_sourceInterface, *value));
                    return std::make_unique<ErrorExpression>(node);
                }
            }
            else if (value->getType().is<PointerType>())
            {
                if (!type->is<PointerType>() && !isInteger(type))
                {
                    log(Errors::Semantics::CANNOT_CAST_POINTER_TYPE_TO_NON_INTEGER_AND_POINTER_TYPE.args(
                        cast.typeName, m_sourceInterface, cast.typeName, *value));
                    return std::make_unique<ErrorExpression>(node);
                }
            }
            return std::make_unique<Cast>(std::move(type), cast.openParentheses, cast.closeParentheses,
                                          std::move(value));
        });
}

bool cld::Semantics::SemanticAnalysis::checkVectorBinaryOp(const IntrVarPtr<ExpressionBase>& lhs,
                                                           Lexer::CTokenIterator token,
                                                           const IntrVarPtr<ExpressionBase>& rhs)
{
    if (lhs->getType().is<VectorType>() || rhs->getType().is<VectorType>())
    {
        if (lhs->getType().is<VectorType>() && rhs->getType().is<VectorType>())
        {
            if (rhs->getType() != lhs->getType())
            {
                log(Errors::Semantics::TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH.args(
                    *token, m_sourceInterface, *token, *lhs, *rhs));
                return true;
            }
        }
        else
        {
            log(Errors::Semantics::CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION.args(
                *token, m_sourceInterface, *lhs, *rhs));
            return true;
        }
    }
    return false;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase> cld::Semantics::SemanticAnalysis::visit(const Syntax::Term& node)
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
                arithmeticConversion(&value, rhsValue);
                bool errors = false;
                if (!value->isUndefined() && !isArithmetic(value->getType()) && !value->getType().is<VectorType>())
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        *value, m_sourceInterface, *token, *value));
                    errors = true;
                }
                if (!rhsValue->isUndefined() && !isArithmetic(rhsValue->getType())
                    && !value->getType().is<VectorType>())
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue));
                    errors = true;
                }
                errors = checkVectorBinaryOp(value, token, rhsValue) || errors;
                if (value->isUndefined() || rhsValue->isUndefined() || errors)
                {
                    value = std::make_unique<ErrorExpression>(value->begin(), rhsValue->end());
                    continue;
                }
                IntrVarValue type = value->getType();
                value = std::make_unique<BinaryOperator>(type, std::move(value),
                                                         kind == Syntax::Term::BinaryDivide ? BinaryOperator::Divide :
                                                                                              BinaryOperator::Multiply,
                                                         token, std::move(rhsValue));
                continue;
            }
            case Syntax::Term::BinaryModulo:
            {
                arithmeticConversion(&value, rhsValue);
                bool errors = false;
                if (!value->isUndefined() && !isInteger(value->getType())
                    && (!value->getType().is<VectorType>() || !isInteger(getVectorElementType(value->getType()))))
                {
                    log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        *value, m_sourceInterface, *token, *value));
                    errors = true;
                }
                if (!rhsValue->isUndefined() && !isInteger(rhsValue->getType())
                    && (!value->getType().is<VectorType>() || !isInteger(getVectorElementType(value->getType()))))
                {
                    log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue));
                    errors = true;
                }
                errors = checkVectorBinaryOp(value, token, rhsValue) || errors;
                if (value->isUndefined() || rhsValue->isUndefined() || errors)
                {
                    value = std::make_unique<ErrorExpression>(value->begin(), rhsValue->end());
                    continue;
                }
                IntrVarValue type = value->getType();
                value = std::make_unique<BinaryOperator>(type, std::move(value), BinaryOperator::Modulo, token,
                                                         std::move(rhsValue));
                continue;
            }
        }
    }
    return value;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::AdditiveExpression& node)
{
    auto value = visit(node.getTerm());
    if (node.getOptionalTerms().empty())
    {
        return value;
    }
    for (auto& [kind, token, rhs] : node.getOptionalTerms())
    {
        auto rhsValue = visit(rhs);
        if ((isArithmetic(value->getType()) && isArithmetic(rhsValue->getType())) || value->getType().is<VectorType>()
            || rhsValue->getType().is<VectorType>())
        {
            arithmeticConversion(&value, rhsValue);
        }
        else
        {
            value = lvalueConversion(std::move(value));
            rhsValue = lvalueConversion(std::move(rhsValue));
        }
        bool errors = false;
        if (!value->getType().isUndefined() && !isScalar(value->getType()) && !value->getType().is<VectorType>())
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *value, m_sourceInterface, *token, *value));
            errors = true;
        }
        if (!rhsValue->getType().isUndefined() && !isScalar(rhsValue->getType()) && !value->getType().is<VectorType>())
        {
            log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *rhsValue, m_sourceInterface, *token, *rhsValue));
            errors = true;
        }
        if ((value->getType().is<VectorType>() && rhsValue->getType().is<PointerType>())
            || (value->getType().is<PointerType>() && rhsValue->getType().is<VectorType>()))
        {
            log(Errors::Semantics::POINTER_ARITHMETIC_WITH_VECTORS_IS_NOT_ALLOWED.args(*token, m_sourceInterface,
                                                                                       *value, *token, *rhsValue));
            errors = true;
        }
        else
        {
            errors = checkVectorBinaryOp(value, token, rhsValue) || errors;
        }
        if (value->getType().isUndefined() || rhsValue->getType().isUndefined() || errors)
        {
            value = std::make_unique<ErrorExpression>(value->begin(), rhsValue->end());
            continue;
        }
        switch (kind)
        {
            case Syntax::AdditiveExpression::BinaryMinus:
            {
                if (isArithmetic(value->getType()) && isArithmetic(rhsValue->getType()))
                {
                    IntrVarValue type = value->getType();
                    value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                             BinaryOperator::Subtraction, token, std::move(rhsValue));
                    continue;
                }
                if (value->getType().is<VectorType>())
                {
                    IntrVarValue type = value->getType();
                    value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                             BinaryOperator::Subtraction, token, std::move(rhsValue));
                    continue;
                }
                if (isArithmetic(value->getType()) && rhsValue->getType().is<PointerType>())
                {
                    log(Errors::Semantics::CANNOT_SUBTRACT_POINTER_FROM_ARITHMETIC_TYPE.args(
                        *value, m_sourceInterface, *value, *token, *rhsValue));
                    IntrVarValue type = rhsValue->getType();
                    value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                             BinaryOperator::Subtraction, token, std::move(rhsValue));
                    continue;
                }
                // value is guaranteed to be a pointer type now, rhsValue is a scalar
                auto& valueElementType = getPointerElementType(value->getType());
                if (!isCompleteType(valueElementType))
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                        *value, m_sourceInterface, valueElementType, *value, value->getType()));
                }
                else if (valueElementType.is<FunctionType>())
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        *value, m_sourceInterface, *value));
                }
                if (rhsValue->getType().is<PointerType>())
                {
                    auto& rhsElementType = getPointerElementType(rhsValue->getType());
                    if (!isCompleteType(rhsElementType))
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            *rhsValue, m_sourceInterface, rhsElementType, *rhsValue, rhsValue->getType()));
                    }
                    else if (rhsElementType.is<FunctionType>())
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            *rhsValue, m_sourceInterface, *rhsValue));
                    }

                    if (!typesAreCompatible(*removeQualifiers(valueElementType), *removeQualifiers(rhsElementType)))
                    {
                        log(Errors::Semantics::CANNOT_SUBTRACT_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                            *value, m_sourceInterface, *value, *token, *rhsValue));
                    }
                    value = std::make_unique<BinaryOperator>(
                        PrimitiveType(getLanguageOptions().ptrdiffType, getLanguageOptions()), std::move(value),
                        BinaryOperator::Subtraction, token, std::move(rhsValue));
                }
                else
                {
                    if (!isInteger(rhsValue->getType()))
                    {
                        log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE.args(
                            *rhsValue, m_sourceInterface, *token, *rhsValue));
                    }
                    IntrVarValue type = value->getType();
                    value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                             BinaryOperator::Subtraction, token, std::move(rhsValue));
                }
                continue;
            }
            case Syntax::AdditiveExpression::BinaryPlus:
            {
                if (isArithmetic(value->getType()) && isArithmetic(rhsValue->getType()))
                {
                    IntrVarValue type = value->getType();
                    value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                             BinaryOperator::Addition, token, std::move(rhsValue));
                    continue;
                }
                if (value->getType().is<VectorType>())
                {
                    IntrVarValue type = value->getType();
                    value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                             BinaryOperator::Addition, token, std::move(rhsValue));
                    continue;
                }
                auto& pointerExpr = value->getType().is<PointerType>() ? value : rhsValue;
                auto& intExpr = &pointerExpr == &value ? rhsValue : value;
                if (!isInteger(intExpr->getType()))
                {
                    log(Errors::Semantics::EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE.args(
                        *intExpr, m_sourceInterface, *token, *intExpr));
                }
                auto& elementType = getPointerElementType(pointerExpr->getType());
                if (!isCompleteType(elementType))
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                        *pointerExpr, m_sourceInterface, elementType, *pointerExpr, pointerExpr->getType()));
                }
                else if (elementType.is<FunctionType>())
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        *pointerExpr, m_sourceInterface, *pointerExpr));
                }
                IntrVarValue type = pointerExpr->getType();
                value = std::make_unique<BinaryOperator>(std::move(type), std::move(value), BinaryOperator::Addition,
                                                         token, std::move(rhsValue));
                continue;
            }
        }
    }
    return value;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::ShiftExpression& node)
{
    auto value = visit(node.getAdditiveExpression());
    if (node.getOptionalAdditiveExpressions().empty())
    {
        return value;
    }
    value = integerPromotion(std::move(value));
    for (auto& [kind, token, rhs] : node.getOptionalAdditiveExpressions())
    {
        auto rhsValue = visit(rhs);
        if (value->getType().is<VectorType>() || rhsValue->getType().is<VectorType>())
        {
            arithmeticConversion(&value, rhsValue);
        }
        else
        {
            rhsValue = integerPromotion(std::move(rhsValue));
        }
        bool errors = false;
        if (!value->getType().isUndefined() && !isInteger(value->getType())
            && (!value->getType().is<VectorType>() || !isInteger(getVectorElementType(value->getType()))))
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(*value, m_sourceInterface,
                                                                                           *token, *value));
            errors = true;
        }
        if (!rhsValue->getType().isUndefined() && !isInteger(rhsValue->getType())
            && (!rhsValue->getType().is<VectorType>() || !isInteger(getVectorElementType(rhsValue->getType()))))
        {
            log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(
                *rhsValue, m_sourceInterface, *token, *rhsValue));
            errors = true;
        }
        errors = checkVectorBinaryOp(value, token, rhsValue) || errors;
        if (value->getType().isUndefined() || rhsValue->getType().isUndefined() || errors)
        {
            value = std::make_unique<ErrorExpression>(value->begin(), rhsValue->end());
            continue;
        }
        IntrVarValue type = value->getType();
        value = std::make_unique<BinaryOperator>(std::move(type), std::move(value),
                                                 kind == Syntax::ShiftExpression::Left ? BinaryOperator::LeftShift :
                                                                                         BinaryOperator::RightShift,
                                                 token, std::move(rhsValue));
        continue;
    }
    return value;
}

cld::Semantics::VectorType cld::Semantics::SemanticAnalysis::vectorCompResultType(const VectorType& vectorType,
                                                                                  const LanguageOptions& options)
{
    auto elementCount = vectorType.getSize();
    if (isInteger(getVectorElementType(vectorType)))
    {
        switch (getVectorElementType(vectorType).as<PrimitiveType>().getKind())
        {
            case PrimitiveType::Char:
                if (options.charIsSigned)
                {
                    return vectorType;
                }
                [[fallthrough]];
            case PrimitiveType::UnsignedChar:
                return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::SignedChar, options), elementCount);
            case PrimitiveType::UnsignedShort:
                return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::Short, options), elementCount);
            case PrimitiveType::UnsignedInt:
                return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::Int, options), elementCount);
            case PrimitiveType::UnsignedLong:
                return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::Long, options), elementCount);
            case PrimitiveType::UnsignedLongLong:
                return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::LongLong, options), elementCount);
            default: return vectorType;
        }
    }
    auto bitWidth = getVectorElementType(vectorType).as<PrimitiveType>().getBitCount();
    if (bitWidth == options.sizeOfShort * 8)
    {
        return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::Short, options), elementCount);
    }
    if (bitWidth == options.sizeOfInt * 8)
    {
        return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::Int, options), elementCount);
    }
    if (bitWidth == options.sizeOfLong * 8)
    {
        return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::Long, options), elementCount);
    }
    if (bitWidth == 64)
    {
        return VectorType(typeAlloc<PrimitiveType>(PrimitiveType::LongLong, options), elementCount);
    }
    CLD_UNREACHABLE;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::RelationalExpression& node)
{
    auto value = visit(node.getShiftExpression());
    if (node.getOptionalShiftExpressions().empty())
    {
        return value;
    }
    for (auto& [kind, token, rhs] : node.getOptionalShiftExpressions())
    {
        IntrVarValue resultType = PrimitiveType(PrimitiveType::Int, getLanguageOptions());
        auto rhsValue = visit(rhs);
        if ((isArithmetic(value->getType()) && isArithmetic(rhsValue->getType())) || value->getType().is<VectorType>()
            || rhsValue->getType().is<VectorType>())
        {
            arithmeticConversion(&value, rhsValue);
        }
        else
        {
            value = lvalueConversion(std::move(value));
            rhsValue = lvalueConversion(std::move(rhsValue));
        }
        if (!value->isUndefined() && !isScalar(value->getType()) && !value->getType().is<VectorType>())
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *value, m_sourceInterface, *token, *value));
        }
        else if (isArithmetic(value->getType()) && !rhsValue->getType().is<VectorType>())
        {
            if (!rhsValue->isUndefined() && !isArithmetic(rhsValue->getType()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                    *rhsValue, m_sourceInterface, *token, *rhsValue));
            }
        }
        else if (value->getType().is<PointerType>())
        {
            if (!rhsValue->isUndefined() && !rhsValue->getType().is<PointerType>())
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                    *rhsValue, m_sourceInterface, *token, *rhsValue));
            }
            else if (!rhsValue->isUndefined())
            {
                auto rhsElementType = removeQualifiers(getPointerElementType(rhsValue->getType()));
                auto valueElementType = removeQualifiers(getPointerElementType(value->getType()));
                if (valueElementType->is<FunctionType>())
                {
                    log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                        *value, m_sourceInterface, *value));
                    if (rhsElementType->is<FunctionType>())
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            *rhsValue, m_sourceInterface, *rhsValue));
                    }
                }
                else
                {
                    if (rhsElementType->is<FunctionType>())
                    {
                        log(Errors::Semantics::POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC.args(
                            *rhsValue, m_sourceInterface, *rhsValue));
                    }
                    else if (!typesAreCompatible(valueElementType, rhsElementType))
                    {
                        log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                            *value, m_sourceInterface, *value, *token, *rhsValue));
                    }
                }
            }
        }
        else if (value->getType().is<VectorType>() || rhsValue->getType().is<VectorType>())
        {
            if (checkVectorBinaryOp(value, token, rhsValue))
            {
                value = std::make_unique<ErrorExpression>(value->begin(), rhsValue->end());
                continue;
            }
            resultType = vectorCompResultType(value->getType().as<VectorType>(), getLanguageOptions());
        }
        value = std::make_unique<BinaryOperator>(
            std::move(resultType), std::move(value),
            [kind = kind]
            {
                switch (kind)
                {
                    case Syntax::RelationalExpression::GreaterThan: return BinaryOperator::GreaterThan;
                    case Syntax::RelationalExpression::GreaterThanOrEqual: return BinaryOperator::GreaterOrEqual;
                    case Syntax::RelationalExpression::LessThan: return BinaryOperator::LessThan;
                    case Syntax::RelationalExpression::LessThanOrEqual: return BinaryOperator::LessOrEqual;
                }
                CLD_UNREACHABLE;
            }(),
            token, std::move(rhsValue));
    }
    return value;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::EqualityExpression& node)
{
    auto value = visit(node.getRelationalExpression());
    if (node.getOptionalRelationalExpressions().empty())
    {
        return value;
    }
    for (auto& [kind, token, rhs] : node.getOptionalRelationalExpressions())
    {
        IntrVarValue resultType = PrimitiveType(PrimitiveType::Int, getLanguageOptions());
        auto rhsValue = visit(rhs);
        if ((isArithmetic(value->getType()) && isArithmetic(rhsValue->getType())) || value->getType().is<VectorType>()
            || rhsValue->getType().is<VectorType>())
        {
            arithmeticConversion(&value, rhsValue);
        }
        else
        {
            value = lvalueConversion(std::move(value));
            rhsValue = lvalueConversion(std::move(rhsValue));
        }
        if (!value->isUndefined() && !isScalar(value->getType()) && !value->getType().is<VectorType>())
        {
            log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *value, m_sourceInterface, *token, *value));
        }
        else if (isArithmetic(value->getType()) && !rhsValue->getType().is<VectorType>())
        {
            if (rhsValue->getType().is<PointerType>() && isInteger(value->getType()))
            {
                auto constant = evaluateConstantExpression(*value);
                if (!constant || constant->getInteger() != 0)
                {
                    log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue));
                }
                value = std::make_unique<Conversion>(rhsValue->getType(), Conversion::Implicit, std::move(value));
            }
            else if (!rhsValue->isUndefined() && !isArithmetic(rhsValue->getType()))
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE.args(
                    *rhsValue, m_sourceInterface, *token, *rhsValue));
            }
        }
        else if (value->getType().is<PointerType>())
        {
            if (auto npc = checkPointerOperandsForNPC(*rhsValue, value->getType()); !npc || *npc != NPCCheck::WrongType)
            {
                if (!npc)
                {
                    log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL.args(
                        *rhsValue, m_sourceInterface, *token, *rhsValue, npc.error()));
                }
                else
                {
                    switch (*npc)
                    {
                        case NPCCheck::NotConstExpr:
                            log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL_2.args(
                                *rhsValue, m_sourceInterface, *token, *rhsValue));
                            break;
                        default: break;
                    }
                }
                rhsValue = std::make_unique<Conversion>(value->getType(), Conversion::Implicit, std::move(rhsValue));
            }
            else if (!rhsValue->isUndefined() && !rhsValue->getType().is<PointerType>())
            {
                log(Errors::Semantics::EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE.args(
                    *rhsValue, m_sourceInterface, *token, *rhsValue));
            }
            else if (!rhsValue->isUndefined())
            {
                auto valueElementType = removeQualifiers(getPointerElementType(value->getType()));
                auto rhsElementType = removeQualifiers(getPointerElementType(rhsValue->getType()));
                if (!isVoid(valueElementType) && !isVoid(rhsElementType))
                {
                    if (!typesAreCompatible(valueElementType, rhsElementType))
                    {
                        log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                            *value, m_sourceInterface, *value, *token, *rhsValue));
                    }
                }
                else
                {
                    if (isVoid(valueElementType))
                    {
                        if (rhsElementType->is<FunctionType>())
                        {
                            if (npc = checkNullPointerConstant(*value), !npc || *npc != NPCCheck::Success)
                            {
                                if (!npc)
                                {
                                    log(Errors::Semantics::EXPECTED_LEFT_OPERAND_OF_OPERATOR_N_TO_BE_NULL.args(
                                        *value, m_sourceInterface, *token, *value, npc.error()));
                                }
                                else
                                {
                                    log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                                        *value, m_sourceInterface, *value, *token, *rhsValue));
                                }
                            }
                            else
                            {
                                value = std::make_unique<Conversion>(rhsValue->getType(), Conversion::Implicit,
                                                                     std::move(value));
                            }
                        }
                        else
                        {
                            value = std::make_unique<Conversion>(rhsValue->getType(), Conversion::Implicit,
                                                                 std::move(value));
                        }
                    }
                    else
                    {
                        if (valueElementType->is<FunctionType>())
                        {
                            log(Errors::Semantics::CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES.args(
                                *value, m_sourceInterface, *value, *token, *rhsValue));
                        }
                        else
                        {
                            rhsValue = std::make_unique<Conversion>(value->getType(), Conversion::Implicit,
                                                                    std::move(rhsValue));
                        }
                    }
                }
            }
        }
        else if (value->getType().is<VectorType>() || rhsValue->getType().is<VectorType>())
        {
            if (checkVectorBinaryOp(value, token, rhsValue))
            {
                value = std::make_unique<ErrorExpression>(value->begin(), rhsValue->end());
                continue;
            }
            resultType = vectorCompResultType(value->getType().as<VectorType>(), getLanguageOptions());
        }
        value = std::make_unique<BinaryOperator>(std::move(resultType), std::move(value),
                                                 kind == Syntax::EqualityExpression::Equal ? BinaryOperator::Equal :
                                                                                             BinaryOperator::NotEqual,
                                                 token, std::move(rhsValue));
    }
    return value;
}

std::unique_ptr<cld::Semantics::BinaryOperator>
    cld::Semantics::SemanticAnalysis::doBitOperators(IntrVarPtr<ExpressionBase>&& lhs, BinaryOperator::Kind kind,
                                                     Lexer::CTokenIterator token, IntrVarPtr<ExpressionBase>&& rhs)
{
    if ((isArithmetic(lhs->getType()) && isArithmetic(rhs->getType())) || lhs->getType().is<VectorType>()
        || rhs->getType().is<VectorType>())
    {
        arithmeticConversion(&lhs, rhs);
    }
    bool errors = false;
    if (!lhs->isUndefined() && !isInteger(lhs->getType())
        && (!lhs->getType().is<VectorType>() || !isInteger(getVectorElementType(lhs->getType()))))
    {
        log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(*lhs, m_sourceInterface, *token,
                                                                                       *lhs));
        errors = true;
    }
    if (!rhs->isUndefined() && !isInteger(rhs->getType())
        && (!rhs->getType().is<VectorType>() || !isInteger(getVectorElementType(rhs->getType()))))
    {
        log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE.args(*rhs, m_sourceInterface, *token,
                                                                                        *rhs));
        errors = true;
    }
    errors = checkVectorBinaryOp(lhs, token, rhs) || errors;
    if (errors)
    {
        return std::make_unique<BinaryOperator>(ErrorType{}, std::move(lhs), kind, token, std::move(rhs));
    }
    IntrVarValue type = lhs->getType();
    return std::make_unique<BinaryOperator>(std::move(type), std::move(lhs), kind, token, std::move(rhs));
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::BitAndExpression& node)
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

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::BitXorExpression& node)
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

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::BitOrExpression& node)
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

std::unique_ptr<cld::Semantics::BinaryOperator>
    cld::Semantics::SemanticAnalysis::doLogicOperators(IntrVarPtr<ExpressionBase>&& lhs, BinaryOperator::Kind kind,
                                                       Lexer::CTokenIterator token, IntrVarPtr<ExpressionBase>&& rhs)
{
    lhs = lvalueConversion(std::move(lhs));
    rhs = lvalueConversion(std::move(rhs));
    if (!lhs->isUndefined() && !isScalar(lhs->getType()))
    {
        log(Errors::Semantics::LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *lhs, m_sourceInterface, *token, *lhs));
    }
    if (!rhs->isUndefined() && !isScalar(rhs->getType()))
    {
        log(Errors::Semantics::RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *rhs, m_sourceInterface, *token, *rhs));
    }
    lhs = toBool(std::move(lhs));
    rhs = toBool(std::move(rhs));
    return std::make_unique<BinaryOperator>(PrimitiveType(PrimitiveType::Int, getLanguageOptions()), std::move(lhs),
                                            kind, token, std::move(rhs));
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalAndExpression& node)
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

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::LogicalOrExpression& node)
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

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::ConditionalExpression& node)
{
    auto condition = visit(node.getLogicalOrExpression());
    if (!node.getOptionalConditionalExpression() && !node.getOptionalExpression() && !node.getOptionalQuestionMark()
        && !node.getOptionalColon())
    {
        return condition;
    }
    condition = lvalueConversion(std::move(condition));
    if (!condition->isUndefined() && !isScalar(condition->getType()))
    {
        log(Errors::Semantics::FIRST_OPERAND_OF_CONDITIONAL_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *condition, m_sourceInterface, *condition, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
    }
    condition = toBool(std::move(condition));
    auto second = visit(*node.getOptionalExpression());
    auto third = visit(*node.getOptionalConditionalExpression());
    if ((isArithmetic(second->getType()) && isArithmetic(third->getType())) || second->getType().is<VectorType>()
        || third->getType().is<VectorType>())
    {
        arithmeticConversion(&second, third);
    }
    else
    {
        second = lvalueConversion(std::move(second));
        third = lvalueConversion(std::move(third));
    }

    IntrVarValue resultType = ErrorType{};
    if (isArithmetic(second->getType()) && !third->getType().is<VectorType>())
    {
        if (third->getType().is<PointerType>() && isInteger(second->getType()))
        {
            auto constant = evaluateConstantExpression(*second);
            if (!constant || constant->getInteger() != 0)
            {
                log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE.args(
                    *third, m_sourceInterface, *third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
            }
            second = std::make_unique<Conversion>(third->getType(), Conversion::Implicit, std::move(second));
        }
        else if (!third->isUndefined() && !isArithmetic(third->getType()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE.args(
                *third, m_sourceInterface, *third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        resultType = third->getType();
    }
    else if (isVoid(second->getType()))
    {
        if (!third->isUndefined() && !isVoid(third->getType()))
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_VOID.args(
                *third, m_sourceInterface, *third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        resultType = second->getType();
    }
    else if (second->getType().is<PointerType>())
    {
        if (auto npc = checkPointerOperandsForNPC(*third, second->getType()); !npc || *npc != NPCCheck::WrongType)
        {
            if (!npc)
            {
                log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL.args(
                    *third, m_sourceInterface, *third, npc.error(), *node.getOptionalQuestionMark(),
                    *node.getOptionalColon()));
            }
            else
            {
                switch (*npc)
                {
                    case NPCCheck::NotConstExpr:
                        log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL_2.args(
                            *third, m_sourceInterface, *third, *node.getOptionalQuestionMark(),
                            *node.getOptionalColon()));
                        break;
                    default: break;
                }
            }
            third = std::make_unique<Conversion>(second->getType(), Conversion::Implicit, std::move(third));
            resultType = second->getType();
        }
        else if (!third->isUndefined() && !third->getType().is<PointerType>())
        {
            log(Errors::Semantics::EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_A_POINTER_TYPE.args(
                *third, m_sourceInterface, *third, *node.getOptionalQuestionMark(), *node.getOptionalColon()));
        }
        else if (!third->isUndefined())
        {
            auto& secondElementType = getPointerElementType(second->getType());
            auto& thirdElementType = getPointerElementType(third->getType());
            auto secondWithoutQualifier = removeQualifiers(secondElementType);
            auto thirdWithoutQualifier = removeQualifiers(thirdElementType);
            if (!isVoid(secondWithoutQualifier) && !isVoid(thirdWithoutQualifier)
                && !typesAreCompatible(secondWithoutQualifier, thirdWithoutQualifier))
            {
                log(Errors::Semantics::POINTER_TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES.args(
                    *node.getOptionalQuestionMark(), m_sourceInterface, *node.getOptionalQuestionMark(), *second,
                    *node.getOptionalColon(), *third));
            }
            else if (isVoid(secondWithoutQualifier) || isVoid(*thirdWithoutQualifier))
            {
                resultType.emplace<PointerType>(typeAlloc<PrimitiveType>(
                    PrimitiveType::Void, getLanguageOptions(),
                    flag::isConst = secondElementType.isConst() || thirdElementType.isConst(),
                    flag::isVolatile = secondElementType.isVolatile() || thirdElementType.isVolatile()));
            }
            else
            {
                IntrVarValue composite = compositeType(secondWithoutQualifier, thirdWithoutQualifier);
                composite->setConst(secondElementType.isConst() || thirdElementType.isConst());
                composite->setVolatile(secondElementType.isVolatile() || thirdElementType.isVolatile());
                resultType.emplace<PointerType>(typeAlloc(std::move(*composite)));
            }
        }
    }
    else if (isRecord(second->getType()) && !third->isUndefined())
    {
        if (!typesAreCompatible(second->getType(), third->getType()))
        {
            log(Errors::Semantics::TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES.args(
                *node.getOptionalQuestionMark(), m_sourceInterface, *node.getOptionalQuestionMark(), *second,
                *node.getOptionalColon(), *third));
        }
        else
        {
            resultType = second->getType();
        }
    }
    else if (second->getType().is<VectorType>() || third->getType().is<VectorType>())
    {
        if (second->getType().is<VectorType>() && third->getType().is<VectorType>())
        {
            if (second->getType() != third->getType())
            {
                log(Errors::Semantics::TYPE_OF_VECTOR_OPERANDS_IN_CONDITIONAL_OPERATOR_MUST_MATCH.args(
                    *node.getOptionalQuestionMark(), m_sourceInterface, *node.getOptionalQuestionMark(), *second,
                    *node.getOptionalColon(), *third));
            }
        }
        else
        {
            log(Errors::Semantics::CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION.args(
                *node.getOptionalColon(), m_sourceInterface, *second, *third));
        }
    }
    return std::make_unique<Conditional>(resultType, std::move(condition), node.getOptionalQuestionMark(),
                                         std::move(second), node.getOptionalColon(), std::move(third));
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::lvalueConversion(IntrVarPtr<ExpressionBase>&& expression)
{
    if (expression->getValueCategory() != ValueCategory::Lvalue)
    {
        // It's a lil weird to not be doing an lvalue conversion here but pretty much every user of lvalueConversion
        // probably expects it to also handle array to pointer decay even if the array is an rvalue
        if (isArray(expression->getType()))
        {
            auto& elementType = getArrayElementType(expression->getType());
            return std::make_unique<Conversion>(PointerType(&elementType), Conversion::Implicit, std::move(expression));
        }
        return std::move(expression);
    }
    if (isArray(expression->getType()))
    {
        auto& elementType = getArrayElementType(expression->getType());
        return std::make_unique<Conversion>(PointerType(&elementType), Conversion::LValue, std::move(expression));
    }
    if (expression->getType().is<FunctionType>())
    {
        if (isBuiltinFunction(*expression))
        {
            log(Errors::Semantics::BUILTIN_FUNCTION_MAY_ONLY_BE_CALLED_DIRECTLY.args(*expression, m_sourceInterface,
                                                                                     *expression));
        }
        auto type = typeAlloc(expression->getType());
        return std::make_unique<Conversion>(PointerType(type), Conversion::LValue, std::move(expression));
    }
    auto& type = expression->getType();
    if (!type.isVolatile() && !type.isConst() && (!type.is<PointerType>() || !type.as<PointerType>().isRestricted()))
    {
        IntrVarValue copy = type;
        return std::make_unique<Conversion>(std::move(copy), Conversion::LValue, std::move(expression));
    }
    IntrVarValue newType = type;
    newType->setConst(false);
    newType->setVolatile(false);
    if (auto* pointer = newType->tryAs<PointerType>())
    {
        newType = PointerType(&pointer->getElementType());
    }
    return std::make_unique<Conversion>(std::move(newType), Conversion::LValue, std::move(expression));
}

cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::SemanticAnalysis::lvalueConversion(cld::IntrVarValue<Type> type)
{
    if (isArray(type))
    {
        auto& elementType = getArrayElementType(type);
        return PointerType(&elementType);
    }
    if (type->is<FunctionType>())
    {
        return PointerType(typeAlloc(std::move(*type)));
    }
    // If the expression isn't an lvalue and not qualified then conversion is redundant
    if (!type->isVolatile() && !type->isConst()
        && (!type->is<PointerType>() || !type->as<PointerType>().isRestricted()))
    {
        return type;
    }
    type->setConst(false);
    type->setVolatile(false);
    if (auto* pointer = type->tryAs<PointerType>())
    {
        type.emplace<PointerType>(&pointer->getElementType());
    }
    return type;
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::defaultArgumentPromotion(IntrVarPtr<ExpressionBase>&& expression)
{
    expression = integerPromotion(std::move(expression));
    if (!isArithmetic(expression->getType()))
    {
        return std::move(expression);
    }
    auto& prim = expression->getType().as<PrimitiveType>();
    if (prim.getKind() != PrimitiveType::Kind::Float)
    {
        return std::move(expression);
    }
    return std::make_unique<Conversion>(PrimitiveType(PrimitiveType::Double, getLanguageOptions()),
                                        Conversion::DefaultArgumentPromotion, std::move(expression));
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::integerPromotion(IntrVarPtr<ExpressionBase>&& expression)
{
    expression = lvalueConversion(std::move(expression));
    if (auto* enumType = expression->getType().tryAs<EnumType>())
    {
        return std::make_unique<Conversion>(enumType->getInfo().type.getType(), Conversion::IntegerPromotion,
                                            std::move(expression));
    }
    if (!isArithmetic(expression->getType()))
    {
        return std::move(expression);
    }
    auto& prim = expression->getType().as<PrimitiveType>();
    if (prim.isFloatingPoint() || prim.getBitCount() >= getLanguageOptions().sizeOfInt * 8)
    {
        return std::move(expression);
    }
    return std::make_unique<Conversion>(PrimitiveType(PrimitiveType::Int, getLanguageOptions()),
                                        Conversion::IntegerPromotion, std::move(expression));
}

std::unique_ptr<cld::Semantics::Conversion>
    cld::Semantics::SemanticAnalysis::toBool(IntrVarPtr<ExpressionBase>&& expression)
{
    return std::make_unique<Conversion>(PrimitiveType(PrimitiveType::Bool, getLanguageOptions()), Conversion::Implicit,
                                        std::move(expression));
}

void cld::Semantics::SemanticAnalysis::arithmeticConversion(
    std::variant<cld::IntrVarPtr<ExpressionBase> * CLD_NON_NULL, cld::IntrVarValue<Type> * CLD_NON_NULL> lhs,
    cld::IntrVarPtr<cld::Semantics::ExpressionBase>& rhs)
{
    auto getLhsType = [](auto&& value) -> const Type&
    {
        return cld::match(
            value, [](IntrVarValue<Type>* type) -> const Type& { return *type; },
            [](IntrVarPtr<ExpressionBase>* ptr) -> const Type& { return (*ptr)->getType(); });
    };
    if (getLhsType(lhs).is<VectorType>() || rhs->getType().is<VectorType>())
    {
        cld::match(lhs, [&](auto* lhs) { *lhs = lvalueConversion(std::move(*lhs)); });
        rhs = lvalueConversion(std::move(rhs));
        if (getLhsType(lhs).is<VectorType>() && rhs->getType().is<VectorType>())
        {
            std::size_t lhsKind = getVectorElementType(getLhsType(lhs)).as<PrimitiveType>().getKind();
            std::size_t rhsKind = getVectorElementType(rhs->getType()).as<PrimitiveType>().getKind();
            if (lhsKind == rhsKind)
            {
                return;
            }
            if (lhsKind > rhsKind)
            {
                switch (lhsKind)
                {
                    case PrimitiveType::UnsignedChar:
                    case PrimitiveType::UnsignedShort:
                    case PrimitiveType::UnsignedInt:
                    case PrimitiveType::UnsignedLong:
                    case PrimitiveType::UnsignedLongLong: lhsKind--;
                    default: break;
                }
                if (lhsKind == rhsKind)
                {
                    rhs =
                        std::make_unique<Conversion>(getLhsType(lhs), Conversion::ArithmeticConversion, std::move(rhs));
                }
                return;
            }
            switch (rhsKind)
            {
                case PrimitiveType::UnsignedChar:
                case PrimitiveType::UnsignedShort:
                case PrimitiveType::UnsignedInt:
                case PrimitiveType::UnsignedLong:
                case PrimitiveType::UnsignedLongLong: rhsKind--;
                default: break;
            }
            if (lhsKind == rhsKind)
            {
                cld::match(
                    lhs, [&](IntrVarValue<Type>* lhs) { *lhs = rhs->getType(); },
                    [&](IntrVarPtr<ExpressionBase>* lhs) {
                        *lhs = std::make_unique<Conversion>(rhs->getType(), Conversion::ArithmeticConversion,
                                                            std::move(*lhs));
                    });
            }
            return;
        }
        auto& vectorType = getLhsType(lhs).is<VectorType>() ? getLhsType(lhs) : rhs->getType();
        auto& scalarType = &vectorType == &getLhsType(lhs) ? rhs->getType() : getLhsType(lhs);
        auto scalar = &vectorType == &getLhsType(lhs) ? decltype(lhs){&rhs} : lhs;
        if (!isArithmetic(scalarType))
        {
            return;
        }
        auto& elementType = getVectorElementType(vectorType);
        std::size_t scalarKind;
        if (scalarType.is<EnumType>())
        {
            scalarKind = integerPromotion(scalarType)->as<PrimitiveType>().getKind();
        }
        else
        {
            scalarKind = scalarType.as<PrimitiveType>().getKind();
        }
        // Signed and unsigned element types are seen as equal
        switch (scalarKind)
        {
            case PrimitiveType::UnsignedChar:
            case PrimitiveType::UnsignedShort:
            case PrimitiveType::UnsignedInt:
            case PrimitiveType::UnsignedLong:
            case PrimitiveType::UnsignedLongLong: scalarKind--;
            default: break;
        }
        std::size_t elementKind = elementType.as<PrimitiveType>().getKind();
        switch (elementKind)
        {
            case PrimitiveType::UnsignedChar:
            case PrimitiveType::UnsignedShort:
            case PrimitiveType::UnsignedInt:
            case PrimitiveType::UnsignedLong:
            case PrimitiveType::UnsignedLongLong: elementKind--;
            default: break;
        }
        if (elementKind == scalarKind)
        {
            cld::match(
                scalar, [&](IntrVarValue<Type>* type) { *type = vectorType; },
                [&](IntrVarPtr<ExpressionBase>* expr)
                {
                    *expr = std::make_unique<Conversion>(vectorType, Conversion::ArithmeticConversion,
                                                         integerPromotion(std::move(*expr)));
                });
            return;
        }

        auto doCasts = [&]
        {
            cld::match(
                scalar, [&](IntrVarValue<Type>* type) { *type = integerPromotion(*type); },
                [&](IntrVarPtr<ExpressionBase>* expr)
                {
                    *expr = std::make_unique<Conversion>(vectorType, Conversion::ArithmeticConversion,
                                                         integerPromotion(std::move(*expr)));
                });
        };

        // if they are both integers or both floating point types we can safely up cast
        if (isInteger(elementType) == isInteger(scalarType) && scalarKind < elementKind)
        {
            doCasts();
            return;
        }
        // if the type of something is theoretically larger than the element type of a vector that leads to an error.
        // Exception to that however is if the scalar is a constant that would fit into the element type of the vector
        // aka can be safely downcast.
        auto* expr = std::get_if<IntrVarPtr<ExpressionBase>*>(&scalar);
        if (!expr)
        {
            return;
        }
        auto* constant = (**expr)->tryAs<Constant>();
        if (!constant)
        {
            return;
        }
        cld::match(
            constant->getValue(),
            [&](const llvm::APSInt& apsInt)
            {
                if (isInteger(elementType))
                {
                    auto bitWidth = elementType.as<PrimitiveType>().getBitCount();
                    if (llvm::APSInt::isSameValue(apsInt.extOrTrunc(bitWidth), apsInt))
                    {
                        doCasts();
                    }
                }
                else
                {
                    auto kind = elementType.as<PrimitiveType>().getKind();
                    llvm::APFloat temp(kind == PrimitiveType::Double ? llvm::APFloatBase::IEEEdouble() :
                                                                       llvm::APFloatBase::IEEEsingle());
                    temp.convertFromAPInt(apsInt, apsInt.isSigned(),
                                          llvm::APFloatBase::roundingMode::NearestTiesToEven);
                    auto copy = apsInt;
                    bool exact;
                    temp.convertToInteger(copy, llvm::APFloatBase::roundingMode::NearestTiesToEven, &exact);
                    if (llvm::APSInt::isSameValue(copy, apsInt))
                    {
                        doCasts();
                    }
                }
            },
            [&](const llvm::APFloat& apFloat)
            {
                if (isInteger(elementType))
                {
                    auto width = elementType.as<PrimitiveType>().getBitCount();
                    bool isSigned = elementType.as<PrimitiveType>().isSigned();
                    llvm::APSInt temp(width, !isSigned);
                    bool exact = true;
                    apFloat.convertToInteger(temp, llvm::APFloatBase::roundingMode::NearestTiesToEven, &exact);
                    if (exact)
                    {
                        doCasts();
                    }
                }
                else
                {
                    auto kind = elementType.as<PrimitiveType>().getKind();
                    bool notExact = false;
                    auto temp = apFloat;
                    temp.convert(kind == PrimitiveType::Double ? llvm::APFloatBase::IEEEdouble() :
                                                                 llvm::APFloatBase::IEEEsingle(),
                                 llvm::APFloatBase::roundingMode::NearestTiesToEven, &notExact);
                    if (!notExact)
                    {
                        doCasts();
                    }
                }
            },
            [](auto&&) { CLD_UNREACHABLE; });
        return;
    }
    cld::match(lhs, [&](auto* lhs) { *lhs = integerPromotion(std::move(*lhs)); });
    rhs = integerPromotion(std::move(rhs));
    if (!isArithmetic(getLhsType(lhs)) || !isArithmetic(rhs->getType()))
    {
        return;
    }
    if (getLhsType(lhs) == rhs->getType())
    {
        return;
    }
    auto& lhsPrim = getLhsType(lhs).as<PrimitiveType>();
    auto& rhsPrim = rhs->getType().as<PrimitiveType>();
    IntrVarValue type = ErrorType{};
    if (lhsPrim.isFloatingPoint() || rhsPrim.isFloatingPoint())
    {
        auto [floating, biggest] = std::max(std::pair(lhsPrim.isFloatingPoint(), lhsPrim.getKind()),
                                            std::pair(rhsPrim.isFloatingPoint(), rhsPrim.getKind()));
        (void)floating;
        switch (biggest)
        {
            case PrimitiveType::Kind::Float: type = PrimitiveType(PrimitiveType::Float, getLanguageOptions()); break;
            case PrimitiveType::Kind::Double: type = PrimitiveType(PrimitiveType::Double, getLanguageOptions()); break;
            case PrimitiveType::Kind::LongDouble:
                type = PrimitiveType(PrimitiveType::LongDouble, getLanguageOptions());
                break;
            default: CLD_UNREACHABLE;
        }
    }
    else if (rhsPrim.isSigned() == lhsPrim.isSigned() || rhsPrim.getBitCount() != lhsPrim.getBitCount())
    {
        auto [bits, sign, lhsUsed] = std::max(std::tuple(lhsPrim.getBitCount(), lhsPrim.isSigned(), true),
                                              std::tuple(rhsPrim.getBitCount(), rhsPrim.isSigned(), false));
        type = lhsUsed ? getLhsType(lhs) : rhs->getType();
    }
    else
    {
        type = !lhsPrim.isSigned() ? getLhsType(lhs) : rhs->getType();
    }
    cld::match(
        lhs, [&](IntrVarValue<Type>* lhs) { *lhs = type; },
        [&](IntrVarPtr<ExpressionBase>* lhs)
        { *lhs = std::make_unique<Conversion>(type, Conversion::ArithmeticConversion, std::move(*lhs)); });
    rhs = std::make_unique<Conversion>(std::move(type), Conversion::ArithmeticConversion, std::move(rhs));
}

cld::IntrVarPtr<cld::Semantics::ExpressionBase>
    cld::Semantics::SemanticAnalysis::doSingleElementInitialization(const Syntax::Node& node, const Type& type,
                                                                    IntrVarPtr<ExpressionBase>&& expression,
                                                                    bool staticLifetime, std::size_t* size)
{
    CLD_ASSERT(!type.isUndefined() && !expression->isUndefined());

    if (isArray(type))
    {
        auto& elementType = getArrayElementType(type);
        if (!isCharacterLikeType(elementType, getLanguageOptions()))
        {
            log(Errors::Semantics::ARRAY_MUST_BE_INITIALIZED_WITH_INITIALIZER_LIST.args(*expression, m_sourceInterface,
                                                                                        *expression));
            return std::make_unique<ErrorExpression>(node);
        }
        if (!isStringLiteralExpr(*expression))
        {
            if (isCharType(elementType))
            {
                log(Errors::Semantics::ARRAY_MUST_BE_INITIALIZED_WITH_STRING_OR_INITIALIZER_LIST.args(
                    *expression, m_sourceInterface, *expression));
            }
            else
            {
                log(Errors::Semantics::ARRAY_MUST_BE_INITIALIZED_WITH_WIDE_STRING_OR_INITIALIZER_LIST.args(
                    *expression, m_sourceInterface, *expression));
            }
            return std::make_unique<ErrorExpression>(node);
        }
        auto& str = expression->as<Constant>().getValue();
        if (std::holds_alternative<std::string>(str) && !isCharType(elementType))
        {
            log(Errors::Semantics::CANNOT_INITIALIZE_WCHART_ARRAY_WITH_STRING_LITERAL.args(
                *expression, m_sourceInterface, *expression));
            return std::make_unique<ErrorExpression>(node);
        }
        if (isCharType(elementType) && std::holds_alternative<Lexer::NonCharString>(str))
        {
            log(Errors::Semantics::CANNOT_INITIALIZE_CHAR_ARRAY_WITH_WIDE_STRING_LITERAL.args(
                *expression, m_sourceInterface, *expression));
            return std::make_unique<ErrorExpression>(node);
        }
    }
    else
    {
        expression = lvalueConversion(std::move(expression));
    }

    if (staticLifetime)
    {
        auto result = evaluateConstantExpression(*expression, Mode::Initialization);
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
        [&]
        {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_TYPE.args(*expression, m_sourceInterface,
                                                                                      *expression));
        },
        [&]
        {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *expression, m_sourceInterface, *expression));
        },
        [&] { CLD_UNREACHABLE; },
        [&]
        {
            log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_OF_TYPE_N_WITH_INCOMPATIBLE_TYPE_N.args(
                *expression, m_sourceInterface, type, *expression));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_NULL_2.args(*expression, m_sourceInterface, *expression));
        },
        [&](const ConstValue& constant)
        {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_NULL.args(*expression, m_sourceInterface, *expression,
                                                                        constant));
        },
        [&]
        {
            log(Errors::Semantics::EXPECTED_INITIALIZER_TO_BE_A_POINTER_TYPE.args(*expression, m_sourceInterface,
                                                                                  *expression));
        },
        [&]
        {
            if (isVoid(type.as<PointerType>().getElementType()))
            {
                log(Errors::Semantics::CANNOT_INITIALIZE_VOID_POINTER_WITH_FUNCTION_POINTER.args(
                    *expression, m_sourceInterface, *expression));
            }
            else
            {
                log(Errors::Semantics::CANNOT_INITIALIZE_FUNCTION_POINTER_WITH_VOID_POINTER_PARAMETER.args(
                    *expression, m_sourceInterface, *expression));
            }
        });

    if (type.is<AbstractArrayType>() && size)
    {
        auto& elementType = getArrayElementType(type);
        if (isCharType(elementType))
        {
            if (expression->is<Constant>()
                && std::holds_alternative<std::string>(expression->as<Constant>().getValue()))
            {
                *size = cld::get<std::string>(expression->as<Constant>().getValue()).size() + 1;
            }
        }
        else if (elementType.is<PrimitiveType>()
                 && removeQualifiers(elementType)
                        == PrimitiveType(getLanguageOptions().wcharUnderlyingType, getLanguageOptions()))
        {
            if (expression->is<Constant>()
                && std::holds_alternative<Lexer::NonCharString>(expression->as<Constant>().getValue()))
            {
                *size = cld::get<Lexer::NonCharString>(expression->as<Constant>().getValue()).characters.size() + 1;
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
        [&](const Syntax::AssignmentExpression& assignmentExpression) -> Initializer
        {
            auto value = visit(assignmentExpression);
            if (type.isUndefined() || value->isUndefined())
            {
                return std::make_unique<ErrorExpression>(assignmentExpression);
            }

            if (!isStringLiteralExpr(*value))
            {
                value = lvalueConversion(std::move(value));
            }
            return doSingleElementInitialization(assignmentExpression, type, std::move(value), staticLifetime, size);
        },
        [&](const Syntax::InitializerList& initializerList) -> Initializer
        { return visit(initializerList, type, staticLifetime, size); });
}

cld::Semantics::Initializer cld::Semantics::SemanticAnalysis::visit(const Syntax::InitializerList& node,
                                                                    const Type& type, bool staticLifetime,
                                                                    std::size_t* size)
{
    if (!type.isUndefined() && !isArray(type) && !isRecord(type) && node.getNonCommaExpressionsAndBlocks().size() == 1
        && node.getNonCommaExpressionsAndBlocks()[0].second.empty()
        && std::holds_alternative<Syntax::AssignmentExpression>(
            node.getNonCommaExpressionsAndBlocks()[0].first.getVariant()))
    {
        return visit(node.getNonCommaExpressionsAndBlocks()[0].first, type, staticLifetime, size);
    }
    if (!type.isUndefined() && !isAggregate(type))
    {
        log(Errors::Semantics::CANNOT_INITIALIZE_ARITHMETIC_OR_POINTER_TYPE_WITH_INITIALIZER_LIST.args(
            node, m_sourceInterface, node));
        return std::make_unique<ErrorExpression>(node);
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

    auto assignChildren = YComb{[&](auto&& self, const Type& type, INode& parent) -> void
                                {
                                    if (isRecord(type))
                                    {
                                        auto ref = getFieldLayout(type);
                                        if (ref.back().type->is<AbstractArrayType>())
                                        {
                                            ref = ref.subspan(1);
                                        }
                                        parent.resize(ref.size());
                                        for (std::size_t i = 0; i < ref.size(); i++)
                                        {
                                            parent.at(i).type = ref[i].type;
                                            parent.at(i).index = i;
                                            parent.at(i).parent = &parent;
                                            if (isAggregate(*ref[i].type))
                                            {
                                                self(*ref[i].type, parent.at(i));
                                            }
                                        }
                                    }
                                    if (auto* arrayType = type.tryAs<ArrayType>())
                                    {
                                        parent.resize(arrayType->getSize());
                                        for (std::size_t i = 0; i < arrayType->getSize(); i++)
                                        {
                                            parent.at(i).type = &arrayType->getType();
                                            parent.at(i).index = i;
                                            parent.at(i).parent = &parent;
                                            if (isAggregate(arrayType->getType()))
                                            {
                                                self(arrayType->getType(), parent.at(i));
                                            }
                                        }
                                    }
                                    if (auto* vectorType = type.tryAs<VectorType>())
                                    {
                                        parent.resize(vectorType->getSize());
                                        for (std::size_t i = 0; i < vectorType->getSize(); i++)
                                        {
                                            parent.at(i).type = &vectorType->getType();
                                            parent.at(i).index = i;
                                            parent.at(i).parent = &parent;
                                            if (isAggregate(vectorType->getType()))
                                            {
                                                self(vectorType->getType(), parent.at(i));
                                            }
                                        }
                                    }
                                }};
    bool abstractArray = false;
    if (type.is<AbstractArrayType>())
    {
        abstractArray = true;
        top.resize(1);
        top.at(0).type = &getArrayElementType(type);
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
                                Syntax::InitializerList::vector::const_iterator end) -> void
                            {
                                for (auto iter = begin; iter != end; iter++)
                                {
                                    auto& [initializer, designationList] = *iter;
                                    if (!designationList.empty())
                                    {
                                        // We can check if they're proper integer constant expressions but that's about
                                        // it
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
                                        auto& initializerList =
                                            cld::get<Syntax::InitializerList>(initializer.getVariant());
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
        return std::make_unique<ErrorExpression>(node);
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
                        return std::make_unique<ErrorExpression>(node);
                    }
                    auto exp = visit(cld::get<Syntax::ConstantExpression>(desig));
                    auto constant = evaluateConstantExpression(*exp);
                    if (!constant)
                    {
                        for (auto& mes : constant.error())
                        {
                            log(mes);
                        }
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return std::make_unique<ErrorExpression>(node);
                    }
                    if (!isInteger(exp->getType()))
                    {
                        log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                            *exp, m_sourceInterface, *exp));
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return std::make_unique<ErrorExpression>(node);
                    }
                    if (constant->getInteger().isNegative())
                    {
                        log(Errors::Semantics::DESIGNATOR_INDEX_MUST_NOT_BE_NEGATIVE.args(*exp, m_sourceInterface, *exp,
                                                                                          *constant));
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return std::make_unique<ErrorExpression>(node);
                    }
                    if (!(current == &top && abstractArray)
                        && constant->getInteger() >= current->type->as<ArrayType>().getSize())
                    {
                        log(Errors::Semantics::DESIGNATOR_INDEX_OUT_OF_RANGE_FOR_ARRAY_TYPE_N.args(
                            *exp, m_sourceInterface, *current->type, *exp, *constant));
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return std::make_unique<ErrorExpression>(node);
                    }
                    if (current == &top && abstractArray)
                    {
                        *size = std::max(*size, constant->getInteger().getZExtValue() + 1);
                        auto prevSize = top.size();
                        for (std::size_t i = prevSize; i < *size; i++)
                        {
                            top.push_back({top.type->as<AbstractArrayType>().getType(), i, top});
                            assignChildren(*top.at(i).type, top.at(i));
                        }
                    }
                    currentIndex = constant->getInteger().getZExtValue();
                }
                else if (isRecord(*current->type))
                {
                    if (!std::holds_alternative<Lexer::CTokenIterator>(desig))
                    {
                        if (current->type->is<StructType>())
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
                        return std::make_unique<ErrorExpression>(node);
                    }
                    const auto* token = cld::get<Lexer::CTokenIterator>(desig);
                    auto& fields = getFields(*current->type);
                    auto result =
                        std::find_if(fields.begin(), fields.end(),
                                     [name = token->getText()](const auto& pair) { return pair.second.name == name; });
                    if (result == fields.end())
                    {
                        current->type->match(
                            [&](const StructType& structType)
                            {
                                if (structType.isAnonymous())
                                {
                                    log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT.args(
                                        *token, m_sourceInterface, *token));
                                }
                                else
                                {
                                    log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N.args(
                                        *token, m_sourceInterface, *token, structType.getStructName()));
                                }
                            },
                            [&](const UnionType& unionType)
                            {
                                if (unionType.isAnonymous())
                                {
                                    log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION.args(
                                        *token, m_sourceInterface, *token));
                                }
                                else
                                {
                                    log(Errors::Semantics::NO_MEMBER_CALLED_N_FOUND_IN_UNION_N.args(
                                        *token, m_sourceInterface, *token, unionType.getUnionName()));
                                }
                            },
                            [&](const auto&) { CLD_UNREACHABLE; });
                        finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
                        return std::make_unique<ErrorExpression>(node);
                    }
                    if (result->second.type->is<AbstractArrayType>())
                    {
                        log(Errors::Semantics::CANNOT_INITIALIZE_FLEXIBLE_ARRAY_MEMBER.args(*token, m_sourceInterface,
                                                                                            *token));
                        finishRest(iter, node.getNonCommaExpressionsAndBlocks().end());
                        return std::make_unique<ErrorExpression>(node);
                    }
                    if (result->second.parentTypes.empty())
                    {
                        currentIndex = result - fields.begin();
                    }
                    else
                    {
                        for (auto index : tcb::span(result->second.indices.data(), result->second.indices.size() - 1))
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
                    return std::make_unique<ErrorExpression>(node);
                }
            }
        }
        while (current && currentIndex >= current->size())
        {
            if (current == &top && abstractArray)
            {
                top.push_back({getArrayElementType(*top.type), top.size(), top});
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
            return std::make_unique<ErrorExpression>(node);
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
                return std::make_unique<ErrorExpression>(node);
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
                    [&](InitializerList::Initialization&& initialization) -> InitializerList::Initialization&&
                    {
                        initialization.path.insert(initialization.path.begin(), path.rbegin(), path.rend());
                        return std::move(initialization);
                    });
            }
            else
            {
                auto& expr = cld::get<IntrVarPtr<ExpressionBase>>(merge);
                initializations.push_back({{path.rbegin(), path.rend()}, std::move(expr)});
            }
        }
        else
        {
            auto expression = visit(cld::get<Syntax::AssignmentExpression>(initializer.getVariant()));
            if (expression->getType().isUndefined())
            {
                finishRest(iter + 1, node.getNonCommaExpressionsAndBlocks().end());
            }
            if (!isStringLiteralExpr(*expression))
            {
                expression = lvalueConversion(std::move(expression));
            }
            else if (currentIndex == 0 && &top == current && node.getNonCommaExpressionsAndBlocks().size() == 1
                     && designationList.empty() && isCharArray(*top.type, getLanguageOptions()))
            {
                // Handles the case of a string literal being used to initialize the top level object with optional
                // braces surrounding it
                return doSingleElementInitialization(initializer, type, std::move(expression), staticLifetime, size);
            }
            while (isAggregate(*current->at(currentIndex).type))
            {
                if (typesAreCompatible(*removeQualifiers(expression->getType()), *current->at(currentIndex).type)
                    || (isCharArray(*current->at(currentIndex).type, getLanguageOptions())
                        && isStringLiteralExpr(*expression)))
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
                return std::make_unique<ErrorExpression>(node);
            }
            expression = doSingleElementInitialization(initializer, *current->at(currentIndex).type,
                                                       std::move(expression), staticLifetime, nullptr);
            initializations.push_back({{path.rbegin(), path.rend()}, std::move(expression)});
        }
        if (current->type->is<UnionType>())
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

cld::Expected<cld::Semantics::SemanticAnalysis::NPCCheck, cld::Semantics::ConstValue>
    cld::Semantics::SemanticAnalysis::checkNullPointerConstant(const ExpressionBase& expression)
{
    // C99 6.3.2.3§3:
    // An integer constant expression with the value 0, or such an expression cast to type
    // void *, is called a null pointer constant.
    const ExpressionBase* expr = &expression;
    if (expression.is<Cast>() && expression.getType().is<PointerType>()
        && isVoid(expression.getType().as<PointerType>().getElementType())
        && expression.getType().as<PointerType>().getElementType().getFlags() == Type::Nothing)
    {
        expr = &expression.as<Cast>().getExpression();
    }
    if (!isInteger(expr->getType()))
    {
        return NPCCheck::WrongType;
    }
    auto constant = evaluateConstantExpression(*expr);
    if (!constant)
    {
        return NPCCheck::NotConstExpr;
    }
    if (constant->getInteger() != 0)
    {
        return *constant;
    }
    return NPCCheck::Success;
}

cld::Expected<cld::Semantics::SemanticAnalysis::NPCCheck, cld::Semantics::ConstValue>
    cld::Semantics::SemanticAnalysis::checkPointerOperandsForNPC(const ExpressionBase& possibleNPC,
                                                                 const Type& otherType)
{
    // if the expression is void*, the other type is not a pointer to a function and the null pointer check failed,
    // fail with a WrongType error, allowing callees to check whether pointer conversion would work
    auto npc = checkNullPointerConstant(possibleNPC);
    if ((!npc || *npc != NPCCheck::WrongType)
        && ((npc && *npc == NPCCheck::Success) || otherType.as<PointerType>().getElementType().is<FunctionType>()
            || !possibleNPC.getType().is<PointerType>()))
    {
        return npc;
    }
    return NPCCheck::WrongType;
}

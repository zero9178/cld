#include "ConstantEvaluator.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <stdexcept>
#include <utility>

#include "ErrorMessages.hpp"

static_assert(-1 == ~0, "Arithmetic here only works on 2's complement");

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
{
    return std::visit(
        [this, &node](auto&& value) -> Semantics::ConstRetType {
            using T = std::decay_t<decltype(value)>;
            if constexpr (!std::is_same_v<T, std::string>)
            {
                return {value};
            }
            else
            {
                logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("String literals"),
                         Modifier(node.begin(), node.end()));
                return {};
            }
        },
        node.getValue());
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionParenthese& node)
{
    return visit(node.getExpression());
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression& node)
{
    return visit(node.getPostFixExpression());
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
{
    auto value = visit(node.getCastExpression());
    if (value.isUndefined())
    {
        return value;
    }
    if (!value.isInteger() && m_mode == Integer)
    {
        logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                 Modifier(node.begin() + 1, node.end()));
        return {};
    }
    switch (node.getAnOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
            logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args('\'' + node.begin()->emitBack()
                                                                                         + '\''),
                     Modifier(node.begin(), node.begin() + 1));
            return {};
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
            throw std::runtime_error("Not supported yet");
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            if (!value.isArithmetic())
            {
                logError(ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                             "+", value.getType().getFullFormattedTypeName()),
                         Modifier(node.begin(), node.end(), Modifier::PointAtBeginning));
                return {};
            }
            return +value;
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        {
            if (!value.isArithmetic())
            {
                logError(ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                             "-", value.getType().getFullFormattedTypeName()),
                         Modifier(node.begin(), node.end(), Modifier::PointAtBeginning));
                return {};
            }
            return -value;
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            if (!value.isInteger())
            {
                logError(ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                             "~", value.getType().getFullFormattedTypeName()),
                         Modifier(node.begin(), node.end(), Modifier::PointAtBeginning));
                return {};
            }
            return ~value;
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            return !value;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& node)
{
    return match(
        node.getVariant(),
        [this](const std::unique_ptr<Syntax::TypeName>& typeName) -> OpenCL::Semantics::ConstRetType {
            auto type = m_typeCallback ? m_typeCallback(*typeName) : Type();
            if (type.isUndefined())
            {
                // Here we rely on the implementation of the callback to log the error somehow
                return {};
            }
            auto size = Semantics::sizeOf(type);
            if (!size)
            {
                logError(size.error(), Modifier(typeName->begin(), typeName->end()));
                return {};
            }
            return {*size};
        },
        [](auto &&) -> OpenCL::Semantics::ConstRetType { throw std::runtime_error("Not implemented yet"); });
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::CastExpression& node)
{
    return match(
        node.getVariant(),
        [this](const Syntax::UnaryExpression& unaryExpression) -> ConstRetType { return visit(unaryExpression); },
        [this](const std::pair<Syntax::TypeName, std::unique_ptr<Syntax::CastExpression>>& cast) -> ConstRetType {
            auto value = visit(*cast.second);
            if (value.isUndefined())
            {
                return value;
            }
            auto type = m_typeCallback ? m_typeCallback(cast.first) : Type{};
            if (type.isUndefined())
            {
                return {};
            }
            if (auto* primitive = std::get_if<PrimitiveType>(&type.get()))
            {
                if (m_mode == Integer && (!primitive || primitive->isFloatingPoint() || primitive->getBitCount() == 0))
                {
                    logError(ErrorMessages::Semantics::CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION,
                             Modifier(cast.first.begin(), cast.first.end()));
                    return {};
                }
                else if (!value.isArithmetic())
                {
                    logError(ErrorMessages::Semantics::INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args(
                                 value.getType().getFullFormattedTypeName(), type.getFullFormattedTypeName()),
                             Modifier(cast.first.begin(), cast.first.end()));
                    return {};
                }
            }
            else if (std::holds_alternative<PointerType>(type.get()))
            {
                if (m_mode != Initialization)
                {
                    logError(ErrorMessages::Semantics::CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION,
                             Modifier(cast.first.begin(), cast.first.end()));
                    return {};
                }
                else if (!value.isInteger() && value.isArithmetic())
                {
                    logError(ErrorMessages::Semantics::INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args(
                                 value.getType().getFullFormattedTypeName(), type.getFullFormattedTypeName()),
                             Modifier(cast.first.begin(), cast.first.end()));
                    return {};
                }
            }
            else
            {
                logError(ErrorMessages::Semantics::INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args(
                             value.getType().getFullFormattedTypeName(), type.getFullFormattedTypeName()),
                         Modifier(cast.first.begin(), cast.first.end()));
                return {};
            }

            return value.castTo(type);
        });
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::Term& node)
{
    auto value = visit(node.getCastExpression());
    for (auto& [op, exp] : node.getOptionalCastExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getCastExpression().begin(), node.getCastExpression().end()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(exp.begin(), exp.end()));
            return {};
        }
        switch (op)
        {
            case Syntax::Term::BinaryDotOperator::BinaryMultiply:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 "*", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),

                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                value *= other;
            }
            break;
            case Syntax::Term::BinaryDotOperator::BinaryDivide:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 "/", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),

                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                value /= other;
            }
            break;
            case Syntax::Term::BinaryDotOperator::BinaryRemainder:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 "%", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),

                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                value %= other;
            }
            break;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::AdditiveExpression& node)
{
    auto value = visit(node.getTerm());
    for (auto& [op, exp] : node.getOptionalTerms())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getTerm().begin(), node.getTerm().end()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(exp.begin(), exp.end()));
            return {};
        }
        switch (op)
        {
            case Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus:
            {
                if ((!value.isArithmetic() && !other.isArithmetic())
                    || (value.isArithmetic() != other.isArithmetic() && (!value.isInteger() && !other.isInteger())))
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 "+", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),
                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                if (value.isArithmetic() != other.isArithmetic())
                {
                    auto& ptr = value.isArithmetic() ? other : value;
                    auto& elementType = std::get<PointerType>(ptr.getType().get()).getElementType();
                    auto result = sizeOf(elementType);
                    if (!result)
                    {
                        logError(ErrorMessages::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                                     elementType.getFullFormattedTypeName()),
                                 Modifier(value.isArithmetic() ? exp.begin() : node.begin(),
                                          value.isArithmetic() ? exp.end() : exp.begin() - 1));
                        return {};
                    }
                }
                value += other;
            }
            break;
            case Syntax::AdditiveExpression::BinaryDashOperator::BinaryMinus:
            {
                if ((value.isArithmetic() || !other.isInteger()) && (value.isArithmetic() != other.isArithmetic()))
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 "-", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),

                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                if (!value.isArithmetic() && !other.isArithmetic())
                {
                    if (std::get<PointerType>(value.getType().get()).getElementType().get()
                        != std::get<PointerType>(other.getType().get()).getElementType().get())
                    {
                        logError(ErrorMessages::Semantics::
                                     CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N.args(
                                         "-", value.getType().getFullFormattedTypeName(),
                                         other.getType().getFullFormattedTypeName()),
                                 Modifier(exp.begin() - 1, exp.begin()));
                        return {};
                    }
                }
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    auto& ptr = value.isArithmetic() ? other : value;
                    auto& elementType = std::get<PointerType>(ptr.getType().get()).getElementType();
                    auto result = sizeOf(elementType);
                    if (!result)
                    {
                        logError(ErrorMessages::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                                     elementType.getFullFormattedTypeName()),

                                 Modifier(value.isArithmetic() ? exp.begin() : node.begin(),
                                          value.isArithmetic() ? exp.end() : exp.begin() - 1));
                        return {};
                    }
                }
                value -= other;
            }
            break;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::ShiftExpression& node)
{
    auto value = visit(node.getAdditiveExpression());
    for (auto& [op, exp] : node.getOptionalAdditiveExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getAdditiveExpression().begin(), node.getAdditiveExpression().end()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(exp.begin(), exp.end()));
            return {};
        }
        switch (op)
        {
            case Syntax::ShiftExpression::ShiftOperator::Left:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 "<<", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),
                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                value <<= other;
            }
            break;
            case Syntax::ShiftExpression::ShiftOperator::Right:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                 ">>", value.getType().getFullFormattedTypeName(),
                                 other.getType().getFullFormattedTypeName()),
                             Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning));
                    return {};
                }
                value >>= other;
            }
            break;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitAndExpression& node)
{
    auto value = visit(node.getEqualityExpressions()[0]);
    for (auto iter = node.getEqualityExpressions().cbegin() + 1; iter != node.getEqualityExpressions().cend(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getEqualityExpressions()[0].begin(), node.getEqualityExpressions()[0].end()));
            return {};
        }
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(iter->begin(), iter->end()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                         "&", value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),
                     Modifier(iter->begin() - 1, iter->begin(), Modifier::PointAtBeginning));
            return {};
        }
        value &= other;
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitXorExpression& node)
{
    auto value = visit(node.getBitAndExpressions()[0]);
    for (auto iter = node.getBitAndExpressions().begin() + 1; iter != node.getBitAndExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getBitAndExpressions()[0].begin(), node.getBitAndExpressions()[0].end()));
            return {};
        }
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(iter->begin(), iter->end()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                         "^", value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),

                     Modifier(iter->begin() - 1, iter->begin(), Modifier::PointAtBeginning));
            return {};
        }
        value ^= other;
    }
    return value;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitOrExpression& node)
{
    auto value = visit(node.getBitXorExpressions()[0]);
    for (auto iter = node.getBitXorExpressions().begin() + 1; iter != node.getBitXorExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getBitXorExpressions()[0].begin(), node.getBitXorExpressions()[0].end()));
            return {};
        }
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(iter->begin(), iter->end()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            logError(ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                         "|", value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),
                     Modifier(iter->begin() - 1, iter->begin(), Modifier::PointAtBeginning));
            return {};
        }
        value |= other;
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalAndExpression& node)
{
    auto value = visit(node.getBitOrExpressions()[0]);
    for (auto iter = node.getBitOrExpressions().begin() + 1; iter != node.getBitOrExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getBitOrExpressions()[0].begin(), node.getBitOrExpressions()[0].end()));
            return {};
        }
        value = value.isUndefined() ? value : value.toBool();
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(iter->begin(), iter->end()));
            return {};
        }
        if (!value && !value.isUndefined())
        {
            break;
        }
        value = other.isUndefined() ? other : other.toBool();
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalOrExpression& node)
{
    auto value = visit(node.getAndExpressions()[0]);
    for (auto iter = node.getAndExpressions().begin() + 1; iter != node.getAndExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getAndExpressions()[0].begin(), node.getAndExpressions()[0].end()));
            return {};
        }
        value = value.isUndefined() ? value : value.toBool();
        auto other = visit(*iter);
        if (other.isUndefined() || other.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(iter->begin(), iter->end()));
            return {};
        }
        if (value && !value.isUndefined())
        {
            break;
        }
        value = other.isUndefined() ? other : other.toBool();
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::ConditionalExpression& node)
{
    if (node.getOptionalExpression() && node.getOptionalConditionalExpression())
    {
        auto value = visit(node.getLogicalOrExpression());
        // TODO: Type returned is wrong but there needs to be more complex logic to figure out the type of each
        //       expression
        if (value.isUndefined())
        {
            // Evaluate both and return undefined
            visit(*node.getOptionalExpression());
            visit(*node.getOptionalConditionalExpression());
            return {};
        }
        else
        {
            if (value)
            {
                return visit(*node.getOptionalExpression());
            }
            else
            {
                return visit(*node.getOptionalConditionalExpression());
            }
        }
    }
    else
    {
        return visit(node.getLogicalOrExpression());
    }
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::RelationalExpression& node)
{
    auto value = visit(node.getShiftExpression());
    for (auto& [op, exp] : node.getOptionalShiftExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getShiftExpression().begin(), node.getShiftExpression().end()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(exp.begin(), exp.end()));
            return {};
        }
        if (!value.isArithmetic() && !other.isArithmetic())
        {
            if (std::get<PointerType>(value.getType().get()).getElementType().get()
                != std::get<PointerType>(other.getType().get()).getElementType().get())
            {
                logError(
                    ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N
                        .args("-", value.getType().getFullFormattedTypeName(),
                              other.getType().getFullFormattedTypeName()),
                    Modifier(exp.begin() - 1, exp.begin()));
                return {};
            }
        }
        switch (op)
        {
            case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
            {
                value = value > other;
            }
            break;
            case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
            {
                value = value >= other;
            }
            break;
            case Syntax::RelationalExpression::RelationalOperator::LessThan:
            {
                value = value < other;
            }
            break;
            case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
            {
                value = value <= other;
            }
            break;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::EqualityExpression& node)
{
    auto value = visit(node.getRelationalExpression());
    for (auto& [op, exp] : node.getOptionalRelationalExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(node.getRelationalExpression().begin(), node.getRelationalExpression().end()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            logError(ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                     Modifier(exp.begin(), exp.end()));
            return {};
        }
        std::string opName = op == Syntax::EqualityExpression::EqualityOperator::Equal ? "==" : "!=";
        if (!value.isArithmetic() && !other.isArithmetic())
        {
            if (std::get<PointerType>(value.getType().get()).getElementType().get()
                    != std::get<PointerType>(other.getType().get()).getElementType().get()
                && !isVoid(std::get<PointerType>(value.getType().get()).getElementType())
                && !isVoid(std::get<PointerType>(other.getType().get()).getElementType()))
            {
                logError(
                    ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N
                        .args(opName, value.getType().getFullFormattedTypeName(),
                              other.getType().getFullFormattedTypeName()),
                    Modifier(exp.begin() - 1, exp.begin()));
                return {};
            }
        }
        else if (!value.isArithmetic() || !other.isArithmetic())
        {
            if (!value.isInteger() && !other.isInteger())
            {
                logError(
                    ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                        opName, value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),
                    Modifier(exp.begin() - 1, exp.begin()));
                return {};
            }
            else
            {
                auto& null = value.isInteger() ? value : other;
                if (null.to<std::int64_t>() != 0)
                {
                    logError(ErrorMessages::Semantics::INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER,

                             Modifier(value.isInteger() ? node.begin() : exp.begin(),
                                      value.isInteger() ? exp.begin() - 1 : exp.end()));
                    return {};
                }
            }
        }
        switch (op)
        {
            case Syntax::EqualityExpression::EqualityOperator::Equal:
            {
                value = value == other;
            }
            break;
            case Syntax::EqualityExpression::EqualityOperator::NotEqual:
            {
                value = value != other;
            }
            break;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionSubscript&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionArrow&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionDot&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    return match(
        node, [this](auto&& value) -> ConstRetType { return visit(value); },
        [this](const Syntax::PrimaryExpressionIdentifier& identifier) -> ConstRetType {
            auto* decl = m_declarationCallback ? m_declarationCallback(identifier.getIdentifier()) : nullptr;
            if (decl)
            {
                if (std::holds_alternative<Type>(*decl))
                {
                    throw std::runtime_error(
                        "Internal compiler error: identifier is typename and was not found as such by the parser");
                }
                else if (auto* value = std::get_if<std::int32_t>(decl))
                {
                    return {*value};
                }
            }
            logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("variable access"),
                     Modifier(identifier.begin(), identifier.end()));
            return {};
        });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    return match(
        node, [this](auto&& value) -> ConstRetType { return visit(value); },
        [this](const Syntax::PostFixExpressionFunctionCall& call) -> ConstRetType {
            logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("function call"),
                     Modifier(Syntax::nodeFromNodeDerivedVariant(call.getPostFixExpression()).begin() + 1, call.end()));
            return {};
        },
        [this](const Syntax::PostFixExpressionIncrement& increment) -> ConstRetType {
            logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'++'"),
                     Modifier(Syntax::nodeFromNodeDerivedVariant(increment.getPostFixExpression()).begin() + 1,
                              increment.end()));
            return {};
        },
        [this](const Syntax::PostFixExpressionDecrement& decrement) -> ConstRetType {
            logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'--'"),
                     Modifier(Syntax::nodeFromNodeDerivedVariant(decrement.getPostFixExpression()).begin() + 1,
                              decrement.end()));
            return {};
        },
        [this](const Syntax::PostFixExpressionTypeInitializer& initializer) -> ConstRetType {
            logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("initializer"),
                     Modifier(initializer.begin(), initializer.end()));
            return {};
        });
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    return std::visit([this](auto&& value) -> OpenCL::Semantics::ConstRetType { return visit(value); }, node);
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::Expression& node)
{
    if (node.getAssignmentExpressions().size() > 1)
    {
        logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("','"),
                 Modifier(node.getAssignmentExpressions()[1].begin() - 1, node.getAssignmentExpressions()[1].begin()));
        return {};
    }
    return visit(node.getAssignmentExpressions()[0]);
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::AssignmentExpression& node)
{
    for (auto& [op, cond] : node.getAssignments())
    {
        logError(ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args('\'' + [op = op]() -> std::string {
            switch (op)
            {
                case Syntax::AssignmentExpression::AssignOperator::NoOperator: return "=";
                case Syntax::AssignmentExpression::AssignOperator::PlusAssign: return "+=";
                case Syntax::AssignmentExpression::AssignOperator::MinusAssign: return "-=";
                case Syntax::AssignmentExpression::AssignOperator::DivideAssign: return "/=";
                case Syntax::AssignmentExpression::AssignOperator::MultiplyAssign: return "*=";
                case Syntax::AssignmentExpression::AssignOperator::ModuloAssign: return "%=";
                case Syntax::AssignmentExpression::AssignOperator::LeftShiftAssign: return "<<=";
                case Syntax::AssignmentExpression::AssignOperator::RightShiftAssign: return ">>=";
                case Syntax::AssignmentExpression::AssignOperator::BitAndAssign: return "&=";
                case Syntax::AssignmentExpression::AssignOperator::BitOrAssign: return "|=";
                case Syntax::AssignmentExpression::AssignOperator::BitXorAssign: return "^=";
                default: OPENCL_UNREACHABLE;
            }
        }() + '\''),
                 Modifier(cond.begin() - 1, cond.begin()));
    }
    if (!node.getAssignments().empty())
    {
        return {};
    }
    return visit(node.getConditionalExpression());
}

OpenCL::Semantics::ConstantEvaluator::ConstantEvaluator(
    std::function<Type(const Syntax::TypeName&)> typeCallback,
    std::function<const DeclarationTypedefEnums*(const std::string&)> declarationCallback,
    std::function<void(std::string, std::optional<Modifier>)> loggerCallback, Mode mode)
    : m_typeCallback(std::move(typeCallback)),
      m_declarationCallback(std::move(declarationCallback)),
      m_loggerCallback(std::move(loggerCallback)),
      m_mode(mode)
{
}

void OpenCL::Semantics::ConstantEvaluator::logError(std::string message, std::optional<Modifier> modifier)
{
    if (m_loggerCallback)
    {
        m_loggerCallback(std::move(message), std::move(modifier));
    }
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionDefined& node)
{
    return OpenCL::Semantics::ConstRetType();
}

OpenCL::Semantics::ConstRetType::ConstRetType(const OpenCL::Semantics::ConstRetType::ValueType& value,
                                              const OpenCL::Semantics::Type& type)
    : m_value(value), m_type(type.isUndefined() ? valueToType(m_value) : type)
{
}

OpenCL::Semantics::Type
    OpenCL::Semantics::ConstRetType::valueToType(const OpenCL::Semantics::ConstRetType::ValueType& value)
{
    return match(
        value, [](std::int8_t) { return PrimitiveType::createChar(false, false); },
        [](std::uint8_t) { return PrimitiveType::createUnsignedChar(false, false); },
        [](std::int16_t) { return PrimitiveType::createShort(false, false); },
        [](std::uint16_t) { return PrimitiveType::createUnsignedShort(false, false); },
        [](std::int32_t) { return PrimitiveType::createInt(false, false); },
        [](std::uint32_t) { return PrimitiveType::createUnsignedInt(false, false); },
        [](std::int64_t) { return PrimitiveType::createLongLong(false, false); },
        [](std::uint64_t) { return PrimitiveType::createUnsignedLongLong(false, false); },
        [](float) { return PrimitiveType::createFloat(false, false); },
        [](double) { return PrimitiveType::createDouble(false, false); }, [](auto&&) { return Type{}; });
}

template <class F>
OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::applyBinary(const OpenCL::Semantics::ConstRetType& rhs,
                                                                             F&& binaryOperator) const
{
    return match(
        m_value, [](VoidStar) -> ConstRetType { return {}; }, [](std::monostate) -> ConstRetType { return {}; },
        [&rhs, &binaryOperator](auto&& value) -> ConstRetType {
            return match(
                rhs.m_value, [](VoidStar) -> ConstRetType { return {}; },
                [](std::monostate) -> ConstRetType { return {}; },
                [value, &binaryOperator](auto&& otherValue) -> ConstRetType {
                    return {binaryOperator(value, otherValue)};
                });
        });
}

template <class F>
OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::applyIntegerBinary(const OpenCL::Semantics::ConstRetType& rhs,
                                                        F&& binaryOperator) const
{
    return match(
        m_value, [](VoidStar) -> ConstRetType { return {}; }, [](std::monostate) -> ConstRetType { return {}; },
        [](float) -> ConstRetType { return {}; }, [](double) -> ConstRetType { return {}; },
        [&rhs, &binaryOperator](auto&& value) -> ConstRetType {
            return match(
                rhs.m_value, [](VoidStar) -> ConstRetType { return {}; },
                [](std::monostate) -> ConstRetType { return {}; }, [](float) -> ConstRetType { return {}; },
                [](double) -> ConstRetType { return {}; },
                [value, &binaryOperator](auto&& otherValue) -> ConstRetType {
                    return {binaryOperator(value, otherValue)};
                });
        });
}

bool OpenCL::Semantics::ConstRetType::isInteger() const
{
    return std::visit(
        [](auto&& value) {
            using T = std::decay_t<decltype(value)>;
            return std::is_integral_v<T> || std::is_same_v<T, std::monostate>;
        },
        m_value);
}

bool OpenCL::Semantics::ConstRetType::isArithmetic() const
{
    return std::visit(
        [](auto&& value) {
            using T = std::decay_t<decltype(value)>;
            return std::is_arithmetic_v<T> || std::is_same_v<T, std::monostate>;
        },
        m_value);
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator+() const
{
    return {match(
        m_value, [](VoidStar) -> ConstRetType { return {}; }, [this](std::monostate) -> ConstRetType { return *this; },
        [](auto&& value) -> ConstRetType { return {+value}; })};
}

#pragma warning(push)
#pragma warning(disable : 4146)

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator-() const
{
    return {match(
        m_value, [](VoidStar) -> ConstRetType { return {}; }, [this](std::monostate) -> ConstRetType { return *this; },
        [](auto&& value) -> ConstRetType { return {-value}; })};
}

#pragma warning(pop)

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator!() const
{
    return {match(
        m_value, [](VoidStar value) -> ConstRetType { return {static_cast<std::int32_t>(!value.address)}; },
        [this](std::monostate) -> ConstRetType { return *this; },
        [](auto&& value) -> ConstRetType { return {static_cast<std::int32_t>(!value)}; })};
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator~() const
{
    return {match(
        m_value, [](VoidStar) -> ConstRetType { return {}; }, [this](std::monostate) -> ConstRetType { return *this; },
        [](float) -> ConstRetType { return {}; }, [](double) -> ConstRetType { return {}; },
        [](auto&& value) -> ConstRetType { return {~value}; })};
}

const OpenCL::Semantics::Type& OpenCL::Semantics::ConstRetType::getType() const
{
    return m_type;
}

const OpenCL::Semantics::ConstRetType::ValueType& OpenCL::Semantics::ConstRetType::getValue() const
{
    return m_value;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::castTo(const OpenCL::Semantics::Type& type) const
{
    return match(
        m_value, [this](std::monostate) -> ConstRetType { return *this; },
        [&type](VoidStar value) -> ConstRetType {
            return match(
                type.get(),
                [&type, value](const PointerType&) -> ConstRetType {
                    return {value, type};
                },
                [&type, value](const PrimitiveType& primitiveType) -> ConstRetType {
                    if (primitiveType.isFloatingPoint())
                    {
                        return {};
                    }
                    else
                    {
                        switch (primitiveType.getBitCount())
                        {
                            case 8:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int8_t>(value.address), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint8_t>(value.address), type};
                                }
                            }
                            case 16:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int16_t>(value.address), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint16_t>(value.address), type};
                                }
                            }
                            case 32:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int32_t>(value.address), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint32_t>(value.address), type};
                                }
                            }
                            case 64:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int64_t>(value.address), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint64_t>(value.address), type};
                                }
                            }
                        }
                    }
                    return {};
                },
                [](auto &&) -> ConstRetType { return {}; });
        },
        [&type](auto&& value) -> ConstRetType {
            return match(
                type.get(),
                [value, &type](const PrimitiveType& primitiveType) -> ConstRetType {
                    if (primitiveType.isFloatingPoint())
                    {
                        switch (primitiveType.getBitCount())
                        {
                            case 32: return {static_cast<float>(value), type};
                            case 64: return {static_cast<double>(value), type};
                        }
                    }
                    else
                    {
                        switch (primitiveType.getBitCount())
                        {
                            case 8:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int8_t>(value), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint8_t>(value), type};
                                }
                            }
                            case 16:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int16_t>(value), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint16_t>(value), type};
                                }
                            }
                            case 32:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int32_t>(value), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint32_t>(value), type};
                                }
                            }
                            case 64:
                            {
                                if (primitiveType.isSigned())
                                {
                                    return {static_cast<std::int64_t>(value), type};
                                }
                                else
                                {
                                    return {static_cast<std::uint64_t>(value), type};
                                }
                            }
                        }
                    }
                    return {};
                },
                [value, &type](const PointerType&) -> ConstRetType {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (std::is_integral_v<T>)
                    {
                        return {VoidStar{static_cast<std::uint64_t>(value)}, type};
                    }
                    else
                    {
                        (void)value;
                        (void)type;
                        return {};
                    }
                },
                [](auto &&) -> ConstRetType { return {}; });
        });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator*(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return lhs * rhs; });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator/(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return lhs / rhs; });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator%(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyIntegerBinary(rhs, [](auto lhs, auto rhs) { return lhs % rhs; });
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator*=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this * rhs;
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator/=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this / rhs;
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator%=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this % rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator+(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (isArithmetic() != rhs.isArithmetic())
    {
        // One is a pointer
        auto& ptr = rhs.isArithmetic() ? *this : rhs;
        auto& other = rhs.isArithmetic() ? rhs : *this;
        if (!other.isInteger())
        {
            return {};
        }
        auto size = sizeOf(std::get<PointerType>(ptr.getType().get()).getElementType());
        if (!size)
        {
            return {};
        }
        return {VoidStar{std::get<VoidStar>(ptr.getValue()).address + *size * other.to<int64_t>()}, ptr.getType()};
    }
    else
    {
        return applyBinary(rhs, [](auto lhs, auto rhs) { return lhs + rhs; });
    }
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator+=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this + rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator-(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && (rhs.isInteger() || !rhs.isArithmetic()))
    {
        auto size = sizeOf(std::get<PointerType>(getType().get()).getElementType());
        if (!size)
        {
            return {};
        }
        if (!rhs.isArithmetic())
        {
            return {static_cast<std::int64_t>(
                (std::get<VoidStar>(getValue()).address - std::get<VoidStar>(rhs.getValue()).address) / *size)};
        }
        if (!rhs.isInteger())
        {
            return {};
        }
        return {VoidStar{std::get<VoidStar>(getValue()).address - *size * rhs.template to<int64_t>()}, getType()};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return lhs - rhs; });
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator-=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this - rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator<<(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyIntegerBinary(rhs, [](auto lhs, auto rhs) { return lhs << rhs; });
}

OpenCL::Semantics::ConstRetType&
    OpenCL::Semantics::ConstRetType::operator<<=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this << rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator>>(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyIntegerBinary(rhs, [](auto lhs, auto rhs) { return lhs >> rhs; });
}

OpenCL::Semantics::ConstRetType&
    OpenCL::Semantics::ConstRetType::operator>>=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this >> rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator&(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyIntegerBinary(rhs, [](auto lhs, auto rhs) { return lhs & rhs; });
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator&=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this & rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator^(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyIntegerBinary(rhs, [](auto lhs, auto rhs) { return lhs ^ rhs; });
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator^=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this ^ rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator|(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyIntegerBinary(rhs, [](auto lhs, auto rhs) { return lhs | rhs; });
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator|=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this | rhs;
}

OpenCL::Semantics::ConstRetType::operator bool() const
{
    return match(
        m_value, [](std::monostate) { return false; }, [](VoidStar ptr) { return ptr.address != 0; },
        [](auto value) { return value != 0; });
}

bool OpenCL::Semantics::ConstRetType::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_value);
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::toBool() const
{
    return {*this ? 1 : 0};
}

// Clang
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
// GCC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
// MSVC
#pragma warning(push)
#pragma warning(disable : 4018)
#pragma warning(disable : 4389)

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator<(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && !rhs.isArithmetic())
    {
        return {static_cast<std::int32_t>(std::get<VoidStar>(getValue()).address
                                          < std::get<VoidStar>(rhs.getValue()).address)};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs < rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator>(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && !rhs.isArithmetic())
    {
        return {static_cast<std::int32_t>(std::get<VoidStar>(getValue()).address
                                          > std::get<VoidStar>(rhs.getValue()).address)};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs > rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator<=(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && !rhs.isArithmetic())
    {
        return {static_cast<std::int32_t>(std::get<VoidStar>(getValue()).address
                                          <= std::get<VoidStar>(rhs.getValue()).address)};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs <= rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator>=(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && !rhs.isArithmetic())
    {
        return {static_cast<std::int32_t>(std::get<VoidStar>(getValue()).address
                                          >= std::get<VoidStar>(rhs.getValue()).address)};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs >= rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator==(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && !rhs.isArithmetic())
    {
        return {static_cast<std::int32_t>(std::get<VoidStar>(getValue()).address
                                          == std::get<VoidStar>(rhs.getValue()).address)};
    }
    else if ((!isArithmetic() && rhs.isInteger()) || (isInteger() && !rhs.isArithmetic()))
    {
        auto& ptr = isArithmetic() ? rhs : *this;
        auto& null = isArithmetic() ? *this : rhs;
        if (null.to<std::int64_t>() != 0)
        {
            return {};
        }
        return {static_cast<std::int32_t>(std::get<VoidStar>(ptr.getValue()).address == 0)};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs == rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator!=(const OpenCL::Semantics::ConstRetType& rhs) const
{
    if (isUndefined() || rhs.isUndefined())
    {
        return {};
    }
    if (!isArithmetic() && !rhs.isArithmetic())
    {
        return {static_cast<std::int32_t>(std::get<VoidStar>(getValue()).address
                                          != std::get<VoidStar>(rhs.getValue()).address)};
    }
    else if ((!isArithmetic() && rhs.isInteger()) || (isInteger() && !rhs.isArithmetic()))
    {
        auto& ptr = isArithmetic() ? rhs : *this;
        auto& null = isArithmetic() ? *this : rhs;
        if (null.to<std::int64_t>() != 0)
        {
            return {};
        }
        return {static_cast<std::int32_t>(std::get<VoidStar>(ptr.getValue()).address != 0)};
    }
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs != rhs); });
}

#pragma GCC diagnostic pop
#pragma clang diagnostic pop
#pragma warning(pop)

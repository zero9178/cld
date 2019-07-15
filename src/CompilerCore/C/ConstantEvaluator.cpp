#include "ConstantEvaluator.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <stdexcept>
#include <utility>

#include "ErrorMessages.hpp"

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
                logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("String literals"),
                          m_exprStart, m_exprEnd, Modifier(node.begin(), node.end())});
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
    if (!value.isInteger() && m_integerOnly)
    {
        logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                  m_exprEnd, Modifier(node.begin() + 1, node.end())});
        return {};
    }
    switch (node.getAnOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
            logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                          '\'' + node.begin()->emitBack() + '\''),
                      node.begin(), node.end(), Modifier(node.begin(), node.begin() + 1)});
            return {};
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
            throw std::runtime_error("Not supported yet");
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            if (!value.isArithmetic())
            {
                logError({ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                              "+", value.getType().getFullFormattedTypeName()),
                          m_exprStart, m_exprEnd, Modifier(node.begin(), node.end(), Modifier::PointAtBeginning)});
                return {};
            }
            return +value;
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        {
            if (!value.isArithmetic())
            {
                logError({ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                              "-", value.getType().getFullFormattedTypeName()),
                          m_exprStart, m_exprEnd, Modifier(node.begin(), node.end(), Modifier::PointAtBeginning)});
                return {};
            }
            return -value;
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            if (!value.isInteger() && value.isArithmetic())
            {
                logError({ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                              "~", value.getType().getFullFormattedTypeName()),
                          m_exprStart, m_exprEnd, Modifier(node.begin(), node.end(), Modifier::PointAtBeginning)});
                return {};
            }
            return ~value;
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            if (!value.isArithmetic())
            {
                logError({ErrorMessages::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                              "!", value.getType().getFullFormattedTypeName()),
                          m_exprStart, m_exprEnd, Modifier(node.begin(), node.end(), Modifier::PointAtBeginning)});
                return {};
            }
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
                logError({size.error(), m_exprStart, m_exprEnd, Modifier(typeName->begin(), typeName->end())});
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
            auto type = m_typeCallback ? m_typeCallback(cast.first) : Type{};
            if (type.isUndefined())
            {
                return {};
            }
            if (auto* primitive = std::get_if<PrimitiveType>(&type.get());
                m_integerOnly && (!primitive || primitive->isFloatingPoint() || primitive->getBitCount() == 0))
            {
                logError({ErrorMessages::Semantics::CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION,
                          m_exprStart, m_exprEnd, Modifier(cast.first.begin(), cast.first.end())});
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
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getCastExpression().begin(), node.getCastExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
        }
        switch (op)
        {
            case Syntax::Term::BinaryDotOperator::BinaryMultiply:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "*", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
                    return {};
                }
                value *= other;
            }
            break;
            case Syntax::Term::BinaryDotOperator::BinaryDivide:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "/", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
                    return {};
                }
                value /= other;
            }
            break;
            case Syntax::Term::BinaryDotOperator::BinaryRemainder:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "%", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
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
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getTerm().begin(), node.getTerm().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
        }
        switch (op)
        {
            case Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "+", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
                    return {};
                }
                value += other;
            }
            break;
            case Syntax::AdditiveExpression::BinaryDashOperator::BinaryMinus:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "-", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
                    return {};
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
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getAdditiveExpression().begin(), node.getAdditiveExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
        }
        switch (op)
        {
            case Syntax::ShiftExpression::ShiftOperator::Left:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  "<<", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
                    return {};
                }
                value <<= other;
            }
            break;
            case Syntax::ShiftExpression::ShiftOperator::Right:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                                  ">>", value.getType().getFullFormattedTypeName(),
                                  other.getType().getFullFormattedTypeName()),
                              m_exprStart, m_exprEnd,
                              Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
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
    auto value = visit(node.getEqualityExpression());
    for (auto& exp : node.getOptionalEqualityExpressions())
    {
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getEqualityExpression().begin(), node.getEqualityExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                          "&", value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),
                      m_exprStart, m_exprEnd, Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
            return {};
        }
        value &= other;
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitXorExpression& node)
{
    auto value = visit(node.getBitAndExpression());
    for (auto& exp : node.getOptionalBitAndExpressions())
    {
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getBitAndExpression().begin(), node.getBitAndExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                          "^", value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),
                      m_exprStart, m_exprEnd, Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
            return {};
        }
        value ^= other;
    }
    return value;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitOrExpression& node)
{
    auto value = visit(node.getBitXorExpression());
    for (auto& exp : node.getOptionalBitXorExpressions())
    {
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getBitXorExpression().begin(), node.getBitXorExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            logError({ErrorMessages::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                          "|", value.getType().getFullFormattedTypeName(), other.getType().getFullFormattedTypeName()),
                      m_exprStart, m_exprEnd, Modifier(exp.begin() - 1, exp.begin(), Modifier::PointAtBeginning)});
            return {};
        }
        value |= other;
    }
    return value;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalAndExpression& node)
{
    auto value = visit(node.getBitOrExpression());
    for (auto& exp : node.getOptionalBitOrExpressions())
    {
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getBitOrExpression().begin(), node.getBitOrExpression().end())});
            return {};
        }
        value = value.isUndefined() ? value : value.toBool();
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
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
    auto value = visit(node.getAndExpression());
    for (auto& exp : node.getOptionalAndExpressions())
    {
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getAndExpression().begin(), node.getAndExpression().end())});
            return {};
        }
        value = value.isUndefined() ? value : value.toBool();
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
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
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(node.getShiftExpression().begin(), node.getShiftExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
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
        if (!value.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd,
                      Modifier(node.getRelationalExpression().begin(), node.getRelationalExpression().end())});
            return {};
        }
        auto other = visit(exp);
        if (!other.isInteger() && m_integerOnly)
        {
            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS, m_exprStart,
                      m_exprEnd, Modifier(exp.begin(), exp.end())});
            return {};
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
            logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("variable access"),
                      m_exprStart, m_exprEnd, Modifier(identifier.begin(), identifier.end())});
            return {};
        });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    return match(
        node, [this](auto&& value) -> ConstRetType { return visit(value); },
        [this](const Syntax::PostFixExpressionFunctionCall& call) -> ConstRetType {
            logError(
                {ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("function call"), m_exprStart,
                 m_exprEnd,
                 Modifier(Syntax::nodeFromNodeDerivedVariant(call.getPostFixExpression()).begin() + 1, call.end())});
            return {};
        },
        [this](const Syntax::PostFixExpressionIncrement& increment) -> ConstRetType {
            logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'++'"), m_exprStart,
                      m_exprEnd,
                      Modifier(Syntax::nodeFromNodeDerivedVariant(increment.getPostFixExpression()).begin() + 1,
                               increment.end())});
            return {};
        },
        [this](const Syntax::PostFixExpressionDecrement& decrement) -> ConstRetType {
            logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("'--'"), m_exprStart,
                      m_exprEnd,
                      Modifier(Syntax::nodeFromNodeDerivedVariant(decrement.getPostFixExpression()).begin() + 1,
                               decrement.end())});
            return {};
        },
        [this](const Syntax::PostFixExpressionTypeInitializer& initializer) -> ConstRetType {
            logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("initializer"), m_exprStart,
                      m_exprEnd, Modifier(initializer.begin(), initializer.end())});
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
        logError(
            {ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("','"), m_exprStart, m_exprEnd,
             Modifier(node.getAssignmentExpressions()[1].begin() - 1, node.getAssignmentExpressions()[1].begin())});
        return {};
    }
    return visit(node.getAssignmentExpressions()[0]);
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::AssignmentExpression& node)
{
    return match(
        node.getVariant(),
        [this](const Syntax::AssignmentExpressionAssignment& assignmentExpressionAssignment) -> ConstRetType {
            logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                          '\'' + [&assignmentExpressionAssignment]() -> std::string {
                              switch (assignmentExpressionAssignment.getAssignOperator())
                              {
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::NoOperator: return "=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::PlusAssign: return "+=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::MinusAssign: return "-=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::DivideAssign:
                                      return "/=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::MultiplyAssign:
                                      return "*=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::ModuloAssign:
                                      return "%=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign:
                                      return "<<=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::RightShiftAssign:
                                      return ">>=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::BitAndAssign:
                                      return "&=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::BitOrAssign: return "|=";
                                  case Syntax::AssignmentExpressionAssignment::AssignOperator::BitXorAssign:
                                      return "^=";
                              }
                              return "";
                          }() + '\''),
                      m_exprStart, m_exprEnd,
                      Modifier(assignmentExpressionAssignment.getAssignmentExpression().begin() - 1,
                               assignmentExpressionAssignment.getAssignmentExpression().begin())});
            return {};
        },
        [this](const Syntax::ConditionalExpression& conditionalExpression) -> ConstRetType {
            return visit(conditionalExpression);
        });
}

OpenCL::Semantics::ConstantEvaluator::ConstantEvaluator(
    std::vector<Lexer::Token>::const_iterator exprStart, std::vector<Lexer::Token>::const_iterator exprEnd,
    std::function<Type(const Syntax::TypeName&)> typeCallback,
    std::function<const DeclarationTypedefEnums*(const std::string&)> declarationCallback,
    std::function<void(const Message&)> loggerCallback, bool integerOnly)
    : m_exprStart(exprStart),
      m_exprEnd(exprEnd),
      m_typeCallback(std::move(typeCallback)),
      m_declarationCallback(std::move(declarationCallback)),
      m_loggerCallback(std::move(loggerCallback)),
      m_integerOnly(integerOnly)
{
}

void OpenCL::Semantics::ConstantEvaluator::logError(const OpenCL::Message& message)
{
    if (m_loggerCallback)
    {
        m_loggerCallback(message);
    }
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
        m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
        [](std::monostate) -> ConstRetType { return {}; },
        [&rhs, &binaryOperator](auto&& value) -> ConstRetType {
            return match(
                rhs.m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
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
        m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
        [](std::monostate) -> ConstRetType { return {}; }, [](float) -> ConstRetType { return {}; },
        [](double) -> ConstRetType { return {}; },
        [&rhs, &binaryOperator](auto&& value) -> ConstRetType {
            return match(
                rhs.m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
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
        m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
        [this](std::monostate) -> ConstRetType { return *this; },
        [](auto&& value) -> ConstRetType { return {+value}; })};
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator-() const
{
    return {match(
        m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
        [this](std::monostate) -> ConstRetType { return *this; },
        [](auto&& value) -> ConstRetType { return {-value}; })};
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator!() const
{
    return {match(
        m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
        [this](std::monostate) -> ConstRetType { return *this; },
        [](auto&& value) -> ConstRetType { return {static_cast<std::int32_t>(!value)}; })};
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstRetType::operator~() const
{
    return {match(
        m_value, [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
        [this](std::monostate) -> ConstRetType { return *this; }, [](float) -> ConstRetType { return {}; },
        [](double) -> ConstRetType { return {}; }, [](auto&& value) -> ConstRetType { return {~value}; })};
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
        [](void*) -> ConstRetType { throw std::runtime_error("Not implemented yet"); },
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
    return applyBinary(rhs, [](auto lhs, auto rhs) { return lhs + rhs; });
}

OpenCL::Semantics::ConstRetType& OpenCL::Semantics::ConstRetType::operator+=(const OpenCL::Semantics::ConstRetType& rhs)
{
    return *this = *this + rhs;
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator-(const OpenCL::Semantics::ConstRetType& rhs) const
{
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
        m_value, [](std::monostate) { return false; }, [](void* ptr) { return ptr != nullptr; },
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

#pragma GCC diagnostic push
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma clang diagnostic ignored "-Wsign-compare"

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator<(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs < rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator>(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs > rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator<=(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs <= rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator>=(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs >= rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator==(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs == rhs); });
}

OpenCL::Semantics::ConstRetType
    OpenCL::Semantics::ConstRetType::operator!=(const OpenCL::Semantics::ConstRetType& rhs) const
{
    return applyBinary(rhs, [](auto lhs, auto rhs) { return static_cast<std::int32_t>(lhs != rhs); });
}

#pragma GCC diagnostic pop
#pragma clang diagnostic pop

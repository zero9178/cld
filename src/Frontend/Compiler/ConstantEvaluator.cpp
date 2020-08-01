#include "ConstantEvaluator.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <stdexcept>
#include <utility>

#include "ErrorMessages.hpp"

namespace
{
cld::Semantics::Type getPtrdiffT(const cld::LanguageOptions& options)
{
    if (options.sizeOfVoidStar == 4)
    {
        if (options.sizeOfInt == 4)
        {
            return cld::Semantics::PrimitiveType::createInt(false, false, options);
        }
        if (options.sizeOfLong == 4)
        {
            return cld::Semantics::PrimitiveType::createLong(false, false, options);
        }
        if (options.sizeOfShort == 4)
        {
            return cld::Semantics::PrimitiveType::createShort(false, false, options);
        }
        CLD_UNREACHABLE;
    }
    if (options.sizeOfVoidStar == 8)
    {
        if (options.sizeOfInt == 8)
        {
            return cld::Semantics::PrimitiveType::createInt(false, false, options);
        }
        if (options.sizeOfLong == 8)
        {
            return cld::Semantics::PrimitiveType::createLong(false, false, options);
        }
        if (options.sizeOfShort == 8)
        {
            return cld::Semantics::PrimitiveType::createShort(false, false, options);
        }
        return cld::Semantics::PrimitiveType::createLongLong(false, false);
    }
    CLD_UNREACHABLE;
}
} // namespace

cld::Semantics::ConstRetType
    cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PrimaryExpressionConstant& node)
{
    return match(
        node.getValue(),
        [&node, this](const std::string&) -> Semantics::ConstRetType {
            log(Errors::Semantics::STRING_LITERALS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(node, m_sourceInterface,
                                                                                           node));
            return {};
        },
        [&node, this](const Lexer::NonCharString&) -> Semantics::ConstRetType {
            log(Errors::Semantics::STRING_LITERALS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(node, m_sourceInterface,
                                                                                           node));
            return {};
        },
        [this, &node](const llvm::APFloat& floating) -> Semantics::ConstRetType {
            switch (node.getType())
            {
                case Lexer::CToken::Type::Float: return {floating, PrimitiveType::createFloat(false, false)};
                case Lexer::CToken::Type::Double: return {floating, PrimitiveType::createDouble(false, false)};
                case Lexer::CToken::Type::LongDouble:
                    return {floating,
                            PrimitiveType::createLongDouble(false, false, m_sourceInterface.getLanguageOptions())};
                default: CLD_UNREACHABLE;
            }
        },
        [this, &node](const llvm::APSInt& integer) -> Semantics::ConstRetType {
            switch (node.getType())
            {
                case Lexer::CToken::Type::Int:
                    return {integer, PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions())};
                case Lexer::CToken::Type::UnsignedInt:
                    return {integer,
                            PrimitiveType::createUnsignedInt(false, false, m_sourceInterface.getLanguageOptions())};
                case Lexer::CToken::Type::Long:
                    return {integer, PrimitiveType::createLong(false, false, m_sourceInterface.getLanguageOptions())};
                case Lexer::CToken::Type::UnsignedLong:
                    return {integer,
                            PrimitiveType::createUnsignedLong(false, false, m_sourceInterface.getLanguageOptions())};
                case Lexer::CToken::Type::LongLong: return {integer, PrimitiveType::createLongLong(false, false)};
                case Lexer::CToken::Type::UnsignedLongLong:
                    return {integer, PrimitiveType::createUnsignedLongLong(false, false)};
                default: CLD_UNREACHABLE;
            }
        });
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PrimaryExpressionParenthese& node)
{
    return visit(node.getExpression());
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::UnaryExpressionPostFixExpression& node)
{
    return visit(node.getPostFixExpression());
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::UnaryExpressionUnaryOperator& node)
{
    auto value = visit(node.getCastExpression());
    if (value.isUndefined())
    {
        return value;
    }
    if (!value.isInteger() && m_mode == Integer)
    {
        log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
            *(node.begin() + 1), m_sourceInterface, std::forward_as_tuple(*(node.begin() + 1), *(node.end() - 1)),
            value.getType()));
        return {};
    }
    switch (node.getAnOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
            log(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(*node.begin(), m_sourceInterface,
                                                                             *node.begin()));
            return {};
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand: CLD_ASSERT(false && "Not supported yet");
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            if (!value.isArithmetic())
            {
                log(Errors::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                    *node.begin(), m_sourceInterface, *node.begin(), value.getType(),
                    std::forward_as_tuple(*(node.begin() + 1), *(node.end() - 1))));
                return {};
            }
            return value.unaryPlus(m_sourceInterface.getLanguageOptions());
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        {
            if (!value.isArithmetic())
            {
                log(Errors::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                    *node.begin(), m_sourceInterface, *node.begin(), value.getType(),
                    std::forward_as_tuple(*(node.begin() + 1), *(node.end() - 1))));
                return {};
            }
            return value.negate(m_sourceInterface.getLanguageOptions());
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            if (!value.isInteger())
            {
                log(Errors::Semantics::CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N.args(
                    *node.begin(), m_sourceInterface, *node.begin(), value.getType(),
                    std::forward_as_tuple(*(node.begin() + 1), *(node.end() - 1))));
                return {};
            }
            return value.bitwiseNegate(m_sourceInterface.getLanguageOptions());
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            return value.logicalNegate(m_sourceInterface.getLanguageOptions());
        }
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::UnaryExpressionSizeOf& node)
{
    return match(
        node.getVariant(),
        [this](const std::unique_ptr<Syntax::TypeName>& typeName) -> cld::Semantics::ConstRetType {
            CLD_ASSERT(m_typeCallback);
            auto type = m_typeCallback(*typeName);
            if (type.isUndefined())
            {
                // Here we rely on the implementation of the callback to log the error somehow
                return {};
            }
            CLD_ASSERT(m_typeInfoCallback);
            auto size = m_typeInfoCallback(TypeInfo::Size, type, {typeName->begin(), typeName->end()});
            if (!size)
            {
                log(size.error());
                return {};
            }
            return {llvm::APSInt(llvm::APInt(64, *size)), PrimitiveType::createUnsignedLongLong(false, false)};
        },
        [](auto &&) -> cld::Semantics::ConstRetType {
            CLD_ASSERT(false && "Not implemented yet");
            CLD_UNREACHABLE;
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::CastExpression& node)
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
                if (m_mode == Integer && (!primitive || primitive->isFloatingPoint() || primitive->getByteCount() == 0))
                {
                    log(Errors::Semantics::CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION.args(
                        cast.first, m_sourceInterface, cast.first));
                    return {};
                }
                else if (!value.isArithmetic())
                {
                    log(Errors::Semantics::INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args(
                        cast.first, m_sourceInterface, value.getType(), type, cast.first, *cast.second));
                    return {};
                }
            }
            else if (std::holds_alternative<PointerType>(type.get()))
            {
                if (m_mode != Initialization)
                {
                    log(Errors::Semantics::CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION.args(
                        cast.first, m_sourceInterface, cast.first));
                    return {};
                }
                else if (!value.isInteger() && value.isArithmetic())
                {
                    log(Errors::Semantics::INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args(
                        cast.first, m_sourceInterface, value.getType(), type, cast.first, *cast.second));
                    return {};
                }
            }
            else
            {
                log(Errors::Semantics::INVALID_CAST_FROM_TYPE_N_TO_TYPE_N.args(
                    cast.first, m_sourceInterface, value.getType(), type, cast.first, *cast.second));
                return {};
            }

            ConstRetType::Issues issues = ConstRetType::Issues::NoIssues;
            auto ret = value.castTo(type, m_sourceInterface.getLanguageOptions(), &issues);
            if (issues != ConstRetType::Issues::NoIssues)
            {
                log(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                    *cast.second, m_sourceInterface, value.toString(), type, *cast.second));
            }
            return ret;
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::Term& node)
{
    auto value = visit(node.getCastExpression());
    for (auto& [op, exp] : node.getOptionalCastExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getCastExpression(), m_sourceInterface, node.getCastExpression(), value.getType()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                              exp, other.getType()));
            return {};
        }
        switch (op)
        {
            case Syntax::Term::BinaryDotOperator::BinaryMultiply:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                        *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                        std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                    return {};
                }
                value.multiplyAssign(other, m_sourceInterface.getLanguageOptions());
            }
            break;
            case Syntax::Term::BinaryDotOperator::BinaryDivide:
            {
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                        *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                        std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                    return {};
                }
                value.divideAssign(other, m_sourceInterface.getLanguageOptions());
            }
            break;
            case Syntax::Term::BinaryDotOperator::BinaryRemainder:
            {
                if (!value.isInteger() || !other.isInteger())
                {
                    log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                        *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                        std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                    return {};
                }
                value.moduloAssign(other, m_sourceInterface.getLanguageOptions());
            }
            break;
        }
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::AdditiveExpression& node)
{
    auto value = visit(node.getTerm());
    for (auto& [op, exp] : node.getOptionalTerms())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getTerm(), m_sourceInterface, node.getTerm(), value.getType()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                              exp, other.getType()));
            return {};
        }
        switch (op)
        {
            case Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus:
            {
                if ((!value.isArithmetic() && !other.isArithmetic())
                    || (value.isArithmetic() != other.isArithmetic() && (!value.isInteger() && !other.isInteger())))
                {
                    log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                        *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                        std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                    return {};
                }
                if (value.isArithmetic() != other.isArithmetic())
                {
                    auto& ptr = value.isArithmetic() ? other : value;
                    auto& elementType = cld::get<PointerType>(ptr.getType().get()).getElementType();
                    CLD_ASSERT(m_typeInfoCallback);
                    auto result =
                        m_typeInfoCallback(TypeInfo::Size, elementType,
                                           value.isArithmetic() ? llvm::ArrayRef(exp.begin(), exp.end()) :
                                                                  llvm::ArrayRef(node.begin(), exp.begin() - 1));
                    if (!result)
                    {
                        auto loc = value.isArithmetic() ? std::forward_as_tuple(*exp.begin(), *(exp.end() - 1)) :
                                                          std::forward_as_tuple(*node.begin(), *(exp.begin() - 2));
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC.args(
                            loc, m_sourceInterface, elementType, loc, ptr.getType()));
                        return {};
                    }
                }
                value.plusAssign(other, m_sourceInterface.getLanguageOptions(),
                                 [&](const Type& type) { return *m_typeInfoCallback(TypeInfo::Size, type, {}); });
            }
            break;
            case Syntax::AdditiveExpression::BinaryDashOperator::BinaryMinus:
            {
                if ((value.isArithmetic() || !other.isInteger()) && (value.isArithmetic() != other.isArithmetic()))
                {
                    log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                        *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                        std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                    return {};
                }
                if (!value.isArithmetic() && !other.isArithmetic())
                {
                    if (cld::get<PointerType>(value.getType().get()).getElementType().get()
                        != cld::get<PointerType>(other.getType().get()).getElementType().get())
                    {
                        log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                            *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                            std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                        return {};
                    }
                }
                if (!value.isArithmetic() || !other.isArithmetic())
                {
                    auto& ptr = value.isArithmetic() ? other : value;
                    auto& elementType = cld::get<PointerType>(ptr.getType().get()).getElementType();
                    CLD_ASSERT(m_typeInfoCallback);
                    auto result =
                        m_typeInfoCallback(TypeInfo::Size, elementType,
                                           value.isArithmetic() ? llvm::ArrayRef(exp.begin(), exp.end()) :
                                                                  llvm::ArrayRef(node.begin(), exp.begin() - 1));
                    if (!result)
                    {
                        auto lhs = std::forward_as_tuple(*node.begin(), *(exp.begin() - 2));
                        auto rhs = std::forward_as_tuple(*exp.begin(), *(exp.end() - 1));
                        log(Errors::Semantics::INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC_2.args(
                            lhs, m_sourceInterface, elementType, lhs, value.getType(), rhs, other.getType()));
                        return {};
                    }
                }
                value.minusAssign(other, m_sourceInterface.getLanguageOptions(),
                                  [&](const Type& type) { return *m_typeInfoCallback(TypeInfo::Size, type, {}); });
            }
            break;
        }
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::ShiftExpression& node)
{
    auto value = visit(node.getAdditiveExpression());
    for (auto& [op, exp] : node.getOptionalAdditiveExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getAdditiveExpression(), m_sourceInterface, node.getAdditiveExpression(), value.getType()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                              exp, other.getType()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
            return {};
        }
        switch (op)
        {
            case Syntax::ShiftExpression::ShiftOperator::Left:
                value.shiftLeftAssign(other, m_sourceInterface.getLanguageOptions());
                break;
            case Syntax::ShiftExpression::ShiftOperator::Right:
                value.shiftRightAssign(other, m_sourceInterface.getLanguageOptions());
                break;
        }
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::BitAndExpression& node)
{
    auto value = visit(node.getEqualityExpressions()[0]);
    for (auto iter = node.getEqualityExpressions().cbegin() + 1; iter != node.getEqualityExpressions().cend(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getEqualityExpressions()[0], m_sourceInterface, node.getEqualityExpressions()[0],
                value.getType()));
            return {};
        }
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(*iter, m_sourceInterface,
                                                                                              *iter, other.getType()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                *(iter->begin() - 1), m_sourceInterface, *(iter->begin() - 1), value.getType(), other.getType(),
                std::forward_as_tuple(*node.begin(), *(iter->begin() - 2)), *iter));
            return {};
        }
        value.bitAndAssign(other, m_sourceInterface.getLanguageOptions());
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::BitXorExpression& node)
{
    auto value = visit(node.getBitAndExpressions()[0]);
    for (auto iter = node.getBitAndExpressions().begin() + 1; iter != node.getBitAndExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getBitAndExpressions()[0], m_sourceInterface, node.getBitAndExpressions()[0], value.getType()));
            return {};
        }
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(*iter, m_sourceInterface,
                                                                                              *iter, other.getType()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                *(iter->begin() - 1), m_sourceInterface, *(iter->begin() - 1), value.getType(), other.getType(),
                std::forward_as_tuple(*node.begin(), *(iter->begin() - 2)), *iter));
            return {};
        }
        value.bitXorAssign(other, m_sourceInterface.getLanguageOptions());
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::BitOrExpression& node)
{
    auto value = visit(node.getBitXorExpressions()[0]);
    for (auto iter = node.getBitXorExpressions().begin() + 1; iter != node.getBitXorExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getBitXorExpressions()[0], m_sourceInterface, node.getBitXorExpressions()[0], value.getType()));
            return {};
        }
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(*iter, m_sourceInterface,
                                                                                              *iter, other.getType()));
            return {};
        }
        if (!value.isInteger() || !other.isInteger())
        {
            log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                *(iter->begin() - 1), m_sourceInterface, *(iter->begin() - 1), value.getType(), other.getType(),
                std::forward_as_tuple(*node.begin(), *(iter->begin() - 2)), *iter));
            return {};
        }
        value.bitOrAssign(other, m_sourceInterface.getLanguageOptions());
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::LogicalAndExpression& node)
{
    auto value = visit(node.getBitOrExpressions()[0]);
    for (auto iter = node.getBitOrExpressions().begin() + 1; iter != node.getBitOrExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getBitOrExpressions()[0], m_sourceInterface, node.getBitOrExpressions()[0], value.getType()));
            return {};
        }
        value = value.isUndefined() ? value : value.toBool(m_sourceInterface.getLanguageOptions());
        auto other = visit(*iter);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(*iter, m_sourceInterface,
                                                                                              *iter, other.getType()));
            return {};
        }
        if (!value && !value.isUndefined())
        {
            break;
        }
        value = other.isUndefined() ? other : other.toBool(m_sourceInterface.getLanguageOptions());
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::LogicalOrExpression& node)
{
    auto value = visit(node.getAndExpressions()[0]);
    for (auto iter = node.getAndExpressions().begin() + 1; iter != node.getAndExpressions().end(); iter++)
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getAndExpressions()[0], m_sourceInterface, node.getAndExpressions()[0], value.getType()));
            return {};
        }
        value = value.isUndefined() ? value : value.toBool(m_sourceInterface.getLanguageOptions());
        auto other = visit(*iter);
        if (other.isUndefined() || other.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(*iter, m_sourceInterface,
                                                                                              *iter, other.getType()));
            return {};
        }
        if (value && !value.isUndefined())
        {
            break;
        }
        value = other.isUndefined() ? other : other.toBool(m_sourceInterface.getLanguageOptions());
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::ConditionalExpression& node)
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

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::RelationalExpression& node)
{
    auto value = visit(node.getShiftExpression());
    for (auto& [op, exp] : node.getOptionalShiftExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getShiftExpression(), m_sourceInterface, node.getShiftExpression(), value.getType()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                              exp, other.getType()));
            return {};
        }
        if (!value.isArithmetic() && !other.isArithmetic())
        {
            if (cld::get<PointerType>(value.getType().get()).getElementType().get()
                != cld::get<PointerType>(other.getType().get()).getElementType().get())
            {
                log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                    *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                    std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                return {};
            }
        }
        switch (op)
        {
            case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
            {
                value = value.greaterThan(other, m_sourceInterface.getLanguageOptions());
            }
            break;
            case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
            {
                value = value.greaterOrEqual(other, m_sourceInterface.getLanguageOptions());
            }
            break;
            case Syntax::RelationalExpression::RelationalOperator::LessThan:
            {
                value = value.lessThan(other, m_sourceInterface.getLanguageOptions());
            }
            break;
            case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
            {
                value = value.lessOrEqual(other, m_sourceInterface.getLanguageOptions());
            }
            break;
        }
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::EqualityExpression& node)
{
    auto value = visit(node.getRelationalExpression());
    for (auto& [op, exp] : node.getOptionalRelationalExpressions())
    {
        if (!value.isUndefined() && !value.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                node.getRelationalExpression(), m_sourceInterface, node.getRelationalExpression(), value.getType()));
            return {};
        }
        auto other = visit(exp);
        if (other.isUndefined() || value.isUndefined())
        {
            continue;
        }
        if (!other.isInteger() && m_mode == Integer)
        {
            log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                              exp, other.getType()));
            return {};
        }
        std::string opName = op == Syntax::EqualityExpression::EqualityOperator::Equal ? "==" : "!=";
        if (!value.isArithmetic() && !other.isArithmetic())
        {
            if (cld::get<PointerType>(value.getType().get()).getElementType().get()
                    != cld::get<PointerType>(other.getType().get()).getElementType().get()
                && !isVoid(cld::get<PointerType>(value.getType().get()).getElementType())
                && !isVoid(cld::get<PointerType>(other.getType().get()).getElementType()))
            {
                log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                    *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                    std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                return {};
            }
        }
        else if (!value.isArithmetic() || !other.isArithmetic())
        {
            if (!value.isInteger() && !other.isInteger())
            {
                log(Errors::Semantics::CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N.args(
                    *(exp.begin() - 1), m_sourceInterface, *(exp.begin() - 1), value.getType(), other.getType(),
                    std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)), exp));
                return {};
            }
            else
            {
                auto& null = value.isInteger() ? value : other;
                if (null.toUInt() != 0)
                {
                    auto loc = value.isInteger() ? std::forward_as_tuple(*node.begin(), *(exp.begin() - 2)) :
                                                   std::forward_as_tuple(*exp.begin(), *(exp.end() - 1));
                    log(Errors::Semantics::INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER.args(
                        loc, m_sourceInterface, loc));
                    return {};
                }
            }
        }
        switch (op)
        {
            case Syntax::EqualityExpression::EqualityOperator::Equal:
            {
                value = value.equal(other, m_sourceInterface.getLanguageOptions());
            }
            break;
            case Syntax::EqualityExpression::EqualityOperator::NotEqual:
            {
                value = value.notEqual(other, m_sourceInterface.getLanguageOptions());
            }
            break;
        }
    }
    return value;
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PostFixExpressionSubscript&)
{
    CLD_ASSERT(false && "Not implemented yet");
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PostFixExpressionArrow&)
{
    CLD_ASSERT(false && "Not implemented yet");
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PostFixExpressionDot&)
{
    CLD_ASSERT(false && "Not implemented yet");
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PrimaryExpression& node)
{
    return match(
        node, [this](auto&& value) -> ConstRetType { return visit(value); },
        [this](const Syntax::PrimaryExpressionIdentifier& identifier) -> ConstRetType {
            auto decl = m_identifierCallback ? m_identifierCallback(identifier.getIdentifier()) : ConstRetType{};
            if (!decl.isUndefined())
            {
                return decl;
            }
            log(Errors::Semantics::VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                identifier, m_sourceInterface, identifier));
            return {};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::PostFixExpression& node)
{
    return match(
        node, [this](auto&& value) -> ConstRetType { return visit(value); },
        [this](const Syntax::PostFixExpressionFunctionCall& call) -> ConstRetType {
            log(Errors::Semantics::FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(call, m_sourceInterface,
                                                                                         call));
            return {};
        },
        [this](const Syntax::PostFixExpressionIncrement& others) -> ConstRetType {
            log(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(*(others.end() - 1), m_sourceInterface,
                                                                             *(others.end() - 1)));
            return {};
        },
        [this](const Syntax::PostFixExpressionDecrement& others) -> ConstRetType {
            log(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(*(others.end() - 1), m_sourceInterface,
                                                                             *(others.end() - 1)));
            return {};
        },
        [this](const Syntax::PostFixExpressionTypeInitializer& initializer) -> ConstRetType {
            log(Errors::Semantics::INITIALIZER_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(initializer, m_sourceInterface,
                                                                                       initializer));
            return {};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::UnaryExpression& node)
{
    return cld::match(node, [this](auto&& value) -> cld::Semantics::ConstRetType { return visit(value); });
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::Expression& node)
{
    if (node.getAssignmentExpressions().size() > 1)
    {
        log(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
            *(node.getAssignmentExpressions()[1].begin() - 1), m_sourceInterface,
            *(node.getAssignmentExpressions()[1].begin() - 1)));
        return {};
    }
    return visit(node.getAssignmentExpressions()[0]);
}

cld::Semantics::ConstRetType cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::AssignmentExpression& node)
{
    for (auto& [op, cond] : node.getAssignments())
    {
        log(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(*(cond.begin() - 1), m_sourceInterface,
                                                                         *(cond.begin() - 1)));
    }
    if (!node.getAssignments().empty())
    {
        return {};
    }
    return visit(node.getConditionalExpression());
}

cld::Semantics::ConstantEvaluator::ConstantEvaluator(const SourceInterface& sourceInterface,
                                                     std::function<Type(const Syntax::TypeName&)> typeCallback,
                                                     std::function<ConstRetType(std::string_view)> identifierCallback,
                                                     std::function<TypeInfoCallback> typeInfoCallback,
                                                     std::function<void(const Message&)> loggerCallback, Mode mode)
    : m_sourceInterface(sourceInterface),
      m_typeCallback(std::move(typeCallback)),
      m_identifierCallback(std::move(identifierCallback)),
      m_typeInfoCallback(std::move(typeInfoCallback)),
      m_loggerCallback(std::move(loggerCallback)),
      m_mode(mode)
{
}

void cld::Semantics::ConstantEvaluator::log(const Message& message)
{
    if (m_loggerCallback)
    {
        m_loggerCallback(message);
    }
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstantEvaluator::visit(const cld::Syntax::UnaryExpressionDefined& defined)
{
    return m_identifierCallback(defined.getIdentifier());
}

cld::Semantics::ConstRetType::ConstRetType(const cld::Semantics::ConstRetType::ValueType& value,
                                           const cld::Semantics::Type& type)
    : m_value(value), m_type(type)
{
}

bool cld::Semantics::ConstRetType::isInteger() const
{
    return std::holds_alternative<std::monostate>(m_value) || std::holds_alternative<llvm::APSInt>(m_value);
}

bool cld::Semantics::ConstRetType::isArithmetic() const
{
    return !std::holds_alternative<VoidStar>(m_value);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::integerPromotion(const LanguageOptions& options) const
{
    return match(
        m_value, [this](VoidStar) -> ConstRetType { return *this; },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [this](const llvm::APFloat&) -> ConstRetType { return *this; },
        [this, &options](const llvm::APSInt& integer) -> ConstRetType {
            if (integer.getBitWidth() < options.sizeOfInt * 8u)
            {
                auto apsInt = integer.extend(options.sizeOfInt * 8);
                apsInt.setIsSigned(true);
                return {std::move(apsInt), PrimitiveType::createInt(false, false, options)};
            }
            return {std::move(integer), m_type};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::unaryPlus(const LanguageOptions& options) const
{
    return integerPromotion(options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::negate(const LanguageOptions& options) const
{
    auto temp = integerPromotion(options);
    return match(
        temp.getValue(), [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&temp](llvm::APFloat floating) -> ConstRetType {
            floating.changeSign();
            return {floating, temp.getType()};
        },
        [&temp](llvm::APSInt integer) -> ConstRetType {
            integer.negate();
            return {integer, temp.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::logicalNegate(const LanguageOptions& options) const
{
    return match(
        m_value,
        [&options](VoidStar address) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address.address == 0), false),
                    Semantics::PrimitiveType::createInt(false, false, options)};
        },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&options](const llvm::APFloat& floating) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, floating.isZero()), false),
                    Semantics::PrimitiveType::createInt(false, false, options)};
        },
        [&options](const llvm::APSInt& integer) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, integer == 0), false),
                    Semantics::PrimitiveType::createInt(false, false, options)};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitwiseNegate(const LanguageOptions& options) const
{
    auto temp = integerPromotion(options);
    return match(
        temp.getValue(), [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&temp](llvm::APSInt integer) -> ConstRetType {
            integer.flipAllBits();
            return {integer, temp.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::castTo(const cld::Semantics::Type& type,
                                                                  const LanguageOptions& options, Issues* issues) const
{
    auto copy = type.get();
    auto nonLvalue = Type(false, false, cld::to_string(type.getName()), std::move(copy));
    return match(
        m_value, [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&](VoidStar address) -> ConstRetType {
            return match(
                nonLvalue.get(), [](const auto&) -> ConstRetType { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) -> ConstRetType {
                    if (primitiveType.isFloatingPoint())
                    {
                        CLD_UNREACHABLE;
                    }
                    if (primitiveType.getBitCount() == 1)
                    {
                        auto result = toBool(options);
                        if (!std::holds_alternative<llvm::APSInt>(result.getValue()))
                        {
                            CLD_UNREACHABLE;
                        }
                        return {llvm::APSInt(cld::get<llvm::APSInt>(result.getValue()).zextOrTrunc(8)),
                                PrimitiveType::createUnderlineBool(false, false)};
                    }
                    if (issues
                        && (primitiveType.isSigned() ? llvm::APInt::getSignedMaxValue(primitiveType.getBitCount()) :
                                                       llvm::APInt::getMaxValue(primitiveType.getBitCount()))
                               .ugt(address.address))
                    {
                        *issues = Issues::NotRepresentable;
                    }
                    return {llvm::APSInt(llvm::APInt(primitiveType.getBitCount(), address.address),
                                         !primitiveType.isSigned()),
                            nonLvalue};
                },
                [&](const PointerType&) -> ConstRetType {
                    return {address, nonLvalue};
                });
        },
        [&](llvm::APFloat floating) -> ConstRetType {
            return match(
                nonLvalue.get(), [](const auto&) -> ConstRetType { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) mutable -> ConstRetType {
                    bool response;
                    llvm::APFloat::opStatus op;
                    if (primitiveType.isFloatingPoint())
                    {
                        switch (primitiveType.getBitCount())
                        {
                            case 32:
                                op = floating.convert(llvm::APFloat::IEEEsingle(), llvm::APFloat::rmNearestTiesToEven,
                                                      &response);
                                break;
                            case 64:
                                op = floating.convert(llvm::APFloat::IEEEdouble(), llvm::APFloat::rmNearestTiesToEven,
                                                      &response);
                                break;
                            case 80:
                                op = floating.convert(llvm::APFloat::x87DoubleExtended(),
                                                      llvm::APFloat::rmNearestTiesToEven, &response);
                                break;
                            case 128:
                                op = floating.convert(llvm::APFloat::IEEEquad(), llvm::APFloat::rmNearestTiesToEven,
                                                      &response);
                                break;
                            default: CLD_UNREACHABLE;
                        }
                        return {floating, nonLvalue};
                    }
                    if (primitiveType.getBitCount() == 1)
                    {
                        auto result = toBool(options);
                        if (!std::holds_alternative<llvm::APSInt>(result.getValue()))
                        {
                            CLD_UNREACHABLE;
                        }
                        return {llvm::APSInt(cld::get<llvm::APSInt>(result.getValue()).zextOrTrunc(8)),
                                PrimitiveType::createUnderlineBool(false, false)};
                    }

                    llvm::APSInt result(primitiveType.getBitCount(), !primitiveType.isSigned());
                    op = floating.convertToInteger(result, llvm::APFloat::rmNearestTiesToEven, &response);
                    if (issues && op == llvm::APFloat::opInvalidOp)
                    {
                        *issues = Issues::NotRepresentable;
                    }
                    return {result, nonLvalue};
                });
        },
        [&](const llvm::APSInt& integer) -> ConstRetType {
            return match(
                nonLvalue.get(), [](const auto&) -> ConstRetType { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) -> ConstRetType {
                    if (primitiveType.isFloatingPoint())
                    {
                        decltype(auto) semantics = [&primitiveType]() -> decltype(auto) {
                            switch (primitiveType.getBitCount())
                            {
                                case 32: return llvm::APFloat::IEEEsingle();
                                case 64: return llvm::APFloat::IEEEdouble();
                                case 80: return llvm::APFloat::x87DoubleExtended();
                                case 128: return llvm::APFloat::IEEEquad();
                                default: CLD_UNREACHABLE;
                            }
                        }();
                        // A 64 bit integer can always be correctly represented in 32 bit float
                        // Unless we add 16 bit floats we don't need to check for conversion errors
                        llvm::APFloat result(semantics);
                        result.convertFromAPInt(integer, integer.isSigned(), llvm::APFloat::rmNearestTiesToEven);
                        return {result, nonLvalue};
                    }
                    if (primitiveType.getBitCount() == 1)
                    {
                        auto result = toBool(options);
                        if (!std::holds_alternative<llvm::APSInt>(result.getValue()))
                        {
                            CLD_UNREACHABLE;
                        }
                        return {llvm::APSInt(cld::get<llvm::APSInt>(result.getValue()).zextOrTrunc(8)),
                                PrimitiveType::createUnderlineBool(false, false)};
                    }

                    if (issues)
                    {
                        auto apInt = primitiveType.isSigned() ?
                                         llvm::APInt::getSignedMaxValue(primitiveType.getBitCount()) :
                                         llvm::APInt::getMaxValue(primitiveType.getBitCount());
                        auto other = integer.extOrTrunc(
                            std::max<std::size_t>(primitiveType.getBitCount(), integer.getBitWidth()));
                        apInt = apInt.zextOrTrunc(
                            std::max<std::size_t>(primitiveType.getBitCount(), integer.getBitWidth()));
                        if (apInt.ult(other))
                        {
                            *issues = Issues::NotRepresentable;
                        }
                    }

                    auto apsInt = integer.extOrTrunc(primitiveType.getBitCount());
                    apsInt.setIsSigned(primitiveType.isSigned());
                    return {apsInt, nonLvalue};
                },
                [&](const PointerType&) -> ConstRetType {
                    return {VoidStar{integer.getZExtValue()}, nonLvalue};
                });
        });
}

std::pair<cld::Semantics::ConstRetType, cld::Semantics::ConstRetType>
    cld::Semantics::ConstRetType::arithmeticConversions(ConstRetType lhs, ConstRetType rhs,
                                                        const LanguageOptions& options)
{
    if (std::holds_alternative<std::monostate>(lhs.getValue()) || std::holds_alternative<VoidStar>(lhs.getValue())
        || std::holds_alternative<std::monostate>(rhs.getValue()) || std::holds_alternative<VoidStar>(rhs.getValue()))
    {
        return {std::move(lhs), std::move(rhs)};
    }
    lhs = lhs.integerPromotion(options);
    rhs = rhs.integerPromotion(options);
    if (rhs.getType() == lhs.getType())
    {
        return {std::move(lhs), std::move(rhs)};
    }
    if (std::holds_alternative<llvm::APFloat>(lhs.getValue()) || std::holds_alternative<llvm::APFloat>(rhs.getValue()))
    {
        if (std::holds_alternative<llvm::APFloat>(lhs.getValue())
            && std::holds_alternative<llvm::APFloat>(rhs.getValue()))
        {
            bool useless;
            auto lhsFloat = cld::get<llvm::APFloat>(lhs.getValue());
            auto rhsFloat = cld::get<llvm::APFloat>(rhs.getValue());

            auto leftBigger = llvm::APFloat::getSizeInBits(lhsFloat.getSemantics())
                              > llvm::APFloat::getSizeInBits(rhsFloat.getSemantics());
            const auto& biggerSemantics = leftBigger ? lhsFloat.getSemantics() : rhsFloat.getSemantics();
            lhsFloat.convert(biggerSemantics, llvm::APFloat::rmNearestTiesToEven, &useless);
            rhsFloat.convert(biggerSemantics, llvm::APFloat::rmNearestTiesToEven, &useless);
            auto biggerType = leftBigger ? lhs.getType() : rhs.getType();
            return {{lhsFloat, biggerType}, {rhsFloat, biggerType}};
        }
        auto& floating = std::holds_alternative<llvm::APFloat>(lhs.getValue()) ? lhs : rhs;
        const auto& semantics = cld::get<llvm::APFloat>(floating.getValue()).getSemantics();
        if (std::holds_alternative<llvm::APSInt>(lhs.getValue()))
        {
            auto& integer = cld::get<llvm::APSInt>(lhs.getValue());
            auto result = llvm::APFloat(semantics);
            result.convertFromAPInt(integer, integer.isSigned(), llvm::APFloat::rmNearestTiesToEven);
            lhs = {std::move(result), floating.getType()};
        }
        if (std::holds_alternative<llvm::APSInt>(rhs.getValue()))
        {
            auto& integer = cld::get<llvm::APSInt>(rhs.getValue());
            auto result = llvm::APFloat(semantics);
            result.convertFromAPInt(integer, integer.isSigned(), llvm::APFloat::rmNearestTiesToEven);
            rhs = {std::move(result), floating.getType()};
        }
        return {std::move(lhs), std::move(rhs)};
    }
    auto lhsInteger = cld::get<llvm::APSInt>(lhs.getValue());
    auto rhsInteger = cld::get<llvm::APSInt>(rhs.getValue());
    if (lhsInteger.isSigned() == rhsInteger.isSigned() || lhsInteger.getBitWidth() != rhsInteger.getBitWidth())
    {
        auto lhsBigger = lhsInteger.getBitWidth() > rhsInteger.getBitWidth();
        auto biggerBits = lhsBigger ? lhsInteger.getBitWidth() : rhsInteger.getBitWidth();
        auto sign = lhsBigger ? lhsInteger.isSigned() : rhsInteger.isSigned();
        auto& type = lhsBigger ? lhs.getType() : rhs.getType();
        lhsInteger = lhsInteger.extOrTrunc(biggerBits);
        rhsInteger = rhsInteger.extOrTrunc(biggerBits);
        lhsInteger.setIsSigned(sign);
        rhsInteger.setIsSigned(sign);
        return {{std::move(lhsInteger), type}, {std::move(rhsInteger), type}};
    }
    auto unsignedType = lhsInteger.isUnsigned() ? lhs.getType() : rhs.getType();
    lhsInteger.setIsSigned(false);
    rhsInteger.setIsSigned(false);
    return {{std::move(lhsInteger), unsignedType}, {std::move(rhsInteger), unsignedType}};
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::multiply(const cld::Semantics::ConstRetType& rhs,
                                                                    const LanguageOptions& options,
                                                                    Issues* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating * cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.smul_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.umul_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssues;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::divide(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options, Issues* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating / cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sdiv_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.udiv(cld::get<llvm::APSInt>(op2.getValue()));
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssues;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::modulo(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating * cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            auto apsInt = integer.isSigned() ? integer.srem(cld::get<llvm::APSInt>(op2.getValue())) :
                                               integer.urem(cld::get<llvm::APSInt>(op2.getValue()));
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::multiplyAssign(const cld::Semantics::ConstRetType& rhs,
                                                                           const LanguageOptions& options,
                                                                           Issues* issues)
{
    return *this = multiply(rhs, options, issues);
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::divideAssign(const cld::Semantics::ConstRetType& rhs,
                                                                         const LanguageOptions& options, Issues* issues)
{
    return *this = divide(rhs, options, issues);
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::moduloAssign(const cld::Semantics::ConstRetType& rhs,
                                                                         const LanguageOptions& options)
{
    return *this = modulo(rhs, options);
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstRetType::plus(const cld::Semantics::ConstRetType& rhs, const LanguageOptions& options,
                                       llvm::function_ref<std::size_t(const Type&)> sizeCallback, Issues* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &op1 = op1, &sizeCallback](VoidStar address) -> ConstRetType {
            if (!std::holds_alternative<llvm::APSInt>(op2.getValue()))
            {
                CLD_UNREACHABLE;
            }
            auto& integer = cld::get<llvm::APSInt>(op2.getValue());
            CLD_ASSERT(sizeCallback);
            auto size = sizeCallback(cld::get<PointerType>(op1.getType().get()).getElementType());
            if (integer.isUnsigned())
            {
                address.address += size * integer.getZExtValue();
            }
            else
            {
                address.address += static_cast<std::int64_t>(size) * integer.getSExtValue();
            }
            return {address, op1.getType()};
        },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating + cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues, &sizeCallback](const llvm::APSInt& integer) -> ConstRetType {
            if (std::holds_alternative<VoidStar>(op2.getValue()))
            {
                auto address = cld::get<VoidStar>(op2.getValue());
                CLD_ASSERT(sizeCallback);
                auto size = sizeCallback(cld::get<PointerType>(op2.getType().get()).getElementType());
                if (integer.isUnsigned())
                {
                    address.address += size * integer.getZExtValue();
                }
                else
                {
                    address.address += static_cast<std::int64_t>(size) * integer.getSExtValue();
                }
                return {address, op2.getType()};
            }
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sadd_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.uadd_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssues;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType&
    cld::Semantics::ConstRetType::plusAssign(const cld::Semantics::ConstRetType& rhs, const LanguageOptions& options,
                                             llvm::function_ref<std::size_t(const Type&)> sizeCallback, Issues* issues)
{
    return *this = plus(rhs, options, sizeCallback, issues);
}

cld::Semantics::ConstRetType
    cld::Semantics::ConstRetType::minus(const cld::Semantics::ConstRetType& rhs, const LanguageOptions& options,
                                        llvm::function_ref<std::size_t(const Type&)> sizeCallback, Issues* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &op1 = op1, &sizeCallback, &options](VoidStar address) -> ConstRetType {
            auto size = sizeCallback(cld::get<PointerType>(op1.getType().get()).getElementType());
            if (std::holds_alternative<VoidStar>(op2.getValue()))
            {
                return {llvm::APSInt(llvm::APInt(options.sizeOfVoidStar * 8,
                                                 (address.address - cld::get<VoidStar>(op2.getValue()).address) / size,
                                                 true),
                                     false),
                        getPtrdiffT(options)};
            }
            if (!std::holds_alternative<llvm::APSInt>(op2.getValue()))
            {
                CLD_UNREACHABLE;
            }
            auto& integer = cld::get<llvm::APSInt>(op2.getValue());
            if (integer.isUnsigned())
            {
                address.address -= size * integer.getZExtValue();
            }
            else
            {
                address.address -= static_cast<std::int64_t>(size) * integer.getSExtValue();
            }
            return {address, op1.getType()};
        },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating - cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.ssub_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.usub_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssues;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType&
    cld::Semantics::ConstRetType::minusAssign(const cld::Semantics::ConstRetType& rhs, const LanguageOptions& options,
                                              llvm::function_ref<std::size_t(const Type&)> sizeCallback, Issues* issues)
{
    return *this = minus(rhs, options, sizeCallback, issues);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::shiftLeft(const cld::Semantics::ConstRetType& rhs,
                                                                     const LanguageOptions& options,
                                                                     Issues* issues) const
{
    auto op1 = integerPromotion(options);
    auto op2 = rhs.integerPromotion(options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sshl_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.ushl_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssues;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::shiftLeftAssign(const cld::Semantics::ConstRetType& rhs,
                                                                            const LanguageOptions& options,
                                                                            Issues* issues)
{
    return *this = shiftLeft(rhs, options, issues);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::shiftRight(const cld::Semantics::ConstRetType& rhs,
                                                                      const LanguageOptions& options,
                                                                      Issues* issues) const
{
    auto op1 = integerPromotion(options);
    auto op2 = rhs.integerPromotion(options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            auto op2Integer = cld::get<llvm::APSInt>(op2.getValue());
            if (issues && (op2Integer.isSignBitSet() || op2Integer.getZExtValue() >= integer.getBitWidth()))
            {
                *issues = NotRepresentable;
            }
            return {integer >> static_cast<unsigned>(op2Integer.getZExtValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::shiftRightAssign(const cld::Semantics::ConstRetType& rhs,
                                                                             const LanguageOptions& options,
                                                                             Issues* issues)
{
    return *this = shiftRight(rhs, options, issues);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitAnd(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            return {integer & cld::get<llvm::APSInt>(op2.getValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::bitAndAssign(const cld::Semantics::ConstRetType& rhs,
                                                                         const LanguageOptions& options)
{
    return *this = bitAnd(rhs, options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitXor(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            return {integer ^ cld::get<llvm::APSInt>(op2.getValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::bitXorAssign(const cld::Semantics::ConstRetType& rhs,
                                                                         const LanguageOptions& options)
{
    return *this = bitXor(rhs, options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitOr(const cld::Semantics::ConstRetType& rhs,
                                                                 const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            return {integer | cld::get<llvm::APSInt>(op2.getValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType& cld::Semantics::ConstRetType::bitOrAssign(const cld::Semantics::ConstRetType& rhs,
                                                                        const LanguageOptions& options)
{
    return *this = bitOr(rhs, options);
}

cld::Semantics::ConstRetType::operator bool() const
{
    return match(
        m_value, [](VoidStar address) -> bool { return address.address != 0; },
        [](const llvm::APFloat& floating) -> bool { return floating.isNonZero(); },
        [](const llvm::APSInt& integer) -> bool { return !integer.isNullValue(); },
        [](std::monostate) -> bool { CLD_UNREACHABLE; });
}

bool cld::Semantics::ConstRetType::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_value);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::toBool(const LanguageOptions& options) const
{
    return notEqual(
        {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, 0), false), PrimitiveType::createInt(false, false, options)},
        options);
}

std::int64_t cld::Semantics::ConstRetType::toInt() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::int64_t { return pointer.address; },
        [](const llvm::APFloat& floating) -> std::int64_t {
            return static_cast<std::int64_t>(floating.convertToDouble());
        },
        [](const llvm::APSInt& integer) -> std::int64_t { return integer.getSExtValue(); },
        [](std::monostate) -> std::int64_t { CLD_UNREACHABLE; });
}

std::uint64_t cld::Semantics::ConstRetType::toUInt() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::uint64_t { return pointer.address; },
        [](const llvm::APFloat& floating) -> std::uint64_t {
            return static_cast<std::uint64_t>(floating.convertToDouble());
        },
        [](const llvm::APSInt& integer) -> std::uint64_t { return integer.getZExtValue(); },
        [](std::monostate) -> std::uint64_t { CLD_UNREACHABLE; });
}

std::string cld::Semantics::ConstRetType::toString() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::string { return "0x" + llvm::utohexstr(pointer.address); },
        [](const llvm::APFloat& floating) -> std::string {
            llvm::SmallString<20> result;
            floating.toString(result);
            return result.str();
        },
        [](const llvm::APSInt& integer) -> std::string {
            llvm::SmallString<20> result;
            integer.toString(result);
            return result.str();
        },
        [](std::monostate) -> std::string { CLD_UNREACHABLE; });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::lessThan(const cld::Semantics::ConstRetType& rhs,
                                                                    const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &options](VoidStar address) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8,
                                             address.address < cld::get<VoidStar>(op2.getValue()).address),
                                 false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APFloat& floating) -> ConstRetType {
            auto cmp = floating.compare(cld::get<llvm::APFloat>(op2.getValue()));
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, cmp == llvm::APFloat::cmpLessThan), false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APSInt& integer) -> ConstRetType {
            auto apsInt = integer < cld::get<llvm::APSInt>(op2.getValue());
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, apsInt), false),
                    PrimitiveType::createInt(false, false, options)};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::greaterThan(const cld::Semantics::ConstRetType& rhs,
                                                                       const LanguageOptions& options) const
{
    return rhs.lessThan(*this, options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::lessOrEqual(const cld::Semantics::ConstRetType& rhs,
                                                                       const LanguageOptions& options) const
{
    return rhs.lessThan(*this, options).logicalNegate(options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::greaterOrEqual(const cld::Semantics::ConstRetType& rhs,
                                                                          const LanguageOptions& options) const
{
    return lessThan(rhs, options).logicalNegate(options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::equal(const cld::Semantics::ConstRetType& rhs,
                                                                 const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &options](VoidStar address) -> ConstRetType {
            if (std::holds_alternative<llvm::APSInt>(op2.getValue()))
            {
                CLD_ASSERT(cld::get<llvm::APSInt>(op2.getValue()) == 0);
                return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address.address == 0), false),
                        PrimitiveType::createInt(false, false, options)};
            }
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8,
                                             address.address == cld::get<VoidStar>(op2.getValue()).address, true),
                                 false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APFloat& floating) -> ConstRetType {
            return {llvm::APSInt(
                        llvm::APInt(options.sizeOfInt * 8, floating.compare(cld::get<llvm::APFloat>(op2.getValue()))
                                                               == llvm::APFloat::cmpEqual),
                        false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APSInt& integer) -> ConstRetType {
            if (auto* address = std::get_if<VoidStar>(&op2.getValue()))
            {
                CLD_ASSERT(integer == 0);
                return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address->address == 0), false),
                        PrimitiveType::createInt(false, false, options)};
            }
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, integer == cld::get<llvm::APSInt>(op2.getValue())),
                                 false),
                    PrimitiveType::createInt(false, false, options)};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::notEqual(const cld::Semantics::ConstRetType& rhs,
                                                                    const LanguageOptions& options) const
{
    return equal(rhs, options).logicalNegate(options);
}

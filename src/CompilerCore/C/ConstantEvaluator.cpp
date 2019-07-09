#include "ConstantEvaluator.hpp"

#include "ErrorMessages.hpp"
#include <algorithm>
#include <stdexcept>
#include <CompilerCore/Common/Util.hpp>
#include <utility>

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
{
    return std::visit(
        [this, &node](auto&& value) -> Semantics::ConstRetType
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr (!std::is_same_v<T, std::string>)
            {
                return value;
            }
            else
            {
                logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args("String literals"),
                          node.begin(),
                          node.end(), Modifier(node.begin(), node.end())});
                return 0;
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
    auto value = visit(node.getUnaryExpression());
    auto applyUnary = [&](const ConstRetType& value, auto&& op)
    {
        return std::visit(
            [&](auto&& value) -> ConstRetType
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_arithmetic_v<T>)
                {
                    return op(-value);
                }
                else
                {

                    return 0;
                }
            }, value);
    };
    switch (node.getAnOperator())
    {
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        logError({ErrorMessages::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION
                      .args('\'' + node.begin()->emitBack() + '\''), node.begin(), node.end(),
                  Modifier(node.begin(), node.begin() + 1)});
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:return value;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
    {
        return std::visit(
            [](auto&& value) -> ConstRetType
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_arithmetic_v<T>)
                {
                    return -value;
                }
                else
                {

                    return 0;
                }
            }, value);
    }
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
    {
        return std::visit(
            [](auto&& value) -> ConstRetType
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (hasBitNegate < T > {})
                {
                    return Semantics::ConstRetType::ValueType(~value);
                }
                else
                {
                    return FailureReason("Can't apply - to constant operator");
                }
            }, value);
    }
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
    {
        return std::visit(
            [](auto&& value) -> ConstRetType
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (hasLogicNegate < T > {})
                {
                    return Semantics::ConstRetType::ValueType(!value);
                }
                else
                {
                    return FailureReason("Can't apply - to constant operator");
                }
            }, value);
    }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& node)
{
    return match(node.getVariant(),
                 [this](const std::unique_ptr<Syntax::TypeName>& typeName) -> OpenCL::Semantics::ConstRetType
                 {
                     //TODO:
                     //                     std::vector<Semantics::SpecifierQualifierRef> refs;
                     //                     for (auto& iter : typeName->getSpecifierQualifiers())
                     //                     {
                     //                         std::visit([&refs](auto&& value)
                     //                                    {
                     //                                        refs.emplace_back(std::cref(value));
                     //                                    }, iter);
                     //                     }
                     //                     auto type = Semantics::declaratorsToType(refs,
                     //                                                              typeName->getAbstractDeclarator(),
                     //                                                              m_typedefs,
                     //                                                              {},
                     //                                                              m_structOrUnions);
                     //                     auto result = Semantics::sizeOf(type);
                     //                     if (!result)
                     //                     {
                     //                         return result;
                     //                     }
                     //                     return *result;
                     throw std::runtime_error("Not implemented yet");
                 },
                 [](auto&&) -> OpenCL::Semantics::ConstRetType
                 { throw std::runtime_error("Not implemented yet"); });
}

namespace
{
    template <class T>
    OpenCL::Semantics::ConstRetType castVariant(const OpenCL::Semantics::ConstRetType& variant)
    {
        return std::visit(
            [](auto&& value) -> OpenCL::Semantics::ConstRetType
            {
                using U = std::decay_t<decltype(value)>;
                if constexpr (std::is_convertible_v<U, T>)
                {
                    return static_cast<T>(value);
                }
                else
                {
                    return OpenCL::FailureReason("Invalid constant cast");
                }
            },
            variant);
    }
} // namespace

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::CastExpression& node)
{
    return std::visit(
        [this](auto&& value) -> OpenCL::Semantics::ConstRetType
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<T, OpenCL::Syntax::UnaryExpression>)
            {
                return visit(value);
            }
            else
            {
                //                                  value.second->accept(*this);
                //                                  auto old = std::get<ConstRetType>(m_return);
                //                                  if (std::shared_ptr<Syntax::PrimitiveType>
                //                                          primitive =
                //                                          std::dynamic_pointer_cast<Syntax::PrimitiveType>(value
                //                                                                                                           .first);primitive)
                //                                  {
                //                                      if (primitive->isFloatingPoint())
                //                                      {
                //                                          if (primitive->getBitCount() == 32)
                //                                          {
                //                                              return castVariant<float>(old);
                //                                          }
                //                                          else if (primitive->getBitCount() == 64)
                //                                          {
                //                                              return castVariant<double>(old);
                //                                          }
                //                                          else
                //                                          {
                //                                              throw std::runtime_error("Invalid bitcount for floating
                //                                              point type");
                //                                          }
                //                                      }
                //                                      else
                //                                      {
                //                                          switch (primitive->getBitCount())
                //                                          {
                //                                          case 8:
                //                                              if (primitive->isSigned())
                //                                              {
                //                                                  return castVariant<std::int8_t>(old);
                //                                              }
                //                                              else
                //                                              {
                //                                                  return castVariant<std::uint8_t>(old);
                //                                              }
                //                                          case 16:
                //                                              if (primitive->isSigned())
                //                                              {
                //                                                  return castVariant<std::int16_t>(old);
                //                                              }
                //                                              else
                //                                              {
                //                                                  return castVariant<std::uint16_t>(old);
                //                                              }
                //                                          case 32:
                //                                              if (primitive->isSigned())
                //                                              {
                //                                                  return castVariant<std::int32_t>(old);
                //                                              }
                //                                              else
                //                                              {
                //                                                  return castVariant<std::uint32_t>(old);
                //                                              }
                //                                          case 64:
                //                                              if (primitive->isSigned())
                //                                              {
                //                                                  return castVariant<std::int64_t>(old);
                //                                              }
                //                                              else
                //                                              {
                //                                                  return castVariant<std::uint64_t>(old);
                //                                              }
                //                                          default:throw std::runtime_error("Invalid bitcount for
                //                                          integer type");
                //                                          }
                //                                      }
                //                                  }
                //                                  else
                {
                    throw std::runtime_error("Not implemented yet");
                }
            }
        },
        node.getVariant());
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::Term& node)
{
    if (node.getOptionalCastExpressions().empty())
    {
        return visit(node.getCastExpression());
    }
    auto value = visit(node.getCastExpression());
    for (auto&[op, exp] : node.getOptionalCastExpressions())
    {
        if (!value)
        {
            return value;
        }
        switch (op)
        {
        case Syntax::Term::BinaryDotOperator::BinaryMultiply:
        {
            value = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasMultiply < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs * rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *value);
        }
            break;
        case Syntax::Term::BinaryDotOperator::BinaryDivide:
        {
            value = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasDivide < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs / rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *value);
        }
            break;
        case Syntax::Term::BinaryDotOperator::BinaryRemainder:
        {
            value = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasModulo < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs % rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *value);
        }
            break;
        }
    }
    return value;
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::AdditiveExpression& node)
{
    if (node.getOptionalTerms().empty())
    {
        return visit(node.getTerm());
    }
    auto currentValue = visit(node.getTerm());
    for (auto&[op, exp] : node.getOptionalTerms())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        switch (op)
        {
        case Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasPlus < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs + rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        case Syntax::AdditiveExpression::BinaryDashOperator::BinaryMinus:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (!std::is_void_v<std::remove_pointer_t<
                                T1>> || !std::is_void_v<std::remove_pointer_t<T2>>)
                            {
                                if constexpr (hasMinus < T1, T2 > {})
                                {
                                    return ConstRetType::ValueType(lhs - rhs);
                                }
                                else
                                {
                                    return FailureReason("Can't apply minux to operands in constant expression");
                                }
                            }
                            else
                            {
                                return FailureReason("Can't apply minux to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::ShiftExpression& node)
{
    if (node.getOptionalAdditiveExpressions().empty())
    {
        return visit(node.getAdditiveExpression());
    }
    auto currentValue = visit(node.getAdditiveExpression());
    for (auto&[op, exp] : node.getOptionalAdditiveExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        switch (op)
        {
        case Syntax::ShiftExpression::ShiftOperator::Left:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return *second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasLShift < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs << rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        case Syntax::ShiftExpression::ShiftOperator::Right:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasRShift < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs >> rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitAndExpression& node)
{
    if (node.getOptionalEqualityExpressions().empty())
    {
        return visit(node.getEqualityExpression());
    }
    auto currentValue = visit(node.getEqualityExpression());
    for (auto& exp : node.getOptionalEqualityExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        currentValue = std::visit(
            [&exp, this](auto&& lhs) -> ConstRetType
            {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType
                    {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasBitAnd < T1, T2 > {})
                        {
                            return ConstRetType::ValueType(lhs & rhs);
                        }
                        else
                        {
                            return FailureReason("Can't apply plus to operands in constant expression");
                        }
                    },
                    *second);
            },
            *currentValue);
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitXorExpression& node)
{
    if (node.getOptionalBitAndExpressions().empty())
    {
        return visit(node.getBitAndExpression());
    }
    auto currentValue = visit(node.getBitAndExpression());
    for (auto& exp : node.getOptionalBitAndExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        currentValue = std::visit(
            [&exp, this](auto&& lhs) -> ConstRetType
            {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType
                    {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasBitXor < T1, T2 > {})
                        {
                            return ConstRetType::ValueType(lhs ^ rhs);
                        }
                        else
                        {
                            return FailureReason("Can't apply plus to operands in constant expression");
                        }
                    },
                    *second);
            },
            *currentValue);
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::BitOrExpression& node)
{
    if (node.getOptionalBitXorExpressions().empty())
    {
        return visit(node.getBitXorExpression());
    }
    auto currentValue = visit(node.getBitXorExpression());
    for (auto& exp : node.getOptionalBitXorExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        currentValue = std::visit(
            [&exp, this](auto&& lhs) -> ConstRetType
            {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (!second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType
                    {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasBitOr < T1, T2 > {})
                        {
                            return ConstRetType::ValueType(lhs | rhs);
                        }
                        else
                        {
                            return FailureReason("Can't apply plus to operands in constant expression");
                        }
                    },
                    *second);
            },
            *currentValue);
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalAndExpression& node)
{
    if (node.getOptionalBitOrExpressions().empty())
    {
        return visit(node.getBitOrExpression());
    }
    auto currentValue = visit(node.getBitOrExpression());
    for (auto& exp : node.getOptionalBitOrExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        currentValue = std::visit(
            [&exp, this](auto&& lhs) -> ConstRetType
            {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (!second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType
                    {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasLogicAnd < T1, T2 > {})
                        {
                            return ConstRetType::ValueType(lhs && rhs);
                        }
                        else
                        {
                            return FailureReason("Can't apply plus to operands in constant expression");
                        }
                    },
                    *second);
            },
            *currentValue);
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalOrExpression& node)
{
    if (node.getOptionalAndExpressions().empty())
    {
        return visit(node.getAndExpression());
    }
    auto currentValue = visit(node.getAndExpression());
    for (auto& exp : node.getOptionalAndExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        currentValue = std::visit(
            [&exp, this](auto&& lhs) -> ConstRetType
            {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (!second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType
                    {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasLogicOr < T1, T2 > {})
                        {
                            return ConstRetType::ValueType(lhs || rhs);
                        }
                        else
                        {
                            return FailureReason("Can't apply plus to operands in constant expression");
                        }
                    },
                    *second);
            },
            *currentValue);
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::ConditionalExpression& node)
{
    if (node.getOptionalExpression() && node.getOptionalConditionalExpression())
    {
        auto value = visit(node.getLogicalOrExpression());
        if (!value)
        {
            return value;
        }
        if (std::visit([](auto&& value) -> bool
                       { return value; }, *value))
        {
            return visit(*node.getOptionalExpression());
        }
        else
        {
            return visit(*node.getOptionalConditionalExpression());
        }
    }
    else
    {
        return visit(node.getLogicalOrExpression());
    }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::RelationalExpression& node)
{
    if (node.getOptionalShiftExpressions().empty())
    {
        return visit(node.getShiftExpression());
    }
    auto currentValue = visit(node.getShiftExpression());
    for (auto&[op, exp] : node.getOptionalShiftExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }
        switch (op)
        {
        case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasGT < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs > rhs);
                            }
                            else
                            {
                                return FailureReason("Can't > to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasGE < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs > rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply >= to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        case Syntax::RelationalExpression::RelationalOperator::LessThan:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasLT < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs < rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasLE < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs <= rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::EqualityExpression& node)
{
    if (node.getOptionalRelationalExpressions().empty())
    {
        return visit(node.getRelationalExpression());
    }
    auto currentValue = visit(node.getRelationalExpression());
    for (auto&[op, exp] : node.getOptionalRelationalExpressions())
    {
        if (!currentValue)
        {
            return currentValue;
        }

        switch (op)
        {
        case Syntax::EqualityExpression::EqualityOperator::Equal:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasEQ < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs == rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        case Syntax::EqualityExpression::EqualityOperator::NotEqual:
        {
            currentValue = std::visit(
                [exp = &exp, this](auto&& lhs) -> ConstRetType
                {
                    using T1 = std::decay_t<decltype(lhs)>;
                    auto second = visit(*exp);
                    if (!second)
                    {
                        return second;
                    }
                    return std::visit(
                        [lhs](auto&& rhs) -> ConstRetType
                        {
                            using T2 = std::decay_t<decltype(rhs)>;
                            if constexpr (hasNE < T1, T2 > {})
                            {
                                return ConstRetType::ValueType(lhs != rhs);
                            }
                            else
                            {
                                return FailureReason("Can't apply plus to operands in constant expression");
                            }
                        },
                        *second);
                },
                *currentValue);
        }
            break;
        }
    }
    return currentValue;
}

#pragma GCC diagnostic pop
#pragma GCC diagnostic pop

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionSubscript&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionArrow&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionDot&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    return match(node, [this](auto&& value) -> ConstRetType
                 { return visit(value); },
                 [](const Syntax::PrimaryExpressionIdentifier&) -> ConstRetType
                 {
                     return FailureReason("Identifier not allowed in constant expression");
                 });
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    return match(node, [this](auto&& value) -> ConstRetType
                 { return visit(value); },
                 [](const Syntax::PostFixExpressionFunctionCall&) -> ConstRetType
                 {
                     return FailureReason("Function call not allowed in constant expression");
                 },
                 [](const Syntax::PostFixExpressionIncrement&) -> ConstRetType
                 {
                     return FailureReason("Increment not allowed in constant expression");
                 },
                 [](const Syntax::PostFixExpressionDecrement&) -> ConstRetType
                 {
                     return FailureReason("Decrement not allowed in constant expression");
                 },
                 [](const Syntax::PostFixExpressionTypeInitializer&) -> ConstRetType
                 {
                     return FailureReason("Type initializer not allowed in constant expression");
                 });
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    return std::visit([this](auto&& value) -> OpenCL::Semantics::ConstRetType
                      { return visit(value); }, node);
}

OpenCL::Semantics::ConstRetType OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::Expression& node)
{
    if (node.getAssignmentExpressions().size() != 1)
    {
        return FailureReason(", operator not allowed in constant expression");
    }
    return visit(node.getAssignmentExpressions()[0]);
}

OpenCL::Semantics::ConstRetType
OpenCL::Semantics::ConstantEvaluator::visit(const OpenCL::Syntax::AssignmentExpression& node)
{
    return std::visit(
        [this](auto&& value) -> OpenCL::Semantics::ConstRetType
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<Syntax::ConditionalExpression, T>)
            {
                return visit(value);
            }
            else
            {
                return FailureReason("assignment not allowed in constant expression");
            }
        },
        node.getVariant());
}

OpenCL::Semantics::ConstantEvaluator::ConstantEvaluator(std::function<const RecordType*(const std::string&)> recordCallback,
                                                        std::function<const DeclarationTypedefEnums&(const std::string&)> declarationCallback,
                                                        std::function<void(const Message&)> loggerCallback,
                                                        bool integerOnly)
    : m_recordCallback(std::move(recordCallback)), m_declarationCallback(std::move(declarationCallback)),
      m_loggerCallback(std::move(loggerCallback)), m_integerOnly(integerOnly)
{}

void OpenCL::Semantics::ConstantEvaluator::logError(const OpenCL::Message& message)
{
    if (m_loggerCallback)
    {
        m_loggerCallback(message);
    }
}

OpenCL::Semantics::ConstRetType::ConstRetType(const OpenCL::Semantics::ConstRetType::ValueType& value) : m_value(value),
                                                                                                         m_type(
                                                                                                             valueToType(
                                                                                                                 m_value))
{}

OpenCL::Semantics::Type OpenCL::Semantics::ConstRetType::valueToType(const OpenCL::Semantics::ConstRetType::ValueType& value)
{
    return match(value, [](std::int8_t)
                 { return PrimitiveType::createChar(false, false); },
                 [](std::uint8_t)
                 { return PrimitiveType::createUnsignedChar(false, false); },
                 [](std::int16_t)
                 { return PrimitiveType::createShort(false, false); },
                 [](std::uint16_t)
                 { return PrimitiveType::createUnsignedShort(false, false); },
                 [](std::int32_t)
                 { return PrimitiveType::createInt(false, false); },
                 [](std::uint32_t)
                 { return PrimitiveType::createUnsignedInt(false, false); },
                 [](std::int64_t)
                 { return PrimitiveType::createLongLong(false, false); },
                 [](std::uint64_t)
                 { return PrimitiveType::createUnsignedLongLong(false, false); },
                 [](float)
                 { return PrimitiveType::createFloat(false, false); },
                 [](double)
                 { return PrimitiveType::createDouble(false, false); },
                 [](auto&&)
                 { return Type{}; });
}

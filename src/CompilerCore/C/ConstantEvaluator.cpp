#include "ConstantEvaluator.hpp"

namespace
{
    template <class, class, class = void>
    struct hasMultiply : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasMultiply<T1, T2, std::void_t<decltype(std::declval<T1>() * std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasDivide : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasDivide<T1, T2, std::void_t<decltype(std::declval<T1>() / std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasModulo : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasModulo<T1, T2, std::void_t<decltype(std::declval<T1>() % std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasPlus : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasPlus<T1, T2, std::void_t<decltype(std::declval<T1>() + std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasMinus : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasMinus<T1, T2, std::void_t<decltype(std::declval<T1>() - std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasRShift : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasRShift<T1, T2, std::void_t<decltype(std::declval<T1>() >> std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLShift : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLShift<T1, T2, std::void_t<decltype(std::declval<T1>() << std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLT : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLT<T1, T2, std::void_t<decltype(std::declval<T1>() < std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLE : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLE<T1, T2, std::void_t<decltype(std::declval<T1>() <= std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasGT : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasGT<T1, T2, std::void_t<decltype(std::declval<T1>() > std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasGE : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasGE<T1, T2, std::void_t<decltype(std::declval<T1>() >= std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasEQ : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasEQ<T1, T2, std::void_t<decltype(std::declval<T1>() == std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasNE : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasNE<T1, T2, std::void_t<decltype(std::declval<T1>() != std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasBitAnd : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasBitAnd<T1, T2, std::void_t<decltype(std::declval<T1>() & std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasBitXor : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasBitXor<T1, T2, std::void_t<decltype(std::declval<T1>() ^ std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasBitOr : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasBitOr<T1, T2, std::void_t<decltype(std::declval<T1>() | std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLogicAnd : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLogicAnd<T1, T2, std::void_t<decltype(std::declval<T1>() && std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLogicOr : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLogicOr<T1, T2, std::void_t<decltype(std::declval<T1>() || std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class = void>
    struct hasLogicNegate : std::false_type
    {
    };

    template <class T>
    struct hasLogicNegate<T, std::void_t<decltype(!std::declval<T>())>> : std::true_type
    {
    };

    template <class, class = void>
    struct hasBitNegate : std::false_type
    {
    };

    template <class T>
    struct hasBitNegate<T, std::void_t<decltype(~std::declval<T>())>> : std::true_type
    {
    };

    template <class, class = void>
    struct hasNegate : std::false_type
    {
    };

    template <class T>
    struct hasNegate<T, std::void_t<decltype(-std::declval<T>())>> : std::true_type
    {
    };
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Expression& node)
{
    if (node.getOptionalNonCommaExpression())
    {
        node.getOptionalNonCommaExpression()->accept(*this);
    }
    node.getNonCommaExpression().accept(*this);
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionIdentifier&)
{
    throw std::runtime_error("Identifier can't appear in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
{
    m_return = std::visit([](auto&& value) -> Codegen::ConstRetType
                          {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr(!std::is_same_v<T, std::string>)
                              {
                                  return value;
                              }
                              else
                              {
                                  throw std::runtime_error("Can't use string literal in constant expression");
                              }
                          }, node.getValue());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionParenthese& node)
{
    node.getExpression().accept(*this);
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    std::visit([this](auto&& value)
               {
                   value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression& node)
{
    node.getPrimaryExpression().accept(*this);
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionSubscript& )
{
    throw std::runtime_error("Not implemented yet");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionIncrement& )
{
    throw std::runtime_error("Post increment not allowed to occur in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionDecrement&)
{
    throw std::runtime_error("Post decrement not allowed to occur in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionDot& )
{
    throw std::runtime_error("Not implemented yet");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionArrow& )
{
    throw std::runtime_error("Not implemented yet");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionFunctionCall& )
{
    throw std::runtime_error("Function calls are not allowed in constant expressions");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer& )
{
    throw std::runtime_error("Type initializer are not allowed in constant expressions");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    std::visit([this](auto&& value)
    {
        value.accept(*this);
    },node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::AssignmentExpression& )
{
    throw std::runtime_error("Assignment expressios not allowed in constant expressions");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression& node)
{
    node.getPostFixExpression().accept(*this);
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
{
    node.getUnaryExpression().accept(*this);
    switch (node.getAnOperator())
    {
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        throw std::runtime_error("Unary Operator not allowed in constant expression");
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:return;
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
    {
        auto value = std::get<ConstRetType>(m_return);
        m_return = std::visit([](auto&& value) -> ConstRetType
                              {
                                  using T = std::decay_t<decltype(value)>;
                                  if constexpr(hasNegate<T>{})
                                  {
                                      return -value;
                                  }
                                  else
                                  {
                                      throw std::runtime_error("Can't apply - to constant operator");
                                      return {};
                                  }
                              }, value);
        return;
    }
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
    {
        auto value = std::get<ConstRetType>(m_return);
        m_return = std::visit([](auto&& value) -> ConstRetType
                              {
                                  using T = std::decay_t<decltype(value)>;
                                  if constexpr(hasBitNegate<T>{})
                                  {
                                      return ~value;
                                  }
                                  else
                                  {
                                      throw std::runtime_error("Can't apply - to constant operator");
                                      return {};
                                  }
                              }, value);
        return;
    }
    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
    {
        auto value = std::get<ConstRetType>(m_return);
        m_return = std::visit([](auto&& value) -> ConstRetType
                              {
                                  using T = std::decay_t<decltype(value)>;
                                  if constexpr(hasLogicNegate<T>{})
                                  {
                                      return !value;
                                  }
                                  else
                                  {
                                      throw std::runtime_error("Can't apply - to constant operator");
                                      return {};
                                  }
                              }, value);
    }
    }
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& )
{
    throw std::runtime_error("Not implemented yet");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    std::visit([this](auto&& value)
               {
        value.accept(*this);
               },node.getVariant());
}

namespace
{
    template <class T>
    OpenCL::Codegen::ConstRetType castVariant(const OpenCL::Codegen::ConstRetType& variant)
    {
        return std::visit([](auto&& value) -> OpenCL::Codegen::ConstRetType
                          {
                              using U = std::decay_t<decltype(value)>;
                              if constexpr(std::is_convertible_v<U, T>)
                              {
                                  return static_cast<T>(value);
                              }
                              else
                              {
                                  throw std::runtime_error("Invalid constant cast");
                              }
                              return {};
                          }, variant);
    }
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::CastExpression& node)
{
    m_return = std::visit([this](auto&& value) -> ConstRetType
                          {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr(std::is_same_v<T, OpenCL::Syntax::UnaryExpression>)
                              {
                                  value.accept(*this);
                                  return std::get<ConstRetType>(m_return);
                              }
                              else
                              {
                                  value.second->accept(*this);
                                  auto old = std::get<ConstRetType>(m_return);
                                  if (std::shared_ptr<Syntax::PrimitiveType>
                                          primitive = std::dynamic_pointer_cast<Syntax::PrimitiveType>(value
                                                                                                           .first);primitive)
                                  {
                                      if (primitive->isFloatingPoint())
                                      {
                                          if (primitive->getBitCount() == 32)
                                          {
                                              return castVariant<float>(old);
                                          }
                                          else if (primitive->getBitCount() == 64)
                                          {
                                              return castVariant<double>(old);
                                          }
                                          else
                                          {
                                              throw std::runtime_error("Invalid bitcount for floating point type");
                                          }
                                      }
                                      else
                                      {
                                          switch (primitive->getBitCount())
                                          {
                                          case 8:
                                              if (primitive->isSigned())
                                              {
                                                  return castVariant<std::int8_t>(old);
                                              }
                                              else
                                              {
                                                  return castVariant<std::uint8_t>(old);
                                              }
                                          case 16:
                                              if (primitive->isSigned())
                                              {
                                                  return castVariant<std::int16_t>(old);
                                              }
                                              else
                                              {
                                                  return castVariant<std::uint16_t>(old);
                                              }
                                          case 32:
                                              if (primitive->isSigned())
                                              {
                                                  return castVariant<std::int32_t>(old);
                                              }
                                              else
                                              {
                                                  return castVariant<std::uint32_t>(old);
                                              }
                                          case 64:
                                              if (primitive->isSigned())
                                              {
                                                  return castVariant<std::int64_t>(old);
                                              }
                                              else
                                              {
                                                  return castVariant<std::uint64_t>(old);
                                              }
                                          default:throw std::runtime_error("Invalid bitcount for integer type");
                                          }
                                      }
                                  }
                                  else
                                  {
                                      throw std::runtime_error("Not implemented yet");
                                  }
                              }
                          }, node.getUnaryOrCast());
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Term& node)
{
    node.getCastExpression().accept(*this);
    if (node.getOptionalCastExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto&[op, exp] : node.getOptionalCastExpressions())
    {
        switch (op)
        {
        case Syntax::Term::BinaryDotOperator::BinaryMultiply:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          exp->accept(*this);
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasMultiply<T1, T2>{})
                                                                {
                                                                    return lhs * rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::Term::BinaryDotOperator::BinaryDivide:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          exp->accept(*this);
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasDivide<T1, T2>{})
                                                                {
                                                                    return lhs / rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::Term::BinaryDotOperator::BinaryRemainder:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          exp->accept(*this);
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasModulo<T1, T2>{})
                                                                {
                                                                    return lhs % rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        }
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::AdditiveExpression& node)
{
    node.getTerm().accept(*this);
    if (node.getOptionalTerms().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto&[op, exp] : node.getOptionalTerms())
    {
        switch (op)
        {
        case Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          exp->accept(*this);
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasPlus<T1, T2>{})
                                                                {
                                                                    return lhs + rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::AdditiveExpression::BinaryDashOperator::BinaryMinus:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(!std::is_void_v<std::remove_pointer_t<T1>>
                                                                    || !std::is_void_v<std::remove_pointer_t<T2>>)
                                                                {
                                                                    if constexpr(hasMinus<T1, T2>{})
                                                                    {
                                                                        return lhs - rhs;
                                                                    }
                                                                    else
                                                                    {
                                                                        throw std::runtime_error(
                                                                            "Can't apply plus to operands in constant expression");
                                                                        return {};
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        }
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ShiftExpression& node)
{
    node.getAdditiveExpression().accept(*this);
    if (node.getOptionalAdditiveExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto&[op, exp] : node.getOptionalAdditiveExpressions())
    {
        switch (op)
        {
        case Syntax::ShiftExpression::ShiftOperator::Left:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          exp->accept(*this);
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasLShift<T1, T2>{})
                                                                {
                                                                    return lhs << rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::ShiftExpression::ShiftOperator::Right:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasRShift<T1, T2>{})
                                                                {
                                                                    return lhs >> rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        }
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::BitAndExpression& node)
{
    node.getEqualityExpression().accept(*this);
    if (node.getOptionalEqualityExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto& exp : node.getOptionalEqualityExpressions())
    {
        currentValue = std::visit([&exp, this](auto&& lhs) -> ConstRetType
                                  {
                                      exp.accept(*this);
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasBitAnd<T1, T2>{})
                                                            {
                                                                return lhs & rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, std::get<ConstRetType>(m_return));
                                  }, currentValue);
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::BitXorExpression& node)
{
    node.getBitAndExpression().accept(*this);
    if (node.getOptionalBitAndExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto& exp : node.getOptionalBitAndExpressions())
    {
        currentValue = std::visit([&exp, this](auto&& lhs) -> ConstRetType
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      exp.accept(*this);
                                      return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasBitXor<T1, T2>{})
                                                            {
                                                                return lhs ^ rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, std::get<ConstRetType>(m_return));
                                  }, currentValue);
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::BitOrExpression& node)
{
    node.getBitXorExpression().accept(*this);
    if (node.getOptionalBitXorExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto& exp : node.getOptionalBitXorExpressions())
    {
        currentValue = std::visit([&exp, this](auto&& lhs) -> ConstRetType
                                  {
                                      exp.accept(*this);
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasBitOr<T1, T2>{})
                                                            {
                                                                return lhs | rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, std::get<ConstRetType>(m_return));
                                  }, currentValue);
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalAndExpression& node)
{
    node.getBitOrExpression().accept(*this);
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto& exp : node.getOptionalBitOrExpressions())
    {
        currentValue = std::visit([&exp, this](auto&& lhs) -> ConstRetType
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      exp.accept(*this);
                                      return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasLogicAnd<T1, T2>{})
                                                            {
                                                                return lhs && rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, std::get<ConstRetType>(m_return));
                                  }, currentValue);
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalOrExpression& node)
{
    node.getAndExpression().accept(*this);
    if (node.getOptionalAndExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto& exp : node.getOptionalAndExpressions())
    {
        currentValue = std::visit([&exp, this](auto&& lhs) -> ConstRetType
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      exp.accept(*this);
                                      return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasLogicOr<T1, T2>{})
                                                            {
                                                                return lhs || rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, std::get<ConstRetType>(m_return));
                                  }, currentValue);
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ConditionalExpression& node)
{
    node.getLogicalOrExpression().accept(*this);
    if (node.getOptionalExpression() && node.getOptionalConditionalExpression())
    {
        auto value = std::get<ConstRetType>(m_return);
        bool first = std::visit([](auto&& value) -> bool
                                { return value; }, value);
        if (first)
        {
            node.getOptionalExpression()->accept(*this);
        }
        else
        {
            node.getOptionalConditionalExpression()->accept(*this);
        }
    }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::RelationalExpression& node)
{
    node.getShiftExpression().accept(*this);
    if (node.getOptionalShiftExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto&[op, exp] : node.getOptionalShiftExpressions())
    {
        switch (op)
        {
        case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasGT<T1, T2>{})
                                                                {
                                                                    return lhs > rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasGE<T1, T2>{})
                                                                {
                                                                    return lhs > rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::RelationalExpression::RelationalOperator::LessThan:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasLT<T1, T2>{})
                                                                {
                                                                    return lhs < rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasLE<T1, T2>{})
                                                                {
                                                                    return lhs <= rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        }
    }
    m_return = currentValue;
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::EqualityExpression& node)
{
    node.getRelationalExpression().accept(*this);
    if (node.getOptionalRelationalExpressions().empty())
    {
        return;
    }
    auto currentValue = std::get<ConstRetType>(m_return);
    for (auto&[op, exp] : node.getOptionalRelationalExpressions())
    {
        switch (op)
        {
        case Syntax::EqualityExpression::EqualityOperator::Equal:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasEQ<T1, T2>{})
                                                                {
                                                                    return lhs == rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        case Syntax::EqualityExpression::EqualityOperator::NotEqual:
        {
            currentValue = std::visit([exp = &exp, this](auto&& lhs) -> ConstRetType
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          exp->accept(*this);
                                          return std::visit([lhs](auto&& rhs) -> ConstRetType
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasNE<T1, T2>{})
                                                                {
                                                                    return lhs != rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, std::get<ConstRetType>(m_return));
                                      }, currentValue);
        }
            break;
        }
    }
    m_return = currentValue;
}

#pragma GCC diagnostic pop
#pragma GCC diagnostic pop

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::NonCommaExpression& node)
{
    std::visit([this](auto&& value)
               {
        value.accept(*this);
               },node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ReturnStatement& )
{
    throw std::runtime_error("Return statement not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ExpressionStatement& )
{
    throw std::runtime_error("Expression statement not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::IfStatement& )
{
    throw std::runtime_error("If not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::SwitchStatement& )
{
    throw std::runtime_error("switch not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::DefaultStatement& )
{
    throw std::runtime_error("default not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::CaseStatement& )
{
    throw std::runtime_error("case not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::BlockStatement& )
{
    throw std::runtime_error("block not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ForStatement& )
{
    throw std::runtime_error("for loop not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::InitializerListScalarExpression& )
{
    throw std::runtime_error("scalar initializer not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::InitializerListBlock& )
{
    throw std::runtime_error("initializer list not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::InitializerList& node)
{
    std::visit([this](auto&& value)
               {
        value.accept(*this);
        },node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Declarations&)
{
    throw std::runtime_error("Declartaion not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::BlockItem& node)
{
    std::visit([this](auto&& value)
               {
        value.accept(*this);
               },node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ForDeclarationStatement& )
{
    throw std::runtime_error("For loop not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::HeadWhileStatement& )
{
    throw std::runtime_error("while loop not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::FootWhileStatement& )
{
    throw std::runtime_error("do while loop not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::BreakStatement& )
{
    throw std::runtime_error("break not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ContinueStatement& )
{
    throw std::runtime_error("continue not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Statement& node)
{
    std::visit([this](auto&& value)
               {
        value.accept(*this);
               },node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::StructOrUnionDeclaration&)
{
    throw std::runtime_error("struct or union declaration not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::EnumDeclaration& )
{
    throw std::runtime_error("Enum declartaion not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::TypedefDeclaration& )
{
    throw std::runtime_error("Typedef declaration not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Function&)
{
    throw std::runtime_error("Function declaration or definition not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::GlobalDeclaration& )
{
    throw std::runtime_error("Global declaration not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Global& node)
{
    std::visit([this](auto&& value)
               {
        value.accept(*this);
               },node.getVariant());
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::Program&)
{
    throw std::runtime_error("Program not allowed in constant expressions");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PrimitiveType& )
{
    throw std::runtime_error("Type not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::PointerType& )
{
    throw std::runtime_error("Type not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::ArrayType& )
{
    throw std::runtime_error("Type not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::StructType& )
{
    throw std::runtime_error("Type not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::UnionType& )
{
    throw std::runtime_error("Type not allowed in constant expression");
}

void OpenCL::Codegen::ConstantEvaluator::visit(const OpenCL::Syntax::EnumType& )
{
    throw std::runtime_error("Type not allowed in constant expression");
}

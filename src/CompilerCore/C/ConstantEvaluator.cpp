#include "ConstantEvaluator.hpp"

#include <algorithm>

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
} // namespace

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
{
    return std::visit(
        [](auto&& value) -> Constant::ConstRetType {
            using T = std::decay_t<decltype(value)>;
            if constexpr (!std::is_same_v<T, std::string>)
            {
                return Constant::ConstRetType::ValueType(value);
            }
            else
            {
                return FailureReason("Can't use string literal in constant expression");
            }
        },
        node.getValue());
}

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpressionParenthese& node)
{
    return visit(node.getExpression());
}

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression& node)
{
    return visit(node.getPostFixExpression());
}

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
{
    auto value = visit(node.getUnaryExpression());
    if (!value)
    {
        return value;
    }
    switch (node.getAnOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
            return FailureReason("Unary Operator not allowed in constant expression");
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus: return value;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        {
            return std::visit(
                [](auto&& value) -> ConstRetType {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (hasNegate<T>{})
                    {
                        return Constant::ConstRetType::ValueType(-value);
                    }
                    else
                    {
                        return FailureReason("Can't apply - to constant operator");
                    }
                },
                *value);
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            return std::visit(
                [](auto&& value) -> ConstRetType {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (hasBitNegate<T>{})
                    {
                        return Constant::ConstRetType::ValueType(~value);
                    }
                    else
                    {
                        return FailureReason("Can't apply - to constant operator");
                    }
                },
                *value);
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            return std::visit(
                [](auto&& value) -> ConstRetType {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (hasLogicNegate<T>{})
                    {
                        return Constant::ConstRetType::ValueType(!value);
                    }
                    else
                    {
                        return FailureReason("Can't apply - to constant operator");
                    }
                },
                *value);
        }
    }
    return value;
}

// namespace
//{
//    std::size_t getAlignment(const std::shared_ptr<OpenCL::Syntax::IType>& ptr,
//                             const std::map<std::string, const OpenCL::Syntax::StructOrUnionDeclaration*>& map)
//    {
//        if (auto primitives = std::dynamic_pointer_cast<OpenCL::Syntax::PrimitiveType>(ptr))
//        {
//            return primitives->getBitCount() / 8;
//        }
//        else if (std::dynamic_pointer_cast<OpenCL::Syntax::PointerType>(ptr))
//        {
//            return 8;
//        }
//        else if (std::dynamic_pointer_cast<OpenCL::Syntax::EnumType>(ptr))
//        {
//            return 4;
//        }
//        else if (auto array = std::dynamic_pointer_cast<OpenCL::Syntax::ArrayType>(ptr))
//        {
//            return getAlignment(array->getType()->clone(), map);
//        }
//        else if (auto structType = std::dynamic_pointer_cast<OpenCL::Syntax::StructType>(ptr))
//        {
//            auto result = map.find(structType->getName());
//            if (result == map.end() || result->second->isUnion())
//            {
//                throw std::runtime_error("Unknown struct of name " + structType->getName() + " inside of sizeof");
//            }
//            std::size_t currentAlignment = 0;
//            for (auto& iter : result->second->getTypes())
//            {
//                currentAlignment = std::max(getAlignment(iter.first, map), currentAlignment);
//            }
//            return currentAlignment;
//        }
//        else
//        {
//            auto unionType = std::dynamic_pointer_cast<OpenCL::Syntax::UnionType>(ptr);
//            auto result = map.find(unionType->getName());
//            if (result == map.end() || !result->second->isUnion())
//            {
//                throw std::runtime_error("Unknown struct of name " + structType->getName() + " inside of sizeof");
//            }
//            std::vector<std::pair<std::size_t, std::shared_ptr<OpenCL::Syntax::IType>>> sizes;
//            std::transform(result->second->getTypes().begin(),
//                           result->second->getTypes().end(),
//                           std::back_inserter(sizes),
//                           [&map](const std::pair<std::shared_ptr<OpenCL::Syntax::IType>,
//                                                  std::string>& pair) -> std::pair<std::size_t,
//                                                                                   std::shared_ptr<OpenCL::Syntax::IType>>
//                           {
//                               return {getAlignment(pair.first, map), pair.first};
//                           });
//            return getAlignment(std::max_element(sizes.begin(), sizes.end())->second, map);
//        }
//    }
//}

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpressionSizeOf&)
{
    //    auto sizeOf = [this](const std::shared_ptr<Syntax::IType>& ptr, auto&& self) -> OpenCL::Constant::ConstRetType
    //    {
    //        if (auto primitives = std::dynamic_pointer_cast<Syntax::PrimitiveType>(ptr))
    //        {
    //            return primitives->getBitCount() / 8;
    //        }
    //        else if (std::dynamic_pointer_cast<Syntax::PointerType>(ptr))
    //        {
    //            return 8;
    //        }
    //        else if (std::dynamic_pointer_cast<Syntax::EnumType>(ptr))
    //        {
    //            return 4;
    //        }
    //        else if (auto array = std::dynamic_pointer_cast<Syntax::ArrayType>(ptr))
    //        {
    //            return std::visit([&array](auto&& value) -> ConstRetType
    //                              {
    //                                  using T = std::decay_t<decltype(value)>;
    //                                  if constexpr(hasMultiply<std::size_t, T>{})
    //                                  {
    //                                      return array->getSize() * value;
    //                                  }
    //                                  else
    //                                  {
    //                                      throw std::runtime_error("Invalid operands for multiply in constant
    //                                      expression");
    //                                  }
    //                                  return {};
    //                              }, self(array->getType()->clone(), self));
    //        }
    //        else if (auto structType = std::dynamic_pointer_cast<Syntax::StructType>(ptr))
    //        {
    //            auto result = m_structOrUnions.find(structType->getName());
    //            if (result == m_structOrUnions.end() || result->second->isUnion())
    //            {
    //                throw std::runtime_error("Unknown struct of name " + structType->getName() + " inside of sizeof");
    //            }
    //            std::size_t currentSize = 0;
    //            for (auto& iter : result->second->getTypes())
    //            {
    //                auto alignment = getAlignment(iter.first, m_structOrUnions);
    //                auto rest = currentSize % alignment;
    //                if (rest != 0)
    //                {
    //                    currentSize += alignment - rest;
    //                }
    //                currentSize += std::visit([](auto&& value) -> std::size_t
    //                                          {
    //                                              using T = std::decay_t<decltype(value)>;
    //                                              if constexpr(std::is_convertible_v<T, std::size_t>)
    //                                              {
    //                                                  return value;
    //                                              }
    //                                              else
    //                                              {
    //                                                  throw std::runtime_error("Size returned as void*");
    //                                              }
    //                                              return {};
    //                                          }, self(iter.first, self));
    //            }
    //            return currentSize;
    //        }
    //        else
    //        {
    //            auto unionType = std::dynamic_pointer_cast<Syntax::UnionType>(ptr);
    //            auto result = m_structOrUnions.find(unionType->getName());
    //            if (result == m_structOrUnions.end() || !result->second->isUnion())
    //            {
    //                throw std::runtime_error("Unknown struct of name " + structType->getName() + " inside of sizeof");
    //            }
    //            std::vector<std::size_t> sizes;
    //            std::transform(result->second->getTypes().begin(),
    //                           result->second->getTypes().end(),
    //                           std::back_inserter(sizes),
    //                           [&self](const std::pair<std::shared_ptr<Syntax::IType>, std::string>& pair)
    //                           {
    //                               return std::visit([](auto&& value) -> std::size_t
    //                                                 {
    //                                                     using T = std::decay_t<decltype(value)>;
    //                                                     if constexpr(std::is_convertible_v<T, std::size_t>)
    //                                                     {
    //                                                         return value;
    //                                                     }
    //                                                     else
    //                                                     {
    //                                                         throw std::runtime_error("Size returned as void*");
    //                                                     }
    //                                                     return {};
    //                                                 }, self(pair.first, self));
    //                           });
    //            return *std::max_element(sizes.begin(), sizes.end());
    //        }
    //    };
    //    m_return = std::visit([&sizeOf](auto&& value) -> OpenCL::Constant::ConstRetType
    //                          {
    //                              using T = std::decay_t<decltype(value)>;
    //                              if constexpr(std::is_same_v<T, std::shared_ptr<Syntax::IType>>)
    //                              {
    //                                  return sizeOf(value, sizeOf);
    //                              }
    //                              else
    //                              {
    //                                  throw std::runtime_error("Not implemented yet");
    //                              }
    //                          }, node.getUnaryOrType());
    throw std::runtime_error("Not implemented yet");
}

namespace
{
    template <class T>
    OpenCL::Constant::ConstRetType castVariant(const OpenCL::Constant::ConstRetType& variant)
    {
        return std::visit(
            [](auto&& value) -> OpenCL::Constant::ConstRetType {
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

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::CastExpression& node)
{
    return std::visit(
        [this](auto&& value) -> OpenCL::Constant::ConstRetType {
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

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::Term& node)
{
    if (node.getOptionalCastExpressions().empty())
    {
        return visit(node.getCastExpression());
    }
    auto value = visit(node.getCastExpression());
    for (auto& [op, exp] : node.getOptionalCastExpressions())
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasMultiply<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasDivide<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasModulo<T1, T2>{})
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

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::AdditiveExpression& node)
{
    if (node.getOptionalTerms().empty())
    {
        return visit(node.getTerm());
    }
    auto currentValue = visit(node.getTerm());
    for (auto& [op, exp] : node.getOptionalTerms())
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasPlus<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (!std::is_void_v<std::remove_pointer_t<
                                                  T1>> || !std::is_void_v<std::remove_pointer_t<T2>>)
                                {
                                    if constexpr (hasMinus<T1, T2>{})
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

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::ShiftExpression& node)
{
    if (node.getOptionalAdditiveExpressions().empty())
    {
        return visit(node.getAdditiveExpression());
    }
    auto currentValue = visit(node.getAdditiveExpression());
    for (auto& [op, exp] : node.getOptionalAdditiveExpressions())
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return *second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasLShift<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasRShift<T1, T2>{})
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

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::BitAndExpression& node)
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
            [&exp, this](auto&& lhs) -> ConstRetType {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasBitAnd<T1, T2>{})
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

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::BitXorExpression& node)
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
            [&exp, this](auto&& lhs) -> ConstRetType {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasBitXor<T1, T2>{})
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

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::BitOrExpression& node)
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
            [&exp, this](auto&& lhs) -> ConstRetType {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (!second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasBitOr<T1, T2>{})
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

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalAndExpression& node)
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
            [&exp, this](auto&& lhs) -> ConstRetType {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (!second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasLogicAnd<T1, T2>{})
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

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::LogicalOrExpression& node)
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
            [&exp, this](auto&& lhs) -> ConstRetType {
                using T1 = std::decay_t<decltype(lhs)>;
                auto second = visit(exp);
                if (!second)
                {
                    return second;
                }
                return std::visit(
                    [lhs](auto&& rhs) -> ConstRetType {
                        using T2 = std::decay_t<decltype(rhs)>;
                        if constexpr (hasLogicOr<T1, T2>{})
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

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::ConditionalExpression& node)
{
    if (node.getOptionalExpression() && node.getOptionalConditionalExpression())
    {
        auto value = visit(node.getLogicalOrExpression());
        if (!value)
        {
            return value;
        }
        if (std::visit([](auto&& value) -> bool { return value; }, *value))
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

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::RelationalExpression& node)
{
    if (node.getOptionalShiftExpressions().empty())
    {
        return visit(node.getShiftExpression());
    }
    auto currentValue = visit(node.getShiftExpression());
    for (auto& [op, exp] : node.getOptionalShiftExpressions())
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasGT<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasGE<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasLT<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasLE<T1, T2>{})
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

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::EqualityExpression& node)
{
    if (node.getOptionalRelationalExpressions().empty())
    {
        return visit(node.getRelationalExpression());
    }
    auto currentValue = visit(node.getRelationalExpression());
    for (auto& [op, exp] : node.getOptionalRelationalExpressions())
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasEQ<T1, T2>{})
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
                    [exp = &exp, this](auto&& lhs) -> ConstRetType {
                        using T1 = std::decay_t<decltype(lhs)>;
                        auto second = visit(*exp);
                        if (!second)
                        {
                            return second;
                        }
                        return std::visit(
                            [lhs](auto&& rhs) -> ConstRetType {
                                using T2 = std::decay_t<decltype(rhs)>;
                                if constexpr (hasNE<T1, T2>{})
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

OpenCL::Constant::ConstantEvaluator::ConstantEvaluator(
    const std::map<std::string, Representations::RecordType>& structOrUnions)
    : m_structOrUnions(structOrUnions)
{
}

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionSubscript&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionArrow&)
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpressionDot&)
{
    throw std::runtime_error("Not implemented yet");
}

namespace
{
    template <class... Ts>
    struct overload : Ts...
    {
        using Ts::operator()...;
    };
    template <class... Ts>
    overload(Ts...)->overload<Ts...>;
} // namespace

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    return std::visit(overload{[this](auto&& value) -> ConstRetType { return visit(value); },
                               [](const Syntax::PrimaryExpressionIdentifier&) -> ConstRetType {
                                   return FailureReason("Identifier not allowed in constant expression");
                               }},
                      node.getVariant());
}

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    return std::visit(overload{[this](auto&& value) -> ConstRetType { return visit(value); },
                               [](const Syntax::PostFixExpressionFunctionCall&) -> ConstRetType {
                                   return FailureReason("Function call not allowed in constant expression");
                               },
                               [](const Syntax::PostFixExpressionIncrement&) -> ConstRetType {
                                   return FailureReason("Increment not allowed in constant expression");
                               },
                               [](const Syntax::PostFixExpressionDecrement&) -> ConstRetType {
                                   return FailureReason("Decrement not allowed in constant expression");
                               },
                               [](const Syntax::PostFixExpressionTypeInitializer&) -> ConstRetType {
                                   return FailureReason("Type initializer not allowed in constant expression");
                               }},
                      node.getVariant());
}

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    return std::visit([this](auto&& value) -> OpenCL::Constant::ConstRetType { return visit(value); },
                      node.getVariant());
}

OpenCL::Constant::ConstRetType OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::Expression& node)
{
    if (node.getAssignmentExpressions().size() != 1)
    {
        return FailureReason(", operator not allowed in constant expression");
    }
    return visit(node.getAssignmentExpressions()[0]);
}

OpenCL::Constant::ConstRetType
    OpenCL::Constant::ConstantEvaluator::visit(const OpenCL::Syntax::AssignmentExpression& node)
{
    return std::visit(
        [this](auto&& value) -> OpenCL::Constant::ConstRetType {
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

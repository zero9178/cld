#include "Codegen.hpp"

#include "ConstantEvaluator.hpp"

#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <numeric>
#include <sstream>
#include <utility>

namespace
{
    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&&... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G)->Y<G>;

    template <class... Ts>
    struct overload : Ts...
    {
        using Ts::operator()...;
    };
    template <class... Ts>
    overload(Ts...)->overload<Ts...>;
} // namespace

llvm::Value* OpenCL::Codegen::Context::castTo(const OpenCL::Representations::Type& sourceType, llvm::Value* source,
                                              const OpenCL::Representations::Type& destinationType,
                                              bool explicitConversion)
{
    if (sourceType.isCompatibleWith(destinationType))
    {
        return source;
    }
    return std::visit(
        overload{
            [&](const Representations::PrimitiveType& primitiveType) -> llvm::Value* {
                return std::visit(
                    overload{[&](const Representations::PrimitiveType& otherPrimitive) -> llvm::Value* {
                                 if (!primitiveType.isFloatingPoint() && !otherPrimitive.isFloatingPoint())
                                 {
                                     if (primitiveType.isSigned())
                                     {
                                         return builder.CreateSExtOrTrunc(source, visit(destinationType));
                                     }
                                     else
                                     {
                                         return builder.CreateZExtOrTrunc(source, visit(destinationType));
                                     }
                                 }
                                 else if (primitiveType.isFloatingPoint() && !otherPrimitive.isFloatingPoint())
                                 {
                                     if (otherPrimitive.isSigned())
                                     {
                                         return builder.CreateFPToSI(source, visit(destinationType));
                                     }
                                     else
                                     {
                                         return builder.CreateFPToUI(source, visit(destinationType));
                                     }
                                 }
                                 else if (!primitiveType.isFloatingPoint() && otherPrimitive.isFloatingPoint())
                                 {
                                     if (primitiveType.isSigned())
                                     {
                                         return builder.CreateSIToFP(source, visit(destinationType));
                                     }
                                     else
                                     {
                                         return builder.CreateUIToFP(source, visit(destinationType));
                                     }
                                 }
                                 else
                                 {
                                     return builder.CreateFPCast(source, visit(destinationType));
                                 }
                             },
                             [&](const Representations::PointerType& pointerType) -> llvm::Value* {
                                 if (primitiveType.isFloatingPoint())
                                 {
                                     return nullptr;
                                 }
                                 if (explicitConversion)
                                 {
                                     if (!std::holds_alternative<Representations::FunctionType>(
                                             pointerType.getElementType().getType()))
                                     {
                                         return builder.CreateIntToPtr(source, visit(destinationType));
                                     }
                                     else
                                     {
                                         return nullptr;
                                     }
                                 }
                                 else
                                 {
                                     auto* constant = llvm::dyn_cast<llvm::ConstantInt>(source);
                                     if (!constant || constant->getValue() != 0)
                                     {
                                         return nullptr;
                                     }
                                     return builder.CreateIntToPtr(source, visit(destinationType));
                                 }
                             },
                             [&](const Representations::EnumType&) -> llvm::Value* {
                                 if (primitiveType.isFloatingPoint())
                                 {
                                     return builder.CreateFPToSI(source, builder.getInt32Ty());
                                 }
                                 else if (primitiveType.isSigned())
                                 {
                                     return builder.CreateSExtOrTrunc(source, builder.getInt32Ty());
                                 }
                                 else
                                 {
                                     return builder.CreateZExtOrTrunc(source, builder.getInt32Ty());
                                 }
                             },
                             [](auto &&) -> llvm::Value* { return nullptr; }},
                    destinationType.getType());
            },
            [&](const Representations::PointerType& pointerType) -> llvm::Value* {
                return std::visit(overload{[&](const Representations::PrimitiveType& primitiveType) -> llvm::Value* {
                                               if (primitiveType.isFloatingPoint() || !explicitConversion)
                                               {
                                                   return nullptr;
                                               }
                                               return builder.CreatePtrToInt(source, visit(destinationType));
                                           },
                                           [&](const Representations::PointerType& otherPointer) -> llvm::Value* {
                                               if (!explicitConversion)
                                               {
                                                   auto* primitive = std::get_if<Representations::PrimitiveType>(
                                                       &pointerType.getElementType().getType());
                                                   auto* otherPrimitive = std::get_if<Representations::PrimitiveType>(
                                                       &otherPointer.getElementType().getType());
                                                   if ((!primitive || primitive->getBitCount() == 0)
                                                       && (!otherPrimitive || otherPrimitive->getBitCount() == 0))
                                                   {
                                                       return nullptr;
                                                   }
                                               }
                                               return builder.CreateBitCast(source, visit(destinationType));
                                           },
                                           [&](const Representations::EnumType&) -> llvm::Value* {
                                               return builder.CreatePtrToInt(source, builder.getInt32Ty());
                                           },
                                           [](auto &&) -> llvm::Value* { return nullptr; }},
                                  destinationType.getType());
            },
            [&](const Representations::FunctionType& functionType) -> llvm::Value* {
                return std::visit(overload{[&](const Representations::PointerType& pointerType) -> llvm::Value* {
                                               if (auto* function = std::get_if<Representations::FunctionType>(
                                                       &pointerType.getElementType().getType());
                                                   function && *function == functionType)
                                               {
                                                   return source;
                                               }
                                               return nullptr;
                                           },
                                           [](auto &&) -> llvm::Value* { return nullptr; }},
                                  destinationType.getType());
            },
            [&](const Representations::EnumType&) -> llvm::Value* {
                return std::visit(overload{[&](const Representations::PrimitiveType& primitiveType) -> llvm::Value* {
                                               if (primitiveType.isFloatingPoint())
                                               {
                                                   return builder.CreateSIToFP(source, visit(destinationType));
                                               }
                                               else
                                               {
                                                   return builder.CreateSExtOrTrunc(source, visit(destinationType));
                                               }
                                           },
                                           [&](const Representations::PointerType&) -> llvm::Value* {
                                               return builder.CreateIntToPtr(source, visit(destinationType));
                                           },
                                           [&](const Representations::EnumType&) -> llvm::Value* { return source; },
                                           [](auto &&) -> llvm::Value* { return nullptr; }},
                                  destinationType.getType());
            },
            [&](const Representations::ArrayType& arrayType) -> llvm::Value* {
                auto* zero = builder.getInt32(0);
                auto* ptrRep = builder.CreateInBoundsGEP(source, {zero, zero});
                return castTo(Representations::PointerType::create(false, false, false,
                                                                   Representations::Type(arrayType.getType())),
                              ptrRep, destinationType, explicitConversion);
            },
            [](auto &&) -> llvm::Value* { return nullptr; }},
        sourceType.getType());
}

llvm::Value* OpenCL::Codegen::Context::toBool(llvm::Value* source)
{
    if (source->getType()->isIntegerTy())
    {
        return builder.CreateICmpNE(source, builder.getIntN(source->getType()->getIntegerBitWidth(), 0));
    }
    else if (source->getType()->isFloatingPointTy())
    {
        return builder.CreateFCmpUNE(source, llvm::ConstantFP::get(source->getType(), 0));
    }
    else if (source->getType()->isPointerTy())
    {
        return builder.CreateICmpNE(source,
                                    llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(source->getType())));
    }
    else if (source->getType()->isArrayTy())
    {
        return builder.getInt1(true);
    }
    return nullptr;
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Expression& node)
{
    for (std::size_t i = 0; i < node.getAssignmentExpressions().size(); i++)
    {
        if (i + 1 >= node.getAssignmentExpressions().size())
        {
            return visit(node.getAssignmentExpressions()[i]);
        }
        else
        {
            visit(node.getAssignmentExpressions()[i]);
        }
    }
    return FailureReason("Internal compiler error: Expression must contain atleast one assignment expression");
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpressionIdentifier& node)
{
    auto* result = findValue(node.getIdentifier());
    if (!result)
    {
        return FailureReason("Undefined reference to " + node.getIdentifier());
    }
    return *result;
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
{
    return std::visit(
        overload{[this](std::int32_t int32) -> NodeRetType {
                     return std::pair{llvm::ConstantInt::getSigned(builder.getInt32Ty(), int32),
                                      Representations::PrimitiveType::create(false, false, false, true, 32)};
                 },
                 [this](std::uint32_t uint32) -> NodeRetType {
                     return std::pair{llvm::ConstantInt::get(builder.getInt32Ty(), uint32),
                                      Representations::PrimitiveType::create(false, false, false, false, 32)};
                 },
                 [this](std::int64_t int64) -> NodeRetType {
                     return std::pair{llvm::ConstantInt::getSigned(builder.getInt64Ty(), int64),
                                      Representations::PrimitiveType::create(false, false, false, true, 64)};
                 },
                 [this](std::uint64_t uint64) -> NodeRetType {
                     return std::pair{llvm::ConstantInt::get(builder.getInt64Ty(), uint64),
                                      Representations::PrimitiveType::create(false, false, false, false, 64)};
                 },
                 [this](float f) -> NodeRetType {
                     return std::pair{llvm::ConstantFP::get(builder.getFloatTy(), f),
                                      Representations::PrimitiveType::create(false, false, true, true, 32)};
                 },
                 [this](double d) -> NodeRetType {
                     return std::pair{llvm::ConstantFP::get(builder.getDoubleTy(), d),
                                      Representations::PrimitiveType::create(false, false, true, true, 64)};
                 },
                 [this](const std::string& s) -> NodeRetType {
                     return std::pair{builder.CreateGlobalString(s),
                                      Representations::ArrayType::create(
                                          false, false, false,
                                          Representations::PrimitiveType::create(false, false, false, true, 8),
                                          s.size() + 1)};
                 }},
        node.getValue());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpressionParenthese& node)
{
    return visit(node.getExpression());
}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression& node)
{
    return visit(node.getPrimaryExpression());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionSubscript& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionIncrement& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionDecrement& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionDot& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionArrow& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionFunctionCall& node)
{
}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer& node)
{
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::AssignmentExpressionAssignment& node)
{
    auto destination = visit(node.getUnaryFactor());
    if (!destination)
    {
        return destination;
    }
    if (!llvm::isa<llvm::LoadInst>(destination->first))
    {
        return FailureReason("Left hand of assignment expression must be a lvalue");
    }
    auto value = visit(node.getAssignmentExpression());
    if (!value)
    {
        return value;
    }
    auto* ptr = llvm::cast<llvm::LoadInst>(destination->first)->getPointerOperand();
    switch (node.getAssignOperator())
    {
        case Syntax::AssignmentExpressionAssignment::AssignOperator::NoOperator:
        {
            builder.CreateStore(value->first, ptr, destination->second.isVolatile());
            return std::pair{value->first, destination->second};
        }
        case Syntax::AssignmentExpressionAssignment::AssignOperator::PlusAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::MinusAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::DivideAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::MultiplyAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::ModuloAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::RightShiftAssign: break;
        case Syntax::AssignmentExpressionAssignment::AssignOperator::BitAndAssign:
        {
            auto bitAndResult = makeBitAnd(destination->second, destination->first, value->second, value->first);
            if (!bitAndResult)
            {
                return bitAndResult;
            }
            builder.CreateStore(bitAndResult->first, ptr, destination->second.isVolatile());
            return std::pair{bitAndResult->first, bitAndResult->second};
        }
        case Syntax::AssignmentExpressionAssignment::AssignOperator::BitOrAssign:
        {
            auto bitOrResult = makeBitOr(destination->second, destination->first, value->second, value->first);
            if (!bitOrResult)
            {
                return bitOrResult;
            }
            builder.CreateStore(bitOrResult->first, ptr, destination->second.isVolatile());
            return std::pair{bitOrResult->first, bitOrResult->second};
        }
        case Syntax::AssignmentExpressionAssignment::AssignOperator::BitXorAssign:
        {
            auto bitXorResult = makeBitOr(destination->second, destination->first, value->second, value->first);
            if (!bitXorResult)
            {
                return bitXorResult;
            }
            builder.CreateStore(bitXorResult->first, ptr, destination->second.isVolatile());
            return std::pair{bitXorResult->first, bitXorResult->second};
        }
    }
    return value;
}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression& node)
{
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
{
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CastExpression& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeMultiply(Representations::Type leftType, llvm::Value* left,
                                                                    Representations::Type rightType, llvm::Value* right)
{
    auto* leftPrimitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
    auto* rightPrimitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
    if (!leftPrimitive || !rightPrimitive)
    {
        return FailureReason("* only possible between arithmetic types");
    }
    arithmeticCast(leftType, left, rightType);
    arithmeticCast(rightType, right, leftType);
    if (leftPrimitive->isFloatingPoint())
    {
        return std::pair{builder.CreateFMul(left, right), leftType};
    }
    else
    {
        return std::pair{builder.CreateMul(left, right), leftType};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeDivide(Representations::Type leftType, llvm::Value* left,
                                                                  Representations::Type rightType, llvm::Value* right)
{
    auto* leftPrimitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
    auto* rightPrimitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
    if (!leftPrimitive || !rightPrimitive)
    {
        return FailureReason("* only possible between arithmetic types");
    }
    arithmeticCast(leftType, left, rightType);
    arithmeticCast(rightType, right, leftType);
    if (leftPrimitive->isFloatingPoint())
    {
        return std::pair{builder.CreateFDiv(left, right), leftType};
    }
    else
    {
        return std::pair{leftPrimitive->isSigned() ? builder.CreateSDiv(left, right) : builder.CreateUDiv(left, right),
                         leftType};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeRemainder(Representations::Type leftType, llvm::Value* left,
                                                                     Representations::Type rightType,
                                                                     llvm::Value* right)
{
    auto* leftPrimitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
    auto* rightPrimitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
    if (!leftPrimitive || !leftPrimitive->isFloatingPoint() || !rightPrimitive || !rightPrimitive->isFloatingPoint())
    {
        return FailureReason("* only possible between integer types");
    }
    arithmeticCast(leftType, left, rightType);
    arithmeticCast(rightType, right, leftType);
    if (leftPrimitive->isSigned())
    {
        return std::pair{builder.CreateSRem(left, right), leftType};
    }
    else
    {
        return std::pair{builder.CreateURem(left, right), leftType};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Term& node)
{
    auto result = visit(node.getCastExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& [op, cast] : node.getOptionalCastExpressions())
    {
        auto rightResult = visit(cast);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        auto shiftResult = [this, &leftType = leftType, &left = left, &rightType = rightType, &right = right,
                            &op = op] {
            switch (op)
            {
                case Syntax::Term::BinaryDotOperator::BinaryMultiply:
                    return makeMultiply(leftType, left, rightType, right);
                case Syntax::Term::BinaryDotOperator::BinaryDivide: return makeDivide(leftType, left, rightType, right);
                case Syntax::Term::BinaryDotOperator::BinaryRemainder:
                    return makeRemainder(leftType, left, rightType, right);
            }
        }();
        if (!shiftResult)
        {
            return shiftResult;
        }
        left = shiftResult->first;
        leftType = shiftResult->second;
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeMinus(Representations::Type leftType, llvm::Value* left,
                                                                 Representations::Type rightType, llvm::Value* right)
{
    auto* leftPrimitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
    auto* rightPrimitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
    if (leftPrimitive && rightPrimitive)
    {
        arithmeticCast(leftType, left, rightType);
        arithmeticCast(rightType, right, leftType);
        if (leftPrimitive->isFloatingPoint())
        {
            return std::pair{builder.CreateFSub(left, right), leftType};
        }
        else
        {
            return std::pair{builder.CreateSub(left, right), leftType};
        }
    }
    else
    {
        auto* leftPointer = std::get_if<Representations::PointerType>(&leftType.getType());
        auto* rightPointer = std::get_if<Representations::PointerType>(&rightType.getType());
        if ((!leftPointer && !rightPointer)
            || ((!leftPrimitive || !leftPrimitive->isFloatingPoint())
                && (!rightPrimitive || !rightPrimitive->isFloatingPoint())))
        {
            return FailureReason("- only possible between arithmetic or an integer and pointer type");
        }
        return std::pair{builder.CreateInBoundsGEP(leftPointer ? left : right, {leftPrimitive ? left : right}),
                         leftPointer ? leftType : rightType};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makePlus(Representations::Type leftType, llvm::Value* left,
                                                                Representations::Type rightType, llvm::Value* right)
{
    auto* leftPrimitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
    auto* rightPrimitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
    if (leftPrimitive && rightPrimitive)
    {
        arithmeticCast(leftType, left, rightType);
        arithmeticCast(rightType, right, leftType);
        if (leftPrimitive->isFloatingPoint())
        {
            return std::pair{builder.CreateFAdd(left, right), leftType};
        }
        else
        {
            return std::pair{builder.CreateAdd(left, right), leftType};
        }
    }
    else
    {
        auto* leftPointer = std::get_if<Representations::PointerType>(&leftType.getType());
        auto* rightPointer = std::get_if<Representations::PointerType>(&rightType.getType());
        if ((!leftPointer && !rightPointer)
            || ((!leftPrimitive || !leftPrimitive->isFloatingPoint())
                && (!rightPrimitive || !rightPrimitive->isFloatingPoint())))
        {
            return FailureReason("+ only possible between arithmetic or an integer and pointer type");
        }
        return std::pair{builder.CreateInBoundsGEP(leftPointer ? left : right, {leftPrimitive ? left : right}),
                         leftPointer ? leftType : rightType};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::AdditiveExpression& node)
{
    auto result = visit(node.getTerm());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& [op, additive] : node.getOptionalTerms())
    {
        auto rightResult = visit(additive);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        auto shiftResult = op == Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus ?
                               makePlus(leftType, left, rightType, right) :
                               makeMinus(leftType, left, rightType, right);
        if (!shiftResult)
        {
            return shiftResult;
        }
        left = shiftResult->first;
        leftType = shiftResult->second;
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeLeftShift(OpenCL::Representations::Type leftType,
                                                                     llvm::Value* left,
                                                                     OpenCL::Representations::Type rightType,
                                                                     llvm::Value* right)
{
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for & operator");
    }
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for & operator");
    }
    leftType = integerPromotion(leftType, &left);
    rightType = integerPromotion(rightType, &right);
    left = builder.CreateShl(left, right);
    if (!std::get<Representations::PrimitiveType>(leftType.getType()).isSigned()
        && std::get<Representations::PrimitiveType>(rightType.getType()).isSigned())
    {
        leftType = Representations::PrimitiveType::create(
            false, false, false, true, std::get<Representations::PrimitiveType>(leftType.getType()).getBitCount());
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeRightShift(OpenCL::Representations::Type leftType,
                                                                      llvm::Value* left,
                                                                      OpenCL::Representations::Type rightType,
                                                                      llvm::Value* right)
{
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for & operator");
    }
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for & operator");
    }
    leftType = integerPromotion(leftType, &left);
    rightType = integerPromotion(rightType, &right);
    left = builder.CreateAShr(left, right);
    if (!std::get<Representations::PrimitiveType>(leftType.getType()).isSigned()
        && std::get<Representations::PrimitiveType>(rightType.getType()).isSigned())
    {
        leftType = Representations::PrimitiveType::create(
            false, false, false, true, std::get<Representations::PrimitiveType>(leftType.getType()).getBitCount());
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ShiftExpression& node)
{
    auto result = visit(node.getAdditiveExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& [op, additive] : node.getOptionalAdditiveExpressions())
    {
        auto rightResult = visit(additive);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        auto shiftResult = op == Syntax::ShiftExpression::ShiftOperator::Left ?
                               makeLeftShift(leftType, left, rightType, right) :
                               makeRightShift(leftType, left, rightType, right);
        if (!shiftResult)
        {
            return shiftResult;
        }
        left = shiftResult->first;
        leftType = shiftResult->second;
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::RelationalExpression& node)
{
    auto result = visit(node.getShiftExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& [op, relational] : node.getOptionalShiftExpressions())
    {
        auto rightResult = visit(relational);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        if (!std::holds_alternative<Representations::PrimitiveType>(leftType.getType())
            || !std::holds_alternative<Representations::PrimitiveType>(rightType.getType()))
        {
            auto* leftPointer = std::get_if<Representations::PointerType>(&leftType.getType());
            auto* rightPointer = std::get_if<Representations::PointerType>(&rightType.getType());
            if (!leftPointer || !rightPointer)
            {
                return FailureReason("Equality operators only valid for arithmetic as well as pointers to same types");
            }
            if (!leftPointer->getElementType().isCompatibleWith(rightPointer->getElementType()))
            {
                return FailureReason("Pointers supplied to equality operator do not point to same type");
            }
        }
        arithmeticCast(leftType, left, rightType);
        arithmeticCast(rightType, right, leftType);
        if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
            primitive && primitive->isFloatingPoint())
        {
            switch (op)
            {
                case Syntax::RelationalExpression::RelationalOperator::LessThan:
                    left = builder.CreateFCmpULT(left, right);
                    break;
                case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
                    left = builder.CreateFCmpULE(left, right);
                    break;
                case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
                    left = builder.CreateFCmpUGT(left, right);
                    break;
                case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
                    left = builder.CreateFCmpUGE(left, right);
                    break;
            }
        }
        else
        {
            switch (op)
            {
                case Syntax::RelationalExpression::RelationalOperator::LessThan:
                    if (primitive && primitive->isSigned())
                    {
                        left = builder.CreateICmpSLT(left, right);
                    }
                    else
                    {
                        left = builder.CreateICmpULT(left, right);
                    }
                    break;
                case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
                    if (primitive && primitive->isSigned())
                    {
                        left = builder.CreateICmpSLE(left, right);
                    }
                    else
                    {
                        left = builder.CreateICmpULE(left, right);
                    }
                    break;
                case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
                    if (primitive && primitive->isSigned())
                    {
                        left = builder.CreateICmpSGT(left, right);
                    }
                    else
                    {
                        left = builder.CreateICmpUGT(left, right);
                    }
                    break;
                case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
                    if (primitive && primitive->isSigned())
                    {
                        left = builder.CreateICmpSGE(left, right);
                    }
                    else
                    {
                        left = builder.CreateICmpUGE(left, right);
                    }
                    break;
            }
        }
        left = builder.CreateZExt(left, builder.getInt32Ty());
        leftType = Representations::PrimitiveType::create(false, false, false, true, 32);
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::EqualityExpression& node)
{
    auto result = visit(node.getRelationalExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& [op, relational] : node.getOptionalRelationalExpressions())
    {
        auto rightResult = visit(relational);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        if (!std::holds_alternative<Representations::PrimitiveType>(leftType.getType())
            || !std::holds_alternative<Representations::PrimitiveType>(rightType.getType()))
        {
            auto* leftPointer = std::get_if<Representations::PointerType>(&leftType.getType());
            auto* rightPointer = std::get_if<Representations::PointerType>(&rightType.getType());
            if (!leftPointer || !rightPointer)
            {
                return FailureReason("Equality operators only valid for arithmetic as well as pointers to same types");
            }
            if (!leftPointer->getElementType().isCompatibleWith(rightPointer->getElementType()))
            {
                return FailureReason("Pointers supplied to equality operator do not point to same type");
            }
        }
        arithmeticCast(leftType, left, rightType);
        arithmeticCast(rightType, right, leftType);
        if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
            primitive && primitive->isFloatingPoint())
        {
            if (op == Syntax::EqualityExpression::EqualityOperator::Equal)
            {
                left = builder.CreateFCmpUEQ(left, right);
            }
            else
            {
                left = builder.CreateFCmpUNE(left, right);
            }
        }
        else
        {
            if (op == Syntax::EqualityExpression::EqualityOperator::Equal)
            {
                left = builder.CreateICmpEQ(left, right);
            }
            else
            {
                left = builder.CreateICmpNE(left, right);
            }
        }
        left = builder.CreateZExt(left, builder.getInt32Ty());
        leftType = Representations::PrimitiveType::create(false, false, false, true, 32);
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeBitAnd(Representations::Type leftType, llvm::Value* left,
                                                                  Representations::Type rightType, llvm::Value* right)
{
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for & operator");
    }
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for & operator");
    }
    arithmeticCast(leftType, left, rightType);
    arithmeticCast(rightType, right, leftType);
    left = builder.CreateAnd(left, right);
    if (!std::get<Representations::PrimitiveType>(leftType.getType()).isSigned()
        && std::get<Representations::PrimitiveType>(rightType.getType()).isSigned())
    {
        leftType = Representations::PrimitiveType::create(
            false, false, false, true, std::get<Representations::PrimitiveType>(leftType.getType()).getBitCount());
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BitAndExpression& node)
{
    auto result = visit(node.getEqualityExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& equality : node.getOptionalEqualityExpressions())
    {
        auto rightResult = visit(equality);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        auto bitAndResult = makeBitAnd(leftType, left, rightType, right);
        if (!bitAndResult)
        {
            return bitAndResult;
        }
        left = bitAndResult->first;
        leftType = bitAndResult->second;
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeBitXor(Representations::Type leftType, llvm::Value* left,
                                                                  Representations::Type rightType, llvm::Value* right)
{
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for ^ operator");
    }
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for ^ operator");
    }
    arithmeticCast(leftType, left, rightType);
    arithmeticCast(rightType, right, leftType);
    left = builder.CreateXor(left, right);
    if (!std::get<Representations::PrimitiveType>(leftType.getType()).isSigned()
        && std::get<Representations::PrimitiveType>(rightType.getType()).isSigned())
    {
        leftType = Representations::PrimitiveType::create(
            false, false, false, true, std::get<Representations::PrimitiveType>(leftType.getType()).getBitCount());
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BitXorExpression& node)
{
    auto result = visit(node.getBitAndExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& bitAnd : node.getOptionalBitAndExpressions())
    {
        auto rightResult = visit(bitAnd);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        auto bitXorResult = makeBitXor(leftType, left, rightType, right);
        if (!bitXorResult)
        {
            return bitXorResult;
        }
        left = bitXorResult->first;
        leftType = bitXorResult->second;
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::makeBitOr(OpenCL::Representations::Type leftType,
                                                                 llvm::Value* left,
                                                                 OpenCL::Representations::Type rightType,
                                                                 llvm::Value* right)
{
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&leftType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for | operator");
    }
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&rightType.getType());
        !primitive || primitive->isFloatingPoint())
    {
        return FailureReason("Type must be of integer type for | operator");
    }
    arithmeticCast(leftType, left, rightType);
    arithmeticCast(rightType, right, leftType);
    left = builder.CreateOr(left, right);
    if (!std::get<Representations::PrimitiveType>(leftType.getType()).isSigned()
        && std::get<Representations::PrimitiveType>(rightType.getType()).isSigned())
    {
        leftType = Representations::PrimitiveType::create(
            false, false, false, true, std::get<Representations::PrimitiveType>(leftType.getType()).getBitCount());
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BitOrExpression& node)
{
    auto result = visit(node.getBitXorExpression());
    if (!result)
    {
        return result;
    }
    auto [left, leftType] = *result;
    for (auto& bitXor : node.getOptionalBitXorExpressions())
    {
        auto rightResult = visit(bitXor);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        auto bitOrResult = makeBitOr(leftType, left, rightType, right);
        if (!bitOrResult)
        {
            return bitOrResult;
        }
        left = bitOrResult->first;
        leftType = bitOrResult->second;
    }
    return std::pair{left, leftType};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::LogicalAndExpression& node)
{
    auto result = visit(node.getBitOrExpression());
    if (!result)
    {
        return result;
    }
    auto [value, type] = *result;
    for (auto& andExpression : node.getOptionalBitOrExpressions())
    {
        auto* function = builder.GetInsertBlock()->getParent();
        value = toBool(value);
        auto* thenBB = llvm::BasicBlock::Create(context, "", function);
        auto* elseBB = llvm::BasicBlock::Create(context);
        auto* mergeBB = llvm::BasicBlock::Create(context);

        builder.CreateCondBr(value, elseBB, thenBB);

        builder.SetInsertPoint(thenBB);
        auto rightResult = visit(andExpression);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        right = toBool(right);
        right = builder.CreateZExt(right, builder.getInt32Ty());
        builder.CreateBr(mergeBB);
        thenBB = builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        builder.SetInsertPoint(elseBB);

        builder.CreateBr(mergeBB);
        elseBB = builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
        auto* pn = builder.CreatePHI(builder.getInt32Ty(), 2);
        pn->addIncoming(right, thenBB);
        pn->addIncoming(builder.getInt32(0), elseBB);
        value = pn;
        type = Representations::PrimitiveType::create(false, false, false, true, 32);
    }
    return std::pair{value, type};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::LogicalOrExpression& node)
{
    auto result = visit(node.getAndExpression());
    if (!result)
    {
        return result;
    }
    auto [value, type] = *result;
    for (auto& andExpression : node.getOptionalAndExpressions())
    {
        auto* function = builder.GetInsertBlock()->getParent();
        value = toBool(value);
        auto* thenBB = llvm::BasicBlock::Create(context, "", function);
        auto* elseBB = llvm::BasicBlock::Create(context);
        auto* mergeBB = llvm::BasicBlock::Create(context);

        builder.CreateCondBr(value, elseBB, thenBB);

        builder.SetInsertPoint(thenBB);
        auto rightResult = visit(andExpression);
        if (!rightResult)
        {
            return rightResult;
        }
        auto [right, rightType] = *rightResult;
        right = toBool(right);
        right = builder.CreateZExt(right, builder.getInt32Ty());
        builder.CreateBr(mergeBB);
        thenBB = builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        builder.SetInsertPoint(elseBB);

        builder.CreateBr(mergeBB);
        elseBB = builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
        auto* pn = builder.CreatePHI(builder.getInt32Ty(), 2);
        pn->addIncoming(right, thenBB);
        pn->addIncoming(builder.getInt32(1), elseBB);
        value = pn;
        type = Representations::PrimitiveType::create(false, false, false, true, 32);
    }
    return std::pair{value, type};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ConditionalExpression& node)
{
    if (node.getOptionalConditionalExpression() && node.getOptionalExpression())
    {
        auto booleanE = visit(node.getLogicalOrExpression());
        if (!booleanE)
        {
            return booleanE;
        }
        auto* boolean = toBool(booleanE->first);
        auto* function = builder.GetInsertBlock()->getParent();

        auto* thenBB = llvm::BasicBlock::Create(context, "", function);
        auto* elseBB = llvm::BasicBlock::Create(context);
        auto* mergeBB = llvm::BasicBlock::Create(context);

        builder.CreateCondBr(boolean, thenBB, elseBB);

        builder.SetInsertPoint(thenBB);
        auto thenE = visit(*node.getOptionalExpression());
        if (!thenE)
        {
            return thenE;
        }
        auto [thenV, thenT] = *thenE;

        builder.CreateBr(mergeBB);
        thenBB = builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        builder.SetInsertPoint(elseBB);

        auto elseE = visit(*node.getOptionalConditionalExpression());
        if (!elseE)
        {
            return elseE;
        }
        auto [elseV, elseT] = *elseE;

        builder.CreateBr(mergeBB);
        elseBB = builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
        // TODO: arithmeticCast
        auto* pn = builder.CreatePHI(thenV->getType(), 2);
        pn->addIncoming(thenV, thenBB);
        pn->addIncoming(elseV, elseBB);
        return std::pair{pn, thenT};
    }
    else
    {
        return visit(node.getLogicalOrExpression());
    }
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ReturnStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ExpressionStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::IfStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::SwitchStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DefaultStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CaseStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CompoundStatement& node,
                                                                     bool pushScope)
{
    if (pushScope)
    {
        this->pushScope();
    }
    for (auto& iter : node.getBlockItems())
    {
        auto result = visit(iter);
        if (result)
        {
            return result;
        }
    }
    if (pushScope)
    {
        popScope();
    }
    return {};
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ForStatement& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::InitializerList& node) {}

namespace
{
    template <class T, class InputIterator>
    bool declarationSpecifierHas(InputIterator&& begin, InputIterator&& end, const T& value)
    {
        return std::any_of(begin, end, [&value](const OpenCL::Syntax::DeclarationSpecifier& declarationSpecifier) {
            auto* t = std::get_if<T>(&declarationSpecifier);
            if (!t)
            {
                return false;
            }
            return *t == value;
        });
    }

    template <class T, class InputIterator, class Predicate>
    bool declarationSpecifierHasIf(InputIterator&& begin, InputIterator&& end, Predicate&& predicate)
    {
        return std::any_of(begin, end, [&predicate](const OpenCL::Syntax::DeclarationSpecifier& declarationSpecifier) {
            auto* t = std::get_if<T>(&declarationSpecifier);
            if (!t)
            {
                return false;
            }
            return predicate(*t);
        });
    }
} // namespace

llvm::Constant* OpenCL::Codegen::Context::createZeroValue(llvm::Type* type)
{
    if (type->isIntegerTy())
    {
        return llvm::ConstantInt::get(type, 0);
    }
    else if (type->isFloatingPointTy())
    {
        return llvm::ConstantFP::get(type, 0);
    }
    else if (type->isPointerTy())
    {
        return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
    }
    else if (type->isStructTy())
    {
        std::vector<llvm::Constant*> membersZeroes;
        membersZeroes.reserve(type->getStructNumElements());
        for (std::size_t i = 0; i < type->getStructNumElements(); i++)
        {
            membersZeroes.push_back(createZeroValue(type->getStructElementType(i)));
        }
        return llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(type), membersZeroes);
    }
    else if (type->isArrayTy())
    {
        return llvm::ConstantArray::get(
            llvm::cast<llvm::ArrayType>(type),
            std::vector<llvm::Constant*>(type->getArrayNumElements(), createZeroValue(type->getArrayElementType())));
    }
    return nullptr;
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Declaration& node)
{
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    std::vector<Representations::SpecifierQualifierRef> specifierQualifiers;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        std::visit(overload{[&specifierQualifiers](const Syntax::TypeSpecifier& typeSpecifier) {
                                specifierQualifiers.emplace_back(typeSpecifier);
                            },
                            [&specifierQualifiers](const Syntax::TypeQualifier& typeQualifier) {
                                specifierQualifiers.emplace_back(typeQualifier);
                            },
                            [&storageClassSpecifier](const Syntax::StorageClassSpecifier& otherStorageClassSpecifier) {
                                storageClassSpecifier = &otherStorageClassSpecifier;
                            },
                            [](auto&&) {}},
                   iter);
    }

    if (std::count_if(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                      [](const Syntax::DeclarationSpecifier& specifier) {
                          return std::holds_alternative<Syntax::StorageClassSpecifier>(specifier);
                      })
        > 1)
    {
        return FailureReason("A maximum of one storage class specifier allowed in declaration");
    }
    for (auto& [declarator, initializer] : node.getInitDeclarators())
    {
        auto type = Representations::declaratorsToType(
            specifierQualifiers,
            [& declarator = declarator]() -> Representations::PossiblyAbstractQualifierRef {
                if (declarator)
                {
                    return *declarator;
                }
                else
                {
                    return nullptr;
                }
            }(),
            gatherTypedefs());
        if (!type)
        {
            return type.error();
        }
        if (std::holds_alternative<Representations::RecordType>(type->getType()))
        {
            auto record = *Representations::declaratorsToType(
                {*std::find_if(specifierQualifiers.begin(), specifierQualifiers.end(),
                               [](const Representations::SpecifierQualifierRef& value) {
                                   auto* specifier =
                                       std::get_if<std::reference_wrapper<const Syntax::TypeSpecifier>>(&value);
                                   if (!specifier)
                                   {
                                       return false;
                                   }
                                   return std::holds_alternative<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(
                                       specifier->get().getVariant());
                               })},
                nullptr, gatherTypedefs());
            auto [result, inserted] = m_structsUnionsAndEnums.back().insert({record.getName(), record});
            if (!inserted)
            {
                if (auto structDecl = std::get_if<Representations::RecordType>(&result->second.getType());
                    structDecl && !structDecl->isDeclaration())
                {
                    return FailureReason("Redefinition of record type " + result->first + " in the same scope");
                }
                else if (!structDecl)
                {
                    return FailureReason("Internal Compiler error: Non record type with name " + result->first
                                         + " exists");
                }
                else
                {
                    result->second = record;
                }
            }
        }

        auto name = declarator ? Representations::declaratorToName(*declarator) : "";
        if (storageClassSpecifier && *storageClassSpecifier == Syntax::StorageClassSpecifier::Typedef)
        {
            if (!m_typedefs.back().insert({name, *type}).second)
            {
                return FailureReason("Redefinition of typedef " + name + " in the same scope");
            }
            continue;
        }
        if (name.empty())
        {
            continue;
        }
        if (inGlobalScope() && !std::holds_alternative<Representations::FunctionType>(type->getType()))
        {
            if (std::holds_alternative<Representations::ValArrayType>(type->getType()))
            {
                return FailureReason("Variable arrays not allowed in global scope");
            }
            bool internalLinkage =
                declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                        Syntax::StorageClassSpecifier::Extern);
            bool externalLinkage =
                declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                        Syntax::StorageClassSpecifier::Static);
            if (internalLinkage && externalLinkage)
            {
                return FailureReason("Can't combine static with extern");
            }
            llvm::Type* llvmType = visit(*type);
            llvm::Constant* initial = createZeroValue(llvmType);
            if (initializer)
            {
                if (auto* assignment = std::get_if<Syntax::AssignmentExpression>(&initializer->getVariant()))
                {
                    Constant::ConstantEvaluator evaluator;
                    auto result = evaluator.visit(*assignment);
                    if (!result)
                    {
                        return result.error();
                    }
                    if (llvmType->isIntegerTy())
                    {
                        bool isSigned = std::get<Representations::PrimitiveType>(type->getType()).isSigned();
                        if (isSigned)
                        {
                            initial = llvm::ConstantInt::getSigned(
                                llvmType,
                                std::visit([](auto&& value) -> std::int64_t { return (std::int64_t)value; }, *result));
                        }
                        else
                        {
                            initial = llvm::ConstantInt::get(
                                llvmType, std::visit([](auto&& value) -> std::uint64_t { return (std::uint64_t)value; },
                                                     *result));
                        }
                    }
                    else if (llvmType->isFloatingPointTy())
                    {
                        initial =
                            llvm::ConstantFP::get(llvmType, std::visit(
                                                                [](auto&& value) -> double {
                                                                    using T = std::decay_t<decltype(value)>;
                                                                    if constexpr (std::is_convertible_v<T, double>)
                                                                    {
                                                                        return value;
                                                                    }
                                                                    else
                                                                    {
                                                                        return std::numeric_limits<double>::quiet_NaN();
                                                                    }
                                                                },
                                                                *result));
                    }
                }
                else
                {
                    // TODO: Initializer list
                }
            }
            auto* newGlobal = new llvm::GlobalVariable(*module, llvmType, type->isConst(),
                                                       [internalLinkage, externalLinkage] {
                                                           if (internalLinkage)
                                                           {
                                                               return llvm::GlobalValue::InternalLinkage;
                                                           }
                                                           if (externalLinkage)
                                                           {
                                                               return llvm::GlobalValue::ExternalLinkage;
                                                           }
                                                           return llvm::GlobalValue::CommonLinkage;
                                                       }(),
                                                       initial);
            if (auto [prev, success] = m_namedValues.back().insert({name, {newGlobal, *type}}); !success)
            {
                return FailureReason("Redefinition of symbol " + prev->first);
            }
        }
        else if (std::holds_alternative<Representations::FunctionType>(type->getType()))
        {
            const auto& functionRP = std::get<Representations::FunctionType>(type->getType());
            bool internalLinkage =
                declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                        Syntax::StorageClassSpecifier::Extern);
            bool externalLinkage =
                declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                        Syntax::StorageClassSpecifier::Static);
            if (internalLinkage && externalLinkage)
            {
                return FailureReason("Can't combine static with extern");
            }
            auto* ft = visit(*type);
            if (!llvm::isa<llvm::FunctionType>(ft))
            {
                return FailureReason("Internal Compiler error: Non function type returned from visit to function type");
            }
            auto* func = llvm::Function::Create(llvm::cast<llvm::FunctionType>(ft),
                                                internalLinkage ? llvm::Function::InternalLinkage :
                                                                  llvm::Function::ExternalLinkage,
                                                name, module.get());
            bool retIsStruct =
                std::holds_alternative<Representations::RecordType>(functionRP.getReturnType().getType());
            if (retIsStruct)
            {
                func->addAttribute(1, llvm::Attribute::get(context, llvm::Attribute::StructRet));
                func->addAttribute(1, llvm::Attribute::get(context, llvm::Attribute::NoAlias));
            }
            std::size_t i = 0;
            for (auto& iter : functionRP.getArguments())
            {
                if (std::holds_alternative<Representations::RecordType>(iter.getType()))
                {
                    func->addAttribute(i + (retIsStruct ? 2 : 1),
                                       llvm::Attribute::get(context, llvm::Attribute::ByVal));
                }
                if (auto* ptr = std::get_if<Representations::PointerType>(&iter.getType()); ptr && ptr->isRestricted())
                {
                    func->addAttribute(i + (retIsStruct ? 2 : 1),
                                       llvm::Attribute::get(context, llvm::Attribute::NoAlias));
                }
                i++;
            }
            if (auto [prev, success] = m_namedValues.back().insert({name, {func, std::move(*type)}}); !success)
            {
                return FailureReason("Redefinition of symbol " + prev->first);
            }
        }
        else
        {
            llvm::IRBuilder<> tmpB(builder.GetInsertBlock(), builder.GetInsertBlock()->begin());
            auto* allocaType = visit(*type);
            auto* alloca = tmpB.CreateAlloca(allocaType);
            if (auto [prev, success] = m_namedValues.back().insert({name, {alloca, std::move(*type)}}); !success)
            {
                return FailureReason("Redefinition of symbol " + prev->first);
            }
            if (initializer)
            {
                if (auto* assignment = std::get_if<Syntax::AssignmentExpression>(&initializer->getVariant()))
                {
                    auto result = visit(*assignment);
                    if (!result)
                    {
                        return result.error();
                    }
                    builder.CreateStore(castTo(result->second, result->first, *type), alloca, type->isVolatile());
                }
                else
                {
                    // TODO: Initializer
                }
            }
        }
    }
    return {};
}

std::optional<OpenCL::FailureReason>
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ForDeclarationStatement& node)
{
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::HeadWhileStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::FootWhileStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BreakStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ContinueStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::FunctionDefinition& node)
{
    const Representations::FunctionType* ft = nullptr;
    auto name = Representations::declaratorToName(node.getDeclarator());
    auto* thisFunction = module->getFunction(name);
    if (!thisFunction)
    {
        // TODO: Refactor so this and Declarations have a common function
        const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
        std::vector<Representations::SpecifierQualifierRef> specifierQualifiers;
        for (auto& iter : node.getDeclarationSpecifiers())
        {
            std::visit(
                overload{[&specifierQualifiers](const Syntax::TypeSpecifier& typeSpecifier) {
                             specifierQualifiers.emplace_back(typeSpecifier);
                         },
                         [&specifierQualifiers](const Syntax::TypeQualifier& typeQualifier) {
                             specifierQualifiers.emplace_back(typeQualifier);
                         },
                         [&storageClassSpecifier](const Syntax::StorageClassSpecifier& otherStorageClassSpecifier) {
                             storageClassSpecifier = &otherStorageClassSpecifier;
                         },
                         [](auto&&) {}},
                iter);
        }
        if (std::count_if(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                          [](const Syntax::DeclarationSpecifier& specifier) {
                              return std::holds_alternative<Syntax::StorageClassSpecifier>(specifier);
                          })
            > 1)
        {
            return FailureReason("A maximum of one storage class specifier allowed in declaration");
        }
        auto type = Representations::declaratorsToType(specifierQualifiers, node.getDeclarator(), gatherTypedefs());
        if (!std::holds_alternative<Representations::FunctionType>(type->getType()))
        {
            return FailureReason("Internal compiler error: Function definition did not return a function type");
        }
        const auto& functionRP = std::get<Representations::FunctionType>(type->getType());
        bool internalLinkage =
            declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                    Syntax::StorageClassSpecifier::Extern);
        bool externalLinkage =
            declarationSpecifierHas(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                    Syntax::StorageClassSpecifier::Static);
        if (internalLinkage && externalLinkage)
        {
            return FailureReason("Can't combine static with extern");
        }

        // TODO:Refactor to be able to use Identifiers instead
        auto* llvmFt = visit(*type);
        if (!llvm::isa<llvm::FunctionType>(llvmFt))
        {
            return FailureReason("Expected Function type in function definition");
        }
        thisFunction = llvm::Function::Create(
            llvm::cast<llvm::FunctionType>(llvmFt),
            internalLinkage ? llvm::Function::InternalLinkage : llvm::Function::ExternalLinkage, name, module.get());
        bool retIsStruct = std::holds_alternative<Representations::RecordType>(functionRP.getReturnType().getType());
        if (retIsStruct)
        {
            thisFunction->addAttribute(1, llvm::Attribute::get(context, llvm::Attribute::StructRet));
            thisFunction->addAttribute(1, llvm::Attribute::get(context, llvm::Attribute::NoAlias));
        }
        std::size_t i = 0;
        for (auto& iter : functionRP.getArguments())
        {
            if (std::holds_alternative<Representations::RecordType>(iter.getType()))
            {
                thisFunction->addAttribute(i + (retIsStruct ? 2 : 1),
                                           llvm::Attribute::get(context, llvm::Attribute::ByVal));
            }
            if (auto* ptr = std::get_if<Representations::PointerType>(&iter.getType()); ptr && ptr->isRestricted())
            {
                thisFunction->addAttribute(i + (retIsStruct ? 2 : 1),
                                           llvm::Attribute::get(context, llvm::Attribute::NoAlias));
            }
            i++;
        }
        if (auto [prev, success] = m_namedValues.back().insert({name, {thisFunction, std::move(*type)}}); !success)
        {
            return FailureReason("Redefinition of symbol " + prev->first);
        }
        else
        {
            ft = &std::get<Representations::FunctionType>(prev->second.second.getType());
        }
    }
    else
    {
        auto* result = findValue(name);
        if (!result)
        {
            return FailureReason(
                "Internal compiler error: Function not found in scope even though it was just created");
        }
        const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
        for (auto& iter : node.getDeclarationSpecifiers())
        {
            std::visit(
                overload{[&storageClassSpecifier](const Syntax::StorageClassSpecifier& otherStorageClassSpecifier) {
                             storageClassSpecifier = &otherStorageClassSpecifier;
                         },
                         [](auto&&) {}},
                iter);
        }
        if (*storageClassSpecifier == Syntax::StorageClassSpecifier::Static
            && thisFunction->getLinkage() != llvm::Function::InternalLinkage)
        {
            return FailureReason("static in definition does not match (implicit) extern in declaration");
        }
        ft = &std::get<Representations::FunctionType>(result->second.getType());
    }

    std::size_t i = 0;
    auto* paramterTypeList = std::get_if<Syntax::DirectDeclaratorParentheseParameters>(
        &node.getDeclarator().getDirectDeclarator().getVariant());
    auto* identifierList = std::get_if<Syntax::DirectDeclaratorParentheseIdentifiers>(
        &node.getDeclarator().getDirectDeclarator().getVariant());
    for (auto& iter : thisFunction->args())
    {
        if (iter.hasStructRetAttr())
        {
            continue;
        }
        if (paramterTypeList)
        {
            auto* declarator = std::get_if<std::unique_ptr<Syntax::Declarator>>(
                &paramterTypeList->getParameterTypeList().getParameterList().getParameterDeclarations()[i++].second);
            if (!declarator)
            {
                return FailureReason("Parameter name omitted");
            }
            iter.setName(Representations::declaratorToName(**declarator));
        }
        else if (identifierList)
        {
            // TODO:
        }
    }

    pushScope();
    auto* bb = llvm::BasicBlock::Create(context, "entry", thisFunction);
    builder.SetInsertPoint(bb);
    i = 0;
    for (auto& iter : thisFunction->args())
    {
        if (iter.hasStructRetAttr())
        {
            continue;
        }
        llvm::AllocaInst* alloca = nullptr;
        llvm::IRBuilder<> tmpB(&thisFunction->getEntryBlock(), thisFunction->getEntryBlock().begin());
        if (!iter.hasByValAttr())
        {
            alloca = tmpB.CreateAlloca(iter.getType());
            builder.CreateStore(&iter, alloca);
            if (!m_namedValues.back().insert({iter.getName(), {alloca, ft->getArguments()[i++]}}).second)
            {
                return FailureReason("Parameter name already exists");
            }
        }
        else
        {
            auto* ptrType = llvm::cast<llvm::PointerType>(iter.getType());
            alloca = tmpB.CreateAlloca(ptrType->getPointerElementType());
            auto* zero = builder.getInt32(0);
            auto* value = builder.CreateInBoundsGEP(alloca, {zero, zero});
            builder.CreateStore(&iter, value);
            if (!m_namedValues.back().insert({iter.getName(), {alloca, ft->getArguments()[i++]}}).second)
            {
                return FailureReason("Parameter name already exists");
            }
        }
    }

    auto result = visit(node.getCompoundStatement(), false);
    if (result)
    {
        return result;
    }

    auto& block = thisFunction->back();
    if (block.empty() || !block.back().isTerminator())
    {
        if (!thisFunction->getReturnType()->isVoidTy())
        {
            auto* retType = thisFunction->getReturnType();
            llvm::Value* value = nullptr;
            if (retType->isIntegerTy())
            {
                value = llvm::ConstantInt::get(retType, 0);
            }
            else if (retType->isFloatingPointTy())
            {
                value = llvm::ConstantFP::get(retType, 0);
            }
            else if (retType->isPointerTy())
            {
                value = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(retType));
            }
            builder.CreateRet(value);
        }
        else
        {
            builder.CreateRetVoid();
        }
    }

    popScope();

    if (llvm::verifyFunction(*thisFunction, &llvm::errs()))
    {
        thisFunction->print(llvm::outs());
        std::terminate();
    }

    return {};
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TranslationUnit& node)
{
    clearScope();
    module = std::make_unique<llvm::Module>("main", context);
    std::string error;
    const std::string tripleStr = llvm::sys::getProcessTriple();
    llvm::Triple t(tripleStr);
    if (t.isOSBinFormatCOFF())
    {
        t.setObjectFormat(llvm::Triple::ELF);
    }
    auto target = llvm::TargetRegistry::lookupTarget(t.str(), error);
    if (!target)
    {
        throw std::runtime_error(error);
    }
    auto targetMachine = target->createTargetMachine(t.str(), "generic", "", {}, {});
    module->setDataLayout(targetMachine->createDataLayout());
    module->setTargetTriple(t.str());
    debugBuilder = new llvm::DIBuilder(*module);
    debugBuilder->finalize();
    debugUnit = debugBuilder->createFile("input.c", "../src");
    debugBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C99, debugUnit, "OpenCL Compiler", false, "", 0);
    for (auto& iter : node.getGlobals())
    {
        auto result = visit(iter);
        if (result)
        {
            return result;
        }
    }
    debugBuilder->finalize();
    return {};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    return std::visit([this](auto&& value) { return visit(value); }, node.getVariant());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    return std::visit([this](auto&& value) { return visit(value); }, node.getVariant());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    return std::visit([this](auto&& value) { return visit(value); }, node.getVariant());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::AssignmentExpression& node)
{
    return std::visit([this](auto&& value) -> OpenCL::Codegen::NodeRetType { return visit(value); }, node.getVariant());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Initializer& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CompoundItem& node)
{
    return std::visit([this](auto&& value) { return visit(value); }, node.getVariant());
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Statement& node)
{
    return std::visit([this](auto&& value)->std::optional<OpenCL::FailureReason>  { return visit(value); }, node.getVariant());
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ExternalDeclaration& node)
{
    return std::visit([this](auto&& value) { return visit(value); }, node.getVariant());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TypeName& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Declarator& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::EnumDeclaration& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TypeSpecifier& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclarator& node) {}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk& node)
{
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorStatic& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorAsterisk& node) {}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorParentheseParameters& node)
{
}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers& node)
{
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectAbstractDeclarator& node) {}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList& node)
{
}

OpenCL::Codegen::NodeRetType
    OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression& node)
{
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Pointer& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ParameterTypeList& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ParameterList& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::LabelStatement& node) {}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::GotoStatement& node) {}

std::map<std::string, std::reference_wrapper<const OpenCL::Representations::Type>>
    OpenCL::Codegen::Context::gatherTypedefs() const
{
    std::map<std::string, std::reference_wrapper<const OpenCL::Representations::Type>> result;
    for (auto iter = m_typedefs.rbegin(); iter != m_typedefs.rend(); iter++)
    {
        result.insert(iter->begin(), iter->end());
    }
    return result;
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::Type& node)
{
    return std::visit([this](auto&& value) -> llvm::Type* { return visit(value); }, node.getType());
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::PrimitiveType& node)
{
    if (node.isFloatingPoint())
    {
        if (node.getBitCount() == 32)
        {
            return builder.getFloatTy();
        }
        else if (node.getBitCount() == 64)
        {
            return builder.getDoubleTy();
        }
    }
    else
    {
        switch (node.getBitCount())
        {
            case 0: return builder.getVoidTy();
            case 8: return builder.getInt8Ty();
            case 16: return builder.getInt16Ty();
            case 32: return builder.getInt32Ty();
            case 64: return builder.getInt64Ty();
            default: break;
        }
    }
    return nullptr;
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::ArrayType& node)
{
    return llvm::ArrayType::get(visit(node.getType()), node.getSize());
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::AbstractArrayType& node)
{
    return llvm::PointerType::getUnqual(visit(node.getType()));
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::ValArrayType& node)
{
    return llvm::PointerType::getUnqual(visit(node.getType()));
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::FunctionType& node)
{
    std::vector<llvm::Type*> arguments;
    bool isStruct = std::holds_alternative<Representations::RecordType>(node.getReturnType().getType());
    for (auto& type : node.getArguments())
    {
        auto* argType = visit(type);
        if (!std::holds_alternative<Representations::RecordType>(type.getType()))
        {
            arguments.emplace_back(argType);
        }
        else
        {
            arguments.emplace_back(llvm::PointerType::getUnqual(argType));
        }
    }
    if (isStruct)
    {
        arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(visit(node.getReturnType())));
    }
    return llvm::FunctionType::get(isStruct ? builder.getVoidTy() : visit(node.getReturnType()), arguments,
                                   node.isLastVararg());
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::RecordType& node)
{
    // TODO: structs must be scoped and allow overshadowing
    if (auto* record = module->getTypeByName((node.isUnion() ? "union." : "struct.") + node.getName()))
    {
        return record;
    }
    auto* record = llvm::StructType::create(context, (node.isUnion() ? "union." : "struct.") + node.getName());
    std::vector<llvm::Type*> members;
    std::transform(node.getMembers().begin(), node.getMembers().end(), std::back_inserter(members),
                   [this](const auto& tuple) {
                       auto& [type, name, bitCount] = tuple;
                       if (bitCount >= 0)
                       {
                           throw std::runtime_error("Not implemented yet");
                       }
                       (void)name;
                       return visit(type);
                   });
    if (!node.isUnion())
    {
        record->setBody(members);
    }
    else
    {
        auto* maxElement = *std::max_element(members.begin(), members.end(), [this](llvm::Type* lhs, llvm::Type* rhs) {
            auto lhsSize = module->getDataLayout().getTypeAllocSize(lhs);
            auto rhsSize = module->getDataLayout().getTypeAllocSize(rhs);
            return lhsSize < rhsSize;
        });
        record->setBody(maxElement);
    }
    return record;
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::EnumType&)
{
    return builder.getInt32Ty();
}

llvm::Type* OpenCL::Codegen::Context::visit(const OpenCL::Representations::PointerType& node)
{
    auto* elemType = visit(node.getElementType());
    if (elemType->isVoidTy())
    {
        return builder.getInt8PtrTy();
    }
    else
    {
        return llvm::PointerType::getUnqual(elemType);
    }
}

OpenCL::Representations::Type OpenCL::Codegen::Context::integerPromotion(const OpenCL::Representations::Type& type,
                                                                         llvm::Value** optionalValue)
{
    if (auto* primitive = std::get_if<Representations::PrimitiveType>(&type.getType()))
    {
        if (!primitive->isFloatingPoint() && primitive->getBitCount() < 32)
        {
            if (optionalValue)
            {
                *optionalValue = builder.CreateIntCast(*optionalValue, builder.getInt32Ty(), primitive->isSigned());
            }
            return Representations::PrimitiveType::create(type.isConst(), type.isVolatile(), false, true, 32);
        }
    }
    return type;
}

void OpenCL::Codegen::Context::arithmeticCast(Representations::Type& type, llvm::Value*& value,
                                              const Representations::Type& otherType)
{
    if (type.isCompatibleWith(otherType))
    {
        return;
    }
    auto copy = integerPromotion(otherType);
    type = integerPromotion(type, &value);
    if (auto [primitiveType, otherPrimitive] = std::pair(std::get_if<Representations::PrimitiveType>(&type.getType()),
                                                         std::get_if<Representations::PrimitiveType>(&copy.getType()));
        primitiveType && otherPrimitive)
    {
        if (otherPrimitive->isFloatingPoint()
            && (!primitiveType->isFloatingPoint() || primitiveType->getBitCount() < otherPrimitive->getBitCount()))
        {
            if (primitiveType->isFloatingPoint())
            {
                value = builder.CreateFPCast(value, visit(copy));
                type = copy;
            }
            else if (primitiveType->isSigned())
            {
                value = builder.CreateSIToFP(value, visit(copy));
                type = copy;
            }
            else
            {
                value = builder.CreateUIToFP(value, visit(copy));
                type = copy;
            }
        }
        else if (!primitiveType->isFloatingPoint() && !otherPrimitive->isFloatingPoint())
        {
            if (otherPrimitive->isSigned() == primitiveType->isSigned())
            {
                if (otherPrimitive->getBitCount() > primitiveType->getBitCount())
                {
                    value = builder.CreateIntCast(value, visit(copy), primitiveType->isSigned());
                    type = copy;
                }
            }
            else if (otherPrimitive->getBitCount() >= primitiveType->getBitCount())
            {
                value = builder.CreateIntCast(value, visit(copy), !otherPrimitive->isSigned());
                type = copy;
            }
        }
    }
}

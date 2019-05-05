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

    std::size_t getAlignment(llvm::Type* type)
    {
        if (type->isPointerTy())
        {
            return 8;
        }
        else if (type->isIntegerTy())
        {
            if (type->getIntegerBitWidth() <= 64)
            {
                return type->getIntegerBitWidth() / 8;
            }
            else
            {
                return 16;
            }
        }
        else if (type->isFloatingPointTy())
        {
            if (type->isFloatTy())
            {
                return 4;
            }
            else if (type->isDoubleTy())
            {
                return 8;
            }
            else
            {
                throw std::runtime_error("Not implemented yet");
            }
        }
        else if (type->isStructTy())
        {
            auto* structType = llvm::cast<llvm::StructType>(type);
            std::size_t alignment = 0;
            for (std::size_t i = 0; i < structType->getStructNumElements(); i++)
            {
                alignment = std::max(alignment, getAlignment(structType->getStructElementType(i)));
            }
            return alignment;
        }
        else if (type->isArrayTy())
        {
            return getAlignment(type->getArrayElementType());
        }
        else
        {
            throw std::runtime_error("Not implemented yet");
        }
    }
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
                auto* arrayLoad = llvm::dyn_cast<llvm::LoadInst>(source);
                if (!source)
                {
                    return source;
                }
                auto* ptrRep = builder.CreateInBoundsGEP(arrayLoad->getPointerOperand(), {zero, zero});
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
    if (std::holds_alternative<Representations::FunctionType>(result->second.getType()))
    {
        return std::pair{result->first, result->second};
    }
    else
    {
        return std::pair{builder.CreateLoad(result->first, result->second.isVolatile()), result->second};
    }
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

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionSubscript& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    auto index = visit(node.getExpression());
    if (!index)
    {
        return index;
    }
    auto plus = makePlus(result->second, result->first, index->second, index->first);
    if (!plus)
    {
        return plus;
    }
    auto* ptr = std::get_if<Representations::PointerType>(&plus->second.getType());
    if (!ptr)
    {
        return FailureReason("[] operator can only be applied to pointers and arrays");
    }
    return std::pair{builder.CreateLoad(plus->first), ptr->getElementType()};
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionIncrement& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    auto oldValue = *result;
    auto* load = llvm::dyn_cast<llvm::LoadInst>(oldValue.first);
    if (!load)
    {
        return FailureReason("Postfix ++ can only be applied to lvalue");
    }
    auto optional = std::visit(
        overload{[&](const Representations::PrimitiveType& primitiveType) -> std::optional<FailureReason> {
                     if (primitiveType.isFloatingPoint())
                     {
                         builder.CreateStore(
                             builder.CreateFAdd(oldValue.first, llvm::ConstantFP::get(oldValue.first->getType(), 1)),
                             load->getPointerOperand(), oldValue.second.isVolatile());
                     }
                     else
                     {
                         builder.CreateStore(
                             builder.CreateAdd(oldValue.first, llvm::ConstantInt::get(oldValue.first->getType(), 1)),
                             load->getPointerOperand(), oldValue.second.isVolatile());
                     }
                     return {};
                 },
                 [&](const Representations::PointerType&) -> std::optional<FailureReason> {
                     auto* one = llvm::ConstantInt::get(builder.getInt32Ty(), 1);
                     builder.CreateStore(builder.CreateInBoundsGEP(oldValue.first, {one}), load->getPointerOperand(),
                                         oldValue.second.isVolatile());
                     return {};
                 },
                 [](auto &&) -> std::optional<FailureReason> {
                     return FailureReason("Postfix ++ can only be applied to pointer and arithmetic types");
                 }},
        oldValue.second.getType());
    if (optional)
    {
        return *optional;
    }

    return oldValue;
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionDecrement& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    auto oldValue = *result;
    auto* load = llvm::dyn_cast<llvm::LoadInst>(oldValue.first);
    if (!load)
    {
        return FailureReason("Postfix ++ can only be applied to lvalue");
    }
    auto optional = std::visit(
        overload{[&](const Representations::PrimitiveType& primitiveType) -> std::optional<FailureReason> {
                     if (primitiveType.isFloatingPoint())
                     {
                         builder.CreateStore(
                             builder.CreateFAdd(oldValue.first, llvm::ConstantFP::get(oldValue.first->getType(), -1)),
                             load->getPointerOperand(), oldValue.second.isVolatile());
                     }
                     else
                     {
                         builder.CreateStore(builder.CreateAdd(oldValue.first, llvm::ConstantInt::getSigned(
                                                                                   oldValue.first->getType(), -1)),
                                             load->getPointerOperand(), oldValue.second.isVolatile());
                     }
                     return {};
                 },
                 [&](const Representations::PointerType&) -> std::optional<FailureReason> {
                     auto* one = llvm::ConstantInt::get(builder.getInt32Ty(), -1);
                     builder.CreateStore(builder.CreateInBoundsGEP(oldValue.first, {one}), load->getPointerOperand(),
                                         oldValue.second.isVolatile());
                     return {};
                 },
                 [](auto &&) -> std::optional<FailureReason> {
                     return FailureReason("Postfix ++ can only be applied to pointer and arithmetic types");
                 }},
        oldValue.second.getType());
    if (optional)
    {
        return *optional;
    }

    return oldValue;
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionDot& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    const Representations::RecordType* structType = std::get_if<Representations::RecordType>(&result->second.getType());
    if (!structType)
    {
        return FailureReason("Can only apply . to struct or union type");
    }
    auto* structLoad = llvm::cast<llvm::LoadInst>(result->first);
    auto* zero = builder.getInt32(0);
    auto member = std::find_if(structType->getMembers().begin(), structType->getMembers().end(),
                               [&node](const auto& tuple) { return std::get<1>(tuple) == node.getIdentifier(); });
    if (member == structType->getMembers().end())
    {
        return FailureReason("Could not find member " + node.getIdentifier() + " in " + structType->getName());
    }
    if (!structType->isUnion())
    {
        auto* memberIndex = builder.getInt32(member - structType->getMembers().begin());
        auto* pointer = builder.CreateInBoundsGEP(structLoad->getPointerOperand(), {zero, memberIndex});
        return std::pair{builder.CreateLoad(pointer, std::get<0>(*member).isVolatile()), std::get<0>(*member)};
    }
    else
    {
        auto* pointer = builder.CreateInBoundsGEP(structLoad->getPointerOperand(), {zero, zero});
        auto* cast = builder.CreateBitCast(pointer, llvm::PointerType::getUnqual(visit(std::get<0>(*member))));
        return std::pair{builder.CreateLoad(cast, std::get<0>(*member).isVolatile()), std::get<0>(*member)};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionArrow& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    const Representations::PointerType* pointerType =
        std::get_if<Representations::PointerType>(&result->second.getType());
    if (!pointerType)
    {
        return FailureReason("Can only apply -> to pointer types");
    }
    const Representations::RecordType* structType =
        std::get_if<Representations::RecordType>(&pointerType->getElementType().getType());
    if (!structType)
    {
        return FailureReason("Can only apply -> to pointer to struct or union type");
    }
    auto* zero = builder.getInt32(0);
    auto member = std::find_if(structType->getMembers().begin(), structType->getMembers().end(),
                               [&node](const auto& tuple) { return std::get<1>(tuple) == node.getIdentifier(); });
    if (member == structType->getMembers().end())
    {
        return FailureReason("Could not find member " + node.getIdentifier() + " in " + structType->getName());
    }
    if (!structType->isUnion())
    {
        auto* memberIndex = builder.getInt32(member - structType->getMembers().begin());
        auto* pointer = builder.CreateInBoundsGEP(result->first, {zero, memberIndex});
        return std::pair{builder.CreateLoad(pointer, std::get<0>(*member).isVolatile()), std::get<0>(*member)};
    }
    else
    {
        auto* pointer = builder.CreateInBoundsGEP(result->first, {zero, zero});
        auto* cast = builder.CreateBitCast(pointer, llvm::PointerType::getUnqual(visit(std::get<0>(*member))));
        return std::pair{builder.CreateLoad(cast, std::get<0>(*member).isVolatile()), std::get<0>(*member)};
    }
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionFunctionCall& node)
{
    auto result = visit(node.getPostFixExpression());
    if (!result)
    {
        return result;
    }
    auto [value, type] = *result;
    auto* function = std::get_if<Representations::FunctionType>(&type.getType());
    if (!function)
    {
        return FailureReason("Function call only possible on function type");
    }
    std::vector<llvm::Value*> arguments;
    std::size_t i = 0;
    for (auto& iter : node.getOptionalAssignmentExpressions())
    {
        auto argE = visit(*iter);
        if (!argE)
        {
            return argE;
        }
        auto [arg, argType] = *argE;
        if (!std::holds_alternative<Representations::RecordType>(function->getArguments()[i].getType()))
        {
            arguments.emplace_back(castTo(argType, arg, function->getArguments()[i]));
        }
        else
        {
            auto* load = llvm::dyn_cast<llvm::LoadInst>(arg);
            if (!load)
            {
                return FailureReason("Internal compiler error: Struct passed to function call is not a LostInst");
            }
            llvm::IRBuilder<> tmpB(&builder.GetInsertBlock()->getParent()->getEntryBlock(),
                                   builder.GetInsertBlock()->getParent()->getEntryBlock().begin());
            auto* alloca = tmpB.CreateAlloca(load->getType());
            alloca->setAlignment(getAlignment(load->getType()));
            auto* cast = builder.CreateBitCast(alloca, builder.getInt8PtrTy());
            auto* castSource = builder.CreateBitCast(load->getPointerOperand(), builder.getInt8PtrTy());
            auto* one = builder.getInt32(1);
            auto* size = builder.CreateGEP(
                load->getType(), llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(load->getType())), one);
            builder.CreateMemCpy(cast, 0, castSource, 0, builder.CreatePtrToInt(size, builder.getInt32Ty()));
            arguments.emplace_back(alloca);
        }
        i++;
    }
    // TODO: Make ellipses and non prototype functions work
    if (!std::holds_alternative<Representations::RecordType>(function->getReturnType().getType()))
    {
        return std::pair{builder.CreateCall(value, arguments), function->getReturnType()};
    }
    else
    {
        llvm::IRBuilder<> tmpB(&builder.GetInsertBlock()->getParent()->getEntryBlock(),
                               builder.GetInsertBlock()->getParent()->getEntryBlock().begin());
        auto* allocaType = visit(function->getReturnType());
        auto* alloca = builder.CreateAlloca(allocaType);
        alloca->setAlignment(getAlignment(allocaType));
        arguments.insert(arguments.begin(), alloca);
        builder.CreateCall(value, arguments);
        return std::pair{builder.CreateLoad(alloca), function->getReturnType()};
    }
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
    return visit(node.getPostFixExpression());
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
{
    auto result = visit(node.getUnaryExpression());
    if (!result)
    {
        return result;
    }
    switch (node.getAnOperator())
    {
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement: break;
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
        {
            auto* load = llvm::dyn_cast<llvm::LoadInst>(result->first);
            if (!load)
            {
                return FailureReason("& requires an lvalue as operand");
            }
            return std::pair{load->getPointerOperand(),
                             Representations::PointerType::create(false, false, false, std::move(result->second))};
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
        {
            auto* pointer = std::get_if<Representations::PointerType>(&result->second.getType());
            if (!pointer)
            {
                return FailureReason("Can only dereference pointer type");
            }
            if (std::holds_alternative<Representations::FunctionType>(pointer->getElementType().getType()))
            {
                return std::pair{result->first, pointer->getElementType()};
            }
            else
            {
                return std::pair{builder.CreateLoad(result->first, pointer->getElementType().isVolatile()),
                                 pointer->getElementType()};
            }
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
        {
            auto ret = integerPromotion(result->second, &result->first);
            auto* primitive = std::get_if<Representations::PrimitiveType>(&ret.getType());
            if (!primitive)
            {
                return FailureReason("Unary + can only be applied to arithmetic type");
            }
            return std::pair{result->first, std::move(ret)};
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
        {
            auto ret = integerPromotion(result->second, &result->first);
            auto* primitive = std::get_if<Representations::PrimitiveType>(&ret.getType());
            if (!primitive)
            {
                return FailureReason("Unary - can only be applied to arithmetic type");
            }
            if (!primitive->isFloatingPoint())
            {
                return std::pair{builder.CreateNeg(result->first), std::move(ret)};
            }
            else
            {
                return std::pair{builder.CreateFNeg(result->first), std::move(ret)};
            }
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
        {
            auto ret = integerPromotion(result->second, &result->first);
            auto* primitive = std::get_if<Representations::PrimitiveType>(&ret.getType());
            if (!primitive || primitive->isFloatingPoint())
            {
                return FailureReason("Unary ~ can only be applied to integer type");
            }
            return std::pair{builder.CreateNot(result->first), std::move(ret)};
        }
        case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
        {
            integerPromotion(result->second, &result->first);
            result->first = toBool(result->first);
            if (!result->first)
            {
                return FailureReason("Could not convert value of type " + result->second.getName() + " to bool");
            }
            auto* inv = builder.CreateNot(result->first);
            return std::pair{builder.CreateZExt(inv, builder.getInt32Ty()),
                             Representations::PrimitiveType::create(false, false, false, true, 32)};
        }
    }
    return result;
}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& node) {}

OpenCL::Codegen::NodeRetType OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CastExpression& node)
{
    return std::visit(
        overload{[this](const Syntax::UnaryExpression& unaryExpression) -> OpenCL::Codegen::NodeRetType {
                     return visit(unaryExpression);
                 },
                 [this](const std::pair<Syntax::TypeName, std::unique_ptr<Syntax::CastExpression>>& cast)
                     -> OpenCL::Codegen::NodeRetType {
                     auto result = visit(*cast.second);
                     if (!result)
                     {
                         return result;
                     }
                     std::vector<Representations::SpecifierQualifierRef> refs;
                     refs.reserve(cast.first.getSpecifierQualifiers().size());
                     std::transform(
                         cast.first.getSpecifierQualifiers().begin(), cast.first.getSpecifierQualifiers().end(),
                         std::back_inserter(refs), [](const Syntax::SpecifierQualifier& specifierQualifier) {
                             return std::visit(
                                 [](const auto& value) -> Representations::SpecifierQualifierRef { return value; },
                                 specifierQualifier);
                         });
                     auto type = Representations::declaratorsToType(refs, cast.first.getAbstractDeclarator(),
                                                                    gatherTypedefs(), {}, gatherStructsAndUnions());
                     if (!type)
                     {
                         return type;
                     }
                     return std::pair{castTo(result->second, result->first, *type, true), *type};
                 }},
        node.getVariant());
}

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
        auto termResult = [this, &leftType = leftType, &left = left, &rightType = rightType, &right = right,
                           &op = op]() -> NodeRetType {
            switch (op)
            {
                case Syntax::Term::BinaryDotOperator::BinaryMultiply:
                    return makeMultiply(leftType, left, rightType, right);
                case Syntax::Term::BinaryDotOperator::BinaryDivide: return makeDivide(leftType, left, rightType, right);
                case Syntax::Term::BinaryDotOperator::BinaryRemainder:
                    return makeRemainder(leftType, left, rightType, right);
            }
            return FailureReason("Internal Compiler error: OP enum in term is not defined");
        }();
        if (!termResult)
        {
            return termResult;
        }
        left = termResult->first;
        leftType = termResult->second;
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
        if ((!leftPointer && !rightPointer) || (leftPrimitive && leftPrimitive->isFloatingPoint())
            || (rightPrimitive && rightPrimitive->isFloatingPoint()) || (!leftPrimitive && !rightPrimitive))
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
        auto addResult = op == Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus ?
                             makePlus(leftType, left, rightType, right) :
                               makeMinus(leftType, left, rightType, right);
        if (!addResult)
        {
            return addResult;
        }
        left = addResult->first;
        leftType = addResult->second;
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
        if (!boolean)
        {
            return FailureReason("Could not convert value of type " + booleanE->second.getName() + " to boolean");
        }
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

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ReturnStatement& node)
{
    auto result = visit(node.getExpression());
    if (!result)
    {
        return result.error();
    }
    if (!builder.GetInsertBlock()->getParent()->hasStructRetAttr())
    {
        builder.CreateRet(castTo(result->second, result->first, currentFunction->getReturnType()));
    }
    else
    {
        if (!result->second.isCompatibleWith(currentFunction->getReturnType()))
        {
            return FailureReason(result->second.getName() + " is not compatible with "
                                 + currentFunction->getReturnType().getName());
        }
        builder.CreateStore(result->first, builder.GetInsertBlock()->getParent()->args().begin());
        builder.CreateRetVoid();
    }
    return {};
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ExpressionStatement& node)
{
    if (node.getOptionalExpression())
    {
        auto result = visit(*node.getOptionalExpression());
        if (!result)
        {
            return result.error();
        }
    }
    return {};
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::IfStatement& node)
{
    auto booleanE = visit(node.getExpression());
    if (!booleanE)
    {
        return booleanE.error();
    }
    auto [value, type] = *booleanE;
    value = toBool(value);
    if (!value)
    {
        return FailureReason("Can not convert value of type " + type.getName() + " to boolean");
    }
    auto* function = builder.GetInsertBlock()->getParent();

    auto* thenBB = llvm::BasicBlock::Create(context, "", function);
    auto* elseBB = node.getElseBranch() ? llvm::BasicBlock::Create(context) : nullptr;
    auto* mergeBB = llvm::BasicBlock::Create(context);

    builder.CreateCondBr(value, thenBB, elseBB ? elseBB : mergeBB);

    builder.SetInsertPoint(thenBB);
    auto result = visit(node.getBranch());
    if (result)
    {
        return result;
    }

    if (!builder.GetInsertBlock()->getTerminator())
    {
        builder.CreateBr(mergeBB);
    }

    if (elseBB)
    {
        function->getBasicBlockList().push_back(elseBB);
        builder.SetInsertPoint(elseBB);
        result = visit(*node.getElseBranch());
        if (result)
        {
            return result;
        }
        if (!builder.GetInsertBlock()->getTerminator())
        {
            builder.CreateBr(mergeBB);
        }
    }

    function->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    return {};
}

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
        {
            auto result = std::visit(
                Y{overload{
                    [](auto&& self, const Syntax::DirectDeclaratorParentheseIdentifiers& identifiers)
                        -> std::optional<FailureReason> {
                        if (!identifiers.getIdentifiers().empty())
                        {
                            return FailureReason(
                                "Identifier list not allowed in declaration only in function definition");
                        }
                        else
                        {
                            return std::visit(
                                [&self](auto&& value) -> std::optional<FailureReason> { return self(value); },
                                identifiers.getDirectDeclarator().getVariant());
                        }
                    },
                    [](auto&&, const std::string&) -> std::optional<FailureReason> { return {}; },
                    [](auto&& self,
                       const std::unique_ptr<Syntax::Declarator>& declarator) -> std::optional<FailureReason> {
                        return std::visit([&self](auto&& value) -> std::optional<FailureReason> { return self(value); },
                                          declarator->getDirectDeclarator().getVariant());
                    },
                    [](auto&& self, auto&& value) -> std::optional<FailureReason> {
                        return std::visit([&self](auto&& value) -> std::optional<FailureReason> { return self(value); },
                                          value.getDirectDeclarator().getVariant());
                    }}},
                declarator->getDirectDeclarator().getVariant());
            if (result)
            {
                return result;
            }
        }
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
            gatherTypedefs(), {}, gatherStructsAndUnions());
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
                nullptr, gatherTypedefs(), {}, gatherStructsAndUnions());
            auto [result, inserted] = m_structsUnionsAndEnums.back().insert(
                {std::get<Representations::RecordType>(record.getType()).getName(), record});
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
                    Constant::ConstantEvaluator evaluator(gatherStructsAndUnions(), gatherTypedefs());
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
            alloca->setAlignment(getAlignment(allocaType));
            if (auto [prev, success] = m_namedValues.back().insert({name, {alloca, *type}}); !success)
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

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::HeadWhileStatement& node)
{
    auto* function = builder.GetInsertBlock()->getParent();

    auto* condBB = llvm::BasicBlock::Create(context, "", function);
    builder.CreateBr(condBB);
    auto* blockBB = llvm::BasicBlock::Create(context);
    auto* endBB = llvm::BasicBlock::Create(context);

    breakBlocks.push_back(endBB);
    continueBlocks.push_back(condBB);

    builder.SetInsertPoint(condBB);
    auto boolenE = visit(node.getExpression());
    if (!boolenE)
    {
        return boolenE.error();
    }
    auto [value, type] = *boolenE;
    value = toBool(value);
    if (!value)
    {
        return FailureReason("Can not convert value of type " + type.getName() + " to boolean");
    }
    builder.CreateCondBr(value, blockBB, endBB);

    function->getBasicBlockList().push_back(blockBB);
    builder.SetInsertPoint(blockBB);
    auto result = visit(node.getStatement());
    if (result)
    {
        return result;
    }
    builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(endBB);
    builder.SetInsertPoint(endBB);
    breakBlocks.pop_back();
    continueBlocks.pop_back();
    return {};
}

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
        auto type = Representations::declaratorsToType(specifierQualifiers, node.getDeclarator(), gatherTypedefs(),
                                                       node.getDeclarations(), gatherStructsAndUnions());
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
        if (storageClassSpecifier && *storageClassSpecifier == Syntax::StorageClassSpecifier::Static
            && thisFunction->getLinkage() != llvm::Function::InternalLinkage)
        {
            return FailureReason("static in definition does not match (implicit) extern in declaration");
        }
        ft = &std::get<Representations::FunctionType>(result->second.getType());
    }

    currentFunction = ft;
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
            iter.setName(identifierList->getIdentifiers()[i++]);
        }
    }

    std::map<std::string, Representations::Type> declarationMap;
    for (auto& iter : node.getDeclarations())
    {
        std::vector<Representations::SpecifierQualifierRef> refs;
        for (auto& specifiers : iter.getDeclarationSpecifiers())
        {
            auto result = std::visit(
                overload{[](Syntax::StorageClassSpecifier storageClassSpecifier) -> std::optional<FailureReason> {
                             if (storageClassSpecifier == Syntax::StorageClassSpecifier::Register)
                             {
                                 return {};
                             }
                             else
                             {
                                 return FailureReason(
                                     "Storage class specifiers not allowed in declarations of function parameters");
                             }
                         },
                         [&refs](const Syntax::TypeSpecifier& typeSpecifier) -> std::optional<FailureReason> {
                             refs.emplace_back(typeSpecifier);
                             return {};
                         },
                         [&refs](const Syntax::TypeQualifier& typeQualifier) -> std::optional<FailureReason> {
                             refs.emplace_back(typeQualifier);
                             return {};
                         },
                         [](Syntax::FunctionSpecifier) -> std::optional<FailureReason> {
                             return FailureReason("inline keyword not allowed in this context");
                         }},
                specifiers);
            if (result)
            {
                return result;
            }
        }
        for (auto& pair : iter.getInitDeclarators())
        {
            if (pair.second)
            {
                return FailureReason("Declarations in function definitions are not allowed to have initializers");
            }
            auto result =
                Representations::declaratorsToType(refs, *pair.first, gatherTypedefs(), {}, gatherStructsAndUnions());
            if (!result)
            {
                return result.error();
            }
            declarationMap.emplace(Representations::declaratorToName(*pair.first), *result);
        }
    }
    if (!identifierList && !declarationMap.empty())
    {
        return FailureReason("Declarations even though function has parameter type list");
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
            if (paramterTypeList)
            {
                alloca = tmpB.CreateAlloca(iter.getType());
                alloca->setAlignment(getAlignment(iter.getType()));
                builder.CreateStore(&iter, alloca);
                if (!m_namedValues.back().insert({iter.getName(), {alloca, ft->getArguments()[i++]}}).second)
                {
                    return FailureReason("Parameter name already exists");
                }
            }
            else
            {
                auto result = declarationMap.find(iter.getName());
                if (result == declarationMap.end())
                {
                    alloca = tmpB.CreateAlloca(iter.getType());
                    alloca->setAlignment(getAlignment(iter.getType()));
                    builder.CreateStore(&iter, alloca);
                    if (!m_namedValues.back().insert({iter.getName(), {alloca, ft->getArguments()[i++]}}).second)
                    {
                        return FailureReason("Parameter name already exists");
                    }
                }
                else
                {
                    auto* allocaType = visit(result->second);
                    alloca = tmpB.CreateAlloca(allocaType);
                    alloca->setAlignment(getAlignment(allocaType));
                    builder.CreateStore(castTo(ft->getArguments()[i++], &iter, result->second), alloca);
                    if (!m_namedValues.back().insert({iter.getName(), {alloca, result->second}}).second)
                    {
                        return FailureReason("Parameter name already exists");
                    }
                }
            }
        }
        else
        {
            auto* ptrType = llvm::cast<llvm::PointerType>(iter.getType());
            alloca = tmpB.CreateAlloca(ptrType->getPointerElementType());
            alloca->setAlignment(getAlignment(ptrType->getPointerElementType()));
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
    return std::visit([this](auto&& value) -> std::optional<OpenCL::FailureReason> { return visit(value); },
                      node.getVariant());
}

std::optional<OpenCL::FailureReason> OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ExternalDeclaration& node)
{
    return std::visit([this](auto&& value) { return visit(value); }, node.getVariant());
}

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
                value = builder.CreateIntCast(value, visit(copy), primitiveType->isSigned());
                type = copy;
            }
        }
    }
}
std::map<std::string, OpenCL::Representations::RecordType> OpenCL::Codegen::Context::gatherStructsAndUnions() const
{
    std::map<std::string, OpenCL::Representations::RecordType> result;
    for (auto iter = m_structsUnionsAndEnums.rbegin(); iter != m_structsUnionsAndEnums.rend(); iter++)
    {
        for (auto& [key, value] : *iter)
        {
            if (auto* record = std::get_if<Representations::RecordType>(&value.getType()))
            {
                result.emplace(key, *record);
            }
        }
    }
    return result;
}

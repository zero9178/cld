#include <utility>

#include <utility>

#include "Codegen.hpp"

#include <sstream>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/TargetRegistry.h>
#include <numeric>

namespace
{
    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&& ... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G) -> Y<G>;

    template<class... Ts> struct overload : Ts... { using Ts::operator()...; };
    template<class... Ts> overload(Ts...) -> overload<Ts...>;
}

OpenCL::Expected<std::shared_ptr<OpenCL::Codegen::Type>,
                 OpenCL::FailureReason> OpenCL::Codegen::declaratorsToType(std::vector<Syntax::SpecifierQualifier> specifierQualifiers,
                                                                           std::variant<const Syntax::AbstractDeclarator*,
                                                                                        const Syntax::Declarator*> declarator)
{
    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for(auto& iter : specifierQualifiers)
    {
        if(auto* typeQualifier = std::get_if<Syntax::TypeQualifier>(&iter))
        {
            switch(*typeQualifier)
            {
            case Syntax::TypeQualifier::Const:
            {
                isConst = true;
                break;
            }
            case Syntax::TypeQualifier::Restrict:
            {
                isRestricted = true;
                break;
            }
            case Syntax::TypeQualifier::Volatile:
            {
                isVolatile = true;
                break;
            }
            }
        }
    }
    if(std::count(specifierQualifiers.begin(),specifierQualifiers.end(),std::holds_alternative<Syntax::TypeSpecifier>) > 1)
    {
        return FailureReason("Only one type specifier allowed in type");
    }
    std::visit(Y{overload{
        [&](auto&& self,const Syntax::AbstractDeclarator* abstractDeclarator)->std::shared_ptr<OpenCL::Codegen::Type>
        {

        },
        [&](auto&& self,const Syntax::Declarator* declarator)->std::shared_ptr<OpenCL::Codegen::Type>
        {

        }
    }},declarator);
}

//namespace
//{
//    void castPrimitive(llvm::Value*& value,
//                       bool isSigned,
//                       llvm::Type* destType,
//                       bool destIsSigned,
//                       OpenCL::Codegen::Context& context)
//    {
//        if (value->getType() != destType)
//        {
//            if (value->getType()->isIntegerTy())
//            {
//                if (destType->isIntegerTy())
//                {
//                    value = context.builder.CreateIntCast(value, destType, isSigned);
//                }
//                else if (destType->isFloatingPointTy())
//                {
//                    if (isSigned)
//                    {
//                        value = context.builder.CreateSIToFP(value, destType);
//                    }
//                    else
//                    {
//                        value = context.builder.CreateUIToFP(value, destType);
//                    }
//                }
//                else if (destType->isPointerTy())
//                {
//                    if (value->getType()->getIntegerBitWidth() != 64)
//                    {
//                        value = context.builder.CreateIntCast(value, context.builder.getInt64Ty(), isSigned);
//                    }
//                    value = context.builder.CreateIntToPtr(value, destType);
//                }
//                else
//                {
//                    throw std::runtime_error("Cannot convert type");
//                }
//            }
//            else if (value->getType()->isFloatingPointTy())
//            {
//                if (destType->isFloatingPointTy())
//                {
//                    value = context.builder.CreateFPCast(value, destType);
//                }
//                else if (destType->isIntegerTy())
//                {
//                    if (destIsSigned)
//                    {
//                        value = context.builder.CreateFPToSI(value, destType);
//                    }
//                    else
//                    {
//                        value = context.builder.CreateFPToUI(value, destType);
//                    }
//                }
//                else
//                {
//                    throw std::runtime_error("Cannot convert type");
//                }
//            }
//            else if (value->getType()->isPointerTy())
//            {
//                if (destType->isIntegerTy())
//                {
//                    value = context.builder.CreatePtrToInt(value, destType);
//                }
//                else
//                {
//                    throw std::runtime_error("Cannot convert pointer type to other");
//                }
//            }
//            else if (value->getType()->isArrayTy())
//            {
//                if (llvm::isa<llvm::LoadInst>(value) && destType->isIntOrPtrTy())
//                {
//                    auto* zero = context.builder.getInt32(0);
//                    value = context.builder.CreateInBoundsGEP(llvm::cast<llvm::LoadInst>(value)->getPointerOperand(),
//                                                              {zero, zero});
//                    if (destType->isIntegerTy())
//                    {
//                        value = context.builder.CreatePtrToInt(value, destType);
//                    }
//                }
//                else
//                {
//                    throw std::runtime_error("Cannot convert array type to other");
//                }
//            }
//        }
//        isSigned = destIsSigned;
//    }
//
//    void castToDouble(llvm::Value*& value, bool isSigned, OpenCL::Codegen::Context& context)
//    {
//        if (!value->getType()->isDoubleTy())
//        {
//            if (value->getType()->isIntegerTy())
//            {
//                if (isSigned)
//                {
//                    value = context.builder.CreateSIToFP(value, context.builder.getDoubleTy());
//                }
//                else
//                {
//                    value = context.builder.CreateUIToFP(value, context.builder.getDoubleTy());
//                }
//            }
//            else if (value->getType()->isFloatingPointTy())
//            {
//                value = context.builder.CreateFPCast(value, context.builder.getDoubleTy());
//            }
//            else
//            {
//                throw std::runtime_error("Can't cast to double");
//            }
//        }
//    }
//
//    void castToFloat(llvm::Value*& value, bool isSigned, OpenCL::Codegen::Context& context)
//    {
//        if (!value->getType()->isFloatTy())
//        {
//            if (value->getType()->isIntegerTy())
//            {
//                if (isSigned)
//                {
//                    value = context.builder.CreateSIToFP(value, context.builder.getFloatTy());
//                }
//                else
//                {
//                    value = context.builder.CreateUIToFP(value, context.builder.getFloatTy());
//                }
//            }
//            else if (value->getType()->isFloatingPointTy())
//            {
//                value = context.builder.CreateFPCast(value, context.builder.getFloatTy());
//            }
//            else
//            {
//                throw std::runtime_error("Can't cast to double");
//            }
//        }
//    }
//
//    void arithmeticCast(llvm::Value*& lhs,
//                        bool lhsIsSigned,
//                        llvm::Value*& rhs,
//                        bool rhsIsSigned,
//                        OpenCL::Codegen::Context& context)
//    {
//        if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy())
//        {
//            castToDouble(lhs, lhsIsSigned, context);
//            castToDouble(rhs, rhsIsSigned, context);
//        }
//        else if (lhs->getType()->isFloatTy() || rhs->getType()->isFloatTy())
//        {
//            castToFloat(lhs, lhsIsSigned, context);
//            castToFloat(rhs, rhsIsSigned, context);
//        }
//        else if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy())
//        {
//            if (lhs->getType()->getIntegerBitWidth() < 32u
//                || lhs->getType()->getIntegerBitWidth() < rhs->getType()->getIntegerBitWidth())
//            {
//                lhs = context.builder.CreateIntCast(lhs,
//                                                    context.builder.getIntNTy(std::max(32u,
//                                                                                       rhs->getType()
//                                                                                          ->getIntegerBitWidth())),
//                                                    lhsIsSigned);
//            }
//            if (rhs->getType()->getIntegerBitWidth() < 32u
//                || rhs->getType()->getIntegerBitWidth() < lhs->getType()->getIntegerBitWidth())
//            {
//                rhs = context.builder.CreateIntCast(rhs,
//                                                    context.builder.getIntNTy(std::max(32u,
//                                                                                       lhs->getType()
//                                                                                          ->getIntegerBitWidth())),
//                                                    rhsIsSigned);
//            }
//        }
//        else if (!rhs->getType()->isPointerTy() || !lhs->getType()->isPointerTy())
//        {
//            throw std::runtime_error("Can't cast to common type");
//        }
//    }
//
//    std::size_t getAlignment(llvm::Type* type)
//    {
//        if (type->isPointerTy())
//        {
//            return 8;
//        }
//        else if (type->isIntegerTy())
//        {
//            if (type->getIntegerBitWidth() <= 64)
//            {
//                return type->getIntegerBitWidth() / 8;
//            }
//            else
//            {
//                return 16;
//            }
//        }
//        else if (type->isFloatingPointTy())
//        {
//            if (type->isFloatTy())
//            {
//                return 4;
//            }
//            else if (type->isDoubleTy())
//            {
//                return 8;
//            }
//            else
//            {
//                throw std::runtime_error("Not implemented yet");
//            }
//        }
//        else if (type->isStructTy())
//        {
//            auto* structType = llvm::cast<llvm::StructType>(type);
//            std::size_t alignment = 0;
//            for (std::size_t i = 0; i < structType->getStructNumElements(); i++)
//            {
//                alignment = std::max(alignment, getAlignment(structType->getStructElementType(i)));
//            }
//            return alignment;
//        }
//        else if (type->isArrayTy())
//        {
//            return getAlignment(type->getArrayElementType());
//        }
//        else
//        {
//            throw std::runtime_error("Not implemented yet");
//        }
//    }
//
//    std::unordered_map<std::string, llvm::DIType*> cache;
//
//    class DebugVisitor : public OpenCL::Syntax::NodeVisitor<llvm::DIType*>
//    {
//        OpenCL::Codegen::Context& context;
//
//    public:
//
//        explicit DebugVisitor(OpenCL::Codegen::Context& context) : context(context)
//        {}
//
//        void visit(const OpenCL::Syntax::Expression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PrimaryExpressionIdentifier&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PrimaryExpressionConstant&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PrimaryExpressionParenthese&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionSubscript&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionIncrement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionDecrement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionDot&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionArrow&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionFunctionCall&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::AssignmentExpressionAssignment&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::UnaryExpressionSizeOf&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::CastExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::Term&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::AdditiveExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ShiftExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::RelationalExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::EqualityExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::BitAndExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::BitXorExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::BitOrExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::LogicalAndExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::LogicalOrExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ConditionalExpression&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ReturnStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ExpressionStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::IfStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::SwitchStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::DefaultStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::CaseStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::CompoundStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ForStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::InitializerList&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::Declaration&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ForDeclarationStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::HeadWhileStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::FootWhileStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::BreakStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::ContinueStatement&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::StructOrUnionDeclaration&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::EnumSpecifier&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::TypedefDeclaration&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::FunctionDefinition&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::GlobalDeclaration&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::TranslationUnit&) override
//        {
//
//        }
//
//        void visit(const OpenCL::Syntax::PrimitiveType& node) override
//        {
//            if (node.isVoid())
//            {
//                m_return = nullptr;
//            }
//            auto* result = context.debugBuilder
//                                  ->createBasicType(node.name(), node.getBitCount(), [&node]() -> unsigned int
//                                  {
//                                      if (!node.isFloatingPoint() && node.getBitCount())
//                                      {
//                                          if (node.getBitCount() == 8)
//                                          {
//                                              return node.isSigned() ? llvm::dwarf::DW_ATE_signed_char
//                                                                     : llvm::dwarf::DW_ATE_unsigned_char;
//                                          }
//                                          else
//                                          {
//                                              return node.isSigned() ? llvm::dwarf::DW_ATE_signed
//                                                                     : llvm::dwarf::DW_ATE_unsigned;
//                                          }
//                                      }
//                                      else
//                                      {
//                                          return llvm::dwarf::DW_ATE_float;
//                                      }
//                                  }());
//            if (node.isConst())
//            {
//                m_return = context.debugBuilder->createQualifiedType(llvm::dwarf::DW_TAG_const_type, result);
//            }
//            else
//            {
//                m_return = result;
//            }
//        }
//
//        void visit(const OpenCL::Syntax::PointerType& node) override
//        {
//            node.getType().accept(*this);
//            auto* result = context.debugBuilder->createPointerType(std::get<llvm::DIType*>(m_return), 64, 64);
//            if (node.isConst())
//            {
//                m_return = context.debugBuilder->createQualifiedType(llvm::dwarf::DW_AT_const_value, result);
//            }
//            else
//            {
//                m_return = result;
//            }
//        }
//
//        void visit(const OpenCL::Syntax::ArrayType& node) override
//        {
//            node.getType()->accept(*this);
//            auto* elemenType = std::get<llvm::DIType*>(m_return);
//            node.accept(context);
//            auto* llvmArrayType = context.getReturn<llvm::Type*>();
//            m_return = context.debugBuilder
//                              ->createArrayType(context.module->getDataLayout().getTypeSizeInBits(llvmArrayType),
//                                                getAlignment(llvmArrayType),
//                                                elemenType,
//                                                llvm::MDTuple::get(context.context,
//                                                                   context.debugBuilder
//                                                                          ->getOrCreateSubrange(0, node.getSize())));
//        }
//
//        void visit(const OpenCL::Syntax::StructType& node) override
//        {
//            auto* result = cache.at(node.getName());
//            if (node.isConst())
//            {
//                m_return = context.debugBuilder->createQualifiedType(llvm::dwarf::DW_AT_const_value, result);
//            }
//            else
//            {
//                m_return = result;
//            }
//        }
//
//        void visit(const OpenCL::Syntax::UnionType& node) override
//        {
//            auto* result = cache.at(node.getName());
//            if (node.isConst())
//            {
//                m_return = context.debugBuilder->createQualifiedType(llvm::dwarf::DW_AT_const_value, result);
//            }
//            else
//            {
//                m_return = result;
//            }
//        }
//
//        void visit(const OpenCL::Syntax::EnumType& node) override
//        {
//            auto* result = cache.at(node.getName());
//            if (node.isConst())
//            {
//                m_return = context.debugBuilder->createQualifiedType(llvm::dwarf::DW_AT_const_value, result);
//            }
//            else
//            {
//                m_return = result;
//            }
//        }
//    };
//
//    llvm::DIType* toDwarfType(const std::shared_ptr<OpenCL::Syntax::IType>& type, OpenCL::Codegen::Context& context)
//    {
//        if (auto result = cache.find(type->name()); result != cache.end())
//        {
//            return result->second;
//        }
//        DebugVisitor visitor(context);
//        type->accept(visitor);
//        auto* newDebugType = visitor.getReturn<llvm::DIType*>();
//        cache[type->name()] = newDebugType;
//        return newDebugType;
//    }
//
//    template <class T = void>
//    void emitLocation(const OpenCL::Syntax::Node<T>* node, OpenCL::Codegen::Context& context)
//    {
//        if (!node)
//        {
//            context.builder.SetCurrentDebugLocation(llvm::DebugLoc());
//            return;
//        }
//        auto* scope = context.debugScope.empty() ? context.debugUnit : context.debugScope.back();
//        context.builder.SetCurrentDebugLocation(llvm::DebugLoc::get(node->getLine(), node->getColumn(), scope));
//    }
//
//    llvm::Constant* getZeroFor(llvm::Type* type)
//    {
//        if (type->isIntegerTy())
//        {
//            return llvm::ConstantInt::get(type, 0);
//        }
//        else if (type->isFloatingPointTy())
//        {
//            return llvm::ConstantFP::get(type, 0);
//        }
//        else if (type->isPointerTy())
//        {
//            return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
//        }
//        else if (type->isStructTy())
//        {
//            auto* structType = llvm::cast<llvm::StructType>(type);
//            std::size_t count = structType->getStructNumElements();
//            std::vector<llvm::Constant*> constants;
//            constants.reserve(count);
//            for (std::size_t i = 0; i < count; i++)
//            {
//                constants.push_back(getZeroFor(structType->getTypeAtIndex(i)));
//            }
//            return llvm::ConstantStruct::get(structType, constants);
//        }
//        else if (type->isArrayTy())
//        {
//            auto* arrayType = llvm::cast<llvm::ArrayType>(type);
//            std::size_t count = arrayType->getArrayNumElements();
//            std::vector<llvm::Constant*> constants(count, getZeroFor(arrayType->getArrayElementType()));
//            return llvm::ConstantArray::get(arrayType, constants);
//        }
//        else
//        {
//            throw std::runtime_error("Not implemented yet");
//        }
//    }
//
//    std::size_t elementsNeededForType(llvm::Type* type)
//    {
//        if (type->isArrayTy())
//        {
//            return elementsNeededForType(type->getArrayElementType()) * type->getArrayNumElements();
//        }
//        else if (type->isStructTy())
//        {
//            std::size_t sum = 0;
//            for (std::size_t i = 0; i < type->getStructNumElements(); i++)
//            {
//                sum += elementsNeededForType(type->getStructElementType(i));
//            }
//            return sum;
//        }
//        else
//        {
//            return 1;
//        }
//    }
//
//    //    void match(llvm::ArrayRef<std::pair<std::int64_t, typename OpenCL::Syntax::InitializerList::variant>> list,
//    //               llvm::Value* pointer,
//    //               const std::shared_ptr<OpenCL::Syntax::IType>& type, OpenCL::Codegen::Context& context)
//    //    {
//    //        type->accept(context);
//    //        auto* allocaType = context.getReturn<llvm::Type*>();
//    //        if (list.empty())
//    //        {
//    //            if (allocaType->isStructTy() || allocaType->isArrayTy())
//    //            {
//    //                context.builder.CreateStore(getZeroFor(allocaType), pointer);
//    //            }
//    //            else
//    //            {
//    //                throw std::runtime_error("Scalar initializer can not be empty");
//    //            }
//    //        }
//    //        else if (!allocaType->isStructTy() && !allocaType->isArrayTy())
//    //        {
//    //            auto[value, otherType] = std::visit([&context](auto&& value) -> std::pair<llvm::Value*,
//    //                                                                                      std::shared_ptr<OpenCL::Syntax::IType>>
//    //                                                {
//    //                                                    using T = std::decay_t<decltype(value)>;
//    //                                                    if constexpr(std::is_same_v<T,
//    //                                                                                OpenCL::Syntax::InitializerList>)
//    //                                                    {
//    //                                                        throw std::runtime_error(
//    //                                                            "Only single level of braces allowed for scalar initialization");
//    //                                                    }
//    //                                                    else
//    //                                                    {
//    //                                                        value.accept(context);
//    //                                                        return context.getReturn<OpenCL::Codegen::NodeRetType>();
//    //                                                    }
//    //                                                }, list[0].second);
//    //            castPrimitive(value, otherType->isSigned(), allocaType, type->isSigned(), context);
//    //            context.builder.CreateStore(value, pointer);
//    //        }
//    //        else if (allocaType->isStructTy())
//    //        {
//    //            auto& structInfo = context.structs.at(allocaType->getStructName());
//    //            std::size_t i = 0;
//    //            for (auto& iter : structInfo.types)
//    //            {
//    //                auto* zero = context.builder.getInt32(0);
//    //                auto* index = context.builder.getInt32(i);
//    //                auto* member = context.builder.CreateInBoundsGEP(pointer, {zero, index});
//    //                if (i >= list.size())
//    //                {
//    //                    iter->accept(context);
//    //                    context.builder.CreateStore(getZeroFor(context.getReturn<llvm::Type*>()), member);
//    //                }
//    //                else
//    //                {
//    //                    std::visit([&context, member, iter](auto&& value)
//    //                               {
//    //                                   using T = std::decay_t<decltype(value)>;
//    //                                   if constexpr(std::is_same_v<T, OpenCL::Syntax::InitializerList>)
//    //                                   {
//    //                                       match(value.getNonCommaExpressionsAndBlocks(), member, iter, context);
//    //                                   }
//    //                                   else
//    //                                   {
//    //                                       value.accept(context);
//    //                                       auto[newValue, otherType] = context.getReturn<OpenCL::Codegen::NodeRetType>();
//    //                                       if (auto* load = llvm::dyn_cast<llvm::LoadInst>(newValue);load &&
//    //                                           llvm::isa<llvm::GlobalValue>(load->getPointerOperand())
//    //                                           && load->getType()->getArrayElementType() == context.builder.getInt8Ty())
//    //                                       {
//    //                                           auto* zero = context.builder.getInt32(0);
//    //                                           context.builder.CreateMemCpy(member,
//    //                                                                        0,
//    //                                                                        context.builder
//    //                                                                               .CreateInBoundsGEP(load->getPointerOperand(),
//    //                                                                                                  {zero, zero}),
//    //                                                                        0,
//    //                                                                        load->getType()->getArrayNumElements());
//    //                                       }
//    //                                       else
//    //                                       {
//    //                                           iter->accept(context);
//    //                                           castPrimitive(newValue,
//    //                                                         otherType->isSigned(),
//    //                                                         context.getReturn<llvm::Type*>(),
//    //                                                         iter->isSigned(),
//    //                                                         context);
//    //                                           context.builder.CreateStore(newValue, member);
//    //                                       }
//    //                                   }
//    //                               }, list[i].second);
//    //                }
//    //                i++;
//    //            }
//    //        }
//    //        else if (allocaType->isArrayTy())
//    //        {
//    //            auto* zero = context.builder.getInt32(0);
//    //            auto arrayType = std::dynamic_pointer_cast<OpenCL::Syntax::ArrayType>(type);
//    //            auto elementSize = elementsNeededForType(allocaType->getArrayElementType());
//    //            std::shared_ptr heldType = arrayType->getType()->clone();
//    //            std::size_t i = 0;
//    //            std::set<std::size_t> needsNullInitialization;
//    //            std::generate_n(std::inserter(needsNullInitialization, needsNullInitialization.end()),
//    //                            allocaType->getArrayNumElements(),
//    //                            [&]
//    //                            {
//    //                                return needsNullInitialization.size();
//    //                            });
//    //            for (auto iter = list.begin(); iter < list.end(); i++)
//    //            {
//    //                if (iter->first != -1)
//    //                {
//    //                    i = iter->first;
//    //                }
//    //                if (i >= allocaType->getArrayNumElements())
//    //                {
//    //                    throw std::runtime_error("More elements specified in initializer list than elements in array");
//    //                }
//    //                needsNullInitialization.erase(i);
//    //                auto* index = context.builder.getInt32(i);
//    //                auto* member = context.builder.CreateInBoundsGEP(pointer, {zero, index});
//    //                auto end = iter + elementSize > list.end() ? list.end() : iter + elementSize;
//    //                auto result = std::find_if(iter, end, [](auto&& value)
//    //                {
//    //                    return std::holds_alternative<OpenCL::Syntax::InitializerList>(value.second);
//    //                });
//    //                if (result == iter && std::holds_alternative<OpenCL::Syntax::InitializerList>(result->second))
//    //                {
//    //                    match(std::get<OpenCL::Syntax::InitializerList>(result->second)
//    //                              .getNonCommaExpressionsAndBlocks(),
//    //                          member,
//    //                          heldType,
//    //                          context);
//    //                    iter++;
//    //                }
//    //                else
//    //                {
//    //                    if (iter == result)
//    //                    {
//    //                        result++;
//    //                    }
//    //                    match({iter, result}, member, heldType, context);
//    //                    iter = result;
//    //                }
//    //            }
//    //            for (auto iter : needsNullInitialization)
//    //            {
//    //                auto* index = context.builder.getInt32(iter);
//    //                auto* member = context.builder.CreateInBoundsGEP(pointer, {zero, index});
//    //                context.builder.CreateStore(getZeroFor(allocaType->getArrayElementType()), member);
//    //            }
//    //        }
//    //    }
//
//    llvm::Value* toBool(llvm::Value* value, OpenCL::Codegen::Context& context)
//    {
//        if (value->getType()->isIntegerTy())
//        {
//            if (value->getType()->getIntegerBitWidth() > 1)
//            {
//                return context.builder
//                              .CreateICmpNE(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 0));
//            }
//            return value;
//        }
//        else if (value->getType()->isFloatingPointTy())
//        {
//            return context.builder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
//        }
//        else if (value->getType()->isPointerTy())
//        {
//            return context.builder.CreateICmpNE(value,
//                                                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(value
//                                                                                                                 ->getType())));
//        }
//        else if (value->getType()->isArrayTy())
//        {
//            return context.builder.getIntN(1, 0);
//        }
//        else
//        {
//            return nullptr;
//        }
//    }
//}
//
//void OpenCL::Codegen::Context::addValueToScope(const std::string& name, const OpenCL::Codegen::Context::tuple& value)
//{
//    m_namedValues.back()[name] = value;
//}
//
//void OpenCL::Codegen::Context::doForLoop(const OpenCL::Syntax::Expression* controlling,
//                                         const OpenCL::Syntax::Expression* post,
//                                         const OpenCL::Syntax::Statement& statement)
//{
//    auto* function = builder.GetInsertBlock()->getParent();
//
//    auto* postBB = llvm::BasicBlock::Create(context, "post", function);
//    auto* condBB = llvm::BasicBlock::Create(context, "cond");
//    builder.CreateBr(condBB);
//    auto* blockBB = llvm::BasicBlock::Create(context, "block");
//    auto* endBB = llvm::BasicBlock::Create(context, "end");
//
//    breakBlocks.push_back(endBB);
//    continueBlocks.push_back(postBB);
//
//    builder.SetInsertPoint(postBB);
//    if (post)
//    {
//        post->accept(*this);
//    }
//    builder.CreateBr(condBB);
//
//    function->getBasicBlockList().push_back(condBB);
//    builder.SetInsertPoint(condBB);
//    llvm::Value* value = nullptr;
//    if (controlling)
//    {
//        controlling->accept(*this);
//        value = std::get<NodeRetType>(m_return).first;
//    }
//    else
//    {
//        value = builder.getInt1(true);
//    }
//    value = toBool(value, *this);
//    builder.CreateCondBr(value, blockBB, endBB);
//
//    function->getBasicBlockList().push_back(blockBB);
//    builder.SetInsertPoint(blockBB);
//    statement.accept(*this);
//    builder.CreateBr(postBB);
//
//    function->getBasicBlockList().push_back(endBB);
//    builder.SetInsertPoint(endBB);
//    breakBlocks.pop_back();
//    continueBlocks.pop_back();
//}
//
//void OpenCL::Codegen::Context::visit(const Syntax::Expression& node)
//{
//    emitLocation(&node, *this);
//    for (auto& iter : node.getAssignmentExpressions())
//    {
//        iter.accept(*this);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpressionIdentifier& node)
//{
//    emitLocation(&node, *this);
//    auto[value, sign] = getNamedValue(node.getIdentifier());
//    if (!value)
//    {
//        m_return.emplace<NodeRetType>(module->getFunction(node.getIdentifier()), sign);
//        return;
//    }
//    m_return.emplace<NodeRetType>(builder.CreateLoad(value), sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpressionConstant& node)
//{
//    emitLocation(&node, *this);
//    static std::unordered_map<std::string, llvm::Value*> cache;
//    m_return = std::visit([this](auto&& value) -> std::pair<llvm::Value*, std::shared_ptr<OpenCL::Syntax::IType>>
//                          {
//                              using T = std::decay_t<decltype(value)>;
//                              if constexpr(std::is_same_v<T, std::int32_t>)
//                              {
//                                  return {builder.getInt32(value),
//                                          std::make_shared<Syntax::PrimitiveType>(32, false, false, true)};
//                              }
//                              else if constexpr(std::is_same_v<T, std::uint32_t>)
//                              {
//                                  return {builder.getInt32(value),
//                                          std::make_shared<Syntax::PrimitiveType>(32, false, false, true)};
//                              }
//                              else if constexpr(std::is_same_v<T, std::int64_t>)
//                              {
//                                  return {builder.getInt64(value),
//                                          std::make_shared<Syntax::PrimitiveType>(64, false, false, true)};
//                              }
//                              else if constexpr(std::is_same_v<T, std::uint64_t>)
//                              {
//                                  return {builder.getInt64(value),
//                                          std::make_shared<Syntax::PrimitiveType>(64, false, false, false)};
//                              }
//                              else if constexpr(std::is_same_v<T, float>)
//                              {
//                                  return {llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value),
//                                          std::make_shared<Syntax::PrimitiveType>(32, false, true, true)};
//                              }
//                              else if constexpr(std::is_same_v<T, double>)
//                              {
//                                  return {llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value),
//                                          std::make_shared<Syntax::PrimitiveType>(64, false, true, true)};
//                              }
//                              else if constexpr(std::is_same_v<T, std::string>)
//                              {
//                                  llvm::Value* string = nullptr;
//                                  if (auto result = cache.find(value); result != cache.end())
//                                  {
//                                      string = result->second;
//                                  }
//                                  else
//                                  {
//                                      string = builder.CreateGlobalString(value);
//                                      cache.emplace(value, string);
//                                  }
//                                  return {builder.CreateLoad(string),
//                                          std::make_shared<Syntax::PointerType>(std::make_unique<Syntax::PrimitiveType>(
//                                              8,
//                                              false,
//                                              false,
//                                              true),
//                                                                                false)};
//                              }
//                              else
//                              {
//                                  throw std::runtime_error("Not implemented");
//                              }
//                          }, node.getValue());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpressionParenthese& node)
//{
//    emitLocation(&node, *this);
//    return node.getExpression().accept(*this);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression& node)
//{
//    emitLocation(&node, *this);
//    return node.getPrimaryExpression().accept(*this);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionSubscript& node)
//{
//    emitLocation(&node, *this);
//    node.getPostFixExpression().accept(*this);
//    auto[value, sign] = std::get<NodeRetType>(m_return);
//    if (llvm::isa<llvm::ArrayType>(value->getType()))
//    {
//        auto arrayType = std::dynamic_pointer_cast<Syntax::ArrayType>(sign);
//        auto* arrayPointer = llvm::cast<llvm::LoadInst>(value)->getPointerOperand();
//        node.getExpression().accept(*this);
//        auto* index = std::get<NodeRetType>(m_return).first;
//        auto* zero = builder.getIntN(index->getType()->getIntegerBitWidth(), 0);
//        m_return.emplace<NodeRetType>(builder.CreateLoad(builder.CreateInBoundsGEP(arrayPointer, {zero, index})),
//                                      arrayType->getType()->clone());
//    }
//    else if (llvm::isa<llvm::PointerType>(value->getType()))
//    {
//        auto pointerType = std::dynamic_pointer_cast<Syntax::PointerType>(sign);
//        node.getExpression().accept(*this);
//        auto* index = std::get<NodeRetType>(m_return).first;
//        m_return.emplace<NodeRetType>(builder.CreateLoad(builder.CreateInBoundsGEP(value, index)),
//                                      pointerType->getType().clone());
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionIncrement& node)
//{
//    emitLocation(&node, *this);
//    node.getPostFixExpression().accept(*this);
//    auto[value, sign] = std::get<NodeRetType>(m_return);
//    auto* load = llvm::cast_or_null<llvm::LoadInst>(value);
//    if (!load)
//    {
//        throw std::runtime_error("Can't increment non lvalue");
//    }
//    llvm::Value* newValue = nullptr;
//    if (value->getType()->isIntegerTy())
//    {
//        newValue = builder.CreateAdd(value, builder.getIntN(value->getType()->getIntegerBitWidth(), 1));
//    }
//    else if (value->getType()->isFloatingPointTy())
//    {
//        newValue = builder.CreateFAdd(value, llvm::ConstantFP::get(value->getType(), 1));
//    }
//    else if (value->getType()->isPointerTy())
//    {
//        newValue = builder.CreateInBoundsGEP(value, builder.getInt32(1));
//    }
//    else
//    {
//        throw std::runtime_error("Can't increment value that is not an integer or floating point type");
//    }
//    builder.CreateStore(newValue, load->getPointerOperand());
//    m_return.emplace<NodeRetType>(value, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionDecrement& node)
//{
//    emitLocation(&node, *this);
//    node.getPostFixExpression().accept(*this);
//    auto[value, sign] = std::get<NodeRetType>(m_return);
//    auto* load = llvm::cast_or_null<llvm::LoadInst>(value);
//    if (!load)
//    {
//        throw std::runtime_error("Can't increment non lvalue");
//    }
//    llvm::Value* newValue = nullptr;
//    if (value->getType()->isIntegerTy())
//    {
//        newValue = builder.CreateSub(value, builder.getIntN(value->getType()->getIntegerBitWidth(), 1));
//    }
//    else if (value->getType()->isFloatingPointTy())
//    {
//        newValue = builder.CreateFSub(value, llvm::ConstantFP::get(value->getType(), 1));
//    }
//    else if (value->getType()->isPointerTy())
//    {
//        newValue = builder.CreateInBoundsGEP(value, builder.getInt32(-1));
//    }
//    else
//    {
//        throw std::runtime_error("Can't increment value that is not an integer or floating point type");
//    }
//    builder.CreateStore(newValue, load->getPointerOperand());
//    m_return.emplace<NodeRetType>(value, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionDot& node)
//{
//    emitLocation(&node, *this);
//    node.getPostFixExpression().accept(*this);
//    auto* structValue = std::get<NodeRetType>(m_return).first;
//    auto* structLoad = llvm::cast<llvm::LoadInst>(structValue);
//    auto* type = llvm::dyn_cast<llvm::StructType>(structValue->getType());
//    if (!type)
//    {
//        throw std::runtime_error("Can only apply . to struct or union");
//    }
//    auto* zero = builder.getInt32(0);
//    auto& structInfo = structs.at(structValue->getType()->getStructName());
//    if (!structInfo.isUnion)
//    {
//        auto* index = builder.getInt32(structInfo.order.at(node.getIdentifier()));
//        auto memberType = structInfo.types.at(index->getValue().getLimitedValue());
//        auto* pointer = builder.CreateInBoundsGEP(structLoad->getPointerOperand(), {zero, index});
//        m_return.emplace<NodeRetType>(builder.CreateLoad(pointer), memberType);
//    }
//    else
//    {
//        auto memberType = structInfo.types.at(structInfo.order.at(node.getIdentifier()));
//        auto* pointer = builder.CreateInBoundsGEP(structLoad->getPointerOperand(), {zero, zero});
//        memberType->accept(*this);
//        auto* cast = builder.CreateBitCast(pointer, llvm::PointerType::getUnqual(std::get<llvm::Type*>(m_return)));
//        m_return.emplace<NodeRetType>(builder.CreateLoad(cast), memberType);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionArrow& node)
//{
//    emitLocation(&node, *this);
//    node.getPostFixExpression().accept(*this);
//    auto* structValue = std::get<NodeRetType>(m_return).first;
//    auto* type = llvm::dyn_cast<llvm::StructType>(structValue->getType()->getPointerElementType());
//    if (!type)
//    {
//        throw std::runtime_error("Can only apply -> to pointer to struct or union");
//    }
//    auto* zero = builder.getInt32(0);
//    auto& structInfo = structs.at(type->getName());
//    if (!structInfo.isUnion)
//    {
//        auto* index = builder.getInt32(structInfo.order[node.getIdentifier()]);
//        auto memberType = structInfo.types[index->getValue().getLimitedValue()];
//        auto* pointer = builder.CreateInBoundsGEP(structValue, {zero, index});
//        m_return.emplace<NodeRetType>(builder.CreateLoad(pointer), memberType);
//    }
//    else
//    {
//        auto memberType = structInfo.types.at(structInfo.order.at(node.getIdentifier()));
//        auto* pointer = builder.CreateInBoundsGEP(structValue, {zero, zero});
//        memberType->accept(*this);
//        auto* cast = builder.CreateBitCast(pointer, llvm::PointerType::getUnqual(std::get<llvm::Type*>(m_return)));
//        m_return.emplace<NodeRetType>(builder.CreateLoad(cast), memberType);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionFunctionCall& node)
//{
//    emitLocation(&node, *this);
//    node.getPostFixExpression().accept(*this);
//    auto value = std::get<NodeRetType>(m_return).first;
//    if (!value->getType()->isFunctionTy() && !value->getType()->isPointerTy()
//        && llvm::cast<llvm::PointerType>(value->getType())->getElementType()->isFunctionTy())
//    {
//        throw std::runtime_error("Called object is not a function or function pointer");
//    }
//    auto function = getFunction(value->getName());
//    std::vector<llvm::Value*> arguments;
//    std::size_t i = 0;
//    for (auto& iter : node.getOptionalAssignmentExpressions())
//    {
//        iter->accept(*this);
//        auto[arg, signarg] = std::get<NodeRetType>(m_return);
//        if (!dynamic_cast<const Syntax::StructType*>(function.arguments[i]))
//        {
//            function.arguments[i]->accept(*this);
//            castPrimitive(arg,
//                          signarg->isSigned(),
//                          std::get<llvm::Type*>(m_return),
//                          function.arguments[i]->isSigned(),
//                          *this);
//            arguments.emplace_back(arg);
//        }
//        else
//        {
//            auto* load = llvm::cast<llvm::LoadInst>(arg);
//            llvm::IRBuilder<>
//                tmpB(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
//            auto* alloca = tmpB.CreateAlloca(load->getType());
//            alloca->setAlignment(getAlignment(load->getType()));
//            auto* cast = builder.CreateBitCast(alloca, builder.getInt8PtrTy());
//            auto* castSource = builder.CreateBitCast(load->getPointerOperand(), builder.getInt8PtrTy());
//            auto* one = builder.getInt32(1);
//            auto* size = builder.CreateGEP(load->getType(),
//                                           llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(load->getType())),
//                                           one);
//            builder.CreateMemCpy(cast,
//                                 0,
//                                 castSource,
//                                 0,
//                                 builder.CreatePtrToInt(size, builder.getInt32Ty()));
//            arguments.emplace_back(alloca);
//        }
//        i++;
//    }
//    if (!dynamic_cast<const Syntax::StructType*>(function.retType.get()))
//    {
//        m_return.emplace<NodeRetType>(builder.CreateCall(value, arguments), function.retType);
//    }
//    else
//    {
//        llvm::IRBuilder<>
//            tmpB(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
//        function.retType->accept(*this);
//        auto* alloca = tmpB.CreateAlloca(std::get<llvm::Type*>(m_return));
//        alloca->setAlignment(getAlignment(std::get<llvm::Type*>(m_return)));
//        arguments.insert(arguments.begin(), alloca);
//        builder.CreateCall(value, arguments);
//        m_return.emplace<NodeRetType>(builder.CreateLoad(alloca), function.retType);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer& node)
//{
//    emitLocation(&node, *this);
//    node.getType()->accept(*this);
//    auto* type = std::get<llvm::Type*>(m_return);
//    llvm::IRBuilder<>
//        tmpB(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
//    auto* alloca = tmpB.CreateAlloca(type);
//    alloca->setAlignment(getAlignment(type));
//    //    if (type->isStructTy())
//    //    {
//    //        auto* zero = builder.getInt32(0);
//    //        auto& structInfo = structs.at(type->getStructName());
//    //        if (node.getNonCommaExpressions().size() < type->getStructNumElements())
//    //        {
//    //            throw std::runtime_error("Amount of values in intializer not equal to fields in struct");
//    //        }
//    //        for (std::size_t i = 0; i < type->getStructNumElements(); i++)
//    //        {
//    //            node.getNonCommaExpressions().at(i)->accept(*this);
//    //            auto[value, ntype] = std::get<NodeRetType>(m_return);
//    //            ntype->accept(*this);
//    //            if (std::get<llvm::Type*>(m_return) != type->getStructElementType(i))
//    //            {
//    //                castPrimitive(value,
//    //                              ntype->isSigned(),
//    //                              type->getStructElementType(i),
//    //                              structInfo.types.at(i)->isSigned(),
//    //                              *this);
//    //            }
//    //            auto* index = builder.getInt32(i);
//    //            auto* field = builder.CreateInBoundsGEP(alloca, {zero, index});
//    //            builder.CreateStore(value, field);
//    //        }
//    //    }
//    //    else
//    //    {
//    //        if (node.getNonCommaExpressions().empty())
//    //        {
//    //            throw std::runtime_error("Amount of values unequal to 1");
//    //        }
//    //        node.getNonCommaExpressions()[0]->accept(*this);
//    //        auto[value, ntype] = std::get<NodeRetType>(m_return);
//    //        builder.CreateStore(value, alloca);
//    //    }
//    m_return.emplace<NodeRetType>(builder.CreateLoad(alloca), node.getType());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::AssignmentExpressionAssignment& node)
//{
//    emitLocation(&node, *this);
//    node.getUnaryFactor().accept(*this);
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    if (sign->isConst())
//    {
//        throw std::runtime_error("Can't assign value to const");
//    }
//    auto* load = llvm::dyn_cast<llvm::LoadInst>(left);
//    if (!load)
//    {
//        throw std::runtime_error("Not allowed to assign to non lvalue");
//    }
//    switch (node.getAssignOperator())
//    {
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::NoOperator:
//    {
//        node.getNonCommaExpression().accept(*this);
//        auto[value, sign] = std::get<NodeRetType>(m_return);
//        castPrimitive(value, sign->isSigned(), left->getType(), sign->isSigned(), *this);
//        builder.CreateStore(value, load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::PlusAssign:
//    {
//        llvm::Value* current = left;
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        if (current->getType()->isIntegerTy())
//        {
//            current = builder.CreateAdd(current, newValue);
//        }
//        else if (current->getType()->isFloatingPointTy())
//        {
//            current = builder.CreateFAdd(current, newValue);
//        }
//        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), *this);
//        builder.CreateStore(current, load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::MinusAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        if (current->getType()->isIntegerTy())
//        {
//            current = builder.CreateSub(current, newValue);
//        }
//        else if (current->getType()->isFloatingPointTy())
//        {
//            current = builder.CreateFSub(current, newValue);
//        }
//        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), *this);
//        builder.CreateStore(current, load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::DivideAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        if (current->getType()->isIntegerTy())
//        {
//            if (sign || newSign)
//            {
//                current = builder.CreateSDiv(current, newValue);
//            }
//            else
//            {
//                current = builder.CreateUDiv(current, newValue);
//            }
//        }
//        else
//        {
//            current = builder.CreateFDiv(current, newValue);
//        }
//        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), *this);
//        builder.CreateStore(current, load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::MultiplyAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        if (current->getType()->isIntegerTy())
//        {
//            current = builder.CreateMul(current, newValue);
//        }
//        else
//        {
//            current = builder.CreateFMul(current, newValue);
//        }
//        castPrimitive(current, sign->isSigned() || newSign->isSigned(), left->getType(), sign->isSigned(), *this);
//        builder.CreateStore(current, load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::ModuloAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        builder.CreateStore(builder.CreateSRem(current, newValue), load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        builder.CreateStore(builder.CreateShl(current, newValue), load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::RightShiftAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        builder.CreateStore(builder.CreateAShr(current, newValue), load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::BitAndAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        builder.CreateStore(builder.CreateAnd(current, newValue), load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::BitOrAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        builder.CreateStore(builder.CreateOr(current, newValue), load->getPointerOperand());
//        break;
//    }
//    case Syntax::AssignmentExpressionAssignment::AssignOperator::BitXorAssign:
//    {
//        llvm::Value* current = builder.CreateLoad(left);
//        node.getNonCommaExpression().accept(*this);
//        auto[newValue, newSign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(current, sign->isSigned(), newValue, newSign->isSigned(), *this);
//        builder.CreateStore(builder.CreateXor(current, newValue), load->getPointerOperand());
//        break;
//    }
//    }
//    m_return.emplace<NodeRetType>(builder.CreateLoad(load->getPointerOperand()), sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression& node)
//{
//    emitLocation(&node, *this);
//    return node.getPostFixExpression().accept(*this);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator& node)
//{
//    emitLocation(&node, *this);
//    node.getUnaryExpression().accept(*this);
//    auto[rhs, sign] = std::get<NodeRetType>(m_return);
//    switch (node.getAnOperator())
//    {
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Increment:
//    {
//        llvm::Value* newValue = nullptr;
//        if (rhs->getType()->isIntegerTy())
//        {
//            newValue = builder.CreateAdd(rhs, builder.getIntN(rhs->getType()->getIntegerBitWidth(), 1));
//        }
//        else if (rhs->getType()->isFloatingPointTy())
//        {
//            newValue = builder.CreateFAdd(rhs, llvm::ConstantFP::get(rhs->getType(), 0));
//        }
//        else
//        {
//            throw std::runtime_error("Cannot apply unary ++ to type");
//        }
//        if (!llvm::isa<llvm::LoadInst>(rhs))
//        {
//            throw std::runtime_error("Cannot apply unary ++ to non lvalue");
//        }
//        builder.CreateStore(newValue, llvm::cast<llvm::LoadInst>(rhs)->getPointerOperand());
//        m_return.emplace<NodeRetType>(newValue, sign);
//        return;
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Decrement:
//    {
//        llvm::Value* newValue = nullptr;
//        if (rhs->getType()->isIntegerTy())
//        {
//            newValue = builder.CreateSub(rhs, builder.getIntN(rhs->getType()->getIntegerBitWidth(), 1));
//        }
//        else if (rhs->getType()->isFloatingPointTy())
//        {
//            newValue = builder.CreateFSub(rhs, llvm::ConstantFP::get(rhs->getType(), 0));
//        }
//        else
//        {
//            throw std::runtime_error("Cannot apply unary -- to type");
//        }
//        if (!llvm::isa<llvm::LoadInst>(rhs))
//        {
//            throw std::runtime_error("Cannot apply unary -- to non lvalue");
//        }
//        builder.CreateStore(newValue, llvm::cast<llvm::LoadInst>(rhs)->getPointerOperand());
//        m_return.emplace<NodeRetType>(newValue, sign);
//        return;
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Ampersand:
//    {
//        if (!llvm::isa<llvm::LoadInst>(rhs))
//        {
//            throw std::runtime_error("Cannot take address of type");
//        }
//        m_return.emplace<NodeRetType>(llvm::cast<llvm::LoadInst>(rhs)->getPointerOperand(), sign);
//        return;
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Asterisk:
//    {
//        if (auto result = std::dynamic_pointer_cast<Syntax::PointerType>(sign);result)
//        {
//            m_return.emplace<NodeRetType>(builder.CreateLoad(rhs), result->getType().clone());
//            return;
//        }
//        else
//        {
//            throw std::runtime_error("Can't apply unary * to non pointer type");
//        }
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Plus:
//    {
//        if (rhs->getType()->isIntegerTy() && rhs->getType()->getIntegerBitWidth() < 32)
//        {
//            rhs = builder.CreateIntCast(rhs, builder.getInt32Ty(), sign->isSigned());
//        }
//        m_return.emplace<NodeRetType>(rhs, sign);
//        return;
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::Minus:
//    {
//        if (rhs->getType()->isIntegerTy())
//        {
//            m_return.emplace<NodeRetType>(builder.CreateNeg(rhs),
//                                          std::make_shared<Syntax::PrimitiveType>(std::static_pointer_cast<Syntax::PrimitiveType>(
//                                              sign)
//                                                                                      ->getBitCount(),
//                                                                                  false,
//                                                                                  false,
//                                                                                  true));
//            return;
//        }
//        else
//        {
//            m_return.emplace<NodeRetType>(builder.CreateFNeg(rhs),
//                                          std::make_shared<Syntax::PrimitiveType>(std::static_pointer_cast<Syntax::PrimitiveType>(
//                                              sign)
//                                                                                      ->getBitCount(),
//                                                                                  false,
//                                                                                  true,
//                                                                                  true));
//            return;
//        }
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::BitNot:
//    {
//        if (!rhs->getType()->isIntegerTy())
//        {
//            throw std::runtime_error("Cannot apply ~ to non integer type");
//        }
//        m_return.emplace<NodeRetType>(builder.CreateNot(rhs), sign);
//        return;
//    }
//    case Syntax::UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot:
//    {
//        if (rhs->getType()->isIntegerTy() && rhs->getType()->getIntegerBitWidth() < 1)
//        {
//            rhs = builder.CreateICmpNE(rhs, builder.getIntN(rhs->getType()->getIntegerBitWidth(), 0));
//        }
//        else if (rhs->getType()->isFloatingPointTy())
//        {
//            rhs = builder.CreateFCmpUNE(rhs, llvm::ConstantFP::get(rhs->getType(), 0));
//        }
//        else
//        {
//            throw std::runtime_error("Cannot apply ! operator to specified type");
//        }
//        m_return.emplace<NodeRetType>(builder.CreateZExt(builder.CreateNot(rhs), builder.getInt32Ty()), sign);
//    }
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpressionSizeOf& node)
//{
//    emitLocation(&node, *this);
//    std::visit([this](auto&& value)
//               {
//                   using T = std::decay_t<decltype(value)>;
//                   if constexpr(std::is_same_v<T, std::unique_ptr<OpenCL::Syntax::UnaryExpression>>)
//                   {
//                       throw std::runtime_error("Not implemented yet");
//                   }
//                   else
//                   {
//                       value->accept(*this);
//                       auto size = module->getDataLayout().getTypeAllocSize(std::get<llvm::Type*>(m_return));
//                       m_return.emplace<NodeRetType>(builder.getIntN(64, size),
//                                                     std::make_unique<Syntax::PrimitiveType>(64, false, false, false));
//                   }
//               }, node.getUnaryOrType());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CastExpression& node)
//{
//    emitLocation(&node, *this);
//    std::visit([this](auto&& value)
//               {
//                   using T = std::decay_t<decltype(value)>;
//                   if constexpr(std::is_same_v<T, Syntax::UnaryExpression>)
//                   {
//                       return value.accept(*this);
//                   }
//                   else
//                   {
//                       auto&[type, cast] = value;
//                       cast->accept(*this);
//                       auto[rhs, sign] = std::get<NodeRetType>(m_return);
//                       type->accept(*this);
//                       castPrimitive(rhs, sign->isSigned(), std::get<llvm::Type*>(m_return), type->isSigned(), *this);
//                       m_return.emplace<NodeRetType>(rhs, type);
//                   }
//               }, node.getUnaryOrCast());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Term& node)
//{
//    emitLocation(&node, *this);
//    node.getCastExpression().accept(*this);
//    if (node.getOptionalCastExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto&[op, cast] : node.getOptionalCastExpressions())
//    {
//        cast.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), *this);
//
//        switch (op)
//        {
//        case Syntax::Term::BinaryDotOperator::BinaryMultiply:
//            if (left->getType()->isIntegerTy())
//            {
//                left = builder.CreateMul(left, right);
//            }
//            else
//            {
//                left = builder.CreateFMul(left, right);
//            }
//            break;
//        case Syntax::Term::BinaryDotOperator::BinaryDivide:
//            if (left->getType()->isIntegerTy())
//            {
//                if (sign->isSigned() || rsign->isSigned())
//                {
//                    left = builder.CreateSDiv(left, right, "divtmp");
//                }
//                else
//                {
//                    left = builder.CreateUDiv(left, right);
//                }
//            }
//            else
//            {
//                left = builder.CreateFDiv(left, right);
//            }
//            break;
//        case Syntax::Term::BinaryDotOperator::BinaryRemainder:
//            if (left->getType()->isIntegerTy())
//            {
//                if (sign->isSigned() || rsign->isSigned())
//                {
//                    left = builder.CreateSRem(left, right);
//                }
//                else
//                {
//                    left = builder.CreateURem(left, right);
//                }
//            }
//            else
//            {
//                throw std::runtime_error("Invalid operands to %");
//            }
//            break;
//        }
//        if (std::static_pointer_cast<Syntax::PrimitiveType>(sign)->getBitCount()
//            < std::static_pointer_cast<Syntax::PrimitiveType>(rsign)->getBitCount())
//        {
//            sign = rsign;
//        }
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::AdditiveExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getTerm().accept(*this);
//    if (node.getOptionalTerms().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto&[op, term] : node.getOptionalTerms())
//    {
//        term.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), *this);
//
//        switch (op)
//        {
//        case Syntax::AdditiveExpression::BinaryDashOperator::BinaryPlus:
//        {
//            if (left->getType()->isIntegerTy())
//            {
//                left = builder.CreateAdd(left, right);
//            }
//            else
//            {
//                left = builder.CreateFAdd(left, right);
//            }
//            break;
//        }
//        case Syntax::AdditiveExpression::BinaryDashOperator::BinaryMinus:
//        {
//            if (left->getType()->isIntegerTy())
//            {
//                left = builder.CreateSub(left, right);
//            }
//            else
//            {
//                left = builder.CreateFSub(left, right);
//            }
//            break;
//        }
//        }
//        if (std::static_pointer_cast<Syntax::PrimitiveType>(sign)->getBitCount()
//            < std::static_pointer_cast<Syntax::PrimitiveType>(rsign)->getBitCount())
//        {
//            sign = rsign;
//        }
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ShiftExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getAdditiveExpression().accept(*this);
//    if (node.getOptionalAdditiveExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto&[op, rel] : node.getOptionalAdditiveExpressions())
//    {
//        rel.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), *this);
//        if (!left->getType()->isIntegerTy())
//        {
//            throw std::runtime_error("Only integer types allowed in shift expressions");
//        }
//        switch (op)
//        {
//        case Syntax::ShiftExpression::ShiftOperator::Right:left = builder.CreateAShr(left, right);
//            break;
//        case Syntax::ShiftExpression::ShiftOperator::Left:left = builder.CreateShl(left, right);
//        }
//        if (std::static_pointer_cast<Syntax::PrimitiveType>(sign)->getBitCount()
//            < std::static_pointer_cast<Syntax::PrimitiveType>(rsign)->getBitCount())
//        {
//            sign = rsign;
//        }
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::RelationalExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getShiftExpression().accept(*this);
//    if (node.getOptionalShiftExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto&[op, rel] : node.getOptionalShiftExpressions())
//    {
//        rel.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), *this);
//
//        switch (op)
//        {
//        case Syntax::RelationalExpression::RelationalOperator::LessThan:
//            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
//            {
//                if (sign->isSigned() || rsign->isSigned())
//                {
//                    left = builder.CreateICmpSLT(left, right);
//                }
//                else
//                {
//                    left = builder.CreateICmpULT(left, right);
//                }
//            }
//            else if (left->getType()->isFloatingPointTy())
//            {
//                left = builder.CreateFCmpULT(left, right);
//            }
//            break;
//        case Syntax::RelationalExpression::RelationalOperator::LessThanOrEqual:
//            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
//            {
//                if (sign->isSigned() || rsign->isSigned())
//                {
//                    left = builder.CreateICmpSLE(left, right);
//                }
//                else
//                {
//                    left = builder.CreateICmpULE(left, right);
//                }
//            }
//            else if (left->getType()->isFloatingPointTy())
//            {
//                left = builder.CreateFCmpULE(left, right);
//            }
//            break;
//        case Syntax::RelationalExpression::RelationalOperator::GreaterThan:
//            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
//            {
//                if (sign->isSigned() || rsign->isSigned())
//                {
//                    left = builder.CreateICmpSGT(left, right);
//                }
//                else
//                {
//                    left = builder.CreateICmpUGT(left, right);
//                }
//            }
//            else if (left->getType()->isFloatingPointTy())
//            {
//                left = builder.CreateFCmpUGT(left, right);
//            }
//            break;
//        case Syntax::RelationalExpression::RelationalOperator::GreaterThanOrEqual:
//            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
//            {
//                if (sign->isSigned() || rsign->isSigned())
//                {
//                    left = builder.CreateICmpSGE(left, right);
//                }
//                else
//                {
//                    left = builder.CreateICmpUGE(left, right);
//                }
//            }
//            else if (left->getType()->isFloatingPointTy())
//            {
//                left = builder.CreateFCmpUGE(left, right);
//            }
//            break;
//        }
//        sign = std::make_shared<Syntax::PrimitiveType>(32, false, false, true);
//        left = builder.CreateZExt(left, builder.getInt32Ty());
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::EqualityExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getRelationalExpression().accept(*this);
//    if (node.getOptionalRelationalExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto&[op, factor] : node.getOptionalRelationalExpressions())
//    {
//        factor.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, rsign->isSigned(), *this);
//
//        switch (op)
//        {
//        case Syntax::EqualityExpression::EqualityOperator::Equal:
//            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
//            {
//                left = builder.CreateICmpEQ(left, right);
//            }
//            else if (left->getType()->isFloatingPointTy())
//            {
//                left = builder.CreateFCmpUEQ(left, right);
//            }
//            break;
//        case Syntax::EqualityExpression::EqualityOperator::NotEqual:
//            if (left->getType()->isIntegerTy() || left->getType()->isPointerTy())
//            {
//                left = builder.CreateICmpNE(left, right);
//            }
//            else
//            {
//                left = builder.CreateFCmpUNE(left, right);
//            }
//            break;
//        }
//        sign = std::make_shared<Syntax::PrimitiveType>(32, false, false, true);
//        left = builder.CreateZExt(left, builder.getInt32Ty());
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BitAndExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getEqualityExpression().accept(*this);
//    if (node.getOptionalEqualityExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto& factor : node.getOptionalEqualityExpressions())
//    {
//        factor.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, sign->isSigned(), *this);
//        left = builder.CreateAnd(left, right);
//        if (std::static_pointer_cast<Syntax::PrimitiveType>(sign)->getBitCount()
//            < std::static_pointer_cast<Syntax::PrimitiveType>(rsign)->getBitCount())
//        {
//            sign = rsign;
//        }
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BitXorExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getBitAndExpression().accept(*this);
//    if (node.getOptionalBitAndExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto& factor : node.getOptionalBitAndExpressions())
//    {
//        factor.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, sign->isSigned(), *this);
//        left = builder.CreateXor(left, right);
//        if (std::static_pointer_cast<Syntax::PrimitiveType>(sign)->getBitCount()
//            < std::static_pointer_cast<Syntax::PrimitiveType>(rsign)->getBitCount())
//        {
//            sign = rsign;
//        }
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BitOrExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getBitXorExpression().accept(*this);
//    if (node.getOptionalBitXorExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto& factor : node.getOptionalBitXorExpressions())
//    {
//        factor.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        arithmeticCast(left, sign->isSigned(), right, sign->isSigned(), *this);
//        left = builder.CreateOr(left, right);
//        if (std::static_pointer_cast<Syntax::PrimitiveType>(sign)->getBitCount()
//            < std::static_pointer_cast<Syntax::PrimitiveType>(rsign)->getBitCount())
//        {
//            sign = rsign;
//        }
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::LogicalAndExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getBitOrExpression().accept(*this);
//    if (node.getOptionalBitOrExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto& factor : node.getOptionalBitOrExpressions())
//    {
//        auto* function = builder.GetInsertBlock()->getParent();
//        left = toBool(left, *this);
//        auto* thenBB = llvm::BasicBlock::Create(context, "then", function);
//        auto* elseBB = llvm::BasicBlock::Create(context, "else");
//        auto* mergeBB = llvm::BasicBlock::Create(context, "ifcond");
//
//        builder.CreateCondBr(left, thenBB, elseBB);
//
//        builder.SetInsertPoint(thenBB);
//        factor.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        right = toBool(right, *this);
//        right = builder.CreateZExt(right, builder.getInt32Ty());
//        builder.CreateBr(mergeBB);
//        thenBB = builder.GetInsertBlock();
//
//        function->getBasicBlockList().push_back(elseBB);
//        builder.SetInsertPoint(elseBB);
//
//        builder.CreateBr(mergeBB);
//        elseBB = builder.GetInsertBlock();
//
//        function->getBasicBlockList().push_back(mergeBB);
//        builder.SetInsertPoint(mergeBB);
//        auto* pn = builder.CreatePHI(builder.getInt32Ty(), 2, "iftmp");
//        pn->addIncoming(right, thenBB);
//        pn->addIncoming(builder.getInt32(0), elseBB);
//        left = pn;
//        sign = std::make_shared<Syntax::PrimitiveType>(32, false, false, true);
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::LogicalOrExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getAndExpression().accept(*this);
//    if (node.getOptionalAndExpressions().empty())
//    {
//        return;
//    }
//    auto[left, sign] = std::get<NodeRetType>(m_return);
//    for (auto& factor : node.getOptionalAndExpressions())
//    {
//        auto* function = builder.GetInsertBlock()->getParent();
//        left = toBool(left, *this);
//        auto* thenBB = llvm::BasicBlock::Create(context, "then", function);
//        auto* elseBB = llvm::BasicBlock::Create(context, "else");
//        auto* mergeBB = llvm::BasicBlock::Create(context, "ifcond");
//
//        builder.CreateCondBr(left, elseBB, thenBB);
//
//        builder.SetInsertPoint(thenBB);
//        factor.accept(*this);
//        auto[right, rsign] = std::get<NodeRetType>(m_return);
//        right = toBool(right, *this);
//        right = builder.CreateZExt(right, builder.getInt32Ty());
//        builder.CreateBr(mergeBB);
//        thenBB = builder.GetInsertBlock();
//
//        function->getBasicBlockList().push_back(elseBB);
//        builder.SetInsertPoint(elseBB);
//
//        builder.CreateBr(mergeBB);
//        elseBB = builder.GetInsertBlock();
//
//        function->getBasicBlockList().push_back(mergeBB);
//        builder.SetInsertPoint(mergeBB);
//        auto* pn = builder.CreatePHI(builder.getInt32Ty(), 2);
//        pn->addIncoming(right, thenBB);
//        pn->addIncoming(builder.getInt32(1), elseBB);
//        left = pn;
//        sign = std::make_shared<Syntax::PrimitiveType>(32, false, false, true);
//    }
//    m_return.emplace<NodeRetType>(left, sign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ConditionalExpression& node)
//{
//    emitLocation(&node, *this);
//    node.getLogicalOrExpression().accept(*this);
//    if (!node.getOptionalConditionalExpression() && !node.getOptionalConditionalExpression())
//    {
//        return;
//    }
//    auto[value, vsign] = std::get<NodeRetType>(m_return);
//    if (node.getOptionalExpression() && node.getOptionalConditionalExpression())
//    {
//        value = toBool(value, *this);
//        auto* function = builder.GetInsertBlock()->getParent();
//
//        auto* thenBB = llvm::BasicBlock::Create(context, "then", function);
//        auto* elseBB = llvm::BasicBlock::Create(context, "else");
//        auto* mergeBB = llvm::BasicBlock::Create(context, "ifcont");
//
//        builder.CreateCondBr(value, thenBB, elseBB);
//
//        builder.SetInsertPoint(thenBB);
//        node.getOptionalExpression()->accept(*this);
//        auto[thenV, tsign] = std::get<NodeRetType>(m_return);
//
//        builder.CreateBr(mergeBB);
//        thenBB = builder.GetInsertBlock();
//
//        function->getBasicBlockList().push_back(elseBB);
//        builder.SetInsertPoint(elseBB);
//
//        node.getOptionalConditionalExpression()->accept(*this);
//        auto[elseV, esign] = std::get<NodeRetType>(m_return);
//
//        builder.CreateBr(mergeBB);
//        elseBB = builder.GetInsertBlock();
//
//        function->getBasicBlockList().push_back(mergeBB);
//        builder.SetInsertPoint(mergeBB);
//        arithmeticCast(thenV, tsign->isSigned(), elseV, esign->isSigned(), *this);
//        auto* pn = builder.CreatePHI(thenV->getType(), 2);
//        pn->addIncoming(thenV, thenBB);
//        pn->addIncoming(elseV, elseBB);
//        m_return.emplace<NodeRetType>(pn, tsign);
//        return;
//    }
//    m_return.emplace<NodeRetType>(value, vsign);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ReturnStatement& node)
//{
//    emitLocation(&node, *this);
//    node.getExpression().accept(*this);
//    auto[value, sign] = std::get<NodeRetType>(m_return);
//    bool isStruct = dynamic_cast<const Syntax::StructType*>(functionRetType);
//    if (!isStruct)
//    {
//        auto* retType = currentFunction->getReturnType();
//        castPrimitive(value, sign->isSigned(), retType, functionRetType->isSigned(), *this);
//        builder.CreateRet(value);
//    }
//    else
//    {
//        auto* args = currentFunction->args().begin();
//        if (value->getType() != args->getType()->getPointerElementType())
//        {
//            throw std::runtime_error("Struct values are not the same");
//        }
//        builder.CreateStore(value, args);
//        builder.CreateRetVoid();
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ExpressionStatement& node)
//{
//    emitLocation(&node, *this);
//    if (node.getOptionalExpression())
//    {
//        return node.getOptionalExpression()->accept(*this);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::IfStatement& node)
//{
//    emitLocation(&node, *this);
//    node.getExpression().accept(*this);
//    auto[value, sign] = std::get<NodeRetType>(m_return);
//    value = toBool(value, *this);
//    auto* function = builder.GetInsertBlock()->getParent();
//
//    auto* thenBB = llvm::BasicBlock::Create(context, "then", function);
//    auto* elseBB = node.getElseBranch() ? llvm::BasicBlock::Create(context, "else") : nullptr;
//    auto* mergeBB = llvm::BasicBlock::Create(context, "ifcont");
//
//    builder.CreateCondBr(value, thenBB, elseBB ? elseBB : mergeBB);
//
//    builder.SetInsertPoint(thenBB);
//    node.getBranch().accept(*this);
//
//    if (!builder.GetInsertBlock()->getTerminator())
//    {
//        builder.CreateBr(mergeBB);
//    }
//
//    if (elseBB)
//    {
//        function->getBasicBlockList().push_back(elseBB);
//        builder.SetInsertPoint(elseBB);
//        node.getElseBranch()->accept(*this);
//        if (!builder.GetInsertBlock()->getTerminator())
//        {
//            builder.CreateBr(mergeBB);
//        }
//    }
//
//    function->getBasicBlockList().push_back(mergeBB);
//    builder.SetInsertPoint(mergeBB);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::SwitchStatement& node)
//{
//    emitLocation(&node, *this);
//    node.getExpression().accept(*this);
//    auto[value, sign] = std::get<NodeRetType>(m_return);
//    auto* defaultBlock = llvm::BasicBlock::Create(context, "default");
//    auto* thenBlock = llvm::BasicBlock::Create(context, "then");
//    breakBlocks.push_back(thenBlock);
//    switchStack.emplace_back(builder.CreateSwitch(value, defaultBlock), sign->isSigned());
//    node.getStatement().accept(*this);
//    auto* function = builder.GetInsertBlock()->getParent();
//    if (!std::any_of(function->getBasicBlockList().begin(),
//                     function->getBasicBlockList().end(),
//                     [defaultBlock](const llvm::BasicBlock& block)
//                     {
//                         return defaultBlock == &block;
//                     }))
//    {
//        function->getBasicBlockList().push_back(defaultBlock);
//        builder.SetInsertPoint(defaultBlock);
//        builder.CreateBr(defaultBlock);
//    }
//    if (!defaultBlock->getTerminator())
//    {
//        builder.SetInsertPoint(defaultBlock);
//        builder.CreateBr(thenBlock);
//    }
//    function->getBasicBlockList().push_back(thenBlock);
//    builder.SetInsertPoint(thenBlock);
//    breakBlocks.pop_back();
//    switchStack.pop_back();
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DefaultStatement& node)
//{
//    emitLocation(&node, *this);
//    if (switchStack.empty())
//    {
//        throw std::runtime_error("default without switch statement");
//    }
//    auto* function = builder.GetInsertBlock()->getParent();
//    auto* block = switchStack.back().first->getDefaultDest();
//    if (switchStack.back().first->getNumCases() > 0)
//    {
//        auto* successor = (switchStack.back().first->case_begin() + (switchStack.back().first->getNumCases() - 1))
//            ->getCaseSuccessor();
//        if (!successor->getTerminator())
//        {
//            builder.CreateBr(block);
//        }
//    }
//    if (std::any_of(function->getBasicBlockList().begin(),
//                    function->getBasicBlockList().end(),
//                    [block](const llvm::BasicBlock& dblock)
//                    {
//                        return block == &dblock;
//                    }))
//    {
//        throw std::runtime_error("There can only be a single default statement");
//    }
//    function->getBasicBlockList().push_back(block);
//    builder.SetInsertPoint(block);
//    node.getStatement().accept(*this);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CaseStatement& node)
//{
//    emitLocation(&node, *this);
//    if (switchStack.empty())
//    {
//        throw std::runtime_error("case without switch statement");
//    }
//    auto[value, sign] = std::visit([this](auto&& value) -> std::pair<llvm::Value*, std::shared_ptr<Syntax::IType>>
//                                   {
//                                       using T = std::decay_t<decltype(value)>;
//                                       if constexpr(std::is_same_v<T, std::int32_t>)
//                                       {
//                                           return {builder.getInt32(value),
//                                                   std::make_shared<Syntax::PrimitiveType>(32, false, false, true)};
//                                       }
//                                       else if constexpr(std::is_same_v<T, std::uint32_t>)
//                                       {
//                                           return {builder.getInt32(value),
//                                                   std::make_shared<Syntax::PrimitiveType>(32, false, false, true)};
//                                       }
//                                       else if constexpr(std::is_same_v<T, std::int64_t>)
//                                       {
//                                           return {builder.getInt64(value),
//                                                   std::make_shared<Syntax::PrimitiveType>(64, false, false, true)};
//                                       }
//                                       else if constexpr(std::is_same_v<T, std::uint64_t>)
//                                       {
//                                           return {builder.getInt64(value),
//                                                   std::make_shared<Syntax::PrimitiveType>(64, false, false, false)};
//                                       }
//                                       else
//                                       {
//                                           throw std::runtime_error("Type not allowed in case");
//                                       }
//                                   }, node.getConstant());
//    if (value->getType() != switchStack.back().first->getCondition()->getType())
//    {
//        castPrimitive(value,
//                      sign->isSigned(),
//                      switchStack.back().first->getCondition()->getType(),
//                      switchStack.back().second,
//                      *this);
//    }
//    auto* function = builder.GetInsertBlock()->getParent();
//    auto* newBlock = llvm::BasicBlock::Create(context);
//    if (switchStack.back().first->getNumCases() > 0)
//    {
//        auto* successor = (switchStack.back().first->case_begin()
//            + (switchStack.back().first->getNumCases() - 1))
//            ->getCaseSuccessor();
//        if (!successor->getTerminator())
//        {
//            builder.CreateBr(newBlock);
//        }
//    }
//    function->getBasicBlockList().push_back(newBlock);
//    auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value);
//    if (!constant)
//    {
//        throw std::runtime_error("Expected constant expression after case");
//    }
//    switchStack.back().first->addCase(constant, newBlock);
//    builder.SetInsertPoint(newBlock);
//    if (node.getStatement())
//    {
//        node.getStatement()->accept(*this);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CompoundStatement& node)
//{
//    pushScope();
//    debugScope.push_back(debugBuilder->createLexicalBlock(debugScope.back(),
//                                                          debugUnit,
//                                                          node.getLine(),
//                                                          node.getColumn()));
//    for (auto& iter : node.getBlockItems())
//    {
//        iter.accept(*this);
//        if (auto* statement = std::get_if<Syntax::Statement>(&iter.getVariant());statement)
//        {
//            if (std::holds_alternative<Syntax::ReturnStatement>(statement->getVariant())
//                || std::holds_alternative<Syntax::BreakStatement>(statement->getVariant())
//                || std::holds_alternative<Syntax::ContinueStatement>(statement->getVariant()))
//            {
//                break;
//            }
//        }
//    }
//    debugScope.pop_back();
//    popScope();
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ForStatement& node)
//{
//    emitLocation(&node, *this);
//    if (node.getInitial())
//    {
//        node.getInitial()->accept(*this);
//    }
//    doForLoop(node.getControlling(), node.getPost(), node.getStatement());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::InitializerList& node)
//{
//
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Declaration& node)
//{
//    emitLocation(&node, *this);
//    llvm::IRBuilder<> tmpB(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
//    for (auto&[type, name, optionalExpression] : node.getDeclarations())
//    {
//        //        if (auto array = std::dynamic_pointer_cast<Syntax::ArrayType>(type);array && array->getSize() == 0)
//        //        {
//        //            if (!optionalExpression)
//        //            {
//        //                throw std::runtime_error("Initializer list needed to deduce size of array");
//        //            }
//        //            auto* result = std::get_if<Syntax::InitializerList>(&optionalExpression->getVariant());
//        //            if (!result)
//        //            {
//        //                if (auto* primitive = dynamic_cast<const Syntax::PrimitiveType*>(array->getType().get());primitive
//        //                    && primitive->getBitCount() == 8)
//        //                {
//        //                    std::get<Syntax::AssignmentExpression>(optionalExpression->getVariant()).accept(*this);
//        //                    auto[value, type] = std::get<NodeRetType>(m_return);
//        //                    auto* load = llvm::dyn_cast<llvm::LoadInst>(value);
//        //                    if (value->getType()->isArrayTy() && load
//        //                        && llvm::isa<llvm::GlobalValue>(load->getPointerOperand()))
//        //                    {
//        //                        array->setSize(value->getType()->getArrayNumElements());
//        //                    }
//        //                    else
//        //                    {
//        //                        throw std::runtime_error("Can only initialize char array with a string literal");
//        //                    }
//        //                }
//        //                else
//        //                {
//        //                    throw std::runtime_error("Initializer list needed to deduce size of array");
//        //                }
//        //            }
//        //            else
//        //            {
//        //                array->getType()->accept(*this);
//        //                auto elementSize = elementsNeededForType(std::get<llvm::Type*>(m_return));
//        //                std::size_t i = 0, max = std::numeric_limits<std::size_t>::lowest();
//        //                for (auto iter = result->getNonCommaExpressionsAndBlocks().begin();
//        //                     iter < result->getNonCommaExpressionsAndBlocks().end(); i++)
//        //                {
//        //                    if (iter->first != -1)
//        //                    {
//        //                        max = std::max(i, max);
//        //                        i = iter->first;
//        //                    }
//        //                    auto end = std::find_if(iter,
//        //                                            iter + elementSize > result->getNonCommaExpressionsAndBlocks().end()
//        //                                            ? result
//        //                                                ->getNonCommaExpressionsAndBlocks().end() : iter + elementSize,
//        //                                            [](auto&& value)
//        //                                            {
//        //                                                return std::holds_alternative<OpenCL::Syntax::InitializerList>(
//        //                                                    value
//        //                                                        .second);
//        //                                            });
//        //                    if (iter == end)
//        //                    {
//        //                        iter++;
//        //                    }
//        //                    else
//        //                    {
//        //                        iter = end;
//        //                    }
//        //                }
//        //                max = std::max(i, max);
//        //                array->setSize(max);
//        //                if (!array->getSize())
//        //                {
//        //                    throw std::runtime_error("Array can't be of size 0");
//        //                }
//        //            }
//        //        }
//        //        else
//        //        {
//        //            if (optionalExpression)
//        //            {
//        //                if (auto* result = std::get_if<Syntax::InitializerList>(&optionalExpression->getVariant()))
//        //                {
//        //                    if (std::any_of(result->getNonCommaExpressionsAndBlocks().begin(),
//        //                                    result->getNonCommaExpressionsAndBlocks().end(),
//        //                                    [](const auto& pair)
//        //                                    {
//        //                                        return pair.first != -1;
//        //                                    }))
//        //                    {
//        //                        throw std::runtime_error("Designators are only allowed in initializers for arrays");
//        //                    }
//        //                }
//        //            }
//        //        }
//        type->accept(*this);
//        auto* allocaType = std::get<llvm::Type*>(m_return);
//        auto* alloca = tmpB.CreateAlloca(allocaType, nullptr);
//        auto* sp = debugScope.empty() ? debugUnit : debugScope
//            .back();
//        auto* di = debugBuilder
//            ->createAutoVariable(sp,
//                                 name,
//                                 debugUnit,
//                                 node.getLine(),
//                                 toDwarfType(type, *this));
//        debugBuilder->insertDeclare(alloca,
//                                    di,
//                                    debugBuilder->createExpression(),
//                                    llvm::DebugLoc::get(node.getLine(), node.getColumn(), sp),
//                                    builder.GetInsertBlock());
//        alloca->setAlignment(getAlignment(allocaType));
//        addValueToScope(name, {alloca, type});
//        if (optionalExpression)
//        {
//            if (std::holds_alternative<Syntax::AssignmentExpression>(optionalExpression->getVariant()))
//            {
//                if (allocaType->isArrayTy() && allocaType->getArrayElementType()->isIntegerTy()
//                    && allocaType->getArrayElementType()->getIntegerBitWidth() == 8)
//                {
//                    std::get<Syntax::AssignmentExpression>(optionalExpression->getVariant()).accept(*this);
//                    auto[value, type] = std::get<NodeRetType>(m_return);
//                    auto* load = llvm::dyn_cast<llvm::LoadInst>(value);
//                    if (value->getType()->isArrayTy() && load
//                        && llvm::isa<llvm::GlobalValue>(load->getPointerOperand()))
//                    {
//                        auto* zero = builder.getInt32(0);
//                        builder.CreateMemCpy(alloca,
//                                             0,
//                                             builder
//                                                 .CreateInBoundsGEP(load->getPointerOperand(), {zero, zero}),
//                                             0,
//                                             load->getType()->getArrayNumElements());
//                    }
//                    else
//                    {
//                        throw std::runtime_error("Can only initialize char array with a string literal");
//                    }
//                }
//                else if (!allocaType->isArrayTy())
//                {
//                    optionalExpression->accept(*this);
//                    auto[value, otherType] = std::get<NodeRetType>(m_return);
//                    castPrimitive(value, otherType->isSigned(), allocaType, type->isSigned(), *this);
//                    builder.CreateStore(value, alloca);
//                }
//                else
//                {
//                    throw std::runtime_error("Can't initialize array with scalar initializer");
//                }
//            }
//            else if (auto result = std::get_if<Syntax::InitializerList>(&optionalExpression->getVariant()))
//            {
//                //                if (allocaType->isArrayTy() && result->getNonCommaExpressionsAndBlocks().size() == 1)
//                //                {
//                //                    auto* zero = builder.getInt32(0);
//                //                    for (std::size_t i = 0; i < allocaType->getArrayNumElements(); i++)
//                //                    {
//                //                        auto* index = builder.getInt32(i);
//                //                        auto* member = builder.CreateInBoundsGEP(alloca, {zero, index});
//                //                        std::visit([this, member,
//                //                                       type = std::dynamic_pointer_cast<Syntax::ArrayType>(type)](auto&& value)
//                //                                   {
//                //                                       using T = std::decay_t<decltype(value)>;
//                //                                       if constexpr(std::is_same_v<T, OpenCL::Syntax::InitializerList>)
//                //                                       {
//                //                                           match(value.getNonCommaExpressionsAndBlocks(),
//                //                                                 member,
//                //                                                 type->getType()->clone(),
//                //                                                 *this);
//                //                                       }
//                //                                       else
//                //                                       {
//                //                                           value.accept(*this);
//                //                                           auto[newValue, otherType] = std::get<NodeRetType>(m_return);
//                //                                           type->getType()->accept(*this);
//                //                                           castPrimitive(newValue,
//                //                                                         otherType->isSigned(),
//                //                                                         std::get<llvm::Type*>(m_return),
//                //                                                         type->getType()->isSigned(),
//                //                                                         *this);
//                //                                           builder.CreateStore(newValue, member);
//                //                                       }
//                //                                   }, result->getNonCommaExpressionsAndBlocks()[0].second);
//                //                    }
//                //                }
//                //                match(result->getNonCommaExpressionsAndBlocks(), alloca, type, *this);
//            }
//        }
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ForDeclarationStatement& node)
//{
//    emitLocation(&node, *this);
//    pushScope();
//    debugScope.push_back(debugBuilder->createLexicalBlock(debugScope.back(),
//                                                          debugUnit,
//                                                          node.getLine(),
//                                                          node.getColumn()));
//    node.getInitial().accept(*this);
//    doForLoop(node.getControlling(), node.getPost(), node.getStatement());
//    debugScope.pop_back();
//    popScope();
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::HeadWhileStatement& node)
//{
//    emitLocation(&node, *this);
//    auto* function = builder.GetInsertBlock()->getParent();
//
//    auto* condBB = llvm::BasicBlock::Create(context, "cond", function);
//    builder.CreateBr(condBB);
//    auto* blockBB = llvm::BasicBlock::Create(context, "block");
//    auto* endBB = llvm::BasicBlock::Create(context, "end");
//
//    breakBlocks.push_back(endBB);
//    continueBlocks.push_back(condBB);
//
//    builder.SetInsertPoint(condBB);
//    node.getExpression().accept(*this);
//    auto* value = std::get<NodeRetType>(m_return).first;
//    value = toBool(value, *this);
//    builder.CreateCondBr(value, blockBB, endBB);
//
//    function->getBasicBlockList().push_back(blockBB);
//    builder.SetInsertPoint(blockBB);
//    node.getStatement().accept(*this);
//    builder.CreateBr(condBB);
//
//    function->getBasicBlockList().push_back(endBB);
//    builder.SetInsertPoint(endBB);
//    breakBlocks.pop_back();
//    continueBlocks.pop_back();
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::FootWhileStatement& node)
//{
//    emitLocation(&node, *this);
//    auto* function = builder.GetInsertBlock()->getParent();
//
//    auto* blockBB = llvm::BasicBlock::Create(context, "block", function);
//    builder.CreateBr(blockBB);
//    auto* condBB = llvm::BasicBlock::Create(context, "cond");
//    auto* endBB = llvm::BasicBlock::Create(context, "end");
//
//    breakBlocks.push_back(endBB);
//    continueBlocks.push_back(condBB);
//
//    builder.SetInsertPoint(blockBB);
//    node.getStatement().accept(*this);
//    builder.CreateBr(condBB);
//
//    function->getBasicBlockList().push_back(condBB);
//    builder.SetInsertPoint(condBB);
//    node.getExpression().accept(*this);
//    auto* value = std::get<NodeRetType>(m_return).first;
//    value = toBool(value, *this);
//    builder.CreateCondBr(value, blockBB, endBB);
//
//    function->getBasicBlockList().push_back(endBB);
//    builder.SetInsertPoint(endBB);
//    breakBlocks.pop_back();
//    continueBlocks.pop_back();
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::BreakStatement& node)
//{
//    emitLocation(&node, *this);
//    builder.CreateBr(breakBlocks.back());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ContinueStatement& node)
//{
//    emitLocation(&node, *this);
//    builder.CreateBr(continueBlocks.back());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::StructOrUnionDeclaration& node)
//{
//    emitLocation(&node, *this);
//    StructOrUnion structType;
//    std::vector<llvm::Type*> types;
//    auto* sp = debugScope.empty() ? debugUnit : debugScope.back();
//    auto* llvmStruct = llvm::StructType::create(context, (node.isUnion() ? "union." : "struct.") + node.getName());
//    std::transform(node.getTypes().begin(), node.getTypes().end(), std::back_inserter(types), [&](const auto& pair)
//    {
//        structType.order.insert({pair.second, structType.types.size()});
//        structType.types.push_back(pair.first);
//        pair.first->accept(*this);
//        return std::get<llvm::Type*>(m_return);
//    });
//    if (!node.isUnion())
//    {
//        llvmStruct->setBody(types);
//    }
//    else
//    {
//        llvm::Type
//            * maxElement = *std::max_element(types.begin(), types.end(), [this](llvm::Type* lhs, llvm::Type* rhs)
//        {
//            auto lhsSize = module->getDataLayout().getTypeAllocSize(lhs);
//            auto rhsSize = module->getDataLayout().getTypeAllocSize(rhs);
//            return lhsSize < rhsSize;
//        });
//        llvmStruct->setBody(maxElement);
//    }
//    llvm::DICompositeType* fw_decl, * res;
//    cache[node.getName()] = fw_decl = debugBuilder
//        ->createReplaceableCompositeType(node.isUnion() ? llvm::dwarf::DW_TAG_union_type
//                                                        : llvm::dwarf::DW_TAG_structure_type,
//                                         node.getName(),
//                                         sp,
//                                         debugUnit,
//                                         node.getLine());
//    std::vector<llvm::Metadata*> subTypes;
//    std::size_t i = 0;
//    for (auto& iter : structType.types)
//    {
//        auto* member = toDwarfType(iter, *this);
//        iter->accept(*this);
//        auto* memberType = std::get<llvm::Type*>(m_return);
//        subTypes.push_back(debugBuilder->createMemberType(sp,
//                                                          std::find_if(structType.order.begin(),
//                                                                       structType.order.end(),
//                                                                       [i](const std::pair<const std::string,
//                                                                                           std::uint64_t>& pair)
//                                                                       {
//                                                                           return pair.second == i;
//                                                                       })->first,
//                                                          debugUnit,
//                                                          node.getLine(),
//                                                          module->getDataLayout()
//                                                                .getTypeSizeInBits(memberType),
//                                                          getAlignment(memberType),
//                                                          module->getDataLayout()
//                                                                .getStructLayout(llvmStruct)
//                                                                ->getElementOffsetInBits(i),
//                                                          llvm::DINode::DIFlags::FlagAccessibility,
//                                                          member));
//        i++;
//    }
//    if (!node.isUnion())
//    {
//        res = debugBuilder->createStructType(sp,
//                                             node.getName(),
//                                             debugUnit,
//                                             node.getLine(),
//                                             module->getDataLayout()
//                                                   .getTypeAllocSizeInBits(llvmStruct),
//                                             getAlignment(llvmStruct) * 8,
//                                             llvm::DINode::DIFlags::FlagAccessibility,
//                                             nullptr,
//                                             llvm::MDTuple::get(context, subTypes));
//    }
//    else
//    {
//        res = debugBuilder->createUnionType(sp,
//                                            node.getName(),
//                                            debugUnit,
//                                            node.getLine(),
//                                            module->getDataLayout()
//                                                  .getTypeAllocSizeInBits(llvmStruct),
//                                            getAlignment(llvmStruct) * 8,
//                                            llvm::DINode::DIFlags::FlagAccessibility,
//                                            llvm::MDTuple::get(context, subTypes));
//    }
//    llvm::TempMDNode fwd_decl(fw_decl);
//    debugBuilder->replaceTemporary(std::move(fwd_decl), res);
//    cache[node.getName()] = res;
//    structType.isUnion = node.isUnion();
//    structs[(node.isUnion() ? "union." : "struct.") + node.getName()] = structType;
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimaryExpression& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PostFixExpression& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnaryExpression& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::AssignmentExpression& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Initializer& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::CompoundItem& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Statement& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ExternalDeclaration& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TypeName& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Declarator& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::EnumDeclaration& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TypeSpecifier& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclarator& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorStatic& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorAsterisk& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorParentheseParameters& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectAbstractDeclarator& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::Pointer& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ParameterTypeList& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ParameterList& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::LabelStatement& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Codegen::Context::visit(const OpenCL::Syntax::GotoStatement& node)
//{
//    return INodeVisitor::visit(node);
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::EnumSpecifier& node)
//{
//    if (!node.getName().empty())
//    {
//        auto* sp = debugScope.empty() ? debugUnit : debugScope.back();
//        std::vector<llvm::Metadata*> elements;
//        for (auto&[name, value] : node.getValues())
//        {
//            elements.push_back(debugBuilder->createEnumerator(name, value));
//        }
//        auto* result = debugBuilder->createEnumerationType(sp,
//                                                           node.getName(),
//                                                           debugUnit,
//                                                           node.getLine(),
//                                                           32,
//                                                           32,
//                                                           llvm::MDTuple::get(context, elements),
//                                                           toDwarfType(std::make_shared<Syntax::PrimitiveType>(32,
//                                                                                                               false,
//                                                                                                               false,
//                                                                                                               true),
//                                                                       *this));
//        cache[node.getName()] = result;
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TypedefDeclaration& node)
//{
//    if (node.getOptionalStructOrUnion())
//    {
//        return node.getOptionalStructOrUnion()->accept(*this);
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::FunctionDefinition& node)
//{
//    if (!hasFunction(node.getName()))
//    {
//        {
//            std::vector<const Syntax::IType*> types;
//            std::transform(node.getArguments().begin(),
//                           node.getArguments().end(),
//                           std::back_inserter(types),
//                           [](const auto& pair)
//                           {
//                               return pair.first.get();
//                           });
//            addFunction(node.getName(), {node.getReturnType(), std::move(types)});
//            functionRetType = node.getReturnType().get();
//        }
//        {
//            std::set<std::size_t> needsByVal;
//            std::vector<llvm::Type*> types;
//            std::size_t i = 0;
//            bool isStruct = dynamic_cast<const Syntax::StructType*>(node.getReturnType().get());
//            for (auto&[type, name] : node.getArguments())
//            {
//                type->accept(*this);
//                if (!dynamic_cast<const Syntax::StructType*>(type.get()))
//                {
//                    types.emplace_back(std::get<llvm::Type*>(m_return));
//                }
//                else
//                {
//                    types.emplace_back(llvm::PointerType::getUnqual(std::get<llvm::Type*>(m_return)));
//                    needsByVal.insert(i + (isStruct ? 2 : 1));
//                }
//                i++;
//            }
//            node.getReturnType()->accept(*this);
//            if (isStruct)
//            {
//                types.insert(types.begin(), llvm::PointerType::getUnqual(std::get<llvm::Type*>(m_return)));
//            }
//            auto* ft = llvm::FunctionType::get(isStruct ? builder.getVoidTy() : std::get<llvm::Type*>(m_return),
//                                               types,
//                                               false);
//            auto* fun = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, node.getName(), module.get());
//            if (isStruct)
//            {
//                fun->addAttribute(1, llvm::Attribute::get(context, llvm::Attribute::StructRet));
//                fun->addAttribute(1, llvm::Attribute::get(context, llvm::Attribute::NoAlias));
//            }
//            for (auto& iter : needsByVal)
//            {
//                fun->addAttribute(iter, llvm::Attribute::get(context, llvm::Attribute::ByVal));
//            }
//        }
//    }
//    if (!node.getBlockStatement())
//    {
//        return;
//    }
//    currentFunction = module->getFunction(node.getName());
//    std::size_t i = 0;
//    for (auto& iter : currentFunction->args())
//    {
//        if (iter.hasAttribute(llvm::Attribute::StructRet))
//        {
//            continue;
//        }
//        if (!node.getArguments()[i].second.empty())
//        {
//            iter.setName(node.getArguments()[i++].second);
//        }
//    }
//
//    std::vector<llvm::Metadata*> types;
//    types.push_back(toDwarfType(node.getReturnType(), *this));
//    for (auto& iter : node.getArguments())
//    {
//        types.push_back(toDwarfType(iter.first, *this));
//    }
//    auto* spType = debugBuilder->createSubroutineType({llvm::MDTuple::get(context, types)});
//    auto* sp = debugBuilder
//        ->createFunction(debugUnit,
//                         node.getName(),
//                         {},
//                         debugUnit,
//                         node.getLine(),
//                         spType,
//                         false,
//                         true,
//                         node.getScopeLine());
//    currentFunction->setSubprogram(sp);
//    debugScope.push_back(sp);
//    auto* bb = llvm::BasicBlock::Create(context, "entry", currentFunction);
//    builder.SetInsertPoint(bb);
//    clearScope();
//    i = 0;
//    emitLocation<void>(nullptr, *this);
//    for (auto& iter : currentFunction->args())
//    {
//        if (iter.hasAttribute(llvm::Attribute::StructRet))
//        {
//            continue;
//        }
//        if (node.getArguments()[i].second.empty())
//        {
//            continue;
//        }
//        llvm::AllocaInst* alloca;
//        auto createDebug = [&]
//        {
//            auto* local = debugBuilder->createParameterVariable(sp,
//                                                                iter.getName(),
//                                                                i,
//                                                                debugUnit,
//                                                                node.getLine(),
//                                                                toDwarfType(node.getArguments()[i].first, *this));
//            debugBuilder->insertDeclare(alloca,
//                                        local,
//                                        debugBuilder->createExpression(),
//                                        llvm::DebugLoc::get(node.getLine(), 0, sp),
//                                        builder.GetInsertBlock());
//        };
//        llvm::IRBuilder<>
//            tmpB(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
//        if (!iter.hasByValAttr())
//        {
//            alloca = tmpB.CreateAlloca(iter.getType());
//            createDebug();
//            alloca->setAlignment(getAlignment(iter.getType()));
//            builder.CreateStore(&iter, alloca);
//            addValueToScope(iter.getName(), {alloca, node.getArguments()[i++].first});
//        }
//        else
//        {
//            auto* ptrType = llvm::cast<llvm::PointerType>(iter.getType());
//            alloca = tmpB.CreateAlloca(ptrType->getPointerElementType());
//            createDebug();
//            alloca->setAlignment(getAlignment(ptrType->getPointerElementType()));
//            auto* zero = builder.getInt32(0);
//            auto* value = builder.CreateInBoundsGEP(alloca, {zero, zero});
//            builder.CreateStore(&iter, value);
//            addValueToScope(iter.getName(), {alloca, node.getArguments()[i++].first});
//        }
//    }
//
//    emitLocation(node.getBlockStatement(), *this);
//    node.getBlockStatement()->accept(*this);
//    auto& block = currentFunction->back();
//    if (block.empty() || !block.back().isTerminator())
//    {
//        if (!node.getReturnType()->isVoid())
//        {
//            node.getReturnType()->accept(*this);
//            auto* type = std::get<llvm::Type*>(m_return);
//            llvm::Value* value = nullptr;
//            if (type->isIntegerTy())
//            {
//                value = llvm::ConstantInt::get(type, 0);
//            }
//            else if (type->isFloatingPointTy())
//            {
//                value = llvm::ConstantFP::get(type, 0);
//            }
//            builder.CreateRet(value);
//        }
//        else
//        {
//            builder.CreateRetVoid();
//        }
//    }
//
//    debugScope.pop_back();
//
//    debugBuilder->finalize();
//    if (llvm::verifyFunction(*currentFunction, &llvm::errs()))
//    {
//        currentFunction->print(llvm::outs());
//        std::terminate();
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::GlobalDeclaration& node)
//{
//    //TODO: Make array size deduction and etc work
//    for (auto&[type, name, optionalValue] : node.getDeclarations())
//    {
//        if (optionalValue)
//        {
//            optionalValue->accept(*this);
//        }
//        auto[constant, sign] = optionalValue ? std::get<NodeRetType>(m_return) : NodeRetType{};
//        type->accept(*this);
//        if (constant
//            && (constant->getType() != std::get<llvm::Type*>(m_return)
//                || (sign && sign->isSigned()) != type->isSigned()))
//        {
//            castPrimitive(constant, sign->isSigned(), std::get<llvm::Type*>(m_return), type->isSigned(), *this);
//        }
//        if (constant && !llvm::isa<llvm::Constant>(constant))
//        {
//            throw std::runtime_error("Can only use constant expression to initialize global variable");
//        }
//
//        auto* newGlobal = new llvm::GlobalVariable(*module,
//                                                   std::get<llvm::Type*>(m_return),
//                                                   type->isConst(),
//                                                   llvm::GlobalVariable::LinkageTypes::ExternalLinkage,
//                                                   constant ? llvm::cast<llvm::Constant>(constant) :
//                                                   getZeroFor(std::get<llvm::Type*>(m_return)),
//                                                   name);
//        debugBuilder->createGlobalVariableExpression(debugUnit,
//                                                     name,
//                                                     name,
//                                                     debugUnit,
//                                                     node.getLine(),
//                                                     toDwarfType(type, *this),
//                                                     false);
//        addGlobal(name, {newGlobal, type});
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::TranslationUnit& node)
//{
//    module = std::make_unique<llvm::Module>("main", context);
//    std::string error;
//    const std::string tripleStr = llvm::sys::getProcessTriple();
//    llvm::Triple t(tripleStr);
//    if (t.isOSBinFormatCOFF())
//    {
//        t.setObjectFormat(llvm::Triple::ELF);
//    }
//    auto target = llvm::TargetRegistry::lookupTarget(t.str(), error);
//    if (!target)
//    {
//        throw std::runtime_error(error);
//    }
//    auto targetMachine = target->createTargetMachine(t.str(), "generic", "", {}, {});
//    module->setDataLayout(targetMachine->createDataLayout());
//    module->setTargetTriple(t.str());
//    debugBuilder = new llvm::DIBuilder(*module);
//    debugBuilder->finalize();
//    debugUnit = debugBuilder->createFile("input.c", "../src");
//    debugBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C99, debugUnit,
//                                    "OpenCL Compiler", false, "", 0);
//    for (auto& iter : node.getGlobals())
//    {
//        iter.accept(*this);
//    }
//    debugBuilder->finalize();
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PrimitiveType& node)
//{
//    if (node.isFloatingPoint())
//    {
//        if (node.getBitCount() == 32)
//        {
//            m_return = builder.getFloatTy();
//            return;
//        }
//        else if (node.getBitCount() == 64)
//        {
//            m_return = builder.getDoubleTy();
//        }
//        else
//        {
//            throw std::runtime_error("Invalid bit size for floating point");
//        }
//    }
//    else if (node.getBitCount())
//    {
//        m_return = builder.getIntNTy(node.getBitCount());
//    }
//    else
//    {
//        m_return = builder.getVoidTy();
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::PointerType& node)
//{
//    node.getType().accept(*this);
//    auto* type = std::get<TypeRetType>(m_return);
//    if (!type->isVoidTy())
//    {
//        m_return = llvm::PointerType::getUnqual(type);
//    }
//    else
//    {
//        m_return = builder.getInt64Ty();
//    }
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::ArrayType& node)
//{
//    node.getType()->accept(*this);
//    m_return = llvm::ArrayType::get(std::get<TypeRetType>(m_return), node.getSize());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::StructType& node)
//{
//    m_return = module->getTypeByName("struct." + node.getName());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::UnionType& node)
//{
//    m_return = module->getTypeByName("union." + node.getName());
//}
//
//void OpenCL::Codegen::Context::visit(const OpenCL::Syntax::EnumType&)
//{
//    m_return = builder.getInt32Ty();
//}



OpenCL::Codegen::PrimitiveType::Primitive OpenCL::Codegen::PrimitiveType::getPrimitive() const
{
    return m_primitive;
}



const OpenCL::Codegen::Type& OpenCL::Codegen::ArrayType::getType() const
{
    return *m_type;
}

std::size_t OpenCL::Codegen::ArrayType::getSize() const
{
    return m_size;
}

OpenCL::Codegen::ArrayType::ArrayType(bool isConst,
                                      bool isVolatile,
                                      bool isRestricted,
                                      std::shared_ptr<OpenCL::Codegen::Type>  type,
                                      std::size_t size) : Type(isConst, isVolatile, isRestricted), m_type(std::move(type)), m_size(size)
{}

OpenCL::Codegen::Type::Type(bool isConst, bool isVolatile, bool isRestricted)
    : m_isConst(isConst), m_isVolatile(isVolatile), m_isRestricted(isRestricted)
{}

bool OpenCL::Codegen::Type::isConst() const
{
    return m_isConst;
}

bool OpenCL::Codegen::Type::isVolatile() const
{
    return m_isVolatile;
}

bool OpenCL::Codegen::Type::isRestricted() const
{
    return m_isRestricted;
}

const std::string& OpenCL::Codegen::Type::getName() const
{
    return m_name;
}

void OpenCL::Codegen::Type::setName(const std::string& name)
{
    m_name = name;
}

OpenCL::Codegen::PrimitiveType::PrimitiveType(bool isConst,
                                              bool isVolatile,
                                              bool isRestricted,
                                              OpenCL::Codegen::PrimitiveType::Primitive primitive) : Type(isConst,
                                                                                                          isVolatile,
                                                                                                          isRestricted),
                                                                                                     m_primitive(
                                                                                                         primitive)
{}

OpenCL::Codegen::PointerType::PointerType(bool isConst,
                                          bool isVolatile,
                                          bool isRestricted,
                                          std::shared_ptr<OpenCL::Codegen::Type>  elementType) : Type(isConst,
                                                                                                            isVolatile,
                                                                                                            isRestricted),
                                                                                                       m_elementType(std::move(
                                                                                                           elementType))
{}

const OpenCL::Codegen::Type& OpenCL::Codegen::PointerType::getElementType() const
{
    return *m_elementType;
}

OpenCL::Codegen::StructType::StructType(bool isConst, bool isVolatile, bool isRestricted,const std::string& name)
    : Type(isConst,
           isVolatile,
           isRestricted)
{
    setName(name);
}

OpenCL::Codegen::UnionType::UnionType(bool isConst, bool isVolatile, bool isRestricted,const std::string& name)
    : Type(isConst,
           isVolatile,
           isRestricted)
{
    setName(name);
}

OpenCL::Codegen::EnumType::EnumType(bool isConst, bool isVolatile, bool isRestricted,const std::string& name)
    : Type(isConst,
           isVolatile,
           isRestricted)
{
    setName(name);
}

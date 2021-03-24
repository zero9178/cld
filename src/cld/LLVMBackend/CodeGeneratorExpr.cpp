#include "CodeGenerator.hpp"

#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsX86.h>

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::ExpressionBase& expression)
{
    if (m_currentDebugScope && !llvm::isa<llvm::DICompileUnit>(m_currentDebugScope))
    {
        m_builder.SetCurrentDebugLocation(getLocation(expression.begin()));
    }
    return expression.match([](const Semantics::ErrorExpression&) -> Value { CLD_UNREACHABLE; },
                            [&](const auto& value) -> Value { return visit(value); });
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::Constant& constant)
{
    auto* type = visit(constant.getType());
    if (std::holds_alternative<llvm::APSInt>(constant.getValue()))
    {
        return valueOf(llvm::Constant::getIntegerValue(type, cld::get<llvm::APSInt>(constant.getValue())));
    }
    if (std::holds_alternative<llvm::APFloat>(constant.getValue()))
    {
        return valueOf(llvm::ConstantFP::get(type, cld::get<llvm::APFloat>(constant.getValue())));
    }

    auto array = getStringLiteralData(type->getArrayElementType(), constant.getValue());
    auto* global = new llvm::GlobalVariable(m_module, array.value->getType(), true, llvm::GlobalValue::PrivateLinkage,
                                            llvm::cast<llvm::Constant>(array.value));
    global->setAlignment(array.alignment);
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return global;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::DeclarationRead& declarationRead)
{
    auto result = declarationRead.getDeclRead().match(
        [&](const auto& declaration) { return m_lvalues.find(&declaration); },
        [&](const Semantics::BuiltinFunction&) -> decltype(m_lvalues)::iterator { CLD_UNREACHABLE; });
    CLD_ASSERT(result != m_lvalues.end());
    return result->second;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::Conversion& conversion)
{
    if (auto* subScript = conversion.getExpression().get_if<Semantics::SubscriptOperator>();
        conversion.getKind() == Semantics::Conversion::LValue && subScript
        && Semantics::isVector(subScript->getPointerExpression().getType()))
    {
        auto vector = visit(subScript->getPointerExpression());
        auto integer = visit(subScript->getIntegerExpression());
        return m_builder.CreateExtractElement(vector, integer);
    }
    auto value = visit(conversion.getExpression());
    switch (conversion.getKind())
    {
        case Semantics::Conversion::LValue:
        {
            if (Semantics::isArray(conversion.getExpression().getType())
                && !Semantics::isVariableLengthArray(conversion.getExpression().getType()))
            {
                return createInBoundsGEP(value, {m_builder.getInt64(0), m_builder.getInt64(0)});
            }
            if (Semantics::isFunctionType(conversion.getExpression().getType())
                || Semantics::isBitfieldAccess(conversion.getExpression())
                || Semantics::isVariableLengthArray(conversion.getExpression().getType()))
            {
                return value;
            }

            return createLoad(value, conversion.getExpression().getType().isVolatile());
        }
        case Semantics::Conversion::IntegerPromotion:
        {
            auto& prevType = conversion.getExpression().getType();
            if (Semantics::isEnum(prevType))
            {
                // This should be a noop for enums
                return value;
            }
            return valueOf(m_builder.CreateIntCast(value.value, visit(conversion.getType()),
                                                   prevType.cast<Semantics::PrimitiveType>().isSigned()));
        }
        case Semantics::Conversion::Implicit:
        {
            auto& prevType = conversion.getExpression().getType();
            auto& newType = conversion.getType();
            if (Semantics::isBool(newType))
            {
                return valueOf(m_builder.CreateIntCast(toBool(value.value), visit(newType), false));
            }
            if (newType.is<Semantics::PointerType>())
            {
                if (Semantics::isInteger(prevType))
                {
                    return valueOf(m_builder.CreateIntToPtr(value.value, visit(newType)));
                }
                if (Semantics::isArray(prevType))
                {
                    return createInBoundsGEP(value, {m_builder.getInt64(0), m_builder.getInt64(0)});
                }
                return createPointerCast(value, visit(newType));
            }
            [[fallthrough]];
        }
        case Semantics::Conversion::ArithmeticConversion:
        {
            auto& prevType = conversion.getExpression().getType();
            auto& newType = conversion.getType();
            if (Semantics::isVector(newType))
            {
                if (Semantics::isArithmetic(prevType))
                {
                    return m_builder.CreateVectorSplat(newType.cast<Semantics::VectorType>().getSize(), value);
                }

                // signed to unsigned cast, only implicit cast allowed with vectors and is a noop in LLVM IR
                CLD_ASSERT(Semantics::isVector(prevType)
                           && Semantics::isInteger(Semantics::getVectorElementType(prevType)));
                return value;
            }
            if (Semantics::isInteger(prevType) && Semantics::isInteger(newType))
            {
                return valueOf(m_builder.CreateIntCast(value.value, visit(newType),
                                                       prevType.cast<Semantics::PrimitiveType>().isSigned()));
            }
            if (Semantics::isInteger(prevType))
            {
                if (prevType.cast<Semantics::PrimitiveType>().isSigned())
                {
                    return valueOf(m_builder.CreateSIToFP(value.value, visit(newType)));
                }

                return valueOf(m_builder.CreateUIToFP(value.value, visit(newType)));
            }
            if (Semantics::isInteger(newType))
            {
                if (prevType.is<Semantics::PointerType>())
                {
                    return valueOf(m_builder.CreatePtrToInt(value.value, visit(newType)));
                }
                if (newType.cast<Semantics::PrimitiveType>().isSigned())
                {
                    return valueOf(m_builder.CreateFPToSI(value.value, visit(newType)));
                }

                return valueOf(m_builder.CreateFPToUI(value.value, visit(newType)));
            }
            return valueOf(m_builder.CreateFPCast(value.value, visit(newType)));
        }
        case Semantics::Conversion::DefaultArgumentPromotion:
        {
            auto& prevType = conversion.getExpression().getType();
            if (Semantics::isInteger(prevType))
            {
                return valueOf(m_builder.CreateIntCast(value.value, visit(conversion.getType()),
                                                       prevType.cast<Semantics::PrimitiveType>().isSigned()));
            }
            return valueOf(m_builder.CreateFPCast(value.value, visit(conversion.getType())));
        }
    }
    CLD_UNREACHABLE;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::MemberAccess& memberAccess)
{
    auto value = visit(memberAccess.getRecordExpression());
    auto& type = memberAccess.getRecordExpression().getType().is<Semantics::PointerType>() ?
                     Semantics::getPointerElementType(memberAccess.getRecordExpression().getType()) :
                     memberAccess.getRecordExpression().getType();
    if (!memberAccess.getRecordExpression().getType().is<Semantics::PointerType>()
        && memberAccess.getRecordExpression().getValueCategory() != Semantics::ValueCategory::Lvalue)
    {
        // Struct access is only ever allowed on pointers or lvalues except through the return value of a function
        // then it's also allowed to be an rvalue
        auto* load = llvm::cast<llvm::LoadInst>(value.value);
        value = Value(load->getPointerOperand(), load->getAlign());
        load->eraseFromParent();
    }
    auto& cldField = memberAccess.getField();
    auto indices = llvm::ArrayRef(cldField.indices);
    auto parentTypes = cldField.parentTypes;
    parentTypes.insert(parentTypes.begin(), &type);
    Value field = value;
    for (auto iter = parentTypes.begin(); iter != parentTypes.end(); iter++)
    {
        auto index = iter - parentTypes.begin();
        if (Semantics::isStruct(**iter))
        {
            field = createInBoundsGEP(field, {m_builder.getInt64(0), m_builder.getInt32(indices[index])});
        }
        else
        {
            auto* destTy = visit(iter + 1 == parentTypes.end() ? *cldField.type : **(iter + 1));
            field = createBitCast(field, llvm::PointerType::getUnqual(destTy));
        }
    }

    if (!cldField.bitFieldBounds)
    {
        // If the record expression is the return value of a function and this is a dot access not arrow access
        // we must load because an rvalue is returned and no lvalue conversion will load for us
        if (!memberAccess.getRecordExpression().getType().is<Semantics::PointerType>()
            && (memberAccess.getRecordExpression().getValueCategory() != Semantics::ValueCategory::Lvalue))
        {
            // Arrays are generally passed around as llvm pointers to llvm arrays to be able to decay them to
            // pointers. Best example for this are string literals which for this reason are global variables
            if (cldField.type->is<Semantics::ArrayType>())
            {
                return field;
            }
            return createLoad(field, type.isVolatile());
        }
        return field;
    }

    auto loaded = createLoad(field, memberAccess.getType().isVolatile());
    auto upLeft = loaded.value->getType()->getPrimitiveSizeInBits() - cldField.bitFieldBounds->second;
    auto* shl = m_builder.CreateShl(loaded.value, llvm::ConstantInt::get(loaded.value->getType(), upLeft));
    auto* shrConstant = llvm::ConstantInt::get(loaded.value->getType(), upLeft + cldField.bitFieldBounds->first);
    if (memberAccess.getType().cast<Semantics::PrimitiveType>().isSigned())
    {
        return valueOf(m_builder.CreateAShr(shl, shrConstant));
    }

    return valueOf(m_builder.CreateLShr(shl, shrConstant));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::BinaryOperator& binaryExpression)
{
    auto lhs = visit(binaryExpression.getLeftExpression());
    const auto& lhsType = binaryExpression.getLeftExpression().getType();
    switch (binaryExpression.getKind())
    {
        case Semantics::BinaryOperator::Addition:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return add(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::Subtraction:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return sub(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::Multiply:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return mul(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::Divide:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return div(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::Modulo:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return mod(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::LeftShift:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return shl(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::RightShift:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return shr(lhs, lhsType, rhs, binaryExpression.getRightExpression().getType());
        }
        case Semantics::BinaryOperator::GreaterThan:
        case Semantics::BinaryOperator::LessOrEqual:
        case Semantics::BinaryOperator::GreaterOrEqual:
        case Semantics::BinaryOperator::Equal:
        case Semantics::BinaryOperator::NotEqual:
        case Semantics::BinaryOperator::LessThan:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            llvm::CmpInst::Predicate predicate;
            bool fp;
            bool isSigned;
            if (Semantics::isVector(lhsType))
            {
                fp = Semantics::isArithmetic(Semantics::getVectorElementType(lhsType))
                     && !Semantics::isInteger(Semantics::getVectorElementType(lhsType));
                isSigned = Semantics::isInteger(Semantics::getVectorElementType(lhsType))
                           && Semantics::getVectorElementType(lhsType).cast<Semantics::PrimitiveType>().isSigned();
            }
            else
            {
                fp = Semantics::isArithmetic(lhsType) && !Semantics::isInteger(lhsType);
                isSigned = Semantics::isInteger(lhsType) && lhsType.cast<Semantics::PrimitiveType>().isSigned();
            }

            switch (binaryExpression.getKind())
            {
                case Semantics::BinaryOperator::GreaterThan:
                    if (fp)
                    {
                        predicate = llvm::CmpInst::FCMP_UGT;
                    }
                    else if (isSigned)
                    {
                        predicate = llvm::CmpInst::ICMP_SGT;
                    }
                    else
                    {
                        predicate = llvm::CmpInst::ICMP_UGT;
                    }
                    break;
                case Semantics::BinaryOperator::LessOrEqual:
                    if (fp)
                    {
                        predicate = llvm::CmpInst::FCMP_ULE;
                    }
                    else if (isSigned)
                    {
                        predicate = llvm::CmpInst::ICMP_SLE;
                    }
                    else
                    {
                        predicate = llvm::CmpInst::ICMP_ULE;
                    }
                    break;
                case Semantics::BinaryOperator::GreaterOrEqual:
                    if (fp)
                    {
                        predicate = llvm::CmpInst::FCMP_UGE;
                    }
                    else if (isSigned)
                    {
                        predicate = llvm::CmpInst::ICMP_SGE;
                    }
                    else
                    {
                        predicate = llvm::CmpInst::ICMP_UGE;
                    }
                    break;
                case Semantics::BinaryOperator::Equal:
                    if (fp)
                    {
                        predicate = llvm::CmpInst::FCMP_UEQ;
                    }
                    else
                    {
                        predicate = llvm::CmpInst::ICMP_EQ;
                    }
                    break;
                case Semantics::BinaryOperator::NotEqual:
                    if (fp)
                    {
                        predicate = llvm::CmpInst::FCMP_UGT;
                    }
                    else
                    {
                        predicate = llvm::CmpInst::ICMP_NE;
                    }
                    break;
                case Semantics::BinaryOperator::LessThan:
                    if (fp)
                    {
                        predicate = llvm::CmpInst::FCMP_ULT;
                    }
                    else if (isSigned)
                    {
                        predicate = llvm::CmpInst::ICMP_SLT;
                    }
                    else
                    {
                        predicate = llvm::CmpInst::ICMP_ULT;
                    }
                    break;
                default: CLD_UNREACHABLE;
            }
            auto* value = m_builder.CreateCmp(predicate, lhs, rhs);
            if (Semantics::isVector(lhsType))
            {
                return m_builder.CreateSExt(value, visit(binaryExpression.getType()));
            }
            return m_builder.CreateZExt(value, visit(binaryExpression.getType()));
        }
        case Semantics::BinaryOperator::BitOr:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return m_builder.CreateOr(lhs, rhs);
        }
        case Semantics::BinaryOperator::BitAnd:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return m_builder.CreateAnd(lhs, rhs);
        }
        case Semantics::BinaryOperator::BitXor:
        {
            auto rhs = visit(binaryExpression.getRightExpression());
            return m_builder.CreateXor(lhs, rhs);
        }
        case Semantics::BinaryOperator::LogicAnd:
        {
            if (!m_currentFunction)
            {
                if (llvm::cast<llvm::Constant>(lhs.value)->isNullValue())
                {
                    return lhs;
                }
                return visit(binaryExpression.getRightExpression());
            }
            lhs = boolToi1(lhs);
            auto* falseBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.false", m_currentFunction);
            auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.true", m_currentFunction);
            auto* continueBranch =
                llvm::BasicBlock::Create(m_module.getContext(), "logicAnd.continue", m_currentFunction);
            m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
            m_builder.SetInsertPoint(trueBranch);
            auto rhs = visit(binaryExpression.getRightExpression());
            rhs = boolToi1(rhs);
            m_builder.CreateBr(continueBranch);
            trueBranch = m_builder.GetInsertBlock();
            m_builder.SetInsertPoint(falseBranch);
            m_builder.CreateBr(continueBranch);
            m_builder.SetInsertPoint(continueBranch);
            auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
            phi->addIncoming(m_builder.getFalse(), falseBranch);
            phi->addIncoming(rhs, trueBranch);
            return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
        }
        case Semantics::BinaryOperator::LogicOr:
        {
            if (!m_currentFunction)
            {
                if (!llvm::cast<llvm::Constant>(lhs.value)->isNullValue())
                {
                    return lhs;
                }
                return visit(binaryExpression.getRightExpression());
            }
            lhs = boolToi1(lhs);
            auto* falseBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicOr.false", m_currentFunction);
            auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "logicOr.true", m_currentFunction);
            auto* continueBranch =
                llvm::BasicBlock::Create(m_module.getContext(), "logicOr.continue", m_currentFunction);
            m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
            m_builder.SetInsertPoint(falseBranch);
            auto rhs = visit(binaryExpression.getRightExpression());
            rhs = boolToi1(rhs);
            m_builder.CreateBr(continueBranch);
            falseBranch = m_builder.GetInsertBlock();
            m_builder.SetInsertPoint(trueBranch);
            m_builder.CreateBr(continueBranch);
            m_builder.SetInsertPoint(continueBranch);
            auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
            phi->addIncoming(m_builder.getTrue(), trueBranch);
            phi->addIncoming(rhs, falseBranch);
            return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
        }
    }
    CLD_UNREACHABLE;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::Cast& cast)
{
    auto value = visit(cast.getExpression());
    auto& prevType = cast.getExpression().getType();
    auto& newType = cast.getType();
    return this->cast(value, prevType, newType);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::UnaryOperator& unaryOperator)
{
    auto value = visit(unaryOperator.getOperand());
    bool isVolatile = unaryOperator.getOperand().getType().isVolatile();
    switch (unaryOperator.getKind())
    {
        case Semantics::UnaryOperator::AddressOf:
        case Semantics::UnaryOperator::Dereference:
            // The difference between address of and dereference is that an lvalue conversion follows a dereference
            return value;
        case Semantics::UnaryOperator::PostIncrement:
        {
            auto prev = createLoad(value, isVolatile);
            if (Semantics::isInteger(unaryOperator.getOperand().getType()))
            {
                if (unaryOperator.getOperand().getType().cast<Semantics::PrimitiveType>().isSigned())
                {
                    auto* result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    auto* result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
            }
            else if (!unaryOperator.getOperand().getType().is<Semantics::PointerType>())
            {
                auto* result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                createStore(result, value, isVolatile);
            }
            else
            {
                auto result = createGEP(prev, {m_builder.getInt32(1)});
                createStore(result, value, isVolatile);
            }
            return prev;
        }
        case Semantics::UnaryOperator::PostDecrement:
        {
            auto prev = createLoad(value, isVolatile);
            if (Semantics::isInteger(unaryOperator.getOperand().getType()))
            {
                if (unaryOperator.getOperand().getType().cast<Semantics::PrimitiveType>().isSigned())
                {
                    auto* result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    auto* result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
            }
            else if (!unaryOperator.getOperand().getType().is<Semantics::PointerType>())
            {
                auto* result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                createStore(result, value, isVolatile);
            }
            else
            {
                auto result = createGEP(prev, {m_builder.getInt32(-1)});
                createStore(result, value, isVolatile);
            }
            return prev;
        }
        case Semantics::UnaryOperator::PreIncrement:
        {
            Value result = nullptr;
            auto prev = createLoad(value, isVolatile);
            if (Semantics::isInteger(unaryOperator.getOperand().getType()))
            {
                if (unaryOperator.getOperand().getType().cast<Semantics::PrimitiveType>().isSigned())
                {
                    result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
            }
            else if (!unaryOperator.getOperand().getType().is<Semantics::PointerType>())
            {
                result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                createStore(result, value, isVolatile);
            }
            else
            {
                result = createGEP(prev, {m_builder.getInt32(1)});
                createStore(result, value, isVolatile);
            }
            return result;
        }
        case Semantics::UnaryOperator::PreDecrement:
        {
            Value result = nullptr;
            auto prev = createLoad(value, isVolatile);
            if (Semantics::isInteger(unaryOperator.getOperand().getType()))
            {
                if (unaryOperator.getOperand().getType().cast<Semantics::PrimitiveType>().isSigned())
                {
                    result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
                else
                {
                    result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev.value->getType(), 1));
                    createStore(result, value, isVolatile);
                }
            }
            else if (!unaryOperator.getOperand().getType().is<Semantics::PointerType>())
            {
                result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev.value->getType(), 1));
                createStore(result, value, isVolatile);
            }
            else
            {
                result = createGEP(prev, {m_builder.getInt32(-1)});
                createStore(result, value, isVolatile);
            }
            return result;
        }
        case Semantics::UnaryOperator::Plus: return value;
        case Semantics::UnaryOperator::Minus:
        {
            if (Semantics::isInteger(unaryOperator.getOperand().getType()))
            {
                if (unaryOperator.getOperand().getType().cast<Semantics::PrimitiveType>().isSigned())
                {
                    return m_builder.CreateNSWNeg(value);
                }
                return m_builder.CreateNeg(value);
            }

            return m_builder.CreateFNeg(value);
        }
        case Semantics::UnaryOperator::BooleanNegate:
        {
            value = m_builder.CreateNot(boolToi1(value));
            return m_builder.CreateZExt(value, visit(Semantics::PrimitiveType::createInt(
                                                   false, false, m_sourceInterface.getLanguageOptions())));
        }
        case Semantics::UnaryOperator::BitwiseNegate: return m_builder.CreateNot(value);
    }
    CLD_UNREACHABLE;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::SizeofOperator& sizeofOperator)
{
    if (sizeofOperator.getSize())
    {
        auto* type =
            visit(Semantics::PrimitiveType::createSizeT(false, false, m_programInterface.getLanguageOptions()));
        return llvm::ConstantInt::get(type, *sizeofOperator.getSize());
    }
    const Semantics::Type& type = cld::match(
        sizeofOperator.getVariant(),
        [](const Semantics::SizeofOperator::TypeVariant& typeVariant) -> const Semantics::Type& {
            return *typeVariant.type;
        },
        [](const cld::IntrVarPtr<Semantics::ExpressionBase>& expression) -> const Semantics::Type& {
            return expression->getType();
        });
    auto& elementType = [&]() -> decltype(auto) {
        auto* currType = &type;
        while (Semantics::isArray(*currType))
        {
            currType = &Semantics::getArrayElementType(*currType);
        }
        return *currType;
    }();
    llvm::Value* value = m_builder.getInt64(elementType.getSizeOf(m_programInterface));
    for (auto& iter : Semantics::RecursiveVisitor(type, Semantics::ARRAY_TYPE_NEXT_FN))
    {
        llvm::Value* temp;
        if (iter.is<Semantics::ArrayType>())
        {
            temp = m_builder.getInt64(iter.cast<Semantics::ArrayType>().getSize());
        }
        else
        {
            temp = m_valSizes[iter.cast<Semantics::ValArrayType>().getExpression()];
        }
        value = m_builder.CreateMul(value, temp);
    }
    return value;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::SubscriptOperator& subscriptOperator)
{
    auto& integer = subscriptOperator.getIntegerExpression();
    auto* pointer = &subscriptOperator.getPointerExpression();
    if (!pointer->is<Semantics::Conversion>()
        || pointer->cast<Semantics::Conversion>().getKind() != Semantics::Conversion::LValue
        || !pointer->cast<Semantics::Conversion>().getExpression().getType().is<Semantics::ValArrayType>())
    {
        auto llvmInteger = visit(integer);
        auto llvmPointer = visit(*pointer);

        llvmInteger = m_builder.CreateIntCast(llvmInteger, m_builder.getInt64Ty(),
                                              integer.getType().cast<Semantics::PrimitiveType>().isSigned());
        return createGEP(llvmPointer, {llvmInteger});
    }

    std::vector<llvm::Value*> products = {m_builder.CreateIntCast(
        visit(integer), m_builder.getInt64Ty(), integer.getType().cast<Semantics::PrimitiveType>().isSigned())};
    llvm::Value* dimensionProduct = nullptr;
    while (pointer->is<Semantics::Conversion>()
           && pointer->cast<Semantics::Conversion>().getKind() == Semantics::Conversion::LValue)
    {
        auto& subExpr = pointer->cast<Semantics::Conversion>().getExpression();
        if (!subExpr.is<Semantics::SubscriptOperator>())
        {
            break;
        }
        auto& subOp = subExpr.cast<Semantics::SubscriptOperator>();
        auto& subPointer = subOp.getPointerExpression();
        auto& subInteger = subOp.getIntegerExpression();
        llvm::Value* newInt = visit(subInteger);
        newInt = m_builder.CreateIntCast(newInt, m_builder.getInt64Ty(),
                                         subInteger.getType().cast<Semantics::PrimitiveType>().isSigned());
        llvm::Value* newDimension;
        if (Semantics::isArrayType(subExpr.getType()))
        {
            newDimension = m_builder.getInt64(subExpr.getType().cast<Semantics::ArrayType>().getSize());
        }
        else
        {
            newDimension = m_valSizes[subExpr.getType().cast<Semantics::ValArrayType>().getExpression()];
        }
        if (!dimensionProduct)
        {
            dimensionProduct = newDimension;
        }
        else
        {
            dimensionProduct = m_builder.CreateMul(dimensionProduct, newDimension);
        }
        products.push_back(m_builder.CreateMul(newInt, dimensionProduct));
        pointer = &subPointer;
    }
    auto basePointer = visit(*pointer);
    auto* sum = std::accumulate(products.begin() + 1, products.end(), products.front(),
                                [&](llvm::Value* lhs, llvm::Value* rhs) { return m_builder.CreateAdd(lhs, rhs); });
    return createGEP(basePointer, {sum});
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::Conditional& conditional)
{
    auto boolean = visit(conditional.getBoolExpression());
    boolean = boolToi1(boolean);
    if (!m_currentFunction)
    {
        // We are in a constant expression, most likely in an initializer constant expression
        auto* constant = llvm::cast<llvm::Constant>(boolean.value);
        if (constant->isNullValue())
        {
            return visit(conditional.getFalseExpression());
        }
        return visit(conditional.getTrueExpression());
    }

    auto* trueBranch = llvm::BasicBlock::Create(m_builder.getContext(), "cond.true", m_currentFunction);
    auto* falseBranch = llvm::BasicBlock::Create(m_builder.getContext(), "cond.false", m_currentFunction);
    auto* contBr = llvm::BasicBlock::Create(m_builder.getContext(), "cond.continue", m_currentFunction);
    m_builder.CreateCondBr(boolean, trueBranch, falseBranch);
    m_builder.SetInsertPoint(trueBranch);
    auto trueValue = visit(conditional.getTrueExpression());
    m_builder.CreateBr(contBr);
    trueBranch = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(falseBranch);
    auto falseValue = visit(conditional.getFalseExpression());
    m_builder.CreateBr(contBr);
    falseBranch = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(contBr);
    auto* phi = m_builder.CreatePHI(trueValue.value->getType(), 2);
    phi->addIncoming(trueValue, trueBranch);
    phi->addIncoming(falseValue, falseBranch);
    if (trueValue.value->getType()->isPointerTy())
    {
        return valueOf(phi, std::min(*trueValue.alignment, *falseValue.alignment));
    }
    return phi;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::Assignment& assignment)
{
    if (!Semantics::isBitfieldAccess(assignment.getLeftExpression()))
    {
        auto lhs = [&] {
            if (auto* subscript = assignment.getLeftExpression().get_if<Semantics::SubscriptOperator>();
                subscript && Semantics::isVector(subscript->getPointerExpression().getType()))
            {
                auto vector = visit(subscript->getPointerExpression());
                auto* load = llvm::cast<llvm::LoadInst>(vector.value);
                vector = valueOf(load->getPointerOperand(), load->getAlign());
                load->eraseFromParent();
                return vector;
            }
            return visit(assignment.getLeftExpression());
        }();

        auto rhs = visit(assignment.getRightExpression());
        if (assignment.getKind() != Semantics::Assignment::Simple)
        {
            auto load = createLoad(lhs, assignment.getLeftExpression().getType().isVolatile());
            if (Semantics::isArithmetic(assignment.getLeftExpression().getType()))
            {
                load = cast(load, assignment.getLeftExpression().getType(), assignment.getLeftCalcType());
            }
            switch (assignment.getKind())
            {
                case Semantics::Assignment::Simple: CLD_UNREACHABLE;
                case Semantics::Assignment::Plus:
                    rhs = add(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::Minus:
                    rhs = sub(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::Divide:
                    rhs = div(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::Multiply:
                    rhs = mul(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::Modulo:
                    rhs = mod(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::LeftShift:
                    rhs = shl(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::RightShift:
                    rhs = shr(load, assignment.getLeftCalcType(), rhs, assignment.getRightExpression().getType());
                    break;
                case Semantics::Assignment::BitAnd: rhs = m_builder.CreateAnd(load, rhs); break;
                case Semantics::Assignment::BitOr: rhs = m_builder.CreateOr(load, rhs); break;
                case Semantics::Assignment::BitXor: rhs = m_builder.CreateXor(load, rhs); break;
            }
            rhs = cast(rhs, assignment.getRightExpression().getType(), assignment.getLeftExpression().getType());
        }
        if (auto* subscript = assignment.getLeftExpression().get_if<Semantics::SubscriptOperator>();
            subscript && Semantics::isVector(subscript->getPointerExpression().getType()))
        {
            auto load = createLoad(lhs, assignment.getLeftExpression().getType().isVolatile());
            auto integer = visit(subscript->getIntegerExpression());
            rhs = m_builder.CreateInsertElement(load, rhs, integer);
        }
        createStore(rhs, lhs, assignment.getLeftExpression().getType().isVolatile());
        return createLoad(lhs, assignment.getLeftExpression().getType().isVolatile());
    }
    auto& memberAccess = assignment.getLeftExpression().cast<Semantics::MemberAccess>();
    auto lhsRecord = visit(memberAccess.getRecordExpression());
    auto& type = memberAccess.getRecordExpression().getType().is<Semantics::PointerType>() ?
                     Semantics::getPointerElementType(memberAccess.getRecordExpression().getType()) :
                     memberAccess.getRecordExpression().getType();
    auto rhsValue = visit(assignment.getRightExpression());

    auto& cldField = memberAccess.getField();
    auto field = lhsRecord;
    auto indices = llvm::ArrayRef(cldField.indices);
    auto parentTypes = cldField.parentTypes;
    parentTypes.insert(parentTypes.begin(), &type);
    for (auto iter = parentTypes.begin(); iter != parentTypes.end(); iter++)
    {
        auto index = iter - parentTypes.begin();
        if (Semantics::isStruct(type))
        {
            field = createInBoundsGEP(field, {m_builder.getInt64(0), m_builder.getInt32(indices[index])});
        }
        else
        {
            auto* destTy = visit(iter + 1 == parentTypes.end() ? *cldField.type : **(iter + 1));
            field = createBitCast(field, llvm::PointerType::getUnqual(destTy));
        }
    }

    auto loaded = createLoad(field, type.isVolatile());
    auto size = cldField.bitFieldBounds->second - cldField.bitFieldBounds->first;
    llvm::Value* mask = llvm::ConstantInt::get(rhsValue.value->getType(), (1u << size) - 1);
    if (assignment.getKind() != Semantics::Assignment::Simple)
    {
        llvm::Value* load =
            m_builder.CreateAShr(loaded, llvm::ConstantInt::get(mask->getType(), cldField.bitFieldBounds->first));
        load = m_builder.CreateAnd(load, mask);
        load = cast(load, assignment.getLeftCalcType(), assignment.getRightExpression().getType());
        switch (assignment.getKind())
        {
            case Semantics::Assignment::Simple: CLD_UNREACHABLE;
            case Semantics::Assignment::Plus:
                rhsValue = add(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::Minus:
                rhsValue = sub(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::Divide:
                rhsValue = div(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::Multiply:
                rhsValue = mul(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::Modulo:
                rhsValue = mod(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::LeftShift:
                rhsValue = shl(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::RightShift:
                rhsValue = shr(load, assignment.getLeftCalcType(), rhsValue, assignment.getRightExpression().getType());
                break;
            case Semantics::Assignment::BitAnd: rhsValue = m_builder.CreateAnd(load, rhsValue); break;
            case Semantics::Assignment::BitOr: rhsValue = m_builder.CreateOr(load, rhsValue); break;
            case Semantics::Assignment::BitXor: rhsValue = m_builder.CreateXor(load, rhsValue); break;
        }
    }
    rhsValue = m_builder.CreateAnd(rhsValue, mask);
    rhsValue = m_builder.CreateShl(rhsValue,
                                   llvm::ConstantInt::get(rhsValue.value->getType(), cldField.bitFieldBounds->first));
    mask = m_builder.CreateShl(mask, llvm::ConstantInt::get(mask->getType(), cldField.bitFieldBounds->first));
    mask = m_builder.CreateNot(mask);
    // TODO: Types could mismatch
    loaded = m_builder.CreateAnd(loaded, mask);
    auto* result = m_builder.CreateOr(loaded, rhsValue);
    createStore(result, field, type.isVolatile());
    return result;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::CommaExpression& commaExpression)
{
    for (auto& iter : commaExpression.getCommaExpressions())
    {
        visitVoidExpression(*iter.first);
    }
    return visit(commaExpression.getLastExpression());
}

namespace
{
bool isBuiltinFunctionCall(const cld::Semantics::ExpressionBase& expression)
{
    if (!expression.is<cld::Semantics::Conversion>())
    {
        return false;
    }
    auto& conversion = expression.cast<cld::Semantics::Conversion>();
    if (conversion.getKind() != cld::Semantics::Conversion::LValue)
    {
        return false;
    }
    if (!conversion.getExpression().is<cld::Semantics::DeclarationRead>())
    {
        return false;
    }
    auto& decl = conversion.getExpression().cast<cld::Semantics::DeclarationRead>();
    return decl.getDeclRead().is<cld::Semantics::BuiltinFunction>();
}

const cld::Semantics::BuiltinFunction& getBuiltinFunctionCall(const cld::Semantics::ExpressionBase& expression)
{
    CLD_ASSERT(expression.is<cld::Semantics::Conversion>());
    auto& conversion = expression.cast<cld::Semantics::Conversion>();
    CLD_ASSERT(conversion.getKind() == cld::Semantics::Conversion::LValue);
    CLD_ASSERT(conversion.getExpression().is<cld::Semantics::DeclarationRead>());
    auto& decl = conversion.getExpression().cast<cld::Semantics::DeclarationRead>();
    return decl.getDeclRead().cast<cld::Semantics::BuiltinFunction>();
}

} // namespace

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::CallExpression& call)
{
    if (isBuiltinFunctionCall(call.getFunctionExpression()))
    {
        auto builtin = getBuiltinFunctionCall(call.getFunctionExpression());
        switch (builtin.getKind())
        {
            case Semantics::BuiltinFunction::VAStart:
            {
                auto list = visit(*call.getArgumentExpressions()[0]);
                if (llvm::isa<llvm::LoadInst>(list.value))
                {
                    auto* prev = llvm::cast<llvm::LoadInst>(list.value);
                    list = Value(prev->getPointerOperand(), prev->getAlign());
                    prev->eraseFromParent();
                }
                list = createBitCast(list, m_builder.getInt8PtrTy());
                return m_builder.CreateIntrinsic(llvm::Intrinsic::vastart, {}, {list});
            }
            case Semantics::BuiltinFunction::VAEnd:
            {
                auto list = visit(*call.getArgumentExpressions()[0]);
                if (llvm::isa<llvm::LoadInst>(list.value))
                {
                    auto* prev = llvm::cast<llvm::LoadInst>(list.value);
                    list = Value(prev->getPointerOperand(), prev->getAlign());
                    prev->eraseFromParent();
                }
                list = createBitCast(list, m_builder.getInt8PtrTy());
                return m_builder.CreateIntrinsic(llvm::Intrinsic::vaend, {}, {list});
            }
            case Semantics::BuiltinFunction::VACopy:
            {
                auto list1 = visit(*call.getArgumentExpressions()[0]);
                if (llvm::isa<llvm::LoadInst>(list1.value))
                {
                    auto* prev = llvm::cast<llvm::LoadInst>(list1.value);
                    list1 = Value(prev->getPointerOperand(), prev->getAlign());
                    prev->eraseFromParent();
                }
                list1 = createBitCast(list1, m_builder.getInt8PtrTy());
                auto list2 = visit(*call.getArgumentExpressions()[1]);
                if (llvm::isa<llvm::LoadInst>(list2.value))
                {
                    auto* prev = llvm::cast<llvm::LoadInst>(list2.value);
                    list2 = Value(prev->getPointerOperand(), prev->getAlign());
                    prev->eraseFromParent();
                }
                list2 = createBitCast(list2, m_builder.getInt8PtrTy());
                return m_builder.CreateIntrinsic(llvm::Intrinsic::vacopy, {}, {list1, list2});
            }
            case Semantics::BuiltinFunction::LLAbs:
            case Semantics::BuiltinFunction::LAbs:
            case Semantics::BuiltinFunction::Abs:
            {
                auto longLong = visit(*call.getArgumentExpressions()[0]);
                auto* neg = m_builder.CreateNSWNeg(longLong);
                auto* isNegative =
                    m_builder.CreateICmpSLT(longLong, llvm::ConstantInt::get(longLong.value->getType(), 0));
                return m_builder.CreateSelect(isNegative, neg, longLong);
            }
            case Semantics::BuiltinFunction::FAbs:
            case Semantics::BuiltinFunction::FAbsf:
            case Semantics::BuiltinFunction::FAbsl:
            {
                auto floatingPoint = visit(*call.getArgumentExpressions()[0]);
                return m_builder.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, floatingPoint);
            }
            case Semantics::BuiltinFunction::Inf: return llvm::ConstantFP::getInfinity(m_builder.getDoubleTy());
            case Semantics::BuiltinFunction::Inff: return llvm::ConstantFP::getInfinity(m_builder.getFloatTy());
            case Semantics::BuiltinFunction::Infl:
            {
                auto* type = visit(
                    Semantics::PrimitiveType::createLongDouble(false, false, m_sourceInterface.getLanguageOptions()));
                return llvm::ConstantFP::getInfinity(type);
            }
            case Semantics::BuiltinFunction::SyncSynchronize:
                return m_builder.CreateFence(llvm::AtomicOrdering::SequentiallyConsistent);
            case Semantics::BuiltinFunction::ReturnAddress:
                return m_builder.CreateIntrinsic(llvm::Intrinsic::returnaddress, {},
                                                 {visit(*call.getArgumentExpressions()[0])});
            case Semantics::BuiltinFunction::ExtractReturnAddr:
                // TODO:
                return visit(*call.getArgumentExpressions()[0]);
            case Semantics::BuiltinFunction::FRobReturnAddr:
                // TODO:
                return visit(*call.getArgumentExpressions()[0]);
            case Semantics::BuiltinFunction::FrameAddress:
                return m_builder.CreateIntrinsic(llvm::Intrinsic::frameaddress, {},
                                                 {visit(*call.getArgumentExpressions()[0])});
            case Semantics::BuiltinFunction::ExpectWithProbability:
            {
                auto ret = visit(*call.getArgumentExpressions()[0]);
                auto expected = visit(*call.getArgumentExpressions()[1]);
                auto probability = visit(*call.getArgumentExpressions()[2]);
                return m_builder.CreateIntrinsic(llvm::Intrinsic::expect_with_probability, {ret.value->getType()},
                                                 {ret, expected, probability});
            }
            case Semantics::BuiltinFunction::Expect:
            {
                auto ret = visit(*call.getArgumentExpressions()[0]);
                auto expected = visit(*call.getArgumentExpressions()[1]);
                return m_builder.CreateIntrinsic(llvm::Intrinsic::expect, {ret.value->getType()}, {ret, expected});
            }
            case Semantics::BuiltinFunction::ClearCache:
            {
                auto first = visit(*call.getArgumentExpressions()[0]);
                auto second = visit(*call.getArgumentExpressions()[1]);
                return m_builder.CreateIntrinsic(llvm::Intrinsic::clear_cache, {}, {first, second});
            }
            case Semantics::BuiltinFunction::Prefetch:
            {
                auto address = visit(*call.getArgumentExpressions()[0]);
                llvm::Value* rw = m_builder.getInt32(0);
                llvm::Value* locality = m_builder.getInt32(3);
                if (call.getArgumentExpressions().size() > 1)
                {
                    auto rwValue = visit(*call.getArgumentExpressions()[1]);
                    rw = m_builder.CreateIntCast(rwValue.value, m_builder.getInt32Ty(), false);
                }
                if (call.getArgumentExpressions().size() > 2)
                {
                    auto localityValue = visit(*call.getArgumentExpressions()[2]);
                    locality = m_builder.CreateIntCast(localityValue.value, m_builder.getInt32Ty(), false);
                }

                auto* i8Star = m_builder.CreateBitCast(address, m_builder.getInt8PtrTy());
                return m_builder.CreateIntrinsic(llvm::Intrinsic::prefetch, {i8Star->getType()},
                                                 {i8Star, rw, locality, m_builder.getInt32(1)});
            }
            case Semantics::BuiltinFunction::Unreachable: return m_builder.CreateUnreachable();
            case Semantics::BuiltinFunction::Trap: return m_builder.CreateIntrinsic(llvm::Intrinsic::trap, {}, {});
            case Semantics::BuiltinFunction::SyncFetchAndAdd:
            case Semantics::BuiltinFunction::SyncFetchAndSub:
            case Semantics::BuiltinFunction::SyncFetchAndOr:
            case Semantics::BuiltinFunction::SyncFetchAndAnd:
            case Semantics::BuiltinFunction::SyncFetchAndXor:
            case Semantics::BuiltinFunction::SyncFetchAndNand:
            case Semantics::BuiltinFunction::SyncLockTestAndSet:
            {
                auto pointer = visit(*call.getArgumentExpressions()[0]);
                auto value = visit(*call.getArgumentExpressions()[1]);
                auto* pointerType =
                    value.value->getType()->isPointerTy() ? value.value->getType()->getPointerElementType() : nullptr;
                if (pointerType)
                {
                    llvm::IntegerType* intPtrType = m_module.getDataLayout().getIntPtrType(m_module.getContext(), 0);
                    value = m_builder.CreatePtrToInt(value, intPtrType);
                    pointer = createPointerCast(pointer, llvm::PointerType::getUnqual(intPtrType));
                }
                llvm::AtomicRMWInst::BinOp inst;
                switch (builtin.getKind())
                {
                    case Semantics::BuiltinFunction::SyncFetchAndAdd: inst = llvm::AtomicRMWInst::Add; break;
                    case Semantics::BuiltinFunction::SyncFetchAndSub: inst = llvm::AtomicRMWInst::Sub; break;
                    case Semantics::BuiltinFunction::SyncFetchAndOr: inst = llvm::AtomicRMWInst::Or; break;
                    case Semantics::BuiltinFunction::SyncFetchAndAnd: inst = llvm::AtomicRMWInst::And; break;
                    case Semantics::BuiltinFunction::SyncFetchAndXor: inst = llvm::AtomicRMWInst::Xor; break;
                    case Semantics::BuiltinFunction::SyncFetchAndNand: inst = llvm::AtomicRMWInst::Nand; break;
                    case Semantics::BuiltinFunction::SyncLockTestAndSet: inst = llvm::AtomicRMWInst::Xchg; break;
                    default: CLD_UNREACHABLE;
                }
                auto* ret =
                    m_builder.CreateAtomicRMW(inst, pointer, value, llvm::AtomicOrdering::SequentiallyConsistent);
                if (!pointerType)
                {
                    return ret;
                }
                return valueOf(m_builder.CreateIntToPtr(ret, llvm::PointerType::getUnqual(pointerType)));
            }
            case Semantics::BuiltinFunction::SyncAddAndFetch:
            case Semantics::BuiltinFunction::SyncSubAndFetch:
            case Semantics::BuiltinFunction::SyncOrAndFetch:
            case Semantics::BuiltinFunction::SyncAndAndFetch:
            case Semantics::BuiltinFunction::SyncXorAndFetch:
            case Semantics::BuiltinFunction::SyncNandAndFetch:
            {
                auto pointer = visit(*call.getArgumentExpressions()[0]);
                auto value = visit(*call.getArgumentExpressions()[1]);
                auto* pointerType =
                    value.value->getType()->isPointerTy() ? value.value->getType()->getPointerElementType() : nullptr;
                IntrVarValue<Semantics::Type> type = call.getArgumentExpressions()[1]->getType();
                if (pointerType)
                {
                    llvm::IntegerType* intPtrType = m_module.getDataLayout().getIntPtrType(m_module.getContext(), 0);
                    value = m_builder.CreatePtrToInt(value, intPtrType);
                    pointer = createPointerCast(pointer, llvm::PointerType::getUnqual(intPtrType));
                    type =
                        Semantics::PrimitiveType::createLongLong(false, false, m_sourceInterface.getLanguageOptions());
                }
                llvm::AtomicRMWInst::BinOp inst;
                switch (builtin.getKind())
                {
                    case Semantics::BuiltinFunction::SyncAddAndFetch: inst = llvm::AtomicRMWInst::Add; break;
                    case Semantics::BuiltinFunction::SyncSubAndFetch: inst = llvm::AtomicRMWInst::Sub; break;
                    case Semantics::BuiltinFunction::SyncOrAndFetch: inst = llvm::AtomicRMWInst::Or; break;
                    case Semantics::BuiltinFunction::SyncAndAndFetch: inst = llvm::AtomicRMWInst::And; break;
                    case Semantics::BuiltinFunction::SyncXorAndFetch: inst = llvm::AtomicRMWInst::Xor; break;
                    case Semantics::BuiltinFunction::SyncNandAndFetch: inst = llvm::AtomicRMWInst::Nand; break;
                    default: CLD_UNREACHABLE;
                }
                auto* temp =
                    m_builder.CreateAtomicRMW(inst, pointer, value, llvm::AtomicOrdering::SequentiallyConsistent);
                llvm::Value* ret;
                switch (builtin.getKind())
                {
                    case Semantics::BuiltinFunction::SyncAddAndFetch: ret = add(temp, *type, value, *type); break;
                    case Semantics::BuiltinFunction::SyncSubAndFetch: ret = sub(temp, *type, value, *type); break;
                    case Semantics::BuiltinFunction::SyncOrAndFetch: ret = m_builder.CreateOr(temp, value); break;
                    case Semantics::BuiltinFunction::SyncAndAndFetch: ret = m_builder.CreateAnd(temp, value); break;
                    case Semantics::BuiltinFunction::SyncXorAndFetch: ret = m_builder.CreateXor(temp, value); break;
                    case Semantics::BuiltinFunction::SyncNandAndFetch:
                        ret = m_builder.CreateNot(m_builder.CreateAnd(temp, value));
                        break;
                    default: CLD_UNREACHABLE;
                }
                if (!pointerType)
                {
                    return ret;
                }
                return valueOf(m_builder.CreateIntToPtr(ret, llvm::PointerType::getUnqual(pointerType)));
            }
            case Semantics::BuiltinFunction::SyncValCompareAndSwap:
            case Semantics::BuiltinFunction::SyncBoolCompareAndSwap:
            {
                auto pointer = visit(*call.getArgumentExpressions()[0]);
                auto oldValue = visit(*call.getArgumentExpressions()[1]);
                auto newValue = visit(*call.getArgumentExpressions()[2]);
                auto* pointerType = oldValue.value->getType()->isPointerTy() ?
                                        oldValue.value->getType()->getPointerElementType() :
                                        nullptr;
                if (pointerType)
                {
                    llvm::IntegerType* intPtrType = m_module.getDataLayout().getIntPtrType(m_module.getContext(), 0);
                    oldValue = m_builder.CreatePtrToInt(oldValue, intPtrType);
                    newValue = m_builder.CreatePtrToInt(newValue, intPtrType);
                    pointer = createPointerCast(pointer, llvm::PointerType::getUnqual(intPtrType));
                }
                auto* temp = m_builder.CreateAtomicCmpXchg(pointer, oldValue, newValue,
                                                           llvm::AtomicOrdering::SequentiallyConsistent,
                                                           llvm::AtomicOrdering::SequentiallyConsistent);
                if (builtin.getKind() == Semantics::BuiltinFunction::SyncBoolCompareAndSwap)
                {
                    return m_builder.CreateExtractValue(temp, {1});
                }
                auto* ret = m_builder.CreateExtractValue(temp, {0});
                if (!pointerType)
                {
                    return ret;
                }
                return valueOf(m_builder.CreateIntToPtr(ret, llvm::PointerType::getUnqual(pointerType)));
            }
            case Semantics::BuiltinFunction::SyncLockRelease:
            {
                auto pointer = visit(*call.getArgumentExpressions()[0]);
                if (pointer.value->getType()->getPointerElementType()->isPointerTy())
                {
                    llvm::IntegerType* intPtrType = m_module.getDataLayout().getIntPtrType(m_module.getContext(), 0);
                    pointer = createPointerCast(pointer, llvm::PointerType::getUnqual(intPtrType));
                }
                auto* store = m_builder.CreateAlignedStore(
                    llvm::Constant::getNullValue(pointer.value->getType()->getPointerElementType()), pointer,
                    pointer.alignment);
                store->setAtomic(llvm::AtomicOrdering::Release);
                return store;
            }
            case Semantics::BuiltinFunction::x86CpuInit:
            {
                auto cpuInit = m_module.getOrInsertFunction("__cpu_indicator_init", m_builder.getVoidTy());
                return m_builder.CreateCall(cpuInit);
            }
            case Semantics::BuiltinFunction::x86CpuIs:
            case Semantics::BuiltinFunction::x86CpuSupports:
            {
                llvm::errs() << "Not implemented yet, sorry\n";
                std::terminate();
            }
        }
        CLD_UNREACHABLE;
    }
    auto function = visit(call.getFunctionExpression());
    Semantics::FunctionType cldFt =
        Semantics::getPointerElementType(call.getFunctionExpression().getType()).cast<Semantics::FunctionType>();
    auto* ft = llvm::cast<llvm::FunctionType>(function.value->getType()->getPointerElementType());
    bool isKandR = cldFt.isKandR();
    if (isKandR || cldFt.isLastVararg())
    {
        std::vector<Semantics::FunctionType::Parameter> arguments;
        for (auto& iter : call.getArgumentExpressions())
        {
            arguments.push_back({&iter->getType(), ""});
        }
        cldFt = Semantics::FunctionType(&cldFt.getReturnType(), std::move(arguments), false, false);
        ft = llvm::cast<llvm::FunctionType>(visit(cldFt));
    }
    if (isKandR)
    {
        function = createBitCast(function, llvm::PointerType::getUnqual(ft));
    }
    std::vector<llvm::Value*> arguments;
    for (auto& iter : call.getArgumentExpressions())
    {
        arguments.push_back(visit(*iter));
    }
    return m_abi->generateFunctionCall(*this, function, ft, cldFt, std::move(arguments));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::CompoundLiteral& compoundLiteral)
{
    auto* type = visit(compoundLiteral.getType());
    if (compoundLiteral.hasStaticLifetime())
    {
        llvm::Constant* constant = nullptr;
        constant =
            llvm::cast<llvm::Constant>(visit(compoundLiteral.getInitializer(), compoundLiteral.getType(), type).value);
        type = constant->getType();
        auto* global = new llvm::GlobalVariable(m_module, type, true, llvm::GlobalValue::PrivateLinkage, constant);
        global->setAlignment(llvm::MaybeAlign(compoundLiteral.getType().getAlignOf(m_programInterface)));
        return global;
    }

    auto* var = createAllocaAtTop(type);
    var->setAlignment(llvm::Align(compoundLiteral.getType().getAlignOf(m_programInterface)));
    if (m_builder.GetInsertBlock())
    {
        visit(compoundLiteral.getInitializer(), compoundLiteral.getType(), var);
    }
    return var;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::BuiltinVAArg& vaArg)
{
    auto vaList = visit(vaArg.getExpression());
    return m_abi->generateVAArg(*this, vaList, vaArg.getType());
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::BuiltinOffsetOf& offsetOf)
{
    return llvm::ConstantInt::get(visit(offsetOf.getType()), offsetOf.getOffset());
}

namespace
{
llvm::Value* visitStaticInitializerList(cld::CGLLVM::CodeGenerator& codeGenerator,
                                        const cld::Semantics::InitializerList& initializerList,
                                        const cld::Semantics::Type& type, llvm::Type* llvmType)
{
    struct Aggregate
    {
        std::vector<std::variant<llvm::Constant*, Aggregate>> vector;
        std::optional<std::uint32_t> unionIndex;
    };
    Aggregate constants;
    auto genAggregate = cld::YComb{[&](auto&& self, const cld::Semantics::Type& type, Aggregate& aggregate) -> void
                                   {
                                       if (cld::Semantics::isStruct(type))
                                       {
                                           auto fields = cld::Semantics::getFieldLayout(type);
                                           aggregate.vector.resize(fields.size());
                                           for (const auto* iter = fields.begin(); iter != fields.end(); iter++)
                                           {
                                               if (!cld::Semantics::isAggregate(*iter->type))
                                               {
                                                   continue;
                                               }
                                               auto& vector =
                                                   aggregate.vector[iter - fields.begin()].emplace<Aggregate>();
                                               self(*iter->type, vector);
                                           }
                                       }
                                       else if (auto* array = type.get_if<cld::Semantics::ArrayType>())
                                       {
                                           std::variant<llvm::Constant*, Aggregate> value;
                                           if (cld::Semantics::isAggregate(array->getType()))
                                           {
                                               auto& vector = value.emplace<Aggregate>();
                                               self(array->getType(), vector);
                                           }
                                           aggregate.vector.resize(array->getSize(), value);
                                       }
                                       else if (auto* vector = type.get_if<cld::Semantics::VectorType>())
                                       {
                                           aggregate.vector.resize(vector->getSize());
                                       }
                                   }};
    genAggregate(type, constants);

    for (auto& [path, expression] : initializerList.getFields())
    {
        auto replacement = codeGenerator.visit(*expression);
        llvm::Constant** value = [&, &path = path, &expression = expression]() -> llvm::Constant**
        {
            Aggregate* current = &constants;
            const cld::Semantics::Type* currentType = &type;
            for (auto& iter : llvm::ArrayRef(path).drop_back())
            {
                if (cld::Semantics::isUnion(*currentType))
                {
                    auto fields = cld::Semantics::getFieldLayout(*currentType);
                    if (current->vector.empty() || current->unionIndex != iter)
                    {
                        current->vector.resize(1);
                        current->vector[0] = {};
                        current->unionIndex = iter;
                        if (cld::Semantics::isAggregate(*fields[iter].type))
                        {
                            auto& vector = current->vector.back().emplace<Aggregate>();
                            genAggregate(*fields[iter].type, vector);
                        }
                    }
                    currentType = fields[iter].type;
                    current = &cld::get<Aggregate>(current->vector[0]);
                    continue;
                }
                if (cld::Semantics::isStruct(*currentType))
                {
                    currentType = cld::Semantics::getFieldLayout(*currentType)[iter].type;
                }
                else if (cld::Semantics::isArray(*currentType))
                {
                    currentType = &cld::Semantics::getArrayElementType(*currentType);
                }
                else if (cld::Semantics::isVector(*currentType))
                {
                    currentType = &cld::Semantics::getVectorElementType(*currentType);
                }
                current = &cld::get<Aggregate>(current->vector[iter]);
            }
            if (cld::Semantics::isStringLiteralExpr(*expression))
            {
                auto& constant = expression->cast<cld::Semantics::Constant>();
                auto& aggregate = cld::get<Aggregate>(current->vector[path.back()]);
                auto size = std::min(aggregate.vector.size(),
                                     cld::match(
                                         constant.getValue(),
                                         [](const std::string& str) -> std::size_t { return str.size() + 1; },
                                         [](const cld::Lexer::NonCharString& nonCharString) -> std::size_t {
                                             return nonCharString.characters.size();
                                         },
                                         [](const auto&) -> std::size_t { CLD_UNREACHABLE; }));
                auto* elementType = cld::match(
                    constant.getValue(),
                    [&](const std::string&) -> llvm::Type* {
                        return codeGenerator.visit(cld::Semantics::PrimitiveType::createChar(
                            false, false, codeGenerator.getProgramInterface().getLanguageOptions()));
                    },
                    [&](const cld::Lexer::NonCharString&) -> llvm::Type* {
                        return codeGenerator.visit(cld::Semantics::PrimitiveType::createWcharT(
                            false, false, codeGenerator.getProgramInterface().getLanguageOptions()));
                    },
                    [](const auto&) -> llvm::Type* { CLD_UNREACHABLE; });
                for (std::size_t i = 0; i < size; i++)
                {
                    auto* constantValue = cld::match(
                        constant.getValue(),
                        [&](const std::string& str) -> llvm::Constant* {
                            if (i == str.size())
                            {
                                return llvm::ConstantInt::get(elementType, 0);
                            }
                            return llvm::ConstantInt::get(elementType, str[i], true);
                        },
                        [&](const cld::Lexer::NonCharString& str) -> llvm::Constant* {
                            return llvm::ConstantInt::get(elementType, str.characters[i], false);
                        },
                        [](const auto&) -> llvm::Constant* { CLD_UNREACHABLE; });
                    aggregate.vector[i] = constantValue;
                }
                return nullptr;
            }
            if (cld::Semantics::isUnion(*currentType))
            {
                auto fields = cld::Semantics::getFieldLayout(*currentType);
                if (current->vector.empty() || current->unionIndex != path.back())
                {
                    current->vector.resize(1);
                    current->vector[0] = {};
                    current->unionIndex = path.back();
                    if (cld::Semantics::isAggregate(*fields[path.back()].type))
                    {
                        auto& vector = current->vector.back().emplace<Aggregate>();
                        genAggregate(*fields[path.back()].type, vector);
                    }
                }
                return &cld::get<llvm::Constant*>(current->vector[0]);
            }
            return &cld::get<llvm::Constant*>(current->vector[path.back()]);
        }();
        if (value)
        {
            *value = llvm::cast<llvm::Constant>(replacement.value);
        }
    }

    return cld::YComb{[&](auto&& self, const cld::Semantics::Type& type, llvm::Type* llvmType,
                          const Aggregate& aggregate) -> llvm::Constant* {
        const llvm::Module& module = codeGenerator.getModule();
        if (cld::Semantics::isStruct(type))
        {
            std::vector<llvm::Constant*> elements;
            std::vector<llvm::Type*> elementTypes;
            auto fields = cld::Semantics::getFieldLayout(type);
            for (std::size_t i = 0; i < aggregate.vector.size();)
            {
                if (!fields[i].bitFieldBounds)
                {
                    elements.push_back(cld::match(
                        aggregate.vector[i],
                        [&](const Aggregate& subAggregate) -> llvm::Constant* {
                            return self(*fields[i].type, llvmType->getStructElementType(fields[i].layoutIndex),
                                        subAggregate);
                        },
                        [&](llvm::Constant* constant) {
                            if (constant)
                            {
                                return constant;
                            }
                            return llvm::Constant::getNullValue(llvmType->getStructElementType(fields[i].layoutIndex));
                        }));
                    elementTypes.push_back(elements.back()->getType());
                    i++;
                    continue;
                }
                elements.push_back(llvm::Constant::getNullValue(llvmType->getStructElementType(fields[i].layoutIndex)));
                elementTypes.push_back(elements.back()->getType());
                for (; i < aggregate.vector.size() && fields[i].bitFieldBounds; i++)
                {
                    auto* value = cld::match(
                        aggregate.vector[i],
                        [&](const Aggregate& subAggregate) -> llvm::Constant* {
                            return self(*fields[i].type, llvmType->getStructElementType(fields[i].layoutIndex),
                                        subAggregate);
                        },
                        [&](llvm::Constant* constant) -> llvm::Constant* {
                            if (constant)
                            {
                                return constant;
                            }
                            return llvm::Constant::getNullValue(llvmType->getStructElementType(fields[i].layoutIndex));
                        });
                    auto size = fields[i].bitFieldBounds->second - fields[i].bitFieldBounds->first;
                    auto* mask = llvm::ConstantInt::get(value->getType(), (1u << size) - 1);
                    value = llvm::ConstantExpr::getAnd(value, mask);
                    value = llvm::ConstantExpr::getShl(
                        value, llvm::ConstantInt::get(value->getType(), fields[i].bitFieldBounds->first));
                    mask = llvm::ConstantExpr::getShl(
                        mask, llvm::ConstantInt::get(mask->getType(), fields[i].bitFieldBounds->first));
                    mask = llvm::ConstantExpr::getNot(mask);
                    elements.back() = llvm::ConstantExpr::getAnd(elements.back(), mask);
                    elements.back() = llvm::ConstantExpr::getOr(elements.back(), value);
                }
            }
            return llvm::ConstantStruct::get(llvm::StructType::get(module.getContext(), elementTypes), elements);
        }
        if (cld::Semantics::isArray(type))
        {
            bool isSame = true;
            llvm::Type* elementType = nullptr;
            std::vector<llvm::Type*> elementTypes;
            std::vector<llvm::Constant*> elements;
            for (std::size_t i = 0; i < aggregate.vector.size(); i++)
            {
                elements.push_back(cld::match(
                    aggregate.vector[i],
                    [&](llvm::Constant* constant) {
                        if (constant)
                        {
                            return constant;
                        }
                        return llvm::Constant::getNullValue(llvmType->getArrayElementType());
                    },
                    [&](const Aggregate& subAggregate) -> llvm::Constant* {
                        return self(type.cast<cld::Semantics::ArrayType>().getType(), llvmType->getArrayElementType(),
                                    subAggregate);
                    }));
                elementTypes.push_back(elements.back()->getType());
                if (elementType == nullptr)
                {
                    elementType = elementTypes.back();
                }
                else
                {
                    isSame = isSame && elementType == elementTypes.back();
                }
            }
            if (isSame)
            {
                return llvm::ConstantArray::get(llvm::ArrayType::get(elementType, elements.size()), elements);
            }
            return llvm::ConstantStruct::get(llvm::StructType::get(module.getContext(), elementTypes), elements);
        }
        if (cld::Semantics::isUnion(type))
        {
            // Union
            auto fields = cld::Semantics::getFieldLayout(type);
            auto* llvmSubType = codeGenerator.visit(*fields[*aggregate.unionIndex].type);
            llvm::Constant* element = cld::match(
                aggregate.vector[0], [](llvm::Constant* constant) -> llvm::Constant* { return constant; },
                [&](const Aggregate& subAggregate) -> llvm::Constant* {
                    return self(*fields[*aggregate.unionIndex].type, llvmSubType, subAggregate);
                });
            auto paddingSize = module.getDataLayout().getTypeAllocSize(llvmType).getKnownMinSize()
                               - module.getDataLayout().getTypeAllocSize(element->getType()).getKnownMinSize();
            if (paddingSize == 0)
            {
                return llvm::ConstantStruct::get(llvm::StructType::get(element->getType()), element);
            }
            auto* padding = llvm::ArrayType::get(llvm::IntegerType::getInt8Ty(module.getContext()), paddingSize);
            auto* newType = llvm::StructType::get(element->getType(), padding);

            return llvm::ConstantStruct::get(newType, {element, llvm::UndefValue::get(padding)});
        }
        // Vector
        CLD_ASSERT(cld::Semantics::isVector(type));
        std::vector<llvm::Constant*> elements;
        for (std::size_t i = 0; i < aggregate.vector.size(); i++)
        {
            elements.push_back(cld::get<llvm::Constant*>(aggregate.vector[i]));
            if (!elements.back())
            {
                elements.back() = llvm::Constant::getNullValue(llvmType->getScalarType());
            }
        }
        return llvm::ConstantVector::get(elements);
    }}(type, llvmType, constants);
}
} // namespace

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::Initializer& initializer,
                                                     const Semantics::Type& type,
                                                     std::variant<Value, llvm::Type*> pointer)
{
    return cld::match(
        initializer,
        [&](const cld::IntrVarPtr<Semantics::ExpressionBase>& expression) -> Value {
            if (std::holds_alternative<Value>(pointer))
            {
                auto value = visit(*expression);
                if (Semantics::isStringLiteralExpr(*expression))
                {
                    m_builder.CreateMemCpy(cld::get<Value>(pointer), cld::get<Value>(pointer).alignment, value,
                                           value.alignment, expression->getType().getSizeOf(m_programInterface));
                    return nullptr;
                }
                createStore(value, cld::get<Value>(pointer), type.isVolatile());
                return nullptr;
            }
            if (Semantics::isStringLiteralExpr(*expression))
            {
                auto& constant = expression->cast<Semantics::Constant>();
                return getStringLiteralData(visit(expression->getType())->getArrayElementType(), constant.getValue());
            }
            return visit(*expression);
        },
        [&](const Semantics::InitializerList& initializerList) -> Value {
            if (std::holds_alternative<llvm::Type*>(pointer))
            {
                return visitStaticInitializerList(*this, initializerList, type, cld::get<llvm::Type*>(pointer));
            }
            auto value = cld::get<Value>(pointer);
            m_builder.CreateMemSet(value, m_builder.getInt8(0), type.getSizeOf(m_programInterface), value.alignment,
                                   type.isVolatile());
            for (auto& [path, expression] : initializerList.getFields())
            {
                auto subValue = visit(*expression);
                auto currentPointer = value;
                const Semantics::Type* currentType = &type;
                std::optional<std::pair<std::uint32_t, std::uint32_t>> bitFieldBounds;
                for (auto iter : path)
                {
                    if (Semantics::isStruct(*currentType))
                    {
                        auto fieldLayout = Semantics::getFieldLayout(*currentType);
                        currentPointer = createInBoundsGEP(
                            currentPointer, {m_builder.getInt64(0), m_builder.getInt32(fieldLayout[iter].layoutIndex)});
                        currentType = fieldLayout[iter].type;
                        bitFieldBounds = fieldLayout[iter].bitFieldBounds;
                    }
                    else if (Semantics::isUnion(*currentType))
                    {
                        auto fields = Semantics::getFieldLayout(*currentType);
                        currentType = fields[iter].type;
                        currentPointer =
                            createBitCast(currentPointer, llvm::PointerType::getUnqual(visit(*currentType)));
                        bitFieldBounds = fields[iter].bitFieldBounds;
                    }
                    else if (Semantics::isArray(*currentType))
                    {
                        currentType = &Semantics::getArrayElementType(*currentType);
                        currentPointer =
                            createInBoundsGEP(currentPointer, {m_builder.getInt64(0), m_builder.getInt64(iter)});
                    }
                    else
                    {
                        CLD_ASSERT(Semantics::isVector(*currentType));
                        auto loaded = createLoad(currentPointer, currentType->isVolatile());
                        auto* inserted = m_builder.CreateInsertElement(loaded, subValue, iter);
                        createStore(inserted, currentPointer, currentType->isVolatile());
                        currentType = nullptr;
                        // Vectors cannot contain aggregate types and are therefore always the end of the chain.
                        // Since we also can't take pointer to a vector element we need to handle it specially
                        // and insert here already. currentType is set to null to signify that we are done here
                    }
                }
                if (!currentType)
                {
                    continue;
                }
                if (!bitFieldBounds)
                {
                    if (Semantics::isStringLiteralExpr(*expression))
                    {
                        m_builder.CreateMemCpy(currentPointer, currentPointer.alignment, subValue, subValue.alignment,
                                               expression->getType().getSizeOf(m_programInterface));
                        continue;
                    }
                    createStore(subValue, currentPointer, currentType->isVolatile());
                    continue;
                }
                llvm::Value* loaded = createLoad(currentPointer, currentType->isVolatile());
                auto size = bitFieldBounds->second - bitFieldBounds->first;
                llvm::Value* mask = llvm::ConstantInt::get(subValue.value->getType(), (1u << size) - 1);
                subValue = m_builder.CreateAnd(subValue, mask);
                subValue = m_builder.CreateShl(
                    subValue, llvm::ConstantInt::get(subValue.value->getType(), bitFieldBounds->first));
                mask = m_builder.CreateShl(mask, llvm::ConstantInt::get(mask->getType(), bitFieldBounds->first));
                mask = m_builder.CreateNot(mask);
                loaded = m_builder.CreateAnd(loaded, mask);
                auto* result = m_builder.CreateOr(loaded, subValue);
                createStore(result, currentPointer, type.isVolatile());
                continue;
            }
            return nullptr;
        });
}

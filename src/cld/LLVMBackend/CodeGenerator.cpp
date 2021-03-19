#include "CodeGenerator.hpp"

llvm::Value* cld::CGLLVM::CodeGenerator::toBool(llvm::Value* value)
{
    if (value->getType()->isIntegerTy())
    {
        if (auto* cast = llvm::dyn_cast<llvm::CastInst>(value);
            cast && cast->getSrcTy() == m_builder.getInt1Ty() && cast->getNumUses() == 0)
        {
            auto* result = cast->getOperand(0);
            cast->eraseFromParent();
            return result;
        }
        return m_builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0));
    }
    if (value->getType()->isPointerTy())
    {
        return m_builder.CreateICmpNE(value,
                                      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(value->getType())));
    }

    return m_builder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::add(Value lhs, const Semantics::Type& lhsType, Value rhs,
                                                   const Semantics::Type& rhsType)
{
    if (Semantics::isArithmetic(lhsType) && Semantics::isArithmetic(rhsType))
    {
        if (Semantics::isInteger(lhsType))
        {
            if (cld::get<Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
            {
                return m_builder.CreateNSWAdd(lhs, rhs);
            }

            return m_builder.CreateAdd(lhs, rhs);
        }

        return m_builder.CreateFAdd(lhs, rhs);
    }
    if (Semantics::isVector(lhsType))
    {
        if (Semantics::isInteger(Semantics::getVectorElementType(lhsType)))
        {
            return m_builder.CreateAdd(lhs, rhs);
        }
        return m_builder.CreateFAdd(lhs, rhs);
    }

    auto pointer = lhs.value->getType()->isPointerTy() ? lhs : rhs;
    auto integer = pointer.value == lhs.value ? rhs : lhs;
    auto& pointerType = pointer.value == lhs.value ? lhsType : rhsType;
    integer = m_builder.CreateIntCast(
        integer.value, m_builder.getInt64Ty(),
        cld::get<Semantics::PrimitiveType>((pointer.value == lhs.value ? rhsType : lhsType).getVariant()).isSigned());
    if (!Semantics::isVariableLengthArray(cld::get<Semantics::PointerType>(pointerType.getVariant()).getElementType()))
    {
        return createGEP(pointer, integer.value);
    }
    auto& array = Semantics::getPointerElementType(pointerType);
    Value product = integer;
    for (auto& iter : Semantics::RecursiveVisitor(array, Semantics::ARRAY_TYPE_NEXT_FN))
    {
        llvm::Value* value;
        if (Semantics::isArrayType(iter))
        {
            value = m_builder.getInt64(cld::get<Semantics::ArrayType>(iter.getVariant()).getSize());
        }
        else
        {
            value = m_valSizes[cld::get<Semantics::ValArrayType>(iter.getVariant()).getExpression()];
        }
        product = m_builder.CreateMul(product.value, value);
    }
    return createGEP(pointer, product.value);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::sub(Value lhs, const Semantics::Type& lhsType, Value rhs,
                                                   const Semantics::Type& rhsType)
{
    if (Semantics::isArithmetic(lhsType) && Semantics::isArithmetic(rhsType))
    {
        if (Semantics::isInteger(lhsType))
        {
            if (cld::get<Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
            {
                return m_builder.CreateNSWSub(lhs, rhs);
            }

            return m_builder.CreateSub(lhs, rhs);
        }

        return m_builder.CreateFSub(lhs, rhs);
    }
    if (Semantics::isVector(lhsType))
    {
        if (Semantics::isInteger(Semantics::getVectorElementType(lhsType)))
        {
            return m_builder.CreateSub(lhs, rhs);
        }
        return m_builder.CreateFSub(lhs, rhs);
    }

    CLD_ASSERT(lhs.value->getType()->isPointerTy());
    if (rhs.value->getType()->isIntegerTy())
    {
        rhs = valueOf(m_builder.CreateNeg(rhs.value));
        rhs = valueOf(m_builder.CreateIntCast(rhs.value, m_builder.getInt64Ty(),
                                              cld::get<Semantics::PrimitiveType>(rhsType.getVariant()).isSigned()));
        return createGEP(lhs, rhs.value);
    }

    return m_builder.CreatePtrDiff(lhs, rhs);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::mul(Value lhs, const Semantics::Type& lhsType, Value rhs,
                                                   const Semantics::Type&)
{
    if (Semantics::isInteger(lhsType))
    {
        if (cld::get<Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
        {
            return m_builder.CreateNSWMul(lhs, rhs);
        }

        return m_builder.CreateMul(lhs, rhs);
    }
    if (Semantics::isVector(lhsType))
    {
        if (Semantics::isInteger(Semantics::getVectorElementType(lhsType)))
        {
            return m_builder.CreateMul(lhs, rhs);
        }
        return m_builder.CreateFMul(lhs, rhs);
    }

    return m_builder.CreateFMul(lhs, rhs);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::div(Value lhs, const Semantics::Type& lhsType, Value rhs,
                                                   const Semantics::Type&)
{
    if (Semantics::isInteger(lhsType))
    {
        if (cld::get<Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
        {
            return m_builder.CreateSDiv(lhs, rhs);
        }

        return m_builder.CreateUDiv(lhs, rhs);
    }
    if (Semantics::isVector(lhsType) && Semantics::isInteger(Semantics::getVectorElementType(lhsType)))
    {
        if (cld::get<Semantics::PrimitiveType>(Semantics::getVectorElementType(lhsType).getVariant()).isSigned())
        {
            return m_builder.CreateSDiv(lhs, rhs);
        }
        return m_builder.CreateUDiv(lhs, rhs);
    }

    return m_builder.CreateFDiv(lhs, rhs);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::mod(Value lhs, const Semantics::Type& lhsType, Value rhs,
                                                   const Semantics::Type&)
{
    if (Semantics::isInteger(lhsType) && cld::get<Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
    {
        return m_builder.CreateSRem(lhs, rhs);
    }
    if (Semantics::isVector(lhsType))
    {
        if (cld::get<Semantics::PrimitiveType>(Semantics::getVectorElementType(lhsType).getVariant()).isSigned())
        {
            return m_builder.CreateSRem(lhs, rhs);
        }
    }

    return m_builder.CreateURem(lhs, rhs);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::shl(Value lhs, const Semantics::Type&, Value rhs,
                                                   const Semantics::Type& rhsType)
{
    if (lhs.value->getType() != rhs.value->getType())
    {
        rhs = m_builder.CreateIntCast(rhs, lhs.value->getType(),
                                      cld::get<Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
    }
    return m_builder.CreateShl(lhs, rhs);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::shr(Value lhs, const Semantics::Type& lhsType, Value rhs,
                                                   const Semantics::Type& rhsType)
{
    if (lhs.value->getType() != rhs.value->getType())
    {
        rhs = m_builder.CreateIntCast(rhs, lhs.value->getType(),
                                      cld::get<Semantics::PrimitiveType>(rhsType.getVariant()).isSigned());
    }
    if (Semantics::isInteger(lhsType) && !cld::get<Semantics::PrimitiveType>(lhsType.getVariant()).isSigned())
    {
        return m_builder.CreateLShr(lhs, rhs);
    }
    if (Semantics::isVector(lhsType))
    {
        if (!cld::get<Semantics::PrimitiveType>(Semantics::getVectorElementType(lhsType).getVariant()).isSigned())
        {
            return m_builder.CreateLShr(lhs, rhs);
        }
    }
    return m_builder.CreateAShr(lhs, rhs);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::cast(Value value, const Semantics::Type& from, const Semantics::Type& to)
{
    if (Semantics::isVector(from) || Semantics::isVector(to))
    {
        return m_builder.CreateBitCast(value, visit(to));
    }
    if (Semantics::isPointer(to))
    {
        if (Semantics::isInteger(from))
        {
            return valueOf(m_builder.CreateIntToPtr(value, visit(to)));
        }
        // User requested and apparently does not care about alignment if this were to go wrong
        return createPointerCast(value, visit(to));
    }
    if (Semantics::isBool(to))
    {
        return m_builder.CreateIntCast(toBool(value), visit(to), false);
    }
    if (Semantics::isInteger(to) && Semantics::isInteger(from))
    {
        return m_builder.CreateIntCast(value, visit(to),
                                       cld::get<Semantics::PrimitiveType>(from.getVariant()).isSigned());
    }
    if (Semantics::isArithmetic(to) && Semantics::isInteger(from))
    {
        if (cld::get<Semantics::PrimitiveType>(from.getVariant()).isSigned())
        {
            return m_builder.CreateSIToFP(value, visit(to));
        }

        return m_builder.CreateUIToFP(value, visit(to));
    }
    if (Semantics::isInteger(to))
    {
        if (Semantics::isPointer(from))
        {
            return m_builder.CreatePtrToInt(value, visit(to));
        }
        if (cld::get<Semantics::PrimitiveType>(to.getVariant()).isSigned())
        {
            return m_builder.CreateFPToSI(value, visit(to));
        }

        return m_builder.CreateFPToUI(value, visit(to));
    }
    return m_builder.CreateFPCast(value, visit(to));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::createLoad(Value ptr, bool isVolatile)
{
    return valueOf(m_builder.CreateAlignedLoad(ptr.value, ptr.alignment, isVolatile));
}

void cld::CGLLVM::CodeGenerator::createStore(llvm::Value* value, Value ptr, bool isVolatile)
{
    m_builder.CreateAlignedStore(value, ptr.value, ptr.alignment, isVolatile);
}

llvm::AllocaInst* cld::CGLLVM::CodeGenerator::createAllocaAtTop(llvm::Type* type, std::string_view name)
{
    llvm::IRBuilderBase::InsertPointGuard guard(m_builder);
    m_builder.SetInsertPoint(&m_currentFunction->getEntryBlock(), m_currentFunction->getEntryBlock().begin());
    return m_builder.CreateAlloca(type, nullptr, llvm::StringRef{name});
}

llvm::Align cld::CGLLVM::CodeGenerator::calcAlign(llvm::Value* gep, llvm::Align alignment)
{
    bool deleteInst = false;
    cld::ScopeExit exit([&] {
        if (deleteInst)
        {
            gep->deleteValue();
        }
    });
    auto* constExpr = llvm::dyn_cast<llvm::ConstantExpr>(gep);
    if (constExpr)
    {
        deleteInst = true;
        gep = constExpr->getAsInstruction();
    }
    auto* gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(gep);
    if (!gepInst)
    {
        return m_module.getDataLayout().getABITypeAlign(gep->getType()->getPointerElementType());
    }
    llvm::APInt integer(m_module.getDataLayout().getPointerSizeInBits(0), 0);
    if (gepInst->accumulateConstantOffset(m_module.getDataLayout(), integer))
    {
        for (std::uint64_t value = alignment.value(); value > 0; value >>= 1)
        {
            if (integer.urem(value) == 0)
            {
                return llvm::Align(value);
            }
        }
        CLD_UNREACHABLE;
    }
    return m_module.getDataLayout().getABITypeAlign(gep->getType()->getPointerElementType());
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::createGEP(Value ptr, llvm::ArrayRef<llvm::Value*> indices)
{
    auto* result = m_builder.CreateGEP(ptr.value, indices);
    return valueOf(result, calcAlign(result, *ptr.alignment));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::createInBoundsGEP(Value ptr, llvm::ArrayRef<llvm::Value*> indices)
{
    auto* result = m_builder.CreateInBoundsGEP(ptr.value, indices);
    return valueOf(result, calcAlign(result, *ptr.alignment));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::createPointerCast(Value ptr, llvm::Type* pointerType)
{
    return valueOf(m_builder.CreateBitCast(ptr, pointerType));
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::createBitCast(Value ptr, llvm::Type* pointerType, bool checked)
{
    CLD_ASSERT(
        !checked
        || (ptr.alignment && pointerType->isPointerTy()
            && (pointerType->getPointerElementType()->isFunctionTy()
                || *ptr.alignment >= m_module.getDataLayout().getABITypeAlign(pointerType->getPointerElementType()))));
    return valueOf(m_builder.CreateBitCast(ptr, pointerType), ptr.alignment);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::createSafeBitCast(Value ptr, llvm::Type* pointerType)
{
    CLD_ASSERT(ptr.alignment);
    if (*ptr.alignment >= m_module.getDataLayout().getABITypeAlign(pointerType->getPointerElementType()))
    {
        // Only if the destination element type is allowed to have the alignment of the source pointer is this cast
        // valid
        return valueOf(m_builder.CreateBitCast(ptr, pointerType), ptr.alignment);
    }

    auto* alloca = createAllocaAtTop(pointerType->getPointerElementType());
    alloca->setAlignment(m_module.getDataLayout().getABITypeAlign(pointerType->getPointerElementType()));
    m_builder.CreateLifetimeStart(
        alloca, m_builder.getInt64(m_module.getDataLayout().getTypeAllocSize(pointerType->getPointerElementType())));
    m_builder.CreateMemCpy(alloca, alloca->getAlign(), ptr, *ptr.alignment,
                           m_module.getDataLayout().getTypeAllocSize(pointerType->getPointerElementType()));
    return alloca;
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::getStringLiteralData(llvm::Type* elementType,
                                                                    const Semantics::Constant::Variant& value)
{
    if (std::holds_alternative<std::string>(value))
    {
        return Value(llvm::ConstantDataArray::getString(m_module.getContext(), cld::get<std::string>(value)),
                     llvm::Align(1));
    }

    auto& str = cld::get<Lexer::NonCharString>(value);
    std::uint8_t size =
        m_sourceInterface.getLanguageOptions().sizeOf(m_sourceInterface.getLanguageOptions().wcharUnderlyingType);
    switch (size)
    {
        case 2:
        {
            std::vector<std::uint16_t> convertedData(str.characters.size());
            std::transform(str.characters.begin(), str.characters.end(), convertedData.begin(),
                           [](std::uint32_t value) -> std::uint16_t { return value; });
            std::vector<char> rawData(convertedData.size() * 2);
            std::memcpy(rawData.data(), convertedData.data(), rawData.size());
            return Value(llvm::ConstantDataArray::getRaw(llvm::StringRef(rawData.data(), rawData.size()),
                                                         convertedData.size(), elementType),
                         llvm::Align(2));
        }
        case 4:
        {
            std::vector<char> rawData(str.characters.size() * 4);
            std::memcpy(rawData.data(), str.characters.data(), rawData.size());
            return Value(llvm::ConstantDataArray::getRaw(llvm::StringRef(rawData.data(), rawData.size()),
                                                         str.characters.size(), elementType),
                         llvm::Align(4));
        }
    }
    CLD_UNREACHABLE;
}

void cld::CGLLVM::CodeGenerator::runDestructors(std::size_t from, std::size_t toExclusive)
{
    while (from > 0 && from != toExclusive)
    {
        // Destructors must be run backwards in order of declaration
        for (auto iter = m_programInterface.getScopes()[from].declarations.rbegin();
             iter != m_programInterface.getScopes()[from].declarations.rend(); iter++)
        {
            const auto* decl = std::get_if<Semantics::VariableDeclaration*>(&iter->second.declared);
            if (!decl)
            {
                continue;
            }
            if (Semantics::isVariableLengthArray((*decl)->getType()))
            {
                auto* alloca = m_stackSaves[*decl];
                if (alloca)
                {
                    auto loaded = createLoad(alloca, false);
                    m_builder.CreateIntrinsic(llvm::Intrinsic::stackrestore, {}, {loaded});
                }
                continue;
            }
        }
        from = m_programInterface.getScopes()[from].previousScope;
    }
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::boolToi1(Value value)
{
    if (auto* cast = llvm::dyn_cast<llvm::CastInst>(value.value);
        cast && cast->getSrcTy() == m_builder.getInt1Ty() && cast->getNumUses() == 0)
    {
        auto* result = cast->getOperand(0);
        cast->eraseFromParent();
        return result;
    }
    return m_builder.CreateTrunc(value.value, m_builder.getInt1Ty());
}

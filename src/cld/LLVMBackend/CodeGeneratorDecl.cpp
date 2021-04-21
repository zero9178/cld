#include "CodeGenerator.hpp"

#ifndef NDEBUG
    #include <llvm/IR/Verifier.h>
#endif

llvm::Type* cld::CGLLVM::CodeGenerator::visit(const Semantics::Type& type)
{
    return type.match(
        [&](const Semantics::PrimitiveType& primitiveType) -> llvm::Type*
        {
            switch (primitiveType.getKind())
            {
                case Semantics::PrimitiveType::Char:
                case Semantics::PrimitiveType::SignedChar:
                case Semantics::PrimitiveType::UnsignedChar: return m_builder.getInt8Ty();
                case Semantics::PrimitiveType::Bool:
                    return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfUnderlineBool * 8);
                case Semantics::PrimitiveType::UnsignedShort:
                case Semantics::PrimitiveType::Short:
                    return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfShort * 8);
                case Semantics::PrimitiveType::Int:
                case Semantics::PrimitiveType::UnsignedInt:
                    return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfInt * 8);
                case Semantics::PrimitiveType::Long:
                case Semantics::PrimitiveType::UnsignedLong:
                    return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfLong * 8);
                case Semantics::PrimitiveType::UnsignedLongLong:
                case Semantics::PrimitiveType::LongLong: return m_builder.getInt64Ty();
                case Semantics::PrimitiveType::Int128:
                case Semantics::PrimitiveType::UnsignedInt128: return m_builder.getInt128Ty();
                case Semantics::PrimitiveType::Float: return m_builder.getFloatTy();
                case Semantics::PrimitiveType::Double: return m_builder.getDoubleTy();
                case Semantics::PrimitiveType::LongDouble:
                    switch (m_sourceInterface.getLanguageOptions().sizeOfLongDoubleBits)
                    {
                        case 64: return m_builder.getDoubleTy();
                        case 80: return llvm::Type::getX86_FP80Ty(m_module.getContext());
                        case 128: return llvm::Type::getFP128Ty(m_module.getContext());
                    }
                    CLD_UNREACHABLE;
                case Semantics::PrimitiveType::Void: return m_builder.getVoidTy();
            }
            CLD_UNREACHABLE;
        },
        [&](const Semantics::ArrayType& arrayType) -> llvm::Type*
        {
            auto* elementType = visit(arrayType.getType());
            if (Semantics::isVariableLengthArray(arrayType.getType()))
            {
                return elementType;
            }
            return llvm::ArrayType::get(elementType, arrayType.getSize());
        },
        [&](const Semantics::FunctionType& functionType) -> llvm::Type*
        {
            auto* returnType = visit(functionType.getReturnType());
            std::vector<llvm::Type*> args;
            for (auto& [type, name] : functionType.getParameters())
            {
                (void)name;
                args.push_back(visit(*Semantics::adjustParameterType(*type)));
            }
            m_abi->applyPlatformABI(functionType, returnType, args);
            return llvm::FunctionType::get(returnType, args, functionType.isLastVararg());
        },
        [&](const Semantics::PointerType& pointerType) -> llvm::Type*
        {
            if (Semantics::isVoid(pointerType.getElementType()))
            {
                return m_builder.getInt8PtrTy();
            }
            auto* elementType = visit(pointerType.getElementType());
            return llvm::PointerType::getUnqual(elementType);
        },
        [&](const Semantics::StructType& structType) -> llvm::Type*
        {
            auto result = m_types.find(structType);
            if (result != m_types.end())
            {
                return result->second;
            }
            auto* structDef = std::get_if<Semantics::StructDefinition>(&structType.getInfo().type);
            auto* type = llvm::StructType::create(
                m_module.getContext(), structType.isAnonymous() ? "struct.anon" : structType.getStructName());
            m_types.insert({structType, type});
            if (!structDef)
            {
                return type;
            }

            std::vector<llvm::Type*> fields;
            for (auto& iter : structDef->getMemLayout())
            {
                fields.push_back(visit(*iter.type));
            }
            type->setBody(fields);
            return type;
        },
        [&](const Semantics::UnionType& unionType) -> llvm::Type*
        {
            auto result = m_types.find(unionType);
            if (result != m_types.end())
            {
                return result->second;
            }
            auto* unionDef = std::get_if<Semantics::UnionDefinition>(&unionType.getInfo().type);
            if (!unionDef)
            {
                auto* type = llvm::StructType::create(
                    m_module.getContext(), unionType.isAnonymous() ? "union.anon" : unionType.getUnionName());
                m_types.insert({unionType, type});
                return type;
            }
            const Semantics::Type* largestAlignment = nullptr;
            std::size_t largestSize = 0;
            for (auto& iter : unionDef->getFieldLayout())
            {
                if (!largestAlignment)
                {
                    largestAlignment = iter.type;
                }
                else if (largestAlignment->getAlignOf(m_programInterface) < iter.type->getAlignOf(m_programInterface))
                {
                    largestAlignment = iter.type;
                }
                largestSize = std::max(largestSize, iter.type->getSizeOf(m_programInterface));
            }
            CLD_ASSERT(largestAlignment);
            largestSize = cld::roundUpTo(largestSize, largestAlignment->getAlignOf(m_programInterface));
            auto* type = unionType.isAnonymous() ?
                             llvm::StructType::create(m_module.getContext()) :
                             llvm::StructType::create(m_module.getContext(), unionType.getUnionName());
            std::vector<llvm::Type*> body = {visit(*largestAlignment)};
            if (largestSize > largestAlignment->getSizeOf(m_programInterface))
            {
                body.emplace_back(llvm::ArrayType::get(m_builder.getInt8Ty(),
                                                       largestSize - largestAlignment->getSizeOf(m_programInterface)));
            }
            type->setBody(body);
            m_types.insert({unionType, type});
            return type;
        },
        [&](const Semantics::AbstractArrayType& arrayType) -> llvm::Type*
        {
            auto* elementType = visit(arrayType.getType());
            if (Semantics::isVariableLengthArray(arrayType.getType()))
            {
                return elementType;
            }
            return llvm::ArrayType::get(elementType, 0);
        },
        [&](const Semantics::VectorType& vectorType) -> llvm::Type*
        {
            auto* elementType = visit(vectorType.getType());
            return llvm::FixedVectorType::get(elementType, vectorType.getSize());
        },
        [&](const Semantics::ErrorType&) -> llvm::Type* { CLD_UNREACHABLE; },
        [&](const Semantics::EnumType& enumType) -> llvm::Type* { return visit(enumType.getInfo().type.getType()); },
        [&](const Semantics::ValArrayType& valArrayType) -> llvm::Type*
        {
            auto expression = m_valSizes.find(valArrayType.getExpression());
            if (expression == m_valSizes.end() && m_currentFunction && m_builder.GetInsertBlock())
            {
                m_valSizes.emplace(
                    valArrayType.getExpression(),
                    m_builder.CreateIntCast(
                        visit(*valArrayType.getExpression()), m_builder.getInt64Ty(),
                        valArrayType.getExpression()->getType().as<Semantics::PrimitiveType>().isSigned()));
            }
            return visit(valArrayType.getType());
        });
}

llvm::DIType* cld::CGLLVM::CodeGenerator::visitDebug(const Semantics::Type& type)
{
    auto* result = type.match(
        [](Semantics::ErrorType) -> llvm::DIType* { CLD_UNREACHABLE; },
        [&](const Semantics::PrimitiveType& primitive) -> llvm::DIType*
        {
            std::string_view name;
            unsigned encoding = 0;
            switch (primitive.getKind())
            {
                case Semantics::PrimitiveType::Char:
                    name = "char";
                    encoding = m_programInterface.getLanguageOptions().charIsSigned ? llvm::dwarf::DW_ATE_signed_char :
                                                                                      llvm::dwarf::DW_ATE_unsigned_char;
                    break;
                case Semantics::PrimitiveType::SignedChar:
                    name = "signed char";
                    encoding = llvm::dwarf::DW_ATE_signed_char;
                    break;
                case Semantics::PrimitiveType::UnsignedChar:
                    name = "unsigned char";
                    encoding = llvm::dwarf::DW_ATE_unsigned_char;
                    break;
                case Semantics::PrimitiveType::Bool:
                    name = "bool";
                    encoding = llvm::dwarf::DW_ATE_boolean;
                    break;
                case Semantics::PrimitiveType::Short:
                    name = "short";
                    encoding = llvm::dwarf::DW_ATE_signed;
                    break;
                case Semantics::PrimitiveType::UnsignedShort:
                    name = "unsigned short";
                    encoding = llvm::dwarf::DW_ATE_unsigned;
                    break;
                case Semantics::PrimitiveType::Int:
                    name = "int";
                    encoding = llvm::dwarf::DW_ATE_signed;
                    break;
                case Semantics::PrimitiveType::UnsignedInt:
                    name = "unsigned int";
                    encoding = llvm::dwarf::DW_ATE_unsigned;
                    break;
                case Semantics::PrimitiveType::Long:
                    name = "long";
                    encoding = llvm::dwarf::DW_ATE_signed;
                    break;
                case Semantics::PrimitiveType::UnsignedLong:
                    name = "unsigned long";
                    encoding = llvm::dwarf::DW_ATE_unsigned;
                    break;
                case Semantics::PrimitiveType::LongLong:
                    name = "long long";
                    encoding = llvm::dwarf::DW_ATE_signed;
                    break;
                case Semantics::PrimitiveType::UnsignedLongLong:
                    name = "unsigned long long";
                    encoding = llvm::dwarf::DW_ATE_unsigned;
                    break;
                case Semantics::PrimitiveType::Float:
                    name = "float";
                    encoding = llvm::dwarf::DW_ATE_float;
                    break;
                case Semantics::PrimitiveType::Double:
                    name = "double";
                    encoding = llvm::dwarf::DW_ATE_float;
                    break;
                case Semantics::PrimitiveType::LongDouble:
                    name = "long double";
                    encoding = llvm::dwarf::DW_ATE_float;
                    break;
                case Semantics::PrimitiveType::Void:
                    name = "void";
                    encoding = llvm::dwarf::DW_ATE_unsigned;
                    break;
                case Semantics::PrimitiveType::Int128:
                    name = "__int128";
                    encoding = llvm::dwarf::DW_ATE_signed;
                    break;
                case Semantics::PrimitiveType::UnsignedInt128:
                    name = "unsigned __int128";
                    encoding = llvm::dwarf::DW_ATE_unsigned;
                    break;
            }
            return m_debugInfo->createBasicType(name, primitive.getBitCount(), encoding);
        },
        [&](const Semantics::PointerType& pointerType) -> llvm::DIType*
        {
            auto* element = visitDebug(pointerType.getElementType());
            auto* pointer =
                m_debugInfo->createPointerType(element, m_programInterface.getLanguageOptions().sizeOfVoidStar);
            if (pointerType.isRestricted())
            {
                pointer = m_debugInfo->createQualifiedType(llvm::dwarf::DW_TAG_restrict_type, pointer);
            }
            return pointer;
        },
        [&](const Semantics::ArrayType& arrayType) -> llvm::DIType*
        {
            auto* element = visitDebug(arrayType.getType());
            return m_debugInfo->createArrayType(
                arrayType.getSizeOf(m_programInterface) * 8, arrayType.getType().getAlignOf(m_programInterface) * 8,
                element,
                m_debugInfo->getOrCreateArray(llvm::DISubrange::get(m_module.getContext(), arrayType.getSize())));
        },
        [&](const Semantics::AbstractArrayType& arrayType) -> llvm::DIType*
        {
            auto* element = visitDebug(arrayType.getType());
            return m_debugInfo->createArrayType(
                arrayType.getType().getSizeOf(m_programInterface) * 8,
                arrayType.getType().getAlignOf(m_programInterface) * 8, element,
                m_debugInfo->getOrCreateArray(llvm::DISubrange::get(m_module.getContext(), 1)));
        },
        [&](const Semantics::ValArrayType&) -> llvm::DIType*
        {
            // TODO:
            CLD_UNREACHABLE;
        },
        [&](const Semantics::VectorType& vectorType) -> llvm::DIType*
        {
            auto* element = visitDebug(vectorType.getType());
            return m_debugInfo->createVectorType(
                vectorType.getType().getSizeOf(m_programInterface) * 8,
                vectorType.getType().getAlignOf(m_programInterface) * 8, element,
                m_debugInfo->getOrCreateArray(llvm::DISubrange::get(m_module.getContext(), vectorType.getSize())));
        },
        [&](const Semantics::StructType& structType) -> llvm::DIType*
        {
            auto result = m_debugTypes.find(structType);
            if (result != m_debugTypes.end())
            {
                return result->second;
            }
            auto* structDef = std::get_if<Semantics::StructDefinition>(&structType.getInfo().type);
            if (!structDef)
            {
                auto* structFwdDecl = m_debugInfo->createForwardDecl(
                    llvm::dwarf::DW_TAG_structure_type, structType.getStructName(),
                    m_scopeIdToScope[structType.getInfo().scope], getFile(structType.getInfo().structToken),
                    getLine(structType.getInfo().structToken));
                m_debugTypes.emplace(structType, structFwdDecl);
                return structFwdDecl;
            }
            auto* structFwdDecl = m_debugInfo->createReplaceableCompositeType(
                llvm::dwarf::DW_TAG_structure_type, structType.getStructName(),
                m_scopeIdToScope[structType.getInfo().scope], getFile(structType.getInfo().structToken),
                getLine(structType.getInfo().structToken));
            m_debugTypes.emplace(structType, structFwdDecl);
            std::vector<llvm::Metadata*> elements;
            for (auto& iter : structDef->getFields())
            {
                auto* subType = visitDebug(*iter.second.type);
                std::size_t offset = 0;
                const Semantics::Type* currentType = &type;
                for (auto index : iter.second.indices)
                {
                    if (Semantics::isStruct(*currentType))
                    {
                        const auto& memoryLayout = Semantics::getMemoryLayout(*currentType);
                        offset += memoryLayout[index].offset;
                        currentType = memoryLayout[index].type;
                    }
                    else
                    {
                        currentType = Semantics::getFieldLayout(*currentType)[index].type;
                    }
                }
                if (!iter.second.bitFieldBounds)
                {
                    elements.push_back(m_debugInfo->createMemberType(
                        structFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                        iter.second.type->getSizeOf(m_programInterface) * 8,
                        iter.second.type->getAlignOf(m_programInterface) * 8, offset * 8,
                        llvm::DINode::DIFlags::FlagZero, subType));
                    continue;
                }
                elements.push_back(m_debugInfo->createBitFieldMemberType(
                    structFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                    iter.second.bitFieldBounds->second - iter.second.bitFieldBounds->first,
                    8 * offset + iter.second.bitFieldBounds->first, 8 * offset, llvm::DINode::DIFlags::FlagZero,
                    subType));
            }
            auto* debugStructDef = llvm::DICompositeType::getDistinct(
                m_module.getContext(), llvm::dwarf::DW_TAG_structure_type, structType.getStructName(),
                getFile(structType.getInfo().structToken), getLine(structType.getInfo().structToken),
                m_scopeIdToScope[structType.getInfo().scope], nullptr, structType.getSizeOf(m_programInterface) * 8,
                structType.getAlignOf(m_programInterface) * 8, 0, llvm::DINode::DIFlags::FlagZero,
                m_debugInfo->getOrCreateArray(elements), 0, nullptr);
            structFwdDecl->replaceAllUsesWith(debugStructDef);
            m_debugTypes.insert_or_assign(structType, debugStructDef);
            return debugStructDef;
        },
        [&](const Semantics::UnionType& unionType) -> llvm::DIType*
        {
            auto result = m_debugTypes.find(unionType);
            if (result != m_debugTypes.end())
            {
                return result->second;
            }
            auto* unionDefinition = std::get_if<Semantics::UnionDefinition>(&unionType.getInfo().type);
            if (!unionDefinition)
            {
                auto* structFwdDecl = m_debugInfo->createForwardDecl(
                    llvm::dwarf::DW_TAG_union_type, unionType.getUnionName(),
                    m_scopeIdToScope[unionType.getInfo().scope], getFile(unionType.getInfo().unionToken),
                    getLine(unionType.getInfo().unionToken));
                m_debugTypes.emplace(unionType, structFwdDecl);
                return structFwdDecl;
            }
            auto* unionFwdDecl = m_debugInfo->createReplaceableCompositeType(
                llvm::dwarf::DW_TAG_union_type, unionType.getUnionName(), m_scopeIdToScope[unionType.getInfo().scope],
                getFile(unionType.getInfo().unionToken), getLine(unionType.getInfo().unionToken));
            m_debugTypes.emplace(unionType, unionFwdDecl);
            std::vector<llvm::Metadata*> elements;
            for (auto& iter : unionDefinition->getFields())
            {
                auto* subType = visitDebug(*iter.second.type);
                std::size_t offset = 0;
                const Semantics::Type* currentType = &type;
                for (auto index : iter.second.indices)
                {
                    if (Semantics::isStruct(*currentType))
                    {
                        const auto& memoryLayout = Semantics::getMemoryLayout(*currentType);
                        offset += memoryLayout[index].offset;
                        currentType = memoryLayout[index].type;
                    }
                    else
                    {
                        currentType = Semantics::getFieldLayout(*currentType)[index].type;
                    }
                }
                if (!iter.second.bitFieldBounds)
                {
                    elements.push_back(m_debugInfo->createMemberType(
                        unionFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                        iter.second.type->getSizeOf(m_programInterface) * 8,
                        iter.second.type->getAlignOf(m_programInterface) * 8, offset * 8,
                        llvm::DINode::DIFlags::FlagZero, subType));
                    continue;
                }
                elements.push_back(m_debugInfo->createBitFieldMemberType(
                    unionFwdDecl, iter.first, getFile(iter.second.nameToken), getLine(iter.second.nameToken),
                    iter.second.bitFieldBounds->second - iter.second.bitFieldBounds->first,
                    8 * offset + iter.second.bitFieldBounds->first, 8 * offset, llvm::DINode::DIFlags::FlagZero,
                    subType));
            }
            auto* debugUnionDef = llvm::DICompositeType::getDistinct(
                m_module.getContext(), llvm::dwarf::DW_TAG_union_type, unionType.getUnionName(),
                getFile(unionType.getInfo().unionToken), getLine(unionType.getInfo().unionToken),
                m_scopeIdToScope[unionType.getInfo().scope], nullptr, unionType.getSizeOf(m_programInterface) * 8,
                unionType.getAlignOf(m_programInterface) * 8, 0, llvm::DINode::DIFlags::FlagZero,
                m_debugInfo->getOrCreateArray(elements), 0, nullptr);
            unionFwdDecl->replaceAllUsesWith(debugUnionDef);
            m_debugTypes.insert_or_assign(unionType, debugUnionDef);
            return debugUnionDef;
        },
        [&](const Semantics::FunctionType& functionType) -> llvm::DIType*
        {
            std::vector<llvm::Metadata*> parameters;
            if (Semantics::isVoid(functionType.getReturnType()))
            {
                parameters.push_back(nullptr);
            }
            else
            {
                parameters.push_back(visitDebug(functionType.getReturnType()));
            }
            for (auto& [type, name] : functionType.getParameters())
            {
                (void)name;
                parameters.push_back(visitDebug(*type));
            }
            return m_debugInfo->createSubroutineType(m_debugInfo->getOrCreateTypeArray(parameters));
        },
        [&](const Semantics::EnumType& enumType) -> llvm::DIType*
        {
            auto result = m_debugTypes.find(enumType);
            if (result != m_debugTypes.end())
            {
                return result->second;
            }
            auto* enumDef = &enumType.getInfo().type;
            CLD_ASSERT(enumDef); // Currently not possible
            std::vector<llvm::Metadata*> enumerators;
            for (auto& [name, value] : enumDef->getValues())
            {
                enumerators.push_back(m_debugInfo->createEnumerator(name, value.getSExtValue(), value.isUnsigned()));
            }
            auto* underlying = visitDebug(enumDef->getType());
            auto* debugEnumDef = m_debugInfo->createEnumerationType(
                m_scopeIdToScope[enumType.getInfo().scope], enumType.getEnumName(),
                getFile(enumType.getInfo().enumToken), getLine(enumType.getInfo().enumToken),
                enumType.getSizeOf(m_programInterface) * 8, enumType.getAlignOf(m_programInterface) * 8,
                m_debugInfo->getOrCreateArray(enumerators), underlying);
            m_debugTypes.insert_or_assign(enumType, debugEnumDef);
            return debugEnumDef;
        });
    if (type.isVolatile())
    {
        result = m_debugInfo->createQualifiedType(llvm::dwarf::DW_TAG_volatile_type, result);
    }
    if (type.isConst())
    {
        result = m_debugInfo->createQualifiedType(llvm::dwarf::DW_TAG_const_type, result);
    }
    auto* info = type.getTypedefInfo();
    if (!info || !info->identifierToken)
    {
        return result;
    }
    return m_debugInfo->createTypedef(result, info->name, getFile(info->identifierToken),
                                      getLine(info->identifierToken), m_scopeIdToScope[info->scope]);
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::TranslationUnit& translationUnit)
{
    for (auto& iter : translationUnit.getGlobals())
    {
        iter->match([&](const Semantics::FunctionDefinition& functionDefinition) { visit(functionDefinition); },
                    [&](const Semantics::VariableDeclaration& declaration)
                    {
                        auto global = visit(declaration);
                        if (llvm::isa_and_nonnull<llvm::GlobalVariable>(global.value))
                        {
                            m_cGlobalVariables.emplace(declaration.getNameToken()->getText(),
                                                       llvm::cast<llvm::GlobalVariable>(global.value));
                        }
                    },
                    [&](const Semantics::FunctionDeclaration& functionDeclaration) { visit(functionDeclaration); },
                    [&](const Semantics::BuiltinFunction&) { CLD_UNREACHABLE; });
    }
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::FunctionDeclaration& declaration)
{
    llvm::Function::LinkageTypes linkageType = llvm::GlobalValue::ExternalLinkage;
    switch (declaration.getLinkage())
    {
        case Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
        case Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
        case Semantics::Linkage::None: break;
    }
    CLD_ASSERT(Semantics::isFunctionType(declaration.getType()));
    if (!m_options.emitAllDecls && !declaration.isUsed() && !declaration.hasAttribute<Semantics::UsedAttribute>())
    {
        return nullptr;
    }
    auto* function = m_module.getFunction(declaration.getNameToken()->getText());
    if (function)
    {
        m_lvalues.emplace(&declaration, valueOf(function));
        return valueOf(function);
    }
    auto* ft = llvm::cast<llvm::FunctionType>(visit(declaration.getType()));
    function =
        llvm::Function::Create(ft, linkageType, -1, llvm::StringRef{declaration.getNameToken()->getText()}, &m_module);
    auto attributes = function->getAttributes();
    attributes = m_abi->generateFunctionAttributes(
        std::move(attributes), ft, declaration.getType().as<Semantics::FunctionType>(), m_programInterface);
    function->setAttributes(std::move(attributes));
    if (declaration.hasAttribute<Semantics::DllImportAttribute>())
    {
        function->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    }
    else if (!m_options.reloc)
    {
        function->setDSOLocal(true);
    }
    m_lvalues.emplace(&declaration, valueOf(function));
    return valueOf(function);
}

cld::CGLLVM::Value cld::CGLLVM::CodeGenerator::visit(const Semantics::VariableDeclaration& declaration)
{
    llvm::Function::LinkageTypes linkageType = llvm::GlobalValue::ExternalLinkage;
    switch (declaration.getLinkage())
    {
        case Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
        case Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
        case Semantics::Linkage::None: break;
    }
    if (declaration.getLifetime() == Semantics::Lifetime::Static)
    {
        if (!m_options.emitAllDecls && declaration.getLinkage() == Semantics::Linkage::Internal && !declaration.isUsed()
            && !declaration.hasAttribute<Semantics::UsedAttribute>())
        {
            return nullptr;
        }
        auto& declType = [&]() -> decltype(auto)
        {
            if (m_currentFunction)
            {
                return declaration.getType();
            }

            auto& decl =
                m_programInterface.getScopes()[0].declarations.at(declaration.getNameToken()->getText()).declared;
            return cld::get<Semantics::VariableDeclaration*>(decl)->getType();
        }();
        auto* type = visit(declType);
        if (Semantics::isAbstractArray(declType))
        {
            type = llvm::ArrayType::get(type->getArrayElementType(), 1);
        }

        auto* prevType = type;
        llvm::Constant* constant = nullptr;
        if (declaration.getInitializer() && declaration.getKind() != Semantics::VariableDeclaration::DeclarationOnly)
        {
            constant = llvm::cast<llvm::Constant>(visit(*declaration.getInitializer(), declType, type).value);
            type = constant->getType();
        }
        else if (declaration.getKind() != Semantics::VariableDeclaration::DeclarationOnly)
        {
            constant = llvm::Constant::getNullValue(type);
        }
        if (m_currentFunction && declaration.getKind() != Semantics::VariableDeclaration::DeclarationOnly)
        {
            linkageType = llvm::GlobalValue::InternalLinkage;
        }
        // TODO: Enable with either -fcommon or common attribute
        //        else if (declaration.getLinkage() != Semantics::Linkage::Internal
        //                 && declaration.getKind() == Semantics::VariableDeclaration::TentativeDefinition)
        //        {
        //            linkageType = llvm::GlobalValue::CommonLinkage;
        //        }

        llvm::GlobalVariable* global = nullptr;
        if (m_currentFunction || m_cGlobalVariables.count(declaration.getNameToken()->getText()) == 0)
        {
            global = new llvm::GlobalVariable(
                m_module, type, declType.isConst() && linkageType != llvm::GlobalValue::CommonLinkage, linkageType,
                constant, llvm::StringRef{declaration.getNameToken()->getText()});
            if (Semantics::isCompleteType(declType) || Semantics::isAbstractArray(declType))
            {
                std::uint64_t alignment = declType.getAlignOf(m_programInterface);
                if (m_triple.getArchitecture() == cld::Architecture::x86_64 && Semantics::isArray(declType)
                    && !Semantics::isAbstractArray(declType) && declType.getSizeOf(m_programInterface) >= 16)
                {
                    alignment = std::max<std::uint64_t>(16, alignment);
                }
                if (auto* aligned = declaration.getAttributeIf<Semantics::AlignedAttribute>())
                {
                    alignment = *aligned->alignment;
                }
                global->setAlignment(llvm::Align(alignment));
            }
            if (!m_options.reloc)
            {
                global->setDSOLocal(true);
            }
        }
        else
        {
            global = m_cGlobalVariables[declaration.getNameToken()->getText()];
            global->setConstant(declType.isConst() && linkageType != llvm::GlobalValue::CommonLinkage);
            global->setLinkage(linkageType);
            global->setInitializer(constant);
        }

        m_lvalues.emplace(&declaration, createBitCast(global, llvm::PointerType::getUnqual(prevType)));
        return global;
    }
    if (!m_options.emitAllDecls && !declaration.isUsed())
    {
        return nullptr;
    }
    auto* type = visit(declaration.getType());
    // Place all allocas up top except variably modified types
    llvm::AllocaInst* var = nullptr;
    if (Semantics::isVariableLengthArray(declaration.getType()))
    {
        if (!m_builder.GetInsertBlock())
        {
            return nullptr;
        }
        llvm::Value* value = nullptr;
        Semantics::RecursiveVisitor visitor(declaration.getType(), Semantics::ARRAY_TYPE_NEXT_FN);
        bool valSeen = false;
        for (auto& iter : visitor)
        {
            if (auto* valArray = iter.tryAs<Semantics::ValArrayType>())
            {
                valSeen = true;
                if (!value)
                {
                    value = m_valSizes[valArray->getExpression()];
                    continue;
                }
                value = m_builder.CreateMul(value, m_valSizes[valArray->getExpression()]);
            }
            else
            {
                if (!value)
                {
                    value = m_builder.getInt64(iter.as<Semantics::ArrayType>().getSize());
                    continue;
                }
                if (!valSeen)
                {
                    value = m_builder.CreateMul(m_builder.getInt64(iter.as<Semantics::ArrayType>().getSize()), value);
                    continue;
                }
                break;
            }
        }

        auto* stackSave = createAllocaAtTop(m_builder.getInt8PtrTy(0), "stack.save");
        m_stackSaves[&declaration] = stackSave;
        auto* stack = m_builder.CreateIntrinsic(llvm::Intrinsic::stacksave, {}, {});
        createStore(stack, stackSave, false);
        var = m_builder.CreateAlloca(type, value, llvm::StringRef{declaration.getNameToken()->getText()});
        var->setAlignment(m_module.getDataLayout().getStackAlignment());
    }
    else
    {
        var = createAllocaAtTop(type, declaration.getNameToken()->getText());
        std::uint64_t alignment = declaration.getType().getAlignOf(m_programInterface);
        if (m_triple.getArchitecture() == cld::Architecture::x86_64 && Semantics::isArray(declaration.getType())
            && declaration.getType().getSizeOf(m_programInterface) >= 16)
        {
            alignment = std::max<std::uint64_t>(alignment, 16);
        }
        if (auto* aligned = declaration.getAttributeIf<Semantics::AlignedAttribute>())
        {
            alignment = *aligned->alignment;
        }
        var->setAlignment(llvm::Align(alignment));
        if (m_options.debugEmission > cld::CGLLVM::DebugEmission::Line)
        {
            auto* local = m_debugInfo->createAutoVariable(
                m_currentDebugScope, declaration.getNameToken()->getText(), getFile(declaration.getNameToken()),
                getLine(declaration.getNameToken()), visitDebug(declaration.getType()), true, llvm::DINode::FlagZero,
                var->getAlign().value() * 8);
            if (m_builder.GetInsertBlock())
            {
                m_debugInfo->insertDeclare(var, local, m_debugInfo->createExpression(),
                                           getLocation(declaration.getNameToken()), m_builder.GetInsertBlock());
            }
        }
    }

    m_lvalues.emplace(&declaration, var);
    if (m_builder.GetInsertBlock())
    {
        if (!Semantics::isVariableLengthArray(declaration.getType()))
        {
            auto* size = m_builder.getInt64(declaration.getType().getSizeOf(m_programInterface));
            m_builder.CreateLifetimeStart(var, llvm::cast<llvm::ConstantInt>(size));
        }
        if (declaration.getInitializer())
        {
            visit(*declaration.getInitializer(), declaration.getType(), var);
        }
    }
    return var;
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::FunctionDefinition& functionDefinition)
{
    if (!m_options.emitAllDecls && functionDefinition.getLinkage() != Semantics::Linkage::External
        && !functionDefinition.isUsed() && !functionDefinition.hasAttribute<Semantics::UsedAttribute>())
    {
        return;
    }
    auto* function = m_module.getFunction(functionDefinition.getNameToken()->getText());
    if (!function)
    {
        llvm::Function::LinkageTypes linkageType;
        switch (functionDefinition.getLinkage())
        {
            case Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
            case Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
            case Semantics::Linkage::None: CLD_UNREACHABLE;
        }
        auto* ft = llvm::cast<llvm::FunctionType>(visit(functionDefinition.getType()));
        function = llvm::Function::Create(ft, linkageType, -1,
                                          llvm::StringRef{functionDefinition.getNameToken()->getText()}, &m_module);
    }
    if (auto* aligned = functionDefinition.getAttributeIf<Semantics::AlignedAttribute>())
    {
        if (auto existingAlign = function->getAlign())
        {
            function->setAlignment(llvm::Align(std::max<std::uint64_t>(existingAlign->value(), *aligned->alignment)));
        }
        else
        {
            function->setAlignment(llvm::Align(*aligned->alignment));
        }
    }
    if (functionDefinition.hasAttribute<Semantics::NoinlineAttribute>())
    {
        function->addFnAttr(llvm::Attribute::NoInline);
    }
    if (functionDefinition.hasAttribute<Semantics::AlwaysInlineAttribute>())
    {
        function->addFnAttr(llvm::Attribute::AlwaysInline);
    }
    m_lvalues.emplace(&functionDefinition, valueOf(function));
    auto& ft = functionDefinition.getType().as<Semantics::FunctionType>();
    auto attributes = function->getAttributes();
    attributes =
        m_abi->generateFunctionAttributes(std::move(attributes), function->getFunctionType(), ft, m_programInterface);
    function->setAttributes(std::move(attributes));
    if (functionDefinition.hasAttribute<Semantics::GnuInlineAttribute>())
    {
        // GNU90 semantics
        if (functionDefinition.getInlineKind() == Semantics::InlineKind::Inline)
        {
            return;
        }
    }
    else
    {
        // C99 semantics
        if (functionDefinition.getInlineKind() == Semantics::InlineKind::InlineDefinition
            && functionDefinition.getLinkage() == Semantics::Linkage::External)
        {
            return;
        }
    }

    if (!m_options.reloc)
    {
        function->setDSOLocal(true);
    }

    auto* bb = llvm::BasicBlock::Create(m_module.getContext(), "entry", function);
    m_builder.SetInsertPoint(bb);
    cld::ScopeExit insertionPointReset{[&]
                                       {
                                           m_builder.ClearInsertionPoint();
                                           m_builder.SetCurrentDebugLocation({});
                                       }};

    m_currentFunction = function;
    cld::ValueReset currentFunctionReset(m_currentFunction, nullptr);

    std::optional<cld::ValueReset<llvm::DIScope*>> subProgramReset;
    if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
    {
        auto tempParams = llvm::MDNode::getTemporary(m_module.getContext(), {});
        auto* subRoutineType = m_debugInfo->createSubroutineType(tempParams.get());
        llvm::DISubprogram::DISPFlags spFlags = llvm::DISubprogram::SPFlagDefinition;
        if (functionDefinition.getNameToken()->getText() == "main")
        {
            spFlags |= llvm::DISubprogram::SPFlagMainSubprogram;
        }
        auto diFlags = ft.isKandR() ? llvm::DINode::FlagZero : llvm::DINode::FlagPrototyped;
        if (functionDefinition.hasAttribute<Semantics::ArtificialAttribute>())
        {
            diFlags |= llvm::DINode::FlagArtificial;
        }
        auto* subProgram = m_debugInfo->createFunction(
            getFile(functionDefinition.getNameToken()), functionDefinition.getNameToken()->getText(),
            functionDefinition.getNameToken()->getText(), getFile(functionDefinition.getNameToken()),
            getLine(functionDefinition.getNameToken()), subRoutineType,
            getLine(functionDefinition.getCompoundStatement().getOpenBrace()), diFlags, spFlags);
        m_currentFunction->setSubprogram(subProgram);
        subProgramReset.emplace(m_currentDebugScope, m_currentDebugScope);
        m_scopeIdToScope[functionDefinition.getCompoundStatement().getScope()] = m_currentDebugScope = subProgram;

        std::vector<llvm::Metadata*> parameters;
        if (m_options.debugEmission > cld::CGLLVM::DebugEmission::Line)
        {
            if (Semantics::isVoid(ft.getReturnType()))
            {
                parameters.push_back(nullptr);
            }
            else
            {
                parameters.push_back(visitDebug(ft.getReturnType()));
            }
            for (auto& [type, name] : ft.getParameters())
            {
                (void)name;
                parameters.push_back(visitDebug(*Semantics::adjustParameterType(*type)));
            }
        }
        auto* typeArray = llvm::MDTuple::get(m_module.getContext(), parameters);
        m_debugInfo->replaceTemporary(std::move(tempParams), typeArray);

        m_builder.SetCurrentDebugLocation({});
    }
    m_abi->generateFunctionEntry(*this, m_currentFunction, ft, functionDefinition.getParameterDeclarations());
    for (auto& [type, name] : ft.getParameters())
    {
        // Go through the visit of each parameter again in case one them of was a variably modified type.
        // The expressions of these could previously not be evaluated as there was no function block to
        // evaluate the expressions nor parameters transferred that's why we're doing it now
        (void)name;
        if (Semantics::isVariablyModified(*type))
        {
            visit(*type);
        }
    }

    visit(functionDefinition.getCompoundStatement());
    if (m_builder.GetInsertBlock())
    {
        if (Semantics::isVoid(ft.getReturnType()))
        {
            m_builder.CreateRetVoid();
        }
        else if (functionDefinition.getNameToken()->getText() == "main"
                 && ft.getReturnType()
                        == Semantics::PrimitiveType(Semantics::PrimitiveType::Int,
                                                    m_programInterface.getLanguageOptions()))
        {
            m_builder.CreateRet(m_builder.getIntN(m_programInterface.getLanguageOptions().sizeOfInt * 8, 0));
        }
        else
        {
            m_builder.CreateUnreachable();
        }
    }
    if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
    {
        m_debugInfo->finalizeSubprogram(m_currentFunction->getSubprogram());
    }
#ifndef NDEBUG
    if (llvm::verifyFunction(*m_currentFunction, &llvm::errs()))
    {
        m_currentFunction->print(llvm::errs(), nullptr, false, true);
        std::terminate();
    }
#endif
}

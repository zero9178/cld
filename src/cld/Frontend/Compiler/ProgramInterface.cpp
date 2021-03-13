
#include "ProgramInterface.hpp"

bool cld::Semantics::ProgramInterface::isCompleteType(const Type& type) const
{
    if (isVoid(type))
    {
        return false;
    }
    if (std::holds_alternative<AbstractArrayType>(type.getVariant()))
    {
        return false;
    }
    if (std::holds_alternative<EnumType>(type.getVariant()))
    {
        auto& enumType = cld::get<EnumType>(type.getVariant());
        return getEnumDefinition(enumType.getId());
    }
    if (std::holds_alternative<StructType>(type.getVariant()))
    {
        auto& structType = cld::get<StructType>(type.getVariant());
        return getStructDefinition(structType.getId());
    }
    if (std::holds_alternative<UnionType>(type.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(type.getVariant());
        return getUnionDefinition(unionType.getId());
    }
    return true;
}

const cld::Semantics::FieldMap&
    cld::Semantics::ProgramInterface::getFields(const cld::Semantics::Type& recordType) const
{
    if (std::holds_alternative<StructType>(recordType.getVariant()))
    {
        auto& structType = cld::get<StructType>(recordType.getVariant());
        auto* structDef = getStructDefinition(structType.getId());
        CLD_ASSERT(structDef);
        return structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(recordType.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(recordType.getVariant());
        auto* unionDef = getUnionDefinition(unionType.getId());
        CLD_ASSERT(unionDef);
        return unionDef->getFields();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::MemoryLayout>
    cld::Semantics::ProgramInterface::getMemoryLayout(const cld::Semantics::Type& structType) const
{
    if (std::holds_alternative<StructType>(structType.getVariant()))
    {
        auto& structTy = cld::get<StructType>(structType.getVariant());
        auto* structDef = getStructDefinition(structTy.getId());
        CLD_ASSERT(structDef);
        return structDef->getMemLayout();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::FieldInLayout>
    cld::Semantics::ProgramInterface::getFieldLayout(const cld::Semantics::Type& recordType) const
{
    if (std::holds_alternative<StructType>(recordType.getVariant()))
    {
        auto& structType = cld::get<StructType>(recordType.getVariant());
        auto* structDef = getStructDefinition(structType.getId());
        CLD_ASSERT(structDef);
        return structDef->getFieldLayout();
    }
    if (std::holds_alternative<UnionType>(recordType.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(recordType.getVariant());
        auto* unionDef = getUnionDefinition(unionType.getId());
        CLD_ASSERT(unionDef);
        return unionDef->getFieldLayout();
    }
    CLD_UNREACHABLE;
}

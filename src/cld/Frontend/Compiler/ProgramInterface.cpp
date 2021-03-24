
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
        return true;
    }
    if (std::holds_alternative<StructType>(type.getVariant()))
    {
        auto& structType = cld::get<StructType>(type.getVariant());
        return std::holds_alternative<StructDefinition>(structType.getInfo().type);
    }
    if (std::holds_alternative<UnionType>(type.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(type.getVariant());
        return std::holds_alternative<UnionDefinition>(unionType.getInfo().type);
    }
    return true;
}

const cld::Semantics::FieldMap&
    cld::Semantics::ProgramInterface::getFields(const cld::Semantics::Type& recordType) const
{
    if (std::holds_alternative<StructType>(recordType.getVariant()))
    {
        auto& structType = cld::get<StructType>(recordType.getVariant());
        return cld::get<StructDefinition>(structType.getInfo().type).getFields();
    }
    if (std::holds_alternative<UnionType>(recordType.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(recordType.getVariant());
        return cld::get<UnionDefinition>(unionType.getInfo().type).getFields();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::MemoryLayout>
    cld::Semantics::ProgramInterface::getMemoryLayout(const cld::Semantics::Type& structType) const
{
    if (std::holds_alternative<StructType>(structType.getVariant()))
    {
        auto& structTy = cld::get<StructType>(structType.getVariant());
        return cld::get<StructDefinition>(structTy.getInfo().type).getMemLayout();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::FieldInLayout>
    cld::Semantics::ProgramInterface::getFieldLayout(const cld::Semantics::Type& recordType) const
{
    if (std::holds_alternative<StructType>(recordType.getVariant()))
    {
        auto& structType = cld::get<StructType>(recordType.getVariant());
        return cld::get<StructDefinition>(structType.getInfo().type).getFieldLayout();
    }
    if (std::holds_alternative<UnionType>(recordType.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(recordType.getVariant());
        return cld::get<UnionDefinition>(unionType.getInfo().type).getFieldLayout();
    }
    CLD_UNREACHABLE;
}

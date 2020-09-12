
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
        return getEnumDefinition(enumType.getName(), enumType.getScopeOrId());
    }
    if (std::holds_alternative<StructType>(type.getVariant()))
    {
        auto& structType = cld::get<StructType>(type.getVariant());
        return getStructDefinition(structType.getName(), structType.getScopeOrId());
    }
    if (std::holds_alternative<UnionType>(type.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(type.getVariant());
        return getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
    }
    return true;
}

cld::Semantics::StructDefinition* cld::Semantics::ProgramInterface::getStructDefinition(std::string_view name,
                                                                                        std::uint64_t scopeOrId,
                                                                                        std::uint64_t* idOut)
{
    return const_cast<StructDefinition*>(
        const_cast<const ProgramInterface*>(this)->getStructDefinition(name, scopeOrId, idOut));
}

const cld::Semantics::StructDefinition*
    cld::Semantics::ProgramInterface::getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                          std::uint64_t* idOut) const
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_structDefinitions[scopeOrId];
    }
    auto* type = lookupType<StructDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_structDefinitions[static_cast<std::uint64_t>(*type)];
}

cld::Semantics::EnumDefinition* cld::Semantics::ProgramInterface::getEnumDefinition(std::string_view name,
                                                                                    std::uint64_t scopeOrId,
                                                                                    std::uint64_t* idOut)
{
    return const_cast<EnumDefinition*>(
        const_cast<const ProgramInterface*>(this)->getEnumDefinition(name, scopeOrId, idOut));
}

const cld::Semantics::EnumDefinition* cld::Semantics::ProgramInterface::getEnumDefinition(std::string_view name,
                                                                                          std::uint64_t scopeOrId,
                                                                                          std::uint64_t* idOut) const
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_enumDefinitions[scopeOrId];
    }
    auto* type = lookupType<EnumDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_enumDefinitions[static_cast<std::uint64_t>(*type)];
}

cld::Semantics::UnionDefinition* cld::Semantics::ProgramInterface::getUnionDefinition(std::string_view name,
                                                                                      std::uint64_t scopeOrId,
                                                                                      std::uint64_t* idOut)
{
    return const_cast<UnionDefinition*>(
        const_cast<const ProgramInterface*>(this)->getUnionDefinition(name, scopeOrId, idOut));
}

const cld::Semantics::UnionDefinition* cld::Semantics::ProgramInterface::getUnionDefinition(std::string_view name,
                                                                                            std::uint64_t scopeOrId,
                                                                                            std::uint64_t* idOut) const
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_unionDefinitions[scopeOrId];
    }
    auto* type = lookupType<UnionDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_unionDefinitions[static_cast<std::uint64_t>(*type)];
}

llvm::ArrayRef<cld::Semantics::Field>
    cld::Semantics::ProgramInterface::getFields(const cld::Semantics::Type& recordType) const
{
    if (std::holds_alternative<AnonymousUnionType>(recordType.getVariant()))
    {
        return cld::get<AnonymousUnionType>(recordType.getVariant()).getFields();
    }
    if (std::holds_alternative<AnonymousStructType>(recordType.getVariant()))
    {
        return cld::get<AnonymousStructType>(recordType.getVariant()).getFields();
    }
    if (std::holds_alternative<StructType>(recordType.getVariant()))
    {
        auto& structType = cld::get<StructType>(recordType.getVariant());
        auto* structDef = getStructDefinition(structType.getName(), structType.getScopeOrId());
        CLD_ASSERT(structDef);
        return structDef->getFields();
    }
    if (std::holds_alternative<UnionType>(recordType.getVariant()))
    {
        auto& unionType = cld::get<UnionType>(recordType.getVariant());
        auto* unionDef = getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
        CLD_ASSERT(unionDef);
        return unionDef->getFields();
    }
    CLD_UNREACHABLE;
}

bool cld::Semantics::ProgramInterface::isBitfieldAccess(const Expression& expression) const
{
    if (!std::holds_alternative<MemberAccess>(expression.getVariant()))
    {
        return false;
    }
    auto& mem = cld::get<MemberAccess>(expression.getVariant());
    auto& expr = mem.getRecordExpression();
    auto& recordType = std::holds_alternative<PointerType>(expr.getType().getVariant()) ?
                           cld::get<PointerType>(expr.getType().getVariant()).getElementType() :
                           expr.getType();
    auto fields = getFields(recordType);
    return static_cast<bool>(fields[mem.getMemberIndex()].bitFieldBounds);
}


#include "ProgramInterface.hpp"

bool cld::Semantics::ProgramInterface::isCompleteType(const Type& type) const
{
    if (isVoid(type))
    {
        return false;
    }
    if (std::holds_alternative<AbstractArrayType>(type.get()))
    {
        return false;
    }
    if (std::holds_alternative<EnumType>(type.get()))
    {
        auto& enumType = cld::get<EnumType>(type.get());
        return getEnumDefinition(enumType.getName(), enumType.getScopeOrId());
    }
    if (std::holds_alternative<StructType>(type.get()))
    {
        auto& structType = cld::get<StructType>(type.get());
        return getStructDefinition(structType.getName(), structType.getScopeOrId());
    }
    if (std::holds_alternative<UnionType>(type.get()))
    {
        auto& unionType = cld::get<UnionType>(type.get());
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

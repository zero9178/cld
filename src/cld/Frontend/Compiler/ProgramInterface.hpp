
#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <unordered_map>

#include <tsl/ordered_map.h>

#include "ConstValue.hpp"
#include "Semantics.hpp"

namespace cld::Semantics
{
class ProgramInterface
{
public:
    enum class StructTag : std::size_t
    {
    };
    enum class UnionTag : std::size_t
    {
    };
    enum class EnumTag : std::size_t
    {
    };

    struct DeclarationInScope
    {
        const Lexer::CToken* CLD_NULLABLE identifier; // Guaranteed to be non null if the scope isn't global and the
                                                      // declaration isn't a builtin variable like __func__
        using Variant = std::variant<Declaration * CLD_NON_NULL, FunctionDefinition * CLD_NON_NULL,
                                     BuiltinFunction * CLD_NON_NULL, Type, std::pair<ConstValue, Type>>;
        Variant declared;
    };

    struct TagTypeInScope
    {
        const Lexer::CToken* CLD_NULLABLE identifier; // Guaranteed to be non null if the scope isn't global
        using Variant = std::variant<StructTag, UnionTag, EnumTag>;
        Variant tagType;
    };

    struct Scope
    {
        std::int64_t previousScope;
        std::vector<std::int64_t> subScopes;
        tsl::ordered_map<std::string_view, DeclarationInScope> declarations;
        std::unordered_map<std::string_view, TagTypeInScope> types;
    };

    struct StructDecl
    {
    };

    struct UnionDecl
    {
    };

protected:
    std::vector<Scope> m_scopes = {Scope{-1, {}, {}, {}}};
    std::vector<std::variant<StructDefinition, StructDecl>> m_structDefinitions;
    std::vector<std::variant<UnionDefinition, UnionDecl>> m_unionDefinitions;
    std::vector<EnumDefinition> m_enumDefinitions;
    std::unordered_map<std::string_view, BuiltinFunction> m_usedBuiltins;

public:
    ProgramInterface() = default;

    virtual ~ProgramInterface() = default;
    ProgramInterface(const ProgramInterface&) = delete;
    ProgramInterface& operator=(const ProgramInterface&) = delete;

    ProgramInterface(ProgramInterface&&)
#if !defined(_MSC_VER) || defined(__clang__)
        noexcept
#endif
        = default;

    ProgramInterface& operator=(ProgramInterface&&)
#if !defined(_MSC_VER) || defined(__clang__)
        noexcept
#endif
        = default;

    [[nodiscard]] bool isCompleteType(const Type& type) const;

    [[nodiscard]] virtual const LanguageOptions& getLanguageOptions() const = 0;

    template <class T>
    [[nodiscard]] const T* CLD_NULLABLE lookupType(std::string_view name, std::int64_t scope) const
    {
        auto curr = scope;
        while (curr >= 0)
        {
            auto result = m_scopes[curr].types.find(name);
            if (result != m_scopes[curr].types.end())
            {
                if (auto* ptr = std::get_if<T>(&result->second.tagType))
                {
                    return ptr;
                }
            }
            curr = m_scopes[curr].previousScope;
        }
        return nullptr;
    }

    StructDefinition* CLD_NULLABLE getStructDefinition(std::uint64_t id)
    {
        return std::get_if<StructDefinition>(&m_structDefinitions[id]);
    }

    const StructDefinition* CLD_NULLABLE getStructDefinition(std::uint64_t id) const
    {
        return std::get_if<StructDefinition>(&m_structDefinitions[id]);
    }

    EnumDefinition* CLD_NULLABLE getEnumDefinition(std::uint64_t id)
    {
        return &m_enumDefinitions[id];
    }

    const EnumDefinition* CLD_NULLABLE getEnumDefinition(std::uint64_t id) const
    {
        return &m_enumDefinitions[id];
    }

    UnionDefinition* CLD_NULLABLE getUnionDefinition(std::uint64_t id)
    {
        return std::get_if<UnionDefinition>(&m_unionDefinitions[id]);
    }

    const UnionDefinition* CLD_NULLABLE getUnionDefinition(std::uint64_t id) const
    {
        return std::get_if<UnionDefinition>(&m_unionDefinitions[id]);
    }

    const FieldMap& getFields(const Type& recordType) const;

    llvm::ArrayRef<MemoryLayout> getMemoryLayout(const Type& structType) const;

    llvm::ArrayRef<FieldInLayout> getFieldLayout(const Type& recordType) const;

    bool isBitfieldAccess(const ExpressionBase& expression) const;

    const std::vector<Scope>& getScopes() const
    {
        return m_scopes;
    }
};

} // namespace cld::Semantics

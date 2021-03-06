
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
        using Variant = std::variant<VariableDeclaration * CLD_NON_NULL, FunctionDefinition * CLD_NON_NULL,
                                     FunctionDeclaration * CLD_NON_NULL, BuiltinFunction * CLD_NON_NULL, Type,
                                     std::pair<ConstValue, Type>>;
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
        std::size_t previousScope;
        std::vector<std::size_t> subScopes;
        tsl::ordered_map<std::string_view, DeclarationInScope> declarations;
        std::unordered_map<std::string_view, TagTypeInScope> types;
    };

    struct StructDecl
    {
    };

    struct UnionDecl
    {
    };

    struct StructInfo
    {
        std::variant<StructDefinition, StructDecl> type;
        std::size_t scope;
        const Lexer::CToken* structToken;
    };

    struct UnionInfo
    {
        std::variant<UnionDefinition, UnionDecl> type;
        std::size_t scope;
        const Lexer::CToken* unionToken;
    };

    struct EnumInfo
    {
        EnumDefinition type;
        std::size_t scope;
        const Lexer::CToken* enumToken;
    };

protected:
    std::vector<Scope> m_scopes = {Scope{static_cast<std::size_t>(-1), {}, {}, {}}};
    std::vector<StructInfo> m_structDefinitions;
    std::vector<UnionInfo> m_unionDefinitions;
    std::vector<EnumInfo> m_enumDefinitions;
    std::unordered_map<std::string_view, BuiltinFunction> m_usedBuiltins;

public:
    ProgramInterface() = default;

    virtual ~ProgramInterface() = default;
    ProgramInterface(const ProgramInterface&) = delete;
    ProgramInterface& operator=(const ProgramInterface&) = delete;

    ProgramInterface(ProgramInterface&&)
#if !defined(_MSC_VER) || defined(__clang__) || _MSC_VER >= 1928
        noexcept
#endif
        = default;

    ProgramInterface& operator=(ProgramInterface&&)
#if !defined(_MSC_VER) || defined(__clang__) || _MSC_VER >= 1928
        noexcept
#endif
        = default;

    [[nodiscard]] bool isCompleteType(const Type& type) const;

    [[nodiscard]] virtual const LanguageOptions& getLanguageOptions() const = 0;

    template <class T>
    [[nodiscard]] const T* CLD_NULLABLE lookupType(std::string_view name, std::size_t scope) const
    {
        auto curr = scope;
        while (curr != static_cast<std::size_t>(-1))
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

    StructDefinition* CLD_NULLABLE getStructDefinition(std::size_t id)
    {
        return std::get_if<StructDefinition>(&m_structDefinitions[id].type);
    }

    const StructDefinition* CLD_NULLABLE getStructDefinition(std::size_t id) const
    {
        return std::get_if<StructDefinition>(&m_structDefinitions[id].type);
    }

    std::size_t getStructScope(std::size_t id) const
    {
        return m_structDefinitions[id].scope;
    }

    const Lexer::CToken* CLD_NULLABLE getStructLoc(std::size_t id) const
    {
        return m_structDefinitions[id].structToken;
    }

    EnumDefinition* CLD_NULLABLE getEnumDefinition(std::size_t id)
    {
        return &m_enumDefinitions[id].type;
    }

    const EnumDefinition* CLD_NULLABLE getEnumDefinition(std::size_t id) const
    {
        return &m_enumDefinitions[id].type;
    }

    std::size_t getEnumScope(std::size_t id) const
    {
        return m_enumDefinitions[id].scope;
    }

    const Lexer::CToken* CLD_NULLABLE getEnumLoc(std::size_t id) const
    {
        return m_enumDefinitions[id].enumToken;
    }

    UnionDefinition* CLD_NULLABLE getUnionDefinition(std::size_t id)
    {
        return std::get_if<UnionDefinition>(&m_unionDefinitions[id].type);
    }

    const UnionDefinition* CLD_NULLABLE getUnionDefinition(std::size_t id) const
    {
        return std::get_if<UnionDefinition>(&m_unionDefinitions[id].type);
    }

    std::size_t getUnionScope(std::size_t id) const
    {
        return m_unionDefinitions[id].scope;
    }

    const Lexer::CToken* CLD_NULLABLE getUnionLoc(std::size_t id) const
    {
        return m_unionDefinitions[id].unionToken;
    }

    const FieldMap& getFields(const Type& recordType) const;

    llvm::ArrayRef<MemoryLayout> getMemoryLayout(const Type& structType) const;

    llvm::ArrayRef<FieldInLayout> getFieldLayout(const Type& recordType) const;

    const std::vector<Scope>& getScopes() const
    {
        return m_scopes;
    }
};

} // namespace cld::Semantics

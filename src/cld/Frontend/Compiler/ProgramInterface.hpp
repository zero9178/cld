
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
    enum class StructDefTag : std::size_t
    {
    };
    enum class UnionDefTag : std::size_t
    {
    };
    enum class EnumDefTag : std::size_t
    {
    };

    struct DeclarationInScope
    {
        const Lexer::CToken* CLD_NULLABLE identifier; // Guaranteed to be non null if the scope isn't global
        using Variant = std::variant<const Declaration * CLD_NON_NULL, FunctionDefinition * CLD_NON_NULL,
                                     const BuiltinFunction * CLD_NON_NULL, Type, std::pair<ConstValue, Type>>;
        Variant declared;
    };

    struct TagTypeInScope
    {
        const Lexer::CToken* CLD_NULLABLE identifier; // Guaranteed to be non null if the scope isn't global
        struct StructDecl
        {
        };
        struct UnionDecl
        {
        };
        using Variant = std::variant<StructDecl, UnionDecl, StructDefTag, UnionDefTag, EnumDefTag>;
        Variant tagType;
    };

    struct Scope
    {
        std::int64_t previousScope;
        std::vector<std::int64_t> subScopes;
        tsl::ordered_map<std::string_view, DeclarationInScope> declarations;
        std::unordered_map<std::string_view, TagTypeInScope> types;
    };

protected:
    std::vector<Scope> m_scopes = {Scope{-1, {}, {}, {}}};
    std::vector<StructDefinition> m_structDefinitions;
    std::vector<UnionDefinition> m_unionDefinitions;
    std::vector<EnumDefinition> m_enumDefinitions;

public:
    ProgramInterface() = default;

    virtual ~ProgramInterface() = default;
    ProgramInterface(const ProgramInterface&) = delete;
    ProgramInterface& operator=(const ProgramInterface&) = delete;
    ProgramInterface(ProgramInterface&&) noexcept = default;
    ProgramInterface& operator=(ProgramInterface&&) noexcept = default;

    [[nodiscard]] bool isCompleteType(const Type& type) const;

    [[nodiscard]] virtual const LanguageOptions& getLanguageOptions() const = 0;

    constexpr static std::uint64_t IS_SCOPE = 1ull << 63;
    constexpr static std::uint64_t SCOPE_OR_ID_MASK = ~(1ull << 63);

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

    StructDefinition* CLD_NULLABLE getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                       std::uint64_t* idOut = nullptr);

    const StructDefinition* CLD_NULLABLE getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                             std::uint64_t* idOut = nullptr) const;

    EnumDefinition* CLD_NULLABLE getEnumDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                   std::uint64_t* idOut = nullptr);

    const EnumDefinition* CLD_NULLABLE getEnumDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                         std::uint64_t* idOut = nullptr) const;

    UnionDefinition* CLD_NULLABLE getUnionDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                     std::uint64_t* idOut = nullptr);

    const UnionDefinition* CLD_NULLABLE getUnionDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                           std::uint64_t* idOut = nullptr) const;

    const FieldMap& getFields(const Type& recordType) const;

    llvm::ArrayRef<Type> getLayout(const Type& structType) const;

    bool isBitfieldAccess(const Expression& expression) const;

    const std::vector<Scope>& getScopes() const
    {
        return m_scopes;
    }
};

} // namespace cld::Semantics

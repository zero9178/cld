
#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <cld/Support/IntrVarAllocator.hpp>

#include <deque>
#include <unordered_map>

#include <tsl/ordered_map.h>

#include "ConstValue.hpp"
#include "SemanticUtil.hpp"
#include "Semantics.hpp"

namespace cld::Semantics
{
struct StructDecl
{
};

struct UnionDecl
{
};

struct StructInfo : public AttributeHolder<TypeAttribute>
{
    std::size_t id;
    std::variant<StructDefinition, StructDecl> type;
    std::size_t scope;
    const Lexer::CToken* structToken;
    std::string_view name;

    StructInfo(std::size_t id, std::variant<StructDefinition, StructDecl> type, std::size_t scope,
               const Lexer::CToken* structToken, std::string_view name = "")
        : id(id), type(std::move(type)), scope(scope), structToken(structToken), name(name)
    {
    }
};

struct UnionInfo : public AttributeHolder<TypeAttribute>
{
    std::size_t id;
    std::variant<UnionDefinition, UnionDecl> type;
    std::size_t scope;
    const Lexer::CToken* unionToken;
    std::string_view name;

    UnionInfo(std::size_t id, std::variant<UnionDefinition, UnionDecl> type, std::size_t scope,
              const Lexer::CToken* unionToken, std::string_view name = "")
        : id(id), type(std::move(type)), scope(scope), unionToken(unionToken), name(name)
    {
    }
};

struct EnumInfo : public AttributeHolder<TypeAttribute>
{
    std::size_t id;
    EnumDefinition type;
    std::size_t scope;
    const Lexer::CToken* enumToken;
    std::string_view name;

    EnumInfo(std::size_t id, EnumDefinition type, std::size_t scope, const Lexer::CToken* enumToken,
             std::string_view name = "")
        : id(id), type(std::move(type)), scope(scope), enumToken(enumToken), name(name)
    {
    }
};

struct TypedefInfo : public AttributeHolder<TypeAttribute>
{
    std::string_view name;
    IntrVarValue<Type> type;
    std::size_t scope;
    bool isConst : 1;
    bool isVolatile : 1;
    bool isRestricted : 1;
    const Lexer::CToken* CLD_NULLABLE identifierToken; // nullptr if builtin

    TypedefInfo(std::string_view name, IntrVarValue<Type> type, std::size_t scope,
                const Lexer::CToken* identifierToken = nullptr)
        : name(name),
          type(std::move(type)),
          scope(scope),
          isConst(this->type->isConst()),
          isVolatile(this->type->isVolatile()),
          isRestricted(this->type->isRestricted()),
          identifierToken(identifierToken)
    {
    }
};

class ProgramInterface
{
public:
    struct DeclarationInScope
    {
        const Lexer::CToken* CLD_NULLABLE identifier; // Guaranteed to be non null if the scope isn't global and the
                                                      // declaration isn't a builtin variable like __func__
        using Variant = std::variant<VariableDeclaration * CLD_NON_NULL, FunctionDefinition * CLD_NON_NULL,
                                     FunctionDeclaration * CLD_NON_NULL, BuiltinFunction * CLD_NON_NULL,
                                     TypedefInfo * CLD_NON_NULL, std::pair<ConstValue, IntrVarValue<Type>>>;
        Variant declared;
    };

    struct TagTypeInScope
    {
        const Lexer::CToken* CLD_NULLABLE identifier; // Guaranteed to be non null if not a builtin
        using Variant = std::variant<StructInfo*, UnionInfo*, EnumInfo*>;
        Variant tagType;
    };

    struct Scope
    {
        std::size_t previousScope;
        std::vector<std::size_t> subScopes;
        tsl::ordered_map<std::string_view, DeclarationInScope> declarations;
        std::unordered_map<std::string_view, TagTypeInScope> types;
    };

    constexpr static std::size_t END_OF_SCOPES = static_cast<std::size_t>(-1);
    constexpr static std::size_t GLOBAL_SCOPE = 0;

    cld::IntrVarAllocator<Type> m_typeAlloc;

protected:
    std::vector<Scope> m_scopes = {Scope{END_OF_SCOPES, {}, {}, {}}};
    std::deque<StructInfo> m_structDefinitions;
    std::deque<UnionInfo> m_unionDefinitions;
    std::deque<EnumInfo> m_enumDefinitions;
    std::deque<TypedefInfo> m_typedefDefinitions;
    std::unordered_map<std::string_view, BuiltinFunction> m_usedBuiltins;

    [[nodiscard]] cld::not_null<Type> typeAlloc(const Type& value)
    {
        return m_typeAlloc.alloc(value);
    }

    [[nodiscard]] cld::not_null<Type> typeAlloc(Type&& value)
    {
        return m_typeAlloc.alloc(std::move(value));
    }

    template <class U, class... Args>
    [[nodiscard]] cld::not_null<U> typeAlloc(Args&&... args)
    {
        return m_typeAlloc.alloc<U>(std::forward<Args>(args)...);
    }

    auto scopeIterator(std::size_t scope) const
    {
        return RecursiveVisitor(m_scopes[scope],
                                [this](const Scope& scope) -> const Scope*
                                {
                                    if (scope.previousScope == END_OF_SCOPES)
                                    {
                                        return nullptr;
                                    }
                                    return &m_scopes[scope.previousScope];
                                });
    }

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

    [[nodiscard]] virtual const LanguageOptions& getLanguageOptions() const = 0;

    template <class T>
    [[nodiscard]] const T* CLD_NULLABLE lookupType(std::string_view name, std::size_t scope) const
    {
        for (auto& iter : scopeIterator(scope))
        {
            auto result = iter.types.find(name);
            if (result == iter.types.end())
            {
                continue;
            }
            if (auto* ptr = std::get_if<T*>(&result->second.tagType))
            {
                return *ptr;
            }
        }
        return nullptr;
    }

    template <class T>
    [[nodiscard]] T* CLD_NULLABLE lookupType(std::string_view name, std::size_t scope)
    {
        return const_cast<T*>(static_cast<const ProgramInterface*>(this)->lookupType<T>(name, scope));
    }

    const std::vector<Scope>& getScopes() const
    {
        return m_scopes;
    }
};

} // namespace cld::Semantics

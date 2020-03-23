#pragma once

#pragma warning(push, 0)
#include <llvm/Support/Casting.h>
#pragma warning(pop)

#include <map>

#include "LanguageOptions.hpp"
#include "Lexer.hpp"

namespace cld
{
class SourceObject
{
    std::vector<std::uint64_t> m_starts;
    LanguageOptions m_languageOptions = LanguageOptions::native(LanguageOptions::C99);

protected:
    std::vector<Lexer::Token> m_tokens;

    SourceObject(const SourceObject&) = default;

    SourceObject& operator=(const SourceObject&) = default;

public:
    SourceObject() = default;

    explicit SourceObject(std::vector<std::uint64_t> starts, std::vector<Lexer::Token> tokens,
                          LanguageOptions languageOptions = LanguageOptions::native(LanguageOptions::C99));

    virtual ~SourceObject() = default;

    SourceObject(SourceObject&&) = default;

    SourceObject& operator=(SourceObject&&) = default;

    [[nodiscard]] std::uint64_t getLineNumber(std::uint64_t offset) const noexcept;

    [[nodiscard]] std::uint64_t getLineStartOffset(std::uint64_t line) const noexcept;

    [[nodiscard]] std::uint64_t getLineEndOffset(std::uint64_t line) const noexcept;

    [[nodiscard]] const std::vector<Lexer::Token>& data() const;

    [[nodiscard]] LanguageOptions getLanguageOptions() const;

    [[nodiscard]] virtual bool isPreprocessed() const;
};

class PPSourceObject final : public SourceObject
{
public:
    struct Substitution
    {
        std::vector<Lexer::Token> identifier;
        std::vector<Lexer::Token> replacedTokens;
    };
    using SubstitutionMap = std::map<std::uint64_t, Substitution>;

private:
    SubstitutionMap m_substitutions;
    std::vector<std::uint64_t> m_starts;

public:
    PPSourceObject(const SourceObject& sourceObject, std::vector<Lexer::Token> tokens,
                   const SubstitutionMap& substitutions, const std::vector<std::uint64_t>& ppstarts);

    PPSourceObject(const PPSourceObject&) = delete;

    PPSourceObject& operator=(const PPSourceObject&) = delete;

    PPSourceObject(PPSourceObject&&) = default;

    PPSourceObject& operator=(PPSourceObject&&) = default;

    [[nodiscard]] bool isPreprocessed() const override;

    [[nodiscard]] std::uint64_t getPPLineNumber(std::uint64_t offset) const noexcept;

    [[nodiscard]] std::uint64_t getPPLineStartOffset(std::uint64_t line) const noexcept;

    [[nodiscard]] std::uint64_t getPPLineEndOffset(std::uint64_t line) const noexcept;
};
} // namespace cld

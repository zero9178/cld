#pragma once

#include <unordered_map>

#include "LanguageOptions.hpp"
#include "Lexer.hpp"

namespace cld
{
class SourceObject
{
    std::string m_source;
    std::vector<std::uint64_t> m_starts;
    LanguageOptions m_languageOptions = LanguageOptions::native(LanguageOptions::C99);

protected:
    std::vector<Lexer::Token> m_tokens;

    SourceObject(const SourceObject& rhs);

    SourceObject& operator=(const SourceObject& rhs);

public:
    SourceObject() = default;

    explicit SourceObject(std::string source, std::vector<std::uint64_t> starts, std::vector<Lexer::Token> tokens,
                          LanguageOptions languageOptions = LanguageOptions::native(LanguageOptions::C99));

    virtual ~SourceObject() = default;

    SourceObject(SourceObject&& rhs) noexcept;

    SourceObject& operator=(SourceObject&& rhs) noexcept;

    [[nodiscard]] std::uint64_t getLineNumber(std::uint64_t offset) const noexcept;

    [[nodiscard]] std::uint64_t getLineStartOffset(std::uint64_t line) const noexcept;

    [[nodiscard]] std::uint64_t getLineEndOffset(std::uint64_t line) const noexcept;

    [[nodiscard]] const std::vector<Lexer::Token>& data() const;

    [[nodiscard]] const LanguageOptions& getLanguageOptions() const;

    [[nodiscard]] virtual bool isPreprocessed() const;

    [[nodiscard]] const std::string& getSource() const;
};

class PPSourceObject final : public SourceObject
{
public:
    struct Substitution
    {
        Lexer::Token identifier;
    };
    using SubstitutionMap = std::unordered_map<std::uint64_t, Substitution>;

private:
    SubstitutionMap m_substitutions;
    std::vector<std::uint64_t> m_starts;

public:
    PPSourceObject(const SourceObject& sourceObject, std::vector<Lexer::Token> tokens = {},
                   SubstitutionMap substitutions = {}, std::vector<uint64_t> ppstarts = {});

    PPSourceObject(const PPSourceObject&) = delete;

    PPSourceObject& operator=(const PPSourceObject&) = delete;

    PPSourceObject(PPSourceObject&&) = default;

    PPSourceObject& operator=(PPSourceObject&&) = default;

    [[nodiscard]] bool isPreprocessed() const override;

    [[nodiscard]] std::uint64_t getPPLineNumber(std::uint64_t offset) const noexcept;

    [[nodiscard]] std::uint64_t getPPLineStartOffset(std::uint64_t line) const noexcept;

    [[nodiscard]] std::uint64_t getPPLineEndOffset(std::uint64_t line) const noexcept;

    const SubstitutionMap& getSubstitutions() const;
};
} // namespace cld

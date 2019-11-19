#ifndef OPENCLPARSER_SOURCEOBJECT_HPP
#define OPENCLPARSER_SOURCEOBJECT_HPP

#include <map>

#include "LanguageOptions.hpp"
#include "Lexer.hpp"

namespace OpenCL
{
    class SourceObject final
    {
    public:
        struct Substitution
        {
            std::vector<Lexer::Token> define;
            std::vector<Lexer::Token> replacedTokens;
        };
        using SubstitutionMap = std::map<std::pair<std::uint64_t, std::uint64_t>, Substitution>;

    private:
        std::vector<std::uint64_t> m_starts;
        std::vector<Lexer::Token> m_tokens;
        LanguageOptions m_languageOptions = LanguageOptions::native(LanguageOptions::C99);
        SubstitutionMap m_substitutions;

    public:
        SourceObject() = default;

        explicit SourceObject(std::vector<std::uint64_t> starts, std::vector<Lexer::Token> tokens,
                              LanguageOptions languageOptions = LanguageOptions::native(LanguageOptions::C99),
                              SubstitutionMap substitutions = {});

        std::uint64_t getLineNumber(std::uint64_t offset) const noexcept;

        std::uint64_t getLineStartOffset(std::uint64_t line) const noexcept;

        std::uint64_t getLineEndOffset(std::uint64_t line) const noexcept;

        [[nodiscard]] const std::vector<Lexer::Token>& data() const;

        LanguageOptions getLanguageOptions() const;
    };
} // namespace OpenCL

#endif // OPENCLPARSER_SOURCEOBJECT_HPP

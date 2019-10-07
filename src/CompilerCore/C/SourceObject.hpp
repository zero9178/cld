#ifndef OPENCLPARSER_SOURCEOBJECT_HPP
#define OPENCLPARSER_SOURCEOBJECT_HPP

#include <map>

#include "Lexer.hpp"

namespace OpenCL
{
    enum class Language
    {
        C,
        Preprocessor,
        OpenCL
    };

    class SourceObject final
    {
        std::vector<Lexer::Token> m_tokens;
        std::vector<std::pair<std::vector<Lexer::Token>::const_iterator, std::vector<Lexer::Token>::const_iterator>>
            m_lines;
        Language m_language;

        void constructLineMap();

    public:
        using value_type = Lexer::Token;
        using reference = Lexer::Token&;
        using const_reference = const Lexer::Token&;
        using iterator = std::vector<Lexer::Token>::const_iterator;
        using const_iterator = std::vector<Lexer::Token>::const_iterator;
        using difference_type = std::ptrdiff_t;
        using size_type = std::size_t;

        explicit SourceObject(std::vector<Lexer::Token> tokens, Language language = Language::C);

        SourceObject(const SourceObject& sourceObject);

        SourceObject& operator=(const SourceObject& sourceObject);

        SourceObject(SourceObject&& sourceObject) noexcept;

        SourceObject& operator=(SourceObject&& sourceObject) noexcept;

        [[nodiscard]] const_iterator getLineStart(const_iterator iter) const;

        [[nodiscard]] const_iterator getLineEnd(const_iterator iter) const;

        [[nodiscard]] const std::vector<Lexer::Token>& data() const;

        [[nodiscard]] iterator begin() const;

        [[nodiscard]] iterator end() const;

        [[nodiscard]] const_iterator cbegin() const;

        [[nodiscard]] const_iterator cend() const;

        Language getLanguage() const;
    };
} // namespace OpenCL

#endif // OPENCLPARSER_SOURCEOBJECT_HPP

#ifndef OPENCLPARSER_SOURCEOBJECT_HPP
#define OPENCLPARSER_SOURCEOBJECT_HPP

#include <map>

#include "Lexer.hpp"

namespace OpenCL
{
    class SourceObject final
    {
        std::vector<Lexer::Token> m_tokens;
        std::vector<std::pair<std::vector<Lexer::Token>::const_iterator, std::vector<Lexer::Token>::const_iterator>>
            m_lines;

        void constructLineMap();

    public:
        explicit SourceObject(std::vector<Lexer::Token> tokens);

        SourceObject(const SourceObject& sourceObject);

        SourceObject& operator=(const SourceObject& sourceObject);

        SourceObject(SourceObject&& sourceObject) noexcept;

        SourceObject& operator=(SourceObject&& sourceObject) noexcept;

        [[nodiscard]] std::vector<Lexer::Token>::const_iterator
            getLineStart(std::vector<Lexer::Token>::const_iterator) const;

        [[nodiscard]] std::vector<Lexer::Token>::const_iterator
            getLineEnd(std::vector<Lexer::Token>::const_iterator) const;

        [[nodiscard]] const std::vector<Lexer::Token>& data() const;

        [[nodiscard]] std::vector<Lexer::Token>::const_iterator begin() const;

        [[nodiscard]] std::vector<Lexer::Token>::const_iterator end() const;

        [[nodiscard]] std::vector<Lexer::Token>::const_iterator cbegin() const;

        [[nodiscard]] std::vector<Lexer::Token>::const_iterator cend() const;
    };
} // namespace OpenCL

#endif // OPENCLPARSER_SOURCEOBJECT_HPP

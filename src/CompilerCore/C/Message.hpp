#ifndef OPENCLPARSER_MESSAGE_HPP
#define OPENCLPARSER_MESSAGE_HPP

#include <string>
#include <vector>
#include <optional>
#include <iostream>
#include "Lexer.hpp"

namespace OpenCL
{
    class Modifier
    {

        std::vector<Lexer::Token>::const_iterator m_begin;
        std::vector<Lexer::Token>::const_iterator m_end;

    public:

        enum Action
        {
            Underline,
            PointAtBeginning,
            PointAtEnd
        };

    private:

        Action m_action;

    public:

        Modifier(const std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                 const std::vector<OpenCL::Lexer::Token>::const_iterator& anEnd,
                 Action action);

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        Action getAction() const;

        friend std::ostream& operator<<(std::ostream& os, const Modifier& modifier);
    };

    std::ostream& operator<<(std::ostream& os, const Modifier& modifier);

    class Message
    {
        std::string m_message;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_begin;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_end;
        std::optional<Modifier> m_modifier;

    public:

        struct Note
        {
            std::string message;
            std::vector<OpenCL::Lexer::Token>::const_iterator begin;
            std::vector<OpenCL::Lexer::Token>::const_iterator end;
            std::optional<Modifier> modifier;
        };

    private:

        std::vector<Note> m_notes;

    public:

        enum Severity
        {
            Error,
            Warning
        };

        Message(std::string message,
                std::vector<Lexer::Token>::const_iterator begin = {},
                std::vector<Lexer::Token>::const_iterator end = {},
                std::optional<Modifier> modifier = {},
                std::vector<Note> notes = {});

        const std::string& getMessage() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        const std::vector<Note>& getNotes() const;

        const std::optional<Modifier>& getModifier() const;
    };

    std::ostream& operator<<(std::ostream& os, const Message& message);

    std::vector<Lexer::Token>::const_iterator findEOL(std::vector<Lexer::Token>::const_iterator begin,
                                                      std::vector<Lexer::Token>::const_iterator end);

    std::vector<Lexer::Token>::const_iterator findSemicolon(std::vector<Lexer::Token>::const_iterator begin,
                                                            std::vector<Lexer::Token>::const_iterator end);

    std::vector<Lexer::Token>::const_iterator findSemicolonOrEOL(std::vector<Lexer::Token>::const_iterator begin,
                                                                 std::vector<Lexer::Token>::const_iterator end);
}

#endif //OPENCLPARSER_MESSAGE_HPP

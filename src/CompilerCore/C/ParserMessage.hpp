#ifndef OPENCLPARSER_PARSERMESSAGE_HPP
#define OPENCLPARSER_PARSERMESSAGE_HPP

#include <string>
#include <vector>
#include <iostream>
#include "Lexer.hpp"

namespace OpenCL::Parser
{
    class ParserMessage
    {
        std::string m_message;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_begin;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_end;

    public:

        struct Note
        {
            std::string message;
            std::vector<OpenCL::Lexer::Token>::const_iterator begin;
            std::vector<OpenCL::Lexer::Token>::const_iterator end;
        };

    private:

        std::vector<Note> m_notes;

    public:

        enum Severity
        {
            Error,
            Warning
        };

        ParserMessage(std::string message,
                      std::vector<Lexer::Token>::const_iterator begin = {},
                      std::vector<Lexer::Token>::const_iterator end = {},
                      std::vector<Note> notes = {});

        const std::string& getMessage() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        const std::vector<Note>& getNotes() const;

        friend std::ostream& operator<<(std::ostream& ostream, const ParserMessage& message);
    };

    std::ostream& operator<<(std::ostream& ostream, const ParserMessage& message);
}

#endif //OPENCLPARSER_PARSERMESSAGE_HPP

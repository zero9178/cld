#include "ParserMessage.hpp"

#include <utility>

OpenCL::Parser::ParserMessage::ParserMessage(std::string message,
                                             std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             std::vector<Note> notes)
    : m_message(std::move(message)), m_begin(begin), m_end(end), m_notes(std::move(notes))
{}

std::ostream& OpenCL::Parser::operator<<(std::ostream& ostream, const ParserMessage& message)
{
    return ostream;
}

const std::string& OpenCL::Parser::ParserMessage::getMessage() const
{
    return m_message;
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Parser::ParserMessage::getBegin() const
{
    return m_begin;
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Parser::ParserMessage::getAnEnd() const
{
    return m_end;
}

const std::vector<OpenCL::Parser::ParserMessage::Note>& OpenCL::Parser::ParserMessage::getNotes() const
{
    return m_notes;
}

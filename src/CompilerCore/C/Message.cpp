#include "Message.hpp"
#include <utility>
#include <optional>
#include "rang.hpp"

OpenCL::Message::Message(std::string message,
                         std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end,
                         std::optional<Modifier> modifier,
                         std::vector<Note> notes)
    : m_message(std::move(message)), m_begin(begin), m_end(end), m_modifier(modifier), m_notes(std::move(notes))
{

}

std::ostream& OpenCL::operator<<(std::ostream& os, const Message& message)
{
    os << rang::fg::reset;
    if (message.getBegin() != message.getAnEnd())
    {
        os << message.getBegin()->getLine() << ':' << message.getBegin()->getColumn() << ": ";
    }
    os << rang::fg::red << rang::style::bold << "error: " << rang::fg::blue << rang::style::reset
       << message.getMessage() << '\n';

    auto numSize = std::to_string(message.getAnEnd()->getLine()).size();
    auto remainder = numSize % 4;
    if(remainder != 0)
    {
        numSize += 4 - remainder;
    }
    for (auto curr = message.getBegin(); curr != message.getAnEnd();)
    {
        auto next = findEOL(curr, message.getAnEnd());
        auto text = Lexer::reconstruct(curr, next);
        auto line = std::to_string(curr->getLine());
        os << std::string(numSize - line.size(),' ') << line << '|';
        if(message.getModifier() && message.getModifier()->getBegin()->getLine() == curr->getLine())
        {
            auto highlitedEOL = findEOL(message.getModifier()->getBegin(),message.getModifier()->getAnEnd());
            os << text.substr(0,message.getModifier()->getBegin()->getColumn()) << rang::fg::red ;
        }
        else
        {
            os << text << '\n';
        }
        curr = next;
    }

    for (auto&[noteMessage, noteBegin, noteEnd, noteModifiers] : message.getNotes())
    {
        if (noteBegin != noteEnd)
        {
            os << rang::fg::blue;
            os << noteBegin->getLine() << ':' << noteBegin->getColumn() << ": ";
        }
        os << rang::fg::cyan << "note: " << noteMessage << '\n';
    }
    return os << std::flush;
}

const std::string& OpenCL::Message::getMessage() const
{
    return m_message;
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Message::getBegin() const
{
    return m_begin;
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Message::getAnEnd() const
{
    return m_end;
}

const std::vector<OpenCL::Message::Note>& OpenCL::Message::getNotes() const
{
    return m_notes;
}

const std::optional<OpenCL::Modifier>& OpenCL::Message::getModifier() const
{
    return m_modifier;
}

OpenCL::Modifier::Modifier(const std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                           const std::vector<OpenCL::Lexer::Token>::const_iterator& anEnd,
                           OpenCL::Modifier::Action action) : m_begin(begin), m_end(anEnd), m_action(action)
{}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Modifier::getBegin() const
{
    return m_begin;
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Modifier::getAnEnd() const
{
    return m_end;
}

OpenCL::Modifier::Action OpenCL::Modifier::getAction() const
{
    return m_action;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::findEOL(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                                                  std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    for (auto curr = begin; curr != end; curr++)
    {
        if (curr->getLine() != begin->getLine())
        {
            return curr;
        }
    }
    return end;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::findSemicolon(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                                                        std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    for (auto curr = begin; curr != end; curr++)
    {
        if (curr->getTokenType() == Lexer::TokenType::SemiColon)
        {
            return curr;
        }
    }
    return end;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::findSemicolonOrEOL(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                                                             std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    for (auto curr = begin; curr != end; curr++)
    {
        if (curr->getTokenType() == Lexer::TokenType::SemiColon || curr->getLine() != begin->getLine())
        {
            return curr;
        }
    }
    return end;
}

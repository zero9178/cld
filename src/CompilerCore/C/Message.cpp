#include "Message.hpp"
#include <utility>
#include <optional>
#include "termcolor.hpp"

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
    #ifdef NDEBUG
    auto normalColour = termcolor::white;
    #else
    auto normalColour = termcolor::grey;
    #endif
    if (message.getBegin() != message.getAnEnd())
    {
        os << normalColour << message.getBegin()->getLine() << ':' << message.getBegin()->getColumn() << ": ";
    }
    os << termcolor::red << "error: " << normalColour << message.getMessage() << '\n';

    auto numSize = std::to_string(message.getAnEnd()->getLine()).size();
    auto remainder = numSize % 4;
    if (remainder != 0)
    {
        numSize += 4 - remainder;
    }
    auto modifierBegin = message.getModifier() ? message.getModifier()->getBegin()
                                               : std::vector<Lexer::Token>::const_iterator{};
    for (auto curr = message.getBegin(); curr != message.getAnEnd();)
    {
        auto next = findEOL(curr, message.getAnEnd());
        auto text = Lexer::reconstruct(curr, next);
        auto line = std::to_string(curr->getLine());
        os << normalColour << std::string(numSize - line.size(), ' ') << line << '|';
        if (message.getModifier() && modifierBegin->getLine() == curr->getLine())
        {
            auto highlitedEOL = findEOL(modifierBegin, message.getModifier()->getAnEnd());
            auto highlitedSectionLength = highlitedEOL->getColumn() - modifierBegin->getColumn();
            os << text.substr(0, modifierBegin->getColumn()) << termcolor::red
               << text.substr(modifierBegin->getColumn(), highlitedSectionLength)
               << normalColour << text.substr(highlitedEOL->getColumn()) << '\n';
            os << normalColour << std::string(numSize,' ')<<'|'<<std::string(modifierBegin->getColumn(),' ');
            os << termcolor::red;
            switch(message.getModifier()->getAction())
            {
            case Modifier::Underline:
                os <<std::string(highlitedSectionLength, '~');
                break;
            case Modifier::PointAtBeginning:
                os <<'^'<<std::string(highlitedSectionLength-1, '~');
                break;
            case Modifier::PointAtEnd:
                os <<std::string(highlitedSectionLength-1, '~')<<'^';
                break;
            }
            os << normalColour;
            modifierBegin = highlitedEOL;
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
            os << normalColour << noteBegin->getLine() << ':' << noteBegin->getColumn() << ": ";
        }
        os << termcolor::cyan << "note: " << normalColour << noteMessage << '\n';
    }
    return os << termcolor::reset << std::flush;
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
            return curr + 1;
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
            return curr + 1;
        }
    }
    return end;
}

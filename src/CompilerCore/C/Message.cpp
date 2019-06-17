#include "Message.hpp"
#include <utility>
#include <regex>
#include "termcolor.hpp"

OpenCL::Message::Message(std::string message,
                         std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end,
                         std::optional<Modifier> modifier,
                         std::vector<Note> notes)
    : m_message(std::move(message)), m_begin(begin), m_end(end), m_modifier(std::move(modifier)),
      m_notes(std::move(notes))
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

    auto numSize = message.getBegin() != message.getAnEnd() ? std::to_string((message.getAnEnd() - 1)->getLine()).size()
                                                            : 0;
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
            auto highlightedEOL = findEOL(modifierBegin, message.getModifier()->getAnEnd());
            switch (message.getModifier()->getAction())
            {
            case Modifier::Underline:
            case Modifier::PointAtBeginning:
            case Modifier::PointAtEnd:
            {
                auto highlightedSectionLength = (highlightedEOL - 1)->getColumn() + (highlightedEOL - 1)->getLength()
                    - modifierBegin->getColumn();
                os << text.substr(0, modifierBegin->getColumn() - curr->getColumn()) << termcolor::red
                   << text.substr(modifierBegin->getColumn() - curr->getColumn(), highlightedSectionLength)
                   << normalColour
                   << text.substr(modifierBegin->getColumn() - curr->getColumn() + highlightedSectionLength)
                   << '\n';

                os << normalColour << std::string(numSize, ' ') << '|'
                   << std::string(modifierBegin->getColumn() - curr->getColumn(), ' ');
                os << termcolor::red;
                switch (message.getModifier()->getAction())
                {
                case Modifier::Underline:os << std::string(highlightedSectionLength, '~');
                    break;
                case Modifier::PointAtBeginning:os << '^' << std::string(highlightedSectionLength - 1, '~');
                    break;
                case Modifier::PointAtEnd:os << std::string(highlightedSectionLength - 1, '~') << '^';
                    break;
                default:break;
                }
                os << normalColour << '\n';
                break;
            }
            default:
            {
                if (modifierBegin + 1 != highlightedEOL)
                {
                    throw std::runtime_error("End must be one higher than begin when using in between actions");
                }
                auto start = modifierBegin->getColumn() - curr->getColumn() + modifierBegin->getLength();
                if (start >= text.size() || std::isspace(text[start]))
                {
                    os << text << '\n';
                }
                else
                {
                    os << text.substr(0, start)
                       << termcolor::red << text.substr(start, (modifierBegin + 1)->getLength()) << normalColour
                       << text.substr(start + (modifierBegin + 1)->getLength()) << '\n';
                }

                os << std::string(numSize, ' ') << '|' << std::string(start, ' ') << termcolor::red;
                switch (message.getModifier()->getAction())
                {
                case Modifier::InsertAtEnd:os << '^';
                    if (start < text.size() && !std::isspace(text[start]))
                    {
                        os << std::string(
                            highlightedEOL->getColumn() - modifierBegin->getColumn() + modifierBegin->getLength() - 1,
                            '~');
                    }
                    else if (start < text.size())
                    {
                        os << std::string(modifierBegin->getLength() - 1, '~');
                    }
                    break;
                default:break;
                }
                os << normalColour << '\n';

                if (!message.getModifier()->getActionArgument().empty())
                {
                    os << std::string(numSize, ' ') << '|' << std::string(start, ' ') << termcolor::red
                       << message.getModifier()->getActionArgument() << normalColour << '\n';
                }
            }
            }
            modifierBegin = highlightedEOL;
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
    return os << termcolor::reset << std::endl;
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

OpenCL::Modifier::Modifier(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator anEnd,
                           OpenCL::Modifier::Action action, std::string actionArgument)
    : m_begin(begin), m_end(anEnd), m_action(action), m_actionArgument(std::move(actionArgument))
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

const std::string& OpenCL::Modifier::getActionArgument() const
{
    return m_actionArgument;
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

std::string OpenCL::Format::format(std::vector<std::string> args) const
{
    static std::regex brackets(R"(\\*\{\})");
    std::reverse(args.begin(), args.end());
    std::smatch matches;
    std::string result = m_format;
    auto start = result.cbegin();
    while (start < result.cend() && std::regex_search(start, result.cend(), matches, brackets))
    {
        auto size = matches.suffix().first - result.cbegin()
            - (matches[0].second - matches[0].first) + 1;
        auto pos = matches[0].str().find_first_not_of('\\');
        if (pos != std::string::npos && pos % 2 == 1)
        {
            start = result.cbegin() + size;
            continue;
        }
        if (args.empty())
        {
            throw std::runtime_error("Not enough arguments specified to substitute in format");
        }
        result = std::string(result.cbegin(), matches[0].first + pos) + args.back() +
            std::string(matches[0].second, result.cend());
        args.pop_back();
        start = result.cbegin() + size;
    }
    if (!args.empty())
    {
        throw std::runtime_error("More arguments specified than needed to substitute");
    }
    return result;
}

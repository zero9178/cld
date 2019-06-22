#include "Message.hpp"
#include <utility>
#include <regex>
#include "termcolor.hpp"
#include "Parser.hpp"

OpenCL::Message::Message(std::string message,
                         std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end,
                         std::optional<Modifier> modifier,
                         std::vector<Note> notes)
    : m_message(std::move(message)), m_begin(begin), m_end(end), m_modifier(std::move(modifier)),
      m_notes(std::move(notes))
{

}

namespace
{
    void renderSection(std::ostream& os,
                       const std::string& message,
                       std::ostream& (& colour)(std::ostream&),
                       const std::string& prefix,
                       OpenCL::Parser::Tokens::const_iterator begin,
                       OpenCL::Parser::Tokens::const_iterator end,
                       const std::optional<OpenCL::Modifier>& modifier)
    {
        #ifdef NDEBUG
        auto normalColour = termcolor::white;
        #else
        auto normalColour = termcolor::grey;
        #endif

        if (begin != end)
        {
            os << normalColour << begin->getLine() << ':' << begin->getColumn() << ": ";
            if (modifier && modifier->getAnEnd() > end)
            {
                throw std::runtime_error("Trying to apply action to text not rendered");
            }
        }
        os << colour << prefix << normalColour << message << '\n';

        auto numSize = begin != end ? std::to_string((end - 1)->getLine()).size() : 0;
        auto remainder = numSize % 4;
        if (remainder != 0)
        {
            numSize += 4 - remainder;
        }
        auto modifierBegin = modifier ? modifier->getBegin()
                                      : std::vector<OpenCL::Lexer::Token>::const_iterator{};
        for (auto curr = begin; curr != end;)
        {
            auto next = OpenCL::findEOL(curr, end);
            auto text = OpenCL::Lexer::reconstruct(curr, next);
            auto line = std::to_string(curr->getLine());
            os << normalColour << std::string(numSize - line.size(), ' ') << line << '|';
            if (modifier && modifierBegin->getLine() == curr->getLine())
            {
                auto highlightedEOL = OpenCL::findEOL(modifierBegin, modifier->getAnEnd());
                switch (modifier->getAction())
                {
                case OpenCL::Modifier::Underline:
                case OpenCL::Modifier::PointAtBeginning:
                case OpenCL::Modifier::PointAtEnd:
                {
                    auto
                        highlightedSectionLength = (highlightedEOL - 1)->getColumn() + (highlightedEOL - 1)->getLength()
                        - modifierBegin->getColumn();
                    os << text.substr(0, modifierBegin->getColumn() - curr->getColumn()) << colour
                       << text.substr(modifierBegin->getColumn() - curr->getColumn(), highlightedSectionLength)
                       << normalColour
                       << text.substr(modifierBegin->getColumn() - curr->getColumn() + highlightedSectionLength)
                       << '\n';

                    os << normalColour << std::string(numSize, ' ') << '|'
                       << std::string(modifierBegin->getColumn() - curr->getColumn(), ' ');
                    os << colour;
                    switch (modifier->getAction())
                    {
                    case OpenCL::Modifier::Underline:os << std::string(highlightedSectionLength, '~');
                        break;
                    case OpenCL::Modifier::PointAtBeginning:os << '^' << std::string(highlightedSectionLength - 1, '~');
                        break;
                    case OpenCL::Modifier::PointAtEnd:os << std::string(highlightedSectionLength - 1, '~') << '^';
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
                           << colour << text.substr(start, (modifierBegin + 1)->getLength()) << normalColour
                           << text.substr(start + (modifierBegin + 1)->getLength()) << '\n';
                    }

                    os << std::string(numSize, ' ') << '|' << std::string(start, ' ') << colour;
                    switch (modifier->getAction())
                    {
                    case OpenCL::Modifier::InsertAtEnd:os << '^';
                        if (start < text.size() && !std::isspace(text[start]))
                        {
                            os << std::string(
                                highlightedEOL->getColumn() - modifierBegin->getColumn() + modifierBegin->getLength()
                                    - 1,
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

                    if (!modifier->getActionArgument().empty())
                    {
                        os << std::string(numSize, ' ') << '|' << std::string(start, ' ') << colour
                           << modifier->getActionArgument() << normalColour << '\n';
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
    }
}

std::ostream& OpenCL::operator<<(std::ostream& os, const Message& message)
{
    renderSection(os,
                  message.getMessage(),
                  termcolor::red,
                  "error: ",
                  message.getBegin(),
                  message.getAnEnd(),
                  message.getModifier());

    for (auto&[noteMessage, noteBegin, noteEnd, noteModifiers] : message.getNotes())
    {
        renderSection(os, noteMessage, termcolor::cyan, "note: ", noteBegin, noteEnd, noteModifiers);
    }
    return os;
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

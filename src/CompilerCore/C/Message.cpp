#include "Message.hpp"

#include <llvm/Support/Format.h>

#include <CompilerCore/Common/Util.hpp>

#include <regex>
#include <utility>

#include "Parser.hpp"

OpenCL::Message::Message(Severity severity, std::string message, std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end, std::optional<Modifier> modifier)
    : m_modifier(std::move(modifier)), m_message(std::move(message)), m_begin(begin), m_end(end), m_severity(severity)
{
}

namespace
{
    void renderSection(llvm::raw_ostream& os, const std::string& message, llvm::raw_ostream::Colors colour,
                       const std::string& prefix, OpenCL::SourceObject::const_iterator begin,
                       OpenCL::SourceObject::const_iterator end, const std::optional<OpenCL::Modifier>& modifier)
    {
        auto normalColour = llvm::raw_ostream::Colors::WHITE;

        std::size_t sideOffset = std::numeric_limits<std::size_t>::max();
        if (begin != end)
        {
            os << normalColour << begin->getLine() << ':' << begin->getColumn() << ": ";
            if (modifier && modifier->getAnEnd() > end)
            {
                llvm::errs() << "Trying to apply action to text not rendered\n";
                std::terminate();
            }

            for (auto curr = begin; curr != end;)
            {
                auto next = OpenCL::findEOL(curr, end);
                sideOffset = std::min(sideOffset, curr->getColumn());
                curr = next;
            }
        }
        os << colour << prefix << normalColour << message << '\n';

        auto numSize = begin != end ? std::to_string((end - 1)->getLine()).size() : 0;
        auto remainder = numSize % 4;
        if (remainder != 0)
        {
            numSize += 4 - remainder;
        }
        auto modifierBegin = modifier ? modifier->getBegin() : std::vector<OpenCL::Lexer::Token>::const_iterator{};
        for (auto curr = begin; curr != end;)
        {
            auto next = OpenCL::findEOL(curr, end);
            auto text = OpenCL::Lexer::reconstructTrimmed(curr, next);
            os << normalColour << llvm::format_decimal(curr->getLine(), numSize) << '|'
               << std::string(curr->getColumn() - sideOffset, ' ');
            if (modifier && modifierBegin != modifier->getAnEnd() && modifierBegin->getLine() == curr->getLine())
            {
                auto highlightedEOL = OpenCL::findEOL(modifierBegin, modifier->getAnEnd());
                switch (modifier->getAction())
                {
                    case OpenCL::Modifier::Underline:
                    case OpenCL::Modifier::PointAtBeginning:
                    case OpenCL::Modifier::PointAtEnd:
                    {
                        auto highlightedSectionLength = (highlightedEOL - 1)->getColumn()
                                                        + (highlightedEOL - 1)->getLength()
                                                        - modifierBegin->getColumn();
                        os << text.substr(0, modifierBegin->getColumn() - curr->getColumn()) << colour
                           << text.substr(modifierBegin->getColumn() - curr->getColumn(), highlightedSectionLength)
                           << normalColour;
                        if (modifierBegin->getColumn() - curr->getColumn() + highlightedSectionLength <= text.size())
                        {
                            os << text.substr(modifierBegin->getColumn() - curr->getColumn()
                                              + highlightedSectionLength);
                        }
                        os << '\n';

                        os << normalColour << std::string(numSize, ' ') << '|'
                           << std::string(
                                  modifierBegin->getColumn() - curr->getColumn() + curr->getColumn() - sideOffset, ' ');
                        os << colour;
                        switch (modifier->getAction())
                        {
                            case OpenCL::Modifier::Underline: os << std::string(highlightedSectionLength, '~'); break;
                            case OpenCL::Modifier::PointAtBeginning:
                                os << '^' << std::string(highlightedSectionLength - 1, '~');
                                break;
                            case OpenCL::Modifier::PointAtEnd:
                                os << std::string(highlightedSectionLength - 1, '~') << '^';
                                break;
                            default: break;
                        }
                        os << normalColour << '\n';
                        break;
                    }
                    default:
                    {
                        if (modifierBegin + 1 != highlightedEOL)
                        {
                            llvm::errs() << "End must be one higher than begin when using in between actions\n";
                            std::terminate();
                        }
                        auto start = modifierBegin->getColumn() - curr->getColumn() + modifierBegin->getLength();
                        if (start >= text.size() || std::isspace(text[start]))
                        {
                            os << text << '\n';
                        }
                        else
                        {
                            os << text.substr(0, start) << colour
                               << text.substr(start, (modifierBegin + 1)->getLength()) << normalColour
                               << text.substr(start + (modifierBegin + 1)->getLength()) << '\n';
                        }

                        os << std::string(numSize, ' ') << '|'
                           << std::string(start + curr->getColumn() - sideOffset, ' ') << colour;
                        switch (modifier->getAction())
                        {
                            case OpenCL::Modifier::InsertAtEnd:
                                os << '^';
                                if (start < text.size() && !std::isspace(text[start]))
                                {
                                    os << std::string(highlightedEOL->getColumn() - modifierBegin->getColumn()
                                                          + modifierBegin->getLength() - 1,
                                                      '~');
                                }
                                else if (start < text.size())
                                {
                                    os << std::string(modifierBegin->getLength() - 1, '~');
                                }
                                break;
                            default: break;
                        }
                        os << normalColour << '\n';

                        if (!modifier->getActionArgument().empty())
                        {
                            os << std::string(numSize, ' ') << '|'
                               << std::string(start + curr->getColumn() - sideOffset, ' ') << colour
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
} // namespace

llvm::raw_ostream& OpenCL::operator<<(llvm::raw_ostream& os, const Message& message)
{
    auto [colour, prefix] = [&message]() -> std::pair<llvm::raw_ostream::Colors, std::string> {
        switch (message.getSeverity())
        {
            case Message::Error: return {llvm::raw_ostream::RED, "error: "};
            case Message::Note: return {llvm::raw_ostream::CYAN, "note: "};
            case Message::Warning: return {llvm::raw_ostream::MAGENTA, "warning: "};
            default: OPENCL_UNREACHABLE;
        }
    }();
    renderSection(os, message.getMessage(), colour, prefix, message.getBegin(), message.getAnEnd(),
                  message.getModifier());
    os.flush();
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

const std::optional<OpenCL::Modifier>& OpenCL::Message::getModifier() const
{
    return m_modifier;
}

OpenCL::Message::Severity OpenCL::Message::getSeverity() const
{
    return m_severity;
}

OpenCL::Message OpenCL::Message::error(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                       std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                       std::optional<Modifier> modifier)
{
    return OpenCL::Message(Error, std::move(message), begin, end, std::move(modifier));
}

OpenCL::Message
    OpenCL::Message::note(std::string message,
                          std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator begin,
                          std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator end,
                          std::optional<Modifier> modifier)
{
    return OpenCL::Message(Note, std::move(message), begin, end, std::move(modifier));
}

OpenCL::Message OpenCL::Message::warning(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         std::optional<Modifier> modifier)
{
    return OpenCL::Message(Warning, std::move(message), begin, end, std::move(modifier));
}

OpenCL::Modifier::Modifier(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator anEnd, OpenCL::Modifier::Action action,
                           std::string actionArgument)
    : m_actionArgument(std::move(actionArgument)), m_begin(begin), m_end(anEnd), m_action(action)
{
}

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

std::vector<OpenCL::Lexer::Token>::const_iterator
    OpenCL::findEOL(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
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

std::string OpenCL::Format::format(std::vector<std::string> args) const
{
    static std::regex brackets(R"(\\*\{\})");
    std::reverse(args.begin(), args.end());
    std::smatch matches;
    std::string result = m_format;
    auto start = result.cbegin();
    while (start < result.cend() && std::regex_search(start, result.cend(), matches, brackets))
    {
        auto size = matches.suffix().first - result.cbegin() - (matches[0].second - matches[0].first) + 1;
        auto pos = matches[0].str().find_first_not_of('\\');
        if (pos != std::string::npos && pos % 2 == 1)
        {
            start = result.cbegin() + size;
            continue;
        }
        if (args.empty())
        {
            llvm::errs() << "Not enough arguments specified to substitute in format";
            std::terminate();
        }
        result = std::string(result.cbegin(), matches[0].first + pos) + args.back()
                 + std::string(matches[0].second, result.cend());
        start = result.cbegin() + args.back().size() + (size - 1);
        args.pop_back();
    }
    if (!args.empty())
    {
        llvm::errs() << "More arguments specified than needed to substitute";
        std::terminate();
    }
    return result;
}

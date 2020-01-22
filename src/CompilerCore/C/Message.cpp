#include "Message.hpp"

#pragma warning(push, 0)
#include <llvm/Support/Format.h>
#include <llvm/Support/WithColor.h>
#pragma warning(pop)

#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Util.hpp>

#include <regex>
#include <utility>

OpenCL::Message::Message(Severity severity, std::string message, std::vector<Lexer::Token>::const_iterator begin,
                         std::optional<std::vector<OpenCL::Lexer::Token>::const_iterator> rangeEnd,
                         std::optional<Modifier> modifier)
    : m_modifier(std::move(modifier)),
      m_message(std::move(message)),
      m_begin(begin),
      m_rangeEnd(rangeEnd),
      m_severity(severity)
{
}

OpenCL::Message::Severity OpenCL::Message::getSeverity() const
{
    return m_severity;
}

OpenCL::Message OpenCL::Message::error(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator token,
                                       std::optional<Modifier> modifier)
{
    return OpenCL::Message(Error, std::move(message), token, {}, std::move(modifier));
}

OpenCL::Message
    OpenCL::Message::note(std::string message,
                          std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator token,
                          std::optional<Modifier> modifier)
{
    return OpenCL::Message(Note, std::move(message), token, {}, std::move(modifier));
}

OpenCL::Message OpenCL::Message::warning(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator token,
                                         std::optional<Modifier> modifier)
{
    return OpenCL::Message(Warning, std::move(message), token, {}, std::move(modifier));
}

namespace
{
    llvm::raw_ostream& operator<<(llvm::raw_ostream& os, const std::string_view& sv)
    {
        os.write(sv.data(), sv.size());
        return os;
    }
} // namespace

llvm::raw_ostream& OpenCL::Message::print(llvm::raw_ostream& os, const OpenCL::SourceObject& sourceObject) const
{
    auto [colour, prefix] = [this]() -> std::pair<llvm::raw_ostream::Colors, std::string> {
        switch (getSeverity())
        {
            case Message::Error: return {llvm::raw_ostream::RED, "error: "};
            case Message::Note: return {llvm::raw_ostream::CYAN, "note: "};
            case Message::Warning: return {llvm::raw_ostream::MAGENTA, "warning: "};
            default: OPENCL_UNREACHABLE;
        }
    }();

    const auto begin = [&sourceObject, this] {
        const auto line = sourceObject.getLineNumber(m_begin->getOffset());
        const auto start = sourceObject.getLineStartOffset(line);
        return std::find_if(
                   std::make_reverse_iterator(m_begin), std::make_reverse_iterator(sourceObject.data().begin()),
                   [start](const Lexer::Token& token) { return token.getOffset() + token.getLength() < start; })
            .base();
    }();
    const auto end = [&sourceObject, this] {
        const auto line =
            sourceObject.getLineNumber(m_rangeEnd ? m_rangeEnd.value()->getOffset() : m_begin->getOffset());
        const auto end = sourceObject.getLineEndOffset(line);
        return std::find_if(m_rangeEnd.value_or(m_begin), sourceObject.data().end(),
                            [end](const Lexer::Token& token) { return token.getOffset() > end; });
    }();

    auto text = OpenCL::Lexer::reconstructTrimmed(sourceObject, begin, end);
    if (begin != m_begin
        && sourceObject.getLineNumber(begin->getOffset())
               != sourceObject.getLineNumber(begin->getOffset() + begin->getLength()))
    {
        // The very first token that was used to reconstruct is multiline. Our text needs trimming
        text = text.substr(text.find('\n') + 1);
    }

    if ((!m_rangeEnd || *m_rangeEnd != end)
        && sourceObject.getLineNumber((end - 1)->getOffset())
               != sourceObject.getLineNumber((end - 1)->getOffset() + (end - 1)->getLength()))
    {
        text = text.substr(0, text.rfind('\n'));
    }

    const auto locationLine = sourceObject.getLineNumber(m_begin->getOffset());
    os << locationLine << ':' << locationLine - sourceObject.getLineStartOffset(locationLine) << ": ";
    llvm::WithColor(os, colour) << prefix;
    os << m_message << '\n';

    auto numSize = std::to_string(sourceObject.getLineNumber((end - 1)->getOffset() + (end - 1)->getLength())).size();
    const auto remainder = numSize % 4;
    if (remainder)
    {
        numSize += 4 - remainder;
    }

    std::vector<std::string_view> lines;
    {
        std::size_t pos = 0;
        do
        {
            auto result = text.find('\n', pos);
            lines.emplace_back(text.data() + pos, result != std::string::npos ? pos + result : text.size());
            pos = result != std::string::npos ? result + 1 : result;

        } while (pos != std::string::npos);
    }

    const auto beginLine = sourceObject.getLineNumber(m_begin->getOffset());
    std::vector<std::optional<std::pair<std::uint64_t, std::uint64_t>>> underlined(lines.size());
    if (m_modifier)
    {
        auto underLineBegin = m_modifier->begin()->getOffset();
        const auto underLineEnd = (m_modifier->end() - 1)->getOffset() + (m_modifier->end() - 1)->getLength();
        for (std::size_t i = sourceObject.getLineNumber(underLineBegin); i <= sourceObject.getLineNumber(underLineEnd);
             i++)
        {
            assert(i - beginLine < underlined.size());
            underlined[i - beginLine] = std::pair{std::max(underLineBegin, sourceObject.getLineStartOffset(i)),
                                                  (std::min(underLineEnd, sourceObject.getLineEndOffset(i)))};
        }
    }
    for (std::size_t i = 0; i < lines.size(); i++)
    {
        os << llvm::format_decimal(beginLine + i, numSize) << '|';
        if (!m_modifier)
        {
            os << lines[i] << '\n';
            continue;
        }
        const auto lineStart = sourceObject.getLineStartOffset(i + beginLine);
        os << lines[i].substr(0, underlined[i]->first - lineStart);
        llvm::WithColor(os, colour).get()
            << lines[i].substr(underlined[i]->first - lineStart, underlined[i]->second - underlined[i]->first);
        os << lines[i].substr(underlined[i]->second - lineStart);
        os << '\n';

        os.indent(numSize) << '|';
        switch (m_modifier->getAction())
        {
            case Modifier::Underline:
            {
                os.indent(underlined[i]->first - lineStart);
                llvm::WithColor(os, colour).get() << std::string(underlined[i]->second - underlined[i]->first, '~');
                break;
            }
            case Modifier::PointAtBeginning:
            {
                os.indent(underlined[i]->first - lineStart);
                llvm::WithColor(os, colour).get()
                    << '^' << std::string(underlined[i]->second - underlined[i]->first - 1, '~');
                break;
            }
            case Modifier::PointAtEnd:
            {
                os.indent(underlined[i]->first - lineStart);
                llvm::WithColor(os, colour).get()
                    << std::string(underlined[i]->second - underlined[i]->first - 1, '~') << '^';
                break;
            }
            case Modifier::InsertAtEnd: break;
        }
        os << '\n';
    }

    os.flush();
    return os;
}

OpenCL::Message OpenCL::Message::error(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                       std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                       std::optional<Modifier> modifier)
{
    return OpenCL::Message(Error, std::move(message), begin, end, modifier);
}

OpenCL::Message OpenCL::Message::warning(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         std::optional<Modifier> modifier)
{
    return OpenCL::Message(Warning, std::move(message), begin, end, modifier);
}

OpenCL::Message OpenCL::Message::note(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                      std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                      std::optional<Modifier> modifier)
{
    return OpenCL::Message(Note, std::move(message), begin, end, modifier);
}

OpenCL::Modifier::Modifier(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator anEnd, OpenCL::Modifier::Action action,
                           std::string actionArgument)
    : m_actionArgument(std::move(actionArgument)), m_begin(begin), m_end(anEnd), m_action(action)
{
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::Modifier::begin() const
{
    return m_begin;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::Modifier::end() const
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

#include "Message.hpp"

#pragma warning(push, 0)
#include <llvm/Support/Format.h>
#include <llvm/Support/WithColor.h>
#pragma warning(pop)

#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Util.hpp>

#include <ctre.hpp>
#include <utility>

cld::Message::Message(Severity severity, std::string message, Lexer::TokenIterator begin,
                      std::optional<Lexer::TokenIterator> rangeEnd, std::optional<Modifier> modifier)
    : m_modifier(std::move(modifier)),
      m_message(std::move(message)),
      m_begin(begin),
      m_rangeEnd(rangeEnd),
      m_severity(severity)
{
}

cld::Message::Severity cld::Message::getSeverity() const
{
    return m_severity;
}

cld::Message cld::Message::error(std::string message, Lexer::TokenIterator token, std::optional<Modifier> modifier)
{
    return cld::Message(Error, std::move(message), token, {}, std::move(modifier));
}

cld::Message cld::Message::note(std::string message,
                                std::vector<cld::Lexer::Token, std::allocator<cld::Lexer::Token>>::const_iterator token,
                                std::optional<Modifier> modifier)
{
    return cld::Message(Note, std::move(message), token, {}, std::move(modifier));
}

cld::Message cld::Message::warning(std::string message, Lexer::TokenIterator token, std::optional<Modifier> modifier)
{
    return cld::Message(Warning, std::move(message), token, {}, std::move(modifier));
}

namespace
{
llvm::raw_ostream& operator<<(llvm::raw_ostream& os, const std::string_view& sv)
{
    os.write(sv.data(), sv.size());
    return os;
}
} // namespace

llvm::raw_ostream& cld::Message::printEnd(llvm::raw_ostream& os, const SourceObject& sourceObject,
                                          llvm::raw_ostream::Colors colour, std::string_view prefix) const
{
    CLD_ASSERT(!m_rangeEnd || *m_rangeEnd == m_begin);
    CLD_ASSERT(!m_modifier || m_modifier->getAction() == Modifier::InsertAtEnd);
    const auto end = sourceObject.data().end();
    CLD_ASSERT(!sourceObject.data().empty());
    const auto line = sourceObject.getLineNumber((end - 1)->getOffset() + (end - 1)->getLength());
    const auto start = sourceObject.getLineStartOffset(line);
    const Lexer::TokenIterator begin = [&sourceObject, start] {
        for (std::ptrdiff_t i = sourceObject.data().size() - 1; i >= 0; i--)
        {
            const auto& token = sourceObject.data()[i];
            if (token.getOffset() + token.getLength() <= start)
            {
                return sourceObject.data().begin() + i + 1;
            }
        }
        return sourceObject.data().begin();
    }();
    std::string text;
    if (begin != end)
    {
        text =
            std::string(begin->getColumn(sourceObject), ' ') + cld::Lexer::reconstructTrimmed(sourceObject, begin, end);
        if (sourceObject.getLineNumber(begin->getOffset())
            != sourceObject.getLineNumber(begin->getOffset() + begin->getLength()))
        {
            text = text.substr(text.find('\n') + 1);
        }
    }
    os << line << ':' << ((end - 1)->getOffset() + (end - 1)->getLength() - start) << ": ";
    llvm::WithColor(os, colour, true).get() << prefix;
    llvm::WithColor(os, llvm::raw_ostream::SAVEDCOLOR, true) << m_message << '\n';
    auto numSize = std::to_string(line).size();
    const auto remainder = numSize % 4;
    if (remainder)
    {
        numSize += 4 - remainder;
    }
    os << llvm::format_decimal(line, numSize) << " | ";
    os << text << '\n';
    if (m_modifier)
    {
        os.indent(numSize) << " | ";
        const auto whitespace = stringOfSameWidth(text, ' ');
        llvm::WithColor(os.indent(whitespace.size()), colour) << "^\n";
        if (!m_modifier->getActionArgument().empty())
        {
            os.indent(numSize) << " | ";
            llvm::WithColor(os.indent(whitespace.size()), colour) << m_modifier->getActionArgument() << "\n";
        }
    }
    os.flush();
    return os;
}

llvm::raw_ostream& cld::Message::print(llvm::raw_ostream& os, const SourceObject& sourceObject) const
{
    auto [colour, prefix] = [this]() -> std::pair<llvm::raw_ostream::Colors, std::string_view> {
        switch (getSeverity())
        {
            case Message::Error: return {llvm::raw_ostream::RED, "error: "};
            case Message::Note: return {llvm::raw_ostream::CYAN, "note: "};
            case Message::Warning: return {llvm::raw_ostream::MAGENTA, "warning: "};
            default: CLD_UNREACHABLE;
        }
    }();
    if (m_begin == sourceObject.data().end())
    {
        return printEnd(os, sourceObject, colour, prefix);
    }

    const auto begin = [&sourceObject, this] {
        const auto line = sourceObject.getLineNumber(m_begin->getOffset());
        const auto start = sourceObject.getLineStartOffset(line);
        for (std::ptrdiff_t i = m_begin - sourceObject.data().begin(); i >= 0; i--)
        {
            const auto& token = sourceObject.data()[i];
            if (token.getOffset() + token.getLength() <= start)
            {
                return sourceObject.data().begin() + i + 1;
            }
        }
        return sourceObject.data().begin();
    }();
    const auto end = [&sourceObject, this] {
        if (m_rangeEnd && *m_rangeEnd == sourceObject.data().end())
        {
            return sourceObject.data().end();
        }
        const auto line =
            sourceObject.getLineNumber(m_rangeEnd ? m_rangeEnd.value()->getOffset() : m_begin->getOffset());
        const auto end = sourceObject.getLineEndOffset(line);
        return std::find_if(m_rangeEnd.value_or(m_begin), sourceObject.data().end(), [end](const Lexer::Token& token) {
            return token.getOffset() >= end || token.getTokenType() == Lexer::TokenType::Newline;
        });
    }();

    auto text =
        std::string(begin->getColumn(sourceObject), ' ') + cld::Lexer::reconstructTrimmed(sourceObject, begin, end);
    if (begin != m_begin
        && sourceObject.getLineNumber(begin->getOffset())
               != sourceObject.getLineNumber(begin->getOffset() + begin->getLength()))
    {
        // The very first token that was used to reconstruct is multiline. Our text needs trimming
        text = text.substr(text.find('\n') + 1);
    }

    if ((!m_rangeEnd || *m_rangeEnd != end - 1) && m_begin != end - 1
        && sourceObject.getLineNumber((end - 1)->getOffset())
               != sourceObject.getLineNumber((end - 1)->getOffset() + (end - 1)->getLength()))
    {
        text = text.substr(0, text.rfind('\n'));
    }

    const auto locationLine = sourceObject.getLineNumber(m_begin->getOffset());
    os << locationLine << ':' << m_begin->getOffset() - sourceObject.getLineStartOffset(locationLine) << ": ";
    llvm::WithColor(os, colour, true).get() << prefix;
    llvm::WithColor(os, llvm::raw_ostream::SAVEDCOLOR, true) << m_message << '\n';

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
            lines.emplace_back(text.data() + pos, result != std::string::npos ? result - pos : text.size() - pos);
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
            CLD_ASSERT(i - beginLine < underlined.size());
            underlined[i - beginLine] = std::pair{std::max(underLineBegin, sourceObject.getLineStartOffset(i)),
                                                  (std::min(underLineEnd, sourceObject.getLineEndOffset(i)))};
        }
    }
    for (std::size_t i = 0; i < lines.size(); i++)
    {
        os << llvm::format_decimal(beginLine + i, numSize) << " | ";
        if (!underlined[i])
        {
            os << lines[i] << '\n';
            continue;
        }
        const auto lineStart = sourceObject.getLineStartOffset(i + beginLine);
        if (m_modifier->getAction() != Modifier::InsertAtEnd)
        {
            os << lines[i].substr(0, underlined[i]->first - lineStart);
            llvm::WithColor(os, colour).get()
                << lines[i].substr(underlined[i]->first - lineStart, underlined[i]->second - underlined[i]->first);
            if (underlined[i]->second - lineStart < lines[i].size())
            {
                os << lines[i].substr(underlined[i]->second - lineStart);
            }
        }
        else
        {
            os << lines[i];
        }
        os << '\n';

        os.indent(numSize) << " | ";
        const auto whitespace = stringOfSameWidth(lines[i].substr(0, underlined[i]->first - lineStart), ' ');
        auto string = stringOfSameWidth(
            lines[i].substr(underlined[i]->first - lineStart, underlined[i]->second - underlined[i]->first), '~');
        switch (m_modifier->getAction())
        {
            case Modifier::Underline:
            {
                os << whitespace;
                llvm::WithColor(os, colour).get() << string;
                break;
            }
            case Modifier::PointAtBeginning:
            {
                os << whitespace;
                if (!string.empty())
                {
                    string[0] = '^';
                }
                llvm::WithColor(os, colour).get() << string;
                break;
            }
            case Modifier::PointAtEnd:
            {
                os << whitespace;
                if (!string.empty())
                {
                    string.back() = '^';
                }
                llvm::WithColor(os, colour).get() << string;
                break;
            }
            case Modifier::InsertAtEnd:
            {
                llvm::WithColor(os.indent(whitespace.size() + string.size()), colour) << '^';
                if (!m_modifier->getActionArgument().empty())
                {
                    os << '\n';
                    os.indent(static_cast<unsigned>(numSize)) << " | ";
                    llvm::WithColor(os.indent(whitespace.size() + string.size()), colour)
                        << m_modifier->getActionArgument();
                }
                break;
            }
            default: CLD_UNREACHABLE;
        }
        os << '\n';
    }

    os.flush();
    return os;
}

cld::Message cld::Message::error(std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                                 std::optional<Modifier> modifier)
{
    return cld::Message(Error, std::move(message), begin, end, modifier);
}

cld::Message cld::Message::warning(std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                                   std::optional<Modifier> modifier)
{
    return cld::Message(Warning, std::move(message), begin, end, modifier);
}

cld::Message cld::Message::note(std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                                std::optional<Modifier> modifier)
{
    return cld::Message(Note, std::move(message), begin, end, modifier);
}

cld::Modifier::Modifier(Lexer::TokenIterator begin, Lexer::TokenIterator anEnd, cld::Modifier::Action action,
                        std::string actionArgument)
    : m_actionArgument(std::move(actionArgument)), m_begin(begin), m_end(anEnd), m_action(action)
{
}

std::vector<cld::Lexer::Token>::const_iterator cld::Modifier::begin() const
{
    return m_begin;
}

std::vector<cld::Lexer::Token>::const_iterator cld::Modifier::end() const
{
    return m_end;
}

cld::Modifier::Action cld::Modifier::getAction() const
{
    return m_action;
}

const std::string& cld::Modifier::getActionArgument() const
{
    return m_actionArgument;
}

constexpr static auto pattern = ctll::fixed_string{R"(\{\})"};

std::string cld::Format::format(std::vector<std::string> args) const
{
    std::reverse(args.begin(), args.end());
    std::string result;
    auto stringView = std::string_view(m_format);
    for (auto& iter : ctre::range<pattern>(stringView))
    {
        auto view = iter.view();
        result.insert(result.end(), stringView.data(), view.data());
        if (args.empty())
        {
            CLD_ASSERT(false && "Not enough arguments specified to substitute in format");
        }
        stringView.remove_prefix(std::distance(stringView.data(), view.data() + view.size()));
        result += args.back();
        args.pop_back();
    }
    result += stringView;
    if (!args.empty())
    {
        CLD_ASSERT(false && "More arguments specified than needed to substitute");
    }
    return result;
}

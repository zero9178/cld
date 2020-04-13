#include "Message.hpp"

#include <llvm/Support/Format.h>
#include <llvm/Support/WithColor.h>

#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Util.hpp>

#include <ctre.hpp>
#include <utility>

cld::Message::Message(Severity severity, std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                      std::vector<Modifier> modifiers)
    : m_modifier(std::move(modifiers)), m_message(std::move(message)), m_begin(begin), m_end(end), m_severity(severity)
{
}

cld::Message::Severity cld::Message::getSeverity() const
{
    return m_severity;
}

cld::Message cld::Message::error(std::string message, Lexer::TokenIterator token, std::vector<Modifier> modifiers)
{
    return cld::Message(Error, std::move(message), token, token + 1, std::move(modifiers));
}

cld::Message cld::Message::note(std::string message, Lexer::TokenIterator token, std::vector<Modifier> modifiers)
{
    return cld::Message(Note, std::move(message), token, token + 1, std::move(modifiers));
}

cld::Message cld::Message::warning(std::string message, Lexer::TokenIterator token, std::vector<Modifier> modifiers)
{
    return cld::Message(Warning, std::move(message), token, token + 1, std::move(modifiers));
}

namespace
{
llvm::raw_ostream& operator<<(llvm::raw_ostream& os, const std::string_view& sv)
{
    os.write(sv.data(), sv.size());
    return os;
}
} // namespace
//
// llvm::raw_ostream& cld::Message::printEnd(llvm::raw_ostream& os, const SourceObject& sourceObject,
//                                          llvm::raw_ostream::Colors colour, std::string_view prefix) const
//{
//}

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

    const auto line = m_begin->getLine(sourceObject);
    const auto startOffset = sourceObject.getLineStartOffset(line);
    const auto endLine = sourceObject.getLineNumber((m_end - 1)->getOffset() + (m_end - 1)->getLength());
    const auto endOffset = sourceObject.getLineEndOffset(endLine);
    const auto view = std::string_view(sourceObject.getSource()).substr(startOffset, endOffset - startOffset - 1);

    llvm::WithColor(os, os.SAVEDCOLOR, true) << line << ':' << m_begin->getColumn(sourceObject) << ": ";
    llvm::WithColor(os, colour, true).get() << prefix;
    llvm::WithColor(os, os.SAVEDCOLOR, true) << m_message;
    os << '\n';

    std::vector<std::string_view> lines;
    std::size_t result = 0;
    while (true)
    {
        auto newline = view.find('\n', result);
        if (newline == std::string_view::npos)
        {
            lines.emplace_back(view.substr(result));
            break;
        }
        lines.emplace_back(view.substr(result, newline - result));
        result = newline + 1;
    }

    auto width = 1 + (std::size_t)std::ceil(std::log10f(endLine));
    {
        const auto remainder = width % 4;
        if (remainder)
        {
            width += 4 - remainder;
        }
    }

    for (std::size_t i = 0; i < lines.size(); i++)
    {
        os << llvm::format_decimal(line + i, width) << " | " << lines[i] << '\n';
    }

    os.flush();
    return os;
}

cld::Message cld::Message::error(std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                                 std::vector<Modifier> modifiers)
{
    return cld::Message(Error, std::move(message), begin, end, modifiers);
}

cld::Message cld::Message::warning(std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                                   std::vector<Modifier> modifiers)
{
    return cld::Message(Warning, std::move(message), begin, end, modifiers);
}

cld::Message cld::Message::note(std::string message, Lexer::TokenIterator begin, Lexer::TokenIterator end,
                                std::vector<Modifier> modifiers)
{
    return cld::Message(Note, std::move(message), begin, end, modifiers);
}

cld::Underline::Underline(Lexer::TokenIterator begin, Lexer::TokenIterator end) : m_begin(begin), m_end(end) {}

cld::Underline::Underline(Lexer::TokenIterator begin) : m_begin(begin), m_end(m_begin + 1) {}

cld::Lexer::TokenIterator cld::Underline::begin() const
{
    return m_begin;
}

cld::Lexer::TokenIterator cld::Underline::end() const
{
    return m_end;
}

cld::PointAt::PointAt(Lexer::TokenIterator begin, Lexer::TokenIterator end) : m_begin(begin), m_end(end) {}

cld::PointAt::PointAt(Lexer::TokenIterator begin) : m_begin(begin), m_end(m_begin + 1) {}

cld::Lexer::TokenIterator cld::PointAt::begin() const
{
    return m_begin;
}

cld::Lexer::TokenIterator cld::PointAt::end() const
{
    return m_end;
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
        CLD_ASSERT(!args.empty());
        stringView.remove_prefix(std::distance(stringView.data(), view.data() + view.size()));
        result += args.back();
        args.pop_back();
    }
    result += stringView;
    CLD_ASSERT(args.empty());
    return result;
}

cld::Insert::Insert(Lexer::TokenIterator insertAfter, std::string argument)
    : m_insertAfter(insertAfter), m_argument(std::move(argument))
{
}

cld::Lexer::TokenIterator cld::Insert::getInsertAfter() const noexcept
{
    return m_insertAfter;
}

const std::string& cld::Insert::getArgument() const noexcept
{
    return m_argument;
}

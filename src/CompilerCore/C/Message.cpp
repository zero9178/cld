#include "Message.hpp"

#include <llvm/ADT/SmallString.h>
#include <llvm/Support/Format.h>
#include <llvm/Support/WithColor.h>

#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Text.hpp>

#include <cmath>

#include <ctre.hpp>

cld::Message::Message(Severity severity, std::string message, Lexer::TokenIterator begin,
                      std::optional<Lexer::TokenIterator> end, std::vector<Modifier> modifiers)
    : m_modifiers(std::move(modifiers)), m_message(std::move(message)), m_begin(begin), m_end(end), m_severity(severity)
{
}

cld::Message::Severity cld::Message::getSeverity() const
{
    return m_severity;
}

cld::Message cld::Message::error(std::string message, Lexer::TokenIterator token, std::vector<Modifier> modifiers)
{
    return cld::Message(Error, std::move(message), token, {}, std::move(modifiers));
}

cld::Message cld::Message::note(std::string message, Lexer::TokenIterator token, std::vector<Modifier> modifiers)
{
    return cld::Message(Note, std::move(message), token, {}, std::move(modifiers));
}

cld::Message cld::Message::warning(std::string message, Lexer::TokenIterator token, std::vector<Modifier> modifiers)
{
    return cld::Message(Warning, std::move(message), token, {}, std::move(modifiers));
}

namespace
{
llvm::raw_ostream& operator<<(llvm::raw_ostream& os, const std::string_view& sv)
{
    os.write(sv.data(), sv.size());
    return os;
}
} // namespace

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

    const auto endIterator = m_end.value_or(m_begin == sourceObject.data().end() ? m_begin : m_begin + 1);
    CLD_ASSERT(endIterator > m_begin || m_begin == sourceObject.data().end());
    const auto& back = sourceObject.data().back();
    const auto line = m_begin != endIterator ? m_begin->getLine(sourceObject) : back.getLine(sourceObject);
    const auto startOffset = sourceObject.getLineStartOffset(line);
    const auto endLine = sourceObject.getLineNumber((endIterator - 1)->getOffset() + (endIterator - 1)->getLength());
    const auto endOffset = sourceObject.getLineEndOffset(endLine);

    std::uint64_t column;
    if (m_begin != endIterator)
    {
        column = m_begin->getColumn(sourceObject);
        llvm::WithColor(os, os.SAVEDCOLOR, true) << line << ':' << column << ": ";
    }
    else if (back.getTokenType() == Lexer::TokenType::Newline)
    {
        // If begin was end and the last line is supposed to be shown but we are in the preprocessor and it ends
        // with a newline token we want to output the location PAST the newline (aka in the line after) not the the
        // previous one
        llvm::WithColor(os, os.SAVEDCOLOR, true)
            << sourceObject.getLineNumber(back.getOffset() + back.getLength()) << ":1: ";
    }
    else
    {
        column = back.getColumn(sourceObject) + back.getLength();
        llvm::WithColor(os, os.SAVEDCOLOR, true) << line << ':' << column << ": ";
    }
    llvm::WithColor(os, colour, true).get() << prefix;
    llvm::WithColor(os, os.SAVEDCOLOR, true) << m_message;
    os << '\n';

    auto view = std::string_view(sourceObject.getSource()).substr(startOffset, endOffset - startOffset - 1);
    std::vector<std::int64_t> mapping(endOffset - startOffset);
    llvm::SmallString<128> safeUTF8;
    safeUTF8.reserve(mapping.size() - 1);
    cld::toSafeUTF8(view, std::back_inserter(safeUTF8), mapping.begin());

    constexpr static std::string_view SHOULDERED_OPEN_BOX = "\xE2\x8D\xBD";
    struct InsertData
    {
        std::uint64_t begin;
        std::string_view argument;
        bool asLine;
        bool shoulderInserted;
    };
    llvm::SmallVector<InsertData, 4> inserts;
    // Sort in order of appearance first so that changing to mapping from earlier inserts affect following ones
    std::sort(inserts.begin(), inserts.end(),
              [](const InsertData& lhs, const InsertData& rhs) { return lhs.begin < rhs.begin; });
    auto oldMapping = mapping;
    for (auto& iter : m_modifiers)
    {
        cld::match(
            iter, [](auto&&) {},
            [&](const InsertAfter& insertAfter) {
                CLD_ASSERT(insertAfter.getInsertAfter() != sourceObject.data().end());
                auto begin = insertAfter.getInsertAfter()->getOffset() + insertAfter.getInsertAfter()->getLength();
                auto oldBegin = begin + oldMapping[begin - startOffset];
                begin += mapping[begin - startOffset];
                bool shoulderInserted = false;
                if (begin < safeUTF8.size())
                {
                    llvm::UTF32 utf32;
                    const llvm::UTF8* start =
                        reinterpret_cast<const llvm::UTF8*>(safeUTF8.data() + begin - startOffset);
                    if (llvm::convertUTF8Sequence(
                            &start, reinterpret_cast<const llvm::UTF8*>(safeUTF8.data() + safeUTF8.size()), &utf32,
                            llvm::strictConversion)
                        != llvm::conversionOK)
                    {
                        CLD_UNREACHABLE;
                    }
                    if (!cld::isWhitespace(utf32))
                    {
                        shoulderInserted = true;
                        safeUTF8.insert(safeUTF8.begin() + begin - startOffset, SHOULDERED_OPEN_BOX.begin(),
                                        SHOULDERED_OPEN_BOX.end());
                        std::for_each(mapping.begin() + oldBegin - startOffset, mapping.end(),
                                      [](std::int64_t& number) { number += 3; });
                    }
                }
                inserts.push_back({begin, insertAfter.getArgument(),
                                   insertAfter.getInsertAfter()->getTokenType() == Lexer::TokenType::Newline,
                                   shoulderInserted});
            });
    }
    // Reverse so that the text on the very right is the lowest level

    struct UnderlineData
    {
        std::uint64_t begin;
        std::uint64_t end;
        char character;
    };
    llvm::SmallVector<UnderlineData, 4> underlines;
    struct PointAtData
    {
        std::uint64_t begin;
        std::uint64_t end;
    };
    llvm::SmallVector<PointAtData, 4> pointAts;
    struct AnnotateData
    {
        std::uint64_t begin;
        std::uint64_t end;
        std::uint64_t pos;
        std::string_view argument;
    };
    llvm::SmallVector<AnnotateData, 4> annotates;
    for (auto& iter : m_modifiers)
    {
        cld::match(
            iter,
            [&](const Underline& underline) {
                CLD_ASSERT(underline.begin() < underline.end());
                auto begin = underline.begin()->getOffset();
                CLD_ASSERT(begin >= startOffset);
                auto end = (underline.end() - 1)->getOffset() + (underline.end() - 1)->getLength();
                CLD_ASSERT(end < endOffset);
                begin += mapping[begin - startOffset];
                end += mapping[end - startOffset];
                underlines.push_back({begin, end, underline.getCharacter()});
            },
            [&](const PointAt& pointAt) {
                for (auto iter = pointAt.begin(); iter != pointAt.end(); iter++)
                {
                    CLD_ASSERT(pointAt.begin() < pointAt.end());
                    auto begin = iter->getOffset();
                    CLD_ASSERT(begin >= startOffset);
                    auto end = iter->getOffset() + iter->getLength();
                    CLD_ASSERT(end < endOffset);
                    begin += mapping[begin - startOffset];
                    end += mapping[end - startOffset];
                    pointAts.push_back({begin, end});
                }
            },
            [&](const Annotate& annotate) {
                CLD_ASSERT(std::distance(annotate.begin(), annotate.end()) > 0);
                auto begin = annotate.begin()->getOffset();
                CLD_ASSERT(begin >= startOffset);
                auto end = (annotate.end() - 1)->getOffset() + (annotate.end() - 1)->getLength();
                CLD_ASSERT(end < endOffset);
                auto pos = begin + (end - begin) / 2;
                begin += mapping[begin - startOffset];
                end += mapping[end - startOffset];
                pos += mapping[pos - startOffset];
                CLD_ASSERT(!annotate.getArgument().empty());
                annotates.push_back({begin, end, pos, annotate.getArgument()});
            },
            [](auto&&) {});
    }
    // Sorting the underlines so that the character of underlines that start further to the right are the ones that are
    // printed
    std::sort(underlines.begin(), underlines.end(),
              [](const UnderlineData& lhs, const UnderlineData& rhs) { return lhs.begin < rhs.begin; });

    // Make highlighting sorted, remove overlaps by breaking two ranges into three, merging two to one or
    // trimming one
    struct HighlightData
    {
        std::uint64_t begin;
        std::uint64_t end;
    };
    llvm::SmallVector<HighlightData, 8> highlighting(underlines.size() + pointAts.size());
    auto next =
        std::transform(underlines.begin(), underlines.end(), highlighting.begin(), [](const UnderlineData& underline) {
            return HighlightData{underline.begin, underline.end};
        });
    std::transform(pointAts.begin(), pointAts.end(), next, [](const PointAtData& pointAt) {
        return HighlightData{pointAt.begin, pointAt.end};
    });
    for (auto& iter : inserts)
    {
        if (!iter.shoulderInserted)
        {
            continue;
        }
        highlighting.push_back({iter.begin, iter.begin + 3});
    }
    for (auto& iter : annotates)
    {
        highlighting.push_back({iter.begin, iter.end});
    }
    std::sort(highlighting.begin(), highlighting.end(),
              [](const HighlightData& lhs, const HighlightData& rhs) { return lhs.begin < rhs.begin; });

    for (auto iter = highlighting.begin(); iter != highlighting.end() && iter + 1 != highlighting.end();)
    {
        if (iter->end <= (iter + 1)->begin)
        {
            iter++;
            continue;
        }
        auto thisRange = *iter;
        auto nextRange = *(iter + 1);
        iter = highlighting.erase(iter, iter + 2);
        iter = highlighting.insert(iter, HighlightData{thisRange.begin, std::max(thisRange.end, nextRange.end)});
    }

    llvm::SmallVector<std::string_view, 2> lines;
    view = std::string_view(safeUTF8.data(), safeUTF8.size());
    {
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
    }

    // log10f instead of std::log10f because of a libstdc++ bug
    auto width = 1 + (std::size_t)std::floor(log10f(endLine));
    {
        const auto remainder = width % 4;
        if (remainder)
        {
            width += 4 - remainder;
        }
    }

    llvm::SmallVector<llvm::SmallString<128>, 1> toPaintOn;
    for (std::size_t i = 0; i < lines.size(); i++)
    {
        toPaintOn.clear();
        toPaintOn.resize(1);
        os << llvm::format_decimal(line + i, width) << " | ";
        auto string = lines[i];
        std::int64_t lineStartOffset = sourceObject.getLineStartOffset(line + i);
        lineStartOffset += mapping[lineStartOffset - startOffset];

        std::int64_t result = 0;
        for (auto [begin, end] : highlighting)
        {
            if ((std::int64_t)end <= lineStartOffset || begin >= lineStartOffset + string.size())
            {
                continue;
            }
            auto startIndex = std::max<std::int64_t>((std::int64_t)begin - lineStartOffset, 0);
            auto endIndex =
                std::min<std::int64_t>((std::int64_t)end - lineStartOffset, lineStartOffset + string.size());
            if (result < startIndex)
            {
                os << string.substr(result, startIndex - result);
            }
            llvm::WithColor(os, colour).get() << string.substr(startIndex, endIndex - startIndex);
            result = endIndex;
        }
        os << string.substr(result) << '\n';
        if (m_modifiers.empty())
        {
            continue;
        }

        // Underline
        for (auto [begin, end, c] : underlines)
        {
            if ((std::int64_t)end <= lineStartOffset || begin >= lineStartOffset + string.size())
            {
                continue;
            }
            auto startIndex = std::max<std::int64_t>((std::int64_t)begin - lineStartOffset, 0);
            auto endIndex =
                std::min<std::int64_t>((std::int64_t)end - lineStartOffset, lineStartOffset + string.size());
            auto toHighlightView = string.substr(startIndex, endIndex - startIndex);
            auto prefix = string.substr(0, startIndex);
            auto pos = cld::unsafeColumnWidth(prefix);
            auto size = pos + cld::unsafeColumnWidth(toHighlightView);
            toPaintOn[0].resize(std::max<std::size_t>(size, toPaintOn[0].size()), ' ');
            std::fill(toPaintOn[0].begin() + pos, toPaintOn[0].begin() + size, c);
        }

        // Point At
        for (auto [begin, end] : pointAts)
        {
            if ((std::int64_t)end <= lineStartOffset || begin >= lineStartOffset + string.size())
            {
                continue;
            }
            auto startIndex = std::max<std::int64_t>((std::int64_t)begin - lineStartOffset, 0);
            auto endIndex =
                std::min<std::int64_t>((std::int64_t)end - lineStartOffset, lineStartOffset + string.size());
            auto toHighlightView = string.substr(startIndex, endIndex - startIndex);
            auto prefix = string.substr(0, startIndex);
            auto pos = cld::unsafeColumnWidth(prefix);
            auto size = pos + cld::unsafeColumnWidth(toHighlightView);
            toPaintOn[0].resize(std::max<std::size_t>(size, toPaintOn[0].size()), ' ');
            std::fill(toPaintOn[0].begin() + pos, toPaintOn[0].begin() + size, '^');
        }

        // Annotate underline
        for (auto [begin, end, center, args] : annotates)
        {
            if ((std::int64_t)end <= lineStartOffset || begin >= lineStartOffset + string.size())
            {
                continue;
            }
            auto startIndex = std::max<std::int64_t>((std::int64_t)begin - lineStartOffset, 0);
            auto endIndex =
                std::min<std::int64_t>((std::int64_t)end - lineStartOffset, lineStartOffset + string.size());
            auto toHighlightView = string.substr(startIndex, endIndex - startIndex);
            auto prefix = string.substr(0, startIndex);
            auto pos = cld::unsafeColumnWidth(prefix);
            auto size = pos + cld::unsafeColumnWidth(toHighlightView);
            toPaintOn[0].resize(std::max<std::size_t>(size, toPaintOn[0].size()), ' ');
            std::fill(toPaintOn[0].begin() + pos, toPaintOn[0].begin() + size, size == 1 ? '^' : '~');
        }

        using Inserters = std::variant<InsertData, AnnotateData>;
        llvm::SmallVector<Inserters, 4> inserters;
        inserters.insert(inserters.end(), annotates.begin(), annotates.end());
        for (auto& iter : inserts)
        {
            if (!iter.asLine)
            {
                inserters.push_back(std::move(iter));
            }
        }
        std::sort(inserters.begin(), inserters.end(), [](const Inserters& lhs, const Inserters& rhs) {
            return cld::match(
                       lhs, [](const InsertData& insertData) { return insertData.begin; },
                       [](const AnnotateData& annotateData) { return annotateData.pos; })
                   > cld::match(
                       rhs, [](const InsertData& insertData) { return insertData.begin; },
                       [](const AnnotateData& annotateData) { return annotateData.pos; });
        });

        // Insert After
        std::uint64_t currentHeight = 1;
        for (auto& iter : inserters)
        {
            std::uint64_t cen = cld::match(
                iter, [](const InsertData& insertData) { return insertData.begin; },
                [](const AnnotateData& annotateData) { return annotateData.pos; });
            if ((std::int64_t)cen < lineStartOffset || cen > lineStartOffset + string.size())
            {
                continue;
            }
            auto prefix = string.substr(0, cen - lineStartOffset);
            auto pos = cld::unsafeColumnWidth(prefix);
            if (std::holds_alternative<InsertData>(iter))
            {
                toPaintOn[0].resize(std::max<std::size_t>(pos + 1, toPaintOn[0].size()), ' ');
                toPaintOn[0][pos] = '^';
            }
            std::string_view argument = cld::match(iter, [](auto&& value) { return value.argument; });
            if (!argument.empty())
            {
                if (std::holds_alternative<AnnotateData>(iter) && currentHeight < 3)
                {
                    currentHeight = 3;
                }
                else if (toPaintOn[currentHeight - 1].size() < pos + argument.size() + 1
                         || std::any_of(toPaintOn[currentHeight - 1].begin() + pos,
                                        toPaintOn[currentHeight - 1].begin() + pos + argument.size() + 1,
                                        [](char c) { return c != ' '; }))
                {
                    currentHeight++;
                }
                toPaintOn.resize(currentHeight);
                for (std::uint64_t height = 1; height < currentHeight - 1; height++)
                {
                    toPaintOn[height].resize(std::max<std::size_t>(pos + 1, toPaintOn[height].size()), ' ');
                    toPaintOn[height][pos] = '|';
                }
                toPaintOn[currentHeight - 1].resize(
                    std::max<std::size_t>(pos + argument.size(), toPaintOn[currentHeight - 1].size()), ' ');
                std::copy(argument.begin(), argument.end(), toPaintOn[currentHeight - 1].begin() + pos);
            }
        }

        for (auto& iter : toPaintOn)
        {
            if (!iter.empty())
            {
                llvm::WithColor(os.indent(width) << " | ", colour) << iter << '\n';
            }
        }

        for (auto [begin, argument, onNewline, _] : inserts)
        {
            (void)_;
            if (!onNewline || begin - 1 != lineStartOffset + string.size())
            {
                continue;
            }
            llvm::WithColor(os.indent(width) << " > ", colour).get() << argument << '\n';
        }
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

cld::Underline::Underline(Lexer::TokenIterator begin, Lexer::TokenIterator end, char character)
    : m_begin(begin), m_end(end), m_character(character)
{
}

cld::Underline::Underline(Lexer::TokenIterator begin, char character)
    : m_begin(begin), m_end(m_begin + 1), m_character(character)
{
}

cld::Lexer::TokenIterator cld::Underline::begin() const
{
    return m_begin;
}

cld::Lexer::TokenIterator cld::Underline::end() const
{
    return m_end;
}

char cld::Underline::getCharacter() const
{
    return m_character;
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

cld::InsertAfter::InsertAfter(Lexer::TokenIterator insertAfter, std::string argument)
    : m_insertAfter(insertAfter), m_argument(std::move(argument))
{
}

cld::Lexer::TokenIterator cld::InsertAfter::getInsertAfter() const noexcept
{
    return m_insertAfter;
}

const std::string& cld::InsertAfter::getArgument() const noexcept
{
    return m_argument;
}

cld::Annotate::Annotate(Lexer::TokenIterator begin, std::string argument)
    : m_begin(begin), m_end(begin + 1), m_argument(std::move(argument))
{
}

cld::Annotate::Annotate(Lexer::TokenIterator begin, Lexer::TokenIterator end, std::string argument)
    : m_begin(begin), m_end(end), m_argument(std::move(argument))
{
}

cld::Lexer::TokenIterator cld::Annotate::begin() const
{
    return m_begin;
}

cld::Lexer::TokenIterator cld::Annotate::end() const
{
    return m_end;
}

const std::string& cld::Annotate::getArgument() const
{
    return m_argument;
}

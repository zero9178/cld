#include "Diagnostic.h"

#include <llvm/Support/Format.h>
#include <llvm/Support/WithColor.h>

#include <map>

#include "SourceObject.hpp"

namespace
{
using PointLocation = cld::detail::DiagnosticBase::PointLocation;

PointLocation toMacroId0(PointLocation location, const cld::SourceInterface& sourceInterface)
{
    while (location.macroId != 0)
    {
        location = cld::match(
            sourceInterface.getSubstitutions()[location.macroId],
            [](std::monostate) -> PointLocation { CLD_UNREACHABLE; },
            [](const cld::Source::Substitution& substitution) -> PointLocation {
                return {substitution.replacedIdentifier.getOffset(), substitution.replacedIdentifier.getFileId(),
                        substitution.replacedIdentifier.getMacroId()};
            },
            [](const cld::Source::Stringification& substitution) -> PointLocation {
                return {substitution.replacedIdentifier.getOffset(), substitution.replacedIdentifier.getFileId(),
                        substitution.replacedIdentifier.getMacroId()};
            },
            [](const cld::Source::TokenConcatenation& substitution) -> PointLocation {
                // I think it doesn't matter which we return here? Lets do left for now
                return {substitution.leftToken.getOffset(), substitution.leftToken.getFileId(),
                        substitution.leftToken.getMacroId()};
            });
    }
    return location;
}

std::pair<PointLocation, PointLocation> toMacroId0Range(std::pair<PointLocation, PointLocation> location,
                                                        const cld::SourceInterface& sourceInterface)
{
    while (location.first.macroId != 0)
    {
        location.first = cld::match(
            sourceInterface.getSubstitutions()[location.first.macroId],
            [](std::monostate) -> PointLocation { CLD_UNREACHABLE; },
            [](const cld::Source::Substitution& substitution) -> PointLocation {
                return {substitution.replacedIdentifier.getOffset(), substitution.replacedIdentifier.getFileId(),
                        substitution.replacedIdentifier.getMacroId()};
            },
            [](const cld::Source::Stringification& substitution) -> PointLocation {
                return {substitution.replacedIdentifier.getOffset(), substitution.replacedIdentifier.getFileId(),
                        substitution.replacedIdentifier.getMacroId()};
            },
            [](const cld::Source::TokenConcatenation& substitution) -> PointLocation {
                // I think it doesn't matter which we return here? Lets do left for now
                return {substitution.leftToken.getOffset(), substitution.leftToken.getFileId(),
                        substitution.leftToken.getMacroId()};
            });
    }
    while (location.second.macroId != 0)
    {
        location.second = cld::match(
            sourceInterface.getSubstitutions()[location.second.macroId],
            [](std::monostate) -> PointLocation { CLD_UNREACHABLE; },
            [](const cld::Source::Substitution& substitution) -> PointLocation {
                return {substitution.replacedIdentifier.getOffset() + substitution.replacedIdentifier.getLength(),
                        substitution.replacedIdentifier.getFileId(), substitution.replacedIdentifier.getMacroId()};
            },
            [](const cld::Source::Stringification& substitution) -> PointLocation {
                return {substitution.replacedIdentifier.getOffset() + substitution.replacedIdentifier.getLength(),
                        substitution.replacedIdentifier.getFileId(), substitution.replacedIdentifier.getMacroId()};
            },
            [](const cld::Source::TokenConcatenation& substitution) -> PointLocation {
                // I think it doesn't matter which we return here? Lets do left for now
                return {substitution.rightToken.getOffset() + substitution.rightToken.getLength(),
                        substitution.rightToken.getFileId(), substitution.rightToken.getMacroId()};
            });
    }
    return location;
}

std::pair<std::uint64_t, std::uint64_t> getLineCol(const PointLocation& pointLocation,
                                                   const cld::SourceInterface& sourceInterface)
{
    const auto line = sourceInterface.getLineNumber(pointLocation.fileId, pointLocation.offset);
    const auto column = pointLocation.offset - sourceInterface.getLineStartOffset(pointLocation.fileId, line) + 1;
    return {line, column};
}

void printLine(llvm::raw_ostream& ss, std::uint32_t fileId, std::uint64_t line, std::string_view text,
               std::uint64_t width, const std::vector<std::int64_t>& mapping, std::uint64_t mappingBase,
               const cld::SourceInterface& sourceInterface)
{
    ss << llvm::format_decimal(line, width) << " | ";
    ss.write(text.data(), text.size()) << '\n';
}

} // namespace

cld::Message cld::detail::DiagnosticBase::print(std::pair<PointLocation, PointLocation> location,
                                                std::string_view message, llvm::MutableArrayRef<Argument> arguments,
                                                llvm::ArrayRef<Modifiers> modifiers,
                                                const SourceInterface& sourceInterface) const
{
    std::string result;
    // TODO: Right now the colour is useless. Just putting it here to know what the colour when and were.
    // Will have to create a segment map or so in cld::Message that stores the colour and boldness information
    // and then emits it correctly in it's operator<<
    llvm::raw_string_ostream ss(result);

    location = toMacroId0Range(location, sourceInterface);
    auto [line, column] = getLineCol(location.first, sourceInterface);
    llvm::WithColor(ss, ss.WHITE, true) << sourceInterface.getFiles()[location.first.fileId].path << ':' << line << ':'
                                        << column << ": ";
    switch (m_severity)
    {
        case Severity::Error: llvm::WithColor(ss, ss.RED, true) << "error: "; break;
        case Severity::Warning: llvm::WithColor(ss, ss.MAGENTA, true) << "warning: "; break;
        case Severity::Note: llvm::WithColor(ss, ss.CYAN, true) << "note: "; break;
    }
    for (auto& iter : ctre::range<DIAG_ARG_PATTERN>(message))
    {
        auto index = iter.get<2>().view().back() - '0';
        auto mods = iter.get<1>().view();

        ss.write(message.data(), iter.view().data() - message.data());
        message.remove_prefix(iter.view().data() + iter.view().size() - message.data());
        if (mods.empty())
        {
            CLD_ASSERT(arguments[index].text);
            ss << *arguments[index].text;
        }
        else if (mods == "s")
        {
            CLD_ASSERT(arguments[index].integral);
            if (*arguments[index].integral != 1)
            {
                ss << 's';
            }
        }
        else
        {
            CLD_UNREACHABLE;
        }
    }
    ss.write(message.data(), message.size()) << '\n';

    std::map<std::uint32_t, std::pair<std::uint64_t, std::uint64_t>> fileToMaxMinLine = {
        {location.first.fileId,
         {getLineCol(location.first, sourceInterface).first, getLineCol(location.second, sourceInterface).first}}};
    for (auto& iter : modifiers)
    {
        auto index = cld::match(
            iter, [](const DiagnosticBase::Underline& underline) { return underline.index; },
            [](const DiagnosticBase::InsertAfter& insertAfter) { return insertAfter.index; },
            [](const DiagnosticBase::Annotate& annotate) { return annotate.index; });
        CLD_ASSERT(arguments[index].range);
        arguments[index].range = toMacroId0Range(*arguments[index].range, sourceInterface);
        auto startLine = getLineCol(arguments[index].range->first, sourceInterface).first;
        auto endLine = getLineCol(arguments[index].range->second, sourceInterface).second;
        if (auto found = fileToMaxMinLine.find(arguments[index].range->first.fileId); found != fileToMaxMinLine.end())
        {
            found->second = {std::min(found->second.first, startLine), std::max(found->second.second, endLine)};
        }
        else
        {
            fileToMaxMinLine.insert({arguments[index].range->first.fileId, {startLine, endLine}});
        }
    }

    auto width = std::max_element(fileToMaxMinLine.begin(), fileToMaxMinLine.end(), [](auto&& lhs, auto&& rhs) {
                     return lhs.second.second < rhs.second.second;
                 })->second.second;
    width = 1 + (std::size_t)std::floor(log10f(width));
    {
        const auto remainder = width % 4;
        if (remainder)
        {
            width += 4 - remainder;
        }
    }

    for (auto [fileId, lineBounds] : fileToMaxMinLine)
    {
        auto [minLine, maxLine] = lineBounds;
        const auto startOffset = sourceInterface.getLineStartOffset(fileId, minLine);
        const auto endOffset = sourceInterface.getLineEndOffset(fileId, maxLine);
        auto view =
            std::string_view(sourceInterface.getFiles()[fileId].source).substr(startOffset, endOffset - startOffset);
        std::vector<std::int64_t> mapping(view.size() + 1);
        std::string safeUTF8;
        safeUTF8.reserve(mapping.size());
        cld::toSafeUTF8(view, std::back_inserter(safeUTF8), mapping.begin());
        view = safeUTF8;
        for (auto i = minLine; i <= maxLine; i++)
        {
            auto thisLineStartOffset = sourceInterface.getLineStartOffset(fileId, i);
            thisLineStartOffset += mapping[thisLineStartOffset - startOffset];
            auto thisLineEndOffset = sourceInterface.getLineEndOffset(fileId, i);
            thisLineEndOffset += mapping[thisLineEndOffset - startOffset];
            printLine(ss, fileId, i,
                      view.substr(thisLineStartOffset - startOffset, thisLineEndOffset - thisLineStartOffset), width,
                      mapping, startOffset, sourceInterface);
        }
    }

    ss.flush();
    return Message(m_severity, std::move(result));
}

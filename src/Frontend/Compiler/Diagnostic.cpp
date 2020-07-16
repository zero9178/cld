#include "Diagnostic.h"

#include <llvm/Support/WithColor.h>

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
} // namespace

cld::Message cld::detail::DiagnosticBase::print(PointLocation location, std::string_view message,
                                                llvm::ArrayRef<Argument> arguments,
                                                const SourceInterface& sourceInterface) const
{
    std::string result;
    // TODO: Right now the colour is useless. Just putting it here to know what the colour when and were.
    // Will have to create a segment map or so in cld::Message that stores the colour and boldness information
    // and then emits it correctly in it's operator<<
    llvm::raw_string_ostream ss(result);

    location = toMacroId0(location, sourceInterface);
    const auto line = sourceInterface.getLineNumber(location.fileId, location.offset);
    const auto column = location.offset - sourceInterface.getLineStartOffset(location.fileId, line) + 1;
    llvm::WithColor(ss, ss.WHITE, true) << sourceInterface.getFiles()[location.fileId].path << ':' << line << ':'
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
    ss.write(message.data(), message.size());

    ss.flush();
    return Message(m_severity, std::move(result));
}

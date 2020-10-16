#include "CommandLine.hpp"

#include <llvm/Support/raw_ostream.h>

void cld::detail::CommandLine::printHelp(llvm::raw_ostream& os,
                                         std::vector<std::pair<std::string_view, std::string_view>> options)
{
    if (options.empty())
    {
        return;
    }
    const auto maxLen = std::max_element(options.begin(), options.end(), [](const auto& lhs, const auto& rhs) {
                            return lhs.first.size() < rhs.first.size();
                        })->first.size();
    constexpr auto indentStep = 4;
    auto width = maxLen;
    {
        const auto remainder = width % indentStep;
        if (remainder)
        {
            width += indentStep - remainder;
        }
    }

    for (auto [cli, description] : options)
    {
        os.write(cli.data(), cli.size());
        os.indent(width - cli.size() + indentStep).write(description.data(), description.size()) << '\n';
    }
    os.flush();
}

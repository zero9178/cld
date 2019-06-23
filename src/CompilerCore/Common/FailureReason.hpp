#ifndef OPENCLPARSER_FAILUREREASON_HPP
#define OPENCLPARSER_FAILUREREASON_HPP

#include <string>

namespace OpenCL
{
    class FailureReason
    {
        std::string m_text;

    public:
        explicit FailureReason(std::string text);

        [[nodiscard]] const std::string& getText() const;
    };
} // namespace OpenCL

#endif // OPENCLPARSER_FAILUREREASON_HPP

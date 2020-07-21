#pragma once

#include <llvm/Support/raw_ostream.h>

#include "SourceInterface.hpp"

namespace cld
{
enum class Severity
{
    Error,
    Warning,
    Note
};

class Message final
{
    Severity m_severity;
    std::string m_text;

public:
    Message() = default;

    Message(Severity severity, std::string text) : m_severity(severity), m_text(std::move(text)) {}

    Severity getSeverity() const
    {
        return m_severity;
    }

    const std::string& getText() const
    {
        return m_text;
    }

    friend llvm::raw_ostream& operator<<(llvm::raw_ostream& os, const cld::Message& message);
};

llvm::raw_ostream& operator<<(llvm::raw_ostream& os, const cld::Message& message);

} // namespace cld

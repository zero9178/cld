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
    Severity m_category;

public:
    Severity getSeverity() const
    {
        return m_category;
    }

    llvm::raw_ostream& print(llvm::raw_ostream& os, const SourceInterface& sourceInterface) const
    {
        return os;
    }
};
} // namespace cld

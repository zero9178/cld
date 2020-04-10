#pragma once

#include <llvm/Support/raw_ostream.h>

namespace cld
{
class PPSourceObject;
class SourceObject;

namespace PP
{
cld::PPSourceObject preprocess(const SourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs()) noexcept;
}
} // namespace cld

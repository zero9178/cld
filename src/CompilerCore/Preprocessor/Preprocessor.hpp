#pragma once

#pragma warning(push, 0)
#include <llvm/Support/raw_ostream.h>
#pragma warning(pop)

namespace cld
{
class PPSourceObject;
class SourceObject;

namespace PP
{
cld::PPSourceObject preprocess(const SourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs());
}
} // namespace cld

#ifndef OPENCLPARSER_PREPROCESSOR_HPP
#define OPENCLPARSER_PREPROCESSOR_HPP

#pragma warning(push, 0)
#include <llvm/Support/raw_ostream.h>
#pragma warning(pop)

namespace OpenCL
{
class SourceObject;

namespace PP
{
OpenCL::SourceObject preprocess(const SourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs());
}
} // namespace OpenCL

#endif // OPENCLPARSER_PREPROCESSOR_HPP

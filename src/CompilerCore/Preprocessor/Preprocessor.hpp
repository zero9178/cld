#pragma once

#include <llvm/Support/raw_ostream.h>

#include <string>
#include <vector>

namespace cld
{
class PPSourceObject;
class SourceObject;

namespace PP
{
struct Options
{
    std::string absoluteFilepath;
    std::vector<std::string> includeDirectories;
    std::vector<std::string> includeQuoteDirectories;
};

cld::PPSourceObject preprocess(const SourceObject& sourceObject, const Options& options = {},
                               llvm::raw_ostream* reporter = &llvm::errs()) noexcept;
} // namespace PP
} // namespace cld

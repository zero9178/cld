#pragma once

#include <llvm/Support/raw_ostream.h>

#include <CompilerCore/C/SourceObject.hpp>

#include <string>
#include <vector>

namespace cld
{
struct LanguageOptions;

namespace PP
{
PPSourceObject preprocess(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs()) noexcept;
} // namespace PP
} // namespace cld

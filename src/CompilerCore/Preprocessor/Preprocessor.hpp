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
PPSourceObject preprocess(PPSourceObject&& sourceObject, llvm::raw_ostream* reporter = &llvm::errs()) noexcept;

std::string reconstruct(const cld::Lexer::PPToken* begin, const cld::Lexer::PPToken* end,
                        const SourceInterface& sourceInterface) noexcept;
} // namespace PP
} // namespace cld

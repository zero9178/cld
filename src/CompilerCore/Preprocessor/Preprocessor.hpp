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

enum class ReconstructionMode
{
    Stringification,        ///< What the C Standard specifies for stringification
    SemanticallyEquivalent, ///< A semantically equivalent token stream to the input tokens akin to gcc -E
};

std::string reconstruct(const cld::Lexer::PPToken* begin, const cld::Lexer::PPToken* end,
                        const SourceInterface& sourceInterface,
                        ReconstructionMode reconstructionMode = ReconstructionMode::SemanticallyEquivalent) noexcept;
} // namespace PP
} // namespace cld

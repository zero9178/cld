#pragma once

#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#include <cld/Support/Filesystem.hpp>
#include <cld/Support/Triple.hpp>

namespace cld
{
namespace Semantics
{
class Program;
} // namespace Semantics
namespace CGLLVM
{
enum class DebugEmission
{
    None = 0,
    Line = 1,
    Default = 2,
    Extended = 3
};

struct Options
{
    bool emitAllDecls{};
    DebugEmission debugEmission = DebugEmission::None;
    llvm::Optional<llvm::Reloc::Model> reloc = llvm::None;
    llvm::CodeGenOpt::Level ol = llvm::CodeGenOpt::None;
};

std::unique_ptr<llvm::TargetMachine> generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                  const Options& options = {});
} // namespace CGLLVM
} // namespace cld

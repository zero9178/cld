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
    None,
    Line,
    Default,
    Extended
};

struct Options
{
    bool emitAllDecls{};
    DebugEmission debugEmission = DebugEmission::None;
    llvm::Optional<llvm::Reloc::Model> reloc = llvm::None;
    llvm::CodeGenOpt::Level ol = llvm::CodeGenOpt::None;
};

std::unique_ptr<llvm::TargetMachine> generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                  Triple triple = Triple::native(), const Options& options = {});
} // namespace CGLLVM
} // namespace cld

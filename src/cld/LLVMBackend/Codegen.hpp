#pragma once

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#include <cld/Support/Triple.hpp>

namespace cld
{
namespace Semantics
{
class Program;
} // namespace Semantics
namespace CGLLVM
{
struct Options
{
    bool pic{};
    bool emitAllDecls{};
};

std::unique_ptr<llvm::TargetMachine> generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                  Triple triple = Triple::native(), const Options& options = {},
                                                  llvm::Optional<llvm::Reloc::Model> reloc = llvm::None,
                                                  llvm::CodeGenOpt::Level ol = llvm::CodeGenOpt::None);
} // namespace CGLLVM
} // namespace cld

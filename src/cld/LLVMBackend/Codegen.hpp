#pragma once

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#include <cld/Common/Triple.hpp>

namespace cld
{
namespace Semantics
{
class Program;
} // namespace Semantics
namespace CGLLVM
{
std::unique_ptr<llvm::TargetMachine> generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                  Triple triple = Triple::native(),
                                                  llvm::Optional<llvm::CodeModel::Model> cm = llvm::None,
                                                  llvm::CodeGenOpt::Level ol = llvm::CodeGenOpt::None);
} // namespace CGLLVM
} // namespace cld

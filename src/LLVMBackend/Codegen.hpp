#pragma once

#include <llvm/IR/Module.h>

namespace cld
{
namespace Semantics
{
class Program;
} // namespace Semantics
namespace CGLLVM
{
void generateLLVM(llvm::Module& module, const Semantics::Program& program);
} // namespace CGLLVM
} // namespace cld

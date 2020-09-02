#pragma once

#include <llvm/IR/Module.h>

#include <cld/Common/Triple.hpp>

namespace cld
{
namespace Semantics
{
class Program;
} // namespace Semantics
namespace CGLLVM
{
void generateLLVM(llvm::Module& module, const Semantics::Program& program, Triple triple = Triple::native());
} // namespace CGLLVM
} // namespace cld

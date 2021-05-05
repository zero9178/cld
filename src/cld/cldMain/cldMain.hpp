
#pragma once

#include <llvm/Support/raw_ostream.h>

#include <string_view>

#include <tcb/span.hpp>

namespace cld
{
int main(tcb::span<std::string_view> ref, llvm::raw_ostream* err = &llvm::errs(),
         llvm::raw_ostream* out = &llvm::outs());
} // namespace cld


#pragma once

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/raw_ostream.h>

#include <string_view>

namespace cld
{
int main(llvm::MutableArrayRef<std::string_view> ref, llvm::raw_ostream* reporter = &llvm::errs());
} // namespace cld

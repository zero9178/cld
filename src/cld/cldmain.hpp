
#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <string_view>

namespace cld
{
int main(llvm::MutableArrayRef<std::string_view> ref);
} // namespace cld

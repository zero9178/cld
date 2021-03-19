#include "ABIImplementation.hpp"

#include "CodeGenerator.hpp"

cld::CGLLVM::ABIImplementation::ABIImplementation(const llvm::DataLayout& dataLayout) : m_dataLayout(dataLayout) {}

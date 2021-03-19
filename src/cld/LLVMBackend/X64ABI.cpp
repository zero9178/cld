#include "X64ABI.hpp"

#include "CodeGenerator.hpp"

cld::CGLLVM::X64ABI::X64ABI(const llvm::DataLayout& dataLayout) : ABIImplementation(dataLayout) {}

void cld::CGLLVM::X64ABI::applyPlatformABI(const Semantics::FunctionType& functionType, llvm::Type*& returnType,
                                           std::vector<llvm::Type*>& arguments)
{
}

llvm::AttributeList cld::CGLLVM::X64ABI::generateFunctionAttributes(llvm::AttributeList attributesIn,
                                                                    const llvm::FunctionType* llvmFunctionType,
                                                                    const Semantics::FunctionType& functionType)
{
    return llvm::AttributeList();
}

void cld::CGLLVM::X64ABI::generateFunctionEntry(
    CodeGenerator& codeGenerator, const llvm::Function* llvmFunction, const Semantics::FunctionType& functionType,
    const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>& paramDecls)
{
}

llvm::Value* cld::CGLLVM::X64ABI::generateValueReturn(CodeGenerator& codeGenerator, Value value) {}

cld::CGLLVM::Value cld::CGLLVM::X64ABI::generateVAArg(CodeGenerator& codeGenerator, Value vaList,
                                                      const Semantics::Type& destType)
{
    return nullptr;
}

cld::CGLLVM::Value cld::CGLLVM::X64ABI::generateFunctionCall(CodeGenerator& codeGenerator, Value callee,
                                                             llvm::FunctionType* llvmFunctionType,
                                                             const Semantics::FunctionType& functionType,
                                                             std::vector<llvm::Value*>&& arguments)
{
    return nullptr;
}

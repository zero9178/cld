#pragma once

#include "ABIImplementation.hpp"

namespace cld::CGLLVM
{
class X64ABI final : public ABIImplementation
{
public:
    X64ABI(const llvm::DataLayout& dataLayout);

    void applyPlatformABI(const Semantics::FunctionType& functionType, llvm::Type*& returnType,
                          std::vector<llvm::Type*>& arguments) override;

    llvm::AttributeList generateFunctionAttributes(llvm::AttributeList attributesIn,
                                                   const llvm::FunctionType* llvmFunctionType,
                                                   const Semantics::FunctionType& functionType) override;

    void generateFunctionEntry(CodeGenerator& codeGenerator, const llvm::Function* llvmFunction,
                               const Semantics::FunctionType& functionType,
                               const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>& paramDecls) override;

    llvm::Value* CLD_NULLABLE generateValueReturn(CodeGenerator& codeGenerator, Value value) override;

    Value generateVAArg(CodeGenerator& codeGenerator, Value vaList, const Semantics::Type& destType) override;

    Value generateFunctionCall(CodeGenerator& codeGenerator, Value callee, llvm::FunctionType* llvmFunctionType,
                               const Semantics::FunctionType& functionType,
                               std::vector<llvm::Value*>&& arguments) override;
};
} // namespace cld::CGLLVM

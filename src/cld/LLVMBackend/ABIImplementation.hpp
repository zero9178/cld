#pragma once

#include <llvm/IR/Attributes.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>

#include <cld/Frontend/Compiler/Semantics.hpp>
#include <cld/Support/Util.hpp>

namespace cld::CGLLVM
{
class CodeGenerator;
struct Value;

class ABIImplementation
{
protected:
    const llvm::DataLayout& m_dataLayout;

public:
    ABIImplementation(const llvm::DataLayout& dataLayout);

    virtual ~ABIImplementation() = default;

    virtual void applyPlatformABI(const Semantics::FunctionType& functionType, llvm::Type*& returnType,
                                  std::vector<llvm::Type*>& arguments) = 0;

    virtual llvm::AttributeList generateFunctionAttributes(llvm::AttributeList attributesIn,
                                                           const llvm::FunctionType* llvmFunctionType,
                                                           const Semantics::FunctionType& functionType,
                                                           const Semantics::ProgramInterface& programInterface) = 0;

    virtual void
        generateFunctionEntry(CodeGenerator& codeGenerator, const llvm::Function* CLD_NON_NULL llvmFunction,
                              const Semantics::FunctionType& functionType,
                              const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>& paramDecls) = 0;

    virtual llvm::Value* CLD_NULLABLE generateValueReturn(CodeGenerator& codeGenerator, Value value) = 0;

    virtual Value generateVAArg(CodeGenerator& codeGenerator, Value vaList, const Semantics::Type& destType) = 0;

    virtual Value generateFunctionCall(CodeGenerator& codeGenerator, Value callee, llvm::FunctionType* llvmFunctionType,
                                       const Semantics::FunctionType& functionType,
                                       std::vector<llvm::Value*>&& arguments) = 0;
};

} // namespace cld::CGLLVM

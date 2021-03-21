#pragma once

#include "CommonABIImpl.hpp"

namespace llvm
{
class AllocaInst;
} // namespace llvm

namespace cld::CGLLVM
{
namespace X64ABIImpl
{
struct Unchanged
{
};

struct PointerToTemporary
{
};

struct OnStack
{
};

struct Flattened
{
};

struct MultipleArgs
{
    std::size_t size;
};

struct Adjustments
{
    std::variant<Unchanged, PointerToTemporary, Flattened> returnType;
    using Arg = std::variant<Unchanged, OnStack, MultipleArgs>;
    std::vector<Arg> arguments;
};
} // namespace X64ABIImpl

class X64ABI final : public CommonABIImpl<X64ABIImpl::Adjustments>
{
public:
    X64ABI(const llvm::DataLayout& dataLayout);

protected:
    X64ABIImpl::Adjustments applyPlatformABIImpl(llvm::Type*& returnType, std::vector<llvm::Type*>& arguments) override;

public:
    llvm::AttributeList generateFunctionAttributes(llvm::AttributeList attributesIn,
                                                   const llvm::FunctionType* llvmFunctionType,
                                                   const Semantics::FunctionType& functionType,
                                                   const Semantics::ProgramInterface& programInterface) override;

    void generateFunctionEntry(CodeGenerator& codeGenerator, const llvm::Function* llvmFunction,
                               const Semantics::FunctionType& functionType,
                               const std::vector<std::unique_ptr<Semantics::VariableDeclaration>>& paramDecls) override;

    llvm::Value* CLD_NULLABLE generateValueReturn(CodeGenerator& codeGenerator, Value value) override;

    Value generateVAArg(CodeGenerator& codeGenerator, Value vaList, const Semantics::Type& type) override;

    Value generateFunctionCall(CodeGenerator& codeGenerator, Value callee, llvm::FunctionType* llvmFunctionType,
                               const Semantics::FunctionType& functionType,
                               std::vector<llvm::Value*>&& arguments) override;
};
} // namespace cld::CGLLVM

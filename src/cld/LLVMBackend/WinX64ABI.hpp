
#include "CommonABIImpl.hpp"

#pragma once

#include <cld/Frontend/Compiler/Semantics.hpp>

#include <unordered_map>

namespace llvm
{
class AllocaInst;
} // namespace llvm

namespace cld::CGLLVM
{
namespace WinX64Impl
{
enum Transformations
{
    Nothing,
    IntegerRegister,
    PointerToTemporary
};

struct Adjustments
{
    Transformations returnType = Nothing;
    std::vector<Transformations> arguments;
};
} // namespace WinX64Impl

class WinX64ABI final : public CommonABIImpl<WinX64Impl::Adjustments>
{
    llvm::AllocaInst* m_returnSlot = nullptr;

public:
    WinX64ABI(const llvm::DataLayout& dataLayout);

    WinX64Impl::Adjustments applyPlatformABIImpl(llvm::Type*& returnType, std::vector<llvm::Type*>& arguments) override;

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

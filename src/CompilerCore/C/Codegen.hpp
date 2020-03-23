
#pragma once

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>

#include <map>

#include "../Common/Expected.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace cld::Codegen
{
// using NodeRetType = Expected<std::pair<llvm::Value*, Semantics::Type>, FailureReason>;

using TypeRetType = llvm::Type*;

class Context final
{
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder{context};
    llvm::DIBuilder* debugBuilder;
    llvm::DIFile* debugUnit = nullptr;
    std::vector<llvm::BasicBlock*> continueBlocks;
    std::vector<llvm::BasicBlock*> breakBlocks;
    std::vector<std::pair<llvm::SwitchInst*, bool>> switchStack;
    std::vector<llvm::DIScope*> debugScope;
    const Semantics::FunctionType* currentFunction;
};
} // namespace cld::Codegen

#include "Common.hpp"

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

void cld::Tests::details::computeAndGet(std::unique_ptr<llvm::Module>&& module, std::string_view functionName,
                                        cld::function_ref<void(std::uintptr_t)> symbolCallback)
{
    auto jit = llvm::cantFail(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeContext ctx(std::make_unique<llvm::LLVMContext>());
    llvm::cantFail(jit->addIRModule(llvm::orc::ThreadSafeModule{std::move(module), ctx}));
    auto sym = llvm::cantFail(jit->lookup(functionName.data()));
    symbolCallback(sym.getAddress());
}

#include "Common.hpp"

#include <llvm/ExecutionEngine/ORC/CompileUtils.h>
#include <llvm/ExecutionEngine/ORC/ExecutionUtils.h>
#include <llvm/ExecutionEngine/ORC/IRCompileLayer.h>
#include <llvm/ExecutionEngine/ORC/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/ORC/LLJIT.h>
#include <llvm/ExecutionEngine/ORC/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

void cld::Tests::details::computeAndGet(std::unique_ptr<llvm::Module>&& module, std::string_view functionName,
                                        cld::function_ref<void(std::uintptr_t)> symbolCallback)
{
    auto jit = llvm::cantFail(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeContext ctx(std::make_unique<llvm::LLVMContext>());
    llvm::cantFail(jit->addIRModule(llvm::orc::ThreadSafeModule{std::move(module), ctx}));
    auto sym = llvm::cantFail(jit->lookup(functionName));
    symbolCallback(sym.getAddress());

    //    auto host = llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost());
    //
    //    auto dl = llvm::cantFail(host.getDefaultDataLayoutForTarget());
    //
    //    llvm::orc::ExecutionSession session;
    //    llvm::orc::RTDyldObjectLinkingLayer objectLayer(session,
    //                                                    [] { return std::make_unique<llvm::SectionMemoryManager>();
    //                                                    });
    //    llvm::orc::IRCompileLayer compileLayer(session, objectLayer,
    //                                           std::make_unique<llvm::orc::ConcurrentIRCompiler>(std::move(host)));
    //    llvm::orc::MangleAndInterner mangle(session, dl);
    //    auto& mainJD = session.createBareJITDylib("<main>");
    //    mainJD.addGenerator(
    //        llvm::cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(dl.getGlobalPrefix())));
    //    llvm::cantFail(compileLayer.add(mainJD, llvm::orc::ThreadSafeModule(std::move(module), ctx)));
    //    auto symbol = llvm::cantFail(session.lookup({&mainJD}, mangle(functionName)));
    //    symbolCallback(symbol.getAddress());
}

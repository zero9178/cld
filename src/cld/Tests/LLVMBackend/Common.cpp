#include "Common.hpp"

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

#include <stdarg.h>

#ifdef _WIN64
    #ifdef __MINGW32__
extern "C" void ___chkstk_ms();
    #else
extern "C" void __chkstk();
    #endif
#endif

namespace
{
int debugging_printf(const char* format, ...)
{
    va_list list;
    va_start(list, format);
    auto r = vprintf(format, list);
    va_end(list);
    return r;
}
} // namespace

void cld::Tests::detail::computeAndGet(std::unique_ptr<llvm::Module>&& module, std::string_view functionName,
                                       cld::function_ref<void(std::uintptr_t)> symbolCallback)
{
    llvm::orc::LLJITBuilder builder;
    auto jit = llvm::cantFail(builder.create());
    {
#ifdef _WIN64
    #ifdef __MINGW32__
        llvm::orc::SymbolStringPtr pointer = jit->getExecutionSession().intern("___chkstk_ms");
        llvm::cantFail(jit->getMainJITDylib().define(
            llvm::orc::absoluteSymbols({{pointer, llvm::JITEvaluatedSymbol::fromPointer(___chkstk_ms)}})));
    #else
        llvm::orc::SymbolStringPtr pointer2 = jit->getExecutionSession().intern("__chkstk");
        llvm::cantFail(jit->getMainJITDylib().define(
            llvm::orc::absoluteSymbols({{pointer2, llvm::JITEvaluatedSymbol::fromPointer(__chkstk)}})));
    #endif
#endif
    }
    llvm::orc::SymbolStringPtr pointer = jit->getExecutionSession().intern("printf");
    llvm::cantFail(jit->getMainJITDylib().define(
        llvm::orc::absoluteSymbols({{pointer, llvm::JITEvaluatedSymbol::fromPointer(debugging_printf)}})));

    llvm::orc::ThreadSafeContext ctx(std::make_unique<llvm::LLVMContext>());
    llvm::cantFail(jit->addIRModule(llvm::orc::ThreadSafeModule{std::move(module), ctx}));
    auto sym = llvm::cantFail(jit->lookup(functionName.data()));
    symbolCallback(sym.getAddress());
}

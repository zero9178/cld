//#include <llvm/Bitcode/BitcodeWriter.h>
//#include <llvm/ExecutionEngine/ExecutionEngine.h>
//#include <llvm/ExecutionEngine/SectionMemoryManager.h>
//#include <llvm/IR/Verifier.h>
//#include <llvm/Support/TargetRegistry.h>
//#include <llvm/Support/TargetSelect.h>
//
//#include <CompilerCore/C/Codegen.hpp>
//#include <CompilerCore/C/Lexer.hpp>
//#include <CompilerCore/C/Parser.hpp>
//#include <CompilerCore/Preprocessor/Preprocessor.hpp>
//
//#include <fstream>
//#include <iostream>

int main()
{
    //    std::ifstream file("../src/input.c", std::ios_base::binary);
    //    if (!file.is_open())
    //    {
    //        std::cerr << "Could not open source file";
    //        return -1;
    //    }
    //    std::string source;
    //    file.seekg(0, std::ios_base::end);
    //    std::size_t pos = file.tellg();
    //    source.resize(pos);
    //    file.seekg(0, std::ios_base::beg);
    //    file.read(source.data(), source.size());
    //
    //#ifdef _WIN32
    //    source.erase(std::remove(source.begin(), source.end(), '\r'), source.end());
    //#endif
    //
    //    llvm::InitializeNativeTarget();
    //    llvm::InitializeNativeTargetAsmPrinter();
    //    LLVMLinkInMCJIT();
    //
    //    llvm::LLVMContext context1;
    //    std::string error;
    //    auto target = llvm::TargetRegistry::lookupTarget(llvm::sys::getProcessTriple(), error);
    //    if (!target)
    //    {
    //        throw std::runtime_error(error);
    //    }
    //    auto targetMachine = target->createTargetMachine(llvm::sys::getProcessTriple(), "generic", "", {}, {});
    //
    //    OpenCL::Codegen::Context context;
    //    auto result = OpenCL::Lexer::tokenize("int i ft"); // OpenCL::PP::preprocess(std::move(source))
    //    OpenCL::Parser::buildTree(result, &std::cerr);
    //    return 0;
    //
    //    context.module->print(llvm::outs(), nullptr);
    //
    //    llvm::verifyModule(*context.module, &llvm::errs());
    //
    //    {
    //        llvm::EngineBuilder builder(std::move(context.module));
    //        std::string error;
    //
    //        auto ptr = std::make_unique<llvm::SectionMemoryManager>();
    //        auto ref = ptr.get();
    //        std::unique_ptr<llvm::ExecutionEngine> ee(builder.setErrorStr(&error)
    //                                                      .setEngineKind(llvm::EngineKind::JIT)
    //                                                      .setOptLevel(llvm::CodeGenOpt::Level::None)
    //                                                      .setSymbolResolver(std::move(ptr))
    //                                                      .create());
    //        ref->finalizeMemory(&error);
    //        void* address = (void*)ee->getFunctionAddress("main");
    //        std::cout << ((int (*)())address)() << std::flush;
    //    }
    //
    //    llvm::llvm_shutdown();
    //
    //    return 0;
}

#ifdef __clang__
#if _WIN32 && __clang_major__ == 8 && __clang_minor__ == 0 && __clang_patchlevel__ == 0

#include <windows.h>

extern "C" int lprofGetHostName(char* Name, int Len)
{
    WCHAR Buffer[128];
    DWORD BufferSize = sizeof(Buffer);
    BOOL Result = GetComputerNameExW(ComputerNameDnsFullyQualified, Buffer, &BufferSize);
    if (!Result)
        return -1;
    if (WideCharToMultiByte(CP_UTF8, 0, Buffer, -1, Name, Len, nullptr, nullptr) == 0)
        return -1;
    return 0;
}

#endif
#endif

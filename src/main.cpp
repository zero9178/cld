#include "Lexer.hpp"
#include "Parser.hpp"

#include <iostream>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>
#include <fstream>

int main()
{
    std::ifstream file("../src/input.c");
    if(!file.is_open())
    {
        std::cerr<<"Could not open source file";
        return -1;
    }
    std::string source;
    file.seekg(0,std::ios_base::end);
    std::size_t pos = file.tellg();
    source.resize(pos);
    file.seekg(0,std::ios_base::beg);
    file.read(source.data(),source.size());

    OpenCL::Parser::Context context;
    auto result = OpenCL::Lexer::tokenize(source);
    auto node = OpenCL::Parser::buildTree(std::move(result));
    node.codegen(context);
    context.module->print(llvm::errs(), nullptr);

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    LLVMLinkInMCJIT();

    int res;

    {
        llvm::EngineBuilder builder(std::move(context.module));
        std::string error;

        auto ptr = std::make_unique<llvm::SectionMemoryManager>();
        auto ref = ptr.get();
        std::unique_ptr<llvm::ExecutionEngine> ee(builder.setErrorStr(&error).setEngineKind(llvm::EngineKind::JIT)
                                                         .setOptLevel(llvm::CodeGenOpt::Level::None).setSymbolResolver(std::move(ptr)).create());
        ref->finalizeMemory(&error);
        void* address = (void*)ee->getFunctionAddress("main");
        res = ((int(*)())address)();
    }

    llvm::llvm_shutdown();

    return res;
}

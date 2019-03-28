#include "CompilerCore/Lexer.hpp"
#include "CompilerCore/Parser.hpp"
#include "CompilerCore/Preprocessor.hpp"

#include <iostream>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Verifier.h>
#include <fstream>

int main()
{
    std::ifstream file("../src/input.c",std::ios_base::binary);
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

    #ifdef _WIN32
    source.erase(std::remove(source.begin(),source.end(),'\r'),source.end());
    #endif

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    LLVMLinkInMCJIT();

    OpenCL::Syntax::CodegenContext context;
    auto result = OpenCL::Lexer::tokenize(OpenCL::PP::preprocess(std::move(source)));
    auto node = OpenCL::Parser::buildTree(std::move(result));
    context.visit(node);

    //node.codegen(context);

    context.module->print(llvm::outs(), nullptr);

    llvm::verifyModule(*context.module,&llvm::errs());

    {
        llvm::EngineBuilder builder(std::move(context.module));
        std::string error;

        auto ptr = std::make_unique<llvm::SectionMemoryManager>();
        auto ref = ptr.get();
        std::unique_ptr<llvm::ExecutionEngine> ee(builder.setErrorStr(&error).setEngineKind(llvm::EngineKind::JIT)
                                                         .setOptLevel(llvm::CodeGenOpt::Level::None).setSymbolResolver(std::move(ptr)).create());
        ref->finalizeMemory(&error);
        void* address = (void*)ee->getFunctionAddress("main");
        std::cout<<((int(*)())address)()<<std::flush;
    }

    llvm::llvm_shutdown();

    return 0;
}

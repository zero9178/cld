#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>

#include <cld/Common/Filesystem.hpp>
#include <cld/Frontend/Compiler/LanguageOptions.hpp>
#include <cld/Frontend/Compiler/Lexer.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/Semantics.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>
#include <cld/LLVMBackend/Codegen.hpp>

int main(int, char** argv)
{
#ifdef _MSC_VER
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
    _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);
#endif

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetDisassembler();

    std::string filename = argv[1];
    cld::fs::ifstream file(filename, std::ios_base::in | std::ios_base::ate | std::ios_base::binary);
    if (!file.is_open())
    {
        llvm::errs() << "Couldn't open file\n";
        return -1;
    }
    std::uint64_t size = file.tellg();
    file.seekg(0);
    std::string input(size, '\0');
    file.read(input.data(), size);

    auto options = cld::LanguageOptions::native();
    bool errors = false;
    auto pptokens = cld::Lexer::tokenize(std::move(input), options, &llvm::errs(), &errors, filename);
    if (errors)
    {
        return 1;
    }
    pptokens = cld::PP::preprocess(std::move(pptokens), &llvm::errs(), &errors);
    if (errors)
    {
        return 1;
    }
    auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);
    if (errors)
    {
        return 1;
    }
    auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
    if (errors)
    {
        return 1;
    }
    auto program = cld::Semantics::analyse(tree, std::move(ctokens), &llvm::errs(), &errors);
    if (errors)
    {
        return 1;
    }
    llvm::LLVMContext context;
    llvm::Module module("", context);
    module.setPICLevel(llvm::PICLevel::Level::BigPIC);
    module.setPIELevel(llvm::PIELevel::Level::Large);
    cld::CGLLVM::generateLLVM(module, program);
    std::error_code ec;
    llvm::raw_fd_ostream os("output.o", ec, llvm::sys::fs::OpenFlags::OF_None);
    if (ec)
    {
        llvm::errs() << "Failed to open output.o";
        return 1;
    }
    std::string error;
    auto* targetM = llvm::TargetRegistry::lookupTarget(module.getTargetTriple(), error);
    if (!targetM)
    {
        llvm::errs() << "Target lookup failed with error: " << error;
        return 1;
    }
    auto machine = std::unique_ptr<llvm::TargetMachine>(
        targetM->createTargetMachine(module.getTargetTriple(), "generic", "", {}, llvm::Reloc::Model::PIC_));
    llvm::legacy::PassManager pass;
    if (machine->addPassesToEmitFile(pass, os, nullptr, llvm::CodeGenFileType::CGFT_ObjectFile))
    {
        return 1;
    }
    pass.run(module);
    os.flush();
    return 0;
}

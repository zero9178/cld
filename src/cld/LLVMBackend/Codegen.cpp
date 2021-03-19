#include "Codegen.hpp"

#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include "CodeGenerator.hpp"

std::unique_ptr<llvm::TargetMachine> cld::CGLLVM::generateLLVM(llvm::Module& module, const Semantics::Program& program,
                                                               Triple triple, const Options& options)
{
    llvm::Triple llvmTriple;
    switch (triple.getArchitecture())
    {
        case cld::Architecture::x86: llvmTriple.setArch(llvm::Triple::ArchType::x86); break;
        case cld::Architecture::x86_64: llvmTriple.setArch(llvm::Triple::ArchType::x86_64); break;
        case cld::Architecture::Unknown: llvmTriple.setArch(llvm::Triple::ArchType::UnknownArch); break;
    }
    switch (triple.getPlatform())
    {
        case cld::Platform::Windows: llvmTriple.setOS(llvm::Triple::OSType::Win32); break;
        case cld::Platform::Linux: llvmTriple.setOS(llvm::Triple::OSType::Linux); break;
        case cld::Platform::Unknown: llvmTriple.setOS(llvm::Triple::OSType::UnknownOS); break;
    }
    switch (triple.getEnvironment())
    {
        case cld::Environment::GNU: llvmTriple.setEnvironment(llvm::Triple::GNU); break;
        case cld::Environment::MSVC: llvmTriple.setEnvironment(llvm::Triple::MSVC); break;
        case cld::Environment::Unknown: llvmTriple.setEnvironment(llvm::Triple::UnknownEnvironment); break;
    }
    module.setTargetTriple(llvmTriple.normalize());
    std::string error;
    auto* targetM = llvm::TargetRegistry::lookupTarget(module.getTargetTriple(), error);
    if (!targetM)
    {
        llvm::errs() << "Target lookup failed with error: " << error;
        return {};
    }
    auto machine = std::unique_ptr<llvm::TargetMachine>(
        targetM->createTargetMachine(module.getTargetTriple(), "generic", "", {}, options.reloc, {}, options.ol));
    module.setDataLayout(machine->createDataLayout());
    CodeGenerator codeGenerator(module, program, program.getSourceObject(), triple, options);
    codeGenerator.visit(program.getTranslationUnit());
    return machine;
}

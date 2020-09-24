#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>

#include <cld/Common/CommandLine.hpp>
#include <cld/Common/Filesystem.hpp>
#include <cld/Common/Triple.hpp>
#include <cld/Frontend/Compiler/LanguageOptions.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>
#include <cld/LLVMBackend/Codegen.hpp>

CLD_CLI_OPT(OUTPUT_FILE, ("-o <file>", "--output=<file>", "--output <file>"), (std::string_view, file))
("Path of the output file");

CLD_CLI_OPT(COMPILE_ONLY, ("-c", "--compile"))("Stop after compiling object files");

CLD_CLI_OPT(ASSEMBLY_OUTPUT, ("-S", "--assemble"))("Stop after compiling and output assembly files");

CLD_CLI_OPT(PREPROCESS_ONLY, ("-E", "--preprocess"))("Preprocess to stdout or output file");

CLD_CLI_OPT(TARGET, ("--target=<arg>", "-target <arg>"), (std::string_view, arg))("Compiler target triple to use");

CLD_CLI_OPT(DEFINE_MACRO, ("-D<macro>=<value>", "-D<macro>", "--define-macro <macro>", "--define-macro=<macro>"),
            (std::string_view, macro), (std::string_view, value))
("Define macro for the whole translation unit", cld::CLIMultiArg::List);

CLD_CLI_OPT(EMIT_LLVM, ("-emit-llvm"))("Use LLVM IR instead of machine code and assembly");

CLD_CLI_OPT(OPT, ("-O<level>", "-O", "--optimize", "--optimize=<level>"), (std::uint8_t, level))("Optimization level");

CLD_CLI_OPT(INCLUDES, ("-I<dir>", "--include-directory <dir>", "--include-directory=<dir>"), (std::string_view, dir))
("Additional include directories", cld::CLIMultiArg::List);

CLD_CLI_OPT(PIE, ("-f[no-]PIE"))("Build a position independent executable");

CLD_CLI_OPT(PIC, ("-f[no-]PIC"))("Build position independent code");

namespace
{
enum class Action
{
    Compile,
    Preprocess,
    AssemblyOutput
};

template <class CL>
std::optional<cld::fs::path> compileCFile(Action action, const cld::fs::path& cSourceFile, const cld::Triple& triple,
                                          const cld::LanguageOptions& languageOptions, const CL& cli)
{
    cld::fs::ifstream file(cSourceFile, std::ios_base::binary | std::ios_base::ate | std::ios_base::in);
    if (!file.is_open())
    {
        // TODO: Error
        return {};
    }

    std::size_t size = file.tellg();
    file.seekg(0);
    std::string input(size, '\0');
    file.read(input.data(), size);
    file.close();

    bool errors = false;
    auto pptokens =
        cld::Lexer::tokenize(std::move(input), languageOptions, &llvm::errs(), &errors, cSourceFile.u8string());
    if (errors)
    {
        return {};
    }
    pptokens = cld::PP::preprocess(std::move(pptokens), &llvm::errs(), &errors);
    if (errors)
    {
        return {};
    }
    if (action == Action::Preprocess)
    {
        auto reconstruction =
            cld::PP::reconstruct(pptokens.data().data(), pptokens.data().data() + pptokens.data().size(), pptokens);
        if (!cli.template get<OUTPUT_FILE>())
        {
            llvm::outs() << reconstruction;
            llvm::outs().flush();
            return cld::fs::path{};
        }

        cld::fs::ofstream outputFile(cld::to_string(*cli.template get<OUTPUT_FILE>()), std::ios_base::out);
        if (!outputFile.is_open())
        {
            // TODO: Error
            return {};
        }
        outputFile << reconstruction;
        outputFile.close();
        return cld::fs::u8path(*cli.template get<OUTPUT_FILE>());
    }
    auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);
    if (errors)
    {
        return {};
    }
    auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
    if (errors)
    {
        return {};
    }
    auto program = cld::Semantics::analyse(tree, std::move(ctokens), &llvm::errs(), &errors);
    if (errors)
    {
        return {};
    }

    llvm::LLVMContext context;
    llvm::Module module("", context);
    llvm::Optional<llvm::Reloc::Model> cm;
    if (triple.getArchitecture() == cld::Architecture::x86_64 && triple.getPlatform() == cld::Platform::Windows)
    {
        cm = llvm::Reloc::Model::PIC_;
    }
    else if (cli.template get<PIE>() || cli.template get<PIC>())
    {
        cm = llvm::Reloc::Model::PIC_;
    }
    llvm::CodeGenOpt::Level ol;
    if (cli.template get<OPT>())
    {
        switch (cli.template get<OPT>()->value_or(0))
        {
            case 0: ol = llvm::CodeGenOpt::None; break;
            case 1: ol = llvm::CodeGenOpt::Less; break;
            case 2: ol = llvm::CodeGenOpt::Default; break;
            default: ol = llvm::CodeGenOpt::Aggressive; break;
        }
    }
    else
    {
        ol = llvm::CodeGenOpt::None;
    }
    auto targetMachine = cld::CGLLVM::generateLLVM(module, program, triple, cm, ol);
    std::string outputFile;
    if (cli.template get<OUTPUT_FILE>())
    {
        outputFile = *cli.template get<OUTPUT_FILE>();
    }
    else
    {
        auto path = cSourceFile;
        if (action == Action::AssemblyOutput)
        {
            path.replace_extension("s");
        }
        else
        {
            path.replace_extension("o");
        }
        outputFile = path.u8string();
    }

    std::error_code ec;
    llvm::raw_fd_ostream os(outputFile, ec, llvm::sys::fs::OpenFlags::OF_None);
    if (ec)
    {
        // TODO: Error
        return {};
    }

    llvm::legacy::PassManager pass;
    if (targetMachine->addPassesToEmitFile(pass, os, nullptr,
                                           action == Action::AssemblyOutput ? llvm::CodeGenFileType::CGFT_AssemblyFile :
                                                                              llvm::CodeGenFileType::CGFT_ObjectFile))
    {
        return {};
    }
    pass.run(module);
    os.flush();

    return cld::fs::u8path(outputFile);
}

template <class CL>
int doActionOnAllFiles(Action action, const cld::LanguageOptions& languageOptions, const cld::Triple& triple,
                       llvm::ArrayRef<std::string_view> files, const CL& cli)
{
    std::vector<cld::fs::path> linkableFiles;
    for (auto& iter : files)
    {
        auto path = cld::fs::u8path(iter);
        auto extension = path.extension();
        if (extension == ".c")
        {
            auto objectFile = compileCFile(action, iter, triple, languageOptions, cli);
            if (!objectFile)
            {
                return -1;
            }
            if (action == Action::Compile)
            {
                linkableFiles.push_back(*objectFile);
            }
        }
    }
    return 0;
}
} // namespace

int main(int argc, char** argv)
{
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();
    llvm::InitializeAllAsmParsers();

    std::vector<std::string_view> elements(argc - 1);
    for (int i = 1; i < argc; i++)
    {
        elements[i - 1] = argv[i];
    }

    auto cli = cld::parseCommandLine<OUTPUT_FILE, COMPILE_ONLY, ASSEMBLY_OUTPUT, PREPROCESS_ONLY, TARGET, EMIT_LLVM,
                                     OPT, DEFINE_MACRO, INCLUDES, PIE, PIC>(elements);
    auto triple = cld::Triple::defaultTarget();
    if (cli.get<TARGET>())
    {
        triple = cld::Triple::fromString(*cli.get<TARGET>());
    }
    auto options = cld::LanguageOptions::fromTriple(triple);
    for (auto& iter : cli.get<INCLUDES>())
    {
        options.includeDirectories.emplace_back(iter);
    }
    for (auto& [name, maybeValue] : cli.get<DEFINE_MACRO>())
    {
        options.additionalMacros.emplace_back(name, maybeValue.value_or(""));
    }

    if (cli.getUnrecognized().empty())
    {
        // TODO: Error
        return -1;
    }
    if ((cli.get<COMPILE_ONLY>() || cli.get<ASSEMBLY_OUTPUT>() || cli.get<PREPROCESS_ONLY>()) && cli.get<OUTPUT_FILE>())
    {
        // TODO: Error
        return -1;
    }
    if (cli.get<COMPILE_ONLY>())
    {
        if (cli.get<ASSEMBLY_OUTPUT>())
        {
            // TODO: Error
        }
        if (cli.get<PREPROCESS_ONLY>())
        {
            // TODO: Error
        }
        return doActionOnAllFiles(Action::Compile, options, triple, cli.getUnrecognized(), cli);
    }
    else if (cli.get<ASSEMBLY_OUTPUT>())
    {
        if (cli.get<PREPROCESS_ONLY>())
        {
            // TODO: Error
        }
        return doActionOnAllFiles(Action::AssemblyOutput, options, triple, cli.getUnrecognized(), cli);
    }
    else
    {
        return doActionOnAllFiles(Action::Preprocess, options, triple, cli.getUnrecognized(), cli);
    }
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

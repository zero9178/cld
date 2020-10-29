
#include "cldmain.hpp"

#include <llvm/Bitcode/BitcodeWriterPass.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/LanguageOptions.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>
#include <cld/LLVMBackend/Codegen.hpp>
#include <cld/Support/CommandLine.hpp>
#include <cld/Support/Filesystem.hpp>
#include <cld/Support/Triple.hpp>

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

CLD_CLI_OPT(INCLUDES, ("-I<dir>", "-I <dir>", "--include-directory <dir>", "--include-directory=<dir>"),
            (std::string_view, dir))
("Additional include directories", cld::CLIMultiArg::List);

CLD_CLI_OPT(ISYSTEM, ("-isystem<dir>", "-isystem <dir>"), (std::string, dir))
("Append to system include directories", cld::CLIMultiArg::List);

CLD_CLI_OPT(PIE, ("-f[no-]PIE"))("Build a position independent executable");

CLD_CLI_OPT(PIC, ("-f[no-]PIC"))("Build position independent code");

CLD_CLI_OPT(FREESTANDING, ("-ffreestanding"))("Compile in a freestanding environment");

CLD_CLI_OPT(EMIT_ALL_DECLS, ("-femit-all-decls"))("Compile in a freestanding environment");

CLD_CLI_OPT(VERSION, ("--version"))("Print version");

CLD_CLI_OPT(HELP, ("--help", "-help"))("Display all options");

CLD_CLI_OPT(STANDARD_VERSION, ("-std=<arg>", "--std=<arg>", "--std <arg>"), (std::string_view, arg))
("C standard version");

CLD_CLI_OPT(WARNINGS, ("-W[no-]<warning>", "--warn-[no-]<warning>", "--warn-=[no-]<warning>"),
            (std::string_view, warning))
("Enable/disable specified warning", cld::CLIMultiArg::List);

namespace cld::cli
{
template <>
struct InitialStorage<WARNINGS>
{
    static auto getInitial()
    {
        return cld::diag::getAllWarnings();
    }
};
} // namespace cld::cli

namespace
{
template <class CL>
void addUnixMacroStyle(cld::PP::Options& ppOptions, const cld::LanguageOptions& languageOptions, std::string_view name,
                       const CL&)
{
    auto alloc = cld::to_string(name);
    ppOptions.additionalMacros.emplace_back("__" + alloc, "1");
    ppOptions.additionalMacros.emplace_back("__" + alloc + "__", "1");
    if (languageOptions.extension == cld::LanguageOptions::Extension::GNU)
    {
        ppOptions.additionalMacros.emplace_back(alloc, "1");
    }
}

template <class Integer>
Integer safeShiftLeft(Integer number, std::size_t value)
{
    if (value >= std::numeric_limits<Integer>::digits)
    {
        return 0;
    }

    return number << value;
}

template <class CL>
cld::PP::Options getTargetSpecificPreprocessorOptions(const cld::LanguageOptions& languageOptions,
                                                      const cld::Triple& triple, const CL& cli)
{
    cld::PP::Options result;
    if (triple.getArchitecture() == cld::Architecture::x86_64)
    {
        result.additionalMacros.emplace_back("__amd64__", "1");
        result.additionalMacros.emplace_back("__amd64", "1");
        result.additionalMacros.emplace_back("__x86_64", "1");
        result.additionalMacros.emplace_back("__x86_64__", "1");
        result.additionalMacros.emplace_back("__NO_MATH_INLINES", "1");
    }
    else if (triple.getArchitecture() == cld::Architecture::x86)
    {
        addUnixMacroStyle(result, languageOptions, "i386", cli);
        result.additionalMacros.emplace_back("__NO_MATH_INLINES", "1");
    }
    if (triple.getPlatform() == cld::Platform::Windows)
    {
        if (triple.getEnvironment() == cld::Environment::GNU)
        {
            result.additionalMacros.emplace_back("__declspec(a)", "__attribute__((a))");
            for (std::string iter : {"cdecl", "stdcall", "fastcall", "thiscall", "pascal"})
            {
                auto attribute = "__attribute__((__" + iter + "__))";
                result.additionalMacros.emplace_back("__" + iter, attribute);
                result.additionalMacros.emplace_back("_" + iter, attribute);
            }
            addUnixMacroStyle(result, languageOptions, "WIN32", cli);
            addUnixMacroStyle(result, languageOptions, "WINNT", cli);
            if (triple.getArchitecture() == cld::Architecture::x86_64)
            {
                addUnixMacroStyle(result, languageOptions, "WIN64", cli);
                result.additionalMacros.emplace_back("__MINGW64__", "1");
            }
            result.additionalMacros.emplace_back("__MINGW632__", "1");
            result.additionalMacros.emplace_back("__MSVCRT__", "1");
        }
        else if (triple.getEnvironment() == cld::Environment::MSVC)
        {
            result.additionalMacros.emplace_back("_INTEGRAL_MAX_BITS", "64");
            // TODO: Few more based on options
        }
        result.additionalMacros.emplace_back("_WIN32", "1");
        if (triple.getArchitecture() == cld::Architecture::x86_64)
        {
            result.additionalMacros.emplace_back("_WIN64", "1");
        }
    }
    else if (triple.getPlatform() == cld::Platform::Linux)
    {
        addUnixMacroStyle(result, languageOptions, "unix", cli);
        addUnixMacroStyle(result, languageOptions, "linux", cli);
        result.additionalMacros.emplace_back("__ELF__", "1");
        result.additionalMacros.emplace_back("__gnu_linux__", "1");
    }

    result.additionalMacros.emplace_back("__llvm__", "1");
    result.additionalMacros.emplace_back("__cld__", "1");
    // TODO: Command line option for this
    result.additionalMacros.emplace_back("__GNUC__", "4");
    result.additionalMacros.emplace_back("__GNUC_MINOR__", "2");
    result.additionalMacros.emplace_back("__GNUC_PATCHLEVEL__", "1");
    if (cli.template get<OPT>() && *cli.template get<OPT>() > 0)
    {
        result.additionalMacros.emplace_back("__OPTIMIZE__", "1");
    }

    // TODO: Macro definitions that follow should be more generalized and not depend on hard coded known values of
    // targets

    result.additionalMacros.emplace_back("__ORDER_LITTLE_ENDIAN__", "1234");
    result.additionalMacros.emplace_back("__ORDER_BIG_ENDIAN__", "4321");

    result.additionalMacros.emplace_back("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__");
    result.additionalMacros.emplace_back("__LITTLE_ENDIAN__", "1");

    if (languageOptions.sizeOfInt == 4 && languageOptions.sizeOfLong == 8 && languageOptions.sizeOfVoidStar == 8)
    {
        result.additionalMacros.emplace_back("_LP64", "1");
        result.additionalMacros.emplace_back("__LP64__", "1");
    }
    else if (languageOptions.sizeOfVoidStar == 4 && languageOptions.sizeOfInt == 4 && languageOptions.sizeOfLong == 4)
    {
        result.additionalMacros.emplace_back("_ILP32", "1");
        result.additionalMacros.emplace_back("__ILP32__", "1");
    }

    result.additionalMacros.emplace_back("__CHAR_BIT__", "8");

    result.additionalMacros.emplace_back("__SCHAR_MAX__", "127");
    result.additionalMacros.emplace_back(
        "__SHRT_MAX__", cld::to_string(safeShiftLeft(1uLL, (8 * languageOptions.sizeOfShort - 1)) - 1));
    result.additionalMacros.emplace_back("__INT_MAX__",
                                         cld::to_string(safeShiftLeft(1uLL, (8 * languageOptions.sizeOfInt - 1)) - 1));
    result.additionalMacros.emplace_back("__LONG_MAX__",
                                         cld::to_string(safeShiftLeft(1uLL, (8 * languageOptions.sizeOfLong - 1)) - 1));
    result.additionalMacros.emplace_back("__LONG_LONG_MAX__", cld::to_string(safeShiftLeft(1ull, (8 * 8 - 1)) - 1));
    std::size_t wcharSize;
    bool wcharSigned;
    switch (languageOptions.wcharUnderlyingType)
    {
        case cld::LanguageOptions::WideCharType::UnsignedShort:
            wcharSigned = false;
            wcharSize = languageOptions.sizeOfShort;
            break;
        case cld::LanguageOptions::WideCharType::Int:
            wcharSigned = true;
            wcharSize = languageOptions.sizeOfInt;
            break;
    }
    result.additionalMacros.emplace_back("__WCHAR_MAX__",
                                         cld::to_string(safeShiftLeft(1uLL, (8 * wcharSize - wcharSigned)) - 1));
    result.additionalMacros.emplace_back("__INTMAX_MAX__", "__LONG_LONG_MAX__");
    result.additionalMacros.emplace_back("__UINTMAX_MAX__", cld::to_string(~0ull));

    std::size_t sizeTSize;
    switch (languageOptions.sizeTType)
    {
        case cld::LanguageOptions::SizeTType::UnsignedInt: sizeTSize = languageOptions.sizeOfInt; break;
        case cld::LanguageOptions::SizeTType::UnsignedLong: sizeTSize = languageOptions.sizeOfLong; break;
        case cld::LanguageOptions::SizeTType::UnsignedLongLong: sizeTSize = 8; break;
    }
    result.additionalMacros.emplace_back("__SIZE_MAX__", cld::to_string(safeShiftLeft(1ull, (8 * sizeTSize)) - 1));
    std::size_t ptrDiffSize;
    switch (languageOptions.ptrdiffType)
    {
        case cld::LanguageOptions::PtrdiffType::Int: ptrDiffSize = languageOptions.sizeOfInt; break;
        case cld::LanguageOptions::PtrdiffType::Long: ptrDiffSize = languageOptions.sizeOfLong; break;
        case cld::LanguageOptions::PtrdiffType::LongLong: ptrDiffSize = 8; break;
    }
    result.additionalMacros.emplace_back("__PTRDIFF_MAX___",
                                         cld::to_string(safeShiftLeft(1ull, (8 * ptrDiffSize - 1)) - 1));
    result.additionalMacros.emplace_back(
        "__INTPTR_MAX__", cld::to_string(safeShiftLeft(1ull, (8 * languageOptions.sizeOfVoidStar - 1)) - 1));
    result.additionalMacros.emplace_back("__UNTPTR_MAX__",
                                         cld::to_string(safeShiftLeft(1ull, (8 * languageOptions.sizeOfVoidStar)) - 1));

    result.additionalMacros.emplace_back("__SIZEOF_DOUBLE__", "8");
    result.additionalMacros.emplace_back("__SIZEOF_FLOAT__", "4");
    result.additionalMacros.emplace_back("__SIZEOF_INT__", cld::to_string(languageOptions.sizeOfInt));
    result.additionalMacros.emplace_back("__SIZEOF_LONG__", cld::to_string(languageOptions.sizeOfLong));
    // TODO: Not always right
    switch (languageOptions.sizeOfLongDoubleBits)
    {
        case 64: result.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(8)); break;
        case 128: result.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(16)); break;
        case 80:
            if (triple.getArchitecture() == cld::Architecture::x86)
            {
                result.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(12));
            }
            else
            {
                result.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(16));
            }
    }
    result.additionalMacros.emplace_back("__SIZEOF_LONG_LONG__", "64");
    result.additionalMacros.emplace_back("__SIZEOF_POINTER__", cld::to_string(languageOptions.sizeOfVoidStar));
    result.additionalMacros.emplace_back("__SIZEOF_SHORT__", cld::to_string(languageOptions.sizeOfShort));
    result.additionalMacros.emplace_back("__SIZEOF_PTRDIFF_T__", cld::to_string(ptrDiffSize));
    result.additionalMacros.emplace_back("__SIZEOF_SIZE_T__", cld::to_string(sizeTSize));
    result.additionalMacros.emplace_back("__SIZEOF_WCHAR_T__", cld::to_string(wcharSize));

    result.additionalMacros.emplace_back("__INTMAX_TYPE__", "long long");
    result.additionalMacros.emplace_back("__INTMAX_C_SUFFIX__", "LL");
    result.additionalMacros.emplace_back("__INTMAX_WIDTH____", "64");
    result.additionalMacros.emplace_back("__UINTMAX_TYPE__", "unsigned long long");
    result.additionalMacros.emplace_back("__UINTMAX_C_SUFFIX__", "ULL");
    result.additionalMacros.emplace_back("__UINTMAX_WIDTH__", "64");
    switch (languageOptions.ptrdiffType)
    {
        case cld::LanguageOptions::PtrdiffType::Int:
            result.additionalMacros.emplace_back("__PTRDIFF_TYPE__", "int");
            break;
        case cld::LanguageOptions::PtrdiffType::Long:
            result.additionalMacros.emplace_back("__PTRDIFF_TYPE__", "long");
            break;
        case cld::LanguageOptions::PtrdiffType::LongLong:
            result.additionalMacros.emplace_back("__PTRDIFF_TYPE__", "long long");
            break;
    }
    result.additionalMacros.emplace_back("__PTRDIFF_WIDTH__", cld::to_string(ptrDiffSize * 8));
    switch (languageOptions.sizeTType)
    {
        case cld::LanguageOptions::SizeTType::UnsignedInt:
            result.additionalMacros.emplace_back("__SIZE_TYPE__", "unsigned int");
            break;
        case cld::LanguageOptions::SizeTType::UnsignedLong:
            result.additionalMacros.emplace_back("__SIZE_TYPE__", "unsigned long");
            break;
        case cld::LanguageOptions::SizeTType::UnsignedLongLong:
            result.additionalMacros.emplace_back("__SIZE_TYPE__", "unsigned long long");
            break;
    }
    result.additionalMacros.emplace_back("__SIZE_WIDTH__", cld::to_string(sizeTSize * 8));
    switch (languageOptions.wcharUnderlyingType)
    {
        case cld::LanguageOptions::WideCharType::UnsignedShort:
            result.additionalMacros.emplace_back("__WCHAR_TYPE__", "unsigned short");
            break;
        case cld::LanguageOptions::WideCharType::Int:
            result.additionalMacros.emplace_back("__WCHAR_TYPE__", "int");
            break;
    }
    result.additionalMacros.emplace_back("__WCHAR_WIDTH__", cld::to_string(wcharSize * 8));

    result.additionalMacros.emplace_back("__POINTER__WIDTH__", cld::to_string(8 * languageOptions.sizeOfVoidStar));
    if (!languageOptions.charIsSigned)
    {
        result.additionalMacros.emplace_back("__CHAR_UNSIGNED__", "1");
    }

    switch (languageOptions.wcharUnderlyingType)
    {
        case cld::LanguageOptions::WideCharType::UnsignedShort:
            result.additionalMacros.emplace_back("__WCHAR_UNSIGNED__", "1");
        default: break;
    }

    result.additionalMacros.emplace_back("__FINITE_MATH_ONLY__", "0");
    result.additionalMacros.emplace_back("__GNUC_STDC_INLINE__", "1");
    if (languageOptions.extension == cld::LanguageOptions::Extension::None)
    {
        result.additionalMacros.emplace_back("__STRICT_ANSI__", "1");
    }
    return result;
}

enum class Action
{
    Compile,
    Preprocess,
    AssemblyOutput,
    Link
};

template <class CL>
std::optional<cld::fs::path> compileCFile(Action action, const cld::fs::path& cSourceFile, const cld::Triple& triple,
                                          const cld::LanguageOptions& languageOptions, const CL& cli,
                                          llvm::raw_ostream* reporter)
{
    cld::fs::ifstream file(cSourceFile, std::ios_base::binary | std::ios_base::ate | std::ios_base::in);
    if (!file.is_open())
    {
        if (reporter)
        {
            *reporter << cld::Errors::CLI::FAILED_TO_OPEN_C_SOURCE_FILE_N.argsCLI(cSourceFile.u8string());
        }
        return {};
    }

    std::size_t size = file.tellg();
    file.seekg(0);
    std::string input(size, '\0');
    file.read(input.data(), size);
    file.close();

    bool errors = false;
    auto pptokens = cld::Lexer::tokenize(std::move(input), languageOptions, reporter, &errors, cSourceFile.u8string());
    if (errors)
    {
        return {};
    }

    cld::PP::Options ppOptions = getTargetSpecificPreprocessorOptions(languageOptions, triple, cli);
    for (auto& iter : cli.template get<INCLUDES>())
    {
        if (std::none_of(cli.template get<ISYSTEM>().begin(), cli.template get<ISYSTEM>().end(),
                         [&iter](const cld::fs::path& path) {
                             std::error_code ec;
                             auto res = cld::fs::equivalent(path, cld::fs::u8path(iter), ec);
                             return res && !ec;
                         }))
        {
            ppOptions.includeDirectories.emplace_back(iter);
        }
    }
    ppOptions.systemDirectories = cli.template get<ISYSTEM>();
    for (auto& [name, maybeValue] : cli.template get<DEFINE_MACRO>())
    {
        ppOptions.additionalMacros.emplace_back(name, maybeValue.value_or("1"));
    }
    pptokens = cld::PP::preprocess(std::move(pptokens), ppOptions, reporter, &errors);
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
            if (reporter)
            {
                *reporter << cld::Errors::CLI::FAILED_TO_OPEN_FILE_N_FOR_OUTPUT.argsCLI(
                    *cli.template get<OUTPUT_FILE>());
            }
            return {};
        }
        outputFile << reconstruction;
        outputFile.close();
        return cld::fs::u8path(*cli.template get<OUTPUT_FILE>());
    }
    auto ctokens = cld::Lexer::toCTokens(pptokens, reporter, &errors);
    if (errors)
    {
        return {};
    }
    auto tree = cld::Parser::buildTree(ctokens, reporter, &errors);
    if (errors)
    {
        return {};
    }
    auto program = cld::Semantics::analyse(tree, std::move(ctokens), reporter, &errors);
    if (errors)
    {
        return {};
    }

    cld::CGLLVM::Options codegenOptions;
    llvm::LLVMContext context;
    llvm::Module module("", context);
    llvm::Optional<llvm::Reloc::Model> cm;
    if (triple.getArchitecture() == cld::Architecture::x86_64 && triple.getPlatform() == cld::Platform::Windows)
    {
        cm = llvm::Reloc::Model::PIC_;
        codegenOptions.pic = true;
    }
    else if (cli.template get<PIE>() || cli.template get<PIC>())
    {
        cm = llvm::Reloc::Model::PIC_;
        codegenOptions.pic = true;
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
    codegenOptions.emitAllDecls = cli.template get<EMIT_ALL_DECLS>();
    auto targetMachine = cld::CGLLVM::generateLLVM(module, program, triple, codegenOptions, cm, ol);
#ifndef NDEBUG
    if (llvm::verifyModule(module, &llvm::errs()))
    {
        llvm::errs().flush();
        module.print(llvm::outs(), nullptr);
    }
#endif
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
            if (cli.template get<EMIT_LLVM>())
            {
                path.replace_extension("ll");
            }
            else
            {
                path.replace_extension("s");
            }
        }
        else
        {
            if (cli.template get<EMIT_LLVM>())
            {
                path.replace_extension("bc");
            }
            else
            {
                path.replace_extension("o");
            }
        }
        outputFile = path.u8string();
    }

    std::error_code ec;
    llvm::raw_fd_ostream os(outputFile, ec, llvm::sys::fs::OpenFlags::OF_None);
    if (ec)
    {
        if (reporter)
        {
            *reporter << cld::Errors::CLI::FAILED_TO_OPEN_FILE_N_FOR_OUTPUT.argsCLI(outputFile);
        }
        return {};
    }

    llvm::legacy::PassManager pass;
    if (cli.template get<EMIT_LLVM>())
    {
        if (action == Action::AssemblyOutput)
        {
            pass.add(llvm::createPrintModulePass(os));
        }
        else
        {
            pass.add(llvm::createBitcodeWriterPass(os));
        }
    }
    else
    {
        if (targetMachine->addPassesToEmitFile(pass, os, nullptr,
                                               action == Action::AssemblyOutput ?
                                                   llvm::CodeGenFileType::CGFT_AssemblyFile :
                                                   llvm::CodeGenFileType::CGFT_ObjectFile))
        {
            return {};
        }
    }
    pass.run(module);
    os.flush();

    return cld::fs::u8path(outputFile);
}

template <class CL>
int doActionOnAllFiles(Action action, const cld::LanguageOptions& languageOptions, const cld::Triple& triple,
                       llvm::ArrayRef<std::string_view> files, const CL& cli, llvm::raw_ostream* reporter)
{
    std::vector<cld::fs::path> linkableFiles;
    for (auto& iter : files)
    {
        auto path = cld::fs::u8path(iter);
        auto extension = path.extension();
        if (extension == ".c")
        {
            auto objectFile = compileCFile(action, iter, triple, languageOptions, cli, reporter);
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

void printVersion(llvm::raw_ostream& os)
{
    os << "cld version " << CLD_VERSION << '\n';
    os.flush();
}

} // namespace

int cld::main(llvm::MutableArrayRef<std::string_view> elements, llvm::raw_ostream* reporter, llvm::raw_ostream* out)
{
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();
    llvm::InitializeAllAsmParsers();

    auto cli = cld::parseCommandLine<OUTPUT_FILE, COMPILE_ONLY, ASSEMBLY_OUTPUT, PREPROCESS_ONLY, TARGET, EMIT_LLVM,
                                     OPT, DEFINE_MACRO, INCLUDES, PIE, PIC, FREESTANDING, HELP, VERSION,
                                     STANDARD_VERSION, WARNINGS, ISYSTEM, EMIT_ALL_DECLS>(elements);
    if (cli.get<HELP>())
    {
        if (out)
        {
            cli.printHelp(*out);
        }
        return 0;
    }
    if (cli.get<VERSION>())
    {
        if (out)
        {
            printVersion(*out);
        }
        return 0;
    }
    auto triple = cld::Triple::defaultTarget();
    if (cli.get<TARGET>())
    {
        triple = cld::Triple::fromString(*cli.get<TARGET>());
    }
    bool gnuExtensions = false;
    auto language = cld::LanguageOptions::Language::C99;
    if (cli.get<STANDARD_VERSION>())
    {
        auto str = *cli.get<STANDARD_VERSION>();
        if (str == "gnu99")
        {
            language = cld::LanguageOptions::Language ::C99;
            gnuExtensions = true;
        }
        else if (str == "c99")
        {
            language = cld::LanguageOptions::Language ::C99;
            gnuExtensions = false;
        }
        else
        {
            if (reporter)
            {
                *reporter << Errors::CLI::UNKNOWN_LANGUAGE_STANDARD_N.argsCLI(str);
            }
            return -1;
        }
    }
    auto options = cld::LanguageOptions::fromTriple(triple, language);
    if (gnuExtensions)
    {
        options.extension = cld::LanguageOptions::Extension::GNU;
    }
    options.enabledWarnings = cli.get<WARNINGS>();
    options.freeStanding = cli.get<FREESTANDING>();

    if (cli.getUnrecognized().empty())
    {
        if (reporter)
        {
            *reporter << Errors::CLI::NO_SOURCE_FILES_SPECIFIED.argsCLI();
        }
        return -1;
    }
    if (cli.get<COMPILE_ONLY>())
    {
        if (cli.get<ASSEMBLY_OUTPUT>())
        {
            if (reporter)
            {
                *reporter << Errors::CLI::CANNOT_COMPILE_TO_OBJECT_FILE_AND_ASSEMBLY_AT_THE_SAME_TIME.argsCLI();
            }
        }
        if (cli.get<PREPROCESS_ONLY>())
        {
            if (reporter)
            {
                *reporter << Errors::CLI::CANNOT_COMPILE_TO_OBJECT_FILE_AND_PREPROCESS_AT_THE_SAME_TIME.argsCLI();
            }
        }
        return doActionOnAllFiles(Action::Compile, options, triple, cli.getUnrecognized(), cli, reporter);
    }
    if (cli.get<ASSEMBLY_OUTPUT>())
    {
        if (cli.get<PREPROCESS_ONLY>())
        {
            if (reporter)
            {
                *reporter << Errors::CLI::CANNOT_COMPILE_TO_ASSEMBLY_AND_PREPROCESS_AT_THE_SAME_TIME.argsCLI();
            }
        }
        return doActionOnAllFiles(Action::AssemblyOutput, options, triple, cli.getUnrecognized(), cli, reporter);
    }
    if (cli.get<PREPROCESS_ONLY>())
    {
        return doActionOnAllFiles(Action::Preprocess, options, triple, cli.getUnrecognized(), cli, reporter);
    }
    return doActionOnAllFiles(Action::Compile, options, triple, cli.getUnrecognized(), cli, reporter);
}

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

CLD_CLI_OPT(FREESTANDING, ("-ffreestanding"))("Compile in a freestanding environment");

CLD_CLI_OPT(VERSION, ("--version"))("Print version");

CLD_CLI_OPT(HELP, ("--help", "-help"))("Display all options");

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

template <class CL>
void addUnixMacroStyle(cld::LanguageOptions& languageOptions, std::string_view name, const CL&)
{
    auto alloc = cld::to_string(name);
    languageOptions.additionalMacros.emplace_back("__" + alloc, "1");
    languageOptions.additionalMacros.emplace_back("__" + alloc + "__", "1");
    // TODO: GNU style
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
void applyTargetSpecificLanguageOptions(cld::LanguageOptions& languageOptions, const cld::Triple& triple, const CL& cli)
{
    if (triple.getArchitecture() == cld::Architecture::x86_64)
    {
        languageOptions.additionalMacros.emplace_back("__amd64__", "1");
        languageOptions.additionalMacros.emplace_back("__amd64", "1");
        languageOptions.additionalMacros.emplace_back("__x86_64", "1");
        languageOptions.additionalMacros.emplace_back("__x86_64__", "1");
        languageOptions.additionalMacros.emplace_back("__NO_MATH_INLINES", "1");
    }
    else if (triple.getArchitecture() == cld::Architecture::x86)
    {
        addUnixMacroStyle(languageOptions, "i386", cli);
        languageOptions.additionalMacros.emplace_back("__NO_MATH_INLINES", "1");
    }
    if (triple.getPlatform() == cld::Platform::Windows)
    {
        if (triple.getEnvironment() == cld::Environment::GNU)
        {
            languageOptions.additionalMacros.emplace_back("__declspec(a)", "__attribute__((a))");
            for (std::string iter : {"cdecl", "stdcall", "fastcall", "thiscall", "pascal"})
            {
                auto attribute = "__attribute__((__" + iter + "__))";
                languageOptions.additionalMacros.emplace_back("__" + iter, attribute);
                languageOptions.additionalMacros.emplace_back("_" + iter, attribute);
            }
            addUnixMacroStyle(languageOptions, "WIN32", cli);
            addUnixMacroStyle(languageOptions, "WINNT", cli);
            if (triple.getArchitecture() == cld::Architecture::x86_64)
            {
                addUnixMacroStyle(languageOptions, "WIN64", cli);
                languageOptions.additionalMacros.emplace_back("__MINGW64__", "1");
            }
            languageOptions.additionalMacros.emplace_back("__MINGW632__", "1");
            languageOptions.additionalMacros.emplace_back("__MSVCRT__", "1");
        }
        else if (triple.getEnvironment() == cld::Environment::MSVC)
        {
            languageOptions.additionalMacros.emplace_back("_INTEGRAL_MAX_BITS", "64");
            // TODO: Few more based on options
        }
        languageOptions.additionalMacros.emplace_back("_WIN32", "1");
        if (triple.getArchitecture() == cld::Architecture::x86_64)
        {
            languageOptions.additionalMacros.emplace_back("_WIN64", "1");
        }
    }
    else if (triple.getPlatform() == cld::Platform::Linux)
    {
        addUnixMacroStyle(languageOptions, "unix", cli);
        addUnixMacroStyle(languageOptions, "linux", cli);
        languageOptions.additionalMacros.emplace_back("__ELF__", "1");
        languageOptions.additionalMacros.emplace_back("__gnu_linux__", "1");
    }

    languageOptions.additionalMacros.emplace_back("__llvm__", "1");
    languageOptions.additionalMacros.emplace_back("__cld__", "1");
    if (triple.getEnvironment() == cld::Environment::GNU)
    {
        // TODO: Command line option for this
        languageOptions.additionalMacros.emplace_back("__GNUC__", "4");
        languageOptions.additionalMacros.emplace_back("__GNUC_MINOR__", "2");
        languageOptions.additionalMacros.emplace_back("__GNUC_PATCHLEVEL__", "1");
    }
    if (cli.template get<OPT>() || *cli.template get<OPT>() > 0)
    {
        languageOptions.additionalMacros.emplace_back("__OPTIMIZE__", "1");
    }

    // TODO: Macro definitions that follow should be more generalized and not depend on hard coded known values of
    // targets

    languageOptions.additionalMacros.emplace_back("__ORDER_LITTLE_ENDIAN__", "1234");
    languageOptions.additionalMacros.emplace_back("__ORDER_BIG_ENDIAN__", "4321");

    languageOptions.additionalMacros.emplace_back("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__");
    languageOptions.additionalMacros.emplace_back("__LITTLE_ENDIAN__", "1");

    if (languageOptions.sizeOfInt == 4 && languageOptions.sizeOfLong == 8 && languageOptions.sizeOfVoidStar == 8)
    {
        languageOptions.additionalMacros.emplace_back("_LP64", "1");
        languageOptions.additionalMacros.emplace_back("__LP64__", "1");
    }
    else if (languageOptions.sizeOfVoidStar == 4 && languageOptions.sizeOfInt == 4 && languageOptions.sizeOfLong == 4)
    {
        languageOptions.additionalMacros.emplace_back("_ILP32", "1");
        languageOptions.additionalMacros.emplace_back("__ILP32__", "1");
    }

    languageOptions.additionalMacros.emplace_back("__CHAR_BIT__", "8");

    languageOptions.additionalMacros.emplace_back("__SCHAR_MAX__", "127");
    languageOptions.additionalMacros.emplace_back(
        "__SHRT_MAX__", cld::to_string(safeShiftLeft(1uLL, (8 * languageOptions.sizeOfShort - 1)) - 1));
    languageOptions.additionalMacros.emplace_back(
        "__INT_MAX__", cld::to_string(safeShiftLeft(1uLL, (8 * languageOptions.sizeOfInt - 1)) - 1));
    languageOptions.additionalMacros.emplace_back(
        "__LONG_MAX__", cld::to_string(safeShiftLeft(1uLL, (8 * languageOptions.sizeOfLong - 1)) - 1));
    languageOptions.additionalMacros.emplace_back("__LONG_LONG_MAX__",
                                                  cld::to_string(safeShiftLeft(1ull, (8 * 8 - 1)) - 1));
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
    languageOptions.additionalMacros.emplace_back(
        "__WCHAR_MAX__", cld::to_string(safeShiftLeft(1uLL, (8 * wcharSize - wcharSigned)) - 1));
    languageOptions.additionalMacros.emplace_back("__INTMAX_MAX__", "__LONG_LONG_MAX__");
    languageOptions.additionalMacros.emplace_back("__UINTMAX_MAX__", cld::to_string(~0ull));

    std::size_t sizeTSize;
    switch (languageOptions.sizeTType)
    {
        case cld::LanguageOptions::SizeTType::UnsignedInt: sizeTSize = languageOptions.sizeOfInt; break;
        case cld::LanguageOptions::SizeTType::UnsignedLong: sizeTSize = languageOptions.sizeOfLong; break;
        case cld::LanguageOptions::SizeTType::UnsignedLongLong: sizeTSize = 8; break;
    }
    languageOptions.additionalMacros.emplace_back("__SIZE_MAX__",
                                                  cld::to_string(safeShiftLeft(1ull, (8 * sizeTSize)) - 1));
    std::size_t ptrDiffSize;
    switch (languageOptions.ptrdiffType)
    {
        case cld::LanguageOptions::PtrdiffType::Int: ptrDiffSize = languageOptions.sizeOfInt; break;
        case cld::LanguageOptions::PtrdiffType::Long: ptrDiffSize = languageOptions.sizeOfLong; break;
        case cld::LanguageOptions::PtrdiffType::LongLong: ptrDiffSize = 8; break;
    }
    languageOptions.additionalMacros.emplace_back("__PTRDIFF_MAX___",
                                                  cld::to_string(safeShiftLeft(1ull, (8 * ptrDiffSize - 1)) - 1));
    languageOptions.additionalMacros.emplace_back(
        "__INTPTR_MAX__", cld::to_string(safeShiftLeft(1ull, (8 * languageOptions.sizeOfVoidStar - 1)) - 1));
    languageOptions.additionalMacros.emplace_back(
        "__UNTPTR_MAX__", cld::to_string(safeShiftLeft(1ull, (8 * languageOptions.sizeOfVoidStar)) - 1));

    languageOptions.additionalMacros.emplace_back("__SIZEOF_DOUBLE__", "8");
    languageOptions.additionalMacros.emplace_back("__SIZEOF_FLOAT__", "4");
    languageOptions.additionalMacros.emplace_back("__SIZEOF_INT__", cld::to_string(languageOptions.sizeOfInt));
    languageOptions.additionalMacros.emplace_back("__SIZEOF_LONG__", cld::to_string(languageOptions.sizeOfLong));
    // TODO: Not always right
    switch (languageOptions.sizeOfLongDoubleBits)
    {
        case 64: languageOptions.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(8)); break;
        case 128: languageOptions.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(16)); break;
        case 80:
            if (triple.getArchitecture() == cld::Architecture::x86)
            {
                languageOptions.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(12));
            }
            else
            {
                languageOptions.additionalMacros.emplace_back("__SIZEOF_LONG_DOUBLE__", cld::to_string(16));
            }
    }
    languageOptions.additionalMacros.emplace_back("__SIZEOF_LONG_LONG__", "64");
    languageOptions.additionalMacros.emplace_back("__SIZEOF_POINTER__", cld::to_string(languageOptions.sizeOfVoidStar));
    languageOptions.additionalMacros.emplace_back("__SIZEOF_SHORT__", cld::to_string(languageOptions.sizeOfShort));
    languageOptions.additionalMacros.emplace_back("__SIZEOF_PTRDIFF_T__", cld::to_string(ptrDiffSize));
    languageOptions.additionalMacros.emplace_back("__SIZEOF_SIZE_T__", cld::to_string(sizeTSize));
    languageOptions.additionalMacros.emplace_back("__SIZEOF_WCHAR_T__", cld::to_string(wcharSize));

    languageOptions.additionalMacros.emplace_back("__INTMAX_TYPE__", "long long");
    languageOptions.additionalMacros.emplace_back("__INTMAX_C_SUFFIX__", "LL");
    languageOptions.additionalMacros.emplace_back("__INTMAX_WIDTH____", "64");
    languageOptions.additionalMacros.emplace_back("__UINTMAX_TYPE__", "unsigned long long");
    languageOptions.additionalMacros.emplace_back("__UINTMAX_C_SUFFIX__", "ULL");
    languageOptions.additionalMacros.emplace_back("__UINTMAX_WIDTH__", "64");
    switch (languageOptions.ptrdiffType)
    {
        case cld::LanguageOptions::PtrdiffType::Int:
            languageOptions.additionalMacros.emplace_back("__PTRDIFF_TYPE__", "int");
            break;
        case cld::LanguageOptions::PtrdiffType::Long:
            languageOptions.additionalMacros.emplace_back("__PTRDIFF_TYPE__", "long");
            break;
        case cld::LanguageOptions::PtrdiffType::LongLong:
            languageOptions.additionalMacros.emplace_back("__PTRDIFF_TYPE__", "long long");
            break;
    }
    languageOptions.additionalMacros.emplace_back("__PTRDIFF_WIDTH__", cld::to_string(ptrDiffSize * 8));
    switch (languageOptions.sizeTType)
    {
        case cld::LanguageOptions::SizeTType::UnsignedInt:
            languageOptions.additionalMacros.emplace_back("__SIZE_TYPE__", "unsigned int");
            break;
        case cld::LanguageOptions::SizeTType::UnsignedLong:
            languageOptions.additionalMacros.emplace_back("__SIZE_TYPE__", "unsigned long");
            break;
        case cld::LanguageOptions::SizeTType::UnsignedLongLong:
            languageOptions.additionalMacros.emplace_back("__SIZE_TYPE__", "unsigned long long");
            break;
    }
    languageOptions.additionalMacros.emplace_back("__SIZE_WIDTH__", cld::to_string(sizeTSize * 8));
    switch (languageOptions.wcharUnderlyingType)
    {
        case cld::LanguageOptions::WideCharType::UnsignedShort:
            languageOptions.additionalMacros.emplace_back("__WCHAR_TYPE__", "unsigned short");
            break;
        case cld::LanguageOptions::WideCharType::Int:
            languageOptions.additionalMacros.emplace_back("__WCHAR_TYPE__", "int");
            break;
    }
    languageOptions.additionalMacros.emplace_back("__WCHAR_WIDTH__", cld::to_string(wcharSize * 8));

    languageOptions.additionalMacros.emplace_back("__POINTER__WIDTH__",
                                                  cld::to_string(8 * languageOptions.sizeOfVoidStar));
    if (!languageOptions.charIsSigned)
    {
        languageOptions.additionalMacros.emplace_back("__CHAR_UNSIGNED__", "1");
    }

    switch (languageOptions.wcharUnderlyingType)
    {
        case cld::LanguageOptions::WideCharType::UnsignedShort:
            languageOptions.additionalMacros.emplace_back("__WCHAR_UNSIGNED__", "1");
        default: break;
    }

    languageOptions.additionalMacros.emplace_back("__FINITE_MATH_ONLY__", "0");
    languageOptions.additionalMacros.emplace_back("__GNUC_STDC_INLINE__", "1");
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
                                     OPT, DEFINE_MACRO, INCLUDES, PIE, PIC, FREESTANDING, HELP, VERSION>(elements);
    auto triple = cld::Triple::defaultTarget();
    if (cli.get<TARGET>())
    {
        triple = cld::Triple::fromString(*cli.get<TARGET>());
    }
    auto options = cld::LanguageOptions::fromTriple(triple);
    options.freeStanding = cli.get<FREESTANDING>().has_value();
    for (auto& iter : cli.get<INCLUDES>())
    {
        options.includeDirectories.emplace_back(iter);
    }
    for (auto& [name, maybeValue] : cli.get<DEFINE_MACRO>())
    {
        options.additionalMacros.emplace_back(name, maybeValue.value_or(""));
    }
    applyTargetSpecificLanguageOptions(options, triple, cli);

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
    if (cli.get<ASSEMBLY_OUTPUT>())
    {
        if (cli.get<PREPROCESS_ONLY>())
        {
            // TODO: Error
        }
        return doActionOnAllFiles(Action::AssemblyOutput, options, triple, cli.getUnrecognized(), cli);
    }

    return doActionOnAllFiles(Action::Preprocess, options, triple, cli.getUnrecognized(), cli);
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

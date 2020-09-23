#include <cld/Common/CommandLine.hpp>

#include <iostream>

CLD_CLI_OPT(outputFile, ("-o <file>", "--output=<file>"), (std::string_view, file))();

CLD_CLI_OPT(compileOnly, ("-c", "--compile"))();

CLD_CLI_OPT(defineMacro, ("-D<macro>=<value>", "-D<macro>", "--define-macro <macro>", "--define-macro=<macro>"),
            (std::string_view, macro), (std::string_view, value))
();

int main(int argc, char** argv)
{
    std::vector<std::string_view> elements(argc - 1);
    for (int i = 1; i < argc; i++)
    {
        elements[i - 1] = argv[i];
    }

    auto cli = cld::parseCommandLine<outputFile, compileOnly, defineMacro>(elements);

    std::string_view executable = "a.out";
    bool b = cli.get<compileOnly>().has_value();
    if (cli.get<outputFile>())
    {
        executable = *cli.get<outputFile>();
    }
    std::cout << executable << std::endl;
    if (cli.get<defineMacro>())
    {
        auto& [macroName, maybeValue] = *cli.get<defineMacro>();
        std::cout << macroName << std::endl;
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

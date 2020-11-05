#include <cld/cldMain/cldMain.hpp>

#include <string_view>
#include <vector>

int main(int argc, char** argv)
{
    std::vector<std::string_view> elements(argc - 1);
    for (int i = 1; i < argc; i++)
    {
        elements[i - 1] = argv[i];
    }
    return cld::main(elements);
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

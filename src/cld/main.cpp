#include <llvm/Support/raw_ostream.h>

#include <cld/Common/CommandLine.hpp>

#include <ctre.hpp>

constexpr static auto first = ::ctll::fixed_string{"-D<macro>=<value>"};
constexpr static auto second = ::ctll::fixed_string{"-D<macro>"};
constexpr static auto third = ::ctll::fixed_string{"--define-macro <macro>"};
constexpr static auto fourth = ::ctll::fixed_string{"--define-macro=<macro>"};

constexpr static auto name1 = ::ctll::fixed_string{"macro"};
constexpr static auto name2 = ::ctll::fixed_string{"value"};

constexpr static auto rdfg =
    cld::detail::CommandLine::parseOptions<cld::detail::CommandLine::Pack<first, second, third, fourth>, name1,
                                           std::in_place_type<std::string_view>, name2,
                                           std::in_place_type<std::string_view>>();

int main(int argc, char** argv)
{
    std::vector<std::string_view> elements(argc - 1);
    for (int i = 1; i < argc; i++)
    {
        elements[i - 1] = argv[i];
    }

    auto cli = cld::CommandLine::parse<rdfg>(elements);
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

#include "LanguageOptions.hpp"

#include <cld/Common/Util.hpp>

#include <limits>
#include <numeric>

#include "Diagnostic.hpp"

cld::LanguageOptions cld::LanguageOptions::native(Language language)
{
    auto temp = cld::LanguageOptions{
        language,
        sizeof(bool),
        std::is_signed_v<char>,
        sizeof(wchar_t) == 2 ? WideCharType::UnsignedShort : WideCharType ::Int,
        sizeof(short),
        sizeof(int),
        sizeof(long),
        []() -> std::uint8_t {
            switch (std::numeric_limits<long double>::digits)
            {
                case 53: return 64;
                case 64: return 80;
                case 113: return 128;
                default: CLD_UNREACHABLE;
            }
        }(),
        sizeof(void*),
#ifdef _WIN32
        1,
#else
        0,
#endif
        [] {
            static_assert(
                std::is_same_v<long long,
                               ptrdiff_t> || std::is_same_v<int, ptrdiff_t> || std::is_same_v<long, ptrdiff_t>);
            if constexpr (std::is_same_v<int, ptrdiff_t>)
            {
                return PtrdiffType::Int;
            }
            else if constexpr (std::is_same_v<long, ptrdiff_t>)
            {
                return PtrdiffType::Long;
            }
            else
            {
                return PtrdiffType::LongLong;
            }
        }(),
        [] {
            static_assert(std::is_same_v<
                              unsigned long long,
                              size_t> || std::is_same_v<unsigned int, size_t> || std::is_same_v<unsigned long, size_t>);
            if constexpr (std::is_same_v<unsigned int, size_t>)
            {
                return SizeTType::UnsignedInt;
            }
            else if constexpr (std::is_same_v<unsigned long, size_t>)
            {
                return SizeTType::UnsignedLong;
            }
            else
            {
                return SizeTType::UnsignedLongLong;
            }
        }(),
    };
    temp.enabledWarnings = cld::diag::getAllWarnings();
    return temp;
}

cld::LanguageOptions cld::LanguageOptions::fromTriple(Triple triple, Language language)
{
    LanguageOptions options;
    options.enabledWarnings = cld::diag::getAllWarnings();
    options.language = language;
    options.sizeOfUnderlineBool = 1;
    options.charIsSigned = true;
    if (triple.getPlatform() == Platform::Windows)
    {
        options.wcharUnderlyingType = WideCharType::UnsignedShort;
    }
    else
    {
        options.wcharUnderlyingType = WideCharType::Int;
    }
    options.sizeOfShort = 2;
    options.sizeOfInt = 4;
    if (triple.getPlatform() == Platform ::Windows || triple.getArchitecture() == Architecture::x86)
    {
        options.sizeOfLong = 4;
    }
    else
    {
        options.sizeOfLong = 8;
    }
    if (triple.getEnvironment() == Environment::MSVC)
    {
        options.sizeOfLongDoubleBits = 64;
    }
    else
    {
        options.sizeOfLongDoubleBits = 80;
    }
    if (triple.getArchitecture() == Architecture::x86)
    {
        options.sizeOfVoidStar = 4;
    }
    else
    {
        options.sizeOfVoidStar = 8;
    }
    options.discreteBitfields = triple.getPlatform() == Platform::Windows;
    if (triple.getArchitecture() == Architecture::x86)
    {
        options.ptrdiffType = PtrdiffType ::Int;
    }
    else if (triple.getPlatform() == Platform::Windows)
    {
        options.ptrdiffType = PtrdiffType ::LongLong;
    }
    else
    {
        options.ptrdiffType = PtrdiffType ::Long;
    }
    if (triple.getArchitecture() == Architecture::x86)
    {
        options.sizeTType = SizeTType ::UnsignedInt;
    }
    else if (triple.getPlatform() == Platform::Windows)
    {
        options.sizeTType = SizeTType ::UnsignedLongLong;
    }
    else
    {
        options.sizeTType = SizeTType ::UnsignedLong;
    }
    if (triple.getArchitecture() == Architecture::x86_64 && triple.getPlatform() != Platform::Windows)
    {
        options.vaListKind = BuiltInVaList::x86_64ABI;
    }
    else if ((triple.getArchitecture() == Architecture::x86_64 && triple.getPlatform() == Platform::Windows)
             || triple.getArchitecture() == Architecture::x86)
    {
        options.vaListKind = BuiltInVaList ::CharPtr;
    }
    return options;
}

#include "LanguageOptions.hpp"

#include <cld/Support/Util.hpp>

#include <limits>
#include <numeric>

#include "Diagnostic.hpp"
#include "Targets/X86TargetFeatures.hpp"

namespace
{
std::unique_ptr<cld::TargetFeatures> tripleToFeatures(const cld::Triple& triple)
{
    if (triple.getArchitecture() == cld::Architecture::x86 || triple.getArchitecture() == cld::Architecture::x86_64)
    {
        auto result = std::make_unique<cld::X86TargetFeatures>();
        if (triple.getArchitecture() == cld::Architecture::x86_64)
        {
            result->setFeature(cld::TargetFeatures::SSE, true);
            result->setFeature(cld::TargetFeatures::SSE2, true);
        }
        return result;
    }
    return std::make_unique<cld::DefaultTargetFeatures>();
}
} // namespace

cld::LanguageOptions cld::LanguageOptions::native(Language language)
{
    auto temp = cld::LanguageOptions{
        language,
        sizeof(bool),
        std::is_signed_v<char>,
        // wchar_t is a builtin type in C++, can't use underlyingType<wchar_t>
        sizeof(wchar_t) == 2 ? UnderlyingType::UnsignedShort : UnderlyingType ::Int,
        sizeof(short),
        sizeof(int),
        sizeof(long),
        alignof(long long),
        alignof(double),
        []() -> std::uint8_t {
            switch (std::numeric_limits<long double>::digits)
            {
                case 53: return 64;
                case 64: return 80;
                case 113: return 128;
                default: CLD_UNREACHABLE;
            }
        }(),
        alignof(long double),
        sizeof(void*),
#ifdef _WIN32
        1,
#else
        0,
#endif
        underlyingType<ptrdiff_t>(),
        underlyingType<size_t>(),
#ifdef __SIZEOF_INT128__
        true,
#else
        false,
#endif
    };
    temp.enabledWarnings = cld::diag::getAllWarnings();
    temp.targetFeatures = tripleToFeatures(cld::Triple::native());
    return temp;
}

cld::LanguageOptions cld::LanguageOptions::fromTriple(Triple triple, Language language)
{
    LanguageOptions options = native(language);
    options.targetFeatures = tripleToFeatures(triple);
    options.enabledWarnings = cld::diag::getAllWarnings();
    options.language = language;
    options.sizeOfUnderlineBool = 1;
    options.charIsSigned = true;
    if (triple.getPlatform() == Platform::Windows)
    {
        options.wcharUnderlyingType = UnderlyingType::UnsignedShort;
    }
    else
    {
        options.wcharUnderlyingType = UnderlyingType::Int;
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
    if (triple.getArchitecture() == Architecture::x86)
    {
        if (triple.getPlatform() == Platform::Windows)
        {
            options.alignOfDouble = 8;
        }
        else
        {
            options.alignOfDouble = 4;
        }
    }
    else
    {
        options.alignOfDouble = 8;
    }
    if (triple.getArchitecture() == Architecture::x86)
    {
        options.alignOfLongLong = 4;
    }
    else
    {
        options.alignOfLongLong = 8;
    }
    if (triple.getEnvironment() == Environment::MSVC)
    {
        options.alignOfLongDouble = 8;
        options.sizeOfLongDoubleBits = 64;
    }
    else
    {
        if (triple.getArchitecture() == Architecture::x86)
        {
            options.alignOfLongDouble = 4;
        }
        else
        {
            options.alignOfLongDouble = 16;
        }
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
        options.ptrdiffType = UnderlyingType::Int;
    }
    else if (triple.getPlatform() == Platform::Windows)
    {
        options.ptrdiffType = UnderlyingType ::LongLong;
    }
    else
    {
        options.ptrdiffType = UnderlyingType ::Long;
    }
    if (triple.getArchitecture() == Architecture::x86)
    {
        options.sizeTType = UnderlyingType ::UnsignedInt;
    }
    else if (triple.getPlatform() == Platform::Windows)
    {
        options.sizeTType = UnderlyingType ::UnsignedLongLong;
    }
    else
    {
        options.sizeTType = UnderlyingType ::UnsignedLong;
    }
    options.int128Enabled = options.sizeOfVoidStar >= 8;
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

bool cld::LanguageOptions::isSigned(cld::LanguageOptions::UnderlyingType type)
{
    switch (type)
    {
        case UnderlyingType::UnsignedShort:
        case UnderlyingType::UnsignedInt:
        case UnderlyingType::UnsignedLong:
        case UnderlyingType::UnsignedLongLong: return false;
        case UnderlyingType::Int:
        case UnderlyingType::Long:
        case UnderlyingType::LongLong: return true;
    }
    CLD_UNREACHABLE;
}

std::uint8_t cld::LanguageOptions::sizeOf(cld::LanguageOptions::UnderlyingType type) const
{
    switch (type)
    {
        case UnderlyingType::UnsignedShort: return sizeOfShort;
        case UnderlyingType::Int:
        case UnderlyingType::UnsignedInt: return sizeOfInt;
        case UnderlyingType::Long:
        case UnderlyingType::UnsignedLong: return sizeOfLong;
        case UnderlyingType::LongLong:
        case UnderlyingType::UnsignedLongLong: return 8;
    }
    CLD_UNREACHABLE;
}

std::uint8_t cld::LanguageOptions::alignOf(cld::LanguageOptions::UnderlyingType type) const
{
    switch (type)
    {
        case UnderlyingType::UnsignedShort: return sizeOfShort;
        case UnderlyingType::Int:
        case UnderlyingType::UnsignedInt: return sizeOfInt;
        case UnderlyingType::Long:
        case UnderlyingType::UnsignedLong: return sizeOfLong;
        case UnderlyingType::LongLong:
        case UnderlyingType::UnsignedLongLong: return alignOfLongLong;
    }
    CLD_UNREACHABLE;
}

std::string_view cld::LanguageOptions::string(cld::LanguageOptions::UnderlyingType type)
{
    switch (type)
    {
        case UnderlyingType::UnsignedShort: return "unsigned short";
        case UnderlyingType::Int: return "int";
        case UnderlyingType::UnsignedInt: return "unsigned int";
        case UnderlyingType::Long: return "long";
        case UnderlyingType::UnsignedLong: return "unsigned long";
        case UnderlyingType::LongLong: return "long long";
        case UnderlyingType::UnsignedLongLong: return "unsigned long long";
    }
    CLD_UNREACHABLE;
}

bool cld::DefaultTargetFeatures::hasFeature(cld::TargetFeatures::Features) const
{
    return false;
}

bool cld::DefaultTargetFeatures::setFeature(cld::TargetFeatures::Features, bool)
{
    return false;
}

#pragma once

#include <cstdint>
#include <string>
#include <unordered_set>
#include <vector>

namespace cld
{
struct LanguageOptions
{
    enum Language
    {
        C99,
        OpenCL1_2,
    };

    Language language;
    std::uint8_t sizeOfUnderlineBool;
    bool charIsSigned;
    std::uint8_t sizeOfWChar;
    bool wcharIsSigned;
    std::uint8_t sizeOfShort;
    std::uint8_t sizeOfInt;
    std::uint8_t sizeOfLong;
    std::uint8_t sizeOfLongDoubleBits;
    std::uint8_t sizeOfVoidStar;
    bool discreteBitfields;
    std::vector<std::string> includeDirectories{};
    std::vector<std::string> includeQuoteDirectories{};
    std::unordered_set<std::string> disabledWarnings{};

    static LanguageOptions native(Language language = Language::C99);
};
} // namespace cld

#pragma once

#include <cld/Common/Triple.hpp>

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
    enum class WideCharType
    {
        UnsignedShort,
        Int
    } wcharUnderlyingType;
    std::uint8_t sizeOfShort;
    std::uint8_t sizeOfInt;
    std::uint8_t sizeOfLong;
    std::uint8_t sizeOfLongDoubleBits;
    std::uint8_t sizeOfVoidStar;
    bool discreteBitfields;
    enum class PtrdiffType
    {
        Int,
        Long,
        LongLong,
    } ptrdiffType;
    enum class SizeTType
    {
        UnsignedInt,
        UnsignedLong,
        UnsignedLongLong,
    } sizeTType;
    std::vector<std::string> includeDirectories{};
    std::vector<std::string> includeQuoteDirectories{};
    std::vector<std::pair<std::string, std::string>> additionalMacros{};
    std::unordered_set<std::string> disabledWarnings{};
    bool freeStanding{};
    enum class Extension
    {
        None,
        GNU,
        Microsoft
    } extension{Extension::None};
    enum class BuiltInVaList
    {
        CharPtr,
        VoidPtr,
        x86_64ABI
    } vaListKind{};

    static LanguageOptions native(Language language = Language::C99);

    static LanguageOptions fromTriple(Triple triple, Language language = Language::C99);
};
} // namespace cld

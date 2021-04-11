#pragma once

#include <cld/Support/Triple.hpp>

#include <cstdint>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

namespace cld
{
class TargetFeatures
{
public:
    virtual ~TargetFeatures() = default;

    enum Features
    {
        IsX86,
    // x86 features
#define HANDLE_VALUE(x) x,
#include "Targets/X86Features.def"
    };

    virtual bool hasFeature(Features features) const = 0;

    virtual bool setFeature(Features features, bool value) = 0;
};

class DefaultTargetFeatures final : public TargetFeatures
{
public:
    bool hasFeature(Features features) const override;

    bool setFeature(Features features, bool value) override;
};

struct LanguageOptions
{
    enum Language
    {
        C99,
        OpenCL1_2,
    };

    enum class UnderlyingType
    {
        UnsignedShort,
        Int,
        UnsignedInt,
        Long,
        UnsignedLong,
        LongLong,
        UnsignedLongLong
    };

    static bool isSigned(UnderlyingType type);

    std::uint8_t sizeOf(UnderlyingType type) const;

    std::uint8_t alignOf(UnderlyingType type) const;

    static std::string_view string(UnderlyingType type);

    template <class T>
    static UnderlyingType underlyingType();

    Language language;
    Triple triple;
    std::uint8_t sizeOfUnderlineBool;
    bool charIsSigned;
    UnderlyingType wcharUnderlyingType;
    std::uint8_t sizeOfShort;
    std::uint8_t sizeOfInt;
    std::uint8_t sizeOfLong;
    std::uint8_t alignOfLongLong;
    std::uint8_t alignOfDouble;
    std::uint8_t sizeOfLongDoubleBits;
    std::uint8_t alignOfLongDouble;
    std::uint8_t sizeOfVoidStar;
    bool discreteBitfields;
    UnderlyingType ptrdiffType;
    UnderlyingType sizeTType;
    bool int128Enabled;

    std::shared_ptr<TargetFeatures> targetFeatures = std::make_shared<DefaultTargetFeatures>();
    std::unordered_set<std::string_view> enabledWarnings{};
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

template <class T>
LanguageOptions::UnderlyingType LanguageOptions::underlyingType()
{
    if constexpr (std::is_same_v<unsigned short, T>)
    {
        return UnderlyingType ::UnsignedShort;
    }
    else if constexpr (std::is_same_v<int, T>)
    {
        return UnderlyingType ::Int;
    }
    else if constexpr (std::is_same_v<unsigned int, T>)
    {
        return UnderlyingType ::UnsignedInt;
    }
    else if constexpr (std::is_same_v<long, T>)
    {
        return UnderlyingType ::Long;
    }
    else if constexpr (std::is_same_v<unsigned long, T>)
    {
        return UnderlyingType ::UnsignedLong;
    }
    else if constexpr (std::is_same_v<long long, T>)
    {
        return UnderlyingType ::LongLong;
    }
    else if constexpr (std::is_same_v<unsigned long long, T>)
    {
        return UnderlyingType ::UnsignedLongLong;
    }
    else
    {
        // Always false but it depends on type T
        static_assert(sizeof(T) < 0);
    }
}

} // namespace cld

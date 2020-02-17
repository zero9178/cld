#ifndef OPENCLPARSER_LANGUAGEOPTIONS_HPP
#define OPENCLPARSER_LANGUAGEOPTIONS_HPP

#include <cstdint>

namespace cld
{
class LanguageOptions
{
public:
    enum Language
    {
        C99,
        OpenCL1_2,
    };

private:
    Language m_language;
    std::uint8_t m_sizeOfUnderlineBool;
    bool m_charIsSigned;
    std::uint8_t m_sizeOfWChar;
    bool m_wcharIsSigned;
    std::uint8_t m_sizeOfShort;
    std::uint8_t m_sizeOfInt;
    std::uint8_t m_sizeOfLong;
    std::uint8_t m_sizeOfLongDoubleBits;
    std::uint8_t m_sizeOfVoidStar;

public:
    static LanguageOptions native(Language language = Language::C99);

    LanguageOptions(Language language, std::uint8_t sizeOfUnderlineBool, bool charIsSigned, std::uint8_t sizeOfWChar,
                    bool wcharIsSigned, std::uint8_t sizeOfShort, std::uint8_t sizeOfInt, std::uint8_t sizeOfLong,
                    std::uint8_t sizeOfLongDoubleBits, std::uint8_t sizeOfVoidStar);

    Language getLanguage() const;

    std::uint8_t getSizeOfUnderlineBool() const;

    bool isCharSigned() const;

    std::uint8_t getSizeOfWChar() const;

    bool isWCharSigned() const;

    std::uint8_t getSizeOfShort() const;

    std::uint8_t getSizeOfInt() const;

    std::uint8_t getSizeOfLong() const;

    std::uint8_t getSizeOfLongLong() const;

    std::uint8_t getSizeOfLongDoubleBits() const;

    std::uint8_t getSizeOfVoidStar() const;
};
} // namespace cld

#endif // OPENCLPARSER_LANGUAGEOPTIONS_HPP

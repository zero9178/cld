#include "LanguageOptions.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <cassert>
#include <limits>

OpenCL::LanguageOptions::LanguageOptions(Language language, std::uint8_t sizeOfUnderlineBool, bool charIsSigned,
                                         std::uint8_t sizeOfWChar, std::uint8_t sizeOfShort, std::uint8_t sizeOfInt,
                                         std::uint8_t sizeOfLong, std::uint8_t sizeOfLongDoubleBits,
                                         std::uint8_t sizeOfVoidStar)
    : m_language(language),
      m_sizeOfUnderlineBool(sizeOfUnderlineBool),
      m_charIsSigned(charIsSigned),
      m_sizeOfWChar(sizeOfWChar),
      m_sizeOfShort(sizeOfShort),
      m_sizeOfInt(sizeOfInt),
      m_sizeOfLong(sizeOfLong),
      m_sizeOfLongDoubleBits(sizeOfLongDoubleBits),
      m_sizeOfVoidStar(sizeOfVoidStar)
{
    assert(m_sizeOfShort >= 2);
    assert(m_sizeOfInt >= 2);
    assert(m_sizeOfShort <= m_sizeOfInt);
    assert(m_sizeOfLong >= 4);
    assert(m_sizeOfInt <= m_sizeOfLong);

    assert(m_sizeOfUnderlineBool <= 8);
    assert(m_sizeOfShort <= 8);
    assert(m_sizeOfWChar <= 8);
    assert(m_sizeOfInt <= 8);
    assert(m_sizeOfLong <= 8);

    assert(m_sizeOfLongDoubleBits == 64 || m_sizeOfLongDoubleBits == 80 || m_sizeOfLongDoubleBits == 128);
}

OpenCL::LanguageOptions::Language OpenCL::LanguageOptions::getLanguage() const
{
    return m_language;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfUnderlineBool() const
{
    return m_sizeOfUnderlineBool;
}

bool OpenCL::LanguageOptions::isCharIsSigned() const
{
    return m_charIsSigned;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfWChar() const
{
    return m_sizeOfWChar;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfShort() const
{
    return m_sizeOfShort;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfInt() const
{
    return m_sizeOfInt;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfLong() const
{
    return m_sizeOfLong;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfLongLong() const
{
    return 64;
}

OpenCL::LanguageOptions OpenCL::LanguageOptions::native(Language language)
{
    return OpenCL::LanguageOptions(
        language, sizeof(bool), std::numeric_limits<char>::is_signed, sizeof(wchar_t), sizeof(short), sizeof(int),
        sizeof(long),
        []() -> std::uint8_t {
            switch (std::numeric_limits<long double>::digits)
            {
                case 53: return 64;
                case 64: return 80;
                case 113: return 128;
                default: OPENCL_UNREACHABLE;
            }
        }(),
        sizeof(void*));
#pragma clang diagnostic pop
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfLongDoubleBits() const
{
    return m_sizeOfLongDoubleBits;
}

std::uint8_t OpenCL::LanguageOptions::getSizeOfVoidStar() const
{
    return m_sizeOfVoidStar;
}

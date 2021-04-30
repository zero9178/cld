#pragma once

#include <llvm/Support/ConvertUTF.h>
#include <llvm/Support/Unicode.h>

#include <cstdint>
#include <cstring>
#include <string>
#include <string_view>

#include "Constexpr.hpp"
#include "MaxVector.hpp"
#include "Util.hpp"

namespace llvm
{
class APSInt;
class APFloat;
} // namespace llvm

namespace cld
{
bool isWhitespace(std::uint32_t c) noexcept;

template <class Iter>
std::uint8_t getNumBytesForUTF8(Iter begin, Iter end)
{
    CLD_ASSERT(begin != end);
    std::uint8_t des;
    std::int8_t temp = *begin;
    std::memcpy(&des, &temp, 1);
    auto step = llvm::getNumBytesForUTF8(*begin);
    for (std::uint8_t i = 1; i < step; i++)
    {
        if (begin + i == end || (*(begin + i) & 0b11000000) != 0b10000000)
        {
            return i;
        }
    }
    return step;
}

template <class OutputIter1, class OutputIterator2>
void toSafeUTF8(std::string_view inputLine, OutputIter1 outputIter, OutputIterator2 mappingIter)
{
    constexpr static std::string_view REPLACEMENT_CHARACTER = "\xEF\xBF\xBD";
    std::int64_t delta = 0;
    auto sourceStart = inputLine.data();
    const auto sourceEnd = inputLine.data() + inputLine.size();
    while (sourceStart != sourceEnd)
    {
        do
        {
            llvm::UTF32 utf32;
            const auto prev = sourceStart;
            if (llvm::convertUTF8Sequence(reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                          reinterpret_cast<const llvm::UTF8*>(sourceEnd), &utf32,
                                          llvm::strictConversion)
                != llvm::conversionOK)
            {
                break;
            }
            mappingIter = std::fill_n(mappingIter, std::distance(prev, sourceStart), delta);
            if (cld::isWhitespace(utf32) || llvm::sys::unicode::isPrintable(utf32))
            {
                outputIter = std::copy(prev, sourceStart, outputIter);
                continue;
            }

            delta += 3 - std::distance(prev, sourceStart);
            if (utf32 <= 0x1F)
            {
                char nullControlCharacter[] = {static_cast<char>(0xE2), static_cast<char>(0x90),
                                               static_cast<char>(0x80)};
                nullControlCharacter[2] |= utf32;
                outputIter = std::copy(std::begin(nullControlCharacter), std::end(nullControlCharacter), outputIter);
            }
            else
            {
                outputIter = std::copy(REPLACEMENT_CHARACTER.begin(), REPLACEMENT_CHARACTER.end(), outputIter);
            }
        } while (sourceStart != sourceEnd);

        if (sourceStart != sourceEnd)
        {
            outputIter = std::copy(REPLACEMENT_CHARACTER.begin(), REPLACEMENT_CHARACTER.end(), outputIter);
            const auto step = cld::getNumBytesForUTF8(sourceStart, sourceEnd);
            mappingIter = std::fill_n(mappingIter, step, delta);
            delta += 3 - step;
            sourceStart += step;
        }
    }

    *mappingIter = delta;
}

int unsafeCharWidth(std::uint32_t UCS);

/**
 * Returns an approximation for the width of the UTF8 text on a terminal. Passing Invalid UTF-8 or Non printable
 * Characters is UB
 * @param text
 * @return
 */
unsigned unsafeColumnWidth(std::string_view text);

template <class BackInserter>
BackInserter stringOfSameWidth(std::string_view original, char characterToReplace, BackInserter backInserter)
{
    auto utf8Width = unsafeColumnWidth(original);
    return std::fill_n(backInserter, utf8Width, characterToReplace);
}

inline std::string to_string(std::string_view stringView)
{
    return std::string(stringView.begin(), stringView.end());
}

namespace Constexpr
{
template <std::size_t maxSize>
constexpr MaxVector<char, maxSize> utf32ToUtf8(std::u32string_view stringView)
{
    MaxVector<char, maxSize> result;
    for (std::size_t i = 0; i < stringView.size(); i++)
    {
        char32_t code = stringView[i];
        if (code <= 0x7F)
        {
            result.push_back(code);
        }
        else if (code <= 0x7FF)
        {
            result.push_back(0xC0 | (code >> 6));
            result.push_back(0x80 | (code & 0x3F));
        }
        else if (code <= 0xFFFF)
        {
            result.push_back(0xE0 | (code >> 12));
            result.push_back(0x80 | ((code >> 6) & 0x3F));
            result.push_back(0x80 | (code & 0x3F));
        }
        else if (code <= 0x10FFFF)
        {
            result.push_back(0xF0 | (code >> 18));
            result.push_back(0x80 | ((code >> 12) & 0x3F));
            result.push_back(0x80 | ((code >> 6) & 0x3F));
            result.push_back(0x80 | (code & 0x3F));
        }
    }
    return result;
}

} // namespace Constexpr

template <class T, class = void>
struct ToString : std::false_type
{
};

template <class T>
struct ToString<T, std::void_t<decltype(std::to_string(std::declval<T>()))>> : std::true_type
{
};

template <class T, class = std::enable_if_t<ToString<T>{}>>
std::string to_string(T value)
{
    return std::to_string(value);
}

inline std::string to_string(std::u8string_view u8string)
{
    std::string result(u8string.size(), '\0');
    std::copy(u8string.begin(), u8string.end(), result.begin());
    return result;
}

std::string to_string(const llvm::APSInt& apsInt);

std::string to_string(const llvm::APFloat& apFloat);

inline std::u8string to_u8string(std::string_view view)
{
    std::u8string string(view.size(), u8'\0');
    std::copy(view.begin(), view.end(), string.begin());
    return string;
}

} // namespace cld

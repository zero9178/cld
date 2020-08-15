#include "LanguageOptions.hpp"

#include <Frontend/Common/Util.hpp>

#include <limits>
#include <numeric>

cld::LanguageOptions cld::LanguageOptions::native(Language language)
{
    return cld::LanguageOptions{
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
}

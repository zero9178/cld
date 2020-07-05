#include "LanguageOptions.hpp"

#include <Frontend/Common/Util.hpp>

#include <limits>
#include <numeric>

cld::LanguageOptions cld::LanguageOptions::native(Language language)
{
    return cld::LanguageOptions{language,
                                sizeof(bool),
                                std::is_signed_v<char>,
                                sizeof(wchar_t),
                                std::is_signed_v<wchar_t>,
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
                                {},
                                {}};
}

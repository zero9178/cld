#pragma once

#include <Frontend/Compiler/LanguageOptions.hpp>

namespace cld::Tests
{
const inline auto x64windowsGnu = LanguageOptions{LanguageOptions::C99,
                                                  1,
                                                  true,
                                                  LanguageOptions::WideCharType::UnsignedShort,
                                                  2,
                                                  4,
                                                  4,
                                                  80,
                                                  8,
                                                  true,
                                                  cld::LanguageOptions::PtrdiffType::LongLong,
                                                  cld::LanguageOptions::SizeTType::UnsignedLongLong};
const inline auto x86windowsGnu = LanguageOptions{LanguageOptions::C99,
                                                  1,
                                                  true,
                                                  LanguageOptions::WideCharType::UnsignedShort,
                                                  2,
                                                  4,
                                                  4,
                                                  80,
                                                  4,
                                                  true,
                                                  cld::LanguageOptions::PtrdiffType::Int,
                                                  cld::LanguageOptions::SizeTType::UnsignedInt};
const inline auto x64windowsMsvc = LanguageOptions{LanguageOptions::C99,
                                                   1,
                                                   true,
                                                   LanguageOptions::WideCharType::UnsignedShort,
                                                   2,
                                                   4,
                                                   4,
                                                   64,
                                                   8,
                                                   true,
                                                   cld::LanguageOptions::PtrdiffType::LongLong,
                                                   cld::LanguageOptions::SizeTType::UnsignedLongLong};
const inline auto x86windowsMsvc = LanguageOptions{LanguageOptions::C99,
                                                   1,
                                                   true,
                                                   LanguageOptions::WideCharType::UnsignedShort,
                                                   2,
                                                   4,
                                                   4,
                                                   64,
                                                   4,
                                                   true,
                                                   cld::LanguageOptions::PtrdiffType::Int,
                                                   cld::LanguageOptions::SizeTType::UnsignedInt};
const inline auto x64linux = LanguageOptions{LanguageOptions::C99,
                                             1,
                                             true,
                                             LanguageOptions::WideCharType::Int,
                                             2,
                                             4,
                                             8,
                                             80,
                                             8,
                                             false,
                                             cld::LanguageOptions::PtrdiffType::Long,
                                             cld::LanguageOptions::SizeTType::UnsignedLong};
const inline auto x86linux = LanguageOptions{LanguageOptions::C99,
                                             1,
                                             true,
                                             LanguageOptions::WideCharType::Int,
                                             2,
                                             4,
                                             4,
                                             80,
                                             4,
                                             false,
                                             cld::LanguageOptions::PtrdiffType::Int,
                                             cld::LanguageOptions::SizeTType::UnsignedInt};

} // namespace cld::Tests
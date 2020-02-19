#ifndef OPENCLPARSER_TESTCONFIG_HPP
#define OPENCLPARSER_TESTCONFIG_HPP

#pragma warning(push, 0)
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>
#pragma warning(pop)

#include <CompilerCore/C/LanguageOptions.hpp>
#include <CompilerCore/C/Semantics.hpp>

namespace cld::Tests
{
const inline auto x64windowsGnu = LanguageOptions(cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 8);
const inline auto x86windowsGnu = LanguageOptions(cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 4);
const inline auto x64windowsMsvc = LanguageOptions(cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 8);
const inline auto x86windowsMsvc = LanguageOptions(cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 4);
const inline auto x64linux = LanguageOptions(cld::LanguageOptions::C99, 1, true, 4, true, 2, 4, 8, 80, 8);
const inline auto x86linux = LanguageOptions(cld::LanguageOptions::C99, 1, true, 4, true, 2, 4, 4, 80, 4);
} // namespace cld::Tests

namespace Catch
{
template <>
struct StringMaker<llvm::APSInt>
{
    static std::string convert(const llvm::APSInt& value)
    {
        return "APSInt{" + std::to_string(value.getBitWidth()) + " bits, " + value.toString(10) + "}";
    }
};

template <>
struct StringMaker<llvm::APInt>
{
    static std::string convert(const llvm::APSInt& value)
    {
        return "APInt{" + std::to_string(value.getBitWidth()) + " bits, " + value.toString(10) + "}";
    }
};

template <>
struct StringMaker<cld::Semantics::Type>
{
    static std::string convert(const cld::Semantics::Type& type)
    {
        return "'" + type.getFullFormattedTypeName() + "'";
    }
};
} // namespace Catch

#endif // OPENCLPARSER_TESTCONFIG_HPP

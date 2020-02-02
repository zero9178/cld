#ifndef OPENCLPARSER_TESTCONFIG_HPP
#define OPENCLPARSER_TESTCONFIG_HPP

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>

#include <CompilerCore/C/LanguageOptions.hpp>

namespace OpenCL::Tests
{
const inline auto x64windowsGnu = LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 8);
const inline auto x64windowsMsvc = LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 8);
const inline auto x64linux = LanguageOptions(OpenCL::LanguageOptions::C99, 1, true, 4, true, 2, 4, 8, 80, 8);
} // namespace OpenCL::Tests

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
} // namespace Catch

#endif // OPENCLPARSER_TESTCONFIG_HPP

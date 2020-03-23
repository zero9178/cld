#pragma once

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

struct ProducesError : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesError(const std::string& comparator)
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("error: " + comparator, Catch::CaseSensitive::Yes))
    {
    }
};

struct ProducesNoErrors : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNoErrors() : ContainsMatcher(Catch::Matchers::StdString::CasedString("error: ", Catch::CaseSensitive::Yes))
    {
    }

    bool match(const std::string& source) const override
    {
        return !ContainsMatcher::match(source);
    }

    std::string describe() const override
    {
        return "Produces no errors";
    }
};

struct ProducesNote : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNote(const std::string& comparator)
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("note: " + comparator, Catch::CaseSensitive::Yes))
    {
    }
};

struct ProducesNoNotes : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNoNotes() : ContainsMatcher(Catch::Matchers::StdString::CasedString("note: ", Catch::CaseSensitive::Yes)) {}

    bool match(const std::string& source) const override
    {
        return !ContainsMatcher::match(source);
    }

    std::string describe() const override
    {
        return "Produces no notes";
    }
};

struct ProducesWarning : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesWarning(const std::string& comparator)
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("warning: " + comparator, Catch::CaseSensitive::Yes))
    {
    }
};

struct ProducesNoWarnings : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNoWarnings()
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("warning: ", Catch::CaseSensitive::Yes))
    {
    }

    bool match(const std::string& source) const override
    {
        return !ContainsMatcher::match(source);
    }

    std::string describe() const override
    {
        return "Produces no warnings";
    }
};

#define producesNothing() ProducesNoErrors() && ProducesNoNotes() && ProducesNoWarnings()

} // namespace cld::Tests

// IT'S ONLY FOR TESTS I SWEAR. It's in a namepspace for mangling
using namespace cld::Tests;

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

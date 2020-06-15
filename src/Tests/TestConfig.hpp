#pragma once

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>

#include <CompilerCore/C/LanguageOptions.hpp>
#include <CompilerCore/C/Semantics.hpp>

#include <numeric>

namespace cld::Tests
{
const inline auto x64windowsGnu = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 8};
const inline auto x86windowsGnu = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 4};
const inline auto x64windowsMsvc = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 8};
const inline auto x86windowsMsvc = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 4};
const inline auto x64linux = LanguageOptions{cld::LanguageOptions::C99, 1, true, 4, true, 2, 4, 8, 80, 8};
const inline auto x86linux = LanguageOptions{cld::LanguageOptions::C99, 1, true, 4, true, 2, 4, 4, 80, 4};

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

#define ProducesNothing() ProducesNoErrors() && ProducesNoNotes() && ProducesNoWarnings()

// Matcher that works almost like == of two strings except it doesn't care of any leading whitespaces per line
struct ProducesLines : Catch::MatcherBase<std::string_view>
{
    ProducesLines(std::string_view text)
    {
        std::size_t result = 0;
        while (true)
        {
            auto newline = text.find('\n', result);
            if (newline == std::string_view::npos)
            {
                m_linesTrimmed.emplace_back(trimEnd(text.substr(result)));
                break;
            }
            m_linesTrimmed.emplace_back(trimEnd(text.substr(result, newline - result)));
            result = newline + 1;
        }
    }

    std::string describe() const override
    {
        return "lines match \""
               + std::accumulate(m_linesTrimmed.begin(), m_linesTrimmed.end(), std::string(),
                                 [first = true](const std::string& lhs, const std::string& rhs) mutable {
                                     if (!first)
                                     {
                                         return lhs + '\n' + rhs;
                                     }
                                     else
                                     {
                                         first = false;
                                         return rhs;
                                     }
                                 })
               + '"';
    }

    bool match(const std::string_view& source) const override
    {
        std::size_t i = 0;
        std::size_t result = 0;
        while (i < m_linesTrimmed.size())
        {
            auto newline = source.find('\n', result);
            if (newline == std::string_view::npos)
            {
                return trimEnd(source.substr(result)) == m_linesTrimmed[i] && i + 1 == m_linesTrimmed.size();
            }
            if (trimEnd(source.substr(result, newline - result)) != m_linesTrimmed[i])
            {
                return false;
            }
            result = newline + 1;
            i++;
        }
        auto leftOver = source.substr(result);
        return std::all_of(leftOver.begin(), leftOver.end(), [](unsigned char c) { return std::isspace(c & 0x7F); });
    }

private:
    static std::string_view trimEnd(std::string_view text)
    {
        // ASCII should hopefully suffice for our tests... Otherwise I will have to properly parse it to UTF32
        // and use one of the whitespace sets of the Lexer to match whitespace characters
        auto end =
            std::find_if(std::make_reverse_iterator(text.end()), std::make_reverse_iterator(text.begin()), [](char c) {
                return c != ' ' && c != '\t';
            }).base();
        return text.substr(0, end - text.begin());
    }

    std::vector<std::string> m_linesTrimmed;
};

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

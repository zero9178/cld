#pragma once

#include <cstdint>
#include <variant>

#include <tcb/span.hpp>

#include "LanguageOptions.hpp"

namespace cld
{
namespace Lexer
{
class TokenBase;
using IntervalMap = std::vector<std::tuple<std::uint64_t, std::uint64_t, std::uint64_t>>;
} // namespace Lexer

namespace Source
{
struct File;
struct Substitution;
struct Stringification;
struct TokenConcatenation;
using PPRecord =
    std::variant<std::monostate, Source::Substitution, Source::Stringification, Source::TokenConcatenation>;
} // namespace Source

class SourceInterface
{
public:
    SourceInterface() = default;
    virtual ~SourceInterface() = default;
    SourceInterface(const SourceInterface&) = default;
    SourceInterface& operator=(const SourceInterface&) = default;
    SourceInterface(SourceInterface&&) noexcept = default;
    SourceInterface& operator=(SourceInterface&&) noexcept = default;

    [[nodiscard]] virtual std::uint64_t getLineNumber(std::uint32_t fileID, std::uint64_t offset) const noexcept = 0;

    [[nodiscard]] virtual std::uint64_t getLineStartOffset(std::uint32_t fileID, std::uint64_t line) const noexcept = 0;

    [[nodiscard]] virtual std::uint64_t getLineEndOffset(std::uint32_t fileID, std::uint64_t line) const noexcept = 0;

    [[nodiscard]] virtual tcb::span<const Source::File> getFiles() const noexcept = 0;

    [[nodiscard]] virtual tcb::span<const Source::PPRecord> getSubstitutions() const noexcept = 0;

    [[nodiscard]] virtual const LanguageOptions& getLanguageOptions() const noexcept = 0;
};

class PPSourceInterface : virtual public SourceInterface
{
public:
    PPSourceInterface() = default;
    virtual ~PPSourceInterface() = default;
    PPSourceInterface(const PPSourceInterface&) = default;
    PPSourceInterface& operator=(const PPSourceInterface&) = default;
    PPSourceInterface(PPSourceInterface&&) noexcept = default;
    PPSourceInterface& operator=(PPSourceInterface&&) noexcept = default;

    [[nodiscard]] virtual tcb::span<const Lexer::IntervalMap> getIntervalMaps() const noexcept = 0;
};
} // namespace cld

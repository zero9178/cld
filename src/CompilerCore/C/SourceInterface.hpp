#pragma once

#include <cstdint>
#include <vector>

namespace cld
{
namespace Lexer
{
class TokenBase;
} // namespace Lexer

namespace Source
{
struct File;
struct Substitution;
struct Stringification;
struct TokenConcatenation;
using PPRecord = std::vector<
    std::variant<std::monostate, Source::Substitution, Source::Stringification, Source::TokenConcatenation>>;
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

    [[nodiscard]] virtual const std::vector<Source::File>& getFiles() const noexcept = 0;

    [[nodiscard]] virtual const Source::PPRecord& getSubstitutions() const noexcept = 0;
};
} // namespace cld

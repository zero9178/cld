#pragma once

#include <cstdint>
#include <vector>

namespace cld
{
namespace Lexer
{
enum class FileID : std::uint32_t;

class TokenBase;
} // namespace Lexer

namespace Source
{
struct File;
struct Substitution;
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

    [[nodiscard]] virtual std::uint64_t getLineNumber(Lexer::FileID fileID, std::uint64_t offset) const noexcept = 0;

    [[nodiscard]] virtual std::uint64_t getLineStartOffset(Lexer::FileID fileID, std::uint64_t line) const noexcept = 0;

    [[nodiscard]] virtual std::uint64_t getLineEndOffset(Lexer::FileID fileID, std::uint64_t line) const noexcept = 0;

    [[nodiscard]] virtual const std::vector<Source::File>& getFiles() const noexcept = 0;

    [[nodiscard]] virtual const std::vector<Source::Substitution>& getSubstitutions() const noexcept = 0;
};
} // namespace cld

#pragma once

#include <cstdint>
#include <vector>

namespace cld
{
namespace Lexer
{
enum class FileID : std::uint64_t;

class TokenBase;
} // namespace Lexer

namespace Source
{
struct File;
}

class SourceInterface
{
    [[nodiscard]] virtual const Lexer::TokenBase* inc(const Lexer::TokenBase* ptr) const noexcept = 0;

    [[nodiscard]] virtual const Lexer::TokenBase* dec(const Lexer::TokenBase* ptr) const noexcept = 0;

    friend class Message;

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

    [[nodiscard]] virtual std::uint64_t getPPLineNumber(std::uint64_t offset) const noexcept = 0;

    [[nodiscard]] virtual std::uint64_t getPPLineStartOffset(std::uint64_t line) const noexcept = 0;

    [[nodiscard]] virtual std::uint64_t getPPLineEndOffset(std::uint64_t line) const noexcept = 0;

    [[nodiscard]] virtual const std::vector<Source::File>& getFiles() const noexcept = 0;
};
} // namespace cld

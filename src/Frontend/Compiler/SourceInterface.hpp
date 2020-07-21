#pragma once

#include <llvm/ADT/ArrayRef.h>

#include <cstdint>
#include <variant>

#include "LanguageOptions.hpp"

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

    [[nodiscard]] virtual llvm::ArrayRef<Source::File> getFiles() const noexcept = 0;

    [[nodiscard]] virtual llvm::ArrayRef<Source::PPRecord> getSubstitutions() const noexcept = 0;

    [[nodiscard]] virtual const LanguageOptions& getLanguageOptions() const noexcept = 0;
};
} // namespace cld

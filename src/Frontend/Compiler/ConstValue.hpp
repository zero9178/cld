#pragma once

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>

#include <variant>

#include "CustomDiag.hpp"

namespace cld::Semantics
{
class Type;
class SemanticAnalysis;

struct VoidStar final
{
    std::uint64_t address;
    std::uint32_t elementSize;
};

struct AddressConstant
{
};

class ConstValue final
{
public:
    using ValueType = std::variant<std::monostate, llvm::APSInt, llvm::APFloat, VoidStar, AddressConstant>;

private:
    ValueType m_value;

public:
    enum Issue
    {
        NoIssue,
        NotRepresentable
    };

    ConstValue() = default;

    /*implicit*/ ConstValue(const ValueType& value);

    [[nodiscard]] const ValueType& getValue() const
    {
        return m_value;
    }

    [[nodiscard]] bool isUndefined() const;

    ConstValue negate(const LanguageOptions& options) const;

    ConstValue logicalNegate(const LanguageOptions& options) const;

    ConstValue bitwiseNegate(const LanguageOptions& options) const;

    [[nodiscard]] ConstValue castTo(const Type& type, const SemanticAnalysis* analysis, const LanguageOptions& options,
                                    Issue* issues = nullptr) const;

    ConstValue multiply(const ConstValue& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstValue divide(const ConstValue& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstValue modulo(const ConstValue& rhs, const LanguageOptions& options) const;

    // These are different due to the need of calling sizeof

    ConstValue plus(const ConstValue& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstValue minus(const ConstValue& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstValue shiftLeft(const ConstValue& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstValue shiftRight(const ConstValue& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstValue bitAnd(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue bitXor(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue bitOr(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue lessThan(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue greaterThan(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue lessOrEqual(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue greaterOrEqual(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue equal(const ConstValue& rhs, const LanguageOptions& options) const;

    ConstValue notEqual(const ConstValue& rhs, const LanguageOptions& options) const;

    [[nodiscard]] ConstValue toBool(const LanguageOptions& options) const;

    explicit operator bool() const;

    std::int64_t toInt() const;

    std::uint64_t toUInt() const;

    std::string toString() const;
};
} // namespace cld::Semantics

namespace cld::diag
{
template <>
struct StringConverter<Semantics::ConstValue>
{
    static std::string inFormat(const Semantics::ConstValue& arg, const SourceInterface&);

    static std::string inArg(const Semantics::ConstValue& arg, const SourceInterface&);
};

} // namespace cld::diag

#pragma once

#include <Frontend/Common/Expected.hpp>

#include <functional>

#include "Message.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace cld::Semantics
{
struct VoidStar final
{
    std::uint64_t address;
};

class ConstRetType final
{
public:
    using ValueType = std::variant<std::monostate, llvm::APSInt, llvm::APFloat, VoidStar>;

private:
    ValueType m_value;
    Type m_type;

    ConstRetType integerPromotion(const LanguageOptions& options) const;

    static std::pair<ConstRetType, ConstRetType> arithmeticConversions(ConstRetType lhs, ConstRetType rhs,
                                                                       const LanguageOptions& options);

public:
    enum Issue
    {
        NoIssue,
        NotRepresentable
    };

    ConstRetType() = default;

    /* implicit */ ConstRetType(const ValueType& value, const Type& type);

    [[nodiscard]] const Type& getType() const
    {
        return m_type;
    }

    [[nodiscard]] const ValueType& getValue() const
    {
        return m_value;
    }

    [[nodiscard]] bool isUndefined() const;

    ConstRetType unaryPlus(const LanguageOptions& options) const;

    ConstRetType negate(const LanguageOptions& options) const;

    ConstRetType logicalNegate(const LanguageOptions& options) const;

    ConstRetType bitwiseNegate(const LanguageOptions& options) const;

    [[nodiscard]] ConstRetType castTo(const Type& type, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstRetType multiply(const ConstRetType& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstRetType divide(const ConstRetType& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstRetType modulo(const ConstRetType& rhs, const LanguageOptions& options) const;

    // These are different due to the need of calling sizeof

    ConstRetType plus(const ConstRetType& rhs, const LanguageOptions& options,
                      const SemanticAnalysis* analysis = nullptr, Issue* issues = nullptr) const;

    ConstRetType minus(const ConstRetType& rhs, const LanguageOptions& options,
                       const SemanticAnalysis* analysis = nullptr, Issue* issues = nullptr) const;

    ConstRetType shiftLeft(const ConstRetType& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstRetType shiftRight(const ConstRetType& rhs, const LanguageOptions& options, Issue* issues = nullptr) const;

    ConstRetType bitAnd(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType bitXor(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType bitOr(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType lessThan(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType greaterThan(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType lessOrEqual(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType greaterOrEqual(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType equal(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType notEqual(const ConstRetType& rhs, const LanguageOptions& options) const;

    [[nodiscard]] ConstRetType toBool(const LanguageOptions& options) const;

    explicit operator bool() const;

    std::int64_t toInt() const;

    std::uint64_t toUInt() const;

    std::string toString() const;
};

class ConstantEvaluator final
{
public:
    enum Mode
    {
        Integer,
        Arithmetic,
        Initialization
    };
};

} // namespace cld::Semantics

namespace cld::diag
{
template <>
struct StringConverter<Semantics::ConstRetType>
{
    static std::string inFormat(const Semantics::ConstRetType& arg, const SourceInterface&);

    static std::string inArg(const Semantics::ConstRetType& arg, const SourceInterface&);
};

} // namespace cld::diag

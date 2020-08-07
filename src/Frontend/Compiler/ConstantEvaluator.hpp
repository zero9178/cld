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
    enum Issues
    {
        NoIssues,
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

    [[nodiscard]] bool isInteger() const;

    [[nodiscard]] bool isArithmetic() const;

    [[nodiscard]] bool isUndefined() const;

    ConstRetType unaryPlus(const LanguageOptions& options) const;

    ConstRetType negate(const LanguageOptions& options) const;

    ConstRetType logicalNegate(const LanguageOptions& options) const;

    ConstRetType bitwiseNegate(const LanguageOptions& options) const;

    [[nodiscard]] ConstRetType castTo(const Type& type, const LanguageOptions& options, Issues* issues = nullptr) const;

    ConstRetType multiply(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr) const;

    ConstRetType& multiplyAssign(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr);

    ConstRetType divide(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr) const;

    ConstRetType& divideAssign(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr);

    ConstRetType modulo(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType& moduloAssign(const ConstRetType& rhs, const LanguageOptions& options);

    // These are different due to the need of calling sizeof

    ConstRetType plus(const ConstRetType& rhs, const LanguageOptions& options,
                      const SemanticAnalysis* analysis = nullptr, Issues* issues = nullptr) const;

    ConstRetType& plusAssign(const ConstRetType& rhs, const LanguageOptions& options,
                             const SemanticAnalysis* analysis = nullptr, Issues* issues = nullptr);

    ConstRetType minus(const ConstRetType& rhs, const LanguageOptions& options,
                       const SemanticAnalysis* analysis = nullptr, Issues* issues = nullptr) const;

    ConstRetType& minusAssign(const ConstRetType& rhs, const LanguageOptions& options,
                              const SemanticAnalysis* analysis = nullptr, Issues* issues = nullptr);

    ConstRetType shiftLeft(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr) const;

    ConstRetType& shiftLeftAssign(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr);

    ConstRetType shiftRight(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr) const;

    ConstRetType& shiftRightAssign(const ConstRetType& rhs, const LanguageOptions& options, Issues* issues = nullptr);

    ConstRetType bitAnd(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType& bitAndAssign(const ConstRetType& rhs, const LanguageOptions& options);

    ConstRetType bitXor(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType& bitXorAssign(const ConstRetType& rhs, const LanguageOptions& options);

    ConstRetType bitOr(const ConstRetType& rhs, const LanguageOptions& options) const;

    ConstRetType& bitOrAssign(const ConstRetType& rhs, const LanguageOptions& options);

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
    enum TypeInfo
    {
        Alignment,
        Size
    };

private:
    const SourceInterface& m_sourceInterface;
    std::function<ConstRetType(std::string_view)> m_identifierCallback;
    SemanticAnalysis* m_analyser = nullptr;
    std::function<void(const Message&)> m_loggerCallback;

public:
    enum Mode
    {
        Integer,
        Arithmetic,
        Initialization
    };

private:
    Mode m_mode;

    void log(const Message& message);

public:
    explicit ConstantEvaluator(const SourceInterface& sourceInterface,
                               std::function<ConstRetType(std::string_view)> identifierCallback = {},
                               SemanticAnalysis* analyser = nullptr,
                               std::function<void(const Message&)> loggerCallback = {}, Mode mode = Integer);

    ConstRetType visit(const Syntax::Expression& node);

    ConstRetType visit(const Syntax::AssignmentExpression& node);

    ConstRetType visit(const Syntax::PrimaryExpression& node);

    ConstRetType visit(const Syntax::PrimaryExpressionConstant& node);

    ConstRetType visit(const Syntax::PrimaryExpressionParenthese& node);

    ConstRetType visit(const Syntax::PostFixExpression& node);

    ConstRetType visit(const Syntax::PostFixExpressionPrimaryExpression& node);

    ConstRetType visit(const Syntax::PostFixExpressionSubscript& node);

    ConstRetType visit(const Syntax::PostFixExpressionDot& node);

    ConstRetType visit(const Syntax::PostFixExpressionArrow& node);

    ConstRetType visit(const Syntax::UnaryExpression& node);

    ConstRetType visit(const Syntax::UnaryExpressionPostFixExpression& node);

    ConstRetType visit(const Syntax::UnaryExpressionUnaryOperator& node);

    ConstRetType visit(const Syntax::UnaryExpressionSizeOf& node);

    ConstRetType visit(const Syntax::UnaryExpressionDefined& node);

    ConstRetType visit(const Syntax::CastExpression& node);

    ConstRetType visit(const Syntax::Term& node);

    ConstRetType visit(const Syntax::AdditiveExpression& node);

    ConstRetType visit(const Syntax::ShiftExpression& node);

    ConstRetType visit(const Syntax::RelationalExpression& node);

    ConstRetType visit(const Syntax::EqualityExpression& node);

    ConstRetType visit(const Syntax::BitAndExpression& node);

    ConstRetType visit(const Syntax::BitXorExpression& node);

    ConstRetType visit(const Syntax::BitOrExpression& node);

    ConstRetType visit(const Syntax::LogicalAndExpression& node);

    ConstRetType visit(const Syntax::LogicalOrExpression& node);

    ConstRetType visit(const Syntax::ConditionalExpression& node);
};

} // namespace cld::Semantics

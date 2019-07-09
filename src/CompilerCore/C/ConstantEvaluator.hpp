#ifndef OPENCLPARSER_CONSTANTEVALUATOR_HPP
#define OPENCLPARSER_CONSTANTEVALUATOR_HPP

#include "../Common/Expected.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"
#include <functional>

namespace OpenCL::Semantics
{
    //TODO: Pointer arithmetic support
    class ConstRetType final
    {
    public:
        using ValueType = std::variant<std::monostate,
                                       std::int8_t,
                                       std::uint8_t,
                                       std::int16_t,
                                       std::uint16_t,
                                       std::int32_t,
                                       std::uint32_t,
                                       std::int64_t,
                                       std::uint64_t,
                                       float,
                                       double,
                                       void*>;

    private:
        ValueType m_value;
        Type m_type;

        template <class F>
        ConstRetType applyBinary(const ConstRetType& rhs, F&& binaryOperator) const;

        template <class F>
        ConstRetType applyIntegerBinary(const ConstRetType& rhs, F&& binaryOperator) const;

        static Type valueToType(const ValueType& value);

    public:

        ConstRetType() = default;

        /* implicit */ ConstRetType(const ValueType& value, const Type& type = Type{});

        [[nodiscard]] const Type& getType() const;

        [[nodiscard]] const ValueType& getValue() const;

        [[nodiscard]] bool isInteger() const;

        [[nodiscard]] bool isArithmetic() const;

        [[nodiscard]] bool isUndefined() const;

        ConstRetType operator+() const;

        ConstRetType operator-() const;

        ConstRetType operator!() const;

        ConstRetType operator~() const;

        [[nodiscard]] ConstRetType castTo(const Type& type) const;

        ConstRetType operator*(const ConstRetType& rhs) const;

        ConstRetType& operator*=(const ConstRetType& rhs);

        ConstRetType operator/(const ConstRetType& rhs) const;

        ConstRetType& operator/=(const ConstRetType& rhs);

        ConstRetType operator%(const ConstRetType& rhs) const;

        ConstRetType& operator%=(const ConstRetType& rhs);

        ConstRetType operator+(const ConstRetType& rhs) const;

        ConstRetType& operator+=(const ConstRetType& rhs);

        ConstRetType operator-(const ConstRetType& rhs) const;

        ConstRetType& operator-=(const ConstRetType& rhs);

        ConstRetType operator<<(const ConstRetType& rhs) const;

        ConstRetType& operator<<=(const ConstRetType& rhs);

        ConstRetType operator>>(const ConstRetType& rhs) const;

        ConstRetType& operator>>=(const ConstRetType& rhs);

        ConstRetType operator&(const ConstRetType& rhs) const;

        ConstRetType& operator&=(const ConstRetType& rhs);

        ConstRetType operator^(const ConstRetType& rhs) const;

        ConstRetType& operator^=(const ConstRetType& rhs);

        ConstRetType operator|(const ConstRetType& rhs) const;

        ConstRetType& operator|=(const ConstRetType& rhs);

        ConstRetType operator<(const ConstRetType& rhs) const;

        ConstRetType operator>(const ConstRetType& rhs) const;

        ConstRetType operator<=(const ConstRetType& rhs) const;

        ConstRetType operator>=(const ConstRetType& rhs) const;

        ConstRetType operator==(const ConstRetType& rhs) const;

        ConstRetType operator!=(const ConstRetType& rhs) const;

        [[nodiscard]] ConstRetType toBool() const;

        explicit operator bool() const;
    };

    class ConstantEvaluator final
    {
        std::vector<Lexer::Token>::const_iterator m_exprStart;
        std::vector<Lexer::Token>::const_iterator m_exprEnd;
        std::function<Type(const Syntax::TypeName&)> m_typeCallback;
        std::function<const DeclarationTypedefEnums&(const std::string&)> m_declarationCallback;
        std::function<void(const Message&)> m_loggerCallback;
        bool m_integerOnly;

        void logError(const Message& message);

    public:

        explicit ConstantEvaluator(std::vector<Lexer::Token>::const_iterator exprStart,
                                   std::vector<Lexer::Token>::const_iterator exprEnd,
                                   std::function<Type(const Syntax::TypeName&)> typeCallback = {},
                                   std::function<const DeclarationTypedefEnums&(const std::string&)> declarationCallback = {},
                                   std::function<void(const Message&)> loggerCallback = {},
                                   bool integerOnly = true);

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
} // namespace OpenCL::Semantics

#endif // OPENCLPARSER_CONSTANTEVALUATOR_HPP

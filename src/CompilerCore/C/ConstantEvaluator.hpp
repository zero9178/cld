#ifndef OPENCLPARSER_CONSTANTEVALUATOR_HPP
#define OPENCLPARSER_CONSTANTEVALUATOR_HPP

#include "Expected.hpp"
#include "FailureReason.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

#include <map>

namespace OpenCL::Constant
{
    using ConstRetType =
        OpenCL::Expected<std::variant<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t, float, double, void*>,
                         OpenCL::FailureReason>;

    class ConstantEvaluator final
    {
        std::map<std::string, Semantics::RecordType> m_structOrUnions;
        std::map<std::string, std::reference_wrapper<const Semantics::Type>> m_typedefs;

    public:
        explicit ConstantEvaluator(
            const std::map<std::string, Semantics::RecordType>& structOrUnions = {},
            const std::map<std::string, std::reference_wrapper<const Semantics::Type>>& typedefs = {});

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
} // namespace OpenCL::Constant

#endif // OPENCLPARSER_CONSTANTEVALUATOR_HPP

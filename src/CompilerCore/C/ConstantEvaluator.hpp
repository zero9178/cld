#ifndef OPENCLPARSER_CONSTANTEVALUATOR_HPP
#define OPENCLPARSER_CONSTANTEVALUATOR_HPP

#include "../Common/Expected.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"
#include <functional>

namespace OpenCL::Semantics
{
    using ConstRetType = std::variant<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t, float, double, void*>;

    class ConstantEvaluator final
    {
        std::function<const RecordType*(const std::string&)> m_recordCallback;
        std::function<const Type*(const std::string&)> m_typedefCallback;
        std::function<void(const Message&)> m_loggerCallback;

        void logError(const Message& message);

    public:

        explicit ConstantEvaluator(const std::function<const RecordType*(const std::string&)>& recordCallback = {},
                                   const std::function<const Type*(const std::string&)>& typedefCallback = {},
                                   const std::function<void(const Message&)>& loggerCallback = {});

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

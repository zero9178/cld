#ifndef OPENCLPARSER_RESULTINGTYPE_HPP
#define OPENCLPARSER_RESULTINGTYPE_HPP

#include <map>
#include "Representations.hpp"
#include "Syntax.hpp"

namespace OpenCL::Constant
{
    class ResultingType final
    {
        std::map<std::string, Representations::RecordType> m_structOrUnions;
        std::map<std::string, std::reference_wrapper<const Representations::Type>> m_typedefs;
        std::map<std::string, Representations::Type> m_typesOfNamedValues;

    public:

        ResultingType(std::map<std::string, Representations::RecordType> structOrUnions,
                      std::map<std::string, std::reference_wrapper<const Representations::Type>> typedefs,
                      std::map<std::string, Representations::Type> typesOfNamedValues);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PrimaryExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PrimaryExpressionIdentifier& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PrimaryExpressionConstant& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PrimaryExpressionParenthese& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionPrimaryExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionSubscript& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionIncrement& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionDecrement& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionDot& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionArrow& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionFunctionCall& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::PostFixExpressionTypeInitializer& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::UnaryExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::UnaryExpressionPostFixExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::UnaryExpressionSizeOf& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::UnaryExpressionUnaryOperator& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::CastExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::Term& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::AdditiveExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::ShiftExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::RelationalExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::EqualityExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::BitAndExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::BitXorExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::BitOrExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::LogicalAndExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::LogicalOrExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::ConditionalExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::AssignmentExpression& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::AssignmentExpressionAssignment& node);

        Expected<Representations::Type, FailureReason> visit(const Syntax::Expression& node);
    };
}

#endif //OPENCLPARSER_RESULTINGTYPE_HPP

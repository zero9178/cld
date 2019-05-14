#ifndef OPENCLPARSER_SEMANTICANALYSIS_HPP
#define OPENCLPARSER_SEMANTICANALYSIS_HPP

#include <map>
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace OpenCL::Semantics
{
    class SemanticAnalysis final
    {
        std::vector<std::map<std::string, Semantics::RecordType>> m_structsUnions{1};
        std::vector<std::map<std::string, Semantics::Type>> m_typedefs{1};
        std::vector<std::map<std::string, Semantics::Type>> m_typesOfNamedValues{1};

        std::map<std::string, std::reference_wrapper<const Semantics::Type>> gatherTypedefs() const;

        std::map<std::string, Semantics::RecordType> gatherStructsAndUnions() const;

        void popScope()
        {
            m_typesOfNamedValues.pop_back();
            m_structsUnions.pop_back();
            m_typedefs.pop_back();
        }

        void pushScope()
        {
            m_typesOfNamedValues.emplace_back();
            m_structsUnions.emplace_back();
            m_typedefs.emplace_back();
        }

    public:

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PrimaryExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PrimaryExpressionIdentifier& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PrimaryExpressionConstant& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PrimaryExpressionParenthese& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionPrimaryExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionSubscript& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionIncrement& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionDecrement& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionDot& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionArrow& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionFunctionCall& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::PostFixExpressionTypeInitializer& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::UnaryExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::UnaryExpressionPostFixExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::UnaryExpressionSizeOf& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::UnaryExpressionUnaryOperator& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::CastExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::Term& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::AdditiveExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::ShiftExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::RelationalExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::EqualityExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::BitAndExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::BitXorExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::BitOrExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::LogicalAndExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::LogicalOrExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::ConditionalExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::AssignmentExpression& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::AssignmentExpressionAssignment& node);

        Expected<Semantics::Type, FailureReason> visit(const Syntax::Expression& node);

        Expected<Semantics::TranslationUnit, FailureReason> visit(const Syntax::TranslationUnit& node);

        Expected<Semantics::FunctionDefinition, FailureReason> visit(const Syntax::FunctionDefinition& node);
    };
}

#endif //OPENCLPARSER_SEMANTICANALYSIS_HPP

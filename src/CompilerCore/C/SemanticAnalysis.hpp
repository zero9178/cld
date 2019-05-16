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

        Expected<Type, FailureReason> visit(const Syntax::PrimaryExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::PrimaryExpressionIdentifier& node);

        Expected<Type, FailureReason> visit(const Syntax::PrimaryExpressionConstant& node);

        Expected<Type, FailureReason> visit(const Syntax::PrimaryExpressionParenthese& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionPrimaryExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionSubscript& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionIncrement& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionDecrement& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionDot& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionArrow& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionFunctionCall& node);

        Expected<Type, FailureReason> visit(const Syntax::PostFixExpressionTypeInitializer& node);

        Expected<Type, FailureReason> visit(const Syntax::UnaryExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::UnaryExpressionPostFixExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::UnaryExpressionSizeOf& node);

        Expected<Type, FailureReason> visit(const Syntax::UnaryExpressionUnaryOperator& node);

        Expected<Type, FailureReason> visit(const Syntax::CastExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::Term& node);

        Expected<Type, FailureReason> visit(const Syntax::AdditiveExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::ShiftExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::RelationalExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::EqualityExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::BitAndExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::BitXorExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::BitOrExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::LogicalAndExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::LogicalOrExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::ConditionalExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::AssignmentExpression& node);

        Expected<Type, FailureReason> visit(const Syntax::AssignmentExpressionAssignment& node);

        Expected<Type, FailureReason> visit(const Syntax::Expression& node);

        Expected<TranslationUnit, FailureReason> visit(const Syntax::TranslationUnit& node);

        Expected<FunctionDefinition, FailureReason> visit(const Syntax::FunctionDefinition& node);

        Expected<std::vector<std::variant<FunctionPrototype,Declaration>>,FailureReason> visit(const Syntax::Declaration& node);
    };
}

#endif //OPENCLPARSER_SEMANTICANALYSIS_HPP

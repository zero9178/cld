#ifndef OPENCLPARSER_CONSTANTEVALUATOR_HPP
#define OPENCLPARSER_CONSTANTEVALUATOR_HPP

#include "Syntax.hpp"

namespace OpenCL::Codegen
{
    using ConstRetType = std::variant<std::int32_t,
                                      std::uint32_t,
                                      std::int64_t,
                                      std::uint64_t,
                                      float,
                                      double,
                                      void*>;

    class ConstantEvaluator final : public OpenCL::Syntax::NodeVisitor<ConstRetType>
    {
    public:

        void visit(const Syntax::Expression& node) override;

        void visit(const Syntax::PrimaryExpressionIdentifier& node) override;

        void visit(const Syntax::PrimaryExpressionConstant& node) override;

        void visit(const Syntax::PrimaryExpressionParenthese& node) override;

        void visit(const Syntax::PrimaryExpression& node) override;

        void visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        void visit(const Syntax::PostFixExpressionSubscript& node) override;

        void visit(const Syntax::PostFixExpressionIncrement& node) override;

        void visit(const Syntax::PostFixExpressionDecrement& node) override;

        void visit(const Syntax::PostFixExpressionDot& node) override;

        void visit(const Syntax::PostFixExpressionArrow& node) override;

        void visit(const Syntax::PostFixExpressionFunctionCall& node) override;

        void visit(const Syntax::PostFixExpressionTypeInitializer& node) override;

        void visit(const Syntax::PostFixExpression& node) override;

        void visit(const Syntax::AssignmentExpression& node) override;

        void visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        void visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        void visit(const Syntax::UnaryExpressionSizeOf& node) override;

        void visit(const Syntax::UnaryExpression& node) override;

        void visit(const Syntax::CastExpression& node) override;

        void visit(const Syntax::Term& node) override;

        void visit(const Syntax::AdditiveExpression& node) override;

        void visit(const Syntax::ShiftExpression& node) override;

        void visit(const Syntax::RelationalExpression& node) override;

        void visit(const Syntax::EqualityExpression& node) override;

        void visit(const Syntax::BitAndExpression& node) override;

        void visit(const Syntax::BitXorExpression& node) override;

        void visit(const Syntax::BitOrExpression& node) override;

        void visit(const Syntax::LogicalAndExpression& node) override;

        void visit(const Syntax::LogicalOrExpression& node) override;

        void visit(const Syntax::ConditionalExpression& node) override;

        void visit(const Syntax::NonCommaExpression& node) override;

        void visit(const Syntax::ReturnStatement& node) override;

        void visit(const Syntax::ExpressionStatement& node) override;

        void visit(const Syntax::IfStatement& node) override;

        void visit(const Syntax::SwitchStatement& node) override;

        void visit(const Syntax::DefaultStatement& node) override;

        void visit(const Syntax::CaseStatement& node) override;

        void visit(const Syntax::BlockStatement& node) override;

        void visit(const Syntax::ForStatement& node) override;

        void visit(const Syntax::InitializerListScalarExpression& node) override;

        void visit(const Syntax::InitializerListBlock& node) override;

        void visit(const Syntax::InitializerList& node) override;

        void visit(const Syntax::Declarations& node) override;

        void visit(const Syntax::BlockItem& node) override;

        void visit(const Syntax::ForDeclarationStatement& node) override;

        void visit(const Syntax::HeadWhileStatement& node) override;

        void visit(const Syntax::FootWhileStatement& node) override;

        void visit(const Syntax::BreakStatement& node) override;

        void visit(const Syntax::ContinueStatement& node) override;

        void visit(const Syntax::Statement& node) override;

        void visit(const Syntax::StructOrUnionDeclaration& node) override;

        void visit(const Syntax::EnumDeclaration& node) override;

        void visit(const Syntax::TypedefDeclaration& node) override;

        void visit(const Syntax::Function& node) override;

        void visit(const Syntax::GlobalDeclaration& node) override;

        void visit(const Syntax::Global& node) override;

        void visit(const Syntax::Program& node) override;
    };
}

#endif //OPENCLPARSER_CONSTANTEVALUATOR_HPP

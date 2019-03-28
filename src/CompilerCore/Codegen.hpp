#ifndef OPENCLPARSER_CODEGEN_HPP
#define OPENCLPARSER_CODEGEN_HPP

#include "Syntax.hpp"

namespace OpenCL::Codegen
{
    class CodegenVisitor final : public Syntax::NodeVisitor
    {
    public:
        void visit(Syntax::Expression& node) override;

        void visit(Syntax::PrimaryExpressionIdentifier& node) override;

        void visit(Syntax::PrimaryExpressionConstant& node) override;

        void visit(Syntax::PrimaryExpressionParenthese& node) override;

        void visit(Syntax::PrimaryExpression& node) override;

        void visit(Syntax::PostFixExpressionPrimaryExpression& node) override;

        void visit(Syntax::PostFixExpressionSubscript& node) override;

        void visit(Syntax::PostFixExpressionIncrement& node) override;

        void visit(Syntax::PostFixExpressionDecrement& node) override;

        void visit(Syntax::PostFixExpressionDot& node) override;

        void visit(Syntax::PostFixExpressionArrow& node) override;

        void visit(Syntax::PostFixExpressionFunctionCall& node) override;

        void visit(Syntax::PostFixExpressionTypeInitializer& node) override;

        void visit(Syntax::PostFixExpression& node) override;

        void visit(Syntax::AssignmentExpression& node) override;

        void visit(Syntax::UnaryExpressionPostFixExpression& node) override;

        void visit(Syntax::UnaryExpressionUnaryOperator& node) override;

        void visit(Syntax::UnaryExpressionSizeOf& node) override;

        void visit(Syntax::UnaryExpression& node) override;

        void visit(Syntax::CastExpression& node) override;

        void visit(Syntax::Term& node) override;

        void visit(Syntax::AdditiveExpression& node) override;

        void visit(Syntax::ShiftExpression& node) override;

        void visit(Syntax::RelationalExpression& node) override;

        void visit(Syntax::EqualityExpression& node) override;

        void visit(Syntax::BitAndExpression& node) override;

        void visit(Syntax::BitXorExpression& node) override;

        void visit(Syntax::BitOrExpression& node) override;

        void visit(Syntax::LogicalAndExpression& node) override;

        void visit(Syntax::LogicalOrExpression& node) override;

        void visit(Syntax::ConditionalExpression& node) override;

        void visit(Syntax::NonCommaExpression& node) override;

        void visit(Syntax::ReturnStatement& node) override;

        void visit(Syntax::ExpressionStatement& node) override;

        void visit(Syntax::IfStatement& node) override;

        void visit(Syntax::SwitchStatement& node) override;

        void visit(Syntax::DefaultStatement& node) override;

        void visit(Syntax::CaseStatement& node) override;

        void visit(Syntax::BlockStatement& node) override;

        void visit(Syntax::ForStatement& node) override;

        void visit(Syntax::InitializerListScalarExpression& node) override;

        void visit(Syntax::InitializerListBlock& node) override;

        void visit(Syntax::InitializerList& node) override;

        void visit(Syntax::Declarations& node) override;

        void visit(Syntax::BlockItem& node) override;

        void visit(Syntax::ForDeclarationStatement& node) override;

        void visit(Syntax::HeadWhileStatement& node) override;

        void visit(Syntax::FootWhileStatement& node) override;

        void visit(Syntax::BreakStatement& node) override;

        void visit(Syntax::ContinueStatement& node) override;

        void visit(Syntax::Statement& node) override;

        void visit(Syntax::StructOrUnionDeclaration& node) override;

        void visit(Syntax::EnumDeclaration& node) override;

        void visit(Syntax::TypedefDeclaration& node) override;

        void visit(Syntax::Function& node) override;

        void visit(Syntax::GlobalDeclaration& node) override;

        void visit(Syntax::Global& node) override;

        void visit(Syntax::Program& node) override;
    };
}

#endif //OPENCLPARSER_CODEGEN_HPP

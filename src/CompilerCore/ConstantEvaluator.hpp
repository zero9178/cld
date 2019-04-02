#ifndef OPENCLPARSER_CONSTANTEVALUATOR_HPP
#define OPENCLPARSER_CONSTANTEVALUATOR_HPP

#include "Syntax.hpp"

namespace OpenCL::Codegen
{
    class ConstantEvaluator final : public OpenCL::Syntax::NodeVisitor
    {
    public:

        using ConstRetType =

        retType visit(const Syntax::Expression& node) override;

        retType visit(const Syntax::PrimaryExpressionIdentifier& node) override;

        retType visit(const Syntax::PrimaryExpressionConstant& node) override;

        retType visit(const Syntax::PrimaryExpressionParenthese& node) override;

        retType visit(const Syntax::PrimaryExpression& node) override;

        retType visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        retType visit(const Syntax::PostFixExpressionSubscript& node) override;

        retType visit(const Syntax::PostFixExpressionIncrement& node) override;

        retType visit(const Syntax::PostFixExpressionDecrement& node) override;

        retType visit(const Syntax::PostFixExpressionDot& node) override;

        retType visit(const Syntax::PostFixExpressionArrow& node) override;

        retType visit(const Syntax::PostFixExpressionFunctionCall& node) override;

        retType visit(const Syntax::PostFixExpressionTypeInitializer& node) override;

        retType visit(const Syntax::PostFixExpression& node) override;

        retType visit(const Syntax::AssignmentExpression& node) override;

        retType visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        retType visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        retType visit(const Syntax::UnaryExpressionSizeOf& node) override;

        retType visit(const Syntax::UnaryExpression& node) override;

        retType visit(const Syntax::CastExpression& node) override;

        retType visit(const Syntax::Term& node) override;

        retType visit(const Syntax::AdditiveExpression& node) override;

        retType visit(const Syntax::ShiftExpression& node) override;

        retType visit(const Syntax::RelationalExpression& node) override;

        retType visit(const Syntax::EqualityExpression& node) override;

        retType visit(const Syntax::BitAndExpression& node) override;

        retType visit(const Syntax::BitXorExpression& node) override;

        retType visit(const Syntax::BitOrExpression& node) override;

        retType visit(const Syntax::LogicalAndExpression& node) override;

        retType visit(const Syntax::LogicalOrExpression& node) override;

        retType visit(const Syntax::ConditionalExpression& node) override;

        retType visit(const Syntax::NonCommaExpression& node) override;

        retType visit(const Syntax::ReturnStatement& node) override;

        retType visit(const Syntax::ExpressionStatement& node) override;

        retType visit(const Syntax::IfStatement& node) override;

        retType visit(const Syntax::SwitchStatement& node) override;

        retType visit(const Syntax::DefaultStatement& node) override;

        retType visit(const Syntax::CaseStatement& node) override;

        retType visit(const Syntax::BlockStatement& node) override;

        retType visit(const Syntax::ForStatement& node) override;

        retType visit(const Syntax::InitializerListScalarExpression& node) override;

        retType visit(const Syntax::InitializerListBlock& node) override;

        retType visit(const Syntax::InitializerList& node) override;

        retType visit(const Syntax::Declarations& node) override;

        retType visit(const Syntax::BlockItem& node) override;

        retType visit(const Syntax::ForDeclarationStatement& node) override;

        retType visit(const Syntax::HeadWhileStatement& node) override;

        retType visit(const Syntax::FootWhileStatement& node) override;

        retType visit(const Syntax::BreakStatement& node) override;

        retType visit(const Syntax::ContinueStatement& node) override;

        retType visit(const Syntax::Statement& node) override;

        retType visit(const Syntax::StructOrUnionDeclaration& node) override;

        retType visit(const Syntax::EnumDeclaration& node) override;

        retType visit(const Syntax::TypedefDeclaration& node) override;

        retType visit(const Syntax::Function& node) override;

        retType visit(const Syntax::GlobalDeclaration& node) override;

        retType visit(const Syntax::Global& node) override;

        retType visit(const Syntax::Program& node) override;
    };
}

#endif //OPENCLPARSER_CONSTANTEVALUATOR_HPP

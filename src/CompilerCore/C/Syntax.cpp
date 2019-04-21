#include "Syntax.hpp"

#include <cassert>
#include <algorithm>

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Expression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PrimaryExpressionIdentifier&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PrimaryExpressionConstant&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PrimaryExpressionParenthese&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PrimaryExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionPrimaryExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionSubscript&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionIncrement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionDecrement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionDot&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionArrow&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionFunctionCall&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpressionTypeInitializer&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpression& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::AssignmentExpressionAssignment&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::UnaryExpressionPostFixExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::UnaryExpressionUnaryOperator&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::UnaryExpressionSizeOf&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::UnaryExpression& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::CastExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Term&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::AdditiveExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ShiftExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::RelationalExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::EqualityExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::BitAndExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::BitXorExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::BitOrExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::LogicalAndExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::LogicalOrExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ConditionalExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::AssignmentExpression& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ReturnStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ExpressionStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::IfStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::SwitchStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DefaultStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::CaseStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::CompoundStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ForStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::InitializerList&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Initializer& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Declaration&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::CompoundItem& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ForDeclarationStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::HeadWhileStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::FootWhileStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::BreakStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ContinueStatement&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Statement& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::EnumSpecifier&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::FunctionDefinition&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ExternalDeclaration& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::TranslationUnit&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::TypeName&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Declarator&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::EnumDeclaration&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::StructOrUnionSpecifier&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::TypeSpecifier&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectDeclarator& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectDeclaratorStatic&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectDeclaratorAsterisk&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectDeclaratorParentheseParameters&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectAbstractDeclarator&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Pointer&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ParameterTypeList&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::ParameterList&)
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::LabelStatement& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::INodeVisitor::ReturnType& OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::GotoStatement& )
{
    throw std::runtime_error(std::string(__FUNCTION__) + " not overwritten in visitor");
}

OpenCL::Syntax::Expression::Expression(std::uint64_t line,
                                       std::uint64_t column,
                                       std::vector<OpenCL::Syntax::AssignmentExpression> assignmanetExpressions)
    : Node(line, column), m_assignmentExpressions(std::move(assignmanetExpressions))
{
    assert(!m_assignmentExpressions.empty());
}

const std::vector<OpenCL::Syntax::AssignmentExpression>& OpenCL::Syntax::Expression::getAssignmentExpressions() const
{
    return m_assignmentExpressions;
}

OpenCL::Syntax::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(std::uint64_t line,
                                                                         std::uint64_t column,
                                                                         std::string identifier) : Node(line, column),
                                                                                                   m_identifier(std::move(
                                                                                                       identifier))
{}

const std::string& OpenCL::Syntax::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PrimaryExpressionConstant::PrimaryExpressionConstant(std::uint64_t line,
                                                                     std::uint64_t column,
                                                                     OpenCL::Syntax::PrimaryExpressionConstant::variant value)
    : Node(line, column), m_value(std::move(value))
{

}

const OpenCL::Syntax::PrimaryExpressionConstant::variant& OpenCL::Syntax::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

OpenCL::Syntax::PrimaryExpressionParenthese::PrimaryExpressionParenthese(std::uint64_t line,
                                                                         std::uint64_t column,
                                                                         OpenCL::Syntax::Expression&& expression)
    : Node(line, column), m_expression(std::move(expression))
{}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PrimaryExpressionParenthese::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PrimaryExpression::PrimaryExpression(std::uint64_t line,
                                                     std::uint64_t column,
                                                     OpenCL::Syntax::PrimaryExpression::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::PrimaryExpression::variant& OpenCL::Syntax::PrimaryExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(std::uint64_t line,
                                                                                       std::uint64_t column,
                                                                                       OpenCL::Syntax::PrimaryExpression&& primaryExpression)
    : Node(line, column), m_primaryExpression(std::move(primaryExpression))
{

}

const OpenCL::Syntax::PrimaryExpression& OpenCL::Syntax::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return m_primaryExpression;
}

OpenCL::Syntax::PostFixExpressionSubscript::PostFixExpressionSubscript(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression,
                                                                       OpenCL::Syntax::Expression&& expression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression)), m_expression(std::move(expression))
{
    assert(m_postFixExpression);
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionSubscript::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PostFixExpressionSubscript::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PostFixExpressionIncrement::PostFixExpressionIncrement(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression))
{
    assert(m_postFixExpression);
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::PostFixExpressionDecrement::PostFixExpressionDecrement(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression))
{
    assert(m_postFixExpression);
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::PostFixExpressionDot::PostFixExpressionDot(std::uint64_t line,
                                                           std::uint64_t column,
                                                           std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression,
                                                           std::string identifier) : Node(line, column),
                                                                                     m_postFixExpression(std::move(
                                                                                         postFixExpression)),
                                                                                     m_identifier(std::move(identifier))
{
    assert(m_postFixExpression);
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionDot::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& OpenCL::Syntax::PostFixExpressionDot::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PostFixExpressionArrow::PostFixExpressionArrow(std::uint64_t line,
                                                               std::uint64_t column,
                                                               std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression,
                                                               std::string identifier) : Node(line, column),
                                                                                         m_postFixExpression(std::move(
                                                                                             postFixExpression)),
                                                                                         m_identifier(std::move(
                                                                                             identifier))
{

}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionArrow::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& OpenCL::Syntax::PostFixExpressionArrow::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(std::uint64_t line,
                                                                             std::uint64_t column,
                                                                             std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression,
                                                                             std::vector<std::unique_ptr<OpenCL::Syntax::AssignmentExpression>>&& optionalAssignmanetExpressions)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression)),
      m_optionalAssignmanetExpressions(std::move(optionalAssignmanetExpressions))
{

}

OpenCL::Syntax::PostFixExpressionTypeInitializer::PostFixExpressionTypeInitializer(std::uint64_t line,
                                                                                   std::uint64_t column,
                                                                                   TypeName&& typeName,
                                                                                   OpenCL::Syntax::InitializerList&& initializerList)
    : Node(line, column), m_typeName(std::make_unique<TypeName>(std::move(typeName))),
    m_initializerList(std::make_unique<InitializerList>(std::move(initializerList)))
{

}

const OpenCL::Syntax::InitializerList& OpenCL::Syntax::PostFixExpressionTypeInitializer::getInitializerList() const
{
    return *m_initializerList;
}

const OpenCL::Syntax::TypeName& OpenCL::Syntax::PostFixExpressionTypeInitializer::getTypeName() const
{
    return *m_typeName;
}

OpenCL::Syntax::PostFixExpression::PostFixExpression(std::uint64_t line,
                                                     std::uint64_t column,
                                                     OpenCL::Syntax::PostFixExpression::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{

}

const OpenCL::Syntax::PostFixExpression::variant& OpenCL::Syntax::PostFixExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::UnaryExpressionPostFixExpression::UnaryExpressionPostFixExpression(std::uint64_t line,
                                                                                   std::uint64_t column,
                                                                                   OpenCL::Syntax::PostFixExpression&& postFixExpression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression))
{

}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return m_postFixExpression;
}

OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(std::uint64_t line,
                                                                           std::uint64_t column,
                                                                           OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryOperator anOperator,
                                                                           std::unique_ptr<OpenCL::Syntax::UnaryExpression>&& unaryExpression)
    : Node(line, column), m_operator(anOperator), m_unaryExpression(std::move(unaryExpression))
{

}

OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryOperator OpenCL::Syntax::UnaryExpressionUnaryOperator::getAnOperator() const
{
    return m_operator;
}

const OpenCL::Syntax::UnaryExpression& OpenCL::Syntax::UnaryExpressionUnaryOperator::getUnaryExpression() const
{
    return *m_unaryExpression;
}

OpenCL::Syntax::UnaryExpressionSizeOf::UnaryExpressionSizeOf(std::uint64_t line,
                                                             std::uint64_t column,
                                                             OpenCL::Syntax::UnaryExpressionSizeOf::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::UnaryExpressionSizeOf::variant& OpenCL::Syntax::UnaryExpressionSizeOf::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::UnaryExpression::UnaryExpression(std::uint64_t line,
                                                 std::uint64_t column,
                                                 OpenCL::Syntax::UnaryExpression::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{

}

const OpenCL::Syntax::UnaryExpression::variant& OpenCL::Syntax::UnaryExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::AssignmentExpressionAssignment::AssignmentExpressionAssignment(std::uint64_t line,
                                                                               std::uint64_t column,
                                                                               OpenCL::Syntax::UnaryExpression&& unaryFactor,
                                                                               OpenCL::Syntax::AssignmentExpressionAssignment::AssignOperator assignOperator,
                                                                               std::unique_ptr<OpenCL::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(line, column), m_unaryFactor(std::move(unaryFactor)), m_assignOperator(assignOperator),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_assignmentExpression);
}

const OpenCL::Syntax::UnaryExpression& OpenCL::Syntax::AssignmentExpressionAssignment::getUnaryFactor() const
{
    return m_unaryFactor;
}

OpenCL::Syntax::AssignmentExpressionAssignment::AssignOperator OpenCL::Syntax::AssignmentExpressionAssignment::getAssignOperator() const
{
    return m_assignOperator;
}

const OpenCL::Syntax::AssignmentExpression& OpenCL::Syntax::AssignmentExpressionAssignment::getAssignmentExpression() const
{
    return *m_assignmentExpression;
}

OpenCL::Syntax::CastExpression::CastExpression(std::uint64_t line,
                                               std::uint64_t column,
                                               OpenCL::Syntax::CastExpression::variant&& variant) : Node(line,
                                                                                                         column),
                                                                                                    m_variant(
                                                                                                        std::move(
                                                                                                            variant))
{}

const OpenCL::Syntax::CastExpression::variant& OpenCL::Syntax::CastExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Term::Term(std::uint64_t line,
                           std::uint64_t column,
                           OpenCL::Syntax::CastExpression&& castExpressions,
                           std::vector<std::pair<OpenCL::Syntax::Term::BinaryDotOperator,
                                                 OpenCL::Syntax::CastExpression>>&& optionalCastExpressions)
    : Node(line, column), m_castExpression(std::move(castExpressions)),
      m_optionalCastExpressions(std::move(optionalCastExpressions))
{}

const OpenCL::Syntax::CastExpression& OpenCL::Syntax::Term::getCastExpression() const
{
    return m_castExpression;
}

const std::vector<std::pair<OpenCL::Syntax::Term::BinaryDotOperator,
                            OpenCL::Syntax::CastExpression>>& OpenCL::Syntax::Term::getOptionalCastExpressions() const
{
    return m_optionalCastExpressions;
}

OpenCL::Syntax::AdditiveExpression::AdditiveExpression(std::uint64_t line,
                                                       std::uint64_t column,
                                                       OpenCL::Syntax::Term&& term,
                                                       std::vector<std::pair<OpenCL::Syntax::AdditiveExpression::BinaryDashOperator,
                                                                             OpenCL::Syntax::Term>>&& optionalTerms)
    : Node(line, column), m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{}

const OpenCL::Syntax::Term& OpenCL::Syntax::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<std::pair<OpenCL::Syntax::AdditiveExpression::BinaryDashOperator,
                            OpenCL::Syntax::Term>>& OpenCL::Syntax::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

OpenCL::Syntax::ShiftExpression::ShiftExpression(std::uint64_t line,
                                                 std::uint64_t column,
                                                 OpenCL::Syntax::AdditiveExpression&& additiveExpression,
                                                 std::vector<std::pair<OpenCL::Syntax::ShiftExpression::ShiftOperator,
                                                                       OpenCL::Syntax::AdditiveExpression>>&& optionalAdditiveExpressions)
    : Node(line, column), m_additiveExpression(std::move(additiveExpression)),
      m_optionalAdditiveExpressions(std::move(optionalAdditiveExpressions))
{}

const OpenCL::Syntax::AdditiveExpression& OpenCL::Syntax::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::ReturnStatement::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::ReturnStatement::ReturnStatement(std::uint64_t line, std::uint64_t column, Expression&& expression)
    : Node(line, column), m_expression(std::move(expression))
{}

OpenCL::Syntax::ExpressionStatement::ExpressionStatement(std::uint64_t line,
                                                         std::uint64_t column,
                                                         std::unique_ptr<Expression>&& optionalExpression)
    : Node(line, column), m_optionalExpression(std::move(optionalExpression))
{}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ExpressionStatement::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

std::unique_ptr<OpenCL::Syntax::Expression> OpenCL::Syntax::ExpressionStatement::moveOptionalExpression()
{
    return std::move(m_optionalExpression);
}

OpenCL::Syntax::IfStatement::IfStatement(std::uint64_t line,
                                         std::uint64_t column,
                                         Expression&& expression,
                                         std::unique_ptr<Statement>&& branch,
                                         std::unique_ptr<Statement>&& elseBranch)
    : Node(line, column), m_expression(std::move(expression)), m_branch(std::move(branch)),
      m_elseBranch(std::move(elseBranch))
{
    assert(m_branch);
}

OpenCL::Syntax::ContinueStatement::ContinueStatement(std::uint64_t line, std::uint64_t column) : Node(line, column)
{

}

const OpenCL::Syntax::Expression& OpenCL::Syntax::IfStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::IfStatement::getBranch() const
{
    return *m_branch;
}

const OpenCL::Syntax::Statement* OpenCL::Syntax::IfStatement::getElseBranch() const
{
    return m_elseBranch.get();
}

OpenCL::Syntax::CompoundStatement::CompoundStatement(std::uint64_t line,
                                                     std::uint64_t column,
                                                     std::vector<CompoundItem>&& blockItems)
    : Node(line, column), m_blockItems(std::move(blockItems))
{}

const std::vector<OpenCL::Syntax::CompoundItem>& OpenCL::Syntax::CompoundStatement::getBlockItems() const
{
    return m_blockItems;
}

OpenCL::Syntax::ForStatement::ForStatement(std::uint64_t line,
                                           std::uint64_t column,
                                           std::unique_ptr<Statement>&& statement,
                                           std::unique_ptr<Expression>&& initial,
                                           std::unique_ptr<Expression>&& controlling,
                                           std::unique_ptr<Expression>&& post)
    : Node(line, column), m_statement(std::move(statement)), m_initial(std::move(initial)),
      m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForStatement::getInitial() const
{
    return m_initial.get();
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Syntax::ForDeclarationStatement::ForDeclarationStatement(std::uint64_t line,
                                                                 std::uint64_t column,
                                                                 std::unique_ptr<Statement>&& statement,
                                                                 Declaration&& initial,
                                                                 std::unique_ptr<Expression>&& controlling,
                                                                 std::unique_ptr<Expression>&& post)
    : Node(line, column), m_statement(std::move(statement)), m_initial(std::move(initial)),
      m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const OpenCL::Syntax::Declaration& OpenCL::Syntax::ForDeclarationStatement::getInitial() const
{
    return m_initial;
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForDeclarationStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForDeclarationStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Syntax::HeadWhileStatement::HeadWhileStatement(std::uint64_t line,
                                                       std::uint64_t column,
                                                       Expression&& expression,
                                                       std::unique_ptr<Statement>&& statement)
    : Node(line, column), m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::HeadWhileStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::HeadWhileStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Syntax::FootWhileStatement::FootWhileStatement(std::uint64_t line,
                                                       std::uint64_t column,
                                                       std::unique_ptr<Statement>&& statement,
                                                       Expression&& expression)
    : Node(line, column), m_statement(
    std::move(statement)), m_expression(std::move(expression))
{
    assert(m_statement);
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::FootWhileStatement::getStatement() const
{
    return *m_statement;
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::FootWhileStatement::getExpression() const
{
    return m_expression;
}

const std::vector<std::pair<OpenCL::Syntax::ShiftExpression::ShiftOperator,
                            OpenCL::Syntax::AdditiveExpression>>& OpenCL::Syntax::ShiftExpression::getOptionalAdditiveExpressions() const
{
    return m_optionalAdditiveExpressions;
}

OpenCL::Syntax::RelationalExpression::RelationalExpression(std::uint64_t line,
                                                           std::uint64_t column,
                                                           ShiftExpression&& shiftExpression,
                                                           std::vector<std::pair<RelationalOperator,
                                                                                 ShiftExpression>>&& optionalRelationalExpressions)
    : Node(line, column), m_shiftExpression(std::move(shiftExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{}

const OpenCL::Syntax::ShiftExpression& OpenCL::Syntax::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

const std::vector<std::pair<OpenCL::Syntax::RelationalExpression::RelationalOperator,
                            OpenCL::Syntax::ShiftExpression>>& OpenCL::Syntax::RelationalExpression::getOptionalShiftExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Syntax::EqualityExpression::EqualityExpression(std::uint64_t line,
                                                       std::uint64_t column,
                                                       RelationalExpression&& relationalExpression,
                                                       std::vector<std::pair<EqualityOperator,
                                                                             RelationalExpression>>&& optionalRelationalExpressions)
    : Node(line, column), m_relationalExpression(std::move(relationalExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{}

const OpenCL::Syntax::RelationalExpression& OpenCL::Syntax::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

const std::vector<std::pair<OpenCL::Syntax::EqualityExpression::EqualityOperator,
                            OpenCL::Syntax::RelationalExpression>>& OpenCL::Syntax::EqualityExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Syntax::LogicalAndExpression::LogicalAndExpression(std::uint64_t line,
                                                           std::uint64_t column,
                                                           BitOrExpression&& equalityExpression,
                                                           std::vector<BitOrExpression>&& optionalEqualityExpressions)
    : Node(line, column), m_bitOrExpression(std::move(equalityExpression)),
      m_optionalBitOrExpressions(std::move(optionalEqualityExpressions))
{}

const OpenCL::Syntax::BitOrExpression& OpenCL::Syntax::LogicalAndExpression::getBitOrExpression() const
{
    return m_bitOrExpression;
}

const std::vector<OpenCL::Syntax::BitOrExpression>& OpenCL::Syntax::LogicalAndExpression::getOptionalBitOrExpressions() const
{
    return m_optionalBitOrExpressions;
}

OpenCL::Syntax::LogicalOrExpression::LogicalOrExpression(std::uint64_t line,
                                                         std::uint64_t column,
                                                         LogicalAndExpression&& andExpression,
                                                         std::vector<LogicalAndExpression>&& optionalAndExpressions)
    : Node(line, column), m_andExpression(std::move(andExpression)),
      m_optionalAndExpressions(std::move(optionalAndExpressions))
{}

const OpenCL::Syntax::LogicalAndExpression& OpenCL::Syntax::LogicalOrExpression::getAndExpression() const
{
    return m_andExpression;
}

const std::vector<OpenCL::Syntax::LogicalAndExpression>& OpenCL::Syntax::LogicalOrExpression::getOptionalAndExpressions() const
{
    return m_optionalAndExpressions;
}

OpenCL::Syntax::ConditionalExpression::ConditionalExpression(std::uint64_t line,
                                                             std::uint64_t column,
                                                             LogicalOrExpression&& logicalOrExpression,
                                                             std::unique_ptr<Expression>&& optionalExpression,
                                                             std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression)
    : Node(line, column), m_logicalOrExpression(std::move(logicalOrExpression)),
      m_optionalExpression(std::move(optionalExpression)),
      m_optionalConditionalExpression(std::move(optionalConditionalExpression))
{}

const OpenCL::Syntax::LogicalOrExpression& OpenCL::Syntax::ConditionalExpression::getLogicalOrExpression() const
{
    return m_logicalOrExpression;
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ConditionalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

const OpenCL::Syntax::ConditionalExpression* OpenCL::Syntax::ConditionalExpression::getOptionalConditionalExpression() const
{
    return m_optionalConditionalExpression.get();
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::ForStatement::getStatement() const
{
    return *m_statement;
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::ForDeclarationStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Syntax::BitAndExpression::BitAndExpression(std::uint64_t line,
                                                   std::uint64_t column,
                                                   EqualityExpression&& equalityExpression,
                                                   std::vector<EqualityExpression>&& optionalEqualityExpressions)
    : Node(line, column), m_equalityExpression(std::move(equalityExpression)),
      m_optionalEqualityExpressions(std::move(optionalEqualityExpressions))
{

}

const OpenCL::Syntax::EqualityExpression& OpenCL::Syntax::BitAndExpression::getEqualityExpression() const
{
    return m_equalityExpression;
}

const std::vector<OpenCL::Syntax::EqualityExpression>& OpenCL::Syntax::BitAndExpression::getOptionalEqualityExpressions() const
{
    return m_optionalEqualityExpressions;
}

OpenCL::Syntax::BitXorExpression::BitXorExpression(std::uint64_t line,
                                                   std::uint64_t column,
                                                   BitAndExpression&& bitAndExpression,
                                                   std::vector<BitAndExpression>&& optionalBitAndExpressions)
    : Node(line, column), m_bitAndExpression(
    std::move(bitAndExpression)), m_optionalBitAndExpressions(std::move(optionalBitAndExpressions))
{}

const OpenCL::Syntax::BitAndExpression& OpenCL::Syntax::BitXorExpression::getBitAndExpression() const
{
    return m_bitAndExpression;
}

const std::vector<OpenCL::Syntax::BitAndExpression>& OpenCL::Syntax::BitXorExpression::getOptionalBitAndExpressions() const
{
    return m_optionalBitAndExpressions;
}

OpenCL::Syntax::BitOrExpression::BitOrExpression(std::uint64_t line,
                                                 std::uint64_t column,
                                                 BitXorExpression&& bitXorExpression,
                                                 std::vector<BitXorExpression>&& optionalBitXorExpressions)
    : Node(line, column), m_bitXorExpression(
    std::move(bitXorExpression)), m_optionalBitXorExpressions(std::move(optionalBitXorExpressions))
{}

const OpenCL::Syntax::BitXorExpression& OpenCL::Syntax::BitOrExpression::getBitXorExpression() const
{
    return m_bitXorExpression;
}

const std::vector<OpenCL::Syntax::BitXorExpression>& OpenCL::Syntax::BitOrExpression::getOptionalBitXorExpressions() const
{
    return m_optionalBitXorExpressions;
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionFunctionCall::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::vector<std::unique_ptr<OpenCL::Syntax::AssignmentExpression>>& OpenCL::Syntax::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmanetExpressions;
}

OpenCL::Syntax::SwitchStatement::SwitchStatement(std::uint64_t line,
                                                 std::uint64_t column,
                                                 Expression&& expression,
                                                 std::unique_ptr<Statement>&& statement)
    : Node(line, column), m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::SwitchStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::SwitchStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Syntax::DefaultStatement::DefaultStatement(std::uint64_t line,
                                                   std::uint64_t column,
                                                   std::unique_ptr<Statement>&& statement)
    : Node(line, column), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::DefaultStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Syntax::CaseStatement::CaseStatement(std::uint64_t line,
                                             std::uint64_t column,
                                             const constantVariant& constant,
                                             std::unique_ptr<Statement>&& statement)
    : Node(line, column), m_constant(constant), m_statement(std::move(statement))
{

}

const OpenCL::Syntax::Statement* OpenCL::Syntax::CaseStatement::getStatement() const
{
    return m_statement.get();
}

const OpenCL::Syntax::CaseStatement::constantVariant& OpenCL::Syntax::CaseStatement::getConstant() const
{
    return m_constant;
}

OpenCL::Syntax::InitializerList::InitializerList(std::uint64_t line,
                                                 std::uint64_t column,
                                                 vector&& nonCommaExpressionsAndBlocks)
    : Node(line, column), m_nonCommaExpressionsAndBlocks(std::move(nonCommaExpressionsAndBlocks))
{}

const typename OpenCL::Syntax::InitializerList::vector& OpenCL::Syntax::InitializerList::getNonCommaExpressionsAndBlocks() const
{
    return m_nonCommaExpressionsAndBlocks;
}

OpenCL::Syntax::EnumDeclaration::EnumDeclaration(std::uint64_t line,
                                                 std::uint64_t column,
                                                 std::string name,
                                                 std::vector<std::pair<std::string, std::int32_t>> values) : Node(line,
                                                                                                                  column),
                                                                                                             m_name(std::move(
                                                                                                                 name)),
                                                                                                             m_values(
                                                                                                                 std::move(
                                                                                                                     values))
{}

const std::string& OpenCL::Syntax::EnumDeclaration::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::string, std::int32_t>>& OpenCL::Syntax::EnumDeclaration::getValues() const
{
    return m_values;
}

OpenCL::Syntax::AssignmentExpression::AssignmentExpression(std::uint64_t line,
                                                           std::uint64_t column,
                                                           std::variant<OpenCL::Syntax::AssignmentExpressionAssignment,
                                                                        OpenCL::Syntax::ConditionalExpression>&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const std::variant<OpenCL::Syntax::AssignmentExpressionAssignment,
                   OpenCL::Syntax::ConditionalExpression>& OpenCL::Syntax::AssignmentExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Initializer::Initializer(std::uint64_t line,
                                         std::uint64_t column,
                                         OpenCL::Syntax::Initializer::variant&& variant) : Node(line,
                                                                                                column),
                                                                                           m_variant(
                                                                                               std::move(
                                                                                                   variant))
{}

const OpenCL::Syntax::Initializer::variant& OpenCL::Syntax::Initializer::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::CompoundItem::CompoundItem(std::uint64_t line,
                                           std::uint64_t column,
                                           OpenCL::Syntax::CompoundItem::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::CompoundItem::variant& OpenCL::Syntax::CompoundItem::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::CompoundItem::variant& OpenCL::Syntax::CompoundItem::getVariant()
{
    return m_variant;
}

OpenCL::Syntax::Statement::Statement(std::uint64_t line,
                                     std::uint64_t column,
                                     OpenCL::Syntax::Statement::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::Statement::variant& OpenCL::Syntax::Statement::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Statement::variant& OpenCL::Syntax::Statement::getVariant()
{
    return m_variant;
}

OpenCL::Syntax::BreakStatement::BreakStatement(std::uint64_t line, std::uint64_t column) : Node(line, column)
{

}

OpenCL::Syntax::EnumSpecifier::EnumSpecifier(std::uint64_t line,
                                             std::uint64_t column,
                                             OpenCL::Syntax::EnumSpecifier::variant&& variant) : Node(line,
                                                                                                      column),
                                                                                                 m_variant(std::move(
                                                                                                     variant))
{}

const OpenCL::Syntax::EnumSpecifier::variant& OpenCL::Syntax::EnumSpecifier::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::TypeSpecifier::TypeSpecifier(std::uint64_t line,
                                             std::uint64_t column,
                                             OpenCL::Syntax::TypeSpecifier::variant&& variant) : Node(line,
                                                                                                      column),
                                                                                                 m_variant(std::move(
                                                                                                     variant))
{}

const OpenCL::Syntax::TypeSpecifier::variant& OpenCL::Syntax::TypeSpecifier::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Declaration::Declaration(std::uint64_t line,
                                         std::uint64_t column,
                                         std::vector<OpenCL::Syntax::DeclarationSpecifier>&& declarationSpecifiers,
                                         std::vector<std::pair<std::unique_ptr<OpenCL::Syntax::Declarator>,
                                                               std::unique_ptr<OpenCL::Syntax::Initializer>>>&& initDeclarators)
    : Node(line, column), m_declarationSpecifiers(std::move(declarationSpecifiers)),
      m_initDeclarators(std::move(initDeclarators))
{
    assert(std::all_of(m_initDeclarators.begin(), m_initDeclarators.end(), [](const auto& pair) -> bool
    {
        return pair.first.get();
    }));
}

const std::vector<OpenCL::Syntax::DeclarationSpecifier>& OpenCL::Syntax::Declaration::getDeclarationSpecifiers() const
{
    return m_declarationSpecifiers;
}

const std::vector<std::pair<std::unique_ptr<OpenCL::Syntax::Declarator>,
                            std::unique_ptr<OpenCL::Syntax::Initializer>>>& OpenCL::Syntax::Declaration::getInitDeclarators() const
{
    return m_initDeclarators;
}

OpenCL::Syntax::StructOrUnionSpecifier::StructOrUnionSpecifier(std::uint64_t line,
                                                               std::uint64_t column,
                                                               bool isUnion,
                                                               const std::string& identifier,
                                                               std::vector<OpenCL::Syntax::StructOrUnionSpecifier::StructDeclaration>&& structDeclarations)
    : Node(line, column), m_isUnion(isUnion), m_identifier(identifier),
      m_structDeclarations(std::move(structDeclarations))
{}

bool OpenCL::Syntax::StructOrUnionSpecifier::isUnion() const
{
    return m_isUnion;
}

const std::string& OpenCL::Syntax::StructOrUnionSpecifier::getIdentifier() const
{
    return m_identifier;
}

const std::vector<OpenCL::Syntax::StructOrUnionSpecifier::StructDeclaration>& OpenCL::Syntax::StructOrUnionSpecifier::getStructDeclarations() const
{
    return m_structDeclarations;
}

OpenCL::Syntax::ParameterList::ParameterList(std::uint64_t line,
                                             std::uint64_t column,
                                             std::vector<OpenCL::Syntax::ParameterDeclaration>&& parameterList)
    : Node(line, column), m_parameterList(std::move(parameterList))
{}

const std::vector<OpenCL::Syntax::ParameterDeclaration>& OpenCL::Syntax::ParameterList::getParameterList() const
{
    return m_parameterList;
}

OpenCL::Syntax::ParameterTypeList::ParameterTypeList(uint64_t line,
                                                     uint64_t column,
                                                     OpenCL::Syntax::ParameterList&& parameterList,
                                                     bool hasEllipse) : Node(line, column),
                                                                        m_parameterList(std::move(parameterList)),
                                                                        m_hasEllipse(hasEllipse)
{}

const OpenCL::Syntax::ParameterList& OpenCL::Syntax::ParameterTypeList::getParameterList() const
{
    return m_parameterList;
}

bool OpenCL::Syntax::ParameterTypeList::hasEllipse() const
{
    return m_hasEllipse;
}

OpenCL::Syntax::Pointer::Pointer(std::uint64_t line,
                                 std::uint64_t column,
                                 std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers) : Node(line, column),
                                                                                                m_typeQualifiers(
                                                                                                    std::move(
                                                                                                        typeQualifiers))
{}

const std::vector<OpenCL::Syntax::TypeQualifier>& OpenCL::Syntax::Pointer::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::DirectDeclaratorNoStaticOrAsterisk(std::uint64_t line,
                                                                                       std::uint64_t column,
                                                                                       std::unique_ptr<OpenCL::Syntax::DirectDeclarator>&& directDeclarator,
                                                                                       std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers,
                                                                                       std::unique_ptr<OpenCL::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(line, column), m_directDeclarator(std::move(directDeclarator)), m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_directDeclarator);
}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<OpenCL::Syntax::TypeQualifier>& OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const std::unique_ptr<OpenCL::Syntax::AssignmentExpression>& OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::getAssignmentExpression() const
{
    return m_assignmentExpression;
}

OpenCL::Syntax::DirectDeclaratorStatic::DirectDeclaratorStatic(std::uint64_t line,
                                                               std::uint64_t column,
                                                               std::unique_ptr<OpenCL::Syntax::DirectDeclarator>&& directDeclarator,
                                                               std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers,
                                                               OpenCL::Syntax::AssignmentExpression&& assignmentExpression)
    : Node(line, column), m_directDeclarator(std::move(directDeclarator)), m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_directDeclarator);
}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorStatic::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<OpenCL::Syntax::TypeQualifier>& OpenCL::Syntax::DirectDeclaratorStatic::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const OpenCL::Syntax::AssignmentExpression& OpenCL::Syntax::DirectDeclaratorStatic::getAssignmentExpression() const
{
    return m_assignmentExpression;
}

OpenCL::Syntax::DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(std::uint64_t line,
                                                                   std::uint64_t column,
                                                                   OpenCL::Syntax::DirectDeclarator&& directDeclarator,
                                                                   std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers)
    : Node(line, column), m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_typeQualifiers(std::move(typeQualifiers))
{}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorAsterisk::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<OpenCL::Syntax::TypeQualifier>& OpenCL::Syntax::DirectDeclaratorAsterisk::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

OpenCL::Syntax::DirectDeclaratorParentheseParameters::DirectDeclaratorParentheseParameters(std::uint64_t line,
                                                                                           std::uint64_t column,
                                                                                           OpenCL::Syntax::DirectDeclarator&& directDeclarator,
                                                                                           OpenCL::Syntax::ParameterTypeList&& parameterTypeList)
    : Node(line, column), m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_parameterTypeList(std::move(parameterTypeList))
{}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorParentheseParameters::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const OpenCL::Syntax::ParameterTypeList& OpenCL::Syntax::DirectDeclaratorParentheseParameters::getParameterTypeList() const
{
    return m_parameterTypeList;
}

OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers::DirectDeclaratorParentheseIdentifiers(std::uint64_t line,
                                                                                             std::uint64_t column,
                                                                                             OpenCL::Syntax::DirectDeclarator&& directDeclarator,
                                                                                             std::vector<std::string>&& identifiers)
    : Node(line, column), m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_identifiers(std::move(identifiers))
{}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<std::string>& OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers::getIdentifiers() const
{
    return m_identifiers;
}

OpenCL::Syntax::DirectDeclarator::DirectDeclarator(std::uint64_t line,
                                                   std::uint64_t column,
                                                   OpenCL::Syntax::DirectDeclarator::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::DirectDeclarator::variant& OpenCL::Syntax::DirectDeclarator::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Declarator::Declarator(std::uint64_t line,
                                       std::uint64_t column,
                                       std::vector<OpenCL::Syntax::Pointer>&& pointers,
                                       OpenCL::Syntax::DirectDeclarator&& directDeclarator) : Node(line, column),
                                                                                              m_pointers(std::move(
                                                                                                  pointers)),
                                                                                              m_directDeclarator(
                                                                                                  std::move(
                                                                                                      directDeclarator))
{}

const std::vector<OpenCL::Syntax::Pointer>& OpenCL::Syntax::Declarator::getPointers() const
{
    return m_pointers;
}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::Declarator::getDirectDeclarator() const
{
    return m_directDeclarator;
}

OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression::DirectAbstractDeclaratorAssignmentExpression(std::uint64_t line,
                                                                                                           std::uint64_t column,
                                                                                                           std::unique_ptr<
                                                                                                               OpenCL::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
                                                                                                           std::unique_ptr<
                                                                                                               OpenCL::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(line, column), m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_assignmentExpression(std::move(assignmentExpression))
{}

const OpenCL::Syntax::DirectAbstractDeclarator* OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const OpenCL::Syntax::AssignmentExpression* OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression::getAssignmentExpression() const
{
    return m_assignmentExpression.get();
}

OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList::DirectAbstractDeclaratorParameterTypeList(std::uint64_t line,
                                                                                                     std::uint64_t column,
                                                                                                     std::unique_ptr<
                                                                                                         OpenCL::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
                                                                                                     std::unique_ptr<
                                                                                                         OpenCL::Syntax::ParameterTypeList>&& parameterTypeList)
    : Node(line, column), m_directAbstractDeclarator(std::move(directAbstractDeclarator)), m_parameterTypeList(std::move(parameterTypeList))
{}

const OpenCL::Syntax::DirectAbstractDeclarator* OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const OpenCL::Syntax::ParameterTypeList* OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList::getParameterTypeList() const
{
    return m_parameterTypeList.get();
}

OpenCL::Syntax::AbstractDeclarator::AbstractDeclarator(std::uint64_t line,
                                                       std::uint64_t column,
                                                       std::vector<OpenCL::Syntax::Pointer>&& pointers,
                                                       OpenCL::Syntax::DirectAbstractDeclarator&& directAbstractDeclarator)
    : Node(line, column), m_pointers(std::move(pointers)), m_directAbstractDeclarator(std::move(directAbstractDeclarator))
{}

const std::vector<OpenCL::Syntax::Pointer>& OpenCL::Syntax::AbstractDeclarator::getPointers() const
{
    return m_pointers;
}

const OpenCL::Syntax::DirectAbstractDeclarator& OpenCL::Syntax::AbstractDeclarator::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator;
}

OpenCL::Syntax::TypeName::TypeName(std::uint64_t line,
                                   std::uint64_t column,
                                   std::vector<OpenCL::Syntax::SpecifierQualifier>&& specifierQualifiers,
                                   std::unique_ptr<OpenCL::Syntax::AbstractDeclarator>&& abstractDeclarator)
    : Node(line, column), m_specifierQualifiers(std::move(specifierQualifiers)), m_abstractDeclarator(std::move(abstractDeclarator))
{}

const std::vector<OpenCL::Syntax::SpecifierQualifier>& OpenCL::Syntax::TypeName::getSpecifierQualifiers() const
{
    return m_specifierQualifiers;
}

const OpenCL::Syntax::AbstractDeclarator* OpenCL::Syntax::TypeName::getAbstractDeclarator() const
{
    return m_abstractDeclarator.get();
}

OpenCL::Syntax::GotoStatement::GotoStatement(std::uint64_t line, std::uint64_t column, const std::string& identifier) : Node(line,
                                                                                                                   column),
                                                                                                              m_identifier(
                                                                                                                  identifier)
{}

const std::string& OpenCL::Syntax::GotoStatement::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::LabelStatement::LabelStatement(std::uint64_t line, std::uint64_t column, const std::string& identifier) : Node(
    line,
    column), m_identifier(identifier)
{}

OpenCL::Syntax::ExternalDeclaration::ExternalDeclaration(std::uint64_t line,
                                                         std::uint64_t column,
                                                         OpenCL::Syntax::ExternalDeclaration::variant&& variant)
                                                         : Node(line,column), m_variant(std::move(variant))
{

}

const OpenCL::Syntax::ExternalDeclaration::variant& OpenCL::Syntax::ExternalDeclaration::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::TranslationUnit::TranslationUnit(std::vector<OpenCL::Syntax::ExternalDeclaration>&& globals) noexcept
: m_globals(std::move(globals))
{

}

const std::vector<OpenCL::Syntax::ExternalDeclaration>& OpenCL::Syntax::TranslationUnit::getGlobals() const
{
    return m_globals;
}

OpenCL::Syntax::FunctionDefinition::FunctionDefinition(std::uint64_t line,
                                                       std::uint64_t column,
                                                       std::vector<OpenCL::Syntax::DeclarationSpecifier>&& declarationSpecifiers,
                                                       OpenCL::Syntax::Declarator&& declarator,
                                                       std::vector<OpenCL::Syntax::Declaration>&& declarations,
                                                       OpenCL::Syntax::CompoundStatement&& compoundStatement)
    : Node(line, column), m_declarationSpecifiers(std::move(declarationSpecifiers)), m_declarator(std::move(declarator)),
      m_declarations(std::move(declarations)), m_compoundStatement(std::move(compoundStatement))
{}

const std::vector<OpenCL::Syntax::DeclarationSpecifier>& OpenCL::Syntax::FunctionDefinition::getDeclarationSpecifiers() const
{
    return m_declarationSpecifiers;
}

const OpenCL::Syntax::Declarator& OpenCL::Syntax::FunctionDefinition::getDeclarator() const
{
    return m_declarator;
}

const std::vector<OpenCL::Syntax::Declaration>& OpenCL::Syntax::FunctionDefinition::getDeclarations() const
{
    return m_declarations;
}

const OpenCL::Syntax::CompoundStatement& OpenCL::Syntax::FunctionDefinition::getCompoundStatement() const
{
    return m_compoundStatement;
}

OpenCL::Syntax::DirectAbstractDeclarator::DirectAbstractDeclarator(std::uint64_t line,
                                                                   std::uint64_t column,
                                                                   OpenCL::Syntax::DirectAbstractDeclarator::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::DirectAbstractDeclarator::variant& OpenCL::Syntax::DirectAbstractDeclarator::getVariant() const
{
    return m_variant;
}

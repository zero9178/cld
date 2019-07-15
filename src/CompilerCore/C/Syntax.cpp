#include "Syntax.hpp"

#include <algorithm>
#include <cassert>
#include <utility>

OpenCL::Syntax::Expression::Expression(std::vector<Lexer::Token>::const_iterator begin,
                                       std::vector<Lexer::Token>::const_iterator end,
                                       std::vector<OpenCL::Syntax::AssignmentExpression> assignmentExpressions)
    : Node(begin, end), m_assignmentExpressions(std::move(assignmentExpressions))
{
}

const std::vector<OpenCL::Syntax::AssignmentExpression>& OpenCL::Syntax::Expression::getAssignmentExpressions() const
{
    return m_assignmentExpressions;
}

OpenCL::Syntax::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::string identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

const std::string& OpenCL::Syntax::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PrimaryExpressionConstant::PrimaryExpressionConstant(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::PrimaryExpressionConstant::variant value)
    : Node(begin, end), m_value(std::move(value))
{
}

const OpenCL::Syntax::PrimaryExpressionConstant::variant& OpenCL::Syntax::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

OpenCL::Syntax::PrimaryExpressionParenthese::PrimaryExpressionParenthese(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::Expression&& expression)
    : Node(begin, end), m_expression(std::move(expression))
{
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PrimaryExpressionParenthese::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::PrimaryExpression&& primaryExpression)
    : Node(begin, end), m_primaryExpression(std::move(primaryExpression))
{
}

const OpenCL::Syntax::PrimaryExpression&
    OpenCL::Syntax::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return m_primaryExpression;
}

OpenCL::Syntax::PostFixExpressionSubscript::PostFixExpressionSubscript(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression, OpenCL::Syntax::Expression&& expression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_expression(std::move(expression))
{
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionSubscript::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PostFixExpressionSubscript::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PostFixExpressionIncrement::PostFixExpressionIncrement(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression))
{
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::PostFixExpressionDecrement::PostFixExpressionDecrement(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression))
{
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::PostFixExpressionDot::PostFixExpressionDot(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression, std::string identifier)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_identifier(std::move(identifier))
{
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionDot::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& OpenCL::Syntax::PostFixExpressionDot::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PostFixExpressionArrow::PostFixExpressionArrow(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression, std::string identifier)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_identifier(std::move(identifier))
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

OpenCL::Syntax::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::PostFixExpression>&& postFixExpression,
    std::vector<std::unique_ptr<OpenCL::Syntax::AssignmentExpression>>&& optionalAssignmanetExpressions)
    : Node(begin, end),
      m_postFixExpression(std::move(postFixExpression)),
      m_optionalAssignmanetExpressions(std::move(optionalAssignmanetExpressions))
{
}

OpenCL::Syntax::PostFixExpressionTypeInitializer::PostFixExpressionTypeInitializer(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end, TypeName&& typeName,
    OpenCL::Syntax::InitializerList&& initializerList)
    : Node(begin, end),
      m_typeName(std::make_unique<TypeName>(std::move(typeName))),
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

OpenCL::Syntax::UnaryExpressionPostFixExpression::UnaryExpressionPostFixExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::PostFixExpression&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression))
{
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return m_postFixExpression;
}

OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryOperator anOperator,
    std::unique_ptr<OpenCL::Syntax::CastExpression>&& unaryExpression)
    : Node(begin, end), m_operator(anOperator), m_castExpression(std::move(unaryExpression))
{
}

OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryOperator
    OpenCL::Syntax::UnaryExpressionUnaryOperator::getAnOperator() const
{
    return m_operator;
}

const OpenCL::Syntax::CastExpression& OpenCL::Syntax::UnaryExpressionUnaryOperator::getCastExpression() const
{
    return *m_castExpression;
}

OpenCL::Syntax::UnaryExpressionSizeOf::UnaryExpressionSizeOf(std::vector<Lexer::Token>::const_iterator begin,
                                                             std::vector<Lexer::Token>::const_iterator end,
                                                             OpenCL::Syntax::UnaryExpressionSizeOf::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const OpenCL::Syntax::UnaryExpressionSizeOf::variant& OpenCL::Syntax::UnaryExpressionSizeOf::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::AssignmentExpressionAssignment::AssignmentExpressionAssignment(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::UnaryExpression&& unaryFactor,
    OpenCL::Syntax::AssignmentExpressionAssignment::AssignOperator assignOperator,
    std::unique_ptr<OpenCL::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_unaryFactor(std::move(unaryFactor)),
      m_assignOperator(assignOperator),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_assignmentExpression);
}

const OpenCL::Syntax::UnaryExpression& OpenCL::Syntax::AssignmentExpressionAssignment::getUnaryFactor() const
{
    return m_unaryFactor;
}

OpenCL::Syntax::AssignmentExpressionAssignment::AssignOperator
    OpenCL::Syntax::AssignmentExpressionAssignment::getAssignOperator() const
{
    return m_assignOperator;
}

const OpenCL::Syntax::AssignmentExpression&
    OpenCL::Syntax::AssignmentExpressionAssignment::getAssignmentExpression() const
{
    return *m_assignmentExpression;
}

OpenCL::Syntax::CastExpression::CastExpression(std::vector<Lexer::Token>::const_iterator begin,
                                               std::vector<Lexer::Token>::const_iterator end,
                                               OpenCL::Syntax::CastExpression::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const OpenCL::Syntax::CastExpression::variant& OpenCL::Syntax::CastExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Term::Term(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::CastExpression&& castExpressions,
    std::vector<std::pair<OpenCL::Syntax::Term::BinaryDotOperator, OpenCL::Syntax::CastExpression>>&&
        optionalCastExpressions)
    : Node(begin, end),
      m_castExpression(std::move(castExpressions)),
      m_optionalCastExpressions(std::move(optionalCastExpressions))
{
}

const OpenCL::Syntax::CastExpression& OpenCL::Syntax::Term::getCastExpression() const
{
    return m_castExpression;
}

const std::vector<std::pair<OpenCL::Syntax::Term::BinaryDotOperator, OpenCL::Syntax::CastExpression>>&
    OpenCL::Syntax::Term::getOptionalCastExpressions() const
{
    return m_optionalCastExpressions;
}

OpenCL::Syntax::AdditiveExpression::AdditiveExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::Term&& term,
    std::vector<std::pair<OpenCL::Syntax::AdditiveExpression::BinaryDashOperator, OpenCL::Syntax::Term>>&&
        optionalTerms)
    : Node(begin, end), m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{
}

const OpenCL::Syntax::Term& OpenCL::Syntax::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<std::pair<OpenCL::Syntax::AdditiveExpression::BinaryDashOperator, OpenCL::Syntax::Term>>&
    OpenCL::Syntax::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

OpenCL::Syntax::ShiftExpression::ShiftExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::AdditiveExpression&& additiveExpression,
    std::vector<std::pair<OpenCL::Syntax::ShiftExpression::ShiftOperator, OpenCL::Syntax::AdditiveExpression>>&&
        optionalAdditiveExpressions)
    : Node(begin, end),
      m_additiveExpression(std::move(additiveExpression)),
      m_optionalAdditiveExpressions(std::move(optionalAdditiveExpressions))
{
}

const OpenCL::Syntax::AdditiveExpression& OpenCL::Syntax::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ReturnStatement::getExpression() const
{
    return m_expression.get();
}

OpenCL::Syntax::ReturnStatement::ReturnStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                 std::vector<Lexer::Token>::const_iterator end,
                                                 std::unique_ptr<Expression>&& expression)
    : Node(begin, end), m_expression(std::move(expression))
{
}

OpenCL::Syntax::ExpressionStatement::ExpressionStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                         std::vector<Lexer::Token>::const_iterator end,
                                                         std::unique_ptr<Expression>&& optionalExpression)
    : Node(begin, end), m_optionalExpression(std::move(optionalExpression))
{
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ExpressionStatement::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

std::unique_ptr<OpenCL::Syntax::Expression> OpenCL::Syntax::ExpressionStatement::moveOptionalExpression()
{
    return std::move(m_optionalExpression);
}

OpenCL::Syntax::IfStatement::IfStatement(std::vector<Lexer::Token>::const_iterator begin,
                                         std::vector<Lexer::Token>::const_iterator end, Expression&& expression,
                                         std::unique_ptr<Statement>&& branch, std::unique_ptr<Statement>&& elseBranch)
    : Node(begin, end),
      m_expression(std::move(expression)),
      m_branch(std::move(branch)),
      m_elseBranch(std::move(elseBranch))
{
    assert(m_branch);
}

OpenCL::Syntax::ContinueStatement::ContinueStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                     std::vector<Lexer::Token>::const_iterator end)
    : Node(begin, end)
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

OpenCL::Syntax::CompoundStatement::CompoundStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                     std::vector<Lexer::Token>::const_iterator end,
                                                     std::vector<CompoundItem>&& blockItems)
    : Node(begin, end), m_blockItems(std::move(blockItems))
{
}

const std::vector<OpenCL::Syntax::CompoundItem>& OpenCL::Syntax::CompoundStatement::getBlockItems() const
{
    return m_blockItems;
}

OpenCL::Syntax::ForStatement::ForStatement(std::vector<Lexer::Token>::const_iterator begin,
                                           std::vector<Lexer::Token>::const_iterator end,
                                           std::unique_ptr<Statement>&& statement,
                                           std::variant<Declaration, std::unique_ptr<Expression>>&& initial,
                                           std::unique_ptr<Expression>&& controlling,
                                           std::unique_ptr<Expression>&& post)
    : Node(begin, end),
      m_statement(std::move(statement)),
      m_initial(std::move(initial)),
      m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const std::variant<OpenCL::Syntax::Declaration, std::unique_ptr<OpenCL::Syntax::Expression>>&
    OpenCL::Syntax::ForStatement::getInitial() const
{
    return m_initial;
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ForStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Syntax::HeadWhileStatement::HeadWhileStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                       std::vector<Lexer::Token>::const_iterator end,
                                                       Expression&& expression, std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_expression(std::move(expression)), m_statement(std::move(statement))
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

OpenCL::Syntax::FootWhileStatement::FootWhileStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                       std::vector<Lexer::Token>::const_iterator end,
                                                       std::unique_ptr<Statement>&& statement, Expression&& expression)
    : Node(begin, end), m_statement(std::move(statement)), m_expression(std::move(expression))
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

const std::vector<std::pair<OpenCL::Syntax::ShiftExpression::ShiftOperator, OpenCL::Syntax::AdditiveExpression>>&
    OpenCL::Syntax::ShiftExpression::getOptionalAdditiveExpressions() const
{
    return m_optionalAdditiveExpressions;
}

OpenCL::Syntax::RelationalExpression::RelationalExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    ShiftExpression&& shiftExpression,
    std::vector<std::pair<RelationalOperator, ShiftExpression>>&& optionalRelationalExpressions)
    : Node(begin, end),
      m_shiftExpression(std::move(shiftExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{
}

const OpenCL::Syntax::ShiftExpression& OpenCL::Syntax::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

const std::vector<std::pair<OpenCL::Syntax::RelationalExpression::RelationalOperator, OpenCL::Syntax::ShiftExpression>>&
    OpenCL::Syntax::RelationalExpression::getOptionalShiftExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Syntax::EqualityExpression::EqualityExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    RelationalExpression&& relationalExpression,
    std::vector<std::pair<EqualityOperator, RelationalExpression>>&& optionalRelationalExpressions)
    : Node(begin, end),
      m_relationalExpression(std::move(relationalExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{
}

const OpenCL::Syntax::RelationalExpression& OpenCL::Syntax::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

const std::vector<
    std::pair<OpenCL::Syntax::EqualityExpression::EqualityOperator, OpenCL::Syntax::RelationalExpression>>&
    OpenCL::Syntax::EqualityExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Syntax::LogicalAndExpression::LogicalAndExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                           std::vector<Lexer::Token>::const_iterator end,
                                                           BitOrExpression&& equalityExpression,
                                                           std::vector<BitOrExpression>&& optionalEqualityExpressions)
    : Node(begin, end),
      m_bitOrExpression(std::move(equalityExpression)),
      m_optionalBitOrExpressions(std::move(optionalEqualityExpressions))
{
}

const OpenCL::Syntax::BitOrExpression& OpenCL::Syntax::LogicalAndExpression::getBitOrExpression() const
{
    return m_bitOrExpression;
}

const std::vector<OpenCL::Syntax::BitOrExpression>&
    OpenCL::Syntax::LogicalAndExpression::getOptionalBitOrExpressions() const
{
    return m_optionalBitOrExpressions;
}

OpenCL::Syntax::LogicalOrExpression::LogicalOrExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                         std::vector<Lexer::Token>::const_iterator end,
                                                         LogicalAndExpression&& andExpression,
                                                         std::vector<LogicalAndExpression>&& optionalAndExpressions)
    : Node(begin, end),
      m_andExpression(std::move(andExpression)),
      m_optionalAndExpressions(std::move(optionalAndExpressions))
{
}

const OpenCL::Syntax::LogicalAndExpression& OpenCL::Syntax::LogicalOrExpression::getAndExpression() const
{
    return m_andExpression;
}

const std::vector<OpenCL::Syntax::LogicalAndExpression>&
    OpenCL::Syntax::LogicalOrExpression::getOptionalAndExpressions() const
{
    return m_optionalAndExpressions;
}

OpenCL::Syntax::ConditionalExpression::ConditionalExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    LogicalOrExpression&& logicalOrExpression, std::unique_ptr<Expression>&& optionalExpression,
    std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression)
    : Node(begin, end),
      m_logicalOrExpression(std::move(logicalOrExpression)),
      m_optionalExpression(std::move(optionalExpression)),
      m_optionalConditionalExpression(std::move(optionalConditionalExpression))
{
}

const OpenCL::Syntax::LogicalOrExpression& OpenCL::Syntax::ConditionalExpression::getLogicalOrExpression() const
{
    return m_logicalOrExpression;
}

const OpenCL::Syntax::Expression* OpenCL::Syntax::ConditionalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

const OpenCL::Syntax::ConditionalExpression*
    OpenCL::Syntax::ConditionalExpression::getOptionalConditionalExpression() const
{
    return m_optionalConditionalExpression.get();
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::ForStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Syntax::BitAndExpression::BitAndExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                   std::vector<Lexer::Token>::const_iterator end,
                                                   EqualityExpression&& equalityExpression,
                                                   std::vector<EqualityExpression>&& optionalEqualityExpressions)
    : Node(begin, end),
      m_equalityExpression(std::move(equalityExpression)),
      m_optionalEqualityExpressions(std::move(optionalEqualityExpressions))
{
}

const OpenCL::Syntax::EqualityExpression& OpenCL::Syntax::BitAndExpression::getEqualityExpression() const
{
    return m_equalityExpression;
}

const std::vector<OpenCL::Syntax::EqualityExpression>&
    OpenCL::Syntax::BitAndExpression::getOptionalEqualityExpressions() const
{
    return m_optionalEqualityExpressions;
}

OpenCL::Syntax::BitXorExpression::BitXorExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                   std::vector<Lexer::Token>::const_iterator end,
                                                   BitAndExpression&& bitAndExpression,
                                                   std::vector<BitAndExpression>&& optionalBitAndExpressions)
    : Node(begin, end),
      m_bitAndExpression(std::move(bitAndExpression)),
      m_optionalBitAndExpressions(std::move(optionalBitAndExpressions))
{
}

const OpenCL::Syntax::BitAndExpression& OpenCL::Syntax::BitXorExpression::getBitAndExpression() const
{
    return m_bitAndExpression;
}

const std::vector<OpenCL::Syntax::BitAndExpression>&
    OpenCL::Syntax::BitXorExpression::getOptionalBitAndExpressions() const
{
    return m_optionalBitAndExpressions;
}

OpenCL::Syntax::BitOrExpression::BitOrExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                 std::vector<Lexer::Token>::const_iterator end,
                                                 BitXorExpression&& bitXorExpression,
                                                 std::vector<BitXorExpression>&& optionalBitXorExpressions)
    : Node(begin, end),
      m_bitXorExpression(std::move(bitXorExpression)),
      m_optionalBitXorExpressions(std::move(optionalBitXorExpressions))
{
}

const OpenCL::Syntax::BitXorExpression& OpenCL::Syntax::BitOrExpression::getBitXorExpression() const
{
    return m_bitXorExpression;
}

const std::vector<OpenCL::Syntax::BitXorExpression>&
    OpenCL::Syntax::BitOrExpression::getOptionalBitXorExpressions() const
{
    return m_optionalBitXorExpressions;
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionFunctionCall::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::vector<std::unique_ptr<OpenCL::Syntax::AssignmentExpression>>&
    OpenCL::Syntax::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmanetExpressions;
}

OpenCL::Syntax::SwitchStatement::SwitchStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                 std::vector<Lexer::Token>::const_iterator end, Expression&& expression,
                                                 std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_expression(std::move(expression)), m_statement(std::move(statement))
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

OpenCL::Syntax::DefaultStatement::DefaultStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                   std::vector<Lexer::Token>::const_iterator end,
                                                   std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Syntax::Statement& OpenCL::Syntax::DefaultStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Syntax::CaseStatement::CaseStatement(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             AssignmentExpression&& constantExpression,
                                             std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_constantExpression(std::move(constantExpression)), m_statement(std::move(statement))
{
}

const OpenCL::Syntax::Statement* OpenCL::Syntax::CaseStatement::getStatement() const
{
    return m_statement.get();
}

const OpenCL::Syntax::AssignmentExpression& OpenCL::Syntax::CaseStatement::getConstantExpression() const
{
    return m_constantExpression;
}

OpenCL::Syntax::InitializerList::InitializerList(std::vector<Lexer::Token>::const_iterator begin,
                                                 std::vector<Lexer::Token>::const_iterator end,
                                                 vector&& nonCommaExpressionsAndBlocks)
    : Node(begin, end), m_nonCommaExpressionsAndBlocks(std::move(nonCommaExpressionsAndBlocks))
{
}

const typename OpenCL::Syntax::InitializerList::vector&
    OpenCL::Syntax::InitializerList::getNonCommaExpressionsAndBlocks() const
{
    return m_nonCommaExpressionsAndBlocks;
}

OpenCL::Syntax::EnumDeclaration::EnumDeclaration(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end, std::string name,
    std::vector<std::pair<std::string, std::optional<ConstantExpression>>>&& values)
    : Node(begin, end), m_name(std::move(name)), m_values(std::move(values))
{
}

const std::string& OpenCL::Syntax::EnumDeclaration::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::string, std::optional<OpenCL::Syntax::ConstantExpression>>>&
    OpenCL::Syntax::EnumDeclaration::getValues() const
{
    return m_values;
}

OpenCL::Syntax::AssignmentExpression::AssignmentExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::variant<OpenCL::Syntax::AssignmentExpressionAssignment, OpenCL::Syntax::ConditionalExpression>&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const std::variant<OpenCL::Syntax::AssignmentExpressionAssignment, OpenCL::Syntax::ConditionalExpression>&
    OpenCL::Syntax::AssignmentExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Initializer::Initializer(std::vector<Lexer::Token>::const_iterator begin,
                                         std::vector<Lexer::Token>::const_iterator end,
                                         OpenCL::Syntax::Initializer::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const OpenCL::Syntax::Initializer::variant& OpenCL::Syntax::Initializer::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::BreakStatement::BreakStatement(std::vector<Lexer::Token>::const_iterator begin,
                                               std::vector<Lexer::Token>::const_iterator end)
    : Node(begin, end)
{
}

OpenCL::Syntax::EnumSpecifier::EnumSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             OpenCL::Syntax::EnumSpecifier::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const OpenCL::Syntax::EnumSpecifier::variant& OpenCL::Syntax::EnumSpecifier::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::TypeSpecifier::TypeSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             OpenCL::Syntax::TypeSpecifier::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const OpenCL::Syntax::TypeSpecifier::variant& OpenCL::Syntax::TypeSpecifier::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::Declaration::Declaration(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::vector<OpenCL::Syntax::DeclarationSpecifier>&& declarationSpecifiers,
    std::vector<std::pair<std::unique_ptr<OpenCL::Syntax::Declarator>, std::unique_ptr<OpenCL::Syntax::Initializer>>>&&
        initDeclarators)
    : Node(begin, end),
      m_declarationSpecifiers(std::move(declarationSpecifiers)),
      m_initDeclarators(std::move(initDeclarators))
{
    assert(std::all_of(m_initDeclarators.begin(), m_initDeclarators.end(),
                       [](const auto& pair) -> bool { return pair.first.get(); }));
}

const std::vector<OpenCL::Syntax::DeclarationSpecifier>& OpenCL::Syntax::Declaration::getDeclarationSpecifiers() const
{
    return m_declarationSpecifiers;
}

const std::vector<std::pair<std::unique_ptr<OpenCL::Syntax::Declarator>, std::unique_ptr<OpenCL::Syntax::Initializer>>>&
    OpenCL::Syntax::Declaration::getInitDeclarators() const
{
    return m_initDeclarators;
}

OpenCL::Syntax::StructOrUnionSpecifier::StructOrUnionSpecifier(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end, bool isUnion,
    std::string identifier, std::vector<OpenCL::Syntax::StructOrUnionSpecifier::StructDeclaration>&& structDeclarations)
    : Node(begin, end),
      m_isUnion(isUnion),
      m_identifier(std::move(identifier)),
      m_structDeclarations(std::move(structDeclarations))
{
}

bool OpenCL::Syntax::StructOrUnionSpecifier::isUnion() const
{
    return m_isUnion;
}

const std::string& OpenCL::Syntax::StructOrUnionSpecifier::getIdentifier() const
{
    return m_identifier;
}

const std::vector<OpenCL::Syntax::StructOrUnionSpecifier::StructDeclaration>&
    OpenCL::Syntax::StructOrUnionSpecifier::getStructDeclarations() const
{
    return m_structDeclarations;
}

OpenCL::Syntax::ParameterList::ParameterList(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             std::vector<OpenCL::Syntax::ParameterDeclaration>&& parameterList)
    : Node(begin, end), m_parameterList(std::move(parameterList))
{
}

const std::vector<OpenCL::Syntax::ParameterDeclaration>& OpenCL::Syntax::ParameterList::getParameterDeclarations() const
{
    return m_parameterList;
}

OpenCL::Syntax::ParameterTypeList::ParameterTypeList(std::vector<Lexer::Token>::const_iterator begin,
                                                     std::vector<Lexer::Token>::const_iterator end,
                                                     OpenCL::Syntax::ParameterList&& parameterList, bool hasEllipse)
    : Node(begin, end), m_parameterList(std::move(parameterList)), m_hasEllipse(hasEllipse)
{
}

const OpenCL::Syntax::ParameterList& OpenCL::Syntax::ParameterTypeList::getParameterList() const
{
    return m_parameterList;
}

bool OpenCL::Syntax::ParameterTypeList::hasEllipse() const
{
    return m_hasEllipse;
}

OpenCL::Syntax::Pointer::Pointer(std::vector<Lexer::Token>::const_iterator begin,
                                 std::vector<Lexer::Token>::const_iterator end,
                                 std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers)
    : Node(begin, end), m_typeQualifiers(std::move(typeQualifiers))
{
}

const std::vector<OpenCL::Syntax::TypeQualifier>& OpenCL::Syntax::Pointer::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::DirectDeclaratorNoStaticOrAsterisk(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::DirectDeclarator>&& directDeclarator,
    std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers,
    std::unique_ptr<OpenCL::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_directDeclarator(std::move(directDeclarator)),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_directDeclarator);
}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<OpenCL::Syntax::TypeQualifier>&
    OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const std::unique_ptr<OpenCL::Syntax::AssignmentExpression>&
    OpenCL::Syntax::DirectDeclaratorNoStaticOrAsterisk::getAssignmentExpression() const
{
    return m_assignmentExpression;
}

OpenCL::Syntax::DirectDeclaratorStatic::DirectDeclaratorStatic(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::DirectDeclarator>&& directDeclarator,
    std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers,
    OpenCL::Syntax::AssignmentExpression&& assignmentExpression)
    : Node(begin, end),
      m_directDeclarator(std::move(directDeclarator)),
      m_typeQualifiers(std::move(typeQualifiers)),
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

OpenCL::Syntax::DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::DirectDeclarator&& directDeclarator, std::vector<OpenCL::Syntax::TypeQualifier>&& typeQualifiers)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_typeQualifiers(std::move(typeQualifiers))
{
}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::DirectDeclaratorAsterisk::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<OpenCL::Syntax::TypeQualifier>& OpenCL::Syntax::DirectDeclaratorAsterisk::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

OpenCL::Syntax::DirectDeclaratorParentheseParameters::DirectDeclaratorParentheseParameters(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::DirectDeclarator&& directDeclarator, OpenCL::Syntax::ParameterTypeList&& parameterTypeList)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_parameterTypeList(std::move(parameterTypeList))
{
}

const OpenCL::Syntax::DirectDeclarator&
    OpenCL::Syntax::DirectDeclaratorParentheseParameters::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const OpenCL::Syntax::ParameterTypeList&
    OpenCL::Syntax::DirectDeclaratorParentheseParameters::getParameterTypeList() const
{
    return m_parameterTypeList;
}

OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers::DirectDeclaratorParentheseIdentifiers(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::DirectDeclarator&& directDeclarator,
    std::vector<std::pair<std::string, std::vector<Lexer::Token>::const_iterator>>&& identifiers)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_identifiers(std::move(identifiers))
{
}

const OpenCL::Syntax::DirectDeclarator&
    OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<std::pair<std::string, std::vector<OpenCL::Lexer::Token>::const_iterator>>&
    OpenCL::Syntax::DirectDeclaratorParentheseIdentifiers::getIdentifiers() const
{
    return m_identifiers;
}

OpenCL::Syntax::Declarator::Declarator(std::vector<Lexer::Token>::const_iterator begin,
                                       std::vector<Lexer::Token>::const_iterator end,
                                       std::vector<OpenCL::Syntax::Pointer>&& pointers,
                                       OpenCL::Syntax::DirectDeclarator&& directDeclarator)
    : Node(begin, end), m_pointers(std::move(pointers)), m_directDeclarator(std::move(directDeclarator))
{
}

const std::vector<OpenCL::Syntax::Pointer>& OpenCL::Syntax::Declarator::getPointers() const
{
    return m_pointers;
}

const OpenCL::Syntax::DirectDeclarator& OpenCL::Syntax::Declarator::getDirectDeclarator() const
{
    return m_directDeclarator;
}

OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression::DirectAbstractDeclaratorAssignmentExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
    std::unique_ptr<OpenCL::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_assignmentExpression(std::move(assignmentExpression))
{
}

const OpenCL::Syntax::DirectAbstractDeclarator*
    OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const OpenCL::Syntax::AssignmentExpression*
    OpenCL::Syntax::DirectAbstractDeclaratorAssignmentExpression::getAssignmentExpression() const
{
    return m_assignmentExpression.get();
}

OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList::DirectAbstractDeclaratorParameterTypeList(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<OpenCL::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
    std::unique_ptr<OpenCL::Syntax::ParameterTypeList>&& parameterTypeList)
    : Node(begin, end),
      m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_parameterTypeList(std::move(parameterTypeList))
{
}

const OpenCL::Syntax::DirectAbstractDeclarator*
    OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const OpenCL::Syntax::ParameterTypeList*
    OpenCL::Syntax::DirectAbstractDeclaratorParameterTypeList::getParameterTypeList() const
{
    return m_parameterTypeList.get();
}

OpenCL::Syntax::AbstractDeclarator::AbstractDeclarator(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::vector<OpenCL::Syntax::Pointer>&& pointers, std::optional<DirectAbstractDeclarator>&& directAbstractDeclarator)
    : Node(begin, end), m_pointers(std::move(pointers)), m_directAbstractDeclarator(std::move(directAbstractDeclarator))
{
}

const std::vector<OpenCL::Syntax::Pointer>& OpenCL::Syntax::AbstractDeclarator::getPointers() const
{
    return m_pointers;
}

const OpenCL::Syntax::DirectAbstractDeclarator* OpenCL::Syntax::AbstractDeclarator::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator ? &*m_directAbstractDeclarator : nullptr;
}

OpenCL::Syntax::TypeName::TypeName(std::vector<Lexer::Token>::const_iterator begin,
                                   std::vector<Lexer::Token>::const_iterator end,
                                   std::vector<OpenCL::Syntax::SpecifierQualifier>&& specifierQualifiers,
                                   std::unique_ptr<OpenCL::Syntax::AbstractDeclarator>&& abstractDeclarator)
    : Node(begin, end),
      m_specifierQualifiers(std::move(specifierQualifiers)),
      m_abstractDeclarator(std::move(abstractDeclarator))
{
}

const std::vector<OpenCL::Syntax::SpecifierQualifier>& OpenCL::Syntax::TypeName::getSpecifierQualifiers() const
{
    return m_specifierQualifiers;
}

const OpenCL::Syntax::AbstractDeclarator* OpenCL::Syntax::TypeName::getAbstractDeclarator() const
{
    return m_abstractDeclarator.get();
}

OpenCL::Syntax::GotoStatement::GotoStatement(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end, std::string identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

const std::string& OpenCL::Syntax::GotoStatement::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::LabelStatement::LabelStatement(std::vector<Lexer::Token>::const_iterator begin,
                                               std::vector<Lexer::Token>::const_iterator end, std::string identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

OpenCL::Syntax::TranslationUnit::TranslationUnit(std::vector<OpenCL::Syntax::ExternalDeclaration>&& globals) noexcept
    : m_globals(std::move(globals))
{
}

const std::vector<OpenCL::Syntax::ExternalDeclaration>& OpenCL::Syntax::TranslationUnit::getGlobals() const
{
    return m_globals;
}

OpenCL::Syntax::FunctionDefinition::FunctionDefinition(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::vector<OpenCL::Syntax::DeclarationSpecifier>&& declarationSpecifiers, OpenCL::Syntax::Declarator&& declarator,
    std::vector<OpenCL::Syntax::Declaration>&& declarations, OpenCL::Syntax::CompoundStatement&& compoundStatement)
    : Node(begin, end),
      m_declarationSpecifiers(std::move(declarationSpecifiers)),
      m_declarator(std::move(declarator)),
      m_declarations(std::move(declarations)),
      m_compoundStatement(std::move(compoundStatement))
{
}

const std::vector<OpenCL::Syntax::DeclarationSpecifier>&
    OpenCL::Syntax::FunctionDefinition::getDeclarationSpecifiers() const
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

OpenCL::Syntax::Node::Node(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                           std::vector<OpenCL::Lexer::Token>::const_iterator end)
    : m_begin(begin), m_end(end)
{
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::Syntax::Node::begin() const
{
    return m_begin;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::Syntax::Node::end() const
{
    return m_end;
}

OpenCL::Syntax::TypeQualifier::TypeQualifier(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             OpenCL::Syntax::TypeQualifier::Qualifier qualifier)
    : Node(begin, end), m_qualifier(qualifier)
{
}

OpenCL::Syntax::TypeQualifier::Qualifier OpenCL::Syntax::TypeQualifier::getQualifier() const
{
    return m_qualifier;
}

OpenCL::Syntax::StorageClassSpecifier::StorageClassSpecifier(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    OpenCL::Syntax::StorageClassSpecifier::Specifiers specifier)
    : Node(begin, end), m_specifier(specifier)
{
}

OpenCL::Syntax::StorageClassSpecifier::Specifiers OpenCL::Syntax::StorageClassSpecifier::getSpecifier() const
{
    return m_specifier;
}

OpenCL::Syntax::FunctionSpecifier::FunctionSpecifier(const std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                     const std::vector<OpenCL::Lexer::Token>::const_iterator& end)
    : Node(begin, end)
{
}

OpenCL::Syntax::DirectDeclaratorIdentifier::DirectDeclaratorIdentifier(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::string identifier, std::vector<Lexer::Token>::const_iterator identifierLoc)
    : Node(begin, end), m_identifier(std::move(identifier)), m_identifierLoc(identifierLoc)
{
}

const std::string& OpenCL::Syntax::DirectDeclaratorIdentifier::getIdentifier() const
{
    return m_identifier;
}

std::vector<OpenCL::Lexer::Token>::const_iterator OpenCL::Syntax::DirectDeclaratorIdentifier::getIdentifierLoc() const
{
    return m_identifierLoc;
}

OpenCL::Syntax::DirectDeclaratorParenthese::DirectDeclaratorParenthese(std::vector<Lexer::Token>::const_iterator begin,
                                                                       std::vector<Lexer::Token>::const_iterator end,
                                                                       std::unique_ptr<Declarator>&& declarator)
    : Node(begin, end), m_declarator(std::move(declarator))
{
}

const OpenCL::Syntax::Declarator& OpenCL::Syntax::DirectDeclaratorParenthese::getDeclarator() const
{
    return *m_declarator;
}

OpenCL::Syntax::DirectAbstractDeclaratorParenthese::DirectAbstractDeclaratorParenthese(
    std::vector<OpenCL::Lexer::Token>::const_iterator begin, std::vector<OpenCL::Lexer::Token>::const_iterator end,
    std::unique_ptr<AbstractDeclarator>&& abstractDeclarator)
    : Node(begin, end), m_abstractDeclarator(std::move(abstractDeclarator))
{
}

const OpenCL::Syntax::AbstractDeclarator&
    OpenCL::Syntax::DirectAbstractDeclaratorParenthese::getAbstractDeclarator() const
{
    return *m_abstractDeclarator;
}

OpenCL::Syntax::DirectAbstractDeclaratorAsterisk::DirectAbstractDeclaratorAsterisk(
    std::vector<OpenCL::Lexer::Token>::const_iterator begin, std::vector<OpenCL::Lexer::Token>::const_iterator end,
    std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator)
    : Node(begin, end), m_directAbstractDeclarator(std::move(directAbstractDeclarator))
{
}

const std::unique_ptr<OpenCL::Syntax::DirectAbstractDeclarator>&
    OpenCL::Syntax::DirectAbstractDeclaratorAsterisk::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator;
}

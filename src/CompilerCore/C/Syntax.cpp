#include "Syntax.hpp"

#include <algorithm>
#include <cassert>
#include <utility>

cld::Syntax::Expression::Expression(std::vector<Lexer::Token>::const_iterator begin,
                                    std::vector<Lexer::Token>::const_iterator end,
                                    std::vector<cld::Syntax::AssignmentExpression> assignmentExpressions)
    : Node(begin, end), m_assignmentExpressions(std::move(assignmentExpressions))
{
}

const std::vector<cld::Syntax::AssignmentExpression>& cld::Syntax::Expression::getAssignmentExpressions() const
{
    return m_assignmentExpressions;
}

cld::Syntax::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(std::vector<Lexer::Token>::const_iterator begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      std::string identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

const std::string& cld::Syntax::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::PrimaryExpressionConstant::PrimaryExpressionConstant(std::vector<Lexer::Token>::const_iterator begin,
                                                                  std::vector<Lexer::Token>::const_iterator end,
                                                                  cld::Syntax::PrimaryExpressionConstant::variant value,
                                                                  Lexer::Token::Type type)
    : Node(begin, end), m_value(std::move(value)), m_type(type)
{
}

const cld::Syntax::PrimaryExpressionConstant::variant& cld::Syntax::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

cld::Lexer::Token::Type cld::Syntax::PrimaryExpressionConstant::getType() const
{
    return m_type;
}

cld::Syntax::PrimaryExpressionParenthese::PrimaryExpressionParenthese(std::vector<Lexer::Token>::const_iterator begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      cld::Syntax::Expression&& expression)
    : Node(begin, end), m_expression(std::move(expression))
{
}

const cld::Syntax::Expression& cld::Syntax::PrimaryExpressionParenthese::getExpression() const
{
    return m_expression;
}

cld::Syntax::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::PrimaryExpression&& primaryExpression)
    : Node(begin, end), m_primaryExpression(std::move(primaryExpression))
{
}

const cld::Syntax::PrimaryExpression& cld::Syntax::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return m_primaryExpression;
}

cld::Syntax::PostFixExpressionSubscript::PostFixExpressionSubscript(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression, cld::Syntax::Expression&& expression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_expression(std::move(expression))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionSubscript::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const cld::Syntax::Expression& cld::Syntax::PostFixExpressionSubscript::getExpression() const
{
    return m_expression;
}

cld::Syntax::PostFixExpressionIncrement::PostFixExpressionIncrement(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Syntax::PostFixExpressionDecrement::PostFixExpressionDecrement(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Syntax::PostFixExpressionDot::PostFixExpressionDot(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression, std::string identifier)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_identifier(std::move(identifier))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionDot::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& cld::Syntax::PostFixExpressionDot::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::PostFixExpressionArrow::PostFixExpressionArrow(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression, std::string identifier)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_identifier(std::move(identifier))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionArrow::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& cld::Syntax::PostFixExpressionArrow::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression,
    std::vector<std::unique_ptr<cld::Syntax::AssignmentExpression>>&& optionalAssignmanetExpressions)
    : Node(begin, end),
      m_postFixExpression(std::move(postFixExpression)),
      m_optionalAssignmanetExpressions(std::move(optionalAssignmanetExpressions))
{
}

cld::Syntax::PostFixExpressionTypeInitializer::PostFixExpressionTypeInitializer(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end, TypeName&& typeName,
    cld::Syntax::InitializerList&& initializerList)
    : Node(begin, end),
      m_typeName(std::make_unique<TypeName>(std::move(typeName))),
      m_initializerList(std::make_unique<InitializerList>(std::move(initializerList)))
{
}

const cld::Syntax::InitializerList& cld::Syntax::PostFixExpressionTypeInitializer::getInitializerList() const
{
    return *m_initializerList;
}

const cld::Syntax::TypeName& cld::Syntax::PostFixExpressionTypeInitializer::getTypeName() const
{
    return *m_typeName;
}

cld::Syntax::UnaryExpressionPostFixExpression::UnaryExpressionPostFixExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::PostFixExpression&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::make_unique<PostFixExpression>(std::move(postFixExpression)))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Syntax::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::UnaryExpressionUnaryOperator::UnaryOperator anOperator,
    std::unique_ptr<cld::Syntax::CastExpression>&& unaryExpression)
    : Node(begin, end), m_castExpression(std::move(unaryExpression)), m_operator(anOperator)
{
}

cld::Syntax::UnaryExpressionUnaryOperator::UnaryOperator
    cld::Syntax::UnaryExpressionUnaryOperator::getAnOperator() const
{
    return m_operator;
}

const cld::Syntax::CastExpression& cld::Syntax::UnaryExpressionUnaryOperator::getCastExpression() const
{
    return *m_castExpression;
}

cld::Syntax::UnaryExpressionSizeOf::UnaryExpressionSizeOf(std::vector<Lexer::Token>::const_iterator begin,
                                                          std::vector<Lexer::Token>::const_iterator end,
                                                          cld::Syntax::UnaryExpressionSizeOf::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::UnaryExpressionSizeOf::variant& cld::Syntax::UnaryExpressionSizeOf::getVariant() const
{
    return m_variant;
}

cld::Syntax::CastExpression::CastExpression(std::vector<Lexer::Token>::const_iterator begin,
                                            std::vector<Lexer::Token>::const_iterator end,
                                            cld::Syntax::CastExpression::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::CastExpression::variant& cld::Syntax::CastExpression::getVariant() const
{
    return m_variant;
}

cld::Syntax::Term::Term(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::CastExpression&& castExpressions,
    std::vector<std::pair<cld::Syntax::Term::BinaryDotOperator, cld::Syntax::CastExpression>>&& optionalCastExpressions)
    : Node(begin, end),
      m_castExpression(std::move(castExpressions)),
      m_optionalCastExpressions(std::move(optionalCastExpressions))
{
}

const cld::Syntax::CastExpression& cld::Syntax::Term::getCastExpression() const
{
    return m_castExpression;
}

const std::vector<std::pair<cld::Syntax::Term::BinaryDotOperator, cld::Syntax::CastExpression>>&
    cld::Syntax::Term::getOptionalCastExpressions() const
{
    return m_optionalCastExpressions;
}

cld::Syntax::AdditiveExpression::AdditiveExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::Term&& term,
    std::vector<std::pair<cld::Syntax::AdditiveExpression::BinaryDashOperator, cld::Syntax::Term>>&& optionalTerms)
    : Node(begin, end), m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{
}

const cld::Syntax::Term& cld::Syntax::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<std::pair<cld::Syntax::AdditiveExpression::BinaryDashOperator, cld::Syntax::Term>>&
    cld::Syntax::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

cld::Syntax::ShiftExpression::ShiftExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::AdditiveExpression&& additiveExpression,
    std::vector<std::pair<cld::Syntax::ShiftExpression::ShiftOperator, cld::Syntax::AdditiveExpression>>&&
        optionalAdditiveExpressions)
    : Node(begin, end),
      m_additiveExpression(std::move(additiveExpression)),
      m_optionalAdditiveExpressions(std::move(optionalAdditiveExpressions))
{
}

const cld::Syntax::AdditiveExpression& cld::Syntax::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
}

const cld::Syntax::Expression* cld::Syntax::ReturnStatement::getExpression() const
{
    return m_expression.get();
}

cld::Syntax::ReturnStatement::ReturnStatement(std::vector<Lexer::Token>::const_iterator begin,
                                              std::vector<Lexer::Token>::const_iterator end,
                                              std::unique_ptr<Expression>&& expression)
    : Node(begin, end), m_expression(std::move(expression))
{
}

cld::Syntax::ExpressionStatement::ExpressionStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                      std::vector<Lexer::Token>::const_iterator end,
                                                      std::unique_ptr<Expression>&& optionalExpression)
    : Node(begin, end), m_optionalExpression(std::move(optionalExpression))
{
}

const cld::Syntax::Expression* cld::Syntax::ExpressionStatement::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

std::unique_ptr<cld::Syntax::Expression> cld::Syntax::ExpressionStatement::moveOptionalExpression()
{
    return std::move(m_optionalExpression);
}

cld::Syntax::IfStatement::IfStatement(std::vector<Lexer::Token>::const_iterator begin,
                                      std::vector<Lexer::Token>::const_iterator end, Expression&& expression,
                                      std::unique_ptr<Statement>&& branch, std::unique_ptr<Statement>&& elseBranch)
    : Node(begin, end),
      m_expression(std::move(expression)),
      m_branch(std::move(branch)),
      m_elseBranch(std::move(elseBranch))
{
    assert(m_branch);
}

cld::Syntax::ContinueStatement::ContinueStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                  std::vector<Lexer::Token>::const_iterator end)
    : Node(begin, end)
{
}

const cld::Syntax::Expression& cld::Syntax::IfStatement::getExpression() const
{
    return m_expression;
}

const cld::Syntax::Statement& cld::Syntax::IfStatement::getBranch() const
{
    return *m_branch;
}

const cld::Syntax::Statement* cld::Syntax::IfStatement::getElseBranch() const
{
    return m_elseBranch.get();
}

cld::Syntax::CompoundStatement::CompoundStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                  std::vector<Lexer::Token>::const_iterator end,
                                                  std::vector<CompoundItem>&& blockItems)
    : Node(begin, end), m_blockItems(std::move(blockItems))
{
}

const std::vector<cld::Syntax::CompoundItem>& cld::Syntax::CompoundStatement::getBlockItems() const
{
    return m_blockItems;
}

cld::Syntax::ForStatement::ForStatement(std::vector<Lexer::Token>::const_iterator begin,
                                        std::vector<Lexer::Token>::const_iterator end,
                                        std::unique_ptr<Statement>&& statement,
                                        std::variant<Declaration, std::unique_ptr<Expression>>&& initial,
                                        std::unique_ptr<Expression>&& controlling, std::unique_ptr<Expression>&& post)
    : Node(begin, end),
      m_statement(std::move(statement)),
      m_initial(std::move(initial)),
      m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const std::variant<cld::Syntax::Declaration, std::unique_ptr<cld::Syntax::Expression>>&
    cld::Syntax::ForStatement::getInitial() const
{
    return m_initial;
}

const cld::Syntax::Expression* cld::Syntax::ForStatement::getControlling() const
{
    return m_controlling.get();
}

const cld::Syntax::Expression* cld::Syntax::ForStatement::getPost() const
{
    return m_post.get();
}

cld::Syntax::HeadWhileStatement::HeadWhileStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                    std::vector<Lexer::Token>::const_iterator end,
                                                    Expression&& expression, std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const cld::Syntax::Expression& cld::Syntax::HeadWhileStatement::getExpression() const
{
    return m_expression;
}

const cld::Syntax::Statement& cld::Syntax::HeadWhileStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::FootWhileStatement::FootWhileStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                    std::vector<Lexer::Token>::const_iterator end,
                                                    std::unique_ptr<Statement>&& statement, Expression&& expression)
    : Node(begin, end), m_statement(std::move(statement)), m_expression(std::move(expression))
{
    assert(m_statement);
}

const cld::Syntax::Statement& cld::Syntax::FootWhileStatement::getStatement() const
{
    return *m_statement;
}

const cld::Syntax::Expression& cld::Syntax::FootWhileStatement::getExpression() const
{
    return m_expression;
}

const std::vector<std::pair<cld::Syntax::ShiftExpression::ShiftOperator, cld::Syntax::AdditiveExpression>>&
    cld::Syntax::ShiftExpression::getOptionalAdditiveExpressions() const
{
    return m_optionalAdditiveExpressions;
}

cld::Syntax::RelationalExpression::RelationalExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    ShiftExpression&& shiftExpression,
    std::vector<std::pair<RelationalOperator, ShiftExpression>>&& optionalRelationalExpressions)
    : Node(begin, end),
      m_shiftExpression(std::move(shiftExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{
}

const cld::Syntax::ShiftExpression& cld::Syntax::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

const std::vector<std::pair<cld::Syntax::RelationalExpression::RelationalOperator, cld::Syntax::ShiftExpression>>&
    cld::Syntax::RelationalExpression::getOptionalShiftExpressions() const
{
    return m_optionalRelationalExpressions;
}

cld::Syntax::EqualityExpression::EqualityExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    RelationalExpression&& relationalExpression,
    std::vector<std::pair<EqualityOperator, RelationalExpression>>&& optionalRelationalExpressions)
    : Node(begin, end),
      m_relationalExpression(std::move(relationalExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{
}

const cld::Syntax::RelationalExpression& cld::Syntax::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

const std::vector<std::pair<cld::Syntax::EqualityExpression::EqualityOperator, cld::Syntax::RelationalExpression>>&
    cld::Syntax::EqualityExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

cld::Syntax::LogicalAndExpression::LogicalAndExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                        std::vector<Lexer::Token>::const_iterator end,
                                                        std::vector<BitOrExpression>&& equalityExpressions)
    : Node(begin, end), m_bitOrExpressions(std::move(equalityExpressions))
{
    assert(!m_bitOrExpressions.empty());
}

const std::vector<cld::Syntax::BitOrExpression>& cld::Syntax::LogicalAndExpression::getBitOrExpressions() const
{
    return m_bitOrExpressions;
}

cld::Syntax::LogicalOrExpression::LogicalOrExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                      std::vector<Lexer::Token>::const_iterator end,
                                                      std::vector<LogicalAndExpression>&& andExpressions)
    : Node(begin, end), m_andExpressions(std::move(andExpressions))
{
}

const std::vector<cld::Syntax::LogicalAndExpression>& cld::Syntax::LogicalOrExpression::getAndExpressions() const
{
    return m_andExpressions;
}

cld::Syntax::ConditionalExpression::ConditionalExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    LogicalOrExpression&& logicalOrExpression, std::unique_ptr<Expression>&& optionalExpression,
    std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression)
    : Node(begin, end),
      m_logicalOrExpression(std::move(logicalOrExpression)),
      m_optionalExpression(std::move(optionalExpression)),
      m_optionalConditionalExpression(std::move(optionalConditionalExpression))
{
}

const cld::Syntax::LogicalOrExpression& cld::Syntax::ConditionalExpression::getLogicalOrExpression() const
{
    return m_logicalOrExpression;
}

const cld::Syntax::Expression* cld::Syntax::ConditionalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

const cld::Syntax::ConditionalExpression* cld::Syntax::ConditionalExpression::getOptionalConditionalExpression() const
{
    return m_optionalConditionalExpression.get();
}

const cld::Syntax::Statement& cld::Syntax::ForStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::BitAndExpression::BitAndExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                std::vector<Lexer::Token>::const_iterator end,
                                                std::vector<EqualityExpression>&& equalityExpressions)
    : Node(begin, end), m_equalityExpressions(std::move(equalityExpressions))
{
}

const std::vector<cld::Syntax::EqualityExpression>& cld::Syntax::BitAndExpression::getEqualityExpressions() const
{
    return m_equalityExpressions;
}

cld::Syntax::BitXorExpression::BitXorExpression(std::vector<Lexer::Token>::const_iterator begin,
                                                std::vector<Lexer::Token>::const_iterator end,
                                                std::vector<BitAndExpression>&& bitAndExpressions)
    : Node(begin, end), m_bitAndExpressions(std::move(bitAndExpressions))
{
}

const std::vector<cld::Syntax::BitAndExpression>& cld::Syntax::BitXorExpression::getBitAndExpressions() const
{
    return m_bitAndExpressions;
}

cld::Syntax::BitOrExpression::BitOrExpression(std::vector<Lexer::Token>::const_iterator begin,
                                              std::vector<Lexer::Token>::const_iterator end,
                                              std::vector<BitXorExpression>&& bitXorExpressions)
    : Node(begin, end), m_bitXorExpressions(std::move(bitXorExpressions))
{
}

const std::vector<cld::Syntax::BitXorExpression>& cld::Syntax::BitOrExpression::getBitXorExpressions() const
{
    return m_bitXorExpressions;
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionFunctionCall::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::vector<std::unique_ptr<cld::Syntax::AssignmentExpression>>&
    cld::Syntax::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmanetExpressions;
}

cld::Syntax::SwitchStatement::SwitchStatement(std::vector<Lexer::Token>::const_iterator begin,
                                              std::vector<Lexer::Token>::const_iterator end, Expression&& expression,
                                              std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const cld::Syntax::Expression& cld::Syntax::SwitchStatement::getExpression() const
{
    return m_expression;
}

const cld::Syntax::Statement& cld::Syntax::SwitchStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::DefaultStatement::DefaultStatement(std::vector<Lexer::Token>::const_iterator begin,
                                                std::vector<Lexer::Token>::const_iterator end,
                                                std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_statement(std::move(statement))
{
    assert(m_statement);
}

const cld::Syntax::Statement& cld::Syntax::DefaultStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::CaseStatement::CaseStatement(std::vector<Lexer::Token>::const_iterator begin,
                                          std::vector<Lexer::Token>::const_iterator end,
                                          ConstantExpression&& constantExpression,
                                          std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_constantExpression(std::move(constantExpression)), m_statement(std::move(statement))
{
}

const cld::Syntax::Statement& cld::Syntax::CaseStatement::getStatement() const
{
    return *m_statement;
}

const cld::Syntax::ConstantExpression& cld::Syntax::CaseStatement::getConstantExpression() const
{
    return m_constantExpression;
}

cld::Syntax::InitializerList::InitializerList(std::vector<Lexer::Token>::const_iterator begin,
                                              std::vector<Lexer::Token>::const_iterator end,
                                              vector&& nonCommaExpressionsAndBlocks)
    : Node(begin, end), m_nonCommaExpressionsAndBlocks(std::move(nonCommaExpressionsAndBlocks))
{
}

const typename cld::Syntax::InitializerList::vector&
    cld::Syntax::InitializerList::getNonCommaExpressionsAndBlocks() const
{
    return m_nonCommaExpressionsAndBlocks;
}

cld::Syntax::EnumDeclaration::EnumDeclaration(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end, std::string name,
    std::vector<std::pair<std::string, std::optional<ConstantExpression>>>&& values)
    : Node(begin, end), m_name(std::move(name)), m_values(std::move(values))
{
}

const std::string& cld::Syntax::EnumDeclaration::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::string, std::optional<cld::Syntax::ConstantExpression>>>&
    cld::Syntax::EnumDeclaration::getValues() const
{
    return m_values;
}

cld::Syntax::Initializer::Initializer(std::vector<Lexer::Token>::const_iterator begin,
                                      std::vector<Lexer::Token>::const_iterator end,
                                      cld::Syntax::Initializer::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::Initializer::variant& cld::Syntax::Initializer::getVariant() const
{
    return m_variant;
}

cld::Syntax::BreakStatement::BreakStatement(std::vector<Lexer::Token>::const_iterator begin,
                                            std::vector<Lexer::Token>::const_iterator end)
    : Node(begin, end)
{
}

cld::Syntax::EnumSpecifier::EnumSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                                          std::vector<Lexer::Token>::const_iterator end,
                                          cld::Syntax::EnumSpecifier::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::EnumSpecifier::variant& cld::Syntax::EnumSpecifier::getVariant() const
{
    return m_variant;
}

cld::Syntax::TypeSpecifier::TypeSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                                          std::vector<Lexer::Token>::const_iterator end,
                                          cld::Syntax::TypeSpecifier::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::TypeSpecifier::variant& cld::Syntax::TypeSpecifier::getVariant() const
{
    return m_variant;
}

cld::Syntax::Declaration::Declaration(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::vector<cld::Syntax::DeclarationSpecifier>&& declarationSpecifiers,
    std::vector<std::pair<std::unique_ptr<cld::Syntax::Declarator>, std::unique_ptr<cld::Syntax::Initializer>>>&&
        initDeclarators)
    : Node(begin, end),
      m_declarationSpecifiers(std::move(declarationSpecifiers)),
      m_initDeclarators(std::move(initDeclarators))
{
    assert(std::all_of(m_initDeclarators.begin(), m_initDeclarators.end(),
                       [](const auto& pair) -> bool { return pair.first.get() != nullptr; }));
}

const std::vector<cld::Syntax::DeclarationSpecifier>& cld::Syntax::Declaration::getDeclarationSpecifiers() const
{
    return m_declarationSpecifiers;
}

const std::vector<std::pair<std::unique_ptr<cld::Syntax::Declarator>, std::unique_ptr<cld::Syntax::Initializer>>>&
    cld::Syntax::Declaration::getInitDeclarators() const
{
    return m_initDeclarators;
}

cld::Syntax::StructOrUnionSpecifier::StructOrUnionSpecifier(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end, bool isUnion,
    std::string identifier, std::vector<cld::Syntax::StructOrUnionSpecifier::StructDeclaration>&& structDeclarations)
    : Node(begin, end),
      m_isUnion(isUnion),
      m_identifier(std::move(identifier)),
      m_structDeclarations(std::move(structDeclarations))
{
}

bool cld::Syntax::StructOrUnionSpecifier::isUnion() const
{
    return m_isUnion;
}

const std::string& cld::Syntax::StructOrUnionSpecifier::getIdentifier() const
{
    return m_identifier;
}

const std::vector<cld::Syntax::StructOrUnionSpecifier::StructDeclaration>&
    cld::Syntax::StructOrUnionSpecifier::getStructDeclarations() const
{
    return m_structDeclarations;
}

cld::Syntax::ParameterList::ParameterList(std::vector<Lexer::Token>::const_iterator begin,
                                          std::vector<Lexer::Token>::const_iterator end,
                                          std::vector<cld::Syntax::ParameterDeclaration>&& parameterList)
    : Node(begin, end), m_parameterList(std::move(parameterList))
{
}

const std::vector<cld::Syntax::ParameterDeclaration>& cld::Syntax::ParameterList::getParameterDeclarations() const
{
    return m_parameterList;
}

cld::Syntax::ParameterTypeList::ParameterTypeList(std::vector<Lexer::Token>::const_iterator begin,
                                                  std::vector<Lexer::Token>::const_iterator end,
                                                  cld::Syntax::ParameterList&& parameterList, bool hasEllipse)
    : Node(begin, end), m_parameterList(std::move(parameterList)), m_hasEllipse(hasEllipse)
{
}

const cld::Syntax::ParameterList& cld::Syntax::ParameterTypeList::getParameterList() const
{
    return m_parameterList;
}

bool cld::Syntax::ParameterTypeList::hasEllipse() const
{
    return m_hasEllipse;
}

cld::Syntax::Pointer::Pointer(std::vector<Lexer::Token>::const_iterator begin,
                              std::vector<Lexer::Token>::const_iterator end,
                              std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers)
    : Node(begin, end), m_typeQualifiers(std::move(typeQualifiers))
{
}

const std::vector<cld::Syntax::TypeQualifier>& cld::Syntax::Pointer::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

cld::Syntax::DirectDeclaratorNoStaticOrAsterisk::DirectDeclaratorNoStaticOrAsterisk(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::DirectDeclarator>&& directDeclarator,
    std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers,
    std::unique_ptr<cld::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_directDeclarator(std::move(directDeclarator)),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_directDeclarator);
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorNoStaticOrAsterisk::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<cld::Syntax::TypeQualifier>&
    cld::Syntax::DirectDeclaratorNoStaticOrAsterisk::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const std::unique_ptr<cld::Syntax::AssignmentExpression>&
    cld::Syntax::DirectDeclaratorNoStaticOrAsterisk::getAssignmentExpression() const
{
    return m_assignmentExpression;
}

cld::Syntax::DirectDeclaratorStatic::DirectDeclaratorStatic(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::DirectDeclarator>&& directDeclarator,
    std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers, cld::Syntax::AssignmentExpression&& assignmentExpression)
    : Node(begin, end),
      m_directDeclarator(std::move(directDeclarator)),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    assert(m_directDeclarator);
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorStatic::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<cld::Syntax::TypeQualifier>& cld::Syntax::DirectDeclaratorStatic::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const cld::Syntax::AssignmentExpression& cld::Syntax::DirectDeclaratorStatic::getAssignmentExpression() const
{
    return m_assignmentExpression;
}

cld::Syntax::DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::DirectDeclarator&& directDeclarator, std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_typeQualifiers(std::move(typeQualifiers))
{
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorAsterisk::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<cld::Syntax::TypeQualifier>& cld::Syntax::DirectDeclaratorAsterisk::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

cld::Syntax::DirectDeclaratorParentheseParameters::DirectDeclaratorParentheseParameters(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::DirectDeclarator&& directDeclarator, cld::Syntax::ParameterTypeList&& parameterTypeList)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_parameterTypeList(std::move(parameterTypeList))
{
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorParentheseParameters::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const cld::Syntax::ParameterTypeList& cld::Syntax::DirectDeclaratorParentheseParameters::getParameterTypeList() const
{
    return m_parameterTypeList;
}

cld::Syntax::DirectDeclaratorParentheseIdentifiers::DirectDeclaratorParentheseIdentifiers(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::DirectDeclarator&& directDeclarator,
    std::vector<std::pair<std::string, std::vector<Lexer::Token>::const_iterator>>&& identifiers)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_identifiers(std::move(identifiers))
{
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorParentheseIdentifiers::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<std::pair<std::string, std::vector<cld::Lexer::Token>::const_iterator>>&
    cld::Syntax::DirectDeclaratorParentheseIdentifiers::getIdentifiers() const
{
    return m_identifiers;
}

cld::Syntax::Declarator::Declarator(std::vector<Lexer::Token>::const_iterator begin,
                                    std::vector<Lexer::Token>::const_iterator end,
                                    std::vector<cld::Syntax::Pointer>&& pointers,
                                    cld::Syntax::DirectDeclarator&& directDeclarator)
    : Node(begin, end), m_pointers(std::move(pointers)), m_directDeclarator(std::move(directDeclarator))
{
}

const std::vector<cld::Syntax::Pointer>& cld::Syntax::Declarator::getPointers() const
{
    return m_pointers;
}

const cld::Syntax::DirectDeclarator& cld::Syntax::Declarator::getDirectDeclarator() const
{
    return m_directDeclarator;
}

cld::Syntax::DirectAbstractDeclaratorAssignmentExpression::DirectAbstractDeclaratorAssignmentExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
    std::unique_ptr<cld::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_assignmentExpression(std::move(assignmentExpression))
{
}

const cld::Syntax::DirectAbstractDeclarator*
    cld::Syntax::DirectAbstractDeclaratorAssignmentExpression::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const cld::Syntax::AssignmentExpression*
    cld::Syntax::DirectAbstractDeclaratorAssignmentExpression::getAssignmentExpression() const
{
    return m_assignmentExpression.get();
}

cld::Syntax::DirectAbstractDeclaratorParameterTypeList::DirectAbstractDeclaratorParameterTypeList(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::unique_ptr<cld::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
    std::unique_ptr<cld::Syntax::ParameterTypeList>&& parameterTypeList)
    : Node(begin, end),
      m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_parameterTypeList(std::move(parameterTypeList))
{
}

const cld::Syntax::DirectAbstractDeclarator*
    cld::Syntax::DirectAbstractDeclaratorParameterTypeList::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const cld::Syntax::ParameterTypeList*
    cld::Syntax::DirectAbstractDeclaratorParameterTypeList::getParameterTypeList() const
{
    return m_parameterTypeList.get();
}

cld::Syntax::AbstractDeclarator::AbstractDeclarator(std::vector<Lexer::Token>::const_iterator begin,
                                                    std::vector<Lexer::Token>::const_iterator end,
                                                    std::vector<cld::Syntax::Pointer>&& pointers,
                                                    std::optional<DirectAbstractDeclarator>&& directAbstractDeclarator)
    : Node(begin, end), m_pointers(std::move(pointers)), m_directAbstractDeclarator(std::move(directAbstractDeclarator))
{
}

const std::vector<cld::Syntax::Pointer>& cld::Syntax::AbstractDeclarator::getPointers() const
{
    return m_pointers;
}

const cld::Syntax::DirectAbstractDeclarator* cld::Syntax::AbstractDeclarator::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator ? &*m_directAbstractDeclarator : nullptr;
}

cld::Syntax::TypeName::TypeName(std::vector<Lexer::Token>::const_iterator begin,
                                std::vector<Lexer::Token>::const_iterator end,
                                std::vector<cld::Syntax::SpecifierQualifier>&& specifierQualifiers,
                                std::unique_ptr<cld::Syntax::AbstractDeclarator>&& abstractDeclarator)
    : Node(begin, end),
      m_specifierQualifiers(std::move(specifierQualifiers)),
      m_abstractDeclarator(std::move(abstractDeclarator))
{
}

const std::vector<cld::Syntax::SpecifierQualifier>& cld::Syntax::TypeName::getSpecifierQualifiers() const
{
    return m_specifierQualifiers;
}

const cld::Syntax::AbstractDeclarator* cld::Syntax::TypeName::getAbstractDeclarator() const
{
    return m_abstractDeclarator.get();
}

cld::Syntax::GotoStatement::GotoStatement(std::vector<Lexer::Token>::const_iterator begin,
                                          std::vector<Lexer::Token>::const_iterator end, std::string identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

const std::string& cld::Syntax::GotoStatement::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::LabelStatement::LabelStatement(std::vector<Lexer::Token>::const_iterator begin,
                                            std::vector<Lexer::Token>::const_iterator end, std::string identifier,
                                            Statement&& statement)
    : Node(begin, end),
      m_identifier(std::move(identifier)),
      m_statement(std::make_unique<Statement>(std::move(statement)))
{
}

const cld::Syntax::Statement& cld::Syntax::LabelStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::TranslationUnit::TranslationUnit(std::vector<cld::Syntax::ExternalDeclaration>&& globals) noexcept
    : m_globals(std::move(globals))
{
}

const std::vector<cld::Syntax::ExternalDeclaration>& cld::Syntax::TranslationUnit::getGlobals() const
{
    return m_globals;
}

cld::Syntax::FunctionDefinition::FunctionDefinition(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::vector<cld::Syntax::DeclarationSpecifier>&& declarationSpecifiers, cld::Syntax::Declarator&& declarator,
    std::vector<cld::Syntax::Declaration>&& declarations, cld::Syntax::CompoundStatement&& compoundStatement)
    : Node(begin, end),
      m_declarationSpecifiers(std::move(declarationSpecifiers)),
      m_declarator(std::move(declarator)),
      m_declarations(std::move(declarations)),
      m_compoundStatement(std::move(compoundStatement))
{
}

const std::vector<cld::Syntax::DeclarationSpecifier>& cld::Syntax::FunctionDefinition::getDeclarationSpecifiers() const
{
    return m_declarationSpecifiers;
}

const cld::Syntax::Declarator& cld::Syntax::FunctionDefinition::getDeclarator() const
{
    return m_declarator;
}

const std::vector<cld::Syntax::Declaration>& cld::Syntax::FunctionDefinition::getDeclarations() const
{
    return m_declarations;
}

const cld::Syntax::CompoundStatement& cld::Syntax::FunctionDefinition::getCompoundStatement() const
{
    return m_compoundStatement;
}

cld::Syntax::Node::Node(std::vector<cld::Lexer::Token>::const_iterator begin,
                        std::vector<cld::Lexer::Token>::const_iterator end)
    : m_begin(begin), m_end(end)
{
}

std::vector<cld::Lexer::Token>::const_iterator cld::Syntax::Node::begin() const
{
    return m_begin;
}

std::vector<cld::Lexer::Token>::const_iterator cld::Syntax::Node::end() const
{
    return m_end;
}

cld::Syntax::TypeQualifier::TypeQualifier(std::vector<Lexer::Token>::const_iterator begin,
                                          std::vector<Lexer::Token>::const_iterator end,
                                          cld::Syntax::TypeQualifier::Qualifier qualifier)
    : Node(begin, end), m_qualifier(qualifier)
{
}

cld::Syntax::TypeQualifier::Qualifier cld::Syntax::TypeQualifier::getQualifier() const
{
    return m_qualifier;
}

cld::Syntax::StorageClassSpecifier::StorageClassSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                                                          std::vector<Lexer::Token>::const_iterator end,
                                                          cld::Syntax::StorageClassSpecifier::Specifiers specifier)
    : Node(begin, end), m_specifier(specifier)
{
}

cld::Syntax::StorageClassSpecifier::Specifiers cld::Syntax::StorageClassSpecifier::getSpecifier() const
{
    return m_specifier;
}

cld::Syntax::FunctionSpecifier::FunctionSpecifier(const std::vector<cld::Lexer::Token>::const_iterator& begin,
                                                  const std::vector<cld::Lexer::Token>::const_iterator& end)
    : Node(begin, end)
{
}

cld::Syntax::DirectDeclaratorIdentifier::DirectDeclaratorIdentifier(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    std::string identifier, std::vector<Lexer::Token>::const_iterator identifierLoc)
    : Node(begin, end), m_identifier(std::move(identifier)), m_identifierLoc(identifierLoc)
{
}

const std::string& cld::Syntax::DirectDeclaratorIdentifier::getIdentifier() const
{
    return m_identifier;
}

std::vector<cld::Lexer::Token>::const_iterator cld::Syntax::DirectDeclaratorIdentifier::getIdentifierLoc() const
{
    return m_identifierLoc;
}

cld::Syntax::DirectDeclaratorParenthese::DirectDeclaratorParenthese(std::vector<Lexer::Token>::const_iterator begin,
                                                                    std::vector<Lexer::Token>::const_iterator end,
                                                                    std::unique_ptr<Declarator>&& declarator)
    : Node(begin, end), m_declarator(std::move(declarator))
{
    assert(m_declarator);
}

const cld::Syntax::Declarator& cld::Syntax::DirectDeclaratorParenthese::getDeclarator() const
{
    return *m_declarator;
}

cld::Syntax::DirectAbstractDeclaratorParenthese::DirectAbstractDeclaratorParenthese(
    std::vector<cld::Lexer::Token>::const_iterator begin, std::vector<cld::Lexer::Token>::const_iterator end,
    std::unique_ptr<AbstractDeclarator>&& abstractDeclarator)
    : Node(begin, end), m_abstractDeclarator(std::move(abstractDeclarator))
{
}

const cld::Syntax::AbstractDeclarator& cld::Syntax::DirectAbstractDeclaratorParenthese::getAbstractDeclarator() const
{
    return *m_abstractDeclarator;
}

cld::Syntax::DirectAbstractDeclaratorAsterisk::DirectAbstractDeclaratorAsterisk(
    std::vector<cld::Lexer::Token>::const_iterator begin, std::vector<cld::Lexer::Token>::const_iterator end,
    std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator)
    : Node(begin, end), m_directAbstractDeclarator(std::move(directAbstractDeclarator))
{
}

const std::unique_ptr<cld::Syntax::DirectAbstractDeclarator>&
    cld::Syntax::DirectAbstractDeclaratorAsterisk::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator;
}
cld::Syntax::AssignmentExpression::AssignmentExpression(
    std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end,
    cld::Syntax::ConditionalExpression&& conditionalExpression,
    std::vector<std::pair<AssignOperator, ConditionalExpression>>&& assignments)
    : Node(begin, end), m_conditionalExpression(std::move(conditionalExpression)), m_assignments(std::move(assignments))
{
}
const cld::Syntax::ConditionalExpression& cld::Syntax::AssignmentExpression::getConditionalExpression() const
{
    return m_conditionalExpression;
}
const std::vector<std::pair<cld::Syntax::AssignmentExpression::AssignOperator, cld::Syntax::ConditionalExpression>>&
    cld::Syntax::AssignmentExpression::getAssignments() const
{
    return m_assignments;
}

cld::Syntax::UnaryExpressionDefined::UnaryExpressionDefined(std::vector<cld::Lexer::Token>::const_iterator begin,
                                                            std::vector<cld::Lexer::Token>::const_iterator end,
                                                            std::string identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

const std::string& cld::Syntax::UnaryExpressionDefined::getIdentifier() const
{
    return m_identifier;
}

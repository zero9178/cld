#include "Parser.hpp"

#include <algorithm>

OpenCL::Parser::Program::Program(std::vector<std::unique_ptr<Global>>&& globals) noexcept : m_globals(std::move(
    globals))
{

}

const std::vector<std::unique_ptr<OpenCL::Parser::Global>>& OpenCL::Parser::Program::getGlobals() const
{
    return m_globals;
}

const std::string& OpenCL::Parser::Function::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::unique_ptr<OpenCL::Parser::Type>,
                            std::string>>& OpenCL::Parser::Function::getArguments() const
{
    return m_arguments;
}

const OpenCL::Parser::BlockStatement* OpenCL::Parser::Function::getBlockStatement() const
{
    return m_block.get();
}

OpenCL::Parser::Function::Function(std::unique_ptr<Type>&& returnType,
                                   std::string name,
                                   std::vector<std::pair<std::unique_ptr<Type>,
                                                         std::string>> arguments,
                                   std::unique_ptr<BlockStatement>&& blockItems)
    : m_returnType(std::move(returnType)), m_name(std::move(
    name)), m_arguments(std::move(arguments)), m_block(std::move(blockItems))
{
    assert(m_returnType);
    assert(std::all_of(m_arguments.begin(), m_arguments.begin(), [](const auto& pair)
    { return static_cast<bool>(pair.first); }));
}

const std::string& OpenCL::Parser::Declaration::getName() const
{
    return m_name;
}

const OpenCL::Parser::Expression* OpenCL::Parser::Declaration::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

OpenCL::Parser::Declaration::Declaration(std::unique_ptr<Type>&& type,
                                         std::string name,
                                         std::unique_ptr<Expression>&& optionalExpression)
    : m_type(std::move(type)), m_name(std::move(name)), m_optionalExpression(std::move(optionalExpression))
{
    assert(m_type);
}

const OpenCL::Parser::Type& OpenCL::Parser::Declaration::getType() const
{
    return *m_type;
}

const OpenCL::Parser::Expression& OpenCL::Parser::ReturnStatement::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::ReturnStatement::ReturnStatement(Expression&& expression)
    : m_expression(std::move(expression))
{}

OpenCL::Parser::ExpressionStatement::ExpressionStatement(std::unique_ptr<Expression>&& optionalExpression)
    : m_optionalExpression(std::move(optionalExpression))
{}

const OpenCL::Parser::Expression* OpenCL::Parser::ExpressionStatement::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

std::unique_ptr<OpenCL::Parser::Expression> OpenCL::Parser::ExpressionStatement::moveOptionalExpression()
{
    return std::move(m_optionalExpression);
}

OpenCL::Parser::IfStatement::IfStatement(Expression&& expression,
                                         std::unique_ptr<Statement>&& branch,
                                         std::unique_ptr<Statement>&& elseBranch) : m_expression(
    std::move(expression)), m_branch(std::move(branch)), m_elseBranch(std::move(elseBranch))
{
    assert(m_branch);
}

const OpenCL::Parser::Expression& OpenCL::Parser::IfStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Parser::Statement& OpenCL::Parser::IfStatement::getBranch() const
{
    return *m_branch;
}

const OpenCL::Parser::Statement* OpenCL::Parser::IfStatement::getElseBranch() const
{
    return m_elseBranch.get();
}

OpenCL::Parser::BlockStatement::BlockStatement(std::vector<std::unique_ptr<OpenCL::Parser::BlockItem>> blockItems)
    : m_blockItems(std::move(blockItems))
{
    assert(std::all_of(m_blockItems.begin(),
                       m_blockItems.end(),
                       [](const std::unique_ptr<OpenCL::Parser::BlockItem>& ptr)
                       { return ptr.get(); }));
}

const std::vector<std::unique_ptr<OpenCL::Parser::BlockItem>>& OpenCL::Parser::BlockStatement::getBlockItems() const
{
    return m_blockItems;
}

OpenCL::Parser::ForStatement::ForStatement(std::unique_ptr<Statement>&& statement,
                                           std::unique_ptr<Expression>&& initial,
                                           std::unique_ptr<Expression>&& controlling,
                                           std::unique_ptr<Expression>&& post)
    : m_statement(std::move(statement)), m_initial(std::move(initial)), m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForStatement::getInitial() const
{
    return m_initial.get();
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Parser::ForDeclarationStatement::ForDeclarationStatement(std::unique_ptr<Statement>&& statement,
                                                                 Declaration&& initial,
                                                                 std::unique_ptr<Expression>&& controlling,
                                                                 std::unique_ptr<Expression>&& post)
    : m_statement(std::move(statement)), m_initial(std::move(initial)), m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const OpenCL::Parser::Declaration& OpenCL::Parser::ForDeclarationStatement::getInitial() const
{
    return m_initial;
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForDeclarationStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForDeclarationStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Parser::HeadWhileStatement::HeadWhileStatement(Expression&& expression,
                                                       std::unique_ptr<Statement>&& statement)
    : m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Parser::Expression& OpenCL::Parser::HeadWhileStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Parser::Statement& OpenCL::Parser::HeadWhileStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Parser::FootWhileStatement::FootWhileStatement(std::unique_ptr<Statement>&& statement,
                                                       Expression&& expression) : m_statement(
    std::move(statement)), m_expression(std::move(expression))
{
    assert(m_statement);
}

const OpenCL::Parser::Statement& OpenCL::Parser::FootWhileStatement::getStatement() const
{
    return *m_statement;
}

const OpenCL::Parser::Expression& OpenCL::Parser::FootWhileStatement::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::Expression::Expression(std::unique_ptr<NonCommaExpression>&& nonCommaExpression,
                                       std::unique_ptr<NonCommaExpression>&& optionalNonCommaExpression)
    : m_nonCommaExpression(std::move(nonCommaExpression)),
      m_optionalNonCommaExpression(std::move(optionalNonCommaExpression))
{
    assert(m_nonCommaExpression);
}

const OpenCL::Parser::NonCommaExpression& OpenCL::Parser::Expression::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

const OpenCL::Parser::NonCommaExpression* OpenCL::Parser::Expression::getOptionalNonCommaExpression() const
{
    return m_optionalNonCommaExpression.get();
}

OpenCL::Parser::AssignmentExpression::AssignOperator OpenCL::Parser::AssignmentExpression::getAssignOperator() const
{
    return m_assignOperator;
}

const OpenCL::Parser::UnaryFactor& OpenCL::Parser::AssignmentExpression::getUnaryFactor() const
{
    return m_unaryFactor;
}

const OpenCL::Parser::NonCommaExpression& OpenCL::Parser::AssignmentExpression::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

OpenCL::Parser::AssignmentExpression::AssignmentExpression(UnaryFactor&& unaryFactor,
                                                           OpenCL::Parser::AssignmentExpression::AssignOperator assignOperator,
                                                           std::unique_ptr<NonCommaExpression>&& nonCommaExpression)
    : m_unaryFactor(std::move(unaryFactor)), m_assignOperator(assignOperator),
      m_nonCommaExpression(std::move(nonCommaExpression))
{
    assert(m_nonCommaExpression);
}

OpenCL::Parser::Term::Term(std::unique_ptr<Factor>&& factor,
                           std::vector<std::pair<BinaryDotOperator, std::unique_ptr<Factor>>>&& optionalFactors)
    : m_factor(std::move(factor)), m_optionalFactors(std::move(optionalFactors))
{
    assert(m_factor);
}

const OpenCL::Parser::Factor& OpenCL::Parser::Term::getFactor() const
{
    return *m_factor;
}

const std::vector<std::pair<OpenCL::Parser::Term::BinaryDotOperator,
                            std::unique_ptr<OpenCL::Parser::Factor>>>& OpenCL::Parser::Term::getOptionalFactors() const
{
    return m_optionalFactors;
}

OpenCL::Parser::AdditiveExpression::AdditiveExpression(Term&& term,
                                                       std::vector<std::pair<BinaryDashOperator, Term>>&& optionalTerms)
    : m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{}

const OpenCL::Parser::Term& OpenCL::Parser::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<std::pair<OpenCL::Parser::AdditiveExpression::BinaryDashOperator, OpenCL::Parser::Term>>&
OpenCL::Parser::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

OpenCL::Parser::ShiftExpression::ShiftExpression(AdditiveExpression&& additiveExpression,
                                                 std::vector<std::pair<ShiftOperator,
                                                                       AdditiveExpression>>&& optionalAdditiveExpressions)
    : m_additiveExpression(std::move(additiveExpression)),
      m_optionalAdditiveExpressions(std::move(optionalAdditiveExpressions))
{}

const OpenCL::Parser::AdditiveExpression& OpenCL::Parser::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
}

const std::vector<std::pair<OpenCL::Parser::ShiftExpression::ShiftOperator,
                            OpenCL::Parser::AdditiveExpression>>& OpenCL::Parser::ShiftExpression::getOptionalAdditiveExpressions() const
{
    return m_optionalAdditiveExpressions;
}

OpenCL::Parser::RelationalExpression::RelationalExpression(ShiftExpression&& shiftExpression,
                                                           std::vector<std::pair<RelationalOperator,
                                                                                 ShiftExpression>>&& optionalRelationalExpressions)
    : m_shiftExpression(std::move(shiftExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{}

const OpenCL::Parser::ShiftExpression& OpenCL::Parser::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

const std::vector<std::pair<OpenCL::Parser::RelationalExpression::RelationalOperator,
                            OpenCL::Parser::ShiftExpression>>& OpenCL::Parser::RelationalExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Parser::EqualityExpression::EqualityExpression(RelationalExpression&& relationalExpression,
                                                       std::vector<std::pair<EqualityOperator,
                                                                             RelationalExpression>>&& optionalRelationalExpressions)
    : m_relationalExpression(std::move(relationalExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{}

const OpenCL::Parser::RelationalExpression& OpenCL::Parser::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

const std::vector<std::pair<OpenCL::Parser::EqualityExpression::EqualityOperator,
                            OpenCL::Parser::RelationalExpression>>& OpenCL::Parser::EqualityExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Parser::LogicalAndExpression::LogicalAndExpression(BitOrExpression&& equalityExpression,
                                                           std::vector<BitOrExpression>&& optionalEqualityExpressions)
    : m_bitOrExpression(std::move(equalityExpression)),
      m_optionalBitOrExpressions(std::move(optionalEqualityExpressions))
{}

const OpenCL::Parser::BitOrExpression& OpenCL::Parser::LogicalAndExpression::getBitOrExpression() const
{
    return m_bitOrExpression;
}

const std::vector<OpenCL::Parser::BitOrExpression>& OpenCL::Parser::LogicalAndExpression::getOptionalBitOrExpressions() const
{
    return m_optionalBitOrExpressions;
}

OpenCL::Parser::LogicalOrExpression::LogicalOrExpression(LogicalAndExpression&& andExpression,
                                                         std::vector<LogicalAndExpression>&& optionalAndExpressions)
    : m_andExpression(std::move(andExpression)), m_optionalAndExpressions(std::move(optionalAndExpressions))
{}

const OpenCL::Parser::LogicalAndExpression& OpenCL::Parser::LogicalOrExpression::getAndExpression() const
{
    return m_andExpression;
}

const std::vector<OpenCL::Parser::LogicalAndExpression>& OpenCL::Parser::LogicalOrExpression::getOptionalAndExpressions() const
{
    return m_optionalAndExpressions;
}

OpenCL::Parser::ConditionalExpression::ConditionalExpression(LogicalOrExpression&& logicalOrExpression,
                                                             std::unique_ptr<Expression>&& optionalExpression,
                                                             std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression)
    : m_logicalOrExpression(std::move(logicalOrExpression)), m_optionalExpression(std::move(optionalExpression)),
      m_optionalConditionalExpression(std::move(optionalConditionalExpression))
{}

const OpenCL::Parser::LogicalOrExpression& OpenCL::Parser::ConditionalExpression::getLogicalOrExpression() const
{
    return m_logicalOrExpression;
}

const OpenCL::Parser::Expression* OpenCL::Parser::ConditionalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

const OpenCL::Parser::ConditionalExpression* OpenCL::Parser::ConditionalExpression::getOptionalConditionalExpression() const
{
    return m_optionalConditionalExpression.get();
}

OpenCL::Parser::ParentheseFactor::ParentheseFactor(Expression&& expression) : m_expression(
    std::move(expression))
{}

const OpenCL::Parser::Expression& OpenCL::Parser::ParentheseFactor::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::UnaryFactor::UnaryFactor(OpenCL::Parser::UnaryFactor::UnaryOperator unaryOperator,
                                         std::unique_ptr<Factor>&& factor) : m_unaryOperator(
    unaryOperator), m_factor(std::move(factor))
{
    assert(m_factor);
}

OpenCL::Parser::UnaryFactor::UnaryOperator OpenCL::Parser::UnaryFactor::getUnaryOperator() const
{
    return m_unaryOperator;
}

const OpenCL::Parser::Factor& OpenCL::Parser::UnaryFactor::getFactor() const
{
    return *m_factor;
}

OpenCL::Parser::ConstantFactor::ConstantFactor(const variant& value) : m_value(value)
{}

const OpenCL::Parser::ConstantFactor::variant& OpenCL::Parser::ConstantFactor::getValue() const
{
    return m_value;
}

OpenCL::Parser::VariableFactor::VariableFactor(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::VariableFactor::getName() const
{
    return m_name;
}

OpenCL::Parser::PostIncrement::PostIncrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PostIncrement::getName() const
{
    return m_name;
}

OpenCL::Parser::PreIncrement::PreIncrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PreIncrement::getName() const
{
    return m_name;
}

OpenCL::Parser::PostDecrement::PostDecrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PostDecrement::getName() const
{
    return m_name;
}

OpenCL::Parser::PreDecrement::PreDecrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PreDecrement::getName() const
{
    return m_name;
}

OpenCL::Parser::FunctionCall::FunctionCall(std::string name,
                                           std::vector<std::unique_ptr<OpenCL::Parser::NonCommaExpression>> expressions)
    : m_name(std::move(name)), m_expressions(std::move(expressions))
{
    assert(std::all_of(m_expressions.begin(), m_expressions.end(), [](const std::unique_ptr<NonCommaExpression>& ptr)
    { return ptr.get(); }));
}

const std::string& OpenCL::Parser::FunctionCall::getName() const
{
    return m_name;
}

const std::vector<std::unique_ptr<OpenCL::Parser::NonCommaExpression>>& OpenCL::Parser::FunctionCall::getExpressions() const
{
    return m_expressions;
}

const OpenCL::Parser::Statement& OpenCL::Parser::ForStatement::getStatement() const
{
    return *m_statement;
}

const OpenCL::Parser::Statement& OpenCL::Parser::ForDeclarationStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Parser::BitAndExpression::BitAndExpression(EqualityExpression&& equalityExpression,
                                                   std::vector<EqualityExpression>&& optionalEqualityExpressions)
    : m_equalityExpression(std::move(equalityExpression)),
      m_optionalEqualityExpressions(std::move(optionalEqualityExpressions))
{

}

const OpenCL::Parser::EqualityExpression& OpenCL::Parser::BitAndExpression::getEqualityExpression() const
{
    return m_equalityExpression;
}

const std::vector<OpenCL::Parser::EqualityExpression>& OpenCL::Parser::BitAndExpression::getOptionalEqualityExpressions() const
{
    return m_optionalEqualityExpressions;
}

OpenCL::Parser::BitXorExpression::BitXorExpression(BitAndExpression&& bitAndExpression,
                                                   std::vector<BitAndExpression>&& optionalBitAndExpressions)
    : m_bitAndExpression(
    std::move(bitAndExpression)), m_optionalBitAndExpressions(std::move(optionalBitAndExpressions))
{}

const OpenCL::Parser::BitAndExpression& OpenCL::Parser::BitXorExpression::getBitAndExpression() const
{
    return m_bitAndExpression;
}

const std::vector<OpenCL::Parser::BitAndExpression>& OpenCL::Parser::BitXorExpression::getOptionalBitAndExpressions() const
{
    return m_optionalBitAndExpressions;
}

OpenCL::Parser::BitOrExpression::BitOrExpression(BitXorExpression&& bitXorExpression,
                                                 std::vector<BitXorExpression>&& optionalBitXorExpressions)
    : m_bitXorExpression(
    std::move(bitXorExpression)), m_optionalBitXorExpressions(std::move(optionalBitXorExpressions))
{}

const OpenCL::Parser::BitXorExpression& OpenCL::Parser::BitOrExpression::getBitXorExpression() const
{
    return m_bitXorExpression;
}

const std::vector<OpenCL::Parser::BitXorExpression>& OpenCL::Parser::BitOrExpression::getOptionalBitXorExpressions() const
{
    return m_optionalBitXorExpressions;
}

OpenCL::Parser::GlobalDeclaration::GlobalDeclaration(std::unique_ptr<Type>&& type,
                                                     std::string name,
                                                     std::unique_ptr<ConstantFactor>&& value)
    : m_type(std::move(type)), m_name(std::move(name)), m_optionalValue(std::move(value))
{}

const OpenCL::Parser::Type& OpenCL::Parser::GlobalDeclaration::getType() const
{
    return *m_type;
}

const std::string& OpenCL::Parser::GlobalDeclaration::getName() const
{
    return m_name;
}

const OpenCL::Parser::ConstantFactor* OpenCL::Parser::GlobalDeclaration::getOptionalValue() const
{
    return m_optionalValue.get();
}

OpenCL::Parser::PrimitiveType::PrimitiveType(std::vector<OpenCL::Parser::PrimitiveType::Types>&& types)
    : m_types(std::move(types))
{}

bool OpenCL::Parser::PrimitiveType::isSigned() const
{
    return m_types.empty() || m_types[0] != Types::Unsigned;
}

bool OpenCL::Parser::PrimitiveType::isVoid() const
{
    return m_types.empty();
}

const OpenCL::Parser::Type& OpenCL::Parser::Function::getReturnType() const
{
    return *m_returnType;
}

OpenCL::Parser::CastFactor::CastFactor(std::unique_ptr<Type>&& outType,
                                       Expression&& expression)
    : m_outType(std::move(outType)), m_expression(std::move(expression))
{
    assert(m_outType);
}

const OpenCL::Parser::Type& OpenCL::Parser::CastFactor::getOutType() const
{
    return *m_outType;
}

const OpenCL::Parser::Expression& OpenCL::Parser::CastFactor::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::PointerType::PointerType(std::unique_ptr<Type>&& type) : m_type(std::move(type))
{}

bool OpenCL::Parser::PointerType::isSigned() const
{
    return false;
}

bool OpenCL::Parser::PointerType::isVoid() const
{
    return false;
}

const OpenCL::Parser::Type& OpenCL::Parser::PointerType::getType() const
{
    return *m_type;
}

OpenCL::Parser::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(std::string identifier) : m_identifier(std::move(
    identifier))
{}

const std::string& OpenCL::Parser::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Parser::PrimaryExpressionConstant::PrimaryExpressionConstant(const OpenCL::Parser::PrimaryExpressionConstant::variant& value)
    : m_value(value)
{}

const OpenCL::Parser::PrimaryExpressionConstant::variant& OpenCL::Parser::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

OpenCL::Parser::PrimaryExpressionParenthese::PrimaryExpressionParenthese(std::unique_ptr<Expression>&& expression)
    : m_expression(std::move(expression))
{}

const OpenCL::Parser::Expression& OpenCL::Parser::PrimaryExpressionParenthese::getExpression() const
{
    return *m_expression;
}

OpenCL::Parser::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(std::unique_ptr<PrimaryExpression>&& primaryExpression)
    : m_primaryExpression(std::move(primaryExpression))
{}

const OpenCL::Parser::PrimaryExpression& OpenCL::Parser::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return *m_primaryExpression;
}

OpenCL::Parser::PostFixExpressionSubscript::PostFixExpressionSubscript(std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                       std::unique_ptr<Expression>&& expression)
    : m_postFixExpression(std::move(postFixExpression)), m_expression(std::move(expression))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionSubscript::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const OpenCL::Parser::Expression& OpenCL::Parser::PostFixExpressionSubscript::getExpression() const
{
    return *m_expression;
}

OpenCL::Parser::PostFixExpressionDot::PostFixExpressionDot(std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                           std::string identifier) : m_postFixExpression(
    std::move(postFixExpression)), m_identifier(std::move(identifier))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionDot::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& OpenCL::Parser::PostFixExpressionDot::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Parser::UnaryExpressionPostFixExpression::UnaryExpressionPostFixExpression(std::unique_ptr<PostFixExpression>&& postFixExpression)
    : m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Parser::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(OpenCL::Parser::UnaryExpressionUnaryOperator::UnaryOperator anOperator,
                                                                           std::unique_ptr<UnaryExpression>&& unaryExpression)
    : m_operator(anOperator), m_unaryExpression(std::move(unaryExpression))
{}

OpenCL::Parser::UnaryExpressionUnaryOperator::UnaryOperator OpenCL::Parser::UnaryExpressionUnaryOperator::getAnOperator() const
{
    return m_operator;
}

const OpenCL::Parser::UnaryExpression& OpenCL::Parser::UnaryExpressionUnaryOperator::getUnaryExpression() const
{
    return *m_unaryExpression;
}

OpenCL::Parser::UnaryExpressionSizeOf::UnaryExpressionSizeOf(std::variant<std::unique_ptr<OpenCL::Parser::UnaryExpression>,
                                                                                std::unique_ptr<OpenCL::Parser::Type>>&& unaryOrType)
    : m_unaryOrType(std::move(unaryOrType))
{}

const std::variant<std::unique_ptr<OpenCL::Parser::UnaryExpression>,
                   std::unique_ptr<OpenCL::Parser::Type>>& OpenCL::Parser::UnaryExpressionSizeOf::getUnaryOrType() const
{
    return m_unaryOrType;
}

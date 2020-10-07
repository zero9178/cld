#include "Syntax.hpp"

#include <cld/Common/Util.hpp>

#include <algorithm>

cld::Syntax::Expression::Expression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<AssignmentExpression>&& assignmentExpression,
    std::vector<std::pair<Lexer::CTokenIterator, cld::Syntax::AssignmentExpression>> optionalAssignmentExpressions)
    : Node(begin, end),
      m_assignmentExpression(std::move(assignmentExpression)),
      m_optionalAssignmentExpressions(std::move(optionalAssignmentExpressions))
{
}

const cld::Syntax::AssignmentExpression& cld::Syntax::Expression::getAssignmentExpression() const
{
    return *m_assignmentExpression;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, cld::Syntax::AssignmentExpression>>&
    cld::Syntax::Expression::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmentExpressions;
}

cld::Syntax::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(Lexer::CTokenIterator begin,
                                                                      Lexer::CTokenIterator end,
                                                                      Lexer::CTokenIterator identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

cld::Lexer::CTokenIterator cld::Syntax::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::PrimaryExpressionConstant::PrimaryExpressionConstant(Lexer::CTokenIterator begin,
                                                                  Lexer::CTokenIterator end,
                                                                  PrimaryExpressionConstant::Variant value,
                                                                  Lexer::CToken::Type type)
    : Node(begin, end), m_value(std::move(value)), m_type(type)
{
}

const cld::Syntax::PrimaryExpressionConstant::Variant& cld::Syntax::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

cld::Lexer::CToken::Type cld::Syntax::PrimaryExpressionConstant::getType() const
{
    return m_type;
}

cld::Syntax::PrimaryExpressionParentheses::PrimaryExpressionParentheses(Lexer::CTokenIterator begin,
                                                                        Lexer::CTokenIterator end,
                                                                        cld::Syntax::Expression&& expression)
    : Node(begin, end), m_expression(std::move(expression))
{
}

const cld::Syntax::Expression& cld::Syntax::PrimaryExpressionParentheses::getExpression() const
{
    return m_expression;
}

cld::Syntax::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, cld::Syntax::PrimaryExpression&& primaryExpression)
    : Node(begin, end), m_primaryExpression(std::move(primaryExpression))
{
}

const cld::Syntax::PrimaryExpression& cld::Syntax::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return m_primaryExpression;
}

cld::Syntax::PostFixExpressionSubscript::PostFixExpressionSubscript(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression, Lexer::CTokenIterator openBracket,
    cld::Syntax::Expression&& expression, Lexer::CTokenIterator closeBracket)
    : Node(begin, end),
      m_postFixExpression(std::move(postFixExpression)),
      m_openBracket(openBracket),
      m_expression(std::move(expression)),
      m_closeBracket(closeBracket)
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
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<PostFixExpression>&& postFixExpression,
    Lexer::CTokenIterator incrementToken)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_incrementToken(incrementToken)
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionIncrement::getIncrementToken() const
{
    return m_incrementToken;
}

cld::Syntax::PostFixExpressionDecrement::PostFixExpressionDecrement(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<PostFixExpression>&& postFixExpression,
    Lexer::CTokenIterator decrementToken)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_decrementToken(decrementToken)
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionDecrement::getDecrementToken() const
{
    return m_decrementToken;
}

cld::Syntax::PostFixExpressionDot::PostFixExpressionDot(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression, Lexer::CTokenIterator identifier)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_identifier(identifier)
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionDot::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionDot::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::PostFixExpressionArrow::PostFixExpressionArrow(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<cld::Syntax::PostFixExpression>&& postFixExpression, Lexer::CTokenIterator identifier)
    : Node(begin, end), m_postFixExpression(std::move(postFixExpression)), m_identifier(identifier)
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionArrow::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionArrow::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<PostFixExpression>&& postFixExpression,
    Lexer::CTokenIterator openParentheses, std::vector<AssignmentExpression>&& optionalAssignmentExpressions,
    Lexer::CTokenIterator closeParentheses)
    : Node(begin, end),
      m_postFixExpression(std::move(postFixExpression)),
      m_openParentheses(openParentheses),
      m_optionalAssignmentExpressions(std::move(optionalAssignmentExpressions)),
      m_closeParentheses(closeParentheses)
{
}

cld::Syntax::PostFixExpressionTypeInitializer::PostFixExpressionTypeInitializer(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator openParentheses, TypeName&& typeName,
    Lexer::CTokenIterator closeParentheses, cld::Syntax::InitializerList&& initializerList)
    : Node(begin, end),
      m_openParentheses(openParentheses),
      m_typeName(std::make_unique<TypeName>(std::move(typeName))),
      m_closeParentheses(closeParentheses),
      m_initializerList(std::make_unique<InitializerList>(std::move(initializerList)))
{
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionTypeInitializer::getOpenParentheses() const
{
    return m_openParentheses;
}

const cld::Syntax::InitializerList& cld::Syntax::PostFixExpressionTypeInitializer::getInitializerList() const
{
    return *m_initializerList;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionTypeInitializer::getCloseParentheses() const
{
    return m_closeParentheses;
}

const cld::Syntax::TypeName& cld::Syntax::PostFixExpressionTypeInitializer::getTypeName() const
{
    return *m_typeName;
}

cld::Syntax::UnaryExpressionPostFixExpression::UnaryExpressionPostFixExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, cld::Syntax::PostFixExpression&& postFixExpression)
    : Node(begin, end), m_postFixExpression(std::make_unique<PostFixExpression>(std::move(postFixExpression)))
{
}

const cld::Syntax::PostFixExpression& cld::Syntax::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return *m_postFixExpression;
}

cld::Syntax::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, UnaryExpressionUnaryOperator::UnaryOperator anOperator,
    Lexer::CTokenIterator unaryToken, std::unique_ptr<CastExpression>&& unaryExpression)
    : Node(begin, end), m_castExpression(std::move(unaryExpression)), m_operator(anOperator), m_unaryToken(unaryToken)
{
}

cld::Syntax::UnaryExpressionUnaryOperator::UnaryOperator cld::Syntax::UnaryExpressionUnaryOperator::getOperator() const
{
    return m_operator;
}

const cld::Syntax::CastExpression& cld::Syntax::UnaryExpressionUnaryOperator::getCastExpression() const
{
    return *m_castExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::UnaryExpressionUnaryOperator::getUnaryToken() const
{
    return m_unaryToken;
}

cld::Syntax::UnaryExpressionSizeOf::UnaryExpressionSizeOf(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                          Lexer::CTokenIterator sizeOfToken,
                                                          cld::Syntax::UnaryExpressionSizeOf::variant&& variant)
    : Node(begin, end), m_sizeOfToken(sizeOfToken), m_variant(std::move(variant))
{
}

const cld::Syntax::UnaryExpressionSizeOf::variant& cld::Syntax::UnaryExpressionSizeOf::getVariant() const
{
    return m_variant;
}

cld::Syntax::CastExpression::CastExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                            CastExpression::Variant&& variant)
    : Node(begin, end), m_variant(std::make_unique<CastExpression::Variant>(std::move(variant)))
{
}

const cld::Syntax::CastExpression::Variant& cld::Syntax::CastExpression::getVariant() const
{
    return *m_variant;
}

cld::Syntax::Term::Term(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                        cld::Syntax::CastExpression&& castExpressions,
                        std::vector<Term::Operand>&& optionalCastExpressions)
    : Node(begin, end),
      m_castExpression(std::move(castExpressions)),
      m_optionalCastExpressions(std::move(optionalCastExpressions))
{
}

const cld::Syntax::CastExpression& cld::Syntax::Term::getCastExpression() const
{
    return m_castExpression;
}

const std::vector<cld::Syntax::Term::Operand>& cld::Syntax::Term::getOptionalCastExpressions() const
{
    return m_optionalCastExpressions;
}

cld::Syntax::AdditiveExpression::AdditiveExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                    cld::Syntax::Term&& term, std::vector<Operand>&& optionalTerms)
    : Node(begin, end), m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{
}

const cld::Syntax::Term& cld::Syntax::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<cld::Syntax::AdditiveExpression::Operand>& cld::Syntax::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

cld::Syntax::ShiftExpression::ShiftExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                              cld::Syntax::AdditiveExpression&& additiveExpression,
                                              std::vector<Operand>&& optionalAdditiveExpressions)
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

cld::Syntax::ReturnStatement::ReturnStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                              std::unique_ptr<Expression>&& expression)
    : Node(begin, end), m_expression(std::move(expression))
{
}

cld::Syntax::ExpressionStatement::ExpressionStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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

cld::Syntax::IfStatement::IfStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Expression&& expression,
                                      std::unique_ptr<Statement>&& branch, std::unique_ptr<Statement>&& elseBranch)
    : Node(begin, end),
      m_expression(std::move(expression)),
      m_branch(std::move(branch)),
      m_elseBranch(std::move(elseBranch))
{
    CLD_ASSERT(m_branch);
}

cld::Syntax::ContinueStatement::ContinueStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end)
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

cld::Syntax::CompoundStatement::CompoundStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                  std::vector<CompoundItem>&& blockItems)
    : Node(begin, end), m_blockItems(std::move(blockItems))
{
}

const std::vector<cld::Syntax::CompoundItem>& cld::Syntax::CompoundStatement::getBlockItems() const
{
    return m_blockItems;
}

cld::Syntax::ForStatement::ForStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                        std::unique_ptr<Statement>&& statement,
                                        std::variant<Declaration, std::unique_ptr<Expression>>&& initial,
                                        std::unique_ptr<Expression>&& controlling, std::unique_ptr<Expression>&& post)
    : Node(begin, end),
      m_statement(std::move(statement)),
      m_initial(std::move(initial)),
      m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    CLD_ASSERT(m_statement);
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

cld::Syntax::HeadWhileStatement::HeadWhileStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                    Expression&& expression, std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_expression(std::move(expression)), m_statement(std::move(statement))
{
    CLD_ASSERT(m_statement);
}

const cld::Syntax::Expression& cld::Syntax::HeadWhileStatement::getExpression() const
{
    return m_expression;
}

const cld::Syntax::Statement& cld::Syntax::HeadWhileStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::FootWhileStatement::FootWhileStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                    std::unique_ptr<Statement>&& statement, Expression&& expression)
    : Node(begin, end), m_statement(std::move(statement)), m_expression(std::move(expression))
{
    CLD_ASSERT(m_statement);
}

const cld::Syntax::Statement& cld::Syntax::FootWhileStatement::getStatement() const
{
    return *m_statement;
}

const cld::Syntax::Expression& cld::Syntax::FootWhileStatement::getExpression() const
{
    return m_expression;
}

const std::vector<cld::Syntax::ShiftExpression::Operand>&
    cld::Syntax::ShiftExpression::getOptionalAdditiveExpressions() const
{
    return m_optionalAdditiveExpressions;
}

cld::Syntax::RelationalExpression::RelationalExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                        ShiftExpression&& shiftExpression,
                                                        std::vector<Operand>&& optionalRelationalExpressions)
    : Node(begin, end),
      m_shiftExpression(std::move(shiftExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{
}

const cld::Syntax::ShiftExpression& cld::Syntax::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

const std::vector<cld::Syntax::RelationalExpression::Operand>&
    cld::Syntax::RelationalExpression::getOptionalShiftExpressions() const
{
    return m_optionalRelationalExpressions;
}

cld::Syntax::EqualityExpression::EqualityExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                    RelationalExpression&& relationalExpression,
                                                    std::vector<Operator>&& optionalRelationalExpressions)
    : Node(begin, end),
      m_relationalExpression(std::move(relationalExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{
}

const cld::Syntax::RelationalExpression& cld::Syntax::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

const std::vector<cld::Syntax::EqualityExpression::Operator>&
    cld::Syntax::EqualityExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

cld::Syntax::LogicalAndExpression::LogicalAndExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, BitOrExpression&& bitOrExpression,
    std::vector<std::pair<Lexer::CTokenIterator, BitOrExpression>>&& optionalBitOrExpressions)
    : Node(begin, end),
      m_bitOrExpression(std::move(bitOrExpression)),
      m_optionalBitOrExpressions(std::move(optionalBitOrExpressions))
{
}

const cld::Syntax::BitOrExpression& cld::Syntax::LogicalAndExpression::getBitOrExpression() const
{
    return m_bitOrExpression;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, cld::Syntax::BitOrExpression>>&
    cld::Syntax::LogicalAndExpression::getOptionalBitOrExpressions() const
{
    return m_optionalBitOrExpressions;
}

cld::Syntax::LogicalOrExpression::LogicalOrExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, LogicalAndExpression&& andExpression,
    std::vector<std::pair<Lexer::CTokenIterator, LogicalAndExpression>>&& optionalAndExpressions)
    : Node(begin, end),
      m_andExpression(std::move(andExpression)),
      m_optionalAndExpressions(std::move(optionalAndExpressions))
{
}

const cld::Syntax::LogicalAndExpression& cld::Syntax::LogicalOrExpression::getAndExpression() const
{
    return m_andExpression;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, cld::Syntax::LogicalAndExpression>>&
    cld::Syntax::LogicalOrExpression::getOptionalAndExpressions() const
{
    return m_optionalAndExpressions;
}

cld::Syntax::ConditionalExpression::ConditionalExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, LogicalOrExpression&& logicalOrExpression,
    const cld::Lexer::CToken* questionMark, std::unique_ptr<Expression>&& optionalExpression,
    const cld::Lexer::CToken* colon, std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression)
    : Node(begin, end),
      m_logicalOrExpression(std::make_unique<LogicalOrExpression>(std::move(logicalOrExpression))),
      m_optionalQuestionMark(questionMark),
      m_optionalExpression(std::move(optionalExpression)),
      m_optionalColon(colon),
      m_optionalConditionalExpression(std::move(optionalConditionalExpression))
{
}

const cld::Syntax::LogicalOrExpression& cld::Syntax::ConditionalExpression::getLogicalOrExpression() const
{
    return *m_logicalOrExpression;
}

const cld::Lexer::CToken* cld::Syntax::ConditionalExpression::getOptionalQuestionMark() const
{
    return m_optionalQuestionMark;
}

const cld::Syntax::Expression* cld::Syntax::ConditionalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

const cld::Lexer::CToken* cld::Syntax::ConditionalExpression::getOptionalColon() const
{
    return m_optionalColon;
}

const cld::Syntax::ConditionalExpression* cld::Syntax::ConditionalExpression::getOptionalConditionalExpression() const
{
    return m_optionalConditionalExpression.get();
}

const cld::Syntax::Statement& cld::Syntax::ForStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::BitAndExpression::BitAndExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, EqualityExpression&& equalityExpression,
    std::vector<std::pair<Lexer::CTokenIterator, EqualityExpression>>&& optionalEqualityExpressions)
    : Node(begin, end),
      m_equalityExpression(std::move(equalityExpression)),
      m_optionalEqualityExpressions(std::move(optionalEqualityExpressions))
{
}

const cld::Syntax::EqualityExpression& cld::Syntax::BitAndExpression::getEqualityExpression() const
{
    return m_equalityExpression;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, cld::Syntax::EqualityExpression>>&
    cld::Syntax::BitAndExpression::getOptionalEqualityExpressions() const
{
    return m_optionalEqualityExpressions;
}

cld::Syntax::BitXorExpression::BitXorExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, BitAndExpression&& bitAndExpression,
    std::vector<std::pair<Lexer::CTokenIterator, BitAndExpression>>&& optionalBitAndExpressions)
    : Node(begin, end),
      m_bitAndExpression(std::move(bitAndExpression)),
      m_optionalBitAndExpressions(std::move(optionalBitAndExpressions))
{
}

const cld::Syntax::BitAndExpression& cld::Syntax::BitXorExpression::getBitAndExpression() const
{
    return m_bitAndExpression;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, cld::Syntax::BitAndExpression>>&
    cld::Syntax::BitXorExpression::getOptionalBitAndExpressions() const
{
    return m_optionalBitAndExpressions;
}

cld::Syntax::BitOrExpression::BitOrExpression(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, BitXorExpression&& bitXorExpression,
    std::vector<std::pair<Lexer::CTokenIterator, BitXorExpression>>&& bitXorExpressions)
    : Node(begin, end),
      m_bitXorExpression(std::move(bitXorExpression)),
      m_optionalBitXorExpressions(std::move(bitXorExpressions))
{
}

const cld::Syntax::BitXorExpression& cld::Syntax::BitOrExpression::getBitXorExpression() const
{
    return m_bitXorExpression;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, cld::Syntax::BitXorExpression>>&
    cld::Syntax::BitOrExpression::getOptionalBitXorExpressions() const
{
    return m_optionalBitXorExpressions;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionFunctionCall::getOpenParentheses() const
{
    return m_openParentheses;
}

cld::Lexer::CTokenIterator cld::Syntax::PostFixExpressionFunctionCall::getCloseParentheses() const
{
    return m_closeParentheses;
}

const cld::Syntax::PostFixExpression& cld::Syntax::PostFixExpressionFunctionCall::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::vector<cld::Syntax::AssignmentExpression>&
    cld::Syntax::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmentExpressions;
}

cld::Syntax::SwitchStatement::SwitchStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                              Expression&& expression, std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_expression(std::move(expression)), m_statement(std::move(statement))
{
    CLD_ASSERT(m_statement);
}

const cld::Syntax::Expression& cld::Syntax::SwitchStatement::getExpression() const
{
    return m_expression;
}

const cld::Syntax::Statement& cld::Syntax::SwitchStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::DefaultStatement::DefaultStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                Lexer::CTokenIterator defaultToken, Lexer::CTokenIterator colonToken,
                                                std::unique_ptr<Statement>&& statement)
    : Node(begin, end), m_defaultToken(defaultToken), m_colonToken(colonToken), m_statement(std::move(statement))
{
    CLD_ASSERT(m_statement);
}

cld::Lexer::CTokenIterator cld::Syntax::DefaultStatement::getDefaultToken() const
{
    return m_defaultToken;
}

cld::Lexer::CTokenIterator cld::Syntax::DefaultStatement::getColonToken() const
{
    return m_colonToken;
}

const cld::Syntax::Statement& cld::Syntax::DefaultStatement::getStatement() const
{
    return *m_statement;
}

cld::Syntax::CaseStatement::CaseStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          Lexer::CTokenIterator caseToken, ConstantExpression&& constantExpression,
                                          Lexer::CTokenIterator colonToken, std::unique_ptr<Statement>&& statement)
    : Node(begin, end),
      m_caseToken(caseToken),
      m_constantExpression(std::move(constantExpression)),
      m_colonToken(colonToken),
      m_statement(std::move(statement))
{
}

const cld::Syntax::Statement& cld::Syntax::CaseStatement::getStatement() const
{
    return *m_statement;
}

cld::Lexer::CTokenIterator cld::Syntax::CaseStatement::getCaseToken() const
{
    return m_caseToken;
}

cld::Lexer::CTokenIterator cld::Syntax::CaseStatement::getColonToken() const
{
    return m_colonToken;
}

const cld::Syntax::ConstantExpression& cld::Syntax::CaseStatement::getConstantExpression() const
{
    return m_constantExpression;
}

cld::Syntax::InitializerList::InitializerList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, const Lexer::CToken* name,
    std::vector<std::pair<Lexer::CTokenIterator, std::optional<ConstantExpression>>>&& values)
    : Node(begin, end), m_name(std::move(name)), m_values(std::move(values))
{
}

cld::Lexer::CTokenIterator cld::Syntax::EnumDeclaration::getName() const
{
    return m_name;
}

const std::vector<std::pair<cld::Lexer::CTokenIterator, std::optional<cld::Syntax::ConstantExpression>>>&
    cld::Syntax::EnumDeclaration::getValues() const
{
    return m_values;
}

cld::Syntax::Initializer::Initializer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                      cld::Syntax::Initializer::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::Initializer::variant& cld::Syntax::Initializer::getVariant() const
{
    return m_variant;
}

cld::Syntax::BreakStatement::BreakStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end) : Node(begin, end)
{
}

cld::Syntax::EnumSpecifier::EnumSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          cld::Syntax::EnumSpecifier::Variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::EnumSpecifier::Variant& cld::Syntax::EnumSpecifier::getVariant() const
{
    return m_variant;
}

cld::Syntax::TypeSpecifier::TypeSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          cld::Syntax::TypeSpecifier::variant&& variant)
    : Node(begin, end), m_variant(std::move(variant))
{
}

const cld::Syntax::TypeSpecifier::variant& cld::Syntax::TypeSpecifier::getVariant() const
{
    return m_variant;
}

cld::Syntax::Declaration::Declaration(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::vector<cld::Syntax::DeclarationSpecifier>&& declarationSpecifiers,
    std::vector<std::pair<std::unique_ptr<cld::Syntax::Declarator>, std::unique_ptr<cld::Syntax::Initializer>>>&&
        initDeclarators)
    : Node(begin, end),
      m_declarationSpecifiers(std::move(declarationSpecifiers)),
      m_initDeclarators(std::move(initDeclarators))
{
    CLD_ASSERT(std::all_of(m_initDeclarators.begin(), m_initDeclarators.end(),
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

cld::Syntax::StructOrUnionSpecifier::StructOrUnionSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                            bool isUnion, const Lexer::CToken* identifierLoc,
                                                            std::vector<StructDeclaration>&& structDeclarations,
                                                            bool extensionsEnabled)
    : Node(begin, end),
      m_isUnion(isUnion),
      m_identifierLoc(std::move(identifierLoc)),
      m_structDeclarations(std::move(structDeclarations)),
      m_extensionEnabled(extensionsEnabled)
{
}

bool cld::Syntax::StructOrUnionSpecifier::isUnion() const
{
    return m_isUnion;
}

const cld::Lexer::CToken* cld::Syntax::StructOrUnionSpecifier::getIdentifierLoc() const
{
    return m_identifierLoc;
}

const std::vector<cld::Syntax::StructOrUnionSpecifier::StructDeclaration>&
    cld::Syntax::StructOrUnionSpecifier::getStructDeclarations() const
{
    return m_structDeclarations;
}

bool cld::Syntax::StructOrUnionSpecifier::extensionsEnabled() const
{
    return m_extensionEnabled;
}

cld::Syntax::ParameterList::ParameterList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          std::vector<cld::Syntax::ParameterDeclaration>&& parameterList)
    : Node(begin, end), m_parameterList(std::move(parameterList))
{
}

const std::vector<cld::Syntax::ParameterDeclaration>& cld::Syntax::ParameterList::getParameterDeclarations() const&
{
    return m_parameterList;
}

std::vector<cld::Syntax::ParameterDeclaration>&& cld::Syntax::ParameterList::getParameterDeclarations() &&
{
    return std::move(m_parameterList);
}

cld::Syntax::ParameterTypeList::ParameterTypeList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                  cld::Syntax::ParameterList&& parameterList, bool hasEllipse)
    : Node(begin, end), m_parameterList(std::move(parameterList).getParameterDeclarations()), m_hasEllipse(hasEllipse)
{
}

const std::vector<cld::Syntax::ParameterDeclaration>& cld::Syntax::ParameterTypeList::getParameters() const
{
    return m_parameterList;
}

bool cld::Syntax::ParameterTypeList::hasEllipse() const
{
    return m_hasEllipse;
}

cld::Syntax::Pointer::Pointer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                              std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers)
    : Node(begin, end), m_typeQualifiers(std::move(typeQualifiers))
{
}

const std::vector<cld::Syntax::TypeQualifier>& cld::Syntax::Pointer::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

cld::Syntax::DirectDeclaratorNoStaticOrAsterisk::DirectDeclaratorNoStaticOrAsterisk(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<cld::Syntax::DirectDeclarator>&& directDeclarator,
    std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers,
    std::unique_ptr<cld::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_directDeclarator(std::move(directDeclarator)),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    CLD_ASSERT(m_directDeclarator);
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
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<cld::Syntax::DirectDeclarator>&& directDeclarator, Lexer::CTokenIterator staticLoc,
    std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers, cld::Syntax::AssignmentExpression&& assignmentExpression)
    : Node(begin, end),
      m_directDeclarator(std::move(directDeclarator)),
      m_staticLoc(staticLoc),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
    CLD_ASSERT(m_directDeclarator);
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

cld::Lexer::CTokenIterator cld::Syntax::DirectDeclaratorStatic::getStaticLoc() const
{
    return m_staticLoc;
}

cld::Syntax::DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, cld::Syntax::DirectDeclarator&& directDeclarator,
    std::vector<cld::Syntax::TypeQualifier>&& typeQualifiers, Lexer::CTokenIterator asterisk)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_asterisk(asterisk)
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

cld::Lexer::CTokenIterator cld::Syntax::DirectDeclaratorAsterisk::getAsterisk() const
{
    return m_asterisk;
}

cld::Syntax::DirectDeclaratorParenthesesParameters::DirectDeclaratorParenthesesParameters(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, cld::Syntax::DirectDeclarator&& directDeclarator,
    cld::Syntax::ParameterTypeList&& parameterTypeList)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_parameterTypeList(std::move(parameterTypeList))
{
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorParenthesesParameters::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const cld::Syntax::ParameterTypeList& cld::Syntax::DirectDeclaratorParenthesesParameters::getParameterTypeList() const
{
    return m_parameterTypeList;
}

cld::Syntax::DirectDeclaratorParenthesesIdentifiers::DirectDeclaratorParenthesesIdentifiers(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, cld::Syntax::DirectDeclarator&& directDeclarator,
    std::vector<Lexer::CTokenIterator>&& identifiers)
    : Node(begin, end),
      m_directDeclarator(std::make_unique<DirectDeclarator>(std::move(directDeclarator))),
      m_identifiers(std::move(identifiers))
{
}

const cld::Syntax::DirectDeclarator& cld::Syntax::DirectDeclaratorParenthesesIdentifiers::getDirectDeclarator() const
{
    return *m_directDeclarator;
}

const std::vector<cld::Lexer::CTokenIterator>&
    cld::Syntax::DirectDeclaratorParenthesesIdentifiers::getIdentifiers() const
{
    return m_identifiers;
}

cld::Syntax::Declarator::Declarator(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<cld::Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator,
    std::vector<TypeQualifier>&& typeQualifiers,
    std::unique_ptr<cld::Syntax::AssignmentExpression>&& assignmentExpression)
    : Node(begin, end),
      m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
}

const cld::Syntax::DirectAbstractDeclarator*
    cld::Syntax::DirectAbstractDeclaratorAssignmentExpression::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const std::vector<cld::Syntax::TypeQualifier>&
    cld::Syntax::DirectAbstractDeclaratorAssignmentExpression::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const cld::Syntax::AssignmentExpression*
    cld::Syntax::DirectAbstractDeclaratorAssignmentExpression::getAssignmentExpression() const
{
    return m_assignmentExpression.get();
}

cld::Syntax::DirectAbstractDeclaratorParameterTypeList::DirectAbstractDeclaratorParameterTypeList(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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

cld::Syntax::AbstractDeclarator::AbstractDeclarator(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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

cld::Syntax::TypeName::TypeName(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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

cld::Syntax::GotoStatement::GotoStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          Lexer::CTokenIterator identifier)
    : Node(begin, end), m_identifier(identifier)
{
}

cld::Lexer::CTokenIterator cld::Syntax::GotoStatement::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::LabelStatement::LabelStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                            Lexer::CTokenIterator identifier, Statement&& statement)
    : Node(begin, end), m_identifier(identifier), m_statement(std::make_unique<Statement>(std::move(statement)))
{
}

const cld::Syntax::Statement& cld::Syntax::LabelStatement::getStatement() const
{
    return *m_statement;
}

cld::Lexer::CTokenIterator cld::Syntax::LabelStatement::getIdentifierToken() const
{
    return m_identifier;
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
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
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

cld::Syntax::Node::Node(Lexer::CTokenIterator begin, Lexer::CTokenIterator end) : m_begin(begin), m_end(end) {}

cld::Lexer::CTokenIterator cld::Syntax::Node::begin() const
{
    return m_begin;
}

cld::Lexer::CTokenIterator cld::Syntax::Node::end() const
{
    return m_end;
}

cld::Syntax::TypeQualifier::TypeQualifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          cld::Syntax::TypeQualifier::Qualifier qualifier)
    : Node(begin, end), m_qualifier(qualifier)
{
}

cld::Syntax::TypeQualifier::Qualifier cld::Syntax::TypeQualifier::getQualifier() const
{
    return m_qualifier;
}

cld::Syntax::StorageClassSpecifier::StorageClassSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                          cld::Syntax::StorageClassSpecifier::Specifiers specifier)
    : Node(begin, end), m_specifier(specifier)
{
}

cld::Syntax::StorageClassSpecifier::Specifiers cld::Syntax::StorageClassSpecifier::getSpecifier() const
{
    return m_specifier;
}

cld::Syntax::FunctionSpecifier::FunctionSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end)
    : Node(begin, end)
{
}

cld::Syntax::DirectDeclaratorIdentifier::DirectDeclaratorIdentifier(Lexer::CTokenIterator begin,
                                                                    Lexer::CTokenIterator end,
                                                                    Lexer::CTokenIterator identifierLoc)
    : Node(begin, end), m_identifierLoc(identifierLoc)
{
}

cld::Lexer::CTokenIterator cld::Syntax::DirectDeclaratorIdentifier::getIdentifierLoc() const
{
    return m_identifierLoc;
}

cld::Syntax::DirectDeclaratorParentheses::DirectDeclaratorParentheses(Lexer::CTokenIterator begin,
                                                                      Lexer::CTokenIterator end,
                                                                      std::unique_ptr<Declarator>&& declarator)
    : Node(begin, end), m_declarator(std::move(declarator))
{
    CLD_ASSERT(m_declarator);
}

const cld::Syntax::Declarator& cld::Syntax::DirectDeclaratorParentheses::getDeclarator() const
{
    return *m_declarator;
}

cld::Syntax::DirectAbstractDeclaratorParentheses::DirectAbstractDeclaratorParentheses(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<AbstractDeclarator>&& abstractDeclarator)
    : Node(begin, end), m_abstractDeclarator(std::move(abstractDeclarator))
{
}

const cld::Syntax::AbstractDeclarator& cld::Syntax::DirectAbstractDeclaratorParentheses::getAbstractDeclarator() const
{
    return *m_abstractDeclarator;
}

cld::Syntax::DirectAbstractDeclaratorAsterisk::DirectAbstractDeclaratorAsterisk(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator, Lexer::CTokenIterator asterisk)
    : Node(begin, end), m_directAbstractDeclarator(std::move(directAbstractDeclarator)), m_asterisk(asterisk)
{
}

const cld::Syntax::DirectAbstractDeclarator*
    cld::Syntax::DirectAbstractDeclaratorAsterisk::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

cld::Lexer::CTokenIterator cld::Syntax::DirectAbstractDeclaratorAsterisk::getAsterisk() const
{
    return m_asterisk;
}

cld::Syntax::AssignmentExpression::AssignmentExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                        cld::Syntax::ConditionalExpression&& conditionalExpression,
                                                        std::vector<Operand>&& optionalConditionalExpressions)
    : Node(begin, end),
      m_conditionalExpression(std::move(conditionalExpression)),
      m_optionalConditionalExpression(std::move(optionalConditionalExpressions))
{
}
const cld::Syntax::ConditionalExpression& cld::Syntax::AssignmentExpression::getConditionalExpression() const
{
    return m_conditionalExpression;
}
const std::vector<cld::Syntax::AssignmentExpression::Operand>&
    cld::Syntax::AssignmentExpression::getOptionalConditionalExpressions() const
{
    return m_optionalConditionalExpression;
}

cld::Syntax::UnaryExpressionDefined::UnaryExpressionDefined(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                            std::string_view identifier)
    : Node(begin, end), m_identifier(std::move(identifier))
{
}

std::string_view cld::Syntax::UnaryExpressionDefined::getIdentifier() const
{
    return m_identifier;
}

cld::Syntax::DirectAbstractDeclaratorStatic::DirectAbstractDeclaratorStatic(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
    std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator, Lexer::CTokenIterator staticLoc,
    std::vector<TypeQualifier>&& typeQualifiers, AssignmentExpression&& assignmentExpression)
    : Node(begin, end),
      m_directAbstractDeclarator(std::move(directAbstractDeclarator)),
      m_staticLoc(staticLoc),
      m_typeQualifiers(std::move(typeQualifiers)),
      m_assignmentExpression(std::move(assignmentExpression))
{
}

const cld::Syntax::DirectAbstractDeclarator*
    cld::Syntax::DirectAbstractDeclaratorStatic::getDirectAbstractDeclarator() const
{
    return m_directAbstractDeclarator.get();
}

const std::vector<cld::Syntax::TypeQualifier>& cld::Syntax::DirectAbstractDeclaratorStatic::getTypeQualifiers() const
{
    return m_typeQualifiers;
}

const cld::Syntax::AssignmentExpression& cld::Syntax::DirectAbstractDeclaratorStatic::getAssignmentExpression() const
{
    return m_assignmentExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::DirectAbstractDeclaratorStatic::getStaticLoc() const
{
    return m_staticLoc;
}

cld::Syntax::GNUAttributes::GNUAttributes(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          std::vector<GNUAttribute>&& attributes)
    : Node(begin, end), m_attributes(std::move(attributes))
{
}

cld::Syntax::GNUSimpleASM::GNUSimpleASM(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::string string)
    : Node(begin, end), m_string(std::move(string))
{
}

cld::Syntax::GNUASMQualifier::GNUASMQualifier(Lexer::CTokenIterator qualifier)
    : Node(qualifier, qualifier + 1), m_qualifier([qualifier] {
          switch (qualifier->getTokenType())
          {
              case Lexer::TokenType::VolatileKeyword: return Qualifier::Volatile;
              case Lexer::TokenType::InlineKeyword: return Qualifier::Inline;
              case Lexer::TokenType::GotoKeyword: return Qualifier::Goto;
              default: CLD_UNREACHABLE;
          }
      }())
{
}

cld::Syntax::GNUASMStatement::GNUASMStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                              std::vector<GNUASMQualifier> qualifiers, std::string asmString,
                                              std::vector<GNUASMOperand> firstList,
                                              std::vector<GNUASMOperand> secondList, std::vector<std::string> clobbers)
    : Node(begin, end),
      m_qualifiers(std::move(qualifiers)),
      m_asmString(std::move(asmString)),
      m_firstList(std::move(firstList)),
      m_secondList(std::move(secondList)),
      m_clobbers(std::move(clobbers))
{
}

cld::Syntax::PrimaryExpressionBuiltinVAArg::PrimaryExpressionBuiltinVAArg(
    Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator builtinToken,
    Lexer::CTokenIterator openParentheses, std::unique_ptr<AssignmentExpression>&& assignmentExpression,
    Lexer::CTokenIterator comma, std::unique_ptr<TypeName>&& typeName, Lexer::CTokenIterator closeParentheses)
    : Node(begin, end),
      m_builtinToken(builtinToken),
      m_openParentheses(openParentheses),
      m_assignmentExpression(std::move(assignmentExpression)),
      m_comma(comma),
      m_typeName(std::move(typeName)),
      m_closeParentheses(closeParentheses)
{
    CLD_ASSERT(m_assignmentExpression);
}

cld::Lexer::CTokenIterator cld::Syntax::PrimaryExpressionBuiltinVAArg::getBuiltinToken() const
{
    return m_builtinToken;
}

cld::Lexer::CTokenIterator cld::Syntax::PrimaryExpressionBuiltinVAArg::getOpenParentheses() const
{
    return m_openParentheses;
}

const cld::Syntax::AssignmentExpression& cld::Syntax::PrimaryExpressionBuiltinVAArg::getAssignmentExpression() const
{
    return *m_assignmentExpression;
}

cld::Lexer::CTokenIterator cld::Syntax::PrimaryExpressionBuiltinVAArg::getComma() const
{
    return m_comma;
}

const cld::Syntax::TypeName& cld::Syntax::PrimaryExpressionBuiltinVAArg::getTypeName() const
{
    return *m_typeName;
}

cld::Lexer::CTokenIterator cld::Syntax::PrimaryExpressionBuiltinVAArg::getCloseParentheses() const
{
    return m_closeParentheses;
}

#include <utility>

#include <utility>

#include <utility>

#include <utility>

#include <utility>

#include <utility>

#include <utility>

#include <utility>

#include "Parser.hpp"

#include <algorithm>

OpenCL::Parser::Program::Program(std::vector<OpenCL::Parser::Function>&& functions) noexcept : m_functions(std::move(
    functions))
{

}

const std::vector<OpenCL::Parser::Function>& OpenCL::Parser::Program::getFunctions() const
{
    return m_functions;
}

const std::string& OpenCL::Parser::Function::getName() const
{
    return m_name;
}

const std::vector<std::string>& OpenCL::Parser::Function::getArguments() const
{
    return m_arguments;
}

const std::vector<std::unique_ptr<OpenCL::Parser::BlockItem>>& OpenCL::Parser::Function::getBlockItems() const
{
    return m_blockItems;
}

OpenCL::Parser::Function::Function(std::string name,
                                   std::vector<std::string> arguments,
                                   std::vector<std::unique_ptr<BlockItem>>&& blockItems) : m_name(std::move(
    name)), m_arguments(std::move(arguments)), m_blockItems(std::move(blockItems))
{}

const std::string& OpenCL::Parser::Declaration::getName() const
{
    return m_name;
}

const OpenCL::Parser::Expression* OpenCL::Parser::Declaration::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

OpenCL::Parser::Declaration::Declaration(std::string name) : m_name(std::move(name))
{}

OpenCL::Parser::Declaration::Declaration(std::string name,
                                         std::unique_ptr<Expression>&& optionalExpression)
    : m_name(std::move(name)), m_optionalExpression(std::move(optionalExpression))
{}

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

OpenCL::Parser::ForStatement::ForStatement(std::unique_ptr<Expression>&& initial,
                                           std::unique_ptr<Expression>&& controlling,
                                           std::unique_ptr<Expression>&& post)
    : m_initial(std::move(initial)), m_controlling(std::move(controlling)), m_post(std::move(post))
{}

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

OpenCL::Parser::ForDeclarationStatement::ForDeclarationStatement(Declaration&& initial,
                                                                 std::unique_ptr<Expression>&& controlling,
                                                                 std::unique_ptr<Expression>&& post)
    : m_initial(std::move(initial)), m_controlling(std::move(controlling)), m_post(std::move(post))
{}

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

OpenCL::Parser::AssignmentExpression::AssignmentExpression(std::string identifier,
                                                           std::unique_ptr<NonCommaExpression>&& expression,
                                                           OpenCL::Parser::AssignmentExpression::AssignOperator assignOperator)
    : m_identifier(std::move(identifier)), m_expression(std::move(expression)), m_assignOperator(assignOperator)
{
    assert(m_expression);
}

const std::string& OpenCL::Parser::AssignmentExpression::getIdentifier() const
{
    return m_identifier;
}

const OpenCL::Parser::NonCommaExpression& OpenCL::Parser::AssignmentExpression::getExpression() const
{
    return *m_expression;
}

OpenCL::Parser::AssignmentExpression::AssignOperator OpenCL::Parser::AssignmentExpression::getAssignOperator() const
{
    return m_assignOperator;
}

OpenCL::Parser::Term::Term(std::unique_ptr<Factor>&& factor,
                           OpenCL::Parser::Term::BinaryDotOperator optionalOperator,
                           std::unique_ptr<Factor>&& optionalFactor)
    : m_factor(std::move(factor)), m_optionalOperator(optionalOperator), m_optionalFactor(std::move(optionalFactor))
{
    assert(m_factor);
}

const OpenCL::Parser::Factor& OpenCL::Parser::Term::getFactor() const
{
    return *m_factor;
}

OpenCL::Parser::Term::BinaryDotOperator OpenCL::Parser::Term::getOptionalOperator() const
{
    return m_optionalOperator;
}

const OpenCL::Parser::Factor* OpenCL::Parser::Term::getOptionalFactor() const
{
    return m_optionalFactor.get();
}

OpenCL::Parser::AdditiveExpression::AdditiveExpression(Term&& term,
                                                       OpenCL::Parser::AdditiveExpression::BinaryDashOperator optionalOperator,
                                                       std::unique_ptr<Term>&& optionalTerm)
    : m_term(std::move(term)), m_optionalOperator(optionalOperator), m_optionalTerm(std::move(optionalTerm))
{}

const OpenCL::Parser::Term& OpenCL::Parser::AdditiveExpression::getTerm() const
{
    return m_term;
}

OpenCL::Parser::AdditiveExpression::BinaryDashOperator OpenCL::Parser::AdditiveExpression::getOptionalOperator() const
{
    return m_optionalOperator;
}

const OpenCL::Parser::Term* OpenCL::Parser::AdditiveExpression::getOptionalTerm() const
{
    return m_optionalTerm.get();
}

OpenCL::Parser::ShiftExpression::ShiftExpression(AdditiveExpression&& additiveExpression,
                                                 OpenCL::Parser::ShiftExpression::ShiftOperator optionalOperator,
                                                 std::unique_ptr<AdditiveExpression>&& optionalAdditiveExpression)
    : m_additiveExpression(std::move(additiveExpression)), m_optionalOperator(optionalOperator),
      m_optionalAdditiveExpression(std::move(optionalAdditiveExpression))
{}

const OpenCL::Parser::AdditiveExpression& OpenCL::Parser::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
}

OpenCL::Parser::ShiftExpression::ShiftOperator OpenCL::Parser::ShiftExpression::getOptionalOperator() const
{
    return m_optionalOperator;
}

const OpenCL::Parser::AdditiveExpression* OpenCL::Parser::ShiftExpression::getOptionalAdditiveExpression() const
{
    return m_optionalAdditiveExpression.get();
}

OpenCL::Parser::RelationalExpression::RelationalExpression(ShiftExpression&& shiftExpression,
                                                           OpenCL::Parser::RelationalExpression::RelationalOperator optionalOperator,
                                                           std::unique_ptr<ShiftExpression>&& optionalExpression)
    : m_shiftExpression(std::move(shiftExpression)), m_optionalOperator(optionalOperator),
      m_optionalExpression(std::move(optionalExpression))
{}

const OpenCL::Parser::ShiftExpression& OpenCL::Parser::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

OpenCL::Parser::RelationalExpression::RelationalOperator OpenCL::Parser::RelationalExpression::getOptionalOperator() const
{
    return m_optionalOperator;
}

const OpenCL::Parser::ShiftExpression* OpenCL::Parser::RelationalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

OpenCL::Parser::EqualityExpression::EqualityExpression(RelationalExpression&& relationalExpression,
                                                       OpenCL::Parser::EqualityExpression::EqualityOperator optionalOperator,
                                                       std::unique_ptr<RelationalExpression>&& optionalRelationalExpression)
    : m_relationalExpression(std::move(relationalExpression)), m_optionalOperator(optionalOperator),
      m_optionalRelationalExpression(std::move(optionalRelationalExpression))
{}

const OpenCL::Parser::RelationalExpression& OpenCL::Parser::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

OpenCL::Parser::EqualityExpression::EqualityOperator OpenCL::Parser::EqualityExpression::getOptionalOperator() const
{
    return m_optionalOperator;
}

const OpenCL::Parser::RelationalExpression* OpenCL::Parser::EqualityExpression::getOptionalRelationalExpression() const
{
    return m_optionalRelationalExpression.get();
}

OpenCL::Parser::LogicalAndExpression::LogicalAndExpression(EqualityExpression&& equalityExpression,
                                                           std::unique_ptr<EqualityExpression>&& optionalEqualityExpression)
    : m_equalityExpression(std::move(equalityExpression)),
      m_optionalEqualityExpression(std::move(optionalEqualityExpression))
{}

const OpenCL::Parser::EqualityExpression& OpenCL::Parser::LogicalAndExpression::getEqualityExpression() const
{
    return m_equalityExpression;
}

const OpenCL::Parser::EqualityExpression* OpenCL::Parser::LogicalAndExpression::getOptionalEqualityExpression() const
{
    return m_optionalEqualityExpression.get();
}

OpenCL::Parser::LogicalOrExpression::LogicalOrExpression(LogicalAndExpression&& andExpression,
                                                         std::unique_ptr<LogicalAndExpression>&& optionalAndExpression)
    : m_andExpression(std::move(andExpression)), m_optionalAndExpression(std::move(optionalAndExpression))
{}

const OpenCL::Parser::LogicalAndExpression& OpenCL::Parser::LogicalOrExpression::getAndExpression() const
{
    return m_andExpression;
}

const OpenCL::Parser::LogicalAndExpression* OpenCL::Parser::LogicalOrExpression::getOptionalAndExpression() const
{
    return m_optionalAndExpression.get();
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

OpenCL::Parser::ConstantFactor::ConstantFactor(std::string value) : value(std::move(value))
{}

const std::string& OpenCL::Parser::ConstantFactor::getValue() const
{
    return value;
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

OpenCL::Parser::FunctionalCall::FunctionalCall(std::string name,
                                               std::vector<std::unique_ptr<OpenCL::Parser::NonCommaExpression>> expressions)
    : m_name(std::move(name)), m_expressions(std::move(expressions))
{
    assert(std::all_of(m_expressions.begin(), m_expressions.end(), [](const std::unique_ptr<NonCommaExpression>& ptr)
    { return ptr.get(); }));
}

using Tokens = std::vector<OpenCL::Lexer::Token>;

namespace
{
    OpenCL::Parser::Program parseProgram(Tokens& tokens);

    OpenCL::Parser::Function parseFunction(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::BlockItem> parseBlockItem(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::Statement> parseStatement(Tokens& tokens);

    OpenCL::Parser::Expression parseExpression(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::NonCommaExpression> parseNonCommaExpression(Tokens& tokens);

    OpenCL::Parser::ConditionalExpression parseConditionalExpression(Tokens& tokens);

    OpenCL::Parser::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens);

    OpenCL::Parser::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens);

    OpenCL::Parser::EqualityExpression parseEqualityExpression(Tokens& tokens);

    OpenCL::Parser::RelationalExpression parseRelationalExpression(Tokens& tokens);

    OpenCL::Parser::ShiftExpression parseShiftExpression(Tokens& tokens);

    OpenCL::Parser::AdditiveExpression parseAdditiveExpression(Tokens& tokens);

    OpenCL::Parser::Term parseTerm(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::Factor> parseFactor(Tokens& tokens);
}

OpenCL::Parser::Program OpenCL::Parser::buildTree(std::vector<OpenCL::Lexer::Token>&& tokens)
{
    return parseProgram(tokens);
}

namespace
{
    using namespace OpenCL::Lexer;
    using namespace OpenCL::Parser;

    bool isAssignment(TokenType type)
    {
        return type == TokenType::Assignment
            || type == TokenType::PlusAssign
            || type == TokenType::MinusAssign
            || type == TokenType::DivideAssign
            || type == TokenType::MultiplyAssign
            || type == TokenType::ModuloAssign
            || type == TokenType::ShiftLeftAssign
            || type == TokenType::ShiftRightAssign
            || type == TokenType::BitAndAssign
            || type == TokenType::BitOrAssign
            || type == TokenType::BitXorAssign;
    }

    OpenCL::Parser::Program parseProgram(Tokens& tokens)
    {
        std::vector<Function> functions;
        while (!tokens.empty())
        {
            functions.push_back(parseFunction(tokens));
        }
        return Program(std::move(functions));
    }

    OpenCL::Parser::Function parseFunction(Tokens& tokens)
    {
        auto currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::IntKeyword)
        {
            throw std::runtime_error("Unsupported return type");
        }
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Invalid identifier for function");
        }
        auto name = std::get<std::string>(currToken.getValue());
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::OpenParanthese)
        {
            throw std::runtime_error("Expected Opening Parantheses after function identifier");
        }
        std::vector<std::string> arguments;
        currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::CloseParanthese)
        {
            while (true)
            {
                currToken = tokens.back();
                tokens.pop_back();
                if (currToken.getTokenType() != TokenType::IntKeyword)
                {
                    throw std::runtime_error("Unsupported argument type");
                }
                currToken = tokens.back();
                tokens.pop_back();
                if (currToken.getTokenType() != TokenType::Identifier)
                {
                    throw std::runtime_error("Expected identifier after paramter type");
                }
                arguments.push_back(std::get<std::string>(currToken.getValue()));
                currToken = tokens.back();
                if (currToken.getTokenType() == TokenType::CloseParanthese)
                {
                    break;
                }
                else if (currToken.getTokenType() != TokenType::Comma)
                {
                    throw std::runtime_error("Expected Comma between arguments");
                }
            }
        }
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::CloseParanthese)
        {
            throw std::runtime_error("Expected Close Parantheses after Argument List");
        }

        currToken = tokens.back();
        tokens.pop_back();
        std::vector<std::unique_ptr<BlockItem>> result;
        if (currToken.getTokenType() == TokenType::OpenBrace)
        {
            while (tokens.back().getTokenType() != TokenType::CloseBrace)
            {
                result.push_back(parseBlockItem(tokens));
            }
            if (tokens.empty() || tokens.back().getTokenType() != TokenType::CloseBrace)
            {
                throw std::runtime_error("Expected Closing Brace at the end of Block");
            }
            else
            {
                tokens.pop_back();
            }
        }

        return Function(std::move(name), std::move(arguments), std::move(result));
    }

    std::unique_ptr<OpenCL::Parser::BlockItem> parseBlockItem(Tokens& tokens)
    {
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::IntKeyword)
        {
            tokens.pop_back();
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Identifier)
            {
                throw std::runtime_error("Expected Identifier after variable declaration");
            }
            tokens.pop_back();
            auto name = std::get<std::string>(currToken.getValue());
            auto result = [&tokens, name = std::move(name)]
            {
                auto type = tokens.back().getTokenType();
                if (isAssignment(type))
                {
                    tokens.pop_back();
                    return Declaration(name, std::make_unique<Expression>(parseExpression(tokens)));
                }
                else
                {
                    return Declaration(name);
                }
            }();
            if (tokens.empty() || tokens.back().getTokenType() != TokenType::SemiColon)
            {
                throw std::runtime_error("Declaration not terminated with ;");
            }
            else
            {
                tokens.pop_back();
            }
            return std::make_unique<Declaration>(std::move(result));
        }
        else
        {
            return parseStatement(tokens);
        }
    }

    std::unique_ptr<OpenCL::Parser::Statement> parseStatement(Tokens& tokens)
    {
        auto result = [&tokens]() -> std::unique_ptr<Statement>
        {
            auto curentToken = tokens.back();
            switch (curentToken.getTokenType())
            {
            case TokenType::ReturnKeyword:
            {
                tokens.pop_back();
                return std::make_unique<ReturnStatement>(parseExpression(tokens));
            }
            case TokenType::IfKeyword:
            {
                tokens.pop_back();
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::OpenParanthese)
                {
                    throw std::runtime_error("Expected ( after if");
                }
                auto expression = parseExpression(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParanthese)
                {
                    throw std::runtime_error("Expected ) at the end of if statement");
                }
                auto statement = parseStatement(tokens);
                curentToken = tokens.back();
                if (!tokens.empty() && curentToken.getTokenType() == TokenType::ElseKeyword)
                {
                    tokens.pop_back();
                    return std::make_unique<IfStatement>(std::move(expression),
                                                         std::move(statement),
                                                         parseStatement(tokens));
                }
                else
                {
                    return std::make_unique<IfStatement>(std::move(expression), std::move(statement));
                }
            }
            case TokenType::OpenBrace:
            {
                tokens.pop_back();
                std::vector<std::unique_ptr<BlockItem>> blockItems;
                while (!tokens.empty() && tokens.back().getTokenType() != TokenType::CloseBrace)
                {
                    blockItems.push_back(parseBlockItem(tokens));
                }
                if (tokens.empty())
                {
                    throw std::runtime_error("Expected } to close Block");
                }
                tokens.pop_back();
                return std::make_unique<BlockStatement>(std::move(blockItems));
            }
            case TokenType::ForKeyword:
            {
                tokens.pop_back();
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::OpenParanthese)
                {
                    throw std::runtime_error("Expected ( after for");
                }
                auto blockitem = parseBlockItem(tokens);

                auto control = [&]() -> std::unique_ptr<Expression>
                {
                    if (tokens.back().getTokenType() == TokenType::SemiColon)
                    {
                        auto expression = parseExpression(tokens);
                        if (tokens.back().getTokenType() != TokenType::SemiColon)
                        {
                            throw std::runtime_error("Expected ; after control part of for loop header");
                        }
                        tokens.pop_back();
                        return std::make_unique<Expression>(std::move(expression));
                    }
                    else
                    {
                        if (tokens.back().getTokenType() != TokenType::SemiColon)
                        {
                            throw std::runtime_error("Expected ; after control part of for loop header");
                        }
                        tokens.pop_back();
                        return nullptr;
                    }
                }();

                auto post = [&]() -> std::unique_ptr<Expression>
                {
                    if (tokens.back().getTokenType() == TokenType::CloseParanthese)
                    {
                        auto expression = parseExpression(tokens);
                        if (tokens.back().getTokenType() != TokenType::CloseParanthese)
                        {
                            throw std::runtime_error("Expected ; after control part of for loop header");
                        }
                        tokens.pop_back();
                        return std::make_unique<Expression>(std::move(expression));
                    }
                    else
                    {
                        if (tokens.back().getTokenType() != TokenType::CloseParanthese)
                        {
                            throw std::runtime_error("Expected ; after control part of for loop header");
                        }
                        tokens.pop_back();
                        return nullptr;
                    }
                }();

                if (auto declaration = dynamic_cast<Declaration*>(blockitem.get());declaration)
                {
                    return std::make_unique<ForDeclarationStatement>(std::move(*declaration),
                                                                     std::move(control),
                                                                     std::move(post));
                }
                else if (auto
                        expressionStatement = dynamic_cast<ExpressionStatement*>(blockitem.get());expressionStatement)
                {
                    return std::make_unique<ForStatement>(expressionStatement->moveOptionalExpression(),
                                                          std::move(control),
                                                          std::move(post));
                }
                else
                {
                    throw std::runtime_error("Invalid expression or declaration for initial part of for loop header");
                }
            }
            case TokenType::WhileKeyword:
            {
                tokens.pop_back();
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::OpenParanthese)
                {
                    throw std::runtime_error("Expected ( after while");
                }
                auto expression = parseExpression(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParanthese)
                {
                    throw std::runtime_error("Expected ) after expression in while");
                }
                auto statement = parseStatement(tokens);
                return std::make_unique<HeadWhileStatement>(std::move(expression), std::move(statement));
            }
            case TokenType::DoKeyword:
            {
                tokens.pop_back();
                auto statement = parseStatement(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::WhileKeyword)
                {
                    throw std::runtime_error("Expected while after do");
                }
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::OpenParanthese)
                {
                    throw std::runtime_error("Expected ( after while");
                }
                auto expression = parseExpression(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParanthese)
                {
                    throw std::runtime_error("Expected ) after expression in while");
                }
                return std::make_unique<FootWhileStatement>(std::move(statement), std::move(expression));
            }
            case TokenType::BreakKeyword:return std::make_unique<BreakStatement>();
            case TokenType::ContinueKeyword:return std::make_unique<ContinueStatement>();
            default:
            {
                if (!tokens.empty() && tokens.back().getTokenType() != TokenType::SemiColon)
                {
                    return std::make_unique<ExpressionStatement>(std::make_unique<Expression>(parseExpression(tokens)));
                }
                else
                {
                    return std::make_unique<ExpressionStatement>();
                }
            }
            }
        }();

        if ((dynamic_cast<ExpressionStatement*>(result.get())
            || dynamic_cast<ReturnStatement*>(result.get())
            || dynamic_cast<FootWhileStatement*>(result.get())
            || dynamic_cast<BreakStatement*>(result.get())
            || dynamic_cast<ContinueStatement*>(result.get()))
            && (tokens.empty() || tokens.back().getTokenType() != TokenType::SemiColon))
        {
            throw std::runtime_error("Statement not terminated with ;");
        }
        else if (dynamic_cast<ExpressionStatement*>(result.get())
            || dynamic_cast<ReturnStatement*>(result.get())
            || dynamic_cast<FootWhileStatement*>(result.get())
            || dynamic_cast<BreakStatement*>(result.get())
            || dynamic_cast<ContinueStatement*>(result.get()))
        {
            tokens.pop_back();
        }
        return result;
    }

    OpenCL::Parser::Expression parseExpression(Tokens& tokens)
    {
        auto expression = parseNonCommaExpression(tokens);

        std::unique_ptr<NonCommaExpression> optional;
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::Comma)
        {
            tokens.pop_back();
            optional = parseNonCommaExpression(tokens);
        }
        return Expression(std::move(expression), std::move(optional));
    }

    std::unique_ptr<OpenCL::Parser::NonCommaExpression> parseNonCommaExpression(Tokens& tokens)
    {
        auto currentToken = tokens.back();
        if (tokens.size() > 2 && currentToken.getTokenType() == TokenType::Identifier
            && isAssignment(tokens[tokens.size() - 2].getTokenType()))
        {
            const auto& identifier = std::get<std::string>(currentToken.getValue());
            tokens.pop_back();
            tokens.pop_back();
            return std::make_unique<AssignmentExpression>(identifier,
                                                          parseNonCommaExpression(tokens),
                                                          [assignment = tokens[tokens.size() - 2].getTokenType()]
                                                          {
                                                              switch (assignment)
                                                              {
                                                              case TokenType::Assignment:return AssignmentExpression::AssignOperator::NoOperator;
                                                              case TokenType::PlusAssign:return AssignmentExpression::AssignOperator::PlusAssign;
                                                              case TokenType::MinusAssign:return AssignmentExpression::AssignOperator::MinusAssign;
                                                              case TokenType::DivideAssign:return AssignmentExpression::AssignOperator::DivideAssign;
                                                              case TokenType::MultiplyAssign:return AssignmentExpression::AssignOperator::MultiplyAssign;
                                                              case TokenType::ModuloAssign:return AssignmentExpression::AssignOperator::ModuloAssign;
                                                              case TokenType::ShiftLeftAssign:return AssignmentExpression::AssignOperator::LeftShiftAssign;
                                                              case TokenType::ShiftRightAssign:return AssignmentExpression::AssignOperator::RightShiftAssign;
                                                              case TokenType::BitAndAssign:return AssignmentExpression::AssignOperator::BitAndAssign;
                                                              case TokenType::BitOrAssign:return AssignmentExpression::AssignOperator::BitOrAssign;
                                                              case TokenType::BitXorAssign:return AssignmentExpression::AssignOperator::BitXorAssign;
                                                              default:return AssignmentExpression::AssignOperator::NoOperator;
                                                              }
                                                          }());
        }
        else
        {
            return std::make_unique<ConditionalExpression>(parseConditionalExpression(tokens));
        }
    }

    OpenCL::Parser::ConditionalExpression parseConditionalExpression(Tokens& tokens)
    {
        auto logicalOrExperssion = parseLogicalOrExpression(tokens);
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::QuestionMark)
        {
            tokens.pop_back();
            auto optionalExpression = parseExpression(tokens);
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Colon)
            {
                throw std::runtime_error("Expected : to match ?");
            }
            tokens.pop_back();
            auto optionalConditional = parseConditionalExpression(tokens);
            return ConditionalExpression(std::move(logicalOrExperssion),
                                         std::make_unique<Expression>(std::move(optionalExpression)),
                                         std::make_unique<ConditionalExpression>(std::move(optionalConditional)));
        }
        return ConditionalExpression(std::move(logicalOrExperssion));
    }

    OpenCL::Parser::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens)
    {
        auto logicalAnd = parseLogicalAndExpression(tokens);

        auto curentToken = tokens.back();
        while(curentToken.getTokenType() == TokenType::LogicOr)
        {
            tokens.pop_back();

        }
    }

    OpenCL::Parser::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens)
    {

    }

    OpenCL::Parser::EqualityExpression parseEqualityExpression(Tokens& tokens)
    {

    }

    OpenCL::Parser::RelationalExpression parseRelationalExpression(Tokens& tokens)
    {

    }

    OpenCL::Parser::ShiftExpression parseShiftExpression(Tokens& tokens)
    {

    }

    OpenCL::Parser::AdditiveExpression parseAdditiveExpression(Tokens& tokens)
    {

    }

    OpenCL::Parser::Term parseTerm(Tokens& tokens)
    {

    }

    std::unique_ptr<OpenCL::Parser::Factor> parseFactor(Tokens& tokens)
    {

    }
}

#include <utility>

#include "Parser.hpp"

#include <utility>

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

const std::vector<std::pair<std::shared_ptr<OpenCL::Parser::Type>,
                            std::string>>& OpenCL::Parser::Function::getArguments() const
{
    return m_arguments;
}

const OpenCL::Parser::BlockStatement* OpenCL::Parser::Function::getBlockStatement() const
{
    return m_block.get();
}

OpenCL::Parser::Function::Function(std::shared_ptr<Type> returnType,
                                   std::string name,
                                   std::vector<std::pair<std::shared_ptr<Type>,
                                                         std::string>> arguments,
                                   std::unique_ptr<BlockStatement>&& blockItems)
    : m_returnType(std::move(returnType)), m_name(std::move(
    name)), m_arguments(std::move(arguments)), m_block(std::move(blockItems))
{
    assert(m_returnType);
    assert(std::all_of(m_arguments.begin(), m_arguments.begin(), [](const auto& pair)
    { return static_cast<bool>(pair.first); }));
}

OpenCL::Parser::Declarations::Declarations(std::vector<std::tuple<std::shared_ptr<OpenCL::Parser::Type>,
                                                                        std::string,
                                                                        std::unique_ptr<OpenCL::Parser::Expression>>>&& declarations)
    : m_declarations(std::move(declarations))
{
    assert(std::all_of(m_declarations.begin(),m_declarations.end(),[](const auto& tuple){return std::get<0>(tuple).get();}));
}

const std::vector<std::tuple<std::shared_ptr<OpenCL::Parser::Type>,
                             std::string,
                             std::unique_ptr<OpenCL::Parser::Expression>>>& OpenCL::Parser::Declarations::getDeclarations() const
{
    return m_declarations;
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
                                                                 Declarations&& initial,
                                                                 std::unique_ptr<Expression>&& controlling,
                                                                 std::unique_ptr<Expression>&& post)
    : m_statement(std::move(statement)), m_initial(std::move(initial)), m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const OpenCL::Parser::Declarations& OpenCL::Parser::ForDeclarationStatement::getInitial() const
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

OpenCL::Parser::GlobalDeclaration::GlobalDeclaration(std::shared_ptr<Type> type,
                                                     std::string name,
                                                     std::unique_ptr<PrimaryExpressionConstant>&& value)
    : m_type(std::move(type)), m_name(std::move(name)), m_optionalValue(std::move(value))
{}

const std::shared_ptr<OpenCL::Parser::Type>& OpenCL::Parser::GlobalDeclaration::getType() const
{
    return m_type;
}

const std::string& OpenCL::Parser::GlobalDeclaration::getName() const
{
    return m_name;
}

const OpenCL::Parser::PrimaryExpressionConstant* OpenCL::Parser::GlobalDeclaration::getOptionalValue() const
{
    return m_optionalValue.get();
}

OpenCL::Parser::PrimitiveType::PrimitiveType(std::vector<OpenCL::Parser::PrimitiveType::Types>&& types)
: m_bitCount([&types]
             {
    auto copy = types;
    auto result = std::remove_if(copy.begin(),copy.end(),[](PrimitiveType::Types types)
    {
        return types == PrimitiveType::Types::Const || types == PrimitiveType::Types::Signed || types == PrimitiveType::Types::Unsigned;
    });
    if(result != copy.end())
    {
        copy.erase(result,copy.end());
    }
    if(copy.empty())
    {
        return std::any_of(types.begin(),types.end(),[](PrimitiveType::Types types)
        {
            return types == PrimitiveType::Types::Signed || types == PrimitiveType::Types::Unsigned;
        }) ? 32 : 0;
    }
    switch(copy[0])
    {
    case Types::Char:
        if(copy.size() != 1)
        {
            throw std::runtime_error("Cannot combine char with other");
        }
        return 8;
    case Types::Short:
        if(copy.size() != 1)
        {
            throw std::runtime_error("Cannot combine short with other");
        }
        return 16;
    case Types::Int:
        if(copy.size() != 1)
        {
            throw std::runtime_error("Cannot combine int with other");
        }
        return 32;
    case Types::Long:
    {
        if(copy.size() == 1)
        {
            return 32;
        }
        else if(copy.size() == 2 && copy[1] ==Types::Long)
        {
            return 64;
        }
        else
        {
            throw std::runtime_error("Cannot combine long with other");
        }
    }
    case Types::Float:
        if(copy.size() != 1)
        {
            throw std::runtime_error("Cannot combine float with other");
        }
        return 32;
    case Types::Double:
        if(copy.size() != 1)
        {
            throw std::runtime_error("Cannot combine double with other");
        }
        return 64;
    default:throw std::runtime_error("Invalid token");
    }
             }()),
             m_isConst(std::any_of(types.begin(),types.end(),[](PrimitiveType::Types types)
             {
                 return types == PrimitiveType::Types::Const;
             })),
             m_isFloatingPoint(std::any_of(types.begin(),types.end(),[](PrimitiveType::Types types)
             {
                 return types == PrimitiveType::Types::Float || types == PrimitiveType::Types::Double;
             })),
             m_isSigned(m_isFloatingPoint || (types.empty() ? false : types[0] != PrimitiveType::Types::Unsigned))
{}

OpenCL::Parser::PrimitiveType::PrimitiveType(std::uint64_t bitCount, bool isConst, bool isFloatingPoint, bool isSigned)
: m_bitCount(bitCount), m_isConst(isConst), m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned)
{

}

std::uint64_t OpenCL::Parser::PrimitiveType::getBitCount() const
{
    return m_bitCount;
}

bool OpenCL::Parser::PrimitiveType::isFloatingPoint() const
{
    return m_isFloatingPoint;
}

bool OpenCL::Parser::PrimitiveType::isSigned() const
{
    return m_isSigned;
}

bool OpenCL::Parser::PrimitiveType::isVoid() const
{
    return !m_bitCount;
}

std::unique_ptr<OpenCL::Parser::Type> OpenCL::Parser::PrimitiveType::clone() const
{
    return std::make_unique<PrimitiveType>(getBitCount(),isConst(),isFloatingPoint(),isSigned());
}

bool OpenCL::Parser::PrimitiveType::isConst() const
{
    return m_isConst;
}

const std::shared_ptr<OpenCL::Parser::Type>& OpenCL::Parser::Function::getReturnType() const
{
    return m_returnType;
}

OpenCL::Parser::PointerType::PointerType(std::unique_ptr<Type>&& type, bool isConst) : m_type(std::move(type)), m_isConst(isConst)
{}

const OpenCL::Parser::Type& OpenCL::Parser::PointerType::getType() const
{
    return *m_type;
}

std::unique_ptr<OpenCL::Parser::Type> OpenCL::Parser::PointerType::clone() const
{
    return std::make_unique<PointerType>(m_type->clone(),m_isConst);
}

bool OpenCL::Parser::PointerType::isConst() const
{
    return m_isConst;
}

OpenCL::Parser::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(std::string identifier) : m_identifier(std::move(
    identifier))
{}

const std::string& OpenCL::Parser::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Parser::PrimaryExpressionConstant::PrimaryExpressionConstant(OpenCL::Parser::PrimaryExpressionConstant::variant value)
    : m_value(std::move(value))
{}

const OpenCL::Parser::PrimaryExpressionConstant::variant& OpenCL::Parser::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

OpenCL::Parser::PrimaryExpressionParenthese::PrimaryExpressionParenthese(Expression&& expression)
    : m_expression(std::move(expression))
{}

const OpenCL::Parser::Expression& OpenCL::Parser::PrimaryExpressionParenthese::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(std::unique_ptr<PrimaryExpression>&& primaryExpression)
    : m_primaryExpression(std::move(primaryExpression))
{}

const OpenCL::Parser::PrimaryExpression& OpenCL::Parser::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return *m_primaryExpression;
}

OpenCL::Parser::PostFixExpressionSubscript::PostFixExpressionSubscript(std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                       Expression&& expression)
    : m_postFixExpression(std::move(postFixExpression)), m_expression(std::move(expression))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionSubscript::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const OpenCL::Parser::Expression& OpenCL::Parser::PostFixExpressionSubscript::getExpression() const
{
    return m_expression;
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

OpenCL::Parser::PostFixExpressionArrow::PostFixExpressionArrow(std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                           std::string identifier) : m_postFixExpression(
    std::move(postFixExpression)), m_identifier(std::move(identifier))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionArrow::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& OpenCL::Parser::PostFixExpressionArrow::getIdentifier() const
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
                                                                                std::shared_ptr<OpenCL::Parser::Type>>&& unaryOrType)
    : m_unaryOrType(std::move(unaryOrType))
{}

const std::variant<std::unique_ptr<OpenCL::Parser::UnaryExpression>,
                   std::shared_ptr<OpenCL::Parser::Type>>& OpenCL::Parser::UnaryExpressionSizeOf::getUnaryOrType() const
{
    return m_unaryOrType;
}

OpenCL::Parser::CastExpression::CastExpression(std::variant<std::unique_ptr<OpenCL::Parser::UnaryExpression>,
                                                                  std::pair<std::shared_ptr<OpenCL::Parser::Type>,
                                                                            std::unique_ptr<OpenCL::Parser::CastExpression>>>&& unaryOrCast)
    : m_unaryOrCast(std::move(unaryOrCast))
{}

const std::variant<std::unique_ptr<OpenCL::Parser::UnaryExpression>,
                   std::pair<std::shared_ptr<OpenCL::Parser::Type>,
                             std::unique_ptr<OpenCL::Parser::CastExpression>>>& OpenCL::Parser::CastExpression::getUnaryOrCast() const
{
    return m_unaryOrCast;
}

OpenCL::Parser::Term::Term(CastExpression&& castExpressions,
                           std::vector<std::pair<BinaryDotOperator, CastExpression>>&& optionalCastExpressions) : m_castExpression(
    std::move(castExpressions)), m_optionalCastExpressions(std::move(optionalCastExpressions))
{}

const OpenCL::Parser::CastExpression& OpenCL::Parser::Term::getCastExpression() const
{
    return m_castExpression;
}

const std::vector<std::pair<OpenCL::Parser::Term::BinaryDotOperator,
                            OpenCL::Parser::CastExpression>>& OpenCL::Parser::Term::getOptionalCastExpressions() const
{
    return m_optionalCastExpressions;
}


OpenCL::Parser::AssignmentExpression::AssignmentExpression(std::unique_ptr<OpenCL::Parser::UnaryExpression>&& unaryFactor,
                                                           OpenCL::Parser::AssignmentExpression::AssignOperator assignOperator,
                                                           std::unique_ptr<OpenCL::Parser::NonCommaExpression>&& nonCommaExpression)
    : m_unaryFactor(std::move(unaryFactor)), m_assignOperator(assignOperator), m_nonCommaExpression(std::move(nonCommaExpression))
{
    assert(m_unaryFactor);
}

const OpenCL::Parser::UnaryExpression& OpenCL::Parser::AssignmentExpression::getUnaryFactor() const
{
    return *m_unaryFactor;
}

const OpenCL::Parser::NonCommaExpression& OpenCL::Parser::AssignmentExpression::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

OpenCL::Parser::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                             std::vector<std::unique_ptr<NonCommaExpression>>&& optionalAssignmanetExpressions)
    : m_postFixExpression(std::move(postFixExpression)), m_optionalAssignmanetExpressions(std::move(optionalAssignmanetExpressions))
{
    assert(m_postFixExpression);
    assert(std::all_of(m_optionalAssignmanetExpressions.begin(),m_optionalAssignmanetExpressions.end(),[](const auto& ptr){return ptr.get();}));
}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionFunctionCall::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::vector<std::unique_ptr<OpenCL::Parser::NonCommaExpression>>& OpenCL::Parser::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmanetExpressions;
}

OpenCL::Parser::ArrayType::ArrayType(std::unique_ptr<Type>&& type,std::size_t size)
    : m_type(std::move(type)), m_size(size)
{
    assert(m_type);
    if(size <= 0)
    {
        throw std::runtime_error("Array must have a size greater than 0");
    }
}

const std::unique_ptr<OpenCL::Parser::Type>& OpenCL::Parser::ArrayType::getType() const
{
    return m_type;
}

size_t OpenCL::Parser::ArrayType::getSize() const
{
    return m_size;
}

std::unique_ptr<OpenCL::Parser::Type> OpenCL::Parser::ArrayType::clone() const
{
    return std::make_unique<ArrayType>(m_type->clone(),m_size);
}

OpenCL::Parser::PostFixExpressionIncrement::PostFixExpressionIncrement(std::unique_ptr<PostFixExpression>&& postFixExpression)
    : m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Parser::PostFixExpressionDecrement::PostFixExpressionDecrement(std::unique_ptr<PostFixExpression>&& postFixExpression)
    : m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Parser::PostFixExpression& OpenCL::Parser::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Parser::StructDeclaration::StructDeclaration(std::string name, std::vector<std::pair<std::shared_ptr<Type>, std::string>>&& types)
: m_name(std::move(name)), m_types(std::move(types))
{
    assert(!m_types.empty());
    assert(std::all_of(m_types.begin(),m_types.end(),[](const auto& pair){return pair.first.get();}));
}

const std::vector<std::pair<std::shared_ptr<OpenCL::Parser::Type>,std::string>>& OpenCL::Parser::StructDeclaration::getTypes() const
{
    return m_types;
}

const std::string& OpenCL::Parser::StructDeclaration::getName() const
{
    return m_name;
}

OpenCL::Parser::StructType::StructType(std::string name, bool isConst) : m_name(std::move(name)), m_isConst(isConst)
{}

const std::string& OpenCL::Parser::StructType::getName() const
{
    return m_name;
}

std::unique_ptr<OpenCL::Parser::Type> OpenCL::Parser::StructType::clone() const
{
    return std::make_unique<StructType>(m_name,m_isConst);
}

bool OpenCL::Parser::StructType::isConst() const
{
    return m_isConst;
}

OpenCL::Parser::SwitchStatement::SwitchStatement(OpenCL::Parser::Expression&& expression,
                                                 std::unique_ptr<Statement>&& statement)
    : m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Parser::Expression& OpenCL::Parser::SwitchStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Parser::Statement& OpenCL::Parser::SwitchStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Parser::DefaultStatement::DefaultStatement(std::unique_ptr<Statement>&& statement)
    : m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Parser::Statement& OpenCL::Parser::DefaultStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Parser::CaseStatement::CaseStatement(Expression&& constant,
                                             std::unique_ptr<Statement>&& statement) : m_constant(
    std::move(constant)), m_statement(std::move(statement))
{

}

const OpenCL::Parser::Expression& OpenCL::Parser::CaseStatement::getConstant() const
{
    return m_constant;
}

const OpenCL::Parser::Statement* OpenCL::Parser::CaseStatement::getStatement() const
{
    return m_statement.get();
}

bool OpenCL::Parser::Type::isSigned() const
{
    return false;
}

bool OpenCL::Parser::Type::isVoid() const
{
    return false;
}

bool OpenCL::Parser::Type::isConst() const
{
    return false;
}

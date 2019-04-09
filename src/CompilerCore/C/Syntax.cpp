#include <utility>

#include "Syntax.hpp"

#include <utility>
#include <algorithm>
#include <sstream>
#include <cassert>

OpenCL::Syntax::Program::Program(std::vector<Global>&& globals) noexcept : m_globals(std::move(
    globals))
{

}

const std::vector<OpenCL::Syntax::Global>& OpenCL::Syntax::Program::getGlobals() const
{
    return m_globals;
}

const std::string& OpenCL::Syntax::Function::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::shared_ptr<OpenCL::Syntax::IType>,
                            std::string>>& OpenCL::Syntax::Function::getArguments() const
{
    return m_arguments;
}

const OpenCL::Syntax::BlockStatement* OpenCL::Syntax::Function::getBlockStatement() const
{
    return m_block.get();
}

OpenCL::Syntax::Function::Function(std::uint64_t line,
                                   std::uint64_t column,
                                   std::shared_ptr<IType> returnType,
                                   std::string name,
                                   std::vector<std::pair<std::shared_ptr<IType>,
                                                         std::string>> arguments,
                                   std::uint64_t scopeLine,
                                   std::unique_ptr<BlockStatement>&& blockItems)
    : Node(line, column), m_returnType(std::move(returnType)), m_name(std::move(
    name)), m_arguments(std::move(arguments)), m_scopeLine(scopeLine), m_block(std::move(blockItems))
{
    assert(m_returnType);
    assert(std::all_of(m_arguments.begin(), m_arguments.begin(), [](const auto& pair)
    { return static_cast<bool>(pair.first); }));
}

OpenCL::Syntax::Declaration::Declarations(std::uint64_t line,
                                           std::uint64_t column,
                                           std::vector<std::tuple<std::shared_ptr<IType>,
                                                                  std::string,
                                                                  std::unique_ptr<Initializer>>>&& declarations)
    : Node(line, column), m_declarations(std::move(declarations))
{
    assert(std::all_of(m_declarations.begin(), m_declarations.end(), [](const auto& tuple)
    { return std::get<0>(tuple).get(); }));
}

const std::vector<std::tuple<std::shared_ptr<OpenCL::Syntax::IType>,
                             std::string,
                             std::unique_ptr<OpenCL::Syntax::Initializer>>>& OpenCL::Syntax::Declaration::getDeclarations() const
{
    return m_declarations;
}

std::vector<std::tuple<std::shared_ptr<OpenCL::Syntax::IType>,
                       std::string,
                       std::unique_ptr<OpenCL::Syntax::Initializer>>>& OpenCL::Syntax::Declaration::getDeclarations()
{
    return m_declarations;
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

OpenCL::Syntax::BlockStatement::BlockStatement(std::uint64_t line,
                                               std::uint64_t column,
                                               std::vector<BlockItem> blockItems)
    : Node(line, column), m_blockItems(std::move(blockItems))
{}

const std::vector<OpenCL::Syntax::BlockItem>& OpenCL::Syntax::BlockStatement::getBlockItems() const
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

OpenCL::Syntax::Expression::Expression(std::uint64_t line,
                                       std::uint64_t column,
                                       std::vector<AssignmentExpression> assignmentExpressions)
    : Node(line, column), m_assignmentExpressions(std::move(assignmentExpressions))
{
    assert(!m_assignmentExpressions.empty());
}



OpenCL::Syntax::AssignmentExpressionAssignment::AssignOperator OpenCL::Syntax::AssignmentExpressionAssignment::getAssignOperator() const
{
    return m_assignOperator;
}

OpenCL::Syntax::AdditiveExpression::AdditiveExpression(std::uint64_t line,
                                                       std::uint64_t column,
                                                       Term&& term,
                                                       std::vector<std::pair<BinaryDashOperator, Term>>&& optionalTerms)
    : Node(line, column), m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{}

const OpenCL::Syntax::Term& OpenCL::Syntax::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<std::pair<OpenCL::Syntax::AdditiveExpression::BinaryDashOperator, OpenCL::Syntax::Term>>&
OpenCL::Syntax::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

OpenCL::Syntax::ShiftExpression::ShiftExpression(std::uint64_t line,
                                                 std::uint64_t column,
                                                 AdditiveExpression&& additiveExpression,
                                                 std::vector<std::pair<ShiftOperator,
                                                                       AdditiveExpression>>&& optionalAdditiveExpressions)
    : Node(line, column), m_additiveExpression(std::move(additiveExpression)),
      m_optionalAdditiveExpressions(std::move(optionalAdditiveExpressions))
{}

const OpenCL::Syntax::AdditiveExpression& OpenCL::Syntax::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
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

std::string OpenCL::Syntax::PrimitiveType::name() const
{
    std::string prefix;
    if (isFloatingPoint())
    {
        if (m_bitCount == 32)
        {
            return prefix + "float";
        }
        else if (m_bitCount == 64)
        {
            return prefix + "double";
        }
        else
        {
            throw std::runtime_error("Invalid bitcount for floatingpoint type");
        }
    }
    else
    {
        if (!isSigned())
        {
            prefix += "unsigned ";
        }
        switch (getBitCount())
        {
        case 0:return "void";
        case 8:return prefix + "char";
        case 16:return prefix + "short";
        case 32:return prefix + "int";
        case 64:return prefix + "long long";
        default:throw std::runtime_error("Invalid bitcount for integer type");
        }
    }
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

OpenCL::Syntax::GlobalDeclaration::GlobalDeclaration(std::uint64_t line,
                                                     std::uint64_t column,
                                                     std::vector<std::tuple<std::shared_ptr<IType>,
                                                                            std::string,
                                                                            std::unique_ptr<Initializer>>>&& declarations)
    : Node(line, column), m_declarations(std::move(declarations))
{}

const std::vector<std::tuple<std::shared_ptr<OpenCL::Syntax::IType>,
                             std::string,
                             std::unique_ptr<OpenCL::Syntax::Initializer>>>& OpenCL::Syntax::GlobalDeclaration::getDeclarations() const
{
    return m_declarations;
}

OpenCL::Syntax::PrimitiveType::PrimitiveType(std::vector<OpenCL::Syntax::PrimitiveType::Types>&& types)
    : m_bitCount([&types]
                 {
                     auto copy = types;
                     auto result = std::remove_if(copy.begin(), copy.end(), [](PrimitiveType::Types types)
                     {
                         return types == PrimitiveType::Types::Const || types == PrimitiveType::Types::Signed
                             || types == PrimitiveType::Types::Unsigned;
                     });
                     if (result != copy.end())
                     {
                         copy.erase(result, copy.end());
                     }
                     if (copy.empty())
                     {
                         return std::any_of(types.begin(), types.end(), [](PrimitiveType::Types types)
                         {
                             return types == PrimitiveType::Types::Signed || types == PrimitiveType::Types::Unsigned;
                         }) ? 32 : 0;
                     }
                     switch (copy[0])
                     {
                     case Types::Char:
                         if (copy.size() != 1)
                         {
                             throw std::runtime_error("Cannot combine char with other");
                         }
                         return 8;
                     case Types::Short:
                         if (copy.size() != 1)
                         {
                             throw std::runtime_error("Cannot combine short with other");
                         }
                         return 16;
                     case Types::Int:
                         if (copy.size() != 1)
                         {
                             throw std::runtime_error("Cannot combine int with other");
                         }
                         return 32;
                     case Types::Long:
                     {
                         if (copy.size() == 1)
                         {
                             return 32;
                         }
                         else if (copy.size() == 2 && copy[1] == Types::Long)
                         {
                             return 64;
                         }
                         else
                         {
                             throw std::runtime_error("Cannot combine long with other");
                         }
                     }
                     case Types::Float:
                         if (copy.size() != 1)
                         {
                             throw std::runtime_error("Cannot combine float with other");
                         }
                         return 32;
                     case Types::Double:
                         if (copy.size() != 1)
                         {
                             throw std::runtime_error("Cannot combine double with other");
                         }
                         return 64;
                     default:throw std::runtime_error("Invalid token");
                     }
                 }()),
      m_isConst(std::any_of(types.begin(), types.end(), [](PrimitiveType::Types types)
      {
          return types == PrimitiveType::Types::Const;
      })),
      m_isFloatingPoint(std::any_of(types.begin(), types.end(), [](PrimitiveType::Types types)
      {
          return types == PrimitiveType::Types::Float || types == PrimitiveType::Types::Double;
      })),
      m_isSigned(m_isFloatingPoint || (types.empty() ? false : types[0] != PrimitiveType::Types::Unsigned))
{}

bool OpenCL::Syntax::StructOrUnionDeclaration::isUnion() const
{
    return m_isUnion;
}

OpenCL::Syntax::PrimitiveType::PrimitiveType(std::uint64_t bitCount, bool isConst, bool isFloatingPoint, bool isSigned)
    : m_bitCount(bitCount), m_isConst(isConst), m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned)
{

}

std::uint64_t OpenCL::Syntax::PrimitiveType::getBitCount() const
{
    return m_bitCount;
}

bool OpenCL::Syntax::PrimitiveType::isFloatingPoint() const
{
    return m_isFloatingPoint;
}

bool OpenCL::Syntax::PrimitiveType::isSigned() const
{
    return m_isSigned;
}

bool OpenCL::Syntax::PrimitiveType::isVoid() const
{
    return !m_bitCount;
}

std::unique_ptr<OpenCL::Syntax::IType> OpenCL::Syntax::PrimitiveType::clone() const
{
    return std::make_unique<PrimitiveType>(getBitCount(), isConst(), isFloatingPoint(), isSigned());
}

bool OpenCL::Syntax::PrimitiveType::isConst() const
{
    return m_isConst;
}

const std::shared_ptr<OpenCL::Syntax::IType>& OpenCL::Syntax::Function::getReturnType() const
{
    return m_returnType;
}

uint64_t OpenCL::Syntax::Function::getScopeLine() const
{
    return m_scopeLine;
}

OpenCL::Syntax::PointerType::PointerType(std::unique_ptr<IType>&& type, bool isConst)
    : m_type(std::move(type)), m_isConst(isConst)
{}

const OpenCL::Syntax::IType& OpenCL::Syntax::PointerType::getType() const
{
    return *m_type;
}

std::unique_ptr<OpenCL::Syntax::IType> OpenCL::Syntax::PointerType::clone() const
{
    return std::make_unique<PointerType>(m_type->clone(), m_isConst);
}

bool OpenCL::Syntax::PointerType::isConst() const
{
    return m_isConst;
}

std::string OpenCL::Syntax::PointerType::name() const
{
    return m_type->name() + (isConst() ? " * const" : " *");
}

OpenCL::Syntax::PrimaryExpressionIdentifier::PrimaryExpressionIdentifier(std::uint64_t line,
                                                                         std::uint64_t column,
                                                                         std::string identifier) :
    Node(line, column), m_identifier(std::move(identifier))
{}

const std::string& OpenCL::Syntax::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PrimaryExpressionConstant::PrimaryExpressionConstant(std::uint64_t line,
                                                                     std::uint64_t column,
                                                                     variant value)
    : Node(line, column), m_value(std::move(value))
{}

const OpenCL::Syntax::PrimaryExpressionConstant::variant& OpenCL::Syntax::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

OpenCL::Syntax::PrimaryExpressionParenthese::PrimaryExpressionParenthese(std::uint64_t line,
                                                                         std::uint64_t column,
                                                                         Expression&& expression)
    : Node(line, column), m_expression(std::move(expression))
{}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PrimaryExpressionParenthese::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(std::uint64_t line,
                                                                                       std::uint64_t column,
                                                                                       PrimaryExpression&& primaryExpression)
    : Node(line, column), m_primaryExpression(std::move(primaryExpression))
{}

const OpenCL::Syntax::PrimaryExpression& OpenCL::Syntax::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return m_primaryExpression;
}

OpenCL::Syntax::PostFixExpressionSubscript::PostFixExpressionSubscript(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                       Expression&& expression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression)),
      m_expression(std::move(expression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionSubscript::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PostFixExpressionSubscript::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PostFixExpressionDot::PostFixExpressionDot(std::uint64_t line,
                                                           std::uint64_t column,
                                                           std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                           std::string identifier)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression)),
      m_identifier(std::move(identifier))
{}

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
                                                               std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                               std::string identifier)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression)),
      m_identifier(std::move(identifier))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionArrow::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::string& OpenCL::Syntax::PostFixExpressionArrow::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::UnaryExpressionPostFixExpression::UnaryExpressionPostFixExpression(std::uint64_t line,
                                                                                   std::uint64_t column,
                                                                                   PostFixExpression&& postFixExpression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return m_postFixExpression;
}

OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(std::uint64_t line,
                                                                           std::uint64_t column,
                                                                           UnaryOperator anOperator,
                                                                           std::unique_ptr<UnaryExpression>&& unaryExpression)
    : Node(line, column), m_operator(anOperator), m_unaryExpression(std::move(unaryExpression))
{}

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
                                                             std::variant<std::unique_ptr<UnaryExpression>,
                                                                          std::shared_ptr<IType>>&& unaryOrType)
    : Node(line, column), m_unaryOrType(std::move(unaryOrType))
{}

const std::variant<std::unique_ptr<OpenCL::Syntax::UnaryExpression>,
                   std::shared_ptr<OpenCL::Syntax::IType>>& OpenCL::Syntax::UnaryExpressionSizeOf::getUnaryOrType() const
{
    return m_unaryOrType;
}

OpenCL::Syntax::CastExpression::CastExpression(std::uint64_t line,
                                               std::uint64_t column,
                                               std::variant<UnaryExpression,
                                                            std::pair<std::shared_ptr<IType>,
                                                                      std::unique_ptr<CastExpression>>>&& unaryOrCast)
    : Node(line, column), m_unaryOrCast(std::move(unaryOrCast))
{}

const std::variant<OpenCL::Syntax::UnaryExpression,
                   std::pair<std::shared_ptr<OpenCL::Syntax::IType>,
                             std::unique_ptr<OpenCL::Syntax::CastExpression>>>& OpenCL::Syntax::CastExpression::getUnaryOrCast() const
{
    return m_unaryOrCast;
}

OpenCL::Syntax::Term::Term(std::uint64_t line,
                           std::uint64_t column,
                           CastExpression&& castExpressions,
                           std::vector<std::pair<BinaryDotOperator, CastExpression>>&& optionalCastExpressions)
    : Node(line, column), m_castExpression(
    std::move(castExpressions)), m_optionalCastExpressions(std::move(optionalCastExpressions))
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

OpenCL::Syntax::AssignmentExpressionAssignment::AssignmentExpressionAssignment(std::uint64_t line,
                                                           std::uint64_t column,
                                                           UnaryExpression&& unaryFactor,
                                                           AssignOperator assignOperator,
                                                           std::unique_ptr<AssignmentExpression>&& nonCommaExpression)
    : Node(line, column), m_unaryFactor(std::move(unaryFactor)), m_assignOperator(assignOperator),
      m_nonCommaExpression(std::move(nonCommaExpression))
{

}

const OpenCL::Syntax::UnaryExpression& OpenCL::Syntax::AssignmentExpressionAssignment::getUnaryFactor() const
{
    return m_unaryFactor;
}

const OpenCL::Syntax::AssignmentExpression& OpenCL::Syntax::AssignmentExpressionAssignment::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

OpenCL::Syntax::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(std::uint64_t line,
                                                                             std::uint64_t column,
                                                                             std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                             std::vector<std::unique_ptr<
                                                                                 AssignmentExpression>>&& optionalAssignmanetExpressions)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression)),
      m_optionalAssignmanetExpressions(std::move(optionalAssignmanetExpressions))
{
    assert(m_postFixExpression);
    assert(std::all_of(m_optionalAssignmanetExpressions.begin(),
                       m_optionalAssignmanetExpressions.end(),
                       [](const auto& ptr)
                       { return ptr.get(); }));
}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionFunctionCall::getPostFixExpression() const
{
    return *m_postFixExpression;
}

const std::vector<std::unique_ptr<OpenCL::Syntax::AssignmentExpression>>& OpenCL::Syntax::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmanetExpressions;
}

OpenCL::Syntax::ArrayType::ArrayType(std::unique_ptr<IType>&& type, std::size_t size)
    : m_type(std::move(type)), m_size(size)
{
    assert(m_type);
}

const std::unique_ptr<OpenCL::Syntax::IType>& OpenCL::Syntax::ArrayType::getType() const
{
    return m_type;
}

size_t OpenCL::Syntax::ArrayType::getSize() const
{
    return m_size;
}

std::unique_ptr<OpenCL::Syntax::IType> OpenCL::Syntax::ArrayType::clone() const
{
    return std::make_unique<ArrayType>(m_type->clone(), m_size);
}

std::string OpenCL::Syntax::ArrayType::name() const
{
    std::ostringstream ss;
    ss << getSize();
    return getType()->name() + " [" + ss.str() + "]";
}

void OpenCL::Syntax::ArrayType::setSize(size_t size)
{
    m_size = size;
}

OpenCL::Syntax::PostFixExpressionIncrement::PostFixExpressionIncrement(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<PostFixExpression>&& postFixExpression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::PostFixExpressionDecrement::PostFixExpressionDecrement(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<PostFixExpression>&& postFixExpression)
    : Node(line, column), m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::StructOrUnionDeclaration::StructOrUnionDeclaration(std::uint64_t line,
                                                                   std::uint64_t column,
                                                                   bool isUnion,
                                                                   std::string name,
                                                                   std::vector<std::pair<std::shared_ptr<IType>,
                                                                                         std::string>>&& types)
    : Node(line, column), m_isUnion(isUnion), m_name(std::move(name)), m_types(std::move(types))
{
    if (m_types.empty())
    {
        throw std::runtime_error("Struct and unions needs to have atleast one field");
    }
    assert(std::all_of(m_types.begin(), m_types.end(), [](const auto& pair)
    { return pair.first.get(); }));
}

const std::vector<std::pair<std::shared_ptr<OpenCL::Syntax::IType>,
                            std::string>>& OpenCL::Syntax::StructOrUnionDeclaration::getTypes() const
{
    return m_types;
}

const std::string& OpenCL::Syntax::StructOrUnionDeclaration::getName() const
{
    return m_name;
}

OpenCL::Syntax::StructType::StructType(std::string name, bool isConst) : m_name(std::move(name)), m_isConst(isConst)
{}

const std::string& OpenCL::Syntax::StructType::getName() const
{
    return m_name;
}

std::unique_ptr<OpenCL::Syntax::IType> OpenCL::Syntax::StructType::clone() const
{
    return std::make_unique<StructType>(m_name, m_isConst);
}

bool OpenCL::Syntax::StructType::isConst() const
{
    return m_isConst;
}

std::string OpenCL::Syntax::StructType::name() const
{
    return "struct " + getName();
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
                                             constantVariant&& constant,
                                             std::unique_ptr<Statement>&& statement)
    : Node(line, column), m_constant(constant), m_statement(std::move(statement))
{

}

const OpenCL::Syntax::Statement* OpenCL::Syntax::CaseStatement::getStatement() const
{
    return m_statement.get();
}

const OpenCL::Syntax::Node<OpenCL::Syntax::CaseStatement>::constantVariant& OpenCL::Syntax::CaseStatement::getConstant() const
{
    return m_constant;
}

OpenCL::Syntax::UnionType::UnionType(std::string name, bool isConst) : m_name(std::move(name)), m_isConst(isConst)
{}

const std::string& OpenCL::Syntax::UnionType::getName() const
{
    return m_name;
}

bool OpenCL::Syntax::UnionType::isConst() const
{
    return m_isConst;
}

std::unique_ptr<OpenCL::Syntax::IType> OpenCL::Syntax::UnionType::clone() const
{
    return std::make_unique<UnionType>(getName(), isConst());
}

std::string OpenCL::Syntax::UnionType::name() const
{
    return "union " + getName();
}

OpenCL::Syntax::PostFixExpressionTypeInitializer::PostFixExpressionTypeInitializer(std::uint64_t line,
                                                                                   std::uint64_t column,
                                                                                   std::shared_ptr<IType> type,
                                                                                   InitializerList&& initializerList)
    : Node(line, column), m_type(std::move(type)), m_initializerList(std::make_unique<InitializerList>(std::move(initializerList)))
{}

const std::shared_ptr<OpenCL::Syntax::IType>& OpenCL::Syntax::PostFixExpressionTypeInitializer::getType() const
{
    return m_type;
}

const OpenCL::Syntax::InitializerList& OpenCL::Syntax::PostFixExpressionTypeInitializer::getInitializerList() const
{
    return *m_initializerList;
}

OpenCL::Syntax::TypedefDeclaration::TypedefDeclaration(std::uint64_t line,
                                                       std::uint64_t column,
                                                       std::unique_ptr<StructOrUnionDeclaration>&& optionalStructOrUnion)
    : Node(line, column), m_optionalStructOrUnion(std::move(optionalStructOrUnion))
{}

const std::unique_ptr<OpenCL::Syntax::StructOrUnionDeclaration>& OpenCL::Syntax::TypedefDeclaration::getOptionalStructOrUnion() const
{
    return m_optionalStructOrUnion;
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

OpenCL::Syntax::EnumType::EnumType(std::string name, bool isConst) : m_name(std::move(name)), m_isConst(isConst)
{}

const std::string& OpenCL::Syntax::EnumType::getName() const
{
    return m_name;
}

std::unique_ptr<OpenCL::Syntax::IType> OpenCL::Syntax::EnumType::clone() const
{
    return std::make_unique<EnumType>(getName(), isConst());
}

std::string OpenCL::Syntax::EnumType::name() const
{
    return "enum " + getName();
}

bool OpenCL::Syntax::EnumType::isConst() const
{
    return m_isConst;
}

OpenCL::Syntax::PrimaryExpression::PrimaryExpression(std::uint64_t line,
                                                     std::uint64_t column,
                                                     OpenCL::Syntax::PrimaryExpression::variant&& variant) : Node(
    line,
    column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::PrimaryExpression::variant& OpenCL::Syntax::PrimaryExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::PostFixExpression::PostFixExpression(std::uint64_t line,
                                                     std::uint64_t column,
                                                     OpenCL::Syntax::PostFixExpression::variant&& variant) : Node(
    line,
    column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::PostFixExpression::variant& OpenCL::Syntax::PostFixExpression::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::UnaryExpression::UnaryExpression(std::uint64_t line,
                                                 std::uint64_t column,
                                                 OpenCL::Syntax::UnaryExpression::variant&& variant) : Node(line,
                                                                                                            column),
                                                                                                       m_variant(
                                                                                                           std::move(
                                                                                                               variant))
{}

const OpenCL::Syntax::UnaryExpression::variant& OpenCL::Syntax::UnaryExpression::getVariant() const
{
    return m_variant;
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

OpenCL::Syntax::BlockItem::BlockItem(std::uint64_t line,
                                     std::uint64_t column,
                                     OpenCL::Syntax::BlockItem::variant&& variant)
    : Node(line, column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::BlockItem::variant& OpenCL::Syntax::BlockItem::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::BlockItem::variant& OpenCL::Syntax::BlockItem::getVariant()
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

OpenCL::Syntax::Global::Global(std::uint64_t line, std::uint64_t column, OpenCL::Syntax::Global::variant&& variant)
    : Node(
    line,
    column), m_variant(std::move(variant))
{}

const OpenCL::Syntax::Global::variant& OpenCL::Syntax::Global::getVariant() const
{
    return m_variant;
}

OpenCL::Syntax::BreakStatement::BreakStatement(std::uint64_t line, std::uint64_t column) : Node(line, column)
{

}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PrimaryExpression& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::UnaryExpression& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Statement& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Global& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::PostFixExpression& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::AssignmentExpression& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::Initializer& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

void OpenCL::Syntax::INodeVisitor::visit(const OpenCL::Syntax::BlockItem& node)
{
    std::visit([this](auto&& value)
               {
                   return value.accept(*this);
               }, node.getVariant());
}

const std::vector<OpenCL::Syntax::AssignmentExpression>& OpenCL::Syntax::Expression::getAssignmentExpressions() const
{
    return m_assignmentExpressions;
}

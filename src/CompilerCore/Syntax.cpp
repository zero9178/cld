#include <utility>

#include "Syntax.hpp"

#include <utility>
#include <algorithm>
#include <sstream>

namespace
{
    template <class, class, class = void>
    struct hasMultiply : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasMultiply<T1, T2, std::void_t<decltype(std::declval<T1>() * std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasDivide : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasDivide<T1, T2, std::void_t<decltype(std::declval<T1>() / std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasModulo : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasModulo<T1, T2, std::void_t<decltype(std::declval<T1>() % std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasPlus : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasPlus<T1, T2, std::void_t<decltype(std::declval<T1>() + std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasMinus : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasMinus<T1, T2, std::void_t<decltype(std::declval<T1>() - std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasRShift : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasRShift<T1, T2, std::void_t<decltype(std::declval<T1>() >> std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLShift : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLShift<T1, T2, std::void_t<decltype(std::declval<T1>() << std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLT : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLT<T1, T2, std::void_t<decltype(std::declval<T1>() < std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLE : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLE<T1, T2, std::void_t<decltype(std::declval<T1>() <= std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasGT : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasGT<T1, T2, std::void_t<decltype(std::declval<T1>() > std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasGE : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasGE<T1, T2, std::void_t<decltype(std::declval<T1>() >= std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasEQ : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasEQ<T1, T2, std::void_t<decltype(std::declval<T1>() == std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasNE : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasNE<T1, T2, std::void_t<decltype(std::declval<T1>() != std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasBitAnd : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasBitAnd<T1, T2, std::void_t<decltype(std::declval<T1>() & std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasBitXor : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasBitXor<T1, T2, std::void_t<decltype(std::declval<T1>() ^ std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasBitOr : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasBitOr<T1, T2, std::void_t<decltype(std::declval<T1>() | std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLogicAnd : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLogicAnd<T1, T2, std::void_t<decltype(std::declval<T1>() && std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class, class = void>
    struct hasLogicOr : std::false_type
    {
    };

    template <class T1, class T2>
    struct hasLogicOr<T1, T2, std::void_t<decltype(std::declval<T1>() || std::declval<T2>())>> : std::true_type
    {
    };

    template <class, class = void>
    struct hasLogicNegate : std::false_type
    {
    };

    template <class T>
    struct hasLogicNegate<T, std::void_t<decltype(!std::declval<T>())>> : std::true_type
    {
    };

    template <class, class = void>
    struct hasBitNegate : std::false_type
    {
    };

    template <class T>
    struct hasBitNegate<T, std::void_t<decltype(~std::declval<T>())>> : std::true_type
    {
    };

    template <class, class = void>
    struct hasNegate : std::false_type
    {
    };

    template <class T>
    struct hasNegate<T, std::void_t<decltype(-std::declval<T>())>> : std::true_type
    {
    };
}

OpenCL::Syntax::Program::Program(std::vector<std::unique_ptr<Global>>&& globals) noexcept : m_globals(std::move(
    globals))
{

}

const std::vector<std::unique_ptr<OpenCL::Syntax::Global>>& OpenCL::Syntax::Program::getGlobals() const
{
    return m_globals;
}

const std::string& OpenCL::Syntax::Function::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::shared_ptr<OpenCL::Syntax::Type>,
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
                                   std::shared_ptr<Type> returnType,
                                   std::string name,
                                   std::vector<std::pair<std::shared_ptr<Type>,
                                                         std::string>> arguments,
                                   std::uint64_t scopeLine,
                                   std::unique_ptr<BlockStatement>&& blockItems)
    : Global(line, column), m_returnType(std::move(returnType)), m_name(std::move(
    name)), m_arguments(std::move(arguments)), m_scopeLine(scopeLine), m_block(std::move(blockItems))
{
    assert(m_returnType);
    assert(std::all_of(m_arguments.begin(), m_arguments.begin(), [](const auto& pair)
    { return static_cast<bool>(pair.first); }));
}

OpenCL::Syntax::Declarations::Declarations(std::uint64_t line,
                                           std::uint64_t column,
                                           std::vector<std::tuple<std::shared_ptr<Type>,
                                                                  std::string,
                                                                  std::unique_ptr<InitializerList>>>&& declarations)
    : BlockItem(line, column), m_declarations(std::move(declarations))
{
    assert(std::all_of(m_declarations.begin(), m_declarations.end(), [](const auto& tuple)
    { return std::get<0>(tuple).get(); }));
}

const std::vector<std::tuple<std::shared_ptr<OpenCL::Syntax::Type>,
                             std::string,
                             std::unique_ptr<OpenCL::Syntax::InitializerList>>>& OpenCL::Syntax::Declarations::getDeclarations() const
{
    return m_declarations;
}

std::vector<std::tuple<std::shared_ptr<OpenCL::Syntax::Type>,
                             std::string,
                             std::unique_ptr<OpenCL::Syntax::InitializerList>>>& OpenCL::Syntax::Declarations::getDeclarations()
{
    return m_declarations;
}

const OpenCL::Syntax::Expression& OpenCL::Syntax::ReturnStatement::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::ReturnStatement::ReturnStatement(std::uint64_t line, std::uint64_t column, Expression&& expression)
    : Statement(line, column), m_expression(std::move(expression))
{}

OpenCL::Syntax::ExpressionStatement::ExpressionStatement(std::uint64_t line,
                                                         std::uint64_t column,
                                                         std::unique_ptr<Expression>&& optionalExpression)
    : Statement(line, column), m_optionalExpression(std::move(optionalExpression))
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
    : Statement(line, column), m_expression(std::move(expression)), m_branch(std::move(branch)),
      m_elseBranch(std::move(elseBranch))
{
    assert(m_branch);
}

OpenCL::Syntax::ContinueStatement::ContinueStatement(std::uint64_t line, std::uint64_t column) : Statement(line, column)
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
                                               std::vector<std::unique_ptr<BlockItem>> blockItems)
    : Statement(line, column), m_blockItems(std::move(blockItems))
{
    assert(std::all_of(m_blockItems.begin(),
                       m_blockItems.end(),
                       [](const std::unique_ptr<OpenCL::Syntax::BlockItem>& ptr)
                       { return ptr.get(); }));
}

const std::vector<std::unique_ptr<OpenCL::Syntax::BlockItem>>& OpenCL::Syntax::BlockStatement::getBlockItems() const
{
    return m_blockItems;
}

OpenCL::Syntax::ForStatement::ForStatement(std::uint64_t line,
                                           std::uint64_t column,
                                           std::unique_ptr<Statement>&& statement,
                                           std::unique_ptr<Expression>&& initial,
                                           std::unique_ptr<Expression>&& controlling,
                                           std::unique_ptr<Expression>&& post)
    : Statement(line, column), m_statement(std::move(statement)), m_initial(std::move(initial)),
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
                                                                 Declarations&& initial,
                                                                 std::unique_ptr<Expression>&& controlling,
                                                                 std::unique_ptr<Expression>&& post)
    : Statement(line, column), m_statement(std::move(statement)), m_initial(std::move(initial)),
      m_controlling(std::move(controlling)),
      m_post(std::move(post))
{
    assert(m_statement);
}

const OpenCL::Syntax::Declarations& OpenCL::Syntax::ForDeclarationStatement::getInitial() const
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
    : Statement(line, column), m_expression(std::move(expression)), m_statement(std::move(statement))
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
    : Statement(line, column), m_statement(
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
                                       std::unique_ptr<NonCommaExpression>&& nonCommaExpression,
                                       std::unique_ptr<NonCommaExpression>&& optionalNonCommaExpression)
    : Node(line, column), m_nonCommaExpression(std::move(nonCommaExpression)),
      m_optionalNonCommaExpression(std::move(optionalNonCommaExpression))
{
    assert(m_nonCommaExpression);
}

const OpenCL::Syntax::NonCommaExpression& OpenCL::Syntax::Expression::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

const OpenCL::Syntax::NonCommaExpression* OpenCL::Syntax::Expression::getOptionalNonCommaExpression() const
{
    return m_optionalNonCommaExpression.get();
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::Expression::solveConstantExpression() const
{
    if (getOptionalNonCommaExpression())
    {
        return getOptionalNonCommaExpression()->solveConstantExpression();
    }
    return getNonCommaExpression().solveConstantExpression();
}

OpenCL::Syntax::AssignmentExpression::AssignOperator OpenCL::Syntax::AssignmentExpression::getAssignOperator() const
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
    : NonCommaExpression(line, column), m_logicalOrExpression(std::move(logicalOrExpression)),
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
    std::string prefix = isConst() ? "const " : "";
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
                                                     std::vector<std::tuple<std::shared_ptr<Type>,
                                                                            std::string,
                                                                            std::unique_ptr<InitializerList>>>&& declarations)
    : Global(line, column), m_declarations(std::move(declarations))
{}

const std::vector<std::tuple<std::shared_ptr<OpenCL::Syntax::Type>,
                             std::string,
                             std::unique_ptr<OpenCL::Syntax::InitializerList>>>& OpenCL::Syntax::GlobalDeclaration::getDeclarations() const
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

std::unique_ptr<OpenCL::Syntax::Type> OpenCL::Syntax::PrimitiveType::clone() const
{
    return std::make_unique<PrimitiveType>(getBitCount(), isConst(), isFloatingPoint(), isSigned());
}

bool OpenCL::Syntax::PrimitiveType::isConst() const
{
    return m_isConst;
}

const std::shared_ptr<OpenCL::Syntax::Type>& OpenCL::Syntax::Function::getReturnType() const
{
    return m_returnType;
}

uint64_t OpenCL::Syntax::Function::getScopeLine() const
{
    return m_scopeLine;
}

OpenCL::Syntax::PointerType::PointerType(std::unique_ptr<Type>&& type, bool isConst)
    : m_type(std::move(type)), m_isConst(isConst)
{}

const OpenCL::Syntax::Type& OpenCL::Syntax::PointerType::getType() const
{
    return *m_type;
}

std::unique_ptr<OpenCL::Syntax::Type> OpenCL::Syntax::PointerType::clone() const
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
    PrimaryExpression(line, column), m_identifier(std::move(identifier))
{}

const std::string& OpenCL::Syntax::PrimaryExpressionIdentifier::getIdentifier() const
{
    return m_identifier;
}

OpenCL::Syntax::PrimaryExpressionConstant::PrimaryExpressionConstant(std::uint64_t line,
                                                                     std::uint64_t column,
                                                                     variant value)
    : PrimaryExpression(line, column), m_value(std::move(value))
{}

const OpenCL::Syntax::PrimaryExpressionConstant::variant& OpenCL::Syntax::PrimaryExpressionConstant::getValue() const
{
    return m_value;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::PrimaryExpressionConstant::solveConstantExpression() const
{
    return std::visit([this](auto&& value) -> OpenCL::Syntax::Node::constantVariant
                      {
                          using T = std::decay_t<decltype(value)>;
                          if constexpr(!std::is_same_v<T,std::string>)
                          {
                              return value;
                          }
                          else
                          {
                              return Node::solveConstantExpression();
                          }
                      }, getValue());
}

OpenCL::Syntax::PrimaryExpressionParenthese::PrimaryExpressionParenthese(std::uint64_t line,
                                                                         std::uint64_t column,
                                                                         Expression&& expression)
    : PrimaryExpression(line, column), m_expression(std::move(expression))
{}

const OpenCL::Syntax::Expression& OpenCL::Syntax::PrimaryExpressionParenthese::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::PostFixExpressionPrimaryExpression::PostFixExpressionPrimaryExpression(std::uint64_t line,
                                                                                       std::uint64_t column,
                                                                                       std::unique_ptr<PrimaryExpression>&& primaryExpression)
    : PostFixExpression(line, column), m_primaryExpression(std::move(primaryExpression))
{}

const OpenCL::Syntax::PrimaryExpression& OpenCL::Syntax::PostFixExpressionPrimaryExpression::getPrimaryExpression() const
{
    return *m_primaryExpression;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::PostFixExpressionPrimaryExpression::solveConstantExpression() const
{
    return getPrimaryExpression().solveConstantExpression();
}

OpenCL::Syntax::PostFixExpressionSubscript::PostFixExpressionSubscript(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                       Expression&& expression)
    : PostFixExpression(line, column), m_postFixExpression(std::move(postFixExpression)),
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
    : PostFixExpression(line, column), m_postFixExpression(std::move(postFixExpression)),
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
    : PostFixExpression(line, column), m_postFixExpression(std::move(postFixExpression)),
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
                                                                                   std::unique_ptr<PostFixExpression>&& postFixExpression)
    : UnaryExpression(line, column), m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::UnaryExpressionPostFixExpression::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::UnaryExpressionPostFixExpression::solveConstantExpression() const
{
    return getPostFixExpression().solveConstantExpression();
}

OpenCL::Syntax::UnaryExpressionUnaryOperator::UnaryExpressionUnaryOperator(std::uint64_t line,
                                                                           std::uint64_t column,
                                                                           UnaryOperator anOperator,
                                                                           std::unique_ptr<UnaryExpression>&& unaryExpression)
    : UnaryExpression(line, column), m_operator(anOperator), m_unaryExpression(std::move(unaryExpression))
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
                                                                          std::shared_ptr<Type>>&& unaryOrType)
    : UnaryExpression(line, column), m_unaryOrType(std::move(unaryOrType))
{}

const std::variant<std::unique_ptr<OpenCL::Syntax::UnaryExpression>,
                   std::shared_ptr<OpenCL::Syntax::Type>>& OpenCL::Syntax::UnaryExpressionSizeOf::getUnaryOrType() const
{
    return m_unaryOrType;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::UnaryExpressionSizeOf::solveConstantExpression() const
{
    throw std::runtime_error("Not implemented yet");
}

OpenCL::Syntax::CastExpression::CastExpression(std::uint64_t line,
                                               std::uint64_t column,
                                               std::variant<std::unique_ptr<UnaryExpression>,
                                                            std::pair<std::shared_ptr<Type>,
                                                                      std::unique_ptr<CastExpression>>>&& unaryOrCast)
    : Node(line, column), m_unaryOrCast(std::move(unaryOrCast))
{}

const std::variant<std::unique_ptr<OpenCL::Syntax::UnaryExpression>,
                   std::pair<std::shared_ptr<OpenCL::Syntax::Type>,
                             std::unique_ptr<OpenCL::Syntax::CastExpression>>>& OpenCL::Syntax::CastExpression::getUnaryOrCast() const
{
    return m_unaryOrCast;
}

namespace
{
    template<class T>
    OpenCL::Syntax::Node::constantVariant castVariant(OpenCL::Syntax::Node::constantVariant&& variant)
    {
        return std::visit([](auto&& value)->OpenCL::Syntax::Node::constantVariant
                          {
            using U = std::decay_t<decltype(value)>;
            if constexpr(std::is_convertible_v<U,T>)
            {
                return static_cast<T>(value);
            }
            else
            {
                throw std::runtime_error("Invalid constant cast");
            }
            return {};
                          },variant);
    }
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::CastExpression::solveConstantExpression() const
{
    return std::visit([](auto&& value)->constantVariant
                      {
        using T = std::decay_t<decltype(value)>;
        if constexpr(std::is_same_v<T,std::unique_ptr<OpenCL::Syntax::UnaryExpression>>)
        {
            return value->solveConstantExpression();
        }
        else
        {
            auto old = value.second->solveConstantExpression();
            if(std::shared_ptr<PrimitiveType> primitive = std::dynamic_pointer_cast<PrimitiveType>(value.first);primitive)
            {
                if(primitive->isFloatingPoint())
                {
                    if(primitive->getBitCount() == 32)
                    {
                        return castVariant<float>(std::move(old));
                    }
                    else if(primitive->getBitCount() == 64)
                    {
                        return castVariant<double>(std::move(old));
                    }
                    else
                    {
                        throw std::runtime_error("Invalid bitcount for floating point type");
                    }
                }
                else
                {
                    switch(primitive->getBitCount())
                    {
                    case 8:
                        if(primitive->isSigned())
                        {
                            return castVariant<std::int8_t>(std::move(old));
                        }
                        else
                        {
                            return castVariant<std::uint8_t>(std::move(old));
                        }
                    case 16:
                        if(primitive->isSigned())
                        {
                            return castVariant<std::int16_t>(std::move(old));
                        }
                        else
                        {
                            return castVariant<std::uint16_t>(std::move(old));
                        }
                    case 32:
                        if(primitive->isSigned())
                        {
                            return castVariant<std::int32_t>(std::move(old));
                        }
                        else
                        {
                            return castVariant<std::uint32_t>(std::move(old));
                        }
                    case 64:
                        if(primitive->isSigned())
                        {
                            return castVariant<std::int64_t>(std::move(old));
                        }
                        else
                        {
                            return castVariant<std::uint64_t>(std::move(old));
                        }
                    default:throw std::runtime_error("Invalid bitcount for integer type");
                    }
                }
            }
            else
            {
                throw std::runtime_error("Not implemented yet");
            }
        }
        },getUnaryOrCast());
}

OpenCL::Syntax::Term::Term(std::uint64_t line,
                           std::uint64_t column,
                           CastExpression&& castExpressions,
                           std::vector<std::pair<BinaryDotOperator, CastExpression>>&& optionalCastExpressions)
    : Node(line, column), m_castExpression(
    std::move(castExpressions)), m_optionalCastExpressions(std::move(optionalCastExpressions))
{}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::Term::solveConstantExpression() const
{
    auto currentValue = getCastExpression().solveConstantExpression();
    for (auto&[op, exp] : getOptionalCastExpressions())
    {
        switch (op)
        {
        case BinaryDotOperator::BinaryMultiply:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasMultiply<T1, T2>{})
                                                                {
                                                                    return lhs * rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case BinaryDotOperator::BinaryDivide:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasDivide<T1, T2>{})
                                                                {
                                                                    return lhs / rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case BinaryDotOperator::BinaryRemainder:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasModulo<T1, T2>{})
                                                                {
                                                                    return lhs % rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::AdditiveExpression::solveConstantExpression() const
{
    auto currentValue = getTerm().solveConstantExpression();
    for (auto&[op, exp] : getOptionalTerms())
    {
        switch (op)
        {
        case BinaryDashOperator::BinaryPlus:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasPlus<T1, T2>{})
                                                                {
                                                                    return lhs + rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case BinaryDashOperator::BinaryMinus:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(!std::is_void_v<std::remove_pointer_t<T1>>
                                                                    || !std::is_void_v<std::remove_pointer_t<T2>>)
                                                                {
                                                                    if constexpr(hasMinus<T1, T2>{})
                                                                    {
                                                                        return lhs - rhs;
                                                                    }
                                                                    else
                                                                    {
                                                                        throw std::runtime_error(
                                                                            "Can't apply plus to operands in constant expression");
                                                                        return {};
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::ShiftExpression::solveConstantExpression() const
{
    auto currentValue = getAdditiveExpression().solveConstantExpression();
    for (auto&[op, exp] : getOptionalAdditiveExpressions())
    {
        switch (op)
        {
        case ShiftOperator::Left:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasLShift<T1, T2>{})
                                                                {
                                                                    return lhs << rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case ShiftOperator::Right:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasRShift<T1, T2>{})
                                                                {
                                                                    return lhs >> rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::BitAndExpression::solveConstantExpression() const
{
    auto currentValue = getEqualityExpression().solveConstantExpression();
    for (auto& exp : getOptionalEqualityExpressions())
    {
        currentValue = std::visit([&exp](auto&& lhs) -> Node::constantVariant
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasBitAnd<T1, T2>{})
                                                            {
                                                                return lhs & rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, exp.solveConstantExpression());
                                  }, currentValue);
    }
    return currentValue;
}


OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::BitXorExpression::solveConstantExpression() const
{
    auto currentValue = getBitAndExpression().solveConstantExpression();
    for (auto& exp : getOptionalBitAndExpressions())
    {
        currentValue = std::visit([&exp](auto&& lhs) -> Node::constantVariant
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasBitXor<T1, T2>{})
                                                            {
                                                                return lhs ^ rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, exp.solveConstantExpression());
                                  }, currentValue);
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::BitOrExpression::solveConstantExpression() const
{
    auto currentValue = getBitXorExpression().solveConstantExpression();
    for (auto& exp : getOptionalBitXorExpressions())
    {
        currentValue = std::visit([&exp](auto&& lhs) -> Node::constantVariant
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasBitOr<T1, T2>{})
                                                            {
                                                                return lhs | rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, exp.solveConstantExpression());
                                  }, currentValue);
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::LogicalAndExpression::solveConstantExpression() const
{
    auto currentValue = getBitOrExpression().solveConstantExpression();
    for (auto& exp : getOptionalBitOrExpressions())
    {
        currentValue = std::visit([&exp](auto&& lhs) -> Node::constantVariant
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasLogicAnd<T1, T2>{})
                                                            {
                                                                return lhs && rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, exp.solveConstantExpression());
                                  }, currentValue);
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::LogicalOrExpression::solveConstantExpression() const
{
    auto currentValue = getAndExpression().solveConstantExpression();
    for (auto& exp : getOptionalAndExpressions())
    {
        currentValue = std::visit([&exp](auto&& lhs) -> Node::constantVariant
                                  {
                                      using T1 = std::decay_t<decltype(lhs)>;
                                      return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                        {
                                                            using T2 = std::decay_t<decltype(rhs)>;
                                                            if constexpr(hasLogicOr<T1, T2>{})
                                                            {
                                                                return lhs || rhs;
                                                            }
                                                            else
                                                            {
                                                                throw std::runtime_error(
                                                                    "Can't apply plus to operands in constant expression");
                                                                return {};
                                                            }
                                                        }, exp.solveConstantExpression());
                                  }, currentValue);
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::ConditionalExpression::solveConstantExpression() const
{
    auto value = getLogicalOrExpression().solveConstantExpression();
    if(getOptionalExpression() && getOptionalConditionalExpression())
    {
        bool first = std::visit([](auto&& value)->bool{return value;},value);
        if(first)
        {
            return getOptionalExpression()->solveConstantExpression();
        }
        else
        {
            return getOptionalConditionalExpression()->solveConstantExpression();
        }
    }
    else
    {
        return value;
    }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"


OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::EqualityExpression::solveConstantExpression() const
{
    auto currentValue = getRelationalExpression().solveConstantExpression();
    for (auto&[op, exp] : getOptionalRelationalExpressions())
    {
        switch (op)
        {
        case EqualityOperator::Equal:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasEQ<T1, T2>{})
                                                                {
                                                                    return lhs == rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case EqualityOperator::NotEqual:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasNE<T1, T2>{})
                                                                {
                                                                    return lhs != rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        }
    }
    return currentValue;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::RelationalExpression::solveConstantExpression() const
{
    auto currentValue = getShiftExpression().solveConstantExpression();
    for (auto&[op, exp] : getOptionalShiftExpressions())
    {
        switch (op)
        {
        case RelationalOperator::GreaterThan:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasGT<T1, T2>{})
                                                                {
                                                                    return lhs > rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case RelationalOperator::GreaterThanOrEqual:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasGE<T1, T2>{})
                                                                {
                                                                    return lhs > rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case RelationalOperator::LessThan:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasLT<T1, T2>{})
                                                                {
                                                                    return lhs < rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        case RelationalOperator::LessThanOrEqual:
        {
            currentValue = std::visit([exp = &exp](auto&& lhs) -> Node::constantVariant
                                      {
                                          using T1 = std::decay_t<decltype(lhs)>;
                                          return std::visit([lhs](auto&& rhs) -> Node::constantVariant
                                                            {
                                                                using T2 = std::decay_t<decltype(rhs)>;
                                                                if constexpr(hasLE<T1, T2>{})
                                                                {
                                                                    return lhs <= rhs;
                                                                }
                                                                else
                                                                {
                                                                    throw std::runtime_error(
                                                                        "Can't apply plus to operands in constant expression");
                                                                    return {};
                                                                }
                                                            }, exp->solveConstantExpression());
                                      }, currentValue);
        }
            break;
        }
    }
    return currentValue;
}
#pragma GCC diagnostic pop
#pragma GCC diagnostic pop

const OpenCL::Syntax::CastExpression& OpenCL::Syntax::Term::getCastExpression() const
{
    return m_castExpression;
}

const std::vector<std::pair<OpenCL::Syntax::Term::BinaryDotOperator,
                            OpenCL::Syntax::CastExpression>>& OpenCL::Syntax::Term::getOptionalCastExpressions() const
{
    return m_optionalCastExpressions;
}

OpenCL::Syntax::AssignmentExpression::AssignmentExpression(std::uint64_t line,
                                                           std::uint64_t column,
                                                           std::unique_ptr<UnaryExpression>&& unaryFactor,
                                                           AssignOperator assignOperator,
                                                           std::unique_ptr<NonCommaExpression>&& nonCommaExpression)
    : NonCommaExpression(line, column), m_unaryFactor(std::move(unaryFactor)), m_assignOperator(assignOperator),
      m_nonCommaExpression(std::move(nonCommaExpression))
{
    assert(m_unaryFactor);
}

const OpenCL::Syntax::UnaryExpression& OpenCL::Syntax::AssignmentExpression::getUnaryFactor() const
{
    return *m_unaryFactor;
}

const OpenCL::Syntax::NonCommaExpression& OpenCL::Syntax::AssignmentExpression::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

OpenCL::Syntax::PostFixExpressionFunctionCall::PostFixExpressionFunctionCall(std::uint64_t line,
                                                                             std::uint64_t column,
                                                                             std::unique_ptr<PostFixExpression>&& postFixExpression,
                                                                             std::vector<std::unique_ptr<
                                                                                 NonCommaExpression>>&& optionalAssignmanetExpressions)
    : PostFixExpression(line, column), m_postFixExpression(std::move(postFixExpression)),
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

const std::vector<std::unique_ptr<OpenCL::Syntax::NonCommaExpression>>& OpenCL::Syntax::PostFixExpressionFunctionCall::getOptionalAssignmentExpressions() const
{
    return m_optionalAssignmanetExpressions;
}

OpenCL::Syntax::ArrayType::ArrayType(std::unique_ptr<Type>&& type, std::size_t size)
    : m_type(std::move(type)), m_size(size)
{
    assert(m_type);
}

const std::unique_ptr<OpenCL::Syntax::Type>& OpenCL::Syntax::ArrayType::getType() const
{
    return m_type;
}

size_t OpenCL::Syntax::ArrayType::getSize() const
{
    return m_size;
}

std::unique_ptr<OpenCL::Syntax::Type> OpenCL::Syntax::ArrayType::clone() const
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
    : PostFixExpression(line, column), m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionIncrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::PostFixExpressionDecrement::PostFixExpressionDecrement(std::uint64_t line,
                                                                       std::uint64_t column,
                                                                       std::unique_ptr<PostFixExpression>&& postFixExpression)
    : PostFixExpression(line, column), m_postFixExpression(std::move(postFixExpression))
{}

const OpenCL::Syntax::PostFixExpression& OpenCL::Syntax::PostFixExpressionDecrement::getPostFixExpression() const
{
    return *m_postFixExpression;
}

OpenCL::Syntax::StructOrUnionDeclaration::StructOrUnionDeclaration(std::uint64_t line,
                                                                   std::uint64_t column,
                                                                   bool isUnion,
                                                                   std::string name,
                                                                   std::vector<std::pair<std::shared_ptr<Type>,
                                                                                         std::string>>&& types)
    : Global(line, column), m_isUnion(isUnion), m_name(std::move(name)), m_types(std::move(types))
{
    if (m_types.empty())
    {
        throw std::runtime_error("Struct and unions needs to have atleast one field");
    }
    assert(std::all_of(m_types.begin(), m_types.end(), [](const auto& pair)
    { return pair.first.get(); }));
}

const std::vector<std::pair<std::shared_ptr<OpenCL::Syntax::Type>,
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

std::unique_ptr<OpenCL::Syntax::Type> OpenCL::Syntax::StructType::clone() const
{
    return std::make_unique<StructType>(m_name, m_isConst);
}

bool OpenCL::Syntax::StructType::isConst() const
{
    return m_isConst;
}

std::string OpenCL::Syntax::StructType::name() const
{
    return (isConst() ? "const struct " : "struct ") + getName();
}

OpenCL::Syntax::SwitchStatement::SwitchStatement(std::uint64_t line,
                                                 std::uint64_t column,
                                                 Expression&& expression,
                                                 std::unique_ptr<Statement>&& statement)
    : Statement(line, column), m_expression(std::move(expression)), m_statement(std::move(statement))
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
    : Statement(line, column), m_statement(std::move(statement))
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
    : Statement(line, column), m_constant(constant), m_statement(std::move(statement))
{

}

const OpenCL::Syntax::Node::constantVariant& OpenCL::Syntax::CaseStatement::getConstant() const
{
    return m_constant;
}

const OpenCL::Syntax::Statement* OpenCL::Syntax::CaseStatement::getStatement() const
{
    return m_statement.get();
}

bool OpenCL::Syntax::Type::isSigned() const
{
    return false;
}

bool OpenCL::Syntax::Type::isVoid() const
{
    return false;
}

bool OpenCL::Syntax::Type::isConst() const
{
    return false;
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

std::unique_ptr<OpenCL::Syntax::Type> OpenCL::Syntax::UnionType::clone() const
{
    return std::make_unique<UnionType>(getName(), isConst());
}

std::string OpenCL::Syntax::UnionType::name() const
{
    return (isConst() ? "const union " : "union ") + getName();
}

OpenCL::Syntax::PostFixExpressionTypeInitializer::PostFixExpressionTypeInitializer(std::uint64_t line,
                                                                                   std::uint64_t column,
                                                                                   std::shared_ptr<Type> type,
                                                                                   std::vector<std::unique_ptr<
                                                                                       NonCommaExpression>>&& nonCommaExpressions)
    : PostFixExpression(line, column), m_type(std::move(type)), m_nonCommaExpressions(std::move(nonCommaExpressions))
{}

const std::shared_ptr<OpenCL::Syntax::Type>& OpenCL::Syntax::PostFixExpressionTypeInitializer::getType() const
{
    return m_type;
}

const std::vector<std::unique_ptr<OpenCL::Syntax::NonCommaExpression>>& OpenCL::Syntax::PostFixExpressionTypeInitializer::getNonCommaExpressions() const
{
    return m_nonCommaExpressions;
}

OpenCL::Syntax::TypedefDeclaration::TypedefDeclaration(std::uint64_t line,
                                                       std::uint64_t column,
                                                       std::unique_ptr<StructOrUnionDeclaration>&& optionalStructOrUnion)
    : Global(line, column), m_optionalStructOrUnion(std::move(optionalStructOrUnion))
{}

const std::unique_ptr<OpenCL::Syntax::StructOrUnionDeclaration>& OpenCL::Syntax::TypedefDeclaration::getOptionalStructOrUnion() const
{
    return m_optionalStructOrUnion;
}

OpenCL::Syntax::Node::Node(uint64_t line, uint64_t column) : m_line(line), m_column(column)
{}

uint64_t OpenCL::Syntax::Node::getLine() const
{
    return m_line;
}

uint64_t OpenCL::Syntax::Node::getColumn() const
{
    return m_column;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::Node::solveConstantExpression() const
{
    throw std::runtime_error("Expression is not a constant expression");
}

OpenCL::Syntax::BlockItem::BlockItem(std::uint64_t line, std::uint64_t column) : Node(line, column)
{}

OpenCL::Syntax::Statement::Statement(std::uint64_t line, std::uint64_t column) : BlockItem(line, column)
{}

OpenCL::Syntax::BreakStatement::BreakStatement(std::uint64_t line, std::uint64_t column) : Statement(line, column)
{}

OpenCL::Syntax::NonCommaExpression::NonCommaExpression(std::uint64_t line, std::uint64_t column) : Node(line, column)
{}

OpenCL::Syntax::PrimaryExpression::PrimaryExpression(std::uint64_t line, std::uint64_t column) : Node(line, column)
{}

OpenCL::Syntax::PostFixExpression::PostFixExpression(std::uint64_t line, std::uint64_t column) : Node(line, column)
{}

OpenCL::Syntax::UnaryExpression::UnaryExpression(std::uint64_t line, std::uint64_t column) : Node(line, column)
{}

OpenCL::Syntax::Global::Global(std::uint64_t line, std::uint64_t column) : Node(line, column)
{

}

OpenCL::Syntax::InitializerList::InitializerList(std::uint64_t line, std::uint64_t column) : Node(line, column)
{

}

OpenCL::Syntax::InitializerListScalarExpression::InitializerListScalarExpression(std::uint64_t line,
                                                                                 std::uint64_t column,
                                                                                 OpenCL::Syntax::Expression&& expression)
    : InitializerList(line, column), m_expression(std::move(expression))
{}

const OpenCL::Syntax::Expression& OpenCL::Syntax::InitializerListScalarExpression::getExpression() const
{
    return m_expression;
}

OpenCL::Syntax::InitializerListBlock::InitializerListBlock(std::uint64_t line,
                                                           std::uint64_t column,
                                                           vector&& nonCommaExpressionsAndBlocks)
    : InitializerList(line, column), m_nonCommaExpressionsAndBlocks(std::move(nonCommaExpressionsAndBlocks))
{}

const typename OpenCL::Syntax::InitializerListBlock::vector& OpenCL::Syntax::InitializerListBlock::getNonCommaExpressionsAndBlocks() const
{
    return m_nonCommaExpressionsAndBlocks;
}

OpenCL::Syntax::Node::constantVariant OpenCL::Syntax::UnaryExpressionUnaryOperator::solveConstantExpression() const
{
    auto value = getUnaryExpression().solveConstantExpression();
    switch (getAnOperator())
    {
    case UnaryOperator::Increment:
    case UnaryOperator::Decrement:
    case UnaryOperator::Ampersand:
    case UnaryOperator::Asterisk:return Node::solveConstantExpression();
    case UnaryOperator::Plus:return value;
    case UnaryOperator::Minus:
    {
        return std::visit([](auto&& value) -> Node::constantVariant
                          {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr(hasNegate<T>{})
                              {
                                  return -value;
                              }
                              else
                              {
                                  throw std::runtime_error("Can't apply - to constant operator");
                                  return {};
                              }
                          }, value);
    }
    case UnaryOperator::BitNot:
    {
        return std::visit([](auto&& value) -> Node::constantVariant
                          {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr(hasBitNegate<T>{})
                              {
                                  return ~value;
                              }
                              else
                              {
                                  throw std::runtime_error("Can't apply - to constant operator");
                                  return {};
                              }
                          }, value);
    }
    case UnaryOperator::LogicalNot:
    {
        return std::visit([](auto&& value) -> Node::constantVariant
                          {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr(hasLogicNegate<T>{})
                              {
                                  return !value;
                              }
                              else
                              {
                                  throw std::runtime_error("Can't apply - to constant operator");
                                  return {};
                              }
                          }, value);
    }
    }
    return value;
}

OpenCL::Syntax::EnumDeclaration::EnumDeclaration(std::uint64_t line,
                                                 std::uint64_t column,
                                                 std::string name,
                                                 const std::vector<std::pair<std::string, std::int32_t>>& values) : Global(line,
                                                                                                           column),
                                                                                                    m_name(std::move(
                                                                                                        name)),
                                                                                                    m_values(values)
{}

const std::string& OpenCL::Syntax::EnumDeclaration::getName() const
{
    return m_name;
}

const std::vector<std::pair<std::string,std::int32_t>>& OpenCL::Syntax::EnumDeclaration::getValues() const
{
    return m_values;
}

OpenCL::Syntax::EnumType::EnumType(const std::string& name, bool isConst) : m_name(name), m_isConst(isConst)
{}

const std::string& OpenCL::Syntax::EnumType::getName() const
{
    return m_name;
}

std::unique_ptr<OpenCL::Syntax::Type> OpenCL::Syntax::EnumType::clone() const
{
    return std::make_unique<EnumType>(getName(),isConst());
}

std::string OpenCL::Syntax::EnumType::name() const
{
    return (isConst() ? "const enum " : "enum ") + getName();
}

bool OpenCL::Syntax::EnumType::isConst() const
{
    return m_isConst;
}

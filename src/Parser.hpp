#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Lexer.hpp"
#include <vector>
#include <llvm/IR/Value.h>

namespace OpenCL::Parser
{
    class Node
    {
    public:

        Node() = default;

        virtual ~Node() = default;

        Node(const Node&) = delete;

        Node(Node&&) noexcept = default;

        Node& operator=(const Node&) = delete;

        Node& operator=(Node&&) noexcept = default;

        virtual llvm::Value* codegen()
        {
            return nullptr;
        }
    };

    class NonCommaExpression : public Node
    {
    protected:
        NonCommaExpression() = default;
    };

    class Expression final : public Node
    {
        std::unique_ptr<NonCommaExpression> m_nonCommaExpression;
        std::unique_ptr<NonCommaExpression> m_optionalNonCommaExpression;

    public:

        Expression(std::unique_ptr<NonCommaExpression>&& nonCommaExpression,
                   std::unique_ptr<NonCommaExpression>&& optionalNonCommaExpression);

        const NonCommaExpression& getNonCommaExpression() const;

        const NonCommaExpression* getOptionalNonCommaExpression() const;
    };

    class AssignmentExpression final : public NonCommaExpression
    {
        std::string m_identifier;
        std::unique_ptr<NonCommaExpression> m_expression;

    public:

        enum class AssignOperator
        {
            NoOperator,
            PlusAssign,
            MinusAssign,
            DivideAssign,
            MultiplyAssign,
            ModuloAssign,
            LeftShiftAssign,
            RightShiftAssign,
            BitAndAssign,
            BitOrAssign,
            BitXorAssign
        };

    private:

        AssignOperator m_assignOperator;

    public:

        AssignmentExpression(std::string identifier,
                             std::unique_ptr<NonCommaExpression>&& expression,
                             AssignOperator assignOperator);

        const std::string& getIdentifier() const;

        const NonCommaExpression& getExpression() const;

        AssignOperator getAssignOperator() const;
    };

    class Factor : public Node
    {
    protected:
        Factor() = default;
    };

    class ParentheseFactor final : public Factor
    {
        Expression m_expression;

    public:

        explicit ParentheseFactor(Expression&& expression);

        const Expression& getExpression() const;
    };

    class UnaryFactor final : public Factor
    {
    public:

        enum class UnaryOperator
        {
            UnaryNegation,
            UnaryBitWiseNegation,
            UnaryLogicalNegation,
        };

    private:

        UnaryOperator m_unaryOperator;
        std::unique_ptr<Factor> m_factor;

    public:

        UnaryFactor(UnaryOperator unaryOperator, std::unique_ptr<Factor>&& factor);

        UnaryOperator getUnaryOperator() const;

        const OpenCL::Parser::Factor& getFactor() const;
    };

    class ConstantFactor final : public Factor
    {
        std::string value;

    public:

        explicit ConstantFactor(std::string value);

        const std::string& getValue() const;
    };

    class VariableFactor final : public Factor
    {
        std::string m_name;

    public:

        explicit VariableFactor(std::string name);

        const std::string& getName() const;
    };

    class PostIncrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PostIncrement(std::string name);

        const std::string& getName() const;
    };

    class PreIncrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PreIncrement(std::string name);

        const std::string& getName() const;
    };

    class PostDecrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PostDecrement(std::string name);

        const std::string& getName() const;
    };

    class PreDecrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PreDecrement(std::string name);

        const std::string& getName() const;
    };

    class FunctionalCall final : public Factor
    {
        std::string m_name;
        std::vector<std::unique_ptr<NonCommaExpression>> m_expressions;

    public:

        FunctionalCall(std::string name, std::vector<std::unique_ptr<NonCommaExpression>> expressions);
    };

    class Term final : public Node
    {
        std::unique_ptr<Factor> m_factor;

    public:

        enum class BinaryDotOperator
        {
            NoOperator,
            BinaryMultiply,
            BinaryDivide,
            BinaryModulo
        };

    private:

        BinaryDotOperator m_optionalOperator;
        std::unique_ptr<Factor> m_optionalFactor;

    public:

        explicit Term(std::unique_ptr<Factor>&& factor,
             BinaryDotOperator optionalOperator = BinaryDotOperator::NoOperator,
             std::unique_ptr<Factor>&& optionalFactor = nullptr);

        const Factor& getFactor() const;

        BinaryDotOperator getOptionalOperator() const;

        const Factor* getOptionalFactor() const;
    };

    class AdditiveExpression final : public Node
    {
        Term m_term;

    public:

        enum class BinaryDashOperator
        {
            NoOperator,
            BinaryPlus,
            BianryMinus
        };

    private:

        BinaryDashOperator m_optionalOperator;
        std::unique_ptr<Term> m_optionalTerm;

    public:

        explicit AdditiveExpression(Term&& term,
                           BinaryDashOperator optionalOperator = BinaryDashOperator::NoOperator,
                           std::unique_ptr<Term>&& optionalTerm = nullptr);

        const Term& getTerm() const;

        BinaryDashOperator getOptionalOperator() const;

        const Term* getOptionalTerm() const;
    };

    class ShiftExpression final : public Node
    {
        AdditiveExpression m_additiveExpression;

    public:

        enum class ShiftOperator
        {
            NoOperator,
            Right,
            Left
        };

    private:

        ShiftOperator m_optionalOperator;
        std::unique_ptr<AdditiveExpression> m_optionalAdditiveExpression;

    public:

        explicit ShiftExpression(AdditiveExpression&& additiveExpression,
                        ShiftOperator optionalOperator = ShiftOperator::NoOperator,
                        std::unique_ptr<AdditiveExpression>&& optionalAdditiveExpression = nullptr);

        const AdditiveExpression& getAdditiveExpression() const;

        ShiftOperator getOptionalOperator() const;

        const AdditiveExpression* getOptionalAdditiveExpression() const;
    };

    class RelationalExpression final : public Node
    {
        ShiftExpression m_shiftExpression;

    public:

        enum class RelationalOperator
        {
            NoOperator,
            LessThan,
            LessThanOrEqual,
            GreaterThan,
            GreaterThanOrEqual
        };

    private:

        RelationalOperator m_optionalOperator;
        std::unique_ptr<ShiftExpression> m_optionalExpression;

    public:

        explicit RelationalExpression(ShiftExpression&& shiftExpression,
                             RelationalOperator optionalOperator = RelationalOperator::NoOperator,
                             std::unique_ptr<ShiftExpression>&& optionalExpression = nullptr);

        const ShiftExpression& getShiftExpression() const;

        RelationalOperator getOptionalOperator() const;

        const ShiftExpression* getOptionalExpression() const;
    };

    class EqualityExpression final : public Node
    {
        RelationalExpression m_relationalExpression;

    public:

        enum class EqualityOperator
        {
            NoOperator,
            Equal,
            NotEqual
        };

    private:

        EqualityOperator m_optionalOperator;
        std::unique_ptr<RelationalExpression> m_optionalRelationalExpression;

    public:

        explicit EqualityExpression(RelationalExpression&& relationalExpression,
                           EqualityOperator optionalOperator = EqualityOperator::NoOperator,
                           std::unique_ptr<RelationalExpression>&& optionalRelationalExpression = nullptr);

        const RelationalExpression& getRelationalExpression() const;

        EqualityOperator getOptionalOperator() const;

        const RelationalExpression* getOptionalRelationalExpression() const;
    };

    class LogicalAndExpression final : public Node
    {
        EqualityExpression m_equalityExpression;
        std::unique_ptr<EqualityExpression> m_optionalEqualityExpression;

    public:

        explicit LogicalAndExpression(EqualityExpression&& equalityExpression,
                             std::unique_ptr<EqualityExpression>&& optionalEqualityExpression = nullptr);

        const EqualityExpression& getEqualityExpression() const;

        const EqualityExpression* getOptionalEqualityExpression() const;
    };

    class LogicalOrExpression final : public Node
    {
        LogicalAndExpression m_andExpression;
        std::unique_ptr<LogicalAndExpression> m_optionalAndExpression;

    public:

        explicit LogicalOrExpression(LogicalAndExpression&& andExpression,
                            std::unique_ptr<LogicalAndExpression>&& optionalAndExpression = nullptr);

        const LogicalAndExpression& getAndExpression() const;

        const LogicalAndExpression* getOptionalAndExpression() const;
    };

    class ConditionalExpression final : public NonCommaExpression
    {
        LogicalOrExpression m_logicalOrExpression;
        std::unique_ptr<Expression> m_optionalExpression;
        std::unique_ptr<ConditionalExpression> m_optionalConditionalExpression;

    public:

        explicit ConditionalExpression(LogicalOrExpression&& logicalOrExpression,
                              std::unique_ptr<Expression>&& optionalExpression = nullptr,
                              std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression = nullptr);

        const LogicalOrExpression& getLogicalOrExpression() const;

        const Expression* getOptionalExpression() const;

        const ConditionalExpression* getOptionalConditionalExpression() const;
    };

    class BlockItem : public Node
    {
    protected:
        BlockItem() = default;
    };

    class Statement : public BlockItem
    {
    protected:
        Statement() = default;
    };

    class ReturnStatement final : public Statement
    {
        Expression m_expression;

    public:

        explicit ReturnStatement(Expression&& expression);

        const Expression& getExpression() const;
    };

    class ExpressionStatement final : public Statement
    {
        std::unique_ptr<Expression> m_optionalExpression;

    public:

        explicit ExpressionStatement(std::unique_ptr<Expression>&& optionalExpression = nullptr);

        const Expression* getOptionalExpression() const;

        std::unique_ptr<Expression> moveOptionalExpression();
    };

    class IfStatement final : public Statement
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_branch;
        std::unique_ptr<Statement> m_elseBranch;

    public:

        IfStatement(Expression&& expression,
                    std::unique_ptr<Statement>&& branch,
                    std::unique_ptr<Statement>&& elseBranch = nullptr);

        const Expression& getExpression() const;

        const Statement& getBranch() const;

        const Statement* getElseBranch() const;
    };

    class BlockStatement final : public Statement
    {
        std::vector<std::unique_ptr<BlockItem>> m_blockItems;

    public:

        explicit BlockStatement(std::vector<std::unique_ptr<BlockItem>> blockItems);

        const std::vector<std::unique_ptr<BlockItem>>& getBlockItems() const;
    };

    class ForStatement final : public Statement
    {
        std::unique_ptr<Expression> m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        explicit ForStatement(std::unique_ptr<Expression>&& initial = nullptr,
                     std::unique_ptr<Expression>&& controlling = nullptr,
                     std::unique_ptr<Expression>&& post = nullptr);

        const Expression* getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;
    };

    class Declaration final : public BlockItem
    {
        std::string m_name;
        std::unique_ptr<Expression> m_optionalExpression;

    public:

        explicit Declaration(std::string name);

        Declaration(std::string name, std::unique_ptr<Expression>&& optionalExpression);

        const std::string& getName() const;

        const Expression* getOptionalExpression() const;
    };

    class ForDeclarationStatement final : public Statement
    {
        Declaration m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        explicit ForDeclarationStatement(Declaration&& initial,
                                std::unique_ptr<Expression>&& controlling = nullptr,
                                std::unique_ptr<Expression>&& post = nullptr);

        const Declaration& getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;
    };

    class HeadWhileStatement final : public Statement
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:

        HeadWhileStatement(Expression&& expression, std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;
    };

    class FootWhileStatement final : public Statement
    {
        std::unique_ptr<Statement> m_statement;
        Expression m_expression;

    public:

        FootWhileStatement(std::unique_ptr<Statement>&& statement, Expression&& expression);

        const Statement& getStatement() const;

        const Expression& getExpression() const;
    };

    class BreakStatement final : public Statement
    {};

    class ContinueStatement final : public Statement
    {};

    class Function final : public Node
    {
        std::string m_name;
        std::vector<std::string> m_arguments;
        std::vector<std::unique_ptr<BlockItem>> m_blockItems;

    public:

        Function(std::string name,
                 std::vector<std::string> arguments,
                 std::vector<std::unique_ptr<BlockItem>>&& blockItems);

        const std::string& getName() const;

        const std::vector<std::string>& getArguments() const;

        const std::vector<std::unique_ptr<BlockItem>>& getBlockItems() const;
    };

    class Program final : public Node
    {
        std::vector<Function> m_functions;

    public:

        explicit Program(std::vector<Function>&& functions) noexcept;

        const std::vector<Function>& getFunctions() const;
    };

    Program buildTree(std::vector<Lexer::Token>&& tokens);
}

#endif //OPENCLPARSER_PARSER_HPP

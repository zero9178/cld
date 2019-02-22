#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Lexer.hpp"
#include <vector>
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>

namespace OpenCL::Parser
{
    class Context
    {
        std::vector<std::map<std::string, llvm::AllocaInst*>> m_namedValues;

    public:

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder{context};
        std::unique_ptr<llvm::Module> module;
        llvm::Function* currentFunction;
        std::vector<llvm::BasicBlock*> continueBlocks;
        std::vector<llvm::BasicBlock*> breakBlocks;

        llvm::Value* getNamedValue(const std::string& name)
        {
            for(auto begin = m_namedValues.rbegin(); begin != m_namedValues.rend(); begin++)
            {
                if(auto result = begin->find(name);result != begin->end())
                {
                    return result->second;
                }
            }
            return module->getGlobalVariable(name,true);
        }

        void popScope()
        {
            m_namedValues.pop_back();
        }

        void pushScope()
        {
            m_namedValues.emplace_back();
        }

        void addValueToScope(const std::string& name,llvm::AllocaInst* value)
        {
            m_namedValues.back()[name] = value;
        }

        void clearScope()
        {
            m_namedValues.clear();
            pushScope();
        }
    };

    class Node
    {
    public:

        Node() = default;

        virtual ~Node() = default;

        Node(const Node&) = delete;

        Node(Node&&) noexcept = default;

        Node& operator=(const Node&) = delete;

        Node& operator=(Node&&) noexcept = default;

        virtual llvm::Value* codegen(Context& context) const = 0;
    };

    /**
     * <Type> ::= <TokenType::VoidKeyword>
     *          | <TokenType::CharKeyword>
     *          | <TokenType::ShortKeyword>
     *          | <TokenType::IntKeyword>
     *          | <TokenType::LongKeyword>
     *          | <TokenType::FloatKeyword>
     *          | <TokenType::DoubleKeyword>
     *          | <TokenType::SignedKeyword>
     *          | <TokenType::UnsignedKeyword>
     */
    class Type
    {
    public:

        enum class Types
        {
            Void,
            Char,
            Short,
            Int,
            Long,
            Float,
            Double
        };

    private:

        Types m_type;
        bool m_isSigned;

    public:

        Type(Types type, bool isSigned);

        Types getType() const;

        bool isSigned() const;

        llvm::Type* type(Context& context) const;
    };

    /**
     * <NonCommaExpression> ::= <AssignmentExpression> | <ConditionalExpression>
     */
    class NonCommaExpression : public Node
    {
    protected:
        NonCommaExpression() = default;
    };

    /**
     * <Expression> ::= <NonCommaExpression> [ <TokenType::Comma> <NonCommaExpression>]
     */
    class Expression final : public Node
    {
        std::unique_ptr<NonCommaExpression> m_nonCommaExpression;
        std::unique_ptr<NonCommaExpression> m_optionalNonCommaExpression;

    public:

        Expression(std::unique_ptr<NonCommaExpression>&& nonCommaExpression,
                   std::unique_ptr<NonCommaExpression>&& optionalNonCommaExpression);

        const NonCommaExpression& getNonCommaExpression() const;

        const NonCommaExpression* getOptionalNonCommaExpression() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <AssignmentExpression> ::= <TokenType::Identifier> <AssignmentExpression::AssignOperator> <Expression>
     */
    class AssignmentExpression final : public NonCommaExpression
    {
        std::string m_identifier;
        std::unique_ptr<NonCommaExpression> m_expression;

    public:

        /**
         * <AssignmentExpression::AssignOperator>
         */
        enum class AssignOperator
        {
            NoOperator,///<<TokenType::Assignment>
            PlusAssign,///<<TokenType::PlusAssign>
            MinusAssign,///<<TokenType::MinusAssign>
            DivideAssign,///<TokenType::DiviceAssign>
            MultiplyAssign,///<<TokenType::MultiplyAssign>
            ModuloAssign,///<<TokenType::ModuloAssign>
            LeftShiftAssign,///<<TokenType::LeftShiftAssign>
            RightShiftAssign,///<<TokenType::RightShiftAssign>
            BitAndAssign,///<<TokenType::BitAndAssign>
            BitOrAssign,///<<TokenType::BitOrAssign>
            BitXorAssign///<<TokenType::BitXorAssign>
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

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <Factor>
     * ::=  <FunctionCall>
     * |    <ParentheseFactor>
     * |    <UnaryFactor>
     * |    <ConstantFactor>
     * |    <VariableFactor>
     * |    <PostIncrement>
     * |    <PreIncrement>
     * |    <PreDecrement>
     * |    <PostDecrement>
     */
    class Factor : public Node
    {
    protected:
        Factor() = default;
    };

    /**
     * <ParentheseFactor> ::= <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     */
    class ParentheseFactor final : public Factor
    {
        Expression m_expression;

    public:

        explicit ParentheseFactor(Expression&& expression);

        const Expression& getExpression() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <UnaryFactor> ::= <UnaryFactor::UnaryOperator> <Factor>
     */
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

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ConstantFactor> ::= <TokenType::IntegerLiteral>
     */
    class ConstantFactor final : public Factor
    {
        std::string m_value;

    public:

        explicit ConstantFactor(std::string value);

        const std::string& getValue() const;

        llvm::Constant* codegen(Context& context) const override;
    };

    /**
     * <VariableFactor> ::= <TokenType::Identifier>
     */
    class VariableFactor final : public Factor
    {
        std::string m_name;

    public:

        explicit VariableFactor(std::string name);

        const std::string& getName() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <PostIncrement> ::= <TokenType::Identifier> <TokenType::Increment>
     */
    class PostIncrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PostIncrement(std::string name);

        const std::string& getName() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <PreIncrement> ::= <TokenType::Increment> <TokenType::Identifier>
     */
    class PreIncrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PreIncrement(std::string name);

        const std::string& getName() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <PostDecrement> ::= <TokenType::Identifier> <TokenType::Decrement>
     */
    class PostDecrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PostDecrement(std::string name);

        const std::string& getName() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <PreDecrement> ::= <TokenType::Decrement> <TokenType::Identifier>
     */
    class PreDecrement final : public Factor
    {
        std::string m_name;

    public:

        explicit PreDecrement(std::string name);

        const std::string& getName() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <FunctionCall> ::= <TokenType::Identifier> <TokenType::OpenParenthese>
     *                    [ <NonCommaExpression> { <TokenType::Colon> <NonCommaExpression> } ] <TokenType::CloseParenthese>
     */
    class FunctionCall final : public Factor
    {
        std::string m_name;
        std::vector<std::unique_ptr<NonCommaExpression>> m_expressions;

    public:

        FunctionCall(std::string name, std::vector<std::unique_ptr<NonCommaExpression>> expressions);

        const std::string& getName() const;

        const std::vector<std::unique_ptr<NonCommaExpression>>& getExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <Term> ::= <Factor> { <Term::BinaryDotOperator> <Factor> }
     */
    class Term final : public Node
    {
        std::unique_ptr<Factor> m_factor;

    public:

        /**
         * <Term::BinaryDotOperator>
         */
        enum class BinaryDotOperator
        {
            BinaryMultiply,///<<TokenType::Multiplication>
            BinaryDivide,///<<TokenType::Division>
            BinaryRemainder///<<TokenType::Modulo>
        };

    private:

        std::vector<std::pair<BinaryDotOperator, std::unique_ptr<Factor>>> m_optionalFactors;

    public:

        Term(std::unique_ptr<Factor>&& factor,
             std::vector<std::pair<BinaryDotOperator, std::unique_ptr<Factor>>>&& optionalFactors);

        const Factor& getFactor() const;

        const std::vector<std::pair<BinaryDotOperator, std::unique_ptr<Factor>>>& getOptionalFactors() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <AdditiveExpression> ::= <Term> { <AdditiveExpression::BinaryDashOperator> <Term> }
     */
    class AdditiveExpression final : public Node
    {
        Term m_term;

    public:

        /**
         * <AdditiveExpression::BinaryDashOperator>
         */
        enum class BinaryDashOperator
        {
            BinaryPlus,///<<TokenType::Addition>
            BinaryMinus///<<TokenType::Negation>
        };

    private:

        std::vector<std::pair<BinaryDashOperator, Term>> m_optionalTerms;

    public:

        explicit AdditiveExpression(Term&& term,
                                    std::vector<std::pair<BinaryDashOperator, Term>>&& optionalTerms);

        const Term& getTerm() const;

        const std::vector<std::pair<BinaryDashOperator, Term>>& getOptionalTerms() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ShiftExpression> ::= <AdditiveExpression> { <ShiftExpression::ShiftOperator> <AdditiveExpression> }
     */
    class ShiftExpression final : public Node
    {
        AdditiveExpression m_additiveExpression;

    public:

        /**
         * <ShiftExpression::ShiftOperator>
         */
        enum class ShiftOperator
        {
            Right,///<<TokenType::ShiftRight>
            Left///<<TokenType::ShiftLeft>
        };

    private:

        std::vector<std::pair<ShiftOperator, AdditiveExpression>> m_optionalAdditiveExpressions;

    public:

        explicit ShiftExpression(AdditiveExpression&& additiveExpression,
                                 std::vector<std::pair<ShiftOperator,
                                                       AdditiveExpression>>&& optionalAdditiveExpressions);

        const AdditiveExpression& getAdditiveExpression() const;

        const std::vector<std::pair<ShiftOperator, AdditiveExpression>>& getOptionalAdditiveExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <RelationalExpression> ::= <ShiftExpression> { <RelationalExpression::RelationalOperator> <ShiftExpression> }
     */
    class RelationalExpression final : public Node
    {
        ShiftExpression m_shiftExpression;

    public:

        /**
         * <RelationalExpression::RelationalOperator>
         */
        enum class RelationalOperator
        {
            LessThan,///<<TokenType::LessThan>
            LessThanOrEqual,///<TokenType::LessThanOrEqual>
            GreaterThan,///<TokenType::GreaterThan>
            GreaterThanOrEqual///<TokenType::GreaterThanOrEqual>
        };

    private:

        std::vector<std::pair<RelationalOperator, ShiftExpression>> m_optionalRelationalExpressions;

    public:

        explicit RelationalExpression(ShiftExpression&& shiftExpression,
                                      std::vector<std::pair<RelationalOperator,
                                                            ShiftExpression>>&& optionalRelationalExpressions);

        const ShiftExpression& getShiftExpression() const;

        const std::vector<std::pair<RelationalOperator, ShiftExpression>>& getOptionalRelationalExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <EqualityExpression> ::= <RelationalExpression> { <EqualityExpression::EqualityOperator> <RelationalExpression> }
     */
    class EqualityExpression final : public Node
    {
        RelationalExpression m_relationalExpression;

    public:

        /**
         * <EqualityExpression::EqualityOperator>
         */
        enum class EqualityOperator
        {
            Equal,///<<TokenType::Equal>
            NotEqual///<TokenType::NotEqual>
        };

    private:

        std::vector<std::pair<EqualityOperator, RelationalExpression>> m_optionalRelationalExpressions;

    public:

        explicit EqualityExpression(RelationalExpression&& relationalExpression,
                                    std::vector<std::pair<EqualityOperator,
                                                          RelationalExpression>>&& optionalRelationalExpressions);

        const RelationalExpression& getRelationalExpression() const;

        const std::vector<std::pair<EqualityOperator, RelationalExpression>>& getOptionalRelationalExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <BitAndExpression> ::= <EqualityExpression> { <TokenType::BitAnd> <EqualityExpression> }
     */
    class BitAndExpression final : public Node
    {
        EqualityExpression m_equalityExpression;
        std::vector<EqualityExpression> m_optionalEqualityExpressions;

    public:

        BitAndExpression(EqualityExpression&& equalityExpression,
                         std::vector<EqualityExpression>&& optionalEqualityExpressions);

        const EqualityExpression& getEqualityExpression() const;

        const std::vector<EqualityExpression>& getOptionalEqualityExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <BitXorExpression> ::= <BitAndExpression> { <TokenType::BitXor> <BitAndExpression> }
     */
    class BitXorExpression final : public Node
    {
        BitAndExpression m_bitAndExpression;
        std::vector<BitAndExpression> m_optionalBitAndExpressions;

    public:

        BitXorExpression(BitAndExpression&& bitAndExpression,
                         std::vector<BitAndExpression>&& optionalBitAndExpressions);

        const BitAndExpression& getBitAndExpression() const;

        const std::vector<BitAndExpression>& getOptionalBitAndExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <BitOrExpression> ::= <BitXorExpression> { <TokenType::BitOr> <BitXorExpression> }
     */
    class BitOrExpression final : public Node
    {
        BitXorExpression m_bitXorExpression;
        std::vector<BitXorExpression> m_optionalBitXorExpressions;

    public:

        BitOrExpression(BitXorExpression&& bitXorExpression,
                        std::vector<BitXorExpression>&& optionalBitXorExpressions);

        const BitXorExpression& getBitXorExpression() const;

        const std::vector<BitXorExpression>& getOptionalBitXorExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <LogicalAndExpression> ::= <BitOrExpression> { <TokenType::LogicAnd> <BitOrExpression> }
     */
    class LogicalAndExpression final : public Node
    {
        BitOrExpression m_bitOrExpression;
        std::vector<BitOrExpression> m_optionalBitOrExpressions;

    public:

        LogicalAndExpression(BitOrExpression&& equalityExpression,
                             std::vector<BitOrExpression>&& optionalEqualityExpressions);

        const BitOrExpression& getBitOrExpression() const;

        const std::vector<BitOrExpression>& getOptionalBitOrExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <LogicalOrExpression> ::= <LogicalAndExpression> { <TokenType::LogicOr> <LogicalAndExpression> }
     */
    class LogicalOrExpression final : public Node
    {
        LogicalAndExpression m_andExpression;
        std::vector<LogicalAndExpression> m_optionalAndExpressions;

    public:

        explicit LogicalOrExpression(LogicalAndExpression&& andExpression,
                                     std::vector<LogicalAndExpression>&& optionalAndExpressions);

        const LogicalAndExpression& getAndExpression() const;

        const std::vector<LogicalAndExpression>& getOptionalAndExpressions() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ConditionalExpression> ::= <LogicalOrExpression> [ <TokenType::QuestionMark> <Expression> <TokenType::Colon> <ConditionalExpression> ]
     */
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

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <BlockItem> ::= <Statement> | <Declaration>
     */
    class BlockItem : public Node
    {
    protected:
        BlockItem() = default;
    };

    /**
     * <Statement>
     * ::=  <ReturnStatement>
     * |    <ExpressionStatement>
     * |    <IfStatement>
     * |    <BlockStatement>
     * |    <ForStatement>
     * |    <ForDeclarationStatement>
     * |    <HeadWhileStatement>
     * |    <FootWhileStatement>
     * |    <BreakStatement>
     * |    <ContinueStatement>
     */
    class Statement : public BlockItem
    {
    protected:
        Statement() = default;
    };

    /**
     * <ReturnStatement> ::= <TokenType::ReturnKeyword> <Expression> <TokenType::SemiColon>
     */
    class ReturnStatement final : public Statement
    {
        Expression m_expression;

    public:

        explicit ReturnStatement(Expression&& expression);

        const Expression& getExpression() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ExpressionStatement> ::= [<Expression>] <TokenType::SemiColon>
     */
    class ExpressionStatement final : public Statement
    {
        std::unique_ptr<Expression> m_optionalExpression;

    public:

        explicit ExpressionStatement(std::unique_ptr<Expression>&& optionalExpression = nullptr);

        const Expression* getOptionalExpression() const;

        std::unique_ptr<Expression> moveOptionalExpression();

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <IfStatement> ::= <TokenType::IfKeyword> <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     *               <Statement> [ <TokenType::ElseKeyword> <Statement> ]
     */
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

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <BlockStatement> ::= <TokenType::OpenBrace> { <BlockItem> } <TokenType::CloseBrace>
     */
    class BlockStatement final : public Statement
    {
        std::vector<std::unique_ptr<BlockItem>> m_blockItems;

    public:

        explicit BlockStatement(std::vector<std::unique_ptr<BlockItem>> blockItems);

        const std::vector<std::unique_ptr<BlockItem>>& getBlockItems() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ForStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParenthese> [<Expression>] <TokenType::SemiColon>
     *                    [<Expression>] <TokenType::SemiColon> [<Expression>] <TokenType::CloseParenthese> <Statement>
     */
    class ForStatement final : public Statement
    {
        std::unique_ptr<Statement> m_statement;
        std::unique_ptr<Expression> m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        explicit ForStatement(std::unique_ptr<Statement>&& statement,
                              std::unique_ptr<Expression>&& initial = nullptr,
                              std::unique_ptr<Expression>&& controlling = nullptr,
                              std::unique_ptr<Expression>&& post = nullptr);

        const Statement& getStatement() const;

        const Expression* getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <Declaration> ::= <Type> <TokenType::Identifier> [ <TokenType::Assignment> <Expression> ] <TokenType::SemiColon>
     */
    class Declaration final : public BlockItem
    {
        Type m_type;
        std::string m_name;
        std::unique_ptr<Expression> m_optionalExpression;

    public:

        explicit Declaration(Type type,
                                     std::string name,
                                     std::unique_ptr<Expression>&& optionalExpression = nullptr);

        const Type& getType() const;

        const std::string& getName() const;

        const Expression* getOptionalExpression() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ForDeclarationStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParenthese> <Declaration> [<Expression>]
     *                           <TokenType::SemiColon>  [<Expression>] <TokenType::CloseParenthese> <Statement>
     */
    class ForDeclarationStatement final : public Statement
    {
        std::unique_ptr<Statement> m_statement;
        Declaration m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        explicit ForDeclarationStatement(std::unique_ptr<Statement>&& statement, Declaration&& initial,
                                         std::unique_ptr<Expression>&& controlling = nullptr,
                                         std::unique_ptr<Expression>&& post = nullptr);

        const Statement& getStatement() const;

        const Declaration& getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <HeadWhileStatement> ::= <TokenType::WhileKeyword> <TokenType::OpenParenthese> <Expression>
     *                          <TokenType::CloseParenthese> <Statement>
     */
    class HeadWhileStatement final : public Statement
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:

        HeadWhileStatement(Expression&& expression, std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <FootWhileStatement> ::= <TokenType::DoKeyword> <Statement> <TokenType::WhileKeyword> <Expression> <TokenType::SemiColon>
     */
    class FootWhileStatement final : public Statement
    {
        std::unique_ptr<Statement> m_statement;
        Expression m_expression;

    public:

        FootWhileStatement(std::unique_ptr<Statement>&& statement, Expression&& expression);

        const Statement& getStatement() const;

        const Expression& getExpression() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <BreakStatement> ::= <TokenType::BreakKeyword> <TokenType::SemiColon>
     */
    class BreakStatement final : public Statement
    {
    public:
        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <ContinueStatement> ::= <TokenType::ContinueStatement> <TokenType::SemiColon>
     */
    class ContinueStatement final : public Statement
    {
    public:
        llvm::Value* codegen(Context& context) const override;
    };

    class Global : public Node
    {
    protected:
        Global() = default;
    };

    /**
     * <function> ::= <TokenType::IntKeyword> <TokenType::Identifier> <TokenType::OpenParenthese>
     *                [ <TokenType::IntKeyword> <TokenType::Identifier> { <TokenType::Comma> <TokenType::IntKeyword>
     *                <TokenType::Identifier> } ] <TokenType::CloseParenthese>
     *                ( <TokenType::OpenBrace>  { <BlockItem> } <TokenType::CloseBrace>  | <TokenType::SemiColon>  )
     */
    class Function final : public Global
    {
        std::string m_name;
        std::vector<std::string> m_arguments;
        BlockStatement m_block;

    public:

        Function(std::string name,
                 std::vector<std::string> arguments,
                 BlockStatement&& blockItems);

        const std::string& getName() const;

        const std::vector<std::string>& getArguments() const;

        const BlockStatement& getBlockStatement() const;

        llvm::Function* codegen(Context& context) const override;
    };

    class GlobalDeclaration final : public Global
    {
        std::string m_name;
        std::unique_ptr<ConstantFactor> m_optionalValue;

    public:

        explicit GlobalDeclaration(std::string name, std::unique_ptr<ConstantFactor>&& value = nullptr);

        const std::string& getName() const;

        const ConstantFactor* getOptionalValue() const;

        llvm::Value* codegen(Context& context) const override;
    };

    /**
     * <program> ::= {<Function> | <GlobalDeclaration>}
     */
    class Program final : public Node
    {
        std::vector<std::unique_ptr<Global>> m_globals;

    public:

        explicit Program(std::vector<std::unique_ptr<Global>>&& globals) noexcept;

        const std::vector<std::unique_ptr<Global>>& getGlobals() const;

        llvm::Value* codegen(Context& context) const override;
    };

    Program buildTree(std::vector<Lexer::Token>&& tokens);
}

#endif //OPENCLPARSER_PARSER_HPP

#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Lexer.hpp"
#include <set>
#include <vector>
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>

namespace OpenCL::Parser
{
    class Type;

    class Context
    {
        using tuple = std::pair<llvm::Value*,std::shared_ptr<Type>>;

        struct Function
        {
            std::shared_ptr<Type> retType;
            std::vector<const Type*> arguments;
        };

        std::map<std::string, Function> m_functions;
        std::map<std::string, tuple> m_globalValues;
        std::vector<std::map<std::string, tuple>> m_namedValues;

    public:

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder{context};
        llvm::DIBuilder* debugBuilder;
        llvm::DIFile* debugUnit = nullptr;
        std::unique_ptr<llvm::Module> module;
        llvm::Function* currentFunction;
        const Type* functionRetType = nullptr;
        std::vector<llvm::BasicBlock*> continueBlocks;
        std::vector<llvm::BasicBlock*> breakBlocks;
        std::vector<std::pair<llvm::SwitchInst*,bool>> switchStack;

        struct StructOrUnion
        {
            std::map<std::string,std::uint64_t> order;
            std::vector<std::shared_ptr<Type>> types;
            bool isUnion = false;
        };

        std::map<std::string,StructOrUnion> structs;

        bool hasFunction(const std::string& name) const
        {
            return m_functions.find(name) != m_functions.end();
        }

        const Function& getFunction(const std::string& name) const
        {
            return m_functions.at(name);
        }

        tuple getNamedValue(const std::string& name) const
        {
            for (auto begin = m_namedValues.rbegin(); begin != m_namedValues.rend(); begin++)
            {
                if (auto result = begin->find(name);result != begin->end())
                {
                    return result->second;
                }
            }
            auto result = m_globalValues.find(name);
            return result != m_globalValues.end() ? result->second : tuple{};
        }

        void popScope()
        {
            m_namedValues.pop_back();
        }

        void pushScope()
        {
            m_namedValues.emplace_back();
        }

        void addValueToScope(const std::string& name, const tuple& value)
        {
            m_namedValues.back()[name] = value;
        }

        void addGlobal(const std::string& name, const tuple& value)
        {
            auto [it,ins] = m_globalValues.insert({name,value});
            if(!ins)
            {
                throw std::runtime_error("Redefinition of global symbol " + name);
            }
        }

        void addFunction(const std::string& name, const Function& function)
        {
            m_functions[name] = function;
        }

        void clearScope()
        {
            m_namedValues.clear();
            pushScope();
        }
    };

    class Node
    {
        std::uint64_t m_line;
        std::uint64_t m_column;

    public:

        Node(uint64_t line, uint64_t column);

        virtual ~Node() = default;

        Node(const Node&) = delete;

        Node(Node&&) noexcept = default;

        Node& operator=(const Node&) = delete;

        Node& operator=(Node&&) noexcept = default;

        virtual std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const = 0;
    };

    /**
     * <Type> ::= <PrimitiveType> | <PointerType> | <ArrayType> | <StructType>
     */
    class Type
    {
    protected:

        Type() = default;

    public:

        virtual bool isSigned() const;

        virtual bool isVoid() const;

        virtual bool isConst() const;

        virtual llvm::Type* type(Context& context) const = 0;

        virtual std::unique_ptr<Type> clone() const = 0;
    };

    /**
     * <PrimitiveType> ::= <TokenType::VoidKeyword>
     *          | <TokenType::CharKeyword>
     *          | <TokenType::ShortKeyword>
     *          | <TokenType::IntKeyword>
     *          | <TokenType::LongKeyword>
     *          | <TokenType::FloatKeyword>
     *          | <TokenType::DoubleKeyword>
     *          | <TokenType::SignedKeyword>
     *          | <TokenType::UnsignedKeyword>
     *          | <TokenType::ConstKeyword>
     *          {<TokenType::VoidKeyword>
     *          | <TokenType::CharKeyword>
     *          | <TokenType::ShortKeyword>
     *          | <TokenType::IntKeyword>
     *          | <TokenType::LongKeyword>
     *          | <TokenType::FloatKeyword>
     *          | <TokenType::DoubleKeyword>
     *          | <TokenType::SignedKeyword>
     *          | <TokenType::UnsignedKeyword>
     *          | <TokenType::ConstKeyword>}
     */
    class PrimitiveType final : public Type
    {
    public:

        enum class Types
        {
            Char,
            Short,
            Int,
            Long,
            Float,
            Double,
            Unsigned,
            Signed,
            Const
        };

    private:

        std::uint64_t m_bitCount;
        bool m_isConst;
        bool m_isFloatingPoint;
        bool m_isSigned;

    public:

        explicit PrimitiveType(std::vector<OpenCL::Parser::PrimitiveType::Types>&& types);

        PrimitiveType(std::uint64_t bitCount,bool isConst, bool isFloatingPoint, bool isSigned);

        std::uint64_t getBitCount() const;

        bool isFloatingPoint() const;

        bool isSigned() const override;

        bool isVoid() const override;

        bool isConst() const override;

        llvm::Type* type(Context& context) const override;

        std::unique_ptr<Type> clone() const override;
    };

    /**
     * <PointerType> ::= <Type> <TokenType::Asterisk> [ <TokenType::ConstKeyword> ]
     */
    class PointerType final : public Type
    {
        std::unique_ptr<Type> m_type;
        bool m_isConst;

    public:

        explicit PointerType(std::unique_ptr<Type>&& type, bool isConst);

        const Type& getType() const;

        bool isConst() const override;

        llvm::Type* type(Context& context) const override;

        std::unique_ptr<Type> clone() const override;
    };

    /**
     * <ArrayType> ::= <Type> <TokenType::OpenSquareBracket> <PrimaryExpressionConstant> <TokenType::CloseSquareBracket>
     */
     class ArrayType final : public Type
     {
         std::unique_ptr<Type> m_type;
         std::size_t m_size;

     public:

         ArrayType(std::unique_ptr<Type>&& type, std::size_t size);

         const std::unique_ptr<Type>& getType() const;

         std::size_t getSize() const;

         llvm::Type* type(Context& context) const override;

         std::unique_ptr<Type> clone() const override;
     };

     /**
      * <StructType> ::= [ <TokenType::ConstKeyword> ] <TokenType::StructKeyword> <TokenType::Identifer>
      *                  [ <TokenType::ConstKeyword> ]
      */
     class StructType final : public Type
     {
         std::string m_name;
         bool m_isConst;

     public:

         explicit StructType(std::string name, bool isConst);

         const std::string& getName() const;

         bool isConst() const override;

         llvm::Type* type(Context& context) const override;

         std::unique_ptr<Type> clone() const override;
     };

     /**
      * <UnionType> ::= [ <TokenType::ConstKeyword> ] <TokenType::UnionKeyword> <TokenType::Identifer>
      *                  [ <TokenType::ConstKeyword> ]
      */
     class UnionType final : public Type
     {
         std::string m_name;
         bool m_isConst;

     public:

         UnionType(std::string name, bool isConst);

         const std::string& getName() const;

         bool isConst() const override;

         llvm::Type* type(Context& context) const override;

         std::unique_ptr<Type> clone() const override;
     };

    /**
     * <NonCommaExpression> ::= <AssignmentExpression> | <ConditionalExpression>
     */
    class NonCommaExpression : public Node
    {
    protected:

        NonCommaExpression(std::uint64_t line,std::uint64_t column);
    };

    /**
     * <Expression> ::= <NonCommaExpression> [ <TokenType::Comma> <NonCommaExpression>]
     */
    class Expression final : public Node
    {
        std::unique_ptr<NonCommaExpression> m_nonCommaExpression;
        std::unique_ptr<NonCommaExpression> m_optionalNonCommaExpression;

    public:

        Expression(std::uint64_t line,
                           std::uint64_t column,
                           std::unique_ptr<NonCommaExpression>&& nonCommaExpression,
                           std::unique_ptr<NonCommaExpression>&& optionalNonCommaExpression);

        const NonCommaExpression& getNonCommaExpression() const;

        const NonCommaExpression* getOptionalNonCommaExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };


    /**
     * <PrimaryExpression> ::= <PrimaryExpressionIdentifier>
     *                       | <PrimaryExpressionConstant>
     *                       | <PrimaryExpressionParenthese>
     */
    class PrimaryExpression : public Node
    {
    protected:

        PrimaryExpression(std::uint64_t line,std::uint64_t column);
    };

    /**
     * <PrimaryExpressionIdentifier> ::= <TokenType::Identifier>
     */
    class PrimaryExpressionIdentifier final : public PrimaryExpression
    {
        std::string m_identifier;

    public:

        PrimaryExpressionIdentifier(std::uint64_t line,
                                            std::uint64_t column,
                                            std::string identifier);

        const std::string& getIdentifier() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PrimaryExpressionConstant> ::= <TokenType::Constant>
     */
    class PrimaryExpressionConstant final : public PrimaryExpression
    {

    public:

        using variant = std::variant<std::int32_t,
                                     std::uint32_t,
                                     std::int64_t,
                                     std::uint64_t,
                                     float,
                                     double,
                                     std::string>;

    private:

        variant m_value;

    public:

        PrimaryExpressionConstant(std::uint64_t line, std::uint64_t column, variant value);

        const variant& getValue() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PrimaryExpressionParenthese> ::= <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     */
    class PrimaryExpressionParenthese final : public PrimaryExpression
    {
        Expression m_expression;

    public:

        PrimaryExpressionParenthese(std::uint64_t line,
                                            std::uint64_t column,
                                            Expression&& expression);

        const Expression& getExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpression> ::= <PostFixExpressionPrimaryExpression>
     *                       | <PostFixExpressionSubscript>
     *                       | <PostFixExpressionDot>
     *                       | <PostFixExpressionFunctionCall>
     *                       | <PostFixExpressionArrow>
     *                       | <PostFixExpressionIncrement>
     *                       | <PostFixExpressionDecrement>
     *                       | <PostFixExpressionTypeInitializer>
     */
    class PostFixExpression : public Node
    {
    protected:

        PostFixExpression(std::uint64_t line,std::uint64_t column);
    };

    /**
     * <PostFixExpressionPrimaryExpression> ::= <PrimaryExpression>
     */
    class PostFixExpressionPrimaryExpression final : public PostFixExpression
    {
        std::unique_ptr<PrimaryExpression> m_primaryExpression;

    public:

        PostFixExpressionPrimaryExpression(std::uint64_t line,
                                                   std::uint64_t column,
                                                   std::unique_ptr<PrimaryExpression>&& primaryExpression);

        const PrimaryExpression& getPrimaryExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionSubscript> ::= <PostFixExpression> <TokenType::OpenSquareBracket> <Expression> <TokenType::CloseSquareBracket>
     */
    class PostFixExpressionSubscript final : public PostFixExpression
    {

        std::unique_ptr<PostFixExpression> m_postFixExpression;
        Expression m_expression;

    public:

        PostFixExpressionSubscript(std::uint64_t line,
                                           std::uint64_t column,
                                           std::unique_ptr<PostFixExpression>&& postFixExpression,
                                           Expression&& expression);

        const PostFixExpression& getPostFixExpression() const;

        const Expression& getExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionIncrement> ::= <PostFixExpression> <TokenType::Increment>
     */
    class PostFixExpressionIncrement final : public PostFixExpression
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:

        PostFixExpressionIncrement(std::uint64_t line,
                                           std::uint64_t column,
                                           std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionDecrement> ::= <PostFixExpression> <TokenType::Decrement>
     */
    class PostFixExpressionDecrement final : public PostFixExpression
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:

        PostFixExpressionDecrement(std::uint64_t line,
                                           std::uint64_t column,
                                           std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionDot> ::= <PostFixExpression> <TokenType::Dot> <TokenType::Identifier>
     */
    class PostFixExpressionDot final : public PostFixExpression
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::string m_identifier;

    public:

        PostFixExpressionDot(std::uint64_t line,
                                     std::uint64_t column,
                                     std::unique_ptr<PostFixExpression>&& postFixExpression,
                                     std::string identifier);

        const PostFixExpression& getPostFixExpression() const;

        const std::string& getIdentifier() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionArrow> ::= <PostFixExpression> <TokenType::Arrow> <TokenType::Identifier>
     */
    class PostFixExpressionArrow final : public PostFixExpression
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::string m_identifier;

    public:

        PostFixExpressionArrow(std::uint64_t line,
                                       std::uint64_t column,
                                       std::unique_ptr<PostFixExpression>&& postFixExpression,
                                       std::string identifier);

        const PostFixExpression& getPostFixExpression() const;

        const std::string& getIdentifier() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionFunctionCall> ::= <PostFixExpression> <TokenType::OpenParenthese> <NonCommaExpression>
     *                                     { <TokenType::Comma> <NonCommaExpression> }
     *                                     <TokenType::CloseParenthese>
     */
    class PostFixExpressionFunctionCall final : public PostFixExpression
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::vector<std::unique_ptr<NonCommaExpression>> m_optionalAssignmanetExpressions;

    public:

        PostFixExpressionFunctionCall(std::uint64_t line,
                                              std::uint64_t column,
                                              std::unique_ptr<PostFixExpression>&& postFixExpression,
                                              std::vector<std::unique_ptr<NonCommaExpression>>&& optionalAssignmanetExpressions);

        const PostFixExpression& getPostFixExpression() const;

        const std::vector<std::unique_ptr<NonCommaExpression>>& getOptionalAssignmentExpressions() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <PostFixExpressionTypeInitializer> ::= <TokenType::OpenParenthese> <Type> <TokenType::CloseParenthese>
     *                                        <TokenType::OpenBrace> <NonCommaExpression>
     *                                        { <TokenType::Comma> <NonCommaExpression> } <TokenType::CloseBrace>
     */
    class PostFixExpressionTypeInitializer final : public PostFixExpression
    {
        std::shared_ptr<Type> m_type;
        std::vector<std::unique_ptr<NonCommaExpression>> m_nonCommaExpressions;

    public:

        PostFixExpressionTypeInitializer(std::shared_ptr<Type> type,
                                         std::vector<std::unique_ptr<NonCommaExpression>>&& nonCommaExpressions);

        const std::shared_ptr<Type>& getType() const;

        const std::vector<std::unique_ptr<NonCommaExpression>>& getNonCommaExpressions() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <UnaryExpression> ::= <UnaryExpressionPostFixExpression>
     *                     | <UnaryExpressionUnaryOperator>
     *                     | <UnaryExpressionSizeOf>
     */
    class UnaryExpression : public Node
    {
    protected:

        UnaryExpression() = default;
    };

    /**
     * <AssignmentExpression> ::= <UnaryExpression> <AssignmentExpression::AssignOperator> <AssignmentExpression>
     */
    class AssignmentExpression final : public NonCommaExpression
    {
        std::unique_ptr<UnaryExpression> m_unaryFactor;

    public:

        /**
         * <AssignmentExpression::AssignOperator>
         */
        enum class AssignOperator
        {
            NoOperator,///<<TokenType::Assignment>
            PlusAssign,///<<TokenType::PlusAssign>
            MinusAssign,///<<TokenType::MinusAssign>
            DivideAssign,///<TokenType::DivideAssign>
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
        std::unique_ptr<NonCommaExpression> m_nonCommaExpression;

    public:

        AssignmentExpression(std::unique_ptr<UnaryExpression>&& unaryFactor,
                             AssignOperator assignOperator,
                             std::unique_ptr<NonCommaExpression>&& nonCommaExpression);

        const UnaryExpression& getUnaryFactor() const;

        const NonCommaExpression& getNonCommaExpression() const;

        AssignOperator getAssignOperator() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <UnaryExpressionPostFixExpression> ::= <PostFixExpression>
     */
    class UnaryExpressionPostFixExpression final : public UnaryExpression
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:

        explicit UnaryExpressionPostFixExpression(std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <UnaryExpressionUnaryOperator> ::= <TokenType::Increment> <UnaryExpression>
     *                                  | <TokenType::Decrement> <UnaryExpression>
     *                                  | <TokenType::Ampersand> <UnaryExpression>
     *                                  | <TokenType::Asterisk> <UnaryExpression>
     *                                  | <TokenType::Addition> <UnaryExpression>
     *                                  | <TokenType::Negation> <UnaryExpression>
     *                                  | <TokenType::BitWiseNegation> <UnaryExpression>
     *                                  | <TokenType::LogicalNegation> <UnaryExpression>
     */
    class UnaryExpressionUnaryOperator final : public UnaryExpression
    {
    public:

        enum class UnaryOperator
        {
            Increment,
            Decrement,
            Ampersand,
            Asterisk,
            Plus,
            Minus,
            BitNot,
            LogicalNot
        };

    private:

        UnaryOperator m_operator;
        std::unique_ptr<UnaryExpression> m_unaryExpression;

    public:

        UnaryExpressionUnaryOperator(UnaryOperator anOperator, std::unique_ptr<UnaryExpression>&& unaryExpression);

        UnaryOperator getAnOperator() const;

        const UnaryExpression& getUnaryExpression() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <UnaryExpressionSizeOf> ::= <TokenType::SizeOfKeyword> <UnaryExpression>
     *                           | <TokenType::SizeOfKeyword> <TokenType::OpenParenthese> <Type> <TokenType::CloseParenthese>
     */
    class UnaryExpressionSizeOf final : public UnaryExpression
    {
        std::variant<std::unique_ptr<UnaryExpression>,std::shared_ptr<Type>> m_unaryOrType;

    public:

        explicit UnaryExpressionSizeOf(std::variant<std::unique_ptr<UnaryExpression>, std::shared_ptr<Type>>&& unaryOrType);

        const std::variant<std::unique_ptr<UnaryExpression>, std::shared_ptr<Type>>& getUnaryOrType() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <CastExpression> ::= <UnaryExpression>
     *                    | <TokenType::OpenParenthese> <Type> <TokenType::CloseParentheses> <CastExpression>
     */
    class CastExpression final : public Node
    {
        std::variant<std::unique_ptr<UnaryExpression>,std::pair<std::shared_ptr<Type>,std::unique_ptr<CastExpression>>> m_unaryOrCast;

    public:

        explicit CastExpression(std::variant<std::unique_ptr<UnaryExpression>,
                                          std::pair<std::shared_ptr<Type>,
                                                    std::unique_ptr<CastExpression>>>&& unaryOrCast);

        const std::variant<std::unique_ptr<UnaryExpression>,
                           std::pair<std::shared_ptr<Type>, std::unique_ptr<CastExpression>>>& getUnaryOrCast() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <Term> ::= <CastExpression> { <Term::BinaryDotOperator> <CastExpressions> }
     */
    class Term final : public Node
    {
        CastExpression m_castExpression;

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

        std::vector<std::pair<BinaryDotOperator, CastExpression>> m_optionalCastExpressions;

    public:

        Term(CastExpression&& castExpressions,
             std::vector<std::pair<BinaryDotOperator, CastExpression>>&& optionalCastExpressions);

        const CastExpression& getCastExpression() const;

        const std::vector<std::pair<BinaryDotOperator, CastExpression>>& getOptionalCastExpressions() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <BlockItem> ::= <Statement> | <Declaration>
     */
    class BlockItem : public Node
    {
    protected:

        BlockItem(std::uint64_t line,std::uint64_t column);
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
     * |    <SwitchStatement>
     * |    <DefaultStatement>
     * |    <CaseStatement>
     */
    class Statement : public BlockItem
    {
    protected:

        Statement(std::uint64_t line,std::uint64_t column);
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <SwitchStatement> ::= <TokenType::SwitchKeyword> <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     *                          <Statement>
     */
    class SwitchStatement final : public Statement
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:

        SwitchStatement(Expression&& expression, std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <DefaultStatement> ::= <TokenType::DefaultKeyword> <TokenType::Colon> <Statement>
     */
    class DefaultStatement final : public Statement
    {
        std::unique_ptr<Statement> m_statement;

    public:

        explicit DefaultStatement(std::unique_ptr<Statement>&& statement);

        const Statement& getStatement() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <CaseStatement> ::= <TokenType::CaseKeyword> <ConstantExpression> <TokenType::Colon> [<Statement>]
     */
    class CaseStatement final : public Statement
    {
        Expression m_constant;
        std::unique_ptr<Statement> m_statement;

    public:

        CaseStatement(Expression&& constant, std::unique_ptr<Statement>&& statement);

        const Expression& getConstant() const;

        const Statement* getStatement() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <Declaration> ::= <Type> <TokenType::Identifier> {<TokenType::OpenSquareBracket> <TokenType::Literal>
     * <TokenType::CloseSquareBracket>} [ <TokenType::Assignment> <Expression> ] { <TokenType::Comma> <TokenType::Identifier> {<TokenType::OpenSquareBracket> <TokenType::Literal>
     * <TokenType::CloseSquareBracket>} [ <TokenType::Assignment> <Expression> ]} <TokenType::SemiColon>
     */
    class Declarations final : public BlockItem
    {
        std::vector<std::tuple<std::shared_ptr<Type>,std::string,std::unique_ptr<Expression>>> m_declarations;

    public:

        explicit Declarations(std::vector<std::tuple<std::shared_ptr<Type>,
                                                  std::string,
                                                  std::unique_ptr<Expression>>>&& declarations);

        const std::vector<std::tuple<std::shared_ptr<Type>,
                                     std::string,
                                     std::unique_ptr<Expression>>>& getDeclarations() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <ForDeclarationStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParenthese> <Declaration> [<Expression>]
     *                           <TokenType::SemiColon>  [<Expression>] <TokenType::CloseParenthese> <Statement>
     */
    class ForDeclarationStatement final : public Statement
    {
        std::unique_ptr<Statement> m_statement;
        Declarations m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        explicit ForDeclarationStatement(std::unique_ptr<Statement>&& statement, Declarations&& initial,
                                         std::unique_ptr<Expression>&& controlling = nullptr,
                                         std::unique_ptr<Expression>&& post = nullptr);

        const Statement& getStatement() const;

        const Declarations& getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <BreakStatement> ::= <TokenType::BreakKeyword> <TokenType::SemiColon>
     */
    class BreakStatement final : public Statement
    {
    public:

        BreakStatement(std::uint64_t line,std::uint64_t column);

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <ContinueStatement> ::= <TokenType::ContinueStatement> <TokenType::SemiColon>
     */
    class ContinueStatement final : public Statement
    {
    public:
        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    class Global : public Node
    {
    protected:
        Global() = default;
    };

    /**
      * <StructOrUnion> ::= <TokenType::StructKeyword> | <TokenType::UnionKeyword>
      *
      * <StructOrUnionDeclaration> ::= <StructOrUnion> <TokenType::Identifier> <TokenType::OpenBrace>
      *                 <Type> <TokenType::Identifier> <TokenType::Semicolon>
      *                { <Type> <TokenType::Identifier> <TokenType::Semicolon> }
      *                <TokenType::CloseBrace>
      */
    class StructOrUnionDeclaration final : public Global
    {
        bool m_isUnion;
        std::string m_name;
        std::vector<std::pair<std::shared_ptr<Type>,std::string>> m_types;

    public:

        StructOrUnionDeclaration(bool isUnion,
                                         std::string name,
                                         std::vector<std::pair<std::shared_ptr<Type>,
                                                           std::string>>&& types);

        bool isUnion() const;

        const std::string& getName() const;

        const std::vector<std::pair<std::shared_ptr<Type>,std::string>>& getTypes() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <TypeOrStructOrUnionDeclaration> ::= <Type> | <StructOrUnionDeclaration>
     *
     * <TypedefDeclaration> ::= <TokenType::TypedefKeyword> <TypeOrStructOrUnionDeclaration> [ <TokenType::Asterisk> ] <TokenType::Identifier>
     * [ {<TokenType::OpenSquareBracket> <ConstantExpression> <TokenType::CloseSquareBracket> }]
     * { <TokenType::Comma> [ <TokenType::Asterisk> ] <TokenType::Identifier>
     * [ {<TokenType::OpenSquareBracket> <ConstantExpression> <TokenType::CloseSquareBracket> }] }
     * <TokenType::SemiColon>
     */
    class TypedefDeclaration final : public Global
    {
        std::unique_ptr<StructOrUnionDeclaration> m_optionalStructOrUnion;

    public:

        explicit TypedefDeclaration(std::unique_ptr<StructOrUnionDeclaration>&& optionalStructOrUnion = nullptr);

        const std::unique_ptr<StructOrUnionDeclaration>& getOptionalStructOrUnion() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <Function> ::= <Type> <TokenType::Identifier> <TokenType::OpenParenthese>
     *                [ <Type> [<TokenType::Identifier>] {<TokenType::OpenSquareBracket> <TokenType::Literal>
     *                <TokenType::CloseSquareBracket> }
     *                { <TokenType::Comma> <Type>
     *                [<TokenType::Identifier>] [<TokenType::OpenSquareBracket> <TokenType::Literal>
     *                <TokenType::CloseSquareBracket> ] } ] <TokenType::CloseParenthese>
     *                ( <TokenType::OpenBrace>  { <BlockItem> } <TokenType::CloseBrace>  | <TokenType::SemiColon>  )
     */
    class Function final : public Global
    {
        std::shared_ptr<Type> m_returnType;
        std::string m_name;
        std::vector<std::pair<std::shared_ptr<Type>, std::string>> m_arguments;
        std::unique_ptr<BlockStatement> m_block;

    public:

        Function(std::shared_ptr<Type> returnType,
                 std::string name,
                 std::vector<std::pair<std::shared_ptr<Type>,
                                       std::string>> arguments,
                 std::unique_ptr<BlockStatement>&& blockItems = nullptr);

        const std::shared_ptr<Type>& getReturnType() const;

        const std::string& getName() const;

        const std::vector<std::pair<std::shared_ptr<Type>, std::string>>& getArguments() const;

        const BlockStatement* getBlockStatement() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    /**
     * <GlobalDeclaration> ::= <Type> <TokenType::Identifier> {<TokenType::OpenSquareBracket> <TokenType::Literal>
     *                <TokenType::CloseSquareBracket> }
     */
    class GlobalDeclaration final : public Global
    {
        std::shared_ptr<Type> m_type;
        std::string m_name;
        std::unique_ptr<PrimaryExpressionConstant> m_optionalValue;

    public:

        explicit GlobalDeclaration(std::shared_ptr<Type> type,
                                   std::string name,
                                   std::unique_ptr<PrimaryExpressionConstant>&& value = nullptr);

        const std::shared_ptr<Type>& getType() const;

        const std::string& getName() const;

        const PrimaryExpressionConstant* getOptionalValue() const;

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
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

        std::pair<llvm::Value*, std::shared_ptr<Type>> codegen(Context& context) const override;
    };

    Program buildTree(std::vector<Lexer::Token>&& tokens);
}

#endif //OPENCLPARSER_PARSER_HPP

#ifndef OPENCLPARSER_SYNTAX_HPP
#define OPENCLPARSER_SYNTAX_HPP

#include "Lexer.hpp"
#include <set>
#include <vector>
#include <memory>

namespace OpenCL::Syntax
{
    template <class T>
    class Type;

    class Expression;

    class PrimaryExpressionIdentifier;

    class PrimaryExpressionConstant;

    class PrimaryExpressionParenthese;

    class PrimaryExpression;

    class PostFixExpressionPrimaryExpression;

    class PostFixExpressionSubscript;

    class PostFixExpressionIncrement;

    class PostFixExpressionDecrement;

    class PostFixExpressionDot;

    class PostFixExpressionArrow;

    class PostFixExpressionFunctionCall;

    class PostFixExpressionTypeInitializer;

    class PostFixExpression;

    class AssignmentExpressionAssignment;

    class UnaryExpressionPostFixExpression;

    class UnaryExpressionUnaryOperator;

    class UnaryExpressionSizeOf;

    class UnaryExpression;

    class CastExpression;

    class Term;

    class AdditiveExpression;

    class ShiftExpression;

    class RelationalExpression;

    class EqualityExpression;

    class BitAndExpression;

    class BitXorExpression;

    class BitOrExpression;

    class LogicalAndExpression;

    class LogicalOrExpression;

    class ConditionalExpression;

    class AssignmentExpression;

    class ReturnStatement;

    class ExpressionStatement;

    class IfStatement;

    class SwitchStatement;

    class DefaultStatement;

    class CaseStatement;

    class BlockStatement;

    class ForStatement;

    class InitializerList;

    class Initializer;

    class Declarations;

    class BlockItem;

    class ForDeclarationStatement;

    class HeadWhileStatement;

    class FootWhileStatement;

    class BreakStatement;

    class ContinueStatement;

    class Statement;

    class StructOrUnionDeclaration;

    class EnumDeclaration;

    class TypedefDeclaration;

    class Function;

    class GlobalDeclaration;

    class Global;

    class Program;

    class PrimitiveType;

    class PointerType;

    class ArrayType;

    class StructType;

    class UnionType;

    class EnumType;

    class INodeVisitor
    {
    public:

        virtual void visit(const Expression& node) = 0;

        virtual void visit(const PrimaryExpressionIdentifier& node) = 0;

        virtual void visit(const PrimaryExpressionConstant& node) = 0;

        virtual void visit(const PrimaryExpressionParenthese& node) = 0;

        void visit(const PrimaryExpression& node);

        virtual void visit(const PostFixExpressionPrimaryExpression& node) = 0;

        virtual void visit(const PostFixExpressionSubscript& node) = 0;

        virtual void visit(const PostFixExpressionIncrement& node) = 0;

        virtual void visit(const PostFixExpressionDecrement& node) = 0;

        virtual void visit(const PostFixExpressionDot& node) = 0;

        virtual void visit(const PostFixExpressionArrow& node) = 0;

        virtual void visit(const PostFixExpressionFunctionCall& node) = 0;

        virtual void visit(const PostFixExpressionTypeInitializer& node) = 0;

        void visit(const PostFixExpression& node);

        virtual void visit(const AssignmentExpressionAssignment& node) = 0;

        virtual void visit(const UnaryExpressionPostFixExpression& node) = 0;

        virtual void visit(const UnaryExpressionUnaryOperator& node) = 0;

        virtual void visit(const UnaryExpressionSizeOf& node) = 0;

        void visit(const UnaryExpression& node);

        virtual void visit(const CastExpression& node) = 0;

        virtual void visit(const Term& node) = 0;

        virtual void visit(const AdditiveExpression& node) = 0;

        virtual void visit(const ShiftExpression& node) = 0;

        virtual void visit(const RelationalExpression& node) = 0;

        virtual void visit(const EqualityExpression& node) = 0;

        virtual void visit(const BitAndExpression& node) = 0;

        virtual void visit(const BitXorExpression& node) = 0;

        virtual void visit(const BitOrExpression& node) = 0;

        virtual void visit(const LogicalAndExpression& node) = 0;

        virtual void visit(const LogicalOrExpression& node) = 0;

        virtual void visit(const ConditionalExpression& node) = 0;

        void visit(const AssignmentExpression& node);

        virtual void visit(const ReturnStatement& node) = 0;

        virtual void visit(const ExpressionStatement& node) = 0;

        virtual void visit(const IfStatement& node) = 0;

        virtual void visit(const SwitchStatement& node) = 0;

        virtual void visit(const DefaultStatement& node) = 0;

        virtual void visit(const CaseStatement& node) = 0;

        virtual void visit(const BlockStatement& node) = 0;

        virtual void visit(const ForStatement& node) = 0;

        virtual void visit(const InitializerList& node) = 0;

        void visit(const Initializer& node);

        virtual void visit(const Declarations& node) = 0;

        void visit(const BlockItem& node);

        virtual void visit(const ForDeclarationStatement& node) = 0;

        virtual void visit(const HeadWhileStatement& node) = 0;

        virtual void visit(const FootWhileStatement& node) = 0;

        virtual void visit(const BreakStatement& node) = 0;

        virtual void visit(const ContinueStatement& node) = 0;

        void visit(const Statement& node);

        virtual void visit(const StructOrUnionDeclaration& node) = 0;

        virtual void visit(const EnumDeclaration& node) = 0;

        virtual void visit(const TypedefDeclaration& node) = 0;

        virtual void visit(const Function& node) = 0;

        virtual void visit(const GlobalDeclaration& node) = 0;

        void visit(const Global& node);

        virtual void visit(const Program& node) = 0;

        virtual void visit(const PrimitiveType& node) = 0;

        virtual void visit(const PointerType& node) = 0;

        virtual void visit(const ArrayType& node) = 0;

        virtual void visit(const StructType& node) = 0;

        virtual void visit(const UnionType& node) = 0;

        virtual void visit(const EnumType& node) = 0;
    };

    template <class...Returns>
    class NodeVisitor : public INodeVisitor
    {
    protected:

        std::variant<Returns...> m_return;

    public:

        template <class T>
        T& getReturn()
        {
            return std::get<T>(m_return);
        }

        template <class T>
        const T& getReturn() const
        {
            return std::get<T>(m_return);
        }
    };

    class Visitable
    {
    public:

        virtual void accept(INodeVisitor& visitor) const = 0;
    };

    template <class T>
    class Node : public Visitable
    {
        std::uint64_t m_line;
        std::uint64_t m_column;

    public:

        Node(std::uint64_t line, std::uint64_t column) : m_line(line), m_column(column)
        {}

        virtual ~Node() = default;

        Node(const Node&) = delete;

        Node(Node&&) noexcept = default;

        Node& operator=(const Node&) = delete;

        Node& operator=(Node&&) noexcept = default;

        std::uint64_t getLine() const
        {
            return m_line;
        }

        std::uint64_t getColumn() const
        {
            return m_column;
        }

        void accept(INodeVisitor& visitor) const final
        {
            static_assert(std::is_final_v<T>);
            visitor.visit(*static_cast<const T*>(this));
        }

        using constantVariant = std::variant<std::int32_t,
                                             std::uint32_t,
                                             std::int64_t,
                                             std::uint64_t,
                                             float,
                                             double,
                                             void*>;

    };

    class IType : public Visitable
    {
    protected:

        IType() = default;

    public:

        virtual ~IType() = default;

        IType(const IType&) = delete;

        IType(IType&&) = default;

        IType& operator=(const IType&) = delete;

        IType& operator=(IType&&) = default;

        virtual bool isSigned() const
        {
            return false;
        }

        virtual bool isVoid() const
        {
            return false;
        }

        virtual bool isConst() const
        {
            return false;
        }

        virtual std::unique_ptr<IType> clone() const = 0;

        virtual std::string name() const = 0;
    };

    /**
     * <Type> ::= <PrimitiveType> | <PointerType> | <ArrayType> | <StructType>
     */
    template <class T>
    class Type : public IType
    {
    protected:

        Type() = default;

    public:

        ~Type() override = default;

        Type(Type&&) noexcept = default;

        Type(const Type&) = delete;

        Type& operator=(Type&&) noexcept = default;

        Type& operator=(const Type&) = delete;

        void accept(INodeVisitor& visitor) const final
        {
            static_assert(std::is_final_v<T>);
            visitor.visit(*static_cast<const T*>(this));
        }
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
    class PrimitiveType final : public Type<PrimitiveType>
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

        explicit PrimitiveType(std::vector<OpenCL::Syntax::PrimitiveType::Types>&& types);

        PrimitiveType(std::uint64_t bitCount, bool isConst, bool isFloatingPoint, bool isSigned);

        std::uint64_t getBitCount() const;

        bool isFloatingPoint() const;

        bool isSigned() const override;

        bool isVoid() const override;

        bool isConst() const override;

        std::unique_ptr<IType> clone() const override;

        std::string name() const override;
    };

    /**
     * <PointerType> ::= <Type> <TokenType::Asterisk> [ <TokenType::ConstKeyword> ]
     */
    class PointerType final : public Type<PointerType>
    {
        std::unique_ptr<IType> m_type;
        bool m_isConst;

    public:

        explicit PointerType(std::unique_ptr<IType>&& type, bool isConst);

        const IType& getType() const;

        bool isConst() const override;

        std::unique_ptr<IType> clone() const override;

        std::string name() const override;
    };

    /**
     * <ArrayType> ::= <Type> <TokenType::OpenSquareBracket> <ConstantNonCommaExpression> <TokenType::CloseSquareBracket>
     */
    class ArrayType final : public Type<ArrayType>
    {
        std::unique_ptr<IType> m_type;
        std::size_t m_size;

    public:

        ArrayType(std::unique_ptr<IType>&& type, std::size_t size);

        const std::unique_ptr<IType>& getType() const;

        std::size_t getSize() const;

        void setSize(size_t size);

        std::unique_ptr<IType> clone() const override;

        std::string name() const override;
    };

    /**
     * <StructType> ::= [ <TokenType::ConstKeyword> ] <TokenType::StructKeyword> <TokenType::Identifer>
     *                  [ <TokenType::ConstKeyword> ]
     */
    class StructType final : public Type<StructType>
    {
        std::string m_name;
        bool m_isConst;

    public:

        explicit StructType(std::string name, bool isConst);

        const std::string& getName() const;

        bool isConst() const override;

        std::unique_ptr<IType> clone() const override;

        std::string name() const override;
    };

    /**
     * <UnionType> ::= [ <TokenType::ConstKeyword> ] <TokenType::UnionKeyword> <TokenType::Identifer>
     *                  [ <TokenType::ConstKeyword> ]
     */
    class UnionType final : public Type<UnionType>
    {
        std::string m_name;
        bool m_isConst;

    public:

        UnionType(std::string name, bool isConst);

        const std::string& getName() const;

        bool isConst() const override;

        std::unique_ptr<IType> clone() const override;

        std::string name() const override;
    };

    class EnumType final : public Type<EnumType>
    {
        std::string m_name;
        bool m_isConst;

    public:

        EnumType(std::string name, bool isConst);

        const std::string& getName() const;

        bool isConst() const override;

        std::unique_ptr<IType> clone() const override;

        std::string name() const override;
    };

    /**
     * <Expression> ::= <NonCommaExpression> [ <TokenType::Comma> <NonCommaExpression>]
     */
    class Expression final : public Node<Expression>
    {
        std::unique_ptr<AssignmentExpression> m_nonCommaExpression;
        std::unique_ptr<AssignmentExpression> m_optionalNonCommaExpression;

    public:

        Expression(std::uint64_t line,
                   std::uint64_t column,
                   std::unique_ptr<AssignmentExpression>&& nonCommaExpression,
                   std::unique_ptr<AssignmentExpression>&& optionalNonCommaExpression);

        const AssignmentExpression& getNonCommaExpression() const;

        const AssignmentExpression* getOptionalNonCommaExpression() const;
    };

    /**
     * <PrimaryExpressionIdentifier> ::= <TokenType::Identifier>
     */
    class PrimaryExpressionIdentifier final : public Node<PrimaryExpressionIdentifier>
    {
        std::string m_identifier;

    public:

        PrimaryExpressionIdentifier(std::uint64_t line,
                                    std::uint64_t column,
                                    std::string identifier);

        const std::string& getIdentifier() const;
    };

    /**
     * <PrimaryExpressionConstant> ::= <TokenType::Constant>
     */
    class PrimaryExpressionConstant final : public Node<PrimaryExpressionConstant>
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
    };

    /**
     * <PrimaryExpressionParenthese> ::= <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     */
    class PrimaryExpressionParenthese final : public Node<PrimaryExpressionParenthese>
    {
        Expression m_expression;

    public:

        PrimaryExpressionParenthese(std::uint64_t line,
                                    std::uint64_t column,
                                    Expression&& expression);

        const Expression& getExpression() const;
    };

    /**
    * <PrimaryExpression> ::= <PrimaryExpressionIdentifier>
    *                       | <PrimaryExpressionConstant>
    *                       | <PrimaryExpressionParenthese>
    */
    class PrimaryExpression final : public Node<PrimaryExpression>
    {
        using variant = std::variant<PrimaryExpressionIdentifier,
                                     PrimaryExpressionConstant,
                                     PrimaryExpressionParenthese>;
        variant m_variant;

    public:

        PrimaryExpression(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <PostFixExpressionPrimaryExpression> ::= <PrimaryExpression>
     */
    class PostFixExpressionPrimaryExpression final : public Node<PostFixExpressionPrimaryExpression>
    {
        PrimaryExpression m_primaryExpression;

    public:

        PostFixExpressionPrimaryExpression(std::uint64_t line,
                                           std::uint64_t column,
                                           PrimaryExpression&& primaryExpression);

        const PrimaryExpression& getPrimaryExpression() const;
    };

    /**
     * <PostFixExpressionSubscript> ::= <PostFixExpression> <TokenType::OpenSquareBracket> <Expression> <TokenType::CloseSquareBracket>
     */
    class PostFixExpressionSubscript final : public Node<PostFixExpressionSubscript>
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
    };

    /**
     * <PostFixExpressionIncrement> ::= <PostFixExpression> <TokenType::Increment>
     */
    class PostFixExpressionIncrement final : public Node<PostFixExpressionIncrement>
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:

        PostFixExpressionIncrement(std::uint64_t line,
                                   std::uint64_t column,
                                   std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;
    };

    /**
     * <PostFixExpressionDecrement> ::= <PostFixExpression> <TokenType::Decrement>
     */
    class PostFixExpressionDecrement final : public Node<PostFixExpressionDecrement>
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:

        PostFixExpressionDecrement(std::uint64_t line,
                                   std::uint64_t column,
                                   std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;
    };

    /**
     * <PostFixExpressionDot> ::= <PostFixExpression> <TokenType::Dot> <TokenType::Identifier>
     */
    class PostFixExpressionDot final : public Node<PostFixExpressionDot>
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
    };

    /**
     * <PostFixExpressionArrow> ::= <PostFixExpression> <TokenType::Arrow> <TokenType::Identifier>
     */
    class PostFixExpressionArrow final : public Node<PostFixExpressionArrow>
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
    };

    /**
     * <PostFixExpressionFunctionCall> ::= <PostFixExpression> <TokenType::OpenParenthese> <NonCommaExpression>
     *                                     { <TokenType::Comma> <NonCommaExpression> }
     *                                     <TokenType::CloseParenthese>
     */
    class PostFixExpressionFunctionCall final : public Node<PostFixExpressionFunctionCall>
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::vector<std::unique_ptr<AssignmentExpression>> m_optionalAssignmanetExpressions;

    public:

        PostFixExpressionFunctionCall(std::uint64_t line,
                                      std::uint64_t column,
                                      std::unique_ptr<PostFixExpression>&& postFixExpression,
                                      std::vector<std::unique_ptr<AssignmentExpression>>&& optionalAssignmanetExpressions);

        const PostFixExpression& getPostFixExpression() const;

        const std::vector<std::unique_ptr<AssignmentExpression>>& getOptionalAssignmentExpressions() const;
    };

    /**
     * <PostFixExpressionTypeInitializer> ::= <TokenType::OpenParenthese> <Type> <TokenType::CloseParenthese>
     *                                        <TokenType::OpenBrace> <NonCommaExpression>
     *                                        { <TokenType::Comma> <NonCommaExpression> } <TokenType::CloseBrace>
     */
    class PostFixExpressionTypeInitializer final : public Node<PostFixExpressionTypeInitializer>
    {
        std::shared_ptr<IType> m_type;
        std::vector<std::unique_ptr<AssignmentExpression>> m_nonCommaExpressions;

    public:

        PostFixExpressionTypeInitializer(std::uint64_t line,
                                         std::uint64_t column,
                                         std::shared_ptr<IType> type,
                                         std::vector<std::unique_ptr<AssignmentExpression>>&& nonCommaExpressions);

        const std::shared_ptr<IType>& getType() const;

        const std::vector<std::unique_ptr<AssignmentExpression>>& getNonCommaExpressions() const;
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
    class PostFixExpression final : public Node<PostFixExpression>
    {
        using variant = std::variant<PostFixExpressionPrimaryExpression,
                                     PostFixExpressionSubscript,
                                     PostFixExpressionDot,
                                     PostFixExpressionFunctionCall,
                                     PostFixExpressionArrow,
                                     PostFixExpressionIncrement,
                                     PostFixExpressionDecrement,
                                     PostFixExpressionTypeInitializer>;

        variant m_variant;

    public:

        PostFixExpression(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <UnaryExpressionPostFixExpression> ::= <PostFixExpression>
     */
    class UnaryExpressionPostFixExpression final : public Node<UnaryExpressionPostFixExpression>
    {
        PostFixExpression m_postFixExpression;

    public:

        UnaryExpressionPostFixExpression(std::uint64_t line,
                                         std::uint64_t column,
                                         PostFixExpression&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;
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
    class UnaryExpressionUnaryOperator final : public Node<UnaryExpressionUnaryOperator>
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

        UnaryExpressionUnaryOperator(std::uint64_t line,
                                     std::uint64_t column,
                                     UnaryOperator anOperator,
                                     std::unique_ptr<UnaryExpression>&& unaryExpression);

        UnaryOperator getAnOperator() const;

        const UnaryExpression& getUnaryExpression() const;
    };

    /**
     * <UnaryExpressionSizeOf> ::= <TokenType::SizeOfKeyword> <UnaryExpression>
     *                           | <TokenType::SizeOfKeyword> <TokenType::OpenParenthese> <Type> <TokenType::CloseParenthese>
     */
    class UnaryExpressionSizeOf final : public Node<UnaryExpressionSizeOf>
    {
        std::variant<std::unique_ptr<UnaryExpression>, std::shared_ptr<IType>> m_unaryOrType;

    public:

        UnaryExpressionSizeOf(std::uint64_t line,
                              std::uint64_t column,
                              std::variant<std::unique_ptr<UnaryExpression>,
                                           std::shared_ptr<IType>>&& unaryOrType);

        const std::variant<std::unique_ptr<UnaryExpression>, std::shared_ptr<IType>>& getUnaryOrType() const;
    };

    /**
     * <UnaryExpression> ::= <UnaryExpressionPostFixExpression>
     *                     | <UnaryExpressionUnaryOperator>
     *                     | <UnaryExpressionSizeOf>
     */
    class UnaryExpression final : public Node<UnaryExpression>
    {
        using variant = std::variant<UnaryExpressionPostFixExpression,
                                     UnaryExpressionUnaryOperator,
                                     UnaryExpressionSizeOf>;
        variant m_variant;

    public:

        UnaryExpression(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <AssignmentExpression> ::= <UnaryExpression> <AssignmentExpression::AssignOperator> <AssignmentExpression>
     */
    class AssignmentExpressionAssignment final : public Node<AssignmentExpressionAssignment>
    {
        UnaryExpression m_unaryFactor;

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
        std::unique_ptr<AssignmentExpression> m_nonCommaExpression;

    public:

        AssignmentExpressionAssignment(std::uint64_t line,
                                       std::uint64_t column,
                                       UnaryExpression&& unaryFactor,
                                       AssignOperator assignOperator,
                                       std::unique_ptr<AssignmentExpression>&& nonCommaExpression);

        const UnaryExpression& getUnaryFactor() const;

        const AssignmentExpression& getNonCommaExpression() const;

        AssignOperator getAssignOperator() const;
    };

    /**
     * <CastExpression> ::= <UnaryExpression>
     *                    | <TokenType::OpenParenthese> <Type> <TokenType::CloseParentheses> <CastExpression>
     */
    class CastExpression final : public Node<CastExpression>
    {
        std::variant<UnaryExpression,
                     std::pair<std::shared_ptr<IType>, std::unique_ptr<CastExpression>>> m_unaryOrCast;

    public:

        CastExpression(std::uint64_t line, std::uint64_t column, std::variant<UnaryExpression,
                                                                              std::pair<std::shared_ptr<IType>,
                                                                                        std::unique_ptr<CastExpression>>>&& unaryOrCast);

        const std::variant<UnaryExpression,
                           std::pair<std::shared_ptr<IType>, std::unique_ptr<CastExpression>>>& getUnaryOrCast() const;
    };

    /**
     * <Term> ::= <CastExpression> { <Term::BinaryDotOperator> <CastExpressions> }
     */
    class Term final : public Node<Term>
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

        Term(std::uint64_t line,
             std::uint64_t column,
             CastExpression&& castExpressions,
             std::vector<std::pair<BinaryDotOperator, CastExpression>>&& optionalCastExpressions);

        const CastExpression& getCastExpression() const;

        const std::vector<std::pair<BinaryDotOperator, CastExpression>>& getOptionalCastExpressions() const;
    };

    /**
     * <AdditiveExpression> ::= <Term> { <AdditiveExpression::BinaryDashOperator> <Term> }
     */
    class AdditiveExpression final : public Node<AdditiveExpression>
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

        AdditiveExpression(std::uint64_t line,
                           std::uint64_t column,
                           Term&& term,
                           std::vector<std::pair<BinaryDashOperator, Term>>&& optionalTerms);

        const Term& getTerm() const;

        const std::vector<std::pair<BinaryDashOperator, Term>>& getOptionalTerms() const;
    };

    /**
     * <ShiftExpression> ::= <AdditiveExpression> { <ShiftExpression::ShiftOperator> <AdditiveExpression> }
     */
    class ShiftExpression final : public Node<ShiftExpression>
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

        ShiftExpression(std::uint64_t line,
                        std::uint64_t column,
                        AdditiveExpression&& additiveExpression,
                        std::vector<std::pair<ShiftOperator,
                                              AdditiveExpression>>&& optionalAdditiveExpressions);

        const AdditiveExpression& getAdditiveExpression() const;

        const std::vector<std::pair<ShiftOperator, AdditiveExpression>>& getOptionalAdditiveExpressions() const;
    };

    /**
     * <RelationalExpression> ::= <ShiftExpression> { <RelationalExpression::RelationalOperator> <ShiftExpression> }
     */
    class RelationalExpression final : public Node<RelationalExpression>
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

        RelationalExpression(std::uint64_t line,
                             std::uint64_t column,
                             ShiftExpression&& shiftExpression,
                             std::vector<std::pair<RelationalOperator,
                                                   ShiftExpression>>&& optionalRelationalExpressions);

        const ShiftExpression& getShiftExpression() const;

        const std::vector<std::pair<RelationalOperator, ShiftExpression>>& getOptionalShiftExpressions() const;
    };

    /**
     * <EqualityExpression> ::= <RelationalExpression> { <EqualityExpression::EqualityOperator> <RelationalExpression> }
     */
    class EqualityExpression final : public Node<EqualityExpression>
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

        EqualityExpression(std::uint64_t line,
                           std::uint64_t column,
                           RelationalExpression&& relationalExpression,
                           std::vector<std::pair<EqualityOperator,
                                                 RelationalExpression>>&& optionalRelationalExpressions);

        const RelationalExpression& getRelationalExpression() const;

        const std::vector<std::pair<EqualityOperator, RelationalExpression>>& getOptionalRelationalExpressions() const;
    };

    /**
     * <BitAndExpression> ::= <EqualityExpression> { <TokenType::BitAnd> <EqualityExpression> }
     */
    class BitAndExpression final : public Node<BitAndExpression>
    {
        EqualityExpression m_equalityExpression;
        std::vector<EqualityExpression> m_optionalEqualityExpressions;

    public:

        BitAndExpression(std::uint64_t line,
                         std::uint64_t column,
                         EqualityExpression&& equalityExpression,
                         std::vector<EqualityExpression>&& optionalEqualityExpressions);

        const EqualityExpression& getEqualityExpression() const;

        const std::vector<EqualityExpression>& getOptionalEqualityExpressions() const;
    };

    /**
     * <BitXorExpression> ::= <BitAndExpression> { <TokenType::BitXor> <BitAndExpression> }
     */
    class BitXorExpression final : public Node<BitXorExpression>
    {
        BitAndExpression m_bitAndExpression;
        std::vector<BitAndExpression> m_optionalBitAndExpressions;

    public:

        BitXorExpression(std::uint64_t line,
                         std::uint64_t column,
                         BitAndExpression&& bitAndExpression,
                         std::vector<BitAndExpression>&& optionalBitAndExpressions);

        const BitAndExpression& getBitAndExpression() const;

        const std::vector<BitAndExpression>& getOptionalBitAndExpressions() const;
    };

    /**
     * <BitOrExpression> ::= <BitXorExpression> { <TokenType::BitOr> <BitXorExpression> }
     */
    class BitOrExpression final : public Node<BitOrExpression>
    {
        BitXorExpression m_bitXorExpression;
        std::vector<BitXorExpression> m_optionalBitXorExpressions;

    public:

        BitOrExpression(std::uint64_t line,
                        std::uint64_t column,
                        BitXorExpression&& bitXorExpression,
                        std::vector<BitXorExpression>&& optionalBitXorExpressions);

        const BitXorExpression& getBitXorExpression() const;

        const std::vector<BitXorExpression>& getOptionalBitXorExpressions() const;
    };

    /**
     * <LogicalAndExpression> ::= <BitOrExpression> { <TokenType::LogicAnd> <BitOrExpression> }
     */
    class LogicalAndExpression final : public Node<LogicalAndExpression>
    {
        BitOrExpression m_bitOrExpression;
        std::vector<BitOrExpression> m_optionalBitOrExpressions;

    public:

        LogicalAndExpression(std::uint64_t line,
                             std::uint64_t column,
                             BitOrExpression&& equalityExpression,
                             std::vector<BitOrExpression>&& optionalEqualityExpressions);

        const BitOrExpression& getBitOrExpression() const;

        const std::vector<BitOrExpression>& getOptionalBitOrExpressions() const;
    };

    /**
     * <LogicalOrExpression> ::= <LogicalAndExpression> { <TokenType::LogicOr> <LogicalAndExpression> }
     */
    class LogicalOrExpression final : public Node<LogicalOrExpression>
    {
        LogicalAndExpression m_andExpression;
        std::vector<LogicalAndExpression> m_optionalAndExpressions;

    public:

        LogicalOrExpression(std::uint64_t line,
                            std::uint64_t column,
                            LogicalAndExpression&& andExpression,
                            std::vector<LogicalAndExpression>&& optionalAndExpressions);

        const LogicalAndExpression& getAndExpression() const;

        const std::vector<LogicalAndExpression>& getOptionalAndExpressions() const;
    };

    /**
     * <ConditionalExpression> ::= <LogicalOrExpression> [ <TokenType::QuestionMark> <Expression> <TokenType::Colon> <ConditionalExpression> ]
     */
    class ConditionalExpression final : public Node<ConditionalExpression>
    {
        LogicalOrExpression m_logicalOrExpression;
        std::unique_ptr<Expression> m_optionalExpression;
        std::unique_ptr<ConditionalExpression> m_optionalConditionalExpression;

    public:

        ConditionalExpression(std::uint64_t line,
                              std::uint64_t column,
                              LogicalOrExpression&& logicalOrExpression,
                              std::unique_ptr<Expression>&& optionalExpression = nullptr,
                              std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression = nullptr);

        const LogicalOrExpression& getLogicalOrExpression() const;

        const Expression* getOptionalExpression() const;

        const ConditionalExpression* getOptionalConditionalExpression() const;
    };

    /**
     * <NonCommaExpression> ::= <AssignmentExpression> | <ConditionalExpression>
     */
    class AssignmentExpression final : public Node<AssignmentExpression>
    {

        std::variant<AssignmentExpressionAssignment, ConditionalExpression> m_variant;

    public:

        AssignmentExpression(std::uint64_t line,
                             std::uint64_t column,
                             std::variant<AssignmentExpressionAssignment, ConditionalExpression>&& variant);

        const std::variant<AssignmentExpressionAssignment, ConditionalExpression>& getVariant() const;
    };

    /**
     * <ReturnStatement> ::= <TokenType::ReturnKeyword> <Expression> <TokenType::SemiColon>
     */
    class ReturnStatement final : public Node<ReturnStatement>
    {
        Expression m_expression;

    public:

        ReturnStatement(std::uint64_t line, std::uint64_t column, Expression&& expression);

        const Expression& getExpression() const;
    };

    /**
     * <ExpressionStatement> ::= [<Expression>] <TokenType::SemiColon>
     */
    class ExpressionStatement final : public Node<ExpressionStatement>
    {
        std::unique_ptr<Expression> m_optionalExpression;

    public:

        ExpressionStatement(std::uint64_t line,
                            std::uint64_t column,
                            std::unique_ptr<Expression>&& optionalExpression = nullptr);

        const Expression* getOptionalExpression() const;

        std::unique_ptr<Expression> moveOptionalExpression();
    };

    /**
     * <IfStatement> ::= <TokenType::IfKeyword> <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     *               <Statement> [ <TokenType::ElseKeyword> <Statement> ]
     */
    class IfStatement final : public Node<IfStatement>
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_branch;
        std::unique_ptr<Statement> m_elseBranch;

    public:

        IfStatement(std::uint64_t line,
                    std::uint64_t column,
                    Expression&& expression,
                    std::unique_ptr<Statement>&& branch,
                    std::unique_ptr<Statement>&& elseBranch = nullptr);

        const Expression& getExpression() const;

        const Statement& getBranch() const;

        const Statement* getElseBranch() const;
    };

    /**
     * <SwitchStatement> ::= <TokenType::SwitchKeyword> <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     *                          <Statement>
     */
    class SwitchStatement final : public Node<SwitchStatement>
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:

        SwitchStatement(std::uint64_t line,
                        std::uint64_t column,
                        Expression&& expression,
                        std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;
    };

    /**
     * <DefaultStatement> ::= <TokenType::DefaultKeyword> <TokenType::Colon> <Statement>
     */
    class DefaultStatement final : public Node<DefaultStatement>
    {
        std::unique_ptr<Statement> m_statement;

    public:

        DefaultStatement(std::uint64_t line,
                         std::uint64_t column,
                         std::unique_ptr<Statement>&& statement);

        const Statement& getStatement() const;
    };

    /**
     * <CaseStatement> ::= <TokenType::CaseKeyword> <ConstantNonCommaExpression> <TokenType::Colon> [<Statement>]
     */
    class CaseStatement final : public Node<CaseStatement>
    {
        constantVariant m_constant;
        std::unique_ptr<Statement> m_statement;

    public:

        CaseStatement(std::uint64_t line,
                      std::uint64_t column,
                      Node::constantVariant&& constant,
                      std::unique_ptr<Statement>&& statement);

        const constantVariant& getConstant() const;

        const Statement* getStatement() const;
    };

    /**
     * <BlockStatement> ::= <TokenType::OpenBrace> { <BlockItem> } <TokenType::CloseBrace>
     */
    class BlockStatement final : public Node<BlockStatement>
    {
        std::vector<BlockItem> m_blockItems;

    public:

        BlockStatement(std::uint64_t line,
                       std::uint64_t column,
                       std::vector<BlockItem> blockItems);

        const std::vector<BlockItem>& getBlockItems() const;
    };

    /**
     * <ForStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParenthese> [<Expression>] <TokenType::SemiColon>
     *                    [<Expression>] <TokenType::SemiColon> [<Expression>] <TokenType::CloseParenthese> <Statement>
     */
    class ForStatement final : public Node<ForStatement>
    {
        std::unique_ptr<Statement> m_statement;
        std::unique_ptr<Expression> m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        ForStatement(std::uint64_t line,
                     std::uint64_t column,
                     std::unique_ptr<Statement>&& statement,
                     std::unique_ptr<Expression>&& initial = nullptr,
                     std::unique_ptr<Expression>&& controlling = nullptr,
                     std::unique_ptr<Expression>&& post = nullptr);

        const Statement& getStatement() const;

        const Expression* getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;
    };

    class Initializer;

    /**
     * <Designator> ::= <TokenType::OpenSquareBracket> <ConstantExpression> <TokenType::CloseSquareBracket>]
     *                | <TokenType::Dot> <TokenType::Identifier>
     *
     * <DesignatorList> ::= <Designator> { <Designator> }
     *
     * <Designation> ::= <DesignatorList> <TokenType::Assignment>
     *
     * <InitializerList> ::= [Designation] Initializer { <TokenType::Comma> [Designation] Initializer }
     */
    class InitializerList final : public Node<InitializerList>
    {
    public:

        using DesignatorList = std::vector<std::variant<std::size_t, std::string>>;

        using vector = std::vector<std::pair<Initializer, DesignatorList>>;

    private:

        vector m_nonCommaExpressionsAndBlocks;

    public:

        InitializerList(std::uint64_t line,
                        std::uint64_t column,
                        vector&& nonCommaExpressionsAndBlocks);

        const vector& getNonCommaExpressionsAndBlocks() const;
    };

    /**
     * <Initializer> ::= <AssignmentExpression> | <TokenType::OpenBrace> <InitializerList> [<TokenType::Comma>] <TokenType::CloseBrace>
     */
    class Initializer final : public Node<Initializer>
    {
        using variant = std::variant<AssignmentExpression, InitializerList>;
        variant m_variant;

    public:

        Initializer(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <Declaration> ::= <Type> <TokenType::Identifier> {<TokenType::OpenSquareBracket> [<ConstantNonCommaExpression>]
     * <TokenType::CloseSquareBracket>} [ <TokenType::Assignment> <InitializerList> ] { <TokenType::Comma> <TokenType::Identifier> {<TokenType::OpenSquareBracket> <ConstantNonCommaExpression>
     * <TokenType::CloseSquareBracket>} [ <TokenType::Assignment> <InitializerList> ]} <TokenType::SemiColon>
     */
    class Declarations final : public Node<Declarations>
    {
        std::vector<std::tuple<std::shared_ptr<IType>, std::string, std::unique_ptr<Initializer>>> m_declarations;

    public:

        Declarations(std::uint64_t line, std::uint64_t column, std::vector<std::tuple<std::shared_ptr<IType>,
                                                                                      std::string,
                                                                                      std::unique_ptr<Initializer>>>&& declarations);

        const std::vector<std::tuple<std::shared_ptr<IType>,
                                     std::string,
                                     std::unique_ptr<Initializer>>>& getDeclarations() const;

        std::vector<std::tuple<std::shared_ptr<IType>,
                               std::string,
                               std::unique_ptr<Initializer>>>& getDeclarations();
    };

    /**
     * <ForDeclarationStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParenthese> <Declaration> [<Expression>]
     *                           <TokenType::SemiColon>  [<Expression>] <TokenType::CloseParenthese> <Statement>
     */
    class ForDeclarationStatement final : public Node<ForDeclarationStatement>
    {
        std::unique_ptr<Statement> m_statement;
        Declarations m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:

        ForDeclarationStatement(std::uint64_t line,
                                std::uint64_t column,
                                std::unique_ptr<Statement>&& statement,
                                Declarations&& initial,
                                std::unique_ptr<Expression>&& controlling = nullptr,
                                std::unique_ptr<Expression>&& post = nullptr);

        const Statement& getStatement() const;

        const Declarations& getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;
    };

    /**
     * <HeadWhileStatement> ::= <TokenType::WhileKeyword> <TokenType::OpenParenthese> <Expression>
     *                          <TokenType::CloseParenthese> <Statement>
     */
    class HeadWhileStatement final : public Node<HeadWhileStatement>
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:

        HeadWhileStatement(std::uint64_t line,
                           std::uint64_t column,
                           Expression&& expression,
                           std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;
    };

    /**
     * <FootWhileStatement> ::= <TokenType::DoKeyword> <Statement> <TokenType::WhileKeyword> <Expression> <TokenType::SemiColon>
     */
    class FootWhileStatement final : public Node<FootWhileStatement>
    {
        std::unique_ptr<Statement> m_statement;
        Expression m_expression;

    public:

        FootWhileStatement(std::uint64_t line,
                           std::uint64_t column,
                           std::unique_ptr<Statement>&& statement,
                           Expression&& expression);

        const Statement& getStatement() const;

        const Expression& getExpression() const;
    };

    /**
     * <BreakStatement> ::= <TokenType::BreakKeyword> <TokenType::SemiColon>
     */
    class BreakStatement final : public Node<BreakStatement>
    {
    public:

        BreakStatement(std::uint64_t line, std::uint64_t column);
    };

    /**
     * <ContinueStatement> ::= <TokenType::ContinueStatement> <TokenType::SemiColon>
     */
    class ContinueStatement final : public Node<ContinueStatement>
    {
    public:

        ContinueStatement(std::uint64_t line, std::uint64_t column);
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
    class Statement final : public Node<Statement>
    {
        using variant = std::variant<ReturnStatement,
                                     ExpressionStatement,
                                     IfStatement,
                                     BlockStatement,
                                     ForStatement,
                                     ForDeclarationStatement,
                                     HeadWhileStatement,
                                     FootWhileStatement,
                                     BreakStatement,
                                     ContinueStatement,
                                     SwitchStatement,
                                     DefaultStatement,
                                     CaseStatement>;

        variant m_variant;
    public:

        Statement(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;

        variant& getVariant();
    };

    /**
     * <BlockItem> ::= <Statement> | <Declaration>
     */
    class BlockItem final : public Node<BlockItem>
    {
        using variant = std::variant<Statement, Declarations>;
        variant m_variant;

    public:

        BlockItem(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;

        variant& getVariant();
    };

    /**
      * <StructOrUnion> ::= <TokenType::StructKeyword> | <TokenType::UnionKeyword>
      *
      * <StructOrUnionDeclaration> ::= <StructOrUnion> <TokenType::Identifier> <TokenType::OpenBrace>
      *                 <Type> <TokenType::Identifier> <TokenType::Semicolon>
      *                { <Type> <TokenType::Identifier> <TokenType::Semicolon> }
      *                <TokenType::CloseBrace>
      */
    class StructOrUnionDeclaration final : public Node<StructOrUnionDeclaration>
    {
        bool m_isUnion;
        std::string m_name;
        std::vector<std::pair<std::shared_ptr<IType>, std::string>> m_types;

    public:

        StructOrUnionDeclaration(std::uint64_t line,
                                 std::uint64_t column,
                                 bool isUnion,
                                 std::string name,
                                 std::vector<std::pair<std::shared_ptr<IType>,
                                                       std::string>>&& types);

        bool isUnion() const;

        const std::string& getName() const;

        const std::vector<std::pair<std::shared_ptr<IType>, std::string>>& getTypes() const;
    };

    /**
     * <EnumDeclaration> ::= <TokenType::EnumKeyword> [ <TokenType::Identifier> ] <TokenType::OpenBrace>
     *                       <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> ]
     *                       { <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> <TokenType::Comma> }
     *                       [ <TokenType::Comma> ] <TokenType::CloseBrace> <TokenType::SemiColon>
     */
    class EnumDeclaration final : public Node<EnumDeclaration>
    {
        std::string m_name;
        std::vector<std::pair<std::string, std::int32_t>> m_values;

    public:

        EnumDeclaration(std::uint64_t line,
                        std::uint64_t column,
                        std::string name,
                        std::vector<std::pair<std::string, std::int32_t>> values);

        const std::string& getName() const;

        const std::vector<std::pair<std::string, std::int32_t>>& getValues() const;
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
    class TypedefDeclaration final : public Node<TypedefDeclaration>
    {
        std::unique_ptr<StructOrUnionDeclaration> m_optionalStructOrUnion;

    public:

        TypedefDeclaration(std::uint64_t line,
                           std::uint64_t column,
                           std::unique_ptr<StructOrUnionDeclaration>&& optionalStructOrUnion = nullptr);

        const std::unique_ptr<StructOrUnionDeclaration>& getOptionalStructOrUnion() const;
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
    class Function final : public Node<Function>
    {
        std::shared_ptr<IType> m_returnType;
        std::string m_name;
        std::vector<std::pair<std::shared_ptr<IType>, std::string>> m_arguments;
        std::uint64_t m_scopeLine;
        std::unique_ptr<BlockStatement> m_block;

    public:

        Function(std::uint64_t line,
                 std::uint64_t column,
                 std::shared_ptr<IType> returnType,
                 std::string name,
                 std::vector<std::pair<std::shared_ptr<IType>,
                                       std::string>> arguments,
                 std::uint64_t scopeLine = 0,
                 std::unique_ptr<BlockStatement>&& blockItems = nullptr);

        const std::shared_ptr<IType>& getReturnType() const;

        const std::string& getName() const;

        const std::vector<std::pair<std::shared_ptr<IType>, std::string>>& getArguments() const;

        uint64_t getScopeLine() const;

        const BlockStatement* getBlockStatement() const;
    };

    /**
     * <GlobalDeclaration> ::= <Type> <TokenType::Identifier> {<TokenType::OpenSquareBracket> <TokenType::Literal>
     *                <TokenType::CloseSquareBracket> } { <TokenType::Comma> <TokenType::Identifier> {<TokenType::OpenSquareBracket> <TokenType::Literal>
     *                <TokenType::CloseSquareBracket> }}
     */
    class GlobalDeclaration final : public Node<GlobalDeclaration>
    {
        std::vector<std::tuple<std::shared_ptr<IType>, std::string, std::unique_ptr<Initializer>>> m_declarations;

    public:

        GlobalDeclaration(std::uint64_t line,
                          std::uint64_t column,
                          std::vector<std::tuple<std::shared_ptr<IType>,
                                                 std::string,
                                                 std::unique_ptr<Initializer>>>&& declarations);

        const std::vector<std::tuple<std::shared_ptr<IType>,
                                     std::string,
                                     std::unique_ptr<Initializer>>>& getDeclarations() const;
    };

    /**
    * <Global> ::= <StructOrUnionDeclaration> | <TypedefDeclaration> | <GlobalDeclaration> | <EnumDeclaration> | <Function>
    */
    class Global final : public Node<Global>
    {
        using variant = std::variant<StructOrUnionDeclaration,
                                     TypedefDeclaration,
                                     GlobalDeclaration,
                                     EnumDeclaration,
                                     Function>;
        variant m_variant;

    public:

        Global(std::uint64_t line, std::uint64_t column, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <program> ::= {<Global>}
     */
    class Program final
    {
        std::vector<Global> m_globals;

    public:

        explicit Program(std::vector<Global>&& globals) noexcept;

        const std::vector<Global>& getGlobals() const;
    };
}

#endif //OPENCLPARSER_SYNTAX_HPP

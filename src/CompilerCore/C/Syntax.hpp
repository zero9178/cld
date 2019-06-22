#ifndef OPENCLPARSER_SYNTAX_HPP
#define OPENCLPARSER_SYNTAX_HPP

#include "Lexer.hpp"

#include <memory>
#include <set>
#include <vector>

namespace OpenCL::Syntax
{
    class Expression;

    class PrimaryExpressionIdentifier;

    class PrimaryExpressionConstant;

    class PrimaryExpressionParenthese;

    class PostFixExpressionPrimaryExpression;

    class PostFixExpressionSubscript;

    class PostFixExpressionIncrement;

    class PostFixExpressionDecrement;

    class PostFixExpressionDot;

    class PostFixExpressionArrow;

    class PostFixExpressionFunctionCall;

    class PostFixExpressionTypeInitializer;

    class AssignmentExpressionAssignment;

    class UnaryExpressionPostFixExpression;

    class UnaryExpressionUnaryOperator;

    class UnaryExpressionSizeOf;

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

    class AssignmentExpressionAssignment;

    class ReturnStatement;

    class ExpressionStatement;

    class IfStatement;

    class SwitchStatement;

    class DefaultStatement;

    class CaseStatement;

    class CompoundStatement;

    class ForStatement;

    class InitializerList;

    class Initializer;

    class Declaration;

    class HeadWhileStatement;

    class FootWhileStatement;

    class BreakStatement;

    class ContinueStatement;

    class EnumSpecifier;

    class FunctionDefinition;

    class TranslationUnit;

    class TypeName;

    class Declarator;

    class EnumDeclaration;

    class StructOrUnionSpecifier;

    class TypeSpecifier;

    class DirectDeclaratorIdentifier;

    class DirectDeclaratorParenthese;

    class DirectDeclaratorNoStaticOrAsterisk;

    class DirectDeclaratorStatic;

    class DirectDeclaratorAsterisk;

    class DirectDeclaratorParentheseParameters;

    class DirectDeclaratorParentheseIdentifiers;

    class AbstractDeclarator;

    class DirectAbstractDeclaratorParenthese;

    class DirectAbstractDeclaratorParameterTypeList;

    class DirectAbstractDeclaratorAsterisk;

    class DirectAbstractDeclaratorAssignmentExpression;

    class Pointer;

    class ParameterTypeList;

    class ParameterList;

    class GotoStatement;

    class LabelStatement;

    class Node
    {
        std::vector<Lexer::Token>::const_iterator m_begin;
        std::vector<Lexer::Token>::const_iterator m_end;

    public:
        Node(std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator end);

        virtual ~Node() = default;

        Node(const Node&) = default;

        Node(Node&&) noexcept = default;

        Node& operator=(const Node&) = default;

        Node& operator=(Node&&) noexcept = default;

        std::vector<OpenCL::Lexer::Token>::const_iterator begin() const;

        std::vector<OpenCL::Lexer::Token>::const_iterator end() const;
    };

    /**
     * <Expression> ::= <AssignmentExpression> { <TokenType::Comma> <AssignmentExpression>}
     */
    class Expression final : public Node
    {
        std::vector<AssignmentExpression> m_assignmentExpressions;

    public:
        Expression(std::vector<Lexer::Token>::const_iterator begin,
                   std::vector<Lexer::Token>::const_iterator end,
                   std::vector<AssignmentExpression> assignmanetExpressions);

        const std::vector<AssignmentExpression>& getAssignmentExpressions() const;
    };

    /**
     * <PrimaryExpressionIdentifier> ::= <TokenType::Identifier>
     */
    class PrimaryExpressionIdentifier final : public Node
    {
        std::string m_identifier;

    public:
        PrimaryExpressionIdentifier(std::vector<Lexer::Token>::const_iterator begin,
                                    std::vector<Lexer::Token>::const_iterator end,
                                    std::string identifier);

        const std::string& getIdentifier() const;
    };

    /**
     * <PrimaryExpressionConstant> ::= <TokenType::Literal>
     *                               | <TokenType::StringLiteral> {<TokenType::StringLiteral>}
     */
    class PrimaryExpressionConstant final : public Node
    {
    public:
        using variant =
        std::variant<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t, float, double, std::string>;

    private:
        variant m_value;

    public:
        PrimaryExpressionConstant(std::vector<Lexer::Token>::const_iterator begin,
                                  std::vector<Lexer::Token>::const_iterator end,
                                  variant value);

        const variant& getValue() const;
    };

    /**
     * <PrimaryExpressionParenthese> ::= <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     */
    class PrimaryExpressionParenthese final : public Node
    {
        Expression m_expression;

    public:
        PrimaryExpressionParenthese(std::vector<Lexer::Token>::const_iterator begin,
                                    std::vector<Lexer::Token>::const_iterator end, Expression&& expression);

        const Expression& getExpression() const;
    };

    /**
     * <PrimaryExpression> ::= <PrimaryExpressionIdentifier>
     *                       | <PrimaryExpressionConstant>
     *                       | <PrimaryExpressionParenthese>
     */
    using PrimaryExpression = std::variant<PrimaryExpressionIdentifier,
                                           PrimaryExpressionConstant,
                                           PrimaryExpressionParenthese>;

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
    using PostFixExpression = std::variant<PostFixExpressionPrimaryExpression,
                                           PostFixExpressionSubscript,
                                           PostFixExpressionDot,
                                           PostFixExpressionFunctionCall,
                                           PostFixExpressionArrow,
                                           PostFixExpressionIncrement,
                                           PostFixExpressionDecrement,
                                           PostFixExpressionTypeInitializer>;

    /**
     * <PostFixExpressionPrimaryExpression> ::= <PrimaryExpression>
     */
    class PostFixExpressionPrimaryExpression final : public Node
    {
        PrimaryExpression m_primaryExpression;

    public:
        PostFixExpressionPrimaryExpression(std::vector<Lexer::Token>::const_iterator begin,
                                           std::vector<Lexer::Token>::const_iterator end,
                                           PrimaryExpression&& primaryExpression);

        const PrimaryExpression& getPrimaryExpression() const;
    };

    /**
     * <PostFixExpressionSubscript> ::= <PostFixExpression> <TokenType::OpenSquareBracket> <Expression>
     * <TokenType::CloseSquareBracket>
     */
    class PostFixExpressionSubscript final : public Node
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        Expression m_expression;

    public:
        PostFixExpressionSubscript(std::vector<Lexer::Token>::const_iterator begin,
                                   std::vector<Lexer::Token>::const_iterator end,
                                   std::unique_ptr<PostFixExpression>&& postFixExpression,
                                   Expression&& expression);

        const PostFixExpression& getPostFixExpression() const;

        const Expression& getExpression() const;
    };

    /**
     * <PostFixExpressionIncrement> ::= <PostFixExpression> <TokenType::Increment>
     */
    class PostFixExpressionIncrement final : public Node
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:
        PostFixExpressionIncrement(std::vector<Lexer::Token>::const_iterator begin,
                                   std::vector<Lexer::Token>::const_iterator end,
                                   std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;
    };

    /**
     * <PostFixExpressionDecrement> ::= <PostFixExpression> <TokenType::Decrement>
     */
    class PostFixExpressionDecrement final : public Node
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;

    public:
        PostFixExpressionDecrement(std::vector<Lexer::Token>::const_iterator begin,
                                   std::vector<Lexer::Token>::const_iterator end,
                                   std::unique_ptr<PostFixExpression>&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;
    };

    /**
     * <PostFixExpressionDot> ::= <PostFixExpression> <TokenType::Dot> <TokenType::Identifier>
     */
    class PostFixExpressionDot final : public Node
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::string m_identifier;

    public:
        PostFixExpressionDot(std::vector<Lexer::Token>::const_iterator begin,
                             std::vector<Lexer::Token>::const_iterator end,
                             std::unique_ptr<PostFixExpression>&& postFixExpression,
                             std::string identifier);

        const PostFixExpression& getPostFixExpression() const;

        const std::string& getIdentifier() const;
    };

    /**
     * <PostFixExpressionArrow> ::= <PostFixExpression> <TokenType::Arrow> <TokenType::Identifier>
     */
    class PostFixExpressionArrow final : public Node
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::string m_identifier;

    public:
        PostFixExpressionArrow(std::vector<Lexer::Token>::const_iterator begin,
                               std::vector<Lexer::Token>::const_iterator end,
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
    class PostFixExpressionFunctionCall final : public Node
    {
        std::unique_ptr<PostFixExpression> m_postFixExpression;
        std::vector<std::unique_ptr<AssignmentExpression>> m_optionalAssignmanetExpressions;

    public:
        PostFixExpressionFunctionCall(
            std::vector<Lexer::Token>::const_iterator begin,
            std::vector<Lexer::Token>::const_iterator end,
            std::unique_ptr<PostFixExpression>&& postFixExpression,
            std::vector<std::unique_ptr<AssignmentExpression>>&& optionalAssignmanetExpressions);

        const PostFixExpression& getPostFixExpression() const;

        const std::vector<std::unique_ptr<AssignmentExpression>>& getOptionalAssignmentExpressions() const;
    };

    /**
     * <PostFixExpressionTypeInitializer> ::= <TokenType::OpenParenthese> <TypeName> <TokenType::CloseParenthese>
     *                                        <TokenType::OpenBrace> <InitializerList> [<TokenType::Comma>]
     * <TokenType::CloseBrace>
     */
    class PostFixExpressionTypeInitializer final : public Node
    {
        std::unique_ptr<TypeName> m_typeName;
        std::unique_ptr<InitializerList> m_initializerList;

    public:
        PostFixExpressionTypeInitializer(std::vector<Lexer::Token>::const_iterator begin,
                                         std::vector<Lexer::Token>::const_iterator end,
                                         TypeName&& typeName,
                                         InitializerList&& initializerList);

        const InitializerList& getInitializerList() const;

        const TypeName& getTypeName() const;
    };

    /**
     * <UnaryExpression> ::= <UnaryExpressionPostFixExpression>
     *                     | <UnaryExpressionUnaryOperator>
     *                     | <UnaryExpressionSizeOf>
     */
    using UnaryExpression = std::variant<UnaryExpressionPostFixExpression,
                                         UnaryExpressionUnaryOperator,
                                         UnaryExpressionSizeOf>;

    /**
     * <UnaryExpressionPostFixExpression> ::= <PostFixExpression>
     */
    class UnaryExpressionPostFixExpression final : public Node
    {
        PostFixExpression m_postFixExpression;

    public:
        UnaryExpressionPostFixExpression(std::vector<Lexer::Token>::const_iterator begin,
                                         std::vector<Lexer::Token>::const_iterator end,
                                         PostFixExpression&& postFixExpression);

        const PostFixExpression& getPostFixExpression() const;
    };

    /**
     * <UnaryExpressionUnaryOperator> ::= <TokenType::Increment> <UnaryExpression>
     *                                  | <TokenType::Decrement> <UnaryExpression>
     *                                  | <TokenType::Ampersand> <UnaryExpression>
     *                                  | <TokenType::Asterisk> <UnaryExpression>
     *                                  | <TokenType::Plus> <UnaryExpression>
     *                                  | <TokenType::Minus> <UnaryExpression>
     *                                  | <TokenType::BitWiseNegation> <UnaryExpression>
     *                                  | <TokenType::LogicalNegation> <UnaryExpression>
     */
    class UnaryExpressionUnaryOperator final : public Node
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
        UnaryExpressionUnaryOperator(std::vector<Lexer::Token>::const_iterator begin,
                                     std::vector<Lexer::Token>::const_iterator end,
                                     UnaryOperator anOperator,
                                     std::unique_ptr<UnaryExpression>&& unaryExpression);

        UnaryOperator getAnOperator() const;

        const UnaryExpression& getUnaryExpression() const;
    };

    /**
     * <UnaryExpressionSizeOf> ::= <TokenType::SizeOfKeyword> <UnaryExpression>
     *                           | <TokenType::SizeOfKeyword> <TokenType::OpenParenthese> <TypeName>
     * <TokenType::CloseParenthese>
     */
    class UnaryExpressionSizeOf final : public Node
    {
        using variant = std::variant<std::unique_ptr<UnaryExpression>, std::unique_ptr<TypeName>>;

        variant m_variant;

    public:
        UnaryExpressionSizeOf(std::vector<Lexer::Token>::const_iterator begin,
                              std::vector<Lexer::Token>::const_iterator end,
                              variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <AssignmentExpression> ::= <UnaryExpression> <AssignmentExpression::AssignOperator> <AssignmentExpression>
     */
    class AssignmentExpressionAssignment final : public Node
    {
        UnaryExpression m_unaryFactor;

    public:
        /**
         * <AssignmentExpression::AssignOperator>
         */
        enum class AssignOperator
        {
            NoOperator,       ///<<TokenType::Assignment>
            PlusAssign,       ///<<TokenType::PlusAssign>
            MinusAssign,      ///<<TokenType::MinusAssign>
            DivideAssign,     ///< TokenType::DivideAssign>
            MultiplyAssign,   ///<<TokenType::MultiplyAssign>
            ModuloAssign,     ///<<TokenType::ModuloAssign>
            LeftShiftAssign,  ///<<TokenType::LeftShiftAssign>
            RightShiftAssign, ///<<TokenType::RightShiftAssign>
            BitAndAssign,     ///<<TokenType::BitAndAssign>
            BitOrAssign,      ///<<TokenType::BitOrAssign>
            BitXorAssign      ///<<TokenType::BitXorAssign>
        };

    private:
        AssignOperator m_assignOperator;
        std::unique_ptr<AssignmentExpression> m_assignmentExpression;

    public:
        AssignmentExpressionAssignment(std::vector<Lexer::Token>::const_iterator begin,
                                       std::vector<Lexer::Token>::const_iterator end,
                                       UnaryExpression&& unaryFactor,
                                       AssignOperator assignOperator,
                                       std::unique_ptr<AssignmentExpression>&& assignmentExpression);

        const UnaryExpression& getUnaryFactor() const;

        AssignOperator getAssignOperator() const;

        const AssignmentExpression& getAssignmentExpression() const;
    };

    /**
     * <TypeQualifier> ::= <TokenType::ConstKeyword> | <TokenType::RestrictKeyword> | <TokenType::VolatileKeyword>
     */
    class TypeQualifier final : public Node
    {
    public:

        enum Qualifier
        {
            Const,
            Restrict,
            Volatile
        };

    private:

        Qualifier m_qualifier;

    public:

        TypeQualifier(std::vector<Lexer::Token>::const_iterator begin,
                      std::vector<Lexer::Token>::const_iterator end,
                      Qualifier qualifier);

        Qualifier getQualifier() const;
    };

    /**
     * <SpecifierQualifier> ::= <TypeSpecifier> | <TypeQualifier>
     */
    using SpecifierQualifier = std::variant<TypeSpecifier, TypeQualifier>;

    /**
     * <TypeName> ::= <SpecifierQualifier> { <SpecifierQualifier> } [ <AbstractDeclarator> ]
     */
    class TypeName final : public Node
    {
        std::vector<SpecifierQualifier> m_specifierQualifiers;
        std::unique_ptr<AbstractDeclarator> m_abstractDeclarator;

    public:
        TypeName(std::vector<Lexer::Token>::const_iterator begin,
                 std::vector<Lexer::Token>::const_iterator end,
                 std::vector<SpecifierQualifier>&& specifierQualifiers,
                 std::unique_ptr<AbstractDeclarator>&& abstractDeclarator);

        const std::vector<SpecifierQualifier>& getSpecifierQualifiers() const;

        const AbstractDeclarator* getAbstractDeclarator() const;
    };

    /**
     * <CastExpression> ::= <UnaryExpression>
     *                    | <TokenType::OpenParenthese> <TypeName> <TokenType::CloseParentheses> <CastExpression>
     */
    class CastExpression final : public Node
    {
        using variant = std::variant<UnaryExpression, std::pair<TypeName, std::unique_ptr<CastExpression>>>;

        variant m_variant;

    public:
        CastExpression(std::vector<Lexer::Token>::const_iterator begin,
                       std::vector<Lexer::Token>::const_iterator end,
                       variant&& variant);

        const variant& getVariant() const;
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
            BinaryMultiply, ///<<TokenType::Multiplication>
            BinaryDivide,   ///<<TokenType::Division>
            BinaryRemainder ///<<TokenType::Modulo>
        };

    private:
        std::vector<std::pair<BinaryDotOperator, CastExpression>> m_optionalCastExpressions;

    public:
        Term(std::vector<Lexer::Token>::const_iterator begin,
             std::vector<Lexer::Token>::const_iterator end,
             CastExpression&& castExpressions,
             std::vector<std::pair<BinaryDotOperator, CastExpression>>&& optionalCastExpressions);

        const CastExpression& getCastExpression() const;

        const std::vector<std::pair<BinaryDotOperator, CastExpression>>& getOptionalCastExpressions() const;
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
            BinaryPlus, ///<<TokenType::Addition>
            BinaryMinus ///<<TokenType::Negation>
        };

    private:
        std::vector<std::pair<BinaryDashOperator, Term>> m_optionalTerms;

    public:
        AdditiveExpression(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator end,
                           Term&& term,
                           std::vector<std::pair<BinaryDashOperator, Term>>&& optionalTerms);

        const Term& getTerm() const;

        const std::vector<std::pair<BinaryDashOperator, Term>>& getOptionalTerms() const;
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
            Right, ///<<TokenType::ShiftRight>
            Left   ///<<TokenType::ShiftLeft>
        };

    private:
        std::vector<std::pair<ShiftOperator, AdditiveExpression>> m_optionalAdditiveExpressions;

    public:
        ShiftExpression(std::vector<Lexer::Token>::const_iterator begin,
                        std::vector<Lexer::Token>::const_iterator end,
                        AdditiveExpression&& additiveExpression,
                        std::vector<std::pair<ShiftOperator, AdditiveExpression>>&& optionalAdditiveExpressions);

        const AdditiveExpression& getAdditiveExpression() const;

        const std::vector<std::pair<ShiftOperator, AdditiveExpression>>& getOptionalAdditiveExpressions() const;
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
            LessThan,          ///<<TokenType::LessThan>
            LessThanOrEqual,   ///< TokenType::LessThanOrEqual>
            GreaterThan,       ///< TokenType::GreaterThan>
            GreaterThanOrEqual ///< TokenType::GreaterThanOrEqual>
        };

    private:
        std::vector<std::pair<RelationalOperator, ShiftExpression>> m_optionalRelationalExpressions;

    public:
        RelationalExpression(
            std::vector<Lexer::Token>::const_iterator begin,
            std::vector<Lexer::Token>::const_iterator end,
            ShiftExpression&& shiftExpression,
            std::vector<std::pair<RelationalOperator, ShiftExpression>>&& optionalRelationalExpressions);

        const ShiftExpression& getShiftExpression() const;

        const std::vector<std::pair<RelationalOperator, ShiftExpression>>& getOptionalShiftExpressions() const;
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
            Equal,   ///<<TokenType::Equal>
            NotEqual ///< TokenType::NotEqual>
        };

    private:
        std::vector<std::pair<EqualityOperator, RelationalExpression>> m_optionalRelationalExpressions;

    public:
        EqualityExpression(
            std::vector<Lexer::Token>::const_iterator begin,
            std::vector<Lexer::Token>::const_iterator end,
            RelationalExpression&& relationalExpression,
            std::vector<std::pair<EqualityOperator, RelationalExpression>>&& optionalRelationalExpressions);

        const RelationalExpression& getRelationalExpression() const;

        const std::vector<std::pair<EqualityOperator, RelationalExpression>>& getOptionalRelationalExpressions() const;
    };

    /**
     * <BitAndExpression> ::= <EqualityExpression> { <TokenType::BitAnd> <EqualityExpression> }
     */
    class BitAndExpression final : public Node
    {
        EqualityExpression m_equalityExpression;
        std::vector<EqualityExpression> m_optionalEqualityExpressions;

    public:
        BitAndExpression(std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end,
                         EqualityExpression&& equalityExpression,
                         std::vector<EqualityExpression>&& optionalEqualityExpressions);

        const EqualityExpression& getEqualityExpression() const;

        const std::vector<EqualityExpression>& getOptionalEqualityExpressions() const;
    };

    /**
     * <BitXorExpression> ::= <BitAndExpression> { <TokenType::BitXor> <BitAndExpression> }
     */
    class BitXorExpression final : public Node
    {
        BitAndExpression m_bitAndExpression;
        std::vector<BitAndExpression> m_optionalBitAndExpressions;

    public:
        BitXorExpression(std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end,
                         BitAndExpression&& bitAndExpression,
                         std::vector<BitAndExpression>&& optionalBitAndExpressions);

        const BitAndExpression& getBitAndExpression() const;

        const std::vector<BitAndExpression>& getOptionalBitAndExpressions() const;
    };

    /**
     * <BitOrExpression> ::= <BitXorExpression> { <TokenType::BitOr> <BitXorExpression> }
     */
    class BitOrExpression final : public Node
    {
        BitXorExpression m_bitXorExpression;
        std::vector<BitXorExpression> m_optionalBitXorExpressions;

    public:
        BitOrExpression(std::vector<Lexer::Token>::const_iterator begin,
                        std::vector<Lexer::Token>::const_iterator end,
                        BitXorExpression&& bitXorExpression,
                        std::vector<BitXorExpression>&& optionalBitXorExpressions);

        const BitXorExpression& getBitXorExpression() const;

        const std::vector<BitXorExpression>& getOptionalBitXorExpressions() const;
    };

    /**
     * <LogicalAndExpression> ::= <BitOrExpression> { <TokenType::LogicAnd> <BitOrExpression> }
     */
    class LogicalAndExpression final : public Node
    {
        BitOrExpression m_bitOrExpression;
        std::vector<BitOrExpression> m_optionalBitOrExpressions;

    public:
        LogicalAndExpression(std::vector<Lexer::Token>::const_iterator begin,
                             std::vector<Lexer::Token>::const_iterator end,
                             BitOrExpression&& equalityExpression,
                             std::vector<BitOrExpression>&& optionalEqualityExpressions);

        const BitOrExpression& getBitOrExpression() const;

        const std::vector<BitOrExpression>& getOptionalBitOrExpressions() const;
    };

    /**
     * <LogicalOrExpression> ::= <LogicalAndExpression> { <TokenType::LogicOr> <LogicalAndExpression> }
     */
    class LogicalOrExpression final : public Node
    {
        LogicalAndExpression m_andExpression;
        std::vector<LogicalAndExpression> m_optionalAndExpressions;

    public:
        LogicalOrExpression(std::vector<Lexer::Token>::const_iterator begin,
                            std::vector<Lexer::Token>::const_iterator end,
                            LogicalAndExpression&& andExpression,
                            std::vector<LogicalAndExpression>&& optionalAndExpressions);

        const LogicalAndExpression& getAndExpression() const;

        const std::vector<LogicalAndExpression>& getOptionalAndExpressions() const;
    };

    /**
     * <ConditionalExpression> ::= <LogicalOrExpression> [ <TokenType::QuestionMark> <Expression> <TokenType::Colon>
     *                             <ConditionalExpression> ]
     *
     * <ConstantExpression> ::= <ConditionalExpression>
     */
    class ConditionalExpression final : public Node
    {
        LogicalOrExpression m_logicalOrExpression;
        std::unique_ptr<Expression> m_optionalExpression;
        std::unique_ptr<ConditionalExpression> m_optionalConditionalExpression;

    public:
        ConditionalExpression(std::vector<Lexer::Token>::const_iterator begin,
                              std::vector<Lexer::Token>::const_iterator end,
                              LogicalOrExpression&& logicalOrExpression,
                              std::unique_ptr<Expression>&& optionalExpression = nullptr,
                              std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression = nullptr);

        const LogicalOrExpression& getLogicalOrExpression() const;

        const Expression* getOptionalExpression() const;

        const ConditionalExpression* getOptionalConditionalExpression() const;
    };

    /**
     * <AssignmentExpression> ::= <AssignmentExpressionAssignment> | <ConditionalExpression>
     */
    class AssignmentExpression final : public Node
    {
        std::variant<AssignmentExpressionAssignment, ConditionalExpression> m_variant;

    public:
        AssignmentExpression(std::vector<Lexer::Token>::const_iterator begin,
                             std::vector<Lexer::Token>::const_iterator end,
                             std::variant<AssignmentExpressionAssignment, ConditionalExpression>&& variant);

        const std::variant<AssignmentExpressionAssignment, ConditionalExpression>& getVariant() const;
    };

    /**
     * <ReturnStatement> ::= <TokenType::ReturnKeyword> <Expression> <TokenType::SemiColon>
     */
    class ReturnStatement final : public Node
    {
        Expression m_expression;

    public:
        ReturnStatement(std::vector<Lexer::Token>::const_iterator begin,
                        std::vector<Lexer::Token>::const_iterator end,
                        Expression&& expression);

        const Expression& getExpression() const;
    };

    /**
     * <ExpressionStatement> ::= [<Expression>] <TokenType::SemiColon>
     */
    class ExpressionStatement final : public Node
    {
        std::unique_ptr<Expression> m_optionalExpression;

    public:
        ExpressionStatement(std::vector<Lexer::Token>::const_iterator begin,
                            std::vector<Lexer::Token>::const_iterator end,
                            std::unique_ptr<Expression>&& optionalExpression = nullptr);

        const Expression* getOptionalExpression() const;

        std::unique_ptr<Expression> moveOptionalExpression();
    };

    /**
     * <LabeledStatement> ::= <LabelStatement> | <CaseStatement> | <DefaultStatement>
     *
     * <SelectionStatement> ::= <IfStatement> | <SwitchStatement>
     *
     * <IterationStatement> ::= <ForStatement> | <ForDeclarationStatement> | <HeadWhileStatement> | <FootWhileStatement>
     *
     * <JumpStatement> ::= <GotoStatement> | <ContinueStatement> | <BreakStatement> | <ReturnStatement>
     *
     * <Statement> ::= <LabeledStatement>
     *               | <CompoundStatement>
     *               | <ExpressionStatement>
     *               | <SelectionStatement>
     *               | <IterationStatement>
     *               | <JumpStatement>
     */
    using Statement = std::variant<ReturnStatement, ExpressionStatement, IfStatement, CompoundStatement, ForStatement,
                                   HeadWhileStatement, FootWhileStatement, BreakStatement,
                                   ContinueStatement, SwitchStatement, DefaultStatement, CaseStatement, GotoStatement,
                                   LabelStatement>;

    /**
     * <IfStatement> ::= <TokenType::IfKeyword> <TokenType::OpenParenthese> <Expression> <TokenType::CloseParenthese>
     *               <Statement> [ <TokenType::ElseKeyword> <Statement> ]
     */
    class IfStatement final : public Node
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_branch;
        std::unique_ptr<Statement> m_elseBranch;

    public:
        IfStatement(std::vector<Lexer::Token>::const_iterator begin,
                    std::vector<Lexer::Token>::const_iterator end,
                    Expression&& expression,
                    std::unique_ptr<Statement>&& branch,
                    std::unique_ptr<Statement>&& elseBranch = nullptr);

        const Expression& getExpression() const;

        const Statement& getBranch() const;

        const Statement* getElseBranch() const;
    };

    /**
     * <SwitchStatement> ::= <TokenType::SwitchKeyword> <TokenType::OpenParenthese> <Expression>
     * <TokenType::CloseParenthese> <Statement>
     */
    class SwitchStatement final : public Node
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:
        SwitchStatement(std::vector<Lexer::Token>::const_iterator begin,
                        std::vector<Lexer::Token>::const_iterator end,
                        Expression&& expression,
                        std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;
    };

    /**
     * <DefaultStatement> ::= <TokenType::DefaultKeyword> <TokenType::Colon> <Statement>
     */
    class DefaultStatement final : public Node
    {
        std::unique_ptr<Statement> m_statement;

    public:
        DefaultStatement(std::vector<Lexer::Token>::const_iterator begin,
                         std::vector<Lexer::Token>::const_iterator end,
                         std::unique_ptr<Statement>&& statement);

        const Statement& getStatement() const;
    };

    /**
     * <CaseStatement> ::= <TokenType::CaseKeyword> <ConstantNonCommaExpression> <TokenType::Colon> <Statement>
     */
    class CaseStatement final : public Node
    {
        using constantVariant =
        std::variant<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t, float, double, void*>;

        constantVariant m_constant;
        std::unique_ptr<Statement> m_statement;

    public:
        CaseStatement(std::vector<Lexer::Token>::const_iterator begin,
                      std::vector<Lexer::Token>::const_iterator end, const constantVariant& constant,
                      std::unique_ptr<Statement>&& statement);

        const constantVariant& getConstant() const;

        const Statement* getStatement() const;
    };

    /**
     * <LabelStatement> ::= <TokenType::Identifier> <TokenType::Colon> <Statement>
     */
    class LabelStatement final : public Node
    {
        std::string m_identifier;

    public:
        LabelStatement(std::vector<Lexer::Token>::const_iterator begin,
                       std::vector<Lexer::Token>::const_iterator end, std::string identifier);
    };

    /**
     * <BlockItem> ::= <Statement> | <Declaration>
     */
    using CompoundItem = std::variant<Statement, Declaration>;

    /**
     * <CompoundStatement> ::= <TokenType::OpenBrace> { <CompoundItem> } <TokenType::CloseBrace>
     */
    class CompoundStatement final : public Node
    {
        std::vector<CompoundItem> m_blockItems;

    public:
        CompoundStatement(std::vector<Lexer::Token>::const_iterator begin,
                          std::vector<Lexer::Token>::const_iterator end, std::vector<CompoundItem>&& blockItems);

        const std::vector<CompoundItem>& getBlockItems() const;
    };

    /**
     * <StorageClassSpecifiers> ::= <TokenType::TypedefKeyword>
     *                            | <TokenType::ExternKeyword>
     *                            | <TokenType::StaticKeyword>
     *                            | <TokenType::AutoKeyword>
     *                            | <TokenType::RegisterKeyword>
     */
    class StorageClassSpecifier final : public Node
    {
    public:

        enum Specifiers
        {
            Typedef,
            Extern,
            Static,
            Auto,
            Register
        };

    private:

        Specifiers m_specifier;

    public:

        StorageClassSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                              std::vector<Lexer::Token>::const_iterator end,
                              Specifiers specifier);

        Specifiers getSpecifier() const;
    };

    /**
     * <FunctionSpecifier> ::= <TokenType::InlineKeyword>
     */
    class FunctionSpecifier final : public Node
    {
    public:

        FunctionSpecifier(const std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                          const std::vector<OpenCL::Lexer::Token>::const_iterator& end);
    };

    /**
     * <DeclarationSpecifier> ::= <StorageClassSpecifier> | <TypeSpecifier> | <TypeQualifier> | <FunctionSpecifier>
     */
    using DeclarationSpecifier = std::variant<StorageClassSpecifier, TypeSpecifier, TypeQualifier, FunctionSpecifier>;

    /**
     * <InitDeclarator> ::= <Declarator> [ <TokenType::Assignment> <Initializer> ]
     *
     * <Declaration> ::= <DeclarationSpecifier> {<DeclarationSpecifier>} [<InitDeclarator>
     *                   { <TokenType::Comma> <InitDeclarator> } ] <TokenType::SemiColon>
     */
    class Declaration final : public Node
    {
        std::vector<DeclarationSpecifier> m_declarationSpecifiers;
        std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> m_initDeclarators;

    public:
        Declaration(
            std::vector<Lexer::Token>::const_iterator begin,
            std::vector<Lexer::Token>::const_iterator end, std::vector<DeclarationSpecifier>&& declarationSpecifiers,
            std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>>&& initDeclarators);

        const std::vector<DeclarationSpecifier>& getDeclarationSpecifiers() const;

        const std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>>&
        getInitDeclarators() const;
    };

    /**
     * <ExpressionOrDeclaration> ::= <Declaration> | [<Expression>] <TokenType::SemiColon>
     *
     * <ForStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParenthese> <ExpressionOrDeclaration>
     *                    [<Expression>] <TokenType::SemiColon> [<Expression>] <TokenType::CloseParenthese> <Statement>
     */
    class ForStatement final : public Node
    {
        std::unique_ptr<Statement> m_statement;
        std::variant<Declaration, std::unique_ptr<Expression>> m_initial;
        std::unique_ptr<Expression> m_controlling;
        std::unique_ptr<Expression> m_post;

    public:
        ForStatement(std::vector<Lexer::Token>::const_iterator begin,
                     std::vector<Lexer::Token>::const_iterator end, std::unique_ptr<Statement>&& statement,
                     std::variant<Declaration, std::unique_ptr<Expression>>&& initial,
                     std::unique_ptr<Expression>&& controlling, std::unique_ptr<Expression>&& post);

        const Statement& getStatement() const;

        const std::variant<Declaration, std::unique_ptr<Expression>>& getInitial() const;

        const Expression* getControlling() const;

        const Expression* getPost() const;
    };

    /**
     * <HeadWhileStatement> ::= <TokenType::WhileKeyword> <TokenType::OpenParenthese> <Expression>
     *                          <TokenType::CloseParenthese> <Statement>
     */
    class HeadWhileStatement final : public Node
    {
        Expression m_expression;
        std::unique_ptr<Statement> m_statement;

    public:
        HeadWhileStatement(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator end, Expression&& expression,
                           std::unique_ptr<Statement>&& statement);

        const Expression& getExpression() const;

        const Statement& getStatement() const;
    };

    /**
     * <FootWhileStatement> ::= <TokenType::DoKeyword> <Statement> <TokenType::WhileKeyword> <Expression>
     * <TokenType::SemiColon>
     */
    class FootWhileStatement final : public Node
    {
        std::unique_ptr<Statement> m_statement;
        Expression m_expression;

    public:
        FootWhileStatement(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator end, std::unique_ptr<Statement>&& statement,
                           Expression&& expression);

        const Statement& getStatement() const;

        const Expression& getExpression() const;
    };

    /**
     * <BreakStatement> ::= <TokenType::BreakKeyword> <TokenType::SemiColon>
     */
    class BreakStatement final : public Node
    {
    public:
        BreakStatement(std::vector<Lexer::Token>::const_iterator begin,
                       std::vector<Lexer::Token>::const_iterator end);
    };

    /**
     * <ContinueStatement> ::= <TokenType::ContinueStatement> <TokenType::SemiColon>
     */
    class ContinueStatement final : public Node
    {
    public:
        ContinueStatement(std::vector<Lexer::Token>::const_iterator begin,
                          std::vector<Lexer::Token>::const_iterator end);
    };

    /**
     * <GotoStatement> ::= <TokenType::GotoStatement> <TokenType::Identifier> <TokenType::SemiColon>
     */
    class GotoStatement final : public Node
    {
        std::string m_identifier;

    public:
        GotoStatement(std::vector<Lexer::Token>::const_iterator begin,
                      std::vector<Lexer::Token>::const_iterator end, std::string identifier);

        const std::string& getIdentifier() const;
    };

    /**
     * <DirectAbstractDeclarator> ::= <DirectAbstractDeclaratorParenthese>
     *                              | <DirectAbstractDeclaratorAssignmentExpression>
     *                              | <DirectAbstractDeclaratorAsterisk>
     *                              | <DirectAbstractDeclaratorParameterTypeList>
     */
    using DirectAbstractDeclarator = std::variant<DirectAbstractDeclaratorParenthese,
                                                  DirectAbstractDeclaratorAssignmentExpression,
                                                  DirectAbstractDeclaratorAsterisk,
                                                  DirectAbstractDeclaratorParameterTypeList>;

    /**
     * <DirectAbstractDeclaratorParenthese> ::= <TokenType::OpenParenthese> <AbstractDeclarator> <TokenType::CloseParenthese>
     */
    class DirectAbstractDeclaratorParenthese final : public Node
    {
        std::unique_ptr<AbstractDeclarator> m_abstractDeclarator;

    public:

        DirectAbstractDeclaratorParenthese(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                           std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                           std::unique_ptr<AbstractDeclarator>&& abstractDeclarator);

        const AbstractDeclarator& getAbstractDeclarator() const;
    };

    /**
     * <DirectAbstractDeclaratorAssignmentExpression> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket>
     * [<AssignmentExpression>] <TokenType::CloseSquareBracket>
     */
    class DirectAbstractDeclaratorAssignmentExpression final : public Node
    {
        std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
        std::unique_ptr<AssignmentExpression> m_assignmentExpression;

    public:
        DirectAbstractDeclaratorAssignmentExpression(
            std::vector<Lexer::Token>::const_iterator begin,
            std::vector<Lexer::Token>::const_iterator end,
            std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
            std::unique_ptr<AssignmentExpression>&& assignmentExpression);

        const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

        const AssignmentExpression* getAssignmentExpression() const;
    };

    /**
     * <DirectAbstractDeclaratorAsterisk> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket> <TokenType::Asterisk>
     *                                        <TokenType::CloseSquareBracket>
     */
    class DirectAbstractDeclaratorAsterisk final : public Node
    {
        std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;

    public:

        DirectAbstractDeclaratorAsterisk(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator);

        const std::unique_ptr<DirectAbstractDeclarator>& getDirectAbstractDeclarator() const;
    };

    /**
     * <DirectAbstractDeclaratorParameterTypeList> ::= [<DirectAbstractDeclarator>] <TokenType::OpenParenthese> [
     * <ParameterTypeList>] <TokenType::CloseParenthese>
     */
    class DirectAbstractDeclaratorParameterTypeList final : public Node
    {
        std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
        std::unique_ptr<ParameterTypeList> m_parameterTypeList;

    public:
        DirectAbstractDeclaratorParameterTypeList(std::vector<Lexer::Token>::const_iterator begin,
                                                  std::vector<Lexer::Token>::const_iterator end,
                                                  std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
                                                  std::unique_ptr<ParameterTypeList>&& parameterTypeList);

        const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

        const ParameterTypeList* getParameterTypeList() const;
    };

    /**
     * <AbstractDeclarator> ::= { <Pointer> } <DirectAbstractDeclarator>
     */
    class AbstractDeclarator final : public Node
    {
        std::vector<Pointer> m_pointers;
        DirectAbstractDeclarator m_directAbstractDeclarator;

    public:
        AbstractDeclarator(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator end, std::vector<Pointer>&& pointers,
                           DirectAbstractDeclarator&& directAbstractDeclarator);

        const std::vector<Pointer>& getPointers() const;

        const DirectAbstractDeclarator& getDirectAbstractDeclarator() const;
    };

    /**
     * <ParameterDeclaration> ::= <DeclarationSpecifier> { <DeclarationSpecifier> } <Declarator>
     *                          | <DeclarationSpecifier> { <DelcarationSpecifier> } [ <AbstractDeclarator> ]
     */
    using ParameterDeclaration =
    std::pair<std::vector<DeclarationSpecifier>,
              std::variant<std::unique_ptr<Declarator>, std::unique_ptr<AbstractDeclarator>>>;

    /**
     * <ParameterList> ::= <ParameterDeclaration> { <TokenType::Comma> <ParameterDeclaration> }
     */
    class ParameterList final : public Node
    {
    private:
        std::vector<ParameterDeclaration> m_parameterList;

    public:
        ParameterList(std::vector<Lexer::Token>::const_iterator begin,
                      std::vector<Lexer::Token>::const_iterator end, std::vector<ParameterDeclaration>&& parameterList);

        const std::vector<ParameterDeclaration>& getParameterDeclarations() const;
    };

    /**
     * <ParameterTypeList> ::= <ParameterList> [ <TokenType::Comma> <TokenType::Ellipse> ]
     */
    class ParameterTypeList final : public Node
    {
        ParameterList m_parameterList;
        bool m_hasEllipse;

    public:
        ParameterTypeList(std::vector<Lexer::Token>::const_iterator begin,
                          std::vector<Lexer::Token>::const_iterator end,
                          ParameterList&& parameterList,
                          bool hasEllipse);

        const ParameterList& getParameterList() const;

        bool hasEllipse() const;
    };

    /**
     * <DirectDeclarator> ::= <DirectDeclaratorIdentifier>
     *                      | <DirectDeclaratorParenthese>
     *                      | <DirectDeclaratorNoStaticOrAsterisk>
     *                      | <DirectDeclaratorStatic>
     *                      | <DirectDeclaratorAsterisk>
     *                      | <DirectDeclaratorParentheseParameters>
     *                      | <DirectDeclaratorParentheseIdentifier>
     */
    using DirectDeclarator = std::variant<DirectDeclaratorIdentifier,
                                          DirectDeclaratorParenthese,
                                          DirectDeclaratorNoStaticOrAsterisk,
                                          DirectDeclaratorStatic,
                                          DirectDeclaratorAsterisk,
                                          DirectDeclaratorParentheseParameters,
                                          DirectDeclaratorParentheseIdentifiers>;

    /**
     * <DirectDeclaratorIdentifier> ::= <TokenType::Identifier>
     */
    class DirectDeclaratorIdentifier final : public Node
    {
        std::string m_identifier;
        std::vector<Lexer::Token>::const_iterator m_identifierLoc;

    public:

        DirectDeclaratorIdentifier(std::vector<Lexer::Token>::const_iterator begin,
                                   std::vector<Lexer::Token>::const_iterator end,
                                   std::string identifier,
                                   std::vector<Lexer::Token>::const_iterator identifierLoc);

        const std::string& getIdentifier() const;

        std::vector<OpenCL::Lexer::Token>::const_iterator getIdentifierLoc() const;
    };

    /**
     * <DirectDeclaratorParenthese> ::= <TokenType::OpenParenthese> <DirectDeclarator> <TokenType::CloseParenthese>
     */
    class DirectDeclaratorParenthese final : public Node
    {
        std::unique_ptr<Declarator> m_declarator;

    public:

        DirectDeclaratorParenthese(std::vector<Lexer::Token>::const_iterator begin,
                                   std::vector<Lexer::Token>::const_iterator end,
                                   std::unique_ptr<Declarator>&& declarator);

        const Declarator& getDeclarator() const;
    };

    /**
     * <DirectDeclaratorParentheseParameters> ::= <DirectDeclarator> <TokenType::OpenParenthese> <ParameterTypeList>
     * <TokenType::CloseParenthese>
     */
    class DirectDeclaratorParentheseParameters final : public Node
    {
        std::unique_ptr<DirectDeclarator> m_directDeclarator;
        ParameterTypeList m_parameterTypeList;

    public:
        DirectDeclaratorParentheseParameters(std::vector<Lexer::Token>::const_iterator begin,
                                             std::vector<Lexer::Token>::const_iterator end,
                                             DirectDeclarator&& directDeclarator,
                                             ParameterTypeList&& parameterTypeList);

        const DirectDeclarator& getDirectDeclarator() const;

        const ParameterTypeList& getParameterTypeList() const;
    };

    /**
     * <DirectDeclaratorParentheseIdentifiers> ::= <DirectDeclarator> <TokenType::OpenParenthese> [
     * <TokenType::Identifier> {<TokenType::Comma> <TokenType::Identifier>}] <TokenType::CloseParenthese>
     */
    class DirectDeclaratorParentheseIdentifiers final : public Node
    {
        std::unique_ptr<DirectDeclarator> m_directDeclarator;
        std::vector<std::pair<std::string, std::vector<Lexer::Token>::const_iterator>> m_identifiers;

    public:
        DirectDeclaratorParentheseIdentifiers(std::vector<Lexer::Token>::const_iterator begin,
                                              std::vector<Lexer::Token>::const_iterator end,
                                              DirectDeclarator&& directDeclarator,
                                              std::vector<std::pair<std::string,
                                                                    std::vector<Lexer::Token>::const_iterator>>&& identifiers);

        const DirectDeclarator& getDirectDeclarator() const;

        const std::vector<std::pair<std::string, std::vector<Lexer::Token>::const_iterator>>& getIdentifiers() const;
    };

    /**
     * <DirectDeclaratorAsterisk> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> {<TypeQualifier> }
     * <TokenType::Asterisk> <TokenType::CloseSquareBracket>
     */
    class DirectDeclaratorAsterisk final : public Node
    {
        std::unique_ptr<DirectDeclarator> m_directDeclarator;
        std::vector<TypeQualifier> m_typeQualifiers;

    public:
        DirectDeclaratorAsterisk(std::vector<Lexer::Token>::const_iterator begin,
                                 std::vector<Lexer::Token>::const_iterator end, DirectDeclarator&& directDeclarator,
                                 std::vector<TypeQualifier>&& typeQualifiers);

        const DirectDeclarator& getDirectDeclarator() const;

        const std::vector<TypeQualifier>& getTypeQualifiers() const;
    };

    /**
     * <DirectDeclaratorNoStaticOrAsterisk> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> {<TypeQualifier>}
     * [<AssignmentExpression>] <TokenType::CloseSquareBracket>
     */
    class DirectDeclaratorNoStaticOrAsterisk final : public Node
    {
        std::unique_ptr<DirectDeclarator> m_directDeclarator;
        std::vector<TypeQualifier> m_typeQualifiers;
        std::unique_ptr<AssignmentExpression> m_assignmentExpression;

    public:
        DirectDeclaratorNoStaticOrAsterisk(std::vector<Lexer::Token>::const_iterator begin,
                                           std::vector<Lexer::Token>::const_iterator end,
                                           std::unique_ptr<DirectDeclarator>&& directDeclarator,
                                           std::vector<TypeQualifier>&& typeQualifiers,
                                           std::unique_ptr<AssignmentExpression>&& assignmentExpression);

        const DirectDeclarator& getDirectDeclarator() const;

        const std::vector<TypeQualifier>& getTypeQualifiers() const;

        const std::unique_ptr<AssignmentExpression>& getAssignmentExpression() const;
    };

    /**
     * <DirectDeclaratorStatic> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> <TokenType::StaticKeyword>
     * {<TypeQualifier>} <AssignmentExpression> <TokenType::CloseSquareBracket> | <DirectDeclarator>
     * <TokenType::OpenSquareBracket> <TypeQualifier> {<TypeQualifier>} <TokenType::StaticKeyword>
     * <AssignmentExpression> <TokenType::CloseSquareBracket>
     */
    class DirectDeclaratorStatic final : public Node
    {
        std::unique_ptr<DirectDeclarator> m_directDeclarator;
        std::vector<TypeQualifier> m_typeQualifiers;
        AssignmentExpression m_assignmentExpression;

    public:
        DirectDeclaratorStatic(std::vector<Lexer::Token>::const_iterator begin,
                               std::vector<Lexer::Token>::const_iterator end,
                               std::unique_ptr<DirectDeclarator>&& directDeclarator,
                               std::vector<TypeQualifier>&& typeQualifiers,
                               AssignmentExpression&& assignmentExpression);

        const DirectDeclarator& getDirectDeclarator() const;

        const std::vector<TypeQualifier>& getTypeQualifiers() const;

        const AssignmentExpression& getAssignmentExpression() const;
    };

    /**
     * <Declarator> ::= { <Pointer> } <DirectDeclarator>
     */
    class Declarator final : public Node
    {
        std::vector<Pointer> m_pointers;
        DirectDeclarator m_directDeclarator;

    public:
        Declarator(std::vector<Lexer::Token>::const_iterator begin,
                   std::vector<Lexer::Token>::const_iterator end, std::vector<Pointer>&& pointers,
                   DirectDeclarator&& directDeclarator);

        const std::vector<Pointer>& getPointers() const;

        const DirectDeclarator& getDirectDeclarator() const;
    };

    /**
     * <StructOrUnion> ::= <TokenType::StructKeyword> | <TokenType::UnionKeyword>
     *
     * <StructOrUnionSpecifier> ::= <StructOrUnion> [ <TokenType::Identifier> ]
     *                              <TokenType::OpenBrace> <StructDeclaration> { <StructDeclaration> }
     * <TokenType::CloseBrace> | <StructOrUnion> <TokenType::Identifier>
     */
    class StructOrUnionSpecifier final : public Node
    {
        bool m_isUnion;
        std::string m_identifier;

    public:
        /**
         *
         * <StructDeclarator> ::= <Declarator> | [<Declarator>] <TokenType::Colon> <ConstantExpression>
         *
         * <StructDeclaration> ::= <SpecifierQualifier> { <SpecifierQualifier> }
         *                         <StructDeclarator> { <StructDeclarator> } <TokenType::SemiColon>
         */
        struct StructDeclaration
        {
            std::vector<SpecifierQualifier> specifierQualifiers;
            std::vector<std::pair<std::unique_ptr<Declarator>, std::int64_t>> structDeclarators;
        };

    private:
        std::vector<StructDeclaration> m_structDeclarations;

    public:
        StructOrUnionSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                               std::vector<Lexer::Token>::const_iterator end, bool isUnion, std::string identifier,
                               std::vector<StructDeclaration>&& structDeclarations);

        bool isUnion() const;

        const std::string& getIdentifier() const;

        const std::vector<StructDeclaration>& getStructDeclarations() const;
    };

    /**
     * <EnumDeclaration> ::= <TokenType::EnumKeyword> [ <TokenType::Identifier> ] <TokenType::OpenBrace>
     *                       <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> ]
     *                       { <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> <TokenType::Comma>
     * } [ <TokenType::Comma> ] <TokenType::CloseBrace>
     */
    class EnumDeclaration final : public Node
    {
        std::string m_name;
        std::vector<std::pair<std::string, std::int32_t>> m_values;

    public:
        EnumDeclaration(std::vector<Lexer::Token>::const_iterator begin,
                        std::vector<Lexer::Token>::const_iterator end, std::string name,
                        std::vector<std::pair<std::string, std::int32_t>> values);

        const std::string& getName() const;

        const std::vector<std::pair<std::string, std::int32_t>>& getValues() const;
    };

    /**
     * <EnumSpecifier> ::= <EnumDeclaration> | <TokenType::EnumKeyword> <TokenType::Identifier>
     */
    class EnumSpecifier final : public Node
    {
        using variant = std::variant<EnumDeclaration, std::string>;

        variant m_variant;

    public:
        EnumSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                      std::vector<Lexer::Token>::const_iterator end, variant&& variant);

        [[nodiscard]] const variant& getVariant() const;
    };

    /**
     * <TypeSpecifier> ::= <TokenType::VoidKeyword>
     *                   | <TokenType::CharKeyword>
     *                   | <TokenType::ShortKeyword>
     *                   | <TokenType::IntKeyword>
     *                   | <TokenType::LongKeyword>
     *                   | <TokenType::FloatKeyword>
     *                   | <TokenType::DoubleKeyword>
     *                   | <TokenType::SignedKeyword>
     *                   | <TokenType::UnsignedKeyword>
     *                   | <StructOrUnionSpecifier>
     *                   | <EnumSpecifier>
     *                   | <TokenType::Identifier>
     */
    class TypeSpecifier final : public Node
    {
    public:
        enum class PrimitiveTypeSpecifier
        {
            Void,
            Char,
            Short,
            Int,
            Long,
            Float,
            Double,
            Signed,
            Unsigned,
        };

    private:
        using variant = std::variant<PrimitiveTypeSpecifier, std::unique_ptr<StructOrUnionSpecifier>,
                                     std::unique_ptr<EnumSpecifier>, std::string>;

        variant m_variant;

    public:
        TypeSpecifier(std::vector<Lexer::Token>::const_iterator begin,
                      std::vector<Lexer::Token>::const_iterator end, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <Pointer> ::= <TokenType::Asterisk> { <TypeQualifier> }
     */
    class Pointer final : public Node
    {
        std::vector<TypeQualifier> m_typeQualifiers;

    public:
        Pointer(std::vector<Lexer::Token>::const_iterator begin,
                std::vector<Lexer::Token>::const_iterator end, std::vector<TypeQualifier>&& typeQualifiers);

        const std::vector<TypeQualifier>& getTypeQualifiers() const;
    };

    /**
     * <Designation> ::= <DesignatorList> <TokenType::Assignment>
     *
     * <InitializerList> ::= [<Designation>] Initializer { <TokenType::Comma> [<Designation>] Initializer }
     */
    class InitializerList final : public Node
    {
    public:
        /**
         * <Designator> ::= <TokenType::OpenSquareBracket> <ConstantExpression> <TokenType::CloseSquareBracket>]
         *                | <TokenType::Dot> <TokenType::Identifier>
         */
        using Designator = std::variant<std::size_t, std::string>;

        /**
         * <DesignatorList> ::= <Designator> { <Designator> }
         */
        using DesignatorList = std::vector<Designator>;

        using vector = std::vector<std::pair<Initializer, DesignatorList>>;

    private:
        vector m_nonCommaExpressionsAndBlocks;

    public:
        InitializerList(std::vector<Lexer::Token>::const_iterator begin,
                        std::vector<Lexer::Token>::const_iterator end, vector&& nonCommaExpressionsAndBlocks);

        const vector& getNonCommaExpressionsAndBlocks() const;
    };

    /**
     * <Initializer> ::= <AssignmentExpression> | <TokenType::OpenBrace> <InitializerList> [<TokenType::Comma>]
     * <TokenType::CloseBrace>
     */
    class Initializer final : public Node
    {
        using variant = std::variant<AssignmentExpression, InitializerList>;
        variant m_variant;

    public:
        Initializer(std::vector<Lexer::Token>::const_iterator begin,
                    std::vector<Lexer::Token>::const_iterator end, variant&& variant);

        const variant& getVariant() const;
    };

    /**
     * <FunctionDefinition> ::= <DeclarationSpecifier> {<DeclarationSpecifier>} <Declarator> { <Declaration> }
     * <CompoundStatement>
     */
    class FunctionDefinition final : public Node
    {
        std::vector<DeclarationSpecifier> m_declarationSpecifiers;
        Declarator m_declarator;
        std::vector<Declaration> m_declarations;
        CompoundStatement m_compoundStatement;

    public:
        FunctionDefinition(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator end,
                           std::vector<DeclarationSpecifier>&& declarationSpecifiers, Declarator&& declarator,
                           std::vector<Declaration>&& declarations, CompoundStatement&& compoundStatement);

        const std::vector<DeclarationSpecifier>& getDeclarationSpecifiers() const;

        const Declarator& getDeclarator() const;

        const std::vector<Declaration>& getDeclarations() const;

        const CompoundStatement& getCompoundStatement() const;
    };

    /**
     * <ExternalDeclaration> ::= <FunctionDefinition> | <Declaration>
     */
    using ExternalDeclaration = std::variant<Declaration, FunctionDefinition>;

    /**
     * <TranslationUnit> ::= <ExternalDeclaration> {<ExternalDeclaration>}
     */
    class TranslationUnit final
    {
        std::vector<ExternalDeclaration> m_globals;

    public:
        explicit TranslationUnit(std::vector<ExternalDeclaration>&& globals) noexcept;

        const std::vector<ExternalDeclaration>& getGlobals() const;
    };

    template <class...T>
    Node& nodeFromNodeDerivedVariant(std::variant<T...>& variant)
    {
        static_assert(std::conjunction_v<std::is_base_of<Node, T>...>,
                      "All alternatives in variant must derive of Node");
        return std::visit([](auto&& value) -> Node&
                          { return value; }, variant);
    }

    template <class...T>
    const Node& nodeFromNodeDerivedVariant(const std::variant<T...>& variant)
    {
        static_assert(std::conjunction_v<std::is_base_of<Node, T>...>,
                      "All alternatives in variant must derive of Node");
        return std::visit([](auto&& value) -> const Node&
                          { return value; }, variant);
    }
} // namespace OpenCL::Syntax

#endif // OPENCLPARSER_SYNTAX_HPP

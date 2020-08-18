#pragma once

#include <memory>
#include <optional>
#include <set>
#include <vector>

#include "Lexer.hpp"

/**
 * This uses EBNF syntax meaning that: { <A> } means 0 to infinite amount of times and [ <A> ] 0 or 1 times
 */

namespace cld::Syntax
{
class Expression;

class PrimaryExpressionIdentifier;

class PrimaryExpressionConstant;

class PrimaryExpressionParentheses;

class PostFixExpressionPrimaryExpression;

class PostFixExpressionSubscript;

class PostFixExpressionIncrement;

class PostFixExpressionDecrement;

class PostFixExpressionDot;

class PostFixExpressionArrow;

class PostFixExpressionFunctionCall;

class PostFixExpressionTypeInitializer;

class UnaryExpressionPostFixExpression;

class UnaryExpressionUnaryOperator;

class UnaryExpressionSizeOf;

class UnaryExpressionDefined;

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

using ConstantExpression = ConditionalExpression;

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

class DirectDeclaratorParentheses;

class DirectDeclaratorNoStaticOrAsterisk;

class DirectDeclaratorStatic;

class DirectDeclaratorAsterisk;

class DirectDeclaratorParenthesesParameters;

class DirectDeclaratorParenthesesIdentifiers;

class AbstractDeclarator;

class DirectAbstractDeclaratorParentheses;

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
    Lexer::CTokenIterator m_begin;
    Lexer::CTokenIterator m_end;

public:
    Node(Lexer::CTokenIterator begin, Lexer::CTokenIterator end);

    ~Node() = default;

    Node(const Node&) = delete;

    Node(Node&&) noexcept = default;

    Node& operator=(const Node&) = delete;

    Node& operator=(Node&&) noexcept = default;

    [[nodiscard]] Lexer::CTokenIterator begin() const;

    [[nodiscard]] Lexer::CTokenIterator end() const;
};

/**
 * <Expression> ::= <AssignmentExpression> { <TokenType::Comma> <AssignmentExpression>}
 */
class Expression final : public Node
{
    std::vector<AssignmentExpression> m_assignmentExpressions;

public:
    Expression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
               std::vector<AssignmentExpression> assignmentExpressions);

    [[nodiscard]] const std::vector<AssignmentExpression>& getAssignmentExpressions() const;
};

/**
 * <PrimaryExpressionIdentifier> ::= <TokenType::Identifier>
 */
class PrimaryExpressionIdentifier final : public Node
{
    Lexer::CTokenIterator m_identifier;

public:
    PrimaryExpressionIdentifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                Lexer::CTokenIterator identifier);

    [[nodiscard]] Lexer::CTokenIterator getIdentifier() const;
};

/**
 * <PrimaryExpressionConstant> ::= <TokenType::Literal>
 *                               | <TokenType::StringLiteral> {<TokenType::StringLiteral>}
 */
class PrimaryExpressionConstant final : public Node
{
public:
    using Variant = std::variant<llvm::APSInt, llvm::APFloat, std::string, Lexer::NonCharString>;

private:
    Variant m_value;
    Lexer::CToken::Type m_type;

public:
    PrimaryExpressionConstant(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Variant value,
                              Lexer::CToken::Type type);

    [[nodiscard]] const Variant& getValue() const;

    [[nodiscard]] Lexer::CToken::Type getType() const;
};

/**
 * <PrimaryExpressionParentheses> ::= <TokenType::OpenParentheses> <Expression> <TokenType::CloseParentheses>
 */
class PrimaryExpressionParentheses final : public Node
{
    Expression m_expression;

public:
    PrimaryExpressionParentheses(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Expression&& expression);

    [[nodiscard]] const Expression& getExpression() const;
};

/**
 * <PrimaryExpression> ::= <PrimaryExpressionIdentifier>
 *                       | <PrimaryExpressionConstant>
 *                       | <PrimaryExpressionParentheses>
 */
using PrimaryExpression =
    std::variant<PrimaryExpressionIdentifier, PrimaryExpressionConstant, PrimaryExpressionParentheses>;

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
using PostFixExpression =
    std::variant<PostFixExpressionPrimaryExpression, PostFixExpressionSubscript, PostFixExpressionDot,
                 PostFixExpressionFunctionCall, PostFixExpressionArrow, PostFixExpressionIncrement,
                 PostFixExpressionDecrement, PostFixExpressionTypeInitializer>;

/**
 * <PostFixExpressionPrimaryExpression> ::= <PrimaryExpression>
 */
class PostFixExpressionPrimaryExpression final : public Node
{
    PrimaryExpression m_primaryExpression;

public:
    PostFixExpressionPrimaryExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                       PrimaryExpression&& primaryExpression);

    [[nodiscard]] const PrimaryExpression& getPrimaryExpression() const;
};

/**
 * <PostFixExpressionSubscript> ::= <PostFixExpression> <TokenType::OpenSquareBracket> <Expression>
 * <TokenType::CloseSquareBracket>
 */
class PostFixExpressionSubscript final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_openBracket;
    Expression m_expression;
    Lexer::CTokenIterator m_closeBracket;

public:
    PostFixExpressionSubscript(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                               std::unique_ptr<PostFixExpression>&& postFixExpression,
                               Lexer::CTokenIterator openBracket, Expression&& expression,
                               Lexer::CTokenIterator closeBracket);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] const Expression& getExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getOpenBracket() const
    {
        return m_openBracket;
    }

    [[nodiscard]] Lexer::CTokenIterator getCloseBracket() const
    {
        return m_closeBracket;
    }
};

/**
 * <PostFixExpressionIncrement> ::= <PostFixExpression> <TokenType::Increment>
 */
class PostFixExpressionIncrement final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_incrementToken;

public:
    PostFixExpressionIncrement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                               std::unique_ptr<PostFixExpression>&& postFixExpression,
                               Lexer::CTokenIterator incrementToken);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getIncrementToken() const;
};

/**
 * <PostFixExpressionDecrement> ::= <PostFixExpression> <TokenType::Decrement>
 */
class PostFixExpressionDecrement final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_decrementToken;

public:
    PostFixExpressionDecrement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                               std::unique_ptr<PostFixExpression>&& postFixExpression,
                               Lexer::CTokenIterator decrementToken);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getDecrementToken() const;
};

/**
 * <PostFixExpressionDot> ::= <PostFixExpression> <TokenType::Dot> <TokenType::Identifier>
 */
class PostFixExpressionDot final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_identifier;

public:
    PostFixExpressionDot(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                         std::unique_ptr<PostFixExpression>&& postFixExpression, Lexer::CTokenIterator identifier);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getIdentifier() const;
};

/**
 * <PostFixExpressionArrow> ::= <PostFixExpression> <TokenType::Arrow> <TokenType::Identifier>
 */
class PostFixExpressionArrow final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_identifier;

public:
    PostFixExpressionArrow(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                           std::unique_ptr<PostFixExpression>&& postFixExpression, Lexer::CTokenIterator identifier);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getIdentifier() const;
};

/**
 * <PostFixExpressionFunctionCall> ::= <PostFixExpression> <TokenType::OpenParentheses> <NonCommaExpression>
 *                                     { <TokenType::Comma> <NonCommaExpression> }
 *                                     <TokenType::CloseParentheses>
 */
class PostFixExpressionFunctionCall final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_openParentheses;
    std::vector<std::unique_ptr<AssignmentExpression>> m_optionalAssignmentExpressions;
    Lexer::CTokenIterator m_closeParentheses;

public:
    PostFixExpressionFunctionCall(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                  std::unique_ptr<PostFixExpression>&& postFixExpression,
                                  Lexer::CTokenIterator openParentheses,
                                  std::vector<std::unique_ptr<AssignmentExpression>>&& optionalAssignmentExpressions,
                                  Lexer::CTokenIterator closeParentheses);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const std::vector<std::unique_ptr<AssignmentExpression>>& getOptionalAssignmentExpressions() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;
};

/**
 * <PostFixExpressionTypeInitializer> ::= <TokenType::OpenParentheses> <TypeName> <TokenType::CloseParentheses>
 *                                        <TokenType::OpenBrace> <InitializerList> [<TokenType::Comma>]
 * <TokenType::CloseBrace>
 */
class PostFixExpressionTypeInitializer final : public Node
{
    std::unique_ptr<TypeName> m_typeName;
    std::unique_ptr<InitializerList> m_initializerList;

public:
    PostFixExpressionTypeInitializer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, TypeName&& typeName,
                                     InitializerList&& initializerList);

    [[nodiscard]] const InitializerList& getInitializerList() const;

    [[nodiscard]] const TypeName& getTypeName() const;
};

/**
 * <UnaryExpression> ::= <UnaryExpressionPostFixExpression>
 *                     | <UnaryExpressionUnaryOperator>
 *                     | <UnaryExpressionSizeOf>
 *                 [PP]| <UnaryExpressionDefined>
 */
using UnaryExpression = std::variant<UnaryExpressionPostFixExpression, UnaryExpressionUnaryOperator,
                                     UnaryExpressionSizeOf, UnaryExpressionDefined>;

/**
 * <UnaryExpressionPostFixExpression> ::= <PostFixExpression>
 */
class UnaryExpressionPostFixExpression final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;

public:
    UnaryExpressionPostFixExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                     PostFixExpression&& postFixExpression);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;
};

/**
 * <UnaryExpressionUnaryOperator> ::= <TokenType::Increment> <CastExpression>
 *                                  | <TokenType::Decrement> <CastExpression>
 *                                  | <TokenType::Ampersand> <CastExpression>
 *                                  | <TokenType::Asterisk> <CastExpression>
 *                                  | <TokenType::Plus> <CastExpression>
 *                                  | <TokenType::Minus> <CastExpression>
 *                                  | <TokenType::BitWiseNegation> <CastExpression>
 *                                  | <TokenType::LogicalNegation> <CastExpression>
 */
class UnaryExpressionUnaryOperator final : public Node
{
public:
    enum class UnaryOperator : std::uint8_t
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
    std::unique_ptr<CastExpression> m_castExpression;
    UnaryOperator m_operator;
    Lexer::CTokenIterator m_unaryToken;

public:
    UnaryExpressionUnaryOperator(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, UnaryOperator anOperator,
                                 Lexer::CTokenIterator unaryToken, std::unique_ptr<CastExpression>&& unaryExpression);

    [[nodiscard]] UnaryOperator getOperator() const;

    [[nodiscard]] Lexer::CTokenIterator getUnaryToken() const;

    [[nodiscard]] const CastExpression& getCastExpression() const;
};

/**
 * <UnaryExpressionSizeOf> ::= <TokenType::SizeOfKeyword> <UnaryExpression>
 *                           | <TokenType::SizeOfKeyword> <TokenType::OpenParentheses> <TypeName>
 * <TokenType::CloseParentheses>
 */
class UnaryExpressionSizeOf final : public Node
{
    using variant = std::variant<std::unique_ptr<UnaryExpression>, std::unique_ptr<TypeName>>;

    Lexer::CTokenIterator m_sizeOfToken;
    variant m_variant;

public:
    UnaryExpressionSizeOf(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator sizeOfToken,
                          variant&& variant);

    [[nodiscard]] const variant& getVariant() const;

    [[nodiscard]] Lexer::CTokenIterator getSizeOfToken() const
    {
        return m_sizeOfToken;
    }
};

/**
 * <UnaryExpressionDefined> ::= <TokenType::DefinedKeyword> <TokenType::Identifier>
 *                            | <TokenType::DefinedKeyword> <TokenType::OpenParentheses> <TokenType::Identifier>
 * <TokenType::CloseParentheses>
 */
class UnaryExpressionDefined final : public Node
{
    std::string_view m_identifier;

public:
    UnaryExpressionDefined(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::string_view identifier);

    [[nodiscard]] std::string_view getIdentifier() const;
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
    TypeQualifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Qualifier qualifier);

    [[nodiscard]] Qualifier getQualifier() const;
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
    TypeName(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
             std::vector<SpecifierQualifier>&& specifierQualifiers,
             std::unique_ptr<AbstractDeclarator>&& abstractDeclarator);

    [[nodiscard]] const std::vector<SpecifierQualifier>& getSpecifierQualifiers() const;

    [[nodiscard]] const AbstractDeclarator* getAbstractDeclarator() const;
};

/**
 * <CastExpression> ::= <UnaryExpression>
 *                    | <TokenType::OpenParentheses> <TypeName> <TokenType::CloseParentheses> <CastExpression>
 */
class CastExpression final : public Node
{
public:
    struct CastVariant
    {
        Lexer::CTokenIterator openParentheses;
        TypeName typeName;
        Lexer::CTokenIterator closeParentheses;
        std::unique_ptr<CastExpression> cast;
    };

    using Variant = std::variant<UnaryExpression, CastVariant>;

private:
    std::unique_ptr<Variant> m_variant;

public:
    CastExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Variant&& variant);

    [[nodiscard]] const Variant& getVariant() const;
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
    enum BinaryDotOperator
    {
        BinaryMultiply, ///<<TokenType::Multiplication>
        BinaryDivide,   ///<<TokenType::Division>
        BinaryModulo    ///<<TokenType::Modulo>
    };

    struct Operand
    {
        BinaryDotOperator dotOperator;
        Lexer::CTokenIterator operatorToken;
        CastExpression expression;
    };

private:
    std::vector<Operand> m_optionalCastExpressions;

public:
    Term(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, CastExpression&& castExpressions,
         std::vector<Operand>&& optionalCastExpressions);

    [[nodiscard]] const CastExpression& getCastExpression() const;

    [[nodiscard]] const std::vector<Operand>& getOptionalCastExpressions() const;
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
    enum BinaryDashOperator
    {
        BinaryPlus, ///<<TokenType::Addition>
        BinaryMinus ///<<TokenType::Negation>
    };

    struct Operand
    {
        BinaryDashOperator dashOperator;
        Lexer::CTokenIterator operatorToken;
        Term expression;
    };

private:
    std::vector<Operand> m_optionalTerms;

public:
    AdditiveExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Term&& term,
                       std::vector<Operand>&& optionalTerms);

    [[nodiscard]] const Term& getTerm() const;

    [[nodiscard]] const std::vector<Operand>& getOptionalTerms() const;
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
    enum ShiftOperator
    {
        Right, ///<<TokenType::ShiftRight>
        Left   ///<<TokenType::ShiftLeft>
    };

    struct Operand
    {
        ShiftOperator shiftOperator;
        Lexer::CTokenIterator operatorToken;
        AdditiveExpression additiveExpression;
    };

private:
    std::vector<Operand> m_optionalAdditiveExpressions;

public:
    ShiftExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, AdditiveExpression&& additiveExpression,
                    std::vector<Operand>&& optionalAdditiveExpressions);

    [[nodiscard]] const AdditiveExpression& getAdditiveExpression() const;

    [[nodiscard]] const std::vector<Operand>& getOptionalAdditiveExpressions() const;
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
    enum RelationalOperator
    {
        LessThan,          ///<<TokenType::LessThan>
        LessThanOrEqual,   ///< TokenType::LessThanOrEqual>
        GreaterThan,       ///< TokenType::GreaterThan>
        GreaterThanOrEqual ///< TokenType::GreaterThanOrEqual>
    };

    struct Operand
    {
        RelationalOperator relationalOperator;
        Lexer::CTokenIterator operatorToken;
        ShiftExpression shiftExpression;
    };

private:
    std::vector<Operand> m_optionalRelationalExpressions;

public:
    RelationalExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, ShiftExpression&& shiftExpression,
                         std::vector<Operand>&& optionalRelationalExpressions);

    [[nodiscard]] const ShiftExpression& getShiftExpression() const;

    [[nodiscard]] const std::vector<Operand>& getOptionalShiftExpressions() const;
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
    enum EqualityOperator
    {
        Equal,   ///<<TokenType::Equal>
        NotEqual ///< TokenType::NotEqual>
    };

    struct Operator
    {
        EqualityOperator equalityOperator;
        Lexer::CTokenIterator operatorToken;
        RelationalExpression relationalExpression;
    };

private:
    std::vector<Operator> m_optionalRelationalExpressions;

public:
    EqualityExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                       RelationalExpression&& relationalExpression,
                       std::vector<Operator>&& optionalRelationalExpressions);

    [[nodiscard]] const RelationalExpression& getRelationalExpression() const;

    [[nodiscard]] const std::vector<Operator>& getOptionalRelationalExpressions() const;
};

/**
 * <BitAndExpression> ::= <EqualityExpression> { <TokenType::BitAnd> <EqualityExpression> }
 */
class BitAndExpression final : public Node
{
    EqualityExpression m_equalityExpression;
    std::vector<std::pair<Lexer::CTokenIterator, EqualityExpression>> m_optionalEqualityExpressions;

public:
    BitAndExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, EqualityExpression&& equalityExpression,
                     std::vector<std::pair<Lexer::CTokenIterator, EqualityExpression>>&& optionalEqualityExpressions);

    [[nodiscard]] const EqualityExpression& getEqualityExpression() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, EqualityExpression>>&
        getOptionalEqualityExpressions() const;
};

/**
 * <BitXorExpression> ::= <BitAndExpression> { <TokenType::BitXor> <BitAndExpression> }
 */
class BitXorExpression final : public Node
{
    BitAndExpression m_bitAndExpression;
    std::vector<std::pair<Lexer::CTokenIterator, BitAndExpression>> m_optionalBitAndExpressions;

public:
    BitXorExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, BitAndExpression&& bitAndExpression,
                     std::vector<std::pair<Lexer::CTokenIterator, BitAndExpression>>&& optionalBitAndExpressions);

    [[nodiscard]] const BitAndExpression& getBitAndExpression() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, BitAndExpression>>&
        getOptionalBitAndExpressions() const;
};

/**
 * <BitOrExpression> ::= <BitXorExpression> { <TokenType::BitOr> <BitXorExpression> }
 */
class BitOrExpression final : public Node
{
    BitXorExpression m_bitXorExpression;
    std::vector<std::pair<Lexer::CTokenIterator, BitXorExpression>> m_optionalBitXorExpressions;

public:
    BitOrExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, BitXorExpression&& bitXorExpression,
                    std::vector<std::pair<Lexer::CTokenIterator, BitXorExpression>>&& optionalBitXorExpressions);

    [[nodiscard]] const BitXorExpression& getBitXorExpression() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, BitXorExpression>>&
        getOptionalBitXorExpressions() const;
};

/**
 * <LogicalAndExpression> ::= <BitOrExpression> { <TokenType::LogicAnd> <BitOrExpression> }
 */
class LogicalAndExpression final : public Node
{
    BitOrExpression m_bitOrExpression;
    std::vector<std::pair<Lexer::CTokenIterator, BitOrExpression>> m_optionalBitOrExpressions;

public:
    LogicalAndExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, BitOrExpression&& bitOrExpression,
                         std::vector<std::pair<Lexer::CTokenIterator, BitOrExpression>>&& optionalBitOrExpressions);

    [[nodiscard]] const BitOrExpression& getBitOrExpression() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, BitOrExpression>>&
        getOptionalBitOrExpressions() const;
};

/**
 * <LogicalOrExpression> ::= <LogicalAndExpression> { <TokenType::LogicOr> <LogicalAndExpression> }
 */
class LogicalOrExpression final : public Node
{
    LogicalAndExpression m_andExpression;
    std::vector<std::pair<Lexer::CTokenIterator, LogicalAndExpression>> m_optionalAndExpressions;

public:
    LogicalOrExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, LogicalAndExpression&& andExpression,
                        std::vector<std::pair<Lexer::CTokenIterator, LogicalAndExpression>>&& optionalAndExpressions);

    [[nodiscard]] const LogicalAndExpression& getAndExpression() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, LogicalAndExpression>>&
        getOptionalAndExpressions() const;
};

/**
 * <ConditionalExpression> ::= <LogicalOrExpression> [ <TokenType::QuestionMark> <Expression> <TokenType::Colon>
 *                             <ConditionalExpression> ]
 *
 * <ConstantExpression> ::= <ConditionalExpression>
 */
class ConditionalExpression final : public Node
{
    std::unique_ptr<LogicalOrExpression> m_logicalOrExpression;
    const Lexer::CToken* m_optionalQuestionMark;
    std::unique_ptr<Expression> m_optionalExpression;
    const Lexer::CToken* m_optionalColon;
    std::unique_ptr<ConditionalExpression> m_optionalConditionalExpression;

public:
    ConditionalExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                          LogicalOrExpression&& logicalOrExpression,
                          const Lexer::CToken* optionalQuestionMark = nullptr,
                          std::unique_ptr<Expression>&& optionalExpression = nullptr,
                          const Lexer::CToken* optionalColon = nullptr,
                          std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression = nullptr);

    [[nodiscard]] const LogicalOrExpression& getLogicalOrExpression() const;

    [[nodiscard]] const Lexer::CToken* CLD_NULLABLE getOptionalQuestionMark() const;

    [[nodiscard]] const ConditionalExpression* CLD_NULLABLE getOptionalConditionalExpression() const;

    [[nodiscard]] const Lexer::CToken* CLD_NULLABLE getOptionalColon() const;

    [[nodiscard]] const Expression* CLD_NULLABLE getOptionalExpression() const;
};

/**
 * According to C99:
 * <AssignmentExpression> ::= <AssignmentExpressionAssignment> | <ConditionalExpression>
 *
 * Instead we are doing something similar to clang here though:
 * We'll be using the grammar of the form:
 *
 * <AssignmentExpression> ::= <ConditionalExpression> { <AssignOperator> <ConditionalExpression> }
 *
 * Checking if the left operand is an LValue will be done in Semantics
 */
class AssignmentExpression final : public Node
{
public:
    /**
     * <AssignmentExpression::AssignOperator>
     */
    enum AssignOperator
    {
        NoOperator,       ///<<TokenType::Assignment>
        PlusAssign,       ///<<TokenType::PlusAssign>
        MinusAssign,      ///<<TokenType::MinusAssign>
        DivideAssign,     ///<<TokenType::DivideAssign>
        MultiplyAssign,   ///<<TokenType::MultiplyAssign>
        ModuloAssign,     ///<<TokenType::ModuloAssign>
        LeftShiftAssign,  ///<<TokenType::LeftShiftAssign>
        RightShiftAssign, ///<<TokenType::RightShiftAssign>
        BitAndAssign,     ///<<TokenType::BitAndAssign>
        BitOrAssign,      ///<<TokenType::BitOrAssign>
        BitXorAssign      ///<<TokenType::BitXorAssign>
    };

    struct Operand
    {
        AssignOperator assignOperator;
        Lexer::CTokenIterator operatorToken;
        ConditionalExpression conditionalExpression;
    };

private:
    ConditionalExpression m_conditionalExpression;
    std::vector<Operand> m_optionalConditionalExpression;

public:
    AssignmentExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                         ConditionalExpression&& conditionalExpression,
                         std::vector<Operand>&& optionalConditionalExpressions);

    [[nodiscard]] const ConditionalExpression& getConditionalExpression() const;

    [[nodiscard]] const std::vector<Operand>& getOptionalConditionalExpressions() const;
};

/**
 * <ReturnStatement> ::= <TokenType::ReturnKeyword> [<Expression>] <TokenType::SemiColon>
 */
class ReturnStatement final : public Node
{
    std::unique_ptr<Expression> m_expression;

public:
    ReturnStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<Expression>&& expression);

    [[nodiscard]] const Expression* getExpression() const;
};

/**
 * <ExpressionStatement> ::= [<Expression>] <TokenType::SemiColon>
 */
class ExpressionStatement final : public Node
{
    std::unique_ptr<Expression> m_optionalExpression;

public:
    ExpressionStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                        std::unique_ptr<Expression>&& optionalExpression = nullptr);

    [[nodiscard]] const Expression* getOptionalExpression() const;

    std::unique_ptr<Expression> moveOptionalExpression();
};

/**
 * <LabeledStatement> ::= <LabelStatement> | <CaseStatement> | <DefaultStatement>
 *
 * <SelectionStatement> ::= <IfStatement> | <SwitchStatement>
 *
 * <IterationStatement> ::= <ForStatement> | <HeadWhileStatement> | <FootWhileStatement>
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
                               HeadWhileStatement, FootWhileStatement, BreakStatement, ContinueStatement,
                               SwitchStatement, DefaultStatement, CaseStatement, GotoStatement, LabelStatement>;

/**
 * <IfStatement> ::= <TokenType::IfKeyword> <TokenType::OpenParentheses> <Expression> <TokenType::CloseParentheses>
 *               <Statement> [ <TokenType::ElseKeyword> <Statement> ]
 */
class IfStatement final : public Node
{
    Expression m_expression;
    std::unique_ptr<Statement> m_branch;
    std::unique_ptr<Statement> m_elseBranch;

public:
    IfStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Expression&& expression,
                std::unique_ptr<Statement>&& branch, std::unique_ptr<Statement>&& elseBranch = nullptr);

    [[nodiscard]] const Expression& getExpression() const;

    [[nodiscard]] const Statement& getBranch() const;

    [[nodiscard]] const Statement* getElseBranch() const;
};

/**
 * <SwitchStatement> ::= <TokenType::SwitchKeyword> <TokenType::OpenParentheses> <Expression>
 * <TokenType::CloseParentheses> <Statement>
 */
class SwitchStatement final : public Node
{
    Expression m_expression;
    std::unique_ptr<Statement> m_statement;

public:
    SwitchStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Expression&& expression,
                    std::unique_ptr<Statement>&& statement);

    [[nodiscard]] const Expression& getExpression() const;

    [[nodiscard]] const Statement& getStatement() const;
};

/**
 * <DefaultStatement> ::= <TokenType::DefaultKeyword> <TokenType::Colon> <Statement>
 */
class DefaultStatement final : public Node
{
    std::unique_ptr<Statement> m_statement;

public:
    DefaultStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<Statement>&& statement);

    [[nodiscard]] const Statement& getStatement() const;
};

/**
 * <CaseStatement> ::= <TokenType::CaseKeyword> <ConstantNonCommaExpression> <TokenType::Colon> <Statement>
 */
class CaseStatement final : public Node
{
    ConstantExpression m_constantExpression;
    std::unique_ptr<Statement> m_statement;

public:
    CaseStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, ConstantExpression&& constantExpression,
                  std::unique_ptr<Statement>&& statement);

    [[nodiscard]] const ConstantExpression& getConstantExpression() const;

    [[nodiscard]] const Statement& getStatement() const;
};

/**
 * <LabelStatement> ::= <TokenType::Identifier> <TokenType::Colon> <Statement>
 */
class LabelStatement final : public Node
{
    std::string_view m_identifier;
    std::unique_ptr<Statement> m_statement;

public:
    LabelStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::string_view identifier,
                   Statement&& statement);

    [[nodiscard]] const Statement& getStatement() const;
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
    CompoundStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::vector<CompoundItem>&& blockItems);

    [[nodiscard]] const std::vector<CompoundItem>& getBlockItems() const;
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
    StorageClassSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Specifiers specifier);

    [[nodiscard]] Specifiers getSpecifier() const;
};

inline bool operator==(const StorageClassSpecifier& storageClassSpecifier, StorageClassSpecifier::Specifiers specifier)
{
    return specifier == storageClassSpecifier.getSpecifier();
}

inline bool operator==(StorageClassSpecifier::Specifiers specifier, const StorageClassSpecifier& storageClassSpecifier)
{
    return specifier == storageClassSpecifier.getSpecifier();
}

/**
 * <FunctionSpecifier> ::= <TokenType::InlineKeyword>
 */
class FunctionSpecifier final : public Node
{
public:
    FunctionSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end);
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
    Declaration(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                std::vector<DeclarationSpecifier>&& declarationSpecifiers,
                std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>>&& initDeclarators);

    [[nodiscard]] const std::vector<DeclarationSpecifier>& getDeclarationSpecifiers() const;

    [[nodiscard]] const std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>>&
        getInitDeclarators() const;
};

/**
 * <ExpressionOrDeclaration> ::= <Declaration> | [<Expression>] <TokenType::SemiColon>
 *
 * <ForStatement> ::= <TokenType::ForKeyword> <TokenType::OpenParentheses> <ExpressionOrDeclaration>
 *                    [<Expression>] <TokenType::SemiColon> [<Expression>] <TokenType::CloseParentheses> <Statement>
 */
class ForStatement final : public Node
{
    std::unique_ptr<Statement> m_statement;
    std::variant<Declaration, std::unique_ptr<Expression>> m_initial;
    std::unique_ptr<Expression> m_controlling;
    std::unique_ptr<Expression> m_post;

public:
    ForStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<Statement>&& statement,
                 std::variant<Declaration, std::unique_ptr<Expression>>&& initial,
                 std::unique_ptr<Expression>&& controlling, std::unique_ptr<Expression>&& post);

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const std::variant<Declaration, std::unique_ptr<Expression>>& getInitial() const;

    [[nodiscard]] const Expression* getControlling() const;

    [[nodiscard]] const Expression* getPost() const;
};

/**
 * <HeadWhileStatement> ::= <TokenType::WhileKeyword> <TokenType::OpenParentheses> <Expression>
 *                          <TokenType::CloseParentheses> <Statement>
 */
class HeadWhileStatement final : public Node
{
    Expression m_expression;
    std::unique_ptr<Statement> m_statement;

public:
    HeadWhileStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Expression&& expression,
                       std::unique_ptr<Statement>&& statement);

    [[nodiscard]] const Expression& getExpression() const;

    [[nodiscard]] const Statement& getStatement() const;
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
    FootWhileStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::unique_ptr<Statement>&& statement,
                       Expression&& expression);

    [[nodiscard]] const Statement& getStatement() const;

    [[nodiscard]] const Expression& getExpression() const;
};

/**
 * <BreakStatement> ::= <TokenType::BreakKeyword> <TokenType::SemiColon>
 */
class BreakStatement final : public Node
{
public:
    BreakStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end);
};

/**
 * <ContinueStatement> ::= <TokenType::ContinueStatement> <TokenType::SemiColon>
 */
class ContinueStatement final : public Node
{
public:
    ContinueStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end);
};

/**
 * <GotoStatement> ::= <TokenType::GotoStatement> <TokenType::Identifier> <TokenType::SemiColon>
 */
class GotoStatement final : public Node
{
    Lexer::CTokenIterator m_identifier;

public:
    GotoStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator identifier);

    [[nodiscard]] Lexer::CTokenIterator getIdentifier() const;
};

/**
 * <DirectAbstractDeclarator> ::= <DirectAbstractDeclaratorParentheses>
 *                              | <DirectAbstractDeclaratorAssignmentExpression>
 *                              | <DirectAbstractDeclaratorAsterisk>
 *                              | <DirectAbstractDeclaratorParameterTypeList>
 */
using DirectAbstractDeclarator =
    std::variant<DirectAbstractDeclaratorParentheses, DirectAbstractDeclaratorAssignmentExpression,
                 DirectAbstractDeclaratorAsterisk, DirectAbstractDeclaratorParameterTypeList>;

/**
 * <DirectAbstractDeclaratorParentheses> ::= <TokenType::OpenParentheses> <AbstractDeclarator>
 * <TokenType::CloseParentheses>
 */
class DirectAbstractDeclaratorParentheses final : public Node
{
    std::unique_ptr<AbstractDeclarator> m_abstractDeclarator;

public:
    DirectAbstractDeclaratorParentheses(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                        std::unique_ptr<AbstractDeclarator>&& abstractDeclarator);

    [[nodiscard]] const AbstractDeclarator& getAbstractDeclarator() const;
};

/**
 * <DirectAbstractDeclaratorAssignmentExpression> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket>
 *                                                    [<AssignmentExpression>] <TokenType::CloseSquareBracket>
 */
class DirectAbstractDeclaratorAssignmentExpression final : public Node
{
    std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
    std::unique_ptr<AssignmentExpression> m_assignmentExpression;

public:
    DirectAbstractDeclaratorAssignmentExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                 std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
                                                 std::unique_ptr<AssignmentExpression>&& assignmentExpression);

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

    [[nodiscard]] const AssignmentExpression* getAssignmentExpression() const;
};

/**
 * <DirectAbstractDeclaratorAsterisk> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket>
 * <TokenType::Asterisk> <TokenType::CloseSquareBracket>
 */
class DirectAbstractDeclaratorAsterisk final : public Node
{
    std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;

public:
    DirectAbstractDeclaratorAsterisk(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                     std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator);

    [[nodiscard]] const std::unique_ptr<DirectAbstractDeclarator>& getDirectAbstractDeclarator() const;
};

/**
 * <DirectAbstractDeclaratorParameterTypeList> ::= [<DirectAbstractDeclarator>] <TokenType::OpenParentheses>
 *                                                 [<ParameterTypeList>] <TokenType::CloseParentheses>
 */
class DirectAbstractDeclaratorParameterTypeList final : public Node
{
    std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
    std::unique_ptr<ParameterTypeList> m_parameterTypeList;

public:
    DirectAbstractDeclaratorParameterTypeList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                              std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
                                              std::unique_ptr<ParameterTypeList>&& parameterTypeList);

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

    [[nodiscard]] const ParameterTypeList* getParameterTypeList() const;
};

/**
 * <AbstractDeclarator> ::= { <Pointer> } <DirectAbstractDeclarator>
 *                        | <Pointer> { <Pointer> }
 */
class AbstractDeclarator final : public Node
{
    std::vector<Pointer> m_pointers;
    std::optional<DirectAbstractDeclarator> m_directAbstractDeclarator;

public:
    AbstractDeclarator(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::vector<Pointer>&& pointers,
                       std::optional<DirectAbstractDeclarator>&& directAbstractDeclarator);

    [[nodiscard]] const std::vector<Pointer>& getPointers() const;

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;
};

/**
 * <ParameterDeclaration> ::= <DeclarationSpecifier> { <DeclarationSpecifier> } <Declarator>
 *                          | <DeclarationSpecifier> { <DeclarationSpecifier> } [ <AbstractDeclarator> ]
 */
struct ParameterDeclaration : public Node
{
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    std::variant<std::unique_ptr<Declarator>, std::unique_ptr<AbstractDeclarator>> declarator;
};

/**
 * <ParameterList> ::= <ParameterDeclaration> { <TokenType::Comma> <ParameterDeclaration> }
 */
class ParameterList final : public Node
{
private:
    std::vector<ParameterDeclaration> m_parameterList;

public:
    ParameterList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                  std::vector<ParameterDeclaration>&& parameterList);

    [[nodiscard]] const std::vector<ParameterDeclaration>& getParameterDeclarations() const&;

    [[nodiscard]] std::vector<ParameterDeclaration>&& getParameterDeclarations() &&;
};

/**
 * <ParameterTypeList> ::= <ParameterList> [ <TokenType::Comma> <TokenType::Ellipse> ]
 */
class ParameterTypeList final : public Node
{
    std::vector<ParameterDeclaration> m_parameterList;
    bool m_hasEllipse;

public:
    ParameterTypeList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, ParameterList&& parameterList,
                      bool hasEllipse);

    [[nodiscard]] const std::vector<ParameterDeclaration>& getParameters() const;

    [[nodiscard]] bool hasEllipse() const;
};

/**
 * <DirectDeclarator> ::= <DirectDeclaratorIdentifier>
 *                      | <DirectDeclaratorParentheses>
 *                      | <DirectDeclaratorNoStaticOrAsterisk>
 *                      | <DirectDeclaratorStatic>
 *                      | <DirectDeclaratorAsterisk>
 *                      | <DirectDeclaratorParentheseParameters>
 *                      | <DirectDeclaratorParentheseIdentifier>
 */
using DirectDeclarator =
    std::variant<DirectDeclaratorIdentifier, DirectDeclaratorParentheses, DirectDeclaratorNoStaticOrAsterisk,
                 DirectDeclaratorStatic, DirectDeclaratorAsterisk, DirectDeclaratorParenthesesParameters,
                 DirectDeclaratorParenthesesIdentifiers>;

/**
 * <DirectDeclaratorIdentifier> ::= <TokenType::Identifier>
 */
class DirectDeclaratorIdentifier final : public Node
{
    Lexer::CTokenIterator m_identifierLoc;

public:
    DirectDeclaratorIdentifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator identifierLoc);

    [[nodiscard]] Lexer::CTokenIterator getIdentifierLoc() const;
};

/**
 * <DirectDeclaratorParentheses> ::= <TokenType::OpenParentheses> <DirectDeclarator> <TokenType::CloseParentheses>
 */
class DirectDeclaratorParentheses final : public Node
{
    std::unique_ptr<Declarator> m_declarator;

public:
    DirectDeclaratorParentheses(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                std::unique_ptr<Declarator>&& declarator);

    [[nodiscard]] const Declarator& getDeclarator() const;
};

/**
 * <DirectDeclaratorParenthesesParameters> ::= <DirectDeclarator> <TokenType::OpenParentheses> <ParameterTypeList>
 * <TokenType::CloseParentheses>
 */
class DirectDeclaratorParenthesesParameters final : public Node
{
    std::unique_ptr<DirectDeclarator> m_directDeclarator;
    ParameterTypeList m_parameterTypeList;

public:
    DirectDeclaratorParenthesesParameters(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                          DirectDeclarator&& directDeclarator, ParameterTypeList&& parameterTypeList);

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;

    [[nodiscard]] const ParameterTypeList& getParameterTypeList() const;
};

/**
 * <DirectDeclaratorParenthesesIdentifiers> ::= <DirectDeclarator> <TokenType::OpenParentheses> [
 * <TokenType::Identifier> {<TokenType::Comma> <TokenType::Identifier>}] <TokenType::CloseParentheses>
 */
class DirectDeclaratorParenthesesIdentifiers final : public Node
{
    std::unique_ptr<DirectDeclarator> m_directDeclarator;
    std::vector<Lexer::CTokenIterator> m_identifiers;

public:
    DirectDeclaratorParenthesesIdentifiers(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                           DirectDeclarator&& directDeclarator,
                                           std::vector<Lexer::CTokenIterator>&& identifiers);

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;

    [[nodiscard]] const std::vector<Lexer::CTokenIterator>& getIdentifiers() const;
};

/**
 * <DirectDeclaratorAsterisk> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> {<TypeQualifier> }
 *                                <TokenType::Asterisk> <TokenType::CloseSquareBracket>
 */
class DirectDeclaratorAsterisk final : public Node
{
    std::unique_ptr<DirectDeclarator> m_directDeclarator;
    std::vector<TypeQualifier> m_typeQualifiers;

public:
    DirectDeclaratorAsterisk(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                             DirectDeclarator&& directDeclarator, std::vector<TypeQualifier>&& typeQualifiers);

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;
};

/**
 * <DirectDeclaratorNoStaticOrAsterisk> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> {<TypeQualifier>}
 *                                          [<AssignmentExpression>] <TokenType::CloseSquareBracket>
 */
class DirectDeclaratorNoStaticOrAsterisk final : public Node
{
    std::unique_ptr<DirectDeclarator> m_directDeclarator;
    std::vector<TypeQualifier> m_typeQualifiers;
    std::unique_ptr<AssignmentExpression> m_assignmentExpression;

public:
    DirectDeclaratorNoStaticOrAsterisk(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                       std::unique_ptr<DirectDeclarator>&& directDeclarator,
                                       std::vector<TypeQualifier>&& typeQualifiers,
                                       std::unique_ptr<AssignmentExpression>&& assignmentExpression);

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;

    [[nodiscard]] const std::unique_ptr<AssignmentExpression>& getAssignmentExpression() const;
};

/**
 * <DirectDeclaratorStatic> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> <TokenType::StaticKeyword>
 *                              {<TypeQualifier>} <AssignmentExpression> <TokenType::CloseSquareBracket>
 *                            | <DirectDeclarator> <TokenType::OpenSquareBracket> <TypeQualifier> {<TypeQualifier>}
 *                              <TokenType::StaticKeyword> <AssignmentExpression> <TokenType::CloseSquareBracket>
 */
class DirectDeclaratorStatic final : public Node
{
    std::unique_ptr<DirectDeclarator> m_directDeclarator;
    Lexer::CTokenIterator m_staticLoc;
    std::vector<TypeQualifier> m_typeQualifiers;
    AssignmentExpression m_assignmentExpression;

public:
    DirectDeclaratorStatic(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                           std::unique_ptr<DirectDeclarator>&& directDeclarator, Lexer::CTokenIterator staticLoc,
                           std::vector<TypeQualifier>&& typeQualifiers, AssignmentExpression&& assignmentExpression);

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;

    [[nodiscard]] const AssignmentExpression& getAssignmentExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getStaticLoc() const;
};

/**
 * <Declarator> ::= { <Pointer> } <DirectDeclarator>
 */
class Declarator final : public Node
{
    std::vector<Pointer> m_pointers;
    DirectDeclarator m_directDeclarator;

public:
    Declarator(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::vector<Pointer>&& pointers,
               DirectDeclarator&& directDeclarator);

    [[nodiscard]] const std::vector<Pointer>& getPointers() const;

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;
};

/**
 * <StructOrUnion> ::= <TokenType::StructKeyword> | <TokenType::UnionKeyword>
 *
 * <StructOrUnionSpecifier> ::= <StructOrUnion> [ <TokenType::Identifier> ]
 *                              <TokenType::OpenBrace> <StructDeclaration> { <StructDeclaration> }
 *                              <TokenType::CloseBrace> | <StructOrUnion> <TokenType::Identifier>
 */
class StructOrUnionSpecifier final : public Node
{
    bool m_isUnion;
    const Lexer::CToken* m_identifierLoc;

public:
    /**
     *
     * <StructDeclarator> ::= <Declarator> | [<Declarator>] <TokenType::Colon> <ConstantExpression>
     *
     * <StructDeclaration> ::= <SpecifierQualifier> { <SpecifierQualifier> }
     *                         <StructDeclarator> {<TokenType::Comma> <StructDeclarator> } <TokenType::SemiColon>
     */
    struct StructDeclaration
    {
        std::vector<SpecifierQualifier> specifierQualifiers;
        std::vector<std::pair<std::unique_ptr<Declarator>, std::optional<ConstantExpression>>> structDeclarators;
    };

private:
    std::vector<StructDeclaration> m_structDeclarations;

public:
    StructOrUnionSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, bool isUnion,
                           const Lexer::CToken* identifierLoc, std::vector<StructDeclaration>&& structDeclarations);

    [[nodiscard]] bool isUnion() const;

    [[nodiscard]] const Lexer::CToken* getIdentifierLoc() const;

    [[nodiscard]] const std::vector<StructDeclaration>& getStructDeclarations() const;
};

/**
 * <EnumDeclaration> ::= <TokenType::EnumKeyword> [ <TokenType::Identifier> ] <TokenType::OpenBrace>
 *                       <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> ]
 *                       { <TokenType::Comma> <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression>
 * } [ <TokenType::Comma> ] <TokenType::CloseBrace>
 */
class EnumDeclaration final : public Node
{
    const Lexer::CToken* m_name;
    std::vector<std::pair<Lexer::CTokenIterator, std::optional<ConstantExpression>>> m_values;

public:
    EnumDeclaration(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, const Lexer::CToken* name,
                    std::vector<std::pair<Lexer::CTokenIterator, std::optional<ConstantExpression>>>&& values);

    [[nodiscard]] const Lexer::CToken* getName() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, std::optional<ConstantExpression>>>&
        getValues() const;
};

/**
 * <EnumSpecifier> ::= <EnumDeclaration> | <TokenType::EnumKeyword> <TokenType::Identifier>
 */
class EnumSpecifier final : public Node
{
    using Variant = std::variant<EnumDeclaration, const Lexer::CToken * CLD_NULLABLE>;

    Variant m_variant;

public:
    EnumSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Variant&& variant);

    [[nodiscard]] const Variant& getVariant() const;
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
    enum PrimitiveTypeSpecifier
    {
        Void = 0b1,
        Char = 0b10,
        Short = 0b100,
        Int = 0b1000,
        Long = 0b10000,
        // LongLong = 0b100000, Only used in the Semantic stage. Obtained by Long + Long
        Float = 0b1000000,
        Double = 0b10000000,
        Signed = 0b100000000,
        Unsigned = 0b1000000000,
        Bool = 0b10000000000,
    };

private:
    using variant = std::variant<PrimitiveTypeSpecifier, std::unique_ptr<StructOrUnionSpecifier>,
                                 std::unique_ptr<EnumSpecifier>, std::string_view>;

    variant m_variant;

public:
    TypeSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, variant&& variant);

    [[nodiscard]] const variant& getVariant() const;
};

/**
 * <Pointer> ::= <TokenType::Asterisk> { <TypeQualifier> }
 */
class Pointer final : public Node
{
    std::vector<TypeQualifier> m_typeQualifiers;

public:
    Pointer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::vector<TypeQualifier>&& typeQualifiers);

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;
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
    using Designator = std::variant<ConstantExpression, std::string_view>;

    /**
     * <DesignatorList> ::= <Designator> { <Designator> }
     */
    using DesignatorList = std::vector<Designator>;

    using vector = std::vector<std::pair<Initializer, DesignatorList>>;

private:
    vector m_nonCommaExpressionsAndBlocks;

public:
    InitializerList(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, vector&& nonCommaExpressionsAndBlocks);

    [[nodiscard]] const vector& getNonCommaExpressionsAndBlocks() const;
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
    Initializer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, variant&& variant);

    [[nodiscard]] const variant& getVariant() const;
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
    FunctionDefinition(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                       std::vector<DeclarationSpecifier>&& declarationSpecifiers, Declarator&& declarator,
                       std::vector<Declaration>&& declarations, CompoundStatement&& compoundStatement);

    [[nodiscard]] const std::vector<DeclarationSpecifier>& getDeclarationSpecifiers() const;

    [[nodiscard]] const Declarator& getDeclarator() const;

    [[nodiscard]] const std::vector<Declaration>& getDeclarations() const;

    [[nodiscard]] const CompoundStatement& getCompoundStatement() const;
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

    [[nodiscard]] const std::vector<ExternalDeclaration>& getGlobals() const;
};
} // namespace cld::Syntax

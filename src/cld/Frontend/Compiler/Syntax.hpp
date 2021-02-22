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

class PrimaryExpressionBuiltinVAArg;

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

class DirectAbstractDeclaratorStatic;

class Pointer;

class ParameterTypeList;

class ParameterList;

class GotoStatement;

class LabelStatement;

class GNUAttributes;

class GNUSimpleASM;

class GNUASMStatement;

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
    std::unique_ptr<AssignmentExpression> m_assignmentExpression;
    std::vector<std::pair<Lexer::CTokenIterator, AssignmentExpression>> m_optionalAssignmentExpressions;

public:
    Expression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
               std::unique_ptr<AssignmentExpression>&& assignmentExpression,
               std::vector<std::pair<Lexer::CTokenIterator, AssignmentExpression>> optionalAssignmentExpressions);

    [[nodiscard]] const AssignmentExpression& getAssignmentExpression() const;

    [[nodiscard]] const std::vector<std::pair<Lexer::CTokenIterator, AssignmentExpression>>&
        getOptionalAssignmentExpressions() const;
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
 * [GNU]: <PrimaryExpressionBuiltinVAArg> ::= <Identifier=__builtin_va_arg> <TokenType::OpenParentheses>
 *                                         <NonCommaExpression> <TokenType::Comma> <TypeName>
 *                                         <TokenType::CloseParentheses>
 */
class PrimaryExpressionBuiltinVAArg final : public Node
{
    Lexer::CTokenIterator m_builtinToken;
    Lexer::CTokenIterator m_openParentheses;
    std::unique_ptr<AssignmentExpression> m_assignmentExpression;
    Lexer::CTokenIterator m_comma;
    std::unique_ptr<TypeName> m_typeName;
    Lexer::CTokenIterator m_closeParentheses;

public:
    PrimaryExpressionBuiltinVAArg(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                  Lexer::CTokenIterator builtinToken, Lexer::CTokenIterator openParentheses,
                                  std::unique_ptr<AssignmentExpression>&& assignmentExpression,
                                  Lexer::CTokenIterator comma, std::unique_ptr<TypeName>&& typeName,
                                  Lexer::CTokenIterator closeParentheses);

    [[nodiscard]] Lexer::CTokenIterator getBuiltinToken() const;

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const AssignmentExpression& getAssignmentExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getComma() const;

    [[nodiscard]] const TypeName& getTypeName() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;
};

/**
 * [GNU]: <PrimaryExpressionBuiltinOffsetOf> ::= <Identifier=__builtin_offsetof> <TokenType::OpenParentheses>
 *                                               <TypeName> <TokenType::Comma> <MemberDesignation>
 *                                               <TokenType::CloseParentheses>
 *
 * <MemberDesignation> ::= <TokenType::Identifier> { <TokenType::Identifier> | <TokenType::OpenSquareBracket>
 *                                                                        <Expression> <TokenType::CloseSquareBracket> }
 */
class PrimaryExpressionBuiltinOffsetOf final : public Node
{
    Lexer::CTokenIterator m_builtinToken;
    Lexer::CTokenIterator m_openParentheses;
    std::unique_ptr<TypeName> m_typeName;
    Lexer::CTokenIterator m_comma;
    Lexer::CTokenIterator m_memberName;

public:
    struct Subscript
    {
        Lexer::CTokenIterator openBracket;
        std::unique_ptr<Expression> expression;
        Lexer::CTokenIterator closeBracket;
    };

    using Variant = std::variant<Lexer::CTokenIterator, Subscript>;

private:
    std::vector<Variant> m_memberSuffix;
    Lexer::CTokenIterator m_closeParentheses;

public:
    PrimaryExpressionBuiltinOffsetOf(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                     Lexer::CTokenIterator builtinToken, Lexer::CTokenIterator openParentheses,
                                     std::unique_ptr<TypeName>&& typeName, Lexer::CTokenIterator comma,
                                     Lexer::CTokenIterator memberName, std::vector<Variant>&& memberSuffixes,
                                     Lexer::CTokenIterator closeParentheses);

    [[nodiscard]] Lexer::CTokenIterator getBuiltinToken() const;

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const TypeName& getTypeName() const;

    [[nodiscard]] Lexer::CTokenIterator getComma() const;

    [[nodiscard]] Lexer::CTokenIterator getMemberName() const;

    [[nodiscard]] const std::vector<Variant>& getMemberSuffix() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;
};

/**
 * <PrimaryExpression> ::= <PrimaryExpressionIdentifier>
 *                       | <PrimaryExpressionConstant>
 *                       | <PrimaryExpressionParentheses>
 *                 [GNU] | <PrimaryExpressionBuiltinVAArg>
 *                 [GNU] | <PrimaryExpressionBuiltinOffsetOf>
 */
using PrimaryExpression =
    std::variant<PrimaryExpressionIdentifier, PrimaryExpressionConstant, PrimaryExpressionParentheses,
                 PrimaryExpressionBuiltinVAArg, PrimaryExpressionBuiltinOffsetOf>;

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
 * <PostFixExpressionFunctionCall> ::= <PostFixExpression> <TokenType::OpenParentheses> [ <NonCommaExpression>
 *                                     { <TokenType::Comma> <NonCommaExpression> } ]
 *                                     <TokenType::CloseParentheses>
 */
class PostFixExpressionFunctionCall final : public Node
{
    std::unique_ptr<PostFixExpression> m_postFixExpression;
    Lexer::CTokenIterator m_openParentheses;
    std::vector<AssignmentExpression> m_optionalAssignmentExpressions;
    Lexer::CTokenIterator m_closeParentheses;

public:
    PostFixExpressionFunctionCall(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                  std::unique_ptr<PostFixExpression>&& postFixExpression,
                                  Lexer::CTokenIterator openParentheses,
                                  std::vector<AssignmentExpression>&& optionalAssignmentExpressions,
                                  Lexer::CTokenIterator closeParentheses);

    [[nodiscard]] const PostFixExpression& getPostFixExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const std::vector<AssignmentExpression>& getOptionalAssignmentExpressions() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;
};

/**
 * <PostFixExpressionTypeInitializer> ::= <TokenType::OpenParentheses> <TypeName> <TokenType::CloseParentheses>
 *                                        <TokenType::OpenBrace> <InitializerList> [<TokenType::Comma>]
 * <TokenType::CloseBrace>
 */
class PostFixExpressionTypeInitializer final : public Node
{
    Lexer::CTokenIterator m_openParentheses;
    std::unique_ptr<TypeName> m_typeName;
    Lexer::CTokenIterator m_closeParentheses;
    std::unique_ptr<InitializerList> m_initializerList;

public:
    PostFixExpressionTypeInitializer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                     Lexer::CTokenIterator openParentheses, TypeName&& typeName,
                                     Lexer::CTokenIterator closeParentheses, InitializerList&& initializerList);

    [[nodiscard]] Lexer::CTokenIterator getOpenParentheses() const;

    [[nodiscard]] const InitializerList& getInitializerList() const;

    [[nodiscard]] Lexer::CTokenIterator getCloseParentheses() const;

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
 *                         [GNU]:   | <TokenType::GNUExtension> <CastExpression>
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
        LogicalNot,
        GNUExtension // Does nothing semantically
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
 *                             <TokenType::CloseParentheses>
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
 * [PP] <UnaryExpressionDefined> ::= <TokenType::DefinedKeyword> <TokenType::Identifier>
 *                            | <TokenType::DefinedKeyword> <TokenType::OpenParentheses> <TokenType::Identifier>
 *                              <TokenType::CloseParentheses>
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
 *
 * [GNU]: <SpecifierQualifier> ::= <TypeSpecifier> | <TypeQualifier> | <GNUAttributes>
 */
using SpecifierQualifier = std::variant<TypeSpecifier, TypeQualifier, GNUAttributes>;

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
 *
 * [GNU]:
 *
 * <GNUAttributes> ::= <GNUAttribute> { <GNUAttribute> }
 *
 * <GNUAttribute> ::= <TokenType::GNUAttribute> <TokenType::OpenParentheses> <TokenType::OpenParentheses>
 *                    <GNUAttributeList> <TokenType::CloseParentheses> <TokenType::CloseParentheses>
 *
 * <GNUAttributeList> ::= [<GNUAttrib>] { <TokenType::Comma> [<GNUAttrib>] }
 *
 * <GNUAttrib> ::= <GNUAttribName>
 *               | <GNUAttribName> <TokenType::OpenParentheses> <TokenType::Identifier> <TokenType::CloseParentheses>
 *               | <GNUAttribName> <TokenType::OpenParentheses> <TokenType::Identifier> <TokenType::Comma>
 *                 <NonCommaExpression> { <TokenType::Comma> <NonCommaExpression> } <TokenType::CloseParentheses>
 *               | <GNUAttribName> <TokenType::OpenParentheses> [<NonCommaExpression> { <TokenType::Comma>
 *                 <NonCommaExpression> } ] <TokenType::CloseParentheses>
 *
 * <GNUAttribName> ::= <TokenType::Identifier>
 *                   | <TypeSpecifier>
 *                   | <TypeQualifier>
 *                   | <StorageClassSpecifier>
 */
class GNUAttributes final : public Node
{
public:
    struct GNUAttribute
    {
        Lexer::CTokenIterator nameToken;
        const Lexer::CToken* optionalFirstIdentifierArgument;
        std::vector<AssignmentExpression> arguments;
    };

private:
    std::vector<GNUAttribute> m_attributes;

public:
    GNUAttributes(const Lexer::CToken* begin, const Lexer::CToken* end, std::vector<GNUAttribute>&& attributes);

    [[nodiscard]] const std::vector<GNUAttribute>& getAttributes() const;
};

/**
 * <ExpressionStatement> ::= [<Expression>] <TokenType::SemiColon>
 *                         | <GNUAttribute> <TokenType::SemiColon>
 */
class ExpressionStatement final : public Node
{
    std::unique_ptr<Expression> m_optionalExpression;
    std::optional<GNUAttributes> m_optionalAttributes;

public:
    ExpressionStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                        std::unique_ptr<Expression>&& optionalExpression,
                        std::optional<GNUAttributes>&& optionalAttributes = {});

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
 *        [GNU:] | <GNUASMStatement>
 */
using Statement =
    std::variant<ReturnStatement, ExpressionStatement, IfStatement, CompoundStatement, ForStatement, HeadWhileStatement,
                 FootWhileStatement, BreakStatement, ContinueStatement, SwitchStatement, DefaultStatement,
                 CaseStatement, GotoStatement, LabelStatement, GNUASMStatement>;

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
 *                       <TokenType::CloseParentheses> <Statement>
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
    Lexer::CTokenIterator m_defaultToken;
    Lexer::CTokenIterator m_colonToken;
    std::unique_ptr<Statement> m_statement;

public:
    DefaultStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator defaultToken,
                     Lexer::CTokenIterator colonToken, std::unique_ptr<Statement>&& statement);

    [[nodiscard]] Lexer::CTokenIterator getDefaultToken() const;

    [[nodiscard]] Lexer::CTokenIterator getColonToken() const;

    [[nodiscard]] const Statement& getStatement() const;
};

/**
 * <CaseStatement> ::= <TokenType::CaseKeyword> <ConstantNonCommaExpression> <TokenType::Colon> <Statement>
 */
class CaseStatement final : public Node
{
    Lexer::CTokenIterator m_caseToken;
    ConstantExpression m_constantExpression;
    Lexer::CTokenIterator m_colonToken;
    std::unique_ptr<Statement> m_statement;

public:
    CaseStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator caseToken,
                  ConstantExpression&& constantExpression, Lexer::CTokenIterator colonToken,
                  std::unique_ptr<Statement>&& statement);

    [[nodiscard]] Lexer::CTokenIterator getCaseToken() const;

    [[nodiscard]] const ConstantExpression& getConstantExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getColonToken() const;

    [[nodiscard]] const Statement& getStatement() const;
};

/**
 * <LabelStatement> ::= <TokenType::Identifier> <TokenType::Colon> (<Statement> | <GNUAttribute> <TokenType::SemiColon>)
 */
class LabelStatement final : public Node
{
    Lexer::CTokenIterator m_identifier;
    std::variant<std::unique_ptr<Statement>, GNUAttributes> m_statementOrAttribute; // unique_ptr<Statement> is not null

public:
    LabelStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator identifier,
                   Statement&& statement);

    LabelStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, Lexer::CTokenIterator identifier,
                   GNUAttributes&& attributes);

    [[nodiscard]] Lexer::CTokenIterator getIdentifierToken() const;

    [[nodiscard]] const std::variant<std::unique_ptr<Statement>, GNUAttributes>& getStatementOrAttribute() const;
};

/**
 * <BlockItem> ::= <Statement> | <Declaration>
 *
 * [GNU]: <BlockItem> ::= <Statement> | { <TokenType::GNUExtension> } <Declaration>
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

inline bool operator!=(const StorageClassSpecifier& storageClassSpecifier, StorageClassSpecifier::Specifiers specifier)
{
    return !(specifier == storageClassSpecifier.getSpecifier());
}

inline bool operator==(StorageClassSpecifier::Specifiers specifier, const StorageClassSpecifier& storageClassSpecifier)
{
    return specifier == storageClassSpecifier.getSpecifier();
}

inline bool operator!=(StorageClassSpecifier::Specifiers specifier, const StorageClassSpecifier& storageClassSpecifier)
{
    return !(specifier == storageClassSpecifier.getSpecifier());
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
 *
 * [GNU] <DeclarationSpecifier> ::= <StorageClassSpecifier> | <TypeSpecifier> | <TypeQualifier> | <FunctionSpecifier> |
 *                                  <GNUAttributes>
 */
using DeclarationSpecifier =
    std::variant<StorageClassSpecifier, TypeSpecifier, TypeQualifier, FunctionSpecifier, GNUAttributes>;

/**
 * [GNU]:
 *
 * <GNUASMString> ::= <TokenType::StringLiteral>
 * StringLiteral must be a vanilla string however (no suffixes)
 *
 * <GNUSimpleASM> ::= <TokenType::GNUASM> <TokenType::OpenParentheses> <GNUASMString>
 *                    <TokenType::CloseParentheses>
 */
class GNUSimpleASM final : public Node
{
    std::string m_string;

public:
    GNUSimpleASM(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::string string);
};

/**
 * <InitDeclarator> ::= <Declarator> [ <TokenType::Assignment> <Initializer> ]
 *
 * [GNU]: <InitDeclarator> ::= <Declarator> [ <TokenType::Assignment> <Initializer> ] [<GNUSimpleASM>] [<GNUAttribute>]
 *
 * <Declaration> ::= <DeclarationSpecifier> {<DeclarationSpecifier>} [<InitDeclarator>
 *                   { <TokenType::Comma> [<GNUAttribute>] <InitDeclarator> } ] <TokenType::SemiColon>
 */
class Declaration final : public Node
{
public:
    struct InitDeclarator
    {
        std::unique_ptr<Declarator> declarator;           // NOT NULL
        std::unique_ptr<Initializer> optionalInitializer; // nullable
        std::optional<GNUSimpleASM> optionalSimpleASM;
        std::optional<GNUAttributes> optionalAfterAttributes;
        std::optional<GNUAttributes>
            optionalBeforeAttributes{}; // not allowed to be set for the first InitDeclarator of a Declaration
    };

private:
    std::vector<DeclarationSpecifier> m_declarationSpecifiers;
    std::vector<InitDeclarator> m_initDeclarators;

public:
    Declaration(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                std::vector<DeclarationSpecifier>&& declarationSpecifiers,
                std::vector<InitDeclarator>&& initDeclarators);

    [[nodiscard]] const std::vector<DeclarationSpecifier>& getDeclarationSpecifiers() const;

    [[nodiscard]] const std::vector<InitDeclarator>& getInitDeclarators() const;
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
 *                              | <DirectAbstractDeclaratorStatic>
 */
using DirectAbstractDeclarator =
    std::variant<DirectAbstractDeclaratorParentheses, DirectAbstractDeclaratorAssignmentExpression,
                 DirectAbstractDeclaratorAsterisk, DirectAbstractDeclaratorParameterTypeList,
                 DirectAbstractDeclaratorStatic>;

/**
 * <DirectAbstractDeclaratorParentheses> ::= <TokenType::OpenParentheses> [<GNUAttributes>] <AbstractDeclarator>
 *                                           <TokenType::CloseParentheses>
 */
class DirectAbstractDeclaratorParentheses final : public Node
{
    std::optional<GNUAttributes> m_optionalAttributes;
    std::unique_ptr<AbstractDeclarator> m_abstractDeclarator;

public:
    DirectAbstractDeclaratorParentheses(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                        std::optional<GNUAttributes>&& optionalAttributes,
                                        std::unique_ptr<AbstractDeclarator>&& abstractDeclarator);

    [[nodiscard]] const AbstractDeclarator& getAbstractDeclarator() const;

    [[nodiscard]] const std::optional<GNUAttributes>& getOptionalAttributes() const;
};

/**
 * <DirectAbstractDeclaratorAssignmentExpression> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket>
 *                                                    {<TypeQualifier> } [<AssignmentExpression>]
 * <TokenType::CloseSquareBracket>
 */
class DirectAbstractDeclaratorAssignmentExpression final : public Node
{
    std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
    std::vector<TypeQualifier> m_typeQualifiers;
    std::unique_ptr<AssignmentExpression> m_assignmentExpression;

public:
    DirectAbstractDeclaratorAssignmentExpression(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                                 std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
                                                 std::vector<TypeQualifier>&& typeQualifiers,
                                                 std::unique_ptr<AssignmentExpression>&& assignmentExpression);

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;

    [[nodiscard]] const AssignmentExpression* getAssignmentExpression() const;
};

/**
 * <DirectAbstractDeclaratorStatic> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket>
 *                                      (<TokenType::StaticKeyword> {<TypeQualifiers>} | <TypeQualifiers>
 *                                      {<TypeQualifiers>} <TokenType::StaticKeyword>)
 *                                      <AssignmentExpression> <TokenType::CloseSquareBracket>
 */
class DirectAbstractDeclaratorStatic final : public Node
{
    std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
    Lexer::CTokenIterator m_staticLoc;
    std::vector<TypeQualifier> m_typeQualifiers;
    AssignmentExpression m_assignmentExpression;

public:
    DirectAbstractDeclaratorStatic(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                   std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
                                   Lexer::CTokenIterator staticLoc, std::vector<TypeQualifier>&& typeQualifiers,
                                   AssignmentExpression&& assignmentExpression);

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;

    [[nodiscard]] const AssignmentExpression& getAssignmentExpression() const;

    [[nodiscard]] Lexer::CTokenIterator getStaticLoc() const;
};

/**
 * <DirectAbstractDeclaratorAsterisk> ::= [<DirectAbstractDeclarator>] <TokenType::OpenSquareBracket>
 *                                        <TokenType::Asterisk> <TokenType::CloseSquareBracket>
 */
class DirectAbstractDeclaratorAsterisk final : public Node
{
    std::unique_ptr<DirectAbstractDeclarator> m_directAbstractDeclarator;
    Lexer::CTokenIterator m_asterisk;

public:
    DirectAbstractDeclaratorAsterisk(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                     std::unique_ptr<DirectAbstractDeclarator>&& directAbstractDeclarator,
                                     Lexer::CTokenIterator asterisk);

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;

    [[nodiscard]] Lexer::CTokenIterator getAsterisk() const;
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
                       std::optional<DirectAbstractDeclarator>&& directAbstractDeclarator = {});

    [[nodiscard]] const std::vector<Pointer>& getPointers() const;

    [[nodiscard]] const DirectAbstractDeclarator* getDirectAbstractDeclarator() const;
};

/**
 * <ParameterDeclaration> ::= <DeclarationSpecifier> { <DeclarationSpecifier> } <Declarator>
 *                          | <DeclarationSpecifier> { <DeclarationSpecifier> } [ <AbstractDeclarator> ]
 *
 * [GNU]: <ParameterDeclaration> ::= <DeclarationSpecifier> { <DeclarationSpecifier> } <Declarator> [<GNUAttribute>]
 *                                 | <DeclarationSpecifier> { <DeclarationSpecifier> } [ <AbstractDeclarator> ]
 *                                   [<GNUAttribute>]
 */
struct ParameterDeclaration : public Node
{
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    std::variant<std::unique_ptr<Declarator>, std::unique_ptr<AbstractDeclarator>> declarator;

    ParameterDeclaration(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                         std::vector<DeclarationSpecifier> declarationSpecifiers,
                         std::variant<std::unique_ptr<Declarator>, std::unique_ptr<AbstractDeclarator>> variant =
                             std::unique_ptr<AbstractDeclarator>{})
        : Node(begin, end), declarationSpecifiers(std::move(declarationSpecifiers)), declarator(std::move(variant))
    {
    }
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
    DirectDeclaratorIdentifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                               Lexer::CTokenIterator identifierLoc);

    [[nodiscard]] Lexer::CTokenIterator getIdentifierLoc() const;
};

/**
 * <DirectDeclaratorParentheses> ::= <TokenType::OpenParentheses> <DirectDeclarator> <TokenType::CloseParentheses>
 *
 * [GNU]: <DirectDeclaratorParentheses> ::= <TokenType::OpenParentheses> [<GNUAttributes>] <DirectDeclarator>
 *                                          <TokenType::CloseParentheses>
 */
class DirectDeclaratorParentheses final : public Node
{
    std::optional<GNUAttributes> m_optionalAttributes;
    std::unique_ptr<Declarator> m_declarator;

public:
    DirectDeclaratorParentheses(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                                std::optional<GNUAttributes>&& optionalAttributes,
                                std::unique_ptr<Declarator>&& declarator);

    [[nodiscard]] const Declarator& getDeclarator() const;

    [[nodiscard]] const std::optional<GNUAttributes>& getOptionalAttributes() const;
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
    Lexer::CTokenIterator m_asterisk;

public:
    DirectDeclaratorAsterisk(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                             DirectDeclarator&& directDeclarator, std::vector<TypeQualifier>&& typeQualifiers,
                             Lexer::CTokenIterator asterisk);

    [[nodiscard]] const DirectDeclarator& getDirectDeclarator() const;

    [[nodiscard]] const std::vector<TypeQualifier>& getTypeQualifiers() const;

    [[nodiscard]] Lexer::CTokenIterator getAsterisk() const;
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
 * <DirectDeclaratorStatic> ::= <DirectDeclarator> <TokenType::OpenSquareBracket> (<TokenType::StaticKeyword>
 *                              {<TypeQualifier>} | <TypeQualifier> {<TypeQualifier>}
 *                              <TokenType::StaticKeyword>) <AssignmentExpression> <TokenType::CloseSquareBracket>
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
 *
 * [GNU]: <StructOrUnionSpecifier> ::= { <TokenType::GNUExtension> } <StructOrUnion> [<GNUAttribute>]
 *                                     [ <TokenType::Identifier> ] <TokenType::OpenBrace> <StructDeclaration>
 *                                     { <StructDeclaration> } <TokenType::CloseBrace> [<GNUAttribute>]
 *                                   | <StructOrUnion> [<GNUAttribute>] <TokenType::Identifier>
 */
class StructOrUnionSpecifier final : public Node
{
    bool m_isUnion;
    std::optional<GNUAttributes> m_optionalBeforeAttribute;
    const Lexer::CToken* m_identifierLoc;

public:
    /**
     *
     * <StructDeclarator> ::= <Declarator> | [<Declarator>] <TokenType::Colon> <ConstantExpression>
     *
     * [GNU]: <StructDeclarator> ::= <Declarator> [<GNUAttributes>] | [<Declarator>] <TokenType::Colon>
     *                               <ConstantExpression> [<GNUAttributes>]
     *
     * <StructDeclaration> ::= <SpecifierQualifier> { <SpecifierQualifier> }
     *                         [<StructDeclarator> {<TokenType::Comma> [<GNUAttributes>] <StructDeclarator> }]
     *                         <TokenType::SemiColon>
     */
    struct StructDeclaration
    {
        std::vector<SpecifierQualifier> specifierQualifiers;
        struct StructDeclarator
        {
            std::optional<GNUAttributes> optionalBeforeAttributes; // guaranteed empty for the first declarator
            std::unique_ptr<Declarator> optionalDeclarator;
            std::optional<ConstantExpression> optionalBitfield;
            std::optional<GNUAttributes> optionalAfterAttributes;
        };
        std::vector<StructDeclarator> structDeclarators;
    };

private:
    std::vector<StructDeclaration> m_structDeclarations;
    std::optional<GNUAttributes> m_optionalAfterAttribute;
    bool m_extensionEnabled;

public:
    StructOrUnionSpecifier(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, bool isUnion,
                           std::optional<GNUAttributes>&& optionalBeforeAttribute, const Lexer::CToken* identifierLoc,
                           std::vector<StructDeclaration>&& structDeclarations,
                           std::optional<GNUAttributes>&& optionalAfterAttribute, bool extensionsEnabled);

    [[nodiscard]] bool isUnion() const;

    [[nodiscard]] const Lexer::CToken* getIdentifierLoc() const;

    [[nodiscard]] const std::vector<StructDeclaration>& getStructDeclarations() const;

    [[nodiscard]] bool extensionsEnabled() const;
};

/**
 * <EnumDeclaration> ::= <TokenType::EnumKeyword> [ <TokenType::Identifier> ] <TokenType::OpenBrace>
 *                       <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> ]
 *                       { <TokenType::Comma> <TokenType::Identifier> [ <TokenType::Assignment> <ConstantExpression> }
 *                       [ <TokenType::Comma> ] <TokenType::CloseBrace>
 *
 * [GNU]: <EnumDeclaration> ::= <TokenType::EnumKeyword> [<GNUAttribute>] [ <TokenType::Identifier> ]
 *                              <TokenType::OpenBrace> <TokenType::Identifier> [<GNUAttribute>] [<TokenType::Assignment>
 *                              <ConstantExpression> ]
 *                              { <TokenType::Comma> <TokenType::Identifier> [<GNUAttribute>] [ <TokenType::Assignment>
 *                              <ConstantExpression> } [ <TokenType::Comma> ] <TokenType::CloseBrace> [<GNUAttribute>]
 */
class EnumDeclaration final : public Node
{
    std::optional<GNUAttributes> m_beforeAttributes;
    const Lexer::CToken* m_name;

public:
    struct EnumValue
    {
        Lexer::CTokenIterator name;
        std::optional<GNUAttributes> optionalAttributes;
        std::optional<ConstantExpression> value;
    };

private:
    std::vector<EnumValue> m_values;
    std::optional<GNUAttributes> m_afterAttributes;

public:
    EnumDeclaration(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
                    std::optional<GNUAttributes>&& beforeAttributes, const Lexer::CToken* name,
                    std::vector<EnumValue>&& values, std::optional<GNUAttributes>&& afterAttributes);

    [[nodiscard]] const Lexer::CToken* getName() const;

    [[nodiscard]] const std::vector<EnumValue>& getValues() const;
};

/**
 * <EnumSpecifier> ::= <EnumDeclaration> | <TokenType::EnumKeyword> <TokenType::Identifier>
 *
 * [GNU]: <EnumSpecifier> ::= <EnumDeclaration> | <TokenType::EnumKeyword> [<GNUAttributes>] <TokenType::Identifier>
 */
class EnumSpecifier final : public Node
{
public:
    struct EnumTag
    {
        std::optional<GNUAttributes> optionalAttributes;
        const Lexer::CToken* CLD_NULLABLE identifier;
    };

private:
    using Variant = std::variant<EnumDeclaration, EnumTag>;

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
        Int128 = 0b100000000000,
        MAX_VALUE = Int128
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
 *
 * [GNU]: <Pointer> ::= <TokenType::Asterisk> { <TypeQualifier> | <GNUAttributes> }
 */
class Pointer final : public Node
{
    std::vector<std::variant<TypeQualifier, GNUAttributes>> m_typeQualifiers;

public:
    Pointer(Lexer::CTokenIterator begin, Lexer::CTokenIterator end,
            std::vector<std::variant<TypeQualifier, GNUAttributes>>&& typeQualifiers);

    [[nodiscard]] const std::vector<std::variant<TypeQualifier, GNUAttributes>>& getTypeQualifiers() const;
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
    using Designator = std::variant<ConstantExpression, Lexer::CTokenIterator>;

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
 *                          <CompoundStatement>
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
 *
 * [GNU]: <ExternalDeclaration> ::= { <TokenType::GNUExtension> } (<FunctionDefinition> | <Declaration> |
 *                                    [<GNUSimpleASM>] <TokenType::SemiColon> )
 */
using ExternalDeclaration = std::variant<Declaration, FunctionDefinition, GNUSimpleASM>;

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

/**
 * <GNUASMQualifier> ::= <TokenType::VolatileKeyword>
 *                      | <TokenType::InlineKeyword>
 *                      | <TokenType::GotoKeyword>
 */
class GNUASMQualifier : public Node
{
public:
    enum Qualifier
    {
        Volatile,
        Inline,
        Goto
    };

private:
    Qualifier m_qualifier;

public:
    explicit GNUASMQualifier(Lexer::CTokenIterator qualifier);
};

/**
 * [GNU]:
 *
 *
 * <GNUASMStatement> ::= <TokenType::GNUASM> {<GNUASMQualifier>} <TokenType::OpenParentheses> <GNUASMArgument>
 *                       <TokenType::CloseParentheses> <TokenType::SemiColon>
 *
 * <GNUASMArgument> ::= <GNUASMString>
 *                    | <GNUASMString> <TokenType::Colon> [<GNUASMOperands>]
 *                    | <GNUASMString> <TokenType::Colon> [<GNUASMOperands>] <TokenType::Colon> [<GNUASMOperands>]
 *                    | <GNUASMString> <TokenType::Colon> [<GNUASMOperands>] <TokenType::Colon> [<GNUASMOperands>]
 *                      <TokenType::Colon> <GNUASMClobbers>
 *
 * <GNUASMOperands> ::= <GNUASMOperand> { <TokenType::Comma> <GNUASMOperand> }
 *
 * <GNUASMOperand> ::= [ <TokenType::OpenSquareBracket> <TokenType::Identifier> <TokenType::CloseSquareBracket>]
 *                     <GNUASMString> <TokenType::OpenParentheses> <Expression> <TokenType::CloseParentheses>
 *
 * <GNUASMClobbers> ::= <GNUASMString> { <TokenType::Colon> <GNUASMString> }
 */
class GNUASMStatement final : public Node
{
    std::vector<GNUASMQualifier> m_qualifiers;
    std::string m_asmString;

public:
    struct GNUASMOperand
    {
        const Lexer::CToken* optionalIdentifier;
        std::string string;
        Expression expression;
    };

private:
    std::vector<GNUASMOperand> m_firstList;
    std::vector<GNUASMOperand> m_secondList;
    std::vector<std::string> m_clobbers;

public:
    GNUASMStatement(Lexer::CTokenIterator begin, Lexer::CTokenIterator end, std::vector<GNUASMQualifier> qualifiers,
                    std::string asmString, std::vector<GNUASMOperand> firstList, std::vector<GNUASMOperand> secondList,
                    std::vector<std::string> clobbers);
};

} // namespace cld::Syntax

#include "Parser.hpp"

#include "ConstantEvaluator.hpp"

#include <algorithm>
#include <numeric>
#include <stack>

using namespace OpenCL;
using namespace OpenCL::Lexer;
using namespace OpenCL::Syntax;

std::pair<OpenCL::Syntax::TranslationUnit, bool> OpenCL::Parser::buildTree(const std::vector<Lexer::Token>& tokens,
                                                                           std::ostream* reporter)
{
    ParsingContext context(reporter);
    auto begin = tokens.cbegin();
    return {parseTranslationUnit(begin, tokens.cend(), context), !context.isErrorsOccured()};
}

namespace
{
    bool isAssignment(TokenType type)
    {
        return type == TokenType::Assignment || type == TokenType::PlusAssign || type == TokenType::MinusAssign
            || type == TokenType::DivideAssign || type == TokenType::MultiplyAssign
            || type == TokenType::ModuloAssign || type == TokenType::ShiftLeftAssign
            || type == TokenType::ShiftRightAssign || type == TokenType::BitAndAssign
            || type == TokenType::BitOrAssign || type == TokenType::BitXorAssign;
    }

    bool expect(TokenType expected,
                OpenCL::Parser::Tokens::const_iterator begin,
                std::vector<OpenCL::Lexer::Token>::const_iterator& curr,
                OpenCL::Parser::Tokens::const_iterator end,
                OpenCL::Parser::ParsingContext& context)
    {
        if (curr >= end || curr->getTokenType() != expected)
        {
            if (curr >= end)
            {
                context.logError({OpenCL::Parser::ErrorMessages::EXPECTED_N.args(Token(0, 0, 0, expected).emitBack()),
                                  begin, findEOL(begin, end),
                                  Modifier{end - 1, end, Modifier::PointAtEnd}});
            }
            else
            {
                context.logError({OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                      .args(Token(0, 0, 0, expected).emitBack(), curr->emitBack()), begin,
                                  findEOL(begin, end),
                                  Modifier{curr, curr + 1, Modifier::PointAtBeginning}});
            }
            curr++;
            return false;
        }
        else
        {
            curr++;
            return true;
        }
    }

    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&& ... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G)->Y<G>;

    //firstIsInTranslationUnit not needed

    //firstIsInExternalDeclaration not needed

    bool firstIsInFunctionDefinition(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDeclaration(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDeclarationSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInSpecifierQualifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDirectDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInParameterTypeList(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInAbstractDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInDirectAbstractDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInParameterList(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInPointer(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInStructOrUnionSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInEnumSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInEnumDeclaration(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInCompoundStatement(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInCompoundItem(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInInitializer(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInInitializerList(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInStatement(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInAssignmentExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInConditionalExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInLogicalOrExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInLogicalAndExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInBitOrExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInBitXorExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInBitAndExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInEqualityExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInRelationalExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInShiftExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInAdditiveExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInTerm(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInTypeName(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInCastExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInUnaryExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInPostFixExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInPrimaryExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool firstIsInFunctionDefinition(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInDeclarationSpecifier(token, context);
    }

    bool firstIsInDeclaration(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInDeclarationSpecifier(token, context);
    }

    bool firstIsInDeclarationSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        switch (token.getTokenType())
        {
        case TokenType::TypedefKeyword:
        case TokenType::ExternKeyword:
        case TokenType::StaticKeyword:
        case TokenType::AutoKeyword:
        case TokenType::RegisterKeyword:
        case TokenType::VoidKeyword:
        case TokenType::CharKeyword:
        case TokenType::ShortKeyword:
        case TokenType::IntKeyword:
        case TokenType::LongKeyword:
        case TokenType::FloatKeyword:
        case TokenType::DoubleKeyword:
        case TokenType::SignedKeyword:
        case TokenType::UnsignedKeyword:
        case TokenType::EnumKeyword:
        case TokenType::StructKeyword:
        case TokenType::UnionKeyword:
        case TokenType::ConstKeyword:
        case TokenType::RestrictKeyword:
        case TokenType::VolatileKeyword:
        case TokenType::InlineKeyword: return true;
        case TokenType::Identifier:
            return !context.isInScope(std::get<std::string>(token.getValue()))
                && context.isTypedef(std::get<std::string>(token.getValue()));
        default: return false;
        }
    }

    bool firstIsInSpecifierQualifier(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        switch (token.getTokenType())
        {
        case TokenType::VoidKeyword:
        case TokenType::CharKeyword:
        case TokenType::ShortKeyword:
        case TokenType::IntKeyword:
        case TokenType::LongKeyword:
        case TokenType::FloatKeyword:
        case TokenType::DoubleKeyword:
        case TokenType::SignedKeyword:
        case TokenType::UnsignedKeyword:
        case TokenType::EnumKeyword:
        case TokenType::StructKeyword:
        case TokenType::UnionKeyword:
        case TokenType::ConstKeyword:
        case TokenType::RestrictKeyword:
        case TokenType::VolatileKeyword:return true;
        case TokenType::Identifier:
            return !context.isInScope(std::get<std::string>(token.getValue()))
                && context.isTypedef(std::get<std::string>(token.getValue()));
        default: return false;
        }
    }

    bool firstIsInDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInPointer(token, context) || firstIsInDirectDeclarator(token, context);
    }

    bool firstIsInDirectDeclarator(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::Identifier || token.getTokenType() == TokenType::OpenParenthese;
    }

    bool firstIsInParameterTypeList(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInParameterList(token, context);
    }

    bool firstIsInAbstractDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInPointer(token, context) || firstIsInDirectAbstractDeclarator(token, context);
    }

    bool firstIsInDirectAbstractDeclarator(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::OpenParenthese
            || token.getTokenType() == TokenType::OpenSquareBracket;
    }

    bool firstIsInParameterList(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInDeclarationSpecifier(token, context);
    }

    bool firstIsInPointer(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::Asterisk;
    }

    bool firstIsInStructOrUnionSpecifier(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::StructKeyword || token.getTokenType() == TokenType::UnionKeyword;
    }

    bool firstIsInEnumSpecifier(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::EnumKeyword;
    }

    bool firstIsInEnumDeclaration(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::EnumKeyword;
    }

    bool firstIsInCompoundStatement(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::OpenBrace;
    }

    bool firstIsInCompoundItem(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInDeclaration(token, context) || firstIsInStatement(token, context);
    }

    bool firstIsInInitializer(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInAssignmentExpression(token, context) || token.getTokenType() == TokenType::OpenBrace;
    }

    bool firstIsInInitializerList(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return token.getTokenType() == TokenType::OpenSquareBracket || token.getTokenType() == TokenType::Dot
            || firstIsInInitializer(token, context);
    }

    bool firstIsInStatement(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return token.getTokenType() == TokenType::IfKeyword || token.getTokenType() == TokenType::ForKeyword
            || token.getTokenType() == TokenType::SwitchKeyword || token.getTokenType() == TokenType::ContinueKeyword
            || token.getTokenType() == TokenType::BreakKeyword || token.getTokenType() == TokenType::CaseKeyword
            || token.getTokenType() == TokenType::DefaultKeyword || token.getTokenType() == TokenType::Identifier
            || token.getTokenType() == TokenType::DoKeyword || token.getTokenType() == TokenType::WhileKeyword
            || token.getTokenType() == TokenType::ReturnKeyword || token.getTokenType() == TokenType::GotoKeyword
            || firstIsInExpression(token, context);
    }

    bool firstIsInExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInAssignmentExpression(token, context);
    }

    bool firstIsInAssignmentExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInUnaryExpression(token, context) || firstIsInConditionalExpression(token, context);
    }

    bool firstIsInConditionalExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInLogicalOrExpression(token, context);
    }

    bool firstIsInLogicalOrExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInLogicalAndExpression(token, context);
    }

    bool firstIsInLogicalAndExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInBitOrExpression(token, context);
    }

    bool firstIsInBitOrExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInBitXorExpression(token, context);
    }

    bool firstIsInBitXorExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInBitAndExpression(token, context);
    }

    bool firstIsInBitAndExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInEqualityExpression(token, context);
    }

    bool firstIsInEqualityExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInRelationalExpression(token, context);
    }

    bool firstIsInRelationalExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInShiftExpression(token, context);
    }

    bool firstIsInShiftExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInAdditiveExpression(token, context);
    }

    bool firstIsInAdditiveExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInTerm(token, context);
    }

    bool firstIsInTerm(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInCastExpression(token, context);
    }

    bool firstIsInTypeName(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInSpecifierQualifier(token, context);
    }

    bool firstIsInCastExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return token.getTokenType() == TokenType::OpenParenthese || firstIsInUnaryExpression(token, context);
    }

    bool firstIsInUnaryExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return firstIsInPostFixExpression(token, context) || token.getTokenType() == TokenType::Increment
            || token.getTokenType() == TokenType::Decrement || token.getTokenType() == TokenType::Ampersand
            || token.getTokenType() == TokenType::Plus || token.getTokenType() == TokenType::Minus
            || token.getTokenType() == TokenType::BitWiseNegation || token.getTokenType() == TokenType::LogicalNegation
            || token.getTokenType() == TokenType::SizeofKeyword;
    }

    bool firstIsInPostFixExpression(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        return token.getTokenType() == TokenType::OpenParenthese || firstIsInPrimaryExpression(token, context);
    }

    bool firstIsInPrimaryExpression(const Token& token, const OpenCL::Parser::ParsingContext&)
    {
        return token.getTokenType() == TokenType::OpenParenthese || token.getTokenType() == TokenType::Identifier
            || token.getTokenType() == TokenType::Literal || token.getTokenType() == TokenType::StringLiteral;
    }

    bool followsFunctionDefinition(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsDeclaration(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsDeclarationSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsSpecifierQualifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsDirectDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsParameterTypeList(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsAbstractDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsDirectAbstractDeclarator(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsParameterList(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsPointer(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsStructOrUnionSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsEnumSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsEnumDeclaration(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsCompoundStatement(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsCompoundItem(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsInitializer(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsInitializerList(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsStatement(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsAssignmentExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsConditionalExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsLogicalOrExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsLogicalAndExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsBitOrExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsBitXorExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsBitAndExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsEqualityExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsRelationalExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsShiftExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsAdditiveExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsTerm(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsTypeName(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsCastExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsUnaryExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsPostFixExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool followsPrimaryExpression(const Token& token, const OpenCL::Parser::ParsingContext& context);

    bool isPostFixOperator(const Token& token)
    {
        switch (token.getTokenType())
        {
        case TokenType::Arrow:
        case TokenType::Dot:
        case TokenType::OpenSquareBracket:
        case TokenType::Identifier:
        case TokenType::OpenParenthese:
        case TokenType::Literal:
        case TokenType::Increment:
        case TokenType::Decrement: return true;
        default: break;
        }
        return false;
    }
} // namespace

OpenCL::Syntax::TranslationUnit
OpenCL::Parser::parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                     ParsingContext& context)
{
    std::vector<ExternalDeclaration> global;
    while (begin != end)
    {
        auto result = parseExternalDeclaration(begin, end, context);
        if (result)
        {
            global.push_back(std::move(*result));
        }
    }
    return TranslationUnit(std::move(global));
}

std::optional<Syntax::ExternalDeclaration>
OpenCL::Parser::parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                         ParsingContext& context)
{
    auto start = begin;
    //backtracking
    return context.doBacktracking([&]() -> std::optional<Syntax::ExternalDeclaration>
                                  {
                                      auto functionBranch = context.createBranch(begin);
                                      auto function = parseFunctionDefinition(functionBranch->getCurrent(),
                                                                              end,
                                                                              context);
                                      if (*functionBranch)
                                      {
                                          return ExternalDeclaration(start, begin, std::move(*function));
                                      }

                                      auto declarationBranch = context.createBranch(begin);
                                      auto
                                          declaration = parseDeclaration(declarationBranch->getCurrent(), end, context);
                                      if (*declarationBranch)
                                      {
                                          return ExternalDeclaration(start, begin, std::move(*declaration));
                                      }
                                      else
                                      {
                                          return {};
                                      }
                                  }, [](Tokens::const_iterator begin, Tokens::const_iterator end) -> bool
                                  {
                                      return std::any_of(begin, end, [](const Token& token)
                                      {
                                          return token.getTokenType() == TokenType::OpenParenthese
                                              || token.getTokenType() == TokenType::CloseParenthese;
                                      });
                                  });
}

std::optional<Syntax::Declaration>
OpenCL::Parser::parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    while (begin < end && firstIsInDeclarationSpecifier(*begin, context))
    {
        auto result = parseDeclarationSpecifier(begin, end, context);
        if (!result)
        {
            return {};
        }
        declarationSpecifiers.push_back(std::move(*result));
    }
    if (declarationSpecifiers.empty())
    {
        context.logError({ErrorMessages::MISSING_DECLARATION_SPECIFIER, start, findSemicolonOrEOL(start, end),
                          Modifier{begin, begin + 1, Modifier::PointAtBeginning}});
    }
    if (begin >= end || begin->getTokenType() == TokenType::SemiColon)
    {
        if (begin < end)
        {
            begin++;
        }
        else
        {
            context.logError({ErrorMessages::EXPECTED_N.args(';'), start, findSemicolonOrEOL(start, end),
                              Modifier{end - 1, end, Modifier::PointAtEnd}});
        }
        return Declaration(start, begin, std::move(declarationSpecifiers), {});
    }
    std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> initDeclarators;
    bool first = true;
    do
    {
        if (first)
        {
            first = false;
        }
        else if (begin < end && begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        auto declarator = parseDeclarator(begin, end, context);
        if (!declarator)
        {
            return {};
        }
        context.addToScope(Semantics::declaratorToName(*declarator));
        if (begin >= end || begin->getTokenType() != TokenType::Assignment)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        }
        else
        {
            begin++;
            auto initializer = parseInitializer(begin, end, context);
            if (!initializer)
            {
                return {};
            }
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::make_unique<Initializer>(std::move(*initializer)));
        }
    }
    while (true);
    expect(TokenType::SemiColon, start, begin, end, context);
    if (auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifiers.front());
        storage && storage->getSpecifier() == StorageClassSpecifier::Typedef)
    {
        for (auto&[declator, init] : initDeclarators)
        {
            (void)init;
            auto visitor = [](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, DirectDeclaratorIdentifier>)
                {
                    (void)self;
                    return value.getIdentifier();
                }
                else if constexpr (std::is_same_v<T, DirectDeclaratorParenthese>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value.getDeclarator().getDirectDeclarator());
                }
                else
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value.getDirectDeclarator());
                }
            };
            context.addTypedef(std::visit(Y{visitor}, declator->getDirectDeclarator()));
        }
    }
    return Declaration(start, begin, std::move(declarationSpecifiers), std::move(initDeclarators));
}

std::optional<Syntax::DeclarationSpecifier>
OpenCL::Parser::parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    auto currToken = *begin;
    switch (begin->getTokenType())
    {
    case TokenType::TypedefKeyword:
    case TokenType::ExternKeyword:
    case TokenType::StaticKeyword:
    case TokenType::AutoKeyword:
    case TokenType::RegisterKeyword:
    case TokenType::ConstKeyword:
    case TokenType::RestrictKeyword:
    case TokenType::VolatileKeyword:
    case TokenType::InlineKeyword:
    case TokenType::VoidKeyword:
    case TokenType::CharKeyword:
    case TokenType::ShortKeyword:
    case TokenType::IntKeyword:
    case TokenType::LongKeyword:
    case TokenType::FloatKeyword:
    case TokenType::DoubleKeyword:
    case TokenType::SignedKeyword:
    case TokenType::Identifier:
    case TokenType::UnsignedKeyword: begin++;
    default: break;
    }
    switch (currToken.getTokenType())
    {
    case TokenType::TypedefKeyword:
        return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Typedef)};
    case TokenType::ExternKeyword:
        return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Extern)};
    case TokenType::StaticKeyword:
        return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Static)};
    case TokenType::AutoKeyword:
        return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Auto)};
    case TokenType::RegisterKeyword:
        return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Register)};
    case TokenType::ConstKeyword: return DeclarationSpecifier{TypeQualifier(start, start + 1, TypeQualifier::Const)};
    case TokenType::RestrictKeyword:
        return DeclarationSpecifier{TypeQualifier(start, start + 1, TypeQualifier::Restrict)};
    case TokenType::VolatileKeyword:
        return DeclarationSpecifier{TypeQualifier(start, start + 1, TypeQualifier::Volatile)};
    case TokenType::InlineKeyword: return DeclarationSpecifier{FunctionSpecifier{start, start + 1}};
    case TokenType::VoidKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case TokenType::CharKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case TokenType::ShortKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case TokenType::IntKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case TokenType::LongKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
    case TokenType::FloatKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
    case TokenType::DoubleKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
    case TokenType::SignedKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
    case TokenType::UnsignedKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:
    {
        auto expected = parseStructOrUnionSpecifier(begin, end, context);
        if (expected)
        {
            auto name = expected->getIdentifier();
            auto isDefinition = !expected->getStructDeclarations().empty();
            auto result =
                TypeSpecifier(start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)));
            if (isDefinition)
            {
                auto type = Semantics::declaratorsToType({std::cref(result)});
                if (!type)
                {
                    return {};
                }
                context.structOrUnions.emplace(name, std::get<Semantics::RecordType>(type->get()));
            }
            return DeclarationSpecifier{std::move(result)};
        }
        else
        {
            return {};
        }
    }
    case TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            return DeclarationSpecifier{
                TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return {};
        }
    }
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.isTypedef(name))
        {
            return Syntax::DeclarationSpecifier{TypeSpecifier(start, begin, name)};
        }
        else if (context.isTypedef(name))
        {
            context.logError({
                                 "\"" + name
                                     + "\" is a typedef but cannot be used as such because another symbol overshadows it"});
            return {};
        }
        break;
    }
    default: break;
    }
    context.logError({"Invalid token for declaration specifier"});
    return {};
}

std::optional<OpenCL::Syntax::StructOrUnionSpecifier>
OpenCL::Parser::parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                            OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {

    }
    auto start = begin;
    bool isUnion;
    if (begin->getTokenType() == TokenType::StructKeyword)
    {
        isUnion = false;
    }
    else if (begin->getTokenType() == TokenType::UnionKeyword)
    {
        isUnion = true;
    }
    else
    {
        context.logError({"Expected struct or union keyword at beginning of struct or union specifier"});
        return {};
    }
    begin++;
    if (begin >= end || begin->getTokenType() != TokenType::Identifier)
    {
        context.logError({std::string("Expected identifier after ") + (isUnion ? "union" : "struct")});
        return StructOrUnionSpecifier(start, begin, isUnion, "", {});
    }
    const auto& name = std::get<std::string>(begin->getValue());
    begin++;
    if (begin >= end || begin->getTokenType() != TokenType::OpenBrace)
    {
        return StructOrUnionSpecifier(start, begin, isUnion, name, {});
    }
    begin++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    do
    {
        std::vector<SpecifierQualifier> specifierQualifiers;
        while (begin < end && firstIsInSpecifierQualifier(*begin, context))
        {
            auto result = parseSpecifierQualifier(begin, end, context);
            if (!result)
            {
                return {};
            }
            specifierQualifiers.push_back(std::move(*result));
        }
        if (specifierQualifiers.empty())
        {
            context.logError({"Expected Specifier Qualifiers at beginning of struct declarations"});
        }
        std::vector<std::pair<std::unique_ptr<Declarator>, std::int64_t>> declarators;
        bool first = true;
        do
        {
            if (first)
            {
                first = false;
            }
            else if (begin < end && begin->getTokenType() == TokenType::Comma)
            {
                begin++;
            }
            else
            {
                break;
            }
            if (begin < end && begin->getTokenType() == TokenType::Colon)
            {
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    return {};
                }
                Semantics::ConstantEvaluator evaluator(context.structOrUnions);
                auto value = evaluator.visit(*constant);
                if (!value)
                {
                    return {};
                }
                declarators.emplace_back(nullptr,
                                         std::visit(
                                             [](auto&& value) -> std::size_t
                                             {
                                                 using T = std::decay_t<decltype(value)>;
                                                 if constexpr (std::is_convertible_v<T, std::size_t>)
                                                 {
                                                     return value;
                                                 }
                                                 else
                                                 {
                                                     throw std::runtime_error("Invalid type of constant expression");
                                                 }
                                             },
                                             *value));
            }
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                return {};
            }
            if (begin < end && begin->getTokenType() == TokenType::Colon)
            {
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    return {};
                }
                Semantics::ConstantEvaluator evaluator(context.structOrUnions);
                auto value = evaluator.visit(*constant);
                if (!value)
                {
                    return {};
                }
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::visit(
                                             [](auto&& value) -> std::size_t
                                             {
                                                 using T = std::decay_t<decltype(value)>;
                                                 if constexpr (std::is_convertible_v<T, std::size_t>)
                                                 {
                                                     return value;
                                                 }
                                                 else
                                                 {
                                                     throw std::runtime_error("Invalid type of constant expression");
                                                 }
                                             },
                                             *value));
            }
            else
            {
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), -1);
            }
        }
        while (true);
        if (begin >= end || begin->getTokenType() != TokenType::SemiColon)
        {
            context.logError({std::string("Expected ; at the end of ") + (isUnion ? "union" : "struct")
                                  + " field declaration"});
        }
        begin++;
        structDeclarations.push_back({std::move(specifierQualifiers), std::move(declarators)});
    }
    while (begin < end && begin->getTokenType() != TokenType::CloseBrace);
    if (begin >= end || begin->getTokenType() != TokenType::CloseBrace)
    {
        context.logError({std::string("Expected } at the end of ") + (isUnion ? "union" : "struct") + " definition"});
    }
    else
    {
        begin++;
    }
    return StructOrUnionSpecifier(start, begin, isUnion, name, std::move(structDeclarations));
}

std::optional<OpenCL::Syntax::SpecifierQualifier>
OpenCL::Parser::parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    auto currToken = *begin;
    switch (currToken.getTokenType())
    {
    case TokenType::ConstKeyword:
    case TokenType::RestrictKeyword:
    case TokenType::VolatileKeyword:
    case TokenType::InlineKeyword:
    case TokenType::VoidKeyword:
    case TokenType::CharKeyword:
    case TokenType::ShortKeyword:
    case TokenType::IntKeyword:
    case TokenType::LongKeyword:
    case TokenType::FloatKeyword:
    case TokenType::DoubleKeyword:
    case TokenType::SignedKeyword:
    case TokenType::UnsignedKeyword: begin++;
    default: break;
    }
    switch (currToken.getTokenType())
    {
    case TokenType::ConstKeyword: return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Const)};
    case TokenType::RestrictKeyword:return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Restrict)};
    case TokenType::VolatileKeyword:return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Volatile)};
    case TokenType::VoidKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case TokenType::CharKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case TokenType::ShortKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case TokenType::IntKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case TokenType::LongKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
    case TokenType::FloatKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
    case TokenType::DoubleKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
    case TokenType::SignedKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
    case TokenType::UnsignedKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:
    {
        auto expected = parseStructOrUnionSpecifier(begin, end, context);
        if (expected)
        {
            auto name = expected->getIdentifier();
            bool isDefinition = !expected->getStructDeclarations().empty();
            auto result =
                TypeSpecifier(start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)));
            if (isDefinition)
            {
                auto type = Semantics::declaratorsToType({std::cref(result)});
                if (!type)
                {
                    return {};
                }
                context.structOrUnions.emplace(name, std::get<Semantics::RecordType>(type->get()));
            }
            return SpecifierQualifier{std::move(result)};
        }
        else
        {
            return {};
        }
    }
    case TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            return SpecifierQualifier{
                TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return {};
        }
    }
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.isTypedef(name))
        {
            return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, name)};
        }
        else if (context.isTypedef(name))
        {
            context.logError({
                                 "\"" + name
                                     + "\" is a typedef but cannot be used as such because another symbol overshadows it"});
            return {};
        }
        break;
    }
    default: break;
    }
    context.logError({"Invalid token for declaration specifier"});
    return {};
}

std::optional<OpenCL::Syntax::Declarator>
OpenCL::Parser::parseDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context);
        if (!result)
        {
            return {};
        }
        pointers.push_back(std::move(*result));
    }
    auto directDeclarator = parseDirectDeclarator(begin, end, context);
    if (!directDeclarator)
    {
        return {};
    }
    return Declarator(start, begin, std::move(pointers), std::move(*directDeclarator));
}

namespace
{

    std::optional<DirectDeclaratorNoStaticOrAsterisk> parseDirectDeclaratorNoStaticOrAsterisk(
        DirectDeclarator& declarator, OpenCL::Parser::Tokens::const_iterator& begin,
        OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        if (begin >= end)
        {
            return {};
        }
        auto start = Syntax::nodeFromNodeDerivedVariant(declarator).begin();
        if (begin >= end || begin->getTokenType() != TokenType::OpenSquareBracket)
        {
            context.logError({"Expected ["});
        }
        else
        {
            begin++;
        }
        std::vector<TypeQualifier> typeQualifiers;
        while (begin < end
            && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
                || begin->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (begin->getTokenType())
            {
            case TokenType::ConstKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
                break;
            default: break;
            }
            begin++;
        }
        auto assignment = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
        if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
        {
            context.logError({"Expected ] to close [ in direct declarator"});
        }
        else
        {
            begin++;
        }
        return DirectDeclaratorNoStaticOrAsterisk(
            start, begin, std::make_unique<DirectDeclarator>(std::move(declarator)), std::move(typeQualifiers),
            assignment ? std::make_unique<AssignmentExpression>(std::move(*assignment)) : nullptr);
    }

    std::optional<DirectDeclaratorStatic>
    parseDirectDeclaratorStatic(DirectDeclarator& declarator, OpenCL::Parser::Tokens::const_iterator& begin,
                                OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        if (begin >= end)
        {
            return {};
        }
        auto start = Syntax::nodeFromNodeDerivedVariant(declarator).begin();
        if (begin >= end || begin->getTokenType() != TokenType::OpenSquareBracket)
        {
            context.logError({"Expected ["});
        }
        else
        {
            begin++;
        }
        bool wasStatic = false;
        if (begin < end && begin->getTokenType() == TokenType::StaticKeyword)
        {
            wasStatic = true;
            begin++;
        }
        std::vector<TypeQualifier> typeQualifiers;
        while (begin < end
            && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
                || begin->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (begin->getTokenType())
            {
            case TokenType::ConstKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
                break;
            default: break;
            }
            begin++;
        }
        if (begin < end && begin->getTokenType() == TokenType::StaticKeyword)
        {
            if (wasStatic)
            {
                context.logError({"static appearing twice in direct declarator"});
            }
            begin++;
        }
        auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
        if (!assignmentExpression)
        {
            return {};
        }
        if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
        {
            context.logError({"Expected ]"});
        }
        else
        {
            begin++;
        }
        return DirectDeclaratorStatic(start, begin, std::make_unique<DirectDeclarator>(std::move(declarator)),
                                      std::move(typeQualifiers), std::move(*assignmentExpression));
    }

    std::optional<DirectDeclaratorAsterisk>
    parseDirectDeclaratorAsterisk(DirectDeclarator& declarator, OpenCL::Parser::Tokens::const_iterator& begin,
                                  OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        if (begin >= end || begin->getTokenType() != TokenType::OpenSquareBracket)
        {
            context.logError({"Expected ["});
        }
        auto start = Syntax::nodeFromNodeDerivedVariant(declarator).begin();
        begin++;
        std::vector<TypeQualifier> typeQualifiers;
        while (begin < end
            && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
                || begin->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (begin->getTokenType())
            {
            case TokenType::ConstKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword: typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
                break;
            default: break;
            }
            begin++;
        }
        if (begin >= end || begin->getTokenType() != TokenType::Asterisk)
        {
            context.logError({"Expected *"});
        }
        if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
        {
            context.logError({"Expected ]"});
        }
        else
        {
            begin++;
        }
        return DirectDeclaratorAsterisk(start, begin, std::move(declarator), std::move(typeQualifiers));
    }
} // namespace

std::optional<DirectDeclarator> OpenCL::Parser::parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    std::unique_ptr<DirectDeclarator> directDeclarator;
    while (begin < end
        && ((begin->getTokenType() == TokenType::Identifier && !directDeclarator)
            || begin->getTokenType() == TokenType::OpenParenthese
            || (begin->getTokenType() == TokenType::OpenSquareBracket && directDeclarator)))
    {
        switch (begin->getTokenType())
        {
        case TokenType::Identifier:
        {
            auto start = begin;
            auto currToken = *begin;
            begin++;
            directDeclarator =
                std::make_unique<DirectDeclarator>(DirectDeclaratorIdentifier(start,
                                                                              begin,
                                                                              std::get<std::string>(currToken
                                                                                                        .getValue())));
            break;
        }
        case TokenType::OpenParenthese:
        {
            auto start = begin;
            begin++;
            if (directDeclarator)
            {
                if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(begin, end, context);
                    if (!parameterTypeList)
                    {
                        return {};
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(
                        DirectDeclaratorParentheseParameters(start, begin, std::move(*directDeclarator),
                                                             std::move(*parameterTypeList)));
                }
                else
                {
                    std::vector<std::string> identifiers;
                    while (begin < end && begin->getTokenType() == TokenType::Identifier)
                    {
                        identifiers.push_back(std::get<std::string>(begin->getValue()));
                        begin++;
                        if (begin < end && begin->getTokenType() == TokenType::Comma)
                        {
                            begin++;
                        }
                        else if (begin < end && begin->getTokenType() != TokenType::CloseParenthese)
                        {
                            context.logError({"Expected , to separate identifiers"});
                        }
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(
                        DirectDeclaratorParentheseIdentifiers(start, begin, std::move(*directDeclarator),
                                                              std::move(identifiers)));
                }
            }
            else
            {
                auto declarator = parseDeclarator(begin, end, context);
                if (!declarator)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParenthese(
                    start, begin, std::make_unique<Declarator>(std::move(*declarator))));
            }
            if (begin >= end || begin->getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected ) "});
            }
            else
            {
                begin++;
            }
            break;
        }
        case TokenType::OpenSquareBracket:
        {
            if (!directDeclarator)
            {
                context.logError({"Expected declarator before ["});
                return {};
            }
            begin++;
            if (begin >= end)
            {
                context.logError({"Expected ] to match ["});
                return {};
            }
            if (std::any_of(begin, std::find_if(begin, end, [](const auto& token)
            { return token.getTokenType() == TokenType::CloseSquareBracket; }), [](const auto& token)
                            {
                                return token.getTokenType() == TokenType::Asterisk;
                            }))
            {
                auto asterisk = parseDirectDeclaratorAsterisk(*directDeclarator, begin, end, context);
                if (!asterisk)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(std::move(*asterisk));
            }
            else if (std::any_of(begin, std::find_if(begin, end, [](const auto& token)
            { return token.getTokenType() == TokenType::CloseSquareBracket; }), [](const auto& token)
                                 {
                                     return token.getTokenType() == TokenType::StaticKeyword;
                                 }))
            {
                auto staticDecl = parseDirectDeclaratorStatic(*directDeclarator, begin, end, context);
                if (!staticDecl)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(std::move(*staticDecl));
            }
            else
            {
                auto noStaticDecl = parseDirectDeclaratorNoStaticOrAsterisk(*directDeclarator, begin, end, context);
                if (!noStaticDecl)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(std::move(*noStaticDecl));
            }
            break;
        }
        default:break;
        }
    }
    if (!directDeclarator)
    {
        context.logError({"Expected declarator"});
        return {};
    }
    return std::move(*directDeclarator);
}

std::optional<ParameterTypeList>
OpenCL::Parser::parseParameterTypeList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    auto parameterList = parseParameterList(begin, end, context);
    if (!parameterList)
    {
        return {};
    }
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == TokenType::Comma)
    {
        begin++;
        if (begin >= end || begin->getTokenType() != TokenType::Ellipse)
        {
            context.logError({"Expected another parameter after ,"});
        }
        else
        {
            begin++;
            hasEllipse = true;
        }
    }
    return ParameterTypeList(start, begin, std::move(*parameterList), hasEllipse);
}

std::optional<ParameterList> OpenCL::Parser::parseParameterList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    std::vector<ParameterDeclaration> parameterDeclarations;
    bool first = true;
    while (begin < end)
    {
        auto before = begin;
        if (first)
        {
            first = false;
        }
        else if (begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        std::vector<DeclarationSpecifier> declarationSpecifiers;
        while (begin < end && firstIsInDeclarationSpecifier(*begin, context))
        {
            auto result = parseDeclarationSpecifier(begin, end, context);
            if (!result)
            {
                return {};
            }
            declarationSpecifiers.push_back(std::move(*result));
        }
        if (declarationSpecifiers.empty())
        {
            begin = before;
            break;
        }
        auto result = std::find_if(begin, end, [](const Token& token)
        {
            switch (token.getTokenType())
            {
            case TokenType::Asterisk:
            case TokenType::ConstKeyword:
            case TokenType::VolatileKeyword:
            case TokenType::RestrictKeyword: return false;
            default: break;
            }
            return true;
        });
        if (result == end)
        {
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
            continue;
        }

        if (result->getTokenType() == TokenType::OpenSquareBracket)
        {
            auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
            if (!abstractDeclarator)
            {
                return {};
            }
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
        }
        else if (result->getTokenType() == TokenType::Identifier)
        {
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                return {};
            }
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<Declarator>(std::move(*declarator)));
        }
        else if (result->getTokenType() == TokenType::OpenParenthese)
        {
            while (result->getTokenType() == TokenType::OpenParenthese)
            {
                // Ambigious
                result++;
                if (result->getTokenType() == TokenType::Identifier)
                {
                    auto declarator = parseDeclarator(begin, end, context);
                    if (!declarator)
                    {
                        return {};
                    }
                    parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                       std::make_unique<Declarator>(std::move(*declarator)));
                    break;
                }
                else if (result->getTokenType() != TokenType::OpenParenthese)
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    if (!abstractDeclarator)
                    {
                        return {};
                    }
                    parameterDeclarations.emplace_back(
                        std::move(declarationSpecifiers),
                        std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
                    break;
                }
            }
        }
        else
        {
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
        }
    }
    if (parameterDeclarations.empty())
    {
        context.logError({"Expected at least one parameter declaration"});
    }
    return ParameterList(start, begin, std::move(parameterDeclarations));
}

std::optional<Pointer> OpenCL::Parser::parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                    ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    if (begin->getTokenType() != TokenType::Asterisk)
    {
        context.logError({"Expected * at the beginning of pointer"});
    }
    auto start = begin;
    begin++;
    std::vector<TypeQualifier> typeQualifier;
    while (begin < end
        && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
            || begin->getTokenType() == TokenType::VolatileKeyword))
    {
        switch (begin->getTokenType())
        {
        case TokenType::ConstKeyword: typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Const);
            break;
        case TokenType::RestrictKeyword: typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
            break;
        case TokenType::VolatileKeyword: typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
            break;
        default: break;
        }
        begin++;
    }
    return Pointer(start, begin, std::move(typeQualifier));
}

std::optional<AbstractDeclarator>
OpenCL::Parser::parseAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context);
        if (!result)
        {
            return {};
        }
        pointers.push_back(std::move(*result));
    }
    auto result = parseDirectAbstractDeclarator(begin, end, context);
    if (!result)
    {
        return {};
    }
    return AbstractDeclarator(start, begin, std::move(pointers), std::move(*result));
}

std::optional<DirectAbstractDeclarator>
OpenCL::Parser::parseDirectAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                              Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    while (begin < end)
    {
        switch (begin->getTokenType())
        {
        case TokenType::OpenParenthese:
        {
            auto start = directAbstractDeclarator ? nodeFromNodeDerivedVariant(*directAbstractDeclarator).begin()
                                                  : begin;
            begin++;
            if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
            {
                auto parameterTypeList = parseParameterTypeList(begin, end, context);
                if (!parameterTypeList)
                {
                    return {};
                }
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    DirectAbstractDeclaratorParameterTypeList(
                        start, begin, std::move(directAbstractDeclarator),
                        std::make_unique<ParameterTypeList>(std::move(*parameterTypeList))));
            }
            else if (begin < end && (begin->getTokenType() == TokenType::OpenParenthese
                || begin->getTokenType() == TokenType::OpenSquareBracket
                || begin->getTokenType() == TokenType::Asterisk))
            {
                auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                if (!abstractDeclarator)
                {
                    return {};
                }
                directAbstractDeclarator
                    = std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParenthese(
                    start, begin, std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator))));
            }
            else
            {
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    DirectAbstractDeclaratorParameterTypeList(start, begin, std::move(directAbstractDeclarator),
                                                              nullptr));
            }
            if (begin >= end || begin->getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected ) to match ("});
            }
            else
            {
                begin++;
            }
            break;
        }
        case TokenType::OpenSquareBracket:
        {
            auto start = directAbstractDeclarator ? nodeFromNodeDerivedVariant(*directAbstractDeclarator).begin()
                                                  : begin;
            begin++;
            if (begin < end && begin->getTokenType() == TokenType::Asterisk)
            {
                begin++;
                directAbstractDeclarator =
                    std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorAsterisk(start,
                                                                                                begin,
                                                                                                std::move(
                                                                                                    directAbstractDeclarator)));
            }
            else
            {
                if (begin < end && begin->getTokenType() != TokenType::CloseSquareBracket)
                {
                    auto assignment = parseAssignmentExpression(begin, end, context);
                    if (!assignment)
                    {
                        return {};
                    }
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAssignmentExpression(
                            start, begin, std::move(directAbstractDeclarator),
                            std::make_unique<AssignmentExpression>(std::move(*assignment))));
                }
                else
                {
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAssignmentExpression(start, begin,
                                                                     std::move(directAbstractDeclarator), nullptr));
                }
            }
            if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
            {
                context.logError({"Expected ] to match ["});
            }
            break;
        }
        default:goto Exit;
        }
    }
Exit:
    if (!directAbstractDeclarator)
    {
        context.logError({"Invalid tokens for direct abstract declarator"});
        return {};
    }
    return std::move(*directAbstractDeclarator);
}

std::optional<EnumSpecifier> OpenCL::Parser::parseEnumSpecifier(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    if (begin->getTokenType() != TokenType::EnumKeyword)
    {
        context.logError({"Expected enum keyword at begin of enum specifier"});
        return {};
    }
    begin++;
    if (begin < end && begin->getTokenType() == TokenType::OpenBrace)
    {
        auto declaration = parseEnumDeclaration(begin, end, context);
        if (!declaration)
        {
            return {};
        }
        return EnumSpecifier(start, begin, std::move(*declaration));
    }
    else if (begin >= end || begin->getTokenType() != TokenType::Identifier)
    {
        context.logError({"Expected Identifier or { after enum"});
    }
    else
    {
        begin++;
    }
    if (begin < end && begin->getTokenType() == TokenType::OpenBrace)
    {
        auto declaration = parseEnumDeclaration(begin, end, context);
        if (!declaration)
        {
            return {};
        }
        return EnumSpecifier(start, begin, std::move(*declaration));
    }
    begin++;
    auto name = std::string();
    if (begin < end)
    {
        name = std::get<std::string>(begin->getValue());
    }
    else
    {
        context.logError({"Expected name for enum specifier"});
    }
    begin++;
    return EnumSpecifier(start, begin, std::move(name));
}

std::optional<EnumDeclaration> OpenCL::Parser::parseEnumDeclaration(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {

        return {};
    }
    auto start = begin;
    begin++;
    std::string name;
    if (begin < end && begin->getTokenType() == TokenType::Identifier)
    {
        name = std::get<std::string>(begin->getValue());
        begin++;
    }
    if (begin >= end || begin->getTokenType() != TokenType::OpenBrace)
    {
        context.logError({"Expected { after enum declaration"});
    }
    else
    {
        begin++;
    }
    std::vector<std::pair<std::string, std::int32_t>> values;
    do
    {
        std::string valueName;
        if (begin >= end || begin->getTokenType() != TokenType::Identifier)
        {
            context.logError({"Expected Identifier in enum value list"});
            return {};
        }
        else
        {
            valueName = std::get<std::string>(begin->getValue());
            begin++;
        }
        std::int32_t value = values.empty() ? 0 : values.back().second + 1;
        if (begin < end && begin->getTokenType() == TokenType::Assignment)
        {
            begin++;
            auto constant = parseAssignmentExpression(begin, end, context);
            if (!constant)
            {
                return {};
            }
            Semantics::ConstantEvaluator evaluator(context.structOrUnions);
            auto constValue = evaluator.visit(*constant);
            if (!constValue)
            {
                return {};
            }
            value = std::visit(
                [](auto&& value) -> std::int32_t
                {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (std::is_same_v<T, void*>)
                    {
                        return (std::int32_t)(std::intptr_t)value;
                    }
                    else
                    {
                        return value;
                    }
                },
                *constValue);
        }
        if (begin < end && begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else if (begin >= end && begin->getTokenType() != TokenType::CloseBrace)
        {
            context.logError({"Expected , after non final value in enum list"});
        }
        values.emplace_back(valueName, value);
    }
    while (begin < end && begin->getTokenType() != TokenType::CloseBrace);
    if (begin < end)
    {
        begin++;
    }
    else
    {
        context.logError({"Expected } at the end of enum definition"});
    }
    return EnumDeclaration(start, begin, std::move(name), values);
}

std::optional<Syntax::FunctionDefinition>
OpenCL::Parser::parseFunctionDefinition(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        ParsingContext& context)
{
    auto start = begin;
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    while (begin < end && firstIsInDeclarationSpecifier(*begin, context))
    {
        auto result = parseDeclarationSpecifier(begin, end, context);
        if (!result)
        {
            return {};
        }
        declarationSpecifiers.push_back(std::move(*result));
    }
    if (declarationSpecifiers.empty())
    {
        context.logError({ErrorMessages::MISSING_DECLARATION_SPECIFIER});
    }
    auto declarator = parseDeclarator(begin, end, context);
    if (!declarator)
    {
        return {};
    }
    std::vector<Declaration> declarations;
    while (begin < end && firstIsInDeclarationSpecifier(*begin, context))
    {
        auto result = parseDeclaration(begin, end, context);
        if (!result)
        {
            return {};
        }
        declarations.push_back(std::move(*result));
    }

    context.addToScope(Semantics::declaratorToName(*declarator));
    context.pushScope();
    if (auto* paramters =
        std::get_if<DirectDeclaratorParentheseParameters>(&declarator->getDirectDeclarator()))
    {
        auto& parameterDeclarations = paramters->getParameterTypeList().getParameterList().getParameterDeclarations();
        for (auto&[specifier, paramDeclarator] :
            parameterDeclarations)
        {
            if (parameterDeclarations.size() == 1 && specifier.size() == 1
                && std::holds_alternative<TypeSpecifier>(specifier[0]))
            {
                auto* primitive
                    = std::get_if<TypeSpecifier::PrimitiveTypeSpecifier>(&std::get<TypeSpecifier>(specifier[0])
                        .getVariant());
                if (primitive && *primitive == TypeSpecifier::PrimitiveTypeSpecifier::Void)
                {
                    break;
                }
            }

            if (std::holds_alternative<std::unique_ptr<AbstractDeclarator>>(paramDeclarator))
            {
                context.logError({ErrorMessages::MISSING_PARAMETER_NAME});
                return {};
            }
            auto& decl = std::get<std::unique_ptr<Declarator>>(paramDeclarator);
            auto visitor = [](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, DirectDeclaratorIdentifier>)
                {
                    (void)self;
                    return value.getIdentifier();
                }
                else if constexpr (std::is_same_v<T, DirectDeclaratorParenthese>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value.getDeclarator().getDirectDeclarator());
                }
                else if constexpr (
                    !std::is_same_v<
                        T,
                        DirectDeclaratorParentheseIdentifiers>
                        && !std::is_same_v<T, DirectDeclaratorParentheseParameters>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value.getDirectDeclarator());
                }
                else
                {
                    (void)self;
                    return "";
                }
            };
            auto result = std::visit(Y{visitor}, decl->getDirectDeclarator());
            if (!result.empty())
            {
                context.addToScope(result);
            }
        }
    }
    else if (auto* identifierList =
        std::get_if<DirectDeclaratorParentheseIdentifiers>(&declarator->getDirectDeclarator()))
    {
        for (auto& iter : identifierList->getIdentifiers())
        {
            context.addToScope(iter);
        }
    }
    auto compoundStatement = parseCompoundStatement(begin, end, context);
    context.popScope();
    if (!compoundStatement)
    {
        return {};
    }

    return FunctionDefinition(start, begin, std::move(declarationSpecifiers), std::move(*declarator),
                              std::move(declarations), std::move(*compoundStatement));
}

std::optional<CompoundStatement>
OpenCL::Parser::parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (begin >= end || begin->getTokenType() != TokenType::OpenBrace)
    {
        context.logError({"Expected { at start of Compound Statement"});
    }
    else
    {
        begin++;
    }
    std::vector<CompoundItem> items;
    context.pushScope();
    while (begin < end && begin->getTokenType() != TokenType::CloseBrace)
    {
        auto result = parseCompoundItem(begin, end, context);
        if (!result)
        {
            return {};
        }
        items.push_back(std::move(*result));
    }
    context.popScope();
    if (begin >= end || begin->getTokenType() != TokenType::CloseBrace)
    {
        context.logError({"Expected } at end of Compound Statement"});
    }
    else
    {
        begin++;
    }
    return CompoundStatement(start, begin, std::move(items));
}

std::optional<CompoundItem> OpenCL::Parser::parseCompoundItem(Tokens::const_iterator& begin,
                                                              Tokens::const_iterator end,
                                                              ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    if (firstIsInDeclarationSpecifier(*begin, context)
        && !(begin->getTokenType() == TokenType::Identifier && begin + 1 < end
            && (begin + 1)->getTokenType() == TokenType::Colon))
    {
        auto declaration = parseDeclaration(begin, end, context);
        if (declaration)
        {
            return CompoundItem(std::move(*declaration));
        }
    }
    else
    {
        auto statement = parseStatement(begin, end, context);
        if (statement)
        {
            return CompoundItem(std::move(*statement));
        }
    }
    return {};
}

std::optional<Initializer>
OpenCL::Parser::parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    if (begin->getTokenType() != TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context);
        if (!assignment)
        {
            return {};
        }
        return Initializer(start, begin, std::move(*assignment));
    }
    else
    {
        begin++;
        auto initializerList = parseInitializerList(begin, end, context);
        if (!initializerList)
        {
            return {};
        }
        if (begin == end
            || (begin->getTokenType() != TokenType::CloseBrace && begin->getTokenType() != TokenType::Comma))
        {
            context.logError({"Expected } after initializer list"});
            return {};
        }
        if (begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        if (begin == end || begin->getTokenType() != TokenType::CloseBrace)
        {
            context.logError({"Expected } after initializer list"});
            return {};
        }
        begin++;
        return Initializer{start, begin, std::move(*initializerList)};
    }
}

std::optional<InitializerList> OpenCL::Parser::parseInitializerList(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    typename OpenCL::Syntax::InitializerList::vector vector;
    bool first = true;
    while (true)
    {
        auto before = begin;
        if (first)
        {
            first = false;
        }
        else if (begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        std::vector<std::variant<std::size_t, std::string>> variants;
        while (begin->getTokenType() == TokenType::OpenSquareBracket || begin->getTokenType() == TokenType::Dot)
        {
            if (begin->getTokenType() == TokenType::OpenSquareBracket)
            {
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    if (vector.empty())
                    {
                        return {};
                    }
                    else
                    {
                        begin = before;
                        goto Exit;
                    }
                }
                if (begin->getTokenType() != TokenType::CloseSquareBracket)
                {
                    if (vector.empty())
                    {
                        context.logError({"Expected ] to close designator in initializer list"});
                        return {};
                    }
                    else
                    {
                        begin = before;
                        goto Exit;
                    }
                }
                begin++;
                Semantics::ConstantEvaluator evaluator(context.structOrUnions);
                auto constValue = evaluator.visit(*constant);
                if (!constValue)
                {
                    return {};
                }
                variants.emplace_back(std::visit(
                    [](auto&& value) -> std::size_t
                    {
                        using T = std::decay_t<decltype(value)>;
                        if constexpr (std::is_convertible_v<T, std::size_t>)
                        {
                            return value;
                        }
                        else
                        {
                            throw std::runtime_error("Invalid type of constant expression");
                        }
                    },
                    *constValue));
            }
            else if (begin->getTokenType() == TokenType::Dot)
            {
                begin++;
                if (begin->getTokenType() != TokenType::Identifier)
                {
                    if (vector.empty())
                    {
                        context.logError({"Expected identifier following dot in designation of initializer list"});
                        return {};
                    }
                    else
                    {
                        begin = before;
                        goto Exit;
                    }
                }
                variants.emplace_back(std::get<std::string>(begin->getValue()));
                begin++;
            }
        }
        if (!variants.empty())
        {
            if (begin->getTokenType() == TokenType::Assignment)
            {
                begin++;
            }
            else if (vector.empty())
            {
                context.logError({"Expected = after designators"});
                return {};
            }
            else
            {
                begin = before;
                goto Exit;
            }
        }
        auto initializer = parseInitializer(begin, end, context);
        if (!initializer)
        {
            if (vector.empty())
            {
                return {};
            }
            else
            {
                begin = before;
                goto Exit;
            }
        }
        vector.push_back({std::move(*initializer), variants});
    }
Exit:
    return InitializerList{start, begin, std::move(vector)};
}

std::optional<Statement> OpenCL::Parser::parseStatement(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    auto result = [&]() -> std::optional<Statement>
    {
        auto curentToken = *begin;
        switch (curentToken.getTokenType())
        {
        case TokenType::ReturnKeyword:
        {
            begin++;
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            return Statement{ReturnStatement(start, begin, std::move(*expression))};
        }
        case TokenType::IfKeyword:
        {
            begin++;
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError({"Expected ( after if"});
            }
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected ) at the end of if statement"});
                return {};
            }
            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return statement;
            }
            curentToken = *begin;
            if (begin != end && curentToken.getTokenType() == TokenType::ElseKeyword)
            {
                begin++;
                auto elseStatement = parseStatement(begin, end, context);
                if (!elseStatement)
                {
                    return statement;
                }
                return Statement{IfStatement(start, begin, std::move(*expression),
                                             std::make_unique<Statement>(std::move(*statement)),
                                             std::make_unique<Statement>(std::move(*elseStatement)))};
            }
            else
            {
                return Statement{IfStatement(start, begin, std::move(*expression),
                                             std::make_unique<Statement>(std::move(*statement)))};
            }
        }
        case TokenType::OpenBrace:
        {
            auto compoundStatement = parseCompoundStatement(begin, end, context);
            if (!compoundStatement)
            {
                return {};
            }
            return Statement{std::move(*compoundStatement)};
        }
        case TokenType::ForKeyword:
        {
            begin++;
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError({"Expected ( after for"});
                return {};
            }
            auto blockitem = parseCompoundItem(begin, end, context);
            if (!blockitem)
            {
                return {};
            }

            std::unique_ptr<Expression> control;
            {
                if (std::holds_alternative<Declaration>(*blockitem)
                    || begin->getTokenType() != TokenType::SemiColon)
                {
                    auto expression = parseExpression(begin, end, context);
                    if (!expression)
                    {
                        return {};
                    }
                    if (begin == end || begin->getTokenType() != TokenType::SemiColon)
                    {
                        context.logError({"Expected ; after control part of for loop header"});
                    }
                    begin++;
                    control = std::make_unique<Expression>(std::move(*expression));
                }
                else
                {
                    begin++;
                }
            }

            std::unique_ptr<Expression> post;
            {
                if (begin->getTokenType() != TokenType::CloseParenthese)
                {
                    auto expression = parseExpression(begin, end, context);
                    if (!expression)
                    {
                        return {};
                    }
                    if (begin == end || begin->getTokenType() != TokenType::CloseParenthese)
                    {
                        context.logError({"Expected ) after control part of for loop header"});
                    }
                    begin++;
                    post = std::make_unique<Expression>(std::move(*expression));
                }
                else
                {
                    begin++;
                }
            }

            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return statement;
            }
            if (auto declaration = std::get_if<Declaration>(&*blockitem))
            {
                return Statement(
                    ForDeclarationStatement(start, begin, std::make_unique<Statement>(std::move(*statement)),
                                            std::move(*declaration), std::move(control), std::move(post)));
            }
            else if (auto expressionStatement = std::get_if<ExpressionStatement>(
                &std::get<Statement>(*blockitem)))
            {
                return Statement(
                    ForStatement(start, begin, std::make_unique<Statement>(std::move(*statement)),
                                 expressionStatement->moveOptionalExpression(), std::move(control),
                                 std::move(post)));
            }
            else
            {
                context.logError({"Invalid expression or declaration for initial part of for loop header"});
                return {};
            }
        }
        case TokenType::WhileKeyword:
        {
            begin++;
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError({"Expected ( after while"});
            }
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected ) after expression in while"});
            }
            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(HeadWhileStatement(start, begin, std::move(*expression),
                                                std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::DoKeyword:
        {
            begin++;
            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return statement;
            }
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::WhileKeyword)
            {
                context.logError({"Expected while after do"});
            }
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError({"Expected ( after while"});
            }
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *begin;
            begin++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected ) after expression in while"});
            }
            return Statement(FootWhileStatement(start, begin, std::make_unique<Statement>(std::move(*statement)),
                                                std::move(*expression)));
        }
        case TokenType::BreakKeyword:
        {
            begin++;
            return Statement(BreakStatement(start, begin));
        }
        case TokenType::ContinueKeyword:
        {
            begin++;
            return Statement(ContinueStatement(start, begin));
        }
        case TokenType::SwitchKeyword:
        {
            begin++;
            curentToken = *begin;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError({"Expected ( after switch keyword"});
            }
            begin++;
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *begin;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected ) after expression in switch "});
            }
            begin++;
            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(SwitchStatement(start, begin, std::move(*expression),
                                             std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::DefaultKeyword:
        {
            begin++;
            curentToken = *begin;
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                context.logError({"Expected : after default"});
            }
            begin++;
            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return {};
            }
            return Statement(DefaultStatement(start, begin, std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::CaseKeyword:
        {
            begin++;
            auto expression = parseAssignmentExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *begin;
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                context.logError({"Expected : after constant expression of case"});
            }
            begin++;
            Semantics::ConstantEvaluator evaluator(context.structOrUnions);
            auto statement = parseStatement(begin, end, context);
            if (!statement)
            {
                return statement;
            }
            auto constValue = evaluator.visit(*expression);
            if (!constValue)
            {
                return {};
            }
            return Statement(
                CaseStatement(start, begin, *constValue, std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::GotoKeyword:
        {
            begin++;
            if (begin->getTokenType() != TokenType::Identifier)
            {
                context.logError({"Expected identifier following goto keyword"});
                return {};
            }
            const auto& name = std::get<std::string>(begin->getValue());
            begin++;
            return Statement(GotoStatement(start, begin, name));
        }
        case TokenType::Identifier:
        {
            if (begin + 1 < end && (begin + 1)->getTokenType() == TokenType::Colon)
            {
                const auto& name = std::get<std::string>(begin->getValue());
                begin += 2;
                return Statement(LabelStatement(start, begin, name));
            }
            [[fallthrough]];
        }
        default:
        {
            if (begin != end && begin->getTokenType() != TokenType::SemiColon)
            {
                auto expression = parseExpression(begin, end, context);
                if (!expression)
                {
                    return {};
                }
                return Statement(
                    ExpressionStatement(start, begin, std::make_unique<Expression>(std::move(*expression))));
            }
            else
            {
                return Statement(ExpressionStatement(start, begin));
            }
        }
        }
    }();
    if (!result)
    {
        return result;
    }

    if ((std::holds_alternative<ExpressionStatement>(*result)
        || std::holds_alternative<ReturnStatement>(*result)
        || std::holds_alternative<FootWhileStatement>(*result)
        || std::holds_alternative<BreakStatement>(*result)
        || std::holds_alternative<ContinueStatement>(*result)
        || std::holds_alternative<GotoStatement>(*result))
        && (begin == end || begin->getTokenType() != TokenType::SemiColon))
    {
        context.logError({"Statement not terminated with ;"});
    }
    else if (std::holds_alternative<ExpressionStatement>(*result)
        || std::holds_alternative<ReturnStatement>(*result)
        || std::holds_alternative<FootWhileStatement>(*result)
        || std::holds_alternative<BreakStatement>(*result)
        || std::holds_alternative<ContinueStatement>(*result)
        || std::holds_alternative<GotoStatement>(*result))
    {
        begin++;
    }
    return result;
}

std::optional<Expression> OpenCL::Parser::parseExpression(Tokens::const_iterator& begin,
                                                          Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    std::vector<AssignmentExpression> expressions;
    auto assignment = parseAssignmentExpression(begin, end, context);
    if (!assignment)
    {
        return {};
    }
    expressions.push_back(std::move(*assignment));

    if (begin != end)
    {
        while (begin->getTokenType() == TokenType::Comma)
        {
            auto before = begin;
            begin++;
            assignment = parseAssignmentExpression(begin, end, context);
            if (!assignment)
            {
                begin = before;
                break;
            }
            expressions.push_back(std::move(*assignment));
        }
    }
    return Expression(start, begin, std::move(expressions));
}

std::optional<Syntax::AssignmentExpression>
OpenCL::Parser::parseAssignmentExpression(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          ParsingContext& context)
{
    if (begin == end)
    {

        return {};
    }
    auto start = begin;
    return context.doBacktracking([&]() -> std::optional<Syntax::AssignmentExpression>
                                  {
                                      auto assignmentBranch = context.createBranch(begin);
                                      auto unary = parseUnaryExpression(assignmentBranch->getCurrent(), end, context);
                                      if (*assignmentBranch)
                                      {
                                          if (isAssignment(assignmentBranch->getCurrent()->getTokenType()))
                                          {
                                              auto currentToken = *assignmentBranch->getCurrent();
                                              assignmentBranch->getCurrent()++;
                                              auto
                                                  assignment = parseAssignmentExpression(assignmentBranch->getCurrent(),
                                                                                         end,
                                                                                         context);
                                              if (!assignment)
                                              {
                                                  return {};
                                              }
                                              return AssignmentExpression(
                                                  start, begin,
                                                  AssignmentExpressionAssignment(
                                                      start, begin, std::move(*unary),
                                                      [assignment = currentToken.getTokenType()]
                                                      {
                                                          switch (assignment)
                                                          {
                                                          case TokenType::Assignment:return AssignmentExpressionAssignment::AssignOperator::NoOperator;
                                                          case TokenType::PlusAssign:return AssignmentExpressionAssignment::AssignOperator::PlusAssign;
                                                          case TokenType::MinusAssign:return AssignmentExpressionAssignment::AssignOperator::MinusAssign;
                                                          case TokenType::DivideAssign:return AssignmentExpressionAssignment::AssignOperator::DivideAssign;
                                                          case TokenType::MultiplyAssign:return AssignmentExpressionAssignment::AssignOperator::MultiplyAssign;
                                                          case TokenType::ModuloAssign:return AssignmentExpressionAssignment::AssignOperator::ModuloAssign;
                                                          case TokenType::ShiftLeftAssign:return AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign;
                                                          case TokenType::ShiftRightAssign:return AssignmentExpressionAssignment::AssignOperator::RightShiftAssign;
                                                          case TokenType::BitAndAssign:return AssignmentExpressionAssignment::AssignOperator::BitAndAssign;
                                                          case TokenType::BitOrAssign:return AssignmentExpressionAssignment::AssignOperator::BitOrAssign;
                                                          case TokenType::BitXorAssign:return AssignmentExpressionAssignment::AssignOperator::BitXorAssign;
                                                          default:
                                                              throw std::runtime_error("Invalid token for assignment");
                                                          }
                                                      }(),
                                                      std::make_unique<AssignmentExpression>(std::move(*assignment))));
                                          }
                                      }

                                      auto condBranch = context.createBranch(begin);
                                      auto cond = parseConditionalExpression(condBranch->getCurrent(), end, context);
                                      if (!cond)
                                      {
                                          return {};
                                      }
                                      return AssignmentExpression(start, begin, std::move(*cond));
                                  });
}

std::optional<ConditionalExpression> OpenCL::Parser::parseConditionalExpression(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    auto logicalOrExperssion = parseLogicalOrExpression(begin, end, context);
    if (!logicalOrExperssion)
    {
        return {};
    }
    if (begin != end)
    {
        if (begin->getTokenType() == TokenType::QuestionMark)
        {
            begin++;
            auto optionalExpression = parseExpression(begin, end, context);
            if (!optionalExpression)
            {
                return {};
            }
            if (begin->getTokenType() != TokenType::Colon)
            {
                context.logError({"Expected : to match ?"});
                return {};
            }
            begin++;
            auto optionalConditional = parseConditionalExpression(begin, end, context);
            if (!optionalConditional)
            {
                return optionalConditional;
            }
            return ConditionalExpression(start, begin, std::move(*logicalOrExperssion),
                                         std::make_unique<Expression>(std::move(*optionalExpression)),
                                         std::make_unique<ConditionalExpression>(std::move(*optionalConditional)));
        }
    }
    return ConditionalExpression(start, begin, std::move(*logicalOrExperssion));
}

std::optional<LogicalOrExpression> OpenCL::Parser::parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    auto logicalAnd = parseLogicalAndExpression(begin, end, context);
    if (!logicalAnd)
    {
        return {};
    }

    std::vector<LogicalAndExpression> optionalLogicalAnds;
    while (begin != end && begin->getTokenType() == TokenType::LogicOr)
    {
        auto before = begin;
        begin++;
        auto newAnd = parseLogicalAndExpression(begin, end, context);
        if (!newAnd)
        {
            begin = before;
            break;
        }
        optionalLogicalAnds.push_back(std::move(*newAnd));
    }

    return LogicalOrExpression(start, begin, std::move(*logicalAnd), std::move(optionalLogicalAnds));
}

std::optional<LogicalAndExpression> OpenCL::Parser::parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    auto result = parseBitOrExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<BitOrExpression> list;
    while (begin != end && begin->getTokenType() == TokenType::LogicAnd)
    {
        auto before = begin;
        begin++;
        auto newOr = parseBitOrExpression(begin, end, context);
        if (!newOr)
        {
            begin = before;
            break;
        }
        list.push_back(std::move(*newOr));
    }

    return LogicalAndExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<BitOrExpression> OpenCL::Parser::parseBitOrExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseBitXorExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<BitXorExpression> list;
    while (begin != end && begin->getTokenType() == TokenType::BitOr)
    {
        auto before = begin;
        begin++;
        auto newXor = parseBitXorExpression(begin, end, context);
        if (!newXor)
        {
            begin = before;
            break;
        }
        list.push_back(std::move(*newXor));
    }

    return BitOrExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<BitXorExpression> OpenCL::Parser::parseBitXorExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseBitAndExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<BitAndExpression> list;
    while (begin != end && begin->getTokenType() == TokenType::BitXor)
    {
        auto before = begin;
        begin++;
        auto newAnd = parseBitAndExpression(begin, end, context);
        if (!newAnd)
        {
            begin = before;
            break;
        }
        list.push_back(std::move(*newAnd));
    }

    return BitXorExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<BitAndExpression> OpenCL::Parser::parseBitAndExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseEqualityExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<EqualityExpression> list;
    while (begin != end && begin->getTokenType() == TokenType::Ampersand)
    {
        auto before = begin;
        begin++;
        auto newEqual = parseEqualityExpression(begin, end, context);
        if (!newEqual)
        {
            begin = before;
            break;
        }
        list.push_back(std::move(*newEqual));
    }

    return BitAndExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<EqualityExpression> OpenCL::Parser::parseEqualityExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseRelationalExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> relationalExpressions;
    while (begin != end && (begin->getTokenType() == TokenType::Equal || begin->getTokenType() == TokenType::NotEqual))
    {
        auto before = begin;
        auto token = begin->getTokenType();
        begin++;
        auto newRelational = parseRelationalExpression(begin, end, context);
        if (!newRelational)
        {
            begin = before;
            break;
        }
        relationalExpressions.emplace_back(token == TokenType::Equal ? EqualityExpression::EqualityOperator::Equal :
                                           EqualityExpression::EqualityOperator::NotEqual,
                                           std::move(*newRelational));
    }

    return EqualityExpression(start, begin, std::move(*result), std::move(relationalExpressions));
}

std::optional<RelationalExpression> OpenCL::Parser::parseRelationalExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseShiftExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
    while (begin != end
        && (begin->getTokenType() == TokenType::LessThan || begin->getTokenType() == TokenType::LessThanOrEqual
            || begin->getTokenType() == TokenType::GreaterThan
            || begin->getTokenType() == TokenType::GreaterThanOrEqual))
    {
        auto before = begin;
        auto token = begin->getTokenType();
        begin++;
        auto newShift = parseShiftExpression(begin, end, context);
        if (!newShift)
        {
            begin = before;
            break;
        }

        list.emplace_back(
            [token]() -> RelationalExpression::RelationalOperator
            {
                switch (token)
                {
                case TokenType::LessThan: return RelationalExpression::RelationalOperator::LessThan;
                case TokenType::LessThanOrEqual: return RelationalExpression::RelationalOperator::LessThanOrEqual;
                case TokenType::GreaterThan: return RelationalExpression::RelationalOperator::GreaterThan;
                case TokenType::GreaterThanOrEqual:return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                default: throw std::runtime_error("Invalid token for relational LogicalOrExpression");
                }
            }(),
            std::move(*newShift));
    }

    return RelationalExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<ShiftExpression> OpenCL::Parser::parseShiftExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseAdditiveExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
    while (begin != end
        && (begin->getTokenType() == TokenType::ShiftRight || begin->getTokenType() == TokenType::ShiftLeft))
    {
        auto before = begin;
        auto token = begin->getTokenType();
        begin++;
        auto newAdd = parseAdditiveExpression(begin, end, context);
        if (!newAdd)
        {
            begin = before;
            break;
        }
        list.emplace_back(token == TokenType::ShiftRight ? ShiftExpression::ShiftOperator::Right :
                          ShiftExpression::ShiftOperator::Left,
                          std::move(*newAdd));
    }

    return ShiftExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<AdditiveExpression> OpenCL::Parser::parseAdditiveExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseTerm(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
    while (begin != end && (begin->getTokenType() == TokenType::Plus || begin->getTokenType() == TokenType::Minus))
    {
        auto before = begin;
        auto token = begin->getTokenType();
        begin++;
        auto newTerm = parseTerm(begin, end, context);
        if (!newTerm)
        {
            begin = before;
            break;
        }
        list.emplace_back(token == TokenType::Plus ? AdditiveExpression::BinaryDashOperator::BinaryPlus :
                          AdditiveExpression::BinaryDashOperator::BinaryMinus,
                          std::move(*newTerm));
    }

    return AdditiveExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<Term> OpenCL::Parser::parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                              ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto result = parseCastExpression(begin, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
    while (begin != end
        && (begin->getTokenType() == TokenType::Asterisk || begin->getTokenType() == TokenType::Division
            || begin->getTokenType() == TokenType::Modulo))
    {
        auto before = begin;
        auto token = begin->getTokenType();
        begin++;
        auto newCast = parseCastExpression(begin, end, context);
        if (!newCast)
        {
            begin = before;
            break;
        }
        list.emplace_back(
            [token]
            {
                switch (token)
                {
                case TokenType::Asterisk: return Term::BinaryDotOperator::BinaryMultiply;
                case TokenType::Division: return Term::BinaryDotOperator::BinaryDivide;
                case TokenType::Modulo: return Term::BinaryDotOperator::BinaryRemainder;
                default: throw std::runtime_error("Invalid token");
                }
            }(),
            std::move(*newCast));
    }

    return Term(start, begin, std::move(*result), std::move(list));
}

std::optional<TypeName> OpenCL::Parser::parseTypeName(Tokens::const_iterator& begin,
                                                      Tokens::const_iterator end,
                                                      OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    std::vector<SpecifierQualifier> specifierQualifiers;
    while (auto result = parseSpecifierQualifier(begin, end, context))
    {
        specifierQualifiers.push_back(std::move(*result));
    }
    if (specifierQualifiers.empty())
    {
        context.logError({"Expected at least one specifier qualifier at beginning of typename"});
        return {};
    }

    if (auto abstractDec = parseAbstractDeclarator(begin, end, context))
    {

        return TypeName(start, begin, std::move(specifierQualifiers),
                        std::make_unique<AbstractDeclarator>(std::move(*abstractDec)));
    }

    return TypeName(start, begin, std::move(specifierQualifiers), nullptr);
}

std::optional<CastExpression> OpenCL::Parser::parseCastExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end,
                                                                  ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto before = begin;
    if (before->getTokenType() == TokenType::OpenParenthese)
    {
        before++;
        auto typeName = parseTypeName(before, end, context);
        if (typeName)
        {
            if (before->getTokenType() == TokenType::CloseParenthese)
            {
                before++;
                auto cast = parseCastExpression(before, end, context);
                if (cast)
                {
                    begin = before;
                    return CastExpression(
                        start, begin,
                        std::pair{std::move(*typeName), std::make_unique<CastExpression>(std::move(*cast))});
                }
            }
        }
    }
    auto unary = parseUnaryExpression(begin, end, context);
    if (!unary)
    {
        return {};
    }

    return CastExpression(start, begin, std::move(*unary));
}

std::optional<UnaryExpression> OpenCL::Parser::parseUnaryExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    if (begin->getTokenType() == TokenType::SizeofKeyword)
    {
        begin++;
        if (begin->getTokenType() == TokenType::OpenParenthese)
        {
            begin++;
            auto type = parseTypeName(begin, end, context);
            if (!type)
            {
                return {};
            }
            if (begin->getTokenType() != TokenType::CloseParenthese)
            {
                context.logError({"Expected Close Parenthese after type in sizeof"});
                return {};
            }
            begin++;
            return UnaryExpression(UnaryExpressionSizeOf(start, begin, std::make_unique<TypeName>(std::move(*type))));
        }
        else
        {
            auto unary = parseUnaryExpression(begin, end, context);
            if (!unary)
            {
                return {};
            }
            return UnaryExpression(UnaryExpressionSizeOf(start,
                                                         begin,
                                                         std::make_unique<UnaryExpression>(std::move(*unary))));
        }
    }
    else if (begin->getTokenType() == TokenType::Increment || begin->getTokenType() == TokenType::Decrement
        || begin->getTokenType() == TokenType::Ampersand || begin->getTokenType() == TokenType::Asterisk
        || begin->getTokenType() == TokenType::Plus || begin->getTokenType() == TokenType::Minus
        || begin->getTokenType() == TokenType::LogicalNegation
        || begin->getTokenType() == TokenType::BitWiseNegation)
    {
        auto token = begin->getTokenType();
        begin++;
        auto op = [token]
        {
            switch (token)
            {
            case TokenType::Increment: return UnaryExpressionUnaryOperator::UnaryOperator::Increment;
            case TokenType::Decrement: return UnaryExpressionUnaryOperator::UnaryOperator::Decrement;
            case TokenType::Ampersand: return UnaryExpressionUnaryOperator::UnaryOperator::Ampersand;
            case TokenType::Asterisk: return UnaryExpressionUnaryOperator::UnaryOperator::Asterisk;
            case TokenType::Plus: return UnaryExpressionUnaryOperator::UnaryOperator::Plus;
            case TokenType::Minus: return UnaryExpressionUnaryOperator::UnaryOperator::Minus;
            case TokenType::LogicalNegation: return UnaryExpressionUnaryOperator::UnaryOperator::BitNot;
            case TokenType::BitWiseNegation: return UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot;
            default: throw std::runtime_error("Invalid token");
            }
        }();
        auto unary = parseUnaryExpression(begin, end, context);
        if (!unary)
        {
            return {};
        }
        return UnaryExpression(
            UnaryExpressionUnaryOperator(start, begin, op, std::make_unique<UnaryExpression>(std::move(*unary))));
    }

    auto postFix = parsePostFixExpression(begin, end, context);
    if (!postFix)
    {
        return {};
    }
    return UnaryExpression(UnaryExpressionPostFixExpression(start, begin, std::move(*postFix)));
}

std::optional<PostFixExpression> OpenCL::Parser::parsePostFixExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    std::stack<std::unique_ptr<PostFixExpression>> stack;
    while (begin != end && isPostFixOperator(*begin))
    {
        if (begin->getTokenType() == TokenType::Identifier || begin->getTokenType() == TokenType::Literal)
        {
            auto start = begin;
            auto newPrimary = parsePrimaryExpression(begin, end, context);
            if (!newPrimary)
            {
                return {};
            }
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionPrimaryExpression(start,
                                                                                              begin,
                                                                                              std::move(*newPrimary))));
        }
        else if (begin->getTokenType() == TokenType::OpenParenthese && stack.empty())
        {
            auto start = begin;
            if (begin + 1 < end && firstIsInDeclarationSpecifier(*(begin + 1), context))
            {
                auto type = parseTypeName(begin, end, context);
                if (!type)
                {
                    return {};
                }
                if (begin->getTokenType() != TokenType::CloseParenthese)
                {
                    context.logError({"Expected ) after type name in type initializer"});
                    return {};
                }
                begin++;
                if (begin->getTokenType() != TokenType::OpenBrace)
                {
                    context.logError({"Expected { after type around parenthesis"});
                    return {};
                }
                begin++;
                auto initializer = parseInitializerList(begin, end, context);
                if (!initializer)
                {
                    return {};
                }
                if (begin->getTokenType() == TokenType::Comma)
                {
                    begin++;
                }
                if (begin->getTokenType() != TokenType::CloseBrace)
                {
                    context.logError({"Expected { after type around parenthesis"});
                    return {};
                }
                begin++;
                stack.push(std::make_unique<PostFixExpression>(
                    PostFixExpressionTypeInitializer(start, begin, std::move(*type), std::move(*initializer))));
            }
            else
            {
                auto newPrimary = parsePrimaryExpression(begin, end, context);
                if (!newPrimary)
                {
                    return {};
                }
                stack.push(std::make_unique<PostFixExpression>(PostFixExpressionPrimaryExpression(start,
                                                                                                  begin,
                                                                                                  std::move(*newPrimary))));
            }
        }
        else if (begin->getTokenType() == TokenType::OpenParenthese)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
            while (begin->getTokenType() != TokenType::CloseParenthese)
            {
                auto assignment = parseAssignmentExpression(begin, end, context);
                if (!assignment)
                {
                    return {};
                }
                nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                if (begin->getTokenType() == TokenType::CloseParenthese)
                {
                    return {};
                }
                else if (begin->getTokenType() != TokenType::Comma)
                {
                    return {};
                }
                begin++;
            }
            begin++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(
                PostFixExpressionFunctionCall(start, begin, std::move(postExpression),
                                              std::move(nonCommaExpressions))));
        }
        else if (begin->getTokenType() == TokenType::OpenSquareBracket)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                return {};
            }
            if (begin->getTokenType() != TokenType::CloseSquareBracket)
            {
                return {};
            }
            begin++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(
                PostFixExpressionSubscript(start, begin, std::move(postExpression), std::move(*expression))));
        }
        else if (begin->getTokenType() == TokenType::Increment)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionIncrement(start,
                                                                                      begin,
                                                                                      std::move(postExpression))));
        }
        else if (begin->getTokenType() == TokenType::Decrement)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionDecrement(start,
                                                                                      begin,
                                                                                      std::move(postExpression))));
        }
        else if (begin->getTokenType() == TokenType::Dot)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            if (begin->getTokenType() != TokenType::Identifier)
            {
                return {};
            }
            const auto& name = std::get<std::string>(begin->getValue());
            begin++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionDot(start,
                                                                                begin,
                                                                                std::move(postExpression),
                                                                                name)));
        }
        else if (begin->getTokenType() == TokenType::Arrow)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            if (begin->getTokenType() != TokenType::Identifier)
            {
                return {};
            }
            const auto& name = std::get<std::string>(begin->getValue());
            begin++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionArrow(start,
                                                                                  begin,
                                                                                  std::move(postExpression),
                                                                                  name)));
        }
    }

    if (stack.size() != 1)
    {
        context.logError({"Invalid amount of post fix expressions"});
        return {};
    }
    auto ret = std::move(*stack.top());
    stack.pop();

    return ret;
}

std::optional<PrimaryExpression> OpenCL::Parser::parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }

    auto start = begin;
    auto currToken = *begin;
    begin++;
    if (currToken.getTokenType() == TokenType::Identifier)
    {
        return PrimaryExpression(PrimaryExpressionIdentifier(start,
                                                             begin,
                                                             std::get<std::string>(currToken.getValue())));
    }
    else if (currToken.getTokenType() == TokenType::Literal)
    {
        return PrimaryExpression(
            PrimaryExpressionConstant(
                start, begin,
                std::visit([](auto&& value) -> typename PrimaryExpressionConstant::variant
                           {
                               using T = std::decay_t<decltype(value)>;
                               if constexpr (std::is_constructible_v<typename PrimaryExpressionConstant::variant, T>)
                               {
                                   return {std::forward<decltype(value)>(value)};
                               }
                               else
                               {
                                   throw std::runtime_error("Can't convert type of variant to constant expression");
                               }
                           },
                           currToken.getValue())));
    }
    else if (currToken.getTokenType() == TokenType::StringLiteral)
    {
        std::string result = std::get<std::string>(currToken.getValue());
        while (begin < end && begin->getTokenType() == TokenType::StringLiteral)
        {
            result += std::get<std::string>(begin->getValue());
        }
        return PrimaryExpression(PrimaryExpressionConstant(start, begin, result));
    }
    else if (currToken.getTokenType() == TokenType::OpenParenthese)
    {
        auto expression = parseExpression(begin, end, context);
        if (!expression)
        {
            return {};
        }
        if (begin->getTokenType() != TokenType::CloseParenthese)
        {
            context.logError({"Expected Close Parentheses after expression in primary expression"});
        }
        begin++;

        return PrimaryExpression(PrimaryExpressionParenthese(start, begin, std::move(*expression)));
    }
    else
    {
        context.logError({"Invalid token for primary expression"});
        return {};
    }
}

void Parser::ParsingContext::addTypedef(const std::string& name)
{
    m_typedefs.back().insert(name);
}

bool Parser::ParsingContext::isTypedef(const std::string& name) const
{
    for (auto& iter : m_typedefs)
    {
        if (iter.count(name))
        {
            return true;
        }
    }
    return false;
}

void Parser::ParsingContext::logError(const ErrorReporter& errorReporter)
{
    if (m_branches.empty() || (m_branches.size() == 1 && m_branches.back().first.empty()))
    {
        m_errorsOccured = true;
        if (m_reporter)
        {
            *m_reporter << errorReporter;
        }
    }
    else if (m_branches.back().first.empty())
    {
        m_branches[m_branches.size() - 2].first.back()->m_errors.push_back(errorReporter);
    }
    else
    {
        m_branches.back().first.back()->m_errors.push_back(errorReporter);
    }
}

void Parser::ParsingContext::addToScope(std::string name)
{
    m_currentScope.back().insert(std::move(name));
}

bool Parser::ParsingContext::isInScope(const std::string& name) const
{
    for (auto& iter : m_currentScope)
    {
        if (iter.count(name))
        {
            return true;
        }
    }
    return false;
}

void Parser::ParsingContext::pushScope()
{
    m_currentScope.emplace_back();
    m_typedefs.emplace_back();
}

void Parser::ParsingContext::popScope()
{
    m_currentScope.pop_back();
    m_typedefs.pop_back();
}

bool Parser::ParsingContext::isErrorsOccured() const
{
    return m_errorsOccured;
}

std::unique_ptr<Parser::ParsingContext::Branch> Parser::ParsingContext::createBranch(Tokens::const_iterator& begin)
{
    return std::make_unique<Branch>(*this, begin);
}

Parser::ParsingContext::Branch::Branch(ParsingContext& context, std::vector<Lexer::Token>::const_iterator& begin)
    : context(context), m_begin(begin), m_curr(begin)
{
    context.m_branches.back().first.push_back(this);
}

std::vector<Token>::const_iterator& Parser::ParsingContext::Branch::getCurrent()
{
    return m_curr;
}

Parser::ParsingContext::Branch::operator bool() const
{
    return m_errors.empty();
}

Parser::ParsingContext::Branch::~Branch()
{
    if (!context.m_branches.back().first.empty())
    {
        if (*this)
        {
            m_begin = m_curr;
            context.m_branches.back().first.clear();
        }
        else
        {
            Branch* result = *std::max_element(context.m_branches.back().first.begin(),
                                               context.m_branches.back().first.end(),
                                               [](Branch* const lhs, Branch* const rhs)
                                               {
                                                   return std::tuple(std::numeric_limits<std::size_t>::max()
                                                                         - lhs->m_errors.size(),
                                                                     std::distance(lhs->m_begin, lhs->m_curr),
                                                                     [lhs, rhs]() -> bool
                                                                     {
                                                                         for (auto& iter : lhs->context.m_branches
                                                                                              .back().second)
                                                                         {
                                                                             bool leftSat = iter(lhs->m_begin,
                                                                                                 lhs->m_curr);
                                                                             bool rightSat = iter(rhs->m_begin,
                                                                                                  rhs->m_curr);
                                                                             if (leftSat != rightSat)
                                                                             {
                                                                                 return leftSat;
                                                                             }
                                                                         }
                                                                         return false;
                                                                     }())
                                                       < std::tuple(
                                                           std::numeric_limits<std::size_t>::max()
                                                               - rhs->m_errors.size(),
                                                           std::distance(rhs->m_begin, rhs->m_curr),
                                                           false);
                                               });
            context.m_branches.back().first.clear();
            m_begin = result->m_curr;
            for (auto& iter : result->m_errors)
            {
                context.logError(iter);
            }
        }
    }
}

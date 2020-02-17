#include "Preprocessor.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/C/Syntax.hpp>
#include <CompilerCore/Common/Util.hpp>

#include <cassert>
#include <numeric>
#include <unordered_map>
#include <unordered_set>

namespace
{
struct Group;

struct State;

std::vector<cld::Lexer::Token> processGroup(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                            std::vector<cld::Lexer::Token>::const_iterator end,
                                            const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                            State* state);

std::vector<cld::Lexer::Token> macroSubstitute(std::vector<cld::Lexer::Token>::const_iterator begin,
                                               std::vector<cld::Lexer::Token>::const_iterator end,
                                               const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                               State* state);

std::vector<cld::Lexer::Token>::const_iterator findNewline(std::vector<cld::Lexer::Token>::const_iterator begin,
                                                           std::vector<cld::Lexer::Token>::const_iterator end)
{
    return std::find_if(begin, end, [](const cld::Lexer::Token& token) {
        return token.getTokenType() == cld::Lexer::TokenType::Newline;
    });
}

std::vector<cld::Lexer::Token> filterNewline(std::vector<cld::Lexer::Token>::const_iterator begin,
                                             std::vector<cld::Lexer::Token>::const_iterator end)
{
    std::vector<cld::Lexer::Token> result(begin, end);
    result.erase(std::remove_if(result.begin(), result.end(),
                                [](const cld::Lexer::Token& token) {
                                    return token.getTokenType() == cld::Lexer::TokenType::Newline;
                                }),
                 result.end());
    return result;
}

bool tokenStructureEqual(std::vector<cld::Lexer::Token>::const_iterator begin1,
                         std::vector<cld::Lexer::Token>::const_iterator end1,
                         std::vector<cld::Lexer::Token>::const_iterator begin2,
                         std::vector<cld::Lexer::Token>::const_iterator end2)
{
    return std::mismatch(begin1, end1, begin2, end2,
                         [](const cld::Lexer::Token& lhs, const cld::Lexer::Token& rhs) {
                             return lhs.getRepresentation() == rhs.getRepresentation();
                         })
               .first
           == end1;
}

/**
 * <IfGroup> ::= <TokenType::Pound> <Identifier=if> <ConstantExpression> <TokenType::Newline> [ <Group> ]
 *             | <TokenType::Pound> <Identifier=ifdef> <TokenType::Identifier> <TokenType::Newline> [ <Group> ]
 *             | <TokenType::Pound> <Identifier=ifndef> <TokenType::Identifier> <TokenType::Newline> [ <Group> ]
 */
struct IfGroup final
{
    struct IfDefTag final
    {
        std::string identifier;
    };
    struct IfnDefTag final
    {
        std::string identifier;
    };
    std::variant<IfDefTag, IfnDefTag, cld::Syntax::ConstantExpression> ifs;
    std::unique_ptr<Group> optionalGroup;
};

/**
 * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <ConstantExpression> <TokenType::Newline> [ <Group> ]
 */
struct ElIfGroup final
{
    cld::Syntax::ConstantExpression constantExpression;
    std::unique_ptr<Group> optionalGroup;
};

/**
 * <ElseGroup> ::= <TokenType::Pound> <Identifier=else> <TokenType::Newline> [<Group>]
 */
struct ElseGroup final
{
    std::unique_ptr<Group> optionalGroup;
};

/**
 * <IfSection> ::= <IfGroup> {<ElIfGroup>} [<ElseGroup] <TokenType::Pound> <Identifier=endif> <TokenType::Newline>
 */
struct IfSection final
{
    IfGroup ifGroup;
    std::vector<ElIfGroup> elifGroups;
    std::optional<ElseGroup> optionalElseGroup;
};

/**
 * OpenParentheses must immediately follow the define keyword for the alternative to be valid
 * otherwise its just another token inside of <TOKENS>
 *
 * <DefineDirectives> ::= <TokenType::Pound> <Identifier=define> <TokenType::Identifier> [<TOKENS>]
 * <TokenType::Newline> | <TokenType::Pound> <Identifier=define> <TokenType::Identifier>
 *                        <TokenType::OpenParentheses> [ <TokenType::Identifier { <TokenType::Comma>
 * <TokenType::Identifier> } ] <TokenType::CloseParentheses> [<TOKENS>] <TokenType::Newline> | <TokenType::Pound>
 * <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses> <TokenType::Ellipse>
 * <TokenType::CloseParentheses> [ <TOKENS> ] <TokenType::Newline> | <TokenType::Pound> <Identifier=define>
 * <TokenType::Identifier> <TokenType::OpenParentheses> [ <TokenType::Identifier { <TokenType::Comma>
 * <TokenType::Identifier> } ] <TokenType::Comma> <TokenType::Ellipse> <TokenType::CloseParentheses> [ <TOKENS> ]
 * <TokenType::Newline>
 *
 * <ControlLine> ::= <TokenType::Pound> <Identifier=include> <TOKENS> <TokenType::Newline>
 *                 | <DefineDirectives>
 *                 | <TokenType::Pound> <Identifier=undef> <TokenType::Identifier> <TokenType::Newline>
 *                 | <TokenType::Pound> <Identifier=line> <TOKENS> <TokenType::Newline>
 *                 | <TokenType::Pound> <Identifier=error> [<TOKENS>] <TokenType::Newline>
 *                 | <TokenType::Pound> <Identifier=pragma> [<TOKENS>] <TokenType::Newline>
 */
struct ControlLine final
{
    struct DefineDirective
    {
        std::vector<cld::Lexer::Token>::const_iterator begin;
        std::vector<cld::Lexer::Token>::const_iterator end;
        std::vector<cld::Lexer::Token>::const_iterator identifierPos;
        std::string identifier;
        /**
         * Its an optional to differentiate between an empty identifier list and no identifier list
         */
        std::optional<std::vector<std::string>> identifierList;
        bool hasEllipse;
        std::vector<cld::Lexer::Token> replacementList;
    };

    struct IncludeTag final
    {
        std::vector<cld::Lexer::Token> tokens;
    };

    struct LineTag final
    {
        std::vector<cld::Lexer::Token> tokens;
    };

    struct ErrorTag final
    {
        std::vector<cld::Lexer::Token> tokens;
    };

    struct PragmaTag final
    {
        std::vector<cld::Lexer::Token> tokens;
    };

    std::variant<IncludeTag, LineTag, ErrorTag, PragmaTag, std::string, DefineDirective> variant;
};

struct NonDirective
{
    std::vector<cld::Lexer::Token> tokens;
};

/**
 * <GroupPart> ::= <IfSection> | <ControlLine> | <TextLine> | <NonDirective>
 */
using GroupPart = std::variant<IfSection, ControlLine, std::vector<cld::Lexer::Token>, NonDirective>;

/**
 * <Group> ::= <GroupPart> { <GroupPart> }
 */
struct Group final
{
    std::vector<GroupPart> groupPart;
};

/**
 * <File> := { <Group> }
 */
struct File final
{
    std::vector<Group> groups;
};

bool expect(cld::Lexer::TokenType tokenType, std::vector<cld::Lexer::Token>::const_iterator& begin,
            std::vector<cld::Lexer::Token>::const_iterator end, const cld::SourceObject& sourceObject,
            llvm::raw_ostream* reporter)
{
    if (begin == end || begin->getTokenType() != tokenType)
    {
        if (reporter)
        {
            if (begin == end)
            {
                cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args(cld::Lexer::tokenName(tokenType)),
                                    begin, cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                    .print(*reporter, sourceObject);
            }
            else
            {
                cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                        cld::Lexer::tokenName(tokenType), '\'' + begin->getRepresentation() + '\''),
                                    begin, cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))
                    .print(*reporter, sourceObject);
                ;
            }
        }
        return false;
    }
    begin++;
    return true;
}

struct State
{
    std::unordered_map<std::string, ControlLine::DefineDirective> defines;
    std::map<std::pair<std::uint64_t, std::uint64_t>, cld::SourceObject::Substitution> substitutions;
    std::uint64_t currentID = 1;
    std::vector<std::unordered_set<std::string>> disabledMacros;
};

template <class InputIterator>
void assignMacroID(InputIterator begin, InputIterator end, State& state)
{
    static_assert(std::is_same_v<typename InputIterator::value_type, cld::Lexer::Token>);
    bool setOne = false;
    std::for_each(begin, end, [state, &setOne](cld::Lexer::Token& token) {
        if (!token.macroInserted())
        {
            token.setMacroId(state.currentID);
            setOne = true;
        }
    });
    if (setOne)
    {
        state.currentID++;
    }
}

std::unordered_map<std::string, std::vector<cld::Lexer::Token>>
    getArguments(std::vector<cld::Lexer::Token>::const_iterator& begin,
                 std::vector<cld::Lexer::Token>::const_iterator end,
                 std::vector<cld::Lexer::Token>::const_iterator namePos,
                 std::unordered_map<std::string, ControlLine::DefineDirective>::iterator define,
                 const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter, State* state)
{
    std::unordered_map<std::string, std::vector<cld::Lexer::Token>> arguments;
    auto argsStart = begin;
    while (begin != end && begin->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
    {
        std::size_t i = 0;
        auto argEnd = std::find_if(begin, end, [&i](const cld::Lexer::Token& token) {
            if (token.getTokenType() == cld::Lexer::TokenType::OpenParentheses)
            {
                i++;
            }
            else if (token.getTokenType() == cld::Lexer::TokenType::CloseParentheses)
            {
                if (!i)
                {
                    return true;
                }
                i--;
            }
            else if (token.getTokenType() == cld::Lexer::TokenType::Comma)
            {
                return !i;
            }
            return false;
        });
        auto temp = filterNewline(begin, argEnd);
        arguments.emplace((*define->second.identifierList)[arguments.size()],
                          macroSubstitute(temp.begin(), temp.end(), cld::SourceObject(/*TODO:*/), reporter, state));
        begin = argEnd;
        if (begin != end && begin->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
        {
            begin++;
        }
    }
    if (arguments.size() < define->second.identifierList->size())
    {
        if (reporter)
        {
            cld::Message::error(
                cld::ErrorMessages::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N.args('\'' + define->first + '\''), namePos,
                cld::Modifier(argsStart, begin != argsStart ? begin : begin + 1))
                .print(*reporter, sourceObject);
            cld::Message::note(
                cld::Notes::MISSING_N.args("arguments "
                                           + static_cast<std::string>(cld::Format::List(
                                               ", ", " and ",
                                               {define->second.identifierList->begin() + arguments.size(),
                                                define->second.identifierList->end()}))),
                namePos)
                .print(*reporter, sourceObject);
        }
    }
    else if (arguments.size() > define->second.identifierList->size() && !define->second.hasEllipse)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::PP::TOO_MANY_ARGUMENTS_FOR_MACRO_N.args(define->first), namePos,
                                cld::Modifier(argsStart, begin != argsStart ? begin : begin + 1))
                .print(*reporter, sourceObject);
        }
    }
    return arguments;
}

std::vector<cld::Lexer::Token>
    argumentSubstitution(std::unordered_map<std::string, ControlLine::DefineDirective>::iterator define,
                         std::unordered_map<std::string, std::vector<cld::Lexer::Token>>& arguments, State* state)
{
    std::vector<cld::Lexer::Token> replacementSubstituted;
    auto argStart = define->second.replacementList.begin();
    for (auto tokenIter = define->second.replacementList.begin(); tokenIter != define->second.replacementList.end();
         tokenIter++)
    {
        if (tokenIter->getTokenType() != cld::Lexer::TokenType::Identifier)
        {
            continue;
        }
        auto argument = arguments.find(std::get<std::string>(tokenIter->getValue()));
        if (argument == arguments.end())
        {
            continue;
        }
        if (tokenIter != define->second.replacementList.begin()
            && (tokenIter - 1)->getTokenType() == cld::Lexer::TokenType::Pound)
        {
            replacementSubstituted.insert(replacementSubstituted.end(), argStart, tokenIter - 1);
            //                auto stringValue = cld::Lexer::reconstructTrimmed(, argument->second.begin(),
            //                                                                     argument->second.end());
            //                auto string = '\''
            //                              + std::accumulate(stringValue.begin(), stringValue.end(), std::string{},
            //                                                [](const std::string& init, char character) {
            //                                                    if (character == '\'' || character == '"' ||
            //                                                    character == '\\')
            //                                                    {
            //                                                        return init + '\\' + character;
            //                                                    }
            //                                                    return init + character;
            //                                                })
            //                              + '\'';
            //                replacementSubstituted.emplace_back(tokenIter->getLine(), tokenIter->getColumn(),
            //                                                    tokenIter->getLength(),
            //                                                    cld::Lexer::TokenType::StringLiteral,
            //                                                    std::move(stringValue), std::move(string));
        }
        else
        {
            replacementSubstituted.insert(replacementSubstituted.end(), argStart, tokenIter);
            replacementSubstituted.insert(replacementSubstituted.end(), argument->second.begin(),
                                          argument->second.end());
            assignMacroID(replacementSubstituted.end()
                              - std::distance(argument->second.begin(), argument->second.end()),
                          replacementSubstituted.end(), *state);
        }
        argStart = tokenIter + 1;
    }
    replacementSubstituted.insert(replacementSubstituted.end(), argStart, define->second.replacementList.end());
    return replacementSubstituted;
}

std::vector<cld::Lexer::Token> macroSubstitute(std::vector<cld::Lexer::Token>::const_iterator begin,
                                               std::vector<cld::Lexer::Token>::const_iterator end,
                                               const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                               State* state)
{
    if (!state)
    {
        return {begin, end};
    }

    std::vector<cld::Lexer::Token> result;
    auto start = begin;
    std::int64_t columnOffset = 0;
    for (auto iter = begin; iter != end; iter++)
    {
        if (iter != begin && iter->getTokenType() == cld::Lexer::TokenType::Newline)
        {
            columnOffset = 0;
        }
        if (iter->getTokenType() != cld::Lexer::TokenType::Identifier)
        {
            continue;
        }
        //            const auto& name = std::get<std::string>(iter->getValue());
        //            auto namePos = iter;
        //            auto define = state->defines.find(name);
        //            if (define == state->defines.end()
        //                || (iter->getMacroId() < state->disabledMacros.size()
        //                    && state->disabledMacros[iter->getMacroId()].count(name)))
        //            {
        //                continue;
        //            }
        //            if (!define->second.identifierList)
        //            {
        //                result.reserve(result.size() + std::distance(start, iter));
        //                std::transform(start, iter, std::back_inserter(result),
        //                [columnOffset](cld::Lexer::Token token) {
        //                    token.setColumn(token.getColumn() + columnOffset);
        //                    return token;
        //                });
        //                start = iter + 1;
        //                // Creating a temporary here to create proper context in case of errors in the later
        //                macroSubstitute
        //                // call
        //                auto temp = result;
        //                temp.insert(temp.end(), define->second.replacementList.begin(),
        //                define->second.replacementList.end()); columnOffset +=
        //                define->second.replacementList.empty() ?
        //                                    -static_cast<std::int64_t>(iter->getLength()) :
        //                                    static_cast<std::int64_t>(define->second.replacementList.back().getColumn()
        //                                                              +
        //                                                              define->second.replacementList.back().getLength())
        //                                        - define->second.replacementList.front().getColumn() -
        //                                        iter->getLength();
        //                std::for_each(temp.begin() + result.size(), temp.end(),
        //                              [iter, begin = define->second.replacementList.begin()](cld::Lexer::Token&
        //                              token) {
        //                                  token.setLine(iter->getLine());
        //                                  token.setColumn(iter->getColumn() + token.getColumn() -
        //                                  begin->getColumn());
        //                              });
        //
        //                assignMacroID(temp.begin() + result.size(), temp.end(), *state);
        //                state->disabledMacros.resize(std::max(state->disabledMacros.size(), state->currentID));
        //                {
        //                    std::unordered_set<std::uint64_t> uniqueSet;
        //                    std::transform(temp.begin() + result.size(), temp.end(),
        //                                   std::inserter(uniqueSet, uniqueSet.begin()),
        //                                   [](const cld::Lexer::Token& token) { return token.getMacroId(); });
        //                    for (auto& id : uniqueSet)
        //                    {
        //                        state->disabledMacros[id].insert(name);
        //                        state->disabledMacros[id].insert(state->disabledMacros[iter->getMacroId()].begin(),
        //                                                         state->disabledMacros[iter->getMacroId()].end());
        //                    }
        //                }
        //
        //                auto vector = macroSubstitute(temp.begin() + result.size(), temp.end(),
        //                cld::SourceObject(temp),
        //                                              reporter, state);
        //                result.reserve(result.size() + vector.size());
        //                std::move(vector.begin(), vector.end(), std::back_inserter(result));
        //            }
        //            else if (iter + 1 != end && (iter + 1)->getTokenType() ==
        //            cld::Lexer::TokenType::OpenParentheses
        //                     && iter->getColumn() + iter->getLength() == (iter + 1)->getColumn())
        //            {
        //                result.reserve(result.size() + std::distance(start, iter));
        //                std::transform(start, iter, std::back_inserter(result),
        //                [columnOffset](cld::Lexer::Token token) {
        //                    token.setColumn(token.getColumn() + columnOffset);
        //                    return token;
        //                });
        //                //( must immediately follow the function like macro identifier
        //                iter += 2;
        //                auto arguments = getArguments(iter, end, namePos, define, sourceObject, reporter, state);
        //                start = iter + 1;
        //
        //                auto replacementSubstituted = argumentSubstitution(define, arguments, state);
        //
        //                auto temp = result;
        //                temp.reserve(temp.size() + replacementSubstituted.size());
        //                std::move(replacementSubstituted.begin(), replacementSubstituted.end(),
        //                std::back_inserter(temp));
        //
        //                assignMacroID(temp.begin() + result.size(), temp.end(), *state);
        //                state->disabledMacros.resize(std::max(state->disabledMacros.size(), state->currentID));
        //                {
        //                    std::unordered_set<std::uint64_t> uniqueSet;
        //                    std::transform(temp.begin() + result.size(), temp.end(),
        //                                   std::inserter(uniqueSet, uniqueSet.begin()),
        //                                   [](const cld::Lexer::Token& token) { return token.getMacroId(); });
        //                    for (auto& id : uniqueSet)
        //                    {
        //                        state->disabledMacros[id].insert(name);
        //                        state->disabledMacros[id].insert(state->disabledMacros[namePos->getMacroId()].begin(),
        //                                                         state->disabledMacros[namePos->getMacroId()].end());
        //                    }
        //                }
        //
        //                auto vector = macroSubstitute(temp.begin() + result.size(), temp.end(),
        //                cld::SourceObject(temp),
        //                                              reporter, state);
        //                result.reserve(result.size() + vector.size());
        //                std::move(vector.begin(), vector.end(), std::back_inserter(result));
        //            }
        //            else
        //            {
        //                continue;
        //            }
        //            state->substitutions.insert(
        //                {{iter->getLine(), iter->getColumn()}, {{define->second.begin, define->second.end},
        //                {*iter}}});
    }
    result.reserve(result.size() + std::distance(start, end));
    //        std::transform(start, end, std::back_inserter(result), [columnOffset](cld::Lexer::Token token) {
    //            token.setColumn(token.getColumn() + columnOffset);
    //            return token;
    //        });
    return result;
}

std::vector<cld::Lexer::Token> processIfGroup(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                              std::vector<cld::Lexer::Token>::const_iterator end,
                                              const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                              State* state)
{
    IfGroup result{};
    assert(begin != end && begin->getTokenType() == cld::Lexer::TokenType::Identifier);
    const auto& value = std::get<std::string>(begin->getValue());
    assert(value == "if" || value == "ifdef" || value == "ifndef");
    begin++;
    if (value == "if")
    {
        cld::Parser::Context context(sourceObject, reporter);
        auto expEnd = findNewline(begin, end);
        auto constantExpression = cld::Parser::parseConditionalExpression(begin, expEnd, context);
        if (begin != expEnd)
        {
            if (reporter)
            {
                cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("newline", "expression"), begin,
                                    cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))
                    .print(*reporter, sourceObject);
            }
            return {};
        }
        result = IfGroup{std::move(constantExpression), nullptr};
    }
    else
    {
        if (!expect(cld::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
        {
            return {};
        }
        const auto& identifier = std::get<std::string>((begin - 1)->getValue());
        if (begin != end && !expect(cld::Lexer::TokenType::Newline, begin, end, sourceObject, reporter))
        {
            return {};
        }
        if (value == "ifdef")
        {
            result = IfGroup{IfGroup::IfDefTag{identifier}, nullptr};
        }
        else if (value == "ifndef")
        {
            result = IfGroup{IfGroup::IfnDefTag{identifier}, nullptr};
        }
        else
        {
            OPENCL_UNREACHABLE;
        }
    }

    if (begin != end)
    {
        // TODO: result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
    }
    return {};
}

std::vector<cld::Lexer::Token> processElIfGroup(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                                std::vector<cld::Lexer::Token>::const_iterator end,
                                                const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                                State* state)
{
    assert(begin->getTokenType() == cld::Lexer::TokenType::Identifier
           && std::get<std::string>(begin->getValue()) == "elif");
    begin++;
    auto newline = findNewline(begin, end);
    cld::Parser::Context context(sourceObject, reporter);
    auto constantExpression = cld::Parser::parseConditionalExpression(begin, newline, context);
    ElIfGroup result{std::move(constantExpression), {}};
    if (begin != newline)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("newline", "expression"), begin,
                                cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))
                .print(*reporter, sourceObject);
        }
        return {};
    }
    if (begin != end)
    {
        // TODO: result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
    }
    return {};
}

std::vector<cld::Lexer::Token> processElseGroup(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                                std::vector<cld::Lexer::Token>::const_iterator end,
                                                const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                                State* state)
{
    assert(begin->getTokenType() == cld::Lexer::TokenType::Identifier
           && std::get<std::string>(begin->getValue()) == "else");
    begin++;
    if (!expect(cld::Lexer::TokenType::Newline, begin, end, sourceObject, reporter))
    {
        return {{}};
    }
    ElseGroup result{};
    if (begin != end)
    {
        // TODO: result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
    }
    return {};
}

std::vector<cld::Lexer::Token> processIfSection(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                                std::vector<cld::Lexer::Token>::const_iterator end,
                                                const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                                State* state)
{
    assert(begin != end);
    auto ifGroup = processIfGroup(begin, end, sourceObject, reporter, state);
    if (begin == end)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args("#endif"), begin,
                                cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                .print(*reporter, sourceObject);
        }
        return {}; //{std::move(ifGroup), {}, {}};
    }
    else if (begin->getTokenType() != cld::Lexer::TokenType::Pound)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args(
                                    "#endif", '\'' + begin->getRepresentation() + '\''),
                                begin, cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))
                .print(*reporter, sourceObject);
        }
        return {}; //{std::move(ifGroup), {}, {}};
    }
    else
    {
        begin++;
    }

    std::vector<ElIfGroup> elifGroups;
    while (begin != end && begin->getTokenType() == cld::Lexer::TokenType::Identifier
           && std::get<std::string>(begin->getValue()) == "elif")
    {
        // TODO: elifGroups.push_back(processElIfGroup(begin, end, sourceObject, reporter));
    }

    if (begin == end)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args("#endif"), begin,
                                cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                .print(*reporter, sourceObject);
        }
        return {}; //{std::move(ifGroup), std::move(elifGroups), {}};
    }
    else if (begin->getTokenType() != cld::Lexer::TokenType::Pound)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args(
                                    "#endif", '\'' + begin->getRepresentation() + '\''),
                                begin, cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))
                .print(*reporter, sourceObject);
        }
        return {}; //{std::move(ifGroup), std::move(elifGroups), {}};
    }
    else
    {
        begin++;
    }

    //        IfSection result{std::move(ifGroup), std::move(elifGroups), {}};
    //        if (begin != end && begin->getTokenType() == cld::Lexer::TokenType::Identifier
    //            && std::get<std::string>(begin->getValue()) == "else")
    //        {
    //            result.optionalElseGroup = processElseGroup(begin, end, sourceObject, reporter);
    //        }
    //
    //        if (begin == end)
    //        {
    //            if (reporter)
    //            {
    //                (*reporter) << cld::Message::error(
    //                    cld::ErrorMessages::Parser::EXPECTED_N.args("#endif"),
    //                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin), cld::Modifier(begin,
    //                    begin + 1, cld::Modifier::PointAtBeginning));
    //            }
    //        }
    //        else
    //        {
    //            begin++;
    //            if (begin == end || begin->getTokenType() != cld::Lexer::TokenType::Identifier
    //                || std::get<std::string>(begin->getValue()) != "endif")
    //            {
    //                if (reporter)
    //                {
    //                    if (begin == end)
    //                    {
    //                        (*reporter) << cld::Message::error(
    //                            cld::ErrorMessages::Parser::EXPECTED_N.args("#endif"),
    //                            sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
    //                            cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd));
    //                    }
    //                    else
    //                    {
    //                        (*reporter) << cld::Message::error(
    //                            cld::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif",
    //                                                                                    '\'' +
    //                                                                                    begin->getRepresentation()
    //                                                                                    +
    //                                                                                    '\''),
    //                            sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
    //                            cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning));
    //                    }
    //                }
    //            }
    //        }
    //
    //        return result;
}

std::optional<ControlLine::DefineDirective>
    processDefineDirective(std::vector<cld::Lexer::Token>::const_iterator& begin,
                           std::vector<cld::Lexer::Token>::const_iterator end, const cld::SourceObject& sourceObject,
                           llvm::raw_ostream* reporter, State*)
{
    auto start = begin - 1;
    assert(begin->getTokenType() == cld::Lexer::TokenType::Identifier
           && std::get<std::string>(begin->getValue()) == "define");
    begin++;
    if (!expect(cld::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
    {
        return {};
    }
    auto namePos = begin - 1;
    const auto& name = std::get<std::string>(namePos->getValue());
    if (begin == end || begin->getTokenType() != cld::Lexer::TokenType::OpenParentheses
        || begin->getOffset() != namePos->getOffset() + namePos->getLength())
    {
        // No ( after the identifier or there's whitespace in between the identifier and the (
        if (begin != end && begin->getOffset() == namePos->getOffset() + namePos->getLength()
            && begin->getTokenType() != cld::Lexer::TokenType::Newline)
        {
            if (reporter)
            {
                cld::Message::error(cld::ErrorMessages::PP::WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION, namePos,
                                    cld::Modifier(begin, begin + 1))
                    .print(*reporter, sourceObject);
            }
            return {};
        }
        auto eol = findNewline(begin, end);
        auto tokens = std::vector<cld::Lexer::Token>(begin, eol);
        begin = eol;
        return ControlLine::DefineDirective{start, begin, namePos, name, {}, false, std::move(tokens)};
    }
    else
    {
        begin++;
        bool hasEllipse = false;
        std::vector<std::string> identifierList;
        bool first = true;
        while (begin != end
               && (first ? begin->getTokenType() == cld::Lexer::TokenType::Identifier
                               || begin->getTokenType() == cld::Lexer::TokenType::Ellipse :
                           begin->getTokenType() == cld::Lexer::TokenType::Comma))
        {
            if (first)
            {
                first = false;
            }
            else
            {
                begin++;
            }
            if (begin != end && begin->getTokenType() == cld::Lexer::TokenType::Identifier)
            {
                identifierList.push_back(std::get<std::string>(begin->getValue()));
                begin++;
            }
            else if (begin != end && begin->getTokenType() == cld::Lexer::TokenType::Ellipse)
            {
                hasEllipse = true;
                break;
            }
            else
            {
                if (reporter)
                {
                    if (begin == end)
                    {
                        cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args("identifier or '...'"), begin,
                                            cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                            .print(*reporter, sourceObject);
                    }
                    else
                    {
                        cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                                "identifier or '...'", '\'' + begin->getRepresentation() + '\''),
                                            begin, cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))
                            .print(*reporter, sourceObject);
                    }
                }
                return ControlLine::DefineDirective{start, begin, namePos, name, std::move(identifierList), false, {}};
            }
        }
        expect(cld::Lexer::TokenType::CloseParentheses, begin, end, sourceObject, reporter);
        auto newline = findNewline(begin, end);
        auto token = std::vector<cld::Lexer::Token>(begin, newline);
        begin = newline;
        return ControlLine::DefineDirective{start,      begin,           namePos, name, std::move(identifierList),
                                            hasEllipse, std::move(token)};
    }
}

void processControlLine(std::vector<cld::Lexer::Token>::const_iterator& begin,
                        std::vector<cld::Lexer::Token>::const_iterator end, const cld::SourceObject& sourceObject,
                        llvm::raw_ostream* reporter, State* state)
{
    const auto& value = std::get<std::string>(begin->getValue());
    if (value == "define")
    {
        auto define = processDefineDirective(begin, end, sourceObject, reporter, state);
        if (state && define)
        {
            auto [result, success] = state->defines.emplace(define->identifier, *define);
            if (!success && reporter)
            {
                if (define->identifierList.has_value() != result->second.identifierList.has_value()
                    || !define->identifierList
                    || define->identifierList->size() != result->second.identifierList->size()
                    || define->hasEllipse != result->second.hasEllipse
                    || !tokenStructureEqual(define->replacementList.begin(), define->replacementList.end(),
                                            result->second.replacementList.begin(),
                                            result->second.replacementList.end()))
                {
                    cld::Message::error(
                        cld::ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + define->identifier + '\''),
                        define->identifierPos, define->identifierPos,
                        cld::Modifier(define->identifierPos, define->identifierPos + 1))
                        .print(*reporter, sourceObject);

                    cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                       result->second.identifierPos,
                                       cld::Modifier(result->second.identifierPos, result->second.identifierPos))
                        .print(*reporter, sourceObject);
                }
            }
        }
    }
    else if (value == "undef")
    {
        begin++;
        if (!expect(cld::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
        {
            return;
        }
        if (state)
        {
            state->defines.erase(std::get<std::string>((begin - 1)->getValue()));
        }
        if (begin != end)
        {
            expect(cld::Lexer::TokenType::Newline, begin, end, sourceObject, reporter);
        }
        return;
    }
    else
    {
        begin++;
        auto newline = findNewline(begin, end);
        auto tokens = filterNewline(begin, newline);
        begin = newline;
        if (value == "include")
        {
            if (tokens.empty() && reporter)
            {
                cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("tokens", "include"), begin - 1,
                                    cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                    .print(*reporter, sourceObject);
            }
            // TODO: Process tokens to open file, tokenize it and call processFile with it
            return;
        }
        else if (value == "line")
        {
            if (tokens.empty() && reporter)
            {
                cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("tokens", "line"), begin - 1,
                                    cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                    .print(*reporter, sourceObject);
            }
            return;
        }
        else if (value == "error")
        {
            if (reporter)
            {
                // TODO reporting
            }
            return;
        }
        else if (value == "pragma")
        {
            return;
        }
        else
        {
            OPENCL_UNREACHABLE;
        }
    }
}

std::vector<cld::Lexer::Token> processGroup(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                            std::vector<cld::Lexer::Token>::const_iterator end,
                                            const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                            State* state)
{
    if (begin == end)
    {
        if (reporter)
        {
            cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args("Tokens"), begin, begin,
                                cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))
                .print(*reporter, sourceObject);
        }
        return {};
    }

    std::vector<cld::Lexer::Token> result;
    do
    {
        if (begin->getTokenType() != cld::Lexer::TokenType::Pound)
        {
            auto eol = findNewline(begin, end);
            auto vector = macroSubstitute(begin, eol, sourceObject, reporter, state);
            result.reserve(result.size() + vector.size());
            std::move(vector.begin(), vector.end(), std::back_inserter(result));
            begin = eol;
            continue;
        }
        begin++;
        if (begin == end || begin->getTokenType() == cld::Lexer::TokenType::Newline)
        {
            continue;
        }
        if (begin->getTokenType() == cld::Lexer::TokenType::Identifier)
        {
            const auto& value = std::get<std::string>(begin->getValue());
            if (value == "elif" || value == "endif" || value == "else")
            {
                begin--;
                break;
            }
            if (value == "ifdef" || value == "ifndef" || value == "if")
            {
                auto vector = processIfSection(begin, end, sourceObject, reporter, state);
                result.insert(result.end(), vector.begin(), vector.end());
                continue;
            }
            if (value == "include" || value == "undef" || value == "line" || value == "error" || value == "pragma"
                || value == "define")
            {
                processControlLine(begin, end, sourceObject, reporter, state);
                continue;
            }
        }
        auto eol = findNewline(begin, end);
        if (reporter && state)
        {
            cld::Message::error(cld::ErrorMessages::PP::N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE.args(
                                    "'#" + begin->getRepresentation() + '\''),
                                begin, cld::Modifier(begin - 1, begin + 1))
                .print(*reporter, sourceObject);
        }
        begin = eol;
    } while (begin != end);
    return result;
}

std::vector<cld::Lexer::Token> processFile(std::vector<cld::Lexer::Token>::const_iterator& begin,
                                           std::vector<cld::Lexer::Token>::const_iterator end,
                                           const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter,
                                           State* state)
{
    std::vector<cld::Lexer::Token> result;
    while (begin != end)
    {
        auto vector = processGroup(begin, end, sourceObject, reporter, state);
        result.reserve(result.size() + vector.size());
        std::move(vector.begin(), vector.end(), std::back_inserter(result));
    }
    return result;
}
} // namespace

cld::SourceObject cld::PP::preprocess(const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter)
{
    auto begin = sourceObject.data().begin();
    State state;
    auto file = processFile(begin, sourceObject.data().end(), sourceObject, reporter, &state);
    return SourceObject({}, std::move(file), LanguageOptions::native(), state.substitutions);
}

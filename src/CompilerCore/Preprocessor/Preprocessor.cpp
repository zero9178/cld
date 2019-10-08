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

    std::vector<OpenCL::Lexer::Token> processGroup(OpenCL::SourceObject::const_iterator& begin,
                                                   OpenCL::SourceObject::const_iterator end,
                                                   const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                   State* state);

    std::vector<OpenCL::Lexer::Token>::const_iterator
        findEOLWithOutBackslash(OpenCL::SourceObject::const_iterator begin, OpenCL::SourceObject::const_iterator end)
    {
        auto result = begin;
        do
        {
            result = OpenCL::findEOL(result, end);
        } while ((result - 1)->getTokenType() == OpenCL::Lexer::TokenType::Backslash);
        return result;
    }

    std::vector<OpenCL::Lexer::Token> filterForNewlineAndBackslash(OpenCL::SourceObject::const_iterator begin,
                                                                   OpenCL::SourceObject::const_iterator end)
    {
        std::vector<OpenCL::Lexer::Token> result(begin, end);
        result.erase(std::remove_if(result.begin(), result.end(),
                                    [](const OpenCL::Lexer::Token& token) {
                                        return token.getTokenType() == OpenCL::Lexer::TokenType::Newline
                                               || token.getTokenType() == OpenCL::Lexer::TokenType::Backslash;
                                    }),
                     result.end());
        return result;
    }

    bool tokenStructureEqual(OpenCL::SourceObject::const_iterator begin1, OpenCL::SourceObject::const_iterator end1,
                             OpenCL::SourceObject::const_iterator begin2, OpenCL::SourceObject::const_iterator end2)
    {
        return std::mismatch(begin1, end1, begin2, end2,
                             [](const OpenCL::Lexer::Token& lhs, const OpenCL::Lexer::Token& rhs) {
                                 return lhs.emitBack() == rhs.emitBack();
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
        std::variant<IfDefTag, IfnDefTag, OpenCL::Syntax::ConstantExpression> ifs;
        std::unique_ptr<Group> optionalGroup;
    };

    /**
     * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <ConstantExpression> <TokenType::Newline> [ <Group> ]
     */
    struct ElIfGroup final
    {
        OpenCL::Syntax::ConstantExpression constantExpression;
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
            OpenCL::SourceObject::const_iterator begin;
            OpenCL::SourceObject::const_iterator end;
            OpenCL::SourceObject::const_iterator identifierPos;
            std::string identifier;
            /**
             * Its an optional to differentiate between an empty identifier list and no identifier list
             */
            std::optional<std::vector<std::string>> identifierList;
            bool hasEllipse;
            std::vector<OpenCL::Lexer::Token> replacementList;
        };

        struct IncludeTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        struct LineTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        struct ErrorTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        struct PragmaTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        std::variant<IncludeTag, LineTag, ErrorTag, PragmaTag, std::string, DefineDirective> variant;
    };

    struct NonDirective
    {
        std::vector<OpenCL::Lexer::Token> tokens;
    };

    /**
     * <GroupPart> ::= <IfSection> | <ControlLine> | <TextLine> | <NonDirective>
     */
    using GroupPart = std::variant<IfSection, ControlLine, std::vector<OpenCL::Lexer::Token>, NonDirective>;

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

    bool expect(OpenCL::Lexer::TokenType tokenType, OpenCL::SourceObject::const_iterator& begin,
                OpenCL::SourceObject::const_iterator end, const OpenCL::SourceObject& sourceObject,
                std::ostream* reporter)
    {
        if (begin == end || begin->getTokenType() != tokenType)
        {
            if (reporter)
            {
                if (begin == end)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Lexer::tokenName(tokenType)),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                }
                else
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Lexer::tokenName(tokenType),
                                                                                    '\'' + begin->emitBack() + '\''),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
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
        std::map<std::pair<std::uint64_t, std::uint64_t>, OpenCL::SourceObject::Substitution> substitutions;
        std::uint64_t currentID = 1;
    };

    std::vector<OpenCL::Lexer::Token> macroSubstitute(OpenCL::SourceObject::const_iterator begin,
                                                      OpenCL::SourceObject::const_iterator end,
                                                      const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                      State* state, std::unordered_set<std::string_view> disabled = {})
    {
        if (!state)
        {
            return {begin, end};
        }

        std::vector<OpenCL::Lexer::Token> result;
        auto start = begin;
        for (auto iter = begin; iter != end; iter++)
        {
            if (iter->getTokenType() != OpenCL::Lexer::TokenType::Identifier)
            {
                continue;
            }
            const auto& name = std::get<std::string>(iter->getValue());
            auto namePos = iter;
            auto define = state->defines.find(name);
            if (define == state->defines.end() || disabled.count(name) != 0)
            {
                continue;
            }
            result.insert(result.end(), start, iter);
            if (!define->second.identifierList)
            {
                disabled.insert(name);
                start = iter + 1;
                auto temp = result;
                temp.insert(temp.end(), define->second.replacementList.begin(), define->second.replacementList.end());
                std::for_each(temp.begin() + result.size(), temp.end(), [iter, state](OpenCL::Lexer::Token& token) {
                    token.setDefLine(token.getLine());
                    token.setDefColumn(token.getColumn());
                    token.setDefLength(token.getLength());
                    token.setLine(iter->getLine());
                    token.setColumn(iter->getColumn());
                    token.setLength(iter->getLength());
                    token.setMacroId(state->currentID);
                });
                state->currentID++;
                auto vector = macroSubstitute(temp.begin() + result.size(), temp.end(), OpenCL::SourceObject(temp),
                                              reporter, state, disabled);
                result.reserve(result.size() + vector.size());
                std::move(vector.begin(), vector.end(), std::back_inserter(result));
            }
            else if (iter + 1 != end && (iter + 1)->getTokenType() == OpenCL::Lexer::TokenType::OpenParentheses
                     && iter->getColumn() + iter->getLength() == (iter + 1)->getColumn())
            {
                //( must immediately follow the function like macro identifier
                iter += 2;
                std::unordered_map<std::string, std::vector<OpenCL::Lexer::Token>> arguments;
                {
                    auto argsStart = iter;
                    while (iter != end && iter->getTokenType() != OpenCL::Lexer::TokenType::CloseParentheses)
                    {
                        std::size_t i = 0;
                        auto argEnd = std::find_if(iter, end, [&i](const OpenCL::Lexer::Token& token) mutable {
                            if (token.getTokenType() == OpenCL::Lexer::TokenType::OpenParentheses)
                            {
                                i++;
                            }
                            else if (token.getTokenType() == OpenCL::Lexer::TokenType::CloseParentheses)
                            {
                                if (!i)
                                {
                                    return true;
                                }
                                i--;
                            }
                            else if (token.getTokenType() == OpenCL::Lexer::TokenType::Comma)
                            {
                                return !i;
                            }
                            return false;
                        });
                        auto temp = filterForNewlineAndBackslash(iter, argEnd);
                        arguments.emplace((*define->second.identifierList)[arguments.size()],
                                          macroSubstitute(temp.begin(), temp.end(), OpenCL::SourceObject(temp),
                                                          reporter, state, disabled));
                        iter = argEnd;
                        if (iter != end && iter->getTokenType() != OpenCL::Lexer::TokenType::CloseParentheses)
                        {
                            iter++;
                        }
                    }
                    start = iter + 1;
                    if (arguments.size() < define->second.identifierList->size())
                    {
                        if (reporter)
                        {
                            *reporter << OpenCL::Message::error(
                                OpenCL::ErrorMessages::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N.args('\'' + define->first
                                                                                                 + '\''),
                                sourceObject.getLineStart(namePos), sourceObject.getLineEnd(iter),
                                OpenCL::Modifier(argsStart, iter != argsStart ? iter : iter + 1))
                                      << OpenCL::Message::note(
                                             OpenCL::Notes::MISSING_N.args(
                                                 "arguments "
                                                 + static_cast<std::string>(OpenCL::Format::List(
                                                     ", ", " and ",
                                                     {define->second.identifierList->begin() + arguments.size(),
                                                      define->second.identifierList->end()}))),
                                             sourceObject.getLineStart(namePos), sourceObject.getLineEnd(iter));
                        }
                        continue;
                    }
                    else if (arguments.size() > define->second.identifierList->size() && !define->second.hasEllipse)
                    {
                        if (reporter)
                        {
                            *reporter << OpenCL::Message::error(
                                OpenCL::ErrorMessages::PP::TOO_MANY_ARGUMENTS_FOR_MACRO_N.args(define->first),
                                sourceObject.getLineStart(namePos), sourceObject.getLineEnd(iter),
                                OpenCL::Modifier(argsStart, iter != argsStart ? iter : iter + 1));
                        }
                        continue;
                    }
                }

                std::vector<OpenCL::Lexer::Token> replacementSubstituted;
                auto argStart = define->second.replacementList.begin();
                for (auto tokenIter = define->second.replacementList.begin();
                     tokenIter != define->second.replacementList.end(); tokenIter++)
                {
                    if (tokenIter->getTokenType() != OpenCL::Lexer::TokenType::Identifier)
                    {
                        continue;
                    }
                    auto argument = arguments.find(std::get<std::string>(tokenIter->getValue()));
                    if (argument == arguments.end())
                    {
                        continue;
                    }
                    if (tokenIter != define->second.replacementList.begin()
                        && (tokenIter - 1)->getTokenType() == OpenCL::Lexer::TokenType::Pound)
                    {
                        replacementSubstituted.insert(replacementSubstituted.end(), argStart, tokenIter - 1);
                        auto stringValue =
                            OpenCL::Lexer::reconstructTrimmed(argument->second.begin(), argument->second.end());
                        auto string =
                            '\''
                            + std::accumulate(stringValue.begin(), stringValue.end(), std::string{},
                                              [](const std::string& init, char character) {
                                                  if (character == '\'' || character == '"' || character == '\\')
                                                  {
                                                      return init + '\\' + character;
                                                  }
                                                  return init + character;
                                              })
                            + '\'';
                        replacementSubstituted.emplace_back(
                            tokenIter->getLine(), tokenIter->getColumn(), tokenIter->getLength(),
                            OpenCL::Lexer::TokenType::StringLiteral, std::move(stringValue), std::move(string));
                    }
                    else
                    {
                        replacementSubstituted.insert(replacementSubstituted.end(), argStart, tokenIter);
                        replacementSubstituted.insert(replacementSubstituted.end(), argument->second.begin(),
                                                      argument->second.end());
                        std::for_each(replacementSubstituted.end()
                                          - std::distance(argument->second.begin(), argument->second.end()),
                                      replacementSubstituted.end(), [state](OpenCL::Lexer::Token& token) {
                                          if (token.getMacroId() == 0)
                                          {
                                              token.setMacroId(state->currentID);
                                          }
                                      });
                        state->currentID++;
                    }
                    argStart = tokenIter + 1;
                }
                replacementSubstituted.insert(replacementSubstituted.end(), argStart,
                                              define->second.replacementList.end());
                disabled.emplace(define->first);
                auto temp = result;
                temp.reserve(temp.size() + replacementSubstituted.size());
                std::move(replacementSubstituted.begin(), replacementSubstituted.end(), std::back_inserter(temp));
                std::for_each(temp.begin() + result.size(), temp.end(), [namePos, state](OpenCL::Lexer::Token& token) {
                    token.setDefLine(token.getLine());
                    token.setDefColumn(token.getColumn());
                    token.setDefLength(token.getLength());
                    token.setLine(namePos->getLine());
                    token.setColumn(namePos->getColumn());
                    token.setLength(namePos->getLength());
                    if (token.getMacroId() == 0)
                    {
                        token.setMacroId(state->currentID);
                    }
                });
                state->currentID++;
                auto vector = macroSubstitute(temp.begin() + result.size(), temp.end(), OpenCL::SourceObject(temp),
                                              reporter, state, disabled);
                result.reserve(result.size() + vector.size());
                std::move(vector.begin(), vector.end(), std::back_inserter(result));
            }
            state->substitutions.insert(
                {{iter->getLine(), iter->getColumn()}, {{define->second.begin, define->second.end}, {*iter}}});
        }
        result.insert(result.end(), start, end);
        return result;
    }

    std::vector<OpenCL::Lexer::Token> processIfGroup(OpenCL::SourceObject::const_iterator& begin,
                                                     OpenCL::SourceObject::const_iterator end,
                                                     const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                     State* state)
    {
        IfGroup result{};
        assert(begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier);
        const auto& value = std::get<std::string>(begin->getValue());
        assert(value == "if" || value == "ifdef" || value == "ifndef");
        begin++;
        if (value == "if")
        {
            OpenCL::Parser::Context context(sourceObject, reporter);
            auto expEnd = findEOLWithOutBackslash(begin, end);
            auto constantExpression = OpenCL::Parser::parseConditionalExpression(begin, expEnd, context);
            if (begin != expEnd)
            {
                if (reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("newline", "expression"),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                }
                return {};
            }
            result = IfGroup{std::move(constantExpression), nullptr};
        }
        else
        {
            if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
            {
                return {};
            }
            const auto& identifier = std::get<std::string>((begin - 1)->getValue());
            if (begin != end && !expect(OpenCL::Lexer::TokenType::Newline, begin, end, sourceObject, reporter))
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

    std::vector<OpenCL::Lexer::Token> processElIfGroup(OpenCL::SourceObject::const_iterator& begin,
                                                       OpenCL::SourceObject::const_iterator end,
                                                       const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                       State* state)
    {
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "elif");
        begin++;
        auto newline = findEOLWithOutBackslash(begin, end);
        OpenCL::Parser::Context context(sourceObject, reporter);
        auto constantExpression = OpenCL::Parser::parseConditionalExpression(begin, newline, context);
        ElIfGroup result{std::move(constantExpression), {}};
        if (begin != newline)
        {
            if (reporter)
            {
                *reporter << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("newline", "expression"),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return {};
        }
        if (begin != end)
        {
            // TODO: result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
        }
        return {};
    }

    std::vector<OpenCL::Lexer::Token> processElseGroup(OpenCL::SourceObject::const_iterator& begin,
                                                       OpenCL::SourceObject::const_iterator end,
                                                       const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                       State* state)
    {
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "else");
        begin++;
        if (!expect(OpenCL::Lexer::TokenType::Newline, begin, end, sourceObject, reporter))
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

    std::vector<OpenCL::Lexer::Token> processIfSection(OpenCL::SourceObject::const_iterator& begin,
                                                       OpenCL::SourceObject::const_iterator end,
                                                       const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                       State* state)
    {
        assert(begin != end);
        auto ifGroup = processIfGroup(begin, end, sourceObject, reporter, state);
        if (begin == end)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"), sourceObject.getLineStart(begin),
                    sourceObject.getLineEnd(begin), OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
            }
            return {}; //{std::move(ifGroup), {}, {}};
        }
        else if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif", '\'' + begin->emitBack() + '\''),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return {}; //{std::move(ifGroup), {}, {}};
        }
        else
        {
            begin++;
        }

        std::vector<ElIfGroup> elifGroups;
        while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "elif")
        {
            // TODO: elifGroups.push_back(processElIfGroup(begin, end, sourceObject, reporter));
        }

        if (begin == end)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"), sourceObject.getLineStart(begin),
                    sourceObject.getLineEnd(begin), OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
            }
            return {}; //{std::move(ifGroup), std::move(elifGroups), {}};
        }
        else if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif", '\'' + begin->emitBack() + '\''),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return {}; //{std::move(ifGroup), std::move(elifGroups), {}};
        }
        else
        {
            begin++;
        }

        //        IfSection result{std::move(ifGroup), std::move(elifGroups), {}};
        //        if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
        //            && std::get<std::string>(begin->getValue()) == "else")
        //        {
        //            result.optionalElseGroup = processElseGroup(begin, end, sourceObject, reporter);
        //        }
        //
        //        if (begin == end)
        //        {
        //            if (reporter)
        //            {
        //                (*reporter) << OpenCL::Message::error(
        //                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"),
        //                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin), OpenCL::Modifier(begin,
        //                    begin + 1, OpenCL::Modifier::PointAtBeginning));
        //            }
        //        }
        //        else
        //        {
        //            begin++;
        //            if (begin == end || begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier
        //                || std::get<std::string>(begin->getValue()) != "endif")
        //            {
        //                if (reporter)
        //                {
        //                    if (begin == end)
        //                    {
        //                        (*reporter) << OpenCL::Message::error(
        //                            OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"),
        //                            sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
        //                            OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
        //                    }
        //                    else
        //                    {
        //                        (*reporter) << OpenCL::Message::error(
        //                            OpenCL::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif",
        //                                                                                    '\'' + begin->emitBack() +
        //                                                                                    '\''),
        //                            sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
        //                            OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
        //                    }
        //                }
        //            }
        //        }
        //
        //        return result;
    }

    ControlLine::DefineDirective processDefineDirective(OpenCL::SourceObject::const_iterator& begin,
                                                        OpenCL::SourceObject::const_iterator end,
                                                        const OpenCL::SourceObject& sourceObject,
                                                        std::ostream* reporter, State*)
    {
        auto start = begin - 1;
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "define");
        begin++;
        if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
        {
            return {};
        }
        auto namePos = begin - 1;
        const auto& name = std::get<std::string>(namePos->getValue());
        if (begin == end || begin->getTokenType() != OpenCL::Lexer::TokenType::OpenParentheses
            || begin->getLine() != (begin - 1)->getLine()
            || begin->getColumn() != (begin - 1)->getColumn() + (begin - 1)->getLength())
        {
            // No ( after the identifier or there's whitespace in between the identifier and the (
            auto eol = findEOLWithOutBackslash(begin, end);
            auto tokens = filterForNewlineAndBackslash(begin, eol);
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
                   && (first ? begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
                                   || begin->getTokenType() == OpenCL::Lexer::TokenType::Ellipse :
                               begin->getTokenType() == OpenCL::Lexer::TokenType::Comma))
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    begin++;
                }
                if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier)
                {
                    identifierList.push_back(std::get<std::string>(begin->getValue()));
                    begin++;
                }
                else if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Ellipse)
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
                            *reporter << OpenCL::Message::error(
                                OpenCL::ErrorMessages::Parser::EXPECTED_N.args("identifier or '...'"),
                                sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                                OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                        }
                        else
                        {
                            *reporter << OpenCL::Message::error(
                                OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                    "identifier or '...'", '\'' + begin->emitBack() + '\''),
                                sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                                OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                        }
                    }
                    return ControlLine::DefineDirective{start, begin, namePos, name, std::move(identifierList),
                                                        false, {}};
                }
            }
            expect(OpenCL::Lexer::TokenType::CloseParentheses, begin, end, sourceObject, reporter);
            auto newline = findEOLWithOutBackslash(begin, end);
            auto token = filterForNewlineAndBackslash(begin, newline);
            begin = newline;
            return ControlLine::DefineDirective{start,      begin,           namePos, name, std::move(identifierList),
                                                hasEllipse, std::move(token)};
        }
    }

    void processControlLine(OpenCL::SourceObject::const_iterator& begin, OpenCL::SourceObject::const_iterator end,
                            const OpenCL::SourceObject& sourceObject, std::ostream* reporter, State* state)
    {
        const auto& value = std::get<std::string>(begin->getValue());
        if (value == "define")
        {
            auto define = processDefineDirective(begin, end, sourceObject, reporter, state);
            if (state)
            {
                auto [result, success] = state->defines.emplace(define.identifier, define);
                if (!success && reporter)
                {
                    if (define.identifierList.has_value() != result->second.identifierList.has_value()
                        || !define.identifierList
                        || define.identifierList->size() != result->second.identifierList->size()
                        || define.hasEllipse != result->second.hasEllipse
                        || !tokenStructureEqual(define.replacementList.begin(), define.replacementList.end(),
                                                result->second.replacementList.begin(),
                                                result->second.replacementList.end()))
                    {
                        *reporter << OpenCL::Message::error(
                            OpenCL::ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + define.identifier + '\''),
                            sourceObject.getLineStart(define.identifierPos),
                            sourceObject.getLineEnd(define.identifierPos),
                            OpenCL::Modifier(define.identifierPos, define.identifierPos + 1))
                                  << OpenCL::Message::note(
                                         OpenCL::Notes::PREVIOUSLY_DECLARED_HERE,
                                         sourceObject.getLineStart(result->second.identifierPos),
                                         sourceObject.getLineEnd(result->second.identifierPos),
                                         OpenCL::Modifier(result->second.identifierPos, result->second.identifierPos));
                    }
                }
            }
        }
        else if (value == "undef")
        {
            begin++;
            if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
            {
                return;
            }
            if (state)
            {
                state->defines.erase(std::get<std::string>((begin - 1)->getValue()));
            }
            if (begin != end)
            {
                expect(OpenCL::Lexer::TokenType::Newline, begin, end, sourceObject, reporter);
            }
            return;
        }
        else
        {
            begin++;
            auto newline = findEOLWithOutBackslash(begin, end);
            auto tokens = filterForNewlineAndBackslash(begin, newline);
            begin = newline;
            if (value == "include")
            {
                if (tokens.empty() && reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("tokens", "include"),
                        sourceObject.getLineStart(begin - 1), sourceObject.getLineEnd(begin - 1),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                }
                // TODO: Process tokens to open file, tokenize it and call processFile with it
                return;
            }
            else if (value == "line")
            {
                if (tokens.empty() && reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("tokens", "line"),
                        sourceObject.getLineStart(begin - 1), sourceObject.getLineEnd(begin - 1),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
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

    std::vector<OpenCL::Lexer::Token> processGroup(OpenCL::SourceObject::const_iterator& begin,
                                                   OpenCL::SourceObject::const_iterator end,
                                                   const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                   State* state)
    {
        if (begin == end)
        {
            if (reporter)
            {
                *reporter << OpenCL::Message::error(OpenCL::ErrorMessages::Parser::EXPECTED_N.args("Tokens"),
                                                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                                                    OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
            }
            return {};
        }

        std::vector<OpenCL::Lexer::Token> result;
        do
        {
            if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
            {
                auto eol = findEOLWithOutBackslash(begin, end);
                auto vector = macroSubstitute(begin, eol, sourceObject, reporter, state);
                result.reserve(result.size() + vector.size());
                std::move(vector.begin(), vector.end(), std::back_inserter(result));
                begin = eol;
                continue;
            }
            begin++;
            if (begin == end || begin->getTokenType() == OpenCL::Lexer::TokenType::Newline)
            {
                continue;
            }
            if (begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier)
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
            auto eol = findEOLWithOutBackslash(begin, end);
            if (reporter && state)
            {
                *reporter << OpenCL::Message::error(
                    OpenCL::ErrorMessages::PP::N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE.args("'#" + begin->emitBack()
                                                                                           + '\''),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin - 1, begin + 1));
            }
            begin = eol;
        } while (begin != end);
        return result;
    }

    std::vector<OpenCL::Lexer::Token> processFile(OpenCL::SourceObject::const_iterator& begin,
                                                  OpenCL::SourceObject::const_iterator end,
                                                  const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                  State* state)
    {
        std::vector<OpenCL::Lexer::Token> result;
        while (begin != end)
        {
            auto vector = processGroup(begin, end, sourceObject, reporter, state);
            result.reserve(result.size() + vector.size());
            std::move(vector.begin(), vector.end(), std::back_inserter(result));
        }
        return result;
    }
} // namespace

OpenCL::SourceObject OpenCL::PP::preprocess(const SourceObject& sourceObject, std::ostream* reporter)
{
    auto begin = sourceObject.begin();
    State state;
    auto file = processFile(begin, sourceObject.end(), sourceObject, reporter, &state);
    return SourceObject(filterForNewlineAndBackslash(file.begin(), file.end()), Language::C, state.substitutions);
}

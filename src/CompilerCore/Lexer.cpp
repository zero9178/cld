#include "Lexer.hpp"

#include <algorithm>
#include <regex>

std::vector<OpenCL::Lexer::Token> OpenCL::Lexer::tokenize(const std::string& source)
{
    bool isCharacter = false;
    bool isStringLiteral = false;
    bool isComment = false;
    bool isBlockComment = false;
    std::regex identifierMatch("[a-zA-Z_]\\w*");
    std::regex integerLiteralMatch("(0x)?[0-9a-fA-F]+([uU]?(l{0,2}|L{0,2})|(l{0,2}|L{0,2})[uU]?)?");
    std::regex floatLiteralMatch("[0-9]+(\\.[0-9]*)?[fF]?");
    std::vector<Token> result;
    std::string lastText;
    std::uint64_t line = 1, column = 0;

    auto processLastWord = [&]() -> bool
    {
        if (lastText.empty())
        {
            return true;
        }
        else if (lastText == "//")
        {
            isComment = true;
            return false;
        }
        else if (lastText == "/*")
        {
            isBlockComment = true;
            return false;
        }
        else if (lastText == ".")
        {
            result.emplace_back(line, column - lastText.size(), TokenType::Dot);
        }
        else if (lastText == "->")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Arrow);
        }
        else if (lastText == "switch")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::SwitchKeyword);
        }
        else if (lastText == "case")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::CaseKeyword);
        }
        else if (lastText == "default")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::DefaultKeyword);
        }
        else if (lastText == "void")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::VoidKeyword);
        }
        else if (lastText == "struct")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::StructKeyword);
        }
        else if (lastText == "char")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::CharKeyword);
        }
        else if (lastText == "short")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ShortKeyword);
        }
        else if (lastText == "int")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::IntKeyword);
        }
        else if (lastText == "long")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::LongKeyword);
        }
        else if (lastText == "float")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::FloatKeyword);
        }
        else if (lastText == "double")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::DoubleKeyword);
        }
        else if (lastText == "signed")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::SignedKeyword);
        }
        else if (lastText == "const")
        {
            result.emplace_back(line, column - lastText.size(),TokenType::ConstKeyword);
        }
        else if (lastText == "sizeof")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::SizeofKeyword);
        }
        else if (lastText == "unsigned")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::UnsignedKeyword);
        }
        else if (lastText == "typedef")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::TypedefKeyword);
        }
        else if (lastText == "extern")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ExternKeyword);
        }
        else if (lastText == "static")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::StaticKeyword);
        }
        else if (lastText == "auto")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::AutoKeyword);
        }
        else if (lastText == "register")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::RegisterKeyword);
        }
        else if (lastText == "return")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ReturnKeyword);
        }
        else if (lastText == "break")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BreakKeyword);
        }
        else if (lastText == "continue")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ContinueKeyword);
        }
        else if (lastText == "union")
        {
            result.emplace_back(line, column - lastText.size(),TokenType::UnionKeyword);
        }
        else if (lastText == "do")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::DoKeyword);
        }
        else if (lastText == "else")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ElseKeyword);
        }
        else if (lastText == "for")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ForKeyword);
        }
        else if (lastText == "if")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::IfKeyword);
        }
        else if (lastText == "while")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::WhileKeyword);
        }
        else if (lastText == "~")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BitWiseNegation);
        }
        else if (lastText == "!")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::LogicalNegation);
        }
        else if (lastText == "&")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Ampersand);
        }
        else if (lastText == "^")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BitXor);
        }
        else if (lastText == "|")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BitOr);
        }
        else if (lastText == "+")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Addition);
        }
        else if (lastText == "++")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Increment);
        }
        else if (lastText == "--")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Decrement);
        }
        else if (lastText == "-")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Negation);
        }
        else if (lastText == "*")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Asterisk);
        }
        else if (lastText == "/")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Division);
        }
        else if (lastText == "%")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Modulo);
        }
        else if (lastText == "=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Assignment);
        }
        else if (lastText == "==")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Equal);
        }
        else if (lastText == "&&")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::LogicAnd);
        }
        else if (lastText == "||")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::LogicOr);
        }
        else if (lastText == "!=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::NotEqual);
        }
        else if (lastText == "<")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::LessThan);
        }
        else if (lastText == "<=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::LessThanOrEqual);
        }
        else if (lastText == ">")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::GreaterThan);
        }
        else if (lastText == ">=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::GreaterThanOrEqual);
        }
        else if (lastText == "+=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::PlusAssign);
        }
        else if (lastText == "-=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::MinusAssign);
        }
        else if (lastText == "/=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::DivideAssign);
        }
        else if (lastText == "*=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::MultiplyAssign);
        }
        else if (lastText == "%=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ModuloAssign);
        }
        else if (lastText == "<<=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ShiftLeftAssign);
        }
        else if (lastText == ">>=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ShiftRightAssign);
        }
        else if (lastText == "&=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BitAndAssign);
        }
        else if (lastText == "|=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BitOrAssign);
        }
        else if (lastText == "^=")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::BitXorAssign);
        }
        else if (lastText == ">>")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ShiftRight);
        }
        else if (lastText == "<<")
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::ShiftLeft);
        }
        else if (std::regex_match(lastText, identifierMatch))
        {
            result.emplace_back(line,  column - lastText.size(), TokenType::Identifier, std::move(lastText));
        }
        else if (std::regex_match(lastText, integerLiteralMatch))
        {
            static std::regex numbers("[0-9a-fA-F]+");
            bool isHex = false;
            if (lastText.size() > 1 && lastText[0] == '0' && lastText[1] == 'x')
            {
                lastText = lastText.substr(2, lastText.size() - 2);
                isHex = true;
            }
            std::smatch match;
            std::regex_search(lastText, match, numbers);
            std::string filtered = match[0];

            std::stringstream ss = [&lastText, &filtered, isHex]
            {
                std::stringstream ss;
                if (isHex)
                {
                    ss << std::hex;
                }
                else if (lastText[0] == '0')
                {
                    ss << std::oct;
                }
                ss << filtered;
                return ss;
            }();
            std::string suffix = lastText.substr(filtered.size(), lastText.size() - filtered.size());
            if (std::any_of(suffix.begin(), suffix.end(), [](char c)
            { return c == 'u'; }))
            {
                auto erase = std::remove(suffix.begin(), suffix.end(), 'u');
                suffix.erase(erase, suffix.end());
                if (suffix.empty() || suffix == "l" || suffix == "L")
                {
                    std::uint64_t number;
                    ss >> number;
                    if (number > std::numeric_limits<std::uint32_t>::max())
                    {
                        result.emplace_back(line,  column - lastText.size(), TokenType::Literal, number);
                    }
                    else
                    {
                        result.emplace_back(line,  column - lastText.size(), TokenType::Literal, static_cast<std::uint32_t>(number));
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    std::uint64_t number;
                    ss >> number;
                    result.emplace_back(line,  column - lastText.size(), TokenType::Literal, number);
                }
                else
                {
                    throw std::runtime_error("Invalid suffix " + suffix);
                }
            }
            else
            {
                if (suffix.empty() || suffix == "l" || suffix == "L")
                {
                    std::int64_t number;
                    ss >> number;
                    if (number > std::numeric_limits<std::int32_t>::max())
                    {
                        if(number <= std::numeric_limits<std::uint32_t>::max())
                        {
                            result.emplace_back(line,  column - lastText.size(), TokenType::Literal, static_cast<std::uint32_t>(number));
                        }
                        else
                        {
                            result.emplace_back(line,  column - lastText.size(), TokenType::Literal, number);
                        }
                    }
                    else
                    {
                        result.emplace_back(line,  column - lastText.size(), TokenType::Literal, static_cast<std::int32_t>(number));
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    std::int64_t number;
                    ss >> number;
                    result.emplace_back(line,  column - lastText.size(), TokenType::Literal, number);
                }
                else
                {
                    throw std::runtime_error("Invalid suffix " + suffix);
                }
            }
        }
        else if (std::regex_match(lastText, floatLiteralMatch))
        {
            if (lastText.back() == 'f' || lastText.back() == 'F')
            {
                std::istringstream ss(lastText.substr(0, lastText.size() - 1));
                float number;
                ss >> number;
                result.emplace_back(line,  column - lastText.size(), TokenType::Literal, number);
            }
            else
            {
                std::istringstream ss(lastText);
                double number;
                ss >> number;
                result.emplace_back(line,  column - lastText.size(), TokenType::Literal, number);
            }
        }
        else
        {
            throw std::runtime_error("Illegal token" + lastText);
        }
        lastText.clear();
        return true;
    };

    auto matches = [&]
    {
        return lastText == "0x" || std::regex_search(lastText, identifierMatch)
            || std::regex_search(lastText, integerLiteralMatch);
    };

    for (auto iter : source)
    {
        if(iter == '\n' || iter == '\r')
        {
            line++;
            column = 0;
        }
        else
        {
            column++;
        }
        if (isComment)
        {
            if (iter == '\n')
            {
                isComment = false;
                lastText.clear();
                continue;
            }
            else
            {
                continue;
            }
        }
        if (isBlockComment)
        {
            lastText += iter;
            if (lastText.size() >= 2 && lastText.substr(lastText.size() - 2) == "*/")
            {
                isBlockComment = false;
                lastText.clear();
                continue;
            }
            else
            {
                continue;
            }
        }
        if (isStringLiteral)
        {
            if (iter == '"')
            {
                if (!lastText.empty() && lastText.back() != '\\')
                {
                    isStringLiteral = false;
                    result.emplace_back(line,  column - lastText.size(), TokenType::Literal, lastText);
                    lastText.clear();
                    continue;
                }
                else if (lastText.empty())
                {
                    isStringLiteral = false;
                    result.emplace_back(line,  column - lastText.size(), TokenType::Literal, lastText);
                    lastText.clear();
                    continue;
                }
            }
            lastText += iter;
            continue;
        }
        if (isCharacter)
        {
            if (iter == '\'')
            {
                isCharacter = false;
                result.emplace_back(line,  column - lastText.size(), TokenType::Literal, [lastText]() -> std::int32_t
                {
                    if (lastText.empty())
                    {
                        throw std::runtime_error("Character constant can't be empty");
                    }
                    if (lastText.size() == 1)
                    {
                        return lastText.front();
                    }
                    if(lastText == "\\'")
                    {
                        return '\'';
                    }
                    else if(lastText == "\\\"")
                    {
                        return '"';
                    }
                    else if(lastText == "\\\\")
                    {
                        return '\\';
                    }
                    else if(lastText == "\\a")
                    {
                        return '\a';
                    }
                    else if(lastText == "\\b")
                    {
                        return '\b';
                    }
                    else if(lastText == "\\f")
                    {
                        return '\f';
                    }
                    else if(lastText == "\\n")
                    {
                        return '\n';
                    }
                    else if(lastText == "\\r")
                    {
                        return '\r';
                    }
                    else if(lastText == "\\t")
                    {
                        return '\t';
                    }
                    else if(lastText == "\\v")
                    {
                        return '\v';
                    }
                    else
                    {
                        std::regex octalChar("\\\\[0-7]{1,3}");
                        if(std::regex_match(lastText,octalChar))
                        {
                            std::istringstream ss(lastText.substr(1,lastText.size()-1));
                            std::int32_t number;
                            ss>>number;
                            return number;
                        }
                        else
                        {
                            std::regex hexChar("\\\\x[0-9a-fA-F]*");
                            std::smatch match;
                            std::regex_search(lastText,match,hexChar);
                            if(match.empty())
                            {
                                throw std::runtime_error("Could not find hex chars");
                            }
                            std::istringstream ss(match[0]);
                            std::int32_t number;
                            ss>>number;
                            return number;
                        }
                    }
                }());
                lastText.clear();
                continue;
            }
            lastText += iter;
            continue;
        }
        if (!iter)
        {
            continue;
        }
        column++;
        switch (iter)
        {
        case '\'':
        {
            if (processLastWord())
            {
                isCharacter = true;
            }
            break;
        }
        case '(':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::OpenParenthese);
            }
            break;
        }
        case ')':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::CloseParenthese);
            }
            break;
        }
        case '{':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::OpenBrace);
            }
            break;
        }
        case '}':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::CloseBrace);
            }
            break;
        }
        case ';':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::SemiColon);
            }
            break;
        }
        case ',':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::Comma);
            }
            break;
        }
        case '?':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::QuestionMark);
            }
            break;
        }
        case ':':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::Colon);
            }
            break;
        }
        case '[':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::OpenSquareBracket);
            }
            break;
        }
        case ']':
        {
            if (processLastWord())
            {
                result.emplace_back(line, column, TokenType::CloseSquareBracket);
            }
            break;
        }
        case '"':
        {
            if (processLastWord())
            {
                isStringLiteral = true;
            }
            break;
        }
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case '^':
        case '=':
        case '&':
        case '|':
        case '!':
        case '~':
        case '<':
        case '>':
        {
            if (matches())
            {
                processLastWord();
            }
            lastText += iter;
            break;
        }
        case '.':
        {
            if (!(std::regex_match(lastText, integerLiteralMatch) || std::regex_match(lastText, floatLiteralMatch)))
            {
                processLastWord();
            }
            lastText += iter;
            break;
        }
        case '\r':
        case '\n':
        {
            if(!processLastWord())
            {
                isComment = false;
                lastText.clear();
            }
            break;
        }
        case '\t':
            [[fallthrough]];
        case ' ':
        {
            processLastWord();
            break;
        }
        default:
        {
            if (!matches())
            {
                processLastWord();
            }
            lastText += iter;
        }
        }
    }
    if(!lastText.empty())
    {
        processLastWord();
    }

    std::reverse(result.begin(), result.end());
    return result;
}

#include "Lexer.hpp"

#include <algorithm>
#include <regex>

std::vector<OpenCL::Lexer::Token> OpenCL::Lexer::tokenize(const std::string& source)
{
    bool isStringLiteral = false;
    bool isComment = false;
    bool isBlockComment = false;
    std::regex identifierMatch("[a-zA-Z_]\\w*");
    std::regex integerLiteralMatch("(0x)?[0-9a-fA-F]+([uU]?(l{0,2}|L{0,2})|(l{0,2}|L{0,2})[uU]?)?");
    std::regex floatLiteralMatch("[0-9]+(\\.[0-9]+)?[fF]?");
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
        if (lastText == "void")
        {
            result.emplace_back(line, column, TokenType::VoidKeyword);
        }
        else if (lastText == "void")
        {
            result.emplace_back(line, column, TokenType::VoidKeyword);
        }
        else if (lastText == "char")
        {
            result.emplace_back(line, column, TokenType::CharKeyword);
        }
        else if (lastText == "short")
        {
            result.emplace_back(line, column, TokenType::ShortKeyword);
        }
        else if (lastText == "int")
        {
            result.emplace_back(line, column, TokenType::IntKeyword);
        }
        else if (lastText == "long")
        {
            result.emplace_back(line, column, TokenType::LongKeyword);
        }
        else if (lastText == "float")
        {
            result.emplace_back(line, column, TokenType::FloatKeyword);
        }
        else if (lastText == "double")
        {
            result.emplace_back(line, column, TokenType::DoubleKeyword);
        }
        else if (lastText == "signed")
        {
            result.emplace_back(line, column, TokenType::SignedKeyword);
        }
        else if(lastText == "sizeof")
        {
            result.emplace_back(line,column,TokenType::SizeofKeyword);
        }
        else if (lastText == "unsigned")
        {
            result.emplace_back(line, column, TokenType::UnsignedKeyword);
        }
        else if (lastText == "typedef")
        {
            result.emplace_back(line, column, TokenType::TypedefKeyword);
        }
        else if (lastText == "extern")
        {
            result.emplace_back(line, column, TokenType::ExternKeyword);
        }
        else if (lastText == "static")
        {
            result.emplace_back(line, column, TokenType::StaticKeyword);
        }
        else if (lastText == "auto")
        {
            result.emplace_back(line, column, TokenType::AutoKeyword);
        }
        else if (lastText == "register")
        {
            result.emplace_back(line, column, TokenType::RegisterKeyword);
        }
        else if (lastText == "return")
        {
            result.emplace_back(line, column, TokenType::ReturnKeyword);
        }
        else if (lastText == "break")
        {
            result.emplace_back(line, column, TokenType::BreakKeyword);
        }
        else if (lastText == "continue")
        {
            result.emplace_back(line, column, TokenType::ContinueKeyword);
        }
        else if (lastText == "do")
        {
            result.emplace_back(line, column, TokenType::DoKeyword);
        }
        else if (lastText == "else")
        {
            result.emplace_back(line, column, TokenType::ElseKeyword);
        }
        else if (lastText == "for")
        {
            result.emplace_back(line, column, TokenType::ForKeyword);
        }
        else if (lastText == "if")
        {
            result.emplace_back(line, column, TokenType::IfKeyword);
        }
        else if (lastText == "while")
        {
            result.emplace_back(line, column, TokenType::WhileKeyword);
        }
        else if (lastText == "~")
        {
            result.emplace_back(line, column, TokenType::BitWiseNegation);
        }
        else if (lastText == "!")
        {
            result.emplace_back(line, column, TokenType::LogicalNegation);
        }
        else if (lastText == "&")
        {
            result.emplace_back(line, column, TokenType::Ampersand);
        }
        else if (lastText == "^")
        {
            result.emplace_back(line, column, TokenType::BitXor);
        }
        else if (lastText == "|")
        {
            result.emplace_back(line, column, TokenType::BitOr);
        }
        else if (lastText == "+")
        {
            result.emplace_back(line, column, TokenType::Addition);
        }
        else if (lastText == "++")
        {
            result.emplace_back(line, column, TokenType::Increment);
        }
        else if (lastText == "--")
        {
            result.emplace_back(line, column, TokenType::Decrement);
        }
        else if (lastText == "-")
        {
            result.emplace_back(line, column, TokenType::Negation);
        }
        else if (lastText == "*")
        {
            result.emplace_back(line, column, TokenType::Asterisk);
        }
        else if (lastText == "/")
        {
            result.emplace_back(line, column, TokenType::Division);
        }
        else if (lastText == "%")
        {
            result.emplace_back(line, column, TokenType::Modulo);
        }
        else if (lastText == "=")
        {
            result.emplace_back(line, column, TokenType::Assignment);
        }
        else if (lastText == "==")
        {
            result.emplace_back(line, column, TokenType::Equal);
        }
        else if (lastText == "&&")
        {
            result.emplace_back(line, column, TokenType::LogicAnd);
        }
        else if (lastText == "||")
        {
            result.emplace_back(line, column, TokenType::LogicOr);
        }
        else if (lastText == "!=")
        {
            result.emplace_back(line, column, TokenType::NotEqual);
        }
        else if (lastText == "<")
        {
            result.emplace_back(line, column, TokenType::LessThan);
        }
        else if (lastText == "<=")
        {
            result.emplace_back(line, column, TokenType::LessThanOrEqual);
        }
        else if (lastText == ">")
        {
            result.emplace_back(line, column, TokenType::GreaterThan);
        }
        else if (lastText == ">=")
        {
            result.emplace_back(line, column, TokenType::GreaterThanOrEqual);
        }
        else if (lastText == "+=")
        {
            result.emplace_back(line, column, TokenType::PlusAssign);
        }
        else if (lastText == "-=")
        {
            result.emplace_back(line, column, TokenType::MinusAssign);
        }
        else if (lastText == "/=")
        {
            result.emplace_back(line, column, TokenType::DivideAssign);
        }
        else if (lastText == "*=")
        {
            result.emplace_back(line, column, TokenType::MultiplyAssign);
        }
        else if (lastText == "%=")
        {
            result.emplace_back(line, column, TokenType::ModuloAssign);
        }
        else if (lastText == "<<=")
        {
            result.emplace_back(line, column, TokenType::ShiftLeftAssign);
        }
        else if (lastText == ">>=")
        {
            result.emplace_back(line, column, TokenType::ShiftRightAssign);
        }
        else if (lastText == "&=")
        {
            result.emplace_back(line, column, TokenType::BitAndAssign);
        }
        else if (lastText == "|=")
        {
            result.emplace_back(line, column, TokenType::BitOrAssign);
        }
        else if (lastText == "^=")
        {
            result.emplace_back(line, column, TokenType::BitXorAssign);
        }
        else if (lastText == ">>")
        {
            result.emplace_back(line, column, TokenType::ShiftRight);
        }
        else if (lastText == "<<")
        {
            result.emplace_back(line, column, TokenType::ShiftLeft);
        }
        else if (std::regex_match(lastText, identifierMatch))
        {
            result.emplace_back(line, column, TokenType::Identifier, std::move(lastText));
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
                        result.emplace_back(line, column, TokenType::Literal, number);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Literal, static_cast<std::uint32_t>(number));
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    std::uint64_t number;
                    ss >> number;
                    result.emplace_back(line, column, TokenType::Literal, number);
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
                    if (number > std::numeric_limits<std::uint32_t>::max())
                    {
                        result.emplace_back(line, column, TokenType::Literal, number);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Literal, static_cast<std::uint32_t>(number));
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    std::int64_t number;
                    ss >> number;
                    result.emplace_back(line, column, TokenType::Literal, number);
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
                result.emplace_back(line, column, TokenType::Literal, number);
            }
            else
            {
                std::istringstream ss(lastText);
                double number;
                ss >> number;
                result.emplace_back(line, column, TokenType::Literal, number);
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
                    result.emplace_back(line, column, TokenType::StringLiteral, lastText);
                    continue;
                }
                else if (lastText.empty())
                {
                    isStringLiteral = false;
                    result.emplace_back(line, column, TokenType::StringLiteral, lastText);
                    continue;
                }
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
            if(processLastWord())
            {
                result.emplace_back(line,column,TokenType::OpenSquareBracket);
            }
            break;
        }
        case ']':
        {
            if(processLastWord())
            {
                result.emplace_back(line,column,TokenType::CloseSquareBracket);
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
        case '\r':
        case '\n':
        {
            column = 0;
            line++;
            processLastWord();
            break;
        }
        case '\t':column += 7;
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

    std::reverse(result.begin(), result.end());
    return result;
}

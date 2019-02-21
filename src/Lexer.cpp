#include "Lexer.hpp"

#include <algorithm>
#include <regex>

std::vector<OpenCL::Lexer::Token> OpenCL::Lexer::tokenize(const std::string& source)
{
    bool isStringLiteral = false;
    bool isComment = false;
    bool isBlockComment = false;
    std::regex identifierMatch("[a-zA-Z_]\\w*");
    std::regex integerLiteralMatch("(0x)?[0-9a-fA-F]+");
    std::vector<Token> result;
    std::string lastText;
    std::uint64_t line = 1,column = 0;

    auto processLastWord = [&]
    {
        if (lastText.empty())
        {
            return;
        }
        if (lastText == "void")
        {
           result.emplace_back(line,column,TokenType::VoidKeyword);
        }
        else if (lastText == "void")
        {
            result.emplace_back(line,column,TokenType::VoidKeyword);
        }
        else if (lastText == "char")
        {
            result.emplace_back(line,column,TokenType::CharKeyword);
        }
        else if (lastText == "short")
        {
            result.emplace_back(line,column,TokenType::ShortKeyword);
        }
        else if (lastText == "int")
        {
            result.emplace_back(line,column,TokenType::IntKeyword);
        }
        else if (lastText == "long")
        {
            result.emplace_back(line,column,TokenType::LongKeyword);
        }
        else if (lastText == "float")
        {
            result.emplace_back(line,column,TokenType::FloatKeyword);
        }
        else if (lastText == "double")
        {
            result.emplace_back(line,column,TokenType::DoubleKeyword);
        }
        else if (lastText == "signed")
        {
            result.emplace_back(line,column,TokenType::SignedKeyword);
        }
        else if (lastText == "unsigned")
        {
            result.emplace_back(line,column,TokenType::UnsignedKeyword);
        }
        else if (lastText == "typedef")
        {
            result.emplace_back(line,column,TokenType::TypedefKeyword);
        }
        else if (lastText == "extern")
        {
            result.emplace_back(line,column,TokenType::ExternKeyword);
        }
        else if (lastText == "static")
        {
            result.emplace_back(line,column,TokenType::StaticKeyword);
        }
        else if (lastText == "auto")
        {
            result.emplace_back(line,column,TokenType::AutoKeyword);
        }
        else if (lastText == "register")
        {
            result.emplace_back(line,column,TokenType::RegisterKeyword);
        }
        else if (lastText == "return")
        {
            result.emplace_back(line,column,TokenType::ReturnKeyword);
        }
        else if (lastText == "break")
        {
            result.emplace_back(line,column,TokenType::BreakKeyword);
        }
        else if (lastText == "continue")
        {
            result.emplace_back(line,column,TokenType::ContinueKeyword);
        }
        else if (lastText == "do")
        {
            result.emplace_back(line,column,TokenType::DoKeyword);
        }
        else if (lastText == "else")
        {
            result.emplace_back(line,column,TokenType::ElseKeyword);
        }
        else if (lastText == "for")
        {
            result.emplace_back(line,column,TokenType::ForKeyword);
        }
        else if (lastText == "if")
        {
            result.emplace_back(line,column,TokenType::IfKeyword);
        }
        else if (lastText == "while")
        {
            result.emplace_back(line,column,TokenType::WhileKeyword);
        }
        else if (lastText == "~")
        {
            result.emplace_back(line,column,TokenType::BitWiseNegation);
        }
        else if (lastText == "!")
        {
            result.emplace_back(line,column,TokenType::LogicalNegation);
        }
        else if (lastText == "&")
        {
            result.emplace_back(line,column,TokenType::BitAnd);
        }
        else if (lastText == "^")
        {
            result.emplace_back(line,column,TokenType::BitXor);
        }
        else if (lastText == "|")
        {
            result.emplace_back(line,column,TokenType::BitOr);
        }
        else if (lastText == "+")
        {
            result.emplace_back(line,column,TokenType::Addition);
        }
        else if (lastText == "++")
        {
            result.emplace_back(line,column,TokenType::Increment);
        }
        else if (lastText == "--")
        {
            result.emplace_back(line,column,TokenType::Decrement);
        }
        else if (lastText == "-")
        {
            result.emplace_back(line,column,TokenType::Negation);
        }
        else if (lastText == "*")
        {
            result.emplace_back(line,column,TokenType::Multiplication);
        }
        else if (lastText == "/")
        {
            result.emplace_back(line,column,TokenType::Division);
        }
        else if (lastText == "%")
        {
            result.emplace_back(line,column,TokenType::Modulo);
        }
        else if (lastText == "=")
        {
            result.emplace_back(line,column,TokenType::Assignment);
        }
        else if (lastText == "==")
        {
            result.emplace_back(line,column,TokenType::Equal);
        }
        else if (lastText == "&&")
        {
            result.emplace_back(line,column,TokenType::LogicAnd);
        }
        else if (lastText == "||")
        {
            result.emplace_back(line,column,TokenType::LogicOr);
        }
        else if (lastText == "!=")
        {
            result.emplace_back(line,column,TokenType::NotEqual);
        }
        else if (lastText == "<")
        {
            result.emplace_back(line,column,TokenType::LessThan);
        }
        else if (lastText == "<=")
        {
            result.emplace_back(line,column,TokenType::LessThanOrEqual);
        }
        else if (lastText == ">")
        {
            result.emplace_back(line,column,TokenType::GreaterThan);
        }
        else if (lastText == ">=")
        {
            result.emplace_back(line,column,TokenType::GreaterThanOrEqual);
        }
        else if (lastText == "+=")
        {
            result.emplace_back(line,column,TokenType::PlusAssign);
        }
        else if (lastText == "-=")
        {
            result.emplace_back(line,column,TokenType::MinusAssign);
        }
        else if (lastText == "/=")
        {
            result.emplace_back(line,column,TokenType::DivideAssign);
        }
        else if (lastText == "*=")
        {
            result.emplace_back(line,column,TokenType::MultiplyAssign);
        }
        else if (lastText == "%=")
        {
            result.emplace_back(line,column,TokenType::ModuloAssign);
        }
        else if (lastText == "<<=")
        {
            result.emplace_back(line,column,TokenType::ShiftLeftAssign);
        }
        else if (lastText == ">>=")
        {
            result.emplace_back(line,column,TokenType::ShiftRightAssign);
        }
        else if (lastText == "&=")
        {
            result.emplace_back(line,column,TokenType::BitAndAssign);
        }
        else if (lastText == "|=")
        {
            result.emplace_back(line,column,TokenType::BitOrAssign);
        }
        else if (lastText == "^=")
        {
            result.emplace_back(line,column,TokenType::BitXorAssign);
        }
        else if (lastText == ">>")
        {
            result.emplace_back(line,column,TokenType::ShiftRight);
        }
        else if (lastText == "<<")
        {
            result.emplace_back(line,column,TokenType::ShiftLeft);
        }
        else if (lastText == "//")
        {
            isComment = true;
        }
        else if(lastText == "/*")
        {
            isBlockComment = true;
        }
        else if (std::regex_match(lastText, identifierMatch))
        {
            result.emplace_back(line,column,TokenType::Identifier,std::move(lastText));
        }
        else if (std::regex_match(lastText, integerLiteralMatch))
        {
            if (lastText.size() > 1 && lastText[0] == '0' && lastText[1] == 'x')
            {
                std::stringstream ss;
                ss << std::hex << lastText.substr(2, lastText.size() - 2);
                std::uint64_t number;
                ss >> number;
                result.emplace_back(line,column,TokenType::IntegerLiteral,number);
            }
            else if (lastText[0] == '0')
            {
                std::stringstream ss;
                ss << std::oct << lastText;
                std::uint64_t number;
                ss >> number;
                result.emplace_back(line,column,TokenType::IntegerLiteral,number);
            }
            else
            {
                std::stringstream ss;
                ss << lastText;
                std::uint64_t number;
                ss >> number;
                result.emplace_back(line,column,TokenType::IntegerLiteral,number);
            }
        }
        else
        {
            throw std::runtime_error("Illegal token" + lastText);
        }
        lastText.clear();
    };

    auto matches = [&]
    {
        return lastText == "0x" || std::regex_search(lastText, identifierMatch)
            || std::regex_search(lastText, integerLiteralMatch);
    };

    for (auto iter : source)
    {
        if(isComment)
        {
            if(iter == '\n')
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
        if(isBlockComment)
        {
            lastText += iter;
            if(lastText.size() >= 2 && lastText.substr(lastText.size()-2) == "*/")
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
        if(isStringLiteral)
        {
            if(iter == '"')
            {
                if(!lastText.empty() && lastText.back() != '\\')
                {
                    isStringLiteral = false;
                    result.emplace_back(line,column,TokenType::StringLiteral,lastText);
                    continue;
                }
                else if(lastText.empty())
                {
                    isStringLiteral = false;
                    result.emplace_back(line,column,TokenType::StringLiteral,lastText);
                    continue;
                }
            }
            lastText += iter;
            continue;
        }
        if(!iter)
        {
            continue;
        }
        column++;
        switch (iter)
        {
        case '(':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::OpenParenthese);
            break;
        }
        case ')':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::CloseParenthese);
            break;
        }
        case '{':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::OpenBrace);
            break;
        }
        case '}':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::CloseBrace);
            break;
        }
        case ';':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::SemiColon);
            break;
        }
        case ',':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::Comma);
            break;
        }
        case '?':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::QuestionMark);
            break;
        }
        case ':':
        {
            processLastWord();
            result.emplace_back(line,column,TokenType::Colon);
            break;
        }
        case '"':
        {
            processLastWord();
            isStringLiteral = true;
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
        case '\n':
        {
            column = 0;
            line++;
            processLastWord();
            break;
        }
        case '\t':
            column += 7;
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

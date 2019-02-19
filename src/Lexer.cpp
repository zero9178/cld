#include "Lexer.hpp"

#include <algorithm>
#include <regex>


std::vector<OpenCL::Tokens::TokenVariant> OpenCL::Tokens::tokenize(const std::string& source)
{
    std::regex identifierMatch("[a-zA-Z_]\\w*");
    std::regex integerLiteralMatch("(0x)?[0-9a-fA-F]+");
    std::vector<TokenVariant> result;
    std::string lastText;
    auto processLastWord = [&]
    {
        if (lastText.empty())
        {
            return;
        }
        if (lastText == "int")
        {
            result.emplace_back(IntKeyword());
        } else if (lastText == "return")
        {
            result.emplace_back(ReturnKeyword());
        } else if (lastText == "break")
        {
            result.emplace_back(BreakKeyword());
        } else if (lastText == "continue")
        {
            result.emplace_back(ContinueKeyword());
        } else if (lastText == "do")
        {
            result.emplace_back(DoKeyword());
        } else if (lastText == "else")
        {
            result.emplace_back(ElseKeyword());
        } else if (lastText == "for")
        {
            result.emplace_back(ForKeyword());
        } else if (lastText == "if")
        {
            result.emplace_back(IfKeyword());
        } else if (lastText == "while")
        {
            result.emplace_back(WhileKeyword());
        } else if (lastText == "~")
        {
            result.emplace_back(BitWiseNegation());
        } else if (lastText == "!")
        {
            result.emplace_back(LogicalNegation());
        } else if (lastText == "&")
        {
            result.emplace_back(BitAnd());
        } else if (lastText == "^")
        {
            result.emplace_back(BitXor());
        } else if (lastText == "|")
        {
            result.emplace_back(BitOr());
        } else if (lastText == "+")
        {
            result.emplace_back(Addition());
        } else if (lastText == "++")
        {
            result.emplace_back(Increment());
        } else if (lastText == "--")
        {
            result.emplace_back(Decrement());
        } else if (lastText == "-")
        {
            result.emplace_back(Negation());
        } else if (lastText == "*")
        {
            result.emplace_back(Multiplication());
        } else if (lastText == "/")
        {
            result.emplace_back(Division());
        } else if (lastText == "%")
        {
            result.emplace_back(Modulo());
        } else if (lastText == "=")
        {
            result.emplace_back(Assignment());
        } else if (lastText == "==")
        {
            result.emplace_back(Equal());
        } else if (lastText == "&&")
        {
            result.emplace_back(LogicAnd());
        } else if (lastText == "||")
        {
            result.emplace_back(LogicOr());
        } else if (lastText == "!=")
        {
            result.emplace_back(NotEqual());
        } else if (lastText == "<")
        {
            result.emplace_back(LessThan());
        } else if (lastText == "<=")
        {
            result.emplace_back(LessThanOrEqual());
        } else if (lastText == ">")
        {
            result.emplace_back(GreaterThan());
        } else if (lastText == ">=")
        {
            result.emplace_back(GreaterThanOrEqual());
        } else if (lastText == "+=")
        {
            result.emplace_back(PlusAssign());
        } else if (lastText == "-=")
        {
            result.emplace_back(MinusAssign());
        } else if (lastText == "/=")
        {
            result.emplace_back(DivideAssign());
        } else if (lastText == "*=")
        {
            result.emplace_back(MultiplyAssign());
        } else if (lastText == "%=")
        {
            result.emplace_back(ModuloAssign());
        } else if (lastText == "<<=")
        {
            result.emplace_back(ShiftLeftAssign());
        } else if (lastText == ">>=")
        {
            result.emplace_back(ShiftRightAssign());
        } else if (lastText == "&=")
        {
            result.emplace_back(BitAndAssign());
        } else if (lastText == "|=")
        {
            result.emplace_back(BitOrAssign());
        } else if (lastText == "^=")
        {
            result.emplace_back(BitXorAssign());
        } else if (lastText == ">>")
        {
            result.emplace_back(ShiftRight());
        } else if (lastText == "<<")
        {
            result.emplace_back(ShiftLeft());
        } else if (std::regex_search(lastText, identifierMatch))
        {
            result.emplace_back(Identifier{lastText});
        } else if (std::regex_search(lastText, integerLiteralMatch))
        {
            if (lastText.size() > 1 && lastText[0] == '0' && lastText[1] == 'x')
            {
                std::stringstream ss;
                ss << std::hex << lastText.substr(2, lastText.size() - 2);
                std::uint64_t number;
                ss >> number;
                result.emplace_back(IntegerLiteral{number});
            } else if (lastText[0] == '0')
            {
                std::stringstream ss;
                ss << std::oct << lastText;
                std::uint64_t number;
                ss >> number;
                result.emplace_back(IntegerLiteral{number});
            } else
            {
                std::stringstream ss;
                ss << lastText;
                std::uint64_t number;
                ss >> number;
                result.emplace_back(IntegerLiteral{number});
            }
        } else
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
        switch (iter)
        {
        case '(':
        {
            processLastWord();
            result.emplace_back(OpenParanthese());
            break;
        }
        case ')':
        {
            processLastWord();
            result.emplace_back(CloseParanthese());
            break;
        }
        case '{':
        {
            processLastWord();
            result.emplace_back(OpenBrace());
            break;
        }
        case '}':
        {
            processLastWord();
            result.emplace_back(CloseBrace());
            break;
        }
        case ';':
        {
            processLastWord();
            result.emplace_back(SemiColon());
            break;
        }
        case ',':
        {
            processLastWord();
            result.emplace_back(Comma());
            break;
        }
        case '?':
        {
            processLastWord();
            result.emplace_back(QuestionMark());
            break;
        }
        case ':':
        {
            processLastWord();
            result.emplace_back(Colon());
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
        case ' ':
        case '\t':
        case '\n':
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

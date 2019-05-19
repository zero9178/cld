#include "Lexer.hpp"

#include <algorithm>
#include <regex>

namespace
{
    std::int32_t charactersToCharLiteral(const std::string& characters)
    {
        if (characters.empty())
        {
            throw std::runtime_error("Character constant can't be empty");
        }
        if (characters.size() == 1)
        {
            return characters.front();
        }
        if (characters == "\\'")
        {
            return '\'';
        }
        else if (characters == "\\\"")
        {
            return '"';
        }
        else if (characters == "\\?")
        {
            return '\?';
        }
        else if (characters == "\\\\")
        {
            return '\\';
        }
        else if (characters == "\\a")
        {
            return '\a';
        }
        else if (characters == "\\b")
        {
            return '\b';
        }
        else if (characters == "\\f")
        {
            return '\f';
        }
        else if (characters == "\\n")
        {
            return '\n';
        }
        else if (characters == "\\r")
        {
            return '\r';
        }
        else if (characters == "\\t")
        {
            return '\t';
        }
        else if (characters == "\\v")
        {
            return '\v';
        }
        else if (characters.front() == '\\')
        {
            if (characters[1] == 'x')
            {
                if (characters.size() <= 2)
                {
                    throw std::runtime_error("At least one hexadecimal digit required");
                }
                std::istringstream ss(characters.substr(2, characters.size() - 1));
                std::int32_t number;
                if (!(ss >> std::hex >> number))
                {
                    throw std::runtime_error("Failed to convert " + ss.str() + " to hex character");
                }
                if (number > std::numeric_limits<std::uint8_t>::max())
                {
                    throw std::runtime_error(
                        "Character constant is not allowed to have a value higher than the maximum value of unsigned char");
                }
                return number;
            }
            else
            {
                if (characters.size() <= 1)
                {
                    throw std::runtime_error("At least one octal digit required");
                }
                std::istringstream ss(characters.substr(1, characters.size() - 1));
                std::int32_t number;
                if (!(ss >> std::oct >> number))
                {
                    throw std::runtime_error("Failed to convert " + ss.str() + " to octal character");
                }
                if (number > std::numeric_limits<std::uint8_t>::max())
                {
                    throw std::runtime_error(
                        "Character constant is not allowed to have a value higher than the maximum value of unsigned char");
                }
                return number;
            }
        }
        throw std::runtime_error("Incorrect sequence for character literal:" + characters);
    }

    bool isKeyword(const std::string& characters)
    {
        return characters == "auto" || characters == "double" || characters == "int" || characters == "struct"
            || characters == "break" || characters == "else" || characters == "long" || characters == "switch"
            || characters == "case" || characters == "enum" || characters == "register" || characters == "typedef"
            || characters == "char" || characters == "extern" || characters == "return" || characters == "union"
            || characters == "const" || characters == "float" || characters == "short" || characters == "unsigned"
            || characters == "continue" || characters == "for" || characters == "signed" || characters == "void"
            || characters == "default" || characters == "goto" || characters == "sizeof" || characters == "volatile"
            || characters == "restrict" || characters == "do" || characters == "if" || characters == "static"
            || characters == "while" || characters == "inline";
    }

    OpenCL::Lexer::TokenType charactersToKeyword(const std::string& characters)
    {
        using namespace OpenCL::Lexer;
        if (characters == "auto")
        {
            return TokenType::AutoKeyword;
        }
        if (characters == "double")
        {
            return TokenType::DoubleKeyword;
        }
        if (characters == "int")
        {
            return TokenType::IntKeyword;
        }
        if (characters == "struct")
        {
            return TokenType::StructKeyword;
        }
        if (characters == "break")
        {
            return TokenType::BreakKeyword;
        }
        if (characters == "else")
        {
            return TokenType::ElseKeyword;
        }
        if (characters == "long")
        {
            return TokenType::LongKeyword;
        }
        if (characters == "switch")
        {
            return TokenType::SwitchKeyword;
        }
        if (characters == "case")
        {
            return TokenType::CaseKeyword;
        }
        if (characters == "enum")
        {
            return TokenType::EnumKeyword;
        }
        if (characters == "register")
        {
            return TokenType::RegisterKeyword;
        }
        if (characters == "typedef")
        {
            return TokenType::TypedefKeyword;
        }
        if (characters == "char")
        {
            return TokenType::CharKeyword;
        }
        if (characters == "extern")
        {
            return TokenType::ExternKeyword;
        }
        if (characters == "return")
        {
            return TokenType::ReturnKeyword;
        }
        if (characters == "union")
        {
            return TokenType::UnionKeyword;
        }
        if (characters == "const")
        {
            return TokenType::ConstKeyword;
        }
        if (characters == "float")
        {
            return TokenType::FloatKeyword;
        }
        if (characters == "short")
        {
            return TokenType::ShortKeyword;
        }
        if (characters == "unsigned")
        {
            return TokenType::UnsignedKeyword;
        }
        if (characters == "continue")
        {
            return TokenType::ContinueKeyword;
        }
        if (characters == "for")
        {
            return TokenType::ForKeyword;
        }
        if (characters == "signed")
        {
            return TokenType::SignedKeyword;
        }
        if (characters == "default")
        {
            return TokenType::DefaultKeyword;
        }
        if (characters == "goto")
        {
            return TokenType::GotoKeyword;
        }
        if (characters == "sizeof")
        {
            return TokenType::SizeofKeyword;
        }
        if (characters == "volatile")
        {
            return TokenType::VolatileKeyword;
        }
        if (characters == "do")
        {
            return TokenType::DoKeyword;
        }
        if (characters == "if")
        {
            return TokenType::IfKeyword;
        }
        if (characters == "static")
        {
            return TokenType::StaticKeyword;
        }
        if (characters == "while")
        {
            return TokenType::WhileKeyword;
        }
        if (characters == "void")
        {
            return TokenType::VoidKeyword;
        }
        if (characters == "restrict")
        {
            return TokenType::RestrictKeyword;
        }
        if (characters == "inline")
        {
            return TokenType::InlineKeyword;
        }
        throw std::runtime_error("Invalid keyword " + characters);
    }

    OpenCL::Lexer::Token charactersToNumber(std::string lastText, std::uint64_t line, std::uint64_t column)
    {
        if (!(lastText.find('.') != std::string::npos || (lastText.size() >= 2 && lastText.substr(0, 2) == "0x"
            && (lastText.find('e') != std::string::npos || lastText.find('E') != std::string::npos))))
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
                        return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal, number);
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal,
                                                    static_cast<std::uint32_t>(number));
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    std::uint64_t number;
                    ss >> number;
                    return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal, number);
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
                        if (number <= std::numeric_limits<std::uint32_t>::max())
                        {
                            return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal,
                                                        static_cast<std::uint32_t>(number));
                        }
                        else
                        {
                            return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal, number);
                        }
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal,
                                                    static_cast<std::int32_t>(number));
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    std::int64_t number;
                    ss >> number;
                    return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal, number);
                }
                else
                {
                    throw std::runtime_error("Invalid suffix " + suffix);
                }
            }
        }
        else
        {
            if (lastText.back() == 'f' || lastText.back() == 'F')
            {
                std::istringstream ss(lastText.substr(0, lastText.size() - 1));
                float number;
                ss >> number;
                return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal, number);
            }
            else
            {
                std::istringstream ss(lastText);
                double number;
                ss >> number;
                return OpenCL::Lexer::Token(line, column, OpenCL::Lexer::TokenType::Literal, number);
            }
        }
    }

    enum class State
    {
        Start,
        CharacterLiteral,
        StringLiteral,
        Text,
        LineComment,
        BlockComment,
        Ambiguous,
        Number,
    };
} // namespace

std::vector<OpenCL::Lexer::Token> OpenCL::Lexer::tokenize(std::string source)
{
    if (source.back() != ' ')
    {
        source += ' ';
    }
    State currentState = State::Start;
    std::string characters;
    std::regex identifierMatch("[a-zA-Z_]\\w*");
    std::vector<Token> result;
    std::uint64_t line = 1, column = 0;
    bool lastTokenIsAmbiguous = false;

    for (auto iter : source)
    {
        if (!iter)
        {
            continue;
        }
        if (iter == '\n')
        {
            lastTokenIsAmbiguous = false;
            if (currentState == State::LineComment)
            {
                currentState = State::Start;
            }
            line++;
            column = 0;
            continue;
        }
        else
        {
            column++;
        }
        bool handeled;
        do
        {
            handeled = true;
            switch (currentState)
            {
            case State::Start:
            {
                switch (iter)
                {
                case '\'':characters.clear();
                    currentState = State::CharacterLiteral;
                    break;
                case '"':characters.clear();
                    currentState = State::StringLiteral;
                    break;
                case '(': result.emplace_back(line, column, TokenType::OpenParenthese);
                    break;
                case ')': result.emplace_back(line, column, TokenType::CloseParenthese);
                    break;
                case '{': result.emplace_back(line, column, TokenType::OpenBrace);
                    break;
                case '}': result.emplace_back(line, column, TokenType::CloseBrace);
                    break;
                case '[': result.emplace_back(line, column, TokenType::OpenSquareBracket);
                    break;
                case ']': result.emplace_back(line, column, TokenType::CloseSquareBracket);
                    break;
                case ';': result.emplace_back(line, column, TokenType::SemiColon);
                    break;
                case ',': result.emplace_back(line, column, TokenType::Comma);
                    break;
                case ':': result.emplace_back(line, column, TokenType::Colon);
                    break;
                case '?': result.emplace_back(line, column, TokenType::QuestionMark);
                    break;
                case '~':result.emplace_back(line, column, TokenType::BitWiseNegation);
                    lastTokenIsAmbiguous = true;
                    break;
                case '^':result.emplace_back(line, column, TokenType::BitXor);
                    lastTokenIsAmbiguous = true;
                    break;
                case '%':result.emplace_back(line, column, TokenType::Modulo);
                    lastTokenIsAmbiguous = true;
                    break;
                case '!':result.emplace_back(line, column, TokenType::LogicalNegation);
                    lastTokenIsAmbiguous = true;
                    break;
                case ' ': break;
                default:handeled = false;
                    characters.clear();
                    if ((iter >= 'a' && iter <= 'z') || (iter >= 'A' && iter <= 'Z') || (iter == '_'))
                    {
                        currentState = State::Text;
                    }
                    else if ((iter >= '0' && iter <= '9'))
                    {
                        currentState = State::Number;
                    }
                    else
                    {
                        currentState = State::Ambiguous;
                    }
                    break;
                }
                break;
            }
            case State::CharacterLiteral:
            {
                if (iter == '\'' && (characters.empty() || characters.back() != '\\'))
                {
                    currentState = State::Start;
                    result.emplace_back(line, column, TokenType::Literal, charactersToCharLiteral(characters));
                    characters.clear();
                    continue;
                }
                characters += iter;
                break;
            }
            case State::StringLiteral:
            {
                if (iter == '"' && (characters.empty() || characters.back() != '\\'))
                {
                    currentState = State::Start;
                    result.emplace_back(line, column, TokenType::Literal, characters);
                    characters.clear();
                    continue;
                }
                characters += iter;
                break;
            }
            case State::Text:
            {
                if ((iter >= '0' && iter <= '9') || (iter >= 'a' && iter <= 'z') || (iter >= 'A' && iter <= 'Z')
                    || (iter == '_'))
                {
                    characters += iter;
                }
                else
                {
                    if (isKeyword(characters))
                    {
                        result.emplace_back(line, column, charactersToKeyword(characters));
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Identifier, characters);
                    }
                    characters.clear();
                    currentState = State::Start;
                    handeled = false;
                }
                break;
            }
            case State::Number:
            {
                if ((iter >= '0' && iter <= '9') || (iter >= 'a' && iter <= 'f') || (iter >= 'A' && iter <= 'F')
                    || iter == 'x' || iter == '.' || iter == 'u' || iter == 'U' || iter == 'l' || iter == 'L')
                {
                    characters += iter;
                }
                else
                {
                    result.push_back(charactersToNumber(characters, line, column));
                    characters.clear();
                    currentState = State::Start;
                    handeled = false;
                }
                break;
            }
            case State::LineComment: break;
            case State::BlockComment:
            {
                lastTokenIsAmbiguous = false;
                characters += iter;
                if (characters.size() > 2 && characters.back() == '/'
                    && characters.at(characters.size() - 2) == '*')
                {
                    currentState = State::Start;
                }
                break;
            }
            case State::Ambiguous:
            {
                switch (iter)
                {
                case '/':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::Division)
                    {
                        currentState = State::LineComment;
                        result.pop_back();
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Division);
                    }
                    break;
                }
                case '*':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::Division)
                    {
                        currentState = State::BlockComment;
                        result.pop_back();
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Asterisk);
                    }
                    break;
                }
                case '.':
                {
                    if (result.size() > 2 && result.back().getTokenType() == TokenType::Dot
                        && result.at(result.size() - 2).getTokenType() == TokenType::Dot)
                    {
                        result.pop_back();
                        result.pop_back();
                        result.emplace_back(line, column, TokenType::Ellipse);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Dot);
                    }
                    break;
                }
                case '>':
                {
                    if (lastTokenIsAmbiguous)
                    {
                        if (result.back().getTokenType() == TokenType::Negation)
                        {
                            result.pop_back();
                            result.emplace_back(line, column, TokenType::Arrow);
                        }
                        else if (result.back().getTokenType() == TokenType::GreaterThan)
                        {
                            result.pop_back();
                            result.emplace_back(line, column, TokenType::ShiftRight);
                        }
                        else
                        {
                            result.emplace_back(line, column, TokenType::GreaterThan);
                        }
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::GreaterThan);
                    }
                    break;
                }
                case '<':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::LessThan)
                    {
                        result.pop_back();
                        result.emplace_back(line, column, TokenType::ShiftLeft);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::LessThan);
                    }
                    break;
                }
                case '&':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::Ampersand)
                    {
                        result.pop_back();
                        result.emplace_back(line, column, TokenType::LogicAnd);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Ampersand);
                    }
                    break;
                }
                case '|':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::BitOr)
                    {
                        result.pop_back();
                        result.emplace_back(line, column, TokenType::LogicOr);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::BitOr);
                    }
                    break;
                }
                case '+':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::Addition)
                    {
                        result.pop_back();
                        result.emplace_back(line, column, TokenType::Increment);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Addition);
                    }
                    break;
                }
                case '-':
                {
                    if (lastTokenIsAmbiguous && result.back().getTokenType() == TokenType::Negation)
                    {
                        result.pop_back();
                        result.emplace_back(line, column, TokenType::Decrement);
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Negation);
                    }
                    break;
                }
                case '=':
                {
                    if (lastTokenIsAmbiguous)
                    {
                        switch (result.back().getTokenType())
                        {
                        case TokenType::Assignment:result.pop_back();
                            result.emplace_back(line, column, TokenType::Equal);
                            break;
                        case TokenType::LogicalNegation:result.pop_back();
                            result.emplace_back(line, column, TokenType::NotEqual);
                            break;
                        case TokenType::GreaterThan:result.pop_back();
                            result.emplace_back(line, column, TokenType::GreaterThanOrEqual);
                            break;
                        case TokenType::LessThan:result.pop_back();
                            result.emplace_back(line, column, TokenType::LessThanOrEqual);
                            break;
                        case TokenType::Addition:result.pop_back();
                            result.emplace_back(line, column, TokenType::PlusAssign);
                            break;
                        case TokenType::Negation:result.pop_back();
                            result.emplace_back(line, column, TokenType::MinusAssign);
                            break;
                        case TokenType::Division:result.pop_back();
                            result.emplace_back(line, column, TokenType::DivideAssign);
                            break;
                        case TokenType::Asterisk:result.pop_back();
                            result.emplace_back(line, column, TokenType::MultiplyAssign);
                            break;
                        case TokenType::Modulo:result.pop_back();
                            result.emplace_back(line, column, TokenType::ModuloAssign);
                            break;
                        case TokenType::Ampersand:result.pop_back();
                            result.emplace_back(line, column, TokenType::BitAndAssign);
                            break;
                        case TokenType::BitOr:result.pop_back();
                            result.emplace_back(line, column, TokenType::BitOrAssign);
                            break;
                        case TokenType::BitXor:result.pop_back();
                            result.emplace_back(line, column, TokenType::BitXorAssign);
                            break;
                        case TokenType::ShiftLeft:result.pop_back();
                            result.emplace_back(line, column, TokenType::ShiftLeftAssign);
                            break;
                        case TokenType::ShiftRight:result.pop_back();
                            result.emplace_back(line, column, TokenType::ShiftRightAssign);
                            break;
                        default: result.emplace_back(line, column, TokenType::Assignment);
                            break;
                        }
                    }
                    else
                    {
                        result.emplace_back(line, column, TokenType::Assignment);
                    }
                    break;
                }
                default:lastTokenIsAmbiguous = false;
                    if (result.back().getTokenType() == TokenType::Dot && iter >= '0' && iter <= '9')
                    {
                        result.pop_back();
                        characters += '.';
                        currentState = State::Number;
                    }
                    else
                    {
                        currentState = State::Start;
                    }
                    handeled = false;
                    break;
                }
                if (handeled)
                {
                    lastTokenIsAmbiguous = true;
                }
                break;
            }
            }
        }
        while (!handeled);
    }

    return result;
}

std::string OpenCL::Lexer::Token::emitBack() const
{
    switch (getTokenType())
    {
    case TokenType::Identifier: return std::get<std::string>(getValue());
    case TokenType::OpenParenthese: return "(";
    case TokenType::CloseParenthese: return ")";
    case TokenType::OpenBrace: return "{";
    case TokenType::CloseBrace: return "}";
    case TokenType::Literal:
        return std::visit(
            [](auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<std::string, T>)
                {
                    return value;
                }
                else if constexpr (!std::is_same_v<std::monostate, T>)
                {
                    std::ostringstream ss;
                    ss << value;
                    return ss.str();
                }
                else
                {
                    return "";
                }
            },
            getValue());
    case TokenType::SemiColon: return ";";
    case TokenType::Comma: return ",";
    case TokenType::Negation: return "-";
    case TokenType::BitWiseNegation: return "~";
    case TokenType::LogicalNegation: return "!";
    case TokenType::Addition: return "+";
    case TokenType::Asterisk: return "*";
    case TokenType::Division: return "/";
    case TokenType::Modulo: return "%";
    case TokenType::LogicAnd: return "&&";
    case TokenType::LogicOr: return "||";
    case TokenType::Ampersand: return "&";
    case TokenType::BitOr: return "|";
    case TokenType::BitXor: return "^";
    case TokenType::Equal: return "==";
    case TokenType::NotEqual: return "!=";
    case TokenType::LessThan: return "<";
    case TokenType::LessThanOrEqual: return "<=";
    case TokenType::GreaterThan: return ">";
    case TokenType::GreaterThanOrEqual: return ">=";
    case TokenType::Assignment: return "=";
    case TokenType::PlusAssign: return "+=";
    case TokenType::MinusAssign: return "-=";
    case TokenType::DivideAssign: return "/=";
    case TokenType::MultiplyAssign: return "*=";
    case TokenType::ModuloAssign: return "%=";
    case TokenType::ShiftLeftAssign: return "<<=";
    case TokenType::ShiftRightAssign: return ">>=";
    case TokenType::BitAndAssign: return "&=";
    case TokenType::BitOrAssign: return "|=";
    case TokenType::BitXorAssign: return "^=";
    case TokenType::ShiftRight: return ">>";
    case TokenType::ShiftLeft: return "<<";
    case TokenType::Increment: return "++";
    case TokenType::Decrement: return "--";
    case TokenType::Colon: return ":";
    case TokenType::QuestionMark: return "?";
    case TokenType::VoidKeyword: return "void";
    case TokenType::CharKeyword: return "char";
    case TokenType::ShortKeyword: return "short";
    case TokenType::IntKeyword: return "int";
    case TokenType::LongKeyword: return "long";
    case TokenType::FloatKeyword: return "float";
    case TokenType::DoubleKeyword: return "double";
    case TokenType::SignedKeyword: return "signed";
    case TokenType::UnsignedKeyword: return "unsigned";
    case TokenType::TypedefKeyword: return "typedef";
    case TokenType::ExternKeyword: return "extern";
    case TokenType::StaticKeyword: return "static";
    case TokenType::AutoKeyword: return "auto";
    case TokenType::RegisterKeyword: return "register";
    case TokenType::ConstKeyword: return "const";
    case TokenType::SizeofKeyword: return "sizeof";
    case TokenType::ReturnKeyword: return "return";
    case TokenType::BreakKeyword: return "break";
    case TokenType::ContinueKeyword: return "continue";
    case TokenType::DoKeyword: return "do";
    case TokenType::ElseKeyword: return "else";
    case TokenType::ForKeyword: return "for";
    case TokenType::IfKeyword: return "if";
    case TokenType::WhileKeyword: return "while";
    case TokenType::OpenSquareBracket: return "[";
    case TokenType::CloseSquareBracket: return "]";
    case TokenType::StructKeyword: return "struct";
    case TokenType::Dot: return ".";
    case TokenType::Arrow: return "->";
    case TokenType::SwitchKeyword: return "switch";
    case TokenType::CaseKeyword: return "case";
    case TokenType::DefaultKeyword: return "default";
    case TokenType::UnionKeyword: return "union";
    case TokenType::VolatileKeyword: return "volatile";
    case TokenType::EnumKeyword: return "enum";
    case TokenType::GotoKeyword: return "goto";
    case TokenType::Ellipse: return "...";
    case TokenType::RestrictKeyword: return "restrict";
    case TokenType::InlineKeyword: return "inline";
    }
    return "";
}

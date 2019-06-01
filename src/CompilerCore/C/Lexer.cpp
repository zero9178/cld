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
        if (characters == "\\\"")
        {
            return '"';
        }
        if (characters == "\\?")
        {
            return '\?';
        }
        if (characters == "\\\\")
        {
            return '\\';
        }
        if (characters == "\\a")
        {
            return '\a';
        }
        if (characters == "\\b")
        {
            return '\b';
        }
        if (characters == "\\f")
        {
            return '\f';
        }
        if (characters == "\\n")
        {
            return '\n';
        }
        if (characters == "\\r")
        {
            return '\r';
        }
        if (characters == "\\t")
        {
            return '\t';
        }
        if (characters == "\\v")
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

    OpenCL::Lexer::Token charactersToNumber(const std::string& lastText, std::uint64_t line, std::uint64_t column)
    {
        if (lastText.find('.') == std::string::npos
            && ((lastText.size() >= 2 && (lastText.substr(0, 2) == "0x" || lastText.substr(0, 2) == "0X"))
                || (lastText.find('e') == std::string::npos && lastText.find('E') == std::string::npos))
            && lastText.find('p') == std::string::npos && lastText.find('P') == std::string::npos)
        {
            static std::regex numbers("(0x)?[0-9a-fA-F]+");
            std::smatch match;
            std::regex_search(lastText, match, numbers);
            std::string filtered = match[0];

            std::string suffix = lastText.substr(filtered.size(), lastText.size() - filtered.size());
            if (std::any_of(suffix.begin(), suffix.end(), [](char c)
            { return c == 'u'; }))
            {
                auto erase = std::remove(suffix.begin(), suffix.end(), 'u');
                suffix.erase(erase, suffix.end());

                char* endptr = nullptr;
                std::uint64_t number = std::strtoull(filtered.c_str(), &endptr, 0);
                if (endptr != filtered.c_str() + filtered.size())
                {
                    throw std::runtime_error("Invalid constant " + filtered);
                }
                if (suffix.empty() || suffix == "l" || suffix == "L")
                {
                    if (number > std::numeric_limits<std::uint32_t>::max())
                    {
                        return OpenCL::Lexer::Token(line,
                                                    column,
                                                    lastText.size(),
                                                    OpenCL::Lexer::TokenType::Literal,
                                                    number,
                                                    lastText);
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, lastText.size(), OpenCL::Lexer::TokenType::Literal,
                                                    static_cast<std::uint32_t>(number), lastText);
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    return OpenCL::Lexer::Token(line, column,
                                                lastText.size(), OpenCL::Lexer::TokenType::Literal, number, lastText);
                }
                else
                {
                    throw std::runtime_error("Invalid suffix " + suffix);
                }
            }
            else
            {
                char* endptr = nullptr;
                std::int64_t number = std::strtoull(filtered.c_str(), &endptr, 0);
                if (endptr != filtered.c_str() + filtered.size())
                {
                    throw std::runtime_error("Invalid constant " + filtered);
                }
                if (suffix.empty() || suffix == "l" || suffix == "L")
                {
                    if (number > std::numeric_limits<std::int32_t>::max())
                    {
                        if (number <= std::numeric_limits<std::uint32_t>::max())
                        {
                            return OpenCL::Lexer::Token(line,
                                                        column,
                                                        lastText.size(),
                                                        OpenCL::Lexer::TokenType::Literal,
                                                        static_cast<std::uint32_t>(number),
                                                        lastText);
                        }
                        else
                        {
                            return OpenCL::Lexer::Token(line,
                                                        column,
                                                        lastText.size(),
                                                        OpenCL::Lexer::TokenType::Literal,
                                                        number,
                                                        lastText);
                        }
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, lastText.size(), OpenCL::Lexer::TokenType::Literal,
                                                    static_cast<std::int32_t>(number), lastText);
                    }
                }
                else if (suffix == "ll" || suffix == "LL")
                {
                    return OpenCL::Lexer::Token(line, column,
                                                lastText.size(), OpenCL::Lexer::TokenType::Literal, number, lastText);
                }
                else
                {
                    throw std::runtime_error("Invalid suffix " + suffix);
                }
            }
        }
        else
        {
            char* endptr = nullptr;
            if (lastText.back() == 'f' || lastText.back() == 'F')
            {
                auto filtered = lastText.substr(0, lastText.size() - 1);
                float number = std::strtof(filtered.c_str(), &endptr);
                if (endptr != filtered.c_str() + filtered.size())
                {
                    throw std::runtime_error("Invalid floating point constant " + lastText);
                }
                return OpenCL::Lexer::Token(line, column,
                                            lastText.size(), OpenCL::Lexer::TokenType::Literal, number, lastText);
            }
            else
            {
                double number = std::strtod(lastText.c_str(), &endptr);
                if (endptr != lastText.c_str() + lastText.size())
                {
                    throw std::runtime_error("Invalid floating point constant " + lastText);
                }
                return OpenCL::Lexer::Token(line, column,
                                            lastText.size(), OpenCL::Lexer::TokenType::Literal, number, lastText);
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
                case '(': result.emplace_back(line, column, 1, TokenType::OpenParenthese);
                    break;
                case ')': result.emplace_back(line, column, 1, TokenType::CloseParenthese);
                    break;
                case '{': result.emplace_back(line, column, 1, TokenType::OpenBrace);
                    break;
                case '}': result.emplace_back(line, column, 1, TokenType::CloseBrace);
                    break;
                case '[': result.emplace_back(line, column, 1, TokenType::OpenSquareBracket);
                    break;
                case ']': result.emplace_back(line, column, 1, TokenType::CloseSquareBracket);
                    break;
                case ';': result.emplace_back(line, column, 1, TokenType::SemiColon);
                    break;
                case ',': result.emplace_back(line, column, 1, TokenType::Comma);
                    break;
                case ':': result.emplace_back(line, column, 1, TokenType::Colon);
                    break;
                case '?': result.emplace_back(line, column, 1, TokenType::QuestionMark);
                    break;
                case '~':result.emplace_back(line, column, 1, TokenType::BitWiseNegation);
                    lastTokenIsAmbiguous = true;
                    break;
                case '^':result.emplace_back(line, column, 1, TokenType::BitXor);
                    lastTokenIsAmbiguous = true;
                    break;
                case '%':result.emplace_back(line, column, 1, TokenType::Modulo);
                    lastTokenIsAmbiguous = true;
                    break;
                case '!':result.emplace_back(line, column, 1, TokenType::LogicalNegation);
                    lastTokenIsAmbiguous = true;
                    break;
                default:
                    if (std::isspace(iter))
                    {
                        break;
                    }
                    handeled = false;
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
                    result.emplace_back(line,
                                        column - characters.size() - 1,
                                        characters.size() + 2,
                                        TokenType::Literal,
                                        charactersToCharLiteral(characters), '\'' + characters + '\'');
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
                    auto csize = characters.size();
                    currentState = State::Start;
                    {
                        static std::regex escapes(R"(\\([0-7]{1,3}|x[0-9a-fA-F]+|.))");
                        std::smatch matches;
                        auto start = characters.cbegin();
                        while (start < characters.cend()
                            && std::regex_search(start, characters.cend(), matches, escapes))
                        {
                            auto size = matches.suffix().first - characters.cbegin()
                                - (matches[0].second - matches[0].first) + 1;
                            characters = std::string(characters.cbegin(), matches[0].first) +
                                static_cast<char>(charactersToCharLiteral({matches[0].first, matches[0].second}))
                                + std::string(matches[0].second, characters.cend());
                            start = characters.cbegin() + size;
                        }
                    }
                    result.emplace_back(line,
                                        column - 1 - csize,
                                        2 + csize,
                                        TokenType::StringLiteral,
                                        characters,
                                        '\"' + characters + '\"');
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
                        result.emplace_back(line,
                                            column - characters.size(),
                                            characters.size(),
                                            charactersToKeyword(characters));
                    }
                    else
                    {
                        result.emplace_back(line,
                                            column - characters.size(),
                                            characters.size(),
                                            TokenType::Identifier,
                                            characters, characters);
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
                    || iter == 'x' || iter == 'X' || iter == '.' || iter == 'u' || iter == 'U' || iter == 'l'
                    || iter == 'L')
                {
                    characters += iter;
                }
                else if ((iter == '+' || iter == '-') && !characters.empty()
                    && (characters.back() == 'e' || characters.back() == 'E' || characters.back() == 'p'
                        || characters.back() == 'P'))
                {
                    characters += iter;
                }
                else
                {
                    result.push_back(charactersToNumber(characters, line, column - characters.size()));
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
                if (characters.size() > 3 && characters.back() == '/'
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
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::Division)
                    {
                        currentState = State::LineComment;
                        result.pop_back();
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Division);
                    }
                    break;
                }
                case '*':
                {
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::Division)
                    {
                        currentState = State::BlockComment;
                        result.pop_back();
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Asterisk);
                    }
                    break;
                }
                case '.':
                {
                    if (lastTokenIsAmbiguous && result.size() > 2 && result.back().getTokenType() == TokenType::Dot
                        && result.at(result.size() - 2).getTokenType() == TokenType::Dot
                        && result.back().getColumn() - 1 == result.at(result.size() - 2).getColumn())
                    {
                        result.pop_back();
                        result.pop_back();
                        result.emplace_back(line, column - 2, 3, TokenType::Ellipse);
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Dot);
                    }
                    break;
                }
                case '>':
                {
                    if (lastTokenIsAmbiguous)
                    {
                        if (!result.empty() && result.back().getTokenType() == TokenType::Minus)
                        {
                            result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::Arrow);
                        }
                        else if (!result.empty() && result.back().getTokenType() == TokenType::GreaterThan)
                        {
                            result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::ShiftRight);
                        }
                        else
                        {
                            result.emplace_back(line, column, 1, TokenType::GreaterThan);
                        }
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::GreaterThan);
                    }
                    break;
                }
                case '<':
                {
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::LessThan)
                    {
                        result.pop_back();
                        result.emplace_back(line, column - 1, 2, TokenType::ShiftLeft);
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::LessThan);
                    }
                    break;
                }
                case '&':
                {
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::Ampersand)
                    {
                        result.pop_back();
                        result.emplace_back(line, column - 1, 2, TokenType::LogicAnd);
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Ampersand);
                    }
                    break;
                }
                case '|':
                {
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::BitOr)
                    {
                        result.pop_back();
                        result.emplace_back(line, column - 1, 2, TokenType::LogicOr);
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::BitOr);
                    }
                    break;
                }
                case '+':
                {
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::Plus)
                    {
                        result.pop_back();
                        result.emplace_back(line, column - 1, 2, TokenType::Increment);
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Plus);
                    }
                    break;
                }
                case '-':
                {
                    if (lastTokenIsAmbiguous && !result.empty() && result.back().getTokenType() == TokenType::Minus)
                    {
                        result.pop_back();
                        result.emplace_back(line, column - 1, 2, TokenType::Decrement);
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Minus);
                    }
                    break;
                }
                case '=':
                {
                    if (!result.empty() && lastTokenIsAmbiguous)
                    {
                        switch (result.back().getTokenType())
                        {
                        case TokenType::Assignment:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::Equal);
                            break;
                        case TokenType::LogicalNegation:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::NotEqual);
                            break;
                        case TokenType::GreaterThan:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::GreaterThanOrEqual);
                            break;
                        case TokenType::LessThan:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::LessThanOrEqual);
                            break;
                        case TokenType::Plus:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::PlusAssign);
                            break;
                        case TokenType::Minus:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::MinusAssign);
                            break;
                        case TokenType::Division:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::DivideAssign);
                            break;
                        case TokenType::Asterisk:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::MultiplyAssign);
                            break;
                        case TokenType::Modulo:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::ModuloAssign);
                            break;
                        case TokenType::Ampersand:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::BitAndAssign);
                            break;
                        case TokenType::BitOr:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::BitOrAssign);
                            break;
                        case TokenType::BitXor:result.pop_back();
                            result.emplace_back(line, column - 1, 2, TokenType::BitXorAssign);
                            break;
                        case TokenType::ShiftLeft:result.pop_back();
                            result.emplace_back(line, column - 2, 3, TokenType::ShiftLeftAssign);
                            break;
                        case TokenType::ShiftRight:result.pop_back();
                            result.emplace_back(line, column - 2, 3, TokenType::ShiftRightAssign);
                            break;
                        default: result.emplace_back(line, column, 1, TokenType::Assignment);
                            break;
                        }
                    }
                    else
                    {
                        result.emplace_back(line, column, 1, TokenType::Assignment);
                    }
                    break;
                }
                default:lastTokenIsAmbiguous = false;
                    if (!result.empty() && result.back().getTokenType() == TokenType::Dot && iter >= '0' && iter <= '9')
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
        if (iter == '\n')
        {
            lastTokenIsAmbiguous = false;
            if (currentState == State::LineComment)
            {
                currentState = State::Start;
            }
            else if (currentState == State::CharacterLiteral)
            {
                throw std::runtime_error("Newline in character literal, use \\n instead");
            }
            else if (currentState == State::StringLiteral)
            {
                throw std::runtime_error("Newline in string literal, use \\n instead");
            }
            line++;
            column = 0;
            continue;
        }
        else if (iter == '\t')
        {
            column += 4;
        }
        else
        {
            column++;
        }
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
    case TokenType::StringLiteral:
    case TokenType::Literal:return m_valueRepresentation;
    case TokenType::SemiColon: return ";";
    case TokenType::Comma: return ",";
    case TokenType::Minus: return "-";
    case TokenType::BitWiseNegation: return "~";
    case TokenType::LogicalNegation: return "!";
    case TokenType::Plus: return "+";
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

std::uint64_t OpenCL::Lexer::Token::getLength() const
{
    return m_length;
}

std::string OpenCL::Lexer::reconstruct(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                       std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    std::string result;
    for (auto curr = begin; curr != end; curr++)
    {
        if (curr != begin)
        {
            auto prev = curr - 1;
            if (curr->getLine() == prev->getLine())
            {
                result += std::string(curr->getColumn() - (prev->getColumn() + prev->getLength()), ' ');
            }
            else
            {
                result += '\n' + std::string(curr->getColumn(), ' ');
            }
        }
        result += curr->emitBack();
    }
    return result;
}

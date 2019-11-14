#include "Lexer.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <regex>
#include <sstream>
#include <unordered_map>

#include "ErrorMessages.hpp"
#include "SourceObject.hpp"

#define WIN32_LEAN_AND_MEAN
#ifdef NOMINMAX
    #undef NOMINMAX
#endif
#define NOMINMAX
#include "termcolor.hpp"

namespace
{
    enum class HighlightEffect
    {
        Underline,
        PointAtBeginning,
        PointAtEnd,
        InsertAtEnd,
    };

    void reportError(std::ostream* reporter, const std::string& message, std::uint64_t line, std::uint64_t column,
                     const std::string& lineText,
                     std::optional<std::pair<std::uint64_t, std::uint64_t>> highLightRange = {},
                     HighlightEffect highlightEffect = HighlightEffect::Underline)
    {
        if (!reporter)
        {
            return;
        }
#ifdef NDEBUG
        auto normalColour = termcolor::white;
#else
        auto normalColour = termcolor::grey;
#endif
        auto lineNumberText = std::to_string(line);
        *reporter << normalColour << lineNumberText << ':' << column << ": " << termcolor::red
                  << "error: " << normalColour << message << '\n';
        auto numSize = lineNumberText.size();
        auto remainder = numSize % 4;
        if (remainder)
        {
            numSize += 4 - remainder;
        }
        *reporter << normalColour << std::string(numSize - lineNumberText.size(), ' ') << lineNumberText << '|';
        if (!highLightRange || highLightRange->first == highLightRange->second)
        {
            *reporter << lineText << std::endl;
            return;
        }
        if (highlightEffect != HighlightEffect::InsertAtEnd && highLightRange->first > highLightRange->second)
        {
            std::cerr << "Highlight column range start greater than end" << std::endl;
            std::terminate();
        }
        if (highlightEffect != HighlightEffect::InsertAtEnd && highLightRange->second > lineText.size())
        {
            std::cerr << "Highlight column range end greater than line size" << std::endl;
            std::terminate();
        }
        if (highlightEffect != HighlightEffect::InsertAtEnd)
        {
            *reporter << lineText.substr(0, highLightRange->first) << termcolor::red
                      << lineText.substr(highLightRange->first, highLightRange->second - highLightRange->first)
                      << normalColour << lineText.substr(highLightRange->second) << '\n';
        }
        else
        {
            *reporter << lineText << '\n';
        }
        *reporter << std::string(numSize, ' ') << '|' << std::string(highLightRange->first, ' ') << termcolor::red;
        switch (highlightEffect)
        {
            case HighlightEffect::Underline:
                *reporter << std::string(highLightRange->second - highLightRange->first, '~');
                break;
            case HighlightEffect::PointAtBeginning:
                *reporter << '^' << std::string(highLightRange->second - highLightRange->first - 1, '~');
                break;
            case HighlightEffect::PointAtEnd:
                *reporter << std::string(highLightRange->second - highLightRange->first - 1, '~') << '^';
                break;
            case HighlightEffect::InsertAtEnd:
                *reporter << std::string(lineText.empty() ? 0 : lineText.size() - 1, ' ') << '^';
                break;
        }
        *reporter << normalColour << std::endl;
    }

    std::int32_t charactersToCharLiteral(std::ostream* reporter, const std::string& characters, std::uint64_t line,
                                         std::uint64_t column, const std::string& lineText)
    {
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
            std::int32_t number = 0;
            if (characters[1] == 'x')
            {
                if (characters.size() <= 2)
                {
                    reportError(reporter, OpenCL::ErrorMessages::Lexer::AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED, line,
                                column - characters.size() + 1, lineText,
                                {{column - characters.size() - 1, column - 1}});
                    return 0;
                }
                std::istringstream ss(characters.substr(2, characters.size() - 1));
                if (!(ss >> std::hex >> number))
                {
                    reportError(reporter, OpenCL::ErrorMessages::Lexer::INVALID_HEXADECIMAL_CHARACTER.args(ss.str()),
                                line, column - characters.size() + 1, lineText,
                                {{column - characters.size() - 1, column - 1}});
                }
            }
            else if (std::isdigit(characters[1]))
            {
                std::istringstream ss(characters.substr(1, characters.size() - 1));
                if (!(ss >> std::oct >> number))
                {
                    reportError(reporter, OpenCL::ErrorMessages::Lexer::INVALID_OCTAL_CHARACTER.args(ss.str()), line,
                                column - characters.size() + 1, lineText,
                                {{column - characters.size() - 1, column - 1}});
                }
            }
            else
            {
                reportError(reporter, OpenCL::ErrorMessages::Lexer::INCORRECT_CHARACTER_LITERAL.args(characters), line,
                            column - characters.size() - 2, lineText, {{column - characters.size() - 1, column}});
                return 0;
            }
            if (number > std::numeric_limits<std::uint8_t>::max())
            {
                reportError(
                    reporter,
                    OpenCL::ErrorMessages::Lexer::CHARACTER_MUSTNT_HAVE_HIGHER_VALUE_THAN_MAXIMUM_VALUE_OF_UCHAR, line,
                    column - characters.size() - 1, lineText, {{column - characters.size() - 1, column}});
            }
            return number;
        }
        reportError(reporter, OpenCL::ErrorMessages::Lexer::INCORRECT_CHARACTER_LITERAL.args(characters), line,
                    column - characters.size() - 1, lineText, {{column - characters.size() - 1, column}});
        return 0;
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
               || characters == "while" || characters == "inline" || characters == "_Bool";
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
        if (characters == "_Bool")
        {
            return TokenType::UnderlineBool;
        }
        OPENCL_UNREACHABLE;
    }

    OpenCL::Lexer::Token charactersToNumber(std::ostream* reporter, const std::string& literal, std::uint64_t line,
                                            std::uint64_t column, const std::string& lineText, bool isPreprocessor)
    {
        if (literal.find('.') == std::string::npos
            && ((literal.size() >= 2 && (literal.substr(0, 2) == "0x" || literal.substr(0, 2) == "0X"))
                || (literal.find('e') == std::string::npos && literal.find('E') == std::string::npos))
            && literal.find('p') == std::string::npos && literal.find('P') == std::string::npos)
        {
            static std::regex numbers("(0x)?[0-9a-fA-F]+");
            bool isHexOrOctal = literal[0] == '0';
            std::smatch match;
            std::regex_search(literal, match, numbers);
            std::string filtered = match[0];

            std::string suffix = literal.substr(filtered.size(), literal.size() - filtered.size());
            auto originalSuffix = suffix;
            if (std::any_of(suffix.begin(), suffix.end(), [](char c) { return c == 'u'; }))
            {
                auto erase = std::remove(suffix.begin(), suffix.end(), 'u');
                suffix.erase(erase, suffix.end());

                char* endptr = nullptr;
                std::uint64_t number = std::strtoull(filtered.c_str(), &endptr, 0);
                if (isPreprocessor || suffix == "ll" || suffix == "LL")
                {
                    return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                literal, number);
                }
                else
                {
                    if (!suffix.empty() && suffix != "l" && suffix != "L")
                    {
                        reportError(reporter,
                                    OpenCL::ErrorMessages::Lexer::INVALID_INTEGER_LITERAL_SUFFIX.args(originalSuffix),
                                    line, column, lineText,
                                    {{column + literal.size() - originalSuffix.size(), column + literal.size()}});
                    }
                    if (number > std::numeric_limits<std::uint32_t>::max())
                    {
                        return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                    literal, number);
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                    literal, static_cast<std::uint32_t>(number));
                    }
                }
            }
            else
            {
                char* endptr = nullptr;
                std::uint64_t number = std::strtoull(filtered.c_str(), &endptr, 0);
                if (isPreprocessor || suffix == "ll" || suffix == "LL")
                {
                    if (isHexOrOctal && number > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
                    {
                        return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                    literal, number);
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                    literal, static_cast<std::int64_t>(number));
                    }
                }
                else
                {
                    if (!suffix.empty() && suffix != "l" && suffix != "L")
                    {
                        reportError(reporter,
                                    OpenCL::ErrorMessages::Lexer::INVALID_INTEGER_LITERAL_SUFFIX.args(originalSuffix),
                                    line, column, lineText,
                                    {{column + literal.size() - originalSuffix.size(), column + literal.size()}});
                    }
                    if (number > static_cast<std::uint64_t>(std::numeric_limits<std::int32_t>::max()))
                    {
                        if (isHexOrOctal && number <= std::numeric_limits<std::uint32_t>::max())
                        {
                            return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                        literal, static_cast<std::uint32_t>(number));
                        }
                        else if (isHexOrOctal
                                 && number > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
                        {
                            return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                        literal, number);
                        }
                        else
                        {
                            return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                        literal, static_cast<std::int64_t>(number));
                        }
                    }
                    else
                    {
                        return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal,
                                                    literal, static_cast<std::int32_t>(number));
                    }
                }
            }
        }
        else
        {
            char* endptr = nullptr;
            if (literal.back() == 'f' || literal.back() == 'F')
            {
                auto filtered = literal.substr(0, literal.size() - 1);
                float number = std::strtof(filtered.c_str(), &endptr);
                if (endptr != filtered.c_str() + filtered.size())
                {
                    reportError(reporter, OpenCL::ErrorMessages::Lexer::INVALID_FLOATING_POINT_LITERAL.args(literal),
                                line, column, lineText, {{column, column + literal.size()}});
                }
                return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal, literal,
                                            number);
            }
            else
            {
                if (literal.size() >= 2 && literal[0] == '0' && std::tolower(literal[1]) == 'x'
                    && std::none_of(literal.begin(), literal.end(), [](char c) { return c == 'p' || c == 'P'; }))
                {
                    reportError(reporter, OpenCL::ErrorMessages::Lexer::BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT,
                                line, column, lineText, {{column, column + literal.size()}});
                }
                double number = std::strtod(literal.c_str(), &endptr);
                if (endptr != literal.c_str() + literal.size())
                {
                    reportError(reporter, OpenCL::ErrorMessages::Lexer::INVALID_FLOATING_POINT_LITERAL.args(literal),
                                line, column, lineText, {{column, column + literal.size()}});
                }
                return OpenCL::Lexer::Token(line, column, literal.size(), OpenCL::Lexer::TokenType::Literal, literal,
                                            number);
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
        AfterInclude,
        L,
    };
} // namespace

OpenCL::SourceObject OpenCL::Lexer::tokenize(std::string source, Language language, std::ostream* reporter)
{
    if (source.empty() || source.back() != ' ')
    {
        source += '\n';
    }
    State currentState = State::Start;
    std::string characters;
    static std::regex identifierMatch("[a-zA-Z_]\\w*");
    std::vector<Token> result;
    std::pair<std::uint64_t, std::uint64_t> blockCommentStart;
    std::uint64_t line = 1, column = 0;
    bool lastTokenIsAmbiguous = false;
    bool wide = false;
    auto lineMap = [&source]() -> std::unordered_map<std::uint64_t, std::string> {
        std::unordered_map<std::uint64_t, std::string> result;
        std::stringstream ss(source);
        while (std::getline(ss, result[result.size() + 1], '\n'))
            ;
        return result;
    }();

    for (auto iter : source)
    {
        bool handled;
        do
        {
            handled = true;
            switch (currentState)
            {
                case State::Start:
                {
                    switch (iter)
                    {
                        case '\'':
                            characters.clear();
                            currentState = State::CharacterLiteral;
                            break;
                        case '"':
                            characters.clear();
                            currentState = State::StringLiteral;
                            break;
                        case '(': result.emplace_back(line, column, 1, TokenType::OpenParentheses, "("); break;
                        case ')': result.emplace_back(line, column, 1, TokenType::CloseParentheses, ")"); break;
                        case '{': result.emplace_back(line, column, 1, TokenType::OpenBrace, "{"); break;
                        case '}': result.emplace_back(line, column, 1, TokenType::CloseBrace, "}"); break;
                        case '[': result.emplace_back(line, column, 1, TokenType::OpenSquareBracket, "["); break;
                        case ']': result.emplace_back(line, column, 1, TokenType::CloseSquareBracket, "]"); break;
                        case ';': result.emplace_back(line, column, 1, TokenType::SemiColon, ";"); break;
                        case ',': result.emplace_back(line, column, 1, TokenType::Comma, ","); break;
                        case '?': result.emplace_back(line, column, 1, TokenType::QuestionMark, "?"); break;
                        case '~':
                            result.emplace_back(line, column, 1, TokenType::BitWiseNegation, "~");
                            lastTokenIsAmbiguous = true;
                            break;
                        case '^':
                            result.emplace_back(line, column, 1, TokenType::BitXor, "^");
                            lastTokenIsAmbiguous = true;
                            break;
                        case '!':
                            result.emplace_back(line, column, 1, TokenType::LogicalNegation, "!");
                            lastTokenIsAmbiguous = true;
                            break;
                        case 'L':
                            characters.clear();
                            characters += 'L';
                            currentState = State::L;
                            break;
                        default:
                            if (iter > 0 && std::isspace(iter)) // Its UB to pass negative value to std::isspace. TIL
                            {
                                lastTokenIsAmbiguous = false;
                                break;
                            }
                            handled = false;
                            characters.clear();
                            if ((iter >= 'a' && iter <= 'z') || (iter >= 'A' && iter <= 'Z') || (iter == '_'))
                            {
                                currentState = State::Text;
                            }
                            else if ((iter >= '0' && iter <= '9'))
                            {
                                currentState = State::Number;
                            }
                            else if (iter >= ' ' && iter < '~' && iter != '`' && iter != '`' && iter != '@'
                                     && iter != '$' && iter != '\\')
                            {
                                currentState = State::Ambiguous;
                            }
                            else
                            {
                                reportError(reporter,
                                            ErrorMessages::Lexer::UNEXPECTED_CHARACTER.args(std::to_string(iter)), line,
                                            column, lineMap[line], std::pair{column, column + 1});
                                handled = true;
                            }
                            break;
                    }
                    break;
                }
                case State::L:
                {
                    if (iter == '\'' || iter == '"')
                    {
                        currentState = State::Start;
                        wide = true;
                    }
                    else
                    {
                        currentState = State::Text;
                    }
                    handled = false;
                    break;
                }
                case State::CharacterLiteral:
                {
                    if (iter == '\'' && (characters.empty() || characters.back() != '\\'))
                    {
                        currentState = State::Start;
                        if (std::none_of(characters.begin(), characters.end(), [](char c) { return c == '\n'; }))
                        {
                            std::int32_t characterValue =
                                charactersToCharLiteral(reporter, characters, line, column + 1, lineMap[line]);
                            if (language != Language::Preprocessor)
                            {
                                if (wide)
                                {
                                    wide = false;
                                    wchar_t wideChar;
                                    std::uint8_t charSize = 0;
                                    std::array<char, 4> chars;
                                    while (characterValue != 0)
                                    {
                                        chars[charSize++] = characterValue & 0xFF;
                                        characterValue >>= 8;
                                    }
                                    std::mbtowc(&wideChar, chars.data(), charSize);
                                    result.emplace_back(
                                        line, column - characters.size() - 2, characters.size() + 3, TokenType::Literal,
                                        "L\'" + characters + '\'',
                                        /*TODO: Target dependent size */ static_cast<std::int32_t>(wideChar));
                                }
                                else
                                {
                                    result.emplace_back(line, column - characters.size() - 1, characters.size() + 2,
                                                        TokenType::Literal, '\'' + characters + '\'', characterValue);
                                }
                            }
                            else
                            {
                                result.emplace_back(line, column - characters.size() - 1, characters.size() + 2,
                                                    TokenType::Literal, '\'' + characters + '\'',
                                                    static_cast<std::int64_t>(characterValue));
                            }
                        }
                        characters.clear();
                        continue;
                    }
                    if (iter == '\n')
                    {
                        reportError(reporter,
                                    ErrorMessages::Lexer::NEWLINE_IN_N_USE_BACKLASH_N.args("character literal"), line,
                                    column, lineMap[line], {{column, column + 1}}, HighlightEffect::InsertAtEnd);
                    }
                    characters += iter;
                    break;
                }
                case State::StringLiteral:
                {
                    if (iter == '"' && (characters.empty() || characters.back() != '\\'))
                    {
                        auto originalCharacters = characters;
                        auto csize = characters.size();
                        currentState = State::Start;
                        bool followsInclude = result.size() >= 2
                                              && result[result.size() - 2].getTokenType() == TokenType::Pound
                                              && result.back().getTokenType() == TokenType::Identifier
                                              && std::get<std::string>(result.back().getValue()) == "include";
                        if (!followsInclude
                            && std::none_of(characters.begin(), characters.end(), [](char c) { return c == '\n'; }))
                        {
                            static std::regex escapes(R"(\\([0-7]{1,3}|x[0-9a-fA-F]+|.))");
                            std::smatch matches;
                            auto start = characters.cbegin();
                            while (start < characters.cend()
                                   && std::regex_search(start, characters.cend(), matches, escapes))
                            {
                                auto size = matches.suffix().first - characters.cbegin()
                                            - (matches[0].second - matches[0].first) + 1;
                                characters =
                                    std::string(characters.cbegin(), matches[0].first)
                                    + static_cast<char>(charactersToCharLiteral(
                                        reporter, {matches[0].first, matches[0].second}, line, column, lineMap[line]))
                                    + std::string(matches[0].second, characters.cend());
                                start = characters.cbegin() + size;
                            }
                        }
                        if (wide)
                        {
                            std::wstring wideString(characters.size(), L'0');
                            auto size = std::mbstowcs(wideString.data(), characters.c_str(), characters.size());
                            wideString.resize(size);
                            result.emplace_back(line, column - 1 - csize - 1, 3 + csize, TokenType::StringLiteral,
                                                "L\"" + std::move(originalCharacters) + '\"', wideString);
                            wide = false;
                        }
                        else
                        {
                            result.emplace_back(line, column - 1 - csize, 2 + csize, TokenType::StringLiteral,
                                                '\"' + std::move(originalCharacters) + '\"', characters);
                        }
                        characters.clear();
                        continue;
                    }
                    if (iter == '\n')
                    {
                        reportError(reporter, ErrorMessages::Lexer::NEWLINE_IN_N_USE_BACKLASH_N.args("string literal"),
                                    line, column, lineMap[line], {{column, column + 1}}, HighlightEffect::InsertAtEnd);
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
                        if (language != Language::Preprocessor && isKeyword(characters))
                        {
                            result.emplace_back(line, column - characters.size(), characters.size(),
                                                charactersToKeyword(characters), characters);
                        }
                        else if (language == Language::Preprocessor && characters == "defined")
                        {
                            result.emplace_back(line, column - characters.size(), characters.size(),
                                                TokenType::DefinedKeyword, characters);
                        }
                        else
                        {
                            result.emplace_back(line, column - characters.size(), characters.size(),
                                                TokenType::Identifier, characters, characters);
                        }
                        characters.clear();
                        currentState = State::Start;
                        handled = false;
                    }
                    break;
                }
                case State::Number:
                {
                    if ((iter >= '0' && iter <= '9') || (iter >= 'a' && iter <= 'f') || (iter >= 'A' && iter <= 'F')
                        || iter == 'x' || iter == 'X' || iter == '.' || iter == 'u' || iter == 'U' || iter == 'l'
                        || iter == 'L' || iter == 'P' || iter == 'p')
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
                        result.push_back(charactersToNumber(reporter, characters, line, column - characters.size(),
                                                            lineMap[line], language == Language::Preprocessor));
                        characters.clear();
                        currentState = State::Start;
                        handled = false;
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
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::Division)
                            {
                                currentState = State::LineComment;
                                result.pop_back();
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Division, "/");
                            }
                            break;
                        }
                        case '*':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::Division)
                            {
                                currentState = State::BlockComment;
                                blockCommentStart = std::pair{line, column};
                                result.pop_back();
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Asterisk, "*");
                            }
                            break;
                        }
                        case '.':
                        {
                            if (lastTokenIsAmbiguous && result.size() > 2
                                && result.back().getTokenType() == TokenType::Dot
                                && result.at(result.size() - 2).getTokenType() == TokenType::Dot
                                && result.back().getColumn() - 1 == result.at(result.size() - 2).getColumn())
                            {
                                result.pop_back();
                                result.pop_back();
                                result.emplace_back(line, column - 2, 3, TokenType::Ellipse, "...");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Dot, ".");
                            }
                            break;
                        }
                        case '#':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::Pound)
                            {
                                auto representation = result.back().getRepresentation();
                                result.pop_back();
                                result.emplace_back(line, column - representation.size(), 1 + representation.size(),
                                                    TokenType::DoublePound, representation + '#');
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Pound, "#");
                            }
                            break;
                        }
                        case '%':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::LessThan)
                            {
                                result.pop_back();
                                result.emplace_back(line, column - 1, 2, TokenType::OpenBrace, "<%");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Percent, "%");
                            }
                            break;
                        }
                        case ':':
                        {
                            if (lastTokenIsAmbiguous)
                            {
                                if (!result.empty() && result.back().getTokenType() == TokenType::LessThan)
                                {
                                    result.pop_back();
                                    result.emplace_back(line, column - 1, 2, TokenType::OpenSquareBracket, "<:");
                                }
                                else if (!result.empty() && result.back().getTokenType() == TokenType::Percent)
                                {
                                    result.pop_back();
                                    if (!result.empty() && result.back().getTokenType() == TokenType::Pound
                                        && result.back().getColumn() + result.back().getLength() == column - 1)
                                    {
                                        auto representation = result.back().getRepresentation();
                                        result.pop_back();
                                        result.emplace_back(line, column - representation.size() - 1,
                                                            representation.size() + 2, TokenType::DoublePound,
                                                            representation + "%:");
                                    }
                                    else
                                    {
                                        result.emplace_back(line, column - 1, 2, TokenType::Pound, "%:");
                                    }
                                }
                                else
                                {
                                    result.emplace_back(line, column, 1, TokenType::Colon, ":");
                                }
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Colon, ":");
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
                                    result.emplace_back(line, column - 1, 2, TokenType::Arrow, "->");
                                }
                                else if (!result.empty() && result.back().getTokenType() == TokenType::GreaterThan)
                                {
                                    result.pop_back();
                                    result.emplace_back(line, column - 1, 2, TokenType::ShiftRight, ">>");
                                }
                                else if (!result.empty() && result.back().getTokenType() == TokenType::Colon)
                                {
                                    result.pop_back();
                                    result.emplace_back(line, column - 1, 2, TokenType::CloseSquareBracket, ":>");
                                }
                                else if (!result.empty() && result.back().getTokenType() == TokenType::Percent)
                                {
                                    result.pop_back();
                                    result.emplace_back(line, column - 1, 2, TokenType::CloseBrace, "%>");
                                }
                                else
                                {
                                    result.emplace_back(line, column, 1, TokenType::GreaterThan, ">");
                                }
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::GreaterThan, ">");
                            }
                            break;
                        }
                        case '<':
                        {
                            if (result.size() >= 2 && result[result.size() - 2].getTokenType() == TokenType::Pound
                                && result.back().getTokenType() == TokenType::Identifier
                                && std::get<std::string>(result.back().getValue()) == "include")
                            {
                                currentState = State::AfterInclude;
                                characters.clear();
                                break;
                            }
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::LessThan)
                            {
                                result.pop_back();
                                result.emplace_back(line, column - 1, 2, TokenType::ShiftLeft, "<<");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::LessThan, "<");
                            }
                            break;
                        }
                        case '&':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::Ampersand)
                            {
                                result.pop_back();
                                result.emplace_back(line, column - 1, 2, TokenType::LogicAnd, "&&");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Ampersand, "&");
                            }
                            break;
                        }
                        case '|':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::BitOr)
                            {
                                result.pop_back();
                                result.emplace_back(line, column - 1, 2, TokenType::LogicOr, "||");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::BitOr, "|");
                            }
                            break;
                        }
                        case '+':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::Plus)
                            {
                                result.pop_back();
                                result.emplace_back(line, column - 1, 2, TokenType::Increment, "++");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Plus, "+");
                            }
                            break;
                        }
                        case '-':
                        {
                            if (lastTokenIsAmbiguous && !result.empty()
                                && result.back().getTokenType() == TokenType::Minus)
                            {
                                result.pop_back();
                                result.emplace_back(line, column - 1, 2, TokenType::Decrement, "--");
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Minus, "-");
                            }
                            break;
                        }
                        case '=':
                        {
                            if (!result.empty() && lastTokenIsAmbiguous)
                            {
                                switch (result.back().getTokenType())
                                {
                                    case TokenType::Assignment:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::Equal, "==");
                                        break;
                                    case TokenType::LogicalNegation:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::NotEqual, "!=");
                                        break;
                                    case TokenType::GreaterThan:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::GreaterThanOrEqual, ">=");
                                        break;
                                    case TokenType::LessThan:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::LessThanOrEqual, "<=");
                                        break;
                                    case TokenType::Plus:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::PlusAssign, "+=");
                                        break;
                                    case TokenType::Minus:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::MinusAssign, "-=");
                                        break;
                                    case TokenType::Division:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::DivideAssign, "/=");
                                        break;
                                    case TokenType::Asterisk:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::MultiplyAssign, "*=");
                                        break;
                                    case TokenType::Percent:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::ModuloAssign, "%=");
                                        break;
                                    case TokenType::Ampersand:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::BitAndAssign, "&=");
                                        break;
                                    case TokenType::BitOr:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::BitOrAssign, "|=");
                                        break;
                                    case TokenType::BitXor:
                                        result.pop_back();
                                        result.emplace_back(line, column - 1, 2, TokenType::BitXorAssign, "^=");
                                        break;
                                    case TokenType::ShiftLeft:
                                        result.pop_back();
                                        result.emplace_back(line, column - 2, 3, TokenType::ShiftLeftAssign, "<<=");
                                        break;
                                    case TokenType::ShiftRight:
                                        result.pop_back();
                                        result.emplace_back(line, column - 2, 3, TokenType::ShiftRightAssign, ">>=");
                                        break;
                                    default: result.emplace_back(line, column, 1, TokenType::Assignment, "="); break;
                                }
                            }
                            else
                            {
                                result.emplace_back(line, column, 1, TokenType::Assignment, "=");
                            }
                            break;
                        }
                        default:
                            lastTokenIsAmbiguous = false;
                            handled = false;
                            if (!result.empty() && result.back().getTokenType() == TokenType::Dot && iter >= '0'
                                && iter <= '9')
                            {
                                result.pop_back();
                                characters += '.';
                                currentState = State::Number;
                            }
                            else
                            {
                                currentState = State::Start;
                            }
                            break;
                    }
                    if (handled)
                    {
                        lastTokenIsAmbiguous = true;
                    }
                    break;
                }
                case State::AfterInclude:
                {
                    if (iter == '>')
                    {
                        currentState = State::Start;
                        result.emplace_back(line, column - 1 - characters.size(), 2 + characters.size(),
                                            TokenType::StringLiteral, '<' + characters + '>', characters);
                        characters.clear();
                        break;
                    }
                    if (iter == '\n')
                    {
                        reportError(reporter, ErrorMessages::Lexer::NEWLINE_IN_N_USE_BACKLASH_N.args("string literal"),
                                    line, column, lineMap[line], {{column, column + 1}}, HighlightEffect::InsertAtEnd);
                    }
                    characters += iter;
                    break;
                }
            }
        } while (!handled);
        if (iter == '\n')
        {
            lastTokenIsAmbiguous = false;
            if (currentState == State::LineComment)
            {
                currentState = State::Start;
            }
            else if (language == Language::Preprocessor)
            {
                result.emplace_back(line, column, 0, TokenType::Newline, "\n");
            }
            line++;
            column = 0;
            continue;
        }
        else
        {
            column++;
        }
    }
    if (currentState == State::BlockComment)
    {
        reportError(reporter, ErrorMessages::Lexer::UNTERMINATED_COMMENT, blockCommentStart.first,
                    blockCommentStart.second, lineMap[blockCommentStart.first],
                    {{blockCommentStart.first, blockCommentStart.first + 1}}, HighlightEffect::PointAtEnd);
    }
    return SourceObject(std::move(result), language);
}

std::string OpenCL::Lexer::Token::getRepresentation() const
{
    return m_representation;
}

std::string OpenCL::Lexer::tokenName(OpenCL::Lexer::TokenType tokenType)
{
    switch (tokenType)
    {
        case TokenType::Identifier: return "identifier";
        case TokenType::OpenParentheses: return "'('";
        case TokenType::CloseParentheses: return "')'";
        case TokenType::OpenBrace: return "'{'";
        case TokenType::CloseBrace: return "'}'";
        case TokenType::StringLiteral: return "string literal";
        case TokenType::Literal: return "literal";
        case TokenType::SemiColon: return "';'";
        case TokenType::Comma: return "','";
        case TokenType::Minus: return "'-'";
        case TokenType::BitWiseNegation: return "'~'";
        case TokenType::LogicalNegation: return "'!'";
        case TokenType::Plus: return "'+'";
        case TokenType::Asterisk: return "'*'";
        case TokenType::Division: return "'/'";
        case TokenType::Percent: return "'%'";
        case TokenType::LogicAnd: return "'&&'";
        case TokenType::LogicOr: return "'||'";
        case TokenType::Ampersand: return "'&'";
        case TokenType::BitOr: return "'|'";
        case TokenType::BitXor: return "'^'";
        case TokenType::Equal: return "'=='";
        case TokenType::NotEqual: return "'!='";
        case TokenType::LessThan: return "'<'";
        case TokenType::LessThanOrEqual: return "'<='";
        case TokenType::GreaterThan: return "'>'";
        case TokenType::GreaterThanOrEqual: return "'>='";
        case TokenType::Assignment: return "'='";
        case TokenType::PlusAssign: return "'+='";
        case TokenType::MinusAssign: return "'-='";
        case TokenType::DivideAssign: return "'/='";
        case TokenType::MultiplyAssign: return "'*='";
        case TokenType::ModuloAssign: return "'%='";
        case TokenType::ShiftLeftAssign: return "'<<='";
        case TokenType::ShiftRightAssign: return "'>>='";
        case TokenType::BitAndAssign: return "'&='";
        case TokenType::BitOrAssign: return "'|='";
        case TokenType::BitXorAssign: return "'^='";
        case TokenType::ShiftRight: return "'>>'";
        case TokenType::ShiftLeft: return "'<<'";
        case TokenType::Increment: return "'++'";
        case TokenType::Decrement: return "'--'";
        case TokenType::Colon: return "':'";
        case TokenType::QuestionMark: return "'?'";
        case TokenType::VoidKeyword: return "'void'";
        case TokenType::CharKeyword: return "'char'";
        case TokenType::ShortKeyword: return "'short'";
        case TokenType::IntKeyword: return "'int'";
        case TokenType::LongKeyword: return "'long'";
        case TokenType::FloatKeyword: return "'float'";
        case TokenType::DoubleKeyword: return "'double'";
        case TokenType::SignedKeyword: return "'signed'";
        case TokenType::UnsignedKeyword: return "'unsigned'";
        case TokenType::TypedefKeyword: return "'typedef'";
        case TokenType::ExternKeyword: return "'extern'";
        case TokenType::StaticKeyword: return "'static'";
        case TokenType::AutoKeyword: return "'auto'";
        case TokenType::RegisterKeyword: return "'register'";
        case TokenType::ConstKeyword: return "'const'";
        case TokenType::SizeofKeyword: return "'sizeof'";
        case TokenType::ReturnKeyword: return "'return'";
        case TokenType::BreakKeyword: return "'break'";
        case TokenType::ContinueKeyword: return "'continue'";
        case TokenType::DoKeyword: return "'do'";
        case TokenType::ElseKeyword: return "'else'";
        case TokenType::ForKeyword: return "'for'";
        case TokenType::IfKeyword: return "'if'";
        case TokenType::WhileKeyword: return "'while'";
        case TokenType::OpenSquareBracket: return "'['";
        case TokenType::CloseSquareBracket: return "']'";
        case TokenType::StructKeyword: return "'struct'";
        case TokenType::Dot: return "'.'";
        case TokenType::Arrow: return "'->'";
        case TokenType::SwitchKeyword: return "'switch'";
        case TokenType::CaseKeyword: return "'case'";
        case TokenType::DefaultKeyword: return "'default'";
        case TokenType::UnionKeyword: return "'union'";
        case TokenType::VolatileKeyword: return "'volatile'";
        case TokenType::EnumKeyword: return "'enum'";
        case TokenType::GotoKeyword: return "'goto'";
        case TokenType::Ellipse: return "'...'";
        case TokenType::RestrictKeyword: return "'restrict'";
        case TokenType::InlineKeyword: return "'inline'";
        case TokenType::Pound: return "'#'";
        case TokenType::DoublePound: return "'##'";
        case TokenType::DefinedKeyword: return "'defined'";
        case TokenType::Newline: return "'Newline'";
        case TokenType::UnderlineBool: return "'_Bool'";
        case TokenType::Miscellaneous: OPENCL_UNREACHABLE;
    }
    OPENCL_UNREACHABLE;
}

std::string OpenCL::Lexer::tokenValue(OpenCL::Lexer::TokenType tokenType)
{
    switch (tokenType)
    {
        case TokenType::Identifier: return "identifier";
        case TokenType::OpenParentheses: return "(";
        case TokenType::CloseParentheses: return ")";
        case TokenType::OpenBrace: return "{";
        case TokenType::CloseBrace: return "}";
        case TokenType::StringLiteral: return "string literal";
        case TokenType::Literal: return "literal";
        case TokenType::SemiColon: return ";";
        case TokenType::Comma: return ",";
        case TokenType::Minus: return "-";
        case TokenType::BitWiseNegation: return "~";
        case TokenType::LogicalNegation: return "!";
        case TokenType::Plus: return "+";
        case TokenType::Asterisk: return "*";
        case TokenType::Division: return "/";
        case TokenType::Percent: return "%";
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
        case TokenType::Pound: return "#";
        case TokenType::DoublePound: return "##";
        case TokenType::DefinedKeyword: return "defined";
        case TokenType::Newline: return "Newline";
        case TokenType::UnderlineBool: return "_Bool";
        case TokenType::Miscellaneous: OPENCL_UNREACHABLE;
    }
    OPENCL_UNREACHABLE;
}

std::uint64_t OpenCL::Lexer::Token::getLength() const noexcept
{
    return m_length;
}

void OpenCL::Lexer::Token::setLine(std::uint64_t line) noexcept
{
    m_line = line;
}

void OpenCL::Lexer::Token::setColumn(std::uint64_t column) noexcept
{
    m_column = column;
}

void OpenCL::Lexer::Token::setLength(std::uint64_t length) noexcept
{
    m_length = length;
}

std::uint64_t OpenCL::Lexer::Token::getSourceLine() const noexcept
{
    return m_sourceLine;
}

void OpenCL::Lexer::Token::setSourceLine(std::uint64_t sourceLine) noexcept
{
    m_sourceLine = sourceLine;
}

std::uint64_t OpenCL::Lexer::Token::getSourceColumn() const noexcept
{
    return m_sourceColumn;
}

void OpenCL::Lexer::Token::setSourceColumn(std::uint64_t sourceColumn) noexcept
{
    m_sourceColumn = sourceColumn;
}

std::uint64_t OpenCL::Lexer::Token::getSourceLength() const noexcept
{
    return m_sourceLength;
}

void OpenCL::Lexer::Token::setSourceLength(std::uint64_t sourceLength) noexcept
{
    m_sourceLength = sourceLength;
}

bool OpenCL::Lexer::Token::macroInserted() const noexcept
{
    return m_macroId;
}

std::uint64_t OpenCL::Lexer::Token::getColumn() const noexcept
{
    return m_column;
}

std::uint64_t OpenCL::Lexer::Token::getLine() const noexcept
{
    return m_line;
}

const OpenCL::Lexer::Token::variant& OpenCL::Lexer::Token::getValue() const noexcept
{
    return m_value;
}

OpenCL::Lexer::TokenType OpenCL::Lexer::Token::getTokenType() const noexcept
{
    return m_tokenType;
}

OpenCL::Lexer::Token::Token(std::uint64_t line, std::uint64_t column, std::uint64_t length,
                            OpenCL::Lexer::TokenType tokenType, std::string representation,
                            OpenCL::Lexer::Token::variant value)
    : m_line(line),
      m_column(column),
      m_length(length),
      m_tokenType(tokenType),
      m_representation(std::move(representation)),
      m_value(std::move(value)),
      m_sourceLine(line),
      m_sourceColumn(column),
      m_sourceLength(length)
{
    assert(!m_representation.empty());
}

std::uint64_t OpenCL::Lexer::Token::getMacroId() const noexcept
{
    return m_macroId;
}

void OpenCL::Lexer::Token::setMacroId(std::uint64_t macroId) noexcept
{
    m_macroId = macroId;
}

std::string OpenCL::Lexer::reconstruct(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                       std::vector<OpenCL::Lexer::Token>::const_iterator end)
{
    if (begin == end)
    {
        return {};
    }
    return std::string(begin->getLine() - 1, '\n') + std::string(begin->getColumn(), ' ')
           + reconstructTrimmed(begin, end);
}

std::string OpenCL::Lexer::reconstructTrimmed(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
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
        result += curr->getRepresentation();
    }
    return result;
}

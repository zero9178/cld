#include "Lexer.hpp"

#include <llvm/Support/ConvertUTF.h>

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <cassert>
#include <cctype>
#include <regex>
#include <sstream>
#include <unordered_map>

#include "ErrorMessages.hpp"
#include "SourceObject.hpp"
#include "termcolor.hpp"

using namespace OpenCL::Lexer;

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

    //    std::int32_t charactersToCharLiteral(std::ostream* reporter, const std::string& characters, std::uint64_t
    //    line,
    //                                         std::uint64_t column, const std::string& lineText)
    //    {
    //        if (characters.empty() || characters[0] != '\\')
    //        {
    //            return characters.front();
    //        }
    //        if (characters == "\\'")
    //        {
    //            return '\'';
    //        }
    //        if (characters == "\\\"")
    //        {
    //            return '"';
    //        }
    //        if (characters == "\\?")
    //        {
    //            return '\?';
    //        }
    //        if (characters == "\\\\")
    //        {
    //            return '\\';
    //        }
    //        if (characters == "\\a")
    //        {
    //            return '\a';
    //        }
    //        if (characters == "\\b")
    //        {
    //            return '\b';
    //        }
    //        if (characters == "\\f")
    //        {
    //            return '\f';
    //        }
    //        if (characters == "\\n")
    //        {
    //            return '\n';
    //        }
    //        if (characters == "\\r")
    //        {
    //            return '\r';
    //        }
    //        if (characters == "\\t")
    //        {
    //            return '\t';
    //        }
    //        if (characters == "\\v")
    //        {
    //            return '\v';
    //        }
    //        else if (characters.front() == '\\')
    //        {
    //            std::int32_t number = 0;
    //            if (characters[1] == 'x')
    //            {
    //                if (characters.size() <= 2)
    //                {
    //                    reportError(reporter, OpenCL::ErrorMessages::Lexer::AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED,
    //                    line,
    //                                column - characters.size() + 1, lineText,
    //                                {{column - characters.size() - 1, column - 1}});
    //                    return 0;
    //                }
    //                std::istringstream ss(characters.substr(2, characters.size() - 1));
    //                if (!(ss >> std::hex >> number))
    //                {
    //                    reportError(reporter,
    //                    OpenCL::ErrorMessages::Lexer::INVALID_HEXADECIMAL_CHARACTER.args(ss.str()),
    //                                line, column - characters.size() + 1, lineText,
    //                                {{column - characters.size() - 1, column - 1}});
    //                }
    //            }
    //            else if (std::isdigit(characters[1]))
    //            {
    //                std::istringstream ss(characters.substr(1, characters.size() - 1));
    //                if (!(ss >> std::oct >> number))
    //                {
    //                    reportError(reporter, OpenCL::ErrorMessages::Lexer::INVALID_OCTAL_CHARACTER.args(ss.str()),
    //                    line,
    //                                column - characters.size() + 1, lineText,
    //                                {{column - characters.size() - 1, column - 1}});
    //                }
    //            }
    //            else
    //            {
    //                reportError(reporter, OpenCL::ErrorMessages::Lexer::INCORRECT_CHARACTER_LITERAL.args(characters),
    //                line,
    //                            column - characters.size() - 2, lineText, {{column - characters.size() - 1, column}});
    //                return 0;
    //            }
    //            if (number > std::numeric_limits<std::uint8_t>::max())
    //            {
    //                reportError(
    //                    reporter,
    //                    OpenCL::ErrorMessages::Lexer::CHARACTER_MUSTNT_HAVE_HIGHER_VALUE_THAN_MAXIMUM_VALUE_OF_UCHAR,
    //                    line, column - characters.size() - 1, lineText, {{column - characters.size() - 1, column}});
    //            }
    //            return number;
    //        }
    //        reportError(reporter, OpenCL::ErrorMessages::Lexer::INCORRECT_CHARACTER_LITERAL.args(characters), line,
    //                    column - characters.size() - 1, lineText, {{column - characters.size() - 1, column}});
    //        return 0;
    //    }

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

    TokenType charactersToKeyword(const std::string& characters)
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

    Token charactersToNumber(std::ostream* reporter, const std::string& literal, std::uint64_t line,
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

    class Context
    {
        OpenCL::LanguageOptions m_languageOptions;
        bool m_inPreprocessor;
        std::ostream* m_reporter;
        std::vector<Token> m_result;

    public:
        std::uint64_t line = 1;
        std::uint64_t column = 0;

        Context(OpenCL::LanguageOptions languageOptions, bool inPreprocessor, std::ostream* reporter) noexcept
            : m_languageOptions(languageOptions), m_inPreprocessor(inPreprocessor), m_reporter(reporter)
        {
        }

        [[nodiscard]] OpenCL::LanguageOptions getLanguageOptions() const
        {
            return m_languageOptions;
        }

        [[nodiscard]] std::ostream* getReporter() const noexcept
        {
            return m_reporter;
        }

        [[nodiscard]] const std::vector<OpenCL::Lexer::Token>& getResult() const& noexcept
        {
            return m_result;
        }

        [[nodiscard]] std::vector<OpenCL::Lexer::Token> getResult() && noexcept
        {
            return m_result;
        }

        bool isInPreprocessor() const
        {
            return m_inPreprocessor;
        }

        void push(Token&& token) noexcept
        {
            m_result.push_back(std::move(token));
        }

        template <class... T>
        void emplace(T&&... args) noexcept
        {
            m_result.emplace_back(std::forward<T>(args)...);
        }
    };

    struct Start;
    struct CharacterLiteral;
    struct StringLiteral;
    struct Text;
    struct LineComment;
    struct BlockComment;
    struct Number;
    struct AfterInclude;
    struct L;

    using StateMachine =
        std::variant<Start, CharacterLiteral, StringLiteral, Text, LineComment, BlockComment, Number, AfterInclude, L>;

    struct Start final
    {
        StateMachine advance(char c, Context& context) noexcept;
    };

    struct CharacterLiteral final
    {
        bool wide = false;
        std::string characters;

        StateMachine advance(char c, Context& context) noexcept;
    };

    struct StringLiteral final
    {
        bool wide = false;
        std::string characters;

        StateMachine advance(char c, Context& context) noexcept;
    };

    struct Text final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct LineComment final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct BlockComment final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct Number final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct AfterInclude final
    {
        void advance(char, Context& context) noexcept {}
    };

    struct L final
    {
        std::pair<StateMachine, bool> advance(char, Context& context) noexcept;
    };

    StateMachine Start::advance(char c, Context&) noexcept
    {
        switch (c)
        {
            case '\'': return CharacterLiteral{};
            case '"': return StringLiteral{};
            case 'L': return L{};
            default: return *this;
        }
    }

    StateMachine CharacterLiteral::advance(char c, Context& context) noexcept
    {
        if (c == '\'' && (characters.empty() || characters.back() != '\\'))
        {
            std::vector<std::uint32_t> result;
            result.resize(characters.size());
            auto* destBegin = result.data();
            const auto* begin = characters.data();
            auto res = llvm::ConvertUTF8toUTF32(
                reinterpret_cast<const llvm::UTF8**>(&begin),
                reinterpret_cast<const llvm::UTF8*>(
                    std::find_if(begin + 1, characters.c_str() + characters.size(), [](char c) { return c == '\\'; })),
                &destBegin, result.data() + result.size(), llvm::strictConversion);
            // TODO: check res, handle escape characters, make character literal only single character wide. Check
            //  ranges
            if (!context.isInPreprocessor())
            {
                if (wide)
                {
                    std::int32_t value;
                    std::memcpy(&value, &result[0], sizeof(std::int32_t));
                    // TODO: Error on platforms where wchar_t isn't 32 bit if value is bigger than max utf 16.
                    //  Conversion to utf 16 is truncation otherwise
                    context.emplace(context.line, context.column - characters.size() - 1, characters.size() + 2,
                                    TokenType::Literal, '\'' + characters + '\'', value);
                }
                else
                {
                    // TODO: check that result[0] isn't bigger than 255
                    context.emplace(context.line, context.column - characters.size() - 1, characters.size() + 2,
                                    TokenType::Literal, '\'' + characters + '\'',
                                    static_cast<std::int32_t>(static_cast<std::uint8_t>(result[0])));
                }
            }
            else
            {
                context.emplace(context.line, context.column - characters.size() - 1, characters.size() + 2,
                                TokenType::Literal, '\'' + characters + '\'', static_cast<std::int64_t>(result[0]));
            }
            return Start{};
        }
        characters += c;
        return std::move(*this);
    }

    StateMachine StringLiteral::advance(char c, Context& context) noexcept
    {
        if (c == '"' && (characters.empty() || characters.back() != '\\'))
        {
            auto originalCharacters = characters;
            auto csize = characters.size();
            bool followsInclude =
                context.isInPreprocessor() && context.getResult().size() >= 2
                && (context.getResult()[context.getResult().size() - 2].getTokenType() == TokenType::Pound
                    && context.getResult().back().getTokenType() == TokenType::Identifier
                    && std::get<std::string>(context.getResult().back().getValue()) == "include");
            if (!followsInclude) {}
            if (wide) {}
            else
            {
                context.emplace(context.line, context.column - 1 - csize, 2 + csize, TokenType::StringLiteral,
                                '"' + std::move(originalCharacters) + '"', characters);
            }
        }
        characters += c;
        return std::move(*this);
    }

    std::pair<StateMachine, bool> L::advance(char c, Context& context) noexcept
    {
        if (c == '"')
        {
            return {StringLiteral{true}, true};
        }
        else if (c == '\'')
        {
            return {CharacterLiteral{true}, true};
        }
        else
        {
            return {Text{/*TODO:"L" */}, false};
        }
    }
} // namespace

OpenCL::SourceObject OpenCL::Lexer::tokenize(std::string source, LanguageOptions languageOptions, bool isInPreprocessor,
                                             std::ostream* reporter)
{
    if (source.empty() || source.back() != ' ')
    {
        source += '\n';
    }
    static std::regex identifierMatch("[a-zA-Z_]\\w*");

    Context context(languageOptions, isInPreprocessor, reporter);
    StateMachine stateMachine;
    for (auto iter : source)
    {
        while (std::visit(
            [iter, &stateMachine, &context](auto&& state) mutable -> bool {
                if constexpr (std::is_convertible_v<decltype(state.advance(iter, context)), bool>)
                {
                    return !state.advance(iter, context);
                }
                else if constexpr (std::is_same_v<StateMachine, decltype(state.advance(iter, context))>)
                {
                    stateMachine = state.advance(iter, context);
                    return false;
                }
                else if constexpr (std::is_void_v<decltype(state.advance(iter, context))>)
                {
                    state.advance(iter, context);
                    return false;
                }
                else
                {
                    auto&& [lhs, rhs] = state.advance(iter, context);
                    if constexpr (std::is_same_v<std::decay_t<decltype(lhs)>, bool>)
                    {
                        stateMachine = std::move(rhs);
                        return !lhs;
                    }
                    else
                    {
                        stateMachine = std::move(lhs);
                        return !rhs;
                    }
                }
            },
            stateMachine))
            ;
        if (iter == '\n')
        {
            context.line++;
            context.column = 0;
        }
        else
        {
            context.column++;
        }
    }

    return SourceObject(std::move(context).getResult(), languageOptions);
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

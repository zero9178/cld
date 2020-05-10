#pragma once

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "Lexer.hpp"

namespace cld
{
class Format
{
    const char* m_format;

    template <class From, class To, class = void>
    struct canStaticCast : std::false_type
    {
    };

    template <class From, class To>
    struct canStaticCast<From, To, std::void_t<decltype(static_cast<To>(std::declval<From>()))>> : std::true_type
    {
    };

    [[nodiscard]] std::string format(std::vector<std::string> args) const;

    template <class T>
    static std::string toString(T&& value)
    {
        using U = std::decay_t<T>;
        if constexpr (std::is_convertible_v<U, std::string>)
        {
            return value;
        }
        else if constexpr (canStaticCast<U, std::string>{})
        {
            return static_cast<std::string>(value);
        }
        else if constexpr (std::is_same_v<U, char>)
        {
            return std::string(1, value);
        }
        else if constexpr (std::is_same_v<llvm::Twine, U>)
        {
            return value.str();
        }
        else
        {
            return std::to_string(value);
        }
    }

public:
    class List
    {
        std::string m_delimiter;
        std::string m_lastInbetween;
        std::vector<std::string> m_strings;

    public:
        template <class... Args>
        List(std::string delimiter, std::string lastInbetween, Args&&... args)
            : m_delimiter(std::move(delimiter)),
              m_lastInbetween(std::move(lastInbetween)),
              m_strings({toString(args)...})
        {
        }

        List(std::string delimiter, std::string lastInbetween, std::vector<std::string> args)
            : m_delimiter(std::move(delimiter)), m_lastInbetween(std::move(lastInbetween)), m_strings(std::move(args))
        {
        }

        explicit operator std::string() const
        {
            std::string result;
            for (auto iter = m_strings.begin(); iter != m_strings.end(); iter++)
            {
                result += *iter;
                if (iter < m_strings.end() - 2)
                {
                    result += m_delimiter;
                }
                else if (iter != m_strings.end() - 1)
                {
                    result += m_lastInbetween;
                }
            }
            return result;
        }
    };

    constexpr explicit Format(const char* format) : m_format(format) {}

    template <class... Args>
    std::string args(Args&&... args) const
    {
        return format({toString(args)...});
    }
};

template <class T>
class Underline final
{
    Lexer::TokenIterator<T> m_begin;
    Lexer::TokenIterator<T> m_end;
    char m_character;

public:
    template <class Iter>
    Underline(Iter begin, char character = '~') : m_begin(begin), m_end(m_begin + 1), m_character(character)
    {
    }

    template <class Iter>
    Underline(Iter begin, Iter end, char character = '~') : m_begin(begin), m_end(end), m_character(character)
    {
    }

    [[nodiscard]] Lexer::TokenIterator<T> begin() const;

    [[nodiscard]] Lexer::TokenIterator<T> end() const;

    [[nodiscard]] char getCharacter() const;
};

template <class Iter>
Underline(Iter, char) -> Underline<typename Iter::value_type>;

template <class Iter>
Underline(Iter, Iter, char) -> Underline<typename Iter::value_type>;

template <class Iter>
Underline(Iter) -> Underline<typename Iter::value_type>;

template <class Iter>
Underline(Iter, Iter) -> Underline<typename Iter::value_type>;

extern template class Underline<Lexer::CToken>;

extern template class Underline<Lexer::PPToken>;

template <class T>
class PointAt final
{
    Lexer::TokenIterator<T> m_begin;
    Lexer::TokenIterator<T> m_end;

public:
    template <class Iter>
    PointAt(Iter begin) : m_begin(begin), m_end(m_begin + 1)
    {
    }

    template <class Iter>
    PointAt(Iter begin, Iter end) : m_begin(begin), m_end(end)
    {
    }

    [[nodiscard]] Lexer::TokenIterator<T> begin() const;

    [[nodiscard]] Lexer::TokenIterator<T> end() const;
};

template <class Iter>
PointAt(Iter) -> PointAt<typename Iter::value_type>;

template <class Iter>
PointAt(Iter, Iter) -> PointAt<typename Iter::value_type>;

extern template class PointAt<Lexer::CToken>;

extern template class PointAt<Lexer::PPToken>;

template <class T>
class InsertAfter final
{
    Lexer::TokenIterator<T> m_insertAfter;
    std::string m_argument;

public:
    template <class Iter>
    explicit InsertAfter(Iter insertAfter, std::string_view argument = {})
        : m_insertAfter(insertAfter), m_argument(argument.begin(), argument.end())
    {
    }

    [[nodiscard]] Lexer::TokenIterator<T> getInsertAfter() const noexcept;

    [[nodiscard]] const std::string& getArgument() const noexcept;
};

template <class Iter>
InsertAfter(Iter) -> InsertAfter<typename Iter::value_type>;

template <class Iter>
InsertAfter(Iter, std::string_view) -> InsertAfter<typename Iter::value_type>;

extern template class InsertAfter<Lexer::CToken>;

extern template class InsertAfter<Lexer::PPToken>;

template <class T>
class Annotate final
{
    Lexer::TokenIterator<T> m_begin;
    Lexer::TokenIterator<T> m_end;
    std::string m_argument;

public:
    template <class Iter>
    Annotate(Iter begin, std::string_view argument)
        : m_begin(begin), m_end(begin + 1), m_argument(argument.begin(), argument.end())
    {
    }

    template <class Iter>
    Annotate(Iter begin, Iter end, std::string_view argument)
        : m_begin(begin), m_end(end), m_argument(argument.begin(), argument.end())
    {
    }

    [[nodiscard]] Lexer::TokenIterator<T> begin() const;

    [[nodiscard]] Lexer::TokenIterator<T> end() const;

    [[nodiscard]] const std::string& getArgument() const;
};

template <class Iter>
Annotate(Iter, std::string_view) -> Annotate<typename Iter::value_type>;

template <class Iter>
Annotate(Iter, Iter, std::string_view) -> Annotate<typename Iter::value_type>;

extern template class Annotate<Lexer::CToken>;

extern template class Annotate<Lexer::PPToken>;

template <class T>
using Modifier = std::variant<Underline<T>, PointAt<T>, InsertAfter<T>, Annotate<T>>;

/**
 * Class used to present Messages from various components of the compiler, eg. Parser or Semantics.
 *
 * This class operates on Tokens and it's associated SourceObject
 *
 * By passing modifiers one can also add effects to the outputted message
 */
template <class T>
class Message final
{
public:
    /**
     * Severity of the message
     */
    enum Severity : std::uint8_t
    {
        Error,  ///< An error occurred. Modifier colour and text are red
        Note,   ///< A Note used to further explain a warning or Error. Modifier colour and text are cyan
        Warning ///< A warning is issued. Modifier colour and text are purple
    };

private:
    std::vector<Modifier<T>> m_modifiers;
    std::string m_message;
    Lexer::TokenIterator<T> m_begin;
    std::optional<Lexer::TokenIterator<T>> m_end;
    Severity m_severity;

    Message(Severity severity, std::string message, Lexer::TokenIterator<T> begin,
            std::optional<Lexer::TokenIterator<T>> end, std::vector<Modifier<T>> modifiers = {});

public:
    /**
     * Generates an error Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    static Message error(std::string message, Lexer::TokenIterator<T> token, std::vector<Modifier<T>> modifiers = {});

    /**
     * Generates an error Message
     * @param message Message to be printed
     * @param begin Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines from begin until exclusive end.
     * @param end Exclusive end of all tokens whose lines need to be printed
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    static Message error(std::string message, Lexer::TokenIterator<T> begin, Lexer::TokenIterator<T> end,
                         std::vector<Modifier<T>> modifiers = {});

    /**
     * Generates a note Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    static Message note(std::string message, Lexer::TokenIterator<T> token, std::vector<Modifier<T>> modifiers = {});

    /**
     * Generates a note Message
     * @param message Message to be printed
     * @param begin Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines from begin until exclusive end.
     * @param end Exclusive end of all tokens whose lines need to be printed
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    static Message note(std::string message, Lexer::TokenIterator<T> begin, Lexer::TokenIterator<T> end,
                        std::vector<Modifier<T>> modifiers = {});

    /**
     * Generates a warning Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    static Message warning(std::string message, Lexer::TokenIterator<T> token, std::vector<Modifier<T>> modifiers = {});

    /**
     * Generates a warning Message
     * @param message Message to be printed
     * @param begin Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines from begin until exclusive end.
     * @param end Exclusive end of all tokens whose lines need to be printed
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    static Message warning(std::string message, Lexer::TokenIterator<T> begin, Lexer::TokenIterator<T> end,
                           std::vector<Modifier<T>> modifiers = {});

    [[nodiscard]] Severity getSeverity() const;

    llvm::raw_ostream& print(llvm::raw_ostream& os, const SourceObject<T>& sourceObject) const;
};

using CMessage = Message<Lexer::CToken>;
using PPMessage = Message<Lexer::PPToken>;

extern template class Message<Lexer::CToken>;

extern template class Message<Lexer::PPToken>;
} // namespace cld

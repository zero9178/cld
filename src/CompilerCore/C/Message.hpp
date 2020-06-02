#pragma once

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "Lexer.hpp"

namespace cld
{
class Format final
{
    const char* m_format;

    template <class From, class To, class = void>
    struct canStaticCast final : std::false_type
    {
    };

    template <class From, class To>
    struct canStaticCast<From, To, std::void_t<decltype(static_cast<To>(std::declval<From>()))>> final : std::true_type
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
    class List final
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

class Underline final
{
    Lexer::TokenIterator m_begin;
    std::optional<Lexer::TokenIterator> m_maybeEnd;
    char m_character;

public:
    Underline(Lexer::TokenIterator begin, char character = '~')
        : m_begin(begin), m_maybeEnd(std::nullopt), m_character(character)
    {
    }

    Underline(Lexer::TokenIterator begin, Lexer::TokenIterator end, char character = '~')
        : m_begin(begin), m_maybeEnd(end), m_character(character)
    {
    }

    [[nodiscard]] Lexer::TokenIterator begin() const noexcept
    {
        return m_begin;
    }

    [[nodiscard]] const std::optional<Lexer::TokenIterator>& maybeEnd() const noexcept
    {
        return m_maybeEnd;
    }

    [[nodiscard]] char getCharacter() const noexcept
    {
        return m_character;
    }
};

class PointAt final
{
    Lexer::TokenIterator m_begin;
    std::optional<Lexer::TokenIterator> m_maybeEnd;

public:
    PointAt(Lexer::TokenIterator begin) : m_begin(begin), m_maybeEnd(std::nullopt) {}

    PointAt(Lexer::TokenIterator begin, Lexer::TokenIterator end) : m_begin(begin), m_maybeEnd(end) {}

    [[nodiscard]] Lexer::TokenIterator begin() const noexcept
    {
        return m_begin;
    }

    [[nodiscard]] const std::optional<Lexer::TokenIterator>& maybeEnd() const noexcept
    {
        return m_maybeEnd;
    }
};

class InsertAfter final
{
    Lexer::TokenIterator m_insertAfter;
    std::string m_argument;

public:
    explicit InsertAfter(Lexer::TokenIterator insertAfter, std::string_view argument = {})
        : m_insertAfter(insertAfter), m_argument(argument.begin(), argument.end())
    {
    }

    [[nodiscard]] Lexer::TokenIterator getInsertAfter() const noexcept
    {
        return m_insertAfter;
    }

    [[nodiscard]] const std::string& getArgument() const noexcept
    {
        return m_argument;
    }
};

class Annotate final
{
    Lexer::TokenIterator m_begin;
    std::optional<Lexer::TokenIterator> m_maybeEnd;
    std::string m_argument;

public:
    Annotate(Lexer::TokenIterator begin, std::string_view argument)
        : m_begin(begin), m_maybeEnd(std::nullopt), m_argument(argument.begin(), argument.end())
    {
    }

    Annotate(Lexer::TokenIterator begin, Lexer::TokenIterator end, std::string_view argument)
        : m_begin(begin), m_maybeEnd(end), m_argument(argument.begin(), argument.end())
    {
    }

    [[nodiscard]] Lexer::TokenIterator begin() const noexcept
    {
        return m_begin;
    }

    [[nodiscard]] const std::optional<Lexer::TokenIterator>& maybeEnd() const noexcept
    {
        return m_maybeEnd;
    }

    [[nodiscard]] const std::string& getArgument() const noexcept
    {
        return m_argument;
    }
};

using Modifier = std::variant<Underline, PointAt, InsertAfter, Annotate>;

/**
 * Class used to present Messages from various components of the compiler, eg. Parser or Semantics.
 *
 * This class operates on Tokens and it's associated SourceObject
 *
 * By passing modifiers one can also add effects to the outputted message
 */
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
    class TypeErasedTokenBaseIterator
    {
        Lexer::TokenIterator m_ptr{};
        using UnaryFn = Lexer::TokenIterator (*)(Lexer::TokenIterator, std::ptrdiff_t number);
        UnaryFn m_increment{};

        template <class T>
        static Lexer::TokenIterator increment(Lexer::TokenIterator ptr, std::ptrdiff_t number)
        {
            return static_cast<const T*>(ptr) + number;
        }

    public:
        using value_type = Lexer::TokenBase;
        using difference_type = std::ptrdiff_t;
        using reference = const Lexer::TokenBase&;
        using pointer = Lexer::TokenIterator;
        using iterator_category = std::forward_iterator_tag;

        TypeErasedTokenBaseIterator() = default;

        template <class T>
        explicit TypeErasedTokenBaseIterator(const T* ptr) noexcept : m_ptr(ptr), m_increment(increment<T>)
        {
            static_assert(std::is_base_of_v<Lexer::TokenBase, T>);
        }

        reference operator*() const noexcept
        {
            return *m_ptr;
        }

        pointer operator->() const noexcept
        {
            return m_ptr;
        }

        TypeErasedTokenBaseIterator& operator++(int) noexcept
        {
            m_ptr = m_increment(m_ptr, 1);
            return *this;
        }

        TypeErasedTokenBaseIterator operator++() noexcept
        {
            auto copy = *this;
            m_ptr = m_increment(m_ptr, 1);
            return copy;
        }

        TypeErasedTokenBaseIterator& operator--(int) noexcept
        {
            m_ptr = m_increment(m_ptr, -1);
            return *this;
        }

        TypeErasedTokenBaseIterator operator--() noexcept
        {
            auto copy = *this;
            m_ptr = m_increment(m_ptr, -1);
            return copy;
        }

        LLVM_READONLY TypeErasedTokenBaseIterator operator-(std::ptrdiff_t number) const noexcept
        {
            auto copy = *this;
            copy.m_ptr = copy.m_increment(copy.m_ptr, -number);
            return copy;
        }

        LLVM_READONLY TypeErasedTokenBaseIterator operator+(std::ptrdiff_t number) const noexcept
        {
            auto copy = *this;
            copy.m_ptr = copy.m_increment(copy.m_ptr, number);
            return copy;
        }

        bool operator==(const TypeErasedTokenBaseIterator& rhs) const noexcept
        {
            return m_ptr == rhs.m_ptr;
        }

        bool operator!=(const TypeErasedTokenBaseIterator& rhs) const noexcept
        {
            return m_ptr != rhs.m_ptr;
        }

        bool operator<(const TypeErasedTokenBaseIterator& rhs) const noexcept
        {
            return m_ptr < rhs.m_ptr;
        }

        bool operator>(const TypeErasedTokenBaseIterator& rhs) const noexcept
        {
            return m_ptr > rhs.m_ptr;
        }

        bool operator<=(const TypeErasedTokenBaseIterator& rhs) const noexcept
        {
            return m_ptr <= rhs.m_ptr;
        }

        bool operator>=(const TypeErasedTokenBaseIterator& rhs) const noexcept
        {
            return m_ptr >= rhs.m_ptr;
        }
    };

    std::vector<Modifier> m_modifiers;
    std::string m_message;
    bool m_after;
    TypeErasedTokenBaseIterator m_begin;
    std::optional<TypeErasedTokenBaseIterator> m_maybeEnd;
    Severity m_severity;

    template <class T>
    Message(Severity severity, std::string message, bool showAfter, const T* begin, std::optional<const T*> end,
            std::vector<Modifier> modifiers = {})
        : m_modifiers(std::move(modifiers)),
          m_message(std::move(message)),
          m_after(showAfter),
          m_begin(begin),
          m_maybeEnd(end),
          m_severity(severity)
    {
    }

    struct After
    {
    };

public:
    constexpr static After after{};

    /**
     * Generates an error Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can't be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message error(std::string message, const T* token, std::vector<Modifier> modifiers = {})
    {
        return Message(Error, std::move(message), false, token, {}, std::move(modifiers));
    }

    /**
     * Generates an error Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can't be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message error(std::string message, After, const T* token, std::vector<Modifier> modifiers = {})
    {
        return Message(Error, std::move(message), false, token, {}, std::move(modifiers));
    }

    /**
     * Generates an error Message
     * @param message Message to be printed
     * @param begin Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines from begin until exclusive end.
     * @param end Exclusive end of all tokens whose lines need to be printed
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message error(std::string message, const T* begin, const T* end, std::vector<Modifier> modifiers = {})
    {
        return Message(Error, std::move(message), false, begin, std::optional{end}, modifiers);
    }

    /**
     * Generates a note Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can't be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message note(std::string message, const T* token, std::vector<Modifier> modifiers = {})
    {
        return Message(Note, std::move(message), false, token, {}, std::move(modifiers));
    }

    /**
     * Generates a note Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can't be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message note(std::string message, After, const T* token, std::vector<Modifier> modifiers = {})
    {
        return Message(Note, std::move(message), true, token, {}, std::move(modifiers));
    }

    /**
     * Generates a note Message
     * @param message Message to be printed
     * @param begin Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines from begin until exclusive end.
     * @param end Exclusive end of all tokens whose lines need to be printed
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message note(std::string message, const T* begin, const T* end, std::vector<Modifier> modifiers = {})
    {
        return Message(Note, std::move(message), false, begin, std::optional{end}, modifiers);
    }

    /**
     * Generates a warning Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can't be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message warning(std::string message, const T* token, std::vector<Modifier> modifiers = {})
    {
        return Message(Warning, std::move(message), false, token, {}, std::move(modifiers));
    }

    /**
     * Generates a warning Message
     * @param message Message to be printed
     * @param token Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines that this token is contained in. Can't be end
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message warning(std::string message, After, const T* token, std::vector<Modifier> modifiers = {})
    {
        return Message(Warning, std::move(message), true, token, {}, std::move(modifiers));
    }

    /**
     * Generates a warning Message
     * @param message Message to be printed
     * @param begin Token that is used for the source location to be printed. The source code displayed is
     * guaranteed to display all lines from begin until exclusive end.
     * @param end Exclusive end of all tokens whose lines need to be printed
     * @param modifiers optional modifier to apply
     * @return Message object
     */
    template <class T>
    static Message warning(std::string message, const T* begin, const T* end, std::vector<Modifier> modifiers = {})
    {
        return Message(Warning, std::move(message), false, begin, std::optional{end}, modifiers);
    }

    [[nodiscard]] Severity getSeverity() const;

    llvm::raw_ostream& print(llvm::raw_ostream& os, const SourceInterface& sourceObject) const;
};
} // namespace cld

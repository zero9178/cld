#ifndef OPENCLPARSER_MESSAGE_HPP
#define OPENCLPARSER_MESSAGE_HPP

#include <string>
#include <vector>
#include <optional>
#include <iostream>
#include <sstream>
#include "Lexer.hpp"

namespace OpenCL
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

        std::string format(std::vector<std::string> args) const;

        template <class T>
        static std::string toString(T&& value)
        {
            if constexpr(std::is_convertible_v<T, std::string>)
            {
                return value;
            }
            else if constexpr(canStaticCast<T, std::string>{})
            {
                return static_cast<std::string>(value);
            }
            else if constexpr(std::is_same_v<T, char>)
            {
                return std::string(1, value);
            }
            else
            {
                return std::to_string(value);
            }
            return "";
        }

    public:

        class List
        {
            std::string m_delimiter;
            std::string m_lastInbetween;
            std::vector<std::string> m_strings;

        public:

            template <class...Args>
            List(std::string delimiter, std::string m_lastInbetween, Args&& ...args)
                : m_delimiter(std::move(delimiter)), m_lastInbetween(std::move(m_lastInbetween)),
                  m_strings({toString(args)...})
            {}

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

        constexpr explicit Format(const char* format) : m_format(format)
        {}

        template <class...Args>
        std::string args(Args&& ...args) const
        {
            return format({toString(args)...});
        }
    };

    class Modifier
    {

        std::vector<Lexer::Token>::const_iterator m_begin;
        std::vector<Lexer::Token>::const_iterator m_end;

    public:

        enum Action
        {
            Underline,
            PointAtBeginning,
            PointAtEnd
        };

    private:

        Action m_action;

    public:

        Modifier(std::vector<Lexer::Token>::const_iterator begin,
                 std::vector<Lexer::Token>::const_iterator anEnd,
                 Action action);

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        Action getAction() const;
    };

    class Message
    {
        std::string m_message;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_begin;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_end;
        std::optional<Modifier> m_modifier;

    public:

        struct Note
        {
            std::string message;
            std::vector<OpenCL::Lexer::Token>::const_iterator begin;
            std::vector<OpenCL::Lexer::Token>::const_iterator end;
            std::optional<Modifier> modifier;
        };

    private:

        std::vector<Note> m_notes;

    public:

        enum Severity
        {
            Error,
            Warning
        };

        Message(std::string message,
                std::vector<Lexer::Token>::const_iterator begin = {},
                std::vector<Lexer::Token>::const_iterator end = {},
                std::optional<Modifier> modifier = {},
                std::vector<Note> notes = {});

        const std::string& getMessage() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        const std::vector<Note>& getNotes() const;

        const std::optional<Modifier>& getModifier() const;
    };

    std::ostream& operator<<(std::ostream& os, const Message& message);

    std::vector<Lexer::Token>::const_iterator findEOL(std::vector<Lexer::Token>::const_iterator begin,
                                                      std::vector<Lexer::Token>::const_iterator end);

    std::vector<Lexer::Token>::const_iterator findSemicolon(std::vector<Lexer::Token>::const_iterator begin,
                                                            std::vector<Lexer::Token>::const_iterator end);

    std::vector<Lexer::Token>::const_iterator findSemicolonOrEOL(std::vector<Lexer::Token>::const_iterator begin,
                                                                 std::vector<Lexer::Token>::const_iterator end);
}

#endif //OPENCLPARSER_MESSAGE_HPP

#include <utility>

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

        [[nodiscard]] std::string format(std::vector<std::string> args) const;

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
            PointAtEnd,
            InsertAtEnd
        };

    private:

        Action m_action;
        std::string m_actionArgument;

    public:

        Modifier(std::vector<Lexer::Token>::const_iterator begin,
                 std::vector<Lexer::Token>::const_iterator anEnd,
                 Action action, std::string actionArgument = {});

        [[nodiscard]] const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        [[nodiscard]] const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        [[nodiscard]] Action getAction() const;

        [[nodiscard]] const std::string& getActionArgument() const;
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
                std::vector<Lexer::Token>::const_iterator begin,
                std::vector<Lexer::Token>::const_iterator end,
                std::optional<Modifier> modifier = {},
                std::vector<Note> notes = {});

        [[nodiscard]] const std::string& getMessage() const;

        [[nodiscard]] const std::vector<OpenCL::Lexer::Token>::const_iterator& getBegin() const;

        [[nodiscard]] const std::vector<OpenCL::Lexer::Token>::const_iterator& getAnEnd() const;

        [[nodiscard]] const std::vector<Note>& getNotes() const;

        [[nodiscard]] const std::optional<Modifier>& getModifier() const;
    };

    std::ostream& operator<<(std::ostream& os, const Message& message);

    std::vector<Lexer::Token>::const_iterator findEOL(std::vector<Lexer::Token>::const_iterator begin,
                                                      std::vector<Lexer::Token>::const_iterator end);

    template <class F>
    std::enable_if_t<!std::is_same_v<std::decay_t<F>, Lexer::TokenType>,
                     std::vector<OpenCL::Lexer::Token>::const_iterator> findEOLor(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                                                                  std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                                                  F&& functor)
    {
        for (auto curr = begin; curr != end; curr++)
        {
            if (curr->getLine() != begin->getLine() || functor(*curr))
            {
                return curr;
            }
        }
        return end;
    }

    std::vector<OpenCL::Lexer::Token>::const_iterator findEOLor(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                                                std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                                Lexer::TokenType tokenType);

    std::vector<Lexer::Token>::const_iterator findSemicolon(std::vector<Lexer::Token>::const_iterator begin,
                                                            std::vector<Lexer::Token>::const_iterator end);

    std::vector<Lexer::Token>::const_iterator findSemicolonOrEOL(std::vector<Lexer::Token>::const_iterator begin,
                                                                 std::vector<Lexer::Token>::const_iterator end);
}

#endif //OPENCLPARSER_MESSAGE_HPP

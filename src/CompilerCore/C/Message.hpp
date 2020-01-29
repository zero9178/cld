#ifndef OPENCLPARSER_MESSAGE_HPP
#define OPENCLPARSER_MESSAGE_HPP

#include <optional>
#include <string>
#include <utility>

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
            if constexpr (std::is_convertible_v<T, std::string>)
            {
                return value;
            }
            else if constexpr (canStaticCast<T, std::string>{})
            {
                return static_cast<std::string>(value);
            }
            else if constexpr (std::is_same_v<T, char>)
            {
                return std::string(1, value);
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
                : m_delimiter(std::move(delimiter)),
                  m_lastInbetween(std::move(lastInbetween)),
                  m_strings(std::move(args))
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

    class Modifier final
    {
    public:
        enum Action : std::uint8_t
        {
            Underline,
            PointAtBeginning,
            PointAtEnd,
            InsertAtEnd
        };

    private:
        std::string m_actionArgument;
        std::vector<Lexer::Token>::const_iterator m_begin;
        std::vector<Lexer::Token>::const_iterator m_end;
        Action m_action;

    public:
        Modifier(std::vector<Lexer::Token>::const_iterator begin, std::vector<Lexer::Token>::const_iterator anEnd,
                 Action action = Underline, std::string actionArgument = {});

        [[nodiscard]] std::vector<OpenCL::Lexer::Token>::const_iterator begin() const;

        [[nodiscard]] std::vector<OpenCL::Lexer::Token>::const_iterator end() const;

        [[nodiscard]] Action getAction() const;

        [[nodiscard]] const std::string& getActionArgument() const;
    };

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
        std::optional<Modifier> m_modifier;
        std::string m_message;
        std::vector<OpenCL::Lexer::Token>::const_iterator m_begin;
        std::optional<std::vector<OpenCL::Lexer::Token>::const_iterator> m_rangeEnd;
        Severity m_severity;

        Message(Severity severity, std::string message, std::vector<Lexer::Token>::const_iterator begin,
                std::optional<std::vector<OpenCL::Lexer::Token>::const_iterator> rangeEnd = {},
                std::optional<Modifier> modifier = {});

    public:
        /**
         * Generates an error Message
         * @param message Message to be printed
         * @param token Token that is used for the source location to be printed. The source code displayed is
         * guaranteed to display all lines that this token is contained in. Can be end
         * @param modifier optional modifier to apply
         * @return Message object
         */
        static Message error(std::string message, std::vector<Lexer::Token>::const_iterator token,
                             std::optional<Modifier> modifier = {});

        /**
         * Generates an error Message
         * @param message Message to be printed
         * @param begin Token that is used for the source location to be printed. The source code displayed is
         * guaranteed to display all lines from begin until exclusive end.
         * @param end Exclusive end of all tokens whose lines need to be printed
         * @param modifier optional modifier to apply
         * @return Message object
         */
        static Message error(std::string message, std::vector<Lexer::Token>::const_iterator begin,
                             std::vector<Lexer::Token>::const_iterator end, std::optional<Modifier> modifier = {});

        /**
         * Generates a note Message
         * @param message Message to be printed
         * @param token Token that is used for the source location to be printed. The source code displayed is
         * guaranteed to display all lines that this token is contained in. Can be end
         * @param modifier optional modifier to apply
         * @return Message object
         */
        static Message note(std::string message, std::vector<Lexer::Token>::const_iterator token,
                            std::optional<Modifier> modifier = {});

        /**
         * Generates a note Message
         * @param message Message to be printed
         * @param begin Token that is used for the source location to be printed. The source code displayed is
         * guaranteed to display all lines from begin until exclusive end.
         * @param end Exclusive end of all tokens whose lines need to be printed
         * @param modifier optional modifier to apply
         * @return Message object
         */
        static Message note(std::string message, std::vector<Lexer::Token>::const_iterator begin,
                            std::vector<Lexer::Token>::const_iterator end, std::optional<Modifier> modifier = {});

        /**
         * Generates a warning Message
         * @param message Message to be printed
         * @param token Token that is used for the source location to be printed. The source code displayed is
         * guaranteed to display all lines that this token is contained in. Can be end
         * @param modifier optional modifier to apply
         * @return Message object
         */
        static Message warning(std::string message, std::vector<Lexer::Token>::const_iterator token,
                               std::optional<Modifier> modifier = {});

        /**
         * Generates a warning Message
         * @param message Message to be printed
         * @param begin Token that is used for the source location to be printed. The source code displayed is
         * guaranteed to display all lines from begin until exclusive end.
         * @param end Exclusive end of all tokens whose lines need to be printed
         * @param modifier optional modifier to apply
         * @return Message object
         */
        static Message warning(std::string message, std::vector<Lexer::Token>::const_iterator begin,
                               std::vector<Lexer::Token>::const_iterator end, std::optional<Modifier> modifier = {});

        [[nodiscard]] Severity getSeverity() const;

        llvm::raw_ostream& print(llvm::raw_ostream& os, const SourceObject& sourceObject) const;
    };
} // namespace OpenCL

#endif // OPENCLPARSER_MESSAGE_HPP

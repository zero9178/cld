#ifndef OPENCLPARSER_PARSERRESULT_HPP
#define OPENCLPARSER_PARSERRESULT_HPP
#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-forwarding-reference-overload"

#include <type_traits>

namespace OpenCL::Parser
{
    struct Error
    {
    };

    /**
     * This class is a drop in replacement for std::optional as return type for the parsing functions
     * The advantage of this class compared to std::optional in this use case is that it signifies if the error came
     * from the callee or if the error propagated from a callee of the callee. This is important for panic mode recovery
     * @tparam T Value type
     */
    template <class T>
    class ParserResult
    {
        struct PropagatedError
        {
        };
        using variant = std::variant<PropagatedError, Error, T>;
        variant m_variant;

        // Can't do static constexpr bool because MSVC doesn't like it :(
        template <class U>
        using isSelf = std::is_same<std::remove_cv_t<std::remove_reference_t<U>>, ParserResult>;

        template <class U>
        static constexpr bool notError = !std::is_same_v<Error, std::remove_cv_t<std::remove_reference_t<U>>>;

    public:
        ParserResult() = default;

        ParserResult(Error) : m_variant(Error{}) {}

        template <class U = T,
                  std::enable_if_t<!isSelf<U>{} && notError<U> && !std::is_convertible_v<U&&, T>, bool> = true>
        constexpr explicit ParserResult(U&& value) : m_variant(std::forward<U>(value))
        {
        }

        template <class U = T,
                  std::enable_if_t<!isSelf<U>{} && notError<U> && std::is_convertible_v<U&&, T>, bool> = true>
        constexpr ParserResult(U&& value) : m_variant(std::forward<U>(value))
        {
        }

        template <class U>
        constexpr ParserResult(const ParserResult<U>& rhs) noexcept
            : m_variant(rhs.isError() ? variant{Error{}} : variant{})
        {
        }

        template <class U>
        constexpr ParserResult(ParserResult<U>&& rhs) noexcept : m_variant(rhs.isError() ? variant{Error{}} : variant{})
        {
        }

        constexpr const T* operator->() const noexcept
        {
            return std::get_if<T>(&m_variant);
        }

        constexpr T* operator->() noexcept
        {
            return std::get_if<T>(&m_variant);
        }

        constexpr const T& operator*() const& noexcept
        {
            return *std::get_if<T>(&m_variant);
        }

        constexpr T& operator*() & noexcept
        {
            return *std::get_if<T>(&m_variant);
        }

        constexpr const T&& operator*() const&& noexcept
        {
            return *std::get_if<T>(&m_variant);
        }

        constexpr T&& operator*() && noexcept
        {
            return *std::get_if<T>(&m_variant);
        }

        constexpr explicit operator bool() const noexcept
        {
            return std::holds_alternative<T>(m_variant);
        }

        [[nodiscard]] constexpr bool isPropagated() const noexcept
        {
            return std::holds_alternative<PropagatedError>(m_variant);
        }

        [[nodiscard]] constexpr bool isError() const noexcept
        {
            return std::holds_alternative<Error>(m_variant);
        }

        template <class U>
        constexpr T value_or(U&& defaultValue) const&
        {
            return *this ? **this : static_cast<T>(std::forward<U>(defaultValue));
        }

        template <class U>
        constexpr T value_or(U&& defaultValue) &&
        {
            return *this ? std::move(**this) : static_cast<T>(std::forward<U>(defaultValue));
        }
    };
} // namespace OpenCL::Parser

#pragma clang diagnostic pop

#endif // OPENCLPARSER_PARSERRESULT_HPP

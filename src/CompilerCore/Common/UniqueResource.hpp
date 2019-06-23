#ifndef OPENCLPARSER_UNIQUERESOURCE_HPP
#define OPENCLPARSER_UNIQUERESOURCE_HPP

#include <functional>

namespace OpenCL
{
    class UniqueResource
    {
        std::function<void()> m_function;

    public:

        explicit UniqueResource(std::function<void()>&& function) : m_function(std::move(function))
        {}

        ~UniqueResource()
        {
            m_function();
        }

        UniqueResource(UniqueResource&&) noexcept = default;

        UniqueResource& operator=(UniqueResource&&) noexcept = default;

        UniqueResource(const UniqueResource&) = delete;

        UniqueResource& operator=(const UniqueResource&) = delete;
    };
}

#endif //OPENCLPARSER_UNIQUERESOURCE_HPP

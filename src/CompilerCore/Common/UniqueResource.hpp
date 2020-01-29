#ifndef OPENCLPARSER_UNIQUERESOURCE_HPP
#define OPENCLPARSER_UNIQUERESOURCE_HPP

namespace OpenCL
{
template <class F>
class UniqueResource
{
    F m_function;

public:
    explicit UniqueResource(F&& function) : m_function(std::move(function)) {}

    ~UniqueResource()
    {
        m_function();
    }

    UniqueResource(UniqueResource&&) noexcept = default;

    UniqueResource& operator=(UniqueResource&&) noexcept = default;

    UniqueResource(const UniqueResource&) = delete;

    UniqueResource& operator=(const UniqueResource&) = delete;
};
} // namespace OpenCL

#endif // OPENCLPARSER_UNIQUERESOURCE_HPP

#include "TestConfig.hpp"

#include <llvm/ADT/ScopeExit.h>

cld::Tests::FileScope::~FileScope()
{
    cld::fs::remove(m_path);
}

cld::Tests::FileScope cld::Tests::createInclude(std::string_view path, std::string_view content)
{
    auto dir = cld::fs::path(path.data());
    dir.remove_filename();
    if (!dir.empty())
    {
        REQUIRE_FALSE(cld::fs::create_directories(dir));
    }
    auto utf8path = cld::fs::path(path);
    cld::fs::ofstream file(utf8path, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    REQUIRE(file.is_open());
    file.write(content.data(), content.size());
    return FileScope(utf8path);
}

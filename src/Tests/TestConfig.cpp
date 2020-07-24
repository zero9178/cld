#include "TestConfig.hpp"

#include <llvm/ADT/ScopeExit.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>

cld::Tests::FileScope::~FileScope()
{
    REQUIRE_FALSE(llvm::sys::fs::remove(m_path, true));
}

cld::Tests::FileScope cld::Tests::createInclude(std::string_view path, std::string_view content)
{
    llvm::SmallString<50> dir{path.data()};
    llvm::sys::path::remove_filename(dir);
    if (!dir.empty())
    {
        REQUIRE_FALSE(llvm::sys::fs::create_directories(dir));
    }
    int fd;
    REQUIRE_FALSE(llvm::sys::fs::openFileForWrite(path.data(), fd));
    llvm::raw_fd_ostream ss(fd, true);
    ss.write(content.data(), content.size());
    return FileScope(path);
}

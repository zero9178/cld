#include "TestConfig.hpp"

#include <llvm/Support/Process.h>

cld::Tests::FileScope::~FileScope()
{
    cld::fs::remove(m_path);
}

cld::Tests::FileScope cld::Tests::createInclude(std::string_view path, std::string_view content)
{
    auto dir = cld::fs::u8path(path.data());
    dir.remove_filename();
    if (!dir.empty())
    {
        cld::fs::create_directories(dir);
    }
    auto utf8path = cld::fs::u8path(path);
    cld::fs::ofstream file(utf8path, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    REQUIRE(file.is_open());
    file.write(content.data(), content.size());
    return FileScope(utf8path);
}

EndTest cld::Tests::enableParallelization()
{
    auto id = llvm::sys::Process::getProcessId();
    auto path = cld::fs::current_path() / std::to_string(id);
    cld::fs::create_directory(path);
    cld::fs::current_path(path);
    return EndTest{path};
}

EndTest::~EndTest()
{
    cld::fs::current_path(path / "..");
    cld::fs::remove_all(path);
}

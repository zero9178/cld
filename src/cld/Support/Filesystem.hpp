#pragma once

#if defined(_WIN32) && !defined(_MSC_VER)
    #define GHC_FILESYSTEM_FWD
    #define GHC_WIN_WSTRING_STRING_TYPE
    #include <ghc/filesystem.hpp>
namespace cld::fs
{
using namespace ghc::filesystem;
using ifstream = ghc::filesystem::ifstream;
using ofstream = ghc::filesystem::ofstream;
using fstream = ghc::filesystem::fstream;
} // namespace cld::fs

#else

    #include <filesystem>
    #include <fstream>
namespace cld::fs
{
using namespace std::filesystem;

using ifstream = std::ifstream;
using ofstream = std::ofstream;
using fstream = std::fstream;
} // namespace cld::fs
#endif

namespace cld
{
// Using a template avoids an ambiguous function call with to_string(std::string_view)
template <class = void>
std::string to_string(const cld::fs::path& path)
{
    auto u8string = path.u8string();
    std::string result(u8string.size(), '\0');
    std::copy(u8string.begin(), u8string.end(), result.begin());
    return result;
}

namespace fs
{
inline cld::fs::path to_path(std::string_view view)
{
    std::u8string string(view.size(), u8'\0');
    std::copy(view.begin(), view.end(), string.begin());
    return string;
}
} // namespace fs

} // namespace cld

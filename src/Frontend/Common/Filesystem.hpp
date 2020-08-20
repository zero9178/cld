#pragma once

#include <ciso646>

#if defined(_WIN32) && defined(__clang__) && defined(_LIBCPP_VERSION)
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

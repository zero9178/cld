//---------------------------------------------------------------------------------------
//
// ghc::filesystem - A C++17-like filesystem implementation for C++11/C++14/C++17/C++20
//
//---------------------------------------------------------------------------------------
//
// Copyright (c) 2018, Steffen Schümann <s.schuemann@pobox.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
//---------------------------------------------------------------------------------------
//
// To dynamically select std::filesystem where available on most platforms,
// you could use:
//
// #if ((defined(_MSVC_LANG) && _MSVC_LANG >= 201703L) || (defined(__cplusplus) && __cplusplus >= 201703L)) &&
// defined(__has_include) #if __has_include(<filesystem>) && (!defined(__MAC_OS_X_VERSION_MIN_REQUIRED) ||
// __MAC_OS_X_VERSION_MIN_REQUIRED >= 101500) #define GHC_USE_STD_FS #include <filesystem> namespace fs =
// std::filesystem; #endif #endif #ifndef GHC_USE_STD_FS #include <ghc/filesystem.hpp> namespace fs = ghc::filesystem;
// #endif
//
//---------------------------------------------------------------------------------------
#ifndef GHC_FILESYSTEM_H
    #define GHC_FILESYSTEM_H

    // #define BSD manifest constant only in
    // sys/param.h
    #ifndef _WIN32
        #include <sys/param.h>
    #endif

    #ifndef GHC_OS_DETECTED
        #if defined(__APPLE__) && defined(__MACH__)
            #define GHC_OS_MACOS
        #elif defined(__linux__)
            #define GHC_OS_LINUX
            #if defined(__ANDROID__)
                #define GHC_OS_ANDROID
            #endif
        #elif defined(_WIN64)
            #define GHC_OS_WINDOWS
            #define GHC_OS_WIN64
        #elif defined(_WIN32)
            #define GHC_OS_WINDOWS
            #define GHC_OS_WIN32
        #elif defined(__CYGWIN__)
            #define GHC_OS_CYGWIN
        #elif defined(__svr4__)
            #define GHC_OS_SYS5R4
        #elif defined(BSD)
            #define GHC_OS_BSD
        #elif defined(__EMSCRIPTEN__)
            #define GHC_OS_WEB
            #include <wasi/api.h>
        #else
            #error "Operating system currently not supported!"
        #endif
        #define GHC_OS_DETECTED
        #if (defined(_MSVC_LANG) && _MSVC_LANG >= 201703L)
            #if _MSVC_LANG == 201703L
                #define GHC_FILESYSTEM_RUNNING_CPP17
            #else
                #define GHC_FILESYSTEM_RUNNING_CPP20
            #endif
        #elif (defined(__cplusplus) && __cplusplus >= 201703L)
            #if __cplusplus == 201703L
                #define GHC_FILESYSTEM_RUNNING_CPP17
            #else
                #define GHC_FILESYSTEM_RUNNING_CPP20
            #endif
        #endif
    #endif

    #if defined(GHC_FILESYSTEM_IMPLEMENTATION)
        #define GHC_EXPAND_IMPL
        #define GHC_INLINE
        #ifdef GHC_OS_WINDOWS
            #ifndef GHC_FS_API
                #define GHC_FS_API
            #endif
            #ifndef GHC_FS_API_CLASS
                #define GHC_FS_API_CLASS
            #endif
        #else
            #ifndef GHC_FS_API
                #define GHC_FS_API __attribute__((visibility("default")))
            #endif
            #ifndef GHC_FS_API_CLASS
                #define GHC_FS_API_CLASS __attribute__((visibility("default")))
            #endif
        #endif
    #elif defined(GHC_FILESYSTEM_FWD)
        #define GHC_INLINE
        #ifdef GHC_OS_WINDOWS
            #ifndef GHC_FS_API
                #define GHC_FS_API extern
            #endif
            #ifndef GHC_FS_API_CLASS
                #define GHC_FS_API_CLASS
            #endif
        #else
            #ifndef GHC_FS_API
                #define GHC_FS_API extern
            #endif
            #ifndef GHC_FS_API_CLASS
                #define GHC_FS_API_CLASS
            #endif
        #endif
    #else
        #define GHC_EXPAND_IMPL
        #define GHC_INLINE inline
        #ifndef GHC_FS_API
            #define GHC_FS_API
        #endif
        #ifndef GHC_FS_API_CLASS
            #define GHC_FS_API_CLASS
        #endif
    #endif

    #ifdef GHC_EXPAND_IMPL

        #ifdef GHC_OS_WINDOWS
            #include <windows.h>
            // additional includes
            #include <shellapi.h>
            #include <sys/stat.h>
            #include <sys/types.h>
            #include <wchar.h>
            #include <winioctl.h>
        #else
            #include <dirent.h>
            #include <fcntl.h>
            #include <limits.h>
            #include <sys/param.h>
            #include <sys/stat.h>
            #include <sys/time.h>
            #include <sys/types.h>
            #include <unistd.h>
            #ifdef GHC_OS_ANDROID
                #include <android/api-level.h>
                #if __ANDROID_API__ < 12
                    #include <sys/syscall.h>
                #endif
                #include <sys/vfs.h>
                #define statvfs statfs
            #else
                #include <sys/statvfs.h>
            #endif
            #ifdef GHC_OS_CYGWIN
                #include <strings.h>
            #endif
            #if !defined(__ANDROID__) || __ANDROID_API__ >= 26
                #include <langinfo.h>
            #endif
        #endif
        #ifdef GHC_OS_MACOS
            #include <Availability.h>
        #endif

        #if defined(__cpp_impl_three_way_comparison) && defined(__has_include)
            #if __has_include(<compare>)
                #define GHC_HAS_THREEWAY_COMP
                #include <compare>
            #endif
        #endif

        #include <algorithm>
        #include <cctype>
        #include <chrono>
        #include <clocale>
        #include <cstdlib>
        #include <cstring>
        #include <fstream>
        #include <functional>
        #include <memory>
        #include <stack>
        #include <stdexcept>
        #include <string>
        #include <system_error>
        #include <type_traits>
        #include <utility>
        #include <vector>

    #else // GHC_EXPAND_IMPL

        #if defined(__cpp_impl_three_way_comparison) && defined(__has_include)
            #if __has_include(<compare>)
                #define GHC_HAS_THREEWAY_COMP
                #include <compare>
            #endif
        #endif
        #include <chrono>
        #include <fstream>
        #include <memory>
        #include <stack>
        #include <stdexcept>
        #include <string>
        #include <system_error>
        #ifdef GHC_OS_WINDOWS
            #include <vector>
        #endif
    #endif // GHC_EXPAND_IMPL

    // After standard library includes.
    // Standard library support for std::string_view.
    #if defined(__cpp_lib_string_view)
        #define GHC_HAS_STD_STRING_VIEW
    #elif defined(_LIBCPP_VERSION) && (_LIBCPP_VERSION >= 4000) && (__cplusplus >= 201402)
        #define GHC_HAS_STD_STRING_VIEW
    #elif defined(_GLIBCXX_RELEASE) && (_GLIBCXX_RELEASE >= 7) && (__cplusplus >= 201703)
        #define GHC_HAS_STD_STRING_VIEW
    #elif defined(_MSC_VER) && (_MSC_VER >= 1910 && _MSVC_LANG >= 201703)
        #define GHC_HAS_STD_STRING_VIEW
    #endif

    // Standard library support for std::experimental::string_view.
    #if defined(_LIBCPP_VERSION) && (_LIBCPP_VERSION >= 3700 && _LIBCPP_VERSION < 7000) && (__cplusplus >= 201402)
        #define GHC_HAS_STD_EXPERIMENTAL_STRING_VIEW
    #elif defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 9)) || (__GNUC__ > 4)) && (__cplusplus >= 201402)
        #define GHC_HAS_STD_EXPERIMENTAL_STRING_VIEW
    #endif

    #if defined(GHC_HAS_STD_STRING_VIEW)
        #include <string_view>
    #elif defined(GHC_HAS_STD_EXPERIMENTAL_STRING_VIEW)
        #include <experimental/string_view>
    #endif

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Behaviour Switches (see README.md, should match the config in test/filesystem_test.cpp):
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Enforce C++17 API where possible when compiling for C++20, handles the following cases:
    // * fs::path::u8string() returns std::string instead of std::u8string
    // #define GHC_FILESYSTEM_ENFORCE_CPP17_API
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // LWG #2682 disables the since then invalid use of the copy option create_symlinks on directories
    // configure LWG conformance ()
    #define LWG_2682_BEHAVIOUR
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // LWG #2395 makes crate_directory/create_directories not emit an error if there is a regular
    // file with that name, it is superseded by P1164R1, so only activate if really needed
    // #define LWG_2935_BEHAVIOUR
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // LWG #2936 enables new element wise (more expensive) path comparison
    // * if this->root_name().native().compare(p.root_name().native()) != 0 return result
    // * if this->has_root_directory() and !p.has_root_directory() return -1
    // * if !this->has_root_directory() and p.has_root_directory() return -1
    // * else result of element wise comparison of path iteration where first comparison is != 0 or 0
    //   if all comparisons are 0 (on Windows this implementation does case insensitive root_name()
    //   comparison)
    #define LWG_2936_BEHAVIOUR
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // LWG #2937 enforces that fs::equivalent emits an error, if !fs::exists(p1)||!exists(p2)
    #define LWG_2937_BEHAVIOUR
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // UTF8-Everywhere is the original behaviour of ghc::filesystem. But since v1.5 the windows
    // version defaults to std::wstring storage backend. Still all std::string will be interpreted
    // as UTF-8 encoded. With this define you can enfoce the old behavior on Windows, using
    // std::string as backend and for fs::path::native() and char for fs::path::c_str(). This
    // needs more conversions so it is (an was before v1.5) slower, bot might help keeping source
    // homogeneous in a multi platform project.
    // #define GHC_WIN_DISABLE_WSTRING_STORAGE_TYPE
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Raise errors/exceptions when invalid unicode codepoints or UTF-8 sequences are found,
    // instead of replacing them with the unicode replacement character (U+FFFD).
    // #define GHC_RAISE_UNICODE_ERRORS
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Automatic prefix windows path with "\\?\" if they would break the MAX_PATH length.
    // instead of replacing them with the unicode replacement character (U+FFFD).
    #ifndef GHC_WIN_DISABLE_AUTO_PREFIXES
        #define GHC_WIN_AUTO_PREFIX_LONG_PATH
    #endif // GHC_WIN_DISABLE_AUTO_PREFIXES
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // ghc::filesystem version in decimal (major * 10000 + minor * 100 + patch)
    #define GHC_FILESYSTEM_VERSION 10504L

    #if !defined(GHC_WITH_EXCEPTIONS) && (defined(__EXCEPTIONS) || defined(__cpp_exceptions) || defined(_CPPUNWIND))
        #define GHC_WITH_EXCEPTIONS
    #endif
    #if !defined(GHC_WITH_EXCEPTIONS) && defined(GHC_RAISE_UNICODE_ERRORS)
        #error "Can't raise unicode errors with exception support disabled"
    #endif

namespace ghc
{
namespace filesystem
{
    #if defined(GHC_HAS_CUSTOM_STRING_VIEW)
        #define GHC_WITH_STRING_VIEW
    #elif defined(GHC_HAS_STD_STRING_VIEW)
        #define GHC_WITH_STRING_VIEW
using std::basic_string_view;
    #elif defined(GHC_HAS_STD_EXPERIMENTAL_STRING_VIEW)
        #define GHC_WITH_STRING_VIEW
using std::experimental::basic_string_view;
    #endif

// temporary existing exception type for yet unimplemented parts
class GHC_FS_API_CLASS not_implemented_exception : public std::logic_error
{
public:
    not_implemented_exception() : std::logic_error("function not implemented yet.") {}
};

template <typename char_type>
class path_helper_base
{
public:
    using value_type = char_type;
    #ifdef GHC_OS_WINDOWS
    static constexpr value_type preferred_separator = '\\';
    #else
    static constexpr value_type preferred_separator = '/';
    #endif
};

    #if __cplusplus < 201703L
template <typename char_type>
constexpr char_type path_helper_base<char_type>::preferred_separator;
    #endif

    #ifdef GHC_OS_WINDOWS
class path;
namespace detail
{
bool has_executable_extension(const path& p);
}
    #endif

// 30.10.8 class path
class GHC_FS_API_CLASS path
    #if defined(GHC_OS_WINDOWS) && !defined(GHC_WIN_DISABLE_WSTRING_STORAGE_TYPE)
        #define GHC_USE_WCHAR_T
        #define GHC_NATIVEWP(p) p.c_str()
        #define GHC_PLATFORM_LITERAL(str) L##str
    : private path_helper_base<std::wstring::value_type>
{
public:
    using path_helper_base<std::wstring::value_type>::value_type;
    #else
        #define GHC_NATIVEWP(p) p.wstring().c_str()
        #define GHC_PLATFORM_LITERAL(str) str
    : private path_helper_base<std::string::value_type>
{
public:
    using path_helper_base<std::string::value_type>::value_type;
    #endif
    using string_type = std::basic_string<value_type>;
    using path_helper_base<value_type>::preferred_separator;

    // 30.10.10.1 enumeration format
    /// The path format in which the constructor argument is given.
    enum format
    {
        generic_format, ///< The generic format, internally used by
        ///< ghc::filesystem::path with slashes
        native_format, ///< The format native to the current platform this code
        ///< is build for
        auto_format, ///< Try to auto-detect the format, fallback to native
    };

    template <class T>
    struct _is_basic_string : std::false_type
    {
    };
    template <class CharT, class Traits, class Alloc>
    struct _is_basic_string<std::basic_string<CharT, Traits, Alloc>> : std::true_type
    {
    };
    template <class CharT>
    struct _is_basic_string<std::basic_string<CharT, std::char_traits<CharT>, std::allocator<CharT>>> : std::true_type
    {
    };
    #ifdef GHC_WITH_STRING_VIEW
    template <class CharT, class Traits>
    struct _is_basic_string<basic_string_view<CharT, Traits>> : std::true_type
    {
    };
    template <class CharT>
    struct _is_basic_string<basic_string_view<CharT, std::char_traits<CharT>>> : std::true_type
    {
    };
    #endif

    template <typename T1, typename T2 = void>
    using path_type = typename std::enable_if<!std::is_same<path, T1>::value, path>::type;
    template <typename T>
    using path_from_string =
        typename std::enable_if<_is_basic_string<T>::value
                                    || std::is_same<char const*, typename std::decay<T>::type>::value
                                    || std::is_same<char*, typename std::decay<T>::type>::value
                                    || std::is_same<wchar_t const*, typename std::decay<T>::type>::value
                                    || std::is_same<wchar_t*, typename std::decay<T>::type>::value,
                                path>::type;
    template <typename T>
    using path_type_EcharT =
        typename std::enable_if<std::is_same<T, char>::value || std::is_same<T, char16_t>::value
                                    || std::is_same<T, char32_t>::value || std::is_same<T, wchar_t>::value,
                                path>::type;
    // 30.10.8.4.1 constructors and destructor
    path() noexcept;
    path(const path& p);
    path(path&& p) noexcept;
    path(string_type&& source, format fmt = auto_format);
    template <class Source, typename = path_from_string<Source>>
    path(const Source& source, format fmt = auto_format);
    template <class InputIterator>
    path(InputIterator first, InputIterator last, format fmt = auto_format);
    #ifdef GHC_WITH_EXCEPTIONS
    template <class Source, typename = path_from_string<Source>>
    path(const Source& source, const std::locale& loc, format fmt = auto_format);
    template <class InputIterator>
    path(InputIterator first, InputIterator last, const std::locale& loc, format fmt = auto_format);
    #endif
    ~path();

    // 30.10.8.4.2 assignments
    path& operator=(const path& p);
    path& operator=(path&& p) noexcept;
    path& operator=(string_type&& source);
    path& assign(string_type&& source);
    template <class Source>
    path& operator=(const Source& source);
    template <class Source>
    path& assign(const Source& source);
    template <class InputIterator>
    path& assign(InputIterator first, InputIterator last);

    // 30.10.8.4.3 appends
    path& operator/=(const path& p);
    template <class Source>
    path& operator/=(const Source& source);
    template <class Source>
    path& append(const Source& source);
    template <class InputIterator>
    path& append(InputIterator first, InputIterator last);

    // 30.10.8.4.4 concatenation
    path& operator+=(const path& x);
    path& operator+=(const string_type& x);
    #ifdef GHC_WITH_STRING_VIEW
    path& operator+=(basic_string_view<value_type> x);
    #endif
    path& operator+=(const value_type* x);
    path& operator+=(value_type x);
    template <class Source>
    path_from_string<Source>& operator+=(const Source& x);
    template <class EcharT>
    path_type_EcharT<EcharT>& operator+=(EcharT x);
    template <class Source>
    path& concat(const Source& x);
    template <class InputIterator>
    path& concat(InputIterator first, InputIterator last);

    // 30.10.8.4.5 modifiers
    void clear() noexcept;
    path& make_preferred();
    path& remove_filename();
    path& replace_filename(const path& replacement);
    path& replace_extension(const path& replacement = path());
    void swap(path& rhs) noexcept;

    // 30.10.8.4.6 native format observers
    const string_type& native() const noexcept;
    const value_type* c_str() const noexcept;
    operator string_type() const;
    template <class EcharT, class traits = std::char_traits<EcharT>, class Allocator = std::allocator<EcharT>>
    std::basic_string<EcharT, traits, Allocator> string(const Allocator& a = Allocator()) const;
    std::string string() const;
    std::wstring wstring() const;
    #if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
    std::u8string u8string() const;
    #else
    std::string u8string() const;
    #endif
    std::u16string u16string() const;
    std::u32string u32string() const;

    // 30.10.8.4.7 generic format observers
    template <class EcharT, class traits = std::char_traits<EcharT>, class Allocator = std::allocator<EcharT>>
    std::basic_string<EcharT, traits, Allocator> generic_string(const Allocator& a = Allocator()) const;
    std::string generic_string() const;
    std::wstring generic_wstring() const;
    #if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
    std::u8string generic_u8string() const;
    #else
    std::string generic_u8string() const;
    #endif
    std::u16string generic_u16string() const;
    std::u32string generic_u32string() const;

    // 30.10.8.4.8 compare
    int compare(const path& p) const noexcept;
    int compare(const string_type& s) const;
    #ifdef GHC_WITH_STRING_VIEW
    int compare(basic_string_view<value_type> s) const;
    #endif
    int compare(const value_type* s) const;

    // 30.10.8.4.9 decomposition
    path root_name() const;
    path root_directory() const;
    path root_path() const;
    path relative_path() const;
    path parent_path() const;
    path filename() const;
    path stem() const;
    path extension() const;

    // 30.10.8.4.10 query
    bool empty() const noexcept;
    bool has_root_name() const;
    bool has_root_directory() const;
    bool has_root_path() const;
    bool has_relative_path() const;
    bool has_parent_path() const;
    bool has_filename() const;
    bool has_stem() const;
    bool has_extension() const;
    bool is_absolute() const;
    bool is_relative() const;

    // 30.10.8.4.11 generation
    path lexically_normal() const;
    path lexically_relative(const path& base) const;
    path lexically_proximate(const path& base) const;

    // 30.10.8.5 iterators
    class iterator;
    using const_iterator = iterator;
    iterator begin() const;
    iterator end() const;

private:
    using impl_value_type = value_type;
    using impl_string_type = std::basic_string<impl_value_type>;
    friend class directory_iterator;
    void append_name(const value_type* name);
    static constexpr impl_value_type generic_separator = '/';
    template <typename InputIterator>
    class input_iterator_range
    {
    public:
        typedef InputIterator iterator;
        typedef InputIterator const_iterator;
        typedef typename InputIterator::difference_type difference_type;

        input_iterator_range(const InputIterator& first, const InputIterator& last) : _first(first), _last(last) {}

        InputIterator begin() const
        {
            return _first;
        }
        InputIterator end() const
        {
            return _last;
        }

    private:
        InputIterator _first;
        InputIterator _last;
    };
    friend void swap(path& lhs, path& rhs) noexcept;
    friend size_t hash_value(const path& p) noexcept;
    friend path canonical(const path& p, std::error_code& ec);
    string_type::size_type root_name_length() const noexcept;
    void postprocess_path_with_format(format fmt);
    void check_long_path();
    impl_string_type _path;
    #ifdef GHC_OS_WINDOWS
    void handle_prefixes();
    friend bool detail::has_executable_extension(const path& p);
        #ifdef GHC_WIN_AUTO_PREFIX_LONG_PATH
    string_type::size_type _prefixLength{0};
        #else  // GHC_WIN_AUTO_PREFIX_LONG_PATH
    static const string_type::size_type _prefixLength{0};
        #endif // GHC_WIN_AUTO_PREFIX_LONG_PATH
    #else
    static const string_type::size_type _prefixLength{0};
    #endif
};

// 30.10.8.6 path non-member functions
GHC_FS_API void swap(path& lhs, path& rhs) noexcept;
GHC_FS_API size_t hash_value(const path& p) noexcept;
    #ifdef GHC_HAS_THREEWAY_COMP
GHC_FS_API std::strong_ordering operator<=>(const path& lhs, const path& rhs) noexcept;
    #endif
GHC_FS_API bool operator==(const path& lhs, const path& rhs) noexcept;
GHC_FS_API bool operator!=(const path& lhs, const path& rhs) noexcept;
GHC_FS_API bool operator<(const path& lhs, const path& rhs) noexcept;
GHC_FS_API bool operator<=(const path& lhs, const path& rhs) noexcept;
GHC_FS_API bool operator>(const path& lhs, const path& rhs) noexcept;
GHC_FS_API bool operator>=(const path& lhs, const path& rhs) noexcept;
GHC_FS_API path operator/(const path& lhs, const path& rhs);

// 30.10.8.6.1 path inserter and extractor
template <class charT, class traits>
std::basic_ostream<charT, traits>& operator<<(std::basic_ostream<charT, traits>& os, const path& p);
template <class charT, class traits>
std::basic_istream<charT, traits>& operator>>(std::basic_istream<charT, traits>& is, path& p);

// 30.10.8.6.2 path factory functions
template <class Source, typename = path::path_from_string<Source>>
    #if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
[[deprecated("use ghc::filesystem::path::path() with std::u8string instead")]]
    #endif
path u8path(const Source& source);
template <class InputIterator>
    #if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
[[deprecated("use ghc::filesystem::path::path() with std::u8string instead")]]
    #endif
path u8path(InputIterator first, InputIterator last);

// 30.10.9 class filesystem_error
class GHC_FS_API_CLASS filesystem_error : public std::system_error
{
public:
    filesystem_error(const std::string& what_arg, std::error_code ec);
    filesystem_error(const std::string& what_arg, const path& p1, std::error_code ec);
    filesystem_error(const std::string& what_arg, const path& p1, const path& p2, std::error_code ec);
    const path& path1() const noexcept;
    const path& path2() const noexcept;
    const char* what() const noexcept override;

private:
    std::string _what_arg;
    std::error_code _ec;
    path _p1, _p2;
};

class GHC_FS_API_CLASS path::iterator
{
public:
    using value_type = const path;
    using difference_type = std::ptrdiff_t;
    using pointer = const path*;
    using reference = const path&;
    using iterator_category = std::bidirectional_iterator_tag;

    iterator();
    iterator(const path& p, const impl_string_type::const_iterator& pos);
    iterator& operator++();
    iterator operator++(int);
    iterator& operator--();
    iterator operator--(int);
    bool operator==(const iterator& other) const;
    bool operator!=(const iterator& other) const;
    reference operator*() const;
    pointer operator->() const;

private:
    friend class path;
    impl_string_type::const_iterator increment(const impl_string_type::const_iterator& pos) const;
    impl_string_type::const_iterator decrement(const impl_string_type::const_iterator& pos) const;
    void updateCurrent();
    impl_string_type::const_iterator _first;
    impl_string_type::const_iterator _last;
    impl_string_type::const_iterator _prefix;
    impl_string_type::const_iterator _root;
    impl_string_type::const_iterator _iter;
    path _current;
};

struct space_info
{
    uintmax_t capacity;
    uintmax_t free;
    uintmax_t available;
};

// 30.10.10, enumerations
enum class file_type
{
    none,
    not_found,
    regular,
    directory,
    symlink,
    block,
    character,
    fifo,
    socket,
    unknown,
};

enum class perms : uint16_t
{
    none = 0,

    owner_read = 0400,
    owner_write = 0200,
    owner_exec = 0100,
    owner_all = 0700,

    group_read = 040,
    group_write = 020,
    group_exec = 010,
    group_all = 070,

    others_read = 04,
    others_write = 02,
    others_exec = 01,
    others_all = 07,

    all = 0777,
    set_uid = 04000,
    set_gid = 02000,
    sticky_bit = 01000,

    mask = 07777,
    unknown = 0xffff
};

enum class perm_options : uint16_t
{
    replace = 3,
    add = 1,
    remove = 2,
    nofollow = 4,
};

enum class copy_options : uint16_t
{
    none = 0,

    skip_existing = 1,
    overwrite_existing = 2,
    update_existing = 4,

    recursive = 8,

    copy_symlinks = 0x10,
    skip_symlinks = 0x20,

    directories_only = 0x40,
    create_symlinks = 0x80,
    #ifndef GHC_OS_WEB
    create_hard_links = 0x100
    #endif
};

enum class directory_options : uint16_t
{
    none = 0,
    follow_directory_symlink = 1,
    skip_permission_denied = 2,
};

// 30.10.11 class file_status
class GHC_FS_API_CLASS file_status
{
public:
    // 30.10.11.1 constructors and destructor
    file_status() noexcept;
    explicit file_status(file_type ft, perms prms = perms::unknown) noexcept;
    file_status(const file_status&) noexcept;
    file_status(file_status&&) noexcept;
    ~file_status();
    // assignments:
    file_status& operator=(const file_status&) noexcept;
    file_status& operator=(file_status&&) noexcept;
    // 30.10.11.3 modifiers
    void type(file_type ft) noexcept;
    void permissions(perms prms) noexcept;
    // 30.10.11.2 observers
    file_type type() const noexcept;
    perms permissions() const noexcept;
    friend bool operator==(const file_status& lhs, const file_status& rhs) noexcept
    {
        return lhs.type() == rhs.type() && lhs.permissions() == rhs.permissions();
    }

private:
    file_type _type;
    perms _perms;
};

using file_time_type = std::chrono::time_point<std::chrono::system_clock>;

// 30.10.12 Class directory_entry
class GHC_FS_API_CLASS directory_entry
{
public:
    // 30.10.12.1 constructors and destructor
    directory_entry() noexcept = default;
    directory_entry(const directory_entry&) = default;
    directory_entry(directory_entry&&) noexcept = default;
    #ifdef GHC_WITH_EXCEPTIONS
    explicit directory_entry(const path& p);
    #endif
    directory_entry(const path& p, std::error_code& ec);
    ~directory_entry();

    // assignments:
    directory_entry& operator=(const directory_entry&) = default;
    directory_entry& operator=(directory_entry&&) noexcept = default;

    // 30.10.12.2 modifiers
    #ifdef GHC_WITH_EXCEPTIONS
    void assign(const path& p);
    void replace_filename(const path& p);
    void refresh();
    #endif
    void assign(const path& p, std::error_code& ec);
    void replace_filename(const path& p, std::error_code& ec);
    void refresh(std::error_code& ec) noexcept;

    // 30.10.12.3 observers
    const filesystem::path& path() const noexcept;
    operator const filesystem::path&() const noexcept;
    #ifdef GHC_WITH_EXCEPTIONS
    bool exists() const;
    bool is_block_file() const;
    bool is_character_file() const;
    bool is_directory() const;
    bool is_fifo() const;
    bool is_other() const;
    bool is_regular_file() const;
    bool is_socket() const;
    bool is_symlink() const;
    uintmax_t file_size() const;
    file_time_type last_write_time() const;
    file_status status() const;
    file_status symlink_status() const;
    #endif
    bool exists(std::error_code& ec) const noexcept;
    bool is_block_file(std::error_code& ec) const noexcept;
    bool is_character_file(std::error_code& ec) const noexcept;
    bool is_directory(std::error_code& ec) const noexcept;
    bool is_fifo(std::error_code& ec) const noexcept;
    bool is_other(std::error_code& ec) const noexcept;
    bool is_regular_file(std::error_code& ec) const noexcept;
    bool is_socket(std::error_code& ec) const noexcept;
    bool is_symlink(std::error_code& ec) const noexcept;
    uintmax_t file_size(std::error_code& ec) const noexcept;
    file_time_type last_write_time(std::error_code& ec) const noexcept;
    file_status status(std::error_code& ec) const noexcept;
    file_status symlink_status(std::error_code& ec) const noexcept;

    #ifndef GHC_OS_WEB
        #ifdef GHC_WITH_EXCEPTIONS
    uintmax_t hard_link_count() const;
        #endif
    uintmax_t hard_link_count(std::error_code& ec) const noexcept;
    #endif

    #ifdef GHC_HAS_THREEWAY_COMP
    std::strong_ordering operator<=>(const directory_entry& rhs) const noexcept;
    #endif
    bool operator<(const directory_entry& rhs) const noexcept;
    bool operator==(const directory_entry& rhs) const noexcept;
    bool operator!=(const directory_entry& rhs) const noexcept;
    bool operator<=(const directory_entry& rhs) const noexcept;
    bool operator>(const directory_entry& rhs) const noexcept;
    bool operator>=(const directory_entry& rhs) const noexcept;

private:
    friend class directory_iterator;
    #ifdef GHC_WITH_EXCEPTIONS
    file_type status_file_type() const;
    #endif
    file_type status_file_type(std::error_code& ec) const noexcept;
    filesystem::path _path;
    file_status _status;
    file_status _symlink_status;
    uintmax_t _file_size = static_cast<uintmax_t>(-1);
    #ifndef GHC_OS_WINDOWS
    uintmax_t _hard_link_count = static_cast<uintmax_t>(-1);
    #endif
    time_t _last_write_time = 0;
};

// 30.10.13 Class directory_iterator
class GHC_FS_API_CLASS directory_iterator
{
public:
    class GHC_FS_API_CLASS proxy
    {
    public:
        const directory_entry& operator*() const& noexcept
        {
            return _dir_entry;
        }
        directory_entry operator*() && noexcept
        {
            return std::move(_dir_entry);
        }

    private:
        explicit proxy(const directory_entry& dir_entry) : _dir_entry(dir_entry) {}
        friend class directory_iterator;
        friend class recursive_directory_iterator;
        directory_entry _dir_entry;
    };
    using iterator_category = std::input_iterator_tag;
    using value_type = directory_entry;
    using difference_type = std::ptrdiff_t;
    using pointer = const directory_entry*;
    using reference = const directory_entry&;

    // 30.10.13.1 member functions
    directory_iterator() noexcept;
    #ifdef GHC_WITH_EXCEPTIONS
    explicit directory_iterator(const path& p);
    directory_iterator(const path& p, directory_options options);
    #endif
    directory_iterator(const path& p, std::error_code& ec) noexcept;
    directory_iterator(const path& p, directory_options options, std::error_code& ec) noexcept;
    directory_iterator(const directory_iterator& rhs);
    directory_iterator(directory_iterator&& rhs) noexcept;
    ~directory_iterator();
    directory_iterator& operator=(const directory_iterator& rhs);
    directory_iterator& operator=(directory_iterator&& rhs) noexcept;
    const directory_entry& operator*() const;
    const directory_entry* operator->() const;
    #ifdef GHC_WITH_EXCEPTIONS
    directory_iterator& operator++();
    #endif
    directory_iterator& increment(std::error_code& ec) noexcept;

    // other members as required by 27.2.3, input iterators
    #ifdef GHC_WITH_EXCEPTIONS
    proxy operator++(int)
    {
        proxy p{**this};
        ++*this;
        return p;
    }
    #endif
    bool operator==(const directory_iterator& rhs) const;
    bool operator!=(const directory_iterator& rhs) const;

private:
    friend class recursive_directory_iterator;
    class impl;
    std::shared_ptr<impl> _impl;
};

// 30.10.13.2 directory_iterator non-member functions
GHC_FS_API directory_iterator begin(directory_iterator iter) noexcept;
GHC_FS_API directory_iterator end(const directory_iterator&) noexcept;

// 30.10.14 class recursive_directory_iterator
class GHC_FS_API_CLASS recursive_directory_iterator
{
public:
    using iterator_category = std::input_iterator_tag;
    using value_type = directory_entry;
    using difference_type = std::ptrdiff_t;
    using pointer = const directory_entry*;
    using reference = const directory_entry&;

    // 30.10.14.1 constructors and destructor
    recursive_directory_iterator() noexcept;
    #ifdef GHC_WITH_EXCEPTIONS
    explicit recursive_directory_iterator(const path& p);
    recursive_directory_iterator(const path& p, directory_options options);
    #endif
    recursive_directory_iterator(const path& p, directory_options options, std::error_code& ec) noexcept;
    recursive_directory_iterator(const path& p, std::error_code& ec) noexcept;
    recursive_directory_iterator(const recursive_directory_iterator& rhs);
    recursive_directory_iterator(recursive_directory_iterator&& rhs) noexcept;
    ~recursive_directory_iterator();

    // 30.10.14.1 observers
    directory_options options() const;
    int depth() const;
    bool recursion_pending() const;

    const directory_entry& operator*() const;
    const directory_entry* operator->() const;

    // 30.10.14.1 modifiers recursive_directory_iterator&
    recursive_directory_iterator& operator=(const recursive_directory_iterator& rhs);
    recursive_directory_iterator& operator=(recursive_directory_iterator&& rhs) noexcept;
    #ifdef GHC_WITH_EXCEPTIONS
    recursive_directory_iterator& operator++();
    #endif
    recursive_directory_iterator& increment(std::error_code& ec) noexcept;

    #ifdef GHC_WITH_EXCEPTIONS
    void pop();
    #endif
    void pop(std::error_code& ec);
    void disable_recursion_pending();

    // other members as required by 27.2.3, input iterators
    #ifdef GHC_WITH_EXCEPTIONS
    directory_iterator::proxy operator++(int)
    {
        directory_iterator::proxy proxy{**this};
        ++*this;
        return proxy;
    }
    #endif
    bool operator==(const recursive_directory_iterator& rhs) const;
    bool operator!=(const recursive_directory_iterator& rhs) const;

private:
    struct recursive_directory_iterator_impl
    {
        directory_options _options;
        bool _recursion_pending;
        std::stack<directory_iterator> _dir_iter_stack;
        recursive_directory_iterator_impl(directory_options options, bool recursion_pending)
            : _options(options), _recursion_pending(recursion_pending)
        {
        }
    };
    std::shared_ptr<recursive_directory_iterator_impl> _impl;
};

// 30.10.14.2 directory_iterator non-member functions
GHC_FS_API recursive_directory_iterator begin(recursive_directory_iterator iter) noexcept;
GHC_FS_API recursive_directory_iterator end(const recursive_directory_iterator&) noexcept;

    // 30.10.15 filesystem operations
    #ifdef GHC_WITH_EXCEPTIONS
GHC_FS_API path absolute(const path& p);
GHC_FS_API path canonical(const path& p);
GHC_FS_API void copy(const path& from, const path& to);
GHC_FS_API void copy(const path& from, const path& to, copy_options options);
GHC_FS_API bool copy_file(const path& from, const path& to);
GHC_FS_API bool copy_file(const path& from, const path& to, copy_options option);
GHC_FS_API void copy_symlink(const path& existing_symlink, const path& new_symlink);
GHC_FS_API bool create_directories(const path& p);
GHC_FS_API bool create_directory(const path& p);
GHC_FS_API bool create_directory(const path& p, const path& attributes);
GHC_FS_API void create_directory_symlink(const path& to, const path& new_symlink);
GHC_FS_API void create_symlink(const path& to, const path& new_symlink);
GHC_FS_API path current_path();
GHC_FS_API void current_path(const path& p);
GHC_FS_API bool exists(const path& p);
GHC_FS_API bool equivalent(const path& p1, const path& p2);
GHC_FS_API uintmax_t file_size(const path& p);
GHC_FS_API bool is_block_file(const path& p);
GHC_FS_API bool is_character_file(const path& p);
GHC_FS_API bool is_directory(const path& p);
GHC_FS_API bool is_empty(const path& p);
GHC_FS_API bool is_fifo(const path& p);
GHC_FS_API bool is_other(const path& p);
GHC_FS_API bool is_regular_file(const path& p);
GHC_FS_API bool is_socket(const path& p);
GHC_FS_API bool is_symlink(const path& p);
GHC_FS_API file_time_type last_write_time(const path& p);
GHC_FS_API void last_write_time(const path& p, file_time_type new_time);
GHC_FS_API void permissions(const path& p, perms prms, perm_options opts = perm_options::replace);
GHC_FS_API path proximate(const path& p, const path& base = current_path());
GHC_FS_API path read_symlink(const path& p);
GHC_FS_API path relative(const path& p, const path& base = current_path());
GHC_FS_API bool remove(const path& p);
GHC_FS_API uintmax_t remove_all(const path& p);
GHC_FS_API void rename(const path& from, const path& to);
GHC_FS_API void resize_file(const path& p, uintmax_t size);
GHC_FS_API space_info space(const path& p);
GHC_FS_API file_status status(const path& p);
GHC_FS_API file_status symlink_status(const path& p);
GHC_FS_API path temp_directory_path();
GHC_FS_API path weakly_canonical(const path& p);
    #endif
GHC_FS_API path absolute(const path& p, std::error_code& ec);
GHC_FS_API path canonical(const path& p, std::error_code& ec);
GHC_FS_API void copy(const path& from, const path& to, std::error_code& ec) noexcept;
GHC_FS_API void copy(const path& from, const path& to, copy_options options, std::error_code& ec) noexcept;
GHC_FS_API bool copy_file(const path& from, const path& to, std::error_code& ec) noexcept;
GHC_FS_API bool copy_file(const path& from, const path& to, copy_options option, std::error_code& ec) noexcept;
GHC_FS_API void copy_symlink(const path& existing_symlink, const path& new_symlink, std::error_code& ec) noexcept;
GHC_FS_API bool create_directories(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool create_directory(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool create_directory(const path& p, const path& attributes, std::error_code& ec) noexcept;
GHC_FS_API void create_directory_symlink(const path& to, const path& new_symlink, std::error_code& ec) noexcept;
GHC_FS_API void create_symlink(const path& to, const path& new_symlink, std::error_code& ec) noexcept;
GHC_FS_API path current_path(std::error_code& ec);
GHC_FS_API void current_path(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool exists(file_status s) noexcept;
GHC_FS_API bool exists(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool equivalent(const path& p1, const path& p2, std::error_code& ec) noexcept;
GHC_FS_API uintmax_t file_size(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_block_file(file_status s) noexcept;
GHC_FS_API bool is_block_file(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_character_file(file_status s) noexcept;
GHC_FS_API bool is_character_file(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_directory(file_status s) noexcept;
GHC_FS_API bool is_directory(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_empty(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_fifo(file_status s) noexcept;
GHC_FS_API bool is_fifo(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_other(file_status s) noexcept;
GHC_FS_API bool is_other(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_regular_file(file_status s) noexcept;
GHC_FS_API bool is_regular_file(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_socket(file_status s) noexcept;
GHC_FS_API bool is_socket(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool is_symlink(file_status s) noexcept;
GHC_FS_API bool is_symlink(const path& p, std::error_code& ec) noexcept;
GHC_FS_API file_time_type last_write_time(const path& p, std::error_code& ec) noexcept;
GHC_FS_API void last_write_time(const path& p, file_time_type new_time, std::error_code& ec) noexcept;
GHC_FS_API void permissions(const path& p, perms prms, std::error_code& ec) noexcept;
GHC_FS_API void permissions(const path& p, perms prms, perm_options opts, std::error_code& ec) noexcept;
GHC_FS_API path proximate(const path& p, std::error_code& ec);
GHC_FS_API path proximate(const path& p, const path& base, std::error_code& ec);
GHC_FS_API path read_symlink(const path& p, std::error_code& ec);
GHC_FS_API path relative(const path& p, std::error_code& ec);
GHC_FS_API path relative(const path& p, const path& base, std::error_code& ec);
GHC_FS_API bool remove(const path& p, std::error_code& ec) noexcept;
GHC_FS_API uintmax_t remove_all(const path& p, std::error_code& ec) noexcept;
GHC_FS_API void rename(const path& from, const path& to, std::error_code& ec) noexcept;
GHC_FS_API void resize_file(const path& p, uintmax_t size, std::error_code& ec) noexcept;
GHC_FS_API space_info space(const path& p, std::error_code& ec) noexcept;
GHC_FS_API file_status status(const path& p, std::error_code& ec) noexcept;
GHC_FS_API bool status_known(file_status s) noexcept;
GHC_FS_API file_status symlink_status(const path& p, std::error_code& ec) noexcept;
GHC_FS_API path temp_directory_path(std::error_code& ec) noexcept;
GHC_FS_API path weakly_canonical(const path& p, std::error_code& ec) noexcept;

    #ifndef GHC_OS_WEB
        #ifdef GHC_WITH_EXCEPTIONS
GHC_FS_API void create_hard_link(const path& to, const path& new_hard_link);
GHC_FS_API uintmax_t hard_link_count(const path& p);
        #endif
GHC_FS_API void create_hard_link(const path& to, const path& new_hard_link, std::error_code& ec) noexcept;
GHC_FS_API uintmax_t hard_link_count(const path& p, std::error_code& ec) noexcept;
    #endif

// Non-C++17 add-on std::fstream wrappers with path
template <class charT, class traits = std::char_traits<charT>>
class basic_filebuf : public std::basic_filebuf<charT, traits>
{
public:
    basic_filebuf() {}
    ~basic_filebuf() override {}
    basic_filebuf(const basic_filebuf&) = delete;
    const basic_filebuf& operator=(const basic_filebuf&) = delete;
    basic_filebuf<charT, traits>* open(const path& p, std::ios_base::openmode mode)
    {
    #if defined(GHC_OS_WINDOWS) && !defined(__GLIBCXX__)
        return std::basic_filebuf<charT, traits>::open(p.wstring().c_str(), mode) ? this : 0;
    #else
        return std::basic_filebuf<charT, traits>::open(p.string().c_str(), mode) ? this : 0;
    #endif
    }
};

template <class charT, class traits = std::char_traits<charT>>
class basic_ifstream : public std::basic_ifstream<charT, traits>
{
public:
    basic_ifstream() {}
    #if defined(GHC_OS_WINDOWS) && !defined(__GLIBCXX__)
    explicit basic_ifstream(const path& p, std::ios_base::openmode mode = std::ios_base::in)
        : std::basic_ifstream<charT, traits>(p.wstring().c_str(), mode)
    {
    }
    void open(const path& p, std::ios_base::openmode mode = std::ios_base::in)
    {
        std::basic_ifstream<charT, traits>::open(p.wstring().c_str(), mode);
    }
    #else
    explicit basic_ifstream(const path& p, std::ios_base::openmode mode = std::ios_base::in)
        : std::basic_ifstream<charT, traits>(p.string().c_str(), mode)
    {
    }
    void open(const path& p, std::ios_base::openmode mode = std::ios_base::in)
    {
        std::basic_ifstream<charT, traits>::open(p.string().c_str(), mode);
    }
    #endif
    basic_ifstream(const basic_ifstream&) = delete;
    const basic_ifstream& operator=(const basic_ifstream&) = delete;
    ~basic_ifstream() override {}
};

template <class charT, class traits = std::char_traits<charT>>
class basic_ofstream : public std::basic_ofstream<charT, traits>
{
public:
    basic_ofstream() {}
    #if defined(GHC_OS_WINDOWS) && !defined(__GLIBCXX__)
    explicit basic_ofstream(const path& p, std::ios_base::openmode mode = std::ios_base::out)
        : std::basic_ofstream<charT, traits>(p.wstring().c_str(), mode)
    {
    }
    void open(const path& p, std::ios_base::openmode mode = std::ios_base::out)
    {
        std::basic_ofstream<charT, traits>::open(p.wstring().c_str(), mode);
    }
    #else
    explicit basic_ofstream(const path& p, std::ios_base::openmode mode = std::ios_base::out)
        : std::basic_ofstream<charT, traits>(p.string().c_str(), mode)
    {
    }
    void open(const path& p, std::ios_base::openmode mode = std::ios_base::out)
    {
        std::basic_ofstream<charT, traits>::open(p.string().c_str(), mode);
    }
    #endif
    basic_ofstream(const basic_ofstream&) = delete;
    const basic_ofstream& operator=(const basic_ofstream&) = delete;
    ~basic_ofstream() override {}
};

template <class charT, class traits = std::char_traits<charT>>
class basic_fstream : public std::basic_fstream<charT, traits>
{
public:
    basic_fstream() {}
    #if defined(GHC_OS_WINDOWS) && !defined(__GLIBCXX__)
    explicit basic_fstream(const path& p, std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out)
        : std::basic_fstream<charT, traits>(p.wstring().c_str(), mode)
    {
    }
    void open(const path& p, std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out)
    {
        std::basic_fstream<charT, traits>::open(p.wstring().c_str(), mode);
    }
    #else
    explicit basic_fstream(const path& p, std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out)
        : std::basic_fstream<charT, traits>(p.string().c_str(), mode)
    {
    }
    void open(const path& p, std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out)
    {
        std::basic_fstream<charT, traits>::open(p.string().c_str(), mode);
    }
    #endif
    basic_fstream(const basic_fstream&) = delete;
    const basic_fstream& operator=(const basic_fstream&) = delete;
    ~basic_fstream() override {}
};

typedef basic_filebuf<char> filebuf;
typedef basic_filebuf<wchar_t> wfilebuf;
typedef basic_ifstream<char> ifstream;
typedef basic_ifstream<wchar_t> wifstream;
typedef basic_ofstream<char> ofstream;
typedef basic_ofstream<wchar_t> wofstream;
typedef basic_fstream<char> fstream;
typedef basic_fstream<wchar_t> wfstream;

class GHC_FS_API_CLASS u8arguments
{
public:
    u8arguments(int& argc, char**& argv);
    ~u8arguments()
    {
        _refargc = _argc;
        _refargv = _argv;
    }

    bool valid() const
    {
        return _isvalid;
    }

private:
    int _argc;
    char** _argv;
    int& _refargc;
    char**& _refargv;
    bool _isvalid;
    #ifdef GHC_OS_WINDOWS
    std::vector<std::string> _args;
    std::vector<char*> _argp;
    #endif
};

//-------------------------------------------------------------------------------------------------
//  Implementation
//-------------------------------------------------------------------------------------------------

namespace detail
{
enum utf8_states_t
{
    S_STRT = 0,
    S_RJCT = 8
};
GHC_FS_API void appendUTF8(std::string& str, uint32_t unicode);
GHC_FS_API bool is_surrogate(uint32_t c);
GHC_FS_API bool is_high_surrogate(uint32_t c);
GHC_FS_API bool is_low_surrogate(uint32_t c);
GHC_FS_API unsigned consumeUtf8Fragment(const unsigned state, const uint8_t fragment, uint32_t& codepoint);
enum class portable_error
{
    none = 0,
    exists,
    not_found,
    not_supported,
    not_implemented,
    invalid_argument,
    is_a_directory,
};
GHC_FS_API std::error_code make_error_code(portable_error err);
    #ifdef GHC_OS_WINDOWS
GHC_FS_API std::error_code make_system_error(uint32_t err = 0);
    #else
GHC_FS_API std::error_code make_system_error(int err = 0);
    #endif
} // namespace detail

namespace detail
{
    #ifdef GHC_EXPAND_IMPL

GHC_INLINE std::error_code make_error_code(portable_error err)
{
        #ifdef GHC_OS_WINDOWS
    switch (err)
    {
        case portable_error::none: return std::error_code();
        case portable_error::exists: return std::error_code(ERROR_ALREADY_EXISTS, std::system_category());
        case portable_error::not_found: return std::error_code(ERROR_PATH_NOT_FOUND, std::system_category());
        case portable_error::not_supported: return std::error_code(ERROR_NOT_SUPPORTED, std::system_category());
        case portable_error::not_implemented:
            return std::error_code(ERROR_CALL_NOT_IMPLEMENTED, std::system_category());
        case portable_error::invalid_argument: return std::error_code(ERROR_INVALID_PARAMETER, std::system_category());
        case portable_error::is_a_directory:
            #ifdef ERROR_DIRECTORY_NOT_SUPPORTED
            return std::error_code(ERROR_DIRECTORY_NOT_SUPPORTED, std::system_category());
            #else
            return std::error_code(ERROR_NOT_SUPPORTED, std::system_category());
            #endif
    }
        #else
    switch (err)
    {
        case portable_error::none: return std::error_code();
        case portable_error::exists: return std::error_code(EEXIST, std::system_category());
        case portable_error::not_found: return std::error_code(ENOENT, std::system_category());
        case portable_error::not_supported: return std::error_code(ENOTSUP, std::system_category());
        case portable_error::not_implemented: return std::error_code(ENOSYS, std::system_category());
        case portable_error::invalid_argument: return std::error_code(EINVAL, std::system_category());
        case portable_error::is_a_directory: return std::error_code(EISDIR, std::system_category());
    }
        #endif
    return std::error_code();
}

        #ifdef GHC_OS_WINDOWS
GHC_INLINE std::error_code make_system_error(uint32_t err)
{
    return std::error_code(err ? static_cast<int>(err) : static_cast<int>(::GetLastError()), std::system_category());
}
        #else
GHC_INLINE std::error_code make_system_error(int err)
{
    return std::error_code(err ? err : errno, std::system_category());
}
        #endif

    #endif // GHC_EXPAND_IMPL

template <typename Enum>
using EnableBitmask =
    typename std::enable_if<std::is_same<Enum, perms>::value || std::is_same<Enum, perm_options>::value
                                || std::is_same<Enum, copy_options>::value
                                || std::is_same<Enum, directory_options>::value,
                            Enum>::type;
} // namespace detail

template <typename Enum>
constexpr detail::EnableBitmask<Enum> operator&(Enum X, Enum Y)
{
    using underlying = typename std::underlying_type<Enum>::type;
    return static_cast<Enum>(static_cast<underlying>(X) & static_cast<underlying>(Y));
}

template <typename Enum>
constexpr detail::EnableBitmask<Enum> operator|(Enum X, Enum Y)
{
    using underlying = typename std::underlying_type<Enum>::type;
    return static_cast<Enum>(static_cast<underlying>(X) | static_cast<underlying>(Y));
}

template <typename Enum>
constexpr detail::EnableBitmask<Enum> operator^(Enum X, Enum Y)
{
    using underlying = typename std::underlying_type<Enum>::type;
    return static_cast<Enum>(static_cast<underlying>(X) ^ static_cast<underlying>(Y));
}

template <typename Enum>
constexpr detail::EnableBitmask<Enum> operator~(Enum X)
{
    using underlying = typename std::underlying_type<Enum>::type;
    return static_cast<Enum>(~static_cast<underlying>(X));
}

template <typename Enum>
detail::EnableBitmask<Enum>& operator&=(Enum& X, Enum Y)
{
    X = X & Y;
    return X;
}

template <typename Enum>
detail::EnableBitmask<Enum>& operator|=(Enum& X, Enum Y)
{
    X = X | Y;
    return X;
}

template <typename Enum>
detail::EnableBitmask<Enum>& operator^=(Enum& X, Enum Y)
{
    X = X ^ Y;
    return X;
}

    #ifdef GHC_EXPAND_IMPL

namespace detail
{
GHC_INLINE bool in_range(uint32_t c, uint32_t lo, uint32_t hi)
{
    return (static_cast<uint32_t>(c - lo) < (hi - lo + 1));
}

GHC_INLINE bool is_surrogate(uint32_t c)
{
    return in_range(c, 0xd800, 0xdfff);
}

GHC_INLINE bool is_high_surrogate(uint32_t c)
{
    return (c & 0xfffffc00) == 0xd800;
}

GHC_INLINE bool is_low_surrogate(uint32_t c)
{
    return (c & 0xfffffc00) == 0xdc00;
}

GHC_INLINE void appendUTF8(std::string& str, uint32_t unicode)
{
    if (unicode <= 0x7f)
    {
        str.push_back(static_cast<char>(unicode));
    }
    else if (unicode >= 0x80 && unicode <= 0x7ff)
    {
        str.push_back(static_cast<char>((unicode >> 6) + 192));
        str.push_back(static_cast<char>((unicode & 0x3f) + 128));
    }
    else if ((unicode >= 0x800 && unicode <= 0xd7ff) || (unicode >= 0xe000 && unicode <= 0xffff))
    {
        str.push_back(static_cast<char>((unicode >> 12) + 224));
        str.push_back(static_cast<char>(((unicode & 0xfff) >> 6) + 128));
        str.push_back(static_cast<char>((unicode & 0x3f) + 128));
    }
    else if (unicode >= 0x10000 && unicode <= 0x10ffff)
    {
        str.push_back(static_cast<char>((unicode >> 18) + 240));
        str.push_back(static_cast<char>(((unicode & 0x3ffff) >> 12) + 128));
        str.push_back(static_cast<char>(((unicode & 0xfff) >> 6) + 128));
        str.push_back(static_cast<char>((unicode & 0x3f) + 128));
    }
    else
    {
        #ifdef GHC_RAISE_UNICODE_ERRORS
        throw filesystem_error("Illegal code point for unicode character.", str,
                               std::make_error_code(std::errc::illegal_byte_sequence));
        #else
        appendUTF8(str, 0xfffd);
        #endif
    }
}

// Thanks to Bjoern Hoehrmann (https://bjoern.hoehrmann.de/utf-8/decoder/dfa/)
// and Taylor R Campbell for the ideas to this DFA approach of UTF-8 decoding;
// Generating debugging and shrinking my own DFA from scratch was a day of fun!
GHC_INLINE unsigned consumeUtf8Fragment(const unsigned state, const uint8_t fragment, uint32_t& codepoint)
{
    static const uint32_t utf8_state_info[] = {
        // encoded states
        0x11111111u, 0x11111111u, 0x77777777u, 0x77777777u, 0x88888888u, 0x88888888u, 0x88888888u, 0x88888888u,
        0x22222299u, 0x22222222u, 0x22222222u, 0x22222222u, 0x3333333au, 0x33433333u, 0x9995666bu, 0x99999999u,
        0x88888880u, 0x22818108u, 0x88888881u, 0x88888882u, 0x88888884u, 0x88888887u, 0x88888886u, 0x82218108u,
        0x82281108u, 0x88888888u, 0x88888883u, 0x88888885u, 0u,          0u,          0u,          0u,
    };
    uint8_t category = fragment < 128 ? 0 : (utf8_state_info[(fragment >> 3) & 0xf] >> ((fragment & 7) << 2)) & 0xf;
    codepoint = (state ? (codepoint << 6) | (fragment & 0x3fu) : (0xffu >> category) & fragment);
    return state == S_RJCT ? static_cast<unsigned>(S_RJCT) :
                             static_cast<unsigned>((utf8_state_info[category + 16] >> (state << 2)) & 0xf);
}

GHC_INLINE bool validUtf8(const std::string& utf8String)
{
    std::string::const_iterator iter = utf8String.begin();
    unsigned utf8_state = S_STRT;
    std::uint32_t codepoint = 0;
    while (iter < utf8String.end())
    {
        if ((utf8_state = consumeUtf8Fragment(utf8_state, static_cast<uint8_t>(*iter++), codepoint)) == S_RJCT)
        {
            return false;
        }
    }
    if (utf8_state)
    {
        return false;
    }
    return true;
}

} // namespace detail

    #endif

namespace detail
{
template <
    class StringType, class Utf8String,
    typename std::enable_if<path::_is_basic_string<Utf8String>::value && (sizeof(typename Utf8String::value_type) == 1)
                            && (sizeof(typename StringType::value_type) == 1)>::type* = nullptr>
inline StringType fromUtf8(const Utf8String& utf8String,
                           const typename StringType::allocator_type& alloc = typename StringType::allocator_type())
{
    return StringType(utf8String.begin(), utf8String.end(), alloc);
}

template <
    class StringType, class Utf8String,
    typename std::enable_if<path::_is_basic_string<Utf8String>::value && (sizeof(typename Utf8String::value_type) == 1)
                            && (sizeof(typename StringType::value_type) == 2)>::type* = nullptr>
inline StringType fromUtf8(const Utf8String& utf8String,
                           const typename StringType::allocator_type& alloc = typename StringType::allocator_type())
{
    StringType result(alloc);
    result.reserve(utf8String.length());
    auto iter = utf8String.cbegin();
    unsigned utf8_state = S_STRT;
    std::uint32_t codepoint = 0;
    while (iter < utf8String.cend())
    {
        if ((utf8_state = consumeUtf8Fragment(utf8_state, static_cast<uint8_t>(*iter++), codepoint)) == S_STRT)
        {
            if (codepoint <= 0xffff)
            {
                result += static_cast<typename StringType::value_type>(codepoint);
            }
            else
            {
                codepoint -= 0x10000;
                result += static_cast<typename StringType::value_type>((codepoint >> 10) + 0xd800);
                result += static_cast<typename StringType::value_type>((codepoint & 0x3ff) + 0xdc00);
            }
            codepoint = 0;
        }
        else if (utf8_state == S_RJCT)
        {
    #ifdef GHC_RAISE_UNICODE_ERRORS
            throw filesystem_error("Illegal byte sequence for unicode character.", utf8String,
                                   std::make_error_code(std::errc::illegal_byte_sequence));
    #else
            result += static_cast<typename StringType::value_type>(0xfffd);
            utf8_state = S_STRT;
            codepoint = 0;
    #endif
        }
    }
    if (utf8_state)
    {
    #ifdef GHC_RAISE_UNICODE_ERRORS
        throw filesystem_error("Illegal byte sequence for unicode character.", utf8String,
                               std::make_error_code(std::errc::illegal_byte_sequence));
    #else
        result += static_cast<typename StringType::value_type>(0xfffd);
    #endif
    }
    return result;
}

template <
    class StringType, class Utf8String,
    typename std::enable_if<path::_is_basic_string<Utf8String>::value && (sizeof(typename Utf8String::value_type) == 1)
                            && (sizeof(typename StringType::value_type) == 4)>::type* = nullptr>
inline StringType fromUtf8(const Utf8String& utf8String,
                           const typename StringType::allocator_type& alloc = typename StringType::allocator_type())
{
    StringType result(alloc);
    result.reserve(utf8String.length());
    auto iter = utf8String.cbegin();
    unsigned utf8_state = S_STRT;
    std::uint32_t codepoint = 0;
    while (iter < utf8String.cend())
    {
        if ((utf8_state = consumeUtf8Fragment(utf8_state, static_cast<uint8_t>(*iter++), codepoint)) == S_STRT)
        {
            result += static_cast<typename StringType::value_type>(codepoint);
            codepoint = 0;
        }
        else if (utf8_state == S_RJCT)
        {
    #ifdef GHC_RAISE_UNICODE_ERRORS
            throw filesystem_error("Illegal byte sequence for unicode character.", utf8String,
                                   std::make_error_code(std::errc::illegal_byte_sequence));
    #else
            result += static_cast<typename StringType::value_type>(0xfffd);
            utf8_state = S_STRT;
            codepoint = 0;
    #endif
        }
    }
    if (utf8_state)
    {
    #ifdef GHC_RAISE_UNICODE_ERRORS
        throw filesystem_error("Illegal byte sequence for unicode character.", utf8String,
                               std::make_error_code(std::errc::illegal_byte_sequence));
    #else
        result += static_cast<typename StringType::value_type>(0xfffd);
    #endif
    }
    return result;
}

template <class StringType, typename charT, std::size_t N>
inline StringType fromUtf8(const charT (&utf8String)[N])
{
    #ifdef GHC_WITH_STRING_VIEW
    return fromUtf8<StringType>(basic_string_view<charT>(utf8String, N - 1));
    #else
    return fromUtf8<StringType>(std::basic_string<charT>(utf8String, N - 1));
    #endif
}

template <typename strT,
          typename std::enable_if<path::_is_basic_string<strT>::value && (sizeof(typename strT::value_type) == 1),
                                  int>::type size = 1>
inline std::string toUtf8(const strT& unicodeString)
{
    return std::string(unicodeString.begin(), unicodeString.end());
}

template <typename strT,
          typename std::enable_if<path::_is_basic_string<strT>::value && (sizeof(typename strT::value_type) == 2),
                                  int>::type size = 2>
inline std::string toUtf8(const strT& unicodeString)
{
    std::string result;
    for (auto iter = unicodeString.begin(); iter != unicodeString.end(); ++iter)
    {
        char32_t c = *iter;
        if (is_surrogate(c))
        {
            ++iter;
            if (iter != unicodeString.end() && is_high_surrogate(c) && is_low_surrogate(*iter))
            {
                appendUTF8(result, (char32_t(c) << 10) + *iter - 0x35fdc00);
            }
            else
            {
    #ifdef GHC_RAISE_UNICODE_ERRORS
                throw filesystem_error("Illegal code point for unicode character.", result,
                                       std::make_error_code(std::errc::illegal_byte_sequence));
    #else
                appendUTF8(result, 0xfffd);
                if (iter == unicodeString.end())
                {
                    break;
                }
    #endif
            }
        }
        else
        {
            appendUTF8(result, c);
        }
    }
    return result;
}

template <typename strT,
          typename std::enable_if<path::_is_basic_string<strT>::value && (sizeof(typename strT::value_type) == 4),
                                  int>::type size = 4>
inline std::string toUtf8(const strT& unicodeString)
{
    std::string result;
    for (auto c : unicodeString)
    {
        appendUTF8(result, static_cast<uint32_t>(c));
    }
    return result;
}

template <typename charT>
inline std::string toUtf8(const charT* unicodeString)
{
    #ifdef GHC_WITH_STRING_VIEW
    return toUtf8(basic_string_view<charT, std::char_traits<charT>>(unicodeString));
    #else
    return toUtf8(std::basic_string<charT, std::char_traits<charT>>(unicodeString));
    #endif
}

    #ifdef GHC_USE_WCHAR_T
template <class StringType, class WString,
          typename std::enable_if<path::_is_basic_string<WString>::value && (sizeof(typename WString::value_type) == 2)
                                      && (sizeof(typename StringType::value_type) == 1),
                                  bool>::type = false>
inline StringType fromWChar(const WString& wString,
                            const typename StringType::allocator_type& alloc = typename StringType::allocator_type())
{
    auto temp = toUtf8(wString);
    return StringType(temp.begin(), temp.end(), alloc);
}

template <class StringType, class WString,
          typename std::enable_if<path::_is_basic_string<WString>::value && (sizeof(typename WString::value_type) == 2)
                                      && (sizeof(typename StringType::value_type) == 2),
                                  bool>::type = false>
inline StringType fromWChar(const WString& wString,
                            const typename StringType::allocator_type& alloc = typename StringType::allocator_type())
{
    return StringType(wString.begin(), wString.end(), alloc);
}

template <class StringType, class WString,
          typename std::enable_if<path::_is_basic_string<WString>::value && (sizeof(typename WString::value_type) == 2)
                                      && (sizeof(typename StringType::value_type) == 4),
                                  bool>::type = false>
inline StringType fromWChar(const WString& wString,
                            const typename StringType::allocator_type& alloc = typename StringType::allocator_type())
{
    auto temp = toUtf8(wString);
    return fromUtf8<StringType>(temp, alloc);
}

template <typename strT,
          typename std::enable_if<path::_is_basic_string<strT>::value && (sizeof(typename strT::value_type) == 1),
                                  bool>::type = false>
inline std::wstring toWChar(const strT& unicodeString)
{
    return fromUtf8<std::wstring>(unicodeString);
}

template <typename strT,
          typename std::enable_if<path::_is_basic_string<strT>::value && (sizeof(typename strT::value_type) == 2),
                                  bool>::type = false>
inline std::wstring toWChar(const strT& unicodeString)
{
    return std::wstring(unicodeString.begin(), unicodeString.end());
}

template <typename strT,
          typename std::enable_if<path::_is_basic_string<strT>::value && (sizeof(typename strT::value_type) == 4),
                                  bool>::type = false>
inline std::wstring toWChar(const strT& unicodeString)
{
    auto temp = toUtf8(unicodeString);
    return fromUtf8<std::wstring>(temp);
}

template <typename charT>
inline std::wstring toWChar(const charT* unicodeString)
{
        #ifdef GHC_WITH_STRING_VIEW
    return toWChar(basic_string_view<charT, std::char_traits<charT>>(unicodeString));
        #else
    return toWChar(std::basic_string<charT, std::char_traits<charT>>(unicodeString));
        #endif
}
    #endif // GHC_USE_WCHAR_T

} // namespace detail

    #ifdef GHC_EXPAND_IMPL

namespace detail
{
template <typename strT, typename std::enable_if<path::_is_basic_string<strT>::value, bool>::type = true>
GHC_INLINE bool startsWith(const strT& what, const strT& with)
{
    return with.length() <= what.length() && equal(with.begin(), with.end(), what.begin());
}

template <typename strT, typename std::enable_if<path::_is_basic_string<strT>::value, bool>::type = true>
GHC_INLINE bool endsWith(const strT& what, const strT& with)
{
    return with.length() <= what.length() && what.compare(what.length() - with.length(), with.size(), with) == 0;
}

} // namespace detail

GHC_INLINE void path::check_long_path()
{
        #if defined(GHC_OS_WINDOWS) && defined(GHC_WIN_AUTO_PREFIX_LONG_PATH)
    if (is_absolute() && _path.length() >= MAX_PATH - 12
        && !detail::startsWith(_path, impl_string_type(GHC_PLATFORM_LITERAL("\\\\?\\"))))
    {
        postprocess_path_with_format(native_format);
    }
        #endif
}

GHC_INLINE void path::postprocess_path_with_format(path::format fmt)
{
        #ifdef GHC_RAISE_UNICODE_ERRORS
    if (!detail::validUtf8(_path))
    {
        path t;
        t._path = _path;
        throw filesystem_error("Illegal byte sequence for unicode character.", t,
                               std::make_error_code(std::errc::illegal_byte_sequence));
    }
        #endif
    switch (fmt)
    {
        #ifdef GHC_OS_WINDOWS
        case path::native_format:
        case path::auto_format:
        case path::generic_format:
            for (auto& c : _path)
            {
                if (c == generic_separator)
                {
                    c = preferred_separator;
                }
            }
            #ifdef GHC_WIN_AUTO_PREFIX_LONG_PATH
            if (is_absolute() && _path.length() >= MAX_PATH - 12
                && !detail::startsWith(_path, impl_string_type(GHC_PLATFORM_LITERAL("\\\\?\\"))))
            {
                _path = GHC_PLATFORM_LITERAL("\\\\?\\") + _path;
            }
            #endif
            handle_prefixes();
            break;
        #else
        case path::auto_format:
        case path::native_format:
        case path::generic_format:
            // nothing to do
            break;
        #endif
    }
    if (_path.length() > _prefixLength + 2 && _path[_prefixLength] == preferred_separator && _path[_prefixLength + 1] == pref

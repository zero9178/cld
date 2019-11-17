#include "termcolor.hpp"

// the following snippet of code detects the current OS and
// defines the appropriate macro that is used to wrap some
// platform specific things
#if defined(_WIN32) || defined(_WIN64)
    #define TERMCOLOR_OS_WINDOWS
#elif defined(__APPLE__)
    #define TERMCOLOR_OS_MACOS
#elif defined(__unix__) || defined(__unix)
    #define TERMCOLOR_OS_LINUX
#else
    #error unsupported platform
#endif

// This headers provides the `isatty()`/`fileno()` functions,
// which are used for testing whether a standart stream refers
// to the terminal. As for Windows, we also need WinApi funcs
// for changing colors attributes of the terminal.
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
    #include <unistd.h>
#elif defined(TERMCOLOR_OS_WINDOWS)
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
#endif

#include <cstdio>

namespace
{
    static int colorize_index = std::ios_base::xalloc();

    //! Since C++ hasn't a true way to extract stream handler
    //! from the a given `std::ostream` object, I have to write
    //! this kind of hack.
    FILE* get_standard_stream(const std::ostream& stream)
    {
        if (&stream == &std::cout)
            return stdout;
        else if ((&stream == &std::cerr) || (&stream == &std::clog))
            return stderr;

        return nullptr;
    }

    //! Test whether a given `std::ostream` object refers to
    //! a terminal.
    bool is_atty(const std::ostream& stream)
    {
        return get_standard_stream(stream);
    }

    // Say whether a given stream should be colorized or not. It's always
    // true for ATTY streams and may be true for streams marked with
    // colorize flag.
    bool is_colorized(std::ostream& stream)
    {
        return is_atty(stream) || static_cast<bool>(stream.iword(colorize_index));
    }

#if defined(TERMCOLOR_OS_WINDOWS)
    //! Change Windows Terminal colors attribute. If some
    //! parameter is `-1` then attribute won't changed.
    void win_change_attributes(std::ostream& stream, int foreground, int background = -1)
    {
        // yeah, i know.. it's ugly, it's windows.
        static WORD defaultAttributes = []() -> std::uint16_t {
            CONSOLE_SCREEN_BUFFER_INFO info;
            if (!GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info))
            {
                return 0;
            }
            return info.wAttributes;
        }();

        // Windows doesn't have ANSI escape sequences and so we use special
        // API to change Terminal output color. That means we can't
        // manipulate colors by means of "std::stringstream" and hence
        // should do nothing in this case.
        if (!::is_atty(stream))
            return;

        // get terminal handle
        HANDLE hTerminal = INVALID_HANDLE_VALUE;
        if (&stream == &std::cout)
            hTerminal = GetStdHandle(STD_OUTPUT_HANDLE);
        else if (&stream == &std::cerr)
            hTerminal = GetStdHandle(STD_ERROR_HANDLE);

        // restore all default settings
        if (foreground == -1 && background == -1)
        {
            SetConsoleTextAttribute(hTerminal, defaultAttributes);
            return;
        }

        // get current settings
        CONSOLE_SCREEN_BUFFER_INFO info;
        if (!GetConsoleScreenBufferInfo(hTerminal, &info))
            return;

        if (foreground != -1)
        {
            info.wAttributes &= ~(info.wAttributes & 0x0F);
            info.wAttributes |= static_cast<WORD>(foreground);
        }

        if (background != -1)
        {
            info.wAttributes &= ~(info.wAttributes & 0xF0);
            info.wAttributes |= static_cast<WORD>(background);
        }

        SetConsoleTextAttribute(hTerminal, info.wAttributes);
    }
#endif // TERMCOLOR_OS_WINDOWS

} // namespace

namespace termcolor
{
    std::ostream& colorize(std::ostream& stream)
    {
        stream.iword(::colorize_index) = 1L;
        return stream;
    }

    std::ostream& nocolorize(std::ostream& stream)
    {
        stream.iword(::colorize_index) = 0L;
        return stream;
    }

    std::ostream& reset(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[00m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, -1);
#endif
        }
        return stream;
    }

    std::ostream& bold(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[1m";
#elif defined(TERMCOLOR_OS_WINDOWS)
#endif
        }
        return stream;
    }

    std::ostream& dark(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[2m";
#elif defined(TERMCOLOR_OS_WINDOWS)
#endif
        }
        return stream;
    }

    std::ostream& underline(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[4m";
#elif defined(TERMCOLOR_OS_WINDOWS)
#endif
        }
        return stream;
    }

    std::ostream& blink(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[5m";
#elif defined(TERMCOLOR_OS_WINDOWS)
#endif
        }
        return stream;
    }

    std::ostream& reverse(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[7m";
#elif defined(TERMCOLOR_OS_WINDOWS)
#endif
        }
        return stream;
    }

    std::ostream& concealed(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[8m";
#elif defined(TERMCOLOR_OS_WINDOWS)
#endif
        }
        return stream;
    }

    std::ostream& grey(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[30m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream,
                                    0 // grey (black)
            );
#endif
        }
        return stream;
    }

    std::ostream& red(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[31m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_RED);
#endif
        }
        return stream;
    }

    std::ostream& green(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[32m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_GREEN);
#endif
        }
        return stream;
    }

    std::ostream& yellow(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[33m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_GREEN | FOREGROUND_RED);
#endif
        }
        return stream;
    }

    std::ostream& blue(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[34m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_BLUE);
#endif
        }
        return stream;
    }
    std::ostream& magenta(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[35m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_BLUE | FOREGROUND_RED);
#endif
        }
        return stream;
    }

    std::ostream& cyan(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[36m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_BLUE | FOREGROUND_GREEN);
#endif
        }
        return stream;
    }

    std::ostream& white(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[37m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED);
#endif
        }
        return stream;
    }

    std::ostream& on_grey(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[40m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1,
                                    0 // grey (black)
            );
#endif
        }
        return stream;
    }

    std::ostream& on_red(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[41m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_RED);
#endif
        }
        return stream;
    }
    std::ostream& on_green(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[42m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_GREEN);
#endif
        }
        return stream;
    }
    std::ostream& on_yellow(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[43m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_GREEN | BACKGROUND_RED);
#endif
        }
        return stream;
    }

    std::ostream& on_blue(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[44m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_BLUE);
#endif
        }
        return stream;
    }

    std::ostream& on_magenta(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[45m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_BLUE | BACKGROUND_RED);
#endif
        }
        return stream;
    }
    std::ostream& on_cyan(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[46m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_GREEN | BACKGROUND_BLUE);
#endif
        }
        return stream;
    }

    std::ostream& on_white(std::ostream& stream)
    {
        if (::is_colorized(stream))
        {
#if defined(TERMCOLOR_OS_MACOS) || defined(TERMCOLOR_OS_LINUX)
            stream << "\033[47m";
#elif defined(TERMCOLOR_OS_WINDOWS)
            ::win_change_attributes(stream, -1, BACKGROUND_GREEN | BACKGROUND_BLUE | BACKGROUND_RED);
#endif
        }

        return stream;
    }
} // namespace termcolor

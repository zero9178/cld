//!
//! termcolor
//! ~~~~~~~~~
//!
//! termcolor is a header-only c++ library for printing colored messages
//! to the terminal. Written just for fun with a help of the Force.
//!
//! :copyright: (c) 2013 by Ihor Kalnytskyi
//! :license: BSD, see LICENSE for details
//!

#ifndef TERMCOLOR_HPP_
#define TERMCOLOR_HPP_

#include <iostream>

namespace termcolor
{
    std::ostream& colorize(std::ostream& stream);

    std::ostream& nocolorize(std::ostream& stream);

    std::ostream& reset(std::ostream& stream);

    std::ostream& bold(std::ostream& stream);

    std::ostream& dark(std::ostream& stream);

    std::ostream& underline(std::ostream& stream);

    std::ostream& blink(std::ostream& stream);

    std::ostream& reverse(std::ostream& stream);

    std::ostream& concealed(std::ostream& stream);

    std::ostream& grey(std::ostream& stream);

    std::ostream& red(std::ostream& stream);

    std::ostream& green(std::ostream& stream);

    std::ostream& yellow(std::ostream& stream);

    std::ostream& blue(std::ostream& stream);

    std::ostream& magenta(std::ostream& stream);

    std::ostream& cyan(std::ostream& stream);

    std::ostream& white(std::ostream& stream);

    std::ostream& on_grey(std::ostream& stream);

    std::ostream& on_red(std::ostream& stream);

    std::ostream& on_green(std::ostream& stream);

    std::ostream& on_yellow(std::ostream& stream);

    std::ostream& on_blue(std::ostream& stream);

    std::ostream& on_magenta(std::ostream& stream);

    std::ostream& on_cyan(std::ostream& stream);

    std::ostream& on_white(std::ostream& stream);

} // namespace termcolor

#undef TERMCOLOR_OS_WINDOWS
#undef TERMCOLOR_OS_MACOS
#undef TERMCOLOR_OS_LINUX

#endif // TERMCOLOR_HPP_

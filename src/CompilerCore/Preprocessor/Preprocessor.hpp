#ifndef OPENCLPARSER_PREPROCESSOR_HPP
#define OPENCLPARSER_PREPROCESSOR_HPP

#include <iostream>

namespace OpenCL
{
    class SourceObject;

    namespace PP
    {
        OpenCL::SourceObject preprocess(const SourceObject& sourceObject, std::ostream* reporter = &std::cerr);
    }
} // namespace OpenCL

#endif // OPENCLPARSER_PREPROCESSOR_HPP

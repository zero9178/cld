#ifndef OPENCLPARSER_PREPROCESSOR_HPP
#define OPENCLPARSER_PREPROCESSOR_HPP

namespace OpenCL
{
    class SourceObject;

    namespace PP
    {
        SourceObject preprocess(SourceObject sourceObject);
    }
} // namespace OpenCL

#endif // OPENCLPARSER_PREPROCESSOR_HPP

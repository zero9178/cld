#include "X86TargetFeatures.hpp"

bool cld::X86TargetFeatures::hasFeature(cld::TargetFeatures::Features features) const
{
    switch (features)
    {
        default: return false;
        case TargetFeatures::IsX86: return true;
#define HANDLE_VALUE(x) \
    case x: return m_##x;
//
#include "X86Features.def"
    }
}

bool cld::X86TargetFeatures::setFeature(cld::TargetFeatures::Features features, bool value)
{
    switch (features)
    {
        default: return false;
#define HANDLE_VALUE(x) \
    case x: m_##x = value; return true;
//
#include "X86Features.def"
    }
}

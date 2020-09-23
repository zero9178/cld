#include "Triple.hpp"

#include "Util.hpp"

cld::Triple::Triple(cld::Architecture architecture, cld::Platform operatingSystem, cld::Environment environment)
    : m_architecture(architecture), m_operatingSystem(operatingSystem), m_environment(environment)
{
}

cld::Triple cld::Triple::defaultTarget()
{
#ifdef CLD_DEFAULT_TARGET
    static auto target = fromString(CLD_DEFAULT_TARGET);
    return target;
#endif
    return native();
}

cld::Triple cld::Triple::native()
{
    Architecture architecture = Architecture ::Unknown;
#if defined(_M_AMD64) || defined(__x86_64__)
    architecture = Architecture::x86_64;
#elif defined(_M_IX86) || defined(__i386__)
    architecture = Architecture::x86;
#endif

    Platform operatingSystem = Platform::Unknown;
#ifdef _WIN32
    operatingSystem = Platform ::Windows;
#elif defined(__linux__)
    operatingSystem = Platform ::Linux;
#endif

    Environment environment = Environment ::Unknown;
#ifdef _MSC_VER
    environment = Environment ::MSVC;
#elif defined(__GNUC__)
    environment = Environment ::GNU;
#endif
    return Triple(architecture, operatingSystem, environment);
}

cld::Triple cld::Triple::fromString(std::string_view)
{
    // TODO:
    // return cld::Triple(cld::Architecture::x86, cld::Platform::Unknown, cld::Environment::GNU);
    CLD_UNREACHABLE;
}

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

cld::Triple cld::Triple::fromString(std::string_view triple)
{
    auto first = triple.substr(0, triple.find('-'));
    triple.remove_prefix(std::min(first.size() + 1, triple.size()));
    auto second = triple.substr(0, triple.find('-'));
    triple.remove_prefix(std::min(second.size() + 1, triple.size()));
    auto third = triple.substr(0, triple.find('-'));
    triple.remove_prefix(std::min(third.size() + 1, triple.size()));
    Architecture architecture = Architecture::Unknown;
    if (first == "x86_64")
    {
        architecture = Architecture::x86_64;
    }
    else if (first == "x86")
    {
        architecture = Architecture::x86;
    }
    Platform platform = Platform::Unknown;
    if (second == "windows")
    {
        platform = Platform::Windows;
    }
    else if (second == "linux")
    {
        platform = Platform::Linux;
    }
    Environment environment = Environment::Unknown;
    if (third == "gnu")
    {
        environment = Environment::GNU;
    }
    else if (third == "msvc")
    {
        environment = Environment::MSVC;
    }
    return Triple(architecture, platform, environment);
}

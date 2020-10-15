#pragma once

#include <string_view>

namespace cld
{
enum class Architecture
{
    x86,
    x86_64,
    Unknown,
};

enum class Platform
{
    Windows,
    Linux,
    Unknown,
};

enum class Environment
{
    GNU,
    MSVC,
    Unknown,
};

class Triple
{
    Architecture m_architecture;
    Platform m_operatingSystem;
    Environment m_environment;

public:
    static Triple fromString(std::string_view string);

    Triple(Architecture architecture, Platform operatingSystem, Environment environment);

    Architecture getArchitecture() const
    {
        return m_architecture;
    }

    Platform getPlatform() const
    {
        return m_operatingSystem;
    }

    Environment getEnvironment() const
    {
        return m_environment;
    }

    static Triple defaultTarget();

    static Triple native();
};

} // namespace cld

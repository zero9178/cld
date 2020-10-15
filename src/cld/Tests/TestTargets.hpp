#pragma once

#include <cld/Support/Triple.hpp>

namespace cld::Tests
{
const inline auto x64windowsGnu = Triple(Architecture::x86_64, Platform::Windows, Environment::GNU);
const inline auto x86windowsGnu = Triple(Architecture::x86, Platform::Windows, Environment::GNU);
const inline auto x64windowsMsvc = Triple(Architecture::x86_64, Platform::Windows, Environment::MSVC);
const inline auto x86windowsMsvc = Triple(Architecture::x86, Platform::Windows, Environment::MSVC);
const inline auto x64linux = Triple(Architecture::x86_64, Platform::Linux, Environment::GNU);
const inline auto x86linux = Triple(Architecture::x86, Platform::Linux, Environment::GNU);

} // namespace cld::Tests

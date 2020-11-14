#pragma once

#include <cld/Frontend/Compiler/LanguageOptions.hpp>

namespace cld
{
class X86TargetFeatures final : public TargetFeatures
{
#define HANDLE_VALUE(x) bool m_##x;
#include "X86Features.def"
public:
    bool hasFeature(Features features) const override;

    bool setFeature(Features features, bool value) override;
};
} // namespace cld

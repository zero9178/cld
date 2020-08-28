
#pragma once

#include "ProgramInterface.hpp"
#include "SourceObject.hpp"

namespace cld::Semantics
{
class Program final : public ProgramInterface
{
    TranslationUnit m_translationUnit;
    CSourceObject m_sourceObject;

public:
    Program(TranslationUnit&& translationUnit, CSourceObject&& sourceObject, ProgramInterface&& programInterface)
        : ProgramInterface(std::move(programInterface)),
          m_translationUnit(std::move(translationUnit)),
          m_sourceObject(std::move(sourceObject))
    {
    }

    const TranslationUnit& getTranslationUnit() const
    {
        return m_translationUnit;
    }

    const LanguageOptions& getLanguageOptions() const override
    {
        return m_sourceObject.getLanguageOptions();
    }

    const CSourceObject& getSourceObject() const
    {
        return m_sourceObject;
    }
};

} // namespace cld::Semantics

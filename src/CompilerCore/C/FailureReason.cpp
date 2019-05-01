#include "FailureReason.hpp"

#include <utility>

OpenCL::FailureReason::FailureReason(std::string text) : m_text(std::move(text)) {}

const std::string& OpenCL::FailureReason::getText() const
{
    return m_text;
}

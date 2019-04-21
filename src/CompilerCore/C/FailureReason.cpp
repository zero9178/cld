#include "FailureReason.hpp"
#include <utility>

OpenCL::FailureReason::FailureReason(std::string  text) : m_text(std::move(text))
{}

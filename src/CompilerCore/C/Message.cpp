#include "Message.hpp"

#pragma warning(push, 0)
#include <llvm/Support/Format.h>
#pragma warning(pop)

#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Util.hpp>

#include <regex>
#include <utility>

OpenCL::Message::Message(Severity severity, std::string message, std::vector<Lexer::Token>::const_iterator begin,
                         std::optional<std::vector<OpenCL::Lexer::Token>::const_iterator> rangeEnd,
                         std::optional<Modifier> modifier)
    : m_modifier(std::move(modifier)),
      m_message(std::move(message)),
      m_begin(begin),
      m_rangeEnd(rangeEnd),
      m_severity(severity)
{
}

OpenCL::Message::Severity OpenCL::Message::getSeverity() const
{
    return m_severity;
}

OpenCL::Message OpenCL::Message::error(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator token,
                                       std::optional<Modifier> modifier)
{
    return OpenCL::Message(Error, std::move(message), token, {}, std::move(modifier));
}

OpenCL::Message
    OpenCL::Message::note(std::string message,
                          std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator token,
                          std::optional<Modifier> modifier)
{
    return OpenCL::Message(Note, std::move(message), token, {}, std::move(modifier));
}

OpenCL::Message OpenCL::Message::warning(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator token,
                                         std::optional<Modifier> modifier)
{
    return OpenCL::Message(Warning, std::move(message), token, {}, std::move(modifier));
}

llvm::raw_ostream& OpenCL::Message::print(llvm::raw_ostream& os, const OpenCL::SourceObject& sourceObject) const
{
    auto [colour, prefix] = [this]() -> std::pair<llvm::raw_ostream::Colors, std::string> {
        switch (getSeverity())
        {
            case Message::Error: return {llvm::raw_ostream::RED, "error: "};
            case Message::Note: return {llvm::raw_ostream::CYAN, "note: "};
            case Message::Warning: return {llvm::raw_ostream::MAGENTA, "warning: "};
            default: OPENCL_UNREACHABLE;
        }
    }();

    os.flush();
    return os;
}

OpenCL::Message OpenCL::Message::error(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                       std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                       std::optional<Modifier> modifier)
{
    return OpenCL::Message(Error, std::move(message), begin, end, modifier);
}

OpenCL::Message OpenCL::Message::warning(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         std::optional<Modifier> modifier)
{
    return OpenCL::Message(Warning, std::move(message), begin, end, modifier);
}

OpenCL::Message OpenCL::Message::note(std::string message, std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                      std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                      std::optional<Modifier> modifier)
{
    return OpenCL::Message(Note, std::move(message), begin, end, modifier);
}

OpenCL::Modifier::Modifier(std::vector<Lexer::Token>::const_iterator begin,
                           std::vector<Lexer::Token>::const_iterator anEnd, OpenCL::Modifier::Action action,
                           std::string actionArgument)
    : m_actionArgument(std::move(actionArgument)), m_begin(begin), m_end(anEnd), m_action(action)
{
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Modifier::getBegin() const
{
    return m_begin;
}

const std::vector<OpenCL::Lexer::Token>::const_iterator& OpenCL::Modifier::getAnEnd() const
{
    return m_end;
}

OpenCL::Modifier::Action OpenCL::Modifier::getAction() const
{
    return m_action;
}

const std::string& OpenCL::Modifier::getActionArgument() const
{
    return m_actionArgument;
}

std::string OpenCL::Format::format(std::vector<std::string> args) const
{
    static std::regex brackets(R"(\\*\{\})");
    std::reverse(args.begin(), args.end());
    std::smatch matches;
    std::string result = m_format;
    auto start = result.cbegin();
    while (start < result.cend() && std::regex_search(start, result.cend(), matches, brackets))
    {
        auto size = matches.suffix().first - result.cbegin() - (matches[0].second - matches[0].first) + 1;
        auto pos = matches[0].str().find_first_not_of('\\');
        if (pos != std::string::npos && pos % 2 == 1)
        {
            start = result.cbegin() + size;
            continue;
        }
        if (args.empty())
        {
            llvm::errs() << "Not enough arguments specified to substitute in format";
            std::terminate();
        }
        result = std::string(result.cbegin(), matches[0].first + pos) + args.back()
                 + std::string(matches[0].second, result.cend());
        start = result.cbegin() + args.back().size() + (size - 1);
        args.pop_back();
    }
    if (!args.empty())
    {
        llvm::errs() << "More arguments specified than needed to substitute";
        std::terminate();
    }
    return result;
}


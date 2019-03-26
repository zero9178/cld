#include "Preprocessor.hpp"

#include <regex>
#include <numeric>
#include <unordered_map>

namespace
{
    std::vector<std::string> split(const std::string& str, const std::string& delim)
    {
        std::vector<std::string> result;
        std::size_t prev = 0, pos = 0;
        do
        {
            pos = str.find(delim, prev);
            if (pos == std::string::npos)
            {
                pos = str.length();
            }
            auto line = str.substr(prev, pos - prev);
            result.push_back(std::move(line));
            prev = pos + delim.length();
        } while (pos < str.length() && prev < str.length());
        return result;
    }

    enum class States
    {
        Start,
        ContinueDefine
    };

    using unordered_map = std::unordered_map<std::string,std::pair<std::vector<std::string>,std::string>>;

    using MacroStrings = std::vector<std::pair<std::string,bool>>;

    void resolveMacro(std::size_t pos, const unordered_map& defines, MacroStrings& newStrings)
    {

    }
}

std::string OpenCL::PP::preprocess(std::string source)
{
    States currentState = States::Start;
    unordered_map defines;
    std::regex isPreprocessor("\\s*#.*", std::regex_constants::optimize);
    source = std::regex_replace(source, std::regex("\r\n"), "\n");
    bool hasPreprocessorTokens;
    std::string* currentDefine = nullptr;
    do
    {
        hasPreprocessorTokens = false;
        auto lines = split(source, "\n");
        std::uint64_t line = 0;
        for (auto& iter : lines)
        {
            line++;
            switch (currentState)
            {
            case States::Start:
            {
                if (iter.empty() || !std::regex_match(iter, isPreprocessor))
                {
                    MacroStrings strings;
                    strings.emplace_back(iter,false);
                    for (auto& [name, replacement] : defines)
                    {
                        for(std::size_t current = 0;current < strings.size();)
                        {
                            if(strings[current].second)
                            {
                                current++;
                                continue;
                            }
                            std::size_t pos = strings[current].first.find(name, 0);
                            if(pos != std::string::npos)
                            {
                                if (replacement.first.empty())
                                {
                                    strings.emplace(strings.begin() + current + 1,iter.substr(pos + name.size(), iter.size() - pos + name.size()),false);
                                    strings.emplace(strings.begin() + current + 1,replacement.second,true);
                                    strings.emplace(strings.begin() + current + 1,iter.substr(0, pos),false);
                                    strings.erase(strings.begin() + current);
                                    current += 2;
                                }
                                else
                                {
                                    resolveMacro(pos, defines, strings);
                                    current += 2;
                                }
                            }
                            else
                            {
                                current++;
                            }
                        }
                    }
                }
                else
                {
                    hasPreprocessorTokens = true;
                    iter = std::regex_replace(iter, std::regex("\\s*#"), "");
                    if (iter.rfind("define", 0) == 0)
                    {
                        iter = iter.substr(6, iter.size() - 6);
                        auto result = std::find_if_not(iter.begin(), iter.end(), [](char c)
                        { return std::isspace(c); });
                        iter.erase(iter.begin(), result);
                        result = std::find_if(iter.begin(), iter.end(), [](char c)
                        { return std::isspace(c) || c == '('; });
                        auto name = iter.substr(0, result - iter.begin());
                        iter = iter.substr(result - iter.begin(), iter.size() - (result - iter.begin()));
                        std::vector<std::string> arguments;
                        if (!iter.empty())
                        {
                            if (iter[0] == '(')
                            {
                                auto closing = std::find(iter.begin(), iter.end(), ')');
                                if (closing == iter.end())
                                {
                                    throw std::runtime_error("Expected ) in preprocessor macro");
                                }
                                std::string substring(iter.begin() + 1, closing);
                                iter.erase(iter.begin(), closing + 1);
                                arguments = split(substring, ",");
                                if (std::any_of(arguments.begin(), arguments.end(), [](const std::string& text)
                                { return text.empty(); }))
                                {
                                    throw std::runtime_error("One of the macro arguments has no name");
                                }
                            }
                        }
                        auto back = std::find_if_not(iter.rbegin(), iter.rend(), [](char c)
                        { return std::isspace(c); });
                        iter = iter.substr(0, iter.size() - (back - iter.rbegin()));
                        if (iter.back() == '\\')
                        {
                            iter.resize(iter.size() - 1);
                            currentState = States::ContinueDefine;
                        }
                        result = std::find_if_not(iter.begin(), iter.end(), [](char c)
                        { return std::isspace(c); });
                        iter.erase(iter.begin(), result);
                        auto pair = defines.insert({name, {arguments, iter}});
                        if (!pair.second)
                        {
                            throw std::runtime_error(name + " is already defined");
                        }
                        currentDefine = &pair.first->second.second;
                        iter = "";
                    }
                    else if(iter.rfind("undef",0) == 0)
                    {
                        iter = iter.substr(5,iter.size() - 5);
                        auto back = std::find_if_not(iter.rbegin(), iter.rend(), [](char c)
                        { return std::isspace(c); });
                        iter = iter.substr(0, iter.size() - (back - iter.rbegin()));
                        defines.erase(iter);
                        iter = "";
                    }
                }
                break;
            }
            case States::ContinueDefine:
            {
                auto back = std::find_if_not(iter.rbegin(), iter.rend(), [](char c)
                { return std::isspace(c); });
                iter = iter.substr(0, iter.size() - (back - iter.rbegin()));
                if (iter.back() != '\\')
                {
                    currentState = States::Start;
                }
                else
                {
                    iter.resize(iter.size() - 1);
                }
                *currentDefine += " " + iter;
                iter = "";
                break;
            }
            }
        }
        source = std::accumulate(lines.begin(), lines.end(), std::string(),[](const std::string& lhd,const std::string& rhs)
        {

        });
    } while (hasPreprocessorTokens);
    return source;
}

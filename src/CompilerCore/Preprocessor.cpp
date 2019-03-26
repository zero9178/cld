#include "Preprocessor.hpp"

#include <regex>
#include <numeric>
#include <unordered_map>

namespace
{
    std::vector<std::string> split(const std::string& str, char delim)
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
            prev = pos + 1;
        } while (pos < str.length() && prev < str.length());
        return result;
    }

    enum class States
    {
        Start,
        ContinueDefine
    };

    using unordered_map = std::unordered_map<std::string, std::pair<std::vector<std::string>, std::string>>;

    using MacroStrings = std::vector<std::pair<std::string, bool>>;

    std::string recursivePreprocess(std::string source, unordered_map& defines);

    void resolveMacro(std::size_t current,
                      std::size_t pos,
                      std::size_t nameSize,
                      unordered_map& defines,
                      MacroStrings& strings,
                      const std::pair<std::vector<std::string>, std::string>& thisDefine,
                      const std::string& iter)
    {
        if (iter[pos + nameSize] != '(')
        {
            throw std::runtime_error("Expected ( after macro function");
        }
        auto result = std::find(iter.begin() + pos + nameSize, iter.end(), ')');
        if (result == iter.end())
        {
            throw std::runtime_error("Expected ) after macro function");
        }
        strings.emplace(strings.begin() + current + 1, iter.substr(result - iter.begin() + 1), false);
        auto argumentStrings = std::string(iter.begin() + pos + nameSize + 1, result);
        auto rawArgumentStringsSplit = split(argumentStrings, ',');
        auto rawArguments = recursivePreprocess({iter.begin() + pos + nameSize + 1, result}, defines);
        auto argumentList = split(rawArguments, ',');
        auto replacement = thisDefine.second;
        if (thisDefine.first.size() != argumentList.size())
        {
            throw std::runtime_error("Not the same amount of arguments supplied as needed");
        }
        std::size_t i = 0;
        for (auto& argNames : thisDefine.first)
        {
            std::size_t argPos = replacement.find(argNames, 0);
            while (argPos != std::string::npos)
            {
                if (argPos == 0 || replacement[argPos - 1] != '#')
                {
                    replacement = replacement.substr(0, argPos) + " " + argumentList[i] + " "
                        + replacement.substr(argPos + argNames.size());
                }
                else if (argPos == 1 || (replacement[argPos - 1] == '#' && replacement[argPos - 2] != '#'))
                {
                    replacement = replacement.substr(0, argPos - 1) + "\"" + rawArgumentStringsSplit[i] + "\""
                        + replacement.substr(argPos + argNames.size());
                }
                else
                {
                    replacement = replacement.substr(0, argPos - 2) + argumentList[i]
                        + replacement.substr(argPos + argNames.size());
                }
                argPos = replacement.find(argNames, argPos);
            }
            i++;
        }
        strings.emplace(strings.begin() + current + 1, replacement, true);
        strings.emplace(strings.begin() + current + 1, iter.substr(0, pos), false);
        strings.erase(strings.begin() + current);
    }

    std::string recursivePreprocess(std::string source, unordered_map& defines)
    {
        States currentState = States::Start;
        static std::regex isPreprocessor("\\s*#.*", std::regex_constants::optimize);
        source = std::regex_replace(source, std::regex("\r\n"), "\n");
        bool hasPreprocessorTokens;
        std::string* currentDefine = nullptr;
        do
        {
            hasPreprocessorTokens = false;
            auto lines = split(source, '\n');
            std::uint64_t line = 0;
            for (auto& iter : lines)
            {
                line++;
                switch (currentState)
                {
                case States::Start:
                {
                    if (iter.empty())
                    {
                        continue;
                    }
                    if (!std::regex_match(iter, isPreprocessor))
                    {
                        std::vector<std::pair<std::size_t,std::size_t>> stringLiterals;
                        bool lastWasBackslash = false;
                        bool inString = false;
                        std::size_t column = 0;
                        for(auto cha : iter)
                        {
                            if(cha == '"' && !lastWasBackslash)
                            {
                                if(!inString)
                                {
                                    stringLiterals.emplace_back(column,0);
                                    inString = true;
                                }
                                else
                                {
                                    stringLiterals.back().second = column;
                                    inString = false;
                                }
                            }
                            else if(cha == '\\')
                            {
                                lastWasBackslash = !lastWasBackslash;
                            }
                            column++;
                        }
                        MacroStrings strings;
                        strings.emplace_back(iter, false);
                        for (auto&[name, replacement] : defines)
                        {
                            for (std::size_t current = 0; current < strings.size();)
                            {
                                if (strings[current].second)
                                {
                                    current++;
                                    continue;
                                }

                                std::size_t pos = strings[current].first.find(name, 0);
                                if (pos != std::string::npos && !std::any_of(stringLiterals.begin(),stringLiterals.end(),[pos](const std::pair<std::size_t,std::size_t>& pair)
                                {
                                    return pair.first < pos && pos < pair.second;
                                }))
                                {
                                    hasPreprocessorTokens = true;
                                    if (replacement.first.empty())
                                    {
                                        strings.emplace(strings.begin() + current + 1,
                                                        iter.substr(pos + name.size()),
                                                        false);
                                        strings.emplace(strings.begin() + current + 1, replacement.second, true);
                                        strings.emplace(strings.begin() + current + 1, iter.substr(0, pos), false);
                                        strings.erase(strings.begin() + current);
                                        current += 2;
                                    }
                                    else
                                    {
                                        resolveMacro(current, pos, name.size(), defines, strings, replacement, iter);
                                        current += 2;
                                    }
                                }
                                else
                                {
                                    current++;
                                }
                            }
                        }
                        iter = std::accumulate(strings.begin(),
                                               strings.end(),
                                               std::string(),
                                               [](const auto& lhs, const auto& rhs)
                                               {
                                                   return lhs + rhs.first;
                                               });
                    }
                    else
                    {
                        iter.erase(iter.begin(),iter.begin() + iter.find('#') + 1);
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
                                    arguments = split(substring, ',');
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
                        else if (iter.rfind("undef", 0) == 0)
                        {
                            iter = iter.substr(5, iter.size() - 5);
                            auto result = std::find_if_not(iter.begin(), iter.end(), [](char c)
                            { return std::isspace(c); });
                            iter.erase(iter.begin(), result);
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
            if (!lines.empty())
            {
                source = std::accumulate(lines.begin() + 1,
                                         lines.end(),
                                         std::string(lines.front()),
                                         [](const std::string& lhs, const std::string& rhs)
                                         {
                                             return lhs + "\n" + rhs;
                                         });
            }
        } while (hasPreprocessorTokens);
        return source;
    }
}

std::string OpenCL::PP::preprocess(std::string&& source)
{
    unordered_map defines;
    return recursivePreprocess(std::move(source), defines);
}

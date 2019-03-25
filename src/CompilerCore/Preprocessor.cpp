#include "Preprocessor.hpp"

#include <regex>
#include <numeric>
#include <unordered_map>

namespace
{
    std::vector<std::string> split(const std::string& str,const std::string& delim)
    {
        std::vector<std::string> result;
        std::size_t prev = 0, pos = 0;
        do
        {
            pos = str.find(delim,prev);
            if (pos == std::string::npos)
            {
                pos = str.length();
            }
            auto line = str.substr(prev,pos-prev);
            result.push_back(std::move(line));
            prev = pos + delim.length();
        }
        while(pos < str.length() && prev < str.length());
        return result;
    }
}

std::string OpenCL::PP::preprocess(std::string source)
{
    std::unordered_map<std::string,std::string> defines;
    std::regex isPreprocessor("\\s*#.*",std::regex_constants::optimize);
    source = std::regex_replace(source,std::regex("\r\n"),"\n");
    bool hasPreprocessorTokens;
    do
    {
        hasPreprocessorTokens = false;
        auto lines = split(source,"\n");
        for(auto& iter : lines)
        {
            if (iter.empty() || !std::regex_match(iter,isPreprocessor))
            {
                continue;
            }
            hasPreprocessorTokens = true;
            iter = std::regex_replace(iter,std::regex("\\s*#"),"");
            if(iter.rfind("define",0) == 0)
            {
                iter = iter.substr(6,iter.size()-6);
                auto result = std::find_if_not(iter.begin(),iter.end(),[](char c){return std::isspace(c);});
                iter.erase(iter.begin(),result);
                result = std::find_if(iter.begin(),iter.end(),[](char c){return std::isspace(c);});
                auto name = iter.substr(0,result - iter.begin());
                if(!defines.emplace(name,iter.substr(result - iter.begin(),iter.size() - (result - iter.begin()))).second)
                {
                    throw std::runtime_error(name + " is already defined");
                }
                iter = "";
            }
        }
        source = std::accumulate(lines.begin(),lines.end(),std::string());
    }
    while(hasPreprocessorTokens);
    return source;
}

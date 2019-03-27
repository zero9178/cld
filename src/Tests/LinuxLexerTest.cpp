#include "CompilerCore/Preprocessor.hpp"
#include <CompilerCore/Lexer.hpp>
#include <iostream>
#include <fstream>

int main()
{
    std::ifstream file("../src/input.c");
    if(!file.is_open())
    {
        std::cerr<<"Could not open source file";
        return -1;
    }
    std::string source;
    file.seekg(0,std::ios_base::end);
    std::size_t pos = file.tellg();
    source.resize(pos);
    file.seekg(0,std::ios_base::beg);
    file.read(source.data(),source.size());

    auto preprocessed = OpenCL::PP::preprocess(std::move(source));
    auto tokens = OpenCL::Lexer::tokenize(preprocessed);
    return 0;
}

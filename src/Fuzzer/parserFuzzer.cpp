#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Parser.hpp>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string>

extern "C" int LLVMFuzzerTestOneInput(const std::uint8_t* data, std::size_t size)
{
    if (size == 0)
    {
        return 0;
    }
    std::string input(size, ' ');
    std::transform(data, data + size, input.begin(), [](std::uint8_t byte) -> char { return static_cast<char>(byte); });

    std::cout << '\"' << input << '\"' << std::endl;
    std::stringstream ss;
    auto tokens = OpenCL::Lexer::tokenize(input, &ss);
    if (!ss.str().empty() || tokens.empty())
    {
        return 0;
    }

    OpenCL::Parser::buildTree(tokens);

    return 0;
}

#include <gtest/gtest.h>
#include <fstream>
#include <CompilerCore/C/Syntax.hpp>

auto start = R"(typedef struct Point
{
    struct Point* next;
    int array[5];
} Point;

unsigned long long getListCount(const Point* first)
{
    const Point* current = first;
    unsigned long long i = 0;
    while(current)
    {
        current = current->next;
        i++;
    }
    return i;
}

int main()
{
    Point one,two,three;
    one.next = &two;
    two.next = &three;
    three.next = 0;
    return getListCount(&one);
})";

TEST(Parser, RandomTokens)
{
    EXPECT_EXIT({
                    auto tokens = OpenCL::Lexer::tokenize(start);
                    while (true)
                    {
                        std::random_shuffle(tokens.begin(),tokens.end());
                        auto copy = tokens;
                        std::string name = "current.c";
                        std::size_t i = 0;
                        while(true)
                        {
                            std::ifstream exists(name);
                            if(exists.good())
                            {
                                std::ostringstream ss;
                                ss<<i++;
                                name = "current" + ss.str() + ".c";
                            }
                            else
                            {
                                break;
                            }
                        }
                        std::ofstream output(name,std::ios_base::trunc);
                        for(auto iter = tokens.rbegin();iter != tokens.rend(); iter++)
                        {
                            output<<iter->emitBack()<<' ';
                        }
                        output.close();
                        try
                        {
                            auto node = OpenCL::Syntax::buildTree(std::move(copy));
                        }
                        catch (std::exception&)
                        {
                        }
                        std::remove(name.c_str());
                    }
                    std::exit(0);
                }, ::testing::ExitedWithCode(0), ".*");
}

TEST(Parser,Failure1)
{
    auto failure1 = "const * & ; main ] two next { next ; i typedef { ( current next Point . array ++ ) ( ; one } i } struct . , ) current three Point ( ; one = two . -> int getListCount = long first unsigned 5 long & one int current = three unsigned ; i = return ; ; Point ) next ) ; struct three long { ; 0 while [ current Point Point ; next ; const = two first = ; } } Point & 0 long * ( * { ; getListCount , return ";
    auto tokens = OpenCL::Lexer::tokenize(failure1);
    ASSERT_ANY_THROW(OpenCL::Syntax::buildTree(std::move(tokens)));
}

TEST(Parser,Failure2)
{
    auto failure2 = "struct main { } next ; int first ( & } ) three ; two long ; ( ; one . current next getListCount ; -> first next ; 5 = 0 long unsigned } { ; { long three getListCount array = unsigned next = ) * * ( ; struct current ] Point { i long current ; two = 0 one return , return i two ++ Point const const one next Point . , * ; = ; Point . typedef int = i [ Point } while ; ) ; & Point current three & ) ( ";
    auto tokens = OpenCL::Lexer::tokenize(failure2);
    ASSERT_ANY_THROW(OpenCL::Syntax::buildTree(std::move(tokens)));
}

TEST(Parser,Declarations)
{

    auto program = R"(int main()
{
    int r;
    int *i = &r,*f = i;
}
)";
    auto tokens = OpenCL::Lexer::tokenize(program);
    ASSERT_ANY_THROW(OpenCL::Syntax::buildTree(std::move(tokens)));
}

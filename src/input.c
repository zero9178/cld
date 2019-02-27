
int foo(unsigned int one,unsigned int two);

int main()
{
    return foo(6,7);
}

int foo(unsigned int one,unsigned int two)
{
    return one < 5 || two < 8 || 3 < two;
}

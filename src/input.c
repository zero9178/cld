
unsigned int fact(unsigned int number)
{
    unsigned int result = 1uLL;
    for(unsigned int i = 2; i <= number; i++)
    {
        result *= i;
    }
    return result;
}

int main()
{
    return fact(6);
}

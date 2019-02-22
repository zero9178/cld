
int fact(int number)
{
    unsigned result = 1;
    for(unsigned i = 2; i <= number; i++)
    {
        result *= i;
    }
    return result;
}

float w0(float number,int n)
{
    float result = 0;
    float pot = number;
    for(unsigned i = 1;i < n; i++)
    {
        int nPot = 1;
        for(unsigned i2 = 0; i2 < i; i++)
        {
            nPot *= i;
        }
        if(i % 2 == 0)
        {
            result -= nPot / fact(i) * pot;
        }
        else
        {
            result += nPot / fact(i) * pot;
        }
        pot *= number;
    }
    return result;
}

float main()
{
    return w0(0.54,10);
}

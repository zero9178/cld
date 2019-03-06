
struct Test
{
    int i;
};

int main()
{
    float f[4][4];
    unsigned long long counter = 0;
    for(unsigned int i = 0; i < 4; i++)
    {
        for(unsigned int i2 = 0; i2 < 4; ++i2)
        {
            f[i][i2] = ++counter / (float)16;
        }
    }

    double sum = 0;
    for(unsigned int i = 0; i < 4; i++)
    {
        for(unsigned int i2 = 0; i2 < 4; i2++)
        {
            sum += f[i][i2];
        }
    }

    return sum * 10000000;
}

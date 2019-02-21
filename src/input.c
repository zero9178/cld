
int getBitCount(int number)
{
    int count = 0;
    for(int i = 0; i < 32 && 1<<i <= number; i++)
    {
        if(number & 1<<i)
        {
            count++;
        }
    }
    return count;
}

int main()
{
    return getBitCount(0xF0);
}

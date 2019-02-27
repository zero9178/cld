
unsigned long long someConstant()
{
    return 0xCAFEBABE;
}

int main()
{
    float f = 5 / (float)someConstant();
    return f * 1000000000000;
}

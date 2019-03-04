
int f = 5;

int* getF()
{
    return &f;
}

int main()
{
    int* i = getF();
    *i = 0;
    i = 0;
    f += 2;
    return f;
}

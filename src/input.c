
int main()
{
    int f = 5;
    int* i = &f;
    f = 5;
    *i = 3;
    return f;
}

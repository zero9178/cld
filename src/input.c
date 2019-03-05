
int foo(int a[6])
{
    return a[0];
}

int main()
{
    int i[6];
    i[0] = 5;
    return foo(i);
}

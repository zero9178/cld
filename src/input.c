
struct Test
{
    int f;
    int i;
};

struct Test foo()
{
    struct Test t;
    t.f = 2;
    t.i = 7;
    return t;
}

int i;

int main()
{
    return foo().i;
}


typedef struct Point
{
    struct Point* next;
    int array[5];
} Point;

unsigned long long getListCount(const Point* first)
{
    const Point* current = first;
    unsigned long long i = 0;
    while(current)
    {
        current = current->next;
        i++;
    }
    return i;
}

int main()
{
    Point one,two,three;
    one.next = &two;
    two.next = &three;
    three.next = 0;
    return getListCount(&one);
}

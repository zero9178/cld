
typedef struct Point
{
    struct Point* next;
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
    Point first;
    Point second;
    Point third;
    first.next = &second;
    second.next = &third;
    third.next = 0;
    return getListCount(&first);
}

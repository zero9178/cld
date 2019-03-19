//
//typedef struct Point
//{
//    struct Point* next;
//    int array[5];
//} Point;
//
//unsigned long long getListCount(const Point* first)
//{
//    const Point* current = first;
//    unsigned long long i = 0;
//    while(current)
//    {
//        current = current->next;
//        i++;
//    }
//    return i;
//}

int main()
{
    int i[][5] = {5,3,43,4,34,34};
    return i[0][0] + i[1][1];
}

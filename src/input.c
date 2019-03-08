
struct Point
{
    double x;
    double y;
};

void incrementPoint(struct Point* p)
{
    p->x += 1;
    p->y += 1;
}

int main()
{
    struct Point p0;
    p0.x = p0.y = 5.0;
    incrementPoint(&p0);
    return (int)(p0.x + p0.y);
}

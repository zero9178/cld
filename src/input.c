
struct Point
{
    double x,y;
};

int main()
{
    struct Point p0,p1;
    p0.x = p0.y = 5;
    p1 = p0;
    return p1.x + p1.y;
}

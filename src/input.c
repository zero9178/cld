
struct Point
{
    double x,y;
};

int main()
{
    struct Point p;
    p.x = p.y = 5.0;
    return p.x * p.y;
}

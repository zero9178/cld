
unsigned int findNull(const char* s)
{
    unsigned int i = 0;
    while(s[i] != '\0')
    {
        i++;
    }
    return i;
}

int main()
{
    char* s = "test";
    return findNull(s);
}

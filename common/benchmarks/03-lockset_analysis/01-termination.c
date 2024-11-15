int f(int i)
{
    if (i == 0)
        return 1;
    else
        return i * f(i-1);
}

int main()
{
    f(42);
}

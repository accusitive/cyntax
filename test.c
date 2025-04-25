int main()
{
    struct outer
    {
        struct
        {
            int *xptr;
        } s;
        int *xptr;
    } s;
    struct xy
    {
        int x;
        int y;
    };

    // int *a = 5, **b = 2;
    // int a = (int b) 5;
    // int sum = 0;
    // for (int i = 0; i < 10; i = i + 1)
    // {
    //     for (int j = 0; i < j; j = j + 1)
    //     {
    // struct outer o = s;
    struct xy lhs;
    struct xy
    {
        int x;
        int y;
    } rhs = lhs;

    // }
    // }
}
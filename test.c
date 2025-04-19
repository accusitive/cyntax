int main()
{
    int x[static const 5];
    int x[const *];
    int (*f(int))(double);
    int (*f())();
    int (((*f)));
    int x[static *];
    int a[const volatile restrict 10];

}
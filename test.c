// prefix an identifier with harness_ to avoid symbol clashing in the test environment
#ifdef harness
#define method_name(name) harness_##name
#else
#define method_name(name) name
#endif

// function parameters
#define params (int lhs, int rhs)
// args for harness to call with
#define args (2, 4)
// used for harness to define the extern symbol
#define proto(name) int name params

int method_name(something)()
{
   return 42;
}

proto(method_name(run))
{

   // int (*test)() = method_name(something);
   int x = 0;
   if (1) {
      x = 5;
   }  // int y = 0;
   // for (int i = 0; i < 100; i = i + 1)
   // {
   //    x = x + i;
   // }
   // while (test(x) < 100) {
   // x = x + 1;
   // }
   return x;
}
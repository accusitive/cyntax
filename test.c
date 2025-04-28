int

#ifdef harness /* true if compiling from GCC, false if compiling with cyntax */
harness_x() {
#else
x() {
#endif
   int a = 55;
   int *b = &a;
   int **c = &b;
   int ***d = &c;
   int e = ***d;
   return e;
}
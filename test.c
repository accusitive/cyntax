int

#ifdef harness /* true if compiling from GCC, false if compiling with cyntax (used to change symbol name)*/
harness_x() {
#else
x() {
#endif
   int a = 55;
   a = 10;
   // xy.x = 0;
   // xy.y = 1;

   return a=20;
}
int
#ifdef harness /* true if compiling from GCC, false if compiling with cyntax (used to change symbol name)*/
harness_x()
{
#else
x()
{
#endif
   int accumulator = 0;
   for(int i = 0; i < 5; i=i+1) {
      for(int j = 0; j < 5; j=j+1) {
         if (i==5 && j==5) {
            accumulator = 100000;
         }
         accumulator  = accumulator + 10;
      }
   }
   while(accumulator < 1000000) {
      accumulator = accumulator + 1;
   }
   
   return accumulator;
}



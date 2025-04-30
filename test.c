#ifdef harness
#define method_name(name) harness_##name
#else
#define method_name(name) name
#endif

int method_name(x)()
{
   struct Vec2_I32
   {
      unsigned short pad;
      int x;
      int y;
   };

   struct Vec2_I32 test;
   test.x = 1;
   test.y = 2;

   return test.x + test.y;
}

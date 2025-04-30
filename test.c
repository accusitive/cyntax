#ifdef harness
#define method_name(name) harness_##name
#else
#define method_name(name) name
#endif

long method_name(x)()
{
   struct I64 {
      long int number;
   };
   struct I32I64 {
      int first;
      struct I64 second;
   };
   struct Vec2_I32
   {
      unsigned short pad;
      struct I32I64 i;
      int x;
      int y;
   };

   struct Vec2_I32 test;

   long int something = test.i.second.number;
   something = 100;
   // test.i.second.number = 155;
   // test.x = 1;
   // test.y = 2;
   // for(int i = 0; i < 100; i = i + 1) {
   //    for(int j = 0; j < 100; j = j + 1) {
   //       struct Vec2_I32 test2;

   //       // test.x = test2.y = test.x + 1;
   //       test.i.first = 0;
   //    }   
   // }
   return test.i.second.number;
}

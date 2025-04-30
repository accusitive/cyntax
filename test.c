#ifdef harness
#define method_name(name) harness_##name
#else
#define method_name(name) name
#endif

#define args (2, 4)
#define invoke method_name(x) args

int method_name(x) (int x, int y) {
   return x + 4;
}

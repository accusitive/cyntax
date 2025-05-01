// prefix an identifier with harness_ to avoid symbol clashing in the test environment
#ifdef harness
# define method_name(name) harness_##name
#else
# define method_name(name) name
#endif

// function parameters
#define params (int lhs, int rhs)
// args for harness to call with 
#define args (2,4)

#define proto(name) int name params

int something() {

}

proto(method_name(run)) {
   int a = 5;
   int b = something;
   return 0;
}
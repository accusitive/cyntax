#ifdef test_exists
int test = 1;
#else
#ifdef a
int inner;
#endif
int test = 0;
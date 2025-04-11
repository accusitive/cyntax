#define foobar 1
#define C(x,y) x##y
#define D(x) (C(x,bar))
// D(foo)
#error "Test"
C("test", "again");

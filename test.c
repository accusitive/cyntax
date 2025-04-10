// #define clam 5
// #define add(x,y) x+y #x x##y
// add(2,clam);
#define glue(x,y,z) x##y##z

glue(2,4,6);
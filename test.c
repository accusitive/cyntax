#define a 10
#define b a
#define c b
#define d c

int x = d;
#define c 42
int y  = d;
// Normal macro
#define PI 3.14159
#define MESSAGE "Hello, Macro!"

// Function-style macros
#define SQUARE(x) ((x) * (x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MULTIPLY(x, y) ((x) * (y))

int pi = PI;
int msg = MESSAGE;

int a = 5;
int square_a = SQUARE(a);
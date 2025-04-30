#include <stdio.h>

extern long int x();

#define harness
#include "test.c"

int main() {
    printf("Base truth: x() = %i\n", invoke);
    printf("GLScc       x() = %i\n", x args);

}
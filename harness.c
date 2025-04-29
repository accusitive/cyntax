#include <stdio.h>

extern long int x();

#define harness
#include "test.c"

int main() {
    printf("Base truth: x() = %i\n", harness_x());
    printf("GLScc       x() = %i\n", x());

}
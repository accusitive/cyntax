#include <stdio.h>

extern long int x();

#define harness
#include "test.c"

int main() {
    printf("x() = %p\n", x());
    printf("h_x() = %p\n", harness_x());
}
#include <stdio.h>


#define harness
#include "test.c"
extern proto(run);

int main() {
    printf("Base truth: x() = %i\n", harness_run args);
    printf("GLScc       x() = %i\n", run args);
}
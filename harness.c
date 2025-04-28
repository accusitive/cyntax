#include <stdio.h>

extern long int x();

int main() {
    int result = x();
    printf("x() = %p\n", result);
}
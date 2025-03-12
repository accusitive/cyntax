// Function for demonstrating function calls
int add(int a, int b) {
    return a + b;
}

int main() {
    // 1. Constant Expression
    const int CONST_VAL = 100;

    // 2. Assignment Expression
    int x = 10, y = 20;

    // 3. Arithmetic Expression
    int sum = x + y;
    int product = x * y;

    // 4. Relational Expression
    int isGreater = x > y;

    // 5. Logical Expression
    int logicResult = (x < y) && (y > 15);

    // 6. Bitwise Expression
    int bitwiseAnd = x & y;
    int bitwiseOr = x | y;

    // 7. Conditional (Ternary) Expression
    int maxVal = (x > y) ? x : y;

    // 8. Comma Expression
    int a = (x++, y++, x + y);  // Both x and y incremented before result

    // 9. Function Call Expression
    int result = add(x, y);

    // 10. Cast Expression
    double divisionResult = (double)x / y;

    // 11. sizeof Expression
    printf("Size of int: %zu\n", sizeof(int));

    // Output results
    printf("Constant Value: %d\n", CONST_VAL);
    printf("Sum: %d, Product: %d\n", sum, product);
    printf("Relational (x > y): %d\n", isGreater);
    printf("Logical ((x < y) && (y > 15)): %d\n", logicResult);
    printf("Bitwise AND: %d, Bitwise OR: %d\n", bitwiseAnd, bitwiseOr);
    printf("Max Value: %d\n", maxVal);
    printf("Comma Expression Result (after increments): %d\n", a);
    printf("Function Call Result: %d\n", result);
    printf("Cast Expression (x / y as double): %.2f\n", divisionResult);

    return 0;
}

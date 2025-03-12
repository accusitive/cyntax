// Initial check
#define FEATURE_A 1
#undef FEATURE_A

#ifdef FEATURE_A
    #define TEST_1 0  // Failed: #undef didn't work
#else
    #define TEST_1 1  // Passed: #undef worked
#endif

// Conditional check with #if and #else
#define VALUE 10

#if VALUE == 10
    #define TEST_2 1  // Passed: Correct #if behavior
#else
    #define TEST_2 0  // Failed: Incorrect #if behavior
#endif

// Ensuring #else works
#undef VALUE

#ifdef VALUE
    #define TEST_3 0  // Failed: VALUE should be undefined
#else
    #define TEST_3 1  // Passed: VALUE is correctly undefined
#endif

// Final pass/fail define
#if TEST_1 && TEST_2 && TEST_3
    #define TEST_PASS 1
#else
    #define TEST_PASS 0
#endif

int main() {
    printf("Preprocessor Test Result: %d\n", TEST_PASS);
    return 0;
}
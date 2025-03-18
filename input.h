#define FLAG_1
#define VALUE 10

#ifdef FLAG_1
    #ifndef FLAG_2
        #if VALUE > 5
            #define RESULT 1
        #else
            #define RESULT 2
        #endif
    #else
        #define RESULT 3
    #endif
#else
    #define RESULT 4
#endif
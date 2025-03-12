#define five 5

#if five == 5
#define FIVE_VALID 1
#else
#define FIVE_VALID 0
#endif

#ifdef clam 
#define CLAM_DEFINED 1
#else
#define CLAM_DEFINED 0
#endif

#ifndef clam2
#define CLAM2_NDEFINED 1
#else 
#define CLAM2_NDEFINED 0
#endif


#if 0==1 
#define asd 0
#elif 1==1
#define asd 1
#else
#define asd 2
#endif

int main () {
    // int fv = FIVE_VALID;
    // int cv = CLAM_DEFINED;
    // int cd2 = CLAM2_NDEFINED;
    int a = asd;
}
#ifdef clam
#if clam == 2
# define _clam 2
#elif clam == 3
# define _clam 3
#else
# define _clam 100
#endif 
#endif


int c = _clam;
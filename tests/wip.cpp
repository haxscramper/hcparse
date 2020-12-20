#define A int
#define B(arg) #arg
#define C(arg, arg2) #arg## #arg2
#define D(arg) "hhhh"## #arg

#ifdef HELLO
#    define world 12
#else
#    define world 0
#endif

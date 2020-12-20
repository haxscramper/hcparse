#include <vector>

#define A int
#define B(arg) #arg
#define C(arg, arg2) #arg## #arg2
#define D(arg) "hhhh"## #arg

#ifdef HELLO
#    define world 12
#else
#    define world 0
#endif


using const_int = const int;

template <class Z>
class Base
{
  public:
    int       fld;
    const_int fld2;
    void      hell() {}
};

template <class Z0>
class Base2
{
  public:
    void fuck() {}
};

template <class T>
class A
    : Base<T>
    , Base2<T>
{
  public:
    int              b;
    std::vector<int> ss;

    A() {}
    ~A() {}
    A(int a) {}

    // template <class TNested>
    // struct MoreGeneric {
    //     TNested field;
    // };

    void operator<<(T arg) {}

#if false
    void operator*(int v) {}
    void operator/(int v) {}
    void operator%(int v) {}
    void operator^(int v) {}
    void operator&(int v) {}
    void operator|(int v) {}
    void operator~() {}
    void operator!() {}
    void operator=(int v) {}
    void operator<(int v) {}
    void operator>(int v) {}
    void operator+=(int v) {}
    void operator-=(int v) {}
    void operator*=(int v) {}
    void operator/=(int v) {}
    void operator%=(int v) {}
    void operator^=(int v) {}
    void operator&=(int v) {}
    void operator|=(int v) {}
    void operator<<(int a) {}
    void operator>>(int a) {}
    void operator>>=(int a) {}
    void operator<<=(int a) {}
    void operator==(int a) {}
    void operator!=(int a) {}
    void operator<=(int a) {}
    void operator>=(int b) {}
    void operator&&(int b) {}
    void operator||(int b) {}
    void operator++(int b) {}
    void operator--(int b) {}
    void operator,(int b) {}
    void operator->*(int c) {}
    void operator->() {}
    void operator()(int b) {}
    void operator[](int b) {}

#endif
};


char operator/(std::vector<int> c, int v) {}
char operator%(std::vector<int> c, int v) {}

typedef int(GetSyncData)(double, double*, double, int, int, int, void*);

struct S {};

S    operator""_S(const char* s, unsigned long len) { return S(); }
void operator"" _km(long double); // OK, will be called for 1.0_km
int  operator"" _i18n(const char*, unsigned long); // OK

float operator""_e(const char*); // OK


namespace cxx {
struct B;

inline namespace __impl12 {
    struct S {
        enum class C
        {
            Hello = 33,
            World = 12,
            Nice  = 13,
            HHHZ,
            ZZZZ = 13
        };
        B* s;
    };
} // namespace __impl12

void operator*(S a, S b) {}

struct B {
    void        hello() {}
    static void explode(B b, S s) {}
    B() {}
};

} // namespace cxx

void usesEnum(cxx::S::C en, cxx::S other) {}

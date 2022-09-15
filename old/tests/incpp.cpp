#pragma once

#include <stdio.h>
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

struct CppBase {
    CppBase(const char* args) {
        printf("Non-default constructor -%s\n", args);
    }

    virtual void baseMethod(int arg) {
        printf("arg from nim - %d -\n", arg);
    }

    virtual void baseMethod2() { printf("baseMethod2()\n"); }
};

// TODO default initalization of template

#include <initializer_list>


struct H {
    int field;
    H(int arg);
    H(std::initializer_list<int> ilist);
};

struct Aggr {
    int  field1;
    char field2;
};

struct Aggr2 {
    Aggr  aggr;
    float field3;
};

void test0(H nice);
void test1(H nice = 12);
void test2(H nice = {1, 2, 3});
void test3(Aggr aggr = {1, 'c'});
void test4(Aggr2 aggr = {{1, 'c'}, 1.2});
void test5(Aggr2 aggr = {Aggr({1, 'c'}), 1.2});

#if false

template <class T>
struct Templated {
    T arg1;
    T arg2;
};

template <class T>
void test6(Templated<T> = {12, 12}) {}

#endif

#include <initializer_list>

template <class T>
struct HashIterator
{
  typedef T* pointer;
  typedef T& reference;
  typedef T valueType;

  reference operator()(const T){}
  reference operator++();

  bool operator!=(const T& other) {}
  bool operator==(const T& other) {}
};

template <class T>
class HashImpl {
  typedef HashIterator<T> iterator;

public:
  iterator begin() {}
  iterator end() {}
};

template <class T>
class Set {
  HashImpl<T> table;

public:
  Set(std::initializer_list<T> ilist) {}
};

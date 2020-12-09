#include <vector>

template <class Z>
class Base
{
  public:
    void hell() {}
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

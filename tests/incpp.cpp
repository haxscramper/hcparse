#include <vector>

template <class T>
class A
{
  public:
    int              b;
    std::vector<int> ss;

    template <class TNested>
    struct MoreGeneric {
        TNested field;
    };

    void operator<<(T arg) {}

    void operator+(int v) {}
    void operator-(int v) {}

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
char operator^(std::vector<int> c, int v) {}
char operator&(std::vector<int> c, int v) {}

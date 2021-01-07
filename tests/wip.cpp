#include <initializer_list>

struct H {
    int field;
    H(int arg) : field(arg) {}
    H(std::initializer_list<int> ilist) {}
};

struct Aggr {
    int  field1;
    char field2;
};

struct Aggr2 {
    Aggr  aggr;
    float field3;
};

void test0(H nice) {}
void test1(H nice = 12) {}
void test2(H nice = {1, 2, 3}) {}
void test3(Aggr aggr = {1, 'c'}) {}
void test4(Aggr2 aggr = {{1, 'c'}, 1.2}) {}
void test5(Aggr2 aggr = {Aggr({1, 'c'}), 1.2}) {}

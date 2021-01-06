#include <iostream>

struct H {
    int field;
    H(int arg) : field(arg) {}
};

void test(H nice = 12) {
    std::cout << "Implementation for C++ side: " << nice.field << "\n";
}

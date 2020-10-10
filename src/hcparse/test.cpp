#define ABCD 2

#pragma STDC FENV_ACCESS
#define EE(X) #X## #X

#ifdef ABCD
struct A {};
#else
struct B {};
#endif

#ifndef ABCD
A ee() {
}
#elif ABCD == 2
A* ee2() {
}
#else
A ee3() {
}
#endif

#if !defined(DCBA) && (ABCD < 2 * 4 - 3)
A* ee4() {
}
#endif


int main() {
}

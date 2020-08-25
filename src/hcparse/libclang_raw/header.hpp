#include <cstring>
#include <iostream>
#include <map>
#include <utility>

class C;

int get_number(int val) {
    return val + 1;
}

void func_pointer_test() {
    int (*func)(int) = &get_number;
    std::cout << " [ " << func(0) << " ] \n";
}

class K
{
  public:
    int get_number(int val) {
        return this->increment + val;
    }
    int increment = 0;
};

void method_pointer_test() {
    K  val;
    K* ptr        = &val;
    val.increment = 10;

    int (K::*method_pointer)(int) = &K::get_number;


    std::cout << " [ " << (ptr->*method_pointer)(0) << " ] \n";
    std::cout << " [ " << (val.*method_pointer)(0) << " ] \n";
}

void void_ptr_test() {
    int var = 10;

    void* void_ptr = &var;
    int*  int_ptr  = &var;

    if (*static_cast<int*>(void_ptr) == var) {
        std::cout << void_ptr << "\n";
        std::cout << int_ptr << "\n";
    }
}


template <class T>
T& scastp(void* arg) {
    return *static_cast<T*>(arg);
}

using signal_msg = void*;

using slot_func   = void (C::*)(signal_msg);
using signal_func = void (C::*)(signal_msg);

template <class D>
slot_func slot_cast(void (D::*func)(signal_msg)) {
    return static_cast<slot_func>(func);
}

template <class D>
signal_func signal_cast(void (D::*func)(signal_msg)) {
    return static_cast<signal_func>(func);
}


struct signal_compare {
    bool operator()(const signal_func& lhs, const signal_func& rhs) {
        return std::memcmp(&lhs, &rhs, sizeof(lhs)) < 0;
    }
};

using signal_map = std::multimap<
    signal_func,              //
    std::pair<C*, slot_func>, //
    signal_compare>;

using signal_iter = signal_map::iterator;


class C
{
  public:
    void set_connection(signal_func signal, C* target, slot_func slot) {
        connects.insert({signal, {target, slot}});
    }

    void emit_signal(signal_func signal, signal_msg data) {
        std::pair<signal_iter, signal_iter>
            equal_range = connects.equal_range(signal);

        for (signal_iter slot = equal_range.first;
             slot != equal_range.second;
             ++slot) {

            C*          target = slot->second.first;
            signal_func signal = slot->second.second;

            (target->*signal)(data);
        }
    }


  public:
    void signal_1(signal_msg arg) {
        std::cout << "Executed signal 1\n";
        emit_signal(&C::signal_1, arg);
    }

    void signal_2(signal_msg arg) {
        std::cout << "Executed signal 2\n";
        emit_signal(&C::signal_2, arg);
    }

  public:
    void slot_1(signal_msg _arg) {
        std::string& arg = scastp<std::string>(_arg);
        std::cout << "Called slot 1: " << arg;
    }

    void slot_2(signal_msg arg) {
        std::cout << "Called slot 2: " << scastp<std::string>(arg);
    }

  private:
    signal_map connects;
};


#define connect(emitter, signal, target, slot)                            \
    (emitter)->set_connection(                                            \
        static_cast<signal_func>(signal),                                 \
        target,                                                           \
        static_cast<slot_func>(slot))

/**
 * A parsed comment.
 */
void signal_slots_test() {
    std::cout << "test 2\n\n\n";

    C emitter;
    C reciever;

    std::string arg = "argument string\n";

    connect(&emitter, &C::signal_2, &reciever, &C::slot_2);

    std::cout << "testing signal 2\n";
    emitter.signal_2(&arg);

    std::cout << "testing signal 1\n";
    emitter.signal_1(&arg);
}


int main() {
    func_pointer_test();
    method_pointer_test();

    void_ptr_test();

    signal_slots_test();
}

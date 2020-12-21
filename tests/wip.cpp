#pragma once

#include <stdio.h>

struct CppBase {
    CppBase(const char* args) {
        printf("Non-default constructor -%s\n", args);
    }

    virtual void baseMethod(int arg) {
        printf("arg from nim - %d -\n", arg);
    }

    virtual void baseMethod2() {
        printf("baseMethod2()\n");
    }
};

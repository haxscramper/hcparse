#pragma once

#include <stdio.h>

struct CppBase {
    virtual void baseMethod(int arg) {
        printf("arg from nim - %d -\n", arg);
    }
};

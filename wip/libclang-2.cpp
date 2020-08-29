#include <clang-c/Index.h>
#include <iostream>

int main() {
    CXIndex           index  = clang_createIndex(0, 0);
    const char*       args[] = {"-v"};
    CXTranslationUnit unit   = clang_parseTranslationUnit(
        index,
        "/tmp/test-file.cpp",
        args,
        1,
        nullptr,
        0,
        CXTranslationUnit_None);


    if (unit == nullptr) {
        std::cout << "Unable to parse translation unit. Quitting.\n";
    }
}

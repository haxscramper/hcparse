// https://shaharmike.com/cpp/libclang/

#include <clang-c/Index.h>
#include <iostream>
#include <vector>
using namespace std;

#define let const auto
#define CHILD_VISIT_PARAMS                                                \
    CXCursor cursor, CXCursor parent, CXClientData client_data

ostream& operator<<(ostream& stream, const CXString& str) {
    stream << clang_getCString(str);
    clang_disposeString(str);
    return stream;
}


using CXBuf    = std::vector<CXCursor>;
using CXBufPtr = CXBuf*;
CXBufPtr toBufPtr(void* client_data) {
    return reinterpret_cast<CXBufPtr>(client_data);
}


CXChildVisitResult visitFunction(CHILD_VISIT_PARAMS) {

    cout << "Cursor " << clang_getCursorSpelling(cursor) << "\n";

    CXBuf children;

    clang_visitChildren(
        cursor,
        [](CHILD_VISIT_PARAMS) {
            CXBufPtr ptr = toBufPtr(client_data);

            let kind = clang_getCursorKind(cursor);

            std::cout << clang_getCursorKindSpelling(kind);

            std::cout << "  \e[33m" << clang_getCursorSpelling(cursor)
                      << "\e[39m \e[32m"
                      << clang_getTypeSpelling(clang_getCursorType(cursor))
                      << "\e[39m\n";

            return CXChildVisit_Continue;
        },
        &children);


    return CXChildVisit_Continue;
}

CXChildVisitResult visitToplevel(CHILD_VISIT_PARAMS) {
    let kind = clang_getCursorKind(cursor);

    if (kind == CXCursor_FunctionDecl) {
        return visitFunction(cursor, parent, client_data);
    } else {
        return CXChildVisit_Recurse;
    }
}

int main() {
    CXIndex           index = clang_createIndex(0, 0);
    CXTranslationUnit unit  = clang_parseTranslationUnit(
        index,
        "/usr/include/clang-c/Index.h",
        nullptr,
        0,
        nullptr,
        0,
        CXTranslationUnit_None);

    if (unit == nullptr) {
        cerr << "Unable to parse translation unit. Quitting." << endl;
        exit(-1);
    }

    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    clang_visitChildren(cursor, visitToplevel, nullptr);

    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(index);
}

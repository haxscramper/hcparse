// https://shaharmike.com/cpp/libclang/

#include <clang-c/Index.h>
#include <fmt/core.h>
#include <fmt/ostream.h>
#include <fstream>
#include <iostream>
#include <utility>
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


ostream& operator<<(ostream& stream, const CXType& type) {
    stream << clang_getTypeSpelling(type);
    return stream;
}


using CXBuf    = std::vector<CXCursor>;
using CXBufPtr = CXBuf*;
using Str      = std::string;

template <typename T1, typename T2>
using Pair = std::pair<T1, T2>;

template <typename T>
using Vec = std::vector<T>;

Str toStr(const CXString& str) {
    let result = Str(clang_getCString(str));
    clang_disposeString(str);
    return result;
}


template <typename T1, typename T2>
Pair<T1, T2> mkPair(T1 t1, T2 t2) {
    return std::make_pair(t1, t2);
}


CXBufPtr toBufPtr(void* client_data) {
    return reinterpret_cast<CXBufPtr>(client_data);
}


Str toNimType(CXType type, int level = 0) {
    let kindStr = toStr(clang_getTypeSpelling(type));
    switch (type.kind) {
        case CXType_Typedef: return kindStr;
        case CXType_Enum: return kindStr;

        case CXType_UInt: return "cuint";
        case CXType_Char_S: return "cstring";
        case CXType_Void: return "void";
        case CXType_Int: return "cint";
        case CXType_LongLong: return "clonglong";
        case CXType_Double: return "cdouble";
        case CXType_ULongLong: return "culonglong";

        case CXType_Elaborated: {
            let fullStr = toStr(clang_getTypeSpelling(type));
            if (fullStr.rfind("enum ") == 0) {
                return fullStr.substr(5, fullStr.size());
            } else if (fullStr.rfind("struct ") == 0) {
                return fullStr.substr(7, fullStr.size());
            } else {
                std::cout << type << " \e[31m"
                          << clang_Type_getNamedType(type) << "\e[39m\n";
                return "\e[91m!!!\e[39m";
            }
        }

        case CXType_Pointer: {
            return fmt::format(
                "ptr[{}]",
                toNimType(clang_getPointeeType(type), level + 1));
        }


        default: {
            std::cout << "  " << kindStr << " "
                      << clang_getTypeKindSpelling(type.kind) << "\n";

            return "eee";
        }
    }
}

Str toNimProcDecl(
    Str                    procName,
    Vec<Pair<Str, CXType>> arguments,
    CXType                 rType) {

    Str args;

    int idx = 0;
    for (auto& arg : arguments) {
        Str name = arg.first;
        if (name.size() == 0) {
            name = "arg_" + std::to_string(idx);
            ++idx;
        }

        args += fmt::format(
            "  {}: {}, # `{}`\n",
            name,
            toNimType(arg.second),
            clang_getTypeSpelling(arg.second));
    }

    return fmt::format(
        "proc {}(\n{}): {}", procName, args, toNimType(rType));
}

CXChildVisitResult visitFunction(CHILD_VISIT_PARAMS) {

    CXBuf          children;
    std::ofstream* outfile = reinterpret_cast<std::ofstream*>(client_data);

    clang_visitChildren( // Visit direct children of function node
        cursor,
        [](CHILD_VISIT_PARAMS) {
            CXBufPtr ptr = toBufPtr(client_data);
            ptr->push_back(cursor);
            return CXChildVisit_Continue;
        },
        &children);

    let funcResT = clang_getResultType(clang_getCursorType(cursor));

    Vec<Pair<Str, CXType>> arguments;
    for (CXCursor& cursor : children) {
        let kind = clang_getCursorKind(cursor);

        arguments.push_back(mkPair(
            toStr(clang_getCursorSpelling(cursor)),
            clang_getCursorType(cursor)));
    }


    let nimProc = toNimProcDecl(
        toStr(clang_getCursorSpelling(cursor)), arguments, funcResT);

    (*outfile) << nimProc << "\n\n";


    return CXChildVisit_Continue;
}

CXChildVisitResult visitToplevel(CHILD_VISIT_PARAMS) {
    let kind = clang_getCursorKind(cursor);

    if (clang_Location_isFromMainFile(clang_getCursorLocation(cursor)))
    // Only interested in functions defined in main file being processed
    {
        if (kind == CXCursor_FunctionDecl) {
            return visitFunction(cursor, parent, client_data);
        } else {
            return CXChildVisit_Recurse;
        }
    } else {
        return CXChildVisit_Continue;
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

    std::ofstream outfile;

    outfile.open("result.nim");

    clang_visitChildren(cursor, visitToplevel, &outfile);

    outfile.close();

    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(index);
}

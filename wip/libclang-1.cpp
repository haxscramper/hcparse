// https://shaharmike.com/cpp/libclang/

#include <clang-c/Index.h>
#include <fmt/core.h>
#include <fmt/ostream.h>
#include <fstream>
#include <iostream>
#include <sstream>
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


ostream& operator<<(ostream& stream, const CXCursor& cursor) {
    stream << clang_getCursorSpelling(cursor);
    return stream;
}


ostream& operator<<(ostream& stream, const CXCursorKind& kind) {
    stream << clang_getCursorKindSpelling(kind);
    return stream;
}

struct ClientData {
    std::ofstream*    outfile;
    CXTranslationUnit unit;
};


ClientData* toClientData(void* data) {
    return reinterpret_cast<ClientData*>(data);
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


Str fixTypeName(Str fullStr) {
    if (fullStr.rfind("enum ") == 0) {
        return fullStr.substr(5, fullStr.size());
    } else if (fullStr.rfind("struct ") == 0) {
        return fullStr.substr(7, fullStr.size());
    } else {
        // std::cout << type << " \e[31m\e[3mUNFIX\e[23m\e[39m: \e[31m"
        //           << clang_Type_getNamedType(type) << "\e[39m\n";
        return fullStr;
        // return "\e[91m!!!\e[39m";
    }
}

Str fixTypeName(CXType type) {
    return fixTypeName(toStr(clang_getTypeSpelling(type)));
}

Str toNimType(CXType type, int level = 0) {
    let kindStr = toStr(clang_getTypeSpelling(type));
    switch (type.kind) {
        case CXType_Typedef: return kindStr;
        case CXType_Enum: return kindStr;
        case CXType_Record: return fixTypeName(type);

        case CXType_UInt: return "cuint";
        case CXType_Char_S: return "cstring";
        case CXType_Void: return "void";
        case CXType_Int: return "cint";
        case CXType_LongLong: return "clonglong";
        case CXType_Double: return "cdouble";
        case CXType_ULongLong: return "culonglong";
        case CXType_ULong: return "culong";

        case CXType_Elaborated: {
            return fixTypeName(type);
        }

        case CXType_Pointer: {
            CXType pointee = clang_getPointeeType(type);
            if (pointee.kind == CXType_Void) {
                return "pointer";
            } else if (pointee.kind == CXType_Char_S) {
                return "cstring";
            } else if (pointee.kind == CXType_FunctionProto) {
                return toNimType(pointee);
            } else {
                return fmt::format(
                    "ptr[{}]", toNimType(pointee, level + 1));
            }
        }

        case CXType_ConstantArray: {

            let elemType = clang_getElementType(type);
            // std::cout << "  \e[31m\e[3mELEMENT\e[23m\e[39m: " <<
            // elemType
            //           << " " << clang_getTypeKindSpelling(elemType.kind)
            //           << "\n";

            return fmt::format(
                "array[{}, {}]",
                clang_getNumElements(type),
                toNimType(elemType));
        }

        case CXType_FunctionProto: {
            let args = clang_getNumArgTypes(type);
            Str argParams;

            for (int arg = 0; arg < args; ++arg) {
                argParams += fmt::format(
                    "a{}: {}",
                    arg,
                    toNimType(clang_getArgType(type, arg)));
                if (arg != args - 1) {
                    argParams += ", ";
                }
            }

            return fmt::format(
                "proc({}): {} {{.cdecl.}}",
                argParams,
                fixTypeName(toNimType(clang_getResultType(type))));
        }

        default: {
            std::cout << "  \e[31m\e[3mCANT TRANSFORM\e[23m\e[39m: \e[92m"
                      << clang_getTypeKindSpelling(type.kind) << "\e[39m "
                      << kindStr << "\n";


            return "eee";
        }
    }
}

Str fixArgName(Str name) {
    if (name == "begin") {
        return "begin";
    } else if (name == "end") {
        return "cend";
    } else if (name == "type") {
        return "ctype";
    } else if (name == "range") {
        return "crange"; // cringe
    } else if ('A' <= name[0] and name[0] <= 'Z') {
        name[0] = std::tolower(name[0]);
        return name;
    } else {
        return name;
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
        if (idx == 0) {
            args += "\n";
            ++idx;
        }


        if (name.size() == 0) {
            name = "arg_" + std::to_string(idx);
            ++idx;
        } else {
            name = fixArgName(name);
        }

        args += fmt::format(
            "  {}: {}, # `{}`\n",
            name,
            toNimType(arg.second),
            clang_getTypeSpelling(arg.second));
    }

    // TODO add 'declared in file:line:col' comment into the `importc`
    Str importcInfo = fmt::format(
        "{{.\n    cdecl,\n    importc: \"{}\",\n    dynlib: libclang\n  "
        ".}}",
        procName);

    return fmt::format(
        "proc {}*({}): {} {}",
        procName,
        args,
        toNimType(rType),
        importcInfo);
}

CXChildVisitResult visitFunction(CHILD_VISIT_PARAMS) {

    CXBuf       children;
    ClientData* data = toClientData(client_data);

    clang_visitChildren( // Visit direct children of function node
        cursor,
        [](CHILD_VISIT_PARAMS) {
            CXBufPtr ptr = toBufPtr(client_data);

            if (toStr(clang_getCursorSpelling(cursor))
                == toStr(
                    clang_getTypeSpelling(clang_getCursorType(cursor)))) {
            } else {
                ptr->push_back(cursor);
            }
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

    *(data->outfile) << nimProc << "\n\n";


    return CXChildVisit_Continue;
}


CXBuf directSubnodes(CXCursor cursor) {
    CXBuf children;
    clang_visitChildren(
        cursor,
        [](CHILD_VISIT_PARAMS) {
            CXBufPtr ptr = toBufPtr(client_data);
            ptr->push_back(cursor);
            return CXChildVisit_Continue;
        },
        &children);

    return children;
}

void pprintDirect(CXCursor cursor, int ident) {
    let pref = std::string(' ', ident * 2);
    fmt::print(
        "{}{} {} {}\n",
        pref,
        cursor,
        clang_getCursorKind(cursor),
        clang_getCursorType(cursor));

    for (auto& subnode : directSubnodes(cursor)) {
        let kind = clang_getCursorKind(subnode);
        std::cout << pref << "  " << kind << "\n";
    }
}

CXChildVisitResult visitStructDecl(CHILD_VISIT_PARAMS) {

    let         kind = clang_getCursorKind(cursor);
    ClientData* data = toClientData(client_data);

    *(data->outfile) << fmt::format(
        "type\n  {}* {{.pure, bycopy.}} = object\n",
        fixTypeName(clang_getCursorType(cursor)));

    for (auto& subnode : directSubnodes(cursor)) {
        let kind = clang_getCursorKind(subnode);

        let type = clang_getCursorType(subnode);
        *(data->outfile) << fmt::format(
            "    {}*: {} # `{}`\n",
            fixArgName(toStr(clang_getCursorSpelling(subnode))),
            toNimType(type),
            type);
    }

    *(data->outfile) << "\n";


    return CXChildVisit_Continue;
}


Str cursorTokens(CXCursor cursor, CXTranslationUnit tu) {
    CXSourceRange range   = clang_getCursorExtent(cursor);
    CXToken*      tokens  = 0;
    unsigned int  nTokens = 0;
    Str           result;
    clang_tokenize(tu, range, &tokens, &nTokens);

    for (unsigned int i = 0; i < nTokens; i++) {
        CXString spelling = clang_getTokenSpelling(tu, tokens[i]);
        result += toStr(spelling);
        if (i != nTokens - 1) {
            result += " ";
        }
        // printf("token = %s\n", clang_getCString(spelling));
        // result = std::atoi(clang_getCString(spelling));
        // clang_disposeString(spelling);
    }
    clang_disposeTokens(tu, tokens, nTokens);

    return result;
}


int integerLiteralValue(CXCursor cursor, CXTranslationUnit tu) {
    let tok = cursorTokens(cursor, tu);

    // return std::stoi(tok);
    if (tok.size() > 1 and tok[1] == 'x') {
        int result;
        std::istringstream(tok) >> std::hex >> result;
        return result;
    } else {
        return std::atoi(tok.c_str());
    }
}

Str dropPrefix(Str str, Str pref) {
    if (str.rfind(pref) == 0) {
        return str.substr(pref.size(), str.size());
    } else {
        return str;
    }
}

CXChildVisitResult visitEnumDecl(CHILD_VISIT_PARAMS) {

    let         kind = clang_getCursorKind(cursor);
    ClientData* data = toClientData(client_data);

    // std::cout << "  Enum: \e[95m"
    //           << clang_getCanonicalType(clang_getCursorType(cursor))
    //           << "\e[39m\n";

    *(data->outfile) << fmt::format(
        "type\n  {}* {{.size: sizeof(cint).}} = enum\n",
        fixTypeName(clang_getCursorType(cursor)));

    for (auto& subnode : directSubnodes(cursor)) {
        let kind = clang_getCursorKind(subnode);
        // std::cout << "    \e[92m" << kind << "\e[39m " << subnode
        //           << " \e[93m"
        //           <<
        //           clang_getCanonicalType(clang_getCursorType(subnode))
        //           << "\e[39m\e[34m";
        *(data->outfile) << "    "
                         << toStr(clang_getCursorSpelling(subnode));

        let value = directSubnodes(subnode);

        if (value.size() == 0) {
        } else {
            let kind = clang_getCursorKind(value[0]);
            switch (kind) {
                case CXCursor_BinaryOperator: {
                    // Assume `int << int` for now
                    let operands = directSubnodes(value[0]);
                    let lhs = integerLiteralValue(operands[0], data->unit);
                    let rhs = integerLiteralValue(operands[1], data->unit);
                    // fmt::print(" {} << {} // {}", lhs, rhs, lhs << rhs);
                    *(data->outfile) << " = " << (lhs << rhs);
                    break;
                }
                case CXCursor_IntegerLiteral: {
                    let val = integerLiteralValue(value[0], data->unit);
                    // std::cout << " " << val << " // "
                    //           << cursorTokens(value[0], data->unit);
                    *(data->outfile) << " = " << val;
                    break;
                }
                default: {
                    // std::cout << " \e[34m" << kind << "\e[39m";
                }
            }
        }


        // std::cout << "\e[39m\n";
        *(data->outfile) << "\n";
    }

    *(data->outfile) << "\n";

    return CXChildVisit_Continue;
}

CXChildVisitResult visitTypedef(CHILD_VISIT_PARAMS) {

    let         kind      = clang_getCursorKind(cursor);
    ClientData* data      = toClientData(client_data);
    bool        isTypeDef = true;

    {
        let subnodes = directSubnodes(cursor);
        if (subnodes.size() == 1) {
            let subnode = subnodes[0];
            let kind    = clang_getCursorKind(subnode);
            if (kind == CXCursor_EnumDecl || kind == CXCursor_StructDecl) {
                isTypeDef = false;
            }
        }
    }

    if (isTypeDef) {
        if (false) { // Print typedef and all child nodes
            std::cout << "\e[33m" << clang_getCursorType(cursor)
                      << "\e[39m \e[3mtypedef of\e[23m "
                      << clang_getCanonicalType(
                             clang_getCursorType(cursor))
                      << "\n";


            for (auto& subnode : directSubnodes(cursor)) {
                let kind = clang_getCursorKind(subnode);
                std::cout << "  " << kind << "\n";
            }
        }

        *(data->outfile) << fmt::format(
            "type {}* = distinct {} # {}\n\n",
            cursor,
            toNimType(clang_getCanonicalType(clang_getCursorType(cursor))),
            clang_getCursorType(cursor));

    } else {
        // for (auto& subnode : directSubnodes(cursor)) {
        //     let kind = clang_getCursorKind(subnode);
        //     switch (kind) {
        //         case CXCursor_StructDecl: {
        //             visitStructDecl(subnode, cursor, client_data);
        //             break;
        //         }
        //         case CXCursor_EnumDecl: {
        //             visitEnumDecl(subnode, cursor, client_data);
        //             break;
        //         }
        //         default: {
        //             isTypeDef = true;
        //             std::cout << cursor << " \e[32m" << kind
        //                       << "\e[39m \e[93m"
        //                       << clang_getCanonicalType(
        //                              clang_getCursorType(subnode))
        //                       << "\e[39m\n";
        //         }
        //     }
        // }
    }


    // if (isTypeDef) {
    // }


    return CXChildVisit_Continue;
}

CXChildVisitResult visitToplevel(CHILD_VISIT_PARAMS) {
    let kind = clang_getCursorKind(cursor);

    if (clang_Location_isFromMainFile(clang_getCursorLocation(cursor)))
    // Only interested in functions defined in main file being processed
    {
        // if (kind != CXCursor_FunctionDecl) {
        //     std::cout << cursor << " \e[32m" << kind << "\e[39m\n";
        // }

        if (kind == CXCursor_FunctionDecl) {
            return visitFunction(cursor, parent, client_data);
        } else if (kind == CXCursor_TypedefDecl) {
            return visitTypedef(cursor, parent, client_data);
        } else if (kind == CXCursor_EnumDecl) {
            return visitEnumDecl(cursor, parent, client_data);
        } else if (kind == CXCursor_StructDecl) {
            return visitStructDecl(cursor, parent, client_data);
        } else {
            return CXChildVisit_Recurse;
        }
    } else {
        return CXChildVisit_Continue;
    }
}

int main() {
    Str targetDir = "../src/hcparse/libclang_raw/";

    Vec<Pair<Str, Str>> translations = {
        {"Index.h", "index.nim"},
        {"BuildSystem.h", "build_system.nim"},
        {"CXCompilationDatabase.h", "cxcompilation_database.nim"},
        {"CXErrorCode.h", "cxerror_code.nim"},
        {"Documentation.h", "documentation.nim"},
        {"ExternC.h", "externc.nim"},
        {"FatalErrorHandler.h", "fatal_error_handler.nim"},
        {"Platform.h", "platform.nim"},
        {"CXString.h", "cxstring.nim"},
        //
    };

    for (auto& tr : translations) {
        CXIndex           index = clang_createIndex(0, 0);
        CXTranslationUnit unit  = clang_parseTranslationUnit(
            index,
            fmt::format("/usr/include/clang-c/{}", tr.first).c_str(),
            nullptr,
            0,
            nullptr,
            0,
            CXTranslationUnit_None);

        if (unit == nullptr) {
            cerr << "Unable to parse translation unit. Quitting." << endl;
            exit(-1);
        }

        CXCursor   cursor = clang_getTranslationUnitCursor(unit);
        ClientData data;
        data.unit = unit;

        std::ofstream outfile;
        data.outfile = &outfile;

        fmt::print("{} -> {}\n", tr.first, targetDir + tr.second);

        data.outfile->open(targetDir + tr.second);

        *data.outfile << "from times import Time\n"
                         "{.deadCodeElim: on.}\n"
                         "{.push callconv: cdecl.}\n"
                         "import opaque_impls\n"
                         "\n"
                         "when defined(windows):\n"
                         "  const\n"
                         "    libclang = \"libclang.dll\"\n"
                         "elif defined(macosx):\n"
                         "  const\n"
                         "    libclang = \"libclang.dylib\"\n"
                         "else:\n"
                         "  const\n"
                         "    libclang = \"libclang.so\"\n"
                         "\n"
                         "\n";

        clang_visitChildren(cursor, visitToplevel, &data);

        data.outfile->close();

        clang_disposeTranslationUnit(unit);
        clang_disposeIndex(index);
    }
}

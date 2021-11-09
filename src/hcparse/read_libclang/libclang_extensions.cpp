// #include <clang-c/CXCursor.h>
#include "cxcursor.h"
#include <clang-c/Index.h>
#include <clang/AST/Attr.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Type.h>
#include <clang/Basic/ExceptionSpecificationType.h>
#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/Version.h>
#include <clang/Frontend/ASTUnit.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>

using namespace clang;
using namespace clang::cxcursor;
// using namespace clang::cxtu;
// using namespace clang::cxindex;

static const IdentifierInfo* getMacroIdentifier(CXCursor C) {
    if (C.kind == CXCursor_MacroDefinition) {
        if (const MacroDefinitionRecord* MDR = getCursorMacroDefinition(
                C)) {
            return MDR->getName();
        }
    } else if (C.kind == CXCursor_MacroExpansion) {
        MacroExpansionCursor ME = getCursorMacroExpansion(C);
        return ME.getName();
    }
    return nullptr;
}


int clang_Cursor_getParamNum(CXCursor C) {
    const IdentifierInfo* II = getMacroIdentifier(C);
    if (!II) {
        return -1;
    }
    ASTUnit*      ASTU = getCursorASTUnit(C);
    Preprocessor& PP   = ASTU->getPreprocessor();
    if (const MacroInfo* MI = PP.getMacroInfo(II)) {
        return MI->getParameterNum(II);
    }
    return -1;
}

int main() {
}

// static inline QualType GetQualType(CXType CT) {
//     return QualType::getFromOpaquePtr(CT.data[0]);
// }

// CXType clang_DependentVectorType_GetSizeExpr(CXType CT) {
//     Expr*       EC;
//     QualType    T  = GetQualType(CT);
//     const Type* TP = T.getTypePtrOrNull();
//     if (TP) {
//         switch (TP->getTypeClass()) {
//             case Type::DependentSizedArray:
//                 EC = cast<DependentSizedArrayType>(TP)->getSizeExpr();
//                 break;
//             default: break;
//         }
//     }
//     return cxcursor::MakeCXCursor(
//         EC, getCursorDecl(c), cxcursor::getCursorTU(EC));
// }

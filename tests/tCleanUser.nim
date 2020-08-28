import hcparse/libclang
import std/decls
import hmisc/helpers
import hpprint, hnimast

import unittest

suite "Translation unit cursor":
  test "test file":
    let outfile = "/tmp/example.cpp"
    outfile.writeFile """
    #include <iostream>

    /// Documentation comments for class
    class MyClass
    {
    public:
      int field;
      virtual void method() const = 0;
      static const int static_field;
      static int static_method();
    };

    /** This is a main function documenation comment */
    int main() {
      1 + 2;
    }
    """.dedent


    let
      trIndex = createIndex(0, 0)
      unit = parseTranslationUnit(trIndex, outfile)
      topCursor = unit.getTranslationUnitCursor()

    echo unit.isNil
    echo topCursor.cxKind

    echo topCursor.getFirstOfKind({ckCXXMethod}).treeRepr(unit)

    topCursor.visitChildren do:
      makeVisitor [unit]:
        if cursor.isFromMainFile():
          echo cursor.treeRepr(unit)
          return cvrContinue
        else:
          return cvrRecurse

suite "Type mapping":
  const tmpfile = "/tmp/type-mapping.cpp"
  proc toProc(str: string): PProcDecl =
    tmpfile.writeFile(str)
    let
      index = createIndex(showDiagnostics = true)
      unit = parseTranslationUnit(index, tmpfile)
      curs = getTranslationUnitCursor(unit)


    let cfunc = curs.getFirstOfKind({ckFunctionDecl})

    result = cfunc.convertCFunction()


  proc toTreeRepr(str: string): string =
    tmpfile.writeFile(str)
    let
      index = createIndex()
      unit = parseTranslationUnit(index, tmpfile)
      curs = getTranslationUnitCursor(unit)

    return curs.treeRepr(unit)


  test "Function mapping":
    echo toTreeRepr("namespace E::A { class Q {}; }; void test(E::A::Q e){}")

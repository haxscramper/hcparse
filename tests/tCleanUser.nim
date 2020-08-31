import hcparse/libclang
import std/decls
import hmisc/helpers
import hpprint, hnimast, hpprint/hpprint_repr

import unittest, macros
import compiler/ast, options
import terminal
import hmisc/types/colorstring

proc prettyPrintConverter(
  val: NIdentDefs[PNode], path: seq[int] = @[0]): ObjTree =
    pptMap(
      ("e", "e"),
      {
        "name": pptConst(val.varname, initPrintStyling(fg = fgGreen)),
        "kind": pptConst($val.kind, initPrintStyling(fg = fgRed)),
        "type": pptConst($val.vtype, initPrintStyling(fg = fgYellow))
      }
    )

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

suite "Declaration mapping":
  const tmpfile = "/tmp/declaration-mapping.cpp"
  proc splitDecls(str: string): seq[CDecl] =
    tmpfile.writeFile(str)
    let index = createIndex(showDiagnostics = true)
    let unit = parseTranslationUnit(index, tmpfile)
    let curs = getTranslationUnitCursor(unit)

    result = unit.splitDeclarations().decls
    # echo "split decls"

  macro assertItPPrint(head, body: untyped): untyped =
    result = newStmtList()
    result.add quote do:
      var anyFail {.inject.} = false
      var it {.inject.} = `head`

    for line in body:
      let astLit = line.toStrLit()
      let pos = newLit(astLit.lineInfoObj().line)
      result.add quote do:
        if not `line`:
          echo "\e[31mFAILED: \e[39m", `astLit`, " on line ", `pos`
          anyFail = true

    result = quote do:
      block:
        `result`
        if anyFail:
          pprint it
          fail()


  test "single class declaration":
    assert splitDecls("class Q {};")[0].name == "Q"
    block:
      let spl = splitDecls("class A { int hello(); };")
      assert spl[0].member(0).name == "hello"

    assertItPPrint splitDecls("namespace A { class B {}; }"):
      it[0].namespace == @["A"]
      it[0].name == "B"

    assertItPPrint splitDecls(
      "class B {int func(); public: float ce(int b);};"):
      it[0].name == "B"
      it[0].member(0).name == "func"
      it[0].member(1).accs == asPublic
      it[0].member(1).arg(0).name == "b"
      it[0].member(1).arg(0).cursor.cxType().cxKind() == tkInt

  test "wrapping methods":
    assertItPPrint splitDecls(
      "class Z { int hello(); int eee() const; };"
    )[0].wrapMethods(WrapConfig(), newPType("Z")):
      it[0].name == "hello"
      it[0].signature.arg(0).kind == nvdVar
      it[1].name == "eee"
      it[1].signature.arg(0).kind == nvdLet

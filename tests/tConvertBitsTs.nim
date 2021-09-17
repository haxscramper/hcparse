import
  hmisc/preludes/unittest,
  hcparse/[hc_parsefront, hc_codegen],
  hcparse/interop_ir/wrap_store,
  compiler/[ast, renderer],
  hnimast/nim_decl

import pkg/jsony

proc convStr(str: string): string =
  renderer.`$`(
    nim_decl.toNNode(
      hc_codegen.toNNode[PNode](
        wrapViaTs(str))))

proc convJson(str: string): string =
  wrapViaTs(str).cxxFile(cxxLibImport("", @[])).
    toJson().fromJson(CxxFile).toJson()

proc convPPrint(str: string) =
  pprint wrapViaTs(str)

suite "Convert type declarations":
  test "Regular struct":
    echo convStr("class S {};")
    echo convJson("class S {};")

  test "Struct with fields":
    echo convStr("struct WithFields { int field; };")
    echo convStr("struct WithFields { int __field; };")

  test "Class with methods":
    echo convStr("class A { void get(); };")


  test "Class with documentation":
    echo convStr("class A { int field; /* doc comment */ };")

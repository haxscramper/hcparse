import
  hmisc/preludes/unittest,
  hcparse/[hc_parsefront, hc_codegen],
  hcparse/interop_ir/wrap_store,
  compiler/[ast, renderer],
  hnimast/[nim_decl, object_decl, obj_field_macros, hast_common, proc_decl]

import std/[strutils]

import pkg/jsony

func getFields[N](o: ObjectDecl[N]): seq[ObjectField[N]] = getFlatFields(o)

proc convStr(str: string, conf: CodegenConf = cxxCodegenConf): string =
  renderer.`$`(
    nim_decl.toNNode(
      hc_codegen.toNNode[PNode](
        wrapViaTs(str, true, cxxHeader("?")), conf)))

proc convDecls(
    str: string, conf: CodegenConf = cxxCodegenConf): seq[NimDecl[PNode]] =
  hc_codegen.toNNode[PNode](wrapViaTs(str, true, cxxHeader("?")), conf)

proc convJson(str: string): string =
  wrapViaTs(str, true, cxxHeader("?")).cxxFile(cxxLibImport("", @[])).
    toJson().fromJson(CxxFile).toJson()

proc convPPrint(str: string) =
  pprint wrapViaTs(str, true)

suite "Convert type declarations":
  test "Regular struct":
    check convDecls("class S {};")[0].getObject().getName() == "S"
    discard convJson("class S {};")

  test "Struct with fields":
    let
      f1 = convDecls(
        "struct WithFields { int field; };")[0].getObject().getFields()[0]
      f2 = convDecls(
        "struct WithFields { int __field; };")[0].getObject().getFields()[0]

    check:
      not f1.hasPragma("importcpp")
      f1.name == "field"

      f2.hasPragma("importcpp")
      f2.name == "field"
      f2.getPragmaArgs("importcpp")[0].getStrVal() == "__field"

  test "Class with methods":
    let
      decls = convDecls("class A { void get(); void set(int val); };")
      declA = decls[0].getObject()
      declGet = decls[1].getProc()
      declSet = decls[2].getProc()

    check:
      declA.getName() == "A"
      declGet.getName() == "get"
      declGet.argumentNames() == @["this"]
      declSet.argumentNames() == @["this", "val"]


  test "Class with documentation":
    let
      decls = convDecls("class A { int field; /* doc comment */ };")
      declA = decls[0].getObject()

    check:
      declA.getName() == "A"
      declA.getFields()[0].getName() == "field"
      "doc comment" in declA.getFields()[0].docComment

suite "Repeated names":
  test "Multiple structs":
    let decls = convDecls("struct _S {}; struct S{};")[0].getTypes()
    check:
      decls[0].getObject().getName() == "S"
      decls[1].getObject().getName() == "S1"

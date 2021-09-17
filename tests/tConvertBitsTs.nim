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

suite "Convert type declarations":
  test "Regular struct":
    echo convStr("class S {};")
    echo convJson("class S {};")

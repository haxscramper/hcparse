import
  hmisc/preludes/unittest,
  hcparse/[hc_parsefront, hc_codegen],
  compiler/[ast, renderer],
  hnimast/nim_decl

proc convStr(str: string): string =
  renderer.`$`(
    nim_decl.toNNode(
      hc_codegen.toNNode[PNode](
        wrapViaTs(str))))

suite "Convert type declarations":
  test "Regular struct":
    echo convStr("class S {};")

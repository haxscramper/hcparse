import
  hmisc/preludes/unittest,
  hcparse/[hc_parsefront, hc_codegen],
  compiler/[ast, renderer]

proc conv(str: string): string =
  renderer.`$`(hc_codegen.toNNode[PNode](wrapViaTs(str)))

suite "Convert type declarations":
  test "Regular struct":
    echo conv("class S {};")

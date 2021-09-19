import hmisc/preludes/unittest

import
  hcparse/[hc_parsefront, hc_codegen, hc_grouping],
  hcparse/interop_ir/wrap_store

import compiler/[ast, renderer]

proc convFile(str, name: string): CxxFile =
  wrapViaTs(str, true, cxxHeader(name)).cxxFile(cxxLibImport("test", @[name]))


suite "Forward-declare in files":
  test "two separate types":
    var files = @[
      convFile("struct A; struct B { A* ptrA; }", "decl_B.hpp"),
      convFile("struct B; struct A { B* ptrB; }", "decl_A.hpp")
    ]

    registerTypes(files)

    let group = updateImports(files)
    for file in group:
      echov file.savePath
      echo toNNode[PNode](file, cxxCodegenConf)
    # pprint group

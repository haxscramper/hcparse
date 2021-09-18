import hmisc/preludes/unittest

import
  hcparse/[hc_parsefront, hc_codegen, hc_grouping],
  hcparse/interop_ir/wrap_store

proc convFile(str, name: string): CxxFile =
  wrapViaTs(str, true).cxxFile(cxxLibImport("test", @[name]))

suite "Forward-declare in files":
  test "two separate types":
    let f1 = convFile("struct A; struct B { A* ptrA; }", "decl_B.hpp")
    let f2 = convFile("struct B; struct A { B* ptrB; }", "decl_A.hpp")

    let group = updateImports(@[f1, f2])

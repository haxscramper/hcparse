import hmisc/preludes/unittest

import
  hcparse/[hc_parsefront, hc_codegen, hc_grouping],
  hcparse/interop_ir/wrap_store

import std/[strutils, sets]
import compiler/[ast, renderer]

proc lib(path: varargs[string]): CxxLibImport =
  cxxLibImport("test", @path)

proc convFile(str, name: string): CxxFile =
  wrapViaTs(str, true, cxxHeader(name)).cxxFile(lib(name))

proc findFile(files: seq[CxxFile], name: string): CxxFile =
  files[files.findIt(it.getFilename().startsWith(name))]

suite "Forward-declare in files":
  test "two separate types":
    var files = @[
      convFile("struct A; struct B { A* ptrA; };", "decl_B.hpp"),
      convFile("struct B; struct A { B* ptrB; };", "decl_A.hpp"),
      convFile("struct C { A* ptrA; B* ptrB; };", "decl_C.hpp")
    ]

    registerTypes(files)

    let group = updateImports(files)

    for file in group:
      echov file.savePath
      echo toNNode[PNode](file, cxxCodegenConf)

    let
      fileA = group.findFile("decl_A")
      fileB = group.findFile("decl_B")
      fileC = group.findFile("decl_C")

      merged = "decl_A_decl_B"
      fileM = group.findFile(merged)

    # pprint fileA.entries
    # pprint group

    check:
      lib(merged) in fileA.imports
      lib(merged) in fileA.exports

      lib(merged) in fileB.imports
      lib(merged) in fileB.exports

      lib("decl_A.hpp") in fileC.imports
      lib("decl_B.hpp") in fileC.imports

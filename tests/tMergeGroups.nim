import hmisc/preludes/unittest
import hmisc/algo/hseq_mapping
import hnimast

import hmisc/hasts/graphviz_ast


import
  hcparse/[hc_parsefront, hc_codegen, hc_grouping, hc_impls],
  hcparse/interop_ir/wrap_store

import std/[strutils, sets, sequtils]
import compiler/[ast]


proc lib(path: varargs[string]): CxxLibImport =
  cxxLibImport("test", @path)

template convFile(str, name: string): CxxFile =
  conf.onGetBind():
    return cxxHeader(name)

  wrapViaTs(str, conf, lib(name)).cxxFile(lib(name))

configureDefaultTestContext(
  skipAfterException = true
)

proc findFile(files: seq[CxxFile], name: string): CxxFile =
  files[files.findIt(it.getFilename().startsWith(name))]

suite "Forward-declare in files":
  var conf = baseFixConf.withIt do:
    it.typeStore = newTypeStore()

  conf.onFixName():
    result = name.nim
    cache.newRename(name.nim, result)

  test "Single file with forward declarattion":
    conf.typeStore = newTypeStore()
    let files = @[convFile("struct W { int f; }; typedef struct W W;", "decl_W.hpp")]
    let group = regroupFiles(files)
    let file = group.findFile("decl_W")
    let decl = file.entries.toNNode[:PNode](cxxCodegenConf).
      findItFirst(it of nekMultitype).typedecls[0].objectDecl

    check:
      decl.getField("f").fldType.head == "cint"
      decl.hasPragma("bycopy")
      decl.hasPragma("importcpp")

  test "forward declare, define elsewhere":
    conf.typeStore = newTypeStore()
    let files = @[
      convFile("struct Forward; struct User { Forward* userField; }", "user.hpp"),
      convFile("struct Forward { int forwardField; };", "forward.hpp")
    ]

    let
      group = regroupFiles(files)
      forward = group.findFile("forward")
      user = group.findFile("user")

    check:
      lib("forward.hpp") in user.imports

  test "Two separate files, get pointer":
    conf.typeStore = newTypeStore()
    let files = @[
      convFile(
        "struct Forward; struct GetForward { Forward* get(); };",
        "get_forward.hpp"),
      convFile("struct Forward {};", "forward.hpp")
    ]

    # TEST should generate two separate files with 'get_forward' importing
    # 'forward'
    # assert false

  test "two separate types, mutually recursive":
    conf.typeStore = newTypeStore()
    let files = @[
      convFile("struct A; struct B { A* ptrA; };", "decl_B.hpp"),
      convFile("struct B; struct A { B* ptrB; };", "decl_A.hpp"),
      convFile("struct C { A* ptrA; B* ptrB; };", "decl_C.hpp")
    ]

    let group = regroupFiles(files)

    let
      fileA = group.findFile("decl_A")
      fileB = group.findFile("decl_B")
      fileC = group.findFile("decl_C")

      merged = "decl_A_decl_B"

    let
      codeM = group.findFile(merged).entries.toNNode[:PNode](cxxCodegenConf)

    let
      typesM = codeM.findItFirst(it of nekMultitype).typedecls
      declA = typesM.getFirst("A").objectDecl
      declB = typesM.getFirst("B").objectDecl
      fieldB = declA.getField("ptrB")
      fieldA = declB.getField("ptrA")

    check:
      fieldB.fldType.head == "ptr"
      fieldA.fldType.head == "ptr"

      fieldB.fldType.genParams[0].head == "B"
      fieldA.fldType.genParams[0].head == "A"

    check:
      lib(merged) in fileA.imports
      lib(merged) in fileA.exports

      lib(merged) in fileB.imports
      lib(merged) in fileB.exports

      lib("decl_A.hpp") in fileC.imports
      lib("decl_B.hpp") in fileC.imports

  test "Depends on forward declaration and recursive":
    conf.typeStore = newTypeStore()
    let files = @[
      convFile("struct Forward {}; struct BaseUser{ Forward* forward; };", "forward.hpp"),

      convFile(lit3"""
        struct Forward;

        struct User1B;
        struct User1A { User1B* ptrB; Forward* forward; }
        """, "user_1A.hpp"),

      convFile(lit3"""
        struct User1A;
        struct User1B { User1A* ptrA; }
        """, "user_1B.hpp"),

      convFile(lit3"""
        struct Forward;

        struct User2B;
        struct User2A { User2B* ptrB; Forward* forward; }
        """, "user_2A.hpp"),

      convFile(lit3"""
        struct User2A;
        struct User2B { User2A* ptrA; }
        """, "user_2B.hpp")
    ]

    let group = regroupFiles(files)
    echo group.toString(cxxCodegenConf)

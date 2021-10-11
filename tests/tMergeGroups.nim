import hmisc/preludes/unittest
import hmisc/algo/hseq_mapping
import hmisc/types/hgraph
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

func getTypes(decl: seq[NimDecl[PNode]]): seq[NimTypeDecl[PNode]] =
  decl.findItFirst(it of nekMultitype).typedecls

func genEntries(file: CxxFile): seq[NimDecl[PNode]] =
  toNNode[PNode](file.entries, cxxCodegenConf)

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
      typesM = group.findFile(merged).genEntries().getTypes()
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

  test "Depends on forward declaration":
    conf.typeStore = newTypeStore()
    let files = @[
      convFile("struct Forward;", "forward.hpp"),
      convFile("typedef struct User { Forward* forward; } User;", "user.hpp")
    ]

    let group = regroupFiles(files)

    let
      userFile = group.findFile("user")
      userCode = userFile.genEntries().getTypes()
      userDecl = userCode.getFirst("User").objectDecl

      forwardFile = group.findFile("forward")
      forwardCode = forwardFile.genEntries().getTypes()
      forwardDecl = forwardCode.getFirst("Forward").objectDecl

    check:
      lib("forward.hpp") in userFile.imports

      forwardDecl.hasPragma("incompleteStruct")

  test "Forward declaration and recursive":
    conf.typeStore = newTypeStore()
    let files = @[
      convFile("struct Forward1;", "forward1.hpp"),
      convFile("struct Forward2;", "forward2.hpp"),
      convFile(lit3"""
        struct User1 {
          Forward1* forward1;
          Forward2* forward2;
          User2* user2;
        };

        Forward1* procForward1_1();
        Forward2* procForward1_2();
        """, "user1.hpp"),
      convFile(lit3"""
        struct User2 {
          Forward1* forward1;
          Forward2* forward2;
          User1* user1;
        };

        Forward1* procForward2_1();
        Forward2* procForward2_2();
        """, "user2.hpp")
    ]

    let group = regroupFiles(files)

    let
      merged = "user1_user2"
      f1     = group.findFile("forward1")
      f2     = group.findFile("forward2")
      m      = group.findFile(merged)
      u1     = group.findFile("user1")
      u2     = group.findFile("user2")

    check:
      lib(merged) in u1.exports
      lib(merged) in u2.exports

      lib(merged) in u1.imports
      lib(merged) in u2.imports

      lib("forward1.hpp") in m.imports
      lib("forward2.hpp") in m.imports

      lib("forward1.hpp") in m.imports
      lib("forward2.hpp") in m.imports

    let path = getTestTempFile("png")
    let g = files.buildTypeGraph()
    g.dotRepr().toPng(path)

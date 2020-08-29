import sugar, strutils, sequtils, strformat, os

import hcparse/libclang
import std/decls
import hmisc/helpers
import hpprint, hnimast, hpprint/hpprint_repr

import unittest, macros
import compiler/ast, options
import hmisc/hexceptions

import unittest
import logging
import hmisc/other/hshell

type
  ColorLogger = ref object of Logger

method log(logger: ColorLogger, level: Level, args: varargs[string, `$`]) =
  let prefix =
    case level:
      of lvlDebug: "DEBUG"
      of lvlInfo: "\e[94mINFO\e[39m"
      of lvlNotice: "\e[32mNOTICE\e[39m"
      of lvlWarn: "\e[33mWARN\e[39m"
      of lvlError: "\e[31mERROR\e[39m"
      of lvlFatal: "\e[1m\e[35mFATAL\e[39m\e[21m"
      of lvlAll: "ALL"
      of lvlNone: ""

  echo prefix, " ", args.join(" ")

var logger = ColorLogger()
addHandler logger

macro err(args: varargs[untyped]): untyped =
  result = newCall(newDotExpr(ident "logging", ident "error"))
  for arg in args:
    result.add arg

func toAssgnKV(nodes: seq[NimNode]): seq[NimNode] =
  for node in nodes:
    node.assertNodeKind({nnkCall})
    node[1].assertNodeKind({nnkStmtList})
    result.add nnkExprEqExpr.newTree(node[0], node[1][0])

macro wrapgenTest(body: untyped): untyped =
  result = newCall("wrapgen", toSeq(body).toAssgnKV())

proc wrapgen(
  cpp, nim: string,
  dirname: string = "/tmp",
  cppfile: string = "wrapgen-test",
  nimfile: string = "wrapgen_test"): void =

  let
    wrapfile = dirname / nimfile & "_wrap.nim"
    cppfile = dirname / cppfile & ".cpp"
    nimfile = dirname / nimfile & ".nim"

  echo cpp
  info "Writing CPP file", cppfile
  cppfile.writeFile(cpp)
  nimfile.writeFile(nim)

  let index = createIndex()
  let unit = parseTranslationUnit(index, cppfile)
  if unit.isNil:
    err "Translation unit parse failed"
    fail()
  else:
    info "Parsed file", cppfile


  info "Wrapper file is", wrapfile
  var outwrap = ""

  let decls = unit.splitDeclarations()
  for decl in decls:
    case decl.kind:
      of cdkClass:
        outwrap &= makeCommentSection("Type definition", 1) & "\n"
        outwrap &= $decl.wrapObject().toNNode() & "\n"
        outwrap &= makeCommentSection("Methods", 1) & "\n"
        outwrap &= decl.wrapMethods().mapIt(
          $it.toNNode()).joinl()
      else:
        discard

  wrapfile.writeFile(outwrap)
  echo outwrap


suite "Wrapgen":
  test "single method":
    wrapgenTest:
      cpp:
        "class Q { int hhh(); };"
      nim:
        ""

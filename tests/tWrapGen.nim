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
    ident: int

method log(logger: ColorLogger, level: Level, args: varargs[string, `$`]) =
  let ident = "  ".repeat(logger.ident)
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

  echo ident, prefix, " ", args.join(" ")

var logger = ColorLogger(ident: 2)
addHandler logger

macro err(args: varargs[untyped]): untyped =
  result = newCall(newDotExpr(ident "logging", ident "error"))
  for arg in args:
    result.add arg

macro kvCall(head, nodes: untyped): untyped =
  result = newCall(head)
  for node in nodes:
    node.assertNodeKind({nnkCall})
    node[1].assertNodeKind({nnkStmtList})
    result.add nnkExprEqExpr.newTree(node[0], node[1][0])

# macro wrapgenTest(body: untyped): untyped =
#   result = newCall("wrapgen", toSeq(body).toAssgnKV())

proc wrapgen(
  cpp, nim: string,
  dirname: string = "/tmp",
  cppfile: string = "wrapgen-test",
  nimfile: string = "wrapgen_test",
  nimcache: string = "nimcache.d",
  stdout: string = ""): void =

  let
    wrapfile = dirname / nimfile & "_wrap.nim"
    cppfile = dirname / cppfile & ".cpp"
    nimfile = dirname / nimfile & ".nim"
    nimcache = dirname / nimcache

  createDir nimcache

  info "Nimcache directory", nimcache
  info "Writing CPP file", cppfile
  cppfile.writeFile(cpp)
  nimfile.writeFile(&"import \"{wrapfile}\"\n" & nim)

  let index = createIndex()
  let unit = parseTranslationUnit(
    index, cppfile, @[], {tufSkipFunctionBodies})

  if unit.isNil:
    err "Translation unit parse failed"
    echo cpp
    fail()
  else:
    info "Parsed file", cppfile


  info "Wrapper file is", wrapfile
  var outwrap = ""

  let conf = WrapConfig(
    header: cppfile
  )

  let decls = unit.splitDeclarations()
  for decl in decls:
    case decl.kind:
      of cdkClass:
        outwrap &= makeCommentSection("Type definition", 1) & "\n"
        outwrap &= $decl.wrapObject(conf).toNNode(true) & "\n"
        outwrap &= makeCommentSection("Methods", 1) & "\n"
        outwrap &= decl.wrapMethods(conf).mapIt(
          $it.toNNode()).joinl()
      else:
        discard

  wrapfile.writeFile(outwrap)
  let binfile = nimfile & ".bin"
  block:
    let command = &"nim cpp -o:{binfile} \"{nimfile}\""
    info &"Compiling nim file '{command}'"
    let (stdout, err, code) = runShell(command)
    if code != 0:
      err "Compilation failed"

      echo "Generated nim wrapper:\n", outwrap
      echo stdout
      echo err
      fail()

  block:
    let command = binfile
    info "Running", command
    let (outstr, err, code) = runShell(command)
    if stdout.len > 0:
      assertEq outstr.strip(), stdout.strip()
    else:
      echo outstr




suite "Wrapgen":
  test "single method":
    kvCall wrapgen:
      cpp:
        """
        class Q {
          public:
            int a;
            void hhh() { a += 2; };
            int qq() { return 1; };
        };
        """.dedent()
      nim:
        """
        var q: Q
        q.hhh()
        echo q.qq()
        """.dedent()
      stdout:
        "1"

  test "Namespaces & includes":
    kvCall wrapgen:
      cpp:
        """
        #include <iostream>

        namespace Q {
          class Z {
            public:
              void hello() const { std::cout << "Hello from C++ code"; };
          };
        }
        """.dedent()
      nim:
        """
        let z = Z()
        z.hello()
        """.dedent()
      stdout:
        "Hello from C++ code"

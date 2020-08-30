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

func makeRstCodeBlock*(str: string, lang: string = "nim"): string =
  let str = str.split("\n").mapIt("    " & it).joinl()
  &"""
.. code-block:: {lang}
{str}
"""

func makeRstList*(elems: seq[string], ident: int = 0): string =
  elems.mapIt("  ".repeat(ident) & it).joinl()

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

var convertExamples: seq[string]

proc wrapgen(
  cpp, nim, name: string,
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
  cppfile.writeFile(cpp.dedent())
  nimfile.writeFile(&"import \"{wrapfile}\"\n" & nim.dedent())




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
        let (obj, procs) = decl.wrapObject(conf)

        outwrap &= makeCommentSection("Type definition", 1) & "\n"
        outwrap &= $obj.toNNode(true) & "\n"
        outwrap &= makeCommentSection("Methods", 1) & "\n"
        outwrap &= procs.mapIt($it.toNNode()).joinl()
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

      echo "Translation unit tree:\n"
      let curs = unit.getTranslationUnitCursor()
      curs.visitMainFile do:
        makeVisitor [unit]:
          echo cursor.treeRepr(unit)
          return cvrContinue

      echo "Generated nim wrapper:\n"

      var idx = 0
      for line in wrapfile.lines:
        echo &"{idx:>2} | {line}"
        inc idx

      idx = 0
      echo "User file:\n"
      for line in nimfile.lines:
        echo &"{idx:>2} | {line}"
        inc idx

      echo stdout
      echo err
      # quit 1

  block:
    let command = binfile
    info "Running", command
    let (outstr, err, code) = runShell(command)
    if stdout.len > 0:
      assertEq outstr.strip(), stdout.strip()
      notice "stdout comparison ok"
    else:
      echo outstr

    convertExamples.add """
# $5

## C++ code

$1

## Generated nim wrapper

$2

## Code using wrapper

$3

## Execution result

$4

""" % [
    cpp.dedent().makeRstCodeBlock("C++"),
    outwrap.makeRstCodeBlock("nim"),
    nim.dedent().makeRstCodeBlock("nim"),
    outstr.makeRstCodeBlock(""),
    name]





suite "Wrapgen":
  test "single method":
    kvCall wrapgen:
      name: "Single method"
      cpp:
        """
        class Q {
          public:
            int a;
            void hhh() { a += 2; };
            int qq() { return 1; };
        };
        """
      nim:
        """
        var q: Q
        q.hhh()
        echo q.qq()
        """
      stdout:
        "1"

  test "Namespaces & includes":
    kvCall wrapgen:
      name: "Namespace & includes"
      cpp:
        """
        #include <iostream>

        namespace Q {
          class Z {
            public:
              void hello() const { std::cout << "Hello from C++ code"; };
          };
        }
        """
      nim:
        """
        let z = Z()
        z.hello()
        """
      stdout:
        "Hello from C++ code"

  test "Porting operators":
    kvCall wrapgen:
      name: "Wrapping operators"
      cpp:
        """
        #include <iostream>

        class Z {
          public:
            int a;
            void operator+=(const Z& rhs) { a += rhs.a; }
        };
        """
      nim:
        """
        var z = Z(a: cint 12)
        z += Z(a: cint 22)
        echo z.a
        """
      stdout:
        "34"

  test "Wrapping templates":
    kvCall wrapgen:
      name: "Wrapping template classes"
      cpp:
        """
        #include <iostream>
        #include <typeinfo>

        template <typename T>
        class Z {
          public:
            void getParam() const {
              std::cout << "Template parameter name [" <<
                typeid(T).name() << "] \n";
            }
        };
        """
      nim:
        """
        let z = Z[tuple[a: int, b: float]]()
        z.getParam()
        """


  test "`std::string` field":
    warn "Not implemented"


  test "Write results":
    let file = currentSourcePath().splitFile().dir /../
      "wrap-examples.rst"
    echo file
    convertExamples = @["""
For ease of testing all files use absolute paths for wrappers.
This is of course configurable. This file is automatically generated
from unit tests. You can view source code for tests
`here <https://github.com/haxscramper/hcparse/blob/master/tests/tWrapGen.nim>`_
"""] & convertExamples

    file.writeFile(convertExamples.join("\n\n"))

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

macro varargsCall(head, nodes: untyped): untyped =
  result = newCall(head)
  for node in nodes:
    result.add node

macro stringKvTable(nodes: untyped): untyped =
  result = nnkTableConstr.newTree()
  for node in nodes:
    node.assertNodeKind({nnkCall, nnkStrLit})
    var val = node[1][0]
    if val.kind == nnkTripleStrLit:
      val.strVal = val.strVal.dedent()

    result.add nnkExprColonExpr.newTree(
      newLit node[0].strVal(), val)


macro passKVargs(head: untyped, args: varargs[untyped]): untyped =
  result = newCall(head)
  head.assertNodeKind({nnkIdent})
  for arg in args:
    arg.assertNodeKind({nnkIdent})
    result.add nnkExprEqExpr.newTree(arg, arg)


type
  ColorLogger = ref object of Logger
    ident: int

proc identLog =
  for handler in getHandlers():
    if ColorLogger(handler) != nil:
      inc ColorLogger(handler).ident

proc dedentLog =
  for handler in getHandlers():
    if ColorLogger(handler) != nil:
      dec ColorLogger(handler).ident

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

func makeRstSection*(body, header: string, level: int): string =
  &"""
{"#".repeat(level)} {header}

{body}
"""

func makeRstSection*(
  level: int, header: string, body: varargs[string]): string =
  makeRstSection(body.join("\n\n"), header, level)

func makeRstList*(elems: seq[string], ident: int = 0): string =
  elems.mapIt("  ".repeat(ident) & it).joinl()



proc wrapDeclarations*(
  decls: seq[CDecl], conf: WrapConfig): string =
  for decl in decls:
    case decl.kind:
      of cdkClass:
        let (obj, procs) = decl.wrapObject(conf)

        result &= makeCommentSection("Type definition", 1) & "\n"
        result &= $obj.toNNode(true) & "\n"
        result &= makeCommentSection("Methods", 1) & "\n"
        result &= procs.mapIt($it.toNNode()).joinl()
      else:
        discard

proc printFile(file: string): void =
  var idx = 0
  for line in file.lines():
    echo &"{idx:>2} | {line}"
    inc idx

proc printTUMain(unit: CXTranslationUnit): void =
  let curs = unit.getTranslationUnitCursor()
  curs.visitMainFile do:
    makeVisitor [unit]:
      echo cursor.treeRepr(unit)
      return cvrContinue

proc compileRunNim(nimfile, wrapfile, stdout: string,
  unit: CXTranslationUnit): tuple[outstr: string] =
  let binfile = nimfile & ".bin"
  block:
    let command = &"nim cpp -o:{binfile} \"{nimfile}\""
    info &"Compiling nim file '{command}'"

    let (stdout, err, code) = runShell(command, doRaise = false)
    if code != 0:
      err "Compilation failed"

      echo("Translation unit tree:\n"); unit.printTUMain()
      echo("Generated nim wrapper:\n"); wrapfile.printFile()
      echo("User file:\n"); nimfile.printFile()

      echo stdout
      echo err

  block:
    let command = binfile
    info "Running", command
    let (outstr, err, code) = runShell(command)
    result.outstr = outstr
    if stdout.len > 0:
      assertEq outstr.strip(), stdout.strip()
      notice "stdout comparison ok"
    else:
      echo outstr




proc parseCPP(cppfile: string, flags: seq[string] = @[]): tuple[
  api: CApiUnit, unit: CXTranslationUnit, index: CXIndex] =

  result.index = createIndex()
  result.unit = parseTranslationUnit(
    result.index, cppfile, flags, {tufSkipFunctionBodies})

  if result.unit.isNil:
    err "Translation unit parse failed"
    fail()
  else:
    info "Parsed file", cppfile

  result.api = result.unit.splitDeclarations()

#=======================  Example page generation  =======================#
var convertExamples: seq[string]


#=========================  Common setup parts  ==========================#

template commonSetup() {.dirty.} =
  let
    wrapfile = dirname / nimfile & "_wrap.nim"
    cppfile = dirname / cppfile & ".cpp"
    nimfile = dirname / nimfile & ".nim"
    nimcache = dirname / nimcache

  createDir nimcache

  info "Nimcache directory", nimcache
  info "Writing CPP file", cppfile
  cppfile.writeFile(cpp.dedent())
  info "Wrapper file is", wrapfile



#======================  Direct wrapper generation  ======================#

proc wrapgen(
  cpp, nim, name: string,
  dirname: string = "/tmp",
  cppfile: string = "wrapgen-test",
  nimfile: string = "wrapgen_test",
  nimcache: string = "nimcache.d",
  stdout: string = ""): void =

  commonSetup()
  nimfile.writeFile(&"import \"{wrapfile}\"\n" & nim.dedent())
  let conf = WrapConfig(header: cppfile)
  let (api, unit, index) = parseCPP(cppfile)

  let wrapText = api.decls.wrapDeclarations(conf)
  wrapfile.writeFile(wrapText)
  let binfile = nimfile & ".bin"
  let runres = passKVargs(compileRunNim, nimfile, unit, wrapfile, stdout)


  convertExamples.add makeRstSection(
    1, name,
    cpp.dedent().makeRstCodeBlock("C++").makeRstSection(
      "C++ code", 2),
    nim.dedent().makeRstCodeBlock("nim").makeRstSection(
      "Code using wrapper", 2),
    runres.outstr.makeRstCodeBlock("").makeRstSection(
      "Execution result", 2),
    wrapfile.readFile().makeRstCodeBlock("nim").makeRstSection(
      "Nim wrappers generated for all dependent files", 2)
  )

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


#======================  Api dependency inference  =======================#

proc getIncludePaths(): seq[string] = @[]

proc inferapi(
  nim, name: string,
  cpp: string,
  deps: openarray[(string, string)],
  dirname: string = "/tmp/inferapi-test",
  cppfile: string = "wrapgen-test",
  nimfile: string = "wrapgen_test",
  nimcache: string = "nimcache.d",
  includepaths: seq[string] = getIncludePaths(),
  stdout: string = ""): void =

  commonSetup()

  var wrapsection = ""
  for (name, content) in deps:
    (dirname / name).writeFile(content)
    wrapsection.add makeRstSection(
      3, name, makeRstCodeBlock(content, "c++"))

  let (api, unit, index) = parseCPP(cppfile, @[&"-I{dirname}"])

  var depImports: seq[string]
  # Generate wrappers for dependencies
  block:
    identLog()
    for file in api.publicAPI.getDepFiles():
      debug "Found dependency", file

      let
        conf = WrapConfig(header: file)
        (dapi, dunit, dindex) = parseCPP(file, @[&"-I{dirname}"])
        # NOTE for now we only go one level deeper into dependency
        # tree
        wrapText = dapi.decls.wrapDeclarations(conf)
        depname = file.dropSuffix(@[".hpp", ".cpp"]).addSuffix(".nim")

      wrapsection.add makeRstSection(
        3, &"Wrapper for dependency {file}",
        makeRstCodeBlock(wrapText, "nim"))

      depname.writeFile(wrapText)
      debug "Wrote", depname
      depImports.add depname

    dedentLog()

  nimfile.writeFile(&"import \"{wrapfile}\"\n" & nim.dedent())

  # Generate main wrapper
  block:
    let conf = WrapConfig(header: cppfile)
    let wrapText = api.decls.wrapDeclarations(conf)
    wrapfile.writeFile(depImports.mapIt(
      &"import \"{it}\"").joinl() & "\n" & wrapText)

  let runres = passKVargs(compileRunNim, nimfile, unit, wrapfile, stdout)

  convertExamples.add makeRstSection(
    1, name,
    cpp.dedent().makeRstCodeBlock("C++").makeRstSection(
      "C++ code", 2),
    wrapsection.makeRstSection(
      "Nim wrappers generated for all dependent files", 2),
    nim.dedent().makeRstCodeBlock("nim").makeRstSection(
      "Code using wrapper", 2),
    runres.outstr.makeRstCodeBlock("").makeRstSection(
      "Execution result", 2)
  )

suite "Public API inference":
  test "Imported class":
    kvCall inferapi:
      name: "Single external dependency"
      nim:
        """
        var q: Q
        echo typeof q.dep
        """
      cpp:
        """
        #include "dependency.hpp"
        // Main file that we are interseted in wrapping
        // Public API uses type `D` that was imported from another
        // header. In order to compile wraper we must know how this
        // type is defined (where it is imported from etc.) or treat
        // it as opaque handle - e.g provide no implementation except
        // for `type D {.importcpp: "someheader".} = object`
        class Q { public: D dep; };
        """
      deps:
        stringKvTable:
          "dependency.hpp": # This dependency file should be wrapped
                            # automatically
            """
            class D { public: int d; };
            """


let file = currentSourcePath().splitFile().dir /../
  "wrap-examples.rst"
convertExamples = @["""
For ease of testing all files use absolute paths for wrappers.
This is of course configurable. This file is automatically generated
from unit tests. You can view source code for tests
`here <https://github.com/haxscramper/hcparse/blob/master/tests/tWrapGen.nim>`_
"""] & convertExamples

file.writeFile(convertExamples.join("\n\n"))
echo "Unit test finished. Examples were written to ", file
discard runShell(&"nim rst2html -o:{file}.html {file}")

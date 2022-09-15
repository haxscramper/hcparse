import
  hmisc/preludes/cli_app


proc initHcparseGenerator*(): string =
  "import hcparse"

proc initConanPackage*(
    dir: AbsDir,
    name, version: string
  ) =

  echov dir

  mkWithDirStructure dir:
    file ".gitignore":
      """
*
!*.*
!**/*.*
!/**/
!.*
!/*

nimcache/
**/*.bin
**/*.png
**/*.dot
**/*.tmp.*
**/*.tmp
**/*.html
**/*.css

**/*tmp-*
.nojekyll
nimdoc.cfg
/scripts/.circuit
/scripts/.latex
callgrind.out.*

*.code-workspace

nim.cfg
"""

    file "conanfile.txt":
      fmt"""
[requires]
{name}/{version}

[generators]
json
"""

    dir ".github":
      dir "workflows":
        file "test.yaml":
          fmt"""
name:
  test
on:
  push
jobs:
  test:
    strategy:
      matrix:
        nim: ['version-1-6', 'version-1-2']

    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: alaviss/setup-nim@master
        with:
          path: '../nim'
          version: ${{{{ matrix.nim }}}}
      - run: |
          pip install conan
          conan install . --build=missing
          nimble -y develop
          nim c -r \
            --passL:"@$(pwd)/conanbuildinfo.gcc" \
            --passC:"@$(pwd)/conanbuildinfo.gcc" \
            -d:hcparseLinkMode=dlink \
            tests/test1.nim
"""

    file &"{name}_config.nim":
      fmt"""
const
  hcparseLinkMode* {{.strdefine.}} = "dynlib"
  {name}LinkMode* {{.strdefine.}} = hcparseLinkMode
  {name}Lib* {{.strdefine.}} =
    when defined(windows): "{name}.dll"
    elif defined(macosx):  "{name}.dylib"
    else:                  "{name}.so"


import std/[macros]

macro {name}Proc*(a: untyped): untyped =
  result = a
  result.addPragma(ident"importc")
  when {name}LinkMode == "dynlib":
    result.addPragma(nnkExprColonExpr.newTree(
      ident"dynlib", newLit({name}Lib)))

  elif {name}LinkMode in ["static", "dlink"]:
    # Default dynamic or static linking, handled by user via `{{.passl.}}`
    # etc.
    discard

  else:
    {{.error: "Invalid {name} link mode specified" &
      " expected 'dynlib', 'static' or 'dlink', but got " &
      {name}LinkMode.}}

"""



    file &"hcparse_generate_{name}.nim":
      initHcparseGenerator()

    dir "tests":
      file "test1.nim":
        "import {name}"

    file &"{name}.nimble":
      fmt"""
author = ""
version = "0.1.0"
description "nim wrapper for the {name} library"


requires "nim >= 1.2.0"
"""

proc hcparseMain*(app: var CliApp, l: HLogger) =
  echov app.getCmd().treeRepr()

  case app.getCmdName():
    of "init":
      let init = app.getCmd()
      case init.getCmdName():
        of "conan":
          let
            conan = init.getCmd()
            name = conan.getArg("name") as string
            version = conan.getArg("version") as string

          initConanPackage(cwd(), name, version)

        else:
          raise newUnexpectedKindError(init.getCmdName())

    else:
      raise newUnexpectedKindError(app.getCmdName())


proc addInitCmd*(app: var CliApp) =
  var cmd = cmd("init", "Init default project structure")

  block:
    var conan = cmd("conan", "Init wrapper for conan package")
    conan.add arg("name", "Target library name")
    conan.add arg("version", "Target library version")

    cmd.add conan

  app.add cmd

proc hcparseCli*(args: seq[string], doRaise = false) =
  var
    app = newCliApp(
      "hcparse",
      (0, 1, 3),
      "haxscramper",
      "CLI driver for hcparse wrapper generator"
    )

    logger = newTermLogger()

  app.addInitCmd()

  app.acceptArgsAndRunBody(logger, args):
    app.runMain(hcparseMain, logger, not doRaise, argpass(app, logger))

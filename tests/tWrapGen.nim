import hcparse/libclang
import hpprint, hnimast, hpprint/hpprint_repr
import std/[unittest, macros, options, sugar, streams,
            strutils, sequtils, strformat, tables]
import compiler/ast

import yaml, yaml/[dom, presenter]
import hmisc/algo/make_rst
import hmisc/[hexceptions, helpers]
import hmisc/types/colortext
import hmisc/macros/[iflet]
import hmisc/other/[hshell, colorlogger, oswrap]
import hpprint

startColorLogger()

# execShell(ShellExpr "kitty @ kitten icat /tmp/img.png")


type
  TestFile = object
    filename: string
    internal {.defaultVal: false}: bool
    contents: string

  TestRecord {.sparse.} = object
    name: string
    forceFirst {.defaultVal: 0.}: int
    cfiles: seq[TestFile]
    nimfiles: seq[TestFile]
    stdout: Option[string]
    comment: Option[string]

let file = currentSourcePath().getAbsDir() /. "ccode.yaml"

template loadYaml(ResType: typed, file: string): untyped =
  var res: ResType
  var stream = newFileStream(file)
  load(stream, res)
  res

let files = loadYaml(seq[TestRecord], file.string)

var wrapConf = baseWrapConfig
var parseConf = baseCppParseConfig
# pprint files

# echo files.sortedByIt(-it.forceFirst).mapIt(it.name)

# quit 0

for file in files.sortedByIt(-it.forceFirst):
  notice file.name
  identLog()
  withTempDir(false) do:
    tempRoot = getTempDir() / "wrapgen"
    tempPatt = file.name.dashedWords()
  do:
    var cfiles: seq[AbsFile]
    var nimfiles: seq[AbsFile]
    var internFiles: seq[AbsFile]
    for file in file.cfiles & file.nimfiles:
      let path = tempDir /. file.filename

      if path.endsWith("nim"):
        nimfiles.add path
      else:
        if file.internal:
          internFiles.add path

        cfiles.add path

      writeFile(path, file.contents)

    wrapConf.isInternal = proc(
      dep: AbsFile, conf: WrapConfig, idx: FileIndex): bool =
        if dep in internFiles:
          return true
        else:
          return isInternalImpl(dep, conf, idx)


    identLog()
    let (wrapped, index) = wrapAll(cfiles, parseConf, wrapConf)
    dedentLog()

    proc showFiles() =
      for file in
          sorted(wrapped.mapIt(toAbsFile it.wrapName()) &
          cfiles & nimfiles):
        info file
        var body = file.readFile().strip().colorizeToStr(file.ext)
        body = body.split("\n").enumerate().mapIt(
          &"{(it[0] + 1):<2} {it[1]}"
        ).join("\n")
        # body = body.wrap("```" & file.ext & "\n", "\n```")
        debug body, "\n"


    for res in wrapped:
      let file = res.wrapName()
      file.writeFile($res.wrapped)

    for nfile in nimfiles:
      notice "Compiling", nfile
      try:
        execShell makeNimShellCmd("nim").withIt do:
          it.cmd "cpp"
          it - ("nimcache", "cache.d")
          it - ("o", nfile.withExt "bin")
          it.arg nfile
      except ShellError:
        err "Compilation for '", file.name, "' has failed"
        showFiles()
        quit 1

    for nfile in nimfiles:
      notice "Executing", nfile
      try:
        execShell makeFileShellCmd(nfile.withExt "bin")
        echo ""
      except ShellError:
        err "Execution for '", file.name, "' has failed"
        showFiles()
        quit 1

  dedentLog()

notice "All tests passed"

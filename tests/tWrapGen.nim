import hcparse/libclang
import hpprint, hnimast, hpprint/hpprint_repr
import std/[unittest, macros, options, sugar, streams,
            strutils, sequtils, strformat, os, tables]
import compiler/ast

import yaml, yaml/[dom, presenter]
import hmisc/algo/make_rst
import hmisc/[hexceptions, helpers]
import hmisc/macros/[iflet]
import hmisc/other/[hshell, colorlogger, oswrap]
import hpprint

startColorLogger()

type
  TestFile = object
    filename: string
    contents: string

  TestRecord {.sparse.} = object
    name: string
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

for file in files:
  notice file.name
  iflet (comm = file.comment):
    debug comm

  withTempDir(true):
    var cfiles: seq[AbsFile]
    var nimfiles: seq[AbsFile]

    for file in file.cfiles & file.nimfiles:
      let path = tempDir /. file.filename

      if path.endsWith("nim"):
        nimfiles.add path
      else:
        cfiles.add path

      debug "Writing ", path

      writeFile(path, file.contents)

    identLog()
    let (wrapped, index) = wrapAll(cfiles, parseConf, wrapConf)
    dedentLog()

    for res in wrapped:
      let file = res.importName.join("/") & ".nim"
      file.writeFile($res.wrapped)
      info "Wrote wrapper file", file

    for nfile in nimfiles:
      notice "Compiling", nfile
      try:
        execShell makeNimShellCmd("nim").withIt do:
          it.cmd "c"
          it.arg nfile
      except:
        err "Compilation for'", file.name, "'has failed"
        for file in wrapped:
          let f = file.importName.join("/") & ".nim"
          info f
          debug f.readFile()

        quit 1

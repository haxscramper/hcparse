## Generate wrappers for Clang CXX API

import hcparse/[wrap_common]
import std/[tables]

import cxxstd/make_wrap as cxxstd_wrap

let basedir = AbsDir("/usr/include/clang")

let parseConf* = baseCppParseConfig.withIt do:
  it.includepaths.add @[baseDir]
  it.globalFlags = @["-xc++"]

let wrapConf* = baseCppWrapConf.withDeepIt do:
  it.parseConf = parseConf
  it.baseDir = baseDir

  it.makeHeader = (
    proc(cursor: CXCursor, conf: WrapConfig): NimHeaderSpec =
      asIncludeFromDir(cursor, conf, conf.baseDir.parentDir()).
        initHeaderSpec()
  )

  it.getImport = (
    proc(dependency: AbsFile, conf: WrapConfig, isExternalImport: bool):
      NimImportSpec {.closure.} =

      return getImportUsingDependencies(
        dependency,
        @[cxxstd_wrap.wrapConf, baseCppWrapConf],
        isExternalImport
      )
  )

when isMainModule:
  var files = collect(newSeq):
    for file in walkDir(basedir, RelFile, recurse = true):
      if file.ext() == "h":
        file

  # files = files[0 ..< min(files.high, 10)]

  for file in files:
    let res = cwd() / file.toNimFile()
    if not exists(res) or ("/Rewriter.h" in file):
      debug baseDir / file, "->", res
      wrapWithConfig(baseDir / file, res, wrapConf, parseConf)

    # else:
    #   warn res, "already exists"

  for file in files:
    info "Checking", file
    execShell shCmd(nim, check, warnings=off, errorMax=2, $file.withExt("nim"))

  info "Done wrapping C++ standard library"

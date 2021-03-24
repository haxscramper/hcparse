import std/[sugar, strutils, sequtils, strformat, options]
import hmisc/other/[oswrap, hshell, colorlogger]
import hmisc/types/[colortext]
import hmisc/helpers
import hcparse/libclang
import htsparse/cpp/cpp

startColorLogger(showfile = true)
startHax()

import unittest

let file = currentSourceDir() / "sfinae_header.hpp"

let wrapConf = baseCppWrapConf.withDeepIt do:
  it.baseDir = AbsDir("/tmp")
  it.showParsed = true

suite "Wrap sfinae":
  let resFile = "/tmp/res.nim"
  wrapWithConfig(
    AbsFile file,
    AbsFile resFile,
    wrapConf,
    baseCppParseConfig
  )

  execShell shCmd(nim, cpp, $resFile)

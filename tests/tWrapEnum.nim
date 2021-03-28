import std/[sugar, strutils, sequtils, strformat, options]
import hmisc/other/[oswrap, hshell, colorlogger]
import hmisc/types/[colortext]
import hmisc/helpers
import hcparse/libclang
import htsparse/cpp/cpp

startColorLogger()
startHax()

import unittest

let file = "/tmp/a.c"
let str = """

class NeverDefined;
class Forward;

class Forward {
};

"""

file.writeFile(str)

let wrapConf = baseCppWrapConf.withDeepIt do:
  it.baseDir = AbsDir("/tmp")
  it.showParsed = true

suite "Wrap enum":
  let resFile = "/tmp/res.nim"
  wrapWithConfig(AbsFile file, AbsFile resFile, wrapConf, baseCppParseConfig)

  execShell shCmd(nim, c).withIt do:
    it.arg resFile

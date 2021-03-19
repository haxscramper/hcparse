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
typedef struct {
    enum {kFloat, kString} kind;
    union {
       float floatVal;
       char* strVal;
    };

    struct {
        int field1;
    };

    struct named {
        int field1;
    } namedFld;
} Value;
"""

file.writeFile(str)

let wrapConf = baseCWrapConf.withDeepIt do:
  it.baseDir = AbsDir("/tmp")
  it.setPrefixForEnum @{
    "mandoclevel" : "ml",
    "mandocerr" : "me"
  }

suite "Wrap enum":
  # echo parseCppString(str).treeRepr(str)

  let resFile = "/tmp/res.nim"
  wrapWithConfig(
    AbsFile file,
    AbsFile resFile,
    wrapConf,
    baseCParseConfig
  )


  # echo readFile(resFile).colorizeToStr("nim")
  execShell shCmd(nim, c).withIt do:
    it.arg resFile

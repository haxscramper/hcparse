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
/*
 * Status level.  This refers to both internal status
 */
enum class mandoclevel {
	MANDOCLEVEL_OK = 0,
	MANDOCLEVEL_STYLE, /* style suggestions */
	MANDOCLEVEL_WARNING, /* warnings: syntax, whitespace, etc. */
};

enum	mandocerr {
	MANDOCERR_OK,
	MANDOCERR_MDOCDATE, /* Mdocdate found: Dd ... */
};

void usesEnum(mandocerr en) {}
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
    baseCppParseConfig
  )


  # echo readFile(resFile).colorizeToStr("nim")
  execShell shCmd(nim, c).withIt do:
    it.arg resFile

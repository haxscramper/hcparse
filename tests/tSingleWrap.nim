import std/[sugar, strutils, sequtils, strformat]
import hcparse/hcparse_cli
import hmisc/other/oswrap

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

let srcd = AbsDir(currentSourcePath()).splitDir().head

suite "single file wrap":
  test "test":
    wrapCpp(
      srcd /. "incpp.cpp",
      srcd /. "resnim.nim"
    )

suite "cxstdlib garbage wrap test":
  test "std::string":
    wrapCPP(
      AbsFile("/usr/include/c++/10.2.0/string"),
      AbsFile("/tmp/res.nim")
    )

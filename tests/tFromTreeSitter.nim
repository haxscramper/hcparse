import
  hmisc/core/all

import
  hcparse, hcparse/[hc_tsreader, hc_save]

import
  jsony

import
  std/unittest

import
  hmisc/algo/htemplates,
  hmisc/other/oswrap

suite "Convert PAPI":
  var conf = baseCppWrapConf.withDeepIt do:
    it.wrapName = "papi"


  test "Enum":
    let save = conf.toCxx("""
enum a { b, c };
""")

  test "Convert":
    let file = AbsFile("/tmp/papi.h")
    if exists(file):
      let save = conf.toCxx(file)
      "/tmp/papi.json".writeFile(save.toJson())

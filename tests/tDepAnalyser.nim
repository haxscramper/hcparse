import sugar, strutils, sequtils, strformat, os
import hcparse/libclang

import hmisc/macros/kv_transform
import hmisc/algo/make_rst
import hmisc/[hexceptions, helpers]
import hmisc/other/[hshell, colorlogger]

startColorLogger()

#===========================  implementation  ============================#

var wrapResults: seq[string]

proc makeDepGraph(
  name: string,
  dirname: string = "/tmp/dep-analyser",
  files: openarray[tuple[file, content: string]]): void =

  createDir(dirname)

  var section: string
  for (file, content) in files:
    let content = "#pragma once \n" & content.dedent()
    (dirname / file).writeFile(content)

    section.add makeRstSection(
      2, file, content.makeRstCodeBlock("c++")) & "\n"

  wrapResults.add makeRstSection(1, name, section)

  var conf = ParseConfiguration(
    globalPaths: @[dirname]
  )

  var parsed = files.mapIt(dirname / it.file).parseAll(conf)



#================================  tests  ================================#

import unittest

suite "CPP dependency analysis graph":
  test "test":
    kvCall makeDepGraph:
      name: "Two level dependency"
      files:
        stringKvTable:
          "dep1.hpp": "class D1 {};"
          "main.hpp":
            """
            #include "dep1.hpp"
            class Main { public: D1 dep1; };
            """


rst2html(wrapResults.joinl(), "dep-analyser.html")

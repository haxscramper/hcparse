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


  var conf = ParseConfiguration(
    globalPaths: @[dirname]
  )

  var parsed = files.mapIt(dirname / it.file).parseAll(conf)

  let graph = parsed.dotRepr()
  let name = name.dashedWords() & ".png"
  graph.toPng(dirname / name)

  section.add makeRstSection(2, name, makeRstImage(dirname / name))

  wrapResults.add makeRstSection(1, name, section)


#================================  tests  ================================#

import unittest

suite "CPP dependency analysis graph":
  test "test":
    kvCall makeDepGraph:
      name: "Two level dependency"
      files:
        stringKvTable:
          "common.hpp":
            """
            struct Aux { int hello; };
            """

          "dep1.hpp": "class D1 {};"

          "dep2.hpp":
            """
            #include "dep2-1.hpp"
            #include "common.hpp"
            class D2 { public: D2_1 dep2_1; };
            Aux impl() {}
            """

          "dep2-1.hpp":
            """
            #include "common.hpp"
            class D2_1 {};
            Aux impl2() {}
            """

          "main.hpp":
            """
            #include "dep1.hpp"
            #include "dep2.hpp"
            class Main { public: D1 dep1; D2 dep2; };
            """


rst2html(wrapResults.joinl(), "dep-analyser.html")

import sugar, strutils, sequtils, strformat, os

import hcparse/libclang
import hmisc/other/[hshell, colorlogger, oswrap]
import hmisc/[hexceptions, helpers]

import unittest

let
  qtSource = ~"workspace/git-snadbox/qt/qt-everywhere-src-5.15.0"
  includePaths = @[
    qtSource / "qtbase/include"
  ]

suite "Wrap qt widgets":
  test "main":
    echo qtSource / "qtbase/src/widgets/widgets"
    let
      index = createIndex()
      unit = index.parseTranslationUnit(
        qtSource / "qcombobox.h",
        includePaths.addPrefix("-I") & @[
          "-xc++"
        ],
        {tufSkipFunctionBodies}
      )

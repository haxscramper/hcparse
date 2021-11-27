# Package

version       = "0.1.2"
author        = "haxscramper"
description   = "High-level nim wrapper for C/C++ parsing"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.6.0"
requires "hnimast"
requires "htsparse >= 0.1.10"
requires "https://github.com/disruptek/frosty.git >= 2.1.1"
requires "hmisc >= 0.11.5"

import std/[os, strutils]

task test, "Run tests":
  let dir = currentSourcePath().parentDir() / "tests"
  for file in listFiles(dir):
    let (_, name, _) = splitFile(file)
    if name.startsWith("t") and file.endsWith(".nim"):
      echo file
      exec "nim r " & file
  # exec "nim r tests/runall.nim test " & currentSourcePath() & " --parse-errors=false"

task docgen, "Generate documentation":
  exec "nim c -r tests/runall.nim doc " & currentSourcePath()

task push, "Execute checks and push ":
  exec "nim r tests/runall.nim push " & currentSourcePath()

task newversion, "Tag new version and push it to git":
  exec "nim r tests/runall.nim newversion " & currentSourcePath()

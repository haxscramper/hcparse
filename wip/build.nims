#!/usr/bin/env nim
import shell
import strutils, sequtils, strformat

shell:
  "clang++" "-fcolor-diagnostics" -lfmt "-lclang" "-olibclang" "libclang-1.cpp"
  ./libclang
  # bat "--line-range 180:210 --color=always result.nim"
  ./fixshit.sh

proc addImports(file: string, imports: seq[string]): void =
  let file = "../src/hcparse/libclang_raw/" & file & ".nim"
  echo &"updated {file}"
  let body = file.readFile()
  file.writeFile imports.mapIt(&"import {it}\n").join("") & body

let importMap = {
  "index" : @[
    "build_system",
    "cxcompilation_database",
    "cxerror_code",
    "cxstring",
    "externc",
    "fatal_error_handler",
    "opaque_impls",
    "platform"
  ],
  "build_system" : @[
    "cxerror_code",
    "cxstring",
    "platform",
    "externc"
  ],
  "cxcompilation_database" : @[
    "cxstring",
  ],
  "documentation" : @[
    "externc",
    "index"
  ]
}

for (file, imports) in importMap:
  addImports(file, imports)

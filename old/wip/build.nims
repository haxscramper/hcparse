#!/usr/bin/env nim
import shell
import strutils, sequtils, strformat

proc generate_depgraph =
  shell:
    "clang++ -lboost_system -lboost_filesystem -lboost_thread -lboost_wave -odeps deps.cpp"
    "./deps -S/usr/include/c++/10.2.0 -S/usr/include/c++/10.2.0/x86_64-pc-linux-gnu test.cpp"

generate_depgraph()

proc generateLibclang_v0_1 =
  shell:
    "clang++" "-fcolor-diagnostics" -lfmt "-lclang" "-olibclang" "libclang-1.cpp"
    ./libclang
    # bat "--line-range 180:210 --color=always result.nim"
    ./fixshit.sh

  func makeFileName(file: string): string =
    "../src/hcparse/libclang_raw/" & file & ".nim"

  proc addImports(file: string, imports: seq[string]): void =
    let file = makeFileName(file)
    # let file = "../src/hcparse/libclang_raw/" & file & ".nim"
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

  var sedfix = {
    "index" : @[
      "'s/PriorityForAll = 0/PriorityForAll = 4/'",
      "'s/CXCursor_FirstDecl/# CXCursor_FirstDecl = 1/'",
      "'s/CXCursor_LastDecl/# CXCursor_LastDecl = 39/'",
      "'s/CXCursor_FirstRef/# CXCursor_FirstRef = 40/'",
      "'s/CXCursor_InvalidFile = 70/# CXCursor_InvalidFile = 70/'",
      "'s/CXCursor_FirstExpr = 100/# CXCursor_FirstExpr = 100/'",
      "'s/CXCursor_FirstStmt = 200/# CXCursor_FirstStmt = 200/'",
      "'s/CXCursor_AsmStmt/# CXCursor_AsmStmt/'",
      "'s/CXCursor_UnexposedAttr = 400/# CXCursor_UnexposedAttr = 400/'",
      "'s/CXCursor_InclusionDirective = 503/# CXCursor_InclusionDirective = 503/'",
      "'s/CXCallingConv_X86_64SysV = 11/# CXCallingConv_X86_64SysV = 11/'",
      "'s/CXEval_UnExposed = 0//'",
      r"'s/CXEval_Int = 1/CXEval_UnExposed = 0\n    CXEval_Int = 1/'",
      r"'s/ptr\[const /ptr\[/'"
    ]}

  for (file, fix) in sedfix:
    let file = makeFileName(file)
    for edit in fix:
      shell:
        sed -i ($edit) ($file)

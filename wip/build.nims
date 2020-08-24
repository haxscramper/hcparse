#!/usr/bin/env nim
import shell

shell:
  "clang++" "-fcolor-diagnostics" -lfmt "-lclang" "-olibclang" "libclang-1.cpp"
  ./libclang
  bat "--line-range 180:210 --color=always result.nim"

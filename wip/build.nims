#!/usr/bin/env nim
import shell

shell:
  "clang++" "-fcolor-diagnostics" "-lclang" "-olibclang" "libclang-1.cpp"
  ./libclang

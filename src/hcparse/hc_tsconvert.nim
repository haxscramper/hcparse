import
  htsparse/cpp/cpp

import
  std/[json]

import
  ./hc_types,
  ./hc_save

import
  hmisc/core/all,
  hmisc/wrappers/[treesitter],
  hmisc/other/oswrap

export parseCppString

when isMainModule:
  echo 1

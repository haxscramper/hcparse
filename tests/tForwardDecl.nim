import hcparse/[wrap_common]
import hmisc/other/oswrap

withTempDir():
  mkDirStructure:
    file "class-B.hpp": "class A; class B { A* ptr; }"
    file "class-A.hpp": "class B; class A { B* ptr; }"

  let parseConf = baseCppParseConf.withIt do:
    discard

  let wrapConf = baseCppWrapConf.withDeepIt do:
    it.baseDir = cwd()
    it.parseConf = parseConf

  wrapAllFiles(@[
    cwd() /. "class-B.hpp",
    cwd() /. "class-A.hpp"
  ], wrapConf, parseConf)

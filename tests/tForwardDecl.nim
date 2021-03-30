import hcparse/[wrap_common]
import hmisc/other/oswrap

withTempDir():
  startColorLogger()

  mkDirStructure:
    file "class-B.hpp": "struct A; struct B { A* ptr; };"
    file "class-A.hpp": "struct B; struct A { B* ptr; };"

  let parseConf = baseCppParseConf.withIt do:
    discard

  let wrapConf = baseCppWrapConf.withDeepIt do:
    it.baseDir = cwd()
    it.parseConf = parseConf

  wrapAllFiles(@[
    cwd() /. "class-B.hpp",
    cwd() /. "class-A.hpp"
  ], wrapConf, parseConf)

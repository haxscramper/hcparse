import hmisc/preludes/unittest
import hcparse/hc_parsefront

suite "Parse basic file":
  let dir = getTestTempDir()
  mkDir dir
  test "Dump tree repr":
    writeFile(dir /. "main.cpp", "int main() {}")

    let unit = parseFile(dir /. "main.cpp")
    echo unit.getTranslationUnitCursor().treeRepr(some unit, tokenKinds = true)

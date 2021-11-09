import hmisc/preludes/unittest
import hmisc/other/hpprint
import
  hcparse/[
    hc_parsefront,
    hc_impls
  ],

  hcparse/read_libclang/[
      hc_types,
      hc_visitors,
      hc_clangreader,
  ],

  hcparse/processor/[hc_postprocess]

suite "Parse basic file":
  let dir = getTestTempDir()
  mkDir dir
  let file = dir /. "file.c"
  test "Dump tree repr":
    writeFile(file, """
int main() {}
""")

    let unit = parseFile(file)
    echo unit.getTranslationUnitCursor().treeRepr(unit)

  test "Visitors":
    var cache = newWrapCache()
    let api = parseFile(file).splitDeclarations(baseCppWrapConf, cache)

    pprint api

    var fix = baseFixConf.withIt do:
      it.typeStore = newTypeStore()

    fix.onGetBind():
      return cxxHeader("?")

    let wrapped = api.wrapApiUnit(baseCppWrapConf, cache).
      postFixEntries(fix, cxxLibImport("h", @["h"]))

    echo "Generated wrapped"
    echo wrapped.toString(cxxCodegenConf)

    # pprint wrapped[0]

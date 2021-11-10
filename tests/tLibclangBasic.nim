import hmisc/preludes/unittest
import
  hmisc/other/[
    hpprint,
    hlogger
  ],

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
  let file = dir /. "file.hpp"
  test "Dump tree repr":
    writeFile(file, """
struct Struct {
  int field;

  int get() const;
  void set(int arg);
};

template <typename T, class Z = int>
struct Templated {
  T get() const noexcept;
  void set(Z value);
};

// enum class Enum {en1, en2};
// Enum getEnum(Enum in);

Templated<int> operator%(int arg1, Struct arg2);


int main() {}
""")

    let unit = parseFile(file)
    echo unit.getTranslationUnitCursor().treeRepr()

  test "Visitors":
    var cache = newWrapCache()
    let api = parseFile(file).splitDeclarations(baseCppWrapConf, cache)

    # pprint api

    var fix = baseFixConf.withIt do:
      it.typeStore = newTypeStore()

    fix.onGetBind():
      return cxxHeader("?")

    var wrap = baseCppWrapConf.withDeepIt do:
      it.logger = newTermLogger()

    let wrapped = api.wrapApiUnit(wrap, cache).
      postFixEntries(fix, cxxLibImport("h", @["h"]))

    echo "Generated wrapped"
    echo wrapped.toString(cxxCodegenConf)

    # pprint wrapped[0]

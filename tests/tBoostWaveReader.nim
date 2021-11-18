import hcparse
import hmisc/preludes/unittest
import hmisc/other/oswrap

suite "Basic reader":
  test "Include unistd":
    var cache = newWaveCache()
    var reader = newWaveReader(
      currentSourcePath().AbsFile(),
      cache,
      baseCParseConf,
      some "#include <unistd.h>\n"
    )

    for tok in items(reader.ctx):
      echov "safasdf"

  test "Parse relative source":
    var cache = newWaveCache()
    var reader = newWaveReader(
      AbsFile(relToSource"files/wavereader_main.h"),
      cache,
      baseCParseConf)

    echo reader.getExpanded()

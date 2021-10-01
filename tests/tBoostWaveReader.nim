import hcparse/hc_wavereader
import hmisc/preludes/unittest
import hmisc/other/oswrap

block:
  echov "Without hook"
  var reader = newWaveReader(AbsFile(
    relToSource"files/wavereader_main.h"))

  echo reader.getExpanded()

block:
  echov "With hook"
  var reader = newWaveReader(AbsFile(
    relToSource"files/wavereader_main.h"), true)

  echo reader.getExpanded()

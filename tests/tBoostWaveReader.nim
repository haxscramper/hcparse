import hcparse/hc_wavereader
import hmisc/preludes/unittest
import hmisc/other/oswrap

var reader = newWaveReader(AbsFile(
  relToSource"files/wavereader_main.h"), true)

echo reader.getExpanded()

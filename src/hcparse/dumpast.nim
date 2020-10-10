import hmisc/other/oswrap
import hmisc/macros/matching

import hcparse/libclang

[opt @file or "test.cpp", .._] := paramStrs()

echo file


let flags = @[
  "-DDDDDDDDDDDDDDD"
]

var index = createIndex()
var unit = parseTranslationUnit(
  index, file, flags, {
    tufSkipFunctionBodies,
    tufDetailedPreprocessingRecord,
    # tufRetainExcludedConditionalBlocks
})

echo unit.getTranslationUnitCursor().treeRepr(unit)

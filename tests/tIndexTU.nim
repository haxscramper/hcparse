import hcparse/libclang
import hmisc/other/oswrap


let
  file = currentSourceDir() /. "header.hpp"
  trIndex = createIndex(0, 0)
  unit = parseTranslationUnit(trIndex, file, trOptions = {tufNone})

var (data, visitor) = makeDeclarationIndexer []:
  echo "Invoked callback"

var callbacks = IndexerCallbacks(
  indexDeclaration: visitor,
)

let action = indexActionCreate(trIndex)

var indexOpts = 0

discard indexTranslationUnit(
  action,
  toClientData(data),
  addr callbacks,
  1,
  0,
  unit
)

import hcparse/libclang

let index = createIndex(0, 0)

let args = allocCStringArray(["-v"])

let unit = parseTranslationUnit(
  index,
  "/tmp/test-file.cpp".cstring,
  args,
  1,
  nil,
  0,
  0)

echo cast[cuint](tufNone)

if unit.isNil():
  echo "error"

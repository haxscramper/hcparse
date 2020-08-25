import index

let trIndex = clang_createIndex(0, 0);

let unit = clang_parseTranslationUnit(
  trIndex,
  "/usr/include/clang-c/Index.h".cstring,
  nil,
  0,
  nil,
  0,
  cuint(CXTranslationUnit_None));

if unit.isNil:
  raiseAssert "Unable to parse translation unit. Quitting."

# CXCursor   cursor = clang_getTranslationUnitCursor(unit);

import libclang
import bitops


proc parseTranslationUnit*(
  trIndex: CXIndex,
  filename: string,
  cmdline: seq[string] = @[],
  trOptions: set[CXTranslationUnit_Flags] = {
    tufSingleFileParse}): CXTranslationUnit =

  var flags: int
  for opt in trOptions:
    flags = bitor(flags, int(opt))

  let argc = cmdline.len
  let cmdline = allocCSTringArray(cmdline)

  result = parseTranslationUnit(
    trIndex, filename.cstring, cmdline, cint(argc), nil, 0, cuint(flags))

  deallocCStringArray(cmdline)

import hmisc/helpers, std/[strutils, sequtils]
import hmisc/other/oswrap

func toCamelCase*(str: string): string =
  var buf = str.split("_").
    filterIt(it.len > 0).
    mapIt(it.capitalizeAscii())

  buf[0][0] = buf[0][0].toLowerAscii()

  return buf.join("")

proc fixIdentName*(str: string): string =
  result = str.toCamelCase()
  result = case result:
    of "set": "cxSet"
    of "type": "cxType"
    of "range": "cxRange"
    of "string": "cxString"
    of "begin": "cxBegin"
    of "end": "cxEnd"
    of "is": "cxIs"
    of "in": "cxIn"
    of "include": "cxInclude"
    of "proc": "cxProc"
    of "method": "cxMethod"
    else: result

func toPascalCase*(str: string): string =
  str.split("_").
    filterIt(it.len > 0).
    mapIt(it.capitalizeAscii()).
    join("")

proc fixStdTypeName*(head: string, idx: int): string =
  if head.len == 0:
    result = "T" & $idx
  else:
    let split = head.split("::")
    for name in split[0 .. ^1]:
      result &= name.toPascalCase()

func toIncludes*(files: seq[AbsDir]): seq[string] =
  for file in files:
    result.add file.getStr().addPrefix("-I")

func validCxxIdentifier*(str: string): bool =
  if str[0] notin IdentStartChars:
    return false

  for ch in str:
    if ch notin IdentChars:
      return false

  return true

proc fixFileName*(name: string): string =
  name.multiReplace({
    "-": "_",
    "+": "p"
  })

import hmisc/helpers, std/[strutils, sequtils, parseutils]
import hmisc/other/oswrap

func toCamelCase*(str: string): string =
  var buf = str.split("_").
    filterIt(it.len > 0).
    mapIt(it.capitalizeAscii())

  if buf.len > 0 and buf[0].len > 0:
    buf[0][0] = buf[0][0].toLowerAscii()

  return buf.join("")

proc fixIdentName*(str: string): string =
  var str = str
  str = str[str.skipWhile({'_'}) .. ^1]

  case str:
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
    of "block": "cxBlock"
    elif str.normalize() in ["cstringarray"]:
      str

    else:
      str.toCamelCase()

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
  name.multiReplace({"-": "_", "+": "p"}).
    RelFile().withoutExt().getStr().toSnakeCase()

proc toNimFile*(file: RelFile): RelFile =
  var buf: seq[string]
  for pathPart in file.getStr().split("/"):
    buf.add fixFileName(pathPart)

  result = RelFile(buf.join("/")).withExt("nim")

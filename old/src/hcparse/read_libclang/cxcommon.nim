import std/[strutils, sequtils, parseutils]
import hmisc/other/oswrap
import hmisc/algo/hstring_algo
import hmisc/core/all

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
  str = str.replace("::", "_")

  case str:
    of "set": "cxSet"
    of "type": "cxType"
    of "range": "cxRange"
    of "string": "cxString"
    of "end": "`end`"
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


func stripComment*(text: string): string =
  const starts = ["/*!", "/**<", "/**", "/*", "//<", "///", "//", "*"]
  const ends  = ["*/", "*"]
  var idx = 0
  for line in text.splitLines():
    var pos = line.skipWhile({' '})
    for start in starts:
      if pos + start.len < line.len and
         line[pos .. pos + start.high] == start:
        pos = clamp(pos + start.len, 0, high(line))
        break

    var final = line.high
    for endc in ends:
      if ?line and line[
        clamp(final - endc.high, 0, final) .. final] == endc:
        final = clamp(final - endc.len, pos, high(line))
        break

    if idx != 0:
      result.add "\n"

    else:
      while pos < line.len and line[pos] in {' '}:
        inc pos

    inc idx
    if 0 < line.len:
      if line[pos .. final] in ["*", "/"]:
        result.add ""

      else:
        result.add line[pos .. final]


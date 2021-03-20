import hc_types, cxcommon, cxtypes, hc_typeconv
import std/[strformat, tables, hashes, strutils, options]
import hmisc/other/colorlogger
import hmisc/algo/hstring_algo
import hmisc/base_errors
import hparse/htreesitter/htreesitter

import htsparse/[cpp/cpp, c/c]

proc getName(node: CppNode, text: string): Option[CName] =
  if node.kind in {cppTypeIdentifier, cppIdentifier}:
    some toCName(text[node.slice()])

  elif node.kind in {cppEnumSpecifier, cppEnumerator}:
    getName(node[0], text)

  elif node.kind in {cppEnumeratorList}:
    none CName

  else:
    raiseImplementError(&"Kind {node.kind} {treeRepr(node, text)}")

proc visit(
    node: CppNode,
    cache: var WrapCache,
    lastComment: var seq[Slice[int]],
    instr: string,
    nameCtx: CSCopedIdent
  ) =

  case node.kind:
    of cppTranslationUnit:
      for stmt in node:
        visit(stmt, cache, lastComment, instr, nameCtx)

    of cppComment:
      lastComment.add node.slice()

    of cppEnumSpecifier:
      let name = getName(node, instr)
      if name.isNone():
        discard
        # warn "Cannot get name for enumeration"

      else:
        let name = name.get()
        var buf: seq[string]
        for comment in lastComment:
          # HACK to recognize only comments that are 'close enough'. All
          # comments are collected in buffer and then dumped into adjacent
          # elements. This also indludes file-level comments like GNU license
          # shit etc.
          if comment.b >= node.slice().a - 1:
            buf.add instr[comment]

          cache.addDoc(nameCtx & name, buf)

        lastComment = @[]

        var lastCtx: CScopedIdent

        for fld in node[1]:
          if fld.kind == cppEnumerator:
            lastCtx = nameCtx & name & getName(fld, instr).get()

          else:
            cache.addDoc(lastCtx, @[instr[fld.slice()]])

      lastComment = @[]



    else:
      discard
      # warn treeRepr(node, instr)

proc strip(str, left, right: string): string =
  var slice = 0 ..< str.len
  if str.startsWith(left):
    inc slice.a, left.len

  if str.endsWith(right):
    dec slice.b, right.len

  result = str[slice]

proc formatComment*(str: string): string =
  # result = str
  var buf: seq[string]
  for line in str.strip("/*", "*/").split('\n'):
    buf.add strutils.strip(line).dropPrefix("*").dropPrefix("//")

  result = buf.join("\n") #.strip()

proc fillDocComments*(file: string, cache: var WrapCache) =
  let tree = parseCppString(file)
  var lastComment: seq[Slice[int]]
  logIndented:
    visit(tree, cache, lastComment, file, @[])

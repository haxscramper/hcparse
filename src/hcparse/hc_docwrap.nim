import hc_types, cxcommon, cxtypes, hc_typeconv
import std/[strformat, tables, hashes]
import hmisc/other/colorlogger
import hmisc/base_errors

import htsparse/cpp/cpp

proc getName(node: CppNode, text: string): CName =
  if node.kind in {cppTypeIdentifier, cppIdentifier}:
    toCName(text[node.slice()])

  elif node.kind in {cppEnumSpecifier, cppEnumerator}:
    getName(node[0], text)

  else:
    raiseImplementError(&"Kind {node.kind}")

proc visit(
    node: CppNode,
    cache: var WrapCache,
    lastComment: var seq[string],
    instr: string,
    nameCtx: CSCopedIdent
  ) =

  case node.kind:
    of cppTranslationUnit:
      for stmt in node:
        visit(stmt, cache, lastComment, instr, nameCtx)

    of cppComment2:
      lastComment.add instr[node.slice()]

    of cppEnumSpecifier:
      let name = getName(node, instr)
      cache.addDoc(nameCtx & name, lastComment)
      lastComment = @[]

      var lastCtx: CScopedIdent

      for fld in node[1]:
        if fld.kind == cppEnumerator:
          lastCtx = nameCtx & name & getName(fld, instr)

        else:
          cache.addDoc(lastCtx, @[instr[fld.slice()]])

      lastComment = @[]



    else:
      warn treeRepr(node, instr)

proc formatComment*(str: string): string =
  result = str

proc fillDocComments*(file: string, cache: var WrapCache) =
  info "Filling documentation comments"
  let tree = parseCppString(file)
  echo treeRepr(tree, file)
  var lastComment: seq[string]
  logIndented:
    visit(tree, cache, lastComment, file, @[])

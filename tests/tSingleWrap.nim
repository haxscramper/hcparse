import std/[
  sugar, strutils, sequtils, strformat, httpclient, htmlparser,
  xmltree, unittest, tables, strtabs
]

import hcparse/[hcparse_cli, libclang]
import hmisc/other/[oswrap, hshell, colorlogger]
import hmisc/types/[colorstring, colortext]
import hmisc/helpers
import hasts/html_ast
import hnimast
import std/enumerate

let srcd = AbsDir(currentSourcePath()).splitDir().head

proc hasSubn*(x: XmlNode): bool =
  xmltree.kind(x) == xnElement

proc `[]`(x: XmlNode, k: string): string =
  if x.attrs.isNil or k notin x.attrs:
    ""
  else:
    x.attrs[k]

startColorLogger(showfile = true)

proc treeRepr(xml: XmlNode, prefIdx: bool = false): string =
  func aux(x: XmlNode, level: int, idx: seq[int]): string =
    let pref =
      if prefIdx:
        idx.join("", ("[", "]")) & "    "
      else:
        "  ".repeat(level)

    case x.kind:
      of xnElement:
        result = &"{pref}<{toBlue(x.tag)}> "
        if not x.attrs.isNil:
          for k, v in x.attrs:
            result &= &"{k}=\"{v}\""

        # result &= "[" & $x.len & "]"
        if x.len > 0:
          result &= "\n"


        for newIdx, node in enumerate(x):
          result &= aux(node, level + 1, idx & newIdx)
          if newIdx < x.len - 1:
            result &= "\n"
      of xnText:
        result &= pref & toYellow($x)
      else:
        result &= &"{pref}{x.kind} {x}"

  return aux(xml, 0, @[0])

var annotTable: Table[string, string]

proc fillTable(h: XmlNode) =
  # if h.attrs.isNil:
  #   return

  if h.kind == xnElement and
     h.tag == "div" and
     h["class"] == "sect2":

    var currNodes: seq[XmlNode]
    for node in h:
      if node["class"] == "funcsynopsis":
        if currNodes.len > 0 and
           currNodes[0].tag != "p" and
           currNodes[0][0].tag == "a":

          info "Found documented function"
          var res: string
          for node in currNodes:
            res.add $node & "\n"

          # echo res
          let rst = evalShellStdout(shCmd(
            pandoc, -f, html, -t, rst, "-"
          ), stdin = res)

          annotTable[currNodes[0][0]["id"]] = rst

          # info currNodes[0][0]["id"]
          # # info currNodes[0][0][0]["id"]
          # for node in currNodes[1..^1]:
          #   logIndented:
          #     debug node.treeRepr(true)


          # if (
          #      currNodes[0].len == 2 and
          #      currNodes[0][0].tag == "a" and
          #      currNodes[0][1][0]["class"] == "funcdef"
          #    ) or
          #    (
          #      currNodes[0].len == 1 and
          #      currNodes[0][0][0]["class"] == "funcdef"
          #    )
          #   :
          #   discard
          # else:

        currNodes = @[node]
      if node["class"] in ["titlepage"]:
        discard
      else:
        currNodes.add node

  elif h.kind == xnElement:
    for node in h:
      fillTable(node)



let htmlfile = AbsFile("/tmp/docs.html")
if not htmlfile.fileExists():
  let client = newHttpClient()
  client.downloadFile(
    "https://www.x.org/releases/current/doc/libX11/libX11/libX11.html",
    htmlfile.getStr()
  )

let h = loadHtml(htmlfile.getStr())

# fillTable(h)


proc x11DocAnnotation(
    we: var WrappedEntry,
    conf: WrapConfig,
    codegen: var seq[CxxCodegen]
  ): seq[WrappedEntry] =

  if we.wrapped.kind in {nekProcDecl}:
    let name = we.wrapped.procdecl.name
    if name in annotTable:
      # debug "Annotate", name
      # debug annotTable[name]
      we.mwrapped.procdecl.docComment = annotTable[name]

suite "X11 garbage wrap test":
  test "X11":
    proc doWrap(infile, outfile: AbsFile) =
      let wconf = baseWrapConfig.withIt do:
        # it.isImportcpp = false
        it.depResolver = (
          proc(cursor, referecendBy: CXCursor): DepResolutionKind {.closure.} =
            if cursor.isFromMainFile():
              drkWrapDirectly

            else:
              drkIgnoreIfUsed
        )

      let pconf = baseCppParseConfig.withIt do:
        it.globalFlags = @["-DXLIB_ILLEGAL_ACCESS"]

      let (wrapped, codegen) = wrapSingleFile(
        infile,
        postprocess = @[
          newPostprocess(x11DocAnnotation),
          newPostprocess(nep1Idents)
        ],
        errorReparseVerbose = false,
        wrapConf = wconf,
        parseConf = pconf
      )

      withStreamFile(outfile):
        for entry in wrapped:
          # stdout.writeLine(entry)
          file.write(entry)




    if false:
      doWrap(AbsFile("/usr/include/X11/Xlib.h"), AbsFile("/tmp/xlib.nim"))
      doWrap(AbsFile("/usr/include/X11/X.h"), AbsFile("/tmp/x.nim"))
      doWrap(
        AbsFile("/usr/lib/clang/11.0.0/include/stddef.h"),
        AbsFile("/tmp/stddef.nim"))

      info "Done wrapping, running check"
      execShell shCmd(nim, check, "/tmp/xlib.nim")

      info "Done check, wrap OK !"

    else:
      doWrap(AbsFile("/tmp/a.hpp"), AbsFile("/tmp/b.nim"))

suite "single file wrap":
  test "test":
    wrapCpp(
      srcd /. "incpp.cpp",
      srcd /. "resnim.nim"
    )

suite "WIP tests":
  test "test":
    startHax()
    wrapCpp(
      srcd /. "wip.cpp",
      AbsFile("/tmp/res_a.nim"),
      some AbsDir("/tmp/")
    )

    "/tmp/main.nim".writeFile """
import res_a

test(initH(12))
test()
"""

    execShell shCmd(nim).withIt do:
      it.arg "r"
      it - ("backend", "cpp")
      it - ("nimcache", "cache")
      it.arg "/tmp/main.nim"

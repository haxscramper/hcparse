import libclang_wrap
import std/[macros, sugar, sequtils, decls]
import hmisc/core/[all, code_errors]
import hnimast

export decls

#*************************************************************************#
#**************************  Visitor wrappers  ***************************#
#*************************************************************************#

proc toClientData*[T](val: var T): CXClientData =
  CXClientData(addr val)

type CXVisitor = proc(
  cursor, parent: CXCursor,
  clientData: pointer): CXChildVisitResult {.cdecl.}

proc toCursorVisitor*(impl: CXVisitor): CXCursorVisitor =
  CXCursorVisitor(impl)

proc visitChildren*[T](
  cursor: CXCursor, callback: tuple[data: T, impl: CXVisitor]) =
  ## Overload for child visit used by `makeVisitor` macro
  var data = callback.data
  discard cursor.visitChildren(
    toCursorVisitor(callback.impl),
    toClientData(data))

func makeVisitorImpl*(
    varnames: seq[NimNode],
    procArgs: openarray[(string, NType[NimNode])],
    returnType: NType[NimNode],
    body: NimNode
  ): NimNode =

  var immutableCopies = newStmtList()
  let tupleData = nnkPar.newTree: collect(newSeq):
    for capture in varnames:
      capture.assertNodeKind({nnkIdent})
      immutableCopies.add quote do:
        when not isMutable(`capture`):
          var `capture` = `capture`

      newColonExpr(capture, newCall("addr", capture))

  let byaddrid = ident "byaddr"
  let dataUnpack = newStmtList: collect(newSeq):
    for capture in varnames:
      quote do:
        var `capture` {.`byaddrid`.} = data[].`capture`[]

  let
    # cursorId = ident "cursor"
    # parentId = ident "parent"
    dataId = genSym(nskType, "Data")


  var procDecl = newNProcDecl(
    name = "visitor",
    args = procArgs,
    returnType = some(returnType),
    exported = false,
    pragma = newNPragma("cdecl"),
    impl = (
      quote do:
        let data {.inject.} = cast[ptr[`dataId`]](clientData)
        `dataUnpack`
        `body`
    )
  ).toNNode()


  result = quote do:
    block:
      `immutableCopies`
      var data {.inject.} = `tupleData`
      type `dataId` = typeof(data)
      `procDecl`
      # proc visitor(`cursorId`, `parentId`: CXCursor,
      #              clientData: pointer): CXChildVisitResult {.cdecl.} =

      (data, visitor)


macro makeVisitor*(captureVars, body: untyped): untyped =
  ## Create `{.cdecl.}` callback for child visit. Macro head is a
  ## `[var1, var2, ...]` list - all variables that are available in
  ## the visitor body must be listed explicitly.
  ##
  ## .. code-block:: Nim
  ##
  ##    var buf:  seq[string]
  ##    makeVisitor [unit, functionNames]:
  ##      if cursor.cxKind == CXCursor_FunctionDecl:
  ##        buf.add $cursor
  captureVars.assertNodeKind({nnkBracket})
  result = makeVisitorImpl(
    toSeq(captureVars),
    procArgs = {
      "cursor" : newNType("CXCursor"),
      "parent" : newNType("CXCursor"),
      "clientData" : newNType("pointer"),
    },
    returnType = newNType("CXChildVisitResult"),
    body
  )

  # echo result.repr

macro makeDeclarationIndexer*(captureVars, body: untyped): untyped =
  captureVars.assertNodeKind({nnkBracket})
  result = makeVisitorImpl(
    toSeq(captureVars),
    procArgs = {
      "clientData" : newNType("CXClientData"),
      "idxDeclInfo" : newNType("ptr", @["CXIdxDeclInfo"]),
    },
    returnType = newNType("void"),
    body
  )

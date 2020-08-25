import macros, sugar
import hmisc/hexceptions
import hmisc/types/hnim_ast

import index
import cxstring

proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil

proc `$`(cxstr: CXString): string =
  let str = clang_getCString(cxstr)
  result = $str
  clang_disposeString(cxstr)

proc `$`(cursor: CXCursor): string = $clang_getCursorSpelling(cursor)
proc `$`(cxtype: CXType): string = $clang_getTypeSpelling(cxtype)
proc `$`(cxkind: CXCursorKind): string =
  $clang_getCursorKindSpelling(cxkind)

proc cxKind*(cursor: CXCursor): CXCursorKind =
  clang_getCursorKind(cursor)

proc cxType*(cursor: CXCursor): CXType =
  clang_getCursorType(cursor)


proc toClientData*[T](val: var T): CXClientData =
  CXClientData(addr val)

proc toCursorVisitor*(
  impl: proc(cursor, parent: CXCursor,
             clientData: pointer): CXChildVisitResult {.cdecl.}): CXCursorVisitor =
  CXCursorVisitor(impl)


proc clang_visitChildren[T](
  cursor: CXCursor,
  callback: tuple[
    data: T,
    impl: proc(cursor, parent: CXCursor,
               clientData: pointer): CXChildVisitResult {.cdecl.}]) =

  var data = callback.data
  discard cursor.clang_visitChildren(
    toCursorVisitor(callback.impl),
    toClientData(data)
  )


macro makeVisitor(captureVars, body: untyped): untyped =
  captureVars.assertNodeKind({nnkBracket})
  let tupleData = nnkPar.newTree: collect(newSeq):
    for capture in captureVars:
      capture.assertNodeKind({nnkIdent})
      newColonExpr(capture, newCall("addr", capture))

  let dataUnpack = newStmtList: collect(newSeq):
    for capture in captureVars:
      quote do:
        var `capture` = data[].`capture`[]

  let
    cursorId = ident "cursor"
    parentId = ident "parent"

  result = quote do:
    var data {.inject.} = `tupleData`
    type Data = typeof(data)
    proc visitor(`cursorId`, `parentId`: CXCursor,
                 clientData: pointer): CXChildVisitResult {.cdecl.} =
      let data {.inject.} = cast[ptr[Data]](clientData)
      `dataUnpack`
      `body`

    (data, visitor)

  echo $!result





proc main() =
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

  let cursor: CXCursor = clang_getTranslationUnitCursor(unit)


  var functions: seq[CXCursor]

  expandMacros:
    cursor.clangVisitChildren do:
      makeVisitor [functions]:
        if cursor.cxKind == CXCursor_FunctionDecl:
          echo cursor
          return CXChildVisit_Continue
        else:
          return CXChildVisit_Recurse

main()


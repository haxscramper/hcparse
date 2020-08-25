import macros, sugar, strformat, lenientops, bitops
import hmisc/hexceptions
import hmisc/helpers
import hmisc/types/hnim_ast
import compiler/[parser, llstream, idents, options,
                 pathutils, astalgo, msgs,
                 ast, renderer, lineinfos]


import index, documentation, cxstring

proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil


#==========================  String conversion  ==========================#

proc `$`(cxstr: CXString): string =
  let str = clang_getCString(cxstr)
  result = $str
  clang_disposeString(cxstr)

proc `$`(cursor: CXCursor): string = $clang_getCursorSpelling(cursor)
proc `$`(cxtype: CXType): string = $clang_getTypeSpelling(cxtype)
proc `$`(cxkind: CXCursorKind): string =
  $clang_getCursorKindSpelling(cxkind)

#=====================  Cursor kind/type operations  =====================#

proc cxKind*(cursor: CXCursor): CXCursorKind =
  clang_getCursorKind(cursor)

proc expectKind*(cursor: CXCursor, kind: CXCursorKind) =
  if cursor.cxKind != kind:
    raiseAssert(&"Expected cursor kind {kind}, but got {cursor.cxKind()}")

proc cxType*(cursor: CXCursor): CXType =
  clang_getCursorType(cursor)

proc cxKind*(cxtype: CXType): CXTypeKind = cxtype.kind

#===========================  Location checks  ===========================#

proc isFromMainFile*(cursor: CXCursor): bool =
  cursor.
    clang_getCursorLocation().
    clang_Location_isFromMainFile() != cint(0)

#==========================  Subnode visitors  ===========================#

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

func takesOnlyMutable*[T](v: var T) = discard
template isMutable*(v: typed): untyped = compiles(takesOnlyMutable(v))

macro makeVisitor(captureVars, body: untyped): untyped =
  var immutableCopies = newStmtList()
  captureVars.assertNodeKind({nnkBracket})
  let tupleData = nnkPar.newTree: collect(newSeq):
    for capture in captureVars:
      capture.assertNodeKind({nnkIdent})
      immutableCopies.add quote do:
        when not isMutable(`capture`):
          var `capture` = `capture`

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

#=========================  Filtering subnodes  ==========================#

proc children*(cursor: CXCursor): seq[CXCursor] =
  var buf: seq[CXCursor]
  cursor.clangVisitChildren do:
    makeVisitor [buf]:
      buf.add cursor
      return CXChildVisit_Continue

#============================  Documentation  ============================#

proc comment*(cursor: CXCursor): CXComment =
  cursor.clang_Cursor_getParsedComment()

proc cxKind*(comment: CXComment): CXCommentKind =
  comment.clang_Comment_getKind()

proc `$`*(comment: CXComment): string =
  echo comment.cxKind()
  $comment.clang_FullComment_getAsHTML() # NOTE it seems like libclang
  # API does not provide a way to get full comment string as raw string.

#=============================  Converters  ==============================#

proc newPIdent*(str: string): PNode =
  newIdentNode(PIdent(s: str), TLineInfo())

proc toNimType*(cxtype: CXType): PNode =
  case cxtype.cxKind:
    of CXType_Bool:
      newPIdent("bool")
    else:
      newPIdent("!!!")

#==========================  Toplevel visitors  ==========================#

proc visitFunction(cursor: CXCursor, stmtList: var PNode) =
  echo "function ", cursor, " kind ", cursor.cxKind()
  echo "with comment '", cursor.comment(), "'"


#================================  Main  =================================#

proc parseTranslationUnit(
  trIndex: CXIndex,
  filename: string,
  cmdline: seq[string] = @[],
  trOptions: set[CXTranslationUnit_Flags] = {}): CXTranslationUnit =

  var flags: int
  for opt in trOptions:
    flags = bitor(flags, int(opt))

  let argc = cmdline.len
  let cmdline = allocCSTringArray(cmdline)

  result = clang_parseTranslationUnit(
    trIndex, filename.cstring, cmdline, cint(argc), nil, 0, cuint(flags))

  deallocCStringArray(cmdline)



proc main() =
  let trIndex = clang_createIndex(0, 0);
  let unit = parseTranslationUnit(trIndex, "header.hpp")

  if unit.isNil:
    raiseAssert "Unable to parse translation unit. Quitting."

  let cursor: CXCursor = clang_getTranslationUnitCursor(unit)

  var toplevel: PNode = nkStmtList.newTree()

  cursor.clangVisitChildren do:
    makeVisitor [toplevel]:
      if cursor.isFromMainFile():
        case cursor.cxkind:
          of CXCursor_FunctionDecl:
            visitFunction(cursor, toplevel)
            return CXChildVisit_Continue
          else:
            discard

      return CXChildVisit_Recurse


when isMainModule:
  # block:
    # echo newPIdent("bool")


  try:
    main()
  except:
    pprintErr



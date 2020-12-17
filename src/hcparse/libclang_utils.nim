 ## Helper functions for libclang. This file should not be imported -
## it is `include`'d in the main `libclang` file.

when not defined(libclangIncludeUtils):
  import libclang


#==============================  includes  ===============================#
import bitops, strformat, macros, terminal, sugar, std/decls, strutils,
       sequtils, options, re, hashes, deques, sets, hashes, deques, tables
import gram, tables

import hpprint, hpprint/hpprint_repr
import hnimast
import nimtraits
import compiler/ast
import packages/docutils/rstast
import hmisc/types/[colorstring, hmap]
import hmisc/[hexceptions, helpers, hdebug_misc]
import hmisc/algo/hstring_algo
import hmisc/macros/[iflet]
import hmisc/other/[hshell, colorlogger, oswrap]

export CXCursor, CXString, CXType, write

#==========================  String conversion  ==========================#

proc `$`*(cxstr: CXString): string =
  let str = getCString(cxstr)
  result = $str
  disposeString(cxstr)

proc `$`*(cursor: CXCursor): string = $getCursorSpelling(cursor)
proc `$`*(cxtype: CXType): string =
  if cxtype.kind == tkInvalid:
    "<invalid>"
  else:
    $getTypeSpelling(cxtype)

proc `$`*(cxRange: CXSourceRange): string =
  &"[{cxRange.beginIntData}, {cxRange.endIntData}]"

proc `$`*(cxkind: CXCursorKind): string = $getCursorKindSpelling(cxkind)
proc `$`*(file: CXFile): string = $getFileName(file)

func validCxxIdentifier*(str: string): bool =
  if str[0] notin IdentStartChars:
    return false

  for ch in str:
    if ch notin IdentChars:
      return false

  return true

#*************************************************************************#
#*****************************  Destructors  *****************************#
#*************************************************************************#

when (NimMinor >= 3) and (NimPatch >= 5):
  proc `=destroy`*(tu: var CXTranslationUnit): void =
    echo "Destroying translation unit"
    disposeTranslationUnit(tu)

  proc `=destroy`*(index: var CXIndex): void =
    # echo "Destroying index"
    disposeIndex(index)

  proc `=destroy`*(dbase: var CXCompilationDatabase): void =
    dispose(dbase)

  proc `=dispose`*(commands: var CXCompileCommands): void =
    dispose(commands)



#*************************************************************************#
#***************  Translation unit construction wrappers  ****************#
#*************************************************************************#

#=============================  Diagnostics  =============================#


proc getDiagnostics*(unit: CXTranslationUnit): seq[CXDiagnostic] =
  for i in 0 ..< getNumDiagnostics(unit):
    result.add getDiagnostic(unit, i)

proc `$`*(diag: CXDiagnostic): string =
  $diag.formatDiagnostic(1)

#================================  Index  ================================#

proc createIndex*(
  excludeDeclarations: bool = false,
  showDiagnostics: bool = false): CXIndex =

  createIndex(excludeDeclarations.cint, showDiagnostics.cint)

#=====================  Compile commands & database  =====================#

proc createDatabase*(directory: string): CXCompilationDatabase =
  ## Create compilation database from database found in directory
  ## `directory`. On failure `IOError` is raised.
  var err: CXCompilationDatabase_Error # NOTE might use to provide
  # better diagnostics/exceptions

  result = compilationDatabase_fromDirectory(directory.cstring, addr err)
  if err == cdeCanNotLoadDatabase:
    raise newException(
      IOError,
      &"Failed to find compilation database in directory {directory}")

proc getArgs*(command: CXCompileCommand): seq[string] =
  ## Get arguments for compile command `command`
  for i in 0 ..< command.getNumArgs():
    result.add $command.getArg(i)

iterator items*(commands: CXCompileCommands): CXCompileCommand =
  for i in 0 ..< commands.getSize():
    yield commands.getCommand(i)

proc `[]`*(commands: CXCompileCommands, idx: int): CXCompileCommand =
  assert cuint(idx) < commands.getSize()
  return commands.getCommand(cuint idx)

proc absFile*(command: CXCompileCommand): AbsFile =
  AbsFile $command.getFilename()

#==========================  Translation unit  ===========================#

proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil



proc getBuiltinHeaders*(): seq[AbsDir] =
  ## According to clang `documentation <https://clang.llvm.org/docs/LibTooling.html#builtin-includes>`_
  ## libclang is needs additional precompiled headers paths in
  ## addition to default include.
  ##
  ## NOTE right now I have zero idea how it works on windows, so I
  ## will just hardcode unix-like paths.

  let version = ($getClangVersion()).split(" ")[2] # WARNING
  @[
    toAbsDir &"/usr/lib/clang/{version}/include"
  ]

proc parseTranslationUnit*(
    trIndex: CXIndex,
    filename: AbsFile,
    cmdline: seq[string] = @[],
    trOptions: set[CXTranslationUnit_Flags] = {tufSingleFileParse},
    reparseOnNil: bool = true
  ): CXTranslationUnit =

  filename.assertExists()

  let cmdline = getBuiltinHeaders().mapIt(&"-I{it}") & cmdline

  var flags: int
  for opt in trOptions:
    flags = bitor(flags, int(opt))

  block:
    let argc = cmdline.len
    let cmdlineC = allocCSTringArray(cmdline)

    result = parseTranslationUnit(
      trIndex, filename.cstring, cmdlineC, cint(argc), nil, 0, cuint(flags))
    deallocCStringArray(cmdlineC)

  var hadErrors = false
  for diag in result.getDiagnostics():
    if diag.getDiagnosticSeverity() in {dsError, dsFatal}:
      hadErrors = true
      echo ($diag).toRed()

  if hadErrors or (reparseOnNil and result.isNil):
    echo(&"""
Translation unit parse failed due to errors.
Compilation flags:
{cmdline.joinql()}
Input file:
  {filename.realpath}
      """)


    if reparseOnNil:
      echo "Translation unit parse failed, repeating parse in verbose mode"

      let cmdline = @["-v"] & cmdline
      let argc = cmdline.len
      let cmdlineC = allocCSTringArray(cmdline)

      result = parseTranslationUnit(
        trIndex, filename.cstring, cmdlinec, cint(argc), nil, 0, cuint(flags))

      deallocCStringArray(cmdlineC)

    raiseAssert("Translation unit parse failed")

proc getFlags*(command: CXCompileCommand): seq[string] =
  for arg in command.getArgs():
    if arg.startsWith("-"):
      result.add arg




proc parseTranslationUnit*(
    index: CXIndex,
    command: CXCompileCommand,
    extraFlags: seq[string] = @[],
    reparseOnNil: bool = true
  ): CXTranslationUnit =

  ## Get file and compilation flags from compilation command `command`
  ## and parse translation unit.
  ##
  ## ## Parameters
  ##
  ## :index: compilation index
  ## :command: Compilation command from database
  ## :extraFlags: Additional compilation flags for compiler. When parsing
  ##              c++ header files with `.h` extension you are most
  ##              likely need to use `@["-xc++"]` to make clang correctly
  ##              recognize the language.

  let args = extraFlags & getBuiltinHeaders().mapIt(&"-I{it}") &
    command.getFlags()

  let file = $command.getFilename()
  index.parseTranslationUnit(
    file.toAbsFile(true), args, {},
    reparseOnNIl = reparseOnNil
  )






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
  body: NimNode): NimNode =
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
    cursorId = ident "cursor"
    parentId = ident "parent"
    dataId = genSym(nskType, "Data")


  var procDecl = newNProcDecl(
    name = "visitor",
    args = procArgs,
    rtyp = some(returnType),
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



#*************************************************************************#
#****************  Convinience wrappers for clang types  *****************#
#*************************************************************************#




#=============================  Predicates  ==============================#


proc `==`*(t1, t2: CXType): bool = (equalTypes(t1, t2) != 0)
proc isConstQualified*(t: CXType): bool = isConstQualifiedType(t) != 0

proc `[]`*(t: CXType): CXType = getPointeeType(t)



#*************************************************************************#
#***************************  Cursor wrappers  ***************************#
#*************************************************************************#



#===========================  Kind/Type acess  ===========================#

# ~~~~ Cursor ~~~~ #

proc cxKind*(cursor: CXCursor): CXCursorKind =
  ## Get kind of cursor
  getCursorKind(cursor)

proc expectKind*(cursor: CXCursor, kind: CXCursorKind) =
  ## Raise assertion if cursor kind does not match
  if cursor.cxKind != kind:
    raiseAssert(&"Expected cursor kind {kind}, but got {cursor.cxKind()}")


proc expectKind*(cursor: CXCursor, kind: set[CXCursorKind]) =
  ## Raise assertion if cursor kind is not in set
  if cursor.cxKind notin kind:
    raiseAssert(&"Cursor kind in set: {kind}, but got {cursor.cxKind()}")


proc cxType*(cursor: CXCursor): CXType =
  ## Get type of the cursor
  getCursorType(cursor)

proc comment*(cursor: CXCursor): CXComment =
  cursor.getParsedComment()

#=============================  Predicates  ==============================#
proc isConstMethod*(cursor: CXCursor): bool =
  ## Return true if cursor is a class method with `const`
  ## qualification. No exception is raised on invalid method
  (cursor.kind in {ckMethod}) and (methodIsConst(cursor) == 0)

proc `==`*(c1, c2: CXCursor): bool = equalCursors(c1, c2) == 0


#=========================  Accessing subnodes  ==========================#

proc children*(cursor: CXCursor): seq[CXCursor] =
  ## Get sequence of subnodes for cursor
  var buf: seq[CXCursor]
  var ddd: int
  cursor.visitChildren do:
    makeVisitor [buf, ddd]:
      buf.add cursor
      return cvrContinue

  return buf

proc children*(cursor: CXCursor, kind: set[CXCursorKind]): seq[CXCursor] =
  for ch in cursor.children:
    if ch.cxKind in kind:
      result.add ch


iterator items*(cursor: CXCursor): CXCursor =
  for child in cursor.children():
    yield child

proc `[]`*(cursor: CXCursor, idx: int): CXCursor =
  ## Get `idx`'th subnode of cursor. If index is greater than number
  ## of available subnodes `IndexError` is raised.
  var
    res: CXCursor
    cnt = 0

  cursor.visitChildren do:
    makeVisitor [res, cnt, idx]:
      if cnt == idx:
        res = cursor
        return cvrBreak
      else:
        inc cnt
        return cvrContinue

  if cnt < idx:
    raise newException(IndexError,
                       "Cursor {cursor} no child at index {idx}")
  else:
    return res

proc len*(cursor: CXCursor): int =
  ## Get number of subnodes in cursor
  var cnt = 0
  cursor.visitChildren do:
    makeVisitor [cnt]:
      inc cnt
      return cvrContinue

  return cnt

proc tokens*(cursor: CXCursor, tu: CXTranslationUnit): seq[string] =
  ## Get sequence of tokens that make up cursor.
  let range: CXSourceRange   = getCursorExtent(cursor);
  var tokens: ptr[CXToken]
  var nTokens: cuint = 0
  assert not tu.isNil
  tokenize(tu, range, addr tokens, addr nTokens)

  for i in 0 ..< nTokens:
    result.add $getTokenSpelling(
      tu, (cast[ptr UncheckedArray[CXToken]](tokens))[i])

type
  DepResolutionKind* = enum
    drkIgnoreIfUsed
    drkWrapDirectly
    drkImportUses



proc visitMainFile*[T](
  cursor: CXCursor,
  callback: tuple[
    data: T,
    impl: proc(a, b: CXCursor,
               clientData: pointer): CXChildVisitResult {.cdecl.}
  ]) =
  ## Call visitor in each subnode if it is in main file
  var mainParent = cursor
  cursor.visitChildren do:
    makeVisitor [callback, mainParent, getDepResolutionKind]:
      if getDepResolutionKind(cursor) == drkWrapDirectly:
        return callback.impl(cursor, mainParent, addr callback.data)
      else:
        debug cursor
        return cvrRecurse

proc getFirstOfKind*(
  cursor: CXCursor,
  kindSet: set[CXCursorKind], recurse: bool = true): CXCursor =
  var
    found = false
    res: CXCursor

  cursor.visitChildren do:
    makeVisitor [found, res, kindSet, recurse]:
      if cursor.kind in kindSet:
        res = cursor
        found = true
        return cvrBreak
      else:
        if recurse:
          return cvrRecurse
        else:
          return cvrBreak

  if found:
    return res
  else:
    raise newException(
      KeyError,
      &"Could not find cursor of kind {kindSet} in subnodes")




#========================  Location information  =========================#


proc getFile*(tu: CXTranslationUnit): AbsFile =
  AbsFile($tu.getTranslationUnitSpelling())

proc getTuFile*(cx: CXCursor): AbsFile =
  assert cx.kind == ckTranslationUnit
  AbsFile($cx)

proc isFromMainFile*(cursor: CXCursor): bool =
  ## Return true if cursor posints to main file
  let location = cursor.getCursorLocation()
  result = location.locationIsFromMainFile() != cint(0)

proc isStatic*(cursor: CXCursor): bool =
  cursor.methodIsStatic() == 1

proc `$`*(loc: CXSourceLocation): string =
  var
    file: CXFile
    line: cuint
    column: cuint
    offset: cuint

  loc.getSpellingLocation(
    addr file, addr line, addr column, addr offset)

  return &"{file}:{line}:{column}"


proc getExpansionLocation*(location: CXSourceLocation): Option[tuple[
  file: AbsFile, line, column, offset: int]] =

  var
    file: CXFile
    line: cuint
    column: cuint
    offset: cuint

  location.getSpellingLocation(
    addr file, addr line, addr column, addr offset)

  if len($file.getFileName()) > 0:
    result = some((
      file: toAbsFile($file.getFileName(), true).realpath,
      line: line.int,
      column: column.int,
      offset: offset.int
    ))
    # result.file =  # WARNING set root?
    # result.file = result.file.
    # result.line =
    # result.column =
    # result.offset =


proc getSpellingLocation*(cursor: CXCursor): Option[tuple[
  file: AbsFile, line, column, offset: int]] =
  result = cursor.getCursorLocation().getExpansionLocation()
  # echov result.file

proc getCursorSemanticSiblings*(cursor: CXCursor): tuple[
  before, after: seq[CXCursor]] =

  var foundIt = false
  for curs in cursor.getCursorSemanticParent():
    if curs == cursor:
      foundIt = true
    else:
      if foundIt:
        result.after.add curs
      else:
        result.before.add curs


proc isSemanticAncestorOf*(ancestor, cursor: CXCursor): bool =
  var parent = cursor
  while true:
    parent = parent.getCursorSemanticParent()
    if parent == ancestor:
      return true
    elif parent.cxKind == ckTranslationUnit:
      return false

proc inheritsGenParamsOf*(cursor, ancestor: CXCursor): bool =
  # All arguments are semantic descendants of class
  # declaration, therefore more complicated logic is necessary
  # to check for type definition.
  if cursor.kind in {ckNoDeclFound}:
    false
  else:
    ancestor.isSemanticAncestorOf(
      cursor.cxType.getTypedeclaration())


#*************************************************************************#
#*******************  Documentation comment wrappers  ********************#
#*************************************************************************#
proc cxKind*(comment: CXComment): CXCommentKind = comment.getKind()

proc `$`*(comment: CXComment): string =
  ## Get original representation of the comment. NOTE: it seems like
  ## libclang API does not provide a way to get full comment string (I
  ## haven't found one yet) as raw string, so instead
  ## `fullCommentGetAsHTML` is returned.
  # TODO handle comments that are not 'full comment'
  $comment.fullCommentGetAsHTML()

iterator children*(comment: CXComment): CXComment =
  ## Get all comment subnodes
  for i in 0 .. getNumChildren(comment):
    yield comment.getChild(cuint(i))

proc len*(comment: CXComment): int =
  ## Get number of subnodes for comment
  getNumChildren(comment).int

proc `[]`*(comment: CXComment, idx: int): CXComment =
  ## Get `idx`'th subnode of comment
  assert idx < comment.len
  getChild(comment, cuint(idx))

proc objTreeRepr*(comment: CXComment): ObjTree =
  ## Convert comment to `ObjTree` repr
  case comment.cxKind:
    of cokText:
      pptConst $comment.textComment_getText()
    of cokParamCommand:
      pptObj(
        $comment.cxKind,
        [(
          $paramCommandComment_getParamName(comment),
          pptSeq(toSeq(comment.children).mapIt(it.objTreeRepr()))
          # objTreeRepr(comment[0])
        )]
      )
    of cokInlineCommand:
      let args: seq[string] = collect(newSeq):
        for i in 0 ..< inlineCommandComment_getNumArgs(comment):
          $inlineCommandComment_getArgText(comment, cuint(i))

      pptObj(
        $comment.cxKind,
        [(
          $inlineCommandComment_getCommandName(comment),
          pptSeq(
            args.mapIt(it.pptConst) &
            toSeq(comment.children).mapIt(it.objTreeRepr())
          )
        )]
      )
    of cokBlockCommand:
      pptObj(
        $comment.cxKind,
        [(
          $blockCommandComment_getCommandName(comment),
          pptSeq(toSeq(comment.children).mapIt(it.objTreeRepr()))
          # objTreeRepr(comment[0])
        )]
      )
    else:
      if comment.len == 0:
        pptObj($comment.cxKind,
               pptConst($comment.fullComment_getAsXML()))
      else:
        pptObj($comment.cxKind,
               toSeq(comment.children).mapIt(it.objTreeRepr()))

#==========================  Conversion to rst  ==========================#

proc newRstNode(
  kind: RstNodeKind, subnodes: varargs[PRstNode]): PRstNode =
  result = rstast.newRstNode(kind)
  for node in subnodes:
    result.add node

proc `$`(n: PRstNode): string = renderRstToRst(n, result)

proc toRstNode(comment: CXComment): PRstNode =
  case comment.cxKind:
    of cokText:
      return rnLeaf.newRstNode(
        $textComment_getText(comment))

    of cokFullComment:
      result = rnInner.newRstNode()
      for subnode in comment.children():
        result.add rnParagraph.newRstNode(subnode.toRstNode())

    of cokParagraph:
      result = rnInner.newRstNode()
      for subnode in comment.children():
        result.add rnParagraph.newRstNode(subnode.toRstNode())

    of cokNull:
      return rnLeaf.newRstNode("")

    of cokInlineCommand:
      return rnInner.newRstNode("")

    of cokParamCommand:
      let pn = $paramCommandComment_getParamName(comment)
      # echo "param: ", pn
      result = rnInner.newRstNode(
        @[ rnStrongEmphasis.newRstNode(rnLeaf.newRstNode(pn)) ] &
          toSeq(comment.children).mapIt(it.toRstNode())
      )

      # echo result

    of cokBlockCommand:
      return rnParagraph.newRstNode(
        @[
          rnEmphasis.newRstNode(
            rnInner.newRstNode(
              $blockCommandComment_getCommandName(comment)
            )
          )
        ] & toSeq(comment.children).mapIt(it.toRstNode()))

    of cokVerbatimBlockCommand:
      return rnCodeBlock.newRstNode(
        rnDirArg.newRstNode(""),
        rnFieldList.newRstNode(
          rnField.newRstNode(
            rnFieldName.newRstNode(rnLeaf.newRstNode("default-language")),
            rnFieldBody.newRstNode(rnLeaf.newRstNode("c++"))
          )
        ),
        rnLiteralBlock.newRstNode(
          rnLeaf.newRstNode(
            $verbatimBlockLineComment_getText(comment))))
    of cokVerbatimLine:
      return rnInterpretedText.newRstNode(
        $verbatimLineComment_getText(comment))
    else:
      echo "died".toRed()
      echo comment.objTreeRepr().pstring()
      raiseAssert("#[ IMPLEMENT ]#")

func dropEmptyLines*(str: string): string =
  ## Remove all empty lines from string
  str.split("\n").filterIt(not it.allOfIt(it in Whitespace)).join("\n")


func dropEmptyLines*(str: var string): void =
  ## Remove all empty lines from string
  str = str.split("\n").filterIt(
    not it.allOfIt(it in Whitespace)).join("\n")

proc toNimDoc*(comment: CXComment): string =
  ## Convert Comment to rst string
  # echo comment.objTreeRepr().pstring()
  comment.toRstNode().renderRstToRst(result)
  result.dropEmptyLines()




#*************************************************************************#
#****************************  Type wrappers  ****************************#
#*************************************************************************#
#==========================  Type declaration  ===========================#
const HeaderGraphFlags* = toInt({
  Directed, ValueIndex, UniqueEdges, UniqueNodes})

type
  CDeclKind* = enum
    cdkClass
    cdkStruct
    cdkEnum
    cdkFunction
    cdkMethod
    cdkField
    cdkAlias
    cdkMacro

  CArg* = object
    name*: string
    cursor*: CXCursor

  CXOperatorKind* = enum
    ## Classification for operators
    cxoPrefixOp ## Prefix operator `@a`
    cxoInfixOP ## Infix operator `a @ b`
    cxoAsgnOp ## Assign operator `a = b`
    cxoArrayOp ## Array access operator `a[b]`
    cxoArrowOp ## Arrow operator `a->`
    cxoCallOp ## Call operator `a()`
    cxoDerefOp ## Prefix dereference operator
    cxoCommaOp ## Comma operator
    cxoConvertOp ## User-defined conversion operator
    cxoUserLitOp ## User-defined literal operators

  IncludeDep* = object
    # TODO use it as edge value
    includedAs*: string
    includedPath*: AbsFile
    includedFrom*: AbsFile
    fromLine*: int
    fromColumn*: int
    fromOffset*: int



  CNamespace* = seq[NType[PNode]]
  CDecl* = object
    ## Higher-level wrapper on top of CXCursor. Mostly used to
    ## provide more intuitive API for working with things to be
    ## wrapped.
    name*: NType[PNode]
    genConstraints*: seq[CXCursor]

    namespace*: CNamespace
    cursor* # {.requiresinit.} # FIXME
    : CXCursor
    case kind*: CDeclKind
      of cdkField:
        fldAccs*: CX_AccessSpecifier
      of cdkMethod:
        metAccs*: CX_AccessSpecifier
        metArgs*: seq[CArg]
      of cdkFunction:
        funArgs*: seq[CArg]
      of cdkClass, cdkStruct:
        members*: seq[CDecl]
      of cdkEnum:
        flds*: seq[tuple[
          fldname: string,
          value: Option[CXCursor]
        ]]
      else:
        nil



  CApiUnit* = object
    ## Representation of single API unit - all public
    ## methods/classes/fields/functions declared in main file of
    ## single translation unit.
    ##
    ## ## Fields
    ## :decls: List of declarations in main file
    ## :publicTypes: List of public entries exposed by the API.
    ##
    ##     Class fields, function/method arguments/return values and
    ##     so on. This list allows you to determine whether or not
    ##     wrapping additional API unit & including them in main file
    ##     is necessary.

    # TODO infer 'derived' API that must also be acessible through
    # object - things like public fields and methods of parent class.
    decls*: seq[CDecl]
    publicAPI*: seq[CXCursor]
    includes*: seq[IncludeDep]

  ParsedFile* = object
    unit*: CXTranslationUnit ## Translation unit
    filename*: AbsFile ## Name of the original file
    api*: CApiUnit ## File's API
    index*: CXIndex
    explicitDeps*: seq[AbsFile] ## Filenames in which types exposed in
    ## API are declared. Guaranteed to have every file listed once &
    ## no self-dependencies.
    isExplicitlyAdded*: bool ## Filename has been explicitly listed in
    ## original files for wrapping, or was added as dependency
    ## (/transtitive dependency) for other file?

  HeaderDepGraph* = Graph[AbsFile, string, HeaderGraphFlags]

  ParseConfig* = object
    globalFlags*: seq[string] ## List of parse flags applied on each
    ## file parse. Mostly for things like include paths.
    fileFlags*: Table[AbsFile, seq[string]] ## List of parse flags
    ## specific only to particular file

    includepaths*: seq[AbsDir]

  FileIndex* = object
    index*: Table[AbsFile, ParsedFile] ## Index of all parsed files
    depGraph*: HeaderDepGraph

  WrapConfig* = object
    ## Configuration for wrapping. Mostly deals with type renaming
    header*: AbsFile ## Current main translation file (header)
    unit*: CXTranslationUnit
    makeHeader*: proc(cursor: CXCursor, conf: WrapConfig): PNode ## Genreate
    ## identifier for `{.header: ... .}`
    fixTypeName*: proc(ntype: var NType[PNode], conf: WrapConfig, idx: int)
    ## Change type name for `ntype`. Used to convert things like
    ## `boost::wave::macro_handling_exception::bad_include_file` into
    ## human-readable names.
    ##
    ## First argument is a type to be fixed, second one is parent
    ## configuration type. Third argument is mostly used for internal
    ## purposes - index of the generic argument. For cases like `[__T,
    ## _T]`, where both types should be mapped to `T` you can make `T` and
    ## `T1` respectively, using value provided by `idx`
    getImport*: proc(dep: AbsFile, conf: WrapConfig): seq[string] ## Generate
    ## import statement for header file dependency
    ignoreCursor*: proc(curs: CXCursor, conf: WrapConfig): bool ## User-defined
    ## predicate for determining whether or not cursor should be
    ## considered a part of api. Things like `internal` namespaces.
    collapsibleNamespaces*: seq[string]
    ignoreFile*: proc(file: AbsFile): bool
    isInternal*: proc(
      dep: AbsFile, conf: WrapConfig, index: FileIndex): bool ## Determine
    ## if particular dependency (`dep` file) should be re-exported.
    ## Note that this decision is not tied to particular file *from
    ## which* `dep` has been imported, but instead works the same way
    ## for all headers that depend on `dep`
    isTypeInternal*: proc(cxt: CXType, conf: WrapConfig): bool
    depResolver*: proc(cursor, referencedBy: CXCursor): DepResolutionKind

    isImportcpp*: bool
    parseConf*: ParseConfig

  WrapCache* = object
    hset: HashSet[Hash]
    visited: HashSet[cuint]

  WrappedEntry* = object
    case isMultitype*: bool
      of true:
        decls*: seq[WrappedEntry]
      of false:
        wrapped*: PNimDecl
        case isPassthrough*: bool
          of true:
            postTypes*: bool
          of false:
            original*: CDecl
            cursor*: CXCursor




import hnimast/pprint

func hasCursor*(we: WrappedEntry): bool = not we.isPassthrough

func `$`*(we: WrappedEntry): string = $we.wrapped
func `$`*(we: seq[WrappedEntry]): string =
  {.cast(noSideEffect).}:
    we.mapPairs(rhs.wrapped.toNNode().toPString()).join("\n")

func `==`*(a, b: WrappedEntry): bool =
  hnimast.`==`[Pnode](a.wrapped, b.wrapped)
  # (a.wrapped == b.wrapped)


func newWrappedEntry*(
    wrapped: PNimDecl, original: CDecl, source: CXCursor
  ): WrappedEntry =

  WrappedEntry(
    wrapped: wrapped,
    original: original,
    cursor: source,
    isPassthrough: false,
    isMultitype: false
  )


func newWrappedEntry*(wrapped: seq[WrappedEntry]): WrappedEntry =
  WrappedEntry(decls: wrapped, isMultitype: true)

func newWrappedEntry*(
    wrapped: PNimDecl, postTypes: bool = false
  ): WrappedEntry =

  WrappedEntry(wrapped: wrapped, isPassthrough: true,
               isMultitype: false, postTypes: postTypes)

#==========================  Helper utilities  ===========================#


proc declHash*(cursor: CXCursor): Hash =
  let loc = cursor.getSpellingLocation().get()
  return !$(
    hash(loc.file) !& hash(loc.line) !&
    hash(loc.column) !& hash(loc.offset))

proc markWrap*(cache: var WrapCache, cursor: CXCursor) =
  cache.hset.incl cursor.declHash()

proc canWrap*(cache: WrapCache, cursor: CXCursor): bool =
  cursor.declHash() notin cache.hset

proc markSeen*(cache: var WrapCache, cursor: CXCursor) =
  cache.visited.incl cursor.hashCursor()

proc seenCursor*(cache: WrapCache, cursor: CXCursor): bool =
  cursor.hashCursor() in cache.visited

proc cxKind*(cxtype: CXType): CXTypeKind =
  ## Get type kind
  cxtype.kind

proc expectKind*(cxtype: CXType, kind: CXTypeKind) =
  ## Raise assertion if cursor kind does not match
  if cxtype.cxKind != kind:
    raiseAssert(&"Expected type kind {kind}, but got {cxtype.cxKind()}")

proc argc*(cxtype: CXType): int =
  ## Get numbler of arguments in function type
  cxtype.expectKind(tkFunctionProto)
  getNumArgTypes(cxtype).int

proc params*(cursor: CXCursor): seq[CXCursor] =
  for ch in cursor:
    if ch.kind == ckParmDecl:
      result.add ch

proc argTypes*(cursor: CXType): seq[CXType] =
  for i in 0 ..< cursor.getNumArgTypes():
    result.add cursor.getArgType(cuint i)

proc retType*(cursor: CXCursor): CXType =
  cursor.expectKind({
    ckFunctionDecl,
    ckMethod,
    ckConversionFunction,
    ckFunctionTemplate,
    ckConstructor,
    ckDestructor
  })

  cursor.cxType().getResultType()

proc argc*(cursor: CXCursor): int =
  ## Get number of arguments in cursor pointing to function
  cursor.cxType().argc()

proc nthArg*(cxtype: CXType, idx: int): CXType =
  ## Retrieve type nth argument of function type
  assert idx < cxtype.argc()
  getArgType(cxtype, cuint(idx))


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
    else: result

func toPascalCase*(str: string): string =
  str.split("_").
    filterIt(it.len > 0).
    mapIt(it.capitalizeAscii()).
    join("")

proc fixTypeName*(str: string, idx: int, conf: WrapConfig): string =
  if str.len == 0:
    return "T" & $idx
  elif str in @[
    "bool", "cint", "cuint", "ptr", "void", "char",
    "cuchar", "cstring", "cchar", "uint32", "uint16",
    "culong", "clong", "cshort", "cushort", "array",
    "ushort",
  ]:
    return str
  else:
    let split = str.split("::")
    var idx = 0
    while idx < split.len:
      if (idx + 1 < split.len) and
         (split[idx] in conf.collapsibleNamespaces) and
         (split[idx + 1].normalize().startsWith(split[idx])):
        # For things like `sourcetrail::SourcetrailDBWrapper`
        discard
      else:
        result.add split[idx].toPascalCase()

      inc idx

proc fixStdTypeName*(head: string, idx: int): string =
  if head.len == 0:
    result = "T" & $idx
  else:
    let split = head.split("::")
    for name in split[0 .. ^1]:
      result &= name.toPascalCase()


proc fixTypeName*(
  ntype: var NType[PNode], conf: WrapConfig, idx: int = 0) =
  if ntype.kind in {ntkIdent, ntkGenericSpec}:
    ntype.head = fixTypeName(ntype.head, idx, conf)

    var idx = idx
    for gen in mitems(ntype.genParams):
      conf.fixTypeName(gen, conf, idx)
      inc idx

  else:
    if ntype.rtype.isSome():
      fixTypeName(ntype.rtype.get().getIt(), conf)

    for idx, arg in mpairs(ntype.arguments):
      arg.varname =
        if
          arg.varname.len == 0: "a" & $idx
        else:
          fixIdentName(arg.varname)

      conf.fixtypename(arg.vtype, conf, idx)


proc genParams*(cxtype: CXType): seq[CXType] =
  ## Get list of generic parameters for a type
  # DOC just any type or only generic instantiation?
  # TODO maybe mark generated parameters as defaulted/non-defaulted?
  let args = cxtype.getNumTemplateArguments()
  if args > 0:
    for i in 0 .. args:
      result.add cxtype.getTemplateArgumentAsType(i.cuint)



#===========================  Pretty-printing  ===========================#

proc objTreeRepr*(cxtype: CXType): ObjTree =
  case cxtype.cxKind:
    of tkPointer:
      pptObj("ptr", cxtype[].objTreeRepr())
    else:
      pptObj($cxtype.cxkind, pptConst($cxtype))


proc lispRepr*(cxtype: CXType): string =
  cxtype.objTreeRepr().lispRepr()


proc objTreeRepr*(
  cursor: CXCursor, tu: CXTranslationUnit,
  showtype: bool = true): ObjTree =
  ## Generate ObjTree representation of cursor
  const colorize = not defined(plainStdout)
  let ctype = pptConst(
    "type: " & $cursor.cxType,
    initPrintStyling(fg = fgBlue,
                     style = {styleItalic, styleDim}))

  if cursor.len  == 0:
    let val = pptconst(
      cursor.tokens(tu).join(" "), initprintstyling(fg = fggreen))
    var flds = if showtype: @[ctype, val] else: @[val]

    if cursor.cxKind in {ckMacroExpansion}:
      let cxRange =
        $getCursorLocation(cursor).getExpansionLocation() & " " &
          $getCursorExtent(cursor)

      flds.add pptconst(
        $cxRange, initprintstyling(fg = fgBlue))

    pptObj($cursor.cxkind, initPrintStyling(fg = fgYellow), flds)
  else:
    pptObj(
      ($cursor.cxkind).toMagenta(colorize) & " " & $cursor,
      showtype.tern(@[ctype], @[]) &
        toSeq(cursor.children).mapIt(it.objTreeRepr(tu, showtype))
    )


proc treeRepr*(cursor: CXCursor, tu: CXTranslationUnit,
               showtype: bool = true): string =
  ## Generate pretty-printed tree representation of cursor.
  cursor.objTreeRepr(tu, showtype).treeRepr()


proc lispRepr*(cursor: CXCursor, tu: CXTranslationUnit,
               showtype: bool = true): string =
  ## Generate pretty-printed tree representation of cursor.
  cursor.objTreeRepr(tu, showtype).lispRepr()

proc objTreeRepr*(cursor: CXCursor, showtype: bool = true): ObjTree =
  ## Generate ObjTree representation of cursor
  const colorize = not defined(plainStdout)
  let ctype = pptConst(
    "type: " & $cursor.cxType,
    initPrintStyling(fg = fgBlue,
                     style = {styleItalic, styleDim}))

  if cursor.len  == 0:
    if showType:
      pptObj($cursor.cxkind, initPrintStyling(fg = fgYellow),
             ctype, pptConst($cursor))
    else:
      pptConst($cursor.cxKind)
  else:
    pptObj(
      ("kind: " & $cursor.cxkind).toYellow(colorize) & " " & $cursor,
      showtype.tern(@[ctype], @[]) &
        toSeq(cursor.children).mapIt(it.objTreeRepr(showtype))
    )


proc treeRepr*(cursor: CXCursor, showtype: bool = true): string =
  ## Generate pretty-printed tree representation of cursor.
  cursor.objTreeRepr(showtype).treeRepr()


#===========================  Type conversion  ===========================#
proc getTypeName*(cxtype: CXType, conf: WrapConfig): string

proc toNType*(
  cxtype: CXType,
  conf: WrapConfig): tuple[ntype: NType[PNode], mutable: bool]

proc fromElaboratedPType*(cxtype: CXType, conf: WrapConfig): NType[PNode] =
  # debug cxtype
  let genParams = cxtype.getNumTemplateArguments()
  let decl = cxtype.getTypeDeclaration()
  if genParams > 0:
    case decl.cxKind:
      of ckTypedefDecl:
        # WARNING `template <J, Q> using` is not handled
        result = newPType(cxtype.getTypeName(conf))
      of ckClassDecl, ckStructDecl:
        debug "Class decl"
        let params = cxtype.genParams()
        result = newPType(cxtype.getTypeName(conf))
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add parm.toNType(conf).ntype

      else:
        warn "Conversion from elaborated type: ", decl
        debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

    conf.fixTypeName(result, conf, 0)

  else:
    result = newPType(getTypeName(cxtype, conf))
    # result = ($cxtype).
    #   dropPrefix("enum ").
    #   dropPrefix("struct ").
    #   dropPrefix("union ").
    #   dropPrefix("const "). # WARNING
    #   newPType()

  # debug "Result form elaboreated type", result.toNNode()


proc dropPOD*(cxtype: CXType, conf: WrapConfig): string =
  case cxtype.cxKind:
    of tkElaborated:
      cxtype.fromElaboratedPType(conf).head
    of tkPointer:
      cxtype[].dropPOD(conf)
    of tkTypedef:
      ($cxtype).dropPrefix("const ")
    else:
      ""

proc toPIdentDefs*(cursor: CXCursor, conf: WrapConfig): PIdentDefs =
  result.varname = $cursor
  if result.varname.len == 0:
    result.varname = "arg" & $cursor.cxType().dropPOD(conf)

  result.varname = result.varname.fixIdentName()

  let (ctype, mutable) = cursor.cxType().toNType(conf)
  result.vtype = ctype
  if mutable:
    result.kind = nvdVar


proc getSemanticNamespaces*(
    parent: CXCursor, filterInline: bool = true, withType: bool = true
  ): seq[CXCursor] =

  # info "Semantic namespaces for", parent

  var parent = parent

  if withType:
    result.add parent

  parent = parent.getCursorSemanticParent()

  # info parent

  while parent.cxKind() in {
    # TEST might be necessary to add templated namespacess (fuck, why C++
    # is just so god-awful vomit-inducing garbage?)
    ckNamespace, ckStructDecl, ckClassDecl
  }:
    if filterInline and (parent.isInlineNamespace() == 1):
      discard
    else:
      result.add parent

    parent = parent.getCursorSemanticParent()
    # info parent.cxKind()

  reverse(result)



proc getTypeNamespaces*(
    cxtype: CXType, filterInline: bool = true, withType: bool = true
  ): seq[CXCursor] =
  ## Return list of parent namespaces for given type `cxtype`.
  ## `filterInline` - remove namespaces that are marked as `inline`.
  ## `withType` - return type name too, or only namespaces.

  var parent = cxtype.getTypeDeclaration()

  return getSemanticNamespaces(
    parent, filterInline =  filterInline, withType = withType)

proc getTypeName*(cxtype: CXType, conf: WrapConfig): string =
  let curs = cxtype.getTypeDeclaration()
  case curs.cxKind:
    of ckTypedefDecl:
      return $curs.cxType()
    of ckClassDecl, ckStructDecl, ckEnumDecl, ckUnionDecl:
      result = $curs
    else:
      err $curs
      err "Type name for ", curs.treeRepr(conf.unit)
      raiseAssert(
        &"Cannot convert cursor of kind {curs.cxKind} to type")

  result = cxtype.getTypeNamespaces().mapIt(
    dropPrefix($it, toStrPart(["const ", "enum ", "struct ", "union "]))
  ).join("::")

  # debug cxtype.getTypeNamespaces()
  # debug result

proc isMutableRef*(cxtype: CXType): bool =
  case cxType.cxKind:
    of tkLValueReference, tkRValueReference:
      return not (cxType.isConstQualifiedType() == 0)
    of tkTypeDef:
      # TODO implement mutability checking
      let decl = cxtype.getTypeDeclaration()
      if decl.len == 1 and decl[0].cxKind == ckTypeRef:
        discard
    else:
      raiseAssert(&"#[ IMPLEMENT Is {cxtype.cxKind} a mutable ref? ]#")

proc toNType*(
  cxtype: CXType,
  conf: WrapConfig): tuple[ntype: NType[PNode], mutable: bool] =
  ## Convert CXType to nim type. Due to differences in how mutability
  ## handled in nim and C it is not entirely possible to map `CXType`
  ## to `NType` without losing this information. Instead `mutable` is
  ## returned, indicating whether or not the type was mutable.
  ## Conversion is performed as follows
  ##
  ## - `T&` is considered mutable and mapped to `var T`
  ## - Any kind of pointer is mapped to immutable since it is not possible
  ##   infer this information from C type anyway.
  ## - Function prototype is mapped to `{.cdecl.}` proc type
  ## - 'special' types are mapped
  ##   - `char*` -> `cstring`
  ##   - `char**` -> `cstringArray`
  ##   - `void*` -> `pointer`
  ## - For C types with elaborated specifier (e.g. `enum E` instead of
  ##   simply `E`) specifiers are simply dropped.
  var mutable: bool = false
  let restype = case cxtype.cxKind:
    of tkBool:       newPType("bool")
    of tkInt:        newPType("cint")
    of tkVoid:       newPType("void")
    of tkUInt:       newPType("cuint")
    of tkLongLong:   newPType("clonglong")
    of tkULongLong:  newPType("culonglong")
    of tkDouble:     newPType("cdouble")
    of tkULong:      newPType("culong")
    of tkUChar:      newPType("cuchar")
    of tkChar16:     newPType("uint16") # WARNING C++ type is `char16_t`
    of tkChar32:     newPType("uint32") # WARNING C++ type is `char32_t`
    of tkWChar:      newPType("uint32") # WARNING C++ type is `wchar_t`
    of tkChar_S:     newPType("cchar")
    of tkLong:       newPType("clong")
    of tkUShort:     newPType("cushort")
    of tkNullPtr:    newPType("pointer") # WARNING C++ type is `nullptr_t`
    of tkFloat:      newPType("cfloat")
    of tkLongDouble: newPType("clongdouble")
    of tkShort:      newPType("cshort")
    of tkSChar:      newPType("cschar")
    of tkTypedef:
      result.mutable = cxType.isMutableRef()
      newPType(($cxtype).dropPrefix("const ")) # XXXX typedef processing -

    of tkElaborated, tkRecord, tkEnum:
      # debug "From elaborated type"
      fromElaboratedPType(cxtype, conf)

    of tkPointer:
      case cxtype[].cxkind:
        of tkChar_S:
          newPType("cstring")
        of tkPointer:
          if cxtype[][].cxKind() == tkChar_S:
            newPType("cstringArray")
          else:
            newNType("ptr", [toNType(cxtype[], conf).ntype])
        of tkVoid:
          newPType("pointer")
        of tkFunctionProto:
          toNType(cxtype[], conf).ntype
        else:
          newNType("ptr", [toNType(cxtype[], conf).ntype])
    of tkConstantArray:
      newNType(
        "array",
        @[
          newPType($cxtype.getNumElements()),
          toNType(cxtype.getElementType(), conf).ntype
        ]
      )
    of tkFunctionProto:
      newProcNType[PNode](
        rtype = cxtype.getResultType().toNType(conf).ntype,
        args = cxtype.argTypes.mapIt(toNType(it, conf).ntype),
        pragma = newPPragma("cdecl")
      )
    of tkLValueReference:
      result.mutable = cxType.isMutableRef()
      toNType(cxType[], conf).ntype
    of tkRValueReference: # WARNING I'm not 100% sure this is correct
                          # way to map rvalue references to nim type
                          # system.
      result.mutable = cxType.isMutableRef()
      toNType(cxType[], conf).ntype
    of tkUnexposed:
      let strval = ($cxType).dropPrefix("const ") # WARNING
      if strval.validCxxIdentifier():
        newPtype(strval)
      else:
        # pprintStackTrace()
        let decl = cxtype.getTypeDeclaration()
        var res = newPType($decl)
        if decl.cxKind in {
          # HACK list of necessary kinds is determined by trial and error,
          # I'm still not really sure what `tkUnexposed` actually
          # represents.
          ckClassTemplate, ckClassDecl
        }:
          for elem in decl:
            if elem.cxKind() in {ckTemplateTypeParameter}:
              let (sub, _) = elem.cxType().toNType(conf)
              res.add sub

          # info decl
          # debug res.toNNode()
        else:
          # debug decl.cxKind()
          res = newPType("UNEXPOSED")
          # warn strval, "is not a valid identifier for type name, use UNEXPOSED"

          if decl.cxKind() notin {ckNoDeclFound}:
            warn "No decl found for type"
            logIndented:
              info cxtype.lispRepr()
              debug decl.getSpellingLocation()
              debug decl.cxKind()
              debug decl.treeRepr(conf.unit)


        res
    of tkDependent: newPType("DEPENDENT")
    of tkMemberPointer:
      # WARNING Member pointer
      newPType("!!!")
    else:
      err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newPType("!!!")

  result.ntype = restype
  conf.fixTypeName(result.ntype, conf, 0)
  result.mutable = mutable

func hasUnexposed*(nt: NType[PNode]): bool =
  case nt.kind:
    of ntkIdent, ntkGenericSpec:
      nt.head in [ "UNEXPOSED", "DEPENDENT" ] or
      nt.genParams.anyOfIt(it.hasUnexposed())
    of ntkProc:
      nt.arguments.anyOfIt(it.vtype.hasUnexposed())
    else:
      false







#*************************************************************************#
#***********************  Declaration conversion  ************************#
#*************************************************************************#
proc accs(self: CDecl): CX_AccessSpecifier =
  if contains({cdkField}, self.kind):
    return self.fldAccs
  if contains({cdkMethod}, self.kind):
    return self.metAccs
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `accs=`(self: var CDecl; it: CX_AccessSpecifier) =
  var matched: bool = false
  if contains({cdkField}, self.kind):
    if true:
      matched = true
      self.fldAccs = it
  if contains({cdkMethod}, self.kind):
    if true:
      matched = true
      self.metAccs = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc args(self: CDecl): seq[CArg] =
  if contains({cdkMethod}, self.kind):
    return self.metArgs
  if contains({cdkFunction}, self.kind):
    return self.funArgs
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `args=`(self: var CDecl; it: seq[CArg]) =
  var matched: bool = false
  if contains({cdkMethod}, self.kind):
    if true:
      matched = true
      self.metArgs = it
  if contains({cdkFunction}, self.kind):
    if true:
      matched = true
      self.funArgs = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

#======================  Accessing CDecl elements  =======================#
func arg*(cd: CDecl, idx: int): CArg = cd.args()[idx]
func member*(cd: CDecl, idx: int): CDecl = cd.members[idx]
func methods*(cd: CDecl, kinds: set[CXCursorKind]): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for member in cd.members:
    if (member.kind == cdkMethod) and (member.cursor.cxKind in kinds):
      result.add member

func toCppImport*(ns: CNamespace): string =
  var buf: seq[string]
  var genIdx: int = 0
  for part in ns:
    if part.genParams.len > 0:
      var genTypes: seq[string]
      for param in part.genParams:
        genTypes.add "'" & $genIdx
        inc genIdx

      buf.add part.head & "<" & genTypes.join(", ") & ">"
    else:
      buf.add part.head

  result = buf.join("::")

func toNType*(ns: CNamespace): NType[PNode] =
  var nameBuf: seq[string]
  for part in ns:
    result.add part.genParams
    nameBuf.add part.head

  result.head = nameBuf.join("::")


func inNamespace*(cd: CDecl, ns: CNamespace): NType[PNode] =
  var nameBuf: seq[string]

  for n in ns & @[ cd.name ]:
    result.add n.genParams
    nameBuf.add n.head

  result.head = nameBuf.join("::")

func inNamespace*(cd: CDecl, ns: CDecl): NType[PNode] =
  cd.inNamespace(ns.namespace & @[ ns.name ])

func pubFields*(cd: CDecl): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for member in cd.members:
    if (member.kind == cdkField) and (member.accs == asPublic):
      result.add member

func namespaceName*(cd: CDecl): string =
  (cd.namespace & @[cd.name]).toCppImport()

func isOperator*(cd: CDecl): bool =
  cd.kind in {cdkMethod, cdkFunction} and
  cd.name.head.startsWith("operator") and
  (not cd.name.head.validIdentifier())

# func isOperator
proc isOperator*(cx: CXCursor): bool =
  ($cx).startsWith("operator") and
  (not ($cx).validIdentifier())


proc classifyOperator*(cd: CDecl): CXOperatorKind =
  assert cd.isOperator()
  let name = cd.name.head.dropPrefix("operator")
  case name:
    of "+=", "=", "-=", "*=",
       "<<=", ">>=", "&=", "|=", "/=", "%=", "^="
      : # NOTE `=`
      cxoAsgnOp

    of "[]":
      cxoArrayOp

    of "-", "+", "/",
       "++", "--", # NOTE this is an operator implementation, so we are
                  # not (i hope) dropping information about
                  # prefi/postfix calls
       "<<", ">>", "==", "!=", "&&", "||",
       "%", "^", "&", "|", "<", ">", "<=", ">="
         :
      cxoInfixOp

    of "*": # NOTE this heuristics might not be valid in all cases.
      if cd.args.len == 0:
        cxoDerefOp
      else:
        cxoInfixOp

    of "->", "->*":
      cxoArrowOp

    of "()":
      cxoCallOp

    of "~", "!":
      cxoPrefixOp

    of ",":
      cxoCommaOp

    else:
      if cd.cursor.cxKind() == ckConversionFunction:
        cxoConvertOp

      elif (cd.cursor.cxKind() in {
        ckFunctionDecl, ckFunctionTemplate
      }) and (name.startsWith("\"\"")):
        cxoUserLitOp

      else:
        raiseAssert(
          &"#[ IMPLEMENT '{name}', {cd.cursor.cxKind()} ]#" &
            $cd.cursor.getSpellingLocation()
        )


func getNimName*(cd: CDecl): string =
  case cd.kind:
    of cdkMethod, cdkFunction:
      if cd.isOperator():
        if cd.name.head == "operator=":
          "setFrom" # REVIEW change name to something different if possible
        else:
          cd.name.head.dropPrefix("operator")
      else:
        cd.name.head
    else:
      cd.name.head


#======================  Converting to nim entries  ======================#

proc convertCFunction*(cursor: CXCursor, conf: WrapConfig): ProcDecl[PNode] =
  cursor.expectKind(ckFunctionDecl)
  var prevName = ""
  result = ProcDecl[PNode]().withIt do:
    it.iinfo = currIInfo()
    it.exported = true
    # WARNING temporarily disabled comment processing
    # it.comment = cursor.comment().toNimDoc()
    it.name = ($cursor).fixIdentName()
    it.signature = newProcNType[PNode](@[])
    it.signature.arguments = collect(newSeq):
      for it in cursor.children:
        if it.cxKind == ckParmDecl:
          var res = it.toPIdentDefs(conf)
          if res.varname == prevName:
            res.varname &= "1"

          prevName = res.varname
          res

    # NOTE dropping mutability from return type
    it.signature.setRType toNType(cursor.retType(), conf).ntype
    # WARNING temprarily disabled pragma annotations
    # it.signature.pragma = newPPragma()

#===================  Accessible public API elements  ====================#
proc getPublicAPI*(cd: CDecl): seq[CXCursor] =
  ## Get list of cursors referring to parts of the public API for a
  ## declaration: return and argument types for functions and methods,
  ## public fields for objects.
  case cd.kind:
    of cdkClass, cdkStruct:
      for member in cd.members:
        result.add member.getPublicAPI()
    of cdkField:
      if cd.accs == asPublic:
        return @[ cd.cursor ]
      else:
        return @[]
    of cdkFunction, cdkMethod:
      let exportd: bool = (cd.kind == cdkFunction) or
        (cd.accs() == asPublic)

      if exportd:
        result.add cd.cursor
        for arg in cd.args:
          result.add arg.cursor
    of cdkAlias:
      return @[cd.cursor]
    of cdkEnum:
      return @[]
    of cdkMacro:
      return @[]


#*************************************************************************#
#*********************  Translation unit conversion  *********************#
#*************************************************************************#
#==========================  Type declarations  ==========================#



#===========  Splitting translation unit into logical chunks  ============#
proc getArguments(cursor: CXCursor): seq[CArg] =
  for idx, subn in cursor.children():
    if subn.cxKind in {ckParmDecl}:
      var name = $subn
      if name.len == 0:
        name = "a" & $idx

      result.add CArg(name: name, cursor: subn)

proc visitMethod(cursor: CXCursor, accs: CX_AccessSpecifier): CDecl =
  result = CDecl(kind: cdkMethod, cursor: cursor, name: newPType $cursor)
  result.accs = accs
  result.args = cursor.getArguments()


proc visitField(cursor: CXCursor, accs: CX_AccessSpecifier): CDecl =
  result = CDecl(kind: cdkField, cursor: cursor, name: newPType $cursor)
  result.accs = accs

var undefCnt: int = 0

proc visitAlias*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkAlias, cursor: cursor, namespace: parent)
  result.name = newPType $cursor

proc visitFunction(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkFunction, cursor: cursor, namespace: parent)
  result.name = newPType $cursor
  result.args = cursor.getArguments()

  for subn in cursor:
    case subn.cxKind:
      of ckTemplateTypeParameter:
        result.name.add newPType($subn)
      of ckNonTypeTemplateParameter, ckTemplateRef:
        result.genConstraints.add subn
      of ckParmDecl, ckTypeRef, ckNamespaceRef,
         ckFirstExpr, ckCompoundStmt, ckMemberRef,
         ckFirstAttr, ckVisibilityAttr
           :
         # WARNING right now these things are just droppped. Maybe it
         # will cause some errors in the future, I don't really know.
        discard
      else:
        warn "Unknown element", subn.cxKind, cast[int](subn.cxKind),
                subn, "in\n" & $cursor


proc visitEnum(
  cursor: CXcursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkEnum, cursor: cursor,
                 namespace: parent,
                 name: newPType(($cursor).dropPrefix("enum ")))

  for elem in cursor:
    result.flds.add ($elem, some(elem[0]))

  # info "Found enum", result.name


proc requiredGenericParams(cursor: CXCursor): seq[NType[PNode]] =
  ## Get list of required generic parameters from cursor pointing to
  ## class or struct declaration
  for subn in cursor:
    if subn.cxKind in {ckTemplateTemplateParameter,
                        ckTemplateTypeParameter}:
      if subn.len > 0:
        # WARNING Just drop all template parameters that are not
        # simply `T`.
        discard
      else:
        result.add newPType $subn # WARNING blow up on `a<b>`



proc visitClass(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  ## Convert class under cursor to `CDecl`
  cursor.expectKind({
    ckClassDecl, ckClassTemplate, ckClassTemplatePartialSpecialization,
    ckStructDecl, ckUnionDecl
  })

  let name =
    if cursor.cxKind == ckStructDecl and len($cursor) == 0:
      $cursor.cxType()
    else:
      $cursor

  result = CDecl(kind: cdkClass, cursor: cursor, name: newPType(name))
  result.namespace = parent
  result.name.add cursor.requiredGenericParams()

  identLog()
  defer: dedentLog()

  var currentAccs =
    if cursor.cxKind in {ckStructDecl, ckUnionDecl}:
      asPublic
    else:
      asPrivate


  # debug cursor.treeRepr()

  for subn in cursor:
    if subn.cxKind == ckAccessSpecifier:
      currentAccs = subn.getAccessSpecifier()

    if currentAccs == asPublic:
      case subn.cxKind:
        of ckMethod, ckConversionFunction:
          result.members.add visitMethod(subn, currentAccs)
        of ckConstructor:
          var res = visitMethod(subn, currentAccs)
          res.name.head = result.name.head
          result.members.add res
        of ckAccessSpecifier:
          currentAccs = subn.getAccessSpecifier()
        of ckFieldDecl,
           ckVarDecl # WARNING static fields might need to be wrapped differently
             :

          if not conf.ignoreCursor(subn, conf):
            # debug subn.cxType().getTypeDeclaration()
            result.members.add visitField(subn, currentAccs)

        of ckTemplateTypeParameter, ckFriendDecl,
           ckStaticAssert, ckTemplateTemplateParameter:
          discard
        of ckDestructor:
          # QUESTION WARNING I think using `destroy=` hook from nim side is
          # unnecessary, but still need to verify this later.
          var res = visitMethod(subn, currentAccs)
          res.name.head = result.name.head
          result.members.add res
        of ckFunctionTemplate:
          result.members.add visitFunction(
            subn, parent & @[ result.name ], conf)
        of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
           ckTypedefDecl, ckUsingDeclaration:
          if not conf.ignoreCursor(subn, conf):
            result.members.add visitAlias(
              subn, parent & @[result.name], conf)
        of ckStructDecl, ckClassDecl, ckUnionDecl, ckEnumDecl,
           ckClassTemplate
             :
          # NOTE nested structs will be handled during object wrapping and
          # moved into separate type declarations
          discard
        of ckBaseSpecifier:
          # Base class specifier is ignored. All additional wrapping
          # operations should happend afterwards.
          discard
        of ckVisibilityAttr:
          # Visibility attributes are ignored for now
          discard
        else:
          inc undefCnt
          if undefCnt > 20:
            raiseAssert("Reached unknown class element limit")
          else:
            discard
            warn "IMPLEMENT class element:", ($subn.cxKind).toRed(), subn
            # debug subn.treeRepr()



proc visitCursor(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): tuple[
  decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]]

proc visitNamespace(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.

  let namespace =
    if not (cursor.isInlineNamespace() == 1):
      @[ newPType($cursor) ]
    else:
      @[]

  if not conf.ignoreCursor(cursor, conf):
    for subn in cursor:
      result.add visitCursor(subn, parent & namespace, conf).decls

proc visitMacrodef(cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  CDecl(cursor: cursor, kind: cdkMacro)

proc visitCursor(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): tuple[
  decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]] =

  # debug cursor.cxKind()
  # debug $cursor.cxType()
  # debug cursor.getCursorSemanticParent().cxKind()
  # debug cursor.getCursorLexicalParent().cxKind()
  if not conf.ignoreCursor(cursor, conf):
    case cursor.cxKind:
      of ckNamespace:
        result.decls.add visitNamespace(cursor, parent, conf)
      of ckClassDecl, ckClassTemplate,
         ckClassTemplatePartialSpecialization,
         ckStructDecl:
        result.decls.add visitClass(cursor, parent, conf)
      of ckFunctionDecl, ckFunctionTemplate:
        result.decls.add visitFunction(cursor, parent, conf)
      of ckTypedefDecl, ckTypeAliasDecl:
        # debug cursor.treeRepr()
        result.decls.add visitAlias(cursor, parent, conf)
      of ckEnumDecl:
        result.decls.add visitEnum(cursor, parent, conf)
      of ckInclusionDirective:
        let loc = cursor.getSpellingLocation().get()
        result.includes.add IncludeDep(
          includedAs: $cursor,
          includedFrom: loc.file,
          fromLine: loc.line,
          fromColumn: loc.column,
          fromOffset: loc.offset,
          includedPath: AbsFile($cursor.getIncludedFile()).realpath
        )
      of ckMacroDefinition:
        result.decls.add visitMacrodef(cursor, parent, conf)
      else:
        # warn "Recursing on", cursor, "of kind", cursor.cxKind()
        result.recurse = true


proc splitDeclarations*(
  tu: CXTranslationUnit, conf: WrapConfig): CApiUnit =
  ## Convert main file of translation unit into flattened sequence of
  ## high-level declarations. All cursors for objects/structs are
  ## retained. Public API elements are stored in `publicAPI` field
  assert not tu.isNil
  let tuCursor = tu.getTranslationUnitCursor()
  var res: CApiUnit
  tuCursor.visitChildren do:
    makeVisitor [tu, res, conf, tuCursor]:
      let resolve = conf.depResolver(cursor, tuCursor)
      if resolve == drkWrapDirectly:
        let (decls, rec, incls) = visitCursor(cursor, @[], conf)
        res.includes.add incls
        if rec:
          return cvrRecurse
        else:
          res.decls.add decls
          for decl in decls:
            res.publicAPI.add decl.getPublicAPI()

          return cvrContinue
      else:
        return cvrRecurse

  return res

#=====================  Dependency list generation  ======================#
# ~~~~ CLI helper path resolution ~~~~ #
proc getHCParseBinDir*(): AbsDir =
  ## Return absolute path `/bin` directory with helper cmdline tools;
  ## NOTE right now I have no idea how to handle dependencies like
  ## this - this is just a hacky solution.
  for dir in AbsDir(currentSourcePath()).parentDirs():
    if (dir /. "hcparse.nimble").fileExists():
      return dir / "bin"

  raise newException(
    IOError,
    "Could not find `hcparse.nimble` in any of the parent directories")

proc getHCParseBinPath*(name: string): AbsFile =
  ## Return absolute name of the helper cmdline tool `name`
  let bindir = getHCParseBinDir()
  let file = bindir /. name
  if fileExists(file):
    return file
  else:
    raise newException(IOError, "Could not find '" & $file & "'")

# ~~~~ Dependency tree construction ~~~~ #

type
  CDepsTree* = object
    file*: AbsFile
    name*: string
    deps*: seq[CDepsTree]

proc parseBuildDepsTree*(outs: string): CDepsTree =
  let depLines = outs.split("\n")
  var idx = 0
  info "deps list has ", depLines.len,  " lines"

  proc auxTree(): seq[CDepsTree] {.closure.} =
    while not depLines[idx].startsWith(Whitespace, "}"):
      if deplines[idx] =~ re".*?<(.*?)> (.*)":
        inc idx
        result.add CDepsTree(name: matches[0], file: matches[1].toAbsFile)
      elif deplines[idx] =~ re""".*?"(.*?)" (.*)""":
        inc idx
        result.add CDepsTree(name: matches[0], file: matches[1].toAbsFile)
      elif depLines[idx].startsWith(Whitespace, "{"):
        inc idx
        # echo "startin level ", depLines[idx]
        result.last.deps = auxTree()
      elif depLines[idx].isEmptyOrWhitespace():
        inc idx
        return

    inc idx


  return CDepsTree(deps: auxTree())

proc buildDepsTree*(file: AbsFile, args: seq[string]): CDepsTree =
  let bin = getHCParseBinPath("deps")
  assert file.fileExists()

  try:
    let (outs, _, _) = runShell makeGnuShellCmd($bin).withIt do:
      it.raw args.joinw()
      it.arg file

    result = parseBuildDepsTree(outs)
    result.file = file


  except ShellError:
    printShellError()
    echo "Arguments:"
    echo args.joinql()


proc immediateDeps*(d: CDepsTree): seq[AbsFile] =
  d.deps.mapIt(it.file)

# ~~~~ Collecting dependeny list ~~~~ #


proc getDepFiles*(deps: seq[CXCursor], conf: WrapConfig): seq[AbsFile]

proc isDependency(cursor: CXCursor, conf: WrapConfig): bool =
  return conf.depResolver(
    cursor, conf.unit.getTranslationUnitCursor()) == drkImportUses




proc getDepFiles*(cxtype: CXType, conf: WrapConfig): seq[AbsFile] =
  let decl = cxtype.getTypeDeclaration()
  for parm in cxtype.genParams():
    if parm.cxKind != tkInvalid:
      for file in getDepFiles(parm, conf):
        result.add file

  ignorePathErrors {pekInvalidEntry}:
    if decl.kind notin {ckNoDeclFound}:
      if decl.isDependency(conf):
        let (file, _, _, _) = decl.getSpellingLocation().get()
        result.add file

  for file in result:
    assertExists(file)

proc isInternalImpl*(
  dep: AbsFile, conf: WrapConfig, index: FileIndex): bool =
  return not index.index[dep].isExplicitlyAdded

proc getDepFiles*(deps: seq[CXCursor], conf: WrapConfig): seq[AbsFile] =
  ## Generate list of files that have to be wrapped
  # assert conf.unit.getTranslationUnitCursor().cxKind == ckTranslationUnit

  for dep in deps:
    var decl: (CXCursor, bool)
    case dep.cxKind:
      of ckFunctionDecl, ckMethod, ckConversionFunction:
        result.add getDepFiles(dep.params(), conf).withIt do:
          for file in it:
            assertExists file

        decl = (dep.retType().getTypeDeclaration(), true)

      of ckFunctionTemplate:
        result.add dep.retType().getDepFiles(conf).withIt do:
          for file in it:
            assertExists file

      of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
         ckTypedefDecl, ckUsingDeclaration:

        decl = (
          dep.cxType().getCanonicalType().getTypeDeclaration(),
          true
        )

      of ckParmDecl:
        var cxt = dep.cxType()

        let subn = dep.children()
        var typeRef = false

        for sub in subn:
          case sub.cxKind:
            of ckNamespaceRef:
              discard
            of ckTypeRef:
              cxt = sub.cxType()
              typeRef = true
            else:
              break


        if not typeRef and (cxt.cxKind() notin {tkInt}):
          for parm in cxt.genParams():
            if parm.cxKind != tkInvalid:
              result.add getDepFiles(parm, conf).withIt do:
                for file in it:
                  assertExists file


        decl = (cxt.getTypeDeclaration(), true)
      else:
        # warn "dep for:", dep.cxKind(), dep, dep.cxType()
        decl = (dep.cxType.getTypeDeclaration(), true)

    if decl[1]:
      ignorePathErrors {pekInvalidEntry}:
        # WARNING ignore invalid `#include`
        if decl[0].cxKind() notin {ckNoDeclFound}:
          if decl[0].isDependency(conf):
            let (file, line, column, _) = decl[0].getSpellingLocation().get()
            assertExists(file)
            result.add file


  result = result.deduplicate().
    filterIt(it.len > 0 and it.hasExt()).
    mapIt(it.realpath())



#*************************************************************************#
#*************************  Wrapper generation  **************************#
#*************************************************************************#
proc wrapOperator*(
    oper: CDecl,
    genParams: seq[NType[PNode]],
    conf: WrapConfig
  ): tuple[decl: PProcDecl, addThis: bool] =

  var it = PProcDecl()

  it.iinfo = currIInfo()
  it.signature = newProcNType[PNode](@[])
  it.name = oper.getNimName()
  it.genParams = genParams
  it.exported = true

  assert conf.isImportcpp
  let kind = oper.classifyOperator()
  it.kind = pkOperator

  # debug "Operrator class", kind
  # debug it.name
  if kind == cxoAsgnOp and it.name == "setFrom":
    it.signature.pragma = newPPragma(
      newPIdentColonString("importcpp", &"# = #"))

    it.kind = pkRegular
  else:
    case kind:
      of cxoAsgnOp:
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"# {it.name} #"))

      of cxoArrayOp:
        let rtype = oper.cursor.retType()
        let (_, mutable) = rtype.toNType(conf)
        if mutable:
          it.name = "[]="
          # WARNING potential source of horrible c++ codegen errors
          it.signature.pragma = newPPragma(
            newPIdentColonString("importcpp", &"#[#]= #"))

        else:
          it.signature.pragma = newPPragma(
            newPIdentColonString("importcpp", &"#[#]"))



      of cxoInfixOp:
        let namespace = (oper.namespace & newPType("operator")).toCppImport()

        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"{namespace}{it.name}(#, #)"))

        if oper.args.len == 1:
          result.addThis = true

      of cxoArrowOp:
        # WARNING
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"#.operator->()"))

      of cxoCallOp:
        # NOTE nim does have experimental support for call
        # operator, but I think it is better to wrap this one as
        # separate function `call()`
        it.name = "call"
        it.kind = pkRegular
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"#(@)"))

      of cxoDerefOp:
        it.name = "[]"
        # it.kind = pkRegular
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"*#"))

      of cxoPrefixOp:
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"{it.name}#"))

      of cxoCommaOp:
        it.name = "commaOp"
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"commaOp(@)"))

        it.kind = pkRegular
      of cxoConvertOp:
        let restype = oper.cursor.retType().toNType(conf).ntype

        it.name = "to" & capitalizeAscii(restype.head)
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"@"))


        it.signature.setRType(restype)
        it.declType = ptkConverter
        it.kind = pkRegular
      of cxoUserLitOp:
        let restype = oper.cursor.retType().toNType(conf).ntype

        it.name = "to" & capitalizeAscii(restype.head)
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"{oper.cursor}(@)"))

        it.signature.setRType(restype)
        it.kind = pkRegular
      else:
        raiseAssert("#[ IMPLEMENT ]#")

  it.signature.pragma.add newExprColonExpr(
    newPIdent "header",
    conf.makeHeader(oper.cursor, conf)
  )

  result.decl = it
  result.addThis = result.addThis or
    (kind in {
      cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp, cxoConvertOp
    })


proc wrapProcedure*(
    pr: CDecl,
    conf: WrapConfig,
    parent: Option[NType[PNode]],
    cache: var WrapCache,
    parentDecl: Option[CDecl],
    asNewConstructor: bool
  ): tuple[decl: WrappedEntry, canAdd: bool] =

  var it = PProcDecl()
  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction
    }
  )

  result.canAdd = true
  it.iinfo = currIInfo()

  if pr.isOperator():
    var genp: seq[NType[PNode]]
    iflet (par = parent):
      genp.add par.genParams

    let (decl, adt) = pr.wrapOperator(genp, conf)
    it = decl
    it.iinfo = currIInfo()
    addThis = adt
  else:
    it.signature = newProcNType[PNode](@[])
    it.name = pr.getNimName()
    it.exported = true
    iflet (par = parent):
      it.genParams = par.genParams

    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      it.iinfo = currIInfo()
      it.signature.pragma = newPPragma(
        newPIdentColonString(
          "importcpp",
          if pr.cursor.isStatic():
            let namespace = (@[parent.get()]).toCppImport()
            addThis = false
            &"{namespace}::{it.name}(@)"
          else:
            &"#.{it.name}(@)"
        ),
        newExprColonExpr(
          newPIdent "header",
          conf.makeHeader(pr.cursor, conf)))

    else:
      let pragma =
        if conf.isImportcpp:
          let namespace = pr.namespace.toCppImport()
          newPIdentColonString(
            "importcpp",
            tern(namespace.len > 0,
                 &"{namespace}::{it.name}(@)",
                 &"{it.name}(@)"
            )
          )
        else:
          newPIdentColonString("importc", &"{it.name}")

      it.iinfo = currIInfo()
      it.signature.pragma = newPPragma(
        pragma, newExprColonExpr(
          newPIdent "header",
          conf.makeHeader(pr.cursor, conf)
        )
      )

  if addThis:
    assert parent.isSome()
    it.signature.arguments.add PIdentDefs(
      varname: "self",
      vtype: parent.get(),
      kind: pr.cursor.isConstMethod.tern(nvdVar, nvdLet))


  for arg in pr.args:
    var (vtype, mutable) = arg.cursor.cxType().toNType(conf)
    # debug vtype
    if vtype.kind in {ntkIdent, ntkGenericSpec}:
      if vtype.head == "UNEXPOSED":
        # WARNING currently parameters which contain `tkUnexposed`
        # types are not handled but are skipped instead. I don't
        # know how to fix right now.
        result.canAdd = false

      if parentDecl.isSome() and
         arg.cursor.inheritsGenParamsOf(parentDecl.get().cursor) and
         parent.isSome() and
         (arg.cursor.cxType().kind notin {tkUnexposed})
        :
        # WARNING nested class definitions with additional template
        # parameters are not handled right now. It will break for
        # code like
        # `<Ta> struct A { <Tb> struct B {void func(); }; };`
        # and only add `Tb` as template parameter for `func()`.
        vtype.add parent.get().genParams

    else:
      # FIXME determine and implement edge case handling for procvar
      # arguments

      # WARNING might cause duplication, for wrapping C++ functors better
      # handling should be implemented
      vtype.pragma.add newPident("cdecl")

    it.signature.arguments.add PIdentDefs(
      varname: fixIdentName(arg.name),
      vtype: vtype,
      kind: mutable.tern(nvdVar, nvdLet))


  if pr.isOperator and pr.classifyOperator() == cxoAsgnOp:
    # Force override return type for assignment operators
    it.signature.setRType newPType("void")

  elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
    # Override handling of return types for constructors
    if not pr.isOperator():
      # But ignore implicit user-defined conversion functions like
      # `operator T()`
      assert parent.isSome(), "Cannot wrap constructor without parent object"

      it.signature.setRType tern(asNewConstructor,
                                 newNType("ptr", @[parent.get()]),
                                 parent.get())

      it.iinfo = currIInfo()
      it.signature.pragma = newPPragma(
        newPIdentColonString(
          "importcpp",
          tern(asNewConstructor,
               &"new {parent.get().head}(@)",
               &"{parent.get().head}(@)"
          )
        ),
        newExprColonExpr(
          newPIdent "header",
          conf.makeHeader(pr.cursor, conf)))

  else:
    # Default handling of return types
    var (rtype, mutable) = toNType(pr.cursor.retType(), conf)
    if parentDecl.isSome() and
       parent.isSome() and
       pr.cursor.retType().
       getTypeDeclaration().
       inheritsGenParamsOf(parentDecl.get().cursor):

      rtype.genParams = parent.get().genParams

    it.signature.setRtype rtype

    if rtype.hasUnexposed():
      # WARNING dropping all methods that use `tkUnexposed` type
      # in return value. This must be fixed in future versions.
      result.canAdd = false

    if pr.cursor.cxkind == ckDestructor:
      # Implicitly calling destructor on object (if someone ever needs
      # something like that)
      it.signature.pragma = newPPragma(
        newPIdentColonString("importcpp", &"~{it.name}()"),
        newExprColonExpr(
          newPIdent "header",
          conf.makeHeader(pr.cursor, conf)))

  if pr.cursor.kind == ckDestructor:
    it.name = "destroy" & it.name

  elif pr.cursor.cxkind in {ckConstructor, ckConversionFunction}:
    if not pr.isOperator():
      it.name = tern(asNewConstructor, "new", "init") &
        it.name.capitalizeAscii()

  if pr.cursor.isVariadic() == 1:
    it.signature.pragma.add newPIdent("varargs")

  result.decl = newWrappedEntry(toNimDecl(it), pr, pr.cursor)


proc fixNames(ppd: var PProcDecl,
              conf: WrapConfig, parent: NType[PNode]) =
  var idx: int = 0
  for param in mitems(ppd.genParams):
    conf.fixTypeName(param, conf, 0)
    inc idx

  idx = 0
  for arg in mitems(ppd.signature.arguments):
    conf.fixTypeName(arg.vtype, conf, idx)
    inc idx

  conf.fixTypeName(ppd.signature.rtype.get().getIt(), conf, 0)

  ppd.name = ppd.name.fixIdentName()





proc wrapMethods*(
  cd: CDecl, conf: WrapConfig,
  parent: NType[PNode], cache: var WrapCache): seq[WrappedEntry] =
  assert cd.kind in {cdkClass, cdkStruct}
  for meth in cd.methods({
    ckMethod, ckDestructor, ckConstructor, ckConversionFunction
  }):
    if meth.cursor.cxKind() in {ckConstructor, ckConversionFunction}:
      block:
        # Wrap as `new` constructor
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some(parent), cache, some(cd), true)

        if canAdd:
          result.add decl

      block:
        # Wrap as `init` constructor
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some(parent), cache, some(cd), false)

        if canAdd:
          result.add decl
    else:
      let (decl, canAdd) = wrapProcedure(
        meth, conf, some(parent), cache, some(cd), true)

      if canAdd:
        result.add decl


  for decl in mitems(result):
    fixNames(decl.wrapped.procdecl, conf, parent)

  result = result.deduplicate()

proc wrapFunction*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): seq[WrappedEntry] =

  var (decl, canAdd) = wrapProcedure(
    cd, conf, none(NType[PNode]), cache, none(CDecl), false)

  if canAdd:
    result.add decl


proc wrapTypeFromNamespace(
  namespace: CNamespace, conf: WrapConfig, cursor: CXCursor): PObjectDecl =
  ## `cursor` points to type declaration being wrapped
  # WARNING for now I assume that 'UNEXPOSED' type only occurs in
  # situations like `std::move_iterator<'0>::pointer` where typedef
  # uses it's semantic parent (class or struct declaration) to get
  # template parameters. This might not be a valid assumption in
  # general case.k

  var name: NType[PNode] = namespace.toNType()

  conf.fixTypeName(name, conf, 0)

  result = PObjectDecl(name: name, exported: true)

  result.annotation = some(newPPragma(
    newExprColonExpr(
      newPIdent (if conf.isImportcpp: "importcpp" else: "importc"),
      namespace.toCppImport().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cursor, conf)),
  ))




proc wrapObject*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): tuple[
    obj: WrappedEntry, genAfter, genBefore: seq[WrappedEntry]
  ]

proc wrapAlias*(
    al: CDecl, parent: CNamespace, conf: WrapConfig, cache: var WrapCache,
  ): seq[WrappedEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, # *APtr` shit that can result in multple
  # declarations.

  # debug "Alias"
  # debug al.cursor.treeRepr()

  # let importas = (parent & @[al.name]).toCppImport()
  var al = al
  al.name = al.inNamespace(parent)
  conf.fixTypeName(al.name, conf, 0)

  let aliasof = al.cursor.cxType().getCanonicalType()
  var full = aliasof.toNType(conf).ntype
  if aliasof.getNumTemplateArguments() > 0:
    let required =
      aliasof.getTypeDeclaration().
      getSpecializedCursorTemplate().
      requiredGenericParams()

    # WARNING HACK - ignore all template parameters that /might/ be
    # defaulted - i.e. only ones that *must* be specified (not
    # defaulted in declaration) are included.
    full.genParams = full.genParams[0 ..< required.len()]

  if full.hasUnexposed():
    let namespace = parent & @[newPType($al.cursor)]
    result = @[newWrappedEntry(
      toNimDecl(
        namespace.wrapTypeFromNamespace(conf, al.cursor)
      ),
      al, al.cursor
    )]
  else:
    if al.cursor[0].cxKind() notin {ckStructDecl}:
      # NOTE ignore `typedef struct` in C
      result = @[newWrappedEntry(
        toNimDecl(newAliasDecl(
          al.name, full, iinfo = currIInfo())), al, al.cursor
      )]
    else:
      if cache.canWrap(al.cursor[0]):
        # Multiple trailing typedefs result in several `ckTypedefDecl`
        # nodes

        cache.markWrap(al.cursor[0])
        let nested = visitClass(al.cursor[0], parent, conf)

        let (obj, pre, post) = wrapObject(nested, conf, cache)

        result.add pre & @[obj] & post
        # debug "Trailing struct", al.name.toNNode(), full.toNNode()

        # result = @[newWrappedEntry(
        #   toNimDecl(newAliasDecl(
        #     al.name, full, iinfo = currIInfo())), al, al.cursor
        # )]
      # else:

      if al.name != full:
        # `typedef struct {} A;` has the same typedef name and type, and
        # should only be wrapped as type definition and not alias.
        result.add newWrappedEntry(
          toNimDecl(newAliasDecl(
            al.name, full,
            iinfo = currIInfo(),
            isDistinct = false,
          )), al, al.cursor
        )

      # warn "Ignoring alias"
      # debug al.cursor.treeRepr()
      # debug al.cursor[0].cxKind()
      # result = newWrappedEntry()


const tkPODKinds = {
  tkVoid,
  tkBool,
  tkChar_U,
  tkUChar,
  tkChar16,
  tkChar32,
  tkUShort,

  tkUInt,
  tkULong,
  tkULongLong,
  tkUInt128,
  tkChar_S,
  tkSChar,

  tkWChar,
  tkShort,
  tkInt,
  tkLong,
  tkLongLong,
  tkInt128,

  tkFloat,
  tkDouble,
  tkLongDouble,
  tkNullPtr,
  tkOverload,

  tkDependent,
  tkObjCId,
  tkObjCClass,
  tkObjCSel,
  tkFloat128,

}

proc isConstQualifiedDeep(cxtype: CXType): bool =
  var cxt = cxtype
  while true:
    if cxt.isConstQualified():
      return true
    elif cxt.kind in {tkRecord, tkPointer} + tkPODKinds:
      # info cxt.kind
      return false
    elif cxt.kind in {tkTypedef}:
      cxt = cxt.getTypeDeclaration().getTypedefDeclUnderlyingType()
    else:
      return cxt.isConstQualified()


proc getClassBaseCursors*(inCursor: CXCursor): seq[CXCursor] =
  var baseQue = Deque[CXCursor]()
  baseQue.addLast inCursor

  while baseQue.len > 0:
    let cursor = baseQue.popFirst
    for node in cursor:
      if node.cxKind == ckBaseSpecifier:
        let base = node[0].getCursorReferenced()
        baseQue.addLast base
        result.add base


proc getParentFields(
    inCursor: CXCursor, obj: PObjectDecl, wrapConf: WrapConfig
  ): seq[PProcDecl] =

  for class in inCUrsor.getClassBaseCursors():
    for entry in class:
      if entry.kind in {ckFieldDecl}:
        let
          (fldType, _) = entry.cxType().toNType(wrapConf)
          fldname = $entry


        result.add newPProcDecl(
          name = fldName,
          rtyp = some(fldType),
          args = { "self" : obj.name },
          iinfo = currIInfo(),
          pragma = newPPragma(newExprColonExpr(
            newPIdent "importcpp", newRStrLit(&"#.{fldName}")))
        )

        result[^1].genParams.add(obj.name.genParams)

        result[^1].addCodeComment(
          &"Parent field getter passtrough from {class}\n")

        if not entry.cxType().isConstQualifiedDeep():
          result.add newPProcDecl(
            name = fldName,
            iinfo = currIInfo(),
            args = { "self" : obj.name, "val" : fldType },
            pragma = newPPragma(newExprColonExpr(
              newPIdent "importcpp", newRStrLit(&"#.{fldName} = @")))
          )

          result[^1].genParams.add(obj.name.genParams)
          result[^1].signature.arguments[0].kind = nvdVar
          result[^1].addCodeComment(
            &"Parent field assignment passtrough from {class}\n")

          result[^1].kind = pkAssgn


proc wrapEnum*(declEn: CDecl, conf: WrapConfig): seq[WrappedEntry]

proc wrapObject*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): tuple[
    obj: WrappedEntry, genAfter, genBefore: seq[WrappedEntry]
  ] =

  let tdecl = cd.cursor.cxType().getTypeDeclaration()

  assert cd.kind in {cdkClass, cdkStruct}
  var obj = PObjectDecl(
    name: cd.inNamespace(cd.namespace),
    exported: true, iinfo: currIInfo(),
  )

  for entry in cd.cursor:
    case entry.cxKind():
      of ckEnumDecl:
        let visited = visitEnum(entry, cd.namespace & @[cd.name], conf)
        result.genBefore.add wrapEnum(visited, conf)

      of ckStructDecl, ckClassDecl, ckUnionDecl:
        let visited = visitClass(entry, cd.namespace & @[cd.name], conf)
        let (obj, pre, post) = wrapObject(visited, conf, cache)
        result.genBefore.add pre & @[obj] & post

      of ckFieldDecl, ckMethod, ckFriendDecl,
         ckFunctionTemplate, ckAccessSpecifier,
         ckConstructor, ckDestructor
           :
        discard

      else:
        # debug entry.getSpellingLocation()
        warn &"#[ IMPLEMENT for kind {entry.cxkind()} {instantiationInfo()} ]#"

  # WARNING might die on `<T<T<T<T<T<T>>>>>` things
  for fld in cd.pubFields:
    var resFld = PObjectField(
      isTuple: false,
      name: fixIdentName(fld.name.head),
      exported: true,
      annotation: some newPPragma(
        newExprColonExpr(
          newPIdent("importc"), newRStrLit(fld.name.head))),
    )

    if fld.cursor[0].cxKind() in {ckUnionDecl, ckStructDecl}:
      let namespace = cd.namespace & @[newPType(cd.name.head)]
      var nested = visitClass(fld.cursor[0], namespace, conf)

      nested.name.head = resFld.name

      nested.name = nested.inNamespace(@[])

      let (obj, pre, post) = wrapObject(nested, conf, cache)

      result.genBefore.add(pre & @[obj] & post)

      resFld.fldType = obj.wrapped.objectDecl.name # pre # newPType(pre.name.head)
    else:
      resFld.fldType = fld.cursor.cxType().toNType(conf).ntype

    if resFld.name == "db":
      warn "sfasd"
      debug resFld.fldType.toNNode()
      debug fld.cursor.cxKind()
      debug fld.cursor.cxType()
      debug fld.cursor.cxType().getTypeDeclaration().treeRepr()


    # if fld.cursor.cxType().getTypeDeclaration().cxKind() in {ckNoDeclFound}:
    #   warn "Field", resFld.name, "has no declaration for type"

    obj.flds.add resFld

  obj.annotation = some(newPPragma(
    newExprColonExpr(
      newPIdent (if conf.isImportcpp: "importcpp" else: "importc"),
      cd.namespaceName().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cd.cursor, conf)
    ),
  ))

  if cd.cursor.cxKind() == ckUnionDecl:
    obj.annotation.get().add newPIdent("union")

  result.genAfter = cd.wrapMethods(conf, obj.name, cache)
  conf.fixTypeName(obj.name, conf, 0)

  for mem in cd.members:
    case mem.kind:
      of cdkAlias:
        result.genBefore.add mem.wrapAlias(
          cd.namespace & @[cd.name], conf, cache)
      else:
        discard

  result.genAfter.add getParentFields(cd.cursor, obj, conf).mapIt(
    newWrappedEntry(toNimDecl(it)))

  result.obj = newWrappedEntry(toNimDecl(obj), cd, cd.cursor)

type
  EnFieldVal = object
    case isRefOther*: bool
      of true:
        othername*: string
      of false:
         value*: BiggestInt

proc initEnFieldVal(v: BiggestInt): EnFieldVal =
  EnFieldVal(isRefOther: false, value: v)

proc getFields*(declEn: CDecl, conf: WrapConfig): tuple[
    namedvals: Table[string, BiggestInt],
    enfields: seq[tuple[name: string, value: Option[EnFieldVal]]]
  ] =

  for (name, value) in declEn.flds:
    let val = value.get()
    var resval: Option[EnFieldVal]
    case val.kind:
      of ckIntegerLiteral:
        resVal = some initEnFieldVal(
          val.tokens(conf.unit)[0].parseInt())

      of ckBinaryOperator:
        let subn = val.children()
        let toks = val.tokens(conf.unit)[1] # TEST for `(1 << 2) | (1 << 3)`
        case toks:
          of "<<":
            resval = some initEnFieldVal(
              subn[0].tokens(conf.unit)[0].parseInt() shl
              subn[1].tokens(conf.unit)[0].parseInt(),
            )

          of "|":
            let toks = val.tokens(conf.unit)
            # NOTE assuming `EnumField | OtherField` for now
            let
              lhs = toks[0]
              rhs = toks[2]
              lhsVal = result.namedVals[lhs]
              rhsVal = result.namedVals[rhs]

            resval = some initEnFieldVal(bitor(lhsVal, rhsVal))

          else:
            discard

      of ckUnaryOperator:
        let toks = val.tokens(conf.unit)
        case toks[0]:
          of "-":
            resval = some initEnFieldVal(toks[1].parseInt())

          else:
            raiseAssert("#[ IMPLEMENT ]#")

      elif $val.kind == "OverloadCandidate": # HACK
        resval = none EnFieldVal

      else:
        raiseAssert(
          &"#[ IMPLEMENT for kind {val.kind} {instantiationInfo()} ]#")

    result.enfields.add (name: name, value: resval)

proc wrapEnum*(declEn: CDecl, conf: WrapConfig): seq[WrappedEntry] =

  var nt = declEn.inNamespace(declEn.namespace)
  conf.fixTypeName(nt, conf, 0)
  let namespace = (declEn.namespace & newPType($declEn.cursor)).toCppImport()

  var ennames: seq[string]

  proc cEnumName(str: string): string {.closure.} =
    result = nt.head
    result[0] = result[0].toLowerAscii()
    result &= "_" & str

  var vals: OrderedTable[string, tuple[
    resName: string,
    resVal: BiggestInt,
    stringif: string
  ]]

  let implName = nt.head & "_Impl"
  block:
    var implEn = newPEnumDecl(name = implName, iinfo = currIInfo())

    implEn.pragma.add newPIdentColonString(
      (if conf.isImportcpp: "importcpp" else: "importc"),
      namespace
    )

    implEn.exported = true


    let (namedvals, enfields) = getFields(declEn, conf)
    var fldVals: Table[BiggestInt, string]
    var repeated: Table[string, seq[string]]

    for (key, val) in enfields:
      if val.isSome():
        if val.get().isRefOther:
          repeated.mgetOrPut(val.get().othername, @[ key ]).add key
        else:
          let val = val.get().value
          if val notin fldVals:
            fldVals[val] = key
          else:
            repeated.mgetOrPut(fldVals[val], @[ key ]).add key


    var flds: seq[(string, BiggestInt)]

    block:
      var prev: BiggestInt = 0
      for (key, val) in enfields:
        if val.isSome():
          if val.get().isRefOther:
            discard
          else:
            prev = val.get().value
            flds.add (key, prev)

        else:
          inc prev
          flds.add (key, prev)


      flds = flds.sorted(
        proc(f1, f2: (string, BiggestInt)): int {.closure.} =
          cmp(f1[1], f2[1])
      )

    block:
      var prev = BiggestInt(-120948783)
      for (name, val) in flds:
        if val != prev:
          prev = val
          implEn.addField(name.cEnumName(), some newPLit(val))

          vals[name] = (
            resName: name.cEnumName(),
            resVal: val,
            stringif:
              declEn.inNamespace(declEn.namespace).head & "::" & name
          )


    # debug flds
    # debug vals

    # debug repeated
    # debug fldVals
    # debug enfields


    result.add newWrappedEntry(toNimDecl(implEn), declEn, declEn.cursor)

  block:
    let pref = declEn.flds.mapIt(it.fldName).commonPrefix()

    let enumPref = declEn.name.head.
      splitCamel().
      mapIt(it[0].toLowerAscii()).
      join("")

    var en = newPEnumDecl(name = nt.head, iinfo = currIInfo())

    proc renameField(fld: string): string {.closure.} =
      fld.dropPrefix(pref).dropPrefix("_").addPrefix(enumPref)

    var arr = nnkBracket.newPTree()

    for name, wrap in vals:
      en.addField(name.renameField())

      arr.add pquote do:
        (
          name: `newPLit(name)`,
          cEnum: `newPIdent(name.cEnumName())`,
          cName: `newPLit(wrap.stringif)`,
          value: `newPLit(wrap.resVal)`
        )

    let
      enName = newPIdent(en.name)
      arrName = newPIdent("arr" & en.name & "mapping")

    let helpers = pquote do:
      const `arrName`: array[`enName`, tuple[
        name: string,
        cEnum: `newPIdent(implName)`,
        cName: string,
        value: int
      ]] = `arr`

      proc toInt*(en: `enName`): int {.inline.} =
        `arrName`[en].value

      proc toInt*(en: set[`enName`]): int {.inline.} =
        for val in en:
          result = bitor(result, `arrName`[val].value)

      proc `$`*(en: `enName`): string {.inline.} =
        `arrName`[en].cName



    result.add newWrappedEntry(toNimDecl(helpers), true)

    # debug arr

    en.exported = true

    result.add newWrappedEntry(toNimDecl(en), declEn, declEn.cursor)

proc wrapMacros*(declMacros: seq[CDecl], conf: WrapConfig): seq[WrappedEntry] =
  discard


proc wrapApiUnit*(
  api: CApiUnit, conf: WrapConfig,
  cache: var WrapCache, index: FileIndex): seq[WrappedEntry] =
  ## Generate wrapper for api unit.
  var macrolist: seq[CDecl]
  for decl in api.decls:
    if cache.canWrap(decl.cursor):
      cache.markWrap(decl.cursor)
    else:
      continue

    case decl.kind:
      of cdkClass:
        identLog()
        let spec = decl.cursor.getSpecializedCursorTemplate()

        if spec.cxKind() != ckFirstInvalid:
          discard
        else:
          let (obj, procs, other) = decl.wrapObject(conf, cache)

          result.add obj

          for it in other:
            result.add it

          for pr in procs:
            result.add pr


        dedentLog()
      of cdkAlias:
        result.add decl.wrapAlias(decl.namespace, conf, cache)
      of cdkEnum:
        result.add decl.wrapEnum(conf)
      of cdkFunction:
        for f in decl.wrapFunction(conf, cache):
          result.add f
      of cdkMacro:
        macrolist.add decl
        # result.add wrapMacro(decl, conf)
      else:
        discard

  result.add wrapMacros(macrolist, conf)

#*************************************************************************#
#********************  Dependency graph construction  ********************#
#*************************************************************************#


func toIncludes*(files: seq[AbsDir]): seq[string] =
  for file in files:
    result.add file.getStr().addPrefix("-I")

proc getFlags*(config: ParseConfig, file: AbsFile): seq[string] =
  result.add config.includepaths.toIncludes()
  result.add config.globalFlags
  result.add config.fileFlags.getOrDefault(file)

proc parseFile*(
    file: AbsFile,
    config: ParseConfig,
    wrapConf: WrapConfig,
    reparseOnNil: bool = true
  ): ParsedFile =


  file.assertExists()
  identLog()

  let flags = config.getFlags(file)
  result.filename = file
  result.index = createIndex()

  var wrapConf = wrapConf
  try:
    result.unit = parseTranslationUnit(
      result.index, file, flags, {
        tufSkipFunctionBodies, tufDetailedPreprocessingRecord},
      reparseOnNil = reparseOnNil
    )

    wrapConf.unit = result.unit

  except:
    err file.realpath
    debug config.getFlags(file).joinl()
    raise


  result.api = result.unit.splitDeclarations(wrapConf)
  result.explicitDeps = result.api.publicApi.
    getDepFiles(wrapConf).filterIt(it != file)

  result.isExplicitlyAdded = true

  dedentLog()

proc incl*[N, E, F](gr: var Graph[N, E, F], val: N) =
  if val notin gr:
    discard gr.add(val)

proc contains*[N, E, F](gr: Graph[N, E, F], pair: (N, N)): bool =
  if (pair[0] notin gr) or (pair[1] notin gr):
    return false

  for (edge, node) in gr[pair[0]].incoming():
    if node.value == pair[1]:
    # if edge in gr:
      return true


  for (edge, node) in gr[pair[0]].outgoing():
    # if edge in gr:
    if node.value == pair[1]:
      return true

proc incl*[N, E, F](
  gr: var Graph[N, E, F], pair: (N, N), edgeVal: E) =
  if pair notin gr:
    discard gr.edge(gr[pair[0]], edgeVal, gr[pair[1]])



proc registerDeps*(graph: var HeaderDepGraph, parsed: ParsedFile) =
  let file = parsed.filename

  for dep in parsed.api.includes:
    let
      path = dep.includedPath.realpath
      ifrm = dep.includedFrom.realpath

    graph.incl(path)
    graph.incl(ifrm)
    # notice ifrm, " -> ", path
    graph.incl((ifrm, path), &"{dep.includedAs}:{dep.fromLine}",)


  for dep in parsed.explicitDeps:
    graph.incl(file.realpath)
    graph.incl(dep.realpath)
    graph.incl((file.realpath, dep.realpath), "@@@")


proc parseAll*(
  files: seq[AbsFile],
  conf: ParseConfig,
  wrapConf: WrapConfig, ): FileIndex =
  for file in files:
    result.index[file] = parseFile(file, conf, wrapConf)

  result.depGraph = newGraph[AbsFile, string](HeaderGraphFlags)

  for file, parsed in result.index:
    result.depGraph.registerDeps(parsed)



import hasts/graphviz_ast
export toPng, toXDot, AbsFile

func dotRepr*(idx: FileIndex, onlyPP: bool = true): DotGraph =
  result.styleNode = makeRectConsolasNode()
  # result.splines = spsLine

  result.rankdir = grdLeftRight
  for file in idx.depGraph.nodes:
    result.addNode(makeDotNode(hash file.value, file.value.getStr()))

  for (source, edge, target) in idx.depGraph.edges:
    var e =  makeDotEdge(
      hash source.value,
      hash target.value,
      edge.value
    )

    if edge.value == "@@@":
      e.style = edsDashed
      e.label = none(string)
      if not onlyPP:
        result.addEdge e
    else:
      result.addEdge e

proc getDepModules*(file: AbsFile, idx: FileIndex): seq[AbsFile] =
  ## Get list of modules that have to be imported in wrapper for file
  ## `file`.
  for dep in idx.depGraph[file].outgoing():
    result.add dep.target.value

#*************************************************************************#
#****************************  File wrapping  ****************************#
#*************************************************************************#
func makeImport*(names: seq[string]): PNode =
  nnkImportStmt.newPTree(
    names.mapIt(it.newPident()).foldl(
      nnkInfix.newPTree(newPident("/"), a, b))
  )

# func makeExport*(names: seq[string]): PNode =
#   nnkExportStmt.newPTree(
#     names.mapIt(it.newPident()).foldl(
#       nnkInfix.newPTree(newPident("/"), a, b))
#   )


proc fixFileName*(name: string): string =
  name.multiReplace({
    "-": "_",
    "+": "p"
  })

proc getExports*(
  parsed: ParsedFile, conf: WrapConfig, index: FileIndex): seq[AbsFile] =
  ## Get list of absolute files that provide types, used in public API
  ## for `parsed` file *and* marked as internal (e.g. not supposed to
  ## be imported separately)
  for dep in parsed.explicitDeps:
    if conf.isInternal(dep, conf, index):
      result.add dep

proc wrapFile*(
  parsed: ParsedFile, conf: WrapConfig,
  cache: var WrapCache, index: FileIndex): seq[WrappedEntry] =
  info "Wrapping", parsed.filename
  var tmpRes: seq[WrappedEntry]
  # tmpRes.add newWrappedEntry(
  #   toNimDecl(
  #     pquote do:
  #       {.experimental: "codeReordering".}
  #   )
  # )

  tmpRes.add newWrappedEntry(
    toNimDecl(
      pquote do:
        import bitops
    )
  )

  # tmpRes.add newWrappedEntry(
  #   toNimDecl(nnkConstSection.newPTree(
  #     nnkConstDef.newPTree(
  #       newPIdent("cxheader"),
  #       newEmptyPNode(),
  #       newPLit(parsed.filename.getStr())
  #     ))))

  for node in parsed.explicitDeps.mapIt(
      conf.getImport(it, conf)).
      deduplicate().
      mapIt(it.makeImport()):
    tmpRes.add node.toNimDecl().newWrappedEntry()

  # for node in parsed.getExports(conf, index).mapIt(
  #   conf.getImport(it, conf)).deduplicate():
    # result.add node.makeExport().toNimDecl().newWrappedEntry()

  tmpRes.add parsed.api.wrapApiUnit(conf, cache, index)

  var res: Table[string, WrappedEntry]

  for elem in tmpRes:
    if elem.wrapped.kind == nekObjectDecl:
      let name = elem.wrapped.objectdecl.name.head
      res[name] = elem


    elif elem.wrapped.kind == nekEnumDecl:
      let name = elem.wrapped.enumdecl.name
      res[name] = elem

    elif elem.wrapped.kind == nekAliasDecl:
      let name = elem.wrapped.aliasdecl.newType.head
      if name in res:
        warn "Override type alias for ", name

      res[name] = elem

    elif elem.wrapped.kind == nekPasstroughCode:
      if not elem.postTypes:
        result.add elem


  block:
    let elems = collect(newSeq):
      for k, v in res:
        v

    result.add(newWrappedEntry(elems))

  for elem in tmpRes:
    if elem.wrapped.kind notin {
      nekObjectDecl, nekAliasDecl, nekPasstroughCode, nekEnumDecl
    }:

      result.add elem

    elif elem.wrapped.kind == nekPasstroughCode and
         elem.postTypes:

      result.add elem



proc wrapFile*(
    file: AbsFile,
    flags: seq[string],
    conf: WrapConfig,
    cache: var WrapCache,
    index: FileIndex
   ): tuple[parsed: ParsedFile, wrapped: seq[WrappedEntry]] =

  let parsedConf = ParseConfig(
    globalFlags: getBuiltinHeaders().toIncludes(),
    fileFlags: { file : flags }.toTable()
  )

  result.parsed = parseFile(file, parsedConf, conf)
  result.wrapped = result.parsed.wrapFile(conf, cache, index)

proc wrapFile*(
    cmd: CXCompileCommand,
    extraFlags: seq[string],
    conf: WrapConfig,
    cache: var WrapCache,
    index: FileIndex
  ): tuple[parsed: ParsedFile, wrapped: seq[WrappedEntry]] =

  wrapFile(
    toAbsFile($cmd.getFilename(), true),
    extraFlags & cmd.getFlags(), conf, cache, index)

type
  WrapResult* = object
    parsed*: ParsedFile
    wrapped*: seq[WrappedEntry]
    infile*: AbsFile
    importName*: seq[string]

proc boolCall*[A](
  cb: proc(a: A): bool, arg: A, default: bool = true): bool =
  if cb == nil: default else: cb(arg)

func wrapName*(res: WrapResult): string =
  res.importName.join("/") & ".nim"

proc wrapAll*(
  files: seq[AbsFile],
  parseConf: ParseConfig,
  wrapConf: WrapConfig
            ): tuple[wrapped: seq[WrapResult], index: FileIndex] =

  var
    que = initDeque[AbsFile]()
    visited: HashSet[AbsFile]
    cache: WrapCache
    parsed: FileIndex = files.parseAll(parseConf, wrapConf)

  for file, _ in parsed.index:
    que.addLast file # Add all files to que
    visited.incl file # Mark all as visited

  while que.len > 0:
    let file = que.popFirst()
    # info "Parsing file", file
    if file notin visited: # If dependency is new parse it
      parsed.index[file] = file.parseFile(parseConf, wrapConf)
      visited.incl file

    # Add to graph
    parsed.depGraph.registerDeps(parsed.index[file])

    # Store all explicit dependencies for file
    for dep in parsed.index[file].explicitDeps:
      if dep in visited or wrapConf.ignoreFile(dep):
        discard
      else:
        que.addLast dep

    for dep in parsed.index[file].api.includes:
      if dep.includedPath in visited or
         wrapConf.ignoreFile.boolCall(dep.includedPath):
        discard
      else:
        que.addLast dep.includedPath

  var wrap = wrapConf

  for file in visited:
    wrap.header = file
    wrap.unit = parsed.index[file].unit
    result.wrapped.add WrapResult(
      parsed: parsed.index[file],
      infile: file,
      importName: wrap.getImport(file, wrap),
      wrapped: parsed.index[file].wrapFile(wrap, cache, parsed)
    )

  result.index = parsed

let baseCppParseConfig* = ParseConfig(
  includepaths: getBuiltinHeaders(),
  globalFlags: @["-xc++", "-std=c++11"]
)

proc contains*(dir: AbsDir, file: AbsFile): bool =
  let dir = dir.getStr()
  let file = file.getStr()

  if file.len < dir.len:
    return false
  else:
    return file[0 .. dir.high] == dir

proc asGlobalInclude*(cursor: CXCursor, conf: WrapConfig): string =
  let loc = cursor.getSpellingLocation().get()
  for dir in conf.parseConf.includepaths:
    if loc.file in dir:
      return loc.file.getStr().dropPrefix(dir.getStr()).dropPrefix("/")

  return $loc


let baseWrapConfig* = WrapConfig(
  isImportcpp: true,
  parseConf: baseCppParseConfig,
  makeHeader: (
    proc(cursor: CXCursor, conf: WrapConfig): PNode {.closure.} =
      return newPLit("<" & cursor.asGlobalInclude(conf) & ">")
  ),
  getImport: (
    proc(dep: AbsFile, conf: WrapConfig): seq[string] {.closure.} =
      if dep.startsWith("/usr/include/c++"):
        let (dir, name, ext) = dep.splitFile()
        @["cxxstd", name.fixFileName()]
      else:
        let (dir, name, ext) = dep.splitFile()
        @[
          name.splitCamel().
            mapIt(it.toLowerAscii()).join("_").
            fixFileName()
        ]
  ),
  fixTypeName: (
    proc(ntype: var NType[PNode],
         conf: WrapConfig, idx: int) {.closure.} =
      # Default implementation for type name fixes
      fixTypeName(ntype, conf, 0)
  ),
  ignoreCursor: (
    proc(cursor: CXCursor, conf: WrapConfig): bool {.closure.} =
      if not ($cursor).startsWith("__cxx11") and
        (
          cursor.cxKind() notin { ckTypedefDecl } and
          (
            # Default convention is to prefix private parts with underscores
            ($cursor).startsWith(@[ "__", "_" ]) and
            # But inlien namespaces are still parsed by default
            (not (cursor.isInlineNamespace() == 1))
          )
        ):

        if cursor.cxKind() in {ckStructDecl, ckUnionDecl} and
           not startsWith($cursor.cxType(), @["__", "_"]):
          # `typedef struct _A {} A;` for C wrapping
          return false
        else:
          return true

      if cursor.cxKind == ckNamespace and
         ($cursor in @["detail", "internal"]):
        return true

      elif cursor.cxKind == ckFieldDecl:
        if startsWith($cursor, "private"):
          return true

        else:
          if conf.isTypeInternal(cursor.cxType(), conf):
            return true

      else:
        return false
  ),
  isTypeInternal: (
    proc(cxt: CXType, conf: WrapConfig): bool {.closure.} =
      case cxt.cxKind:
        of tkPodKinds:
          result = false
        of tkTypedef:
          # debug cxt.lispRepr()
          result = startsWith($cxt, "_")
        of tkPointer:
          result = conf.isTypeInternal(cxt[], conf)
        else:
          result = false

  ),
  isInternal: (
    proc(dep: AbsFile, conf: WrapConfig,
         index: FileIndex): bool {.closure.} =
      isInternalImpl(dep, conf, index)
  ),
  depResolver: (
    proc(cursor, referencedBy: CXCursor): DepResolutionKind {.closure.} =
      if cursor.isFromMainFile():
        result = drkWrapDirectly

      else:
        result = drkImportUses
        # let loc = cursor.getSpellingLocation()
        # if loc.isSome():
        #   let loc = loc.get()
        #   let (dir, file, ext) = loc.file.splitFile()
        #   if "string" in file:
        #     result = drkWrapDirectly
  )
)

#*************************************************************************#
#*********************  Wrapped code postprocessing  *********************#
#*************************************************************************#
proc nimifyInfixOperators*(we: var WrappedEntry): seq[WrappedEntry] {.nimcall.} =
  if we.isMultitype:
    return

  var we = we
  we.wrapped.iinfo = currIInfo()

  if we.wrapped.kind == nekProcDecl and
     we.wrapped.procdecl.kind == pkOperator:
    var opd {.byaddr.} = we.wrapped.procdecl
    case opd.name:
      of "<<":
        opd.name = "shl"
        result.add we

      of ">>":
        opd.name = "shr"
        result.add we

      of "%":
        opd.name = "mod"
        result.add we

      else:
        discard

proc nep1Idents*(we: var WrappedEntry): seq[WrappedEntry] {.nimcall.} =
  if not we.isMultitype:
    if we.wrapped.kind == nekProcDecl and
       we.wrapped.procdecl.kind == pkRegular:
      we.wrapped.procdecl.name[0] = toLowerAscii(we.wrapped.procdecl.name[0])

proc enumOverloads*(we: var WrappedEntry): seq[WrappedEntry] {.nimcall.} =
  if we.isMultitype:
    return

  if we.wrapped.kind == nekProcDecl:
    let enArgs = toTable: collect(newSeq):
      for idx, arg in we.cursor.getArguments():
        if arg.cursor.cxType().getTypeDeclaration().cxKind() in {ckEnumDecl}:
          (idx, arg)

    # info enArgs

    var pr {.byaddr.} = we.wrapped.procdecl
    if enArgs.len > 0:
      var reproc = we.wrapped.procdecl
      reproc.iinfo = currIInfo()
      reproc.signature.pragma.clear()

      var impl = nnkStmtList.newPTree()
      var subcall = nnkCall.newPTree(newPIdent reproc.name)

      for idx, arg in mpairs(pr.signature.arguments):
        let argname = newPIdent(arg.varname)
        let arrname = newPIdent("arr" & arg.vtype.head & "mapping")

        if idx in enArgs:
          arg.vtype.head &= "_Impl"
          let argImpl = newPIdent(arg.varname & "_main")

          impl = pquote do:
            let `argImpl` = `arrname`[`argname`].cEnum

          subcall.add nnkExprEqExpr.newPTree(argname, argImpl)

        else:
          subcall.add nnkExprEqExpr.newPTree(argname, argname)

      reproc.impl = pquote do:
        `impl`
        `subcall`

      # debug subcall
      # debug impl

      result.add newWrappedEntry(toNimDecl(reproc), we.original, we.cursor)

type
  Postprocess* = object
    impl: proc(we: var WrappedEntry): seq[WrappedEntry]


func newPostprocess*(cb: proc(we: var WrappedEntry): seq[WrappedEntry]): Postprocess =
  Postprocess(impl: cb)

var defaultPostprocessSteps*: seq[Postprocess]

defaultPostprocessSteps.add @[
  newPostprocess(nep1Idents),
  newPostprocess(nimifyInfixOperators),
  newPostprocess(enumOverloads)
]


proc postprocessWrapped*(
    entries: seq[WrappedEntry],
    postprocess: seq[Postprocess] = defaultPostprocessSteps,
  ): seq[WrappedEntry] =

  for we in entries:
    var we = we
    var res: seq[WrappedEntry]

    for step in postprocess:
      res.add step.impl(we)

    result.add we
    result.add res


proc wrapSingleFile*(
    file: FsFile,
    errorReparseVerbose: bool = false,
    wrapConf: WrapConfig = baseWrapConfig,
    parseConf: ParseConfig = baseCppParseConfig,
    postprocess: seq[Postprocess] = defaultPostprocessSteps
  ): seq[NimDecl[PNode]] =

  var
    cache: WrapCache
    index: FileIndex

  let parsed = parseFile(
    file.toAbsFile(), parseConf, wrapConf,
    reparseOnNil = errorReparseVerbose
  )

  var wrapConf = wrapConf

  wrapConf.unit = parsed.unit
  warn wrapConf.unit.getTranslationUnitCursor().cxKind()
  # quit 0

  let wrapped = parsed.wrapFile(wrapConf, cache, index)

  proc updateComments(decl: var PNimDecl, node: WrappedEntry) =
    decl.addCodeComment(
      "Wrapper for `" &
      (
        node.cursor.getSemanticNamespaces(filterInline = false)).join("::") &
      "`\n"
    )

    if node.cursor.getSpellingLocation().getSome(loc):
      decl.addCodeComment(
        &"Declared in {loc.file}:{loc.line}")




  for node in wrapped.postprocessWrapped(postprocess):
    if node.isMultitype:
      var resdecl: seq[PNimTypeDecl]
      for t in node.decls:
        assert not t.isMultitype
        var decl = t.wrapped

        updateComments(decl, t)
        resdecl.add toNimTypeDecl(decl)

      result.add toNimDecl(resdecl)
    else:
      if node.hasCursor:
        var decl = node.wrapped
        updateComments(decl, node)

        result.add decl
      else:
        result.add node.wrapped

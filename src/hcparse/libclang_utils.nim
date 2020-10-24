 ## Helper functions for libclang. This file should not be imported -
## it is `include`'d in the main `libclang` file.

when not defined(libclangIncludeUtils):
  import libclang


#==============================  includes  ===============================#
import bitops, strformat, macros, terminal, sugar, std/decls, strutils,
       sequtils, options, re, hashes, deques, sets, hashes, logging

import hpprint, hpprint/hpprint_repr
import hnimast
import nimtraits
import compiler/ast
import packages/docutils/rstast
import hmisc/types/colorstring
import hmisc/[hexceptions, helpers, hdebug_misc]
import hmisc/other/[hshell, colorlogger, oswrap]



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
  reparseOnNil: bool = true): CXTranslationUnit =

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
  extraFlags: seq[string] = @[]): CXTranslationUnit =
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
  index.parseTranslationUnit(file.toAbsFile(true), args, {})






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

func makeVisitorImpl*(varnames: seq[NimNode], body: NimNode): NimNode =
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

  result = quote do:
    block:
      `immutableCopies`
      var data {.inject.} = `tupleData`
      type Data = typeof(data)
      proc visitor(`cursorId`, `parentId`: CXCursor,
                   clientData: pointer): CXChildVisitResult {.cdecl.} =
        let data {.inject.} = cast[ptr[Data]](clientData)
        `dataUnpack`
        `body`

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
  return makeVisitorImpl(toSeq(captureVars), body)



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
  (cursor.kind in {ckCXXMethod}) and (cxxMethodIsConst(cursor) == 0)

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
  tokenize(tu, range, addr tokens, addr nTokens)

  for i in 0 ..< nTokens:
    result.add $getTokenSpelling(
      tu, (cast[ptr UncheckedArray[CXToken]](tokens))[i])

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
    makeVisitor [callback, mainParent]:
      if cursor.isFromMainFile():
        return callback.impl(cursor, mainParent, addr callback.data)
      else:
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


proc isFromMainFile*(cursor: CXCursor): bool =
  ## Return true if cursor posints to main file
  let location = cursor.getCursorLocation()
  result = location.locationIsFromMainFile() != cint(0)

proc getExpansionLocation*(location: CXSourceLocation): tuple[
  file: AbsFile, line, column, offset: int] =

  var
    file: CXFile
    line: cuint
    column: cuint
    offset: cuint

  location.getSpellingLocation(
    addr file, addr line, addr column, addr offset)

  result.file = toAbsFile($file.getFileName(), true) # WARNING set root?
  # echov file.getFileName()
  # echov result.file
  result.file = result.file.realpath
  result.line = line.int
  result.column = column.int
  result.offset = offset.int


proc getSpellingLocation*(cursor: CXCursor): tuple[
  file: AbsFile, line, column, offset: int] =
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
type
  WrapConfig* = object
    ## Configuration for wrapping. Mostly deals with type renaming
    header*: AbsFile ## Current main translation file (header)
    unit*: CXTranslationUnit
    makeHeader*: proc(conf: WrapConfig): PNode ## Genreate identifier for
    ## `{.header: ... .}`
    fixTypeName*: proc(ntype: var NType[PNode], conf: WrapConfig, idx: int)
    ## Change type name for `ntype`. Used to convert things like
    ## `boost::wave::macro_handling_exception::bad_include_file` into
    ## human-readable names.
    getImport*: proc(dep: AbsFile, conf: WrapConfig): seq[string] ## Generate
    ## import statement for header file dependency
    ignoreCursor*: proc(curs: CXCursor, conf: WrapConfig): bool ## User-defined
    ## predicate for determining whether or not cursor should be
    ## considered a part of api. Things like `internal` namespaces.
    collapsibleNamespaces*: seq[string]
    ignoreFile*: proc(file: AbsFile): bool

  WrapCache* = HashSet[Hash]


#==========================  Helper utilities  ===========================#

proc declHash*(cursor: CXCursor): Hash =
  let loc = cursor.getSpellingLocation()
  return !$(
    hash(loc.file) !& hash(loc.line) !&
    hash(loc.column) !& hash(loc.offset))

proc markWrap*(cache: var WrapCache, cursor: CXCursor) =
  cache.incl cursor.declHash()

proc canWrap*(cache: WrapCache, cursor: CXCursor): bool =
  cursor.declHash() notin cache

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
    ckCXXMethod,
    ckConversionFunction,
    ckFunctionTemplate
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
    of "set": "cxset"
    of "type": "cxtype"
    of "range": "cxrange"
    of "string": "cxstring"
    of "begin": "cxbegin"
    of "end": "cxend"
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
    "cuchar", "cstring", "cchar", "uint32", "uint16"
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
  ntype.head = fixTypeName(ntype.head, idx, conf)

  var idx = idx
  for gen in mitems(ntype.genParams):
    conf.fixTypeName(gen, conf, idx)
    inc idx


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
proc getTypeName*(curs: CXCursor, conf: WrapConfig): string

proc toNType*(
  cxtype: CXType,
  conf: WrapConfig): tuple[ntype: NType[PNode], mutable: bool]


proc fromElaboratedPType*(cxtype: CXType, conf: WrapConfig): NType[PNode] =
  let genParams = cxtype.getNumTemplateArguments()
  # echov genParams
  # echov cxtype
  if genParams > 0:
    let decl = cxtype.getTypeDeclaration()
    # echov decl.cxKind
    # echov decl
    case decl.cxKind:
      of ckTypedefDecl:
        # WARNING `template <J, Q> using` is not handled
        result = newPType(decl.getTypeName(conf))
      of ckClassDecl, ckStructDecl:
        let params = cxtype.genParams()
        result = newPType(decl.getTypeName(conf))
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.genParams.add parm.toNType(conf).ntype

      else:
        warn decl.treeRepr()

    conf.fixTypeName(result, conf, 0)

  else:
    result = ($cxtype).
      dropPrefix("enum ").
      dropPrefix("struct ").
      dropPrefix("const "). # WARNING
      newPType()


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

proc getSemanticNamespaces*(curs: CXCursor): seq[string] =
  let parent = curs.getCursorSemanticParent()
  case parent.cxKind:
    of ckNamespace:
      result = parent.getSemanticNamespaces() & @[$parent]
    else:
      discard

proc getTypeName*(curs: CXCursor, conf: WrapConfig): string =
  case curs.cxKind:
    of ckTypedefDecl:
      return $curs.cxType()
    of ckClassDecl, ckStructDecl:
      result = $curs
    else:
      err $curs
      err "Type name for ", curs.treeRepr(conf.unit)
      raiseAssert(
        &"Cannot convert cursor of kind {curs.cxKind} to type")

  result = (curs.getSemanticNamespaces() & @[result]).join("::")

proc isMutableRef*(cxtype: CXType): bool =
  case cxType.cxKind:
    of tkLValueReference, tkRValueReference:
      return not (cxType.isConstQualifiedType() == 0)
    of tkTypeDef:
      # TODO implement mutability checking
      let decl = cxtype.getTypeDeclaration()
      # info cxtype, "is defined on line", decl.getSpellingLocation().line
      if decl.len == 1 and decl[0].cxKind == ckTypeRef:
        discard
        # info "Simple type alias for", decl[0].cxType()
        # debug decl[0].cxType().getTypeDeclaration().treeRepr()
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
    of tkBool: newPType("bool")
    of tkInt: newPType("cint")
    of tkVoid: newPType("void")
    of tkUInt: newPType("cuint")
    of tkLongLong: newPType("clonglong")
    of tkULongLong: newPType("culonglong")
    of tkDouble: newPType("cdouble")
    of tkULong: newPType("culong")
    of tkUChar: newPType("cuchar")
    of tkChar16: newPType("uint16") # WARNING C++ type is `char16_t`
    of tkChar32: newPType("uint32") # WARNING C++ type is `char32_t`
    of tkWChar: newPType("uint32") # WARNING C++ type is `wchar_t`
    of tkChar_S: newPType("cchar")
    of tkLong: newPType("clong")
    of tkUShort: newPType("cushort")
    of tkNullPtr: newPType("pointer") # WARNING C++ type is `nullptr_t`
    of tkTypedef:
      result.mutable = cxType.isMutableRef()
      newPType(($cxtype).dropPrefix("const ")) # XXXX typedef processing -
    of tkElaborated, tkRecord, tkEnum:
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
    of tkUnexposed: newPType("UNEXPOSED")
    of tkDependent: newPType("DEPENDENT")
    else:
      err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newPType("!!!")

  result.ntype = restype
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
#=========================  C entry declaration  =========================#
when true:
  when true: # derive commonDerives:
    type
      CDeclKind* = enum
        cdkClass
        cdkStruct
        cdkEnum
        cdkFunction
        cdkMethod
        cdkField
        cdkAlias

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
        cursor* {.requiresinit.}: CXCursor
        case kind*: CDeclKind
          of cdkField:
            fldAccs*: CX_CXXAccessSpecifier
          of cdkMethod:
            metAccs*: CX_CXXAccessSpecifier
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

#=================================  ---  =================================#


proc accs(self: CDecl): CX_CXXAccessSpecifier =
  if contains({cdkField}, self.kind):
    return self.fldAccs
  if contains({cdkMethod}, self.kind):
    return self.metAccs
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `accs=`(self: var CDecl; it: CX_CXXAccessSpecifier) =
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
    result.genParams.add part.genParams
    nameBuf.add part.head

  result.head = nameBuf.join("::")


func inNamespace*(cd: CDecl, ns: CNamespace): NType[PNode] =
  var nameBuf: seq[string]

  for n in ns & @[ cd.name ]:
    result.genParams.add n.genParams
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
  cd.kind == cdkMethod and
  cd.name.head.startsWith("operator") and
  (not cd.name.head.validIdentifier())

func classifyOperator*(cd: CDecl): CXOperatorKind =
  assert cd.isOperator()
  let name = cd.name.head.dropPrefix("operator")
  case name:
    of "+=", "=", "-=", "*=": # NOTE `=`
      cxoAsgnOp
    of "[]":
      cxoArrayOp
    of "-", "+", "/",
       "++", "--" # NOTE this is an operator implementation, so we are
                  # not (i hope) dropping information about
                  # prefix/infix calls
         :
      cxoInfixOp
    of "*": # NOTE this heuristics might not be valid in all cases.
      if cd.args.len == 0:
        cxoDerefOp
      else:
        cxoInfixOp
    of "->":
      cxoArrowOp
    of "()":
      cxoCallOp
    else:
      raiseAssert(&"#[ IMPLEMENT {name} ]#")


func getNimName*(cd: CDecl): string =
  case cd.kind:
    of cdkMethod:
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


#*************************************************************************#
#*********************  Translation unit conversion  *********************#
#*************************************************************************#
#==========================  Type declarations  ==========================#
type
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


#===========  Splitting translation unit into logical chunks  ============#
proc getArguments(cursor: CXCursor): seq[CArg] =
  for idx, subn in cursor.children():
    if subn.cxKind in {ckParmDecl}:
      var name = $subn
      if name.len == 0:
        name = "a" & $idx

      result.add CArg(name: name, cursor: subn)

proc visitMethod(cursor: CXCursor, accs: CX_CXXAccessSpecifier): CDecl =
  result = CDecl(kind: cdkMethod, cursor: cursor, name: newPType $cursor)
  result.accs = accs
  result.args = cursor.getArguments()


proc visitField(cursor: CXCursor, accs: CX_CXXAccessSpecifier): CDecl =
  result = CDecl(kind: cdkField, cursor: cursor, name: newPType $cursor)
  result.accs = accs

var undefCnt: int = 0

proc visitAlias*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkAlias, cursor: cursor, namespace: parent)

  result.name = newPType $cursor

  # if "string" in $cursor:
  #   info "Visit alias ", cursor
  #   debug cursor.treeRepr()

proc visitFunction(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkFunction, cursor: cursor, namespace: parent)
  result.name = newPType $cursor
  result.args = cursor.getArguments()
  for subn in cursor:
    case subn.cxKind:
      of ckTemplateTypeParameter:
        result.name.genParams.add newPType $subn
      of ckNonTypeTemplateParameter, ckTemplateRef:
        result.genConstraints.add subn
      of ckParmDecl, ckTypeRef, ckNamespaceRef,
         ckFirstExpr, ckCompoundStmt, ckMemberRef:
         # WARNING right now these things are just droppped. Maybe it
         # will cause some errors in the future, I don't really know.
        discard
      else:
        warn "Unknown element", subn.cxKind, subn, "in\n" & $cursor


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
    ckStructDecl
  })

  result = CDecl(kind: cdkClass, cursor: cursor, name: newPType($cursor))
  result.namespace = parent
  result.name.genParams.add cursor.requiredGenericParams()

  identLog()
  defer: dedentLog()

  var currentAccs =
    if cursor.cxKind == ckStructDecl:
      asPublic
    else:
      asPrivate

  for subn in cursor:
    if subn.cxKind == ckCXXAccessSpecifier:
      currentAccs = subn.getCXXAccessSpecifier()

    if currentAccs == asPublic:
      case subn.cxKind:
        of ckCXXMethod, ckConversionFunction:
          result.members.add visitMethod(subn, currentAccs)
        of ckConstructor:
          var res = visitMethod(subn, currentAccs)
          res.name.head = "new" & result.name.head
        of ckCXXAccessSpecifier:
          currentAccs = subn.getCXXAccessSpecifier()
        of ckFieldDecl,
           ckVarDecl # WARNING static fields might need to be wrapped differently
             :
          result.members.add visitField(subn, currentAccs)
        of ckTemplateTypeParameter, ckFriendDecl,
           ckStaticAssert, ckDestructor, ckTemplateTemplateParameter:
          discard
        of ckFunctionTemplate:
          result.members.add visitFunction(
            subn, parent & @[ result.name ], conf)
        of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
           ckTypedefDecl, ckUsingDeclaration:
          if not conf.ignoreCursor(subn, conf):
            result.members.add visitAlias(
              subn, parent & @[result.name], conf)
        of ckStructDecl, ckClassDecl:
          # WARNING IMPLEMENT nested structure/class declarations are
          # not implemented
          discard
        of ckCXXBaseSpecifier:
          # WARNING base class specifier ignored, FIXME need to
          # implement
          discard
        else:
          inc undefCnt
          debug subn.getSpellingLocation()
          debug subn.treeRepr()
          if undefCnt > 20:
            raiseAssert("Reached unknown class element limit")
          else:
            discard
            # warn "IMPLEMENT class element:", ($subn.cxKind).toRed(), subn
            # debug subn.treeRepr()



proc visitCursor(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): tuple[
  decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]]

proc visitNamespace(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.
  if not conf.ignoreCursor(cursor, conf):
    for subn in cursor:
      result.add visitCursor(subn, parent & @[ newPType($cursor) ], conf).decls
  # else:
  #   warn "Skipping namespace", cursor
  #   debug cursor.getSpellingLocation()

proc visitCursor(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): tuple[
  decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]] =

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
      of ckTypedefDecl:
        result.decls.add visitAlias(cursor, parent, conf) 
      of ckEnumDecl:
        result.decls.add visitEnum(cursor, parent, conf) 
      of ckInclusionDirective:
        let loc = cursor.getSpellingLocation()
        result.includes.add IncludeDep(
          includedAs: $cursor,
          includedFrom: loc.file,
          fromLine: loc.line,
          fromColumn: loc.column,
          fromOffset: loc.offset,
          includedPath: AbsFile($cursor.getIncludedFile()).realpath
        )
        # info "Found include ", cursor
        # debug cursor.getSpellingLocation()
      else:
        # warn "Recursing on", cursor, "of kind", cursor.cxKind()
        result.recurse = true
  # else:
  #   warn "Skipping cursor ", cursor
  #   debug cursor.getSpellingLocation()


proc splitDeclarations*(
  tu: CXTranslationUnit, conf: WrapConfig): CApiUnit =
  ## Convert main file of translation unit into flattened sequence of
  ## high-level declarations. All cursors for objects/structs are
  ## retained. Public API elements are stored in `publicAPI` field
  assert not tu.isNil
  let cursor = tu.getTranslationUnitCursor()
  var res: CApiUnit
  cursor.visitMainFile do:
    makeVisitor [tu, res, conf]:
      let (decls, rec, incls) = visitCursor(cursor, @[], conf)
      res.includes.add incls
      if rec:
        return cvrRecurse
      else:
        res.decls.add decls
        for decl in decls:
          res.publicAPI.add decl.getPublicAPI()

        return cvrContinue

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


proc getDepFiles*(deps: seq[CXCursor]): seq[AbsFile]

proc getDepFiles*(cxtype: CXType): seq[AbsFile] =
  let decl = cxtype.getTypeDeclaration()
  for parm in cxtype.genParams():
    if parm.cxKind != tkInvalid:
      for file in getDepFiles(parm):
        result.add file

  ignorePathErrors {pekInvalidEntry}:
    let (file, _, _, _) = decl.getSpellingLocation()
    result.add file

  for file in result:
    assertExists(file)

proc getDepFiles*(deps: seq[CXCursor]): seq[AbsFile] =
  ## Generate list of files that have to be wrapped
  # TODO:DOC
  for dep in deps:
    var decl: (CXCursor, bool)
    # echov dep
    case dep.cxKind:
      of ckFunctionDecl, ckCXXMethod, ckConversionFunction:
        result.add getDepFiles(dep.params()).withIt do:
          for file in it:
            # echov file
            assertExists file

        decl = (dep.retType().getTypeDeclaration(), true)
      of ckFunctionTemplate:
        result.add dep.retType().getDepFiles().withIt do:
          for file in it:
            assertExists file
      of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
         ckTypedefDecl, ckUsingDeclaration:
        # if dep.cxType().getTypeDeclaration() ==
        #    dep.cxType().getCanonicalType().getTypeDeclaration()
        #    :
        #   info "type alias for ", dep.cxType(),
        #    " uses on forward declaration"
        #   debug dep.cxType().getCanonicalType().
        #     getTypeDeclaration().getSpellingLocation()

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
              result.add getDepFiles(parm).withIt do:
                for file in it:
                  assertExists file


        decl = (cxt.getTypeDeclaration(), true)
      else:
        # warn "dep for:", dep.cxKind(), dep, dep.cxType()
        decl = (dep.cxType.getTypeDeclaration(), true)

    if decl[1]:
      ignorePathErrors {pekInvalidEntry}:
        # WARNING ignore invalid `#include`
        # echov decl
        let (file, line, column, _) = decl[0].getSpellingLocation()
        # echov file
        assertExists(file)
        result.add file


  result = result.deduplicate().
    filterIt(it.len > 0 and it.hasExt()).
    mapIt(it.realpath())



#*************************************************************************#
#*************************  Wrapper generation  **************************#
#*************************************************************************#
proc wrapOperator*(
  oper: CDecl, genParams: seq[NType[PNode]],
  conf: WrapConfig): tuple[
    decl: PProcDecl, addThis: bool] =

  var it = PProcDecl()

  it.signature = newProcNType[PNode](@[])
  it.name = oper.getNimName()
  it.genParams = genParams
  it.exported = true

  let kind = oper.classifyOperator()
  it.kind = pkOperator

  if kind == cxoAsgnOp and it.name == "setFrom":
    it.signature.pragma = newPPragma(
      newPIdentColonString("importcpp", &"# = #"),
      newExprColonExpr(newPIdent "header",
                       conf.makeHeader(conf)))
    it.kind = pkRegular
  else:
    case kind:
      of cxoAsgnOp:
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"# {it.name} #"),
          newExprColonExpr(newPIdent "header",
                           conf.makeHeader(conf)),
        )
      of cxoArrayOp:
        let rtype = oper.cursor.retType()
        let (_, mutable) = rtype.toNType(conf)
        if mutable:
          it.name = "[]="
          # WARNING potential source of horrible c++ codegen errors
          it.signature.pragma = newPPragma(
            newPIdentColonString("importcpp", &"#[#]= #"),
            newExprColonExpr(newPIdent "header",
                             conf.makeHeader(conf)))

        else:
          it.signature.pragma = newPPragma(
            newPIdentColonString("importcpp", &"#[#]"),
            newExprColonExpr(newPIdent "header",
                             conf.makeHeader(conf)))



      of cxoInfixOp:
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"# {it.name} #"),
          newExprColonExpr(newPIdent "header",
                           conf.makeHeader(conf)),
        )
      of cxoArrowOp:
        # WARNING
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"#.operator->()"),
          newExprColonExpr(newPIdent "header",
                           conf.makeHeader(conf)),
        )
      of cxoCallOp:
        # NOTE nim does have experimental support for call
        # operator, but I think it is better to wrap this one as
        # separate function `call()`
        it.name = "call"
        it.kind = pkRegular
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"#(@)"),
          newExprColonExpr(newPIdent "header",
                           conf.makeHeader(conf)),
        )
      of cxoDerefOp:
        it.name = "[]"
        # it.kind = pkRegular
        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"*#"),
          newExprColonExpr(newPIdent "header",
                           conf.makeHeader(conf)))
      else:
        raiseAssert("#[ IMPLEMENT ]#")

  result.decl = it
  result.addThis = kind in {cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp}


proc wrapMethods*(
  cd: CDecl, conf: WrapConfig,
  parent: NType[PNode], cache: var WrapCache): seq[PProcDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for meth in cd.methods({ckCXXMethod}):
    var canAdd: bool = true
    let decl = PProcDecl().withIt do:
      var addThis = true
      if meth.isOperator():
        let (decl, adt) = meth.wrapOperator(parent.genParams, conf)
        it = decl
        addThis = adt
      else:
        it.signature = newProcNType[PNode](@[])
        it.name = meth.getNimName()
        it.exported = true
        it.genParams = parent.genParams

        it.signature.pragma = newPPragma(
          newPIdentColonString("importcpp", &"#.{it.name}(@)"),
          newExprColonExpr(newPIdent "header",
                           conf.makeHeader(conf)))

      if addThis:
        it.signature.arguments.add PIdentDefs(
          varname: "self",
          vtype: parent,
          kind: meth.cursor.isConstMethod.tern(nvdVar, nvdLet))

      for arg in meth.args:
        var (vtype, mutable) = arg.cursor.cxType().toNType(conf)
        if vtype.head == "UNEXPOSED":
          # WARNING currently parameters which contain `tkUnexposed`
          # types are not handled but are skipped instead. I don't
          # know how to fix right now.
          canAdd = false

        if arg.cursor.inheritsGenParamsOf(cd.cursor):
          # WARNING nested class definitions with additional template
          # parameters are not handled right now. It will break for
          # code like
          # `<Ta> struct A { <Tb> struct B {void func(); }; };`
          # and only add `Tb` as template parameter for `func()`.
          vtype.genParams.add parent.genParams

        it.signature.arguments.add PIdentDefs(
          varname: fixIdentName(arg.name),
          vtype: vtype,
          kind: mutable.tern(nvdVar, nvdLet))

      # Force override return type for assignment operators
      if not (meth.isOperator and meth.classifyOperator() == cxoAsgnOp):
        var (rtype, mutable) = toNType(meth.cursor.retType(), conf)
        if meth.cursor.retType().
           getTypeDeclaration().
           inheritsGenParamsOf(cd.cursor):

          rtype.genParams = parent.genParams

        it.signature.setRtype rtype

        if rtype.hasUnexposed():
          # WARNING dropping all methods that use `tkUnexposed` type
          # in return value. This must be fixed in future versions.
          canAdd = false

      else:
        it.signature.setRType newPType("void")


      block: # Name fixes
        var idx: int = 0
        for param in mitems(it.genParams):
          conf.fixTypeName(param, conf, 0)
          inc idx

        idx = 0
        for arg in mitems(it.signature.arguments):
          conf.fixTypeName(arg.vtype, conf, idx)
          inc idx

        conf.fixTypeName(it.signature.rtype.get().getIt(), conf, 0)

        it.name = it.name.fixIdentName()

    if canAdd:
      result.add decl


  result = result.deduplicate()


proc wrapTypeFromNamespace(
  namespace: CNamespace, conf: WrapConfig): PNode =
  # WARNING for now I assume that 'UNEXPOSED' type only occurs in
  # situations like `std::move_iterator<'0>::pointer` where typedef
  # uses it's semantic parent (class or struct declaration) to get
  # template parameters. This might not be a valid assumption in
  # general case.k

  var name: NType[PNode] = namespace.toNType()
  conf.fixTypeName(name, conf, 0)
  var obj = PObject(name: name, exported: true)
  obj.annotation = some(newPPragma(
    newExprColonExpr(newPIdent "importcpp",
                     namespace.toCppImport().newRStrLit()),
    newExprColonExpr(newPIdent "header", conf.makeHeader(conf)),
  ))

  result = obj.toNNode(standalone = true)


proc wrapAlias*(
  al: CDecl, parent: CNamespace, conf: WrapConfig): PNode =
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
    result = namespace.wrapTypeFromNamespace(conf)
  else:
    let (head, genparams) = al.name.toExported(true)
    result = nnkTypeSection.newPTree(
      nnkTypeDef.newPTree(head, genparams, full.toNNode())
    )

proc wrapObject*(cd: CDecl, conf: WrapConfig, cache: var WrapCache): tuple[
  obj: PObject, procs: seq[PProcDecl], other: seq[PNode]] =
  assert cd.kind in {cdkClass, cdkStruct}
  result.obj = PObject(name: cd.inNamespace(cd.namespace), exported: true)
  # WARNING might die on `<T<T<T<T<T<T>>>>>` things
  # for genParam in cd.cursor.children({ckTemplateTypeParameter}):
  #   result.obj.name.genParams.add newPType($genParam)

  for fld in cd.pubFields:
    result.obj.flds.add PField(
      isTuple: false,
      name: fld.name.head,
      exported: true,
      fldType: fld.cursor.cxType().toNType(conf).ntype
    )

  result.obj.annotation = some(newPPragma(
    newExprColonExpr(newPIdent "importcpp",
                     cd.namespaceName().newRStrLit()),
    newExprColonExpr(newPIdent "header", conf.makeHeader(conf)),
  ))

  result.procs = cd.wrapMethods(conf, result.obj.name, cache)
  conf.fixTypeName(result.obj.name, conf, 0)

  for mem in cd.members:
    case mem.kind:
      of cdkAlias:
        result.other.add mem.wrapAlias(cd.namespace & @[cd.name], conf)
      else:
        discard

proc wrapEnum*(declEn: CDecl, conf: WrapConfig): PNode =
  let pref = declEn.flds.mapIt(it.fldName).commonPrefix()

  let enumPref = declEn.name.head.
    splitCamel().
    mapIt(it[0].toLowerAscii()).
    join("")

  var nt = declEn.inNamespace(declEn.namespace)
  conf.fixTypeName(nt, conf, 0)
  var en = PEnum(name: nt.head)

  proc renameField(fld: string): string {.closure.} =
    fld.dropPrefix(pref).dropPrefix("_").addPrefix(enumPref)

  var prevVal: BiggestInt = -123124912
  for (name, value) in declEn.flds:
    let val = value.get()
    var fld = EnumField[PNode](name: name, kind: efvOrdinal)

    var skip = false # C++ allows duplicate elements in enums, so we
                     # need to skip them when wrapping things into nim.
    case val.kind:
      of ckIntegerLiteral:
        fld.ordVal = makeRTOrdinal(val.tokens(conf.unit)[0].parseInt())
      of ckBinaryOperator:
        let subn = val.children()
        let toks = val.tokens(conf.unit)[1]
        case toks:
          of "<<":
            fld.ordVal = makeRTOrdinal(
              subn[0].tokens(conf.unit)[0].parseInt() shl
              subn[1].tokens(conf.unit)[0].parseInt(),
            )
          of "|":
            let toks = val.tokens(conf.unit)
            # NOTE assuming `EnumField | OtherField` for now
            let
              lhs = renameField(toks[0])
              rhs = renameField(toks[2])
              lhsVal = en.values.findItFirst(it.name == lhs)
              rhsVal = en.values.findItFirst(it.name == rhs)

            fld.ordVal = makeRTOrdinal(
              bitor(
                lhsVal.ordVal.intVal,
                rhsVal.ordVal.intVal,
              )
            )

          else:
            discard

      of ckUnaryOperator:
        let toks = val.tokens(conf.unit)
        case toks[0]:
          of "-":
            fld.ordVal = makeRTOrdinal(toks[1].parseInt())
          else:
            raiseAssert("#[ IMPLEMENT ]#")
      of ckDeclRefExpr:
        skip = true
      else:
        if $val.kind == "OverloadCandidate":
          fld = EnumField[PNode](name: name, kind: efvNone)
        else:
          fld = EnumField[PNode](name: name, kind: efvNone)


    if not skip:
      if fld.kind == efvOrdinal:
        if fld.ordVal.intVal != prevVal:
          en.values.add fld
          prevVal = fld.ordVal.intVal
      else:
        inc prevVal
        en.values.add fld


  proc fldCmp(f1, f2: EnumField[PNode]): int {.closure.} =
    # NOTE possible source of incompatibility - only fields with
    # specified ordinal values should be sorted.
    if f1.kind == f2.kind and f1.kind == efvOrdinal:
      cmp(f1.ordVal.intVal, f2.ordVal.intVal)
    else:
      0

  en.values.sort(fldCmp)

  en.exported = true

  return en.toNNode(standalone = true)

proc wrapApiUnit*(
  api: CApiUnit, conf: WrapConfig, cache: var WrapCache): PNode =
  ## Generate wrapper for api unit.
  result = nnkStmtList.newPTree()
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
          # warn "Skipping", decl.cursor.cxType()
          # debug "Which is a specialization of", spec.cxKind()
        else:
          let (obj, procs, other) = decl.wrapObject(conf, cache)

          result.add obj.toNNode(true)

          for it in other:
            result.add it

          for pr in procs:
            result.add pr.toNNode()


        dedentLog()
      of cdkAlias:
        result.add decl.wrapAlias(decl.namespace, conf)
      of cdkEnum:
        result.add decl.wrapEnum(conf)
      else:
        # err "Not wrapping", decl.kind, decl.name
        discard

#*************************************************************************#
#********************  Dependency graph construction  ********************#
#*************************************************************************#
import gram, tables

const HeaderGraphFlags = toInt({
  Directed, ValueIndex, UniqueEdges, UniqueNodes})

type
  ParsedFile* = object
    unit*: CXTranslationUnit ## Translation unit
    filename*: AbsFile ## Name of the original file
    api*: CApiUnit ## File's API
    index*: CXIndex
    explicitDeps*: seq[AbsFile] ## Filenames in which types exposed in
    ## API are declared. Guaranteed to have every file listed once &
    ## no self-dependencies.

  HeaderDepGraph* = Graph[AbsFile, string, HeaderGraphFlags]

  ParseConfiguration* = object
    globalFlags*: seq[string] ## List of parse flags applied on each
    ## file parse. Mostly for things like include paths.
    fileFlags*: Table[AbsFile, seq[string]] ## List of parse flags
    ## specific only to particular file


  FileIndex* = object
    index*: Table[AbsFile, ParsedFile] ## Index of all parsed files
    depGraph*: HeaderDepGraph


proc getFlags*(config: ParseConfiguration, file: AbsFile): seq[string] =
  result.add config.globalFlags
  result.add config.fileFlags.getOrDefault(file)

proc parseFile*(
  file: AbsFile, config: ParseConfiguration,
  wrapConf: WrapConfig): ParsedFile =

  # info "File", file
  file.assertExists()
  identLog()

  let flags = config.getFlags(file)
  result.filename = file
  result.index = createIndex()
  try:
    result.unit = parseTranslationUnit(
      result.index, file, flags, {
        tufSkipFunctionBodies, tufDetailedPreprocessingRecord})
  except:
    error file.realpath
    debug config.getFlags(file).joinl()
    raise


  result.api = result.unit.splitDeclarations(wrapConf)
  # info "Explicit dependencies for", file
  result.explicitDeps = result.api.publicApi.
    getDepFiles().filterIt(it != file)

  # logIdented:
  #   for dep in result.explicitDeps:
  #     debug dep

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
    notice ifrm, " -> ", path
    graph.incl((ifrm, path), &"{dep.includedAs}:{dep.fromLine}",)


  for dep in parsed.explicitDeps:
    graph.incl(file.realpath)
    graph.incl(dep.realpath)
    info file, " -> ", dep
    graph.incl((file.realpath, dep.realpath), "@@@")


proc parseAll*(
  files: seq[AbsFile],
  conf: ParseConfiguration,
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
    nnkInfix.newPTree(
      newPIdent("/"),
      newPIdent(names[0]),
      newPIdent(names[1])
    ))

proc fixFileName*(name: string): string =
  name.multiReplace({
    "-": "_",
    "+": "p"
  })

proc makeCXStdImport*(path: AbsFile): seq[string] =
  let (dir, name, ext) = path.splitFile()
  @["cxstd", name.fixFileName()]

proc wrapFile*(
  parsed: ParsedFile, conf: WrapConfig, cache: var WrapCache): PNode =
  result = parsed.api.wrapApiUnit(conf, cache)

  var imports = nnkStmtList.newPTree()

  imports.add nnkConstSection.newPTree(
    nnkConstDef.newPTree(
      newPIdent("cxheader"),
      newEmptyPNode(),
      newPLit(parsed.filename.getStr())
    ))


  for node in parsed.explicitDeps.mapIt(
      conf.getImport(it, conf)).
      deduplicate().
      mapIt(it.makeImport()):
    imports.add node

  # for dep in parsed.explicitDeps:
  #   imports.add conf.getImport(dep, conf).makeImport()

  result = nnkStmtList.newPTree(imports, result)

func toIncludes*(files: seq[AbsDir]): seq[string] =
  for file in files:
    result.add file.getStr().addPrefix("-I")

proc wrapFile*(
  file: AbsFile, flags: seq[string],
  conf: WrapConfig, cache: var WrapCache):
  tuple[parsed: ParsedFile, wrapped: PNode] =

  let parsedConf = ParseConfiguration(
    globalFlags: getBuiltinHeaders().toIncludes(),
    fileFlags: { file : flags }.toTable()
  )

  result.parsed = parseFile(file, parsedConf, conf)
  result.wrapped = result.parsed.wrapFile(conf, cache)

proc wrapFile*(
  cmd: CXCompileCommand, extraFlags: seq[string],
  conf: WrapConfig, cache: var WrapCache
             ): tuple[parsed: ParsedFile, wrapped: PNode] =
  wrapFile(
    toAbsFile($cmd.getFilename(), true),
    extraFlags & cmd.getFlags(), conf, cache)

type
  WrapResult* = object
    parsed*: ParsedFile
    wrapped*: PNode
    infile*: AbsFile
    importName*: seq[string]

proc wrapAll*(
  files: seq[AbsFile],
  parseConf: ParseConfiguration,
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
         wrapConf.ignoreFile(dep.includedPath):
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
      wrapped: parsed.index[file].wrapFile(wrap, cache)
    )

  result.index = parsed

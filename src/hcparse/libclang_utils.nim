## Helper functions for libclang. This file should not be imported -
## it is `include`'d in the main `libclang` file.

when not defined(libclangIncludeUtils):
  import libclang


#==============================  includes  ===============================#
import bitops, strformat, macros, terminal, sugar, std/decls, strutils,
       sequtils, options, os, re, hashes

import hpprint, hpprint/hpprint_repr
import hmisc/other/hshell
import hnimast
import nimtraits
import hmisc/types/colorstring
import hmisc/[hexceptions, helpers]
import compiler/ast
import packages/docutils/rstast

#==========================  String conversion  ==========================#

proc `$`*(cxstr: CXString): string =
  let str = getCString(cxstr)
  result = $str
  disposeString(cxstr)

proc `$`*(cursor: CXCursor): string = $getCursorSpelling(cursor)
proc `$`*(cxtype: CXType): string = $getTypeSpelling(cxtype)
proc `$`*(cxkind: CXCursorKind): string = $getCursorKindSpelling(cxkind)

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
  $diag.formatDiagnostic(0)

#================================  Index  ================================#

proc createIndex*(
  excludeDeclarations: bool = false,
  showDiagnostics: bool = false): CXIndex =

  createIndex(excludeDeclarations.int, showDiagnostics.int)

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

#==========================  Translation unit  ===========================#

proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil



proc getBuiltinHeaders*(): seq[string] =
  ## According to clang `documentation <https://clang.llvm.org/docs/LibTooling.html#builtin-includes>`_
  ## libclang is needs additional precompiled headers paths in
  ## addition to default include.
  ##
  ## NOTE right now I have zero idea how it works on windows, so I
  ## will just hardcode unix-like paths.

  let version = ($getClangVersion()).split(" ")[2] # WARNING
  @[
    &"/usr/lib/clang/{version}/include"
  ]

proc parseTranslationUnit*(
  trIndex: CXIndex,
  filename: string,
  cmdline: seq[string] = @[],
  trOptions: set[CXTranslationUnit_Flags] = {tufSingleFileParse},
  reparseOnNil: bool = true): CXTranslationUnit =

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
    else:
      echo ($diag).toYellow()

  if hadErrors or (reparseOnNil and result.isNil):
    echo(&"""
Translation unit parse failed due to errors.
Compilation flags:
{cmdline.joinql()}
File:
  {filename}
      """)


    echo "Translation unit parse failed, repeating parse in verbose mode"

    let cmdline = @["-v"] & cmdline
    let argc = cmdline.len
    let cmdlineC = allocCSTringArray(cmdline)

    result = parseTranslationUnit(
      trIndex, filename.cstring, cmdlinec, cint(argc), nil, 0, cuint(flags))

    deallocCStringArray(cmdlineC)

    raiseAssert("Translation unit parse failed")


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

  var args = collect(newSeq):
    for arg in command.getArgs():
      let match = arg.startsWith("-I", "-std=")

      if match:
        arg

  args = extraFlags & getBuiltinHeaders().mapIt(&"-I{it}") & args

  let file = $command.getFilename()
  index.parseTranslationUnit(file, args, {})






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

proc getSpellingLocation*(cursor: CXCursor): tuple[
  file: string, line, column, offset: int] =

  let location = cursor.getCursorLocation()
  var
    file: CXFile
    line: cuint
    column: cuint
    offset: cuint

  location.getSpellingLocation(
    addr file, addr line, addr column, addr offset)

  result.file = $file.getFileName()
  result.line = line.int
  result.column = column.int
  result.offset = offset.int



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

proc argTypes*(cursor: CXType): seq[CXType] =
  for i in 0 ..< cursor.getNumArgTypes():
    result.add cursor.getArgType(cuint i)

proc retType*(cursor: CXCursor): CXType =
  cursor.expectKind({ckFunctionDecl, ckCXXMethod})
  cursor.cxType().getResultType()

proc argc*(cursor: CXCursor): int =
  ## Get number of arguments in cursor pointing to function
  cursor.cxType().argc()

proc nthArg*(cxtype: CXType, idx: int): CXType =
  ## Retrieve type nth argument of function type
  assert idx < cxtype.argc()
  getArgType(cxtype, cuint(idx))

#===========================  Type conversion  ===========================#


proc fromElaboratedPType*(cxtype: CXType): NType[PNode] =
  ($cxtype).
    dropPrefix("enum ").
    dropPrefix("struct ").
    dropPrefix("const "). # WARNING
    newPType()


proc toNType*(cxtype: CXType): tuple[ntype: NType[PNode], mutable: bool] =
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
    of tkInt: newPType("int")
    of tkVoid: newPType("void")
    of tkUInt: newPType("cuint")
    of tkLongLong: newPType("clonglong")
    of tkULongLong: newPType("culonglong")
    of tkDouble: newPType("cdouble")
    of tkULong: newPType("culong")
    of tkTypedef: newPType(($cxtype).dropPrefix("const ")) # XXXX typedef processing -
    of tkElaborated, tkRecord, tkEnum:
      fromElaboratedPType(cxtype)
    of tkPointer:
      case cxtype[].cxkind:
        of tkChar_S:
          newPType("cstring")
        of tkPointer:
          if cxtype[][].cxKind() == tkChar_S:
            newPType("cstringArray")
          else:
            newNType("ptr", [toNType(cxtype[]).ntype])
        of tkVoid:
          newPType("pointer")
        of tkFunctionProto:
          let (t, mut) = toNType(cxtype[])
          t
        else:
          newNType("ptr", [toNType(cxtype[]).ntype])
    of tkConstantArray:
      newNType(
        "array",
        @[
          newPType($cxtype.getNumElements()),
          toNType(cxtype.getElementType()).ntype
        ]
      )
    of tkFunctionProto:
      newProcNType[PNode](
        rtype = cxtype.getResultType().toNType().ntype,
        args = cxtype.argTypes.mapIt(it.toNType().ntype),
        pragma = newPPragma("cdecl")
      )
    of tkLValueReference:
      result.mutable = not (cxType.isConstQualifiedType() == 0)
      toNType(cxType[]).ntype
    else:
      echo "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newPType("!!!")

  result.ntype = restype
  result.mutable = mutable
  # echo cxtype.cxkind, " ", result.toNNode()




#===========================  Pretty-printing  ===========================#

proc objTreeRepr*(cxtype: CXType): ObjTree =
  case cxtype.cxKind:
    of tkPointer:
      pptObj("ptr", cxtype[].objTreeRepr())
    else:
      pptObj($cxtype.cxkind, pptConst($cxtype))


proc lispRepr*(cxtype: CXType): string =
  cxtype.objTreeRepr().lispRepr()


proc objTreeRepr*(cursor: CXCursor, tu: CXTranslationUnit): ObjTree =
  ## Generate ObjTree representation of cursor
  const colorize = not defined(plainStdout)
  let ctype = pptConst(
    "type: " & $cursor.cxType,
    initPrintStyling(fg = fgBlue,
                     style = {styleItalic, styleDim}))

  if cursor.len  == 0:
    pptObj($cursor.cxkind,
           initPrintStyling(fg = fgYellow),
           ctype,
           pptConst(
             cursor.tokens(tu).join(" "), initPrintStyling(fg = fgGreen)))
  else:
    pptObj(
      ($cursor.cxkind).toMagenta(colorize) & " " & $cursor,
      @[ctype] & toSeq(cursor.children).mapIt(it.objTreeRepr(tu))
    )


proc treeRepr*(cursor: CXCursor, tu: CXTranslationUnit): string =
  ## Generate pretty-printed tree representation of cursor.
  cursor.objTreeRepr(tu).treeRepr()

#*************************************************************************#
#***********************  Declaration conversion  ************************#
#*************************************************************************#
#=========================  C entry declaration  =========================#
when true:
  derive commonDerives:
    type
      CDeclKind* = enum
        cdkClass
        cdkStruct
        cdkEnum
        cdkFunction
        cdkMethod
        cdkField

      CArg* = object
        name*: string
        cursor*: CXCursor

      CXOperatorKind* = enum
        cxoPrefixOp ## Prefix operator `@a`
        cxoInfixOP ## Infix operator `a @ b`
        cxoAsgnOp ## Assign operator `a = b`
        cxoArrayOp ## Array access operator `a[b]`
        cxoArrowOp ## Arrow operator `a->`
        cxoCallOp ## Call operator `a()`


      CDecl* {.derive(GetSet).} = object
        ## Higher-level wrapper on top of CXCursor. Mostly used to
        ## provide more intuitive API for working with things to be
        ## wrapped.
        name*: string
        namespace*: seq[string]
        cursor* {.requiresinit.}: CXCursor
        case kind*: CDeclKind
          of cdkField:
            fldAccs* {.name(accs).}: CX_CXXAccessSpecifier
          of cdkMethod:
            metAccs* {.name(accs).}: CX_CXXAccessSpecifier
            metArgs* {.name(args).}: seq[CArg]
          of cdkFunction:
            funArgs* {.name(args).}: seq[CArg]
          of cdkClass, cdkStruct:
            members*: seq[CDecl]
          else:
            nil

#======================  Accessing CDecl elements  =======================#
func arg*(cd: CDecl, idx: int): CArg = cd.args()[idx]
func member*(cd: CDecl, idx: int): CDecl = cd.members[idx]
func methods*(cd: CDecl, kinds: set[CXCursorKind]): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for member in cd.members:
    if (member.kind == cdkMethod) and (member.cursor.cxKind in kinds):
      result.add member

func pubFields*(cd: CDecl): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for member in cd.members:
    if (member.kind == cdkField) and (member.accs == asPublic):
      result.add member

func namespaceName*(cd: CDecl): string =
  (cd.namespace & @[cd.name]).join("::")

func isOperator*(cd: CDecl): bool =
  cd.kind == cdkMethod and
  cd.name.startsWith("operator") and
  (not cd.name.validIdentifier())

func classifyOperator*(cd: CDecl): CXOperatorKind =
  assert cd.isOperator()
  let name = cd.name.dropPrefix("operator")
  case name:
    of "+=":
      cxoAsgnOp
    else:
      raiseAssert("#[ IMPLEMENT ]#")


func getNimName*(cd: CDecl): string =
  case cd.kind:
    of cdkMethod:
      if cd.isOperator():
        cd.name.dropPrefix("operator")
      else:
        cd.name
    else:
      cd.name


#======================  Converting to nim entries  ======================#

proc fixIdentName(str: string): string =
  result = if str[0].isLowerAscii():
    str
  else:
    str[0].toLowerAscii() & str[1..^1]

  result = case result:
    of "set": "cxset"
    of "type": "cxtype"
    of "range": "cxrange"
    of "string": "cxstring"
    of "begin": "cxbegin"
    of "end": "cxend"
    else: result



proc dropPOD*(cxtype: CXType): string =
  case cxtype.cxKind:
    of tkElaborated:
      cxtype.fromElaboratedPType().head
    of tkPointer:
      cxtype[].dropPOD()
    of tkTypedef:
      ($cxtype).dropPrefix("const ")
    else:
      ""

proc toPIdentDefs*(cursor: CXCursor): PIdentDefs =
  result.varname = $cursor
  if result.varname.len == 0:
    result.varname = "arg" & $cursor.cxType().dropPOD()

  result.varname = result.varname.fixIdentName()

  let (ctype, mutable) = cursor.cxType.toNType()
  result.vtype = ctype
  if mutable:
    result.kind = nvdVar

proc convertCFunction*(cursor: CXCursor): ProcDecl[PNode] =
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
          var res = it.toPIdentDefs()
          if res.varname == prevName:
            res.varname &= "1"

          prevName = res.varname
          res

    # NOTE dropping mutability from return type
    it.signature.setRType toNType(cursor.retType()).ntype
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


#===========  Splitting translation unit into logical chunks  ============#
proc getArguments(cursor: CXCursor): seq[CArg] =
  for idx, subn in cursor.children():
    if subn.cxKind in {ckParmDecl}:
      var name = $subn
      if name.len == 0:
        name = "a" & $idx

      result.add CArg(name: name, cursor: subn)

proc visitMethod(cursor: CXCursor, accs: CX_CXXAccessSpecifier): CDecl =
  result = CDecl(kind: cdkMethod, cursor: cursor)
  result.name = $cursor
  result.accs = accs
  result.args = cursor.getArguments()


proc visitField(cursor: CXCursor, accs: CX_CXXAccessSpecifier): CDecl =
  result = CDecl(kind: cdkField, cursor: cursor)
  result.name = $cursor
  result.accs = accs

proc visitClass(cursor: CXCursor, parent: seq[string]): CDecl =
  ## Convert class under cursor to `CDecl`
  cursor.expectKind({ckClassDecl, ckClassTemplate,
                      ckClassTemplatePartialSpecialization})
  result = CDecl(kind: cdkClass, cursor: cursor)
  result.name = $cursor
  result.namespace = parent
  # echo "found class ", cursor

  var currentAccs = asPrivate
  for subn in cursor:
    # echo "found subnode ", subn
    case subn.cxKind:
      of ckCXXMethod, ckConstructor, ckDestructor, ckConversionFunction:
        result.members.add visitMethod(subn, currentAccs)
      of ckCXXAccessSpecifier:
        currentAccs = subn.getCXXAccessSpecifier()
      of ckFieldDecl:
        result.members.add visitField(subn, currentAccs)
      of ckTemplateTypeParameter:
        discard
      else:
        echo "IMPLEMENT: ", ($subn.cxKind).toRed(), " ", subn
        discard

proc visitFunction(cursor: CXCursor, parent: seq[string]): CDecl =
  result = CDecl(kind: cdkFunction, cursor: cursor, namespace: parent)
  result.name = $cursor
  result.args = cursor.getArguments()


proc visitCursor(cursor: CXCursor, parent: seq[string]): tuple[
  decls: seq[CDecl], recurse: bool]

proc visitNamespace(cursor: CXCursor, parent: seq[string]): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.
  for subn in cursor:
    result.add visitCursor(subn, parent & @[ $cursor ]).decls

proc visitCursor(cursor: CXCursor, parent: seq[string]): tuple[
  decls: seq[CDecl], recurse: bool] =

  result.decls.add case cursor.cxKind:
    of ckNamespace:
      visitNamespace(cursor, parent)
    of ckClassDecl, ckClassTemplate, ckClassTemplatePartialSpecialization:
      @[ visitClass(cursor, parent) ]
    of ckFunctionDecl:
      @[ visitFunction(cursor, parent) ]
    else:
      # echo "recursing on: ", cursor.cxKind
      result.recurse = true
      return


proc splitDeclarations*(tu: CXTranslationUnit): CApiUnit =
  ## Convert main file of translation unit into flattened sequence of
  ## high-level declarations. All cursors for objects/structs are
  ## retained. Public API elements are stored in `publicAPI` field
  assert not tu.isNil
  let cursor = tu.getTranslationUnitCursor()
  var res: CApiUnit
  cursor.visitMainFile do:
    makeVisitor [tu, res]:
      let (decls, rec) = visitCursor(cursor, @[])
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
proc getHCParseBinDir*(): string =
  ## Return absolute path `/bin` directory with helper cmdline tools;
  ## NOTE right now I have no idea how to handle dependencies like
  ## this - this is just a hacky solution.
  for dir in currentSourcePath().parentDirs():
    if (dir / "hcparse.nimble").fileExists():
      return dir / "bin"

  raise newException(
    IOError,
    "Could not find `hcparse.nimble` in any of the parent directories")

proc getHCParseBinPath*(name: string): string =
  ## Return absolute name of the helper cmdline tool `name`
  let bindir = getHCParseBinDir()
  let file = bindir / name
  if fileExists(file):
    return file
  else:
    raise newException(IOError, "Could not find '" & file & "'")

# ~~~~ Dependency tree construction ~~~~ #

type
  CDepsTree* = object
    file*: string
    name*: string
    deps*: seq[CDepsTree]

proc parseBuildDepsTree*(outs: string): CDepsTree =
  let depLines = outs.split("\n")
  var idx = 0
  echo "deps list has ", depLines.len,  " lines"

  proc auxTree(): seq[CDepsTree] {.closure.} =
    while not depLines[idx].startsWith(Whitespace, "}"):
      if deplines[idx] =~ re".*?<(.*?)> (.*)":
        inc idx
        result.add CDepsTree(name: matches[0], file: matches[1])
      elif deplines[idx] =~ re""".*?"(.*?)" (.*)""":
        inc idx
        result.add CDepsTree(name: matches[0], file: matches[1])
      elif depLines[idx].startsWith(Whitespace, "{"):
        inc idx
        # echo "startin level ", depLines[idx]
        result.last.deps = auxTree()
      elif depLines[idx].isEmptyOrWhitespace():
        inc idx
        return

    inc idx


  return CDepsTree(deps: auxTree())

proc buildDepsTree*(file: string, args: seq[string]): CDepsTree =
  let bin = getHCParseBinPath("deps")
  assert file.existsFile()
  let command = bin & " " & args.joinw() & " " & file

  try:
    let (outs, _, _) = runShell(command)
    result = parseBuildDepsTree(outs)
    result.file = file


  except ShellError:
    printShellError()
    echo "Arguments:"
    echo args.joinql()


proc immediateDeps*(d: CDepsTree): seq[string] =
  d.deps.mapIt(it.file)

# ~~~~ Collecting dependeny list ~~~~ #

proc getDepFiles*(deps: seq[CXCursor]): seq[string] =
  ## Generate list of files that have to be wrapped
  # TODO:DOC
  for dep in deps:
    let decl: CXCursor =
      case dep.cxKind:
        of ckFunctionDecl:
          dep.retType().getTypeDeclaration()
        else:
          dep.cxType.getTypeDeclaration()

    let (file, line, column, _) = decl.getSpellingLocation()

    result.add file

  result = result.deduplicate()



#*************************************************************************#
#*************************  Wrapper generation  **************************#
#*************************************************************************#
type
  WrapConfig* = object
    header*: string

proc wrapMethods*(
  cd: CDecl, conf: WrapConfig, parent: NType[PNode]): seq[PProcDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for meth in cd.methods({ckCXXMethod}):
    let procdef = PProcDecl().withIt do:
      # TODO set `exported` and `comment`
      it.name = meth.getNimName()
      it.genParams = parent.genParams

      it.signature = newProcNType[PNode](@[])
      it.exported = (meth.accs == asPublic)

      let addThis =
        if meth.isOperator():
          let kind = meth.classifyOperator()
          it.kind = pkOperator

          case kind:
            of cxoAsgnOp:
              it.signature.pragma = newPPragma(
                newPIdentColonString("importcpp", &"# {it.name} #"),
                newPIdentColonString("header", conf.header)
              )
            else:
              raiseAssert("#[ IMPLEMENT ]#")



          kind in {cxoAsgnOp}
        else:
          it.signature.pragma = newPPragma(
            newPIdentColonString("importcpp", &"#.{it.name}(@)"),
            newPIdentColonString("header", conf.header)
          )



          true

      if addThis:
        it.signature.arguments.add PIdentDefs(
          varname: "self",
          vtype: parent,
          kind: meth.cursor.isConstMethod.tern(nvdVar, nvdLet)
        )

      for arg in meth.args:
        let (vtype, mutable) = arg.cursor.cxType().toNType()
        it.signature.arguments.add PIdentDefs(
          varname: arg.name,
          vtype: vtype,
          kind: mutable.tern(nvdVar, nvdLet))

      it.signature.setRtype toNType(meth.cursor.retType()).ntype


    result.add procdef

proc wrapObject*(cd: CDecl, conf: WrapConfig): tuple[
  obj: PObject, procs: seq[PProcDecl]] =
  assert cd.kind in {cdkClass, cdkStruct}
  result.obj = PObject(name: newPType(cd.name), exported: true)
  # WARNING might die on `<T<T<T<T<T<T>>>>>` things
  for genParam in cd.cursor.children({ckTemplateTypeParameter}):
    result.obj.name.genParams.add newPType($genParam)

  for fld in cd.pubFields:
    result.obj.flds.add PField(
      isTuple: false,
      name: fld.name,
      exported: true,
      fldType: fld.cursor.cxType().toNType().ntype
    )

  result.obj.annotation = some(newPPragma(
    newPIdentColonString("importcpp", cd.namespaceName()),
    newPIdentColonString("header", conf.header),
  ))

  result.procs = cd.wrapMethods(conf, result.obj.name)

when isMainModule:
  import hpprint
  pprint (parseBuildDepsTree """
<zzz> /tmp/zzz
{
  <eee> /tmp/ee
  <1> /tmp/werrwe
  {
    <1.1> /tmp/ee23
    <1.2> /tmp/werrfsf34we
    <1.3> /tmp/ewewffv234
  }
  <2> /tmp/ewewffv
  {
    <2.1> /tmp/ee1233
    <2.2> /tmp/werrwe23
    <2.3> /tmp/ewewffv
  }
}
"""), maxwidth = 100

#*************************************************************************#
#********************  Dependency graph construction  ********************#
#*************************************************************************#
import gram, tables

const HeaderGraphFlags = toInt({
  Directed, ValueIndex, UniqueEdges, UniqueNodes})

type
  IncludeDep* = object
    # TODO use it as edge value
    includedAs*: string
    includedPath*: string
    includedFrom*: string

  ParsedFile* = object
    unit*: CXTranslationUnit ## Translation unit
    filename*: string ## Name of the origina file
    api*: CApiUnit ## File's API
    index*: CXIndex
    explicitDeps*: seq[string] ## Filenames in which types exposed in
    ## API are declared. Guaranteed to have every file listed once &
    ## no self-dependencies.

  HeaderDepGraph* = Graph[string, bool, HeaderGraphFlags]

  ParseConfiguration* = object
    globalPaths*: seq[string]
    fileFlags*: Table[string, seq[string]]


  FileIndex* = object
    index*: Table[string, ParsedFile]
    depGraph*: HeaderDepGraph


func getFlags*(config: ParseConfiguration, file: string): seq[string] =
  for path in config.globalPaths:
    result.add path.addPrefix("-I")

  result.add config.fileFlags.getOrDefault(file)

proc parseFile*(file: string, config: ParseConfiguration): ParsedFile =
  let flags = config.getFlags(file)
  result.index = createIndex()
  result.unit = parseTranslationUnit(
    result.index, file, flags, {tufSkipFunctionBodies})

  result.api = result.unit.splitDeclarations()
  result.explicitDeps = result.api.publicApi.
    getDepFiles().filterIt(it != file)

proc parseAll*(files: seq[string], conf: ParseConfiguration): FileIndex =
  for file in files:
    result.index[file] = parseFile(file, conf)

  result.depGraph = newGraph[string, bool](HeaderGraphFlags)

  for file, parsed in result.index:
    for dep in parsed.explicitDeps:
      discard result.depGraph.add(file)
      discard result.depGraph.add(dep)
      discard result.depGraph.edge(
        result.depGraph[file], true, result.depGraph[dep])


import hasts/graphviz_ast
export toPng

func dotRepr*(idx: FileIndex): graphviz_ast.Graph =
  result.styleNode = makeRectConsolasNode()

  for file in idx.depGraph.nodes:
    result.addNode(makeNode(hash file.value, file.value))

  for (source, _, target) in idx.depGraph.edges:
    result.addEdge makeEdge(hash source.value, hash target.value)

import
  ./libclang_wrap, ./cxvisitors
import
  std/[strformat, strutils, options, sequtils, sugar, deques, hashes]

import
  packages/docutils/rstast
import
  hmisc/other/[oswrap, hlogger],
  hmisc/algo/[clformat],
  hmisc/types/colorstring,
  hmisc/helpers


import
  hpprint/hpprint_repr, hpprint, hnimast

export libclang_wrap

## Convenience wrappers for libclang types - stringification, getting kind
## and so on.

#*************************************************************************#
#******************************  CXString  *******************************#
#*************************************************************************#
proc `$`*(cxstr: CXString): string =
  let str = getCString(cxstr)
  result = $str
  disposeString(cxstr)

#*************************************************************************#
#****************************  CXSourceRange  ****************************#
#*************************************************************************#
proc `$`*(cxRange: CXSourceRange): string =
  &"[{cxRange.beginIntData}, {cxRange.endIntData}]"


#*************************************************************************#
#*******************************  CXFile  ********************************#
#*************************************************************************#
proc `$`*(file: CXFile): string =
  $getFileName(file)


#*************************************************************************#
#**************************  CXSourceLocation  ***************************#
#*************************************************************************#
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



#*************************************************************************#
#*******************************  CXType  ********************************#
#*************************************************************************#
proc `$`*(cxtype: CXType): string =
  if cxtype.kind == tkInvalid:
    "<invalid>"
  else:
    $getTypeSpelling(cxtype)

proc `==`*(t1, t2: CXType): bool = (equalTypes(t1, t2) != 0)
proc isConstQualified*(t: CXType): bool = isConstQualifiedType(t) != 0

proc `[]`*(t: CXType): CXType = getPointeeType(t)


proc cxKind*(cxtype: CXType): CXTypeKind =
  ## Get type kind
  cxtype.kind

proc expectKind*(cxtype: CXType, kind: CXTypeKind) =
  ## Raise assertion if cursor kind does not match
  if cxtype.cxKind != kind:
    raiseAssert(&"Expected type kind {kind}, but got {cxtype.cxKind()}")

proc argTypes*(cursor: CXType): seq[CXType] =
  for i in 0 ..< cursor.getNumArgTypes():
    result.add cursor.getArgType(cuint i)

proc genParams*(cxtype: CXType): seq[CXType] =
  ## Get list of generic parameters for a type
  # DOC just any type or only generic instantiation?
  # TODO maybe mark generated parameters as defaulted/non-defaulted?
  let args = cxtype.getNumTemplateArguments()
  if args > 0:
    for i in 0 ..< args:
      result.add cxtype.getTemplateArgumentAsType(i.cuint)

const tkPODKinds* = {
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

proc isConstQualifiedDeep*(cxtype: CXType): bool =
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

proc templateParams*(cxType: CxType): seq[CxType] =
  for i in 0 ..< getNumTemplateArguments(cxType):
    result.add cxType.getTemplateArgumentAsType(i.cuint)


#*************************************************************************#
#*******************************  CXToken  *******************************#
#*************************************************************************#
proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil


proc tokens*(cursor: CXCursor, tu: CXTranslationUnit): seq[CXToken] =
  ## Get sequence of tokens that make up cursor.
  let range: CXSourceRange   = getCursorExtent(cursor);
  var tokens: ptr[CXToken]
  var nTokens: cuint = 0
  assert not tu.isNil
  tokenize(tu, range, addr tokens, addr nTokens)

  for i in 0 ..< nTokens:
    result.add (cast[ptr UncheckedArray[CXToken]](tokens))[i]

proc tokenStrings*(cursor: CXCursor, tu: CXTranslationUnit): seq[string] =
  for tok in cursor.tokens(tu):
    result.add $getTokenSpelling(tu, tok)

proc tokenKinds*(cursor: CXCursor, tu: CXTranslationUnit):
  seq[(string, CXTokenKind)] =

  for tok in cursor.tokens(tu):
    result.add ($getTokenSpelling(tu, tok), tok.getTokenKind())

proc annotateTokens*(cursor: CXCursor, tu: CXTranslationUnit):
  seq[tuple[tok: CXToken, cursor: CXCursor]] =
  var tokens = cursor.tokens(tu)
  var cursors = newSeq[CXCursor](tokens.len)

  annotateTokens(tu, addr tokens[0], tokens.len.cuint, addr cursors[0])

  result = zip(tokens, cursors)

proc str*(tok: CXToken, tu: CXTranslationUnit): string =
  $getTokenSpelling(tu, tok)

proc cxKind*(cxTok: CXToken): CXTokenKind =
  cxtok.getTokenKind()

proc fromTokens*(toks: seq[CXToken], unit: CXTranslationUnit): string =
  ## Perform crude conversion of the tokens back to string
  var prevLine = -1
  var prevCol = -1
  for tok in toks:
    let str = $getTokenSpelling(unit, tok)
    let loc = getTokenLocation(unit, tok).getExpansionLocation().get()
    if prevLine < loc.line:
      prevLine = loc.line
      result.add "\n"
      prevCol = -1

    if prevCol < loc.column:
      result.add " "

    result.add str
    prevCol = loc.column + str.len



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

proc getFile*(tu: CXTranslationUnit): AbsFile =
  AbsFile($tu.getTranslationUnitSpelling())

proc getTuFile*(cx: CXCursor): AbsFile =
  assert cx.kind == ckTranslationUnit
  AbsFile($getCursorSpelling(cx))

#*************************************************************************#
#******************************  CXCursor  *******************************#
#*************************************************************************#
proc `$`*(cxkind: CXCursorKind): string =
  case cxkind:
    of ckBaseSpecifier:        "ckBaseSpecifier"
    of ckAlignedAttr:          "ckAlignedAttr"
    of ckMacroDefinition:      "ckMacroDefinition"
    of ckMacroExpansion:       "ckMacroExpansion"
    of ckFinalAttr:            "ckFinalAttr"
    of ckAccessSpecifier:      "ckAccessSpecifier"
    of ckNullPtrLiteralExpr:   "ckNullPtrLiteralExpr"
    of ckWarnUnusedAttr:       "ckWarnUnusedAttr"
    of ckWarnUnusedResultAttr: "ckWarnUnusedResultAttr"
    of ckConversionFunction:   "ckConversionFunction"
    of ckMethod:               "ckMethod"
    of ckConstructor:          "ckContructor"
    of ckDestructor:           "ckDestructor"
    of ckClassTemplate:        "ckClassTemplate"
    of ckOverloadCandidate:    "ckOverloadCandidate"

    else:
      $getCursorKindSpelling(cxkind)

proc `$`*(cursor: CXCursor): string =
  case cursor.kind:
    of ckOverloadCandidate:
      "<OVERLOAD CANDIDATE>"

    else:
      $getCursorSpelling(cursor)



proc hash*(cursor: CXCursor): Hash = Hash(cursor.hashCursor())

#===========================  Kind/Type acess  ===========================#

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

proc rawComment*(cursor: CXCursor): string =
  $cursor.getRawCommentText()

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


iterator items*(
    cursor: CXCursor,
    filter: set[CXCursorKind] = {low(CXCursorKind) .. high(CXCursorKind)}
  ): CXCursor =

  for child in cursor.children():
    if child.kind in filter:
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

proc isForward*(cursor: CXCursor): bool =
  isCursorDefinition(cursor) == 0

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




proc argc*(cxtype: CXType): int =
  ## Get numbler of arguments in function type
  cxtype.expectKind(tkFunctionProto)
  getNumArgTypes(cxtype).int

proc params*(cursor: CXCursor): seq[CXCursor] =
  for ch in cursor:
    if ch.kind == ckParmDecl:
      result.add ch

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

proc argTypes*(cursor: CXCursor): seq[CXType] =
  cursor.cxType().argTypes()



#=============================  Predicates  ==============================#

proc isConstMethod*(cursor: CXCursor): bool =
  ## Return true if cursor is a class method with `const`
  ## qualification. No exception is raised on invalid method
  (cursor.kind in {ckMethod}) and (methodIsConst(cursor) == 0)

proc isFromMainFile*(cursor: CXCursor): bool =
  ## Return true if cursor posints to main file
  let location = cursor.getCursorLocation()
  result = location.locationIsFromMainFile() != cint(0)

proc isStatic*(cursor: CXCursor): bool =
  cursor.methodIsStatic() == 1


proc `==`*(c1, c2: CXCursor): bool = equalCursors(c1, c2) == 1


#========================  Location information  =========================#

type CxLocation = tuple[file: AbsFile, line, column, offset: int]

proc getSpellingLocation*(cursor: CXCursor): Option[CxLocation] =
  result = cursor.getCursorLocation().getExpansionLocation()

proc `$`*(loc: CxLocation): string =
  &[hshow(loc.file), ":", hshow(loc.line), ":", hshow(loc.column)]

proc relSpellingFile*(cursor: CXCursor): RelFile =
  cursor.getSpellingLocation().get().file.splitFile2().file.RelFile()


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
  var cnt: int = 0
  while true:
    parent = parent.getCursorSemanticParent()
    if parent == ancestor:
      return true

    elif parent.cxKind in {
      ckTranslationUnit, ckInvalidFile
    }:
      return false

    else:
      inc cnt
      if cnt > 100:
        raiseImplementError("")

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
#******************************  CXComment  ******************************#
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
#***************************  Pretty-printing  ***************************#
#*************************************************************************#

func add*(tree: var ObjTree, other: ObjTree) =
  tree.fldPairs.add ("", other)

func add*(tree: var ObjTree, arg: string, other: ObjTree) =
  tree.fldPairs.add (arg, other)

proc objTreeRepr*(cxtype: CXType): ObjTree =
  case cxtype.cxKind:
    of tkPointer:
      result = pptObj("ptr", cxtype[].objTreeRepr())

    of tkLValueReference:
      result = pptObj("lvref", cxtype[].objTreeRepr())

    of tkRValueReference:
      result = pptObj("rvref", cxtype[].objTreeRepr())

    of tkPodKinds:
      result = pptConst("'" & $cxtype & "'")

    of tkFunctionProto:
      result = pptObj($cxType.cxKind, { "func": pptConst($cxtype) })
      result.add("result", objTreeRepr(cxType.getResultType()))

      for idx, arg in argTypes(cxType):
        result.add($idx, objTreeRepr(arg))

    else:
      result = pptObj(
        $cxtype.cxkind, pptConst("'" & $cxtype & "'", fgRed + bgDefault))

  if cxType.kind notin tkPodKinds:
    if isConstQualified(cxtype):
      result.add pptConst("const", fgMagenta + bgDefault)

  for param in cxType.genParams():
    result.add objTreeRepr(param)


proc lispRepr*(cxtype: CXType): string =
  cxtype.objTreeRepr().lispRepr()

proc treeRepr*(cxtype: CXType): string =
  cxtype.objTreeRepr().treeRepr()

proc neededTemplateArgs*(cxtype: CXType): int =
  ## Get number of necessary generic arguments for a type
  let cursor = cxtype.getTypeDeclaration()

proc dedentComment*(str: string): string =
  str.split('\n').mapIt(it.dedent()).join("\n")

proc objTreeRepr*(
    cursor: CXCursor, tu: CXTranslationUnit,
    showtype: bool = true,
    showcomment: bool = true
  ): ObjTree =
  ## Generate ObjTree representation of cursor
  const colorize = not defined(plainStdout)
  var typeText = "type: " & $cursor.cxType & " " & $cursor.cxType().cxKind()

  if cursor.cxKind() in {ckTemplateRef}:
    let specialized = cursor.getSpecializedCursorTemplate()
    typeText = "ref of: " & $specialized.cxKind()

  let ctype = pptConst(
    typeText,
    initPrintStyling(fg = fgBlue, style = {styleItalic, styleDim})
  )


  var commentText = cursor.rawComment()
  let showComment = showcomment and commentText.len > 0
  let comment = pptConst(
    commentText.dedentComment(), initStyle(styleItalic, fgCyan))

  let locRange =
    block:
      let loc = getSpellingLocation(cursor)
      if loc.isSome():
        &" {loc.get().line}:{loc.get().column}"

      else:
        ""

  if cursor.len == 0:
    let val = pptconst(
      cursor.tokens(tu).mapIt(
        getTokenSpelling(tu, it)
      ).join(" "), initprintstyling(fg = fggreen))

    var flds: seq[ObjTree]
    if showtype: flds.add ctype
    if showComment: flds.add comment
    flds.add val
    # @[ctype, val] else: @[val]

    if cursor.cxKind in {ckMacroExpansion}:
      let cxRange =
        $getCursorLocation(cursor).getExpansionLocation() & " " &
          $getCursorExtent(cursor)

      flds.add pptconst(
        $cxRange, initprintstyling(fg = fgBlue))

    pptObj(
      "[_] " & $cursor.cxkind & locRange,
      initPrintStyling(fg = fgYellow), flds)
  else:
    var children: seq[ObjTree]
    for node in cursor.children:
      if not (
        cursor.cxKind() == ckTranslationUnit and
        node.cxKind() in {ckMacroDefinition, ckMacroExpansion}
      ):
        children.add objTreeRepr(node, tu, showType)

    var suffix: string

    if cursor.cxKind() == ckEnumConstantDecl:
      suffix &= " " & toRed($cursor.getEnumConstantDeclValue())

    pptObj(
      &[("[*] " & $cursor.cxkind).toMagenta(colorize), " ", $cursor, $suffix,
        locRange
      ],
      &[showtype.tern(@[ctype], @[]),
        showcomment.tern(@[comment], @[]),
        children
      ]
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

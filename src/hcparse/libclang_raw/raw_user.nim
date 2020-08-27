# {.define(plainStdout).}
import macros, sugar, strformat, lenientops, bitops, sequtils, options,
       terminal, shell, strutils, parseutils, sets

import hpprint, hpprint/hpprint_repr

import hmisc/[hexceptions, hdebug_misc]
import hmisc/helpers
import hnimast
import hmisc/types/colorstring

import compiler/[ast, lineinfos]
import packages/docutils/rstast
import std/decls

import index, documentation, cxstring

proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil


#==========================  String conversion  ==========================#

proc `$`*(cxstr: CXString): string =
  let str = clang_getCString(cxstr)
  result = $str
  clang_disposeString(cxstr)

proc `$`*(cursor: CXCursor): string = $clang_getCursorSpelling(cursor)
proc `$`*(cxtype: CXType): string = $clang_getTypeSpelling(cxtype)
proc `$`*(cxkind: CXCursorKind): string =
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

proc expectKind*(cxtype: CXType, kind: CXTypeKind) =
  if cxtype.cxKind != kind:
    raiseAssert(&"Expected type kind {kind}, but got {cxtype.cxKind()}")

proc argc*(cxtype: CXType): int =
  cxtype.expectKind(CXType_FunctionProto)
  clang_getNumArgTypes(cxtype).int

proc argc*(cursor: CXCursor): int =
  cursor.cxType().argc()

proc nthArg*(cxtype: CXType, idx: int): CXType =
  assert idx < cxtype.argc()
  clang_getArgType(cxtype, cuint(idx))


#===========================  Location checks  ===========================#

proc isFromMainFile*(cursor: CXCursor): bool =
  let location = cursor.clang_getCursorLocation()
  var
    file: CXFIle
    line: cuint
    column: cuint
    offset: cuint
    # filename: cstring

  clang_getSpellingLocation(
    location,
    addr file,
    addr line,
    addr column,
    addr offset
  )

  result = location.clang_Location_isFromMainFile() != cint(0)
  let path = $file.clang_File_tryGetRealPathName()

#==========================  Subnode visitors  ===========================#

proc toClientData*[T](val: var T): CXClientData =
  CXClientData(addr val)

proc toCursorVisitor*(
  impl: proc(cursor, parent: CXCursor,
             clientData: pointer): CXChildVisitResult {.cdecl.}): CXCursorVisitor =
  CXCursorVisitor(impl)


proc clang_visitChildren*[T](
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

macro makeVisitor*(captureVars, body: untyped): untyped =
  var immutableCopies = newStmtList()
  captureVars.assertNodeKind({nnkBracket})
  let tupleData = nnkPar.newTree: collect(newSeq):
    for capture in captureVars:
      capture.assertNodeKind({nnkIdent})
      immutableCopies.add quote do:
        when not isMutable(`capture`):
          var `capture` = `capture`

      newColonExpr(capture, newCall("addr", capture))

  let byaddrid = ident "byaddr"
  let dataUnpack = newStmtList: collect(newSeq):
    for capture in captureVars:
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

  # echo $!result


proc visitMainFile*[T](
  cursor: CXCursor,
  callback: tuple[data: T, impl: CXCursorVisitor]) =
  var mainParent = cursor
  cursor.clang_visitChildren do:
    makeVisitor [callback, mainParent]:
      # echo cursor, " main file: ", cursor.isFromMainFile()
      if cursor.isFromMainFile():
        return callback.impl(cursor, mainParent, addr callback.data)
      else:
        return CXChildVisit_Recurse


      # return CXChildVisit_Recurse




#=========================  Filtering subnodes  ==========================#

proc children*(cursor: CXCursor): seq[CXCursor] =
  var buf: seq[CXCursor]
  var ddd: int
  cursor.clangVisitChildren do:
    makeVisitor [buf, ddd]:
      buf.add cursor
      return CXChildVisit_Continue

  return buf

proc `[]`*(cursor: CXCursor, idx: int): CXCursor =
  var
    res: CXCursor
    cnt = 0

  cursor.clangVisitChildren do:
    makeVisitor [res, cnt, idx]:
      if cnt == idx:
        res = cursor
        return CXChildVisit_Break
      else:
        inc cnt
        return CXChildVisit_Continue

  if cnt < idx:
    raiseAssert("Cursor {cursor} no child at index {idx}")
  else:
    return res

proc len*(cursor: CXCursor): int =
  var cnt = 0
  cursor.clangVisitChildren do:
    makeVisitor [cnt]:
      inc cnt
      return CXChildVisit_Continue

  return cnt

#============================  Documentation  ============================#

proc comment*(cursor: CXCursor): CXComment =
  cursor.clang_Cursor_getParsedComment()

proc cxKind*(comment: CXComment): CXCommentKind =
  comment.clang_Comment_getKind()

proc `$`*(comment: CXComment): string =
  $comment.clang_FullComment_getAsHTML() # NOTE it seems like libclang
  # API does not provide a way to get full comment string as raw string.

iterator children*(comment: CXComment): CXComment =
  for i in 0..clang_Comment_getNumChildren(comment):
    yield comment.clang_Comment_getChild(cuint(i))

proc len*(comment: CXComment): int =
  clang_Comment_getNumChildren(comment).int

proc `[]`*(comment: CXComment, idx: int): CXComment =
  assert idx < comment.len
  clang_Comment_getChild(comment, cuint(idx))

proc objTreeRepr*(comment: CXComment): ObjTree =
  case comment.cxKind:
    of CXComment_Text:
      pptConst $comment.clang_TextComment_getText()
    of CXComment_ParamCommand:
      pptObj(
        $comment.cxKind,
        [(
          $clang_ParamCommandComment_getParamName(comment),
          pptSeq(toSeq(comment.children).mapIt(it.objTreeRepr()))
          # objTreeRepr(comment[0])
        )]
      )
    of CXComment_InlineCommand:
      let args: seq[string] = collect(newSeq):
        for i in 0 ..< clang_InlineCommandComment_getNumArgs(comment):
          $clang_InlineCommandComment_getArgText(comment, cuint(i))

      pptObj(
        $comment.cxKind,
        [(
          $clang_InlineCommandComment_getCommandName(comment),
          pptSeq(
            args.mapIt(it.pptConst) &
            toSeq(comment.children).mapIt(it.objTreeRepr())
          )
        )]
      )
    of CXComment_BlockCommand:
      pptObj(
        $comment.cxKind,
        [(
          $clang_BlockCommandComment_getCommandName(comment),
          pptSeq(toSeq(comment.children).mapIt(it.objTreeRepr()))
          # objTreeRepr(comment[0])
        )]
      )
    else:
      if comment.len == 0:
        pptObj($comment.cxKind,
               pptConst($comment.clang_FullComment_getAsXML()))
      else:
        pptObj($comment.cxKind,
               toSeq(comment.children).mapIt(it.objTreeRepr()))

proc retType*(cursor: CXCursor): CXType =
  cursor.expectKind(CXCursor_FunctionDecl)
  cursor.cxType().clang_getResultType()


proc argTypes*(cursor: CXType): seq[CXType] =
  # cursor.expectKind(CXCursor_FunctionDecl)
  for i in 0 ..< cursor.clang_getNumArgTypes():
    result.add cursor.clang_getArgType(cuint i)
  # cursor.cxType().clang_getResultType()

proc newRstNode*(
  kind: RstNodeKind, subnodes: varargs[PRstNode]): PRstNode =
  result = rstast.newRstNode(kind)
  for node in subnodes:
    result.add node

proc `$`(n: PRstNode): string = renderRstToRst(n, result)

proc toRstNode*(comment: CXComment): PRstNode =
  case comment.cxKind:
    of CXComment_Text:
      return rnLeaf.newRstNode(
        $clang_TextComment_getText(comment))

    of CXComment_FullComment:
      result = rnInner.newRstNode()
      for subnode in comment.children():
        result.add rnParagraph.newRstNode(subnode.toRstNode())

    of CXComment_Paragraph:
      result = rnInner.newRstNode()
      for subnode in comment.children():
        result.add rnParagraph.newRstNode(subnode.toRstNode())

    of CXComment_Null:
      return rnLeaf.newRstNode("")

    of CXComment_InlineCommand:
      return rnInner.newRstNode("")

    of CXComment_ParamCommand:
      let pn = $clang_ParamCommandComment_getParamName(comment)
      # echo "param: ", pn
      result = rnInner.newRstNode(
        @[ rnStrongEmphasis.newRstNode(rnLeaf.newRstNode(pn)) ] &
          toSeq(comment.children).mapIt(it.toRstNode())
      )

      # echo result

    of CXComment_BlockCommand:
      return rnParagraph.newRstNode(
        @[
          rnEmphasis.newRstNode(
            rnInner.newRstNode(
              $clang_BlockCommandComment_getCommandName(comment)
            )
          )
        ] & toSeq(comment.children).mapIt(it.toRstNode()))

    of CXComment_VerbatimBlockCommand:
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
            $clang_VerbatimBlockLineComment_getText(comment))))
    of CXComment_VerbatimLine:
      return rnInterpretedText.newRstNode(
        $clang_VerbatimLineComment_getText(comment))
    else:
      echo "died".toRed()
      echo comment.objTreeRepr().pstring()
      raiseAssert("#[ IMPLEMENT ]#")

proc toNimDoc*(comment: CXComment): string =
  # echo comment.objTreeRepr().pstring()
  comment.toRstNode().renderRstToRst(result)

  result = result.split("\n").filterIt(
    not it.allOfIt(it in Whitespace)).join("\n")

#=============================  Converters  ==============================#

proc fromElaboratedPType(cxtype: CXType): NType[PNode] =
  ($cxtype).
    dropPrefix("enum ").
    dropPrefix("struct ").
    dropPrefix("const "). # XXXX
    mkPType()

proc getPointee*(cxType: CXType): CXType =
  cxtype.expectKind(CXType_Pointer)
  clang_getPointeeType(cxtype)

proc objTreeRepr*(cxtype: CXType): ObjTree =
  case cxtype.cxKind:
    of CXType_Pointer:
      pptObj("ptr", cxtype.getPointee().objTreeRepr())
    else:
      pptObj($cxtype.cxkind, pptConst($cxtype))

proc tokens*(cursor: CXCursor, tu: CXTranslationUnit): seq[string] =
  let range: CXSourceRange   = clang_getCursorExtent(cursor);
  var tokens: ptr[CXToken]
  var nTokens: cuint = 0
  clang_tokenize(tu, range, addr tokens, addr nTokens)

  for i in 0 ..< nTokens:
    result.add $clang_getTokenSpelling(
      tu, (cast[ptr UncheckedArray[CXToken]](tokens))[i])


proc objTreeRepr*(cursor: CXCursor): ObjTree =
  if cursor.len  == 0:
    pptObj($cursor.cxkind, pptConst($cursor))
  else:
    pptObj(
      $cursor.cxkind,
      toSeq(cursor.children).mapIt(it.objTreeRepr()))


proc objTreeRepr*(cursor: CXCursor, tu: CXTranslationUnit): ObjTree =
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
  cursor.objTreeRepr(tu).treeRepr()

proc treeRepr*(cursor: CXCursor): string =
  cursor.objTreeRepr().treeRepr()

proc lispRepr*(cxtype: CXType): string =
  cxtype.objTreeRepr().lispRepr()

proc toNType*(cxtype: CXType): NType[PNode] =
  # echo cxtype.objTreeRepr().treeRepr()
  result = case cxtype.cxKind:
    of CXType_Bool: mkPType("bool")
    of CXType_Int: mkPType("int")
    # of CXType_Record: mkPType()
    of CXType_Void: mkPType("void")
    of CXType_UInt: mkPType("cuint")
    of CXType_LongLong: mkPType("clonglong")
    of CXType_ULongLong: mkPType("culonglong")
    of CXType_Double: mkPType("cdouble")
    of CXType_ULong: mkPType("culong")
    of CXType_Typedef: mkPType(($cxtype).dropPrefix("const ")) # XXX typedef processing -
    of CXType_Elaborated, CXType_Record, CXType_Enum:
      fromElaboratedPType(cxtype)
    of CXType_Pointer:
      let pointee = cxtype.getPointee()
      case pointee.cxkind:
        of CXType_Char_S:
          mkPType("cstring")
        of CXType_Pointer:
          if pointee.getPointee().cxKind() == CXType_Char_S:
            mkPType("cstringArray")
          else:
            mkNType("ptr", [toNType(pointee)])
        of CXType_Void:
          mkPType("pointer")
        else:
          # echo "NESTED ".toYellow(), cxtype.lispRepr()
          mkNType("ptr", [toNType(pointee)])
    of CXType_ConstantArray:
      mkNType(
        "array",
        @[
          mkPType($cxtype.clang_getNumElements()),
          toNType(cxtype.clang_getElementType())
        ]
      )
    of CXType_FunctionProto:
      mkProcNType[PNode](
        rtype = cxtype.clang_getResultType().toNType(),
        args = cxtype.argTypes.mapIt(it.toNType()),
        pragma = mkPPragma("cdecl")
      )
    else:
      echo "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", cxtype.lispRepr().toGreen()
      mkPType("!!!")

  # echo cxtype.cxkind, " ", result.toNNode()

proc dropPOD*(cxtype: CXType): string =
  case cxtype.cxKind:
    of CXType_Elaborated:
      cxtype.fromElaboratedPType().head
    of CXType_Pointer:
      cxtype.getPointee().dropPOD()
    of CXType_Typedef:
      ($cxtype).dropPrefix("const ")
    else:
      ""


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


proc toPIdentDefs*(cursor: CXCursor): PIdentDefs =
  result.varname = $cursor
  if result.varname.len == 0:
    result.varname = "arg" & $cursor.cxType().dropPOD()

  result.varname = result.varname.fixIdentName()

  result.vtype = cursor.cxType.toNType()


#==========================  Toplevel visitors  ==========================#

type
  RewriteContext = object
    resultNode*: PNode
    prefix*: string
    typePrefix*: string
    translationUnit*: CXTranslationUnit
    dropTypeSubseqs*: seq[string]
    usedPrefixes*: HashSet[string]

proc simplifyFuncName(cursor: CXCursor, context: var RewriteContext): string =
  let unprefix = ($cursor).dropPrefix(context.prefix)

  if cursor.argc() > 0:
    let
      first = cursor.cxType().nthArg(0)
      rawName = first.dropPOD()
      name = rawName.dropPrefix(context.typePrefix)

    if name.len > 0:
      return unprefix.
        dropPrefix(name & "_").
        dropPrefix(rawName & "_")
    else:
      return unprefix
  else:
    return unprefix

proc isAliasTypedef*(cursor: CXCursor): bool =
  # echo "IS ALIAS TYPEDEF".toRed()
  result = true
  if cursor.len == 1:
    result = not (cursor[0].cxKind in {
      CXCursor_EnumDecl,
      CXCursor_StructDecl
    })



proc visitFunction(cursor: CXCursor, context: var RewriteContext) =
  cursor.expectKind(CXCursor_FunctionDecl)
  var prevName = ""
  let procdef = ProcDecl[PNode]().withIt do:
    it.exported = true
    it.comment = cursor.comment().toNimDoc()
    it.name = cursor.simplifyFuncName(context).fixIdentName()
    it.signature = mkProcNType[PNode](@[])
    it.signature.arguments = collect(newSeq):
      for it in cursor.children:
        if it.cxKind == CXCursor_ParmDecl:
          var res = it.toPIdentDefs()
          if res.varname == prevName:
            res.varname &= "1"

          prevName = res.varname
          res

    `rtype=`(it.signature, toNType(cursor.retType()))
    it.signature.pragma = mkPPragma(
      newPIdent("cdecl"),
      nnkExprColonExpr.newPTree(
        newPIdent("dynlib"),
        newPIdent("libclang") # XXX
      ),
      nnkExprColonExpr.newPTree(
        newPIdent("importc"),
        newPLit($cursor)
      )
    )

  context.resultNode.add procdef.toNNode()

proc parseInt(tok: string): int =
  if tok.len > 2 and tok[1] == 'x':
    discard parseHex(tok, result)
  else:
    discard parseInt(tok, result)


proc visitEnumDecl*(cursor: CXCursor, context: var RewriteContext) =
  let enumName = ($cursor.cxType).dropPrefix("enum ")

  let names = @[enumName].concat: collect(newSeq): # NICE
      for elem in cursor.children:
        $elem

  let pref = names.commonPrefix()

  let enumPref =
    block:
      var split: seq[(int, string)] = enumName.
        dropLongestSubseq(context.dropTypeSubseqs).
        dropPrefix(context.typePrefix).
        splitCamel().
        mapIt((0, it))

      var res: string
      while true:
        res = ""
        for (pref, str) in split:
          res &= str[0 .. pref].toLowerAscii()

        # res = split.mapIt(it[1][it[0]].toLowerAscii())
        if res in context.usedPrefixes:
          echo "using extended prefix"
          for val in mitems(split):
            if val[0] < val[1].len:
              inc val[0]
              break
        else:
          break

      context.usedPrefixes.incl res
      res

  var en = PEnum(name: enumName)
  en.comment = cursor.comment().toNimDoc()

  var skipped: seq[string]
  let tu = context.translationUnit
  proc renameField(fld: string): string =
    fld.dropPrefix(pref).dropPrefix("_").addPrefix(enumPref)

  var prevVal: BiggestInt = -123124912
  for elem in cursor.children:
    let name = renameField($elem)
    var fld = EnumField[PNode](
      name: name,
      kind: efvOrdinal,
      comment: elem.comment().toNimDoc()
    )

    var skip = false
    let val = elem[0]
    case val.kind:
      of CXCursor_IntegerLiteral:
        fld.ordVal = makeRTOrdinal(val.tokens(tu)[0].parseInt())
      of CXCursor_BinaryOperator:
        let subn = val.children()
        let toks = val.tokens(tu)[1]
        case toks:
          of "<<":
            fld.ordVal = makeRTOrdinal(
              subn[0].tokens(tu)[0].parseInt() shl
              subn[1].tokens(tu)[0].parseInt(),
            )
          of "|":
            let toks = val.tokens(tu)
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
            echo toks

      of CXCursor_UnaryOperator:
        let toks = val.tokens(tu)
        case toks[0]:
          of "-":
            fld.ordVal = makeRTOrdinal(toks[1].parseInt())
          else:
            raiseAssert("#[ IMPLEMENT ]#")
      of CXCursor_DeclRefExpr:
        skipped.add &"- {elem.tokens(tu).joinw()}"
        skip = true
      else:
        if $val.kind == "OverloadCandidate":
          fld = EnumField[PNode](
            name: name, kind: efvNone, comment: elem.comment().toNimDoc())
        else:
          echo val.kind, " ", elem.tokens(tu)
          fld = EnumField[PNode](
            name: name, kind: efvNone, comment: elem.comment().toNimDoc())


    if not skip:
      if fld.kind == efvOrdinal:
        if fld.ordVal.intVal != prevVal:
          en.values.add fld
          prevVal = fld.ordVal.intVal
        else:
          # Node skip enum with the same value
          skipped.add elem.tokens(tu)
      else:
        inc prevVal
        en.values.add fld


      en.comment.add("\n" & skipped.joinl)
      skipped = @[]
    # en.values.add toEnumField(en, elem, context.translationUnit)

  en.values.sort do(f1, f2: EnumField[PNode]) -> int:
    # NOTE possible source of incompatibility - only fields with
    # specified ordinal values should be sorted.
    if f1.kind == f2.kind and f1.kind == efvOrdinal: 
      cmp(f1.ordVal.intVal, f2.ordVal.intVal)
    else:
      0

  en.exported = true

  context.resultNode.add en.toNNode(standalone = true)


proc visitStructDecl*(cursor: CXCursor, context: var RewriteContext) =
  let structName = ($cursor.cxType).dropPrefix("struct ")

  var resType = PObject(name: mkPType(structName))

  for subn in cursor.children:
    subn.expectKind CXCursor_FieldDecl

    resType.flds.add PField(
      exported: true,
      isKind: false,
      isTuple: false,
      name: fixIdentName($subn),
      fldType: subn.cxType.toNType()
    )

  context.resultNode.add resType.toNNode(standalone = true)


proc visitTypedef*(cursor: CXCursor, context: var RewriteContext) =
  var isTypeDef = true

  if cursor.len == 1:
    if cursor[0].cxKind in {CXCursor_EnumDecl, CXCursor_StructDecl}:
      isTypeDef = false


  if isTypedef:
    context.resultNode.add newPTree(
      nnkTypeSection,
      newPTree(
        nnkTypeDef,
        newPTree(nnkPostfix, newPIdent("*"), newPIdent($cursor.cxType())),
        newEmptyNNode[PNode](),
        newPTree(
          nnkDistinctTy,
          cursor.cxtype().clang_getCanonicalType().toNType().toNNode())))
  else:
    when false:
      for subn in cursor.children:
        case subn.cxKind:
          of CXCursor_StructDecl:
            visitStructDecl(subn, context)
          of CXCursor_EnumDecl:
            visitEnumDecl(subn, context)
          else:
            discard


#================================  Main  =================================#

proc parseTranslationUnit*(
  trIndex: CXIndex,
  filename: string,
  cmdline: seq[string] = @[],
  trOptions: set[CXTranslationUnit_Flags] = {
    CXTranslationUnit_SingleFileParse}): CXTranslationUnit =

  var flags: int
  for opt in trOptions:
    flags = bitor(flags, int(opt))

  let argc = cmdline.len
  let cmdline = allocCSTringArray(cmdline)

  result = clang_parseTranslationUnit(
    trIndex, filename.cstring, cmdline, cint(argc), nil, 0, cuint(flags))

  deallocCStringArray(cmdline)

proc parseCXXString*(
  trIndex: CXIndex, str: cstring, size: int): CXTranslationUnit =

  let tmpfile = "/tmp/eee.cpp"
  tmpfile.writeFile("")


  var file = CXUnsavedFile(
    contents: str,
    length: culong(size),
    filename: tmpfile.cstring
  )

  var unit: CXTranslationUnit

  let errcode = clang_parseTranslationUnit2(
    trIndex,
    sourceFilename     = tmpfile.cstring,
    commandLineArgs    = nil,
    numCommandLineArgs = 0,
    unsavedFiles       = addr file,
    numUnsavedFiles    = 1,
    options            = 0,
    outTU              = addr unit
  )

  return unit


proc parseString() =

  let str = """
int main() {
 2 + 2;
}

"""

  block:
    let trIndex = clang_createIndex(0, 0);
    let unit = trIndex.parseCXXString(str.cstring, str.len)

    if unit.isNil:
      raiseAssert "Unable to parse translation unit. Quitting."

    let cursor: CXCursor = clang_getTranslationUnitCursor(unit)
    cursor.visitMainFile do:
      makeVisitor [unit]:
        echo cursor
        echo cursor.treeRepr(unit)
        return CXChildVisit_Continue


proc parseLibclang() =
  var context = RewriteContext(
    resultNode: nkStmtList.newTree(),
    prefix: "clang_",
    typePrefix: "CX",
    dropTypeSubseqs: @["CXX"]
  )

  let targetFiles = @[
    "ExternC.h",
    "FatalErrorHandler.h",
    "Platform.h",
    "CXErrorCode.h",
    "CXString.h",
    "Index.h",
    "Documentation.h"
  ]

  context.resultNode.add parsePNodeStr """
{.deadCodeElim: on.}
{.push callconv: cdecl.}

when defined(windows):
  const
    libclang = "libclang.dll"
elif defined(macosx):
  const
    libclang = "libclang.dylib"
else:
  const
    libclang = "libclang.so"

type
  CXVirtualFileOverlayImpl* = object
  CXModuleMapDescriptorImpl* = object
  time_t* = clong
"""

  for file in targetFiles:
    let trIndex = clang_createIndex(0, 0);
    let unit = parseTranslationUnit(
      trIndex, &"/usr/include/clang-c/{file}", trOptions = {})


    if unit.isNil:
      raiseAssert "Unable to parse translation unit. Quitting."

    context.translationUnit = unit

    let cursor: CXCursor = clang_getTranslationUnitCursor(unit)
    cursor.clangVisitChildren do:
      makeVisitor [context]:
        if cursor.isFromMainFile():
          case cursor.cxkind:
            of CXCursor_FunctionDecl:
              visitFunction(cursor, context)
              return CXChildVisit_Continue
            of CXCursor_TypedefDecl:
              visitTypedef(cursor, context)
              return CXChildVisit_Continue
            of CXCursor_EnumDecl:
              visitEnumDecl(cursor, context)
              return CXChildVisit_Continue
            of CXCursor_StructDecl:
              visitStructDecl(cursor, context)
              return CXChildVisit_Continue
            else:
              discard

        return CXChildVisit_Recurse

  "../libclang.nim".writeFile($context.resultNode)
  shell:
    nim c "../libclang.nim"


proc parseMainFileExample*(withInclude: bool) =
  let outfile = "/tmp/file.cpp"
  let str = """

/// Documentation comments for class
class MyClass
{
public:
  int field;
  virtual void method() const = 0;
  static const int static_field;
  static int static_method();
};

/** This is a main function documenation comment */
int main() {
  1 + 2;
}

"""

  if withInclude:
    outfile.writeFile "#include <iostream>\n" & str
  else:
    outfile.writeFile str


  let
    trIndex = clang_createIndex(0, 0)
    unit = parseTranslationUnit(trIndex, outfile)
    topCursor = unit.clang_getTranslationUnitCursor()

  topCursor.clangVisitChildren do:
    makeVisitor [unit]:
      if cursor.isFromMainFile():
        echo cursor.comment().toNimDoc()
        echo cursor.treeRepr(unit)
        return CXChildVisit_Continue
      else:
        return CXChildVisit_Recurse

proc main() =
  # parseString()
  # echo "main file without include"
  # parseMainFileExample(false)

  # echo "main file with include"
  # parseMainFileExample(true)
  parseLibclang()

when isMainModule:
  try:
    main()
  except:
    pprintErr

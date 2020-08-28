## Helper functions for libclang. This file should not be imported -
## it is `include`'d in the main `libclang` file.

when not defined(libclangIncludeUtils):
  import libclang


#==============================  includes  ===============================#
import bitops, strformat, macros, terminal, sugar, std/decls, strutils,
       sequtils

import hpprint, hpprint/hpprint_repr
import hnimast
import nimtraits
import hmisc/types/colorstring
import hmisc/[hexceptions, helpers]
import compiler/ast

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

#==========================  Translation unit  ===========================#

proc parseTranslationUnit*(
  trIndex: CXIndex,
  filename: string,
  cmdline: seq[string] = @[],
  trOptions: set[CXTranslationUnit_Flags] = {
    tufSingleFileParse}): CXTranslationUnit =

  var flags: int
  for opt in trOptions:
    flags = bitor(flags, int(opt))

  let argc = cmdline.len
  let cmdline = allocCSTringArray(cmdline)

  result = parseTranslationUnit(
    trIndex, filename.cstring, cmdline, cint(argc), nil, 0, cuint(flags))

  var hadErrors = false
  for diag in result.getDiagnostics():
    if diag.getDiagnosticSeverity() in {dsError, dsFatal}:
      hadErrors = true
      echo diag


  deallocCStringArray(cmdline)

  if hadErrors:
    raiseAssert("Translation unit parse failed")

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
  index: CXIndex,
  command: CXCompileCommand): CXTranslationUnit =
  ## Get file and compilation flags from compilation command `command`
  ## and parse translation unit.

  var args = collect(newSeq):
    for arg in command.getArgs():
      let match = arg.startsWith("-I", "-std=")

      if match:
        arg

  args = getBuiltinHeaders().mapIt(&"-I{it}") & args

  let file = $command.getFilename()
  # echo file, "\n", args.joinl()
  index.parseTranslationUnit(file, args, {})

proc isNil*(tu: CXTranslationUnit): bool =
  cast[ptr[CXTranslationUnitImpl]](tu) == nil





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

#==========================  String conversion  ==========================#

proc `$`*(cxstr: CXString): string =
  let str = getCString(cxstr)
  result = $str
  disposeString(cxstr)

proc `$`*(cursor: CXCursor): string = $getCursorSpelling(cursor)
proc `$`*(cxtype: CXType): string = $getTypeSpelling(cxtype)
proc `$`*(cxkind: CXCursorKind): string = $getCursorKindSpelling(cxkind)




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


proc cxType*(cursor: CXCursor): CXType =
  ## Get type of the cursor
  getCursorType(cursor)

proc comment*(cursor: CXCursor): CXComment =
  cursor.getParsedComment()


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
  cursor.expectKind(ckFunctionDecl)
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
    mkPType()


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
    of tkBool: mkPType("bool")
    of tkInt: mkPType("int")
    of tkVoid: mkPType("void")
    of tkUInt: mkPType("cuint")
    of tkLongLong: mkPType("clonglong")
    of tkULongLong: mkPType("culonglong")
    of tkDouble: mkPType("cdouble")
    of tkULong: mkPType("culong")
    of tkTypedef: mkPType(($cxtype).dropPrefix("const ")) # XXXX typedef processing -
    of tkElaborated, tkRecord, tkEnum:
      fromElaboratedPType(cxtype)
    of tkPointer:
      case cxtype[].cxkind:
        of tkChar_S:
          mkPType("cstring")
        of tkPointer:
          if cxtype[][].cxKind() == tkChar_S:
            mkPType("cstringArray")
          else:
            mkNType("ptr", [toNType(cxtype[]).ntype])
        of tkVoid:
          mkPType("pointer")
        of tkFunctionProto:
          let (t, mut) = toNType(cxtype[])
          t
        else:
          mkNType("ptr", [toNType(cxtype[]).ntype])
    of tkConstantArray:
      mkNType(
        "array",
        @[
          mkPType($cxtype.getNumElements()),
          toNType(cxtype.getElementType()).ntype
        ]
      )
    of tkFunctionProto:
      mkProcNType[PNode](
        rtype = cxtype.getResultType().toNType().ntype,
        args = cxtype.argTypes.mapIt(it.toNType().ntype),
        pragma = mkPPragma("cdecl")
      )
    else:
      echo "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen()
      mkPType("!!!")

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

      CDecl* {.derive(GetSet).} = object
        name*: string
        namespace*: seq[string]
        cursor*: CXCursor
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
    it.signature = mkProcNType[PNode](@[])
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
    # it.signature.pragma = mkPPragma()

#*************************************************************************#
#*********************  Translation unit conversion  *********************#
#*************************************************************************#
proc getArguments(cursor: CXCursor): seq[CArg] =
  for idx, subn in cursor.children():
    var name = $subn
    if name.len == 0:
      name = "a" & $idx

    result.add CArg(name: name, cursor: subn)

proc visitMethod(cursor: CXCursor, accs: CX_CXXAccessSpecifier): CDecl =
  result = CDecl(kind: cdkMethod)
  result.name = $cursor
  result.accs = accs
  result.args = cursor.getArguments()

proc visitClass(cursor: CXCursor, parent: seq[string]): CDecl =
  ## Convert class under cursor to `CDecl`
  result = CDecl(kind: cdkClass)
  result.name = $cursor
  result.namespace = parent

  var currentAccs = asPrivate
  for subn in cursor:
    case subn.cxKind:
      of ckCXXMethod:
        result.members.add visitMethod(subn, currentAccs)
      of ckCXXAccessSpecifier:
        currentAccs = subn.getCXXAccessSpecifier()
      else:
        echo ($subn.cxKind).toRed(), " ", subn
        discard


proc visitCursor(cursor: CXCursor, parent: seq[string]): tuple[
  decls: seq[CDecl], recurse: bool]

proc visitNamespace(cursor: CXCursor, parent: seq[string]): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.
  for subn in cursor:
    result = visitCursor(subn, parent & @[ $cursor ]).decls

proc visitCursor(cursor: CXCursor, parent: seq[string]): tuple[
  decls: seq[CDecl], recurse: bool] =

  result.decls.add case cursor.cxKind:
    of ckNamespace:
      visitNamespace(cursor, parent)
    of ckClassDecl:
      @[ visitClass(cursor, parent) ]
    else:
      result.recurse = true
      return


proc splitDeclarations*(tu: CXTranslationUnit): seq[CDecl] =
  ## Convert main file of translation unit into flattened sequence of
  ## high-level declarations. All cursors for objects/structs are
  ## retained.
  assert not tu.isNil
  let cursor = tu.getTranslationUnitCursor()
  var res: seq[CDecl]
  cursor.visitMainFile do:
    makeVisitor [tu, res]:
      let (decls, rec) = visitCursor(cursor, @[])
      if rec:
        return cvrRecurse
      else:
        res.add decls
        return cvrContinue

  return res

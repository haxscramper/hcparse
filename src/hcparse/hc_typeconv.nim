## Conversion of C types to nim

import cxtypes, hc_types
import hnimast

import
  hmisc/other/[hlogger],
  hmisc/types/colorstring,
  hmisc/algo/[hstring_algo, hseq_mapping, clformat],
  hmisc/helpers,
  hmisc/macros/iflet

import std/[algorithm, strformat, sequtils, strutils,
            parseutils, tables, decls]

import cxcommon

proc getTypeName*(cxtype: CXType, conf: WrapConf): string

proc toNimType*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): NimType


proc fromElaboratedPType*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): NimType =
  # debug cxtype
  let genParams = cxtype.getNumTemplateArguments()
  let decl = cxtype.getTypeDeclaration()
  if genParams > 0:
    case decl.cxKind:
      of ckTypedefDecl, ckTypeAliasDecl, ckTypeAliasTemplateDecl:
        # WARNING `template <J, Q> using` is not handled
        result = newNimType(cxtype.getTypeName(conf), cxtype)

      of ckClassDecl, ckStructDecl, ckClassTemplate:
        # debug "Class decl"
        let params = cxtype.genParams()
        result = newNimType(cxtype.getTypeName(conf), cxtype)
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add parm.toNimType(conf, cache)

      else:
        conf.warn "Conversion from elaborated type: ", decl
        conf.debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

    conf.fixTypeName(result, conf, 0)

  else:
    result = newNimType(getTypeName(cxtype, conf), cxtype)

proc dropPOD*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): string =
  case cxtype.cxKind:
    of tkElaborated:
      cxtype.fromElaboratedPType(conf, cache).nimName

    of tkPointer:
      cxtype[].dropPOD(conf, cache)

    of tkTypedef:
      ($cxtype).dropPrefix("const ")

    else:
      ""

proc toCArg*(cursor: CXCursor, conf: WrapConf, cache: var WrapCache): CArg =
  var varname = $cursor
  if varname.len == 0:
    varname = "arg" & $cursor.cxType().dropPOD(conf, cache)

  varname = varname.fixIdentName()
  let argType = cursor.cxType().toNimType(conf, cache)
  return initCArg(varname, argType)



  # if cursor.cxKind() == ckClassTemplate:
  #   logDefer info, "Required generic for", cursor
  #   debug cursor.getSpellingLocation()
  #   debug result.len
  #   debug result
  #   debug cursor.cxKind()
    # debug cursor.treeRepr()


proc toCName*(cursor: CXCursor): CName
proc toScopedIdent*(cursor: CXCursor): CScopedIdent =
  for elem in cursor:
    result.add toCName(elem)


proc fullScopedIdent*(
    cxtype: CXType, filterInline: bool = true, withType: bool = true):
  CScopedIdent =

  for cursor in getTypeNamespaces(cxtype, filterInline, withType):
    result.add toCName(cursor)


proc toCName*(cursor: CXCursor): CName =
  result = CName(cursor: cursor, isGenerated: false)
  for genParam in requiredGenericParams(cursor):
    # HACK add `genParam` explicitly. Was added because template type
    # parameters were ignored otherwise. Not sure if this is a general
    # enough solution, but it worked for now.
    let name = @[toCName(genParam)] & toScopedIdent(genParam)
    result.genParams.add name


proc toCName*(str: string, genp: seq[CScopedIdent] = @[]): CName =
  result = CName(name: str, isGenerated: true)
  result.genParams = genp

proc toScopedIdent*(name: seq[string]): CScopedIdent =
  for elem in name:
    result.add toCName(elem)


proc toScopedIdent*(name: string): CScopedIdent =
  result.add toCName(name)

proc sameNoGeneric*(ident1, ident2: CScopedIdent): bool =
  result = ident1.len == ident2.len
  if result:
    for (a, b) in zip(ident1, ident2):
      if a.getName() != b.getName():
        return false

proc typeName*(ident: CScopedIdent): seq[string] = ident.mapIt($it.cursor)

proc getTypeName*(decl: CxCursor, conf: WrapConf): string =
  assertKind(decl, {ckClassDecl, ckStructDecl, ckClassTemplate})
  decl.getSemanticNamespaces().mapIt(
    dropPrefix($it, toStrPart(["const ", "enum ", "struct ", "union "]))
  ).join("::")


proc defaultTypeParameter*(
  cursor: CxCursor, cache: var WrapCache, conf: WrapConf): Option[NimType] =

  let params = toSeq(cursor)

  proc foldTypes(idx: var int, cache: var WrapCache): NimType =
    case params[idx].kind:
      of ckTypeRef:
        result = toNimType(params[idx].cxType(), conf, cache)
        inc idx

      of ckTemplateRef:
        result = newNimType(
          params[idx].getCursorDefinition().getTypeName(conf))
        inc idx

        result.genericParams.add foldTypes(idx, cache)

      else:
        raise newUnexpectedKindError(params[idx])

  var idx = 0
  return some foldTypes(idx, cache)

proc setParamsForType*(
    cache: var WrapCache, conf: WrapConf,
    ident: CScopedIdent, params: seq[CxCursor]
  ) =

  if params.len > 0:
    var key: seq[string]
    for part in ident:
      key.add $part.cursor

    if key notin cache.paramsForType:
      cache.paramsForType[key] = @[]

    var list {.byaddr1.} = cache.paramsForType[key]

    for idx, param in params:
      var nimType = param.cxtype.toNimType(conf, cache)

      if list.high < idx:
        list.add NimType(kind: ctkIdent)

      if list[idx].defaultType.isNone():
        list[idx] = nimType

      if param.len() > 0:
        let default = defaultTypeParameter(
          param, cache, conf)

        if default.isSome():
          list[idx].defaultType = default
          conf.debug "Set default type for", idx, "type parameter",
            key.hshow()

          conf.debug cache.paramsForType[key][idx]



proc getParamsForType*(
    cache: var WrapCache, cxtype: CScopedIdent,
    conf: WrapConf,
    paramRange: Slice[int] = 0 .. high(int),
    default: bool = false
  ): seq[NimType] =
  ## One or more (potentially defaulted) generic type parameters for type
  ## `cxtype`. Parameter range starts at first requested parameter /index/.
  ## If end of range equal to `high(int)` return all template parameters.


  let name = cxtype.mapIt($it.cursor)

  if name in cache.paramsForType:
    var params {.byaddr1.} = cache.paramsForType[name]
    for paramIdx in paramRange:
      if paramIdx < params.len:
        var res =
          if params[paramIdx].defaultType.isSome() and default:
            params[paramIdx].defaultType.get()

          else:
            params[paramIdx]

        conf.fixTypeName(res, conf, 0)
        result.add res
        #   conf.debug params[paramIdx].defaultType.get()

      elif paramRange.b == high(int):
        break

      else:
        raise newImplementError(
          &"Type {cxtype} does not have generic parameter indexed {paramIdx}")

proc getTypeName*(cxtype: CXType, conf: WrapConf): string =
  let curs = cxtype.getTypeDeclaration()
  case curs.cxKind:
    of ckTypedefDecl, ckTypeAliasDecl:
      return $curs.cxType()

    of ckClassDecl, ckStructDecl, ckEnumDecl, ckUnionDecl,
       ckClassTemplate, ckTypeAliasTemplateDecl:
      result = $curs

    else:
      conf.err $curs
      conf.err "Type name for ", curs.treeRepr(conf.unit)
      raiseAssert(
        &"Cannot convert cursor of kind {curs.cxKind} to type")

  result = cxtype.getTypeNamespaces().mapIt(
    dropPrefix($it, toStrPart(["const ", "enum ", "struct ", "union "]))
  ).join("::")

  conf.debug cxtype.getTypeNamespaces()
  conf.debug result

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

proc fromCxxTypeName*(name: string): string =
  case name:
    of "long": "clong"
    of "int": "cint"
    of "unsigned long": "culong"
    else: ""

proc toNimType*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): NimType =
  ## Convert CXType to nim type. Due to differences in how mutability
  ## handled in nim and C it is not entirely possible to map `CXType` to
  ## `NimType` without losing this information. Instead `isMutable` is set
  ## in resulting type, indicating whether or not the type was mutable.
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
  ##
  ## - TODO :: `const&&` parameters /could/ be mapped to `sink` annotations
  var mutable: bool = false
  result = case cxtype.cxKind():
    of tkBool:       newNimType("bool",        cxtype)
    of tkInt:        newNimType("cint",        cxtype)
    of tkVoid:       newNimType("void",        cxtype)
    of tkUInt:       newNimType("cuint",       cxtype)
    of tkLongLong:   newNimType("clonglong",   cxtype)
    of tkULongLong:  newNimType("culonglong",  cxtype)
    of tkDouble:     newNimType("cdouble",     cxtype)
    of tkULong:      newNimType("culong",      cxtype)
    of tkUChar:      newNimType("cuchar",      cxtype)
    of tkChar16:     newNimType("cchar16",     cxtype)
    of tkChar32:     newNimType("cchar32",     cxtype)
    of tkWChar:      newNimType("cwchar",      cxtype)
    of tkChar_S:     newNimType("cchar",       cxtype)
    of tkLong:       newNimType("clong",       cxtype)
    of tkUShort:     newNimType("cushort",     cxtype)
    of tkNullPtr:    newNimType("pointer",     cxtype) # WARNING C++ type is `nullptr_t`
    of tkFloat:      newNimType("cfloat",      cxtype)
    of tkLongDouble: newNimType("clongdouble", cxtype)
    of tkShort:      newNimType("cshort",      cxtype)
    of tkSChar:      newNimType("cschar",      cxtype)
    of tkTypedef:
      mutable = cxType.isMutableRef()
      newNimType(($cxtype).dropPrefix("const "), cxtype) # XXXX typedef processing -

    of tkElaborated, tkRecord, tkEnum:
      # debug "From elaborated type"
      fromElaboratedPType(cxtype, conf, cache)

    of tkPointer:
      case cxtype[].cxkind:
        of tkChar_S:
          newNimType("cstring", cxtype)

        of tkPointer:
          if cxtype[][].cxKind() == tkChar_S:
            newNimType("cstringArray", cxtype)

          else:
            newNimType("ptr", [toNimType(cxtype[], conf, cache)], cxtype)

        of tkVoid:
          newNimType("pointer", cxtype)

        of tkFunctionProto:
          toNimType(cxtype[], conf, cache)

        else:
          newNimType("ptr", [toNimType(cxtype[], conf, cache)], cxtype)

    of tkConstantArray:
      newNimType(
        "ptr", [
          newNimType(
            "array", @[
              newNimType($cxtype.getNumElements(), cxtype.getElementType()),
              toNimType(cxtype.getElementType(), conf, cache)
            ], cxType)
        ], cxType)

    of tkIncompleteArray:
      # QUESTION maybe convert to `ptr UncheckedArray?` or add user-defined
      # callback for switching between different behaviors.
      newNimType("ptr", [toNimType(
        cxtype.getElementType(), conf, cache)], cxType)

    of tkFunctionProto:
      newNimType(
        cxtype.argTypes.mapIt(initCArg("", toNimType(it, conf, cache))),
        cxtype.getResultType().toNimType(conf, cache)
      )

    of tkLValueReference:
      mutable = cxType.isMutableRef()
      toNimType(cxType[], conf, cache)

    of tkRValueReference: # WARNING I'm not 100% sure this is correct
                          # way to map rvalue references to nim type
                          # system.
      mutable = cxType.isMutableRef()
      toNimType(cxType[], conf, cache)

    of tkUnexposed:
      let strval = ($cxType).dropPrefix("const ") # WARNING
      let db = "string" in strval

      if strval.validCxxIdentifier():
        newNimType(strval, cxtype)

      else:
        # pprintStackTrace()
        let
          decl = cxtype.getTypeDeclaration()
          name = cxType.getTypeNamespaces().mapIt($it).join("::")
          typenameParts = toStrPart(@[
            "type-parameter", "typename type-parameter",
            "typename rebind<type-parameter",
            "typename"
          ])


        var res = newNimType(name, cxType)
        if decl.cxKind in {
          # HACK list of necessary kinds is determined by trial and error,
          # I'm still not really sure what `tkUnexposed` actually
          # represents.
          ckClassTemplate, ckClassDecl
        }:
          for param in cache.getParamsForType(cxType.fullScopedIdent(), conf):
            res.add param

        elif startsWith($cxType, typenameParts):
          let unprefix = dropPrefix($cxType, typenameParts)
          if allIt(unprefix, it in {'0' .. '9', '-'}):
            res = newNimType("TYPE_PARAM " & unprefix, cxtype, true)

          else:
            res = newNimType("COMPLEX_PARAM", cxtype, true)

        else:
          res = newNimType("UNEXPOSED", cxtype, true)
          if decl.cxKind() notin {ckNoDeclFound}:
            conf.warn "No decl found for type"
            conf.logger.indented:
              conf.info cxtype.lispRepr()
              conf.debug decl.getSpellingLocation()
              conf.debug decl.cxKind()
              conf.debug decl.treeRepr()


        res

    of tkDependent:
      newNimType("DEPENDENT", cxType, true)

    of tkMemberPointer:
      # WARNING Member pointer
      newNimType("!!!", cxType, false)

    of tkDependentSizedArray:
      let cx = $cxtype
      let name = cx[cx.skipUntil('[') + 1 .. ^2].strip()
      newNimType("array", @[
        newNimType(name),
        toNimType(cxtype.getElementType(), conf, cache)
      ], cxType)

    else:
      conf.err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newNimType("!!!", cxtype)

  result.isMutable = mutable
  conf.fixTypeName(result, conf, 0)

func fixTypeParams*(nt: var NimType, params: seq[NimType]) =
  func aux(nt: var NimType, idx: var int) =
    case nt.kind:
      of ctkIdent:
        if startsWith(nt.nimName, "TYPE_PARAM"):
          nt.nimName = params[idx].nimName
          inc idx

        for sub in mitems(nt.genericParams):
          aux(sub, idx)

      of ctkProc:
        for arg in mitems(nt.arguments):
          aux(arg.nimType, idx)



  var idx: int
  aux(nt, idx)


func hasSpecial*(nt: NimType, special: seq[string]): bool =
  case nt.kind:
    of ctkIdent:
      nt.nimName in special or
      nt.genericParams.anyOfIt(it.hasSpecial(special))

    of ctkProc:
      nt.arguments.anyOfIt(it.nimType.hasSpecial(special))


func hasUnexposed*(nt: NimType): bool =
  nt.hasSpecial(@[ "UNEXPOSED", "DEPENDENT" ])


func hasComplexParam*(nt: NimType): bool =
  nt.hasSpecial(@[ "COMPLEX_PARAM" ])

proc isEnum*(cxtype: CXType): bool =
  case cxtype.cxKind():
    of tkEnum:
      return true

    of tkElaborated:
      if cxtype.getTypeDeclaration().cxKind() in {ckEnumDecl}:
        return true

      else:
        return false

    else:
      return false

proc toInitCall*(
    cursor: CXCursor, conf: WrapConf, cache: var WrapCache): PNode =
  proc aux(cursor: CXCursor, ilist: bool, cache: var WrapCache): PNode =
    case cursor.cxKind():
      of ckUnexposedExpr:
        # info $cursor.cxType()
        if startsWith($cursor.cxType(), "std::initializer_list"):
          # info "Found init list"
          result = aux(cursor[0], true, cache)

        else:
          result = aux(cursor[0], ilist, cache)

      of ckCallExpr:
        let str = "init" & $cursor.cxType()
        case cursor[0].cxKind():
          of ckUnexposedExpr, ckCallExpr, ckFunctionalCastExpr:
            result = aux(cursor[0], ilist, cache)

          of ckIntegerLiteral, ckNullPtrLiteralExpr:
            result = aux(cursor[0], ilist, cache)

          of ckTypeRef:
            # First found in `clang/Rewriter.h/getRangeSize()`
            result = newPCall(str)
            # assert cursor.len == 1, &[
            #   $cursor.getSpellingLocation(),
            #   "\n",
            #   cursor.treeRepr()
            # ]

          # TEMP
          # of ckCallExpr:
          #   let cType = cursor[0].cxType()
          #   case cType.cxKind():
          #     of tkTypedef:
          #       result = aux(cType.getTypeDeclaration(), ilist)
          #       for arg in cursor[0]:
          #         result.add aux(arg, ilist)

          #     else:
          #       raiseImplementKindError(cType)

          else:
            conf.err cursor[0].cxKind()
            conf.debug "\n" & cursor.treeRepr(conf.unit)
            conf.debug cursor.getSpellingLocation()
            # raiseAssert("#[ IMPLEMENT ]#")

        if isNil(result):
          return

        if result.kind in nkTokenKinds:
          result = newPCall(str, result)

        elif result.kind == nkCall and
             result[0].getStrVal() != str:
          result = newPCall(str, result)

      of ckDeclRefExpr:
        if cursor.cxType().cxKind() == tkFunctionProto:
          result = newPCall($cursor)

        elif cursor.cxType().cxKind() == tkTypedef:
          conf.err "Found typedef used as default value"
          conf.debug cursor.getSpellingLocation()
          discard

        elif cursor.cxType().cxKind() == tkEnum:
          conf.err "Found enum value as default"
          conf.debug cursor.getSpellingLocation()
          discard

        else:
          conf.debug cursor.cxType().getTypeDeclaration().treeRepr()

          conf.debug cursor.getSpellingLocation()
          conf.debug cursor.treeRepr()
          raiseImplementKindError(cursor.cxType())

      of ckFunctionalCastExpr:
        result = aux(cursor[1], ilist, cache)

      of ckNullPtrLiteralExpr:
        result = newPLit(nil)

      of ckInitListExpr:
        # debug "Creating initList"
        # debug ilist
        # raiseAssert("#[ IMPLEMENT ]#")
        if ilist:
          result = newPCall("cxxInitList")

        else:
          result = newPCall("init" & $cursor.cxType())

        for arg in cursor:
          result.add aux(arg, false, cache)

      of ckIntegerLiteral, ckCharacterLiteral, ckFloatingLiteral,
         ckStringLiteral:
        let tokens = cursor.tokenStrings(conf.unit)

        case cursor.cxKind():
          of ckIntegerLiteral:
            result = newPCall("cint", newPLit(parseInt(tokens[0])))

          of ckStringLiteral:
            result = newPCall("cstring", newPLit(tokens[0]))

          of ckCharacterLiteral:
            result = newPLit(tokens[0][1])

          of ckFloatingLiteral:
            result = newPLit(parseFloat(tokens[0]))

          else:
            discard

      of ckLambdaExpr:
        conf.err "FIXME implement conversion to call from lambda expr"
        discard

      of ckTypeRef:
        conf.err "FIXME implement conversion to call from type ref "
        discard

      of ckCStyleCastExpr:
        result = nnkCast.newPTree(
          cursor[0].cxType().toNimType(conf, cache).toNType().toNNode(),
          aux(cursor[1], ilist, cache)
        )

      else:
        conf.err "Implement for kind", cursor.cxKind()
        conf.debug cursor.getSpellingLocation()
        conf.debug cursor.tokenStrings(conf.unit)
        # debug cursor.treeRepr(conf.unit)
        # raiseAssert("#[ IMPLEMENT ]#")

  return aux(cursor, false, cache)


proc setDefaultForArg*(
    arg: var CArg, cursor: CXCursor, conf: WrapConf, cache: var WrapCache) =
  ## Update default value for argument.
  ## - @arg{arg} :: Non-raw argument to update default for
  ## - @arg{cursor} :: original cursor for argument declaration
  ## - @arg{conf} :: Default wrap configuration

  # info cursor.len
  if cursor.len == 2 and
     cursor[1].cxKind() in {ckUnexposedExpr, ckInitListExpr}:
    # debug cursor.treeRepr(conf.unit)
    let default = toInitCall(cursor[1], conf, cache)
    if not isNil(default):
      arg.default = some(default)
    # debug arg.default

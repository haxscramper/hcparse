import
  hmisc/core/all,
  hmisc/algo/[hstring_algo],
  hmisc/types/colorstring



import std/[with, sequtils, strutils, strformat, sets]

import
  ./interop_ir/[wrap_store, wrap_icpp],
  ./hc_types,
  ./cxtypes,
  ./hc_typeconv

proc cxxName*(conf: WrapConf, decl: CDecl): CxxName =
  raise newImplementError()

proc cxxName*(cxtype: CxType, cache: var WrapCache): CxxName =
  raise newImplementError()

proc cxxPair*(conf: WrapConf, decl: CDecl): CxxNamePair =
  cxxPair(decl.getNimName(conf), conf.cxxName(decl))

proc cxxPair*(conf: WrapConf, cursor: CxCursor): CxxNamePair =
  cxxPair($cursor, cxxName(@[$cursor]))

proc cxxPair*(
    conf: WrapConf, ident: CSCopedIdent, cache: var WrapCache): CxxNamePair =
  raise newImplementError()

proc cxxPair*(
  conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxNamePair =
  raise newImplementError()
  # cxxPair(cxtype.getTypeName(conf))

proc toCxxUse*(
  conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxTypeUse

proc cxxGenParams*(
    conf: WrapConf, ident: CSCopedIdent, cache: var WrapCache
  ): CxxGenParams =

  raise newImplementError()

proc toCxxElaborated*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): CxxTypeUse =

  let decl = cxtype.getTypeDeclaration()
  result = conf.cxxPair(cxtype, cache).cxxTypeUse()
  if cxtype.getNumTemplateArguments() > 0:
    case decl.cxKind:
      of ckTypedefDecl, ckTypeAliasDecl, ckTypeAliasTemplateDecl:
        # WARNING `template <J, Q> using` is not handled
        discard
        # result = conf.cxxPair(cxtype, @[]) # newNimType(cxtype.getTypeName(conf), cxtype)

      of ckTypeDeclKinds:
        # result = newNimType(cxtype.getTypeName(conf), cxtype)
        let params = cxtype.templateParams()
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add conf.toCxxUse(parm, cache) # parm.toNimType(conf, cache)

      else:
        conf.warn "Conversion from elaborated type: ", decl
        conf.debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

  # else:
  #   result = newNimType(getTypeName(cxtype, conf), cxtype)

proc toCxxUse*(conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxTypeUse =
  if conf.isComplexType(cxtype, cache):
    raise newImplementError()
    # return conf.newComplexType(cxType, cache)

  var
    mutable: bool = false
    special: CTypeSpecialKind = ctskNone
    pair: CxxNamePair

  case cxtype.cxKind():
    of tkBool, tkint, tkvoid, tkuint, tklonglong, tkulonglong,
       tkdouble, tkulong, tkuchar, tkchar16, tkchar32, tkwchar,
       tkchar_s, tklong, tkushort, tknullptr, tkfloat, tklongdouble,
       tkshort, tkschar:
      let name = $cxtype
      pair = cxxPair(name, mapPrimitiveName(name).cxxName())

    of tkTypedef:
      mutable = cxType.isMutableRef()
      pair = cxxPair(($cxtype).dropPrefix("const "), cxxName(cxtype, cache))

    of tkElaborated, tkRecord, tkEnum:
      result = toCxxElaborated(cxtype, conf, cache) # fromElaboratedPType(cxtype, conf, cache)

    of tkPointer:
      result = toCxxUse(conf, cxtype[], cache)
      result = wrap(result, ctkPtr)

    # of tkConstantArray:
    #   newNimType(
    #     "ptr", [
    #       newNimType(
    #         "array", @[
    #           newNimType($cxtype.getNumElements(), cxtype.getElementType()),
    #           toNimType(cxtype.getElementType(), conf, cache)
    #         ], cxType)
    #     ], cxType)

    # of tkIncompleteArray:
    #   # QUESTION maybe convert to `ptr UncheckedArray?` or add user-defined
    #   # callback for switching between different behaviors.
    #   newNimType("ptr", [toNimType(
    #     cxtype.getElementType(), conf, cache)], cxType)

    # of tkFunctionProto:
    #   newNimType(
    #     cxtype.argTypes.mapIt(initCArg("", toNimType(it, conf, cache))),
    #     cxtype.getResultType().toNimType(conf, cache)
    #   )

    # of tkLValueReference:
    #   # NOTE this implementation does not work as expected, becuase `const
    #   # T&` is not a const-qulified type.
    #   #
    #   # mutable = not cxType.isConstQualified()
    #   mutable = not startsWith($cxType, "const")
    #   special = ctskLValueRef

    #   toNimType(cxType[], conf, cache)

    # of tkRValueReference: # WARNING I'm not 100% sure this is correct
    #                       # way to map rvalue references to nim type
    #                       # system.
    #   mutable = cxType.isMutableRef()
    #   special = ctskRValueRef
    #   toNimType(cxType[], conf, cache)

    # of tkUnexposed:
    #   let strval = ($cxType).dropPrefix("const ") # WARNING
    #   let db = "string" in strval

    #   if strval.validCxxIdentifier():
    #     newNimType(strval, cxtype)

    #   else:
    #     let
    #       decl = cxtype.getTypeDeclaration()
    #       name = cxType.namespacedName(conf)
    #       typenameParts = toStrPart(@[
    #         "type-parameter", "typename type-parameter",
    #         "typename rebind<type-parameter",
    #         "typename"
    #       ])


    #     var res = newNimType(name, cxType)
    #     if decl.cxKind in ckTypeDeclKinds:
    #       # HACK list of necessary kinds is determined by trial and error,
    #       # I'm still not really sure what `tkUnexposed` actually
    #       # represents.
    #       for arg in cxType.templateParams():
    #         res.add toNimType(arg, conf, cache)
    #         res.genParams[^1].isParam = true

    #     elif startsWith($cxType, typenameParts):
    #       let unprefix = dropPrefix($cxType, typenameParts)
    #       if allIt(unprefix, it in {'0' .. '9', '-'}):
    #         res = newNimType("TYPE_PARAM " & unprefix, cxtype, true)

    #       else:
    #         res = newTemplateUndefined(cxType)

    #     else:
    #       res = newNimType("UNEXPOSED", cxtype, true)
    #       if decl.cxKind() notin {ckNoDeclFound}:
    #         conf.warn "No decl found for type"
    #         conf.logger.indented:
    #           conf.info cxtype.lispRepr()
    #           conf.debug decl.getSpellingLocation()
    #           conf.debug decl.cxKind()
    #           conf.debug decl.treeRepr()


    #     res

    # of tkDependent:
    #   newNimType("DEPENDENT", cxType, true)

    # of tkMemberPointer:
    #   # WARNING Member pointer
    #   newNimType("!!!", cxType, false)

    # of tkDependentSizedArray:
    #   let cx = $cxtype
    #   let name = cx[cx.skipUntil('[') + 1 .. ^2].strip()
    #   newNimType("array", @[
    #     newNimType(name),
    #     toNimType(cxtype.getElementType(), conf, cache)
    #   ], cxType)

    else:
      conf.err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      # newNimType("!!!", cxtype)

  # result.isMutable = mutable
  # result.specialKind = special

proc toCxxOperator*(
    oper: CDecl,
    conf: WrapConf,
    cache: var WrapCache
  ): CxxProc =
  assert conf.isImportcpp

  result = conf.cxxPair(oper).cxxProc()
  result.kind = oper.classifyOperator(conf)

  case result.kind:
    of cpkRegular, cpkConstructor, cpkDestructor:
      raise newUnexpectedKindError(result)

    of cpkCopyAsgnOp:
      result.nimName = "setFrom"
      result.icpp = icppInfix("=")

    of cpkNewOp, cpkDeleteOp:
      discard

    of cpkAsgnOp:
      result.icpp = icppInfix("=")

    of cpkArrayOp:
      let rtype = oper.cursor.retType()
      let nimReturn = rtype.toNimType(conf, cache)
      if nimReturn.isMutable:
        result.nimName = "[]="
        # WARNING potential source of horrible c++ codegen errors
        result.icpp = initIcpp(ipkNextArg, "[", ipkNextARg, "]=", ipkNextArg)

      else:
        result.icpp = initIcpp(ipkNextArg, "[", ipkNextARg, "]")

    of cpkInfixOp:
      result.icpp = initIcpp &"({toCppNamespace(oper.ident)}(#, #))"

      # if oper.arguments.len < 2:
      #   result.addThis = true

    of cpkArrowOp:
      # WARNING
      result.icpp = initIcpp &"(#.operator->(@))"

    of cpkCallOp:
      # NOTE nim does have experimental support for call
      # operator, but I think it is better to wrap this one as
      # separate function `call()`
      with result:
        nimName = "call"
        kind = cpkCallOp
        icpp = initIcpp(ipkNextArg, "(", ipkArgSplice, ")") # &"#(@)"

    of cpkDerefOp:
      result.nimName = "[]"
      result.icpp = initIcpp &"(*#)"

    of cpkPrefixOp:
      result.icpp = initIcpp($result.cxxName, ipkNextArg)

    of cpkPostfixOp:
      result.icpp = initIcpp(ipkNextArg, $result.cxxName)


    of cpkCommaOp:
      result.nimName = "commaOp"
      result.icpp = initIcpp &"operator,(@)"

    of cpkConvertOp:
      let restype = toCxxUse(conf, oper.cursor.retType(), cache)
      with result:
        nimName = "to" & capitalizeAscii(restype.nimName)
        icpp = initIcpp(ipkArgSplice)
        returnType = resType



    of cpkUserLitOp:
      let restype = toCxxUse(conf, oper.cursor.retType(), cache)
      with result:
        nimName = "to" & capitalizeAscii(restype.nimName)
        icpp = initIcpp &"({oper.cursor}(@))"
        returnType = restype

  # FIXME
  # result.header = conf.makeHeader(oper.cursor, conf)

proc toCxxArg*(arg: CArg, conf: WrapConf, cache: var WrapCache): CxxArg =
  cxxArg(cxxPair(conf, arg.cursor),
         toCxxUse(conf, arg.cursor.cxType(), cache))

proc toCxxProc*(
    pr: CDecl,
    conf: WrapConf,
    parent: Option[CxxTypeDecl],
    cache: var WrapCache,
    parentDecl: Option[CDecl]
  ): CxxProc =

  result = cxxProc(conf.cxxPair(pr))

  template endProc(result: GenProc): untyped =
    let generated = newProcVisit(result, conf, cache)
    result.decl.add result
    result.decl.add GenPass(iinfo: currLInfo(), passEntries: generated)


  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction })

  result.head.genParams = conf.cxxGenParams(pr.ident, cache) # conf.genParamsForIdent(pr.ident, cache)

  if pr.isOperator:
    # HACK temporary workaround for `new` and `delete` operator handing
    var opKind = classifyOperator(pr, conf)
    if opKind notin {cpkNewOp, cpkDeleteOp}:
      result = pr.toCxxOperator(conf, cache)

  else:
    let icppName = toCppNamespace(pr.ident)
    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      if pr.cursor.isStatic():
        result.icpp = initIcpp &"({icppName}(@))"

      else:
        result.icpp = initIcpp &"(#.{pr.getNimName(conf)}(@))"

      # result.header = conf.makeHeader(pr.cursor, conf)

    else:
      if conf.isImportcpp:
        result.icpp = initIcpp &"({icppName}(@))"

      else:
        result.icpp = initIcpp &"{icppName}"

      # result.header = conf.makeHeader(pr.cursor, conf)

  if pr.cursor.kind in {ckConstructor}:
    result.constructorOf = some parent.get().cxxTypeUse()

  elif parent.isSome():
    result.methodOf = some parent.get().cxxTypeUse()

  for argIdx, arg in pr.arguments:
    result.add toCxxArg(arg, conf, cache)

  if pr.cursor.isVariadic() == 1:
    result.flags.incl cpfVariadic


  # elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
  #   # Override handling of return types for constructors
  #   if not pr.isOperator:
  #     # But ignore implicit user-defined conversion functions like
  #     # `operator T()`

  #     # result.header = conf.makeHeader(pr.cursor, conf)
  #     let suffix = parent.get().nimName.capitalizeAscii()

  #     block initConstructor:
  #       var result = deepCopy(result)
  #       result.name = "init" & suffix
  #       result.returnType = parent.get()
  #       result.icpp = initIcpp &"{toCppNamespace(parentDecl.get().ident)}(@)"
  #       result.decl.add result

  #     block newRefConstructor:
  #       var result = deepCopy(result)
  #       result.name = "new" & suffix
  #       result.impl = some initDestroyCall(
  #         parent.get(), result.arguments,
  #         toCppNamespace(parentDecl.get().ident),
  #         conf, cache)

  #       result.returnType = newNimType("ref", @[parent.get()])
  #       result.icpp = initIcpp &"new {toCppNamespace(parentDecl.get().ident)}(@)"
  #       result.noPragmas = gpcNoPragma

  #       result.decl.add result

  #     block newPtrConstructor:
  #       var result = deepCopy(result)
  #       result.name = "cnew" & suffix
  #       result.returnType = newNimType("ptr", @[parent.get()])
  #       result.icpp = initIcpp &"new {toCppNamespace(parentDecl.get().ident)}(@)"
  #       result.decl.add result


  #   else:
  #     conf.warn "Discarding wrappers for conversion function"
  #     conf.dump pr.cursor
  #     conf.dump pr.cursor.getSpellingLocation()

  # else:
  #   let re = pr.cursor.retType()
  #   var returnType = toNimType(re, conf, cache)

  #   if returnType.isComplex.not() and
  #      parentDecl.isSome() and
  #      parent.isSome() and
  #      pr.cursor.retType().
  #      getTypeDeclaration().
  #      inheritsGenParamsOf(parentDecl.get().cursor):

  #     returnType.genParams = parent.get().genParams
  #     # WARNING(refactor)

  #   result.returnType = returnType

  #   if returnType.hasUnexposed():
  #     # WARNING dropping all methods that use `tkUnexposed` type
  #     # in return value. This must be fixed in future versions.
  #     result.canAdd = false

  #   if pr.cursor.cxkind == ckDestructor:
  #     # Explicitly calling destructor on object
  #     result.arguments.add initCArg("self", newNimType("ptr", @[parent.get()]))
  #     result.icpp = initIcpp &"~{result.name}()"
  #     result.name = "destroy" & parent.get().nimName
  #     result.declareForward = true

  #   endProc(result)


proc toCxxEnum*(declEn: CDecl, conf: WrapConf, cache: var WrapCache): CxxEnum =
  result = cxxEnum(conf.cxxPair(declEn))
  result.isClassEnum = declEn.isClassEnum

  var visited: HashSet[BiggestInt]
  for (cursor, value) in declEn.enumFields:
    let gen = CxxEnumValue(
      name: conf.cxxPair(cursor),
      value: value,
      valueTokens: tokenStrings(cursor, conf.unit))

    if value in visited:
      result.duplicates.add gen

    else:
      result.values.add gen
      visited.incl value


    # if val != prev:
    #   prev = val
    #   result.values.add GenEnumValue(
    #     cdecl: CDecl(cursor: name, kind: cdkField, ident: @[]),
    #     docComment: @[conf.docCommentFor(declEn.ident & toCName(name))],
    #     iinfo: currLInfo(),
    #     baseName: $name,
    #     resCName: cEnumName($name, nt, conf, cache),
    #     resNimName: renameField($name, pref, enumPref, cache),
    #     resVal: val,
    #     stringif: toCppNamespace(declEn.ident) & "::" & $name
    #   )




proc toCxxObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxObject

proc toCxxAlias*(
    al: CDecl, conf: WrapConf, cache: var WrapCache
  ): seq[CxxEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, *APtr` shit that can result in multple
  # declarations, nested types (that might recursively contain who-knows-what)

  if al.isNewType:
    var baseType: CxxTypeUse
    if al.aliasNewType.kind in {cdkClass, cdkStruct, cdkUnion}:
      let wrapBase = al.aliasNewType.toCxxObject(conf, cache)
      result.add wrapBase
      baseType = wrapBase.decl.cxxTypeUse()

    else:
      let wrapBase = al.aliasNewType.toCxxEnum(conf, cache)
      result.add wrapBase
      baseType = wrapBase.decl.cxxTypeUse()

    for newName in al.newTypes:
      var newType = cxxPair(conf, newName).cxxTypeDecl()
      # newType.genParams = baseType.genParams # FIXME port generic parameters from
      if newType.cxxName() != baseType.cxxName():
        # Alias names might be the same for `typedef struct St {} St;`
        result.add cxxAlias(newType, baseType)

  else:
    # Get underlying type for alias
    let aliasof = al.cursor.cxType().getCanonicalType()

    # Create new identifier for aliased type
    var newAlias = conf.cxxPair(al.ident, cache).cxxTypeDecl()

    # Identifier for old aliased type
    var baseType: CxxTypeUse
    if getTypeDeclaration(aliasof).cxKind() == ckNodeclFound:
      baseType = toCxxUse(conf, aliasof, cache)

    else:
      baseType = conf.cxxPair(conf.fullScopedIdent(aliasof), cache).cxxTypeUse()
      # WARNING mismatched generic parameters between lhs and rhs parts of
      # alias might result in broken wrappers.

    var maxIdx = 0
    for idx, param in al.cursor.cxType().templateParams():
      baseType.genParams[idx] = toCxxUse(conf, param, cache)
      maxIdx = idx

    # FIXME WARNING skipping generic parameters for now, I don't remember
    # what went into fixing them in the first place, so most likely it is a
    # completely broken pile of garbage now.

    # for idx in (maxIdx + 1) ..< baseType.genParams.len:
    #   newAlias.genParams.add baseType.genParams[idx]

    # # QUESTION is it necessary?
    # # baseType.genParams.add baseType.getPartialParams(conf, cache, defaulted = true)

    # fixTypeParams(baseType, newAlias.genParams)

    result.add cxxALias(newAlias, baseType)

proc toCxxObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxObject =
  assert cd.kind in {cdkClass, cdkStruct, cdkUnion, cdkForward}, $cd.kind
  result = cxxObject(conf.cxxPair(cd))

  let tdecl = cd.cursor.cxType().getTypeDeclaration()

  if cd.kind != cdkForward:
    for entry in cd.nestedTypes:
      case entry.kind:
        of cdkEnum:
          result.nested.add toCxxEnum(entry, conf, cache).box()

        of cdkStruct, cdkClass, cdkUnion:
          result.nested.add toCxxObject(entry, conf, cache).box()

        of cdkAlias:
          result.nested.add toCxxAlias(entry, conf, cache)

        else:
          discard

    for mem in cd.members:
      case mem.kind:
        of cdkMethod:
          result.methods.add toCxxProc(mem, conf, some result.decl, cache, some cd)

        of cdkField:
          var field = cxxField(
            conf.cxxPair(mem.cursor),
            toCxxUse(conf, mem.cursor.cxType(), cache)
          )

          case mem.access:
            of asPublic: field.flags.incl cffPublic
            of asPrivate: field.flags.incl cffPrivate
            of asProtected: field.flags.incl cffProtected
            of asInvalidAccessSpecifier: discard

          if mem.cursor.isStatic:
            field.flags.incl cffStatic

          result.mfields.add field

        else:
          discard

proc toCxxFile*(
    parsed: ParsedFile, conf: WrapConf, cache: var WrapCache): CxxFile =

  for decl in parsed.api.decls:
    case decl.kind:
      of cdkClass, cdkStruct, cdkUnion:
        result.entries.add decl.toCxxObject(conf, cache)

      of cdkAlias:
        result.entries.add decl.toCxxAlias(conf, cache)

      of cdkEnum:
        result.entries.add decl.toCxxEnum(conf, cache)

      of cdkFunction:
        result.entries.add decl.toCxxProc(
          conf, none CxxTypeDecl, cache, none CDecl)

      of cdkMacro:
        raise newImplementError()

      of cdkMethod, cdkField:
        discard

      of cdkForward:
        raise newImplementError()

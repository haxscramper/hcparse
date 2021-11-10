import
  std/[
    strutils,
    sets,
  ]

import
  ./hc_types,
  ./cxtypes,
  ./libclang_wrap,
  ../hc_typeconv

import
  ../processor/[wrap_store]

import
  hmisc/algo/[halgorithm],
  hmisc/core/all,
  hmisc/other/[hlogger],
  hmisc/types/colorstring

func incl*[I](s1: var OrderedSet[I], other: OrderedSet[I]) =
  for item in other:
    incl(s1, item)

func excl*[I](s1: var OrderedSet[I], other: OrderedSet[I]) =
  for item in other:
    excl(s1, item)

proc wrapEnum*(declEn: CDecl, conf: WrapConf, cache: var WrapCache): CxxEnum

proc cxxPair*(ident: CSCopedIdent): CxxNamePair =
  var scopes: seq[string]
  for part in items(ident):
    case part.isGenerated:
      of true: scopes.add $part.name
      of false: scopes.add $part.cursor

  assert scopes.len > 0, "No scopes found for ident " & $ident

  return cxxPair(scopes.join("_") , cxxName(scopes))

proc cxxPair*(cxtype: CxType, conf: WrapConf): CxxNamePair =
  var scopes: seq[string]
  for space in conf.getTypeNamespaces(cxtype):
    scopes.add $space

  assert scopes.len > 0, "No scopes found for type " & $cxtype
  return cxxPair(scopes.join("_"), cxxName(scopes))

proc cxxPair*(cursor: CxCursor, conf: WrapConf): CxxNamePair =
  var scopes: seq[string]
  for space in conf.getSemanticNamespaces(cursor):
    scopes.add $space

  assert scopes.len > 0, "No scopes found for cursor " & $cursor
  return cxxPair(scopes.join("_"), cxxName(scopes))


proc toCxxType*(cxtype: CXType, conf: WrapConf, cache: var WrapCache): CxxTypeUse =
  if conf.isComplexType(cxtype, cache):
    return CxxTypeUse(kind: ctkIdent, flags: {ctfComplex})

  case cxtype.cxKind():
    of tkBool:       result = cxxTypeUse(cptBool)
    of tkInt:        result = cxxTypeUse(cptInt)
    of tkVoid:       result = cxxTypeUse(cptVoid)
    of tkUInt:       result = cxxTypeUse(cptUInt)
    of tkLongLong:   result = cxxTypeUse(cptI64)
    of tkULongLong:  result = cxxTypeUse(cptU64)
    of tkDouble:     result = cxxTypeUse(cptDouble)
    of tkULong:      result = cxxTypeUse(cptU32)
    of tkUChar:      result = cxxTypeUse(cptUChar)
    of tkChar16:     result = cxxTypeUse(cptChar16)
    of tkChar32:     result = cxxTypeUse(cptChar32)
    of tkWChar:      result = cxxTypeUse(cptWChar)
    of tkChar_S:     result = cxxTypeUse(cptChar)
    of tkLong:       result = cxxTypeUse(cptI32)
    of tkUShort:     result = cxxTypeUse(cptU16)
    of tkNullPtr:    result = cxxTypeUse(cptNullptr)
    of tkFloat:      result = cxxTypeUse(cptFloat)
    of tkLongDouble: result = cxxTypeUse(cptLongDouble)
    of tkShort:      result = cxxTypeUse(cptI16)
    of tkSChar:      result = cxxTypeUse(cptChar)

    of tkTypedef:
      raise newImplementError()

    of tkElaborated, tkRecord, tkEnum:
      result = cxxTypeUse(cxxPair(cxtype, conf), @[])
      {.warning: "[IMPLEMENT] Get generic type parameters from the type instance".}

      # let spaces = conf.getTypeNamespaces(cxtype)
      # raise newImplementError()

    of tkPointer:
      result = cxtype[].toCxxType(conf, cache).wrap(ctkPtr)

    of tkConstantArray:
      result = CxxTypeUse(
        kind: ctkFixedArray,
        arraySize: CxxTypeUse(kind: ctkStaticParam, value: CxxExpr(
          kind: cekIntLit, intVal: cxtype.getNumElements().int())),
        arrayElement: cxtype.getElementType().toCxxType(conf, cache)
      )

    of tkIncompleteArray:
      result = cxtype.getElementType().toCxxType(conf, cache).wrap(ctkDynamicArray)

    of tkFunctionProto:
      var args: seq[CxxArg]
      for t in cxtype.argTypes:
        args.add cxxArg(cxxPair(""), toCxxType(t, conf, cache))

      result = cxxTypeUse(args, cxtype.getResultType().toCxxType(conf, cache),)

    of tkLValueReference:
      result = cxType[].toCxxType(conf, cache)

    of tkRValueReference:
      result = cxType[].toCxxType(conf, cache).wrap(ctkRVRef)

    of tkUnexposed:
      let decl = cxtype.getTypeDeclaration()

      if decl of {ckStructDecl}:
        result = cxxTypeUse(cxtype.cxxPair(conf))

      else:
        raise newImplementError()

      for p in cxtype.templateParams():
        result.genParams.add toCxxType(p, conf, cache)

      when false:
        let
          decl = cxtype.getTypeDeclaration()
          name = cxType.namespacedName(conf)
          typenameParts = toStrPart(@[
            "type-parameter", "typename type-parameter",
            "typename rebind<type-parameter",
            "typename"
          ])


        var res = newNimType(name, cxType)
        if decl.cxKind in ckTypeDeclKinds:
          # HACK list of necessary kinds is determined by trial and error,
          # I'm still not really sure what `tkUnexposed` actually
          # represents.
          for arg in cxType.templateParams():
            res.add toNimType(arg, conf, cache)
            res.genParams[^1].isParam = true

        elif startsWith($cxType, typenameParts):
          let unprefix = dropPrefix($cxType, typenameParts)
          if allIt(unprefix, it in {'0' .. '9', '-'}):
            res = newNimType("TYPE_PARAM " & unprefix, cxtype, true)

          else:
            res = newTemplateUndefined(cxType)

        else:
          res = newNimType("UNEXPOSED", cxtype, true)
          if decl.cxKind() notin {ckNoDeclFound}:
            conf.warn "No decl found for type"
            conf.logger.indented:
              conf.info cxtype.hshow()
              conf.debug decl.getSpellingLocation()
              conf.debug decl.cxKind()
              conf.debug decl.treeRepr()


    else:
      raise newImplementKindError(cxtype.cxKind())


# es

#     of tkDependent:
#       newNimType("DEPENDENT", cxType, true)

#     of tkMemberPointer:
#       # WARNING Member pointer
#       newNimType("!!!", cxType, false)

#     of tkDependentSizedArray:
#       let cx = $cxtype
#       let name = cx[cx.skipUntil('[') + 1 .. ^2].strip()
#       newNimType("array", @[
#         newNimType(name),
#         toNimType(cxtype.getElementType(), conf, cache)
#       ], cxType)

#     else:
#       conf.err "CANT CONVERT: ".toRed({styleItalic}),
#         cxtype.kind, " ", ($cxtype).toGreen(), " ",
#         cxtype[]

#       newNimType("!!!", cxtype)


  if startsWith($cxType, "const"):
    result.flags.incl ctfConst

  if cxtype.isEnum():
    result.flags.incl ctfIsEnumType



proc toCxxArg*(arg: CArg, conf: WrapConf, cache: var WrapCache): CxxArg =
  result = cxxArg(
    cxxPair(arg.name),
    arg.cursor.cxType().toCxxType(conf, cache)
  )

  when false:
    var argType = a.toNimType(conf, cache)
    if arg.cursor.cxType().isEnum():
      argType.nimName &= conf.rawSuffix()

    if argType.kind in {ctkIdent}:
      argType.genParams.add argType.getPartialParams(conf, cache, false)

      if argType.nimName == "UNEXPOSED":
        # WARNING currently parameters which contain `tkUnexposed`
        # types are not handled but are skipped instead. I don't
        # know how to fix right now.
        discard
        # result.canAdd = false

      if argType.isComplex.not() and
         parentDecl.isSome() and
         arg.cursor.inheritsGenParamsOf(parentDecl.get().cursor) and
         parent.isSome() and
         (arg.cursor.cxType().kind notin {tkUnexposed})
        :
        # WARNING nested class definitions with additional template
        # parameters are not handled right now. It will break for
        # code like
        # `<Ta> struct A { <Tb> struct B {void func(); }; };`
        # and only add `Tb` as template parameter for `func()`.
        for param in parent.get().genParams:
          argType.add param
        # FAIL most likely broken with recent refactoring

    else:
      # FIXME determine and implement edge case handling for procvar
      # arguments
      conf.warn "Temporarily droppping procvar arguemtn handling"


    if not (opKind == cxoPostfixOp and argIdx > 0):
      var newArg = initCArg(arg.name, argType)
      setDefaultForArg(newArg, arg.cursor, conf, cache)
      it.arguments.add newArg


# proc wrapOperator*(
#     oper: CDecl,
#     conf: WrapConf,
#     cache: var WrapCache
#   ): CxxProc =

#   var it = cxxProc(oper.ident.cxxPair())

  # it.name = oper.getNimName(conf)
  # # it.genParams = genParams # FIXME: was it necessary to have generic
  # # parameters conversion for operators?

  # assert conf.isImportcpp
  # let kind = oper.classifyOperator(conf)
  # it.kind = pkOperator

  # case oper.operatorKind:
  #   of cxoCopyAsgnOp:
  #     it.name = "setFrom"
  #     it.icpp = initIcpp "(# = #)"
  #     it.kind = pkRegular
  #     it.iinfo = currLInfo()

  #   of cxoNewOp, cxoDeleteOp:
  #     discard

  #   of cxoAsgnOp:
  #     it.icpp = icppInfix(it.name)
  #     it.iinfo = currLInfo()

  #   of cxoArrayOp:
  #     let rtype = oper.cursor.retType()
  #     let nimReturn = rtype.toNimType(conf, cache)
  #     if nimReturn.isMutable:
  #       it.name = "[]="
  #       # WARNING potential source of horrible c++ codegen errors
  #       it.icpp = initIcpp"#[#]= #"

  #     else:
  #       it.icpp = initIcpp"#[#]"

  #   of cxoInfixOp:
  #     it.icpp = initIcpp &"({toCppNamespace(oper.ident)}(#, #))"
  #     it.iinfo = currLInfo()

  #     if oper.arguments.len < 2:
  #       result.addThis = true

  #   of cxoArrowOp:
  #     # WARNING
  #     it.icpp = initIcpp &"(#.operator->(@))"

  #   of cxoCallOp:
  #     # NOTE nim does have experimental support for call
  #     # operator, but I think it is better to wrap this one as
  #     # separate function `call()`
  #     it.name = "call"
  #     it.kind = pkRegular
  #     it.icpp = initIcpp &"#(@)"
  #     it.iinfo = currLInfo()

  #   of cxoDerefOp:
  #     it.name = "[]"
  #     it.icpp = initIcpp &"(*#)"
  #     it.iinfo = currLInfo()

  #   of cxoPrefixOp:
  #     it.icpp = initIcpp &"({it.name}#)" # FIXME use scoped ident
  #     it.iinfo = currLInfo()
  #     if oper.arguments.len == 0:
  #       result.addThis = true

  #   of cxoPostfixOp:
  #     it.icpp = initIcpp &"(#{it.name})" # FIXME use scoped ident
  #     it.iinfo = currLInfo()

  #     if oper.arguments.len == 1:
  #       result.addThis = true

  #   of cxoCommaOp:
  #     it.name = "commaOp"
  #     it.icpp = initIcpp &"operator,(@)"
  #     it.kind = pkRegular
  #     it.iinfo = currLInfo()

  #   of cxoConvertOp:
  #     let restype = oper.cursor.retType().toNimType(conf, cache)

  #     with it:
  #       name = "to" & capitalizeAscii(restype.nimName)
  #       icpp = initIcpp"@"
  #       returnType = resType
  #       declType = ptkConverter
  #       kind = pkRegular


  #     it.iinfo = currLInfo()

  #   of cxoUserLitOp:
  #     let restype = oper.cursor.retType().toNimType(conf, cache)

  #     with it:
  #       name = "to" & capitalizeAscii(restype.nimName)
  #       icpp = initIcpp &"({oper.cursor}(@))"
  #       returnType = restype
  #       kind = pkRegular


  #     it.iinfo = currLInfo()

  # it.header = conf.makeHeader(oper.cursor, conf)
  # result.decl = it
  # result.addThis = result.addThis or
  #   (kind in {
  #     cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp, cxoConvertOp
  #   })



# proc initDestroyCall(
#     parent: NimType,
#     args: seq[CArg],
#     constructorCall: string,
#     conf: WrapConf, cache: var WrapCache
#   ): PNode =

#   let
#     className = parent.nimName
#     argType = newNType("ref", [parent.toNType(conf, cache)]).toNNode()
#     destroyCall = newPIdent(
#       "destroy" & className.fixIdentName().capitalizeAscii())
#     argMixin = args.mapIt("(`" & it.name & "`)").join(", ")
#     emitStr = &"new ((void*)result) {constructorCall}({argMixin}); " &
#       "/* Placement new */"


#   return pquote do:
#     newImportAux() # FIXME This procedure is a necessary hack that provides
#                    # requries `<new>` header import. Maybe this could be
#                    # fixed by somehow grenerating required `header:` pragma
#                    # statements?
#     new(result, proc(self: `argType`) = `destroyCall`(addr self[]))
#     {.emit: `emitStr`.}



proc declGenParams*(part: CName):
  seq[tuple[ptype: CxCursor, pdefault: Option[CxCursor]]] =

  case part.cursor.kind:
    of ckFunctionDecl, ckFunctionTemplate,
       ckDestructor, ckConstructor, ckMethod,
       ckClassTemplate, ckStructDecl:

      for param in part.cursor:
        if param.kind in { ckTemplateTypeParameter }:
          result.add (param, none CxCursor)

        elif param.kind in {
          ckTypedefDecl, ckTypeAliasDecl,
          ckBaseSpecifier, ckAccessSpecifier,
          ckMethod, ckFunctionTemplate,
          ckConstructor, ckDestructor,
          ckVarDecl, ckFieldDecl,
          ckNamespaceRef
        }:
          discard

        else:
          discard

    else:
      discard


proc genParamsForIdent*(
    conf: WrapConf,
    scoped: CSCopedIdent,
    cache: var WrapCache
  ): CxxGenParams =

  for part in scoped:
    for param in part.declGenParams():
      echov param
      # let newt = newNimType($param, isParam = true)
      # result.add newt

proc wrapProcedure*(
    pr: CDecl,
    conf: WrapConf,
    cache: var WrapCache,
    parentDecl: Option[CDecl]
  ): CxxProc =

  result = cxxProc(pr.ident.cxxPair())

  if pr.isOperator:
    result.kind = classifyOperator(pr, conf)
    result.nimName = getNimName(pr, conf)
    echov result.nimName

  result.head.genParams = conf.genParamsForIdent(pr.ident, cache)

  for argIdx, arg in pr.arguments:
    result.arguments.add toCxxArg(arg, conf, cache)

  if pr.cursor.isVariadic() == 1:
    result.flags.incl cpfVariadic

  if pr.cursor.isStatic():
    result.flags.incl cpfStatic

  case pr.cursor.kind:
    of ckConstructor:
      result.constructorOf = some cxxPair(parentDecl.get().ident)
      result.kind = cpkConstructor

    of ckConversionFunction:
      result.constructorOf = some cxxPair(parentDecl.get().ident)
      result.kind = cpkConvertOp

    of ckDestructor:
      result.destructorOf = some cxxPair(parentDecl.get().ident)
      result.kind = cpkDestructor

    else:
      let returnType = pr.cursor.retType().toCxxType(conf, cache)
      result.returnType = returnType

      if parentDecl.isSome():
        result.flags.incl cpfMethod

      if pr.cursor.isConstMethod():
        result.flags.incl cpfConst

  case ExceptionSpecificationKind(pr.cursor.getCursorExceptionSpecificationType()):
    of ceskBasicNoexcept:
      result.flags.incl cpfNoexcept

    else:
      discard
proc wrapField*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxField =
  cxxField(
    cd.ident.cxxPair(),
    cd.cursor.cxType().toCxxType(conf, cache)
  )

proc wrapFunction*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxProc =
  wrapProcedure(cd, conf, cache, none CDecl)

proc wrapObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxObject

proc wrapAlias*(
    al: CDecl, parent: CScopedIdent, conf: WrapConf, cache: var WrapCache
  ): seq[CxxEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, *APtr` shit that can result in multple
  # declarations, nested types (that might recursively contain who-knows-what)

  if al.isNewType:
    var baseType: CxxTypeUse
    if al.aliasNewType.kind in {cdkClass, cdkStruct, cdkUnion}:
      let wrapBase = al.aliasNewType.wrapObject(conf, cache)
      result.add wrapBase
      baseType = wrapBase.decl.cxxTypeUse()

    else:
      let wrapBase = al.aliasNewType.wrapEnum(conf, cache)
      result.add wrapBase
      baseType = wrapBase.decl.cxxTypeUse()

    for newName in al.newTypes:
      var newType = newName.cxxPair(conf).cxxTypeDecl(ctdkTypedef)
      # newType.genParams = baseType.genParams # FIXME port generic parameters from
      if newType.cxxName() != baseType.cxxName():
        # Alias names might be the same for `typedef struct St {} St;`
        result.add cxxAlias(newType, baseType)

  else:
    # Get underlying type for alias
    let aliasof = al.cursor.cxType().getCanonicalType()

    # Create new identifier for aliased type
    var newAlias = cxxPair(al.ident).cxxTypeDecl(ctdkTypedef)

    # Identifier for old aliased type
    var baseType: CxxTypeUse
    if getTypeDeclaration(aliasof).cxKind() == ckNodeclFound:
      baseType = aliasof.toCxxType(conf, cache) # toCxxUse(conf, aliasof, cache)

    else:
      baseType = aliasof.cxxPair(conf).cxxTypeUse()
      # WARNING mismatched generic parameters between lhs and rhs parts of
      # alias might result in broken wrappers.

    var maxIdx = 0
    for idx, param in al.cursor.cxType().templateParams():
      baseType.genParams[idx] = param.toCxxType(conf, cache)
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



proc getDefaultAccess*(cursor: CXCursor): CXAccessSpecifier =
  case cursor.cxKind():
    of ckClassDecl, ckClassTemplate:
      asPrivate

    of ckStructDecl:
      asPublic

    else:
      raiseAssert("Cannot get default visibility for cursor of kind " & $cursor.cxKind())


proc publicFields*(cd: CDecl): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct, cdkUnion}
  for member in cd.members:
    if (member.kind == cdkField) and (member.access == asPublic):
      result.add member

    elif member.kind in {cdkClass, cdkUnion, cdkStruct} and
         member.isAnonymous:
      # Anonymous nested struct, export all fields
      result.add member.publicFields()


# proc updateAggregateInit*(
#     cd: CDecl, conf: WrapConf, cache: var WrapCache, gen: var GenObject) =

#   if conf.isImportcpp and # QUESTION how to handle aggregate initalization
#                           # for C structures? Just declare `{.emit.}`` proc
#                           # (with or without designated initalizers)
#      cd.isAggregateInit and cd.initArgs.len > 0:

#     let pr = initGenProc(cd, currLInfo()).withIt do:
#       it.name = "init" & gen.name.nimName
#       it.arguments = cd.initArgs
#       it.header = conf.makeHeader(cd.cursor, conf)
#       it.icpp = initIcpp &"{toCppNamespace(cd.ident)}({{@}})"
#       it.returnType = gen.name

#     gen.nestedEntries.add pr


# proc updateFieldExport*(
#   cd: CDecl, conf: WrapConf, cache: var WrapCache, gen: var GenObject) =
#   ## Add getter/setter methods for *all* public fields that are accessible
#   ## from this object
#   for fld in cd.publicFields():
#     var res = GenField(
#       # QUESTION `conf.identNameForScoped()?`
#       name: fld.lastName(conf),
#       rawName: fld.lastName(conf),
#       cdecl: fld,
#       fieldType: fld.cursor.cxType().toNimType(conf, cache),
#       isConst: fld.isConst)

#     if fld.fieldTypeDecl.getSome(newFieldType):
#       var newType = newFieldType
#       if newType.isAnonymous:
#         newType.ident[^1] = toCName("Anon")

#       case newType.kind:
#         of cdkStruct, cdkUnion, cdkClass:
#           var decl = wrapObject(newType, conf, cache)

#           gen.nestedEntries.add decl
#           res.fieldType.nimName = decl.name.nimName

#         of cdkEnum:
#           var decl = wrapEnum(newType, conf, cache)

#           res.fieldType.nimName = decl[0].genEnum.name
#           gen.nestedEntries.add decl

#         else:
#           discard

#     if fld.cursor.cxType().isEnum():
#       res.fieldType.nimName &= conf.rawSuffix()

#     gen.memberFields.add res


proc wrapObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxObject =
  assert cd.kind in {
    cdkClass, cdkStruct, cdkUnion, cdkForward}, $cd.kind

  result = cxxObject(cd.ident.cxxPair())
  result.decl.genParams = conf.genParamsForIdent(cd.ident, cache)

  if cd.kind != cdkForward:
    for entry in cd.nestedTypes:
      case entry.kind:
        of cdkEnum:
          result.nested.add wrapEnum(entry, conf, cache)

        of cdkStruct, cdkClass, cdkUnion:
          result.nested.add wrapObject(entry, conf, cache)

        else:
          discard

    for entry in cd.members:
      case entry.kind:
        of cdkMethod:
          result.methods.add wrapProcedure(entry, conf, cache, some cd)

          if entry.cursor.kind in { ckConstructor, ckConversionFunction }:
            result.flags.incl cofExplicitConstructor

          elif entry.cursor.kind in { ckDestructor }:
            result.flags.incl cofExplicitDestructor

        of cdkField:
          var field = wrapField(entry, conf, cache)

          case entry.access:
            of asPublic: field.flags.incl cffPublic
            of asPrivate: field.flags.incl cffPrivate
            of asProtected: field.flags.incl cffProtected
            of asInvalidAccessSpecifier: discard

          if entry.cursor.isStatic:
            field.flags.incl cffStatic

          result.mfields.add field

        else:
          discard

    # wrapMethods(result, cd, conf, result.name, cache)


proc wrapEnum*(
    declEn: CDecl, conf: WrapConf, cache: var WrapCache): CxxEnum =
  ## Generate wrapper for enum declaration
  ##
  ## Generates wrapper for enum declaration, using wrap configuration.
  ## Wrapping is performed in two steps - underlying C enum is wrapped
  ## as-is, and additional nim `enum` is generated. Nim version does not
  ## have holes, which allows it to be used in `array`, iterated upon etc.
  ##
  ## In order to perform conversion between 'proxy' and underlying enum
  ## several helper procs are introduces, such as `toInt`.

  result = cxxEnum(declEn.ident.cxxPair())

  for (field, value) in items(declEn.enumFields):
    result.values.add cxxEnumValue(
      cxxPair($field),
      value
    )

func capitalAscii*(str: string): string =
  toLowerAscii(str).capitalizeAscii()

func capitalAscii*(strs: seq[string]): seq[string] =
  for str in strs:
    result.add toLowerAscii(str).capitalizeAscii()

proc wrapForward*(mdecl: CDecl, conf: WrapConf, cache: var WrapCache): CxxForward =
  raise newImplementError()

proc wrapMacro*(mdecl: CDecl, conf: WrapConf, cache: var WrapCache): CxxMacro =
  raise newImplementError()

proc wrapApiUnit*(
    api: CApiUnit, conf: WrapConf,
    cache: var WrapCache
  ): seq[CxxEntry] =
  ## Generate wrapper for api unit.
  for decl in api.decls:
    if cache.canWrap(decl.cursor):
      cache.markWrap(decl.cursor)

    else:
      continue

    case decl.kind:
      of cdkClass, cdkStruct, cdkUnion:
        conf.logger.thisScope("Class wrapping")

        let spec = decl.cursor.getSpecializedCursorTemplate()

        if spec.cxKind() != ckFirstInvalid:
          discard

        else:
          result.add decl.wrapObject(conf, cache)

      of cdkAlias:
        conf.logger.indented:
          result.add decl.wrapAlias(decl.ident, conf, cache)

      of cdkEnum:
        result.add decl.wrapEnum(conf, cache)

      of cdkFunction:
        result.add decl.wrapFunction(conf, cache):

      of cdkMacro:
        result.add decl.wrapMacro(conf, cache)

      of cdkMethod, cdkField:
        discard

      of cdkForward:
        result.add decl.wrapForward(conf, cache)

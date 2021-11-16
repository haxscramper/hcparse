import
  std/[
    strutils,
    sets,
    tables
  ]

import
  ./hc_types,
  ./cxtypes,
  ./cxcommon,
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

proc cxxPair*(ident: CSCopedIdent, last: bool = false): CxxNamePair =
  let name = cxxName(ident)
  if last:
    return cxxPair(name.scopes[^1], name)

  else:
    return cxxPair(name.scopes.join("_"), name)

proc cxxPair*(cxtype: CxType, conf: WrapConf): CxxNamePair =
  var scopes: seq[string]
  for space in conf.getTypeNamespaces(cxtype):
    scopes.add $space

  assert scopes.len > 0, "No scopes found for type " & $cxtype
  return cxxPair(scopes.join("_"), cxxName(scopes))



proc cxxPair*(cursor: CxCursor, conf: WrapConf): CxxNamePair =
  let name = cxxName(cursor, conf)
  return cxxPair(name.scopes.join("_"), name)

proc getPartialParams*(
    name: CxxName,
    partialMax: int,
    conf: WrapConf,
    cache: var WrapCache,
  ): seq[NimType]


proc toCxxType*(t: NimType, conf: WrapConf, cache: var WrapCache): CxxTypeUse

proc toCxxType*(
    anon: CxxEntry,
    parent: CxxNamePair,
    user: CxxNamePair
  ): CxxTypeUse =

  case anon.kind:
    of cekEnum:
      result = cxxTypeUse(anon.cxxEnum, parent, user)

    of cekObject:
      result = cxxTypeUse(anon.cxxObject, parent, user)

    else:
      raise newUnexpectedKindError(anon)


proc toCxxType*(
    cxtype: CXType,
    conf: WrapConf,
    cache: var WrapCache
  ): CxxTypeUse =

  if conf.isComplexType(cxtype, cache):
    return CxxTypeUse(kind: ctkIdent, flags: {ctfComplex})

  # if "size_t" in $cxtype:
  #   echov cxtype.kind, cxtype.hshow()

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
      case $cxtype:
        of "size_t": result = cxxTypeUse(cptSizeT)
        of "ssize_t": result = cxxTypeUse(cptSSizeT)
        else:
          result = cxxTypeUse(cxxPair(cxtype, conf), @[])
          {.warning: "[IMPLEMENT] Get generic type parameters from the type instance".}

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

      if decl of {ckStructDecl, ckClassDecl}:
        result = cxxTypeUse(cxtype.cxxPair(conf))

      elif decl of {ckNoDeclFound} and validCxxIdentifier($cxtype):
        result = cxxTypeUse(cxxPair($cxtype))

      else:
        raise newImplementKindError(decl, $cxtype)

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

  let params = cxtype.templateParams()
  for p in params:
    result.genParams.add toCxxType(p, conf, cache)

  if result of ctkIdent:
    let part = getPartialParams(
      result.cxxName(), params.high, conf, cache)

    for param in part:
      result.genParams.add param.toCxxType(conf, cache)


  if startsWith($cxType, "const"):
    result.flags.incl ctfConst

  if cxtype.isEnum():
    result.flags.incl ctfIsEnumType


proc toCxxType*(
    t: NimType, conf: WrapConf, cache: var WrapCache): CxxTypeUse =

  case t.kind:
    of ctkIdent:
      if t.fromCxType:
        result = toCxxType(t.cxType, conf, cache)

      else:
        raise newImplementError()

    else:
      raise newImplementError()

proc cxxPair*(t: NimType, conf: WrapConf): CxxNamePair =
  cxxPair($t.cxType)

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


proc getPartialParams*(
    name: CxxName,
    partialMax: int,
    conf: WrapConf,
    cache: var WrapCache
  ): seq[NimType] =

  if name in cache.paramsForType:
    let params = cache.paramsForType[name]
    for idx, param in params:
      if partialMax < idx:
        result.add param
        result[^1].isParam = true

proc genParamsForIdent*(
    conf: WrapConf,
    scoped: CSCopedIdent,
    cache: var WrapCache
  ): CxxGenParams =

  let name = scoped.cxxName()
  for param in getPartialParams(name, -1, conf, cache):
    let name = cxxPair(param, conf)
    if param.defaultType.canGet(def):
      result.add(name, some def.toCxxType(conf, cache))

    else:
      result.add(name, none CxxTypeUse)


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

  result.head.genParams = conf.genParamsForIdent(pr.ident, cache)

  for argIdx, arg in pr.arguments:
    result.arguments.add toCxxArg(arg, conf, cache)

  if pr.cursor.isVariadic() == 1:
    result.flags.incl cpfVariadic

  if pr.cursor.isStatic():
    result.flags.incl cpfStatic

  let tu = pr.cursor.getParentTranslationUnit()

  if pr.cursor.isAnnotatedWith(tu, "qt_signal"):
    result.flags.incl cpfSignal

  if pr.cursor.isAnnotatedWith(tu, "qt_slot"):
    result.flags.incl cpfSlot

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

proc wrapFunction*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxProc =
  wrapProcedure(cd, conf, cache, none CDecl)

proc wrapObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): CxxObject

proc wrapTypeDecl*(
  decl: CDecl, conf: WrapConf, cache: var WrapCache): Option[CxxEntry] =

  case decl.kind:
    of cdkClass, cdkStruct, cdkUnion:
      let spec = decl.cursor.getSpecializedCursorTemplate()
      if spec.cxKind() != ckFirstInvalid:
        discard

      else:
        result = some decl.wrapObject(conf, cache).box()

    of cdkEnum:
      result = some decl.wrapEnum(conf, cache).box()

    else:
      raise newUnexpectedKindError(decl)

proc wrapField*(
    cd: CDecl,
    conf: WrapConf,
    cache: var WrapCache,
    parent: CxxNamePair
  ): CxxField =

  let name = cd.ident.cxxPair(last = true)

  if cd.fieldTypeDecl.canGet(decl):
    let entry = wrapTypeDecl(cd.fieldTypeDecl.get(), conf, cache).get()
    result = cxxField(name, toCxxType(entry, parent, name))

  else:
    result = cxxField(name, cd.cursor.cxType().toCxxType(conf, cache))


proc wrapAlias*(
    al: CDecl, parent: CScopedIdent, conf: WrapConf, cache: var WrapCache
  ): CxxAlias =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, *APtr` shit that can result in multple
  # declarations, nested types (that might recursively contain who-knows-what)

  if al.isNewType:
    var baseType: CxxTypeUse
    if al.aliasNewType.kind in {cdkClass, cdkStruct, cdkUnion}:
      let wrapBase = al.aliasNewType.wrapObject(conf, cache)
      baseType = wrapBase.decl.cxxTypeUse()

    else:
      let wrapBase = al.aliasNewType.wrapEnum(conf, cache)
      baseType = wrapBase.decl.cxxTypeUse()

    var newType = al.newType.cxxPair(conf).cxxTypeDecl(ctdkTypedef)
    # newType.genParams = baseType.genParams # FIXME port generic parameters from
    result = cxxAlias(newType, baseType)

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

    result = cxxALias(newAlias, baseType)



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

        # of cdkAlias:
        #   result.nested.add wrapAlias(entry, conf, cache)

        else:
          discard

    for entry in cd.members:
      case entry.kind:
        of cdkMethod:
          var meth = wrapProcedure(entry, conf, cache, some cd)
          meth.head.genParams = result.decl.genParams & meth.head.genParams

          result.methods.add meth

          if entry.cursor.kind in { ckConstructor, ckConversionFunction }:
            result.flags.incl cofExplicitConstructor

          elif entry.cursor.kind in { ckDestructor }:
            result.flags.incl cofExplicitDestructor

        of cdkField:
          var field = wrapField(entry, conf, cache, result.name())

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
  result = cxxForward(
    mdecl.ident.cxxPair(),
    case mdecl.cursor.kind:
      of ckStructDecl: ctdkStruct
      of ckClassDecl: ctdkClass
      else: raise newImplementKindError(mdecl.cursor)
  )

proc wrapToken*(val: string, tok: CxTokenKind): CxxMacroToken =
  result.strVal = val

  result.kind = case tok:
    of tokLiteral: cmtkIntLit
    of tokIdentifier: cmtkIdent
    of tokPunctuation: cmtkPunctuation
    of tokKeyword: cmtkKeyword
    else: raise newUnexpectedKindError(tok)

proc wrapMacro*(
    mdecl: CDecl,
    conf: WrapConf,
    cache: var WrapCache,
    tu: CxTranslationUnit
  ): CxxMacro =

  result.name = cxxPair($mdecl.cursor)
  let toks = tokenKinds(mdecl.cursor, tu)
  for (val, tok) in toks:
    result.tokens.add wrapToken(val, tok)

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
      of cdkClass, cdkStruct, cdkUnion, cdkEnum:
        let entry = wrapTypeDecl(decl, conf, cache)
        if entry.canGet(decl):
          result.add decl

      of cdkAlias:
        result.add decl.wrapAlias(decl.ident, conf, cache)

      of cdkFunction:
        result.add decl.wrapFunction(conf, cache):

      of cdkMacro:
        result.add decl.wrapMacro(conf, cache, api.unit)

      of cdkMethod, cdkField:
        discard

      of cdkForward:
        result.add decl.wrapForward(conf, cache)

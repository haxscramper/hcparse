import
  ./hc_types,
  ./cxtypes,
  ./cxcommon,
  ./interop_ir/wrap_store

import
  hmisc/other/[oswrap],
  hmisc/core/all

import
  hnimast, jsony

import
  std/[
    options, macros, json, sequtils,
    strformat, with
  ]


proc getCxxName*(conf: WrapConf, decl: CDecl): seq[string] =
  conf.getSemanticNamespaces(decl.cursor).mapIt(getName(it))


proc getSaveSpelling*(conf: WrapConf, cursor: CxCursor): Option[CxxSpellingLocation] =
  if conf.withSpellingLocation:
    let loc = cursor.getSpellingLocation()
    if loc.isSome():
      let (file, line, column, offset) = loc.get()
      result = some CxxSpellingLocation(
        file: file, line: line, column: column)

proc getSaveSpeling*(conf: WrapConf, cdecl: CDecl): Option[CxxSpellingLocation] =
  conf.getSaveSpelling(cdecl.cursor)

proc getCxxName*(conf: WrapConf, cxType: CxType): seq[string] =
  conf.getTypeNamespaces(cxType).mapIt(getName(it))

proc toCxx*(
    conf: WrapConf, entry: CArg, cache: var WrapCache): CxxArg


proc dumpHook*(s: var string, v: CxxLibImport) =
  var tmp: string
  tmp.add "{"
  tmp.add &"\"library\": \"{v.library}\", "
  tmp.add "\"typeImport\": "
  tmp.dumpHook(v.importPath)
  tmp.add "}"
  # echo v, tmp
  s.add tmp

proc getTypeImport*(conf: WrapConf, nimType: NimType): CxxLibImport =
  if nimType.fromCxType or nimType.original.isSome():
    let cxType =
      if nimType.fromCxType:
        nimType.cxType

      else:
        nimType.original.get()

    let decl = getTypeDeclaration(cxType)
    if decl.kind != ckNoDeclFound:
      result = conf.getSavePath(
        decl.getSpellingLocation().get().file)

  else:
    result = nimType.typeImport

  if result.library.len == 0:
    result.library = conf.wrapName


proc toCxxComment*(str: string): CxxEntry =
  CxxEntry(kind: cekComment, cxxComment: str)

proc toCxx*(conf: WrapConf, header: NimHeaderSpec): CxxHeader =
  result = CxxHeader(kind: header.kind)
  case header.kind:
    of chkGlobal: result.global = header.global
    of chkAbsolute: result.file = header.file
    of chkPNode: result.other = $header.pnode


proc toCxx*(
  conf: WrapConf, nimType: NimType, cache: var WrapCache): CxxTypeUse =

  assertRef nimType
  var nimType = nimType
  conf.fixTypeName(nimType, conf, 0)
  result = CxxTypeUse(kind: nimType.kind)

  case nimType.kind:
    of ctkPtr:
      result.wrapped = conf.toCxx(nimType.wrapped, cache)

    of ctkIdent:
      if nimType.defaultType.isSome():
        result.default = some conf.toCxx(nimType.defaultType.get(), cache)

      with result:
        isMutable = nimType.isMutable
        isConst = nimType.isParam
        nimName = nimType.nimName
        genParams = mapIt(nimType.genParams, conf.toCxx(it, cache))
        typeImport = conf.getTypeImport(nimType)
        isComplex = nimType.isComplex

      if nimType.fromCxType:
        result.cxxName = conf.getCxxName(nimType.cxType)

      elif nimType.original.isSome():
        result.cxxName = conf.getCxxName(nimType.original.get())

    of ctkProc:
      result.returnType = conf.toCxx(nimType.returnType, cache)
      result.arguments = mapIt(nimType.arguments, conf.toCxx(it, cache))



proc getHaxdoc*(conf: WrapConf, ident: CScopedIdent): JsonNode =
  newJNull()

proc toCxx*(
    conf: WrapConf, entry: GenEnumValue, cache: var WrapCache
  ): CxxEnumValue =

  result = CxxEnumValue(
    baseName: entry.baseName,
    value: entry.resVal
  )

proc toCxx*(
    conf: WrapConf, entry: GenEnum, cache: var WrapCache): CxxEnum =

  result = CxxEnum(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    nimName: entry.name,
    cxxName: conf.getCxxName(entry.cdecl),
    iinfo: entry.iinfo
  )

  for value in entry.values:
    result.values.add conf.toCxx(value, cache)


proc toCxx*(
    conf: WrapConf, entry: GenEntry, cache: var WrapCache): CxxEntry


proc toCxx*(
    conf: WrapConf, entry: CArg, cache: var WrapCache): CxxArg =

  result = CxxArg(
    haxdocIdent: conf.getHaxdoc(@[]),
    nimName: fixIdentName(entry.name),
    cxxName: @[entry.name],
    nimType: conf.toCxx(entry.nimType, cache)
  )

proc toCxx*(
    conf: WrapConf, entry: GenProc, cache: var WrapCache): CxxProc =

  result = CxxProc(
    icpp: entry.icpp,
    header: some conf.toCxx(entry.header),
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    nimName: entry.name,
    genParams: entry.genParams.mapIt(conf.toCxx(it, cache))
  )

  for arg in entry.arguments:
    result.arguments.add conf.toCxx(arg, cache)


proc toCxx*(
    conf: WrapConf, entry: GenField, cache: var WrapCache): CxxField =

  result = CxxField(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    nimName: entry.name,
    nimType: conf.toCxx(entry.fieldType, cache)
  )

proc toCxx*(
    conf: WrapConf, entry: GenAlias, cache: var WrapCache): CxxAlias =

  result = CxxAlias(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    newAlias: conf.toCxx(entry.newAlias, cache),
    baseType: conf.toCxx(entry.baseType, cache)
  )


proc toCxx*(
    conf: WrapConf, entry: GenForward, cache: var WrapCache): CxxForward =

  result = CxxForward(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    nimName: getName(entry.cdecl.cursor),
    cxxName: conf.getCxxName(entry.cdecl),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    iinfo: entry.iinfo
  )

proc toCxx*(
    conf: WrapConf, entry: GenObject, cache: var WrapCache): CxxObject =

  result = CxxObject(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    nimName: entry.name.nimName,
    cxxName: conf.getCxxName(entry.cdecl),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    iinfo: entry.iinfo,
    genParams: entry.name.genParams.mapIt(conf.toCxx(it, cache)),
    kind: entry.kind
  )

  for field in entry.memberFields:
    result.mfields.add conf.toCxx(field, cache)

  for meth in entry.memberMethods:
    result.methods.add conf.toCxx(meth, cache)

  for nested in entry.nestedEntries:
    let save = conf.toCxx(nested, cache)
    if save.kind != cekEmpty:
      result.nested.add save

proc toCxx*(
    conf: WrapConf, entry: GenEntry, cache: var WrapCache): CxxEntry =

  case entry.kind:
    of gekEnum:
      result = conf.toCxx(entry.genEnum, cache).box()

    of gekObject:
      result = conf.toCxx(entry.genObject, cache).box()

    of gekAlias:
      result = conf.toCxx(entry.genAlias, cache).box()

    of gekForward:
      result = conf.toCxx(entry.genForward, cache).box()

    of gekProc:
      result = conf.toCxx(entry.genProc, cache).box()

    of gekComment:
      result = toCxxComment(entry.comment)

    of gekMacro:
      raise newImplementKindError(entry)

    of gekEmpty, gekImport, gekPass:
      result = CxxEntry(kind: cekEmpty)

proc toCxx*(
    conf: WrapConf, file: WrappedFile, cache: var WrapCache): CxxFile  =

  for entry in file.entries:
    let save = conf.toCxx(entry, cache)
    if save.kind != cekEmpty:
      result.entries.add save

proc newCxxType*(name: string, genParams: seq[CxxType]): CxxType =
  CxxType(nimName: name, genParams: genParams, kind: ctkIdent)

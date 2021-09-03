import
  ./hc_types,
  ./cxtypes,
  ./cxcommon

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

type
  CxxSpellingLocation* = object
    file*: AbsFile
    line*, column*: int

  SaveHeader* = object
    case kind*: NimHeaderSpecKind
      of nhskGlobal:
        global*: string

      of nhskAbsolute:
        file*: AbsFile

      of nhskPNode:
        other*: string

  SaveBase* = object of RootObj
    iinfo*: LineInfo
    spellingLocation*: Option[CxxSpellingLocation]
    nimName*: string
    cxxName*: seq[string]
    icpp*: string
    private*: bool
    header*: Option[SaveHeader]
    docComment*: seq[string]
    haxdocIdent* {.requiresinit.}: JsonNode

  SaveType* = ref object
    case kind*: CTypeKind
      of ctkIdent:
        isConst*: bool
        isMutable*: bool
        isComplex*: bool

        typeImport*: LibImport
        nimName*: string
        cxxName*: seq[string]
        genParams*: seq[SaveType]
        default*: Option[SaveType]

      of ctkProc:
        arguments*: seq[SaveArg]
        returnType*: SaveType


  SaveProc* = object of SaveBase
    arguments*: seq[SaveArg]
    returnType*: SaveType
    genParams*: seq[SaveType]
    kind*: ProcKind

  SaveArg* = object of SaveBase
    nimType*: SaveType
    default*: Option[string] # ???

  SaveField* = object of SaveBase
    nimType*: SaveType
    isStatic*: bool

  SaveEnumValue* = object
    baseName*: string
    cxxName*: seq[string]
    nimName*: string
    value*: BiggestInt
    comment*: string

  SaveAlias* = object of SaveBase
    isDistinct*: bool
    newAlias*: SaveType
    baseType*: SaveType

  SaveEnum* = object of SaveBase
    values*: seq[SaveEnumValue]

  SaveObject* = object of SaveBase
    kind*: GenObjectKind
    genParams*: seq[SaveType]
    mfields*: seq[SaveField]
    methods*: seq[SaveProc]
    nested*: seq[SaveEntry]

  SaveForward* = object of SaveBase

  SaveMacro* = object of SaveBase
    arguments*: seq[string]

  SaveEntry* = ref object
    case kind*: GenEntryKind
      of gekEnum:
        saveEnum*: SaveEnum

      of gekProc:
        saveProc*: SaveProc

      of gekObject:
        saveObject*: SaveObject

      of gekAlias:
        saveAlias*: SaveAlias

      of gekForward:
        saveForward*: SaveForward

      of gekComment:
        saveComment*: string

      of gekMacro:
        saveMacro*: SaveMacro

      else:
        discard

  SaveFile* = object
    entries*: seq[SaveEntry]
    savePath*: LibImport


proc box*(en: SaveEnum): SaveEntry =
  SaveEntry(kind: gekEnum, saveEnum: en)

proc box*(en: SaveForward): SaveEntry =
  SaveEntry(kind: gekForward, saveForward: en)

proc box*(ob: SaveObject): SaveEntry =
  SaveEntry(kind: gekObject, saveObject: ob)

proc box*(en: SaveProc): SaveEntry =
  SaveEntry(kind: gekProc, saveProc: en)

proc box*(en: SaveAlias): SaveEntry =
  SaveEntry(kind: gekAlias, saveAlias: en)

proc box*(en: SaveMacro): SaveEntry =
  SaveEntry(kind: gekMacro, saveMacro: en)

proc add*(
    s: var seq[SaveEntry],
    other: SaveMacro | SaveAlias | SaveObject | SaveForward | SaveProc
  ) =

  s.add box(other)


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

proc toSave*(
    conf: WrapConf, entry: CArg, cache: var WrapCache): SaveArg


proc dumpHook*(s: var string, v: LibImport) =
  var tmp: string
  tmp.add "{"
  tmp.add &"\"library\": \"{v.library}\", "
  tmp.add "\"typeImport\": "
  tmp.dumpHook(v.importPath)
  tmp.add "}"
  # echo v, tmp
  s.add tmp

proc getTypeImport*(conf: WrapConf, nimType: NimType): LibImport =
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


proc toSaveComment*(str: string): SaveEntry =
  SaveEntry(kind: gekComment, saveComment: str)

proc toSave*(conf: WrapConf, header: NimHeaderSpec): SaveHeader =
  result = SaveHeader(kind: header.kind)
  case header.kind:
    of nhskGlobal: result.global = header.global
    of nhskAbsolute: result.file = header.file
    of nhskPNode: result.other = $header.pnode


proc toSave*(
  conf: WrapConf, nimType: NimType, cache: var WrapCache): SaveType =
  assertRef nimType
  var nimType = nimType
  conf.fixTypeName(nimType, conf, 0)
  result = SaveType(kind: nimType.kind)

  case nimType.kind:
    of ctkIdent:
      if nimType.defaultType.isSome():
        result.default = some conf.toSave(nimType.defaultType.get(), cache)

      with result:
        isMutable = nimType.isMutable
        isConst = nimType.isParam
        nimName = nimType.nimName
        genParams = mapIt(nimType.genParams, conf.toSave(it, cache))
        typeImport = conf.getTypeImport(nimType)
        isComplex = nimType.isComplex

      if nimType.fromCxType:
        result.cxxName = conf.getCxxName(nimType.cxType)

      elif nimType.original.isSome():
        result.cxxName = conf.getCxxName(nimType.original.get())

    of ctkProc:
      result.returnType = conf.toSave(nimType.returnType, cache)
      result.arguments = mapIt(nimType.arguments, conf.toSave(it, cache))



proc getHaxdoc*(conf: WrapConf, ident: CScopedIdent): JsonNode =
  newJNull()

proc toSave*(
    conf: WrapConf, entry: GenEnumValue, cache: var WrapCache
  ): SaveEnumValue =

  result = SaveEnumValue(
    baseName: entry.baseName,
    value: entry.resVal
  )

proc toSave*(
    conf: WrapConf, entry: GenEnum, cache: var WrapCache): SaveEnum =

  result = SaveEnum(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    nimName: entry.name,
    cxxName: conf.getCxxName(entry.cdecl),
    iinfo: entry.iinfo
  )

  for value in entry.values:
    result.values.add conf.toSave(value, cache)


proc toSave*(
    conf: WrapConf, entry: GenEntry, cache: var WrapCache): SaveEntry


proc toSave*(
    conf: WrapConf, entry: CArg, cache: var WrapCache): SaveArg =

  result = SaveArg(
    haxdocIdent: conf.getHaxdoc(@[]),
    nimName: fixIdentName(entry.name),
    cxxName: @[entry.name],
    nimType: conf.toSave(entry.nimType, cache)
  )

proc toSave*(
    conf: WrapConf, entry: GenProc, cache: var WrapCache): SaveProc =

  result = SaveProc(
    icpp: entry.icpp,
    header: some conf.toSave(entry.header),
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    nimName: entry.name,
    genParams: entry.genParams.mapIt(conf.toSave(it, cache))
  )

  for arg in entry.arguments:
    result.arguments.add conf.toSave(arg, cache)


proc toSave*(
    conf: WrapConf, entry: GenField, cache: var WrapCache): SaveField =

  result = SaveField(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    nimName: entry.name,
    nimType: conf.toSave(entry.fieldType, cache)
  )

proc toSave*(
    conf: WrapConf, entry: GenAlias, cache: var WrapCache): SaveAlias =

  result = SaveAlias(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    newAlias: conf.toSave(entry.newAlias, cache),
    baseType: conf.toSave(entry.baseType, cache)
  )


proc toSave*(
    conf: WrapConf, entry: GenForward, cache: var WrapCache): SaveForward =

  result = SaveForward(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    nimName: getName(entry.cdecl.cursor),
    cxxName: conf.getCxxName(entry.cdecl),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    iinfo: entry.iinfo
  )

proc toSave*(
    conf: WrapConf, entry: GenObject, cache: var WrapCache): SaveObject =

  result = SaveObject(
    spellingLocation: conf.getSaveSpelling(entry.cdecl.cursor),
    nimName: entry.name.nimName,
    cxxName: conf.getCxxName(entry.cdecl),
    haxdocIdent: conf.getHaxdoc(entry.cdecl.ident),
    iinfo: entry.iinfo,
    genParams: entry.name.genParams.mapIt(conf.toSave(it, cache)),
    kind: entry.kind
  )

  for field in entry.memberFields:
    result.mfields.add conf.toSave(field, cache)

  for meth in entry.memberMethods:
    result.methods.add conf.toSave(meth, cache)

  for nested in entry.nestedEntries:
    let save = conf.toSave(nested, cache)
    if save.kind != gekEmpty:
      result.nested.add save

proc toSave*(
    conf: WrapConf, entry: GenEntry, cache: var WrapCache): SaveEntry =

  case entry.kind:
    of gekEnum:
      result = conf.toSave(entry.genEnum, cache).box()

    of gekObject:
      result = conf.toSave(entry.genObject, cache).box()

    of gekAlias:
      result = conf.toSave(entry.genAlias, cache).box()

    of gekForward:
      result = conf.toSave(entry.genForward, cache).box()

    of gekProc:
      result = conf.toSave(entry.genProc, cache).box()

    of gekComment:
      result = toSaveComment(entry.comment)

    of gekMacro:
      raise newImplementKindError(entry)

    of gekEmpty, gekImport, gekPass:
      result = SaveEntry(kind: gekEmpty)

proc toSave*(
    conf: WrapConf, file: WrappedFile, cache: var WrapCache): SaveFile  =

  for entry in file.entries:
    let save = conf.toSave(entry, cache)
    if save.kind != gekEmpty:
      result.entries.add save

proc newSaveType*(name: string, genParams: seq[SaveType]): SaveType =
  SaveType(nimName: name, genParams: genParams, kind: ctkIdent)

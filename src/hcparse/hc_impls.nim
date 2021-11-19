## Default implementation for user-definable callbacks
import
  ./read_libclang/[
    hc_types,
    cxcommon,
    cxtypes
  ],

  ./hc_typeconv,

  ./processor/[
    wrap_store,
    hc_postprocess
  ]

import
  std/[strutils, strformat, tables]

import
  hmisc/core/all,
  hmisc/algo/[namegen, hstring_algo],
  hmisc/types/colorstring,
  hmisc/other/[oswrap, hlogger]

import
  hnimast

export namegen, wrap_store

proc contains*(dir: AbsDir, file: AbsFile): bool =
  let dir = dir.getStr()
  let file = file.getStr()

  if file.len < dir.len:
    return false
  else:
    return file[0 .. dir.high] == dir

proc skip*(cx: CxType): CxType =
  result = cx
  while true:
    case result.kind:
      of tkLValueReference, tkRValueReference, tkPointer:
        result = result[]

      else:
        return

proc ignoreTypeDecl*(conf: WrapConf, cxType: CxType): bool =
  var decl = cxType.skip().getTypeDeclaration()
  while decl.kind in ckTypeDeclKinds + { ckNamespace }:
    result = conf.ignoreCursor(decl, conf)
    if result:
      break

    else:
      decl = decl.getCursorSemanticParent()


proc ignoreProcCursor*(
  conf: WrapConf, cursor: CxCursor): bool {.logScope(conf.logger).} =
  if cursor.kind in { ckFunctionDecl, ckMethod, ckFunctionTemplate }:
    for argType in cursor.argTypes():
      if conf.ignoreTypeDecl(argType):
        return true

    if conf.ignoreTypeDecl(cursor.retType()):
      return true



proc asIncludeFromDir*(
  cursor: CXCursor | AbsFile, conf: WrapConf, dir: AbsDir): string =

  when cursor is CXCursor:
    let file: AbsFile = cursor.getSpellingLocation().get().file

  else:
    let file: AbsFile = cursor

  return file.getStr().dropPrefix(dir.getStr()).dropPrefix("/")

proc asGlobalInclude*(cursor: CXCursor, conf: WrapConf): string =
  let loc = cursor.getSpellingLocation().get()
  for dir in conf.parseConf.includepaths:
    if loc.file in dir:
      return asIncludeFromDir(cursor, conf, dir)

  return $loc.file

proc isFromDir*(cursor: CXCursor, dir: AbsDir): bool =
  cursor.getSpellingLocation().get().file in dir

proc isFromFile*(cursor: CXCursor, file: AbsFile): bool =
  cursor.getSpellingLocation().get().file == file

proc fixTypeName*(str: string, idx: int, conf: WrapConf): string =
  ## Correct C++ type name to be used in nim wrappers. Convert `::` to
  ## joined name, use correct upper/lowercasing (nep1 style).
  if str.len == 0:
    return "T" & $idx

  elif str.isReservedNimType():
    return str

  else:
    let split = str.split("::")
    var idx = 0
    while idx < split.len:
      if (idx + 1 < split.len) and
         (split[idx] in conf.collapsibleNamespaces) and
         (split[idx + 1].normalize().startsWith(split[idx])):
        # For things like `sourcetrail::SourcetrailDBWrapper`
        discard
      else:
        result.add split[idx].toPascalCase()

      inc idx


proc fixTypeName*(ntype: var NimType, conf: WrapConf, idx: int = 0) =
  case ntype.kind:
    of ctkAnonObject, ctkAnonEnum:
      raise newImplementKindError(ntype)

    of ctkWrapKinds:
      fixTypeName(ntype.wrapped, conf, idx)

    of ctkArrayKinds:
      fixTypeName(ntype.arrayElement, conf, idx)

    of ctkStaticParam, ctkPod:
      discard

    of ctkIdent:
      ntype.nimName = fixTypeName(ntype.nimName, idx, conf)
      var idx = idx
      for gen in mitems(ntype.genParams):
        conf.fixTypeName(gen, conf, idx)
        inc idx

    of ctkProc:
      conf.debug ntype.kind
      if notNil ntype.returnType:
        fixTypeName(ntype.returnType, conf)

      for idx, arg in mpairs(ntype.arguments):
        arg.name =
          if arg.name.len == 0:
            "a" & $idx

          else:
            arg.name

        conf.fixtypename(arg.nimType, conf, idx)

proc fixContextedName*(
    name: CxxNamePair,
    base: string = name.nim,
    style: IdentStyle = idsCamel
  ): string =

  if name.cxx.scopes.len == 0:
    raise newArgumentError(
      "Invalid cxx name pair - empty scopes list")

  assert base.len > 0

  const map = toMapArray({
    cncType:      toMapArray({
      idsCamel: ("",    "T"), idsSnake: ("",    "_t")}),
    cncArg:       toMapArray({
      idsCamel: ("arg", ""),  idsSnake: ("arg_", "")}),
    cncProc:      toMapArray({
      idsCamel: ("c",   ""),  idsSnake: ("c",   "")}),
    cncField:     toMapArray({
      idsCamel: ("f",   ""),  idsSnake: ("",   "_f")}),
    cncMethod:    toMapArray({
      idsCamel: ("m",   ""),  idsSnake: ("",   "_m")}),
    cncVar:       toMapArray({
      idsCamel: ("v",   ""),  idsSnake: ("",   "_v")}),
    cncEnumField: toMapArray({
      idsCamel: ("en",  ""),  idsSnake: ("",  "_en")})
  })

  result = base.keepNimIdentChars()
  let (prefix, suffix) = map[name.context][style]

  if name.context != cncType and isReservedNimType(result):
    result = prefix & result & suffix

  assert result.len > 0
  if isReservedNimIdent(result):
    result = prefix & result & suffix

proc fixContextedName*(
    cache: var StringNameCache,
    name: CxxNamePair,
    style: IdentStyle = idsSnake
  ): string =
  if name.nim.len > 0 and cache.knownRename(name.nim):
    return cache.getRename(name.nim)

  result = fixContextedName(name, name.nim, style)

  const conf = NameFixConf(
    strat: nfsNumerateNew
  )

  if cache.knownGenerated(result):
    result = cache.fixDuplicated(name.nim, result, conf)

  cache.newRename(name.nim, result)




proc getClangSemVersion*(): string =
  ($getClangVersion()).split(" ")[2] # WARNING

proc getClangInclude*(): AbsDir =
  let version = getClangSemVersion()
  return AbsDir(&"/usr/lib/clang/{version}/include")


proc getBuiltinHeaders*(): seq[AbsDir] =
  ## According to clang `documentation <https://clang.llvm.org/docs/LibTooling.html#builtin-includes>`_
  ## libclang is needs additional precompiled headers paths in
  ## addition to default include.
  ##
  ## NOTE right now I have zero idea how it works on windows, so I
  ## will just hardcode unix-like paths.
  @[getClangInclude()]


let baseCppParseConf* = ParseConf(
  includepaths: getBuiltinHeaders(),
  globalFlags: @["-xc++", "-std=c++11"]
)

let baseCParseConf* = ParseConf(
  includePaths: getBuiltinHeaders(),
  sysIncludes: @[
    "/usr/include/linux",
    "/usr/include/sys",
    "/usr/include",
  ],
  globalFlags: @[]
)

let baseCppWrapConf* = WrapConf(
  isImportcpp: true,
  parseConf: baseCppParseConf,
  makeHeader: (
    proc(cursor: CXCursor, conf: WrapConf): NimHeaderSpec {.closure.} =
      let file = cursor.asGlobalInclude(conf)
      if file.startsWith("/"):
        NimHeaderSpec(kind: cbkAbsolute, file: AbsFile(file))

      else:
        NimHeaderSpec(kind: cbkGlobal, global: file)
  ),
  fixTypeName: (
    proc(ntype: var NimType, conf: WrapConf, idx: int) {.closure.} =
      # Default implementation for type name fixes
      fixTypeName(ntype, conf, 0)
  ),

  ignoreCursor: (
    proc(cursor: CXCursor, conf: WrapConf): bool {.closure.} =
      if not ($cursor).startsWith("__cxx11") and
        (
          cursor.cxKind() notin { ckTypedefDecl } and
          (
            # Default convention is to prefix private parts with underscores
            ($cursor).startsWith(@[ "__", "_" ]) and
            # But inline namespaces are still parsed by default
            (not (cursor.isInlineNamespace() == 1))
          )
        ):

        if cursor.cxKind() in {ckStructDecl, ckUnionDecl} and
           not startsWith($cursor.cxType(), @["__", "_"]):
          # `typedef struct _A {} A;` for C wrapping
          return false
        else:
          return true

      if cursor.cxKind == ckNamespace and
         ($cursor in @["detail", "internal"]):
        return true

      elif cursor.cxKind == ckFieldDecl:
        if startsWith($cursor, "private"):
          return true

        else:
          if conf.isTypeInternal(cursor.cxType(), conf):
            return true

      else:
        return false
  ),
  isTypeInternal: (
    proc(cxt: CXType, conf: WrapConf): bool {.closure.} =
      case cxt.cxKind:
        of tkPodKinds:
          result = false
        of tkTypedef:
          # debug cxt.lispRepr()
          result = startsWith($cxt, "_")
        of tkPointer:
          result = conf.isTypeInternal(cxt[], conf)
        else:
          result = false

  ),
  depResolver: (
    proc(cursor, referencedBy: CXCursor): DepResolutionKind {.closure.} =
      if cursor.isFromMainFile():
        result = drkWrapDirectly

      else:
        result = drkImportUses
  )
)

let baseCWrapConf* = baseCPPWrapConf.withDeepIt do:
  it.isImportcpp = false
  it.wrapName = "base-c-wrap-conf"

let baseFixConf* = CxxFixConf(
  fixNameImpl:
    proc(
        name: CxxNamePair,
        cache: var StringNameCache,
        context: CxxNameFixContext,
        conf: CxxFixConf
      ): string {.closure.} =

      result = cache.fixContextedName(name)
)

template onIgnoreCursor*(inConf: var WrapConf, body: untyped): untyped =
  inConf.ignoreCursor = proc(
    cursor {.inject.}: CXCursor,
    conf {.inject.}: WrapConf
  ): bool =
    body

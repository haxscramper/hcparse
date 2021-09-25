## Default implementation for user-definable callbacks
import
  ./hc_types,
  ./cxcommon,
  ./hnimast,
  ./cxtypes,
  ./hc_depresolve,
  ./hc_typeconv,
  ./interop_ir/wrap_store

import
  std/[sequtils, strutils, strformat, tables, sets]

import
  hmisc/core/all,
  hmisc/algo/[namegen, hstring_algo],
  hmisc/types/colorstring,
  hmisc/other/[oswrap, hlogger],
  hmisc/macros/argpass

import
  hnimast


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

proc updateForInternalImport*(
    cursor: CXCursor | AbsFile,
    conf: WrapConf,
    dir: AbsDir, importSpec: var NimImportSpec) =

  when cursor is CXCursor:
    let file = cursor.getSpellingLocation().get().file

  else:
    let file = cursor

  importSpec.relativeDepth = relativeUpCount(dir, file)



proc asImportFromDir*(
    cursor: CXCursor | AbsFile,
    conf: WrapConf, dir: AbsDir,
    isExternalImport: bool
  ): NimImportSpec =
  ## Generate import specification for file or cursor with spelling
  ## location using given configuration.
  ##
  ## - @arg{cursor} :: Input entry (cursor or file) to get import for.
  ##   Cursor must have a valid spelling location.
  ## - @arg{dir} :: Base directory where input entry is originally located.
  ##   Import path will be computed based on it.
  ##
  ##   # Even if path is relative, @ret{.relativeDepth} is not set to any
  ##   # value.

  result = NimImportSpec(isRelative: not isExternalImport)

  for entry in asIncludeFromDir(cursor, conf, dir).split("/"):
    result.importPath.add fixFileName(entry)

  if not isExternalImport:
    updateForInternalImport(cursor, conf, dir, result)

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
    of ctkWrapKinds:
      fixTypeName(ntype.wrapped, conf, idx)

    of ctkArrayKinds:
      fixTypeName(ntype.arrayElement, conf, idx)

    of ctkStaticParam:
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

proc getBaseFile*(conf: WrapConf, wrapped: WrappedFile): AbsFile =
  ## Return base file for generated wrapped one. For generated grouped
  ## files first original one is returned.
  if wrapped.isGenerated:
    # Due to automatically inserted `export` we don't need to perform more
    # concrete resolution
    result = wrapped.original[0]

  else:
    result = wrapped.baseFile




proc getSavePath*(conf: WrapConf, wrapped: WrappedFile): RelFile =
  ## Get save path for generated file - either using @arg{conf.getSavePath}
  ## (for real wrapped files), or @arg{wrapped.newFile} (for newly
  ## generated files.). Generated save files reuse
  ## [[code:WrappedFile.newFile]], non-generated (based on the existing
  ## files) resolve correct relative path based on dependencies and
  ## [[code:WrapConf.getSavePathImpl]])
  if wrapped.isGenerated:
    result = wrapped.newFile

  else:
    result = conf.getSavePath(wrapped.baseFile).toRelative()


proc getLibSavePath*(conf: WrapConf, wrapped: WrappedFile): CxxLibImport =
  if wrapped.isGenerated:
    result = conf.initCxxLibImport(
      wrapped.newFile.withoutExt().getstr().split("/"))

  else:
    result = conf.getSavePath(wrapped.baseFile)

proc getImport*(
    conf: WrapConf, dep, user: CxxLibImport,
    isExternalImport: bool
  ): NimImportSpec =

  if isExternalImport:
    result = initImportSpec(dep.library & dep.importPath)

  else:
    if dep.library == conf.wrapName or
       dep.library.len == 0:
      let (pDep, pUser) = (dep.asImport(), user.asImport())
      let (depth, parts) = importSplit(
        conf.nimOutDir / pUser,
        conf.nimOutDir / pDep)

      result = initImportSpec(parts, depth)

    else:
      result = initImportSpec(dep.library & dep.importPath)

proc getImport*(
  conf: WrapConf, dep, user: AbsFile, isExternalImport: bool): NimImportSpec =
  ## Generate import statement for header file dependency.
  ##
  ## - @arg{isExternalImport} :: Import path is requested for internal use
  ##   (inter-module project dependencies), or for external use
  ## - @arg{dep} :: Location of the original file entry (in the Cxx library)
  ## - @arg{user} :: File importer. In case of external import this
  ##   can be left unspecified as result will be determined solely by
  ##   @arg{conf.getSavePath} callback and @arg{conf.wrapName}
  if isExternalImport:
    result = conf.getImport(conf.getSavePath(dep), CxxLibImport(), true)

  else:
    if conf.isInLibrary(dep, conf):
      result = conf.getImport(
        conf.getSavePath(dep),
        conf.getSavePath(user),
        false)

    else:
      for config in conf.depsConf:
        if config.isInLibrary(dep, config):
          result = config.getImport(dep, user, true)
          break

proc getImportUsingDependencies*(
    conf: WrapConf,
    dependency, user: AbsFile,
    wrapConfurations: seq[WrapConf],
    isExternalImport: bool
  ): NimImportSpec =

  ## Get import specification for using wrapper configuration for a project
  ## and dependencies.
  ##
  ## - @arg{wrapConfigurations} :: List of dependency configurations for
  ##   library. Configurations will be sequentially queried for
  ##   `isInLibrary` check. First matching one will return it's import.
  ## - @arg{conf} :: Main project wrap configuration. @arg{isExternalImport}
  ##   is passed directly to it, so this procedure can also correctly resolve
  ##   in-project import paths.
  ## - @arg{dependency} :: Absolute path to the dependency file

  if conf.isInLibrary(dependency, conf):
    return conf.getImport(dependency, user, isExternalImport)

  for config in wrapConfurations:
    if config.isInLibrary(dependency, config):
      return config.getImport(dependency, user, true)

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
  globalFlags: @[]
)

let baseCppWrapConf* = WrapConf(
  isImportcpp: true,
  parseConf: baseCppParseConf,
  isInLibrary: (
    proc(dep: AbsFile, conf: WrapConf): bool {.closure.} =
      dep.startsWith(conf.baseDir)
  ),
  makeHeader: (
    proc(cursor: CXCursor, conf: WrapConf): NimHeaderSpec {.closure.} =
      let file = cursor.asGlobalInclude(conf)
      if file.startsWith("/"):
        NimHeaderSpec(kind: cbkAbsolute, file: AbsFile(file))

      else:
        NimHeaderSpec(kind: cbkGlobal, global: file)
  ),
  getSavePathImpl: (
    proc(orig: AbsFile, conf: WrapConf): CxxLibImport =
      let rel = relativePath(orig, conf.baseDir)
      return initCxxLibImport(
        conf.wrapName,
          rel.
          withoutExt().
          string.
          split("/").
          mapIt(it.fixFileName()))
  ),

  # typeNameForScoped: (
  #   proc(ident: CScopedIdent, conf: WrapConf): NimType {.closure} =
  #     typeNameForScoped(ident, conf)
  # ),
  isDistinct: (
    proc(ident: CSCopedIdent, conf: WrapConf, cache: var WrapCache):
      bool {.closure.} =

      false
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
  isInternal: (
    proc(dep: AbsFile, conf: WrapConf,
         index: hc_types.FileIndex): bool {.closure.} =
      isInternalImpl(dep, conf, index)
  ),
  prefixForEnum: (
    proc(
      enumId: CScopedIdent, conf: WrapConf, cache: var WrapCache
    ): string =
      result = enumId[^1]
        .getName()
        .splitCamel()
        .mapIt(it[0].toLowerAscii())
        .join("")

      cache.enumPrefs.incl result
  ),
  depResolver: (
    proc(cursor, referencedBy: CXCursor): DepResolutionKind {.closure.} =
      if cursor.isFromMainFile():
        result = drkWrapDirectly

      else:
        result = drkImportUses
        # let loc = cursor.getSpellingLocation()
        # if loc.isSome():
        #   let loc = loc.get()
        #   let (dir, file, ext) = loc.file.splitFile()
        #   if "string" in file:
        #     result = drkWrapDirectly
  ),
  refidFile: RelFile("refid-map.json")
)

let baseCWrapConf* = baseCPPWrapConf.withDeepIt do:
  it.isImportcpp = false
  it.wrapName = "base-c-wrap-conf"

import
  hmisc/types/hgraph,
  hmisc/hasts/graphviz_ast


proc dotDepImports*(
    cache: WrapCache, conf: WrapConf, outFile: AbsFile) =

  var dot = cache.importGraph.dotRepr(
    proc(node: CxxLibImport, _: HNode): DotNode = makeDotNode(0, $node),
    clusters = mapIt(
      cache.importGraph.findCycles(ignoreSelf = true).mergeCycleSets(),
      (it, "")))


  dot.rankdir = grdLeftRight
  dot.toPng(outFile)

import hmisc/algo/hstring_algo

proc dropPrefix*(prefix: string): NameFixImpl =
  return proc(name: string, isType: bool): string =
    name.snakeToCamelCase().dropPrefix(prefix)

import
  std/[
    sequtils, strformat, bitops, strutils,
    tables, parseutils, strutils
  ]

import
  pkg/[jsony],
  pkg/[frosty]

import
  ./cxtypes,
  ./hc_types,
  ./hc_impls,
  ./hc_visitors,
  ./hc_tsreader,
  ./hc_tsconvert,
  ./hc_codegen,
  ./hc_irgen,
  ./hc_wavereader,
  ./hc_grouping,
  ./hc_postprocess,
  ./interop_ir/wrap_store

import
  hnimast,
  hnimast/pprint

import
  hmisc/other/[oswrap, hshell, hpprint, hlogger],
  hmisc/types/[colorstring, hgraph],
  hmisc/algo/[hstring_algo, namegen],
  hmisc/core/all

export
  hc_wavereader, hc_grouping, hc_impls, hc_codegen, oswrap

proc parseTranslationUnit*(
    trIndex: CXIndex,
    filename: AbsFile,
    cmdline: seq[string] = @[],
    trOptions: set[CXTranslationUnit_Flags] = {tufSingleFileParse},
    reparseOnNil: bool = true
  ): CXTranslationUnit =

  ## Parse translation unit for file `filenam`. `cmdline` contains list fo
  ## command-line flags that will be passed to clang parser, file `-xc++`
  ## for enforcing `C++` parse mode for example. List of builtin includes
  ## is added automaticallyt.
  ##
  ## By default, if first parse attempt failed it is repeated in verbose
  ## mode, and all command-line flags are printed into stdout.

  filename.assertExists()

  let cmdline = getBuiltinHeaders().mapIt(&"-I{it}") & cmdline

  var flags: int
  {.warning[HoleEnumConv]:off}:
    for opt in items(trOptions):
      flags = bitor(flags, int(opt))

  block:
    let argc = cmdline.len
    let cmdlineC = allocCSTringArray(cmdline)

    result = parseTranslationUnit(
      trIndex, filename.cstring, cmdlineC, cint(argc), nil, 0, cuint(flags))
    deallocCStringArray(cmdlineC)

  var hadErrors = false
  for diag in result.getDiagnostics():
    if diag.getDiagnosticSeverity() in {dsError, dsFatal}:
      hadErrors = true
      echo ($diag).toRed()

  if hadErrors or (reparseOnNil and result.isNil):
    echo(&"""
Translation unit parse failed due to errors.
Compilation flags:
{cmdline.joinql()}
Input file:
  {filename.realpath}
      """)


    if reparseOnNil:
      echo "Translation unit parse failed, repeating parse in verbose mode"

      let cmdline = @["-v"] & cmdline
      let argc = cmdline.len
      let cmdlineC = allocCSTringArray(cmdline)

      result = parseTranslationUnit(
        trIndex, filename.cstring, cmdlinec, cint(argc), nil, 0, cuint(flags))

      deallocCStringArray(cmdlineC)

    raiseAssert("Translation unit parse failed")

proc getFlags*(command: CXCompileCommand): seq[string] =
  for arg in command.getArgs():
    if arg.startsWith("-"):
      result.add arg

proc parseTranslationUnit*(
    index: CXIndex,
    command: CXCompileCommand,
    extraFlags: seq[string] = @[],
    reparseOnNil: bool = true
  ): CXTranslationUnit =

  ## Get file and compilation flags from compilation command `command`
  ## and parse translation unit.
  ##
  ## ## Parameters
  ##
  ## :index: compilation index
  ## :command: Compilation command from database
  ## :extraFlags: Additional compilation flags for compiler. When parsing
  ##              c++ header files with `.h` extension you are most
  ##              likely need to use `@["-xc++"]` to make clang correctly
  ##              recognize the language.

  let args = extraFlags & getBuiltinHeaders().mapIt(&"-I{it}") &
    command.getFlags()

  let file = $command.getFilename()
  index.parseTranslationUnit(
    file.toAbsFile(true), args, {},
    reparseOnNIl = reparseOnNil
  )


proc parseFile*(
    file: AbsFile,
    config: ParseConf = baseCppParseConf,
    opts: set[CXTranslationUnitFlags] = {
      tufDetailedPreprocessingRecord, tufSkipFunctionBodies}
  ): CXTranslationUnit =
  let flags = config.getFlags(file)
  var index = createIndex()
  result = parseTranslationUnit(index, file, flags, opts)

proc parseFile*(
    file: AbsFile, conf: WrapConf, cache: var WrapCache): ParsedFile =


  file.assertExists()

  let flags = conf.parseConf.getFlags(file)
  result.filename = file
  result.index = createIndex()

  var conf = conf
  result.unit = parseTranslationUnit(
    result.index, file, flags, {
      tufSkipFunctionBodies, tufDetailedPreprocessingRecord})

  conf.unit = result.unit

  result.api = result.unit.splitDeclarations(conf, cache)
  result.isExplicitlyAdded = true


proc parseAll*(
    files: seq[AbsFile], conf: WrapConf, cache: var WrapCache
  ): hc_types.FileIndex =

  for file in files:
    result.index[file] = parseFile(file, conf, cache)

proc convertViaTs*(text: string): PNode =
  var text = text
  var cache: StringNameCache
  return parseCppString(addr text).conv(text, cache)




proc postFixEntries*(
    conf: CxxFixConf,
    entries: var seq[CxxEntry],
    lib: CxxLibImport,
    file: Option[AbsFile] = none AbsFile
  ) =
  var cache: StringNameCache

  var store = conf.typeStore

  # Fix all identifier names in entry lists
  for item in mitems(entries):
    fixIdentsRec(item, cache, conf)

  # Set spelling location file for all entries in the list
  if file.isSome():
    for item in mitems(entries):
      setFileRec(item, file.get())

  for item in mitems(entries):
    item.setStoreRec(store)
    registerDeclarations(item, store, lib)

  # Register type declarations in the store, add missing flags to type uses
  for item in mitems(entries):
    postprocessTypeUses(item, store, lib)

  # Set header for list of entries
  for item in mitems(entries):
    setHeaderRec(item, conf)


proc postFixEntries*(
    entries: sink seq[CxxEntry],
    conf: CxxFixConf,
    lib: CxxLibImport,
    file: Option[AbsFile] = none AbsFile
  ): seq[CxxEntry] =
  result = entries
  conf.postFixEntries(result, lib, file)


iterator mentries*(files: var seq[CxxFile]): var CxxEntry =
  for file in mitems(files):
    for entry in mitems(file.entries):
      yield entry

proc postFixEntries*(conf: CxxFixConf, files: var seq[CxxFile]) =
  var store = conf.typeStore

  # Fix all identifier names in entry lists
  for file in mitems(files):
    var cache: StringNameCache
    for item in mitems(file.entries):
      fixIdentsRec(item, cache, conf)

  # Set spelling location file for all entries in the list
  for file in mitems(files):
    for item in mitems(file.entries):
      setFileRec(item, file.original)

  for file in mitems(files):
    for item in mitems(file.entries):
      item.setStoreRec(store)
      registerDeclarations(item, store, file.savePath)

  # Register type declarations in the store, add missing flags to type uses
  for file in mitems(files):
    for item in mitems(file.entries):
      postprocessTypeUses(item, store, file.savEpath)

  # Set header for list of entries
  for item in mentries(files):
    setHeaderRec(item, conf)


proc postFixEntries*(
  files: sink seq[CxxFile], conf: CxxFixConf): seq[CxxFile] =

  result = files
  conf.postFixEntries(result)


proc wrapViaTs*(
    str: string,
    conf: CxxFixConf,
    lib: CxxLibImport
  ): seq[CxxEntry] =
  assertRef conf.typeStore
  # "Copy input string to local mutable variable":
  var str = str

  # "Parse CXX string":
  let node = parseCppString(addr str)

  # "Create comment sequence":
  var coms: seq[CxxComment]

  # "Convert to CXX":
  result = toCxx(node, coms)



proc wrapViaTs*(
    file: AbsFile,
    lib: CxxLibImport,
    conf: CxxFixConf,
    doPostFix: bool = true
  ): CxxFile =
  assertRef conf.typeStore

  wrapViaTs(
    file.readFile(),
    conf,
    lib,
  ).cxxFile(lib, file)


proc wrapViaTsWave*(
    file: AbsFile,
    lib: CxxLibImport,
    conf: CxxFixConf,
    waveCache: var WaveCache,
    parseConf: ParseConf
  ): CxxFile =
  assertRef conf.typeStore

  # "Construct wave reader object":
  var reader = newWaveReader(file, waveCache, parseConf)

  # "Get sequence of elements for wrapping":
  var s = wrapViaTs(reader.getExpanded(), conf, lib)

  # "Wrap results in file":
  result = s.cxxFile(lib, file)

proc wrapViaClang*(conf: WrapConf, file: AbsFile): CxxFile =
  var cache = WrapCache(
    importGraph: hgraph.default(typeof WrapCache.importGraph))

  let parsed = parseFile(file, conf, cache)
  conf.unit = parsed.unit
  toCxxFile(parsed, conf, cache)

proc expandViaCc*(file: AbsFile, parseConf: ParseConf): string =
  ## Return expanded content of the @arg{file} using @sh{clang}. Uses
  ## include paths and other flags from @arg{parseConf}. Expanded form does
  ## not contain `#line` directives, but preserves comments.
  assertExists(file)
  let flags = getFlags(parseConf, file)
  var cmd = shellCmd(clang, -C, -E, -P)
  for flag in flags:
    cmd.raw flag

  cmd.arg file

  result = evalShellStdout(cmd)



proc expandPartViaCc*(
    file: AbsFile,
    parseConf: ParseConf,
    compiler: string = "clang"
  ): string =
  assertExists(file)
  let flags = getFlags(parseConf, file)
  var cmd = shellCmdGnu(compiler) # shellCmd(clang, -C, -E)
  cmd.flag "C"
  cmd.flag "E"

  cmd.raw "-Wno-deprecated"

  for flag in flags:
    cmd.raw flag

  cmd.arg file

  # try:
  let expanded = evalShellStdout(cmd)

  # except ShellError as e:
  #   echov "-----------------"
  #   echov e.msg
  #   echov "-----------------"

  # echo expanded

  # echov expanded

  var active = false
  for line in strutils.splitLines(expanded):
    if len(line) > 0 and char(line[0]) == char('#'):
      let split = line.split(" ")
      var num: int
      if parseInt(split[1], num) == len(split[1]):
        echov "", split[2][1..^2]
        echov "", file
        if split[2][1 .. ^2] == file.string:
          active = true

        else:
          active = false

    elif active:
      result.add line
      result.add "\n"

  echov result


  raise newImplementError()

template fileExpandLoop(logger, expandExpr: untyped): untyped =
  mkDir outDir
  for file {.inject.} in files:
    let resFile = (outDir /. file.name()) &. "h"
    result[resFile] = file

    if not exists(resFile) or force:
      resFile.writeFile(expandExpr)

      if ?logger:
        logger.info "Expanded", file, "to", resFile

    else:
      if ?logger:
        logger.info "No need to expand", file

type CxxExpandMap* = Table[AbsFile, AbsFile]

proc expandViaCC*(
    files: seq[AbsFile],
    outDir: AbsDir,
    conf: ParseConf,
    compiler: string = "clang",
    force: bool = false,
    logger: HLogger = nil
  ): CxxExpandMap =

  fileExpandLoop(logger, expandPartViaCc(file, conf, compiler))



proc getCompilerDefines*(compiler: string = "clang"):
  seq[(string, seq[string], Option[string])] =

  let file = getTempDir() /. "test.hpp"
  writeFile(file, "")

  var cmd = shellCmdGnu(compiler)
  cmd.raw("-dM")
  cmd.flag("E")
  cmd.arg $file

  let expanded = evalShellStdout(cmd)

  for line in expanded.splitLines():
    let def = line.split(" ")
    assert def[0] == "#define"
    if def[1] notin [
      "__STDC__",
      "__cplusplus",
      "__LINE__",
      "__FILE__",
      "__BASE_FILE__",
      "__DATE__",
      "__TIME__",
      "__INCLUDE_LEVEL__"
    ]:
      result.add((def[1], newSeq[string](), some def[2..^1].join(" ")))

proc expandViaWave*(
    file: AbsFile,
    cache: var WaveCache,
    conf: ParseConf,
    logger: HLogger = nil
  ): string =

  var reader = newWaveReader(file, cache, conf)


  try:
    return reader.getExpanded()

  except WaveError as err:
    if ?logger:
      logger.logLines(
        AbsFile($err.diag.filename),
        lang = "c",
        center = err.diag.line,
        column = err.diag.column
      )

    raise err


proc expandViaWave*(
    files: seq[AbsFile],
    outDir: AbsDir,
    conf: ParseConf,
    force: bool = false,
    logger: HLogger = nil
  ): CxxExpandMap =

  var cache = newWaveCache()
  fileExpandLoop(
    logger, expandViaWave(file, cache, conf, logger))

  # mkDir outDir
  # for file in files:
  #   let resFile = (outDir /. file.name()) &. "h"
  #   result[resFile] = file

  #   if not exists(resFile) or force:
  #     resFile.writeFile()

  #     if ?logger:
  #       logger.info "Expanded", file, "to", resFile

  #   else:
  #     if ?logger:
  #       logger.info "No need to expand", file


const importMapFile* = "type_import.json"

proc getImportMap*(files: seq[CxxFile]): CxxTypeImportMap =
  for file in files:
    for entry in file.entries:
      if entry of {cekObject, cekAlias, cekEnum}:
        result[CxxNameString(entry.cxxName.scopes.join("::"))] = file.savePath

func `$`*(s: CxxNameString): string = s.string

proc readImportMap*(store: var CxxTypeStore, file: AbsFile) =
  assertExists(file)
  let j = readFile(file)
  let map = j.fromJson(Table[string, CxxLibImport])
  for key, val in map:
    store.importDecls[cxxName(key.split("::"))] = val

proc readImportMap*(store: var CxxTypeStore, dirs: seq[AbsDir]) =
  for dir in dirs:
    for file in walkDir(dir, AbsFile, exts = @["json"]):
      if file.name() == "type_import":
        readImportMap(store, file)
        break



proc initCSharedLibFixConf*(
    lib: string,
    packageName: string,
    isIcpp: bool,
    libRoot: AbsDir,
    expandMap: CxxExpandMap,
    configFiles: seq[string] = @["lib" & lib & "_config"],
    base: CxxFixConf         = baseFixConf,
    libIncludePrefix: string = lib,
    nameStyle: IdentStyle    = idsSnake,
    depDirs: seq[AbsDir] = @[]
  ): CxxFixConf =

  var fixConf = base
  fixConf.isIcpp = isIcpp
  fixConf.libName = packageName

  fixConf.onGetBind():
    case entry.kind:
      of cekProc:
        result = cxxMacroBind(lib & "Proc")

      of cekObject, cekForward:
        let base = expandMap[entry.getLocation.file].string
        let path = base.string.dropPrefix(libRoot.string)
        result = cxxHeader("<" & libIncludePrefix & path & ">")

      else:
        result = cxxNoBind()

    result.imports.add cxxLibImport(fixConf.libName, configFiles)

  fixConf.libNameStyle = nameStyle

  fixConf.onFixName():
    cache.fixContextedName(name, fixConf.libNameStyle)

  fixConf.typeStore = newTypeStore()
  fixConf.typeStore.readImportMap(depDirs)

  return fixConf

proc wrapViaTs*(
    root: AbsDir,
    conf: CxxFixConf,
    exts: seq[string] = @["h"]
  ): seq[CxxFile] =

  for file in walkDir(root, AbsFile, exts = exts):
    let lib = conf.libImport(root, file)
    try:
      result.add wrapViaTs(file, lib, conf, doPostFix = false)

    except ImplementKindError as err:
      err.msg.add "\nException raised while processing file " & file.string
      raise err

type
  GenFiles = object
    genNim*: seq[AbsFile]
    genTypeMap*: Option[AbsFile]

proc writeFiles*(
    outDir: AbsDir,
    files: seq[CxxFile],
    codegenConf: CodegenConf,
    extraTypes: seq[(CxxName, CxxLibImport)] = @[]
  ): GenFiles =

  let mapFile = outDir /. importMapFile
  result.genTypeMap = some mapFile
  var map = getImportMap(files)
  for (ctype, cimport) in extraTypes:
    map[CxxNameString(ctype.scopes.join("::"))] = cimport

  writeFile(mapFile, toJson(map))

  let group = regroupFiles(files)

  for fix in group:
    let res = outDir / fix.getFile().withExt("nim")
    res.writeFile(toNNode[PNode](fix, codegenConf).toPString(
      codegenFormatConf))

    result.genNim.add res


proc wrapCSharedLibViaTsWave*(
    inDir, tmpDir, outDir: AbsDir,
    libName, packageName: string,
    ignoreIn: seq[string]                    = @[],
    persistentOut: seq[string]               = @[
      "hcparse_generate", "lib" & libName & "_config"],
    depDirs: seq[AbsDir]                     = @[],
    extraTypes: seq[(CxxName, CxxLibImport)] = @[],
    codegen: CodegenConf = cCodegenConf
  ): GenFiles =
  var expandMap = expandViaWave(
    listFiles(inDir, ignoreNames = ignoreIn),
    tmpDir, baseCParseConf
  )

  rmFiles(outDir, @["nim"], persistentOut)

  var codegen = codegen

  let
    fixConf = initCSharedLibFixConf(
      libName, packageName, false, inDir, expandMap, depDirs = depDirs)

  codegen.nameStyle = fixConf.libNameStyle

  var resultWrapped = tmpDir.wrapViaTs(fixConf)

  fixConf.postFixEntries(resultWrapped)

  var resultGrouped = writeFiles(
    outDir, resultWrapped, codegen, extraTypes = extraTypes)

  return resultGrouped

proc validateGenerated*(files: GenFiles) =
  for file in items(files.genNim):
    try:
      execShell shellCmd(
        nim,
        check,
        errormax = 3,
        spellSuggest = 0,
        $file)

    except ShellError:
      echo "> ", file
      echo "file fail"


# proc registerTypes*(files: var seq[CxxFile]) =
#   var store = CxxTypeStore()
#   for file in mitems(files):
#     for entry in mitems(file.entries):
#       setTypeStoreRec(entry, store, file.savePath)

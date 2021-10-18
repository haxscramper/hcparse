import
  std/[sequtils, strformat, bitops, strutils, tables]

import
  pkg/[jsony]

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
  ./interop_ir/wrap_store

import
  hnimast,
  hnimast/pprint

import
  hmisc/other/[oswrap, hshell, hpprint],
  hmisc/types/colorstring,
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
  for opt in trOptions:
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

proc wrapViaTs*(
    str: string,
    conf: CxxFixConf,
    lib: CxxLibImport,
    file: Option[AbsFile] = none AbsFile
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

  var cache: StringNameCache
  var store: CxxTypeStore = conf.typeStore
  for item in mitems(result):
    if file.isSome():
      setFileRec(item, file.get())

    setHeaderRec(item, conf)
    fixIdentsRec(item, cache, conf)
    setTypeStoreRec(item, store, lib)

proc wrapViaTs*(
    file: AbsFile,
    libRoot: AbsDir,
    conf: CxxFixConf
  ): CxxFile =
  assertRef conf.typeStore
  let relative = file.string.dropPrefix(libRoot.string)
  let lib = cxxLibImport(conf.libName, relative.split("/").filterIt(it.len > 0))
  wrapViaTs(file.readFile(), conf, lib, some file).cxxFile(lib)

proc wrapViaTsWave*(
    file: AbsFile,
    libRoot: AbsDir,
    conf: CxxFixConf,
    waveCache: var WaveCache,
    userIncludes: seq[string] = @[],
    sysIncludes: seq[string] = @[],
    subTargets: seq[string] = @[]
  ): CxxFile =
  assertRef conf.typeStore
  # "Wrap via TS wave":

  # "Get relative path of the file using library root and file path":
  let relative = file.string.dropPrefix(libRoot.string)

  # "Construct wave reader object":
  var reader = newWaveReader(
    file, waveCache, userIncludes, sysIncludes, subTargets)

  let lib = cxxLibImport(libRoot.name(), relative.split("/"))

  # "Get sequence of elements for wrapping":
  var s = wrapViaTs(reader.getExpanded(), conf, lib)

  # "Wrap results in file":
  result = s.cxxFile(lib)

proc wrapViaClang*(conf: WrapConf, file: AbsFile): CxxFile =
  var cache: WrapCache
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

proc expandViaWave*(
    file: AbsFile,
    cache: var WaveCache,
    conf: ParseConf
  ): string =

  var reader = newWaveReader(
    file, cache, conf.userIncludes, conf.sysIncludes)

  return reader.getExpanded()

type CxxExpandMap* = Table[AbsFile, AbsFile]

proc expandViaWave*(
    files: seq[AbsFile],
    outDir: AbsDir,
    conf: ParseConf,
    force: bool = false
  ): CxxExpandMap =

  mkDir outDir
  var cache = newWaveCache()
  for file in files:
    let resFile = (outDir /. file.name()) &. "h"
    result[resFile] = file

    if not exists(resFile) or force:
      resFile.writeFile(expandViaWave(file, cache, conf))


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

      of cekObject:
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
    fixConf: CxxFixConf,
    exts: seq[string] = @["h"]
  ): seq[CxxFile] =

  for file in walkDir(root, AbsFile, exts = exts):
    try:
      result.add wrapViaTs(file, root, fixConf)

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
    res.writeFile(toNNode[PNode](fix, codegenConf).toPString())
    result.genNim.add res


proc wrapCSharedLibViaTsWave*(
    inDir, tmpDir, outDir: AbsDir,
    libName, packageName: string,
    ignoreIn: seq[string] = @[],
    persistentOut: seq[string] = @[
      "hcparse_generate", "lib" & libName & "_config"],
    depDirs: seq[AbsDir] = @[],
    extraTypes: seq[(CxxName, CxxLibImport)] = @[]
  ): GenFiles =
  var expandMap = expandViaWave(
    listFiles(inDir, ignoreNames = ignoreIn),
    tmpDir, baseCParseConf
  )

  rmFiles(outDir, @["nim"], persistentOut)

  let
    fixConf = initCSharedLibFixConf(
      libName, packageName, false, inDir, expandMap, depDirs = depDirs)

    resultWrapped = tmpDir.wrapViaTs(fixConf)
    resultGrouped = writeFiles(outDir, resultWrapped, cCodegenConf, extraTypes = extraTypes)

  return resultGrouped

proc validateGenerated*(files: GenFiles) =
  for file in items(files.genNim):
    try:
      execShell shellCmd(nim, check, errormax = 3, $file)

    except ShellError:
      echo "> ", file
      echo "file fail"


# proc registerTypes*(files: var seq[CxxFile]) =
#   var store = CxxTypeStore()
#   for file in mitems(files):
#     for entry in mitems(file.entries):
#       setTypeStoreRec(entry, store, file.savePath)

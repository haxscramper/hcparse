import
  std/[
    sequtils,
    strformat,
    bitops,
    strutils,
    tables,
    parseutils
  ]

import
  # pkg/[jsony],
  pkg/[frosty]

import
  ./read_libclang/[
    cxtypes,
    libclang_wrap,
    hc_types,
    hc_visitors,
    hc_clangreader
  ],

  ./read_tree_sitter/[
    hc_tsreader,
    hc_tsconvert
  ],

  ./read_boost_wave/[
    hc_wavereader
  ],

  ./processor/[
    hc_grouping,
    hc_postprocess,
    wrap_store
  ],

  ./codegen/[
    hc_codegen
  ],

  ./hc_impls


import
  hnimast,
  hnimast/pprint

import
  hmisc/other/[
    oswrap,
    hshell,
    hpprint,
    hlogger
  ],
  hmisc/types/[
    colorstring,
    hgraph
  ],
  hmisc/algo/[
    hstring_algo,
    namegen
  ],
  hmisc/hasts/[
    xml_serde
  ],
  hmisc/scripts/[
    nim_test
  ],
  hmisc/core/all

export
  hc_wavereader,
  hc_grouping,
  hc_impls,
  hc_codegen,
  hc_postprocess,
  wrap_store,
  oswrap,
  libclang_wrap,
  cxtypes,
  hc_types

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


proc parseFileViaClang*(
    file: AbsFile,
    config: ParseConf = baseCppParseConf,
    opts: set[CXTranslationUnitFlags] = {
      tufDetailedPreprocessingRecord, tufSkipFunctionBodies}
  ): CXTranslationUnit =
  let flags = config.getFlags(file)
  var index = createIndex()
  result = parseTranslationUnit(index, file, flags, opts)

proc parseFileViaClang*(
    file: AbsFile, conf: WrapConf, cache: var WrapCache): ParsedFile =


  file.assertExists()

  let flags = conf.parseConf.getFlags(file)
  result.filename = file
  result.index = createIndex()

  var conf = conf
  let unit = parseTranslationUnit(
    result.index, file, flags, {
      tufSkipFunctionBodies, tufDetailedPreprocessingRecord})

  result.api = unit.splitDeclarations(conf, cache)
  result.isExplicitlyAdded = true

proc wrapViaClang*(
    file: ParsedFile,
    wrapConf: WrapConf,
    fixConf: CxxFixConf,
    cache: var WrapCache,
    dir: AbsDir
  ): CxxFile =

  var fixConf = fixConf
  fixConf.onGetBind():
    return cxxHeader(file.filename)

  let lib = fixConf.libImport(dir, file.filename)

  let wrapped: seq[CxxEntry] = file.api.wrapApiUnit(wrapConf, cache).
    postFixEntries(fixConf, lib)

  return cxxFile(wrapped, lib, file.filename)

proc wrapViaClang*(
    files: seq[AbsFile],
    wrapConf: WrapConf,
    fixConf: CxxFixConf,
    cache: var WrapCache,
    dir: AbsDir
  ): seq[CxxFile] =

  for file in files:
    let file = parseFileViaClang(file, wrapConf, cache)
    result.add wrapViaClang(file, wrapConf, fixConf, cache, dir)

proc convertViaTs*(text: string, conf: ConvConf): PNode =
  var text = text
  var cache: StringNameCache
  return parseCppString(addr text).conv(text, cache, conf)



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

  let expanded = evalShellStdout(cmd)

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

const importMapFile* = "hcparseTypeImport.xml"

type
  CxxImportMap = object
    table*: OrderedTable[CxxName, CxxLibImport]

proc getImportMap*(files: seq[CxxFile]): CxxImportMap =
  for file in files:
    for entry in file.entries:
      if entry of {cekObject, cekAlias, cekEnum}:
        result.table[entry.cxxName()] = file.savePath

proc readImportMap*(store: var CxxTypeStore, file: AbsFile) =
  assertExists(file)
  let j = readFile(file)
  let map = j.fromXml(CxxImportMap, "type-map")
  for key, val in map.table:
    store.importDecls[key] = val

proc findImportMaps*(paths: seq[AbsDir], cwd: AbsDir): seq[AbsFile] =
  for path in paths:
    if (path notin cwd) and (cwd notin path):
      for dir in parentDirs(path):
        if exists(dir /. importMapFile):
          result.add dir /. importMapFile


proc initCSharedLibFixConf*(
    lib:              string,
    packageName:      string,
    isIcpp:           bool,
    libRoot:          AbsDir,
    expandMap:        CxxExpandMap = default(CxxExpandMap),
    configFiles:      seq[string]  = @["lib" & lib & "_config"],
    base:             CxxFixConf   = baseFixConf,
    libIncludePrefix: string       = lib,
    nameStyle:        IdentStyle   = idsSnake,
    typeMaps: seq[AbsFile] = @[]
  ): CxxFixConf =


  var fixConf = base
  fixConf.isIcpp = isIcpp
  fixConf.libName = packageName

  fixConf.onGetBind():
    case entry.kind:
      of cekProc:
        result = cxxMacroBind(lib & "Proc")

      of cekObject, cekForward:
        let file = entry.getLocation.file
        let base =
          if file in expandMap:
            expandMap[file].string

          else:
            file.string

        let path = base.dropPrefix(libRoot.string)
        result = cxxHeader("<" & libIncludePrefix & path & ">")

      else:
        result = cxxNoBind()

    result.imports.add cxxLibImport(fixConf.libName, configFiles)

  fixConf.onFixName():
    cache.fixContextedName(name, nameStyle)

  fixConf.typeStore = newTypeStore()
  for file in typeMaps:
    fixConf.typeStore.readImportMap(file)

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
    extraTypes: seq[(CxxName, CxxLibImport)] = @[],
    outMapDir: AbsDir = outDir.dir()
  ): GenFiles =

  let mapFile = outMapDir /. importMapFile
  result.genTypeMap = some mapFile
  var map = getImportMap(files)
  for (ctype, cimport) in extraTypes:
    map.table[ctype] = cimport

  writeFile(mapFile, toXml(map, "type-map"))

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
      "hcparse_generate_lib" & libName,
      "lib" & libName & "_config"
    ],
    typeMaps: seq[AbsFile]                   = @[],
    extraTypes: seq[(CxxName, CxxLibImport)] = @[],
    codegen: CodegenConf                     = cCodegenConf,
    targetExts: seq[string]                  = @["h", "hpp", "hxx", "cxx", "c", "cpp"],
    nameStyle: IdentStyle                    = idsSnake,
  ): GenFiles =
  var expandMap = expandViaWave(
    listFiles(inDir, ignoreNames = ignoreIn, exts = targetExts),
    tmpDir, baseCParseConf
  )

  if exists(outDir):
    rmFiles(outDir, @["nim"], persistentOut)

  else:
    mkDir outDir

  var codegen = codegen

  let
    fixConf = initCSharedLibFixConf(
      libName,
      packageName,
      false,
      inDir,
      expandMap,
      typeMaps = typeMaps,
      nameStyle = nameStyle
    )

  codegen.nameStyle = nameStyle

  var resultWrapped = tmpDir.wrapViaTs(fixConf)

  fixConf.postFixEntries(resultWrapped)

  var resultGrouped = writeFiles(
    outDir, resultWrapped, codegen, extraTypes = extraTypes)

  return resultGrouped

proc validateGenerated*(
    files: GenFiles,
    testDir: Option[AbsDir] = none AbsDir,
    nimState: Option[NimRunConf] = none NimRunConf,
    logger: HLogger = newTermLogger()
  ) =

  logger.info "Running test validation"
  for file in items(files.genNim):
    try:
      execShell shellCmd(
        nim,
        check,
        errormax = 3,
        spellSuggest = 0,
        $file)

      logger.success file

    except ShellError:
      logger.fail file

  if testDir.canGet(test):
    discard runTestDir(test, nimState.get())


# proc registerTypes*(files: var seq[CxxFile]) =
#   var store = CxxTypeStore()
#   for file in mitems(files):
#     for entry in mitems(file.entries):
#       setTypeStoreRec(entry, store, file.savePath)

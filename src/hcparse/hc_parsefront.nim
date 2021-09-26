import
  std/[sequtils, strformat, bitops, strutils, tables]

import
  ./cxtypes,
  ./hc_types,
  ./hc_impls,
  ./hc_visitors,
  ./hc_tsreader,
  ./hc_tsconvert,
  ./hc_codegen,
  ./hc_irgen,
  ./interop_ir/wrap_store

import
  hnimast

import
  hmisc/other/[oswrap, hshell],
  hmisc/types/colorstring,
  hmisc/algo/[hstring_algo, namegen]

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
    conf: CxxFixConf
  ): seq[CxxEntry] =
  var str = str
  let node = parseCppString(addr str)
  result = toCxx(node)

  var cache: StringNameCache
  for item in mitems(result):
    setHeaderRec(item, conf)
    fixIdentsRec(item, cache, conf)

proc wrapViaTs*(
    file: AbsFile,
    libRoot: AbsDir,
    conf: CxxFixConf
  ): CxxFile =

  let relative = file.string.dropPrefix(libRoot.string)
  wrapViaTs(file.readFile(), conf).cxxFile(
    cxxLibImport(libRoot.name(), relative.split("/")))

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

proc registerTypes*(files: var seq[CxxFile]) =
  var store = CxxTypeStore()
  for file in mitems(files):
    for entry in mitems(file.entries):
      setTypeStoreRec(entry, store, file.savePath)

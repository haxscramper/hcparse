import std/[tables, options, strutils, strformat,
            sequtils, bitops, sugar, deques, sets]

import hnimast

import hmisc/helpers
import hmisc/algo/hstring_algo
import hmisc/other/[oswrap, colorlogger, hshell]
import hmisc/types/colorstring
import gram

import cxtypes, cxcommon, hc_types, hc_visitors, hc_typeconv,
       hc_depresolve, hc_wrapgen, hc_impls, hc_postprocess,
       hc_docwrap


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

  # echo cmdline

  block:
    let argc = cmdline.len
    let cmdlineC = allocCSTringArray(cmdline)

    # echo cmdline

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


proc getFlags*(config: ParseConfig, file: AbsFile): seq[string] =
  ## Get list of command-line flags for partigular `file`. This includes
  ## both global flags, and file-specific ones
  result.add config.includepaths.toIncludes()
  result.add config.globalFlags
  result.add config.fileFlags.getOrDefault(file)

proc parseFile*(
    file: AbsFile,
    config: ParseConfig = baseCppParseConfig,
    opts: set[CXTranslationUnitFlags] = {
      tufDetailedPreprocessingRecord, tufSkipFunctionBodies}
  ): CXTranslationUnit =
  let flags = config.getFlags(file)
  var index = createIndex()
  result = parseTranslationUnit(index, file, flags, opts)

proc parseFile*(
    file: AbsFile,
    config: ParseConfig,
    wrapConf: WrapConfig,
    reparseOnNil: bool = true
  ): ParsedFile =


  file.assertExists()
  # info "Parsing file", file
  identLog()

  let flags = config.getFlags(file)
  result.filename = file
  result.index = createIndex()

  var wrapConf = wrapConf
  try:
    result.unit = parseTranslationUnit(
      result.index, file, flags, {
        tufSkipFunctionBodies, tufDetailedPreprocessingRecord},
      reparseOnNil = reparseOnNil
    )

    wrapConf.unit = result.unit

  except:
    err file.realpath
    debug config.getFlags(file).joinl()
    raise

  result.api = result.unit.splitDeclarations(wrapConf)
  result.explicitDeps = result.api.publicApi.
    getDepFiles(wrapConf).filterIt(it != file)

  result.isExplicitlyAdded = true

  dedentLog()

proc incl*[N, E, F](gr: var Graph[N, E, F], val: N) =
  if val notin gr:
    discard gr.add(val)

proc contains*[N, E, F](gr: Graph[N, E, F], pair: (N, N)): bool =
  if (pair[0] notin gr) or (pair[1] notin gr):
    return false

  for (edge, node) in gr[pair[0]].incoming():
    if node.value == pair[1]:
    # if edge in gr:
      return true


  for (edge, node) in gr[pair[0]].outgoing():
    # if edge in gr:
    if node.value == pair[1]:
      return true

proc incl*[N, E, F](
  gr: var Graph[N, E, F], pair: (N, N), edgeVal: E) =
  if pair notin gr:
    discard gr.edge(gr[pair[0]], edgeVal, gr[pair[1]])



proc registerDeps*(graph: var HeaderDepGraph, parsed: ParsedFile) =
  let file = parsed.filename

  for dep in parsed.api.includes:
    let
      path = dep.includedPath.realpath
      ifrm = dep.includedFrom.realpath

    graph.incl(path)
    graph.incl(ifrm)
    # notice ifrm, " -> ", path
    graph.incl((ifrm, path), &"{dep.includedAs}:{dep.fromLine}",)


  for dep in parsed.explicitDeps:
    graph.incl(file.realpath)
    graph.incl(dep.realpath)
    graph.incl((file.realpath, dep.realpath), "@@@")


proc parseAll*(
  files: seq[AbsFile],
  conf: ParseConfig,
  wrapConf: WrapConfig, ): FileIndex =
  for file in files:
    result.index[file] = parseFile(file, conf, wrapConf)

  result.depGraph = newGraph[AbsFile, string](HeaderGraphFlags)

  for file, parsed in result.index:
    result.depGraph.registerDeps(parsed)



import hasts/graphviz_ast
export toPng, toXDot, AbsFile

func dotRepr*(idx: FileIndex, onlyPP: bool = true): DotGraph =
  result.styleNode = makeRectConsolasNode()
  ## Generate dot representation of dependencies in file index

  result.rankdir = grdLeftRight
  for file in idx.depGraph.nodes:
    result.addNode(makeDotNode(hash file.value, file.value.getStr()))

  for (source, edge, target) in idx.depGraph.edges:
    var e =  makeDotEdge(
      hash source.value,
      hash target.value,
      edge.value
    )

    if edge.value == "@@@":
      e.style = edsDashed
      e.label = none(string)
      if not onlyPP:
        result.addEdge e
    else:
      result.addEdge e

proc getDepModules*(file: AbsFile, idx: FileIndex): seq[AbsFile] =
  ## Get list of modules that have to be imported in wrapper for file
  ## `file`.
  for dep in idx.depGraph[file].outgoing():
    result.add dep.target.value

#*************************************************************************#
#****************************  File wrapping  ****************************#
#*************************************************************************#
func makeImport*(names: seq[string]): PNode =
  nnkImportStmt.newPTree(
    names.mapIt(it.newPident()).foldl(
      nnkInfix.newPTree(newPident("/"), a, b))
  )

proc getExports*(
  parsed: ParsedFile, conf: WrapConfig, index: FileIndex): seq[AbsFile] =
  ## Get list of absolute files that provide types, used in public API
  ## for `parsed` file *and* marked as internal (e.g. not supposed to
  ## be imported separately)
  for dep in parsed.explicitDeps:
    if conf.isInternal(dep, conf, index):
      result.add dep

proc wrapFile*(
  parsed: ParsedFile, conf: WrapConfig,
  cache: var WrapCache, index: FileIndex): seq[WrappedEntry] =
  ## Create wrapper for parsed file and return list of wrapped entries. All
  ## type declarations are deduplicated and combined into single
  ## `WrappedEntry` multitype declaration.

  # info "Wrapping", parsed.filename
  var tmpRes: seq[WrappedEntry]
  tmpRes.add newWrappedEntry(
    toNimDecl(
      pquote do:
        import bitops, hcparse/wraphelp
    )
  )

  for node in parsed.explicitDeps.mapIt(
      conf.getImport(it, conf)).
      deduplicate().
      mapIt(it.makeImport()):
    tmpRes.add node.toNimDecl().newWrappedEntry()

  tmpRes.add parsed.api.wrapApiUnit(conf, cache, index)

  # Coollect all types into single wrapped entry type block. All duplicate
  # types (caused by forward declarations which is not really possible to
  # differentiated) will be overwritten. This should be fine I guess,
  # because you can't declare type again after defining it (I hope), so all
  # last type encounter will always be it's definition.
  var res: Table[string, WrappedEntry]

  for elem in tmpRes:
    if elem.kind == wekNimDecl:
      case elem.wrapped.kind:
        # Filter out all type declarations.
        of nekObjectDecl:
          let name = elem.wrapped.objectdecl.name.head
          res[name] = elem

        of nekEnumDecl:
          let name = elem.wrapped.enumdecl.name
          res[name] = elem

        of nekAliasDecl:
          let name = elem.wrapped.aliasdecl.newType.head
          if name in res:
            warn "Override type alias for ", name

          res[name] = elem

        of nekPasstroughCode:
          raiseAssert("Passthrough code blocks should use `wekNimPass`")

        of nekProcDecl, nekMultitype:
          discard

    elif elem.kind == wekNimPass and
         not elem.postTypes:
      # Immediately append non-`postTypes` declarations
      result.add elem


  block:
    let elems = collect(newSeq):
      for k, v in res:
        v

    result.add(newWrappedEntry(elems))

  for elem in tmpRes:
    if elem.kind == wekProc or
      (elem.kind == wekProc and elem.wrapped.kind notin {
      nekObjectDecl, nekAliasDecl, nekPasstroughCode, nekEnumDecl
    }):

      result.add elem

    elif
      elem.kind == wekNimPass or (
      elem.kind == wekNimDecl and
      elem.wrapped.kind == nekPasstroughCode
      ) and elem.postTypes:

      result.add elem



proc wrapFile*(
    file: AbsFile,
    flags: seq[string],
    conf: WrapConfig,
    cache: var WrapCache,
    index: FileIndex
   ): tuple[parsed: ParsedFile, wrapped: seq[WrappedEntry]] =

  let parsedConf = ParseConfig(
    globalFlags: getBuiltinHeaders().toIncludes(),
    fileFlags: { file : flags }.toTable()
  )

  result.parsed = parseFile(file, parsedConf, conf)
  result.wrapped = result.parsed.wrapFile(conf, cache, index)

proc wrapFile*(
    cmd: CXCompileCommand,
    extraFlags: seq[string],
    conf: WrapConfig,
    cache: var WrapCache,
    index: FileIndex
  ): tuple[parsed: ParsedFile, wrapped: seq[WrappedEntry]] =

  wrapFile(
    toAbsFile($cmd.getFilename(), true),
    extraFlags & cmd.getFlags(), conf, cache, index)

type
  WrapResult* = object
    parsed*: ParsedFile
    wrapped*: seq[WrappedEntry]
    infile*: AbsFile
    importName*: seq[string]

proc boolCall*[A](
  cb: proc(a: A): bool, arg: A, default: bool = true): bool =
  if cb == nil: default else: cb(arg)

func wrapName*(res: WrapResult): string =
  res.importName.join("/") & ".nim"

proc wrapAll*(
  files: seq[AbsFile],
  parseConf: ParseConfig,
  wrapConf: WrapConfig
            ): tuple[wrapped: seq[WrapResult], index: FileIndex] =

  var
    que = initDeque[AbsFile]()
    visited: HashSet[AbsFile]
    cache: WrapCache
    parsed: FileIndex = files.parseAll(parseConf, wrapConf)

  for file, _ in parsed.index:
    que.addLast file # Add all files to que
    visited.incl file # Mark all as visited

  while que.len > 0:
    let file = que.popFirst()
    # info "Parsing file", file
    if file notin visited: # If dependency is new parse it
      parsed.index[file] = file.parseFile(parseConf, wrapConf)
      visited.incl file

    # Add to graph
    parsed.depGraph.registerDeps(parsed.index[file])

    # Store all explicit dependencies for file
    for dep in parsed.index[file].explicitDeps:
      if dep in visited or wrapConf.ignoreFile(dep):
        discard
      else:
        que.addLast dep

    for dep in parsed.index[file].api.includes:
      if dep.includedPath in visited or
         wrapConf.ignoreFile.boolCall(dep.includedPath):
        discard
      else:
        que.addLast dep.includedPath

  var wrap = wrapConf

  for file in visited:
    wrap.header = file
    wrap.unit = parsed.index[file].unit
    result.wrapped.add WrapResult(
      parsed: parsed.index[file],
      infile: file,
      importName: wrap.getImport(file, wrap),
      wrapped: parsed.index[file].wrapFile(wrap, cache, parsed)
    )

  result.index = parsed

proc postprocessWrapped*(
    entries: seq[WrappedEntry],
    wrapConf: WrapConfig,
    postprocess: seq[Postprocess] = defaultPostprocessSteps,
  ): tuple[wrapped: seq[WrappedEntry], codegen: seq[CxxCodegen]] =

  for we in entries:
    var we = we
    var res: seq[WrappedEntry]

    for step in postprocess:
      res.add step.impl(we, wrapConf, result.codegen)

    result.wrapped.add we
    result.wrapped.add res


proc getExpanded*(file: AbsFile, parseConf: ParseConfig): string =
  let flags = getFlags(parseConf, file)
  var cmd = shCmd(clang, -C, -E, -P)
  for flag in flags:
    cmd.raw flag

  cmd.arg file

  result = evalShellStdout(cmd)

proc wrapSingleFile*(
    file: FsFile,
    errorReparseVerbose: bool = false,
    wrapConf: WrapConfig = baseCppWrapConf,
    parseConf: ParseConfig = baseCppParseConfig,
    postprocess: seq[Postprocess] = defaultPostprocessSteps,
  ): tuple[decls: seq[NimDecl[PNode]], codegen: seq[CxxCodegen]] =
  ## Generate wrapper for a single file.
  ##
  ## - @arg{postprocess} :: collection of postprocessing actions on
  ##  generated delarations, before they are converter to sequence of
  ##  `NimDecl`
  ##
  ##`wrapConf` provide user-defined implementation heuristics for necessary
  ## edge cases (see `WrapConfig` type documentation). `postprocess` is a
  ## sequence of postprocessing actions that will be run on generated
  ## `WrappedEntry` structures and then added to final declaration. Default
  ## implementation of postprocessing includes automatic enum overload
  ## derivation, nim-like infix operators (`<<` and `>>` converter to `shl`
  ## and `shr` respectively) and final fixup for all identifiers. Order of
  ## postprocessint steps is important, as every original wrapped entry is
  ## passed to each postprocess in sequential order (if you have three
  ## steps, `class C{};` wrapper will be passed to each of them, before
  ## being added to final result).
  ##
  ## Returns list of nim declarations with associated metadata
  ## (instantiation info in codegen callback, comments etc.) and list of
  ## C++ codegen files.

  if wrapConf.baseDir.len == 0:
    raiseArgumentError(".baseDir is not set for wrapper configuration")

  var
    cache: WrapCache
    index: FileIndex

  fillDocComments(getExpanded(toAbsFile(file), parseConf), cache)

  let parsed = parseFile(
    file.toAbsFile(), parseConf, wrapConf,
    reparseOnNil = errorReparseVerbose
  )

  var wrapConf = wrapConf

  wrapConf.unit = parsed.unit
  # info "Wrapping file", file

  let wrapped = parsed.wrapFile(wrapConf, cache, index)

  proc updateComments(decl: var PNimDecl, node: WrappedEntry) =
    decl.addCodeComment(
      "Wrapper for `" &
      (
        node.getCursor().getSemanticNamespaces(filterInline = false)).join("::") &
      "`\n"
    )

    if node.getCursor().getSpellingLocation().getSome(loc):
      let file = withoutPrefix(AbsFile(loc.file), wrapConf.baseDir)
      decl.addCodeComment(
        &"Declared in {file}:{loc.line}")





  let (wrapResults, codegen) = wrapped.postprocessWrapped(wrapConf, postprocess)
  result.codegen = codegen

  for node in wrapResults:
    case node.kind:
      of wekMultitype:
        var resdecl: seq[PNimTypeDecl]
        for t in node.decls:
          assert t.kind != wekMultitype
          var decl = t.wrapped

          updateComments(decl, t)
          resdecl.add toNimTypeDecl(decl)

        result.decls.add toNimDecl(resdecl)

      of wekProc:
        var gproc = node.gproc
#         let text = split($gproc.cursor.getRawCommentText(), "\n").mapIt("    " & it).join("\n")
#         gproc.docs.add &"""
# .. code-block::
# {text}
# """

        var decl = gproc.toNNode(wrapConf).toNimDecl()

        updateComments(decl, node)
        result.decls.add decl

      of wekNimPass:
        result.decls.add node.wrapped

      of wekNimDecl:
        var decl = node.wrapped
        updateComments(decl, node)
        result.decls.add decl

proc wrapWithConfig*(
  infile, outfile: FsFile, wrapConf: WrapConfig, parseConf: ParseConfig) =

  writeWrapped(
    wrapSingleFile(
      infile,
      errorReparseVerbose = false,
      wrapConf = wrapConf,
      parseConf = parseConf
    ),
    outFile = outfile,
    codegens = none(FsDir),
    compile = @[],
    wrapConf = wrapConf
  )

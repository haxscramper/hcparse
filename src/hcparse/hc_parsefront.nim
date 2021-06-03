import std/[tables, options, strutils, strformat, hashes,
            sequtils, bitops, sugar, deques, sets]

import hnimast

import hmisc/helpers
import hmisc/algo/hstring_algo
import hmisc/other/[oswrap, colorlogger, hshell]
import hmisc/types/[colorstring, hgraph]
import gram

import
  ./cxtypes,
  ./cxcommon,
  ./hc_types,
  ./hc_visitors,
  ./hc_typeconv,
  ./hc_depresolve,
  ./hc_wrapgen,
  ./hc_impls,
  ./hc_docwrap


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


proc getFlags*(config: ParseConf, file: AbsFile): seq[string] =
  ## Get list of command-line flags for partigular `file`. This includes
  ## both global flags, and file-specific ones
  result.add config.includepaths.toIncludes()
  result.add config.globalFlags
  result.add config.fileFlags.getOrDefault(file)

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
    file: AbsFile,
    config: ParseConf,
    wrapConf: WrapConf,
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



proc parseAll*(
    files: seq[AbsFile], conf: ParseConf, wrapConf: WrapConf
  ): hc_types.FileIndex =
  for file in files:
    result.index[file] = parseFile(file, conf, wrapConf)

import hasts/graphviz_ast
export toPng, toXDot, AbsFile

#*************************************************************************#
#****************************  File wrapping  ****************************#
#*************************************************************************#
proc getExports*(
    parsed: ParsedFile, conf: WrapConf, index: hc_types.FileIndex
  ): seq[AbsFile] =
  ## Get list of absolute files that provide types, used in public API
  ## for `parsed` file *and* marked as internal (e.g. not supposed to
  ## be imported separately)
  for dep in parsed.explicitDeps:
    if conf.isInternal(dep, conf, index):
      result.add dep

proc toNNode*(genEntries: seq[GenEntry], conf: WrapConf):
  seq[WrappedEntry] =

  for node in genEntries:
    case node.kind:
      of gekProc:
        result.add newWrappedEntry(
          node.genProc.toNNode(conf).toNimDecl(),
          true,
          node.genProc.iinfo,
          node.genProc.cdecl.cursor
        )

      of gekImport:
        result.add node.genImport.toNNode(conf)

      of gekForward:
        raise newUnexpectedKindError(
          node,
          "forward declaration must be converted to pass/import",
          "by forward declaration patch state. This code should",
          "not be reached..\n",
          "- Entry created in", node.genForward.iinfo, "\n",
          "- CDecl is", node.genForward.cdecl.cursor
        )

      of gekAlias:
        result.add newWrappedEntry(
          node.genAlias.toNNode(conf).toNimDecl(),
          true,
          node.genAlias.iinfo,
          node.genAlias.cdecl.cursor
        )

      of gekObject:
        result.add node.genObject.toNNode(conf)

      of gekEnum:
        let (rawDecl, nimDecl) = node.genEnum.toNNode(conf)
        for decl in [rawDecl, nimDecl]:
          result.add newWrappedEntry(
            decl.toNimDecl(),
            true,
            node.genEnum.iinfo,
            node.genEnum.cdecl.cursor
          )

      of gekPass:
        result.add node.genPass.passEntries



type
  TypeNode = object
    name*: string
    declareFile*: Option[WrappedFile]

func hash*(typeNode: TypeNode): Hash =
  hash(typeNode.name)

func initTypeNode*(name: string): TypeNode =
  TypeNode(name: name)

proc allUsedTypes*(nimType: NimType): seq[NimType] =
  result.add nimType
  case nimType.kind:
    of ctkIdent:
      for param in nimType.genericParams:
        result.add allUsedTypes(param)

    of ctkProc:
      if notNil nimType.returnType:
        result.add nimType.returnType

      for argument in nimType.arguments:
        result.add allUsedTypes(argument.nimType)

proc isPrimitiveHead*(nimType: NimType): bool =
  nimType.kind in {ctkIdent} and nimType.nimName in [
    "ref", "var", "sink", "ptr"]

proc isPodHead*(nimType: NimType): bool =
  nimType.kind in {ctkIdent} and nimType.nimName.normalize() in [
    "int", "float", "string", "cint", "cstring" # ... TODO
  ]

proc patchForward*(
    wrapped: var seq[WrappedFile], conf: WrapConf, cache: var WrapCache):
  seq[WrappedFile] =
  ## Replace `GenForward` declarations with required import. Return list of
  ## additional generated files.

  info "Patching forward declarations"

  var typeGraph = newHGraph[TypeNode, NoProperty]()

  for file in wrapped:
    for entry in file.entries:
      if entry.kind in {gekObject}:
        # Get node for current type
        var objectNode = typeGraph.addOrGetNode(
          initTypeNode(entry.genObject.name.nimName))

        if typeGraph[objectNode].declareFile.isSome():
          # Declaration file is added each node. One node is created for
          # each unique `entry.genObject.name` ecountered. If node with
          # given name already exists it either means tha algorithm is bad,
          # or there is a two identically-named types somewhere. Which not
          # impossible - even more, it can happen quite easily. So I need
          # to IMPLEMENT some way for additional disambiguation of a type
          # based on where it is *declared*. But then I have to somehow
          # deal with type being referenced (and in that case I only know a
          # type name)
          raiseImplementError(
            "Multiple file declarations for type " & $entry.genObject.name)

        else:
          typeGraph[objectNode].declareFile = some(file)

        # Add outgoing edges for all types that were explicitly used.
        for field in entry.genObject.memberFields:
          for used in field.fieldType.allUsedTypes():
            if not (used.isPrimitiveHead() or used.isPodHead()):
              # QUESTION what about `int` fields, or other types that are
              # either not wrapped at all, or wrapped using some convoluted
              # multi-stage generics?
              #
              # QUESTION 2 now I'm not sure what previous question was
              # about, so I need to figure /that/ out too.
              discard typeGraph.addEdge(
                objectNode,
                typeGraph.addOrGetNode(
                  # IMPLEMENT use additional information in `CXType` field
                  # for `used`
                  initTypeNode(used.nimName))
              )


  for typeGroup in typeGraph.connectedComponents():
    info typeGroup.mapIt(typeGraph[it].name)






proc wrapFiles*(
    parsed: seq[ParsedFile], conf: WrapConf,
    cache: var WrapCache, index: hc_types.FileIndex): seq[WrappedFile] =

  var genFiles: seq[WrappedFile]
  for file in parsed:
    var resFile = WrappedFile()
    # Generate necessary default imports for all files
    resFile.entries.add initGenImport(@["bitops"], currIInfo())
    resFile.entries.add initGenImport(@["hcparse", "wraphelp"], currIInfo())

    when false: # QUESTION I don't even remember what this thing does
      if not isNil(conf.userCode):
        tmpRes.add newWrappedEntry(
          toNimDecl(conf.userCode(parsed.filename)),
          false, currIInfo(), CXCursor()
        )

    for node in file.explicitDeps.mapIt(
        conf.getImport(it, conf, false)).
        deduplicate():

      # Add imports for explicit dependencies
      resFile.entries.add initGenImport(node, currIInfo())

    # Add wrapper for main API unit
    resFile.entries.add file.api.wrapApiUnit(conf, cache, index)
    genFiles.add resFile

  # Patch *all* wrapped file entries at once, replacing `GenForward`
  # entries with imports and returning list of additional files (for
  # strongly connected type clusters that span multiple files)
  result.add patchForward(genFiles, conf, cache)

  # Add generated wrapper files themselves
  result.add genFiles


proc wrapFile*(
    wrapped: WrappedFile, conf: WrapConf,
    cache: var WrapCache, index: hc_types.FileIndex
  ): seq[WrappedEntry] =
  # Coollect all types into single wrapped entry type block. All duplicate
  # types (caused by forward declarations which is not really possible to
  # differentiated) will be overwritten. This should be fine I guess,
  # because you can't declare type again after defining it (I hope), so all
  # last type encounter will always be it's definition.
  var res: Table[string, WrappedEntry]
  var tmpRes: seq[WrappedEntry] = wrapped.entries.toNNode(conf)

  for elem in tmpRes:
    case elem.decl.kind:
      # Filter out all type declarations.
      of nekObjectDecl:
        debug elem.decl.objectDecl.name.kind
        debug elem.decl.objectDecl.name, elem.decl.objectDecl.iinfo

        let name = elem.decl.objectdecl.name.head
        res[name] = elem

      of nekEnumDecl:
        let name = elem.decl.enumdecl.name
        res[name] = elem

      of nekAliasDecl:
        let name = elem.decl.aliasdecl.newType.head
        if name in res:
          warn "Override type alias for ", name

        res[name] = elem

      of nekPassthroughCode:
        if not elem.postTypes:
          result.add elem

      of nekProcDecl, nekMultitype:
        discard

  block:
    let elems = collect(newSeq):
      for k, v in res:
        v.decl.toNimTypeDecl()

    result.add(newWrappedEntry(
      toNimDecl(elems), false, currIINfo(), CXCursor()))

  for elem in tmpRes:
    case elem.decl.kind:
      of nekProcDecl, nekMultitype:
        result.add elem

      of nekPassthroughCode:
        if elem.postTypes:
          result.add elem

      else:
        discard

func wrapName*(res: WrapResult): string =
  res.importName.importPath.join("/") & ".nim"

proc getExpanded*(file: AbsFile, parseConf: ParseConf): string =
  ## Return expanded content of the @arg{file} using @sh{clang}. Uses
  ## include paths and other flags from @arg{parseConf}. Expanded form does
  ## not contain `#line` directives, but preserves comments.
  let flags = getFlags(parseConf, file)
  var cmd = shellCmd(clang, -C, -E, -P)
  for flag in flags:
    cmd.raw flag

  cmd.arg file

  result = evalShellStdout(cmd)

proc updateComments(
    decl: var PNimDecl, node: WrappedEntry, wrapConf: WrapConf) =

  decl.addCodeComment("Wrapper for `" & toCppNamespace(
    node.ident, withNames = true) & "`\n")
  if node.cursor.getSpellingLocation().getSome(loc):
    let file = withoutPrefix(AbsFile(loc.file), wrapConf.baseDir)
    decl.addCodeComment(
      &"Declared in {file}:{loc.line}")


proc wrapSingleFile*(
    file: FsFile,
    errorReparseVerbose: bool = false,
    wrapConf: WrapConf = baseCppWrapConf,
    parseConf: ParseConf = baseCppParseConf,
  ): tuple[decls: seq[NimDecl[PNode]], codegen: seq[CxxCodegen]] =
  ## Generate wrapper for a single file.
  ##
  ## - @arg{postprocess} :: collection of postprocessing actions on
  ##  generated delarations, before they are converter to sequence of
  ##  `NimDecl`
  ##
  ## `wrapConf` provide user-defined implementation heuristics for
  ## necessary edge cases (see `WrapConf` type documentation).
  ## `postprocess` is a sequence of postprocessing actions that will be run
  ## on generated `WrappedEntry` structures and then added to final
  ## declaration. Default implementation of postprocessing includes
  ## automatic enum overload derivation, nim-like infix operators (`<<` and
  ## `>>` converter to `shl` and `shr` respectively) and final fixup for
  ## all identifiers. Order of postprocessint steps is important, as every
  ## original wrapped entry is passed to each postprocess in sequential
  ## order (if you have three steps, `class C{};` wrapper will be passed to
  ## each of them, before being added to final result).
  ##
  ## Returns list of nim declarations with associated metadata
  ## (instantiation info in codegen callback, comments etc.) and list of
  ## C++ codegen files.

  if wrapConf.baseDir.len == 0:
    raiseArgumentError(
      "Base director is not set for wrapper configuration. " &
        "Set configuration .baseDir to the root directory of C(++) " &
        "source files"
    )

  var
    cache: WrapCache
    index: hc_types.FileIndex

  let parsed = parseFile(
    file.toAbsFile(), parseConf, wrapConf,
    reparseOnNil = errorReparseVerbose
  )

  if wrapConf.showParsed:
    debug parsed.unit.getTranslationUnitCursor().treeRepr(parsed.unit)

  var wrapConf = wrapConf

  wrapConf.unit = parsed.unit

  let wrapped = wrapFiles(@[parsed], wrapConf, cache, index)[0].
    wrapFile(wrapConf, cache, index)

  for node in wrapped:
    var node = node
    updateComments(node.decl, node, wrapConf)
    result.decls.add node.decl

proc wrapAllFiles*(
    files: seq[AbsFile], wrapConf: WrapConf, parseConf: ParseConf) =
  ## Generate and write wrappers for all `files`

  var
    cache: WrapCache # Global cache for all parsed files
    index: hc_types.FileIndex
    parsed: seq[ParsedFile] # Full list of all parsed files
    wrapConf = wrapConf # Global wrapper configuration

  for file in files:
    fillDocComments(getExpanded(toAbsFile(file), parseConf), cache)

  for file in files:
    var parsedFile = parseFile(file, parseConf, wrapConf)
    wrapConf.unit = parsedFile.unit

    parsed.add parsedFile

  var wrapped: seq[seq[WrappedEntry]]
  for file in wrapFiles(parsed, wrapConf, cache, index):
    wrapped.add wrapFile(file, wrapConf, cache, index)

  for file in mitems(wrapped):
    for node in mitems(file):
      updateComments(node.decl, node, wrapConf)


proc wrapWithConf*(
  infile, outfile: FsFile, wrapConf: WrapConf, parseConf: ParseConf) =

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

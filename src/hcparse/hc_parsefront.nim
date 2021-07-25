import std/[
  tables, options, strutils, strformat,
  hashes, sequtils, bitops, sugar, deques, sets
]

import hnimast

import
  hmisc/[helpers, hexceptions],
  hmisc/algo/[hstring_algo, clformat],
  hmisc/other/[oswrap, hlogger, hshell],
  hmisc/types/[colorstring, hgraph]


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
    cache: var WrapCache,
    reparseOnNil: bool = true
  ): ParsedFile =


  file.assertExists()
  wrapConf.logger.thisScope("Parse file")
  # info "Parsing file", file

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
    wrapConf.err file.realpath
    wrapConf.debug config.getFlags(file).joinl()
    raise

  result.api = result.unit.splitDeclarations(wrapConf, cache)
  result.explicitDeps = result.api.publicApi.
    getDepFiles(wrapConf).filterIt(it != file)

  result.isExplicitlyAdded = true


proc parseAll*(
    files: seq[AbsFile], conf: ParseConf, wrapConf: WrapConf,
    cache: var WrapCache
  ): hc_types.FileIndex =
  for file in files:
    result.index[file] = parseFile(
      file, conf, wrapConf, cache)

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

proc toNNode*(genEntries: seq[GenEntry], conf: WrapConf, cache: var WrapCache):
  seq[WrappedEntry] =

  for node in genEntries:
    case node.kind:
      of gekProc:
        result.add newWrappedEntry(
          node.genProc.toNNode(conf).toNimDecl(),
          true,
          node.genProc.iinfo,
          node.genProc.cdecl
        )

      of gekImport:
        result.add node.genImport.toNNode(conf)

      of gekForward:
        raise newUnexpectedKindError(
          node,
          "forward declaration must be converted to pass/import",
          "by forward declaration patch stage. This code should",
          "not be reached..\n",
          "- Entry created in", node.genForward.iinfo, "\n",
          "- CDecl is", node.genForward.cdecl.cursor
        )

      of gekAlias:
        result.add newWrappedEntry(
          node.genAlias.toNNode(conf).toNimDecl(),
          true,
          node.genAlias.iinfo,
          node.genAlias.cdecl
        )

      of gekObject:
        result.add node.genObject.toNNode(conf, cache)

      of gekEnum:
        result.add node.genEnum.toNNode(conf, cache)

      of gekPass:
        result.add node.genPass.passEntries



type
  TypeNode = object
    name*: string
    path*: seq[string]
    cursor*: CXCursor
    isDef*: bool
    declareFile*: Option[WrappedFile]

func hash*(typeNode: TypeNode): Hash =
  !$(hash(typeNode.name) !& hash(typeNode.isDef) !& hash(typeNode.path))

func `==`*(t1, t2: TypeNode): bool =
  t1.name == t2.name and t1.isDef == t2.isDef and
  t1.path == t2.path

func initTypeNode*(
  name: string, path: seq[string], cursor: CXCursor, isDef: bool): TypeNode =
  TypeNode(name: name, path: path, cursor: cursor, isDef: isDef)


proc isPrimitiveHead*(nimType: NimType): bool =
  nimType.kind in {ctkIdent} and nimType.nimName in [
    "ref", "var", "sink", "ptr"]

proc isPodHead*(nimType: NimType): bool =
  let norm = nimType.nimName.normalize()
  nimType.kind in {ctkIdent} and norm in [
    "int", "float", "string", "cint", "cstring", # ... TODO
    "cuint", "sizet", "cstringarray", "cchar", "ptr",
    "void", "pointer"
  ]

type
  Link = enum direct, forward, forwardReuse
  TypeGraph = HGraph[TypeNode, Link]

proc getTypeGraph(
    wrapped: var seq[WrappedFile], conf: WrapConf,
    cache: var WrapCache
  ): TypeGraph =

  result = newHGraph[TypeNode, Link]()

  for file in mitems(wrapped):
    var extraEntries: seq[GenEntry]
    for entry in file.entries:
      case entry.kind:
        of gekProc:
          # It is possible to use `enum` type without declaring it in the
          # same translation unit (let alone defining). So wonders like this
          # are entirely possible.
          #
          # ```c
          # struct	mparse;
          # struct mparse	 *mparse_alloc(int, enum mandoc_os);
          # ```
          #
          # I suppose (TODO REVIEW) current algorithm
          # will be able to handle use of enum *and* forward declaration as
          # well (e.g. `enum mandoc_os` was used in procedure but
          # forward-declared) in the file as well.
          var procTypes: seq[NimType]
          for arg in entry.genProc.arguments:
            if not arg.isRaw:
              procTypes.add arg.nimType

          procTypes.add entry.genProc.returnType

          var hasEnumArg = false
          var cdecl = CDecl(kind: cdkEnum, ident: @[])
          for it in procTypes:
            for used in it.allUsedTypes():
              let decl = used.cxType.getTypeDeclaration()
              if decl.cxKind() in {ckEnumDecl} and decl.isForward():
                hasEnumArg = true
                cdecl[] = entry.cdecl()[]
                cdecl.cursor = decl
                let
                  loc = decl.getSpellingLocation().get()
                  path = conf.getImport(loc.file, conf.getBaseFile(file), false).importPath
                  node = initTypeNode(
                    used.nimName, path,
                    cdecl.cursor, isDef = false)

                let enumNode = result.addOrGetNode(node)

          if hasEnumArg:
            # Adding nonexistent forward declaration for it to be removed
            # (and replaced) with corresponding import/export pair during
            # drop/move phase.
            extraEntries.add GenForward(
              iinfo: currLInfo(), cdecl: cdecl)


        of gekObject:
          # Get node for current type
          var objectNode = result.addOrGetNode(
            initTypeNode(
              entry.genObject.name.nimName,
              conf.getImport(
                entry.getSpellingLocation(), conf.getBaseFile(file), false).importPath,
              entry.cdecl.cursor,
              true
          ))

          if result[objectNode].declareFile.isSome():
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
            result[objectNode].declareFile = some(file)

          # Add outgoing edges for all types that were explicitly used.
          # info entry.genObject.name.nimName
          for field in entry.genObject.memberFields:
            for used in field.fieldType.allUsedTypes():
              if not used.isPodHead():
                if used.fromCXtype:
                  # QUESTION what about `int` fields, or other types that are
                  # either not wrapped at all, or wrapped using some convoluted
                  # multi-stage generics?
                  #
                  # QUESTION 2 now I'm not sure what previous question was
                  # about, so I need to figure /that/ out too.
                  let
                    decl = used.cxType.getTypeDeclaration()
                    loc = decl.getSpellingLocation.get()
                    path = conf.getImport(
                      loc.file, conf.getBaseFile(file), false).importPath

                  let node = initTypeNode(
                    used.nimName, path, decl, not isForward(decl))

                  discard result.addOrGetEdge(
                    objectNode,
                    result.addOrGetNode(node),
                    if isForward(decl): forward else: direct
                  )

        of gekForward:
          # HACK. Edge case first encountered in
          # `stl_iterator_base_funcs.h:73` as `template <typename> struct
          # _List_iterator;`
          if entry.cdecl().cursor.kind notin {ckClassTemplate}:
            var objectNode = result.addOrGetNode(
              initTypeNode(
                conf.typeNameForScoped(entry.cdecl().ident, conf).nimName,
                conf.getImport(
                  entry.getSpellingLocation(),
                  conf.getBaseFile(file), false).importPath,
                entry.cdecl().cursor,
                false))

        else:
          discard



    file.entries.add extraEntries

  for t1 in nodes(result):
    for t2 in nodes(result):
      if t1 != t2 and result[t1].name == result[t2].name:
        discard result.addOrGetEdge(t1, t2, forwardReuse)

type
  TypeGroup = object
    nodes: HNodeSet
    imports: HashSet[AbsFile]
    files: HashSet[AbsFile]
    external: HashSet[AbsFile]

proc nodeFile(graph: TypeGraph, node: HNode): AbsFile =
  graph[node].cursor.getSpellingLocation().get().file

proc groupFile(group: TypeGroup, graph: TypeGraph): string =
    var files: HashSet[seq[string]]
    for node in group.nodes:
      files.incl graph[node].path

    mapIt(files, it.sorted().join("_")).join("_")

proc dependentComponents(graph: TypeGraph): seq[TypeGroup] =
  var groups: seq[TypeGroup]
  var groupedNodes: HNodeSet
  for path in graph.findCycles(ignoreSelf = true).mergeCycleSets():
    var group = TypeGroup(nodes: path)
    for node in group.nodes:
      group.files.incl graph.nodeFile(node)

    groupedNodes.incl group.nodes
    groups.add group


  var groupDepends: HashSet[(int, int)]
  # Extend each type group with dependencies located in the same file(s).
  # Take note of the all external dependendencies.
  for idx, group in pairs(groups):
    # Each participates in group cluster of size 1 (at least) - this is
    # necessary for it to not be ignored when merging group clusters
    groupDepends.incl((idx, idx))

    # Collect external files but only add them later to avoid repeated
    # extension of the outgoing node set.
    var externalFiles: HashSet[AbsFile]
    let extendedGroup = extendOutgoing(
      graph, group.nodes) do(node: HNode) -> bool:

      let file = graph.nodeFile(node)
      if file in group.files:
        # Take note of each intergroup dependency
        if node in groupednodes:
          for targetIdx, targetGroup in pairs(groups):
            if node in targetGroup.nodes and targetGroup != group:
              groupDepends.incl((idx, targetIdx))

        # If node describes type located in file that is already in the
        # group extend it unless file is located in other group
        #
        # ```C
        # // this structure must be moved to connected
        # // cluster file (if any) and subsequently re-exprted
        # struct Other {};
        # struct Forward;
        # struct User { Forward* forward; Other other; };
        # struct Forward { User* user; }
        # ```
        result = node notin groupedNodes

      else:
        # If requires external file do not extend group, but store file
        # path
        externalFiles.incl file
        result = false


    groups[idx].nodes.incl extendedGroup
    groups[idx].imports.incl externalFiles

  var groupGraph = newHGraph[int, NoProperty]()
  for (g1, g2) in groupDepends:
    discard groupGraph.addOrGetEdge(g1, g2)

  let clusters = groupGraph.
    # Gather connected components in cluster graph, treating it as
    # undirected. I think it is the same as minimal spanning *forest*
    # (there might be multiple disjoing group clusters), but I haven't
    # implemented this algorithm yet.
    connectedComponents(overrideDirected = true)

  let dot = groupGraph.dotRepr(
    proc(node: int, _: HNode): DotNode =
      makeDotNode(0, groups[node].groupFile(graph)),
    clusters = clusters.mapIt((it, ""))
  )

  dot.toPng(getAppTempFile("intergroup.png"))

  # return groups

  for cluster in clusters:
    var mergedGroup: TypeGroup

    for node in cluster:
      let group = groups[groupGraph[node]]
      mergedGroup.files.incl group.files
      mergedGroup.imports.incl group.imports
      mergedGroup.external.incl group.external
      mergedGroup.nodes.incl group.nodes

    result.add mergedGroup




proc dotRepr(typeGraph: TypeGraph, groups: seq[TypeGroup] = @[]): DotGraph =
  var fileMarks: MarkTable[string, TermColor8Bit]
  result = typeGraph.dotRepr(
    proc(node: TypeNode, hnode: HNode): DotNode =
      var importLoc: string
      let file = node.path.joinq()
      result = makeDotNode(
        0, node.name & "\n" & file,
        fillcolor = dotColor(
          if node.isDef: fgYellow else: fgCyan,
          lightDotForegroundMap),
        color = fileMarks.getRandMark(file).toColor())

      result.penWidth = some(2.5)

    ,

    proc(link: Link, hedge: HEdge): DotEdge =
      case link:
        of direct: makeDotEdge(edsBold)
        of forward: makeDotEdge(edsDashed)
        of forwardReuse: makeDotEdge(edsDotted)
    ,

    clusters = groups.mapIt((it.nodes, it.groupFile(typeGraph)))
  )

  result.bgColor = some colLightSlateGray
  result.ranksep = some 1.2


proc patchForward*(
    wrapped: var seq[WrappedFile], conf: WrapConf, cache: var WrapCache):
  seq[WrappedFile] =
  ## Replace `GenForward` declarations with required import. Return list of
  ## additional generated files.

  var typeGraph = getTypeGraph(wrapped, conf, cache)
  let components = typeGraph.dependentComponents()
  typeGraph.dotRepr(components).toPng(getAppTempFile("forwardComponents.png"))


  var
    movedForward: Table[string, tuple[cursors: Hashset[CXCursor], file: WrappedFile]]
    droppedForward: HashSet[CXCursor]

  for group in components:
    if group.files.len == 1:
      # All parts of the forward-declare graph are located in the same file.
      for node in group.nodes:
        if typeGraph[node].cursor.isForward():
          droppedForward.incl typeGraph[node].cursor

    else:
      # Forward-declared nodes form strongly connected graph that would
      # lead to mutually recursive imports. By grouping all declarations in
      # a single file it is now possible to put them in single `type`
      # section as well.
      var cursors: HashSet[CxCursor]
      for node in group.nodes:
        cursors.incl typeGraph[node].cursor

      # Identify list of external files that need to be additionally
      # imported to generated files.

      # TEMP HACK
      var file: AbsFile
      for f in group.files:
        file = f
        break

      let newFile = group.groupFile(typeGraph)
      if newFile notin movedForward:
        var genFile = WrappedFile(
          isGenerated: true,
          # HACK FIXME assuming all files are located in a single directory,
          # which is most likely not the case.
          original: @[ file ],
          newFile: RelFile(newFile & ".nim")
        )

        for extern in group.imports:
          genFile.imports.incl conf.getImport(
            extern, conf.getBaseFile(genFile), false)

        movedForward[newFile] = (cursors, genFile)

      else:
        movedForward[newFile].cursors.incl cursors

  for wrappedFile in mitems(wrapped):
    for entry in mitems(wrappedFile.entries):
      if entry.hasCDecl():
        for _, (cursors, file) in mpairs(movedForward):
          file.imports.incl wrappedFile.imports
          if entry.cdecl().cursor in cursors:
            if entry.kind notin {gekForward}:
              file.entries.add entry

  # drop/move phrase of the patching - all cursors that participate in type
  # groups are either droppped (forward declaration is simply replaced with
  # empty entry in case of group consisting of a single file) *or*
  # import-export pair (via modification of the `.imports` and `.exports`
  # sets for wrapped file)
  var addedRel: HashSet[RelFile]
  for wrappedFile in mitems(wrapped):
    for entry in mitems(wrappedFile.entries):
      if entry.hasCDecl():
        # info "processing", entry.cdecl().ident
        if entry.cdecl().cursor in droppedForward:
          conf.notice "Dropped forward declaration", entry.cdecl().ident
          entry[] = newGenEntry(GenPass(iinfo: currLInfo()))[]

        else:
          var moved: bool = false
          for filename, _ in movedForward:
            # HACK I can't use `filename, (cursors, file)` here because of
            # C codegen bug when compiling.
            let file = movedForward[filename].file
            if entry.cdecl().cursor in movedForward[filename].cursors:
              entry = newGenEntry(GenPass(iinfo: currLInfo()))
              wrappedFile.imports.incl initNimImportSpec(false, @[filename])
              wrappedFile.exports.incl filename

              if file.newFile notin addedRel:
                addedRel.incl file.newFile
                result.add file

              moved = true

              break

          if not moved and entry.kind in {gekForward}:
            # Forward declarations that were never defined in any of the
            # header files (pointer to implementation, opaque handlers
            # etc.)
            if entry.cdecl().cursor.cxKind() in {
              ckStructDecl, ckClassDecl, ckUnionDecl}:
              # Creating new wrapped entry for forward declaration.
              # `wrapObject` won't create any constructor procedures for
              # this type of object, so it can only be created using
              # pointer (i.e. used as in opaque handler)
              let cd = entry.cdecl()
              # warn "Dropping not moved entry", cd.ident
              var cdecl =
                case cd.cursor.cxKind():
                  of ckStructDecl: CDecl(kind: cdkStruct, ident: cd.ident)
                  of ckClassDecl: CDecl(kind: cdkClass, ident: cd.ident)
                  of ckUnionDecl: CDecl(kind: cdkUnion, ident: cd.ident)
                  else:
                    raise newUnexpectedKindError(cd.cursor)

              # debug cd.ident
              cdecl.cursor = cd.cursor
              cdecl.icpp = cd.ident.toCppNamespace()
              if not conf.isImportCpp:
                case cdecl.kind:
                  of cdkUnion: cdecl.icpp = "union " & cdecl.icpp
                  of cdkStruct: cdecl.icpp = "struct " & cdecl.icpp
                  else:
                    raise newUnexpectedKindError(cdecl)

              entry = wrapObject(cdecl, conf, cache).newGenEntry()

            else:
              entry[] = newGenEntry(GenPass(iinfo: currLInfo()))[]










proc wrapFiles*(
    parsed: seq[ParsedFile], conf: WrapConf,
    cache: var WrapCache, index: hc_types.FileIndex): seq[WrappedFile] =

  var genFiles: seq[WrappedFile]
  for file in parsed:
    var resFile = WrappedFile(
      isGenerated: false, baseFile: file.filename)

    # Generate necessary default imports for all files
    # resFile.imports.incl initNimImportSpec(true, @["bitops"])
    # resFile.imports.incl initNimImportSpec(true, @["hcparse", "wraphelp"])

    for node in file.explicitDeps.mapIt(
        conf.getImport(it, conf.getBaseFile(resFile), false)).
        deduplicate():

      # Add imports for explicit dependencies
      resFile.imports.incl node


    conf.unit = file.unit
    # Add wrapper for main API unit
    resFile.entries.add file.api.wrapApiUnit(conf, cache, index)
    genFiles.add resFile

  # Patch *all* wrapped file entries at once, replacing `GenForward`
  # entries with imports and returning list of additional files (for
  # strongly connected type clusters that span multiple files)
  result.add patchForward(genFiles, conf, cache)

  # Add generated wrapper files themselves
  result.add genFiles



proc registerUse*(
    nimType: NimType,
    used: var HashSet[CxCursor],
    conf: WrapConf
  ) =

  if isNil(nimType):
    return

  if nimType.fromCxType:
    let cxDecl = nimType.cxType.getTypeDeclaration()
    if cxDecl.kind notin {ckNoDeclFound}:
      used.incl cxDecl

  if nimType.fullIdent.isSome():
    let last = nimType.fullIdent.get()[^1]
    if not last.isGenerated:
      # conf.dump last.cursor.cxKind(), last.cursor.getSpellingLocation()
      used.incl last.cursor

    for param in last.genParams:
      if param.len > 0 and param[^1].isGenerated.not():
        let lastParam = param[^1]
        if lastParam.cursor.kind notin { ckTemplateTypeParameter }:
          conf.dump param, lastParam.cursor.cxKind()

  case nimType.kind:
    of ctkIdent:
      if nimType.defaultType.isSome():
        registerUse(nimType.defaultType.get(), used, conf)

      for param in nimType.genericParams:
        registerUse(param, used, conf)

    of ctkProc:
      for arg in nimType.arguments:
        if not arg.isRaw:
          registerUse(arg.nimType, used, conf)

      registerUse(nimType.returnType, used, conf)


proc registerUsedTypes*(genProc: GenProc, used: var HashSet[CxCursor], conf: WrapConf) =
  for arg in genProc.arguments:
    if not arg.isRaw:
      registerUse(arg.nimType, used, conf)

  registerUse(genProc.returnType, used, conf)

proc registerUsedTypes*(
    entry: GenEntry,
    used: var HashSet[CxCursor],
    conf: WrapConf) =
  case entry.kind:
    of gekEnum:
      discard

    of gekProc:
      registerUsedTypes(entry.genProc, used, conf)

    of gekObject:
      for field in entry.genObject.memberFields:
        registerUse(field.fieldType, used, conf)

      for meth in entry.genObject.memberMethods:
        registerUsedTypes(meth, used, conf)

      for nested in entry.genObject.nestedEntries:
        registerUsedTypes(nested, used, conf)

    of gekAlias:
      registerUse(entry.genAlias.baseType, used, conf)

    of gekPass, gekImport, gekForward:
      discard


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

  var usedApis: HashSet[CxCursor] # Set of cursors pointing to declarations
                                  # for different types used in the file
                                  # entries (procedure argument and return,
                                  # field, global variable types).

  for entry in wrapped.entries:
    registerUsedTypes(entry, usedApis, conf)

  for usedType in usedApis:
    let loc = usedType.getSpellingLocation()
    if loc.isSome():
      let base = conf.getBaseFile(wrapped)
      let imp = conf.getImport(loc.get().file, base, false)

      if not (imp.isRelative and
              imp.relativeDepth == 0 and
              imp.importPath[0] == conf.getSavePath(base, conf).name()):

        wrapped.imports.incl imp

  var tmpRes: seq[WrappedEntry] = wrapped.entries.toNNode(conf, cache)

  for elem in tmpRes:
    case elem.decl.kind:
      # Filter out all type declarations.
      of nekObjectDecl:
        let name = elem.decl.objectdecl.name.head
        res[name] = elem

      of nekEnumDecl:
        let name = elem.decl.enumdecl.name
        res[name] = elem

      of nekAliasDecl:
        let name = elem.decl.aliasdecl.newType.head
        if name in res:
          conf.warn "Override type alias for ", name

        res[name] = elem

      of nekFieldDecl:
        raise newUnexpectedKindError(
          elem.decl,
          "Field declarations cannot be encountered at toplevel.",
          "This code cannot be reached."
        )

      of nekPassthroughCode:
        if not elem.postTypes:
          result.add elem

      of nekProcDecl, nekMultitype:
        discard

  wrapped.imports.incl initNimImportSpec(true, @["std", "bitops"])
  wrapped.imports.incl initNimImportSpec(true, @[
    "hmisc", "wrappers", "wraphelp"])

  wrapped.exports.incl "wraphelp"

  let push = pquote do:
    {.push warning[UnusedImport]: off.}

  result.add push.toNimDecl().newWrappedEntry(false, currLInfo())


  result.add wrapped.imports.toNNode().
    toNimDecl().newWrappedEntry(false, currLInfo())

  if wrapped.exports.len > 0:
    result.add nnkExportStmt.newTree(
      wrapped.exports.mapIt(newPIdent(it))).
      toNimDecl().newWrappedEntry(false, currLInfo())

  # Insert user-supplied code elements
  var addPost: seq[WrappedEntry]
  if not isNil(conf.userCode):
    let (node, post) = conf.userCode(wrapped)
    let entry = newWrappedEntry(toNimDecl(node), post, currLInfo())
    if not isNil(node):
      if post:
        addPost.add entry

      else:
        result.add entry

  block:
    let elems = collect(newSeq):
      for _, wrapEntry in mpairs(res):
        updateComments(wrapEntry.decl, wrapEntry, conf, cache)
        wrapEntry.decl.toNimTypeDecl()

    result.add(newWrappedEntry(
      toNimDecl(elems), false, currLInfo()))

  result.add addPost

  for elem in mitems(tmpRes):
    case elem.decl.kind:
      of nekProcDecl:
        updateComments(elem.decl, elem, conf, cache)
        result.add elem

      of nekMultitype:
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


proc wrapSingleFile*(
    file: FsFile,
    errorReparseVerbose: bool = false,
    wrapConf: WrapConf = baseCppWrapConf,
    parseConf: ParseConf = baseCppParseConf,
  ): CodegenResult =
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
    cache,
    reparseOnNil = errorReparseVerbose
  )

  if wrapConf.showParsed:
    wrapConf.debug parsed.unit.getTranslationUnitCursor().treeRepr(parsed.unit)

  var wrapConf = wrapConf

  wrapConf.unit = parsed.unit

  let wrapped = wrapFiles(@[parsed], wrapConf, cache, index)[0].
    wrapFile(wrapConf, cache, index)

  for node in wrapped:
    result.decls.add node.decl

  result.cache = cache


proc wrapAllFiles*(
    files: seq[AbsFile], wrapConf: WrapConf, parseConf: ParseConf) =
  ## Generate and write wrappers for all `files`

  var
    cache: WrapCache # Global cache for all parsed files
    index: hc_types.FileIndex
    parsed: seq[ParsedFile] # Full list of all parsed files
    wrapConf = wrapConf # Global wrapper configuration

  assertValid(
    wrapConf.nimOutDir,
    ". Output directory for generated nim files.")

  for file in files:
    var parsedFile = parseFile(file, parseConf, wrapConf, cache)
    wrapConf.unit = parsedFile.unit

    parsed.add parsedFile

  var wrapped: seq[seq[WrappedEntry]]
  for file in wrapFiles(parsed, wrapConf, cache, index):
    let outPath = wrapConf.nimOutDir / getSavePath(wrapConf, file)

    var codegen: CodegenResult
    for node in wrapFile(file, wrapConf, cache, index):
      codegen.decls.add node.decl

    wrapConf.debug outPath
    codegen.writeWrapped(outPath, @[], wrapConf)

  CodegenResult(cache: cache).writeWrapped(
    AbsFile(""), @[], wrapConf)


proc wrapWithConf*(
    files: seq[AbsFile], wrapConf: WrapConf,
    parseConf: ParseConf
  ) = wrapAllFiles(files, wrapConf, parseConf)

proc wrapWithConf*(
    infile, outfile: FsFile, wrapConf: WrapConf,
    parseConf: ParseConf
  ) =

  writeWrapped(
    wrapSingleFile(
      infile,
      errorReparseVerbose = false,
      wrapConf = wrapConf,
      parseConf = parseConf
    ),
    outFile = outfile,
    compile = @[],
    wrapConf = wrapConf
  )

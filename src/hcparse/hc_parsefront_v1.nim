import hmisc/core/all

importx:
  std/[
    tables,
    options,
    strutils,
    strformat,
    hashes,
    sequtils,
    bitops,
    sugar,
    deques,
    sets,
    algorithm
  ]

  hnimast
  hnimast/interop/wrap_store

  hmisc/[
    algo/[hstring_algo, clformat, halgorithm],
    other/[oswrap, hlogger, hshell],
    types/[colorstring, hgraph],
    macros/argpass
  ]

  jsony

  ./[
    cxtypes,
    cxcommon,
    hc_types,
    hc_visitors,
    hc_typeconv,
    hc_depresolve,
    hc_wrapgen,
    hc_impls,
    hc_save
  ]



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

proc toNNode*(
    genEntries: seq[GenEntry], conf: WrapConf, cache: var WrapCache):
  seq[WrappedEntry] =

  for node in genEntries:
    case node.kind:
      of gekProc:
        result.addProcDecl(node.genProc, conf, cache)

      of gekImport:
        result.add node.genImport.toNNode(conf)

      of gekForward:
        raise newUnexpectedKindError(
          node,
          "forward declaration must be converted to pass/import",
          "by forward declaration patch stage. This code should",
          "not be reached..\n",
          "- Entry created in", node.genForward.iinfo, "\n",
          "- CDecl is", node.genForward.cdecl.cursor)

      of gekAlias:
        result.add newWrappedEntry(
          node.genAlias.toNNode(conf, cache).toNimDecl(),
          wepInTypes,
          node.genAlias.iinfo,
          node.genAlias.cdecl)

      of gekObject:
        result.add node.genObject.toNNode(conf, cache)

      of gekEnum:
        result.add node.genEnum.toNNode(conf, cache)

      of gekPass:
        result.add node.genPass.passEntries

      of gekMacro, gekComment:
        raise newImplementKindError(node)

      of gekEmpty:
        discard



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
                  path = conf.getImport(
                    loc.file, conf.getBaseFile(file), false).importPath
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
                conf.typeNameForScoped(entry.cdecl().ident, cache).nimName,
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
  typeGraph.
    dotRepr(components).
    toPng(getAppTempFile("forwardComponents.png"))


  var
    # Mapping between new file name and pair of (cursors that were moved to
    # the file)+(wrapped file object)
    movedForward: Table[string, tuple[cursors: Hashset[CXCursor], file: WrappedFile]]
    # Set of cursors that were forward-declared and defined in the same
    # file. These ones are simply dropped
    droppedForward: HashSet[CXCursor]

  # `find/store` phase of the patching - determine mapping between elements
  # that need to be moved forward, and ones that should be dropped, or kept
  # in place.
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

  # Move entries to generated files and replace them with corresponding
  # imports. `copy/import` phase of the patching.
  for wrappedFile in mitems(wrapped):
    for entry in mitems(wrappedFile.entries):
      if entry.hasCDecl():
        for _, (cursors, file) in mpairs(movedForward):
          file.imports.incl wrappedFile.imports
          if entry.cdecl().cursor in cursors:
            if entry.kind notin {gekForward}:
              file.entries.add entry

  # `drop/move` phrase of the patching - all cursors that participate in
  # type groups are either droppped (forward declaration is simply replaced
  # with empty entry in case of group consisting of a single file) *or*
  # import-export pair (via modification of the `.imports` and `.exports`
  # sets for wrapped file)
  var addedRel: HashSet[RelFile]
  for wrappedFile in mitems(wrapped):
    for entry in mitems(wrappedFile.entries):
      if entry.hasCDecl():
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
            if entry.cdecl().cursor.cxKind() in ckTypeDeclKinds:
              # Creating new wrapped entry for forward declaration.
              # `wrapObject` won't create any constructor procedures for
              # this type of object, so it can only be created using
              # pointer (i.e. used as in opaque handler)
              let cd = entry.cdecl()
              var cdecl =
                case cd.cursor.cxKind():
                  of ckStructDecl: CDecl(kind: cdkStruct, ident: cd.ident)
                  of ckUnionDecl: CDecl(kind: cdkUnion, ident: cd.ident)
                  of ckClassDecl, ckClassTemplate:
                    CDecl(kind: cdkClass, ident: cd.ident)
                  else:
                    raise newUnexpectedKindError(cd.cursor)

              cdecl.cursor = cd.cursor
              cdecl.icpp = cd.ident.toCppNamespace()
              cache.setParamsForType(
                conf, cd.ident, conf.genParamsForIdent(cd.ident, cache))
              # cdecl.genParams = conf.genParamsForIdent(cd.ident, cache)
              if not conf.isImportCpp:
                case cdecl.kind:
                  of cdkUnion: cdecl.icpp = "union " & cdecl.icpp
                  of cdkStruct: cdecl.icpp = "struct " & cdecl.icpp
                  else:
                    raise newUnexpectedKindError(cdecl)

              entry = wrapObject(cdecl, conf, cache).newGenEntry()

            else:
              entry[] = newGenEntry(GenPass(iinfo: currLInfo()))[]


iterator mitems*[T](s1, s2: var seq[T]): var T =
  for it in mitems(s1): yield it
  for it in mitems(s2): yield it

iterator items*[T](s1, s2: seq[T]): T =
  for it in items(s1): yield it
  for it in items(s2): yield it

iterator pairs*[K, V](s1, s2: Table[K, V]): (K, V) =
  for it in pairs(s1): yield it
  for it in pairs(s2): yield it

type
  UsedSet = object
    cursors: Table[CxCursor, HashSet[NimType]]
    libs: Table[CxxLibImport, HashSet[NimType]]

  UsedGroups = object
    inProcs: UsedSet
    inTypes: UsedSet

func mgetOrDefault*[K, V](table: var Table[K, V], key: K): var V =
  if key notin table:
    table[key] = default(V)

  return table[key]



proc registerUse*(nimType: NimType, used: var UsedSet, conf: WrapConf) =

  if isNil(nimType):
    return

  if nimType.fromCxType:
    let cxDecl = nimType.cxType.getTypeDeclaration()
    if cxDecl.kind notin {ckNoDeclFound}:
      used.cursors.mgetOrDefault(cxDecl).incl nimType

  else:
    used.libs.mgetOrDefault(nimType.typeImport).incl nimType

  if nimType.fullIdent.isSome():
    let last = nimType.fullIdent.get()[^1]
    if not last.isGenerated:
      used.cursors.mgetOrDefault(last.cursor).incl nimType

    for param in last.genParams:
      if param.len > 0 and param[^1].isGenerated.not():
        let lastParam = param[^1]
        if lastParam.cursor.kind notin { ckTemplateTypeParameter }:
          conf.dump param, lastParam.cursor.cxKind()

  case nimType.kind:
    of ctkPtr:
      registerUse(nimType.wrapped, used, conf)

    of ctkIdent:
      if nimType.defaultType.isSome():
        registerUse(nimType.defaultType.get(), used, conf)

      for param in nimType.genParams:
        registerUse(param, used, conf)

    of ctkProc:
      for arg in nimType.arguments:
        if not arg.isRaw:
          registerUse(arg.nimType, used, conf)

      registerUse(nimType.returnType, used, conf)


proc registerUsedTypes*(
    genProc: GenProc, used: var UsedSet, conf: WrapConf) =

  for arg in genProc.arguments:
    if not arg.isRaw:
      registerUse(arg.nimType, used, conf)

  registerUse(genProc.returnType, used, conf)

proc registerUsedTypes*(
    entry: GenEntry, used: var UsedGroups, conf: WrapConf) =

  case entry.kind:
    of gekEnum, gekMacro, gekComment:
      discard

    of gekProc:
      registerUsedTypes(entry.genProc, used.inProcs, conf)

    of gekObject:
      for field in entry.genObject.memberFields:
        registerUse(field.fieldType, used.inTypes, conf)

      for meth in entry.genObject.memberMethods:
        registerUsedTypes(meth, used.inProcs, conf)

      for nested in entry.genObject.nestedEntries:
        registerUsedTypes(nested, used, conf)

    of gekAlias:
      registerUse(entry.genAlias.baseType, used.inTypes, conf)

    of gekPass, gekImport, gekForward, gekEmpty:
      discard

proc getUsedTypes*(file: WrappedFile, conf: WrapConf): UsedGroups =
  for e in file.entries:
    e.registerUsedTypes(result, conf)

proc updateImports*(
    wrapped: seq[WrappedFile],
    conf: WrapConf,
    cache: var WrapCache
  ): seq[WrappedFile] =

  # Collect all types into single wrapped entry type block. All duplicate
  # types (caused by forward declarations which is not really possible to
  # differentiated) will be overwritten. This should be fine I guess,
  # because you can't declare type again after defining it (I hope), so all
  # last type encounter will always be it's definition.

  var usedApis: UsedGroups # Set of cursors pointing to declarations for
  # different types used in the file entries (procedure argument and
  # return, field, global variable types).

  for file in wrapped:
    for entry in file.entries:
      registerUsedTypes(entry, usedApis, conf)

  var
    importGraph = newHGraph[CxxLibImport, NimType]()
    fileMap: Table[CxxLibImport, WrappedFile]

  for file in wrapped:
    let lib = conf.getSavePath(conf.getBaseFile(file))
    fileMap[lib] = file

  proc isSelfImport(save: CxxLibImport, imp: NimImportSpec): bool =
    imp.isRelative and
    imp.relativeDepth == 0 and
    imp.importPath.len == 1 and
    imp.importPath[^1] == save.importPath[^1]

  proc addImports(graph: var HGraph[CxxLibImport, NimType], file: WrappedFile, used: UsedSet) =
    block cursorBasedImportrs:
      let
        base = conf.getBaseFile(file)
        user = conf.getSavePath(base)

      for typeCursor, typeSet in used.cursors:
        let loc = typeCursor.getSpellingLocation()
        if loc.isSome():
          let
            dep = conf.getSavePath(loc.get().file)
            imp = conf.getImport(loc.get().file, base, false)

          if not isSelfImport(user, imp):
            for item in typeSet:
              graph.addEdge(
                graph.addOrGetNode(user),
                graph.addOrGetNode(dep),
                item)

    block libraryOverrideImports:
      let base = conf.getSavePath(conf.getBaseFile(file))
      for usedLib, typeSet in used.libs:
        if usedLib.isValid():
          let imp = conf.getImport(usedLib, base, false)
          for usedType in typeSet:
            graph.addEdge(
              graph.addOrGetNode(base),
              graph.addOrGetNode(usedLib),
              usedType)

  block registerImports:
    var
      onlyTypes = newHGraph[CxxLibImport, NimType]()
      onlyProcs = newHGraph[CxxLibImport, NimType]()

    for file in wrapped:
      onlyTypes.addImports file, usedApis.inTypes
      onlyProcs.addImports file, usedApis.inProcs

      importGraph.addImports file, usedApis.inTypes
      importGraph.addImports file, usedApis.inProcs

    onlyTypes.
      dotRepr(
        dotReprDollarNode[CxxLibImport],
        dotReprCollapseEdgesJoin[NimType],
        clusters = onlyTypes.findCycles(ignoreSelf = true).
      mergeCycleSets().mapIt((it, ""))).withResIt do:
        it.rankdir = grdLeftRight
        it.toPng(getAppTempFile("onlyTypes.png"))

    onlyProcs.
      dotRepr(
        dotReprDollarNode[CxxLibImport],
        dotReprCollapseEdgesJoin[NimType],
        clusters = onlyTypes.findCycles(ignoreSelf = true).
      mergeCycleSets().mapIt((it, ""))).withResIt do:
        it.rankdir = grdLeftRight
        it.toPng(getAppTempFile("onlyProcs.png"))



  let groups = importGraph.findCycles().mergeCycleSets()
  let clusteredNodes = groups.mapIt(toHashSet(importGraph[it])).foldl(union(a, b))


  proc addImports(
      file: var WrappedFile,
      usedGroup: UsedSet,
      ignoreImport: HashSet[CxxLibImport] = default(HashSet[CxxLibImport])
    ) =
    let
      base = conf.getBaseFile(file)
      save = conf.getSavePath(base)

    for typeCursor, _ in usedGroup.cursors:
      if typeCursor.getSpellingLocation().getSome(loc):
        let usedLib = conf.getSavePath(loc.file)
        if usedLib notin ignoreImport:
          let imp = conf.getImport(
            dep = usedLib,
            user = conf.getSavePath(base), false)

          if not isSelfImport(save, imp):
            file.addImport imp

    for usedLib, _ in usedGroup.libs:
      if usedLib.isValid() and usedLib notin ignoreImport:
        let imp = conf.getImport(
          dep = usedLib, user = conf.getSavePath(base), false)
        if not isSelfImport(save, imp):
          file.addImport imp

  # Pass files that were not directly affected
  for file, wrapped in mpairs(fileMap):
    if file notin clusteredNodes:
      let used = wrapped.getUsedTypes(conf)
      wrapped.addImports used.inProcs
      wrapped.addImports used.inTypes
      result.add wrapped


  # Process all groups that formed circular imports
  for group in groups:
    var
      mergedFiles: seq[WrappedFile] = mapIt(group, fileMap[importGraph[it]])
      mergedPaths: string = mapIt(group, importGraph[it].importPath[^1]).sorted().join("_")
      ignoreImports: HashSet[CxxLibImport] = mapIt(group, importGraph[it]).toHashSet()
      mergedTypes: seq[GenEntry]

    # conf.dump mergedPaths
    for file in mitems(mergedFiles):
      var recycle: seq[GenEntry]
      for entry in mitems(file.entries):
        # Split all type declarations in the each file. Re-add procedure
        # declarations back, add type declarations to the global list.
        let (newDecl, extras) = entry.fragmentType()
        recycle.add extras
        mergedTypes.add newDecl

      file.entries.add recycle

      block getProcImports:
        let usedGroup = file.getUsedTypes(conf)
        let base = conf.getBaseFile(file)

        file.addImports(usedGroup.inProcs, ignoreImports)

        file.addImport conf.getImport(
          dep = conf.initCxxLibImport(@[mergedPaths]),
          user = conf.getSavePath(base),
          false)

        file.exports.incl mergedPaths


    result.add mergedFiles

    block fillMergedFile:
      var merged =
        WrappedFile(
          entries: mergedTypes,
          isGenerated: true,
          newFile: RelFile(mergedPaths & ".nim"),
          original: @[conf.getBaseFile(mergedFiles[0])])

      var typeImports: HashSet[CxxLibImport]
      block getTypeImports:
        let usedGroup = merged.getUsedTypes(conf)
        for typeCursor, _ in usedGroup.inTypes.cursors:
          if typeCursor.getSpellingLocation().getSome(loc):
            typeImports.incl conf.getSavePath(loc.file)

        for usedLib, _ in usedGroup.inTypes.libs:
          if usedLib.isValid():
            typeImports.incl usedLib

      let mergedLibs = group.mapIt(importGraph[it]).toHashSet()
      for imp in typeImports:
        if imp notin mergedLibs:
          merged.addImport conf.getImport(
            dep = imp,
            user = getLibSavePath(conf, merged),
            false)

          merged.exports.incl imp.importPath[^1]

      result.add merged

  cache.importGraph = importGraph

  for file in mitems(result):
    file.addImport initNimImportSpec(true, @["std", "bitops"])
    file.addImport initNimImportSpec(true, @[
      "hmisc", "wrappers", "wraphelp"])

    file.exports.incl "wraphelp"


proc wrapFiles*(
    parsed: seq[ParsedFile], conf: WrapConf,
    cache: var WrapCache, index: hc_types.FileIndex): seq[WrappedFile] =

  var genFiles: seq[WrappedFile]
  for file in parsed:
    var resFile = WrappedFile(
      isGenerated: false, baseFile: file.filename)

    for node in file.explicitDeps.mapIt(
        conf.getImport(it, conf.getBaseFile(resFile), false)).
        deduplicate():

      # Add imports for explicit dependencies
      resFile.imports.incl node


    conf.unit = file.unit
    # Add wrapper for main API unit
    resFile.entries.add file.api.wrapApiUnit(conf, cache, index)
    genFiles.add resFile

    if conf.serializeTo.isSome():
      let file = withExt(
        conf.serializeTo.get() / conf.getSavePath(resFile), "json")

      ensureDir(file)
      conf.dump file
      let j = conf.toCxx(resFile, cache).toJson()
      writeFile(file, j)




  if conf.onlySerial:
    # Patch *all* wrapped file entries at once, replacing `GenForward`
    # entries with imports and returning list of additional files (for
    # strongly connected type clusters that span multiple files)
    result.add patchForward(genFiles, conf, cache)

    # Correct potential circular imports that resulted from incorrectly
    # written import converters. This stage processes all of the files, and
    # returns new list (with some passthrough elements)
    #
    # - WHY :: Things like this are almost un-debuggable, and it is easier to
    #   just perform additional cycle detection, rather than make user deal
    #   with strange failures that were caused by complicated internal
    #   machinery.

    if false:
      result.add genFiles
      for file in mitems(result):
        file.addImport initNimImportSpec(
          true, @["std", "bitops"])

        file.addImport initNimImportSpec(
          true, @["hmisc", "wrappers", "wraphelp"])

        file.exports.incl "wraphelp"

    else:
      result = updateImports(genFiles, conf, cache)



proc mergesfinae*(
    decls: seq[wrappedentry],
    conf: wrapconf, cache: var wrapcache): seq[wrappedentry] =

  var byarglen: orderedtable[int, seq[wrappedentry]]
  for decl in decls:
    byarglen.mgetorput(decl.decl.procdecl.arguments.len(), @[]).add decl

  for arglen, decls in byarglen:
    if decls.len == 1:
      result.add decls

    else:
      # multiple buckets with equal overloads
      var unifyeq: seq[seq[wrappedentry]]
      # for each declaration in input list

      for decl in decls:
        # for each group in unification buckets
        let gen1 = decl.decl.procdecl.gentable()
        for group in mitems(unifyeq):
          # for each item in bucket
          var foundbucket: bool = false
          for item in group:
            # if it is equal with any of the elements in bucket, add it to
            # the list
            let (sig1, sig2) = (decl.decl.procdecl.signature, item.decl.procdecl.signature)
            conf.dump sig1, sig2
            let gen2 = item.decl.procdecl.gentable()
            var types1, types2: seq[ntype[pnode]]

            # only consider argument types
            types1.add sig1.argumenttypes()
            types2.add sig2.argumenttypes()
            if types1.len == types2.len:
              var allok = true
              for (t1, t2) in zip(types1, types2):
                if not unify(t1, t2, gen1, gen2):
                  allok = false
                  break

              if allok:
                foundbucket = true
                group.add item
                break

          # otherwise create new bucket
          if not foundbucket:
            unifyeq.add @[decl]

      for group in unifyeq:
        if group.len == 1:
          result.add group[0]

        else:
          raise newimplementerror()

proc mergesfinae*(
    decls: orderedtable[string, seq[wrappedentry]],
    conf: wrapconf, cache: var wrapcache): seq[wrappedentry] =

  for name, grouped in pairs(decls):
    if grouped.len == 1:
      result.add grouped

    else:
      result.add mergesfinae(grouped, conf, cache)

proc wrapFile*(
    wrapped: WrappedFile, conf: WrapConf,
    cache: var WrapCache, index: hc_types.FileIndex
  ): seq[WrappedEntry] =

  var sections: tuple[
    inTypes: Table[string, WrappedEntry],
    other: array[WrappedEntryPos, seq[WrappedEntry]],
  ]

  let push = pquote do:
    {.push warning[UnusedImport]: off.}

  var procDecls: OrderedTable[string, seq[WrappedEntry]]

  sections.other[wepBeforeAll].add(
    push.toNimDecl().newWrappedEntry(wepBeforeAll, currLInfo()))

  sections.other[wepBeforeAll].add(
    wrapped.imports.
      toNNode().
      toNimDecl().
      newWrappedEntry(wepBeforeAll, currLInfo()))

  sections.other[wepBeforeAll].add(
    nnkExportStmt.newTree(
      wrapped.exports.mapIt(newPIdent(it))).
    toNimDecl().
    newWrappedEntry(wepBeforeAll, currLInfo()))


  for elem in wrapped.entries.toNNode(conf, cache):
    case elem.decl.kind:
      of nekObjectDecl:
        let name = elem.decl.objectdecl.name.head
        if name in sections.inTypes:
          discard
          # conf.warn "not overriding wrap for", name

        else:
          sections.inTypes[name] = elem

      of nekEnumDecl:
        let name = elem.decl.enumdecl.name
        sections.inTypes[name] = elem

      of nekAliasDecl:
        let name = elem.decl.aliasdecl.newType.head
        if name in sections.inTypes:
          conf.warn "Override type alias for ", name

        sections.inTypes[name] = elem

      of nekFieldDecl:
        raise newUnexpectedKindError(
          elem.decl,
          "Field declarations cannot be encountered at toplevel.",
          "This code cannot be reached.")

      of nekProcDecl:
        procDecls.mgetOrPut(elem.decl.procDecl.name, @[]).add elem

      else:
        sections.other[elem.position].add elem

  for decl in mergeSfinae(procDecls, conf, cache):
    sections.other[decl.position].add decl

  if not isNil(conf.userCode):
    let (node, position) = conf.userCode(wrapped)
    sections.other[position].add(
      newWrappedEntry(toNimDecl(node), position, currLInfo()))

  block:
    let elems = collect(newSeq):
      for _, wrapEntry in mpairs(sections.inTypes):
        updateComments(wrapEntry.decl, wrapEntry, conf, cache)
        wrapEntry.decl.toNimTypeDecl()

    sections.other[wepInTypes].add(
      newWrappedEntry(toNimDecl(elems), wepInTypes, currLInfo()))

  for position in [
    wepBeforeAll,
    wepInTypes,
    wepAfterTypesBeforeProcs,
    wepInProcs,
    wepAfterAll
  ]:
    for elem in mitems(sections.other[position]):
      case elem.decl.kind:
        of nekProcDecl:
          updateComments(elem.decl, elem, conf, cache)
          result.add elem

        else:
          result.add elem


func wrapName*(res: WrapResult): string =
  res.importName.importPath.join("/") & ".nim"

proc wrapSingleFile*(
    file: FsFile,
    errorReparseVerbose: bool = false,
    conf: WrapConf = baseCppWrapConf,
    parseConf: ParseConf = baseCppParseConf,
  ): CodegenResult =
  ## Generate wrapper for a single file.
  ##
  ## - @arg{postprocess} :: collection of postprocessing actions on
  ##  generated delarations, before they are converter to sequence of
  ##  `NimDecl`
  ##
  ## `conf` provide user-defined implementation heuristics for
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

  assertRef conf.logger

  if conf.baseDir.len == 0:
    raise newArgumentError(
      "Base director is not set for wrapper configuration. " &
        "Set configuration .baseDir to the root directory of C(++) " &
        "source files")

  var
    cache: WrapCache
    index: hc_types.FileIndex

  let parsed = parseFile(
    file.toAbsFile(), parseConf, conf,
    cache,
    reparseOnNil = errorReparseVerbose
  )

  if conf.showParsed:
    conf.debug parsed.unit.getTranslationUnitCursor().treeRepr(parsed.unit)

  var conf = conf

  conf.unit = parsed.unit

  let wrapped = wrapFiles(@[parsed], conf, cache, index)[0].
    wrapFile(conf, cache, index)

  for node in wrapped:
    result.decls.add node.decl

  result.cache = cache


proc wrapAllFiles*(
    files: seq[AbsFile],
    conf: WrapConf,
    parseConf: ParseConf
  ): WrapCache =
  ## Generate and write wrappers for all `files`

  assertRef conf.logger

  for dep in conf.depsConf:
    dep.logger = conf.logger

  var
    cache: WrapCache # Global cache for all parsed files
    index: hc_types.FileIndex
    parsed: seq[ParsedFile] # Full list of all parsed files
    conf = conf # Global wrapper configuration

  assertValid(
    conf.nimOutDir,
    ". Output directory for generated nim files.")

  for file in files:
    var parsedFile = parseFile(file, parseConf, conf, cache)
    conf.unit = parsedFile.unit

    parsed.add parsedFile

  var wrapped: seq[seq[WrappedEntry]]
  for file in wrapFiles(parsed, conf, cache, index):
    let outPath = conf.nimOutDir / conf.getSavePath(file)

    var codegen: CodegenResult
    for node in wrapFile(file, conf, cache, index):
      codegen.decls.add node.decl

    codegen.writeWrapped(outPath, @[], conf)

  CodegenResult(cache: cache).writeWrapped(
    AbsFile(""), @[], conf)

  return cache


proc wrapWithConf*(
    files: seq[AbsFile], conf: WrapConf,
    parseConf: ParseConf
  ): WrapCache = wrapAllFiles(files, conf, parseConf)

proc wrapWithConf*(
    infile, outfile: FsFile, conf: WrapConf,
    parseConf: ParseConf
  ) =

  writeWrapped(
    wrapSingleFile(
      infile,
      errorReparseVerbose = false,
      conf = conf,
      parseConf = parseConf
    ),
    outFile = outfile,
    compile = @[],
    conf = conf
  )
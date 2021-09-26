import
  std/[options, hashes, sets, algorithm, sequtils, strutils, tables]

import
  ./interop_ir/wrap_store

import
  hmisc/types/[hgraph, colorstring],
  hmisc/hasts/graphviz_ast,
  hmisc/other/oswrap,
  hmisc/algo/halgorithm,
  hmisc/core/all

type
  TypeNode = object
    decl: CxxTypeDecl

  Link = enum direct, forward, forwardReuse
  TypeGraph = HGraph[TypeNode, Link]

  TypeGroup = object
    nodes: HNodeSet
    imports: HashSet[CxxLibImport]
    files: HashSet[CxxLibImport]
    external: HashSet[CxxLibImport]


proc nodeImport(graph: TypeGraph, node: HNode): CxxLibImport =
  graph[node].decl.getImport()

proc groupFile(group: TypeGroup, graph: TypeGraph): string =
  var files: HashSet[string]
  for node in group.nodes:
    files.incl graph[node].decl.getImport().getFilename()

  result = toSeq(files).sorted().join("_")

func hash(typeNode: TypeNode): Hash = hash(typeNode.decl)

func initTypeNode*(decl: CxxTypeDecl): TypeNode =
  TypeNode(decl: decl)

proc getTypeGraph(wrapped: var seq[CxxFile]): TypeGraph =
  result = newHGraph[TypeNode, Link]()
  for file in mitems(wrapped):
    var extraEntries: seq[CxxEntry]
    for entry in file.entries:
      case entry.kind:
        of cekProc:
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
          var procTypes: seq[CxxTypeUse]
          for arg in entry.cxxProc.arguments:
            procTypes.add arg.getType()

          procTypes.add entry.cxxProc.getReturn()

        of cekObject:
          # Get node for current type
          var objectNode = result.addOrGetNode(initTypeNode(entry.cxxObject.decl))

          # Add outgoing edges for all types that were explicitly used.
          # info entry.genObject.name.nimName
          for field in entry.cxxObject.mfields:
            for used in field.getType().getUsedTypesRec():
              if not used.isPod():
                let decl = used.getDecl()
                if decl.isSome(): # REVIEW do I need to handle missing type
                                  # declarations explicitly? Ideally I
                                  # should, this would make debugging
                                  # thigns much easier.
                  let decl = decl.get()
                  let node = initTypeNode(decl)
                  discard result.addOrGetEdge(
                    objectNode,
                    result.addOrGetNode(node),
                    if decl.isForward: forward else: direct
                  )

        of cekForward:
          raise newImplementError()

        else:
          discard

    file.entries.add extraEntries

  for t1 in nodes(result):
    for t2 in nodes(result):
      if t1 != t2 and
         result[t1].decl.cxxName == result[t2].decl.cxxName
         #[ REVIEW comparing using `cxxName` alone might be insufficient if
         I have forward declarations etc. from different header files. Also
         I don't remember what this part was supposed to be doing, so this
         is also something I need to figure out ]#:
        discard
        result.addOrGetEdge(t1, t2, forwardReuse)


proc dependentComponents(graph: TypeGraph): seq[TypeGroup] =
  var groups: seq[TypeGroup]
  var groupedNodes: HNodeSet
  for path in graph.findCycles(ignoreSelf = true).mergeCycleSets():
    var group = TypeGroup(nodes: path)
    for node in group.nodes:
      group.files.incl graph.nodeImport(node)

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
    var externalFiles: HashSet[CxxLibImport]
    let extendedGroup = extendOutgoing(
      graph, group.nodes) do(node: HNode) -> bool:

      let file = graph.nodeImport(node)
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
      let file = $typeGraph.nodeImport(hnode)
      result = makeDotNode(
        0, $node.decl.cxxName() & "\n" & file,
        fillcolor = dotColor(
          if node.decl.isForward: fgCyan else: fgYellow,
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

proc patchForward*(wrapped: var seq[CxxFile]): seq[CxxFile] =
  ## Replace `GenForward` declarations with required import. Return list of
  ## additional generated files.

  var typeGraph = getTypeGraph(wrapped)
  let components = typeGraph.dependentComponents()
  typeGraph.
    dotRepr(components).
    toPng(getAppTempFile("forwardComponents.png"))


  var
    # Mapping between new file name and pair of (cursors that were moved to
    # the file)+(wrapped file object)
    movedForward: Table[string,
      tuple[cursors: Hashset[CxxTypeDecl], file: CxxFile]]

    # Set of cursors that were forward-declared and defined in the same
    # file. These ones are simply dropped
    droppedForward: HashSet[CxxTypeDecl]

  # `find/store` phase of the patching - determine mapping between elements
  # that need to be moved forward, and ones that should be dropped, or kept
  # in place.
  for group in components:
    if group.files.len == 1:
      # All parts of the forward-declare graph are located in the same file.
      for node in group.nodes:
        if typeGraph[node].decl.isForward:
          droppedForward.incl typeGraph[node].decl

    else:
      # Forward-declared nodes form strongly connected graph that would
      # lead to mutually recursive imports. By grouping all declarations in
      # a single file it is now possible to put them in single `type`
      # section as well.
      var cursors: HashSet[CxxTypeDecl]
      for node in group.nodes:
        cursors.incl typeGraph[node].decl

      # Identify list of external files that need to be additionally
      # imported to generated files.

      let newFile = group.groupFile(typeGraph)
      if newFile notin movedForward:
        var genFile = CxxFile()

        # for extern in group.imports:
        #   genFile.imports.incl conf.getImport(
        #     extern, conf.getBaseFile(genFile), false)

        movedForward[newFile] = (cursors, genFile)

      else:
        movedForward[newFile].cursors.incl cursors

  # Move entries to generated files and replace them with corresponding
  # imports. `copy/import` phase of the patching.
  for wrappedFile in mitems(wrapped):
    for entry in mitems(wrappedFile.entries):
      if entry.hasTypeDecl():
        for _, (cursors, file) in mpairs(movedForward):
          file.imports.incl wrappedFile.imports
          if entry.getTypeDecl() in cursors:
            if entry.kind notin {cekForward}:
              file.entries.add entry

  # `drop/move` phrase of the patching - all cursors that participate in
  # type groups are either droppped (forward declaration is simply replaced
  # with empty entry in case of group consisting of a single file) *or*
  # import-export pair (via modification of the `.imports` and `.exports`
  # sets for wrapped file)
  var addedRel: HashSet[CxxLibImport] # List of newly created files for the
                                      # library
  for wrappedFile in mitems(wrapped):
    for entry in mitems(wrappedFile.entries):
      if entry.hasTypeDecl():
        if entry.getTypeDecl() in droppedForward:
          entry[] = cxxEmpty()[]

        else:
          var moved: bool = false
          for filename, _ in movedForward:
            # HACK I can't use `filename, (cursors, file)` here because of
            # C codegen bug when compiling.
            let file = movedForward[filename].file
            if entry.getTypeDecl() in movedForward[filename].cursors:
              entry = cxxEmpty()

              # FIXME port for new API
              # wrappedFile.imports.incl initNimImportSpec(false, @[filename])
              # wrappedFile.exports.incl filename

              if file.savePath notin addedRel:
                addedRel.incl file.savePath
                result.add file

              moved = true

              break

          if not moved and entry.kind in {cekForward}:
            # Forward declarations that were never defined in any of the
            # header files (pointer to implementation, opaque handlers
            # etc.)
            if entry.hasTypeDecl():
              # Creating new wrapped entry for forward declaration by
              # promoting forward declaration.
              entry = toRealDecl(entry)


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
  UsedSet {.requiresinit.} = ref object
    cursors: Table[CxxTypeDecl, HashSet[CxxTypeUse]]
    libs: Table[CxxLibImport, HashSet[CxxTypeUse]]

  UsedGroups = object
    inProcs: UsedSet
    inTypes: UsedSet

func mgetOrDefault*[K, V](table: var Table[K, V], key: K): var V =
  if key notin table:
    table[key] = default(V)

  return table[key]

proc registerUse*(ctype: CxxTypeUse, used: var UsedSet) =
  if isNil(ctype): return
  var used = used

  eachIdent(ctype) do (use: CxxTypeUse):
    let decl = use.getDecl()
    if decl.isSome():
      used.cursors.mgetOrDefault(decl.get()).incl use


proc registerUsedTypes*(genProc: CxxProc, used: var UsedSet) =
  for arg in genProc.arguments:
    registerUse(arg.getType(), used)

  registerUse(genProc.returnType, used)

proc registerUsedTypes*(entry: CxxEntry, used: var UsedGroups) =

  case entry.kind:
    of cekEnum, cekMacro, cekComment, cekTypeGroup, cekMacroGroup:
      discard

    of cekProc:
      registerUsedTypes(entry.cxxProc, used.inProcs)

    of cekObject:
      for field in entry.cxxObject.mfields:
        registerUse(field.getType(), used.inTypes)

      for meth in entry.cxxObject.methods:
        registerUsedTypes(meth, used.inProcs)

      for nested in entry.cxxObject.nested:
        registerUsedTypes(nested, used)

    of cekAlias:
      registerUse(entry.cxxAlias.baseType, used.inTypes)

    of cekPass, cekImport, cekForward, cekEmpty:
      discard

proc getUsedTypes*(file: CxxFile): UsedGroups =
  result = UsedGroups(inProcs: UsedSet(), inTypes: UsedSet())
  for e in file.entries:
    e.registerUsedTypes(result)

proc updateImports*(wrapped: seq[CxxFile]): seq[CxxFile] =
  ## Construct new group of wrapped files based on the input. Group
  ## mutually recursive types in new temporary files and add needed imports
  ## and exports.

  var usedApis = UsedGroups(inProcs: UsedSet(), inTypes: UsedSet()) # Set of
  # cursors pointing to declarations for different types used in the file
  # entries (procedure argument and return, field, global variable types).

  for file in wrapped:
    for entry in file.entries:
      registerUsedTypes(entry, usedApis)

  var store: CxxTypeStore
  for decl, vals in usedApis.inTypes.cursors:
    if notNil(decl.store): store = decl.store; break

  var
    importGraph = newHGraph[CxxLibImport, CxxTypeUse]()
    fileMap: Table[CxxLibImport, CxxFile]

  for file in wrapped:
    fileMap[file.savePath] = file

  proc addImports(
      graph: var HGraph[CxxLibImport, CxxTypeUse],
      file: CxxFile, used: UsedSet
    ) =

    block cursorBasedImportrs:
      let user = file.savePath
      for typeCursor, typeSet in used.cursors:
        let dep = typeCursor.getImport()
        if user != dep:
          for item in typeSet:
            graph.addOrGetEdge(
              graph.addOrGetNode(user),
              graph.addOrGetNode(dep),
              item)

    block libraryOverrideImports:
      let base = file.savePath
      for usedLib, typeSet in used.libs:
        for usedType in typeSet:
          graph.addOrGetEdge(
            graph.addOrGetNode(base),
            graph.addOrGetNode(usedLib),
            usedType)

  block registerImports:
    var
      onlyTypes = newHGraph[CxxLibImport, CxxTypeUse]()
      onlyProcs = newHGraph[CxxLibImport, CxxTypeUse]()

    for file in wrapped:
      onlyTypes.addImports(file, usedApis.inTypes)
      onlyProcs.addImports(file, usedApis.inProcs)

      importGraph.addImports(file, usedApis.inTypes)
      importGraph.addImports(file, usedApis.inProcs)

    onlyTypes.
      dotRepr(
        dotReprDollarNode[CxxLibImport],
        dotReprCollapseEdgesJoin[CxxTypeUse],
        clusters = onlyTypes.findCycles(ignoreSelf = true).
      mergeCycleSets().mapIt((it, ""))).withResIt do:
        it.rankdir = grdLeftRight
        it.toPng(getAppTempFile("onlyTypes.png"))

    onlyProcs.
      dotRepr(
        dotReprDollarNode[CxxLibImport],
        dotReprCollapseEdgesJoin[CxxTypeUse],
        clusters = onlyTypes.findCycles(ignoreSelf = true).
      mergeCycleSets().mapIt((it, ""))).withResIt do:
        it.rankdir = grdLeftRight
        it.toPng(getAppTempFile("onlyProcs.png"))



  let groups = importGraph.findCycles().mergeCycleSets()
  let clusteredNodes = groups.mapIt(
    toHashSet(importGraph[it])).foldl(union(a, b))


  proc addImports(
      file: var CxxFile,
      usedGroup: UsedSet,
      ignoreImport: HashSet[CxxLibImport] = default(HashSet[CxxLibImport])
    ) =

    let save = file.savePath
    for typeCursor, _ in usedGroup.cursors:
      if typeCursor.hasImport():
        let usedLib = typeCursor.getImport()
        if usedLib notin ignoreImport:
          if save != usedLib:
            file.imports.incl usedLib

    for usedLib, _ in usedGroup.libs:
      if usedLib notin ignoreImport:
        if save != usedLib:
          file.imports.incl usedLib

  # Pass files that were not directly affected
  for file, wrapped in mpairs(fileMap):
    if file notin clusteredNodes:
      let used = wrapped.getUsedTypes()
      wrapped.addImports used.inProcs
      wrapped.addImports used.inTypes
      result.add wrapped


  # Process all groups that formed circular imports
  for group in groups:
    var
      mergedFiles: seq[CxxFile] = mapIt(group, fileMap[importGraph[it]])
      mergedLib: string = mergedFiles.mapIt(
        it.getLibrary()).toHashSet().toSeq()[0] # HACK - does not account
        # for type graphs that spans multiple packages. This is unlikely,
        # but possible in case of inter-*project* circular dependencies.

      generatedFile: CxxLibImport = cxxLibImport(mergedLib, @[
        mergedFiles.mapIt(it.getFilename()).sorted().join("_")])

      ignoreImports: HashSet[CxxLibImport] = mapIt(
        group, importGraph[it]).toHashSet()

      mergedTypes: seq[CxxEntry]

    for file in mitems(mergedFiles):
      var recycle: seq[CxxEntry]
      for entry in mitems(file.entries):
        # Split all type declarations in the each file. Re-add procedure
        # declarations back, add type declarations to the global list.
        if entry of cekForward:
          if entry.cxxForward.decl.hasImport():
            entry = cxxEmpty()

          else:
            raise newImplementError("Promote type declaration")

        else:
          let (newDecl, extras) = entry.fragmentType()
          recycle.add extras
          mergedTypes.add newDecl

      file.entries.add recycle

      block getProcImports:
        let usedGroup = file.getUsedTypes()
        file.addImports(usedGroup.inProcs, ignoreImports)
        file.imports.incl generatedFile
        file.exports.incl generatedFile


    result.add mergedFiles

    block fillMergedFile:
      var merged =
        CxxFile(
          entries: mergedTypes,
          isGenerated: true,
          savePath: generatedFile)

      var typeImports: HashSet[CxxLibImport]
      block getTypeImports:
        let usedGroup = merged.getUsedTypes()
        for typeCursor, _ in usedGroup.inTypes.cursors:
          if typeCursor.hasImport():
            typeImports.incl typeCursor.getImport()

        for usedLib, _ in usedGroup.inTypes.libs:
          typeImports.incl usedLib

      let mergedLibs = group.mapIt(importGraph[it]).toHashSet()
      for imp in typeImports:
        if imp notin mergedLibs:
          merged.imports.incl imp
          merged.exports.incl imp

      result.add merged

  # cache.importGraph = importGraph

  # for file in mitems(result):
  #   file.addImport initNimImportSpec(true, @["std", "bitops"])
  #   file.addImport initNimImportSpec(true, @[
  #     "hmisc", "wrappers", "wraphelp"])

  #   file.exports.incl "wraphelp"

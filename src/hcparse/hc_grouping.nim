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

          # for it in procTypes:
          #   for used in it.getUsedTypesRec():
          #     let decl = used.getDecl()
          #     if decl.cxKind() in {ckEnumDecl} and decl.isForward():
          #       cdecl[] = entry.cdecl()[]
          #       cdecl.cursor = decl
          #       let
          #         loc = decl.getSpellingLocation().get()
          #         path = conf.getImport(
          #           loc.file, conf.getBaseFile(file), false).importPath
          #         node = initTypeNode(
          #           used.nimName, path,
          #           cdecl.cursor, isDef = false)

          #       let enumNode = result.addOrGetNode(node)


        of cekObject:
          # Get node for current type
          var objectNode = result.addOrGetNode(initTypeNode(entry.cxxObject.decl))

          # Add outgoing edges for all types that were explicitly used.
          # info entry.genObject.name.nimName
          for field in entry.cxxObject.mfields:
            for used in field.getType().getUsedTypesRec():
              if not used.isPod():
                let decl = used.getDecl()
                let node = initTypeNode(decl)

                discard result.addOrGetEdge(
                  objectNode,
                  result.addOrGetNode(node),
                  if decl.isForward: forward else: direct
                )

        of cekForward:
          discard

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
  UsedSet = object
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
  when false:
    if isNil(ctype):
      return

    if ctype.fromCxType:
      let cxDecl = ctype.cxType.getTypeDeclaration()
      if cxDecl.kind notin {ckNoDeclFound}:
        used.cursors.mgetOrDefault(cxDecl).incl ctype

    else:
      used.libs.mgetOrDefault(ctype.typeImport).incl ctype

    if ctype.fullIdent.isSome():
      let last = ctype.fullIdent.get()[^1]
      if not last.isGenerated:
        used.cursors.mgetOrDefault(last.cursor).incl ctype

      for param in last.genParams:
        if param.len > 0 and param[^1].isGenerated.not():
          let lastParam = param[^1]
          if lastParam.cursor.kind notin { ckTemplateTypeParameter }:
            conf.dump param, lastParam.cursor.cxKind()

    case ctype.kind:
      of ctkPtr:
        registerUse(ctype.wrapped, used, conf)

      of ctkIdent:
        if ctype.defaultType.isSome():
          registerUse(ctype.defaultType.get(), used, conf)

        for param in ctype.genParams:
          registerUse(param, used, conf)

      of ctkProc:
        for arg in ctype.arguments:
          if not arg.isRaw:
            registerUse(arg.ctype, used, conf)

        registerUse(ctype.returnType, used, conf)


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
      registerUse(entry.cxxAlias.oldType, used.inTypes)

    of cekPass, cekImport, cekForward, cekEmpty:
      discard

proc getUsedTypes*(file: CxxFile): UsedGroups =
  for e in file.entries:
    e.registerUsedTypes(result)

proc updateImports*(wrapped: seq[CxxFile]): seq[CxxFile] =
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
      registerUsedTypes(entry, usedApis)

  var
    importGraph = newHGraph[CxxLibImport, CxxTypeUse]()
    fileMap: Table[CxxLibImport, CxxFile]

  for file in wrapped:
    fileMap[file.savePath] = file

  proc addImports(graph: var HGraph[CxxLibImport, CxxTypeUse], file: CxxFile, used: UsedSet) =
    block cursorBasedImportrs:
      let user = file.savePath
      for typeCursor, typeSet in used.cursors:
        let dep = typeCursor.getImport()
        if user != dep:
          for item in typeSet:
            graph.addEdge(
              graph.addOrGetNode(user),
              graph.addOrGetNode(dep),
              item)

    block libraryOverrideImports:
      let base = file.savePath
      for usedLib, typeSet in used.libs:
        for usedType in typeSet:
          graph.addEdge(
            graph.addOrGetNode(base),
            graph.addOrGetNode(usedLib),
            usedType)

  block registerImports:
    var
      onlyTypes = newHGraph[CxxLibImport, CxxTypeUse]()
      onlyProcs = newHGraph[CxxLibImport, CxxTypeUse]()

    for file in wrapped:
      onlyTypes.addImports file, usedApis.inTypes
      onlyProcs.addImports file, usedApis.inProcs

      importGraph.addImports file, usedApis.inTypes
      importGraph.addImports file, usedApis.inProcs

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
  let clusteredNodes = groups.mapIt(toHashSet(importGraph[it])).foldl(union(a, b))


  proc addImports(
      file: var CxxFile,
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
      mergedFiles: seq[CxxFile] = mapIt(group, fileMap[importGraph[it]])
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
        CxxFile(
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


proc patchForward*(
    wrapped: var seq[CxxFile], conf: WrapConf, cache: var WrapCache):
  seq[CxxFile] =
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
    movedForward: Table[string, tuple[cursors: Hashset[CXCursor], file: CxxFile]]
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
        var genFile = CxxFile(
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
            if entry.kind notin {cekForward}:
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

          if not moved and entry.kind in {cekForward}:
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
    of cekEnum, cekMacro, cekComment:
      discard

    of cekProc:
      registerUsedTypes(entry.genProc, used.inProcs, conf)

    of cekObject:
      for field in entry.genObject.memberFields:
        registerUse(field.fieldType, used.inTypes, conf)

      for meth in entry.genObject.memberMethods:
        registerUsedTypes(meth, used.inProcs, conf)

      for nested in entry.genObject.nestedEntries:
        registerUsedTypes(nested, used, conf)

    of cekAlias:
      registerUse(entry.genAlias.baseType, used.inTypes, conf)

    of cekPass, cekImport, cekForward, cekEmpty:
      discard

proc getUsedTypes*(file: CxxFile, conf: WrapConf): UsedGroups =
  for e in file.entries:
    e.registerUsedTypes(result, conf)

proc updateImports*(
    wrapped: seq[CxxFile],
    conf: WrapConf,
    cache: var WrapCache
  ): seq[CxxFile] =

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
    fileMap: Table[CxxLibImport, CxxFile]

  for file in wrapped:
    let lib = conf.getSavePath(conf.getBaseFile(file))
    fileMap[lib] = file

  proc isSelfImport(save: CxxLibImport, imp: NimImportSpec): bool =
    imp.isRelative and
    imp.relativeDepth == 0 and
    imp.importPath.len == 1 and
    imp.importPath[^1] == save.importPath[^1]

  proc addImports(graph: var HGraph[CxxLibImport, NimType], file: CxxFile, used: UsedSet) =
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
      file: var CxxFile,
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
      mergedFiles: seq[CxxFile] = mapIt(group, fileMap[importGraph[it]])
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
        CxxFile(
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

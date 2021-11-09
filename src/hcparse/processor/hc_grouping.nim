import
  std/[options, hashes, sets, algorithm, sequtils, strutils, tables]

import
  ./wrap_store,
  ./hc_postprocess

import
  hmisc/types/[hgraph, colorstring],
  hmisc/hasts/graphviz_ast,
  hmisc/algo/halgorithm,
  hmisc/core/all

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
    ## Set of cursors used in each declaration. Becase procedures also use
    ## `CxxTypeDecl` for their head declarations `UsedSet` can store any
    ## kind of declaration entry dependencies.
    cursors: Table[CxxTypeDecl, HashSet[CxxTypeUse]]
    libs: Table[CxxLibImport, HashSet[CxxTypeUse]]

  UsedGroups = object
    inProcs: UsedSet
    inTypes: UsedSet

proc registerUse*(ctype: CxxTypeUse, used: var UsedSet) =
  ## Register type and all it's inner used types (proctype arguments,
  ## generic parameters etc) in the used set.
  if isNil(ctype): return
  var used = used

  eachIdent(ctype) do (use: CxxTypeUse):
    let decl = use.getDecl()
    if decl.isSome():
      used.cursors.mgetOrDefault(decl.get()).incl use

    elif use.hasExternalImport():
      let lib = use.getExternalImport()
      used.libs.mgetOrDefault(lib).incl use


proc registerUsedTypes*(genProc: CxxProc, used: var UsedSet) =
  ## Register return type and all argument's types in used set
  for arg in genProc.arguments:
    registerUse(arg.getType(), used)

  registerUse(genProc.returnType, used)

proc registerUsedTypes*(entry: CxxEntry, used: var UsedGroups) =
  ## Register all used types (fields, type alias bases, procedure
  ## parameters etc.) for any entry kinds in used set
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

proc newUsedGroups*(): UsedGroups =
  UsedGroups(inProcs: UsedSet(), inTypes: UsedSet())

proc getUsedTypes*(file: CxxFile): UsedGroups =
  ## Get set of used types for all entries declared in the file
  result = newUsedGroups()
  for e in file.entries:
    e.registerUsedTypes(result)

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
          file.addImport(usedLib)

  for usedLib, _ in usedGroup.libs:
    if usedLib notin ignoreImport:
      if save != usedLib:
        file.addImport(usedLib)

proc addImports(
    graph: var HGraph[CxxLibImport, CxxTypeUse],
    file: CxxFile, used: UsedSet
  ) =
  ## Add imports from used set to file group

  block cursorBasedImportrs:
    # Library imports based on type definition location
    let user = file.savePath
    for typeCursor, typeSet in used.cursors:

      let dep = typeCursor.getImport()
      if user != dep: # Avoid self-imports (creates unnecessary
                      # self-loops in graphs)A
        for item in typeSet:
          graph.addOrGetEdge(
            graph.addOrGetNode(user),
            graph.addOrGetNode(dep),
            item)

  block libraryOverrideImports:
    # Direct library overrides for imports
    let base = file.savePath
    for usedLib, typeSet in used.libs:
      for usedType in typeSet:
        graph.addOrGetEdge(
          graph.addOrGetNode(base),
          graph.addOrGetNode(usedLib),
          usedType)

proc getTypeStore*(wrapped: seq[CxxFile]): CxxTypeStore =
  for file in wrapped:
    for entry in file.entries:
      case entry.kind:
        of cekObject:
          result = entry.cxxObject.decl.store
          assertRef(result, $entry)

        of cekEnum:
          result = entry.cxxEnum.decl.store
          assertRef(result, $entry)

        of cekAlias:
          result = entry.cxxAlias.decl.store
          assertRef(result, $entry)

        else:
          discard

      if isNil(result):
        var used = newUsedGroups()
        registerUsedTypes(entry, used)
        for decl, vals in used.inTypes.cursors:
          assertRef decl.store, "Missing store ref for " & $decl
          result = decl.store
          break

  assertRef result

type TypeGraph* = HGraph[CxxLibImport, CxxTypeUse]

proc getGroups*(importGraph: TypeGraph): seq[HNodeSet] =
  importGraph.findCycles().mergeCycleSets()

proc mergeFileName*(names: seq[string]): string =
  result = sorted(names).join("_")
  var pos = min(result.high, 239)
  while result[pos] == '_': dec pos
  result = result[0 .. pos]

proc groupFileName*(group: HNodeSet, graph: TypeGraph): string =
  mapIt(group, graph[it].getFilename()).mergeFileName()

proc dotRepr*(g: TypeGraph): DotGraph =
  let groups = getGroups(g)
  result = dotRepr(
    g,
    dotReprDollarNode[CxxLibImport],
    dotReprCollapseEdgesJoin[CxxTypeUse],
    clusters = groups.mapIt((it, it.groupFileName(g)))
  )

  result.rankdir = grdLeftRight

proc buildTypeGraph*(wrapped: seq[CxxFile]): TypeGraph =
  ## Type use graph - nodes represent files and `CxxTypeUse` is an edge
  ## between two files. If one file uses type from another link is formed.

  result = newHGraph[CxxLibImport, CxxTypeUse]()

  for file in wrapped:
    let used = file.getUsedTypes()
    result.addImports(file, used.inTypes)
    result.addImports(file, used.inProcs)

proc removeForwardDeclared*(wrapped: var seq[CxxFile], store: CxxtypeStore) =
  for file in mitems(wrapped):
    for entry in mitems(file.entries):
      # Drop forward type declarations that were defined in some other
      # file (can be imported).
      if entry of cekForward:
        let decl = store.getDecl(
          entry.cxxForward.decl.name.cxx,
          some file.savePath.library)

        if decl.isSome():
          let decl = decl.get()
          if not decl.isForward:
            # Has real type declaration, all forwards declarations can be
            # removed
            entry = cxxEmpty()

          # if decl.typeImport.get() != file.savePath:
          #   file.addImport(decl.typeImport.get())

proc regroupFiles*(wrapped: seq[CxxFile]): seq[CxxFile] =
  ## Construct new group of wrapped files based on the input. Group
  ## mutually recursive types in new temporary files and add needed imports
  ## and exports.
  let store = getTypeStore(wrapped)
  let importGraph = buildTypeGraph(wrapped)
  let groups = importGraph.getGroups()
  if groups.len == 0:
    # There is no mutually recursive type cycles in the passed files,
    # returning whole list unmodified.
    result = wrapped
    removeForwardDeclared(result, store)
    for file in mitems(result):
      let used = file.getUsedTypes()
      file.addImports(used.inTypes)
      file.addImports(used.inProcs)

    return

  var fileMap: Table[CxxLibImport, CxxFile] # Mapping of the file import
                                            # location to the actual file
                                            # content

  for file in wrapped:
    fileMap[file.savePath] = file

  let clusteredNodes = groups.mapIt(
    toHashSet(importGraph[it])).foldl(union(a, b))

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

      generatedFile: CxxLibImport = cxxLibImport(
        mergedLib, @[group.groupFileName(importGraph)])
        # Name of the generated file
        #
        # TODO allow overriding into someting more manageable. With large
        # libraries it would be possible to hit file name lenght limit on
        # windows and other oses that have this limitation

      ignoreImports: HashSet[CxxLibImport] = mapIt(
        group, importGraph[it]).toHashSet()

      mergedTypes: seq[CxxEntry]

    for file in mitems(mergedFiles):
      var recycle: seq[CxxEntry]
      for entry in mitems(file.entries):
        # Split all type declarations in the each file. Re-add procedure
        # declarations back, add type declarations to the global list.
        if entry of cekForward:
          if entry.cxxForward.decl.hasFullDecl():
            entry = cxxEmpty()

        else:
          let (newDecl, extras) = entry.fragmentType()
          recycle.add extras
          mergedTypes.add newDecl

      file.entries.add recycle

      block getProcImports:
        let usedGroup = file.getUsedTypes()
        file.addImports(usedGroup.inProcs, ignoreImports)
        file.addReExport(generatedFile)


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
          merged.addReExport(imp)

      result.add merged

  # cache.importGraph = importGraph

  # for file in mitems(result):
  #   file.addImport initNimImportSpec(true, @["std", "bitops"])
  #   file.addImport initNimImportSpec(true, @[
  #     "hmisc", "wrappers", "wraphelp"])

  #   file.exports.incl "wraphelp"

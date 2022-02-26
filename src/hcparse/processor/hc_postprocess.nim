import
  ./wrap_store

import
  std/[tables, strutils, sequtils]

import
  hmisc/core/[all],
  hmisc/other/[oswrap, hpprint],
  hmisc/algo/[namegen, hstring_algo]

type
  CxxAdjacentNameContext* = enum
    cancFirstArgType
    cancFirstArgName
    cancParentEnumName
    cancParentObjectName
    cancLibName

  CxxNameFixContext* = array[CxxAdjacentNameContext, Option[CxxNamePair]]
  CxxFixConf* = object
    typeStore*: CxxTypeStore
    fixNameImpl*: proc(
      name: CxxNamePair,
      cache: var StringNameCache,
      context: CxxNameFixContext,
      conf: CxxFixConf
    ): string

    getBindImpl*: proc(entry: CxxEntry, conf: CxxFixConf): CxxBind
    libName*: string
    isIcpp*: bool

proc getBind*(conf: CxxFixConf, entry: CxxEntry): CxxBind =
  assertRef conf.getBindImpl
  return conf.getBindImpl(entry, conf)

template onGetBind*(fixConf: var CxxFixConf, body: untyped): untyped =
  ## Add 'get bind' callback to fix context configuration.
  ## - @inject{entry: CxxEntry} :: Entry to get bind for
  ## - @inject{conf: CxxFixConf} :: Current fix context configuration
  ## - @ret{CxBind}
  fixConf.getBindImpl = proc(
    entry {.inject.}: CxxEntry,
    conf {.inject.}: CxxFixConf
  ): CxxBind =
    body

template onFixName*(fixConf: var CxxFixConf, body: untyped): untyped =
  ## Add 'fix name' callback to fix context.
  ## - @inject{name} :: Input name to fix
  ## - @inject{cache} :: Global string name cache
  ## - @inject{context} :: Current fix context
  ## - @inject{conf} :: Current fix configuration
  ## - @ret{string} :: Fixed name string. Body is wrapped in the callback
  ##   procedure, `return`/`result=` can be used

  fixConf.fixNameImpl = proc(
      name {.inject.}: CxxNamePair,
      cache {.inject.}: var StringNameCache,
      context {.inject.}: CxxNameFixContext,
      conf {.inject.}: CxxFixConf
    ): string =

    body


proc fixName*(
    conf: CxxFixConf,
    name: CxxNamePair,
    cache: var StringNameCache,
    context: CxxNameFixContext
  ): string =
  result = conf.fixNameImpl(name, cache, context, conf)

  if not cache.knownRename(name.nim):
    raise newLogicError(
      "'fix name' callback implementation did not register new rename: '",
      name, "' is not in list of known renames. In order to register new ",
      "rename call '<cache>.newRename(<name.nim>, <result>)' at the end of callback")


proc setHeaderRec*(entry: var CxxEntry, conf: CxxFixConf) =
  case entry.kind:
    of cekPass, cekImport, cekEmpty, cekComment:
      discard

    of cekTypeGroup:
      raise newImplementKindError(entry)

    of cekMacroGroup:
      for it in mitems(entry.cxxMacroGroup.macros):
        setHeaderRec(it, conf)

    of cekMacro:   entry.cxxMacro.cbind.setCxxBind(conf.getBind(entry))
    of cekForward: entry.cxxForward.cbind.setCxxBind(conf.getBind(entry))
    of cekEnum:    entry.cxxEnum.cbind.setCxxBind(conf.getBind(entry))
    of cekProc:    entry.cxxProc.cbind.setCxxBind(conf.getBind(entry))
    of cekAlias:   entry.cxxAlias.cbind.setCxxBind(conf.getBind(entry))
    of cekObject:
      entry.cxxObject.cbind.setCxxBind(conf.getBind(entry))
      for meth in mitems(entry.cxxObject.methods):
        meth.cbind.setCxxBind(conf.getBind(box(meth)))

      for nest in mitems(entry.cxxObject.nested):
        setHeaderRec(nest, conf)

func libImport*(conf: CxxFixConf, dir: AbsDir, file: AbsFile): CxxLibImport =
  let relative = file.string.dropPrefix(dir.string)
  result = cxxLibImport(
    conf.libName, relative.split("/").filterIt(it.len > 0))


func registerDeclarations*(
    entry: var CxxEntry,
    store: var CxxTypeStore,
    lib: CxxLibImport
  ) =

  case entry.kind:
    of cekPass, cekEmpty, cekImport, cekProc,
       cekMacroGroup, cekMacro, cekComment:
      discard

    of cekTypeGroup:
      for decl in mitems(entry.cxxTypes):
        registerDeclarations(entry, store, lib)

    of cekEnum:
      entry.cxxEnum.decl.typeImport = some lib
      store.addDecl(entry.cxxEnum.decl)

    of cekForward:
      entry.cxxForward.decl.typeImport = some lib
      store.addForwardDecl(entry.cxxForward.decl)

    of cekAlias:
      entry.cxxAlias.decl.typeImport = some lib
      store.addDecl(entry.cxxAlias.decl)

    of cekObject:
      entry.cxxObject.decl.typeImport = some lib
      store.addDecl(entry.cxxObject)

      for nest in mitems(entry.cxxObject.nested):
        registerDeclarations(nest, store, lib)

func postprocessTypeUses*(
    entry: var CxxEntry, store: var CxxTypeStore, lib: CxxLibImport) =

  func aux(use: var CxxTypeUse, store: CxxTypeStore)

  func aux(use: var CxxArg, store: CxxTypeStore) =
    aux(use.nimType, store)

  func aux(use: var CxxTypeUse, store: CxxTypeStore) =
    eachKind(use, {ctkIdent}) do (use: var CxxTypeUse):
      if not use.cxxType.isParam:
        for typ in mitems(use.types):
          typ.cxxType.typeStore = store
          typ.cxxType.typeLib = some lib.library

        let decl = use.getDecl()

        if decl.isSome():
          use.flags.incl():
            case decl.get().kind:
              of ctdkEnum: ctfIsEnumType
              of ctdkStruct: ctfIsStructType
              of ctdkClass: ctfIsClassType
              of ctdkUnion: ctfIsUnionType
              of ctdkTypedef: ctfIsTypedefType
              of ctdkProc, ctdkNone: ctfNone

        else:
          use.flags.incl(ctfIsPodType)

  func aux(decl: var CxxProc, store: var CxxTypeStore) =
    for arg in mitems(decl.arguments):
      aux(arg, store)

    aux(decl.returnType, store)

  case entry.kind:
    of cekProc:
      aux(entry.cxxProc, store)

    of cekPass, cekEmpty, cekImport,
       cekMacroGroup, cekMacro, cekComment:
      discard

    of cekTypeGroup:
      for decl in mitems(entry.cxxTypes):
        postprocessTypeUses(entry, store, lib)

    of cekEnum:
      discard

    of cekForward:
      discard

    of cekAlias:
      aux(entry.cxxAlias.baseType, store)

    of cekObject:
      for meth in mitems(entry.cxxObject.methods):
        aux(meth, store)

      for field in mitems(entry.cxxObject.mfields):
        aux(field.nimType, store)

proc fixIdentsRec*(
    entry: var CxxEntry,
    cache: var StringNameCache,
    conf: CxxFixConf,
    entryIdx: int
  ) =
  ## Fix wrapped entry identifier names, optionally constructing them using
  ## `entryIdx` argument - necessary for completely anonymous enums that
  ## don't have any user declarations inside, and used for field grouping.

  assertRefFields(conf)
  var context: CxxNameFixContext
  context[cancLibName] = some cxxPair(conf.libName, cxxName(conf.libName))
  # pprint entry
  proc aux(
      name: var CxxNamePair, cache: var StringNameCache, entryIdx: int = -1) =
    name.nim = conf.fixName(name, cache, context)

  proc aux(decl: var CxxTypeDecl, cache: var StringNameCache) =
    aux(decl.name, cache, entryIdx)

    for param in mitems(decl.genParams):
      aux(param.name, cache)

  proc aux(obj: var CxxObject, cache: var StringNameCache)
  proc aux(enu: var CxxEnum, cache: var StringNameCache)

  proc aux(use: var CxxTypeUse, cache: var StringNameCache) =
    var cache {.byaddr.} = cache
    eachKind(use, {ctkIdent}) do (use: var CxxTypeUse):
      for typ in mitems(use.types):
        aux(typ.cxxType.name, cache)

    proc auxArg(use: var CxxTypeUse, cache: var StringNameCache) =
      case use.kind:
        of ctkWrapKinds: auxArg(use.wrapped, cache)
        of ctkStaticParam:
          if use.value.kind == cekVar:
            aux(use.value.ident, cache, 0)

        of ctkPod, ctkDecltype: discard
        of ctkAnonEnum:
          if ?use.enumParent:
            aux(use.enumParent.get(), cache)

          if ?use.enumUser:
            aux(use.enumUser.get(), cache)

          aux(use.enumDef, cache)

        of ctkArrayKinds: auxArg(use.arrayElement, cache)
        of ctkIdent:
          for typ in mitems(use.types):
            for param in mitems(typ.genParams):
              auxArg(param, cache)

        of ctkAnonObject:
          if ?use.objParent:
            aux(use.objParent.get(), cache)

          if ?use.objUser:
            aux(use.objUser.get(), cache)

          var cxx = CxxName()

          if ?use.objParent:
            cxx = cxx & use.objParent.get().cxx

          if ?use.objUser:
            cxx = cxx & use.objUser.get().cxx

          use.objDef.name = cxxPair("", cxx & cxxName("Type"))

          aux(use.objDef, cache)

        of ctkProc:
          for idx, arg in mpairs(use.arguments):
            if isEmpty(arg.cxxName()):
              arg.name = cxxPair("arg" & $idx)


            aux(arg.name, cache)
            auxArg(arg.nimType, cache)

          auxArg(use.returnType, cache)

    auxArg(use, cache)

  proc aux(decl: var CxxProc, cache: var StringNameCache) =
    if decl.isConstructor():
      aux(decl.constructorOf.get(), cache)

    for idx, arg in mpairs(decl.arguments):
      if isEmpty(arg.cxxName()):
        arg.name = cxxPair("arg" & $idx)

      aux(arg.name, cache)
      aux(arg.nimType, cache)

    if ?decl.arguments:
      context[cancFirstArgName] = some decl.arguments[0].name
      if decl.arguments[0].nimType.kind == ctkIdent:
        context[cancFirstArgName] = some decl.arguments[0].nimType.cxxType.name

    if decl of cpkOperatorKinds:
      assert decl.head.name.nim.len > 0,
               "Missing name for operator proc."

    else:
      aux(decl.head, cache)

    aux(decl.returnType, cache)

    context[cancFirstArgName].clear()
    context[cancFirstArgType].clear()


  proc aux(entry: var CxxEntry, cache: var StringNameCache)
  proc aux(obj: var CxxObject, cache: var StringNameCache) =
    assertRef obj
    aux(obj.decl, cache)
    context[cancParentObjectName] = some obj.decl.name
    for idx, field in mpairs(obj.mfields):
      if ?field.name:
        aux(field.name.get(), cache, idx)

      else:
        let name = "field" & $idx
        let pair = cxxPair(name, cxxName(name), cncField)
        field.name = some pair
        if field.nimType of ctkAnonObject:
          field.nimType.objUser = some pair

        if field.nimType of ctkAnonEnum:
          field.nimType.enumUser = some pair

      aux(field.nimType, cache)
      echov field

    for mproc in mitems(obj.methods):
      aux(mproc, cache)

    for nestd in mitems(obj.nested):
      aux(nestd, cache)

    context[cancParentObjectName].clear()


  proc aux(enu: var CxxEnum, cache: var StringNameCache) =
    assertRef enu
    aux(enu.decl.name, cache)
    context[cancParentEnumName] = some enu.decl.name
    for value in mitems(enu.values):
      aux(value.name, cache)

    context[cancParentEnumName].clear()


  proc aux(entry: var CxxEntry, cache: var StringNameCache) =

    case entry.kind:
      of cekEnum:
        aux(entry.cxxEnum, cache)

      of cekForward:
        aux(entry.cxxForward.decl.name, cache)

      of cekObject:
        aux(entry.cxxObject, cache)

      of cekProc:
        aux(entry.cxxProc, cache)

      of cekAlias:
        aux(entry.cxxAlias.baseType, cache)
        aux(entry.cxxAlias.decl, cache)

      of cekMacro:
        aux(entry.cxxMacro.name, cache)

      else:
        raise newImplementKindError(entry)


  aux(entry, cache)

proc fragmentType*(entry: var CxxEntry):
  tuple[newDecl: seq[CxxEntry], extras: seq[CxxEntry]] =

  case entry.kind:
    of cekAlias, cekEnum:
      result.newDecl.add entry
      entry = cxxEmpty()

    of cekObject:
      for e in entry.cxxObject.methods:
        result.extras.add e

      entry.cxxObject.methods = @[]

      for nested in mitems(entry.cxxObject.nested):
        if nested.kind in { cekEnum, cekObject, cekAlias }:
          let (newDecls, extras) = fragmentType(nested)
          result.newdecl.add newDecls
          result.extras.add extras

        else:
          result.extras.add nested

      entry.cxxObject.nested = @[]
      result.newDecl.add entry
      entry = cxxEmpty()

    else:
      discard


proc add*(store: var CxxTypeStore, other: CxxTypeStore) =
  assertRef(store)
  for name, types in other.forwardDecls:
    if name notin store.typeDecls:
      for dtype in types:
        store.addForwardDecl(dtype)

  for name, types in other.typeDecls:
    if name in store.typeDecls:
      # REVIEW ideally I need to structurally compared wrapped types, and
      # if they are identical, I can ignore duplication, otherwise raised
      # more detailed error message that describes differences in two type
      # definitions. This error cannot be reliably handled on any of the
      # internal hcparse levels as it would most likely be caused by
      # different macro expansion context that was accumulated during
      # various file inclusion paths. Alternatively this might mean
      # insufficient file cutout heuristics (for example things expanded
      # with :sh:`gcc -e`, but hcparse heuristics for `#line` detection is
      # bugged).
      raise newArgumentError(
        "Type ", name, " has been declared in both source and target ",
        "type stores as a fully specified entry."
      )

proc reuseStore*(
    entry: var CxxEntry,
    store: var CxxTypeStore,
    other: CxxTypeStore = nil
  ) =
  # TODO DOC

  let other = if isNil(other): entry.getTypeStore() else: other

  store.add(other)
  entry.setStoreRec(store)



proc postFixEntries*(
    conf: CxxFixConf,
    entries: var seq[CxxEntry],
    lib: CxxLibImport = default CxxLibImport,
    file: Option[AbsFile] = none AbsFile
  ) =
  assertRefFields(conf)

  var cache: StringNameCache

  var store = conf.typeStore

  # Fix all identifier names in entry lists
  for idx, item in mpairs(entries):
    fixIdentsRec(item, cache, conf, idx)

  # Set spelling location file for all entries in the list
  if file.isSome():
    for item in mitems(entries):
      setFileRec(item, file.get())

  for item in mitems(entries):
    item.setStoreRec(store)
    registerDeclarations(item, store, lib)

  # Register type declarations in the store, add missing flags to type uses
  for item in mitems(entries):
    postprocessTypeUses(item, store, lib)


  # Set header for list of entries
  for item in mitems(entries):
    setHeaderRec(item, conf)


proc postFixEntries*(
    entries: sink seq[CxxEntry],
    conf: CxxFixConf,
    lib: CxxLibImport = default CxxLibImport,
    file: Option[AbsFile] = none AbsFile
  ): seq[CxxEntry] =
  result = entries
  conf.postFixEntries(result, lib, file)

proc postFixEntries*(
    typ: CxxTypeUse,
    conf: CxxFixConf,
    lib: CxxLibImport = default(CxxLibImport),
    file: Option[AbsFile] = none AbsFile
  ): CxxTypeUse =
  ## Post-fix all nested anonymous type declarations in the type use, and
  ## return reconstructed type use.

  result = typ
  var arg: seq[CxxEntry]
  case typ.kind:
    of ctkAnonObject:
      arg.add typ.objDef
      result.objDef = postFixEntries(arg, conf, lib, file)[0].cxxObject

    of ctkAnonEnum:
      arg.add typ.enumDef
      result.enumDef = postFixEntries(arg, conf, lib, file)[0].cxxEnum

    else:
      discard



iterator mentries*(files: var seq[CxxFile]): var CxxEntry =
  for file in mitems(files):
    for entry in mitems(file.entries):
      yield entry

proc postFixEntries*(conf: CxxFixConf, files: var seq[CxxFile]) =
  assertRefFields(conf)
  var store = conf.typeStore

  # Fix all identifier names in entry lists
  for file in mitems(files):
    var cache: StringNameCache
    for idx, item in mpairs(file.entries):
      fixIdentsRec(item, cache, conf, idx)

  # Set spelling location file for all entries in the list
  for file in mitems(files):
    for item in mitems(file.entries):
      setFileRec(item, file.original)

  for file in mitems(files):
    for item in mitems(file.entries):
      item.setStoreRec(store)
      registerDeclarations(item, store, file.savePath)

  # Register type declarations in the store, add missing flags to type uses
  for file in mitems(files):
    for item in mitems(file.entries):
      postprocessTypeUses(item, store, file.savEpath)

  # Set header for list of entries
  for item in mentries(files):
    setHeaderRec(item, conf)


proc postFixEntries*(
  files: sink seq[CxxFile], conf: CxxFixConf): seq[CxxFile] =

  result = files
  conf.postFixEntries(result)

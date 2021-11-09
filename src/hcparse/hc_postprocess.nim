import
  ./interop_ir/wrap_store

import
  std/[tables, sets]

import
  hmisc/core/[all],
  hmisc/algo/[namegen]

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
        registerDeclarations(entry, store, lib)

func postprocessTypeUses*(
    entry: var CxxEntry, store: var CxxTypeStore, lib: CxxLibImport) =

  func aux(use: var CxxTypeUse, store: CxxTypeStore)

  func aux(use: var CxxArg, store: CxxTypeStore) =
    aux(use.nimType, store)

  func aux(use: var CxxTypeUse, store: CxxTypeStore) =
    eachIdent(use) do (use: var CxxTypeUse):
      if not use.cxxType.isParam:
        use.cxxType.typeStore = store
        use.cxxType.typeLib = some lib.library

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
    conf: CxxFixConf
  ) =

  var context: CxxNameFixContext
  context[cancLibName] = some cxxPair(conf.libName, cxxName(conf.libName))
  template aux(name: var CxxNamePair): untyped =
    # let l = "LIBSSH2_LISTENER" in $name
    # if l:
    #   pprintStackTrace()
    #   echov "----"
    #   echov name
    name.nim = conf.fixName(name, cache, context)
    # if l: echov name

  proc aux(decl: var CxxTypeDecl, cache: var StringNameCache) =
    aux(decl.name)

  proc aux(use: var CxxTypeUse, cache: var StringNameCache) =
    var cache {.byaddr1.} = cache
    eachIdent(use) do (use: var CxxTypeUse):
      aux(use.cxxType.name)

    proc auxArg(use: var CxxTypeUse, cache: var StringNameCache) =
      case use.kind:
        of ctkWrapKinds: auxArg(use.wrapped, cache)
        of ctkStaticParam: discard
        of ctkAnonEnum:
          aux(use.enumParent)
          aux(use.enumUser)

        of ctkArrayKinds: auxArg(use.arrayElement, cache)
        of ctkIdent:
          for param in mitems(use.genParams):
            auxArg(param, cache)

        of ctkAnonObject:
          aux(use.objParent)
          aux(use.objUser)

        of ctkProc:
          for idx, arg in mpairs(use.arguments):
            if isEmpty(arg.cxxName()):
              arg.name = cxxPair("arg" & $idx)


            aux(arg.name)
            auxArg(arg.nimType, cache)

          auxArg(use.returnType, cache)

    auxArg(use, cache)

  proc aux(decl: var CxxProc, cache: var StringNameCache) =
    if decl.isConstructor():
      aux(decl.constructorOf.get())

    for idx, arg in mpairs(decl.arguments):
      if isEmpty(arg.cxxName()):
        arg.name = cxxPair("arg" & $idx)

      aux(arg.name)
      aux(arg.nimType, cache)

    if ?decl.arguments:
      context[cancFirstArgName] = some decl.arguments[0].name
      if decl.arguments[0].nimType.kind == ctkIdent:
        context[cancFirstArgName] = some decl.arguments[0].nimType.cxxType.name

    aux(decl.head.name)
    aux(decl.returnType, cache)

    context[cancFirstArgName].clear()
    context[cancFirstArgType].clear()

  proc aux(
      entry: var CxxEntry,
      cache: var StringNameCache
    ) =

    case entry.kind:
      of cekEnum:
        aux(entry.cxxEnum.decl.name)
        context[cancParentEnumName] = some entry.cxxEnum.decl.name
        for value in mitems(entry.cxxEnum.values):
          aux(value.name)

        context[cancParentEnumName].clear()

      of cekForward:
        aux(entry.cxxForward.decl.name)

      of cekObject:
        aux(entry.cxxObject.decl.name)
        context[cancParentObjectName] = some entry.cxxObject.decl.name
        for field in mitems(entry.cxxObject.mfields):
          aux(field.name)
          aux(field.nimType, cache)

        for mproc in mitems(entry.cxxObject.methods):
          aux(mproc, cache)

        for nestd in mitems(entry.cxxObject.nested):
          aux(nestd, cache)

        context[cancParentObjectName].clear()

      of cekProc:
        aux(entry.cxxProc, cache)

      of cekAlias:
        aux(entry.cxxAlias.baseType, cache)
        aux(entry.cxxAlias.decl, cache)

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

  let other = if isNil(other): entry.getTypeStore() else: other

  store.add(other)
  entry.setStoreRec(store)

import
  hmisc/other/oswrap,
  hmisc/core/all,
  hmisc/algo/namegen,
  std/[options, macros, json, strutils, strformat, parseutils,
       tables, hashes, sets, sequtils]

import
  ./wrap_icpp

type
  CxxTypeKind* = enum
    ## Kind of the wrapped Cxx type
    ctkIdent ## Identifier with optional list of template parameters
    ctkProc ## Procedural (callback) type
    ctkPtr
    ctkLVref
    ctkRVref
    ctkFixedArray
    ctkDependentArray
    ctkDynamicArray

    ctkStaticParam

const
  ctkWrapKinds* = { ctkPtr, ctkLVRef, ctkRVRef, ctkDynamicArray }
  ctkArrayKinds* = { ctkFixedArray, ctkDependentArray }



type
  CxxSpellingLocation* = object
    file*: AbsFile
    line*, column*: int


  CxxHeaderKind* = enum
    ## Kind of the nim input header
    chkGlobal ## Global header file, must be installed and accessible via
    ## `includepath` when wrappers are compiled
    chkAbsolute ## Absolute path to the base header file
    chkPNode ## Unconstrained PNode - can be anything

  CxxLibImport* = object
    library*: string
    importPath*: seq[string]

  CxxHeader* = object
    case kind*: CxxHeaderKind
      of chkGlobal:
        global*: string

      of chkAbsolute:
        file*: AbsFile

      of chkPNode:
        other*: string

  CxxBase* = object of RootObj
    iinfo*: LineInfo
    spellingLocation*: Option[CxxSpellingLocation]
    # nimName*: string
    # cxxName*: seq[string]
    icpp*: IcppPattern
    private*: bool
    header*: Option[CxxHeader]
    docComment*: Option[string]
    haxdocIdent* #[ {.requiresinit.} ]#: JsonNode

    isAnonymous*: bool
    isPromotedForward*: bool

  CxxName* = object
    scopes*: seq[string]

  CxxNamePair* = object
    nim*: string
    cxx*: CxxName

  CxxIdent* = object
    name*: CxxNamePair
    genParams*: seq[CxxTypeUse]

  CxxGenParams* = seq[tuple[name: CxxNamePair, default: Option[CxxTypeUse]]]

  CxxTypeDecl* = object
    isForward*: bool
    name*: CxxNamePair
    typeImport*: Option[CxxLibImport]
    genParams*: CxxGenParams

    store*: CxxTypeStore

  CxxTypeStore* = ref object
    ## Lookup table for type and object declarations. Each lookup contains
    ## `seq` of definitions for each `CxxName` to handle potentiall name
    ## duplicates.

    typeDecls*: Table[CxxName, seq[CxxTypeDecl]]
    classDecls*: Table[CxxName, seq[CxxObject]]


  CxxTypeRef* = object
    ## Reference to used C++ type
    name*: CxxNamePair
    case isParam*: bool
      of true:
        discard

      of false:
        typeLib*: Option[string]
        typeStore*: CxxTypeStore

  CxxTypeFlag* = enum
    ctfConst
    ctfMutable
    ctfComplex
    ctfParam
    ctfDefaultedParam
    ctfIsPodType

  CxxTypeUse* = ref object
    ## Instantiated type
    flags*: set[CxxTypeFlag]
    case kind*: CxxTypeKind
      of ctkStaticParam:
        value*: CxxExpr

      of ctkWrapKinds:
        wrapped*: CxxTypeUse

      of ctkArrayKinds:
        arraySize*: CxxTypeUse
        arrayElement*: CxxTypeUse

      of ctkIdent:
        cxxType*: CxxTypeRef
        genParams*: seq[CxxTypeUse]

      of ctkProc:
        arguments*: seq[CxxArg]
        returnType*: CxxTypeUse


  CxxProcKind* = enum
    ## Procedure kind
    cpkRegular ## Regular proc: `hello()`
    cpkConstructor
    cpkDestructor

    cpkPrefixOp ## Prefix operator `@a`
    cpkPostfixOp ## Postfix operator `a@`
    cpkInfixOP ## Infix operator `a @ b`
    cpkAsgnOp ## Assign operator `a += b`
    cpkCopyAsgnOp ## Copy assignment operator `a = b`
    cpkArrayOp ## Array access operator `a[b]`
    cpkArrowOp ## Arrow operator `a->`
    cpkCallOp ## Call operator `a()`
    cpkDerefOp ## Prefix dereference operator
    cpkCommaOp ## Comma operator
    cpkConvertOp ## User-defined conversion operator
    cpkUserLitOp ## User-defined literal operators
    cpkNewOp ## `new` operator
    cpkDeleteOp ## `delete` operator
    # cpk

    # cpkHook ## Destructor/sink (etc.) hook: `=destroy`
    # cpkAssgn ## Assignment proc `field=`

  CxxProcFlag* = enum
    cpfConst
    cpfOperator
    cpfOverride
    cpfExportc
    cpfSlot
    cpfSignal
    cpfVirtual
    cpfVariadic

  CxxProc* = object of CxxBase
    kind*: CxxProcKind
    head*: CxxTypeDecl ## Reuse type declaration for procedure - it has
    ## very similar structure (nim/cxx name, generic parameters with
    ## optional defaults). Missing elements are added as regular fields.

    arguments*: seq[CxxArg]
    returnType*: CxxTypeUse


    flags*: set[CxxProcFlag]

    constructorOf*: Option[CxxTypeUse]
    methodOf*: Option[CxxTypeUse]

  CxxExprKind = enum
    cekIntLit
    cekStrLit
    cekCall

  CxxExpr = object
    case kind*: CxxExprKind
      of cekIntLit:
        intVal*: int

      of cekStrLit:
        strVal*: string

      of cekCall:
        ident*: CxxNamePair

  CxxArg* = object of CxxBase
    name*: CxxNamePair
    nimType*: CxxTypeUse
    default*: Option[CxxExpr]

  CxxFieldFlag* = enum
    cffStatic
    cffPublic
    cffPrivate
    cffProtected

  CxxField* = object of CxxBase
    name*: CxxNamePair
    nimType*: CxxTypeUse
    flags*: set[CxxFieldFlag]


  CxxAlias* = object of CxxBase
    isDistinct*: bool
    decl*: CxxTypeDecl
    baseType*: CxxTypeUse


  CxxEnumValue* = object
    name*: CxxNamePair
    value*: BiggestInt
    valueTokens*: seq[string]
    comment*: string

  CxxEnum* = object of CxxBase
    isClassEnum*: bool
    decl*: CxxTypeDecl
    values*: seq[CxxEnumValue]
    duplicates*: seq[CxxEnumValue]


  CxxObjectKind* = enum
    cokUnion
    cokStruct
    cokClass

  CxxObject* = ref object of CxxBase
    decl*: CxxTypeDecl
    kind*: CxxObjectKind

    super*: seq[CxxTypeUse]
    nested*: seq[CxxEntry]

    isByref*: bool

    mfields*: seq[CxxField]
    methods*: seq[CxxProc]

  CxxForward* = object of CxxBase
    decl*: CxxTypeDecl

  CxxMacro* = object of CxxBase
    name*: CxxNamePair
    arguments*: seq[string]

  CxxMacroGroup* = object
    macros*: seq[CxxMacro]

  CxxEntryKind* = enum
    cekEnum ## Enum wrapper
    cekProc ## Method, operator, or function
    cekObject ## Struct, union, or class
    cekAlias ## `typedef` or `using`
    cekPass ## Raw passthrough

    cekForward ## Forward declaration for struct/union/class/enum
    cekImport ## Import statement
    cekEmpty
    cekTypeGroup
    cekMacroGroup

    cekMacro
    cekComment

  CxxEntry* = ref object
    case kind*: CxxEntryKind
      of cekEnum:
        cxxEnum*: CxxEnum

      of cekProc:
        cxxProc*: CxxProc

      of cekTypeGroup:
        cxxTypes*: seq[CxxEntry]

      of cekObject:
        cxxObject*: CxxObject

      of cekAlias:
        cxxAlias*: CxxAlias

      of cekForward:
        cxxForward*: CxxForward

      of cekComment:
        cxxComment*: string

      of cekMacro:
        cxxMacro*: CxxMacro

      of cekMacroGroup:
        cxxMacroGroup*: CxxMacroGroup

      of cekPass, cekEmpty,
         cekImport #[ REVIEW maybe make import and entry? ]#:
        discard

  CxxFile* = object
    imports*: HashSet[CxxLibImport]
    exports*: HashSet[CxxLibImport]

    entries*: seq[CxxEntry]

    isGenerated*: bool
    savePath*: CxxLibImport


func `$`*(cxx: CxxLibImport): string =
  cxx.library & "@" & cxx.importPath.join("/")

func cxxStr*(name: CxxName): string = name.scopes.join("::")
func `$`*(name: CxxName): string = name.cxxStr()
func `$`*(name: CxxNamePair): string = $name.cxx
func `$`*(expr: CxxExpr): string = raise newImplementError()

func `$`*(tref: CxxTypeRef): string =
  if not tref.isParam:
    if tref.typeLib.isSome():
      result.add tref.typeLib.get()
      result.add "@"

  result.add $tref.name


func `$`*(ct: CxxTypeUse): string

func `$`*(arg: CxxArg): string =
  result = $arg.name & ": " & $arg.nimType
  if arg.default.isSome():
    result &= " = "
    result &= $arg.default.get()

func `$`*(ct: CxxTypeUse): string =
  if isNil(ct):
    result = "void"

  else:
    case ct.kind:
      of ctkPtr:
        result = $ct.wrapped & "*"

      of ctkLVRef:
        result = $ct.wrapped & "&"

      of ctkRVRef:
        result = $ct.wrapped & "&&"

      of ctkDynamicArray:
        result = $ct.wrapped & "[]"

      of ctkFixedArray, ctkDependentArray:
        result = $ct.arrayElement & "[" & $ct.arraySize & "]"

      of ctkStaticParam:
        result = $ct.value

      of ctkIdent:
        result &= $ct.cxxType
        if ct.genParams.len > 0:
          result &= "["
          result &= ct.genParams.mapIt($it).join(", ")
          result &= "]"

      of ctkProc:
        result &= "proc("
        for idx, arg in ct.arguments:
          if idx > 0:
            result.add ", "
          result &= $arg

        result &= "): "
        result &= $ct.returnType

func hash*(name: CxxName): Hash = hash(name.scopes)

func hash*[T](opt: Option[T]): Hash =
  if opt.isSome():
    return hash(opt.get())

func hash*(pair: CxxNamePair): Hash =
  !$(hash(pair.cxx) !& hash(pair.nim))

func hash*(tref: CxxTypeRef): Hash  =
  result = hash(tref.name)
  if not tref.isParam:
    result = result !& hash(tref.typeLib)


func hash*(use: CxxTypeUse): Hash

func hash*(lib: CxxLibImport): Hash = hash(lib.library) !& hash(lib.importPath)

func hash*(arg: CxxArg): Hash = hash(arg.name) !& hash(arg.nimType)

func hash*(use: CxxTypeUse): Hash =
  result = hash(use.kind) !& hash(use.flags)
  case use.kind:
    of ctkStaticParam:
      raise newImplementKindError(use)

    of ctkWrapKinds:
      result = result !& hash(use.wrapped)

    of ctkArrayKinds:
      result = result !& hash(use.arraySize) !& hash(use.arrayElement)

    of ctkIdent:
      result = result !& hash(use.cxxType) !& hash(use.genParams)

    of ctkProc:
      result = result !& hash(use.arguments)
      if use.returnType.notNil():
        result = result !& hash(use.returnType)

func hash*(decl: CxxTypeDecl): Hash =
  !$(hash(decl.name) !& hash(decl.genParams) !& hash(decl.typeImport))

func `nimName=`*(pr: var CxxProc, name: string) =
  pr.head.name.nim = name


func nimName*(pr: CxxProc): string = pr.head.name.nim
func nimName*(arg: CxxArg): string = arg.name.nim
func nimName*(t: CxxTypeUse): string = t.cxxType.name.nim
func nimName*(obj: CxxObject): string = obj.decl.name.nim
func nimName*(field: CxxField): string = field.name.nim

func cxxName*(field: CxxField): CxxName = field.name.cxx
func cxxName*(t: CxxTypeUse): CxxName = t.cxxType.name.cxx
func cxxName*(t: CxxTypeDecl): CxxName = t.name.cxx
func cxxName*(pr: CxxProc): CxxName = pr.head.name.cxx
func cxxName*(obj: CxxObject): CxxName = obj.decl.name.cxx
func cxxName*(name: string): CxxName = CxxName(scopes: @[name])
func cxxName*(scopes: seq[string]): CxxName = CxxName(scopes: scopes)

func cxxPair*(nim: string, cxx: CxxName): CxxNamePair =
  CxxNamePair(nim: nim, cxx: cxx)

func cxxPair*(name: string): CxxNamePair = cxxPair(name, cxxName(@[name]))

func isConst*(pr: CxxProc): bool = cpfConst in pr.flags
func isConstructor*(pr: CxxProc): bool = pr.constructorOf.isSome()
func isMethod*(pr: CxxProc): bool = pr.methodOf.isSome()


func isPOD*(use: CxxTypeUse): bool = ctfIsPodType in use.flags

func add*(t: var CxxTypeUse, other: CxxTypeUse) =
  t.genParams.add other
  t.genParams.last().flags.incl ctfParam

func getConstructed*(pr: CxxProc): CxxTypeUse =
  pr.constructorOf.get()

func getIcppName*(pr: CxxProc, asMethod: bool = false): string =
  if asMethod:
    pr.cxxName.scopes[^1]

  else:
    pr.cxxName.scopes.join("::")

func cxxHeader*(global: string): CxxHeader =
  CxxHeader(global: global, kind: chkGlobal)

func cxxHeader*(file: AbsFile): CxxHeader =
  CxxHeader(kind: chkAbsolute, file: file)

func cxxArg*(name: CxxNamePair, argType: CxxTypeUse): CxxArg =
  CxxArg(nimType: argType, name: name, haxdocIdent: newJNull())

func wrapArray*(size, element: CxxTypeUse): CxxTypeUse =
  if size.kind == ctkStaticParam:
    result = CxxTypeUse(kind: ctkFixedArray)

  else:
    result = CxxTypeUse(kind: ctkDependentArray)

  result.arraySize = size
  result.arrayElement = element


func wrap*(wrapped: CxxTypeUse, kind: CxxTypeKind): CxxTypeUse =
  if kind == ctkIdent:
    result = wrapped

  else:
    result = CxxTypeUse(kind: kind)
    result.wrapped = wrapped

func cxxTypeRef*(
    name: CxxNamePair, store: CxxTypeStore = nil): CxxTypeRef =
  CxxTypeRef(isParam: false, name: name, typeStore: store)

func cxxTypeDecl*(
    head: CxxNamePair, genParams: CxxGenParams = @[]): CxxTypeDecl =
  CxxTypeDecl(name: head, genParams: genParams)

func cxxTypeUse*(
    head: CxxNamePair,
    genParams: seq[CxxTypeUse] = @[],
    store: CxxTypeStore = nil
  ): CxxTypeUse =

  CxxTypeUse(
    kind: ctkIdent, cxxType: cxxTypeRef(head, store), genParams: @genParams)

func toDecl*(use: CxxTypeUse): CxxTypeDecl =
  raise newImplementError()

func addDecl*(store: var CxxTypeStore, decl: CxxTypeDecl) =
  store.typeDecls.mgetOrPut(decl.name.cxx, @[]).add decl

func getDecl*(use: CxxTypeUse): Option[CxxTypeDecl] =
  ## Get first type declaration with matching cxx name. In case of multiple
  ## identical types present they are disambiguated based on the library
  ## name if possible.
  assertKind(use, {ctkIdent})
  assert not use.cxxType.isParam
  assertRef use.cxxType.typeStore
  if use.cxxName() in use.cxxType.typeStore.typeDecls:
    for decl in use.cxxType.typeStore.typeDecls[use.cxxName()]:
      # No library for import, or no library for the type declaration
      if use.cxxType.typeLib.isNone() or decl.typeImport.isNone():
        return some decl

      else:
        # Has library name for both type declaration and use
        if decl.typeImport.get().library == use.cxxType.typeLib.get():
          return some decl

func hasImport*(use: CxxTypeUse): bool =
  if use of ctkIdent:
    let decl = use.getDecl()
    result = decl.isSome() and decl.get().typeImport.isSome()

  else:
    result = false

func getImport*(use: CxxTypeUse): CxxLibImport =
  use.getDecl().getOr("cannot determine declaration for type use").
    typeImport.getOr("missing import for type declarations")

func hasImport*(decl: CxxTypeDecl): bool =
  decl.typeImport.isSome()

func cxxLibImport*(library: string, path: seq[string]): CxxLibImport =
  CxxLibImport(library: library, importPath: path)

func getImport*(decl: CxxTypeDecl): CxxLibImport = decl.typeImport.get()
func getLibrary*(imp: CxxLibImport): string = imp.library
func getLibrary*(file: CxxFile): string = file.savePath.library
func getFilename*(limport: CxxLibImport): string =
  result = limport.importPath[^1]
  let idx = result.find('.')
  if idx != -1:
    result = result[0 ..< idx]

func getFilename*(file: CxxFile): string = file.savePath.getFilename()

func getType*(arg: CxxArg): CxxTypeUse = arg.nimType
func getType*(field: CxxField): CxxTypeUse = field.nimType

template eachIdentAux(inUse, cb, iterateWith: untyped) =
  case inUse.kind:
    of ctkWrapKinds: eachIdent(inUse.wrapped, cb)
    of ctkStaticParam: discard
    of ctkArrayKinds: eachIdent(inUse.arrayElement, cb)
    of ctkIdent:
      cb(inUse)
      for param in iterateWith(inUse.genParams):
        eachIdent(param, cb)

    of ctkProc:
      for arg in iterateWith(inUse.arguments):
        eachIdent(arg.nimType, cb)

      eachIdent(inUse.returnType, cb)

proc eachIdent*(use: var CxxTypeUse, cb: proc(ident: var CxxTypeUse)) =
  if isNil(use): return
  eachIdentAux(use, cb, mitems)

proc eachIdent*(use: CxxTypeUse, cb: proc(ident: CxxTypeUse)) =
  if isNil(use): return
  eachIdentAux(use, cb, items)

proc getUsedTypesRec*(
    t: CxxTypeUse, ignoreHead: bool = false): seq[CxxTypeUse] =
  var res: seq[CxxTypeUse]
  if not ignoreHead:
    res.add t

  eachIdent(t) do(t: CxxTypeUse):
    if notNil(t): res.add t
    res.add getUsedTypesRec(t, ignoreHead = false)

  return res

func getReturn*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): CxxTypeUse =

  if pr.isConstructor:
    result = pr.getConstructed().wrap(onConstructor)

  else:
    assertRef pr.returnType
    result = pr.returnType

func getIcpp*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): IcppPattern =

  if pr.icpp.len > 0:
    return pr.icpp

  else:
    if pr.isConstructor:
      case onConstructor:
        of ctkIdent: result.standaloneProc(pr.getIcppName())
        of ctkPtr: result.standaloneProc("new " & pr.getIcppName())
        else: raise newUnexpectedKindError(onConstructor)

    else:
      if pr.isMethod:
        result.dotMethod(pr.getIcppName())

      else:
        result.standaloneProc(pr.getIcppName())

func getIcppStr*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): string =
  $getIcpp(pr, onConstructor)


func getIcpp*(pr: CxxObject): IcppPattern =
  if pr.icpp.len > 0:
    return pr.icpp

  else:
    result.ctype(pr.cxxName.scopes.join("::"))


func getIcppStr*(pr: CxxObject): string = $getIcpp(pr)

func initIcpp*(
    pr: var CxxProc, onConstructor: CxxTypeKind = ctkIdent) =

  pr.icpp = getIcpp(pr, onConstructor)


func cxxTypeUse*(
    arguments: seq[CxxArg], returnType: CxxTypeUse): CxxTypeUse =
  CxxTypeUse(
    kind: ctkProc, arguments: arguments, returnType: returnType)

func cxxTypeUse*(decl: CxxTypeDecl, store: CxxTypeStore = nil): CxxTypeUse =
  CxxTypeUse(kind: ctkIdent, cxxType: cxxTypeRef(decl.name, store))

func cxxTypeUse*(name: string, args: seq[CxxTypeUse]): CxxTypeUse =
  cxxTypeUse(cxxPair(name), args)

func cxxObject*(name: CxxNamePair, genParams: CxxGenParams = @[]): CxxObject =
  CxxObject(decl: cxxTypeDecl(name, genParams), haxdocIdent: newJNull())

func cxxForward*(name: CxxNamePair): CxxForward =
  CxxForward(decl: cxxTypeDecl(name, @[]), haxdocIdent: newJNull())

func cxxEnum*(name: CxxNamePair): CxxEnum =
  CxxEnum(decl: cxxTypeDecl(name), haxdocIdent: newJNull())

func cxxField*(name: CxxNamePair, nimType: CxxTypeUse): CxxField =
  CxxField(name: name, nimType: nimType, haxdocIdent: newJNull())

func cxxAlias*(decl: CxxTypeDecl, baseType: CxxTypeUse): CxxAlias =
  CxxAlias(decl: decl, baseType: baseType, haxdocIdent: newJNull())

func cxxEmpty*(): CxxEntry = CxxEntry(kind: cekEmpty)

func cxxProc*(
    name: CxxNamePair,
    arguments: seq[CxxArg] = @[],
    returnType: CxxTypeUse = cxxTypeUse(cxxPair"void"),
    genParams: CxxGenParams = @[]
  ): CxxProc =

  CxxProc(
    head: cxxTypeDecl(name, genParams),
    haxdocIdent: newJNull(),
    returnType: returnType,
    arguments: arguments
  )

func cxxMacro*(name: CxxNamePair): CxxMacro =
  CxxMacro(name: name, haxdocIdent: newJNull())

func cxxFile*(entries: seq[CxxEntry], path: CxxLibImport): CxxFile =
  CxxFile(savePath: path, entries: entries)

func add*(pr: var CxxProc, arg: CxxArg) =
  pr.arguments.add arg

func setHeaderRec*(entry: var CxxEntry, header: CxxHeader) =
  case entry.kind:
    of cekPass, cekForward, cekImport, cekEmpty, cekComment:
      discard

    of cekTypeGroup, cekMacroGroup, cekMacro:
      raise newImplementKindError(entry)

    of cekEnum: entry.cxxEnum.header = some header
    of cekProc: entry.cxxProc.header = some header
    of cekAlias: entry.cxxAlias.header = some header
    of cekObject:
      entry.cxxObject.header = some header
      for meth in mitems(entry.cxxObject.methods):
        meth.header = some header

      for nest in mitems(entry.cxxObject.nested):
        setHeaderRec(nest, header)

func setTypeStoreRec*(
    entry: var CxxEntry, store: var CxxTypeStore, lib: CxxLibImport) =

  func aux(use: var CxxTypeUse, store: CxxTypeStore)

  func aux(use: var CxxArg, store: CxxTypeStore) =
    aux(use.nimType, store)

  func aux(use: var CxxTypeUse, store: CxxTypeStore) =
    eachIdent(use) do (use: var CxxTypeUse):
      if not use.cxxType.isParam:
        use.cxxType.typeStore = store
        use.cxxType.typeLib = some lib.library

  func aux(decl: var CxxProc, store: var CxxTypeStore) =
    raise newImplementError()

  case entry.kind:
    of cekProc:
      aux(entry.cxxProc, store)

    of cekPass, cekForward, cekEmpty, cekImport,
       cekMacroGroup, cekMacro, cekComment:
      discard

    of cekTypeGroup:
      for decl in mitems(entry.cxxTypes):
        setTypeStoreRec(entry, store, lib)

    of cekEnum:
      entry.cxxEnum.decl.typeImport = some lib
      store.addDecl(entry.cxxEnum.decl)

    of cekAlias:
      entry.cxxAlias.decl.typeImport = some lib
      store.addDecl(entry.cxxAlias.decl)

    of cekObject:
      entry.cxxObject.decl.typeImport = some lib
      store.addDecl(entry.cxxObject.decl)

      for meth in mitems(entry.cxxObject.methods):
        aux(meth, store)

      for field in mitems(entry.cxxObject.mfields):
        aux(field.nimType, store)

      for nest in mitems(entry.cxxObject.nested):
        setTypeStoreRec(entry, store, lib)

func fixIdentsRec*(
    entry: var CxxEntry, cache: var StringNameCache, prefix: string) =

  template aux(name: var CxxNamePair): untyped =
    name.nim = cache.fixNumerateIdentName(name.nim, prefix)

  template auxType(name: var CxxNamePair): untyped =
    name.nim = cache.fixNumerateTypeName(name.nim, prefix)

  func aux(decl: var CxxProc, cache: var StringNameCache) =
    aux(decl.head.name)
    for arg in mitems(decl.arguments):
      aux(arg.name)


  case entry.kind:
    of cekEnum:
      auxType(entry.cxxEnum.decl.name)
      for value in mitems(entry.cxxEnum.values): aux(value.name)

    of cekForward:
      auxType(entry.cxxForward.decl.name)

    of cekObject:
      auxType(entry.cxxObject.decl.name)
      for field in mitems(entry.cxxObject.mfields): aux(field.name)
      for mproc in mitems(entry.cxxObject.methods): aux(mproc, cache)
      for nestd in mitems(entry.cxxObject.nested):
        fixIdentsRec(nestd, cache, prefix)

    of cekProc:
      aux(entry.cxxProc, cache)

    else:
      raise newImplementKindError(entry)



func hasTypeDecl*(entry: CxxEntry): bool =
  entry.kind in {cekEnum, cekForward, cekObject, cekAlias}

func getTypeDecl*(entry: CxxEntry): CxxTypeDecl =
  case entry.kind:
    of cekEnum: result = entry.cxxEnum.decl
    of cekObject: result = entry.cxxEnum.decl
    of cekAlias: result = entry.cxxAlias.decl
    of cekForward: result = entry.cxxForward.decl
    else: raise newUnexpectedKindError(entry)

func toRealDecl*(entry: CxxEntry): CxxEntry =
  assertKind(entry, cekForward)
  raise newImplementError()

func box*(en: CxxEnum): CxxEntry = CxxEntry(kind: cekEnum, cxxEnum: en)
func box*(en: CxxForward): CxxEntry =
  CxxEntry(kind: cekForward, cxxForward: en)
func box*(ob: CxxObject): CxxEntry = CxxEntry(kind: cekObject, cxxObject: ob)
func box*(en: CxxProc): CxxEntry = CxxEntry(kind: cekProc, cxxProc: en)
func box*(en: CxxAlias): CxxEntry = CxxEntry(kind: cekAlias, cxxAlias: en)
func box*(en: CxxMacro): CxxEntry = CxxEntry(kind: cekMacro, cxxMacro: en)

func add*(
    s: var seq[CxxEntry],
    other: CxxMacro | CxxAlias | CxxObject | CxxForward | CxxProc | CxxEnum
  ) =

  s.add box(other)

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

import pkg/jsony

proc dumpHook*[I](s: var string, ins: set[I]) =
  s.add "["
  for idx, item in pairs(ins):
    if idx > 0:
      s.add ","

    s.add $item
  s.add "]"

proc parseHook*[I](s: string, i: var int, res: var set[I]) =
  s.eatChar(i, '[')
  while s[i] != ']':
    let start = i
    let pos = skipUntil(s, ',', i)
    res.incl parseEnum[I](s[start .. pos])
    i = pos
    s.eatChar(i, ',')
    s.eatChar(i, ' ')

  s.eatChar(i, ']')


proc dumpFieldLines*[T](s: var string, obj: T)
proc dumpSeqLines*[T](s: var string, entries: seq[T])

proc dumpHook*(s: var string, entries: seq[CxxEntry]) = dumpSeqLines(s, entries)
proc dumpHook*(s: var string, procs: seq[CxxProc]) = dumpSeqLines(s, procs)
proc dumpHook*(s: var string, obj: CxxObject) = dumpFieldLines(s, obj)


proc dumpFieldLines*[T](s: var string, obj: T) =
  s.add "{"
  for name, value in fieldPairs((when T is object: obj else: obj[])):
    s.add "\"", name, "\": "
    dumpHook(s, value)
    s.add ",\n"

  s.add "}"

proc dumpSeqLines*[T](s: var string, entries: seq[T]) =
  s.add "["
  for idx, item in pairs(entries):
    if idx > 0:
      s.add ",\n"

    dumpHook(s, item)

  s.add "]"

proc toJson*(file: CxxFile): string =
  dumpFieldLines(result, file)

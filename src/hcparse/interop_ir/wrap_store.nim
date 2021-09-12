import
  hmisc/other/oswrap,
  hmisc/core/all,
  std/[options, macros, json, strutils, strformat,
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
    docComment*: seq[string]
    haxdocIdent* {.requiresinit.}: JsonNode

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
    typeImport*: CxxLibImport
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
    newType*: CxxTypeDecl
    oldType*: CxxTypeUse


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
    gokUnion
    gokStruct
    gokClass

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

func `$`*(name: CxxName): string = name.scopes.join("::")
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
func nimName*(t: CxxTypeUse): string = t.cxxType.name.nim

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

func initCxxHeader*(global: string): CxxHeader =
  CxxHeader(global: global, kind: chkGlobal)

func initCxxHeader*(file: AbsFile): CxxHeader =
  CxxHeader(kind: chkAbsolute, file: file)

func initCxxArg*(name: CxxNamePair, argType: CxxTypeUse): CxxArg =
  CxxArg(nimType: argType, name: name, haxdocIdent: newJNull())

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

func getDecl*(use: CxxTypeUse): CxxTypeDecl =
  assertKind(use, {ctkIdent})
  assert not use.cxxType.isParam
  assertRef use.cxxType.typeStore
  for decl in use.cxxType.typeStore.typeDecls[use.cxxName()]:
    if use.cxxType.typeLib.isNone():
      return decl

    else:
      if decl.typeImport.library == use.cxxType.typeLib.get():
        return decl

  raise newGetterError("Cannot get import for type use")

func getImport*(use: CxxTypeUse): CxxLibImport =
  use.getDecl().typeImport

func getImport*(decl: CxxTypeDecl): CxxLibImport = decl.typeImport
func getFilename*(limport: CxxLibImport): string =
  limport.importPath[^1] # TODO drop extension


func getType*(arg: CxxArg): CxxTypeUse = arg.nimType
func getType*(field: CxxField): CxxTypeUse = field.nimType


proc getUsedTypesRec*(
    t: CxxTypeUse, ignoreHead: bool = false): seq[CxxTypeUse] =
  if not ignoreHead:
    result.add t

  case t.kind:
    of ctkWrapKinds:
      result.add getUsedTypesRec(t.wrapped)

    of ctkArrayKinds:
      result.add getUsedTypesRec(t.arrayElement)

    of ctkStaticParam:
      discard

    of ctkIdent:
      for param in t.genParams:
        result.add getUsedTypesRec(param)

    of ctkProc:
      if notNil t.returnType:
        result.add t.returnType

      for argument in t.arguments:
        result.add getUsedTypesRec(argument.getType())


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

func cxxObject*(name: CxxNamePair, genParams: CxxGenParams = @[]): CxxObject =
  CxxObject(decl: cxxTypeDecl(name, genParams), haxdocIdent: newJNull())

func cxxEnum*(name: CxxNamePair): CxxEnum =
  CxxEnum(decl: cxxTypeDecl(name), haxdocIdent: newJNull())

func cxxField*(name: CxxNamePair, nimType: CxxTypeUse): CxxField =
  CxxField(name: name, nimType: nimType, haxdocIdent: newJNull())

func cxxAlias*(newType: CxxTypeDecl, oldType: CxxTypeUse): CxxAlias =
  CxxAlias(newType: newType, oldType: oldType, haxdocIdent: newJNull())

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


func add*(pr: var CxxProc, arg: CxxArg) =
  pr.arguments.add arg

func setHeaderRec*(entry: var CxxEntry, header: CxxHeader) =
  case entry.kind:
    of cekEnum: entry.cxxEnum.header = some header
    of cekProc: entry.cxxProc.header = some header
    of cekAlias: entry.cxxAlias.header = some header
    of cekObject:
      entry.cxxObject.header = some header
      for meth in mitems(entry.cxxObject.methods):
        meth.header = some header

      for nest in mitems(entry.cxxObject.nested):
        setHeaderRec(nest, header)

    else:
      discard

func hasTypeDecl*(entry: CxxEntry): bool =
  entry.kind in {cekEnum, cekForward, cekObject, cekAlias}

func getTypeDecl*(entry: CxxEntry): CxxTypeDecl =
  case entry.kind:
    of cekEnum: result = entry.cxxEnum.decl
    of cekObject: result = entry.cxxEnum.decl
    of cekAlias: result = entry.cxxAlias.newType
    of cekForward: result = entry.cxxForward.decl
    else: raise newUnexpectedKindError(entry)

func toRealDecl*(entry: CxxEntry): CxxEntry =
  assertKind(entry, cekForward)
  raise newImplementError()

  # result.isPromotedForward = true


func box*(en: CxxEnum): CxxEntry =
  CxxEntry(kind: cekEnum, cxxEnum: en)

func box*(en: CxxForward): CxxEntry =
  CxxEntry(kind: cekForward, cxxForward: en)

func box*(ob: CxxObject): CxxEntry =
  CxxEntry(kind: cekObject, cxxObject: ob)

func box*(en: CxxProc): CxxEntry =
  CxxEntry(kind: cekProc, cxxProc: en)

func box*(en: CxxAlias): CxxEntry =
  CxxEntry(kind: cekAlias, cxxAlias: en)

func box*(en: CxxMacro): CxxEntry =
  CxxEntry(kind: cekMacro, cxxMacro: en)

func add*(
    s: var seq[CxxEntry],
    other: CxxMacro | CxxAlias | CxxObject | CxxForward | CxxProc | CxxEnum
  ) =

  s.add box(other)
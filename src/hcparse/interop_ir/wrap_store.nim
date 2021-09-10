import
  hmisc/other/oswrap,
  hmisc/core/all,
  std/[options, macros, json, strutils, strformat, tables]

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


  CxxTypeRef = object
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

  CxxTypeUse* = ref object
    ## Instantiated type
    flags*: set[CxxTypeFlag]
    case kind*: CxxTypeKind
      of ctkStaticParam:
        value*: CxxExpr

      of ctkPtr, ctkLVref, ctkRVRef, ctkDynamicArray:
        wrapped*: CxxTypeUse

      of ctkFixedArray, ctkDependentArray:
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
    cpkOperator ## Operator: `*`

    cpkAssignOperator
    cpkCopyOperator
    # cpk

    # cpkHook ## Destructor/sink (etc.) hook: `=destroy`
    # cpkAssgn ## Assignment proc `field=`

  CxxProcFlag = enum
    cpfConst
    cpfOperator
    cpfOverride
    cpfExportc
    cpfSlot
    cpfSignal
    cpfVirtual

  CxxProc* = object of CxxBase
    kind*: CxxProcKind
    head*: CxxTypeDecl ## Reuse type declaration for procedure - it has
    ## very similar structure (nim/cxx name, generic parameters with
    ## optional defaults). Missing elements are added as regular fields.

    arguments*: seq[CxxArg]
    returnType*: CxxTypeUse


    flags*: set[CxxProcFlag]

    constructorOf*: Option[CxxNamePair]
    methodOf*: Option[CxxNamePair]

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

  CxxField* = object of CxxBase
    nimType*: CxxTypeUse
    isStatic*: bool

  CxxEnumValue* = object
    baseName*: string
    name*: CxxNamePair
    value*: BiggestInt
    comment*: string

  CxxAlias* = object of CxxBase
    isDistinct*: bool
    newAlias*: CxxTypeDecl
    baseType*: CxxTypeUse

  CxxEnum* = object of CxxBase
    values*: seq[CxxEnumValue]


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

      else:
        discard

  CxxFile* = object
    imports*: seq[CxxLibImport]
    exports*: seq[CxxLibImport]

    entries*: seq[CxxEntry]
    savePath*: CxxLibImport

func `nimName=`*(pr: var CxxProc, name: string) =
  pr.head.name.nim = name

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

func add*(t: var CxxTypeUse, other: CxxTypeUse) =
  t.genParams.add other
  t.genParams.last().flags.incl ctfParam

func getConstructed*(pr: CxxProc): CxxNamePair =
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

func getReturn*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): CxxTypeUse =

  if pr.isConstructor:
    result = cxxTypeUse(pr.getConstructed()).wrap(onConstructor)

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

func cxxObject*(name: CxxNamePair, genParams: CxxGenParams): CxxObject =
  CxxObject(decl: cxxTypeDecl(name, genParams), haxdocIdent: newJNull())


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
    other: CxxMacro | CxxAlias | CxxObject | CxxForward | CxxProc
  ) =

  s.add box(other)

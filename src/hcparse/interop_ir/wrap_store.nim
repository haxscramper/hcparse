import
  hmisc/other/oswrap,
  hmisc/core/all,
  std/[options, macros, json, strutils, strformat, tables]

import
  ./wrap_icpp

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

  CxxName* = object
    scopes*: seq[string]

  CxxIdent* = object
    name*: CxxName
    genParams*: seq[CxxTypeUse]

  CxxTypeDecl* = object
    isForward*: bool
    cxxName*: CxxName
    nimName*: string
    typeImport*: CxxLibImport
    genParams*: seq[tuple[name: string, default: Option[CxxTypeUse]]]

    store*: CxxTypeStore

  CxxTypeStore* = ref object
    ## Lookup table for type and object declarations. Each lookup contains
    ## `seq` of definitions for each `CxxName` to handle potentiall name
    ## duplicates.

    typeDecls*: Table[CxxName, seq[CxxTypeDecl]]
    classDecls*: Table[CxxName, seq[CxxObject]]


  CxxTypeRef = object
    ## Reference to used C++ type
    case isParam*: bool
      of true:
        paramName*: string

      of false:
        nimName*: string
        cxxName*: CxxName
        typeLib*: Option[string]
        typeStore*: CxxTypeStore

  CxxTypeFlag* = enum
    ctfConst
    ctfMutable
    ctfComplex
    ctfParam

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
    cpkHook ## Destructor/sink (etc.) hook: `=destroy`
    cpkAssgn ## Assignment proc `field=`

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

    constructorOf*: Option[CxxName]
    methodOf*: Option[CxxType]

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
        ident*: CxxName

  CxxArg* = object of CxxBase
    nimType*: CxxTypeUse
    default*: Option[CxxExpr]

  CxxField* = object of CxxBase
    nimType*: CxxTypeUse
    isStatic*: bool

  CxxEnumValue* = object
    baseName*: string
    cxxName*: seq[string]
    nimName*: string
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
    nimName*: string
    cxxName*: CxxName
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

func isConst*(pr: CxxProc): bool = cpfConst in pr.flags
func isConstructor*(pr: CxxProc): bool = pr.constructorOf.isSome()
func isMethod*(pr: CxxProc): bool = pr.methodOf.isSome()

func getConstructed*(pr: CxxProc): string =
  pr.constructorOf.get()

func getIcppName*(pr: CxxProc, asMethod: bool = false): string =
  if asMethod:
    pr.cxxName[^1]

  else:
    pr.cxxName.join("::")

func initCxxHeader*(global: string): CxxHeader =
  CxxHeader(global: global, kind: chkGlobal)

func initCxxHeader*(file: AbsFile): CxxHeader =
  CxxHeader(kind: chkAbsolute, file: file)

func initCxxArg*(name: string, argType: CxxType): CxxArg =
  CxxArg(nimType: argType, nimName: name, haxdocIdent: newJNull())


func wrap*(wrapped: CxxType, kind: CxxTypeKind): CxxType =
  if kind == ctkIdent:
    result = wrapped

  else:
    result = CxxType(kind: kind)
    result.wrapped = wrapped

func initCxxType*(head: string, genParams: seq[CxxType] = @[]): CxxType =
  CxxType(kind: ctkIdent, nimName: head, genParams: @genParams)

func getReturn*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): CxxType =

  if pr.isConstructor:
    result = initCxxType(pr.getConstructed()).wrap(onConstructor)

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
    result.ctype(pr.cxxName.join("::"))


func getIcppStr*(pr: CxxObject): string = $getIcpp(pr)

func initIcpp*(
    pr: var CxxProc, onConstructor: CxxTypeKind = ctkIdent) =

  pr.icpp = getIcpp(pr, onConstructor)


func initCxxType*(arguments: seq[CxxArg], returnType: CxxType): CxxType =
  CxxType(
    kind: ctkProc, arguments: arguments, returnType: returnType)

func initCxxObject*(nimName, cxxName: string): CxxObject =
  CxxObject(nimName: nimName, cxxName: @[cxxName], haxdocIdent: newJNull())

func initCxxProc*(
    nimName, cxxName: string,
    arguments: seq[CxxArg] = @[], returnType: CxxType = initCxxType("void")
  ): CxxProc =

  CxxProc(
    nimName: nimName,
    cxxName: @[cxxName],
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

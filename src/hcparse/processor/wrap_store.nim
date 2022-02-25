import
  hmisc/other/[oswrap],
  hmisc/core/[all, code_errors],
  hmisc/algo/[hstring_algo]

import
  std/[
    options, macros, json, strutils,
    tables, hashes, sets, sequtils,
    math
  ]

import
  ./wrap_icpp

type
  CxxTypeKind* = enum
    ## Kind of the wrapped Cxx type
    ctkIdent ## Identifier with optional list of template parameters
    ctkPod
    ctkProc ## Procedural (callback) type
    ctkPtr
    ctkLVref
    ctkRVref
    ctkFixedArray
    ctkDependentArray
    ctkDynamicArray
    ctkDecltype

    ctkStaticParam

    ctkAnonObject ## Anonymous object, struct or union declared in-place
    ctkAnonEnum ## Anonymous enum declared in-place

  NimConstructorTarget* = enum
    nctRegular
    nctPtr
    nctRef

const
  ctkWrapKinds* = { ctkPtr, ctkLVRef, ctkRVRef, ctkDynamicArray }
  ctkArrayKinds* = { ctkFixedArray, ctkDependentArray }



type
  CxxSpellingLocation* = object
    file*: AbsFile
    line*, column*: int


  CxxBindKind* = enum
    ## Kind of the nim input header
    cbkNone
    cbkGlobal ## Global header file, must be installed and accessible via
    ## `includepath` when wrappers are compiled
    cbkAbsolute ## Absolute path to the base header file
    cbkPNode ## Unconstrained PNode - can be anything
    cbkDynamicPatt
    cbkDynamicExpr
    cbkDynamicCall
    cbkMacroBind
    cbkNotImported ## Entry does not have any mapping to the original code
    ## (necessary for code transpiler and conversion of the dynamic
    ## libraries without need for headers afterwards (in that case type
    ## should not be imported from anywhere.))
    cbkLink ## Bind via OS dynamic linker

  CxxLibImport* = object
    library*: string
    importPath*: seq[string]

  CxxBind* = object
    icpp*: IcppPattern ## Binding pattern for header imports
    imports*: seq[CxxLibImport] ## Additional imports that are required for
                                ## object binding

    case kind*: CxxBindKind
      of cbkNone, cbkLink, cbkNotImported:
        discard

      of cbkGlobal:
        global*: string

      of cbkAbsolute:
        file*: AbsFile

      of cbkPNode:
        other*: string

      of cbkDynamicPatt:
        dynPattern*: string

      of cbkDynamicExpr, cbkDynamicCall, cbkMacroBind:
        dynExpr*: string


  CxxComment* = object
    text*: string

  CxxBase* = ref object of RootObj
    store*: CxxTypeStore
    iinfo*: LineInfo
    spellingLocation*: Option[CxxSpellingLocation]
    cbind*: CxxBind
    access*: CxxAccessSpecifier
    docComment*: seq[CxxComment]
    haxdocIdent* #[ {.requiresinit.} ]#: JsonNode

    isAnonymous*: bool

  CxxName* = object
    scopes*: seq[string]

  CxxNameContext* = enum
    cncNone

    cncType
    cncArg
    cncVar
    cncProc
    cncMethod
    cncField
    cncEnumField

  CxxNamePair* = object
    ## Qualified cxx name with full scoping information and optional nim
    ## name override.
    context*: CxxNameContext
    nim*: string
    cxx*: CxxName

  CxxIdent* = object
    name*: CxxNamePair
    genParams*: seq[CxxTypeUse]

  CxxGenParams* = seq[tuple[name: CxxNamePair, default: Option[CxxTypeUse]]]

  CxxTypeDeclKind* = enum
    ctdkNone

    ctdkEnum
    ctdkStruct
    ctdkClass
    ctdkUnion
    ctdkTypedef

    ctdkProc # HACK because proc also uses type declaration for it's head

  CxxTypeDecl* = object
    kind*: CxxTypeDeclKind
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
    forwardDecls*: Table[CxxName, seq[CxxTypeDecl]]
    classDecls*: Table[CxxName, seq[CxxEntry]]

    importDecls*: Table[CxxName, CxxLibImport]

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
    ctfNone

    ctfConst
    ctfMutable
    ctfComplex
    ctfParam
    ctfDefaultedParam
    ctfUnexposed

    ctfIsPodType
    ctfIsEnumType
    ctfIsStructType
    ctfIsUnionType
    ctfIsClassType
    ctfIsTypedefType


    ctfPtrToArray
    ctfNoCdeclProc

  CxxPodTypeKind* = enum
    cptNone

    cptU8
    cptU16
    cptU32
    cptU64

    cptI8
    cptI16
    cptI32
    cptI64

    cptVoid
    cptNullptr
    cptChar
    cptUChar
    cptWChar
    cptChar16
    cptChar32


    cptInt
    cptUInt
    cptBool
    cptFloat
    cptDouble
    cptLongDouble
    cptSizeT
    cptSSizeT
    cptAuto

  CxxTypeUse* = ref object
    ## Instantiated type
    flags*: set[CxxTypeFlag]
    case kind*: CxxTypeKind
      of ctkStaticParam, ctkDecltype:
        value*: CxxExpr

      of ctkPtr, ctkLVRef, ctkRVRef, ctkDynamicArray:
        wrapped*: CxxTypeUse

      of ctkFixedArray, ctkDependentArray:
        arraySize*: CxxTypeUse ## Array size type or static parameter
        ## (usually static parameter). Dynamic arrays without explicitly
        ## specified parameters are handled as 'wrapped'
        arrayElement*: CxxTypeUse ## Array type.

      of ctkPod:
        podKind*: CxxPodTypeKind

      of ctkIdent:
        types*: seq[tuple[
          cxxType: CxxTypeRef,
          genParams: seq[CxxTypeUse]
        ]] ## Scopes of the fully qualified identifier, such as
           ## `std::vector<int>` or `std::map<char, float>`. Might contain
           ## single identifier. For scoped parameters each section
           ## contains fully qualified - `std::vector` is stored as a
           ## single entry, `name::space::type` is stored as single as
           ## well, and only `space::type<A>::nested<B>` will have two
           ## entries (because it actually has two separate types)

      of ctkProc:
        arguments*: seq[CxxArg]
        returnType*: CxxTypeUse

      of ctkAnonObject:
        ## This code is valid and allowed
        ##
        ## ```c
        ## struct { int x,y; } foo() {
        ##    typeof(foo()) ret = {1, 10};
        ##    return ret;
        ## }
        ## ```
        objDef*: CxxObject
        objParent*: CxxNamePair ## Name of the parent declaration. Used to
        ## construct anonymous type name.
        objUser*: CxxNamePair ## Name of the entry that /uses/ the type. Is
        ## necessary to construct the name for anonymous type.

      of ctkAnonEnum:
        enumDef*: CxxEnum
        enumParent*: CxxNamePair
        enumUser*: CxxNamePair





  CxxProcKind* = enum
    ## Procedure kind
    cpkRegular ## Regular proc: `hello()`

    cpkConstructor
    cpkDestructor

    # cxx operator range start
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
    # cxx operator range end

    # cpk

    # cpkHook ## Destructor/sink (etc.) hook: `=destroy`
    # cpkAssgn ## Assignment proc `field=`

  CxxProcFlag* = enum
    cpfConst
    cpfStatic
    cpfOperator
    cpfOverride
    cpfExportc
    cpfSlot
    cpfSignal
    cpfVirtual
    cpfVariadic
    cpfMethod
    cpfNoexcept

  CxxProc* = ref object of CxxBase
    kind*: CxxProcKind
    head*: CxxTypeDecl ## Reuse type declaration for procedure - it has
    ## very similar structure (nim/cxx name, generic parameters with
    ## optional defaults). Missing elements are added as regular fields.

    userData*: pointer ## Additional user data. Is not intended for use in
    ## general, added only to hack in method body transpilation.
    ## Tree-sitter casts the original node, and then codegen triggers 'post
    ## convert' hook.
    arguments*: seq[CxxArg]
    returnType*: CxxTypeUse
    throws*: seq[CxxTypeUse]
    flags*: set[CxxProcFlag]

    constructorOf*: Option[CxxNamePair]
    destructorOf*: Option[CxxNamePair]

  CxxExprKind* = enum
    cekIntLit
    cekStrLit
    cekVar
    cekCallLit

  CxxExpr* = object
    case kind*: CxxExprKind
      of cekIntLit:
        intVal*: int

      of cekStrLit, cekCallLit:
        strVal*: string

      of cekVar:
        ident*: CxxNamePair

  CxxArgFlag = enum
    cafOutParam
    cafInParam

  CxxArg* = ref object of CxxBase
    flags*: set[CxxArgFlag]
    name*: CxxNamePair
    nimType*: CxxTypeUse
    default*: Option[CxxExpr]
    # docComment*: seq[CxxComment]

  CxxFieldFlag* = enum
    cffStatic
    cffPublic
    cffPrivate
    cffProtected

  CxxField* = ref object of CxxBase
    name*: CxxNamePair
    nimType*: CxxTypeUse
    flags*: set[CxxFieldFlag]
    bitsize*: Option[int]


  CxxAlias* = ref object of CxxBase
    isDistinct*: bool
    decl*: CxxTypeDecl
    baseType*: CxxTypeUse


  CxxEnumValue* = object
    name*: CxxNamePair
    value*: BiggestInt
    docComment*: seq[CxxComment]

  CxxEnum* = ref object of CxxBase
    isClassEnum*: bool
    decl*: CxxTypeDecl
    values*: seq[CxxEnumValue]
    duplicates*: seq[CxxEnumValue]


  CxxObjectKind* = enum
    cokUnion
    cokStruct
    cokClass

  CxxAccessSpecifier* = enum
    casPublic
    casProtected
    casPrivate

  CxxQSignals* = enum
    cqsNone
    cqsSlot
    cqsSignal

  CxxObjectFlag* = enum
    cofExplicitConstructor
    cofExplicitDestructor
    cofCanAggregateInit

  CxxObject* = ref object of CxxBase
    flags*: set[CxxObjectFlag]

    decl*: CxxTypeDecl
    kind*: CxxObjectKind

    super*: seq[CxxTypeUse]
    nested*: seq[CxxEntry]

    isByref*: bool

    mfields*: seq[CxxField]
    methods*: seq[CxxProc]

  CxxForward* = object of CxxBase
    decl*: CxxTypeDecl

  CxxMacroTokenKind* = enum
    cmtkIntLit
    cmtkIdent
    cmtkPunctuation
    cmtkKeyword

  CxxMacroToken* = object
    strVal*: string
    kind*: CxxMacroTokenKind

  CxxMacro* = object of CxxBase
    name*: CxxNamePair
    arguments*: seq[string]
    tokens*: seq[CxxMacroToken]

  CxxMacroGroup* = object
    macros*: seq[CxxEntry]

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
    original*: AbsFile

    imports*: HashSet[CxxLibImport]
    exports*: HashSet[CxxLibImport]

    entries*: seq[CxxEntry]

    isGenerated*: bool
    savePath*: CxxLibImport

const
  cekTypeStored* = {
    cekProc, cekTypeGroup, cekEnum, cekForward, cekAlias, cekObject
  }

  cpkOperatorKinds* = {
    cpkPrefixOp .. cpkDeleteOp
  }

export IdentStyle


func `$`*(cxx: CxxLibImport): string =
  cxx.library & "@" & cxx.importPath.join("/")

func `$`*(file: CxxFile): string = $file.savePath

func cxxStr*(name: CxxName): string = name.scopes.join("::")
func cxxStr*(name: CxxNamePair): string = name.cxx.scopes.join("::")
func `$`*(name: CxxName): string = name.cxxStr()
func `$`*(name: CxxNamePair): string =
  let nim = $name.nim
  let cxx = $name.cxx
  result = $name.context & "."
  if nim == cxx:
    result &= nim & "-//-"

  else:
    result &= cxx & "/" & nim

func `$`*(expr: CxxExpr): string =
  case expr.kind:
    of cekIntLit: $expr.intVal
    of cekVar:    $expr.ident

    of cekStrLit, cekCallLit:
      expr.strVal

func `$`*(tref: CxxTypeRef): string =
  if not tref.isParam:
    if tref.typeLib.isSome():
      result.add tref.typeLib.get()
      result.add "@"

  result.add $tref.name


func `$`*(ct: CxxTypeUse): string

func `$`*(arg: CxxArg): string =
  assertRef(arg)
  result = $arg.name & ": " & $arg.nimType
  if arg.default.isSome():
    result &= " = "
    result &= $arg.default.get()

func `$`*(ct: CxxTypeUse): string =
  if isNil(ct):
    result = "void"

  else:
    case ct.kind:
      of ctkPod:
        result = $ct.podKind

      of ctkDecltype:
        result = "typeof($#)" % $ct.value

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
        for idx, part in ct.types:
          if 0 < idx: result.add "+"
          result &= $part.cxxType
          if ?part.genParams:
            result &= "["
            result &= part.genParams.mapIt($it).join(", ")
            result &= "]"

      of ctkAnonEnum:
        result &= "anon-enum"

      of ctkAnonObject:
        result &= "anon-object"

      of ctkProc:
        result &= "proc("
        for idx, arg in ct.arguments:
          if idx > 0:
            result.add ", "
          result &= $arg

        result &= "): "
        result &= $ct.returnType

func `$`*(decl: CxxTypeDecl): string =
  if decl.typeImport.isSome():
    result.add $decl.typeImport.get()
    result.add "@"

  else:
    result.add "?"


  result.add $decl.kind
  result.add "."
  result.add $decl.name
  if ?decl.genParams:
    result.add "["
    for idx, (name, default) in decl.genParams:
      if idx > 0:
        result.add ", "

      result.add $name
      if default.isSome():
        result.add " = "
        result.add $default.get()

    result.add "]"



func `$`*(p: CxxProc): string =
  result = $p.head
  result.add "("
  for idx, arg in p.arguments:
    if idx > 0: result.add ", "
    result.add $arg

  result.add ")"
  if notNil(p.returnType):
    result.add ": "
    result.add $p.returnType

func `$`*(alias: CxxAlias): string =
  $alias.decl & " = " & $alias.baseType

func `$`*(obj: CxxObject): string = "object!" & $obj.decl

func `$`*(obj: CxxField): string =
  "field!$#: $#" % [$obj.name, $obj.nimType]

func `$`*(e: CxxEntry): string =
  case e.kind:
    of cekEnum: result = "enum!" & $e.cxxEnum.decl
    of cekObject: result = "object!" & $e.cxxObject.decl
    of cekProc: result = "proc!" & $e.cxxProc
    of cekAlias: result = "alias!" & $e.cxxAlias
    of cekForward: result = "forward!" & $e.cxxForward.decl
    of cekMacro: result = "macro!" & $e.cxxMacro.name
    of cekEmpty: result = "empty!()"

    else:
      raise newImplementKindError(e)


func `[]`*(t: CxxTypeUse, idx: int): CxxTypeUse =
  ## Return idx'th generic parameter of a type. Regular generic parameter
  ## return their parameters, arrays return `[0 => arraySize, 1 =>
  ## arrayElement]`, wrapped kinds (pointer, ref etc.) return wrapped type
  ## on 0'th parameter, procedural type yield return type on 0'th argument,
  ## and types of arguments on others. For nested types index generic type
  ## parameters from left to right.
  ##
  ## - `array[Enum, int]` :: `0 -> Enum`, `1 -> int`
  ## - `proc(a: int): float` :: `0 -> float`, `1 -> int`
  ## - `ptr X` :: `0 -> X`
  ## - `A::B<C>::<D>` :: `0 -> C`, `0 -> D`
  case t.kind:
    of ctkWrapKinds:
      if idx != 0: raise newArgumentError(
        "Wrap kinds only support indexing into 0'th parameter, but ",
        idx, " was used")

      return t.wrapped

    of ctkArrayKinds:
      case idx:
        of 0: return t.arraySize
        of 1: return t.arrayElement
        else: raise newArgumentError(
          "Array kinds only supports indexing into 0'th of 1st parameters, ",
          "but ", idx, " was used")

    of ctkIdent:
      var curr = 0
      for typ in t.types:
        if idx in curr .. typ.genParams.len:
          return typ.genParams[curr]

        else:
          curr += typ.genParams.len

      raise newArgumentError(
        "No generic parameter indexed ", idx, " for identifier")

    of ctkProc:
      if idx == 0:
        return t.returnType

      else:
        return t.arguments[idx - 1].nimType

    of ctkStaticParam, ctkDecltype, ctkPod, ctkAnonObject, ctkAnonEnum:
      raise newUnexpectedKindError(
        t, "Static parameters, POD types, decltype() calls, anonymous objects/enums ",
        "do not support generic parameter indexing")

func len*(t: CxxTypeUse): int =
  case t.kind:
    of ctkWrapKinds: 1
    of ctkArrayKinds: 2
    of ctkIdent: t.types.mapIt(it.genParams.len).sum()
    of ctkProc: 1 + t.arguments.len
    of ctkStaticParam, ctkAnonObject, ctkAnonEnum, ctkPod, ctkDecltype: 0

func `[]`*(back: CxxTypeUse, idx: BackwardsIndex): CxxTypeUse =
  back[back.len - idx.int]

iterator items*(use: CxxTypeUse): CxxTypeUse =
  for i in 0 ..< len(use):
    yield use[i]

var asdf = false

func `==`*(n1, n2: CxxName): bool =
  if n1.scopes.len != n2.scopes.len:
    return false

  else:
    for (s1, s2) in zip(n1.scopes, n2.scopes):
      {.cast(noSideEffect).}:
        if asdf and s1 == "SGRegister":
          discard
          # echov globalTick()
          # assert false

      if s1 != s2:
        return false

    return true



func `==`*(l1, l2: CxxLibImport): bool =
  l1.library == l2.library and l1.importPath == l2.importPath

func `<`*(l1, l2: CxxLibImport): bool =
  if l1.library == l2.library:
    if l1.importPath.len != l2.importPath.len:
      return l1.importPath.len < l2.importPath.len

    else:
      for (p1, p2) in zip(l1.importPath, l2.importPath):
        if p1 != p2:
          return p1 < p2

  else:
    return l1.library < l2.library

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
    of ctkPod:
      result = hash(use.podKind)

    of ctkAnonEnum:
      result = hash(use.enumDef.values.len)
      for value in items(use.enumDef.values):
        result = hash(value.name) !& result

    of ctkAnonObject:
      result = hash(use.objDef.mfields.len)
      for field in items(use.objDef.mfields):
        result = hash(field.nimType) !& result

    of ctkStaticParam, ctkDecltype:
      raise newImplementKindError(use)

    of ctkWrapKinds:
      result = result !& hash(use.wrapped)

    of ctkArrayKinds:
      result = result !& hash(use.arraySize) !& hash(use.arrayElement)

    of ctkIdent:
      for typ in use.types:
        result = result !& hash(typ.cxxType) !& hash(typ.genParams)

    of ctkProc:
      result = result !& hash(use.arguments)
      if use.returnType.notNil():
        result = result !& hash(use.returnType)

func hash*(decl: CxxTypeDecl): Hash =
  !$(hash(decl.name) !& hash(decl.genParams) !& hash(decl.typeImport))

func cxxType*(use: CxxTypeUse): CxxTypeRef =
  ## Get reference to the last (full) type in the fully qualified type
  ## identifier.
  use.types.last().cxxType

func `cxxName=`*(pr: var CxxProc, name: CxxName) =
  pr.head.name.cxx = name

func `nimName=`*(pr: var CxxProc, name: string) =
  pr.head.name.nim = name

func `name=`*(en: var CxxEnum, name: CxxNamePair) = en.decl.name = name
func `name=`*(obj: var CxxObject, name: CxxNamePair) = obj.decl.name = name

func name*(use: CxxTypeUse): CxxNamePair =
  assertKind(use, {ctkIdent})
  result = use.cxxType.name

func name*(pr: CxxProc): CxxNamePair = pr.head.name
func name*(pr: CxxObject): CxxNamePair = pr.decl.name

func `nimName=`*(obj: var CxxAlias, name: string) = obj.decl.name.nim = name

func nimName*(pr: CxxProc): string       = pr.head.name.nim
func nimName*(arg: CxxArg): string       = arg.name.nim
func nimName*(arg: CxxEnumValue): string = arg.name.nim
func nimName*(t: CxxTypeUse): string     = t.cxxType.name.nim
func nimName*(obj: CxxObject): string    = obj.decl.name.nim
func nimName*(obj: CxxForward): string   = obj.decl.name.nim
func nimName*(obj: CxxEnum): string      = obj.decl.name.nim
func nimName*(obj: CxxAlias): string     = obj.decl.name.nim
func nimName*(field: CxxField): string   = field.name.nim
func nimName*(t: CxxTypeDecl): string    = t.name.nim
func nimName*(pair: CxxNamePair): string = pair.nim

func cxxName*(pair: CxxNamePair): CxxName   = pair.cxx
func cxxName*(arg: CxxArg): CxxName         = arg.name.cxx
func cxxName*(field: CxxField): CxxName     = field.name.cxx
func cxxName*(t: CxxTypeUse): CxxName       = t.cxxType.name.cxx
func cxxName*(t: CxxTypeDecl): CxxName      = t.name.cxx
func cxxName*(pr: CxxProc): CxxName         = pr.head.name.cxx
func cxxName*(obj: CxxObject): CxxName      = obj.decl.name.cxx
func cxxName*(obj: CxxForward): CxxName     = obj.decl.name.cxx
func cxxName*(obj: CxxEnum): CxxName        = obj.decl.name.cxx
func cxxName*(alias: CxxAlias): CxxName     = alias.decl.name.cxx
func cxxName*(name: string): CxxName        = CxxName(scopes: @[name])
func cxxName*(scopes: seq[string]): CxxName = CxxName(scopes: scopes)

func name*(e: CxxEntry): CxxNamePair =
  case e.kind:
    of cekEnum: result = e.cxxEnum.decl.name
    of cekForward: result = e.cxxForward.decl.name
    of cekObject: result = e.cxxObject.decl.name
    of cekProc: result = e.cxxProc.head.name
    of cekAlias: result = e.cxxAlias.decl.name
    of cekMacro: result = e.cxxMacro.name
    of cekEmpty:
      raise newUnexpectedKindError(e)

    else:
      raise newImplementKindError(e)

func cxxName*(e: CxxEntry): CxxName = e.name.cxx

func getLocation*(e: CxxEntry): CxxSpellingLocation =
  var tmp: Option[CxxSpellingLocation]
  case e.kind:
    of cekEnum:    tmp = e.cxxEnum.spellingLocation
    of cekForward: tmp = e.cxxForward.spellingLocation
    of cekObject:  tmp = e.cxxObject.spellingLocation
    of cekProc:    tmp = e.cxxProc.spellingLocation
    of cekAlias:   tmp = e.cxxAlias.spellingLocation
    of cekEmpty:
      raise newUnexpectedKindError(e)

    else:
      raise newImplementKindError(e)

  assertOption tmp, $e
  return tmp.get

func cxxPair*(
    nim: string, cxx: CxxName,
    context: CxxNameContext = cncNone
  ): CxxNamePair =

  CxxNamePair(nim: nim, cxx: cxx, context: context)

func cxxPair*(
    name: string,
    context: CxxNameContext = cncNone
  ): CxxNamePair =

  cxxPair(name, cxxName(@[name]), context)

func `&`*(n1, n2: CxxName): CxxName =
  CxxName(scopes: n1.scopes & n2.scopes)

func `[]`*(n: CxxName, idx: SliceTypes): CxxName =
  CxxName(scopes: n.scopes[idx.normalizeSlice(n.scopes.len)])

func `[]`*(n: CxxName, idx: IndexTypes): CxxName =
  CxxName(scopes: @[n.scopes[idx.normalizeIndex(n.scopes.len)]])

func lastScope*(n: CxxName): string = n.scopes.last()

func `&`*(p1, p2: CxxNamePair): CxxNamePair =
  CxxNamePair(context: p2.context, nim: p1.nim & p2.nim, cxx: p1.cxx & p2.cxx)

func isConst*(pr: CxxProc): bool = cpfConst in pr.flags
func isStatic*(pr: CxxProc): bool = cpfStatic in pr.flags
func isConstructor*(pr: CxxProc): bool =
  assertRef(pr)
  result = pr.constructorOf.isSome()

func isDestructor*(pr: CxxProc): bool = pr.destructorOf.isSome()
func isMethod*(pr: CxxProc): bool = cpfMethod in pr.flags

func isForward*(pr: CxxProc): bool = pr.head.isForward
func isForward*(ob: CxxObject): bool = ob.decl.isForward
func isForward*(en: CxxEntry): bool =
  case en.kind:
    of cekProc: return en.cxxProc.isForward()
    of cekObject: return en.cxxObject.isForward()
    else: raise newUnexpectedKindError(en)

func isEmpty*(name: CxxName): bool =
  name.scopes.len == 0 or
  (name.scopes.len == 1 and name.scopes[0].len == 0)

func isPOD*(use: CxxTypeUse): bool = ctfIsPodType in use.flags

func add*(t: var CxxTypeUse, other: CxxTypeUse) =
  ## Add new generic parameter to the last identifier type.
  t.types.last().genParams.add other
  t.types.last().genParams.last().flags.incl ctfParam

func getConstructed*(pr: CxxProc): CxxNamePair =
  ## Get name of the object constructed by the procedure
  assertOption(pr.constructorOf)
  pr.constructorOf.get()

func getIcppName*(pr: CxxProc, asMethod: bool = false): string =
  ## Get C++ identifier name joined on namespace separators
  if asMethod:
    pr.cxxName.scopes[^1]

  else:
    pr.cxxName.scopes.join("::")

func cxxDynlib*(dyn: string): CxxBind =
  CxxBind(dynPattern: dyn, kind: cbkDynamicPatt)

func cxxDynlibVar*(name: string): CxxBind =
  CxxBind(dynExpr: name, kind: cbkDynamicExpr)

func cxxDynlibCall*(name: string): CxxBind =
  CxxBind(dynExpr: name, kind: cbkDynamicCall)

func cxxMacroBind*(name: string): CxxBind =
  CxxBind(dynExpr: name, kind: cbkMacroBind)

func cxxHeader*(global: string): CxxBind =
  CxxBind(global: global, kind: cbkGlobal)

func cxxHeader*(file: AbsFile): CxxBind =
  CxxBind(kind: cbkAbsolute, file: file)

func cxxLinkBind*(): CxxBind = CxxBind(kind: cbkLink)

func cxxNoBind*(): CxxBind = CxxBind(kind: cbkNone)
func cxxNotImported*(): CxxBind = CxxBind(kind: cbkNotImported)

func cxxArg*(name: CxxNamePair, argType: CxxTypeUse): CxxArg =
  result = CxxArg(nimType: argType, name: name, haxdocIdent: newJNull())
  result.name.context = cncArg

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
  result = CxxTypeRef(isParam: false, name: name, typeStore: store)
  result.name.context = cncType

func cxxTypeDecl*(
    head: CxxNamePair,
    kind: CxxTypeDeclKind,
    genParams: CxxGenParams = @[]): CxxTypeDecl =
  result = CxxTypeDecl(name: head, genParams: genParams, kind: kind)
  result.name.context = cncType

func cxxTypeUse*(pod: CxxPodTypeKind): CxxTypeUse =
  CxxTypeUse(kind: ctkPod, podKind: pod)

func cxxTypeUse*(
    head: CxxNamePair,
    genParams: seq[CxxTypeUse] = @[],
    store: CxxTypeStore = nil
  ): CxxTypeUse =

  var head = head
  head.context = cncType
  CxxTypeUse(
    kind: ctkIdent, types: @[(
      cxxType: cxxTypeRef(head, store),
      genParams: @genParams)])

func cxxTypeParams*(decl: CxxTypeDecl): seq[CxxTypeUse] =
  for (param, default) in decl.genParams:
    if default.isSome():
      result.add default.get()

    else:
      result.add cxxTypeUse(param)


func cxxTypeUse*(
    objDef: CxxObject, parent, user: CxxNamePair
  ): CxxTypeUse =
  CxxTypeUse(
    kind: ctkAnonObject,
    objDef: objDef,
    objParent: parent,
    objUser: user
  )


func cxxTypeUse*(
    enumDef: CxxEnum, parent, user: CxxNamePair
  ): CxxTypeUse =
  CxxTypeUse(
    kind: ctkAnonEnum, enumDef: enumDef,
    enumParent: parent, enumUser: user)

func box*(en: CxxEnum): CxxEntry = CxxEntry(kind: cekEnum, cxxEnum: en)
func box*(en: CxxForward): CxxEntry =
  CxxEntry(kind: cekForward, cxxForward: en)
func box*(ob: CxxObject): CxxEntry = CxxEntry(kind: cekObject, cxxObject: ob)
func box*(en: CxxProc): CxxEntry = CxxEntry(kind: cekProc, cxxProc: en)
func box*(en: CxxAlias): CxxEntry = CxxEntry(kind: cekAlias, cxxAlias: en)
func box*(en: CxxMacro): CxxEntry = CxxEntry(kind: cekMacro, cxxMacro: en)


func toDecl*(use: CxxTypeUse, kind: CxxTypeDeclKind): CxxTypeDecl =
  ## Construct type declaration using type identifier. Use the latest
  ## element of the fully scoped type identifier.
  assertKind(use, {ctkIdent})
  return cxxTypeDecl(use.cxxType.name, kind)

func addDecl*(store: var CxxTypeStore, decl: CxxTypeDecl) =
  store.forwardDecls.del(decl.name.cxx)
  store.typeDecls.mgetOrPut(decl.name.cxx, @[]).add decl

func addDecl*(store: var CxxTypeStore, decl: CxxObject) =
  store.addDecl(decl.decl)
  store.classDecls.mgetOrPut(decl.decl.name.cxx, @[]).add box(decl)

func addForwardDecl*(store: var CxxTypeStore, decl: CxxTypeDecl) =
  store.forwardDecls.mgetOrPut(decl.name.cxx, @[]).add decl

func hasFullDecl*(store: CxxTypestore, decl: CxxTypeDecl): bool =
  decl.cxxName() in store.typeDecls

func getDecl*(
    store: CxxTypeStore,
    name: CxxName,
    lib: Option[string]
  ): Option[CxxTypeDecl] =

  assertRef store
  if name in store.typeDecls:
    for decl in store.typeDecls[name]:
      # No library for import, or no library for the type declaration
      if lib.isNone() or decl.typeImport.isNone():
        result = some decl
        break

      else:
        # Has library name for both type declaration and use
        if decl.typeImport.get().library == lib.get():
          result = some decl
          break


  elif name in store.forwardDecls:
    result = some store.forwardDecls[name][0]

func getTypeImpls*(
    store: CxxTypeStore,
    name: CxxName,
    lib: Option[string]  = none(string),
    kinds: set[CxxEntryKind] = { low(CxxEntryKind) .. high(CxxEntryKind) }
  ): seq[CxxEntry] =
  ## Get all type definitions matching type

  assertRef store
  if name in store.classDecls:
    for class in store.classDecls[name]:
      {.warning: "Check for type's library".}
      result.add class
      # # No library for import, or no library for the type declaration
      # if lib.isNone() or class.decl.typeImport.isNone():
      #   return some class

      # else:
      #   # Has library name for both type declaration and use
      #   if class.decl.typeImport.get().library == lib.get():
      #     return some class


func getObject*(
    store: CxxTypeStore,
    name: CxxName,
    lib: Option[string]
  ): Option[CxxObject] =

  assertRef store
  if name in store.classDecls:
    for class in store.classDecls[name]:
      if class of cekObject:
        let class = class.cxxObject
        # No library for import, or no library for the type declaration
        if lib.isNone() or class.decl.typeImport.isNone():
          return some class

        else:
          # Has library name for both type declaration and use
          if class.decl.typeImport.get().library == lib.get():
            return some class

func getDecl*(use: CxxTypeUse): Option[CxxTypeDecl] =
  ## Get first type declaration with matching cxx name. In case of multiple
  ## identical types present they are disambiguated based on the library
  ## name if possible.
  assertKind(use, {ctkIdent})
  assertRef use.cxxType.typeStore
  return use.cxxType.typeStore.getDecl(
    use.cxxName(), use.cxxType.typeLib)

func hasExternalImport*(use: CxxTypeuse): bool =
  if use of ctkIdent:
    let store = use.cxxType.typeStore
    result = use.cxxName() in store.importDecls

func getExternalImport*(use: CxxTypeUse): CxxLibImport =
  assertKind(use, ctkIdent)
  use.cxxType.typeStore.importDecls.getOr(
    use.cxxName(), "missing mapping for type in external import list")

func hasImport*(use: CxxTypeUse): bool =
  if use of ctkIdent:
    let decl = use.getDecl()
    result = decl.get().typeImport.isSome()

  else:
    result = false

func getImport*(use: CxxTypeUse): CxxLibImport =
  use.getDecl().getOr("cannot determine declaration for type use").
    typeImport.getOr("missing import for type declarations")

func hasImport*(decl: CxxTypeDecl): bool =
  decl.typeImport.isSome()

func hasAnyDecl*(use: CxxTypeUse): bool =
  use.getDecl().isSome()

func hasFullDecl*(decl: CxxTypeDecl): bool =
  assertRef(decl.store)
  decl.name.cxx in decl.store.typeDecls


func cxxLibImport*(library: string, path: seq[string]): CxxLibImport =
  for item in path:
    if item.len == 0:
      raise newArgumentError("Lib import path cannot contain elements of length 0")

  CxxLibImport(library: library, importPath: path)

func getImport*(decl: CxxTypeDecl): CxxLibImport = decl.typeImport.get()
func getLibrary*(imp: CxxLibImport): string = imp.library
func getLibrary*(file: CxxFile): string = file.savePath.library
func getFilename*(limport: CxxLibImport): string =
  result = limport.importPath[^1]
  let idx = result.find('.')
  if idx != -1:
    result = result[0 ..< idx]

func getPathNoExt*(limport: CxxLibImport): seq[string] =
  result.add limport.importPath[0..^2]
  let tmp = limport.importPath[^1]
  let idx = tmp.find('.')
  if idx != -1:
    result.add tmp[0 ..< idx]

  else:
    result.add tmp


func getFile*(lib: CxxLibImport): RelFile =
  assertHasIdx(lib.importPath, 0)
  result = RelFile(lib.importPath.join("/"))

func getFile*(file: CxxFile): RelFile = file.savePath.getFile()

func getFilename*(file: CxxFile): string = file.savePath.getFilename()

func getType*(arg: CxxArg): CxxTypeUse = arg.nimType
func getType*(field: CxxField): CxxTypeUse = field.nimType

func add*(file: var CxxFile, entry: CxxEntry) =
  file.entries.add entry

func addImport*(file: var CxxFile, cimport: CxxLibImport) =
  file.imports.incl cimport

func addExport*(file: var CxxFile, cexport: CxxLibImport) =
  file.exports.incl cexport

func addReExport*(file: var CxxFile, cimport: CxxLibImport) =
  addImport(file, cimport)
  addExport(file, cimport)

# proc eachIdent*(use: var CxxTypeUse, cb: proc(ident: var CxxTypeUse))
# proc eachIdent*(use: CxxTypeUse, cb: proc(ident: CxxTypeUse))

template eachKindAux(inUse, cb, iterateWith: untyped, target: set[CxxTypeKind]) =
  ## Iterate over each subtype in the `inUse` type, using `iterateWith`
  ## call (must be either `mitems` or `items`). If called on type that is
  ## in `target` set, execute callback `cb`
  if inUse.kind in target:
    cb(inUse)

  case inUse.kind:
    of ctkAnonEnum:
      discard

    of ctkAnonObject:
      for field in iterateWith(inUse.objDef.mfields):
        eachKind(field.nimType, target, cb)

    of ctkWrapKinds: eachKind(inUse.wrapped, target, cb)
    of ctkStaticParam, ctkPod, ctkDecltype: discard
    of ctkArrayKinds: eachKind(inUse.arrayElement, target, cb)
    of ctkIdent:
      for typ in iterateWith(inUse.types):
        for param in iterateWith(typ.genParams):
          eachKind(param, target, cb)

    of ctkProc:
      for arg in iterateWith(inUse.arguments):
        eachKind(arg.nimType, target, cb)

      eachKind(inUse.returnType, target, cb)

proc eachKind*(
    use: var CxxTypeUse,
    target: set[CxxTypeKind],
    cb: proc(ident: var CxxTypeUse)
  ) =

  if isNil(use): return
  eachKindAux(use, cb, mitems, target)

proc eachKind*(
    use: CxxTypeUse,
    target: set[CxxTypeKind],
    cb: proc(ident: CxxTypeUse)
  ) =

  if isNil(use): return
  eachKindAux(use, cb, items, target)

proc getUsedTypesRec*(
    t: CxxTypeUse, ignoreHead: bool = false): seq[CxxTypeUse] =
  var res: seq[CxxTypeUse]
  if not ignoreHead:
    res.add t

  eachKind(t, {ctkIdent}) do(t: CxxTypeUse):
    if notNil(t): res.add t
    res.add getUsedTypesRec(t, ignoreHead = false)

  return res

func `icpp=`*(pr: var CxxProc, icpp: IcppPattern) = pr.cbind.icpp = icpp

func getIcpp*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): IcppPattern =
  assertHasIdx(pr.cbind.icpp, 0)
  return pr.cbind.icpp


func getIcppStr*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): string =
  $getIcpp(pr, onConstructor)


func getIcpp*(pr: CxxObject | CxxForward): IcppPattern =
  if pr.cbind.icpp.len > 0:
    return pr.cbind.icpp

  else:
    result.ctype(pr.cxxName.scopes.join("::"))

func getIcppStr*(pr: CxxObject | CxxForward): string = $getIcpp(pr)

func initIcpp*(
    pr: var CxxProc, onConstructor: CxxTypeKind = ctkIdent) =
  pr.cbind.icpp = getIcpp(pr, onConstructor)


func cxxTypeUse*(
    arguments: seq[CxxArg], returnType: CxxTypeUse): CxxTypeUse =
  CxxTypeUse(
    kind: ctkProc, arguments: arguments, returnType: returnType)

func cxxTypeUse*(decl: CxxTypeDecl, store: CxxTypeStore = nil): CxxTypeUse =
  CxxTypeUse(
    kind: ctkIdent,
    types: @[(
      cxxType: cxxTypeRef(decl.name, store),
      genParams: cxxTypeParams(decl))])

func cxxTypeUse*(name: string, args: seq[CxxTypeUse]): CxxTypeUse =
  cxxTypeUse(cxxPair(name), args)

func cxxObject*(name: CxxNamePair, genParams: CxxGenParams = @[]): CxxObject =
  CxxObject(
    decl: cxxTypeDecl(name, ctdkStruct, genParams),
    haxdocIdent: newJNull())

func cxxForward*(name: CxxNamePair, kind: CxxTypeDeclKind): CxxForward =
  result = CxxForward(
    decl: cxxTypeDecl(name, kind, @[]), haxdocIdent: newJNull())

  result.decl.isForward = true

func cxxEnum*(name: CxxNamePair): CxxEnum =
  CxxEnum(decl: cxxTypeDecl(name, ctdkEnum), haxdocIdent: newJNull())


func cxxContext*(name: sink CxxNamePair, ctx: CxxNameContext): CxxNamePair =
  result = name
  result.context = ctx

func cxxContext*(name: sink CxxTypeDecl, ctx: CxxNameContext): CxxTypeDecl =
  result = name
  result.name.context = ctx


func cxxField*(name: CxxNamePair, nimType: CxxTypeUse): CxxField =
  CxxField(
    name: name.cxxContext(cncField),
    nimType: nimType, haxdocIdent: newJNull())

func cxxAlias*(decl: CxxTypeDecl, baseType: CxxTypeUse): CxxAlias =
  CxxAlias(decl: decl, baseType: baseType, haxdocIdent: newJNull())

func cxxEmpty*(): CxxEntry = CxxEntry(kind: cekEmpty)

func cxxEnumValue*(name: CxxNamePair, value: BiggestInt): CxxEnumValue =
  result = CxxEnumValue(name: name, value: value)
  result.name.context = cncEnumField

func cxxComment*(com: string): CxxComment =
  CxxComment(text: com)


func cxxProc*(
    name: CxxNamePair,
    arguments: seq[CxxArg] = @[],
    returnType: CxxTypeUse = cxxTypeUse(cxxPair"void"),
    genParams: CxxGenParams = @[]
  ): CxxProc =

  CxxProc(
    head: cxxTypeDecl(name, ctdkProc, genParams).cxxContext(cncProc),
    haxdocIdent: newJNull(),
    returnType: returnType,
    arguments: arguments
  )

func cxxMacro*(name: CxxNamePair): CxxMacro =
  CxxMacro(name: name, haxdocIdent: newJNull())

func cxxFile*(
    entries: seq[CxxEntry], path: CxxLibImport, original: AbsFile): CxxFile =
  CxxFile(savePath: path, entries: entries, original: original)

func add*(pr: var CxxProc, arg: CxxArg) =
  pr.arguments.add arg

func setCxxBind*(target: var CxxBind, source: CxxBind) =
  let icpp = target.icpp
  target = source
  target.icpp = icpp





proc setStoreRec*(entry: var CxxEntry, store: CxxTypeStore) =
  proc aux(use: var CxxTypeUse)

  proc aux(def: var CxxObject) =
    def.decl.store = store

    for nest in mitems(def.nested):
      setStoreRec(nest, store)

    for meth in mitems(def.methods):
      for arg in mitems(meth.arguments):
        aux(arg.nimType)

      aux(meth.returnType)

    for field in mitems(def.mfields):
      aux(field.nimType)

  proc aux(def: var CxxEnum) =
    def.decl.store = store

  proc aux(use: var CxxTypeUse) =
    eachKind(use, {
      ctkIdent, ctkAnonObject, ctkAnonEnum
    }) do (use: var CxxTypeUse):
      case use.kind:
        of ctkIdent:
          for typ in mitems(use.types):
            typ.cxxType.typeStore = store

        of ctkAnonObject:
          aux(use.objDef)

        of ctkAnonEnum:
          aux(use.enumDef)
        else: discard

  case entry.kind:
    of cekPass, cekEmpty, cekImport, cekMacroGroup,
       cekMacro, cekComment:
      discard

    of cekProc:
      entry.cxxProc.head.store = store
      for arg in mitems(entry.cxxProc.arguments):
        aux(arg.nimType)

      aux(entry.cxxProc.returnType)

    of cekTypeGroup:
      for decl in mitems(entry.cxxTypes):
        setStoreRec(decl, store)

    of cekEnum:
      aux(entry.cxxEnum)

    of cekForward:
      entry.cxxForward.decl.store = store

    of cekAlias:
      entry.cxxAlias.decl.store = store
      aux(entry.cxxAlias.baseType)

    of cekObject:
      aux(entry.cxxObject)

template setFile(entry: typed, file: AbsFile): untyped =
  if entry.spellingLocation.isSome:
    entry.spellingLocation.get().file = file

  else:
    entry.spellingLocation = some CxxSpellingLocation(file: file)

proc setFileRec*(entry: var CxxEntry, file: AbsFile) =
  case entry.kind:
    of cekPass, cekImport, cekEmpty, cekComment, cekMacro:
      discard

    of cekMacroGroup:
      for it in mitems(entry.cxxMacroGroup.macros):
        setFileRec(it, file)

    of cekTypeGroup:
      raise newImplementKindError(entry)

    of cekForward: entry.cxxForward.setFile(file)
    of cekEnum: entry.cxxEnum.setFile(file)
    of cekProc: entry.cxxProc.setFile(file)
    of cekAlias: entry.cxxAlias.setFile(file)
    of cekObject:
      entry.cxxObject.setFile(file)
      for meth in mitems(entry.cxxObject.methods):
        meth.setFile(file)

      for nest in mitems(entry.cxxObject.nested):
        setFileRec(nest, file)



func incl[A](s: var HashSet[A], its: seq[A]) =
  for i in its:
    s.incl i

func getBindImports*(file: CxxFile): HashSet[CxxLibImport] =

  func aux(entry: CxxEntry, res: var HashSet[CxxLibImport]) =
    case entry.kind:
      of cekPass, cekImport, cekEmpty,
         cekComment, cekMacroGroup, cekMacro:
        discard

      of cekTypeGroup:
        raise newImplementKindError(entry)

      of cekForward: res.incl entry.cxxForward.cbind.imports
      of cekEnum: res.incl entry.cxxEnum.cbind.imports
      of cekProc: res.incl entry.cxxProc.cbind.imports
      of cekAlias: res.incl entry.cxxAlias.cbind.imports
      of cekObject:
        res.incl entry.cxxObject.cbind.imports
        for meth in mitems(entry.cxxObject.methods):
          res.incl meth.cbind.imports

        for nest in mitems(entry.cxxObject.nested):
          aux(nest, res)

  for entry in file.entries:
    aux(entry, result)


func newTypeStore*(): CxxTypeStore = CxxTypeStore()

# func registerTypeDeclarations*()

func getSuperTypes*(store: CxxTypeStore, decl: CxxTypeDecl): seq[CxxObject] =
  var outset: OrderedTable[CxxName, CxxObject]
  var stack: seq[CxxName] = @[decl.cxxName()]

  while len(stack) > 0:
    let top = stack.pop()

    let decl = store.getObject(top, none string)
    if ?decl:
      outset[top] = decl.get()
      for super in decl.get().super:
        stack.add super.cxxName()

  return

  {.cast(noSideEffect).}:
    # asdf = true
    let name = decl.cxxName()
    echov "!!!!!!!!!!!!!!!!!!!!!!!!!!11"
    echov repr outset
    echov "ended repr"
    echov outset
    if name in outset:
      outset.del name
    echov "deleted"
    # asdf = false

  for key, val in outset:
    result.add val




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

func add*[
  E: CxxMacro | CxxAlias | CxxObject | CxxForward |
     CxxProc | CxxEnum | CxxEnumValue | CxxField |
     CxxArg
  ](
    s: var E, comment: CxxComment | seq[CxxComment]) =
  s.docComment.add comment

func add*(
    s: var seq[CxxEntry],
    other: CxxMacro | CxxAlias | CxxObject | CxxForward | CxxProc | CxxEnum |
           CxxObject
  ) =

  s.add box(other)

proc getTypeStore*(entry: CxxEntry): CxxTypeStore =
  case entry.kind:
    of cekEnum:    result = entry.cxxEnum.decl.store
    of cekForward: result = entry.cxxForward.decl.store
    of cekObject:  result = entry.cxxObject.decl.store
    of cekProc:    result = entry.cxxProc.head.store
    of cekAlias:   result = entry.cxxAlias.decl.store
    of cekEmpty:
      raise newUnexpectedKindError(entry)

    else:
      raise newImplementKindError(entry)

  assertRef(result, kindToStr(entry))


proc getTypeStore*(entries: seq[CxxEntry]): CxxTypeStore =
  for entry in entries:
    if entry of cekTypeStored:
      return entry.getTypeStore()

  raise newGetterError(
    "Cannot get type store from the list of entries ",
    "- no matching kinds found")

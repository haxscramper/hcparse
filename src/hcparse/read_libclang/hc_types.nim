import ./cxtypes, ./cxcommon
import
  std/[
    tables,
    sets,
    strutils,
    sequtils,
    hashes,
    strformat,
    macros,
    segfaults,
    parseutils
  ]

import hpprint

import
  hmisc/core/[
    all
  ],
  hmisc/other/[
    oswrap,
    hlogger,
    hjson,
    hshell
  ],
  hmisc/types/[
    hmap,
    hgraph
  ],
  hmisc/algo/[
    hseq_mapping,
    hstring_algo,
    namegen,
    halgorithm
  ]



import
  hnimast,
  hnimast/pprint


import
  ../processor/[wrap_store, wrap_icpp]


type
  DepResolutionKind* = enum
    ## Kind of cursor dependency resolution
    drkIgnoreIfUsed ## Ignore dependency
    drkWrapDirectly ## Wrap dependency in main generated wrappers file
    drkImportUses ## Assume dependency is wrapped in other module, and
    ## generate `import` for it.


  CTypeSpecialKind* = enum
    ## Special kind of C++ types that are almost impossbile to correctly
    ## convert to nim and preserve semantics across language barrier
    ctskNone
    ctskLValueRef
    ctskRvalueRef ## Rvalue reference
    ctskConstLvalueRef ## `const value&` is basically nim's default passing
    ## behavior for 'large enough' types, but it has a different behavior
    ## with `{.bycopy.}` objects (which is what any C++ type uses)


  NimType* = ref object
    ## C++ type converter to nim-/like/ representation. Due to differences
    ## in type system (type-level immutability properties in C++ as opposed
    ## to variable-level in NIM) additional layer of indirection was added.
    ## First `CXType` is converter to `NimType`, and then to
    ## `NType[PNode]`.
    specialKind*: CTypeSpecialKind
    isMutable*: bool ## Original type was mutable (mutable reference)
    isConst*: bool ## Original type was immutable (const or const
                   ## reference)
    isParam*: bool ## Type is used as generic parameter for other
                   ## types/procedure arguments.

    isComplex*: bool
    fullIdent*: Option[CScopedIdent] ## Full identifier to Cxx type
                                     ## declaration.

    case fromCXType*: bool
      of true:
        cxType*: CXType

      of false:
        ## Entry was automatically generated or constructed from invalid
        ## CxType.
        typeImport*: CxxLibImport
        original*: Option[CxType]


    case kind*: CxxTypeKind
      of ctkPod:
        podKind*: CxxPodTypeKind

      of ctkIdent:
        defaultType*: Option[NimType] ## Default type value. Used in
                                      ## template type parameters
        nimName*: string ## Converted nim name
        genParams*: seq[NimType] ## Optional list of generic parameters

      of ctkProc:
        arguments*: seq[CArg]
        returnType*: NimType

      of ctkWrapKinds:
        wrapped*: NimType

      of ctkStaticParam:
        param*: CXCursor

      of ctkArrayKinds:
        arraySize*: CXCursor
        arrayElement*: NimType

      of ctkAnonObject, ctkAnonEnum:
        discard


  CDeclKind* = enum
    ## Raw C declaration kind
    cdkClass
    cdkStruct
    cdkUnion
    cdkEnum
    cdkFunction
    cdkMethod ## Object methor or operator
    cdkField ## any kind of field
    cdkAlias ## `typedef` or `using`
    cdkMacro ## Any macro-related entry (instantiation, expansion)
    cdkForward ## Forward declared enum/class/struct/union

  CArg* = object
    name*: string ## Converted argument name
    case isRaw*: bool ## Points to existing entry
      of true:
        cursor*: CXCursor ## Raw Cxx cursor

      of false: ## Either generated from raw cursor or constructed anew
        varkind*: NVarDeclKind ## Kind of the argument (`var`, `lent`, `sink` etc)
        nimType*: NimType ## Argument type
        default*: Option[PNode] ## Optional default value for an argument


  CName* = object
    ## Single element of scoped C++ identifier like `std` or `vector` in
    ## `std::vector`.
    ##
    ## `cursor` points to original AST node from which element was
    ## extracted, `genParams` is a (possibly empty) list of generic
    ## parameters for given name. `nimType` is a resulting nim type created
    ## from `cursor`.
    case isGenerated*: bool ## Corresponds to existing entry or generated
                            ## anew
      of false:
        cursor*: CXCursor ## Name declaration cursor

      of true:
        name*: string

    genParams*: seq[CScopedIdent]

  CScopedIdent* = seq[CName] ## Full scoped C/C++ identifier like
                             ## `std::vector<int>::iterator`
  ParentDecl* = object
    derived*: seq[CDecl]
    cursor*: CXCursor

  CDecl* = ref object
    ## Higher-level wrapper on top of CXCursor. Mostly used to provide more
    ## intuitive API for working with things to be wrapped.

    ident* {.requiresinit.}: CScopedIdent ## Fully qualified identifier
    ## (except namespaces that were explicitly marked as 'collapsible' in
    ## wrap configuration)

    cursor*: CXCursor ## Cursor pointing to main declaration entry
    access*: CXAccessSpecifier ## Access specifier for nested class elements.
    genericParameters*: seq[CXCursor]
    genericConstraints*: seq[CXCursor]
    docComment*: seq[string] ## Documentation comment collected from one or
    ## more sources (inclusing function documentation comments)

    isCTypedef*: bool ## Declaration was introduced by `typedef struct {} T`,
    ## or by `struct T {}`? In latter case C wrappers must use `struct T` for
    ## type wrappers.
    ##
    ## Type declarations introduced using `typedef struct {} T, *PtrT` are
    ## represented by two `CDecl` entries - base enum declaration, and type
    ## alias for `PtrT = *T`.

    isConst*: bool ## Field or method declared as `const`
    isAnonymous*: bool

    case kind*: CDeclKind
      of cdkField:
        fieldValue*: Option[CXCursor] ## Cursor to field value if
                                      ## immediately declared
        fieldTypeDecl*: Option[CDecl] ## Anonymous enum/struct declaration
        ## It is possible to have entries like
        ## `struct A{enum{first,second}field;};`
        ## that don't declare any type.

      of cdkMethod, cdkFunction:
        complexTemplate*: bool
        arguments*: seq[CArg] ## Method or function argumets
        returnType*: Option[CXType] ## Optional return type
        case isOperator*: bool
          of true:
            operatorName*: string ## Name with dropped `operator` prefix
            operatorKind*: CxxProcKind ## Operator classification

          of false:
            discard

      of cdkClass, cdkStruct, cdkUnion:
        isDefaultConstructible*: bool ## Type does not have deleted default
                                      ## constructor
        isCopyable*: bool ## Type can be copied (does not have copy
                          ## constructor deleted)
        icpp*: string
        case isAggregateInit*: bool ## Is type subject to aggregate
                                    ## initalization?
          of true:
            initArgs*: seq[CArg] ## Arguments for aggregate initailization

          else:
            discard

        parentDecls*: seq[ParentDecl]

        nestedTypes*: seq[CDecl] ## Nested struct/union/class/typedef
                                 ## declarations
        members*: seq[CDecl] ## List of public fields and methods
        ## that were defined in the object

      of cdkEnum:
        isClassEnum*: bool ## C++ `class enum`, or old-style `C` enum?
        enumFields*: seq[tuple[
          field: CXCursor,
          value: clonglong
        ]]

      of cdkAlias:
        ## Type alias declaration (`typedef` or `using`).
        ##
        ## To correctly process various C-style abominations like
        ## `typedef ## struct T {} Name2, *Ptr` it is necessary to:
        ## - Have multiple `newType` values  - `Name2`

        newTypes*: seq[CXCursor] ## List of new type names introduced by
                                 ## typedef.
        case isNewType*: bool
          of true:
            ## Typedef contained new type declaration

            withBaseType*: bool ## no `struct T` declaration is present -
            ## `typedef` was immediately used to declare type.

            aliasNewType*: CDecl ## New type declaration introduced by
            ## C-style `typedef`

          of false:
            ## Regular typedef declaration `alias = base`

            aliasBaseType*: CXCursor ## Base type used for alias
            ## declaration

            # genParams*: seq[CxType]


      of cdkMacro:
        discard

      of cdkForward:
        ## Forward declaration contains scoped name for encountered
        ## identifier and cursor pointing to declaration.
        discard


  CApiUnit* = object
    ## Representation of single API unit - all public
    ## methods/classes/fields/functions declared in main file of
    ## single translation unit.
    ##
    ## ## Fields
    ## :decls: List of declarations in main file
    ## :publicTypes: List of public entries exposed by the API.
    ##
    ##     Class fields, function/method arguments/return values and
    ##     so on. This list allows you to determine whether or not
    ##     wrapping additional API unit & including them in main file
    ##     is necessary.

    # TODO infer 'derived' API that must also be acessible through
    # object - things like public fields and methods of parent class.
    decls*: seq[CDecl]

  ParsedFile* = object
    unit*: CXTranslationUnit ## Translation unit
    filename*: AbsFile ## Name of the original file
    api*: CApiUnit ## File's API
    index*: CXIndex ## Liblcang index object
    explicitDeps*: seq[AbsFile] ## Filenames in which types exposed in
    ## API are declared. Guaranteed to have every file listed once &
    ## no self-dependencies.
    isExplicitlyAdded*: bool ## Filename has been explicitly listed in
    ## original files for wrapping, or was added as dependency
    ## (/transtitive dependency) for other file?

  ParseConf* = object
    globalFlags*: seq[string] ## List of parse flags applied on each
    ## file parse. Mostly for things like include paths.
    fileFlags*: Table[AbsFile, seq[string]] ## List of parse flags
    ## specific only to particular file

    includepaths*: seq[AbsDir] ## List of absolute include directoires. `-I` in Cxx compilers

    sysIncludes*: seq[string]
    userIncludes*: seq[string]

    macroDefs*: seq[
      tuple[name: string, args: seq[string], impl: Option[string]]
    ]

  NimHeaderSpec* = object
    ## Configuration for `>header.` generation
    case kind*: CxxBindKind
      of cbkGlobal:
        global*: string ## Global include like `<string>`

      of cbkAbsolute:
        file*: AbsFile ## Absolute path to header file

      of cbkPNode:
        pnode*: PNode ## Anything else

      else:
        discard

  NimImportSpec* = object
    ## Configuration for import of the other files
    importPath*: seq[string] ## Name of the imported file
    case isRelative*: bool ## Import should be performed using relative paths
      ## or absolute.
      of true:
        relativeDepth*: int ## `0` means relative to current file, `./`.
        ## Any number greater than `0` is converted to equal number of
        ## directory-up `..`

      else:
        discard



  DoxRefid* = object
    refid*: string
    name*: string
    line*: int
    column*: int

  RefidMap* = object
    map*: Table[string, Map[int, seq[DoxRefid]]]

  WrapConf* = ref object
    ## Confuration for wrapping.

    logger*: HLogger
    header*: AbsFile ## Current main translation file (header)

    unit*: CXTranslationUnit
    refidMap*: RefidMap

    # REFACTOR return CxxHeader spec directly
    makeHeader*: proc(cursor: CXCursor, conf: WrapConf): NimHeaderSpec ## |
    ## Generate identifier for `{.header: ... .}`

    # REFACTOR move from clang-specific wrap configuration to one that
    # would be used for CxxT results
    fixTypeName*: proc(ntype: var NimType, conf: WrapConf, idx: int)
    ## Change type name for `ntype`.
    ##
    ## First argument is a type to be fixed, second one is parent
    ## configuration type. Third argument is mostly used for internal
    ## purposes - index of the generic argument. For cases like `[__T,
    ## _T]`, where both types should be mapped to `T` you can make `T` and
    ## `T1` respectively, using value provided by `idx`

    overrideComplex*: proc(
      cxType: CxType, conf: WrapConf, cache: var WrapCache): Option[NimType]
    ## Hard override generated complex types

    # QUESTION is this needed right now?
    getSavePathImpl*: proc(orig: AbsFile, conf: WrapConf): CxxLibImport ## Return
    ## path, *relative to project root* (@field{nimOutDir}) where file
    ## generated from `orig` should be saved.

    ignoreCursor*: proc(curs: CXCursor, conf: WrapConf): bool ## User-defined
    ## predicate for determining whether or not cursor should be
    ## considered a part of api. Things like `internal` namespaces.

    collapsibleNamespaces*: seq[string] ## List of namespaces that would be
                                        ## ignored during name construction
    ignoreFile*: proc(file: AbsFile): bool

    isTypeInternal*: proc(cxt: CXType, conf: WrapConf): bool
    depResolver*: proc(cursor, referencedBy: CXCursor): DepResolutionKind

    isImportcpp*: bool ## Is wrapped code a C++ or C?
    parseConf*: ParseConf

    wrapName*: string


  WrapEntryPosition = object
    ## Approximate locatino of the wrapped entry declaration
    file*: AbsFile # FIXME replace with CxxLibImport
    line*: int
    column*: int

  TypeUseKind* = enum
    tukInProcs
    tukInTypes

  WrapCache* = object
    # FIXME add type store
    hset*: HashSet[Hash]
    visited*: HashSet[cuint]
    enumPrefs*: HashSet[string] ## List of used enum prefixes (to avoid
                                ## duplication)
    identComments*: Table[CScopedIdent, seq[string]] ## Mapping between
    ## fully scoped identifiers and documentation comments
    identRefidMap*: seq[tuple[cxx: CScopedIdent, position: CxxSpellingLocation]] ## list
    ## of fully scoped identifiers and corresponding wrapped entry
    ## positions. Used by haxdoc&doxygen to link doxygen processed sources
    ## and actual wrapped entries.
    nameCache*: StringNameCache ## Generated name cache

    paramsForType*: Table[seq[string], seq[NimType]] ## Generic
    ## parameters for each type. Type is uniquely represented using
    ## `fully::scoped::ident`.
    nimParamsForType*: Table[string, seq[NimType]]

    generatedConstructors*: HashSet[string]

    complexCache*: Table[CxCursor, Option[NimType]]

  GenPragmaConf* = enum
    gpcNoImportcpp
    gpcNoHeader

proc newWrapCache*(): WrapCache = WrapCache()

WrapConf.loggerField(logger, doExport = true)
const gpcNoPragma* = { gpcNoHeader, gpcNoImportcpp }

func dropTemplateArgs*(old: string): string =
  result = old[
    old.skip1(toStrPart(["const ", "enum ", "struct ", "union "])) ..<
    old.skipUntil('<')]

  var start = result.high
  if start == old.high:
    return

  else:
    inc start

    let other = old[start .. ^1]
    var
      `<cnt` = 0
      `>cnt` = 0

    for ch in old:
      if ch == '<': inc `<cnt`
      if ch == '>': inc `>cnt`


    if   `<cnt` - 2 == `>cnt`: result &= tern(other["<<="], "<<=", "<<")
    elif `<cnt` - 1 == `>cnt`: result &= tern(other["<="],  "<=",  "<")
    elif `<cnt`     == `>cnt`: result &= tern(other["<=>"], "<=>", "")
    elif `<cnt` + 1 == `>cnt`: result &= tern(other[">="],  ">=",  ">")
    elif `<cnt` + 2 == `>cnt`: result &= tern(other[">>="], ">>=", ">>")
    else: assert false

proc getName*(c: CxCursor): string = dropTemplateArgs($c)

proc getName*(cn: CName): string =
  if cn.isGenerated:
    cn.name

  else:
    getName(cn.cursor)


proc getSemanticNamespaces*(
    conf: WrapConf, parent: CXCursor, filterInline: bool = true, withType: bool = true
  ): seq[CXCursor] =
  ## Get list of semantic namespaces enclosing cursor.
  ##
  ## - @arg{filterInline} :: Drop inline namespaces if encountered
  ## - @arg{withType} :: Include original cursor in the declaration list

  if withType:
    result.add parent

  if parent.cxKind != ckNoDeclFound:
    var parent = parent
    parent = parent.getCursorSemanticParent()

    while parent.cxKind() in ckTypeDeclKinds + {ckNamespace}:
      # TEST might be necessary to add templated namespacess
      if filterInline and (parent.isInlineNamespace() == 1):
        discard

      else:
        result.add parent

      parent = parent.getCursorSemanticParent()

    reverse(result)




proc getTypeNamespaces*(
    conf: WrapConf, cxtype: CXType, filterInline: bool = true, withType: bool = true
  ): seq[CXCursor] =
  ## Return list of parent namespaces for given type `cxtype`.
  ##
  ## - @arg{filterInline} - remove namespaces that are marked as `inline`.
  ## - @arg{withType} - return type name too, or only namespaces.

  var parent = cxtype.getTypeDeclaration()

  result = conf.getSemanticNamespaces(
    parent, filterInline = filterInline, withType = withType)

proc findSemParent*(conf: WrapConf, cxType: CxType, kind: set[CxCursorKind]): CxCursor =
  rfindByKind(conf.getTypeNamespaces(cxType), kind)[1]

proc findSemParentFull*(
    conf: WrapConf,
    cxType: CxType, kind: set[CxCursorKind]): seq[CxCursor] =

  let names = conf.getTypeNamespaces(cxType)
  names[0 .. rfindByKind(names, kind)[0]]

proc requiredGenericParams*(cursor: CXCursor): seq[CXCursor] =
  ## Get list of required generic parameters from cursor pointing to
  ## class or struct declaration
  for subn in cursor:
    if subn.cxKind in {
      ckTemplateTemplateParameter,
      ckTemplateTypeParameter
    }:
      if subn.len > 0:
        # WARNING Just drop all template parameters that are not
        # simply `T`.
        discard

      else:
        result.add subn # WARNING blow up on `a<b>`


proc toCppNamespace*(
    ns: CScopedIdent,
    withGenerics: bool = true,
    withNames: bool = false
  ): string =

  ## Generate `importcpp` pattern for scoped identifier
  var buf: seq[string]
  var genIdx: int = 0
  for part in ns:
    if withGenerics and part.genParams.len > 0:
      var genTypes: seq[string]
      for param in part.genParams:
        if withNames:
          genTypes.add toCppNamespace(
            param, withGenerics, withNames)

        else:
          genTypes.add "'" & $genIdx
          inc genIdx

      buf.add part.getName() & "<" & genTypes.join(", ") & ">"

    else:
      buf.add part.getName()

  result = buf.join("::")

proc toHaxdocType*(conf: WrapConf, cxtype: CXType): JsonNode =
  case cxtype.cxKind():
    of tkBool, tkInt, tkVoid, tkUInt, tkLongLong, tkULongLong,
       tkDouble, tkULong, tkUChar, tkChar16, tkChar32, tkWChar,
       tkChar_S, tkLong, tkUShort, tkNullPtr, tkFloat, tkLongDouble,
       tkShort, tkSChar:
      result = %{"kind": %"Ident", "name": %($cxtype)}

    of tkPointer:
      result = %{
        "kind": %"Ident",
        "name": %"ptr",
        "genParms": %[conf.toHaxdocType(cxtype[])]
      }

    of tkElaborated, tkRecord, tkEnum:
      var spaces = newJArray()
      for name in conf.getTypeNamespaces(cxType, withType = false):
        spaces.add %($name)

      result = %{
        "kind": %"Ident",
        "namespaces": spaces,
        "name": %($cxtype)
      }
      # result = %{"kind": %"Ident", "name": %(cxtype.getTypeName())}
      # fromElaboratedPType(cxtype, conf)

    of tkConstantArray:
      result = %{
        "kind": %"Ident",
        "name": %"array",
        "genParms": %[
          %{"kind": %"Value", "value": %cxtype.getNumElements()},
          conf.toHaxdocType(cxtype.getElementType())]}

    of tkFunctionProto:
      result = %{
        "kind": %"Proc",
        "returnType": conf.toHaxdocType(cxtype.getResultType()),
        "arguments": %cxtype.argTypes.mapIt(%{
          "ident": %"",
          "identType": conf.toHaxdocType(it)
        })
      }

    else:
      result = %($(cxtype.cxKind()))

const ckProcEntryKinds* = {
  ckMethod, ckFunctionDecl, ckConstructor,
  ckMacroDefinition, ckDestructor, ckFunctionTemplate,
  ckConversionFunction
}

proc toHaxdocJson*(conf: WrapConf, ns: CScopedIdent): JsonNode =
  result = newJArray()

  for part in ns:
    var identPart = %{
      "name": %part.getName()
    }

    var kind =
      case part.cursor.cxKind():
        of ckClassDecl: %"Class"
        of ckClassTemplate: %"Class"
        of ckVarDecl: %"Var"
        of ckConstructor: %"Constructor"
        of ckConversionFunction: %"Converter"
        of ckDestructor: %"Destructor"
        of ckFunctionTemplate: %"Proc"

        of ckStructDecl: %"Struct"
        of ckMethod: %"Method"
        of ckFunctionDecl: %"Proc"
        of ckFieldDecl: %"Field"
        of ckEnumDecl: %"Enum"
        of ckEnumConstantDecl: %"EnumField"
        of ckMacroDefinition: %"CMacro"
        of ckNamespace: %"Namespace"
        of ckTypedefDecl: %"TypeDef"
        else:
          raise newImplementKindError(part.cursor.cxKind())

    identPart["kind"] = kind

    if part.cursor.kind in ckProcEntryKinds:
      identPart["procType"] = conf.toHaxdocType(part.cursor.cxType())

    result.add identPart







proc reconst*(cxType: string): string =
  if cxType.startsWith("const "):
    result = "const[" & cxType["const ".len .. ^1] & "]"

  else:
    result = cxType

proc toHaxdocIdentType*(
    conf: WrapConf,
    cxtype: CXType, procname: string = "proc"): string =

  case cxtype.cxKind():
    of tkBool, tkInt, tkVoid, tkUInt, tkLongLong, tkULongLong,
       tkDouble, tkULong, tkUChar, tkChar16, tkChar32, tkWChar,
       tkChar_S, tkLong, tkUShort, tkNullPtr, tkFloat, tkLongDouble,
       tkShort, tkSChar:

      case cxtype.cxKind:
        of tkLongDouble: result = "long[double]"
        of tkULong: result = "unsigned[long]"
        of tkULongLong: result = "unsigned[long[long]]"
        of tkUInt: result = "unsigned[int]"
        of tkLongLong: result = "long[long]"
        else:
          result = dropPrefix($cxtype, "const ")


      if cxtype.isConstQualified():
        result = "const[" & result & "]"

    of tkPointer:
      result = "ptr[" & conf.toHaxdocIdentType(cxtype[]) & "]"

    of tkLValueReference:
      result = "lvref[" & conf.toHaxdocIdentType(cxType[]) & "]"

    of tkRValueReference:
      result = "rvref[" & conf.toHaxdocIdentType(cxType[]) & "]"

    of tkElaborated, tkRecord, tkEnum:
      result = conf.getTypeNamespaces(cxType).mapIt($it).join("::")

    of tkConstantArray:
      result = &"array[{cxtype.getNumElements()}, {conf.toHaxdocIdentType(cxtype.getElementType())}]"

    of tkFunctionProto:
      result = &[
        procname, "(",
        cxtype.argTypes.mapIt(conf.toHaxdocIdentType(it)).join(", "),
        "): ", conf.toHaxdocIdentType(cxtype.getResultType())
      ]

    of tkUnexposed:
      result = dropTemplateArgs($cxtype)
      let params = cxType.templateParams()
      if params.len > 0:
        result.add "["
        for idx, param in params:
          if idx > 0: result.add ", "
          result.add conf.toHaxdocIdentType(param)

        result.add "]"

      result = reconst(result)

    else:
      result = $(cxtype.cxKind())


proc toHaxdocIdent*(conf: WrapConf, ns: CScopedIdent): string =
  for part in ns:
    if part.cursor.kind in ckProcEntryKinds:
      if result.len > 0: result &= "."
      case part.cursor.kind:
        of ckMethod: result &= "method!"
        of ckFunctionDecl: result &= "proc!"
        of ckFunctionTemplate: result &= "proc!"
        of ckConversionFunction: result &= "converter!"
        of ckMacroDefinition: result &= "cmacro!"
        of ckConstructor: result &= "contructor!"
        of ckDestructor: result &= "destructor!"
        else:
          raise newImplementKindError(part.cursor)

      result &= conf.toHaxdocIdentType(part.cursor.cxType())

    elif part.cursor.kind in {ckMacroDefinition}:
      result &= &"cmacro!{part.cursor}"

    else:
      case part.cursor.cxKind():
        of ckClassDecl: result &= "class!"
        of ckClassTemplate: result &= "class!"
        of ckVarDecl: result &= "var!"

        of ckEnumDecl: result &= "enum!"
        of ckStructDecl: result &= "struct!"
        of ckUnionDecl: result &= "union!"
        of ckFieldDecl: result &= ".field!"
        of ckEnumConstantDecl: result &= ".enumField!"
        of ckNamespace: result &= "namespace!"
        of ckTypedefDecl: result &= ".typedef!"
        else:
          raise newImplementKindError(part.cursor.cxKind())

      result &= part.getName()

      if part.cursor.cxKind() == ckNamespace:
        result &= "::"


proc `$`*(ident: CSCopedIdent): string =
  toCppNamespace(ident, withNames = true)

proc `$`*(name: CName): string = $(@[name])

proc hash*(ident: CScopedIdent): Hash =
  ## Computes a Hash from `x`.
  var h: Hash = 0
  for elem in ident:
    h = h !& hash(elem.getName())
  result = !$h

proc `==`*(a, b: CName): bool =
  a.getName() == b.getName()

proc addDoc*(cache: var WrapCache, id: CSCopedIdent, doc: seq[string]) =
  if doc.len > 0:
    cache.identComments.mgetOrPut(id, @[]).add(doc)

proc importX*(conf: WrapConf): string =
  if conf.isImportCpp:
    "importcpp"

  else:
    "importc"

proc rawSuffix*(conf: WrapConf): string =
  "C"

proc initHeaderSpec*(file: AbsFile): NimHeaderSpec =
  NimHeaderSpec(kind: cbkAbsolute, file: file)

proc initHeaderSpec*(global: string): NimHeaderSpec =
  NimHeaderSpec(kind: cbkGlobal, global: global)

proc initHeaderSpec*(pnode: PNode): NimHeaderSpec =
  NimHeaderSpec(kind: cbkPNode, pnode: pnode)

func `==`*(a, b: NimHeaderSpec): bool =
  a.kind == b.kind and ((
    case a.kind:
      of cbkGlobal: a.global == b.global
      of cbkAbsolute: a.file == b.file
      of cbkPNode: a.pnode == b.pnode
      else: raise newImplementError()
  ))

func `==`*(a, b: CArg): bool =
  a.name == b.name and
  a.isRaw == b.isRaw and ((
    if a.isRaw:
      a.cursor == b.cursor

    else:
      a.varkind == b.varkind and
      a.nimType == b.nimType and
      a.default == b.default
  ))

#======================  Accessing CDecl elements  =======================#
func arg*(cd: CDecl, idx: int): CArg = cd.arguments[idx]
func member*(cd: CDecl, idx: int): CDecl = cd.members[idx]
func methods*(cd: CDecl, kinds: set[CXCursorKind]): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct, cdkUnion}
  for member in cd.members:
    if (member.kind == cdkMethod) and (member.cursor.cxKind in kinds):
      result.add member

func `==`*(s1, s2: NimImportSpec): bool =
  s1.importPath == s2.importPath and
  s1.isRelative == s2.isRelative and (
    if s1.isRelative:
      s1.relativeDepth == s2.relativeDepth

    else:
      true
  )

func initNimImportSpec*(isExternalImport: bool, importPath: seq[string]):
  NimImportSpec =

  return NimImportSpec(
    isRelative: not isExternalImport, importPath: importPath)

func isBuiltinGeneric*(str: string): bool =
  str in ["ptr", "ref", "sink", "var"]

{.warning[Deprecated]:on.}

func newNimType*(
    name: string,
    cxType: CXType,
    isParam: bool = false
  ): NimType =

  NimType(kind: ctkIdent, nimName: name, isParam: isParam,
          cxType: cxType, fromCXType: true, defaultType: none(NimType))

func newNimType*(
    name: string,
    genParams: openarray[NimType] = @[],
    libImport: CxxLibImport = CxxLibImport(),
    original: Option[Cxtype] = none(CxType),
    isParam: bool = false
  ): NimType =

  result = NimType(
    isParam: isParam,
    kind: ctkIdent,
    nimName: name,
    fromCXtype: false,
    typeImport: libImport,
    original: original,
    genParams: toSeq(genParams)
  )

func newNimType*(
    arguments: seq[CArg],
    returnType: NimType = newNimType("void", @[], isParam = false)
  ): NimType =

  NimType(
    kind: ctkProc, arguments: arguments,
    returnType: returnType, isParam: false)

func newNimType*(
    name: string,
    genParams: openarray[NimType],
    cxType: CXType,
    isParam: bool = false
  ): NimType =

  # {.warning[Deprecated]:on.}
  result = newNimType(name, cxType, isParam)
  result.genParams.add genParams

func withLib*(ntype: sink NimType, libImport: CxxLibImport): NimType =
  result = ntype
  result.typeImport = libImport

func addIdent*(nimType: sink NimType, id: CScopedIdent): NimType =
  result = nimType
  result.fullIdent = some id

func initCxxLibImport*(name: string, path: seq[string]): CxxLibImport =
  CxxLibImport(library: name, importPath: path)


func hash*(nt: NimType): Hash =
  case nt.kind:
    of ctkPod:
      result = hash(nt.podKind)

    of ctkAnonEnum, ctkAnonObject:
      raise newImplementKindError(nt)

    of ctkWrapKinds:
      result = !$(hash(nt.kind) !& hash(nt.wrapped))

    of ctkArrayKinds:
      result = !$(hash(nt.arraySize) !& hash(nt.arrayElement))

    of ctkStaticParam:
      result = hash(nt.param)

    of ctkIdent:
      result = hash(nt.nimName)

      for param in nt.genParams:
        result = result !& hash(param)

      return !$(result)

    of ctkProc:
      result = hash(nt.returnType)
      for arg in nt.arguments:
        if arg.isRaw:
          result = result !& hash(arg.cursor)

        else:
          result = result !& hash(arg.nimType)

      return !$(result)

func `==`*(t1, t2: NimType): bool =
  if t1.kind == t2.kind:
    case t1.kind:
      of ctkPod:
        return t1.podKind == t2.podKind

      of ctkAnonObject, ctkAnonEnum:
        raise newImplementKindError(t1)

      of ctkWrapKinds:
        return t1.wrapped == t2.wrapped

      of ctkStaticParam:
        return t1.param == t2.param

      of ctkArrayKinds:
        return t1.arraySize == t2.arraySize and
               t1.arrayElement == t2.arrayElement

      of ctkIdent:
        if t1.nimName == t2.nimName and
           t1.genParams == t2.genParams:
          for (p1, p2) in zip(t1.genParams, t2.genParams):
            if p1 != p2:
              return false

          return true

      of ctkProc:
        if t1.returnType == t2.returnType:
          for (a1, a2) in zip(t1.arguments, t2.arguments):
            if a1.nimType != a2.nimType:
              return false

          return true


func hash*(lib: CxxLibImport): Hash =
  !$(hash(lib.library) !& hash(lib.importPath))

# func `$`*(lib: CxxLibImport): string =
#   if lib.library.len > 0:
#     result &= lib.library
#     result &= "@"

#   result &= lib.importPath.join("/")

func libImport*(conf: WrapConf, path: seq[string]): CxxLibImport =
  initCxxLibImport(conf.wrapName, path)

func libImport*(conf: WrapConf, file: RelFile): CxxLibImport =
  initCxxLibImport(
    conf.wrapName,
    file.withoutExt().getStr().split("/"))




func isValid*(lib: CxxLibImport): bool = lib.importPath.len > 0

func toRelative*(lib: CxxLibImport): RelFile =
  assert lib.importPath.len > 0
  result = RelFile(lib.importPath.join("/"))
  result.addExt("nim")

func asImport*(lib: CxxLibImport): RelFile =
  assert lib.importPath.len > 0
  result = RelFile(lib.importPath.join("/"))

func `&`*(lib: CxxLibImport, path: openarray[string]): CxxLibImport =
  result = lib
  result.importPath &= @path

func addNamePrefix*(lib: var CxxLibImport, prefix: string, idx: IndexTypes = ^1) =
  lib.importPath[idx] = prefix & lib.importPath[idx]

func addPathPrefix*(lib: var CxxLibImport, prefix: string) =
  lib.importPath.insert(prefix, 0)


proc `$`*(spec: NimImportSpec): string =
  if spec.isRelative:
    if spec.relativeDepth == 0:
      result = "./"

    else:
      result = "..".repeat(spec.relativeDepth)
      result &= "/"

  result &= join(spec.importPath, "/")

proc `$`*(nimType: NimType): string =
  if isNil(nimType):
    result = "void"

  else:
    case nimType.kind:
      of ctkPod:
        result = $nimType.podKind

      of ctkAnonEnum, ctkAnonObject:
        raise newImplementKindError(nimType)

      of ctkPtr:
        result = $nimType.wrapped & "*"

      of ctkLVRef:
        result = $nimType.wrapped & "&"

      of ctkRVRef:
        result = $nimType.wrapped & "&&"

      of ctkDynamicArray:
        result = $nimType.wrapped & "[]"

      of ctkFixedArray, ctkDependentArray:
        result = $nimType.arrayElement & "[" & $nimType.arraySize & "]"

      of ctkStaticParam:
        result = $nimType.param

      of ctkIdent:
        if nimType.isParam:
          result = "p!"

        result &= nimType.nimName
        if nimType.genParams.len > 0:
          result &= "["
          result &= nimType.genParams.mapIt($it).join(", ")
          result &= "]"

        if not nimType.fromCxType and nimType.original.isSome():
          result &= " (from '" & $nimType.original.get() & "')"

      of ctkProc:
        result &= "proc "
        result &= nimType.arguments.mapIt(it.name & ": " & $it.nimType).join(", ")
        result &= ": "
        result &= $nimType.returnType

    if nimType.kind == ctkIdent:
      if nimType.defaultType.isSome():
        result &= " = "
        result &= $nimType.defaultType.get()



func initCArg*(
    name: string, nimType: NimType, varkind: NVarDeclKind):
  CArg =

  CArg(isRaw: false, name: name, nimType: nimType, varkind: varkind)


func initCArg*(name: string, nimType: NimType): CArg =
  initCArg(name, nimType, if nimType.isMutable: nvdVar else: nvdLet)

func initCArg*(name: string, cursor: CXCursor): CArg =
  ## Init raw C argument with `name`
  CArg(isRaw: true, name: name, cursor: cursor)

func initImportSpec*(path: seq[string]): NimImportSpec =
  ## Create nim import spec with absolute path
  NimImportSpec(isRelative: false, importPath: path)

func initImportSpec*(path: seq[string], depth: int): NimImportSpec =
  ## Create nim import spec with absolute path
  NimImportSpec(
    isRelative: true, importPath: path,
    relativeDepth: depth)

#==========================  Helper utilities  ===========================#
proc declHash*(cursor: CXCursor): Hash =
  let loc = cursor.getSpellingLocation().get()
  return !$(
    hash(loc.file) !& hash(loc.line) !&
    hash(loc.column) !& hash(loc.offset))

proc markWrap*(cache: var WrapCache, cursor: CXCursor) =
  ## Mark cursor as wrapped
  cache.hset.incl cursor.declHash()

proc canWrap*(cache: WrapCache, cursor: CXCursor): bool =
  ## Check if cursor has already been wrapped
  cursor.declHash() notin cache.hset

proc markSeen*(cache: var WrapCache, cursor: CXCursor) =
  ## Mark cursor in cache as already visited
  cache.visited.incl cursor.hashCursor()

proc seenCursor*(cache: WrapCache, cursor: CXCursor): bool =
  ## Cursor has already been recorded in the cache?
  cursor.hashCursor() in cache.visited


proc lastName*(cd: CDecl, conf: WrapConf, dropTemplate: bool = true): string =
  ## Return *last* name for declaration.
  ##
  ## `std::vector<int> -> vector`, `int main() -> main` etc.
  result = $cd.ident[^1].cursor

  if dropTemplate:
    result = dropTemplateArgs(result)


#==========================  Operator handling  ==========================#



proc isOperator*(cd: CDecl, conf: WrapConf): bool =
  ## Check whether C++ declaration is an operator declaration
  cd.kind in {cdkMethod, cdkFunction} and
  cd.lastName(conf).startsWith("operator") and
  (not cd.lastName(conf).validIdentifier())

proc isOperator*(cx: CXCursor): bool =
  ## Check whether cursor points to operator declaration
  ($cx).startsWith("operator") and
  (not ($cx).validIdentifier())


proc cxType*(arg: CArg): CxType =
  if arg.isRaw:
    result = arg.cursor.cxType()

  else:
    if arg.nimType.fromCxType:
      result = arg.nimType.cxType

    elif arg.nimType.original.isSome():
      result = arg.nimType.original.get()

    else:
      raise newArgumentError(
        "Cannot get argument type - it is not raw,",
        "it's type is not `fromNimType`,",
        "`original` type is not set")

proc classifyOperator*(cd: CDecl, conf: WrapConf): CxxProcKind =
  ## Classify C++ operator declaration
  assert cd.isOperator
  let inType = cd.cursor.
    getCursorSemanticParent().cxKind() in ckTypeDeclKinds

  let name = cd.lastName(conf).dropPrefix("operator")
  case name:
    of "=":
      cpkCopyAsgnOp

    of "+=", "-=", "*=",
       "<<=", ">>=", "&=", "|=", "/=", "%=", "^="
      :
      cpkAsgnOp

    of "[]":
      cpkArrayOp

    of "-", "+":
      if cd.arguments.len >= 2 or (cd.arguments.len == 1 and inType):
        cpkInfixOp

      else:
        cpkPrefixOp

    of "++", "--":
      # NOTE this is an operator implementation, so we are not (i hope)
      # dropping information about prefi/postfix calls
      if cd.arguments.len == 1 and
         $cd.arguments[0].cxType() == "int":
        cpkPostfixOp

      else:
        cpkPrefixOp

    of "/",
       "<<", ">>", "==", "!=", "&&", "||",
       "%", "^", "&", "|", "<", ">", "<=", ">=":
      cpkInfixOp

    of "*": # NOTE this heuristics might not be valid in all cases.
      if cd.arguments.len == 0:
        cpkDerefOp
      else:
        cpkInfixOp

    of "->", "->*":
      cpkArrowOp

    of "()":
      cpkCallOp

    of "~", "!":
      cpkPrefixOp

    of ",":
      cpkCommaOp

    of " new", " new[]":
      cpkNewOp

    of " delete", " delete[]":
      cpkDeleteOp


    else:
      if cd.cursor.cxKind() == ckConversionFunction:
        cpkConvertOp

      elif (cd.cursor.cxKind() in {
        ckFunctionDecl, ckFunctionTemplate
      }) and (name.startsWith("\"\"")):
        cpkUserLitOp

      elif cd.cursor.cxKind() in {ckFunctionTemplate}:
        # warn cd.ident
        cpkConvertOp

      else:
        raiseAssert(
          &"#[ IMPLEMENT '{name}', {cd.cursor.cxKind()} ]#" &
            $cd.cursor.getSpellingLocation()
        )


proc getNimName*(
    cd: CDecl, conf: WrapConf, dropTemplate: bool = true): string =

  if cd.kind in { cdkMethod, cdkFunction } and
     cd.isOperator:

    if cd.lastName(conf) == "operator=":
      result = "setFrom" # REVIEW change name to something different if
                         # possible

    else:
      result = cd.lastName(conf, dropTemplate)[len("operator") ..^ 1]

  elif cd.cursor.kind in { ckDestructor }:
    result = cd.lastName(conf, dropTemplate)[1 ..^ 1]

  else:
    result = cd.lastName(conf, dropTemplate)

proc getFlags*(config: ParseConf, file: AbsFile): seq[string] =
  ## Get list of command-line flags for partigular `file`. This includes
  ## both global flags, and file-specific ones
  result.add config.includepaths.toIncludes()

  for path in config.sysIncludes: result.add &"-I\"{path}\""
  for path in config.userIncludes: result.add &"-I\"{path}\""

  result.add config.globalFlags
  result.add config.fileFlags.getOrDefault(file)

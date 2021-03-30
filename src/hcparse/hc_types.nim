import cxtypes, cxcommon
import hmisc/other/[oswrap, colorlogger]
import hnimast, hnimast/pprint
import std/[tables, sets, strutils, sequtils, hashes, strformat, macros]
import hmisc/algo/[hseq_mapping, hstring_algo, hseq_distance]

type
  DepResolutionKind* = enum
    ## Kind of cursor dependency resolution
    drkIgnoreIfUsed ## Ignore dependency
    drkWrapDirectly ## Wrap dependency in main generated wrappers file
    drkImportUses ## Assume dependency is wrapped in other module, and
                  ## generate `import` for it.

  CDeclKind* = enum
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
    name*: string
    case isRaw*: bool ## Points to existing entry
      of true:
        cursor*: CXCursor

      of false: ## Either generated from raw cursor or constructed anew
        varkind*: NVarDeclKind
        ntype*: NType[PNode]
        default*: Option[PNode]


  CXOperatorKind* = enum
    ## Classification for operators
    cxoPrefixOp ## Prefix operator `@a`
    cxoPostfixOp ## Postfix operator `a@`
    cxoInfixOP ## Infix operator `a @ b`
    cxoAsgnOp ## Assign operator `a += b`
    cxoCopyAsgnOp ## Copy assignment operator `a = b`
    cxoArrayOp ## Array access operator `a[b]`
    cxoArrowOp ## Arrow operator `a->`
    cxoCallOp ## Call operator `a()`
    cxoDerefOp ## Prefix dereference operator
    cxoCommaOp ## Comma operator
    cxoConvertOp ## User-defined conversion operator
    cxoUserLitOp ## User-defined literal operators
    cxoNewOp ## `new` operator
    cxoDeleteOp ## `delete` operator

  IncludeDep* = object
    # TODO use it as edge value
    includedAs*: string
    includedPath*: AbsFile
    includedFrom*: AbsFile
    fromLine*: int
    fromColumn*: int
    fromOffset*: int


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

    ident*: CScopedIdent ## Fully qualified identifier (except namespaces
    ## that were explicitly marked as 'collapsible' in wrap configuration)

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
        arguments*: seq[CArg] ## Method or function argumets
        returnType*: Option[CXType] ## Optional return type
        case isOperator*: bool
          of true:
            operatorName*: string ## Name with dropped `operator` prefix
            operatorKind*: CXOperatorKind ## Operator classification

          of false:
            discard

      of cdkClass, cdkStruct, cdkUnion:
        isDefaultConstructible*: bool ## Type does not have deleted default
                                      ## constructor
        isCopyable*: bool ## Type can be copied (does not have copy
                          ## constructor deleted)
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
            withBaseType*: bool ## no `struct T` declaration is present -
            ## `typedef` was immediately used to declare type.

            aliasNewType*: CDecl ## New type declaration introduced by
            ## C-style `typedef`

          else:
            aliasBaseType*: CXCursor ## Base type used for alias
                                     ## declaration

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
    publicAPI*: seq[CXCursor]
    includes*: seq[IncludeDep]

  ParsedFile* = object
    unit*: CXTranslationUnit ## Translation unit
    filename*: AbsFile ## Name of the original file
    api*: CApiUnit ## File's API
    index*: CXIndex
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

    includepaths*: seq[AbsDir]

  FileIndex* = object
    index*: Table[AbsFile, ParsedFile] ## Index of all parsed files

  NimHeaderSpecKind* = enum
    nhskGlobal
    nhskAbsolute
    nhskPNode

  NimHeaderSpec* = object
    case kind*: NimHeaderSpecKind
      of nhskGlobal:
        global*: string ## Global include like `<string>`

      of nhskAbsolute:
        file*: AbsFile ## Absolute path to header file

      of nhskPNode:
        pnode*: PNode ## Anything else

  NimImportSpec* = object
    importPath*: seq[string]
    case isRelative*: bool
      of true:
        relativeDepth*: int ## `0` means relative to current file, `./`.
        ## Any number greater than `0` is converted to equal number of
        ## directory-up `..`

      else:
        discard

  WrapConf* = ref object
    ## Confuration for wrapping. Mostly deals with type renaming

    header*: AbsFile ## Current main translation file (header)

    unit*: CXTranslationUnit

    makeHeader*: proc(cursor: CXCursor, conf: WrapConf): NimHeaderSpec ## |
    ## Generate identifier for `{.header: ... .}`

    typeNameForScoped*: proc(
      ident: CScopedIdent, conf: WrapConf): NType[PNode]
    ## Generate type name for a scoped identifier - type or function
    ## declaration. The only important things are: `head` name and list of
    ## generic parameters, so `ntkIdent` is the optimal return kind.

    fixTypeName*: proc(ntype: var NType[PNode], conf: WrapConf, idx: int)
    ## Change type name for `ntype`.
    ##
    ## First argument is a type to be fixed, second one is parent
    ## configuration type. Third argument is mostly used for internal
    ## purposes - index of the generic argument. For cases like `[__T,
    ## _T]`, where both types should be mapped to `T` you can make `T` and
    ## `T1` respectively, using value provided by `idx`

    getImport*: proc(dep: AbsFile, conf: WrapConf, isExternalImport: bool):
      NimImportSpec ## Generate import statement for header file dependency

    ignoreCursor*: proc(curs: CXCursor, conf: WrapConf): bool ## User-defined
    ## predicate for determining whether or not cursor should be
    ## considered a part of api. Things like `internal` namespaces.

    collapsibleNamespaces*: seq[string]
    ignoreFile*: proc(file: AbsFile): bool
    baseDir*: AbsDir ## Root directory for C++ sources being wrapped. Used
                     ## for debug comments in generated sources
    isInternal*: proc(
      dep: AbsFile, conf: WrapConf, index: FileIndex): bool ## Determine
    ## if particular dependency (`dep` file) should be re-exported.
    ## Note that this decision is not tied to particular file *from
    ## which* `dep` has been imported, but instead works the same way
    ## for all headers that depend on `dep`

    isTypeInternal*: proc(cxt: CXType, conf: WrapConf): bool
    depResolver*: proc(cursor, referencedBy: CXCursor): DepResolutionKind
    isInLibrary*: proc(dep: AbsFile, conf: WrapConf): bool ## Determine
    ## if `dep` file is in the library.

    showParsed*: bool ## Show translation unit tree repr when wrapping
    isImportcpp*: bool ## Is wrapped code a C++ or C?
    parseConf*: ParseConf

    prefixForEnum*: proc(
      enumId: CScopedIdent, conf: WrapConf,
      cache: var WrapCache): string ## Return prefix for enum referred to
    ## by `enumId`. This is used to override autogenrated prefix for
    ## particular enum.

    docCommentFor*: proc(
      id: CSCopedIdent, cursor: CXCursor, cache: var WrapCache): string ## |
    ## Return documentation comment string for entry pointed to by
    ## `cursor`. `id` is a fully qualified/namespaced path for definition
    ## (like `std::vector`)

    userCode*: proc(source: AbsFile): PNode ## Add arbitarry user-defined
    ## code at the start of generated wrapper for `source` file.

    newProcCb*: proc(
      genProc: var GenProc, conf: WrapConf, cache: var WrapCache
    ): seq[WrappedEntry] ## Callback invoked after each new procedure is
    ## generated. Is allowed (and expected to) mutate passed proc, and
    ## generate additional helper wrappers either via return value (added
    ## immediately after proc declaration), or by mutating some external
    ## list of variables.

    isDistinct*: proc(
      ident: CSCopedIdent, conf: WrapConf, cache: var WrapCache): bool ## |
    ## Determine if given `ident` should be wrapped as nim `distinct` type
    ## or not.
    ## - WARNING :: Default implementation always returns `false` i.e. all
    ##   types are wrapped as not typesafe aliases.



  WrapCache* = object
    hset*: HashSet[Hash]
    visited*: HashSet[cuint]
    enumPrefs*: HashSet[string]
    identComments*: Table[CScopedIdent, seq[string]]
    nameCache*: StringNameCache
    genEnums*: seq[GenEnum]

  GenBase* {.inheritable.} = ref object
    ## Common fields for all `GenX` types. Not used for inheritance, only
    ## to avoud code duplication.
    cdecl* {.requiresinit.}: CDecl
    iinfo* {.requiresinit.}: LineInfo
    docComment*: seq[string]


  GenObjectKind = enum
    gokUnion
    gokStruct
    gokClass

  GenField* = ref object of GenBase
    rawName*: string
    name*: string
    fullName*: CSCopedIdent
    value*: Option[PNode] ## /arbitrary expression/ for field initalization
    fieldType*: NType[PNode]
    isConst*: bool ## Field is const-qualified
    anonymousType*: Option[GenEntry] ## Wrapper for anonymous type (if any)

  GenObject* = ref object of GenBase
    kind*: GenObjectKind
    rawName*: string
    name*: NType[PNode]
    fullName*: CScopedIdent
    memberFields*: seq[GenField] ## Direct member fields
    memberMethods*: seq[GenProc]
    isAggregateInit*: bool ## Subject to aggregate initalization
    isIterableOn*: seq[tuple[beginProc, endProc: GenProc]] ## Object has
    ## `begin()/end()` or any kind of similar procs that can be used to
    ## generate `items` iterators.

    nestedEntries*: seq[GenEntry]

  GenProc* = ref object of GenBase
    ## Generated wrapped proc
    name*: string ## Name of the generated proc on nim side
    icpp*: string ## `importcpp` pattern string
    private*: bool ## Generated proc should be private?
    arguments*: seq[CArg] ## Arguments
    returnType*: NType[PNode]
    genParams*: seq[NType[PNode]] ## Nim generic parameters
    declType*: ProcDeclType ## Type of proc declaration (iterator,
                            ## converter etc.)
    header*: NimHeaderSpec ## Header specification for `.header:` pragma
    pragma*: PPragma ## Additional pragmas on top of `importcpp`
    kind*: ProcKind ## Kind of generated nim proc (operator, field setter,
                    ## regular proc etc.)
    impl*: Option[PNode] ## Optional implementation body
    noPragmas*: bool ## Do not add default C wrapper pragamas. Used for
                     ## pure nim enums

  GenEnumValue* = ref object of GenBase
    baseName*: string ## Original name of the enum value
    resCName*: string ## Enum field value for 'raw' C wrapper proc
    resNimName*: string ## Enum field name for 'proxy' nim proc
    resVal*: BiggestInt ## Value of the enum field - from source code or
                        ## generated when filling hole values.
    valTokens*: seq[string] ## Original tokens for enum value declaration
                            ## (if any).
    stringif*: string ## 'stringified' version of fully qualified field
                      ## name (`enumName::fieldname`)

  GenEnum* = ref object of Genbase
    ## Generated enum
    case isMacroEnum* {.requiresinit.}: bool
      of false:
        rawName*: string ## Original name of the enum. Directly corresponds
                         ## to underlying enum name

      else:
        ## Enum generated form seveal macro constant definitions. Does not
        ## have a based name (because it is not `importc`'ed)
        proxyName*: string ## Name for the proxy wrapper enum. Does not
                           ## correspond to any entry in the underlying C
                           ## code

    isCTypedef*: bool
    name*: string ## Converted nim name
    values*: seq[GenEnumValue] ## Filtered, ordered sequence of values

  GenAlias* = ref object of GenBase
    isDistinct*: bool
    newAlias*: NType[PNode]
    baseType*: NType[PNode]

  GenPass* = ref object
    iinfo* {.requiresinit.}: LineInfo
    docComment*: seq[string]
    passEntries*: seq[WrappedEntry]

  GenImport* = ref object
    iinfo* {.requiresinit.}: LineInfo
    importSpec*: NimImportSpec

  GenForward* = ref object of GenBase


  GenEntryKind* = enum
    gekEnum ## Enum wrapper
    gekProc ## Method, operator, or function
    gekObject ## Struct, union, or class
    gekAlias ## `typedef` or `using`
    gekPass ## Raw passthrough

    gekForward ## Forward declaration for struct/union/class/enum
    gekImport ## Import statement

  GenEntry* = ref object
    ## Toplevel wrapper for different entry kinds.
    ##
    ## Does not server any particular purpose other than to allow storing
    ## differnt `GenX` entries in the same container.
    case kind*: GenEntryKind
      of gekEnum:
        genEnum*: GenEnum

      of gekProc:
        genProc*: GenProc

      of gekObject:
        genObject*: GenObject

      of gekAlias:
        genAlias*: GenAlias

      of gekPass:
        genPass*: GenPass

      of gekImport:
        genImport*: GenImport

      of gekForward:
        genForward*: GenForward


  WrappedEntry* = object
    decl*: PNimDecl
    ident*: CSCopedIdent
    postTypes*: bool
    cursor* {.requiresinit.}: CXCursor

  WrappedFile* = ref object
    entries*: seq[GenEntry]
    case isGenerated*: bool ## File was generated from strongly linked
                            ## cluster of forward-declared types.
      of true:
        discard

      of false:
        discard


  EnFieldVal* = object
    case isRefOther*: bool
      of true:
        othername*: string
      of false:
         value*: BiggestInt

  CxxCodegen* = object
    ## Single codegen field entry
    cursor*: CXCursor
    # TODO replace with htsparse AST tree
    code*: string
    header*: string
    filename*: RelFile

  WrapResult* = ref object
    parsed*: ParsedFile
    wrapped*: seq[WrappedEntry] ## Generated wrapped entries
    # FIXME wrap result might contain multiple files
    infile*: AbsFile ## Path to base wrapped file
    # QUESTION `importName` is most likely to cause a lot of headaches if I
    # only use one. Ideally I need to have 'external' import spec, and the
    # one relative to project root.
    importName*: NimImportSpec


proc add*(
    genSeq: var seq[GenEntry],
    gen: GenProc | GenObject | GenEnum | GenAlias |
         GenPass | GenImport | GenForward
  ) =

  when gen is GenProc:
    genSeq.add GenEntry(kind: gekProc, genProc: gen)

  elif gen is GenObject:
    genSeq.add GenEntry(kind: gekObject, genObject: gen)

  elif gen is GenEnum:
    genSeq.add GenEntry(kind: gekEnum, genEnum: gen)

  elif gen is GenAlias:
    genSeq.add GenEntry(kind: gekAlias, genAlias: gen)

  elif gen is GenPass:
    genSeq.add GenEntry(kind: gekPass, genPass: gen)

  elif gen is GenImport:
    genSeq.add GenEntry(kind: gekImport, genImport: gen)

  elif gen is GenForward:
    genSeq.add GenEntry(kind: gekForward, genForward: gen)

proc newProcVisit*(
    genProc: var GenProc, conf: WrapConf, cache: var WrapCache
  ): seq[WrappedEntry] =

  if not isNil(conf.newProcCb):
    return conf.newProcCb(genProc, conf, cache)

proc getName*(cn: CName): string =
  if cn.isGenerated:
    cn.name

  else:
    $cn.cursor

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

proc setPrefixForEnum*(
  wrapConf: var WrapConf, maps: seq[(string, string)]) =

  let oldImpl = wrapConf.prefixForEnum

  wrapConf.prefixForEnum =
    proc(
      enumId: CScopedIdent, conf: WrapConf,
      cache: var WrapCache
    ): string =
      let name = enumId[^1].getName()
      for (full, prefix) in maps:
        if name == full:
          result = prefix
          break

      if result.len == 0:
        result = oldImpl(enumId, conf, cache)

      else:
        cache.enumPrefs.incl result

proc initHeaderSpec*(file: AbsFile): NimHeaderSpec =

  NimHeaderSpec(kind: nhskAbsolute, file: file)

proc initHeaderSpec*(global: string): NimHeaderSpec =
  NimHeaderSpec(kind: nhskGlobal, global: global)

proc initHeaderSpec*(pnode: PNode): NimHeaderSpec =
  NimHeaderSpec(kind: nhskPNode, pnode: pnode)



func cdecl*(gen: GenEntry): CDecl =
  case gen.kind:
    of gekEnum: gen.genEnum.cdecl
    of gekProc: gen.genProc.cdecl
    of gekObject: gen.genObject.cdecl
    of gekAlias: gen.genAlias.cdecl
    of gekForward: gen.genForward.cdecl
    of gekPass, gekImport: raiseUnexpectedKindError(gen)

func `$`*(we: WrappedEntry): string = $we.decl
func `$`*(we: seq[WrappedEntry]): string =
  {.cast(noSideEffect).}:
    we.mapPairs(rhs.decl.toNNode().toPString()).join("\n")

func `==`*(a, b: NimHeaderSpec): bool =
  a.kind == b.kind and ((
    case a.kind:
      of nhskGlobal: a.global == b.global
      of nhskAbsolute: a.file == b.file
      of nhskPNode: a.pnode == b.pnode
  ))

func `==`*(a, b: CArg): bool =
  a.name == b.name and
  a.isRaw == b.isRaw and ((
    if a.isRaw:
      a.cursor == b.cursor

    else:
      a.varkind == b.varkind and
      a.ntype == b.ntype and
      a.default == b.default
  ))

func newWrappedEntry*(
    nimDecl: PNimDecl, postTypes: bool, iinfo: LineInfo, cursor: CXCursor
  ): WrappedEntry =

  result = WrappedEntry(
    postTypes: postTypes, decl: nimDecl, cursor: cursor)

  result.decl.iinfo = iinfo

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


func initCArg*(
    name: string, ntype: NType[PNode], varkind: NVarDeclKind = nvdLet
  ): CArg =

  CArg(isRaw: false, name: name, ntype: ntype, varkind: varkind)


func initCArg*(
    name: string, ntype: NType[PNode], mutable: bool
  ): CArg =

  initCArg(name, ntype, if mutable: nvdVar else: nvdLet)

func initCArg*(name: string, cursor: CXCursor): CArg =
  CArg(isRaw: true, name: name, cursor: cursor)

func initGenProc*(cdecl: CDecl, iinfo: LineInfo): GenProc =
  GenProc(cdecl: cdecl, iinfo: iinfo)

func initGenImport*(importPath: seq[string], iinfo: LineInfo): GenImport =
  GenImport(iinfo: iinfo, importSpec: NimImportSpec(
    isRelative: false, importPath: importPath
  ))

func initGenImport*(importSpec: NimImportSpec, iinfo: LineInfo): GenImport =
  GenImport(iinfo: iinfo, importSpec: importSpec)

#==========================  Helper utilities  ===========================#
proc declHash*(cursor: CXCursor): Hash =
  let loc = cursor.getSpellingLocation().get()
  return !$(
    hash(loc.file) !& hash(loc.line) !&
    hash(loc.column) !& hash(loc.offset))

proc markWrap*(cache: var WrapCache, cursor: CXCursor) =
  cache.hset.incl cursor.declHash()

proc canWrap*(cache: WrapCache, cursor: CXCursor): bool =
  cursor.declHash() notin cache.hset

proc markSeen*(cache: var WrapCache, cursor: CXCursor) =
  cache.visited.incl cursor.hashCursor()

proc seenCursor*(cache: WrapCache, cursor: CXCursor): bool =
  cursor.hashCursor() in cache.visited

proc lastName*(cd: CDecl): string =
  ## Return *last* name for declaration.
  ##
  ## `std::vector<int> -> vector`, `int main() -> main` etc.
  $cd.ident[^1].cursor

#==========================  Operator handling  ==========================#



func isOperator*(cd: CDecl): bool =
  cd.kind in {cdkMethod, cdkFunction} and
  cd.lastName().startsWith("operator") and
  (not cd.lastName().validIdentifier())

proc isOperator*(cx: CXCursor): bool =
  ($cx).startsWith("operator") and
  (not ($cx).validIdentifier())


proc classifyOperator*(cd: CDecl): CXOperatorKind =
  assert cd.isOperator
  let name = cd.lastName().dropPrefix("operator")
  case name:
    of "=":
      cxoCopyAsgnOp

    of "+=", "-=", "*=",
       "<<=", ">>=", "&=", "|=", "/=", "%=", "^="
      :
      cxoAsgnOp

    of "[]":
      cxoArrayOp

    of "-", "+":
      if cd.arguments.len == 1:
        cxoPrefixOp

      else:
        cxoInfixOp

    of "/",
       "++", "--", # NOTE this is an operator implementation, so we are
                  # not (i hope) dropping information about
                  # prefi/postfix calls
       "<<", ">>", "==", "!=", "&&", "||",
       "%", "^", "&", "|", "<", ">", "<=", ">="
         :
      cxoInfixOp

    of "*": # NOTE this heuristics might not be valid in all cases.
      if cd.arguments.len == 0:
        cxoDerefOp
      else:
        cxoInfixOp

    of "->", "->*":
      cxoArrowOp

    of "()":
      cxoCallOp

    of "~", "!":
      cxoPrefixOp

    of ",":
      cxoCommaOp

    of " new", " new[]":
      cxoNewOp

    of " delete", " delete[]":
      cxoDeleteOp


    else:
      if cd.cursor.cxKind() == ckConversionFunction:
        cxoConvertOp

      elif (cd.cursor.cxKind() in {
        ckFunctionDecl, ckFunctionTemplate
      }) and (name.startsWith("\"\"")):
        cxoUserLitOp

      elif cd.cursor.cxKind() in {ckFunctionTemplate}:
        cxoConvertOp

      else:
        raiseAssert(
          &"#[ IMPLEMENT '{name}', {cd.cursor.cxKind()} ]#" &
            $cd.cursor.getSpellingLocation()
        )


func getNimName*(cd: CDecl): string =
  case cd.kind:
    of cdkMethod, cdkFunction:
      if cd.isOperator:
        if cd.lastName() == "operator=":
          "setFrom" # REVIEW change name to something different if possible
        else:
          cd.lastName().dropPrefix("operator")

      else:
        cd.lastName()

    else:
      cd.lastName()

proc initEnFieldVal*(v: BiggestInt): EnFieldVal =
  EnFieldVal(isRefOther: false, value: v)

func toNNode*(nhs: NimHeaderSpec): PNode =
  case nhs.kind:
    of nhskPNode:
      nhs.pnode

    of nhskAbsolute:
      newRStrLit("\"" & nhs.file.getStr() & "\"")

    of nhskGlobal:
      newRStrLit("<" & nhs.global & ">")

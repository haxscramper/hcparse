import gram, cxtypes, cxcommon
import hmisc/other/oswrap
import hnimast, hnimast/pprint
import std/[tables, sets, strutils, sequtils, hashes, strformat, macros]
import hmisc/algo/[hseq_mapping, hstring_algo]

const HeaderGraphFlags* = toInt({
  Directed, ValueIndex, UniqueEdges, UniqueNodes})

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
    cdkEnum
    cdkFunction
    cdkMethod
    cdkField
    cdkAlias
    cdkMacro

  CArg* = object
    name*: string
    case isRaw*: bool
      of true:
        cursor*: CXCursor

      of false:
        varkind*: NVarDeclKind
        ntype*: NType[PNode]
        default*: Option[PNode]


  CXOperatorKind* = enum
    ## Classification for operators
    cxoPrefixOp ## Prefix operator `@a`
    cxoPostfixOp ## Postfix operator `a@`
    cxoInfixOP ## Infix operator `a @ b`
    cxoAsgnOp ## Assign operator `a = b`
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
    cursor*: CXCursor ## Name declaration cursor
    genParams*: seq[CScopedIdent]

  CScopedIdent* = seq[CName] ## Full scoped C/C++ identifier like
                             ## `std::vector<int>::iterator`
  CDecl* = object
    ## Higher-level wrapper on top of CXCursor. Mostly used to
    ## provide more intuitive API for working with things to be
    ## wrapped.
    ident*: CScopedIdent
    cursor*: CXCursor
    case kind*: CDeclKind
      of cdkField:
        fldAccs*: CX_AccessSpecifier

      of cdkMethod:
        metAccs*: CX_AccessSpecifier
        metArgs*: seq[CArg]

      of cdkFunction:
        funArgs*: seq[CArg]

      of cdkClass, cdkStruct:
        members*: seq[CDecl]

      of cdkEnum:
        flds*: seq[tuple[fldname: string, value: Option[CXCursor]]]

      else:
        nil



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

  HeaderDepGraph* = Graph[AbsFile, string, HeaderGraphFlags]

  ParseConfig* = object
    globalFlags*: seq[string] ## List of parse flags applied on each
    ## file parse. Mostly for things like include paths.
    fileFlags*: Table[AbsFile, seq[string]] ## List of parse flags
    ## specific only to particular file

    includepaths*: seq[AbsDir]

  FileIndex* = object
    index*: Table[AbsFile, ParsedFile] ## Index of all parsed files
    depGraph*: HeaderDepGraph

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

  WrapConfig* = ref object
    ## Configuration for wrapping. Mostly deals with type renaming

    header*: AbsFile ## Current main translation file (header)

    unit*: CXTranslationUnit

    makeHeader*: proc(cursor: CXCursor, conf: WrapConfig): NimHeaderSpec ## |
    ## Generate identifier for `{.header: ... .}`

    typeNameForScoped*: proc(
      ident: CScopedIdent, conf: WrapConfig): NType[PNode]
    ## Generate type name for a scoped identifier - type or function
    ## declaration. The only important things are: `head` name and list of
    ## generic parameters, so `ntkIdent` is the optimal return kind.

    fixTypeName*: proc(ntype: var NType[PNode], conf: WrapConfig, idx: int)
    ## Change type name for `ntype`.
    ##
    ## First argument is a type to be fixed, second one is parent
    ## configuration type. Third argument is mostly used for internal
    ## purposes - index of the generic argument. For cases like `[__T,
    ## _T]`, where both types should be mapped to `T` you can make `T` and
    ## `T1` respectively, using value provided by `idx`

    getImport*: proc(dep: AbsFile, conf: WrapConfig): seq[string] ## Generate
    ## import statement for header file dependency

    ignoreCursor*: proc(curs: CXCursor, conf: WrapConfig): bool ## User-defined
    ## predicate for determining whether or not cursor should be
    ## considered a part of api. Things like `internal` namespaces.

    collapsibleNamespaces*: seq[string]
    ignoreFile*: proc(file: AbsFile): bool
    baseDir*: AbsDir ## Root directory for C++ sources being wrapped. Used
                     ## for debug comments in generated sources
    isInternal*: proc(
      dep: AbsFile, conf: WrapConfig, index: FileIndex): bool ## Determine
    ## if particular dependency (`dep` file) should be re-exported.
    ## Note that this decision is not tied to particular file *from
    ## which* `dep` has been imported, but instead works the same way
    ## for all headers that depend on `dep`

    isTypeInternal*: proc(cxt: CXType, conf: WrapConfig): bool
    depResolver*: proc(cursor, referencedBy: CXCursor): DepResolutionKind
    isInLibrary*: proc(dep: AbsFile): bool ## Determine if `dep` file is in
    ## the library.

    isImportcpp*: bool
    parseConf*: ParseConfig

  WrapCache* = object
    hset: HashSet[Hash]
    visited: HashSet[cuint]

  GenProc* = object
    ## Generated wrapped proc
    iinfo* {.requiresinit.}: LineInfo
    name*: string ## Name of the generated proc on nim side
    icpp*: string ## `importcpp` pattern string
    private*: bool ## Generated proc should be private?
    args*: seq[CArg]
    retType*: NType[PNode]
    genParams*: seq[NType[PNode]]
    declType*: ProcDeclType
    header*: NimHeaderSpec
    pragma*: PPragma ## Additional pragmas on top of `importcpp`
    kind*: ProcKind ## Kind of generated nim proc
    cursor* {.requiresinit.}: CXCursor ## Original cursor for proc declaration
    docs*: seq[string]

  WrappedEntryKind* = enum
    wekMultitype
    wekProc
    wekNimDecl
    wekNimPass

  WrappedEntry* = object
    case kind*: WrappedEntryKind
      of wekMultitype:
        decls*: seq[WrappedEntry]

      of wekProc:
        gproc*: GenProc

      of wekNimDecl:
        wrapped: PNimDecl
        original*: CDecl
        cursor*: CXCursor

      of wekNimPass:
        npass: PNimDecl
        postTypes*: bool

  Postprocess* = object
    impl*: proc(we: var WrappedEntry,
                conf: WrapConfig,
                codegen: var seq[CxxCodegen]): seq[WrappedEntry]


  EnFieldVal* = object
    case isRefOther*: bool
      of true:
        othername*: string
      of false:
         value*: BiggestInt

  CxxCodegen* = object
    cursor*: CXCursor
    # TODO replace with htsparse AST tree
    code*: string
    header*: string
    filename*: RelFile


template impl1() {.dirty.} =
  case we.kind:
    of wekNimDecl: result = we.wrapped
    of wekNimPass: result = we.npass
    else: raiseAssert(&"Cannot get wrpped for kind {we.kind}")

proc wrapped*(we: WrappedEntry): PNimDecl {.inline.} = impl1()
proc mWrapped*(we: var WrappedEntry): var PNimDecl {.inline.} = impl1()

proc initHeaderSpec*(file: AbsFile): NimHeaderSpec =
  NimHeaderSpec(kind: nhskAbsolute, file: file)

proc initHeaderSpec*(global: string): NimHeaderSpec =
  NimHeaderSpec(kind: nhskGlobal, global: global)


func hasCursor*(we: WrappedEntry): bool =
  (we.kind in {wekNimDecl, wekProc})

func getCursor*(we: WrappedEntry): CXCursor =
  case we.kind:
    of wekNimDecl: result = we.cursor
    of wekProc: result = we.gproc.cursor
    else: raiseAssert(&"No cursor for kind {we.kind}")

func `$`*(we: WrappedEntry): string = $we.wrapped
func `$`*(we: seq[WrappedEntry]): string =
  {.cast(noSideEffect).}:
    we.mapPairs(rhs.wrapped.toNNode().toPString()).join("\n")

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

func `==`*(a, b: WrappedEntry): bool =
  a.kind == b.kind and
  ((
    case a.kind:
      of wekProc: a.gproc == b.gproc
      of wekMultitype: a.decls == b.decls
      of wekNimDecl: hnimast.`==`(a.wrapped, b.wrapped)
      of wekNimPass: hnimast.`==`(a.wrapped, b.wrapped) and
                     a.postTypes == b.postTypes
  ))
  # hnimast.`==`[Pnode](a.wrapped, b.wrapped)
  # (a.wrapped == b.wrapped)


func newWrappedEntry*(wrapped: PNimDecl, original: CDecl): WrappedEntry =
  WrappedEntry(
    kind: wekNimDecl,
    wrapped: wrapped,
    original: original,
    cursor: original.cursor
  )


func newWrappedEntry*(gproc: GenProc): WrappedEntry =
  WrappedEntry(gproc: gproc, kind: wekProc)

func newWrappedEntry*(wrapped: seq[WrappedEntry]): WrappedEntry =
  WrappedEntry(decls: wrapped, kind: wekMultitype)

func newWrappedEntry*(
    wrapped: PNimDecl, postTypes: bool = false
  ): WrappedEntry =

  WrappedEntry(npass: wrapped, kind: wekNimPass, postTypes: postTypes)

proc accs*(self: CDecl): CX_AccessSpecifier =
  if contains({cdkField}, self.kind):
    return self.fldAccs
  if contains({cdkMethod}, self.kind):
    return self.metAccs
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `accs=`*(self: var CDecl; it: CX_AccessSpecifier) =
  var matched: bool = false
  if contains({cdkField}, self.kind):
    if true:
      matched = true
      self.fldAccs = it
  if contains({cdkMethod}, self.kind):
    if true:
      matched = true
      self.metAccs = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc args*(self: CDecl): seq[CArg] =
  if contains({cdkMethod}, self.kind):
    return self.metArgs
  if contains({cdkFunction}, self.kind):
    return self.funArgs
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `args=`*(self: var CDecl; it: seq[CArg]) =
  var matched: bool = false
  if contains({cdkMethod}, self.kind):
    if true:
      matched = true
      self.metArgs = it
  if contains({cdkFunction}, self.kind):
    if true:
      matched = true
      self.funArgs = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

#======================  Accessing CDecl elements  =======================#
func arg*(cd: CDecl, idx: int): CArg = cd.args()[idx]
func member*(cd: CDecl, idx: int): CDecl = cd.members[idx]
func methods*(cd: CDecl, kinds: set[CXCursorKind]): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for member in cd.members:
    if (member.kind == cdkMethod) and (member.cursor.cxKind in kinds):
      result.add member

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

func initGenProc*(cursor: CXCursor, iinfo: LineInfo): GenProc =
  GenProc(cursor: cursor, iinfo: iinfo)

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
  assert cd.isOperator()
  let name = cd.lastName().dropPrefix("operator")
  case name:
    of "+=", "=", "-=", "*=",
       "<<=", ">>=", "&=", "|=", "/=", "%=", "^="
      : # NOTE `=`
      cxoAsgnOp

    of "[]":
      cxoArrayOp

    of "-", "+":
      if cd.args.len == 1:
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
      if cd.args.len == 0:
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

      else:
        raiseAssert(
          &"#[ IMPLEMENT '{name}', {cd.cursor.cxKind()} ]#" &
            $cd.cursor.getSpellingLocation()
        )


func getNimName*(cd: CDecl): string =
  case cd.kind:
    of cdkMethod, cdkFunction:
      if cd.isOperator():
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



func newPostprocess*(cb: Postprocess.impl): Postprocess =
  Postprocess(impl: cb)

func toNNode*(nhs: NimHeaderSpec): PNode =
  case nhs.kind:
    of nhskPNode:
      nhs.pnode

    of nhskAbsolute:
      newRStrLit("\"" & nhs.file.getStr() & "\"")

    of nhskGlobal:
      newRStrLit("<" & nhs.global & ">")

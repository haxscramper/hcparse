## Default implementation for user-definable callbacks
import hc_types, cxcommon, hnimast, cxtypes,
       std/[sequtils, strutils, strformat]

import hmisc/helpers
import hmisc/other/oswrap

import hc_depresolve


proc contains*(dir: AbsDir, file: AbsFile): bool =
  let dir = dir.getStr()
  let file = file.getStr()

  if file.len < dir.len:
    return false
  else:
    return file[0 .. dir.high] == dir


proc asGlobalInclude*(cursor: CXCursor, conf: WrapConfig): string =
  let loc = cursor.getSpellingLocation().get()
  for dir in conf.parseConf.includepaths:
    if loc.file in dir:
      return loc.file.getStr().dropPrefix(dir.getStr()).dropPrefix("/")

  return $loc.file

proc asIncludeFromDir*(
  cursor: CXCursor, conf: WrapConfig, dir: AbsDir): string =

  let loc = cursor.getSpellingLocation().get()
  return loc.file.getStr().dropPrefix(dir.getStr()).dropPrefix("/")

proc fixTypeName*(str: string, idx: int, conf: WrapConfig): string =
  ## Correct C++ type name to be used in nim wrappers. Convert `::` to
  ## joined name, use correct upper/lowercasing (nep1 style).
  if str.len == 0:
    return "T" & $idx

  elif str in @[
    "bool", "cint", "cuint", "ptr", "void", "char",
    "cuchar", "cstring", "cchar", "uint32", "uint16",
    "culong", "clong", "cshort", "cushort", "array",
    "ushort", "cfloat"
  ]:
    return str
  else:
    let split = str.split("::")
    var idx = 0
    while idx < split.len:
      if (idx + 1 < split.len) and
         (split[idx] in conf.collapsibleNamespaces) and
         (split[idx + 1].normalize().startsWith(split[idx])):
        # For things like `sourcetrail::SourcetrailDBWrapper`
        discard
      else:
        result.add split[idx].toPascalCase()

      inc idx


proc fixTypeName*(
  ntype: var NType[PNode], conf: WrapConfig, idx: int = 0) =
  if ntype.kind in {ntkIdent, ntkGenericSpec}:
    ntype.head = fixTypeName(ntype.head, idx, conf)

    var idx = idx
    for gen in mitems(ntype.genParams):
      conf.fixTypeName(gen, conf, idx)
      inc idx

  else:
    if ntype.rtype.isSome():
      fixTypeName(ntype.rtype.get().getIt(), conf)

    for idx, arg in mpairs(ntype.arguments):
      arg.idents[0] =
        if
          getStrVal(arg.idents[0]).len == 0:
            newPIdent("a" & $idx)

        else:
          newPIdent(fixIdentName(getStrVal(arg.idents[0])))

      conf.fixtypename(arg.vtype, conf, idx)

proc typeNameForScoped*(
    ident: CScopedIdent, conf: WrapConfig): NType[PNode] =

  var resname: string
  var genParams: seq[NType[PNode]]
  for name in ident:
    if $name.cursor notin conf.collapsibleNamespaces:
      resname &= $name.cursor
      for genParam in name.genParams:
        genParams.add conf.typeNameForScoped(genParam, conf)

  result = newNType(resname, genParams)
  conf.fixTypeName(result, conf, 0)


proc getBuiltinHeaders*(): seq[AbsDir] =
  ## According to clang `documentation <https://clang.llvm.org/docs/LibTooling.html#builtin-includes>`_
  ## libclang is needs additional precompiled headers paths in
  ## addition to default include.
  ##
  ## NOTE right now I have zero idea how it works on windows, so I
  ## will just hardcode unix-like paths.

  let version = ($getClangVersion()).split(" ")[2] # WARNING
  @[
    toAbsDir &"/usr/lib/clang/{version}/include"
  ]



let baseCppParseConfig* = ParseConfig(
  includepaths: getBuiltinHeaders(),
  globalFlags: @["-xc++", "-std=c++11"]
)

let baseCParseConfig = ParseConfig(
  includePaths: getBuiltinHeaders(),
  globalFlags: @[]
)

let baseCppWrapConf* = WrapConfig(
  isImportcpp: true,
  parseConf: baseCppParseConfig,
  isInLIbrary: (
    proc(dep: AbsFile): bool {.closure.} = true
  ),
  makeHeader: (
    proc(cursor: CXCursor, conf: WrapConfig): NimHeaderSpec {.closure.} =
      let file = cursor.asGlobalInclude(conf)
      if file.startsWith("/"):
        NimHeaderSpec(kind: nhskAbsolute, file: AbsFile(file))
      else:
        NimHeaderSpec(kind: nhskGlobal, global: file)
  ),
  getImport: (
    proc(dep: AbsFile, conf: WrapConfig): seq[string] {.closure.} =
      # if dep.startsWith("/usr/include/c++"):
      #   let (dir, name, ext) = dep.splitFile()
      #   @["cxxstd", "cxx_" & name.fixFileName()]
      # else:
      let (dir, name, ext) = dep.splitFile()
      @[
        name.splitCamel().
          mapIt(it.toLowerAscii()).join("_").
          fixFileName()
      ]
  ),
  typeNameForScoped: (
    proc(ident: CScopedIdent, conf: WrapConfig): NType[PNode] {.closure} =
      typeNameForScoped(ident, conf)
  ),
  fixTypeName: (
    proc(ntype: var NType[PNode],
         conf: WrapConfig, idx: int) {.closure.} =
      # Default implementation for type name fixes
      fixTypeName(ntype, conf, 0)
  ),
  ignoreCursor: (
    proc(cursor: CXCursor, conf: WrapConfig): bool {.closure.} =
      if not ($cursor).startsWith("__cxx11") and
        (
          cursor.cxKind() notin { ckTypedefDecl } and
          (
            # Default convention is to prefix private parts with underscores
            ($cursor).startsWith(@[ "__", "_" ]) and
            # But inlien namespaces are still parsed by default
            (not (cursor.isInlineNamespace() == 1))
          )
        ):

        if cursor.cxKind() in {ckStructDecl, ckUnionDecl} and
           not startsWith($cursor.cxType(), @["__", "_"]):
          # `typedef struct _A {} A;` for C wrapping
          return false
        else:
          return true

      if cursor.cxKind == ckNamespace and
         ($cursor in @["detail", "internal"]):
        return true

      elif cursor.cxKind == ckFieldDecl:
        if startsWith($cursor, "private"):
          return true

        else:
          if conf.isTypeInternal(cursor.cxType(), conf):
            return true

      else:
        return false
  ),
  isTypeInternal: (
    proc(cxt: CXType, conf: WrapConfig): bool {.closure.} =
      case cxt.cxKind:
        of tkPodKinds:
          result = false
        of tkTypedef:
          # debug cxt.lispRepr()
          result = startsWith($cxt, "_")
        of tkPointer:
          result = conf.isTypeInternal(cxt[], conf)
        else:
          result = false

  ),
  isInternal: (
    proc(dep: AbsFile, conf: WrapConfig,
         index: FileIndex): bool {.closure.} =
      isInternalImpl(dep, conf, index)
  ),
  depResolver: (
    proc(cursor, referencedBy: CXCursor): DepResolutionKind {.closure.} =
      if cursor.isFromMainFile():
        result = drkWrapDirectly

      else:
        result = drkImportUses
        # let loc = cursor.getSpellingLocation()
        # if loc.isSome():
        #   let loc = loc.get()
        #   let (dir, file, ext) = loc.file.splitFile()
        #   if "string" in file:
        #     result = drkWrapDirectly
  )
)

let baseCWrapConf* = baseCPPWrapConf.withIt do:
  it.isImportcpp = false

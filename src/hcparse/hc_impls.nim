## Default implementation for user-definable callbacks
import hc_types, cxcommon, hnimast, cxtypes, hc_docwrap,
       std/[sequtils, strutils, strformat, tables]

import hmisc/helpers
import hmisc/other/[oswrap, hlogger]
import hmisc/types/colorstring
import hnimast
import std/[sets]

import ./hc_depresolve, ./hc_typeconv, ./hc_wrapgen

type
  CErrorCodeKind = enum
    eckBadInteger
    eckErrorEnum

  SetLit = range[0 .. 65535]

  CErrorCode = object
    message*: string
    printArgs*: set[SetLit]
    case kind*: CErrorCodeKind
      of eckErrorEnum:
        enumIdent*: CScopedIdent

      of eckBadInteger:
        validRange*: Slice[cint]

func negativeError*(message: string, printIdx: set[SetLit] = {}): CErrorCode =
  CErrorCode(
    message: message,
    kind: eckBadInteger,
    validRange: (cint(0) .. high(cint)),
    printArgs: printIdx
  )

func errorEnum*(path: CScopedIdent): CErrorCode =
  CErrorCode(kind: eckErrorEnum, enumIdent: path)


proc errorCodesToException*(
    genProc: var GenProc, conf: WrapConf, cache: var WrapCache,
    errorMap: seq[(CSCopedIdent, CErrorCode)]
  ): seq[WrappedEntry] =

  for (ident, code) in errorMap:
    if sameNoGeneric(genProc.cdecl.ident, ident):
      var gen2 = deepCopy(genProc)
      # gen2.name = genProc.name
      genProc.name &= "Raw"
      gen2.noPragmas = gpcNoPragma

      var call = newPCall(genProc.name)
      for arg in genProc.arguments:
        call.add newPIdent(arg.name)

      let validRange = nnkInfix.newPTree(
        newPIdent(".."),
        newPCall("cint", newPLit(code.validRange.a)),
        newPCall("cint", newPLit(code.validRange.b))
      )

      case code.kind:
        of eckBadInteger:
          var msg = &"Return value of the {genProc.cdecl.cursor}"
          msg &= " is not in valid range - expected ["
          msg &= tern(code.validRange.a == low(cint), "low(cint)", $code.validRange.a)
          msg &= " .. "
          msg &= tern(code.validRange.b == high(cint), "high(cint)", $code.validRange.b)
          msg &= "], but got "

          var msg2 = ". "
          if code.message.len > 0:
            msg2 &= code.message & ". "

          var argList = newPStmtList()
          if code.printArgs.len > 0:
            msg2 &= tern(code.printArgs.len > 0, "Arguments were '", "Argument was '")

            var cnt = 0
            for idx, arg in gen2.arguments:
              if SetLit(idx) in code.printArgs:
                if cnt > 0:
                  argList.add pquote(errMsg &= "', '")

                argList.add pquote(errMsg &= $(`newPIdent(arg.name)`))
                inc cnt

            if cnt > 0:
              argList.add pquote(errMsg &= "'.")

          gen2.impl = some pquote do:
            result = `call`
            if result notin `validRange`:
              var errMsg = `msg` & $result & `msg2`
              `argList`
              raise newException(ValueError, errMsg)

        else:
          raiseImplementError("")

      return @[newWrappedEntry(
        gen2.toNNode(conf).toNimDecl(), false, currIInfo(),
        genProc.cdecl
      )]





proc contains*(dir: AbsDir, file: AbsFile): bool =
  let dir = dir.getStr()
  let file = file.getStr()

  if file.len < dir.len:
    return false
  else:
    return file[0 .. dir.high] == dir

proc asIncludeFromDir*(
  cursor: CXCursor | AbsFile, conf: WrapConf, dir: AbsDir): string =

  when cursor is CXCursor:
    let file: AbsFile = cursor.getSpellingLocation().get().file

  else:
    let file: AbsFile = cursor

  return file.getStr().dropPrefix(dir.getStr()).dropPrefix("/")

proc asGlobalInclude*(cursor: CXCursor, conf: WrapConf): string =
  let loc = cursor.getSpellingLocation().get()
  for dir in conf.parseConf.includepaths:
    if loc.file in dir:
      return asIncludeFromDir(cursor, conf, dir)

  return $loc.file

proc updateForInternalImport*(
    cursor: CXCursor | AbsFile,
    conf: WrapConf,
    dir: AbsDir, importSpec: var NimImportSpec) =

  when cursor is CXCursor:
    let file = cursor.getSpellingLocation().get().file

  else:
    let file = cursor

  importSpec.relativeDepth = relativeUpCount(dir, file)



proc asImportFromDir*(
    cursor: CXCursor | AbsFile,
    conf: WrapConf, dir: AbsDir,
    isExternalImport: bool
  ): NimImportSpec =

  result = NimImportSpec(isRelative: not isExternalImport)

  for entry in asIncludeFromDir(cursor, conf, dir).split("/"):
    result.importPath.add fixFileName(entry)

  if not isExternalImport:
    updateForInternalImport(cursor, conf, dir, result)

proc isFromDir*(cursor: CXCursor, dir: AbsDir): bool =
  cursor.getSpellingLocation().get().file in dir

proc isFromFile*(cursor: CXCursor, file: AbsFile): bool =
  cursor.getSpellingLocation().get().file == file

proc fixTypeName*(str: string, idx: int, conf: WrapConf): string =
  ## Correct C++ type name to be used in nim wrappers. Convert `::` to
  ## joined name, use correct upper/lowercasing (nep1 style).
  if str.len == 0:
    return "T" & $idx

  elif str.normalize() in @[
    "bool", "cint", "cuint", "ptr", "void", "char",
    "cuchar", "cstring", "cchar", "uint32", "uint16",
    "culong", "clong", "cshort", "cushort", "array",
    "ushort", "cfloat", "cstringarray", "pointer",
    "culonglong", "clonglong",

    "cwchar", "cchar16", "cchar32"
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


proc fixTypeName*(ntype: var NimType, conf: WrapConf, idx: int = 0) =
  case ntype.kind:
    of ctkIdent:
      ntype.nimName = fixTypeName(ntype.nimName, idx, conf)

      var idx = idx
      for gen in mitems(ntype.genericParams):
        conf.fixTypeName(gen, conf, idx)
        inc idx

    of ctkProc:
      conf.debug ntype.kind
      if notNil ntype.returnType:
        fixTypeName(ntype.returnType, conf)

      for idx, arg in mpairs(ntype.arguments):
        arg.name =
          if arg.name.len == 0:
            "a" & $idx

          else:
            fixIdentName(arg.name)

        conf.fixtypename(arg.nimType, conf, idx)


proc typeNameForScoped*(
    ident: CScopedIdent, conf: WrapConf): NimType {.deprecated: "getParamsForType".} =

  assert ident.len > 0
  var resname: string
  var genParams: seq[NimType]
  for name in ident:
    if name.getName() notin conf.collapsibleNamespaces:
      resname &= capitalizeAscii(name.getName())
      for genParam in name.genParams:
        let tmp = conf.typeNameForScoped(genParam, conf)
        genParams.add tmp


  assert resname.len > 0,
            &"Scoped indent '{ident}' " &
              "got converted to zero-length nim type"

  result = newNimType(resname, genParams)
  conf.fixTypeName(result, conf, 0)

proc getImportUsingDependencies*(
    conf: WrapConf,
    dependency: AbsFile,
    wrapConfurations: seq[WrapConf],
    isExternalImport: bool
  ): NimImportSpec =

  if conf.isInLibrary(dependency, conf):
    return asImportFromDir(
      dependency, conf, conf.baseDir, isExternalImport)

  for config in wrapConfurations:
    if config.isInLibrary(dependency, config):
      return config.getImport(dependency, config, true)

proc getClangSemVersion*(): string =
  ($getClangVersion()).split(" ")[2] # WARNING

proc getClangInclude*(): AbsDir =
  let version = getClangSemVersion()
  return AbsDir(&"/usr/lib/clang/{version}/include")


proc getBuiltinHeaders*(): seq[AbsDir] =
  ## According to clang `documentation <https://clang.llvm.org/docs/LibTooling.html#builtin-includes>`_
  ## libclang is needs additional precompiled headers paths in
  ## addition to default include.
  ##
  ## NOTE right now I have zero idea how it works on windows, so I
  ## will just hardcode unix-like paths.
  @[getClangInclude()]


let baseCppParseConf* = ParseConf(
  includepaths: getBuiltinHeaders(),
  globalFlags: @["-xc++", "-std=c++11"]
)

let baseCParseConf* = ParseConf(
  includePaths: getBuiltinHeaders(),
  globalFlags: @[]
)




let baseCppWrapConf* = WrapConf(
  isImportcpp: true,
  parseConf: baseCppParseConf,
  isInLibrary: (
    proc(dep: AbsFile, conf: WrapConf): bool {.closure.} =
      dep.startsWith(conf.baseDir)
  ),
  makeHeader: (
    proc(cursor: CXCursor, conf: WrapConf): NimHeaderSpec {.closure.} =
      let file = cursor.asGlobalInclude(conf)
      if file.startsWith("/"):
        NimHeaderSpec(kind: nhskAbsolute, file: AbsFile(file))
      else:
        NimHeaderSpec(kind: nhskGlobal, global: file)
  ),
  getImport: (
    proc(dep: AbsFile, conf: WrapConf, isExternalImport: bool):
      NimImportSpec {.closure.} =
      let (dir, name, ext) = dep.splitFile()
      result = initNimImportSpec(
        isExternalImport,
        @[
          name.splitCamel().
            mapIt(it.toLowerAscii()).join("_").
            fixFileName()])

      if isExternalImport:
        if conf.wrapName.isSome():
          result.importPath = conf.wrapName.get() & result.importPath

      else:
        updateForInternalImport(dep, conf, conf.baseDir, result)
  ),
  typeNameForScoped: (
    proc(ident: CScopedIdent, conf: WrapConf): NimType {.closure} =
      typeNameForScoped(ident, conf)
  ),
  isDistinct: (
    proc(ident: CSCopedIdent, conf: WrapConf, cache: var WrapCache):
      bool {.closure.} =

      false
  ),
  fixTypeName: (
    proc(ntype: var NimType, conf: WrapConf, idx: int) {.closure.} =
      # Default implementation for type name fixes
      fixTypeName(ntype, conf, 0)
  ),
  ignoreCursor: (
    proc(cursor: CXCursor, conf: WrapConf): bool {.closure.} =
      if not ($cursor).startsWith("__cxx11") and
        (
          cursor.cxKind() notin { ckTypedefDecl } and
          (
            # Default convention is to prefix private parts with underscores
            ($cursor).startsWith(@[ "__", "_" ]) and
            # But inline namespaces are still parsed by default
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
    proc(cxt: CXType, conf: WrapConf): bool {.closure.} =
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
    proc(dep: AbsFile, conf: WrapConf,
         index: hc_types.FileIndex): bool {.closure.} =
      isInternalImpl(dep, conf, index)
  ),
  prefixForEnum: (
    proc(
      enumId: CScopedIdent, conf: WrapConf, cache: var WrapCache
    ): string =
      result = enumId[^1]
        .getName()
        .splitCamel()
        .mapIt(it[0].toLowerAscii())
        .join("")

      cache.enumPrefs.incl result
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
  ),
  refidFile: RelFile("refid-map.json")
)

let baseCWrapConf* = baseCPPWrapConf.withDeepIt do:
  it.isImportcpp = false

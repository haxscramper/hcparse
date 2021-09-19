import
  ./interop_ir/wrap_store,
  hnimast/hast_common,
  hnimast,
  hnimast/proc_decl

import
  hmisc/core/all,
  hmisc/macros/argpass,
  hmisc/other/oswrap,
  std/[macros, sequtils, sets]

type
  CodegenConf* = object
    isIcpp*: bool

const
  cxxCodegenConf* = CodegenConf(
    isIcpp: true
  )

  cCodegenConf* = cxxCodegenConf.withIt do:
    it.isIcpp = false


func getImport*(conf: CodegenConf): string =
  if conf.isIcpp: "importcpp" else: "importc"

func toNNode*[N](lib: CxxLibImport, asImport: bool): N =
  if asImport:
    result = newNTree[N](
      nnkImportStmt,
      lib.importPath.mapIt(newNIdent[N](it)).
        foldl(newXCall("/", a, b)))

  else:
    result = newNTree[N](nnkExportStmt, newNIdent[N](lib.getFilename()))

proc toNNode*[N](t: CxxTypeUse, conf: CodegenConf): NType[N] =
  case t.kind:
    of ctkIdent:
      result = newNNType[N](t.nimName, @[])

    of ctkPtr:
      result = newNType[N]("ptr", @[toNNode[N](t.wrapped, conf)])

    else:
      raise newImplementKindError(t)

proc toNNode*[N](arg: CxxArg, conf: CodegenConf): NIdentDefs[N] =
  newNIdentDefs[N](
    arg.nimName,
    toNNode[N](arg.nimType, conf),
    value = some newEmptyNNode[N]())

proc toNNode*[N](field: CxxField, conf: CodegenConf): ObjectField[N] =
  result = ObjectField[N](
    isTuple: false,
    name: field.nimName(),
    isExported: true,
    docComment: field.docComment.get(""),
    fldType: toNNode[N](field.getType(), conf))

  let cxx = field.cxxName.cxxStr()
  if cxx != field.nimName():
    result.addPragma(
      conf.getImport(), newNLit[N, string](field.cxxName.cxxStr()))


proc toNNode*[N](header: CxxHeader): N =
  case header.kind:
    of chkGlobal: newNLit[N, string](header.global)
    of chkAbsolute: newNLit[N, string](header.file.string)
    of chkPNode: newNLit[N, string](header.other)

proc toNNode*[N](
    def: CxxProc,
    conf: CodegenConf,
    onConstructor: CxxTypeKind = ctkIdent
  ): ProcDecl[N] =

  result = newProcDecl[N](def.nimName)
  result.exported = true
  result.docComment = def.docComment.get("")

  if cpfExportc in def.flags:
    result.addPragma("exportc", newNLit[N, string](def.cxxName.cxxStr()))

  else:
    if def.header.isSome():
      result.addPragma("header", toNNode[N](def.header.get()))

    result.addPragma("importcpp", newNLit[N, string](def.getIcppStr(ctkPtr)))

  if def.isConstructor and onConstructor == ctkIdent:
    result.addPragma("constructor")


  # nim's overload-resolution-in-generics magic
  proc_decl.`returnType=`(
    result, toNNode[N](def.getReturn(onConstructor), conf))

  if def.methodOf.isSome():
    result.addArgument(
      "this", toNNode[N](def.methodOf.get(), conf))

  for arg in def.arguments:
    result.addArgument toNNode[N](arg, conf)

proc toNNode*[N](entry: CxxEntry, conf: CodegenConf): seq[NimDecl[N]] =
  case entry.kind:
    of cekObject:
      let obj = entry.cxxObject
      var res = newObjectDecl[N](obj.nimName)
      res.docComment = obj.docComment.get("")
      res.addPragma("bycopy")
      # res.addPragma("inheritable")
      # res.addPragma("byref")
      res.addPragma("header", toNNode[N](obj.header.get()))
      res.addPragma(conf.getImport(), newNLit[N, string](obj.getIcppStr()))

      for field in obj.mfields:
        res.add toNNode[N](field, conf)

      result.add toNimDecl(res)

      for meth in obj.methods:
        result.add toNNode[N](meth, conf, ctkPtr)

      for n in obj.nested:
        result.add toNNode[N](n, conf)


    of cekProc:
      result.add toNNode[N](
        entry.cxxProc, conf, ctkPtr).toNimDecl()

    of cekForward:
      raise newUnexpectedKindError(
        entry,
        "forward declaration must be converted to pass/import",
        "or promoted into full type declartions ",
        "by forward declaration patch stage. This code should",
        "not be reached. declaration is ", $entry)

    of cekEmpty:
      discard

    else:
      raise newImplementKindError(entry)

proc toNNode*[N](entries: seq[CxxEntry], conf: CodegenConf): seq[NimDecl[N]] =
  var types: seq[NimTypeDecl[N]]
  var other: seq[NimDecl[N]]
  for item in entries:
    for conv in toNNode[N](item, conf):
      if conv of {nekObjectDecl, nekAliasDecl, nekEnumDecl}:
        types.add toNimTypeDecl(conv)

      else:
        other.add conv

  result.add toNimDecl(types)
  result.add other

proc toNNode*[N](file: CxxFile, conf: CodegenConf): N =
  result = newNTree[N](nnkStmtList)
  for dep in items(file.imports):
    if dep.getLibrary() == file.getLibrary():
      let (pDep, pFile) = (dep.getFile(), file.getFile())
      let (depth, parts) = importSplit(
        AbsDir"/tmp" / pFile, AbsDir"/tmp" / pDep)

      let depthDots = tern(depth == 0, @["."], mapIt(0 ..< depth, ".."))
      let newImp = cxxLibImport(file.getLibrary(), depthDots & parts)

      result.add toNNode[N](newImp, true)

    else:
      let newImp = cxxLibImport(
        dep.getLibrary(), dep.getLibrary() & dep.importPath)

      result.add toNNode[N](newImp, true)

  for exp in items(file.exports):
    result.add toNNode[N](exp, false)

  for decl in toNNode[N](file.entries, conf):
    result.add toNNode[N](decl)

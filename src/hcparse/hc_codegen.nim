import
  ./interop_ir/wrap_store,
  hnimast/hast_common,
  hnimast,
  hnimast/proc_decl

import
  hmisc/core/all,
  hmisc/macros/argpass,
  std/[macros, sequtils]

proc toNNode*[N](t: CxxTypeUse): NType[N] =
  case t.kind:
    of ctkIdent:
      result = newNNType[N](t.nimName, @[])

    of ctkPtr:
      result = newNType[N]("ptr", @[toNNode[N](t.wrapped)])

    else:
      raise newImplementKindError(t)

proc toNNode*[N](arg: CxxArg): NIdentDefs[N] =
  newNIdentDefs[N](
    arg.nimName,
    toNNode[N](arg.nimType),
    value = some newEmptyNNode[N]())

proc toNNode*[N](header: CxxHeader): N =
  case header.kind:
    of chkGlobal: newNLit[N, string](header.global)
    of chkAbsolute: newNLit[N, string](header.file.string)
    of chkPNode: newNLit[N, string](header.other)

proc toNNode*[N](def: CxxProc, onConstructor: CxxTypeKind = ctkIdent): ProcDecl[N] =
  result = newProcDecl[N](def.nimName)
  result.exported = true

  if cpfExportc in def.flags:
    result.addPragma("exportc", newNLit[N, string](def.cxxName.cxxStr()))

  else:
    if def.header.isSome():
      result.addPragma("header", toNNode[N](def.header.get()))

    result.addPragma("importcpp", newNLit[N, string](def.getIcppStr(ctkPtr)))

  if def.isConstructor and onConstructor == ctkIdent:
    result.addPragma("constructor")

  # nim's overload-resolution-in-generics magic
  proc_decl.`returnType=`(result, toNNode[N](def.getReturn(onConstructor)))

  for arg in def.arguments:
    result.addArgument toNNode[N](arg)

proc toNNode*[N](entry: CxxEntry): seq[NimDecl[N]] =
  case entry.kind:
    of cekObject:
      let obj = entry.cxxObject
      var res = newObjectDecl[N](obj.nimName)

      res.addPragma("inheritable")
      res.addPragma("byref")
      res.addPragma("header", toNNode[N](obj.header.get()))
      res.addPragma("importcpp", newNLit[N, string](obj.getIcppStr()))

      for meth in obj.methods:
        result.add toNNode[N](meth, ctkPtr)

      for n in obj.nested:
        result.add toNNode[N](n)

      result.add toNimDecl(res)

    of cekProc:
      result.add toNNode[N](entry.cxxProc, ctkPtr).toNimDecl()

    else:
      raise newImplementKindError(entry)

proc toNNode*[N](entries: seq[CxxEntry]): seq[NimDecl[N]] =
  var types: seq[NimTypeDecl[N]]
  var other: seq[NimDecl[N]]
  for item in entries:
    for conv in toNNode[N](item):
      if conv of {nekObjectDecl, nekAliasDecl, nekEnumDecl}:
        types.add toNimTypeDecl(conv)

      else:
        other.add conv

  result.add toNimDecl(types)
  result.add other

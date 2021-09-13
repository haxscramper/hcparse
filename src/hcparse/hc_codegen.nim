import
  ./interop_ir/wrap_store,
  hnimast/hast_common

import
  hmisc/core/all,
  hmisc/macros/argpass,
  std/[macros, sequtils]

proc toNNode*[N](t: CxxTypeUse): N =
  case t.kind:
    of ctkIdent:
      result = newNIdent[N](t.nimName)

    of ctkPtr:
      result = newNTree[N](nnkPtrTy, toNNode[N](t.wrapped))

    else:
      raise newImplementKindError(t)

proc toNNode*[N](arg: CxxArg): N =
  newNTree[N](
    nnkIdentDefs,
    newNIdent[N](arg.nimName),
    toNNode[N](arg.nimType), newEmptyNNode[N]())

proc toNNode*[N](header: CxxHeader): N =
  case header.kind:
    of chkGlobal: newNLit[N, string](header.global)
    of chkAbsolute: newNLit[N, string](header.file.string)
    of chkPNode: newNLit[N, string](header.other)

proc toNNode*[N](def: CxxProc, onConstructor: CxxTypeKind = ctkIdent): N =
  var pragmas: seq[N]
  if cpfExportc in def.flags:
    pragmas.add newIdentColonExpr(
      "exportc", newNLit[N, string](def.cxxName.cxxStr()))

  else:
    if def.header.isSome():
      pragmas.add newIdentColonExpr[N](
        "header", toNNode[N](def.header.get()))

    pragmas.add newIdentColonExpr(
      "importcpp", newNLit[N, string](def.getIcppStr(ctkPtr)))

  if def.isConstructor and onConstructor == ctkIdent:
    pragmas.add newNIdent[N]("constructor")

  let args = newNTree[N](
    nnkFormalParams,
    toNNode[N](def.getReturn(onConstructor)) & def.arguments.map(toNNode[N]))

  newNTree[N](
    nnkProcDef,
    newNTree[N](nnkPostfix, newNIdent[N]("*"), newNIdent[N](def.nimName)),
    newEmptyNNode[N](),
    newEmptyNNode[N](),
    args,
    nnkPragma.newTree(pragmas),
    newEmptyNNode[N](),
    newEmptyNNode[N]())


proc toNNode*[N](entry: CxxEntry): N =
  result = newNTree[N](nnkStmtList)
  case entry.kind:
    of cekObject:
      var fieldList = newNTree[N](nnkRecList)
      let obj = entry.cxxObject

      result.add newNTree[N](
        nnkTypeDef,
        newNTree[N](
          nnkPragmaExpr,
          newNIdent[N](obj.nimName),
          newNTree[N](
            nnkPragma,
            newNIdent[N]("inheritable"),
            newNIdent[N]("byref"),
            newIdentColonExpr("header", toNNode[N](obj.header.get())),
            newIdentColonExpr("importcpp", newNLit[N, string](obj.getIcppStr())))),
        newEmptyNNode[N](),
        newNTree[N](
          nnkObjectTy,
          newEmptyNNode[N](),
          tern(
            obj.super.len == 0,
            newEmptyNNode[N](),
            newNTree[N](nnkOfInherit, toNNode[N](obj.super[0]))),
          fieldList))


      for meth in obj.methods:
        result.add toNNode[N](meth, ctkPtr)

      for n in obj.nested:
        result.add toNNode[N](n)

    of cekProc:
      result = toNNode[N](entry.cxxProc, ctkPtr)

    else:
      raise newImplementKindError(entry)

proc toNNode*[N](entries: seq[CxxEntry]): N =
  var types: seq[N]
  var other: seq[N]
  for item in entries:
    for conv in toNNode[N](item):
      if conv.nnKind() == nnkTypeDef:
        types.add conv

      else:
        other.add conv

  result = newNTree[N](nnkStmtList, newTree[N](nnkTypeSection, types))
  result.add other

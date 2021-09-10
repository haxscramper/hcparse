
import
  ./wrap_store,
  ./wrap_convert,
  ./wrap_icpp,
  std/[macros, strutils, sequtils],
  hmisc/core/[all, code_errors],
  hmisc/macros/argpass



type
  WrapCtx = object
    namespace: seq[string]
    inClass: bool
    nimClassName: NimNode
    cxxClassName: string
    header: string

func getIcpp(ctx: WrapCtx, name: string, isType: bool): string =
  if ctx.inClass:
    if isType:
      join(ctx.namespace & name, "::")

    else:
      "#." & name

  else:
    join(ctx.namespace & name, "::")

func cxxTypeAux*(t: NimNode): CxxType =
  case t.kind:
    of nnkIdent:
      result = initCxxType(t.strVal())

    of nnkEmpty:
      result = initCxxType("void")

    of nnkPtrTy:
      result = cxxTypeAux(t[0]).wrap(ctkPtr)

    else:
      raise newImplementKindError(t)

func cxxArgAux(arg: NimNode): CxxArg =
  initCxxArg(arg[0].strVal(), arg[1].cxxTypeAux())

func procDeclAux(entry: NimNode, ctx: WrapCtx): CxxProc =
  let name =
    if entry.body.kind == nnkEmpty:
      entry.name().strVal()

    else:
      assertNodeKind(entry.body()[0], {nnkCall})
      entry.body()[0][0].strVal()

  result.nimName = name
  result.cxxName = ctx.namespace & name
  result.header = some initCxxHeader(ctx.header)

  var filter: seq[NimNode]
  result.returnType = entry.params()[0].cxxTypeAux()

  for pr in entry.pragma:
    if pr.eqIdent("const"):
      result.isConst = true

    elif pr.eqIdent("constructor"):
      result.nimName = "new" & ctx.cxxClassName
      result.cxxName = ctx.namespace & ctx.cxxClassName
      result.constructorOf = some ctx.cxxClassName

    else:
      filter.add pr

  if ctx.inClass and not result.isConstructor():
    result.arguments.add initCxxArg("this", initCxxType(ctx.cxxClassName))
    result.methodOf = some initCxxType(ctx.cxxClassName)

  for arg in entry.params()[1 .. ^1]:
    result.arguments.add cxxArgAux(arg)

  entry.pragma = nnkPragma.newTree(filter)
  result.isOperator = not allIt(name, it in IdentChars)


func stmtAux(entry: NimNode, ctx: WrapCtx): seq[CxxEntry]

func headerAux(name: string, body: seq[NimNode], ctx: WrapCtx): seq[CxxEntry] =
  var ctx = ctx
  ctx.header = name
  for node in body:
    result.add stmtAux(node, ctx)

func namespaceAux(name: string, body: seq[NimNode], ctx: WrapCtx): seq[CxxEntry] =
  var ctx = ctx
  ctx.namespace.add name
  for node in body:
    result.add stmtAux(node, ctx)

func splitClassName(name: NimNode):
  tuple[nimName: NimNode, cxxName: string, super: Option[NimNode]] =

  case name.kind:
    of nnkStrLit, nnkIdent:
      result.cxxName = name.strVal()
      result.nimName = ident(name.strVal())

    of nnkInfix:
      case name[0].strVal():
        of "as":
          result.cxxName = name[1].strVal()
          result.nimName = name[2]

        of "of":
          result.super = some name[2]
          let (nim, cxx, _) = splitClassName(name[1])
          result.nimName = nim
          result.cxxName = cxx

        else:
          raise newImplementError()

    else:
      raise newUnexpectedKindError(name)


proc flatStmtList(nodes: seq[NimNode]): seq[NimNode] =
  proc aux(node: NimNode): seq[NimNode] =
    if node of nnkStmtList:
      for sub in node:
        result.add aux(sub)

    else:
      result.add node

  for node in nodes:
    result.add aux(node)


func classAux(name: NimNode, body: seq[NimNode], ctx: WrapCtx): CxxObject =
  var ctx = ctx
  let (nim, cxx, super) = splitClassName(name)
  ctx.inClass = true
  ctx.nimClassName = nim
  if super.isSome():
    result.super.add cxxTypeAux(super.get())

  ctx.cxxClassName = cxx

  result.nimName = nim.repr()
  result.cxxName = ctx.namespace & cxx
  result.icpp.ctype(ctx.namespace, ctx.cxxClassName)
  result.header = some initCxxHeader(ctx.header)

  for entry in body.flatStmtList():
    case entry.kind:
      of nnkProcDef:
        result.methods.add procDeclAux(entry, ctx)

      of nnkStmtList:
        for stmt in entry:
          result.nested.add stmtAux(stmt, ctx)

      else:
        raise newImplementKindError(entry)


  # result = newStmtList(
  #   nnkTypeSection.newTree(
  #     nnkTypeDef.newTree(
  #       nnkPragmaExpr.newTree(
  #         nnkPostfix.newTree(ident"*", nim),
  #         nnkPragma.newTree(
  #           newEcE("importcpp", newLit()),
  #           (if isByref: ident"byref" else: ident"bycopy"),
  #           ident("inheritable"),
  #           newEcE("header", newLit()))),
  #       newEmptyNode(),
  #       nnkObjectTy.newTree(
  #         newEmptyNode(),
  #         (if super.isSome(): nnkOfInherit.newTree(super.get()) else: newEmptyNode()),
  #         fieldList))) & resList)



func stmtAux(entry: NimNode, ctx: WrapCtx): seq[CxxEntry] =
  case entry.kind:
    of nnkProcDef:
      result.add procDeclAux(entry, ctx)

    of nnkCommand:
      let kind = entry[0].strVal()

      case kind:
        of "namespace":
          result = namespaceAux(entry[1].strVal(), entry[2..^1], ctx)

        of "class":
          result.add classAux(entry[1], entry[2..^1], ctx)

        of "static", "struct", "enum", "var", "let", "const":
          raise newImplementError(kind)


        else:
          raise newImplementKindError(kind)

    of nnkStmtList:
      for stmt in entry:
        result.add stmtAux(stmt, ctx)

    else:
      raise newUnexpectedKindError(entry, treeRepr(entry))



macro wrapheader*(name: static[string], body: untyped): untyped =
  result = toNNode[NimNode](headerAux(name, toSeq(body), WrapCtx()))
  echo result.repr()

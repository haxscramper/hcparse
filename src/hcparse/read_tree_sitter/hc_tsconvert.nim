## This module implements conversion of the C and C++ code to nim.
## Translation is performed using simplistic AST-AST rewrite, and requires
## manual correction afterwards.

import
  htsparse/cpp/cpp

import
  std/[strutils]

import
  ./hc_tsreader,
  ../hc_impls,
  ../processor/[wrap_store, hc_postprocess],
  ../codegen/hc_codegen

import
  hmisc/core/all,
  hmisc/other/oswrap,
  hmisc/wrappers/[treesitter],
  hmisc/types/colorstring,
  hmisc/algo/namegen,
  hnimast

export parseCppString

proc getIdent*(node: CppNode, c: var StringNameCache): PNode =
  newPIdent(c.fixIdentName(node.strVal(), "f"))

proc wrap*(ntype: NType[PNode], kind: CxxTypeKind): NType[PNode] =
  case kind:
    of ctkPtr:
       return newNType("ptr", @[ntype])

    of ctkDynamicArray:
      return newNType("ptr", @[newNType("UncheckedArray", @[ntype])])

    else:
      raise newImplementKindError(kind)

proc debug(node: CppNode) =
  echo node.treeRepr()

proc failNode(node: CppNode) {.noreturn.} =
  raise newUnexpectedKindError(
    node,
    node.strVal() &
      "\n" &
      $node.treeRepr(
        unnamed = true,
        opts = hdisplay(maxLen = 5, maxDepth = 3)))

initPointerWraps(newPType, NType[PNode])

proc toPType*(node: CppNode): NType[PNode] =
  case node.kind:
    of cppTypeDescriptor:
      result = toPType(node["type"])
      if "declarator" in node:
        pointerWraps(node["declarator"], result)

    of cppStructSpecifier:
      result = toPType(node["name"])

    else:
      result = newPType(mapTypeName(node))

proc skip*(node: CppNode, idx: int, kind: set[CppNodeKind]): CppNode =
  if node.kind in kind:
    result = node[idx]

  else:
    result = node

proc conv*(node: CppNode, str: string, c: var StringNameCache): PNode =
  template `~`(expr: CppNode): untyped = conv(expr, str, c)

  case node.kind:
    of cppTranslationUnit,
       cppCompoundStatement,
       cppExpressionStatement:
      result = newPStmtList()
      for sub in items(node):
        result.add ~sub

    of cppReturnStatement:
      result = nnkReturnStmt.newPTree(
        tern(node.len > 0, ~node[0], newEmptyPNode()))

    of cppCallExpression:
      result = nnkCall.newPTree(~node[0])
      for arg in node[1]:
        result.add ~arg

    of cppBinaryExpression, cppAssignmentExpression:
      result = newXCall(node{1}.mapOpName(), ~node[0], ~node[1])

    of cppUnaryExpression:
      result = newXCall(node{0}.mapOpName(), ~node[0])

    of cppParenthesizedExpression:
      result = nnkPar.newPTree(~node[0])

    of cppIdentifier,
       cppFieldIdentifier,
       cppTypeIdentifier,
       cppNamespaceIdentifier:
      result = newPident(c.fixIdentName(node.strVal(), "a"))

    of cppFieldExpression:
      # if node{1}.strVal() == "->":
      result = nnkDotExpr.newPTree(~node[0], ~node[1])

    of cppPreprocInclude:
      let file = RelFile(node[0].strVal()[1..^2])
      result = newPTree(
        nnkImportStmt,
        newPIdent(file.withoutExt().getStr() & file.ext()))

    of cppComment, cppPreprocFunctionDef:
      result = newEmptyPNode()

    of cppNumberLiteral:
      result = newPIdent(node.strVal())

    of cppTrue:
      result = newPIdent("true")

    of cppFalse:
      result = newPIdent("false")

    of cppUpdateExpression:
      result = newXCall("postInc", ~node[0])

    of cppStringLiteral:
      result = newPLit(node.strVal().strip(chars = {'"'}))

    of cppPointerExpression:
      if node{0}.strVal() == "*":
        result = newXCall("[]", ~node[0])

      else:
        result = newXCall("addr", ~node[0])

    of cppBreakStatement:
      result = newPBreak()

    of cppSubscriptExpression:
      result = newXCall("[]", ~node["argument"], ~node["index"])

    of cppCastExpression:
      result = nnkCast.newPTree(
        node["type"].toPType().toNNode(), ~node["value"])

    of cppNull:
      result = newPIdent("nil")

    of cppPreprocCall, cppPreprocIfdef, cppPreprocDef:
      result = newEmptyPNode()

    of cppSwitchStatement:
      var before = newPStmtList()
      result = newCase(~node["condition"])
      var emptyList: seq[PNode]
      for branch in node["body"]:
        case branch.kind:
          of cppComment: discard
          of cppCaseStatement:
            if branch.len == 1:
              emptyList.add ~branch[0]
              continue

            var body = newPStmtList()
            for item in branch[1..^1]:
              body.add ~item

            if "value" notin branch:
              result.addBranch(
                emptyList,
                body[0 .. ^tern(body[^1].kind == nkBreakStmt, 2, 1)].
                  newPStmtList().fixEmptyStmt())

            elif 0 < body.len:
              result.addBranch(
                emptyList &  ~branch["value"],
                body[0 .. ^tern(body[^1].kind == nkBreakStmt, 2, 1)].
                  newPStmtList().fixEmptyStmt())

            else:
              result.addBranch(emptyList & ~branch["value"], body)

            emptyList = @[]

          else:
            # It is possible to arbitrary statements and declarations after
            # the `switch` part, so everything is going to be added before
            # the `case`
            before.add ~branch

      if 0 < before.len:
        before.add result
        return before

    of cppStructSpecifier, cppTypeDefinition:
      var coms: seq[CxxComment]
      var anon: seq[NimDecl[PNode]]
      result = nnkTypeSection.newPTree()
      var conf = cxxCodegenConf.withIt do:
        discard

      let fix = baseFixConf.withIt do:
        it.typeStore = newTypeStore()
        it.onGetBind():
          # We are performing code translation here, so there is no need to
          # add any bindings to the generated entries.
          return cxxNoBind()

      for ent in toCxxTypeDefinition(node, coms).postFixEntries(fix, CxxLibImport()):
        for en in ent.toNNode(conf, anon):
          result.add toNNode(en)

        for an in anon:
          result.add toNNode(an)

    of cppFunctionDefinition:
      var impl = newPProcDecl(node["declarator"].getName())
      for arg in node["declarator"].skip(0, {cppPointerDeclarator})["parameters"]:
        var name: string
        var argType: NType[PNode]

        if "declarator" in arg:
          name = c.fixIdentName(arg.getName(), "a")
          argType = arg["type"].toPType()
          pointerWraps(arg["declarator"], argType)

        else:
          name = "arg"
          argType = arg["type"].toPType()

        impl.addArgument(name, argType)

      impl.returnType = node["type"].toPType()
      if impl.returnType.isSome():
        impl.returnType = impl.returnType.get().withIt do:
          pointerWraps(node["declarator"], it)

      impl.impl = ~node["body"]

      result = impl.toNNode()


    of cppInitializerList:
      result = newPTree(nnkPar)
      for item in node:
        result.add ~item

    of cppInitializerPair:
      result = newPTree(nnkExprColonExpr, ~node[0], ~node[1])

    of cppFieldDesignator:
      result = ~node[0]

    # of cppFieldIdentifier:
    #   result = newPIdent(node.strVal())

    of cppForStatement:
      let update = tern("update" in node, ~node["update"], newEmptyPNode())
      result = newWhile(~node["condition"], ~node[^1], update)
      if "initializer" in node:
        result = newBlock(~node["initializer"], result)

    of cppWhileStatement:
      result = newWhile(newPar ~node["condition"], ~node["body"])

    of cppConcatenatedString:
      result = newPLit(node.strVal())

    of cppConditionClause:
      result = ~node[0]

    of cppSizeofExpression:
      result = newXCall("sizeof", ~node["value"])

    of cppTypeDescriptor:
      result = toPType(node).toNNode()

    of cppCharLiteral:
      result = newPLit(node.strVal()[1])

    of cppIfStatement:
      result = newIf(
        ~node["condition"],
        ~node["consequence"],
        tern("alternative" in node, ~node["alternative"], nil))

    of cppDoStatement:
      let body = ~node["body"]
      result = newPStmtList(body, newWhile(~node["condition"], body))

    of cppDeclaration:
      let decl = node["declarator"]
      var value = newEmptyPNode()

      var name = c.fixIdentName(node.getName(), "f").newPIdent()
      var declType = node["type"].toPType()
      pointerWraps(node["declarator"], declType)

      if "value" in decl:
        if decl["declarator"] of cppArrayDeclarator:
          value = newPTree(nnkBracket)
          for item in decl["value"]:
            value.add ~item

          value = newXCall("@", value)

        else:
          value = ~decl["value"]
          if decl["value"] of cppArgumentList:
            # `Instr16 instr16(&emu, &instr);` is converted into
            # `let instr16: Instr16 = initInstr16(addr emu, addr instr)`
            value = newXCall(newPIdent("init" & node["type"].strVal()),
                             value[0..^1])

          

      result = nnkVarSection.newPTree(
        nnkIdentDefs.newPTree(name, declType.toNNode(), value))

    # of cppCatchClause:
    #   result = newPTree(nnkExceptBranch)

    of cppQualifiedIdentifier:
      result = newXCall(".", ~node[0], ~node[1])


    of cppContinueStatement:
      result = newPTree(nnkContinueStmt)

    of cppCatchClause:
      result = newPTree(nnkExceptBranch)
      let param = node["parameters"]
      if param.len == 0:
        result.add ~node["body"]

      else:
        result.add newXCall("as", ~param[0]["type"], ~param[0]["declarator"])

    of cppTryStatement:
      result = newPTree(nnkTryStmt, ~node["body"])
      for clause in node[1..^1]:
        result.add ~clause

    of cppConditionalExpression:
      result = newPar(
        newIf(
          newPar ~node["condition"],
          newPar ~node["consequence"],
          newPar ~node["alternative"]
        )
      )

    of cppArgumentList:
      result = newPTree(nnkPar)
      for item in node:
        result.add ~item

    else:
      failNode node


when isMainModule:
  for file in walkDir(
      AbsDir"/tmp/infiles",
      AbsFile,
      recurse = true,
      exts = @["h", "c", "hpp", "cpp"]):
  # for file in [AbsFile"/tmp/in.c"]:
    echo file
    var str = file.readFile()
    var c: StringNameCache
    let node = parseCppString(addr str)
    # debug node
    let code = node.conv(str, c).`$`
    writeFile(file.withBaseSuffix(file.ext()).withExt("nim"), code)


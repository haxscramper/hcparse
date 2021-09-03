import
  htsparse/cpp/cpp

import
  std/[json, strutils]

import
  ./hc_types,
  ./hc_save,
  ./hc_tsreader

import
  hmisc/core/all,
  hmisc/wrappers/[treesitter],
  hmisc/other/oswrap,
  hmisc/types/colorstring,
  hmisc/algo/namegen,
  hnimast

startHax()

export parseCppString

proc toPType*(node: CppNode): NType[PNode] = newPType(mapTypeName(node))
proc getIdent*(node: CppNode, c: var StringNameCache): PNode =
  newPIdent(c.fixIdentName(node.strVal(), "f"))

initPointerWraps(newPType, NType[PNode])


proc conv(node: CppNode, str: string, c: var StringNameCache): PNode =
  template `~`(expr: CppNode): untyped = conv(expr, str, c)

  case node.kind:
    of cppTranslationUnit,
       cppCompoundStatement,
       cppExpressionStatement:
      result = newPStmtList()
      for sub in items(node):
        result.add ~sub

    of cppReturnStatement:
      result = nnkReturnStmt.newPTree(~node[0])

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

    of cppIdentifier, cppFieldIdentifier:
      result = newPident(node.strVal())

    of cppFieldExpression:
      result = nnkDotExpr.newPTree(~node[0], ~node[1])

    of cppPreprocInclude:
      result = newPTree(nnkImportStmt, newPident(node.strVal()))

    of cppComment, cppPreprocFunctionDef:
      result = newEmptyPNode()

    of cppNumberLiteral:
      result = newPIdent(node.strVal())

    of cppUpdateExpression:
      result = newXCall("postInc", ~node[0])

    of cppStringLiteral:
      result = newPLit(node.strVal().strip(chars = {'"'}))

    of cppPointerExpression:
      result = newXCall("[]", ~node[0])

    of cppBreakStatement:
      result = newPBreak()

    of cppSubscriptExpression:
      result = newXCall("[]", ~node["argument"], ~node["index"])

    of cppNull:
      result = newPIdent("nil")

    of cppPreprocCall:
      result = newEmptyPNode()

    of cppSwitchStatement:
      result = newCase(~node["condition"])
      for branch in node["body"]:
        case branch.kind:
          of cppComment: discard
          of cppCaseStatement:
            var body = newPStmtList()
            for item in branch[1..^1]:
              body.add ~item

            if 0 < body.len:
              result.addBranch(
                ~branch["value"],
                body[0 .. ^tern(body[^1].kind == nkBreakStmt, 2, 1)])

            else:
              result.addBranch(~branch["value"], body)

          else: raise newImplementKindError(branch)

    of cppFunctionDefinition:
      var impl = newPProcDecl(node["declarator"].getName())
      for arg in node["declarator"]["parameters"]:
        var name = c.fixIdentName(arg.getName(), "a")
        var argType = arg["type"].toPType()
        pointerWraps(arg["declarator"], argType)

        impl.addArgument(name, argType)

      impl.returnType = node["type"].toPType()
      impl.impl = ~node["body"]

      result = impl.toNNode()


    of cppInitializerList:
      result = newPTree(nnkBracket)
      for item in node:
        result.add ~item


    of cppForStatement:
      let update = tern("update" in node, ~node["update"], newEmptyPNode())
      result = newBlock(
        ~node["initializer"],
        newWhile(~node["condition"], ~node[^1], update))

    of cppConditionClause:
      result = ~node[0]

    of cppIfStatement:
      result = newIf(~node["condition"], ~node["consequence"])

    of cppDeclaration:
      let decl = node["declarator"]
      var value = newEmptyPNode()

      var name = c.fixIdentName(node.getName(), "f").newPIdent()
      var declType = node["type"].toPType()
      pointerWraps(node["declarator"], declType)

      if "value" in decl:
        value = ~decl["value"]

      result = nnkVarSection.newPTree(
        nnkIdentDefs.newPTree(name, declType.toNNode(), value))


    else:
      raise newImplementKindError(
        node, node.strVal() & "\n" & $node.treeRepr(
          str, unnamed = true, opts = hdisplay(maxLen = 5)))



when isMainModule:
  let file = AbsFile"/tmp/in.c"
  var str = file.readFile()
  var c: StringNameCache
  let code = parseCppString(addr str).conv(str, c).`$`
  "/tmp/out.nim".writeFile(code)

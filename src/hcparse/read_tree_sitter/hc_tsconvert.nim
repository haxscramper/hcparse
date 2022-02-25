## This module implements conversion of the C and C++ code to nim.
## Translation is performed using simplistic AST-AST rewrite, and requires
## manual correction afterwards.

import
  htsparse/cpp/cpp

import
  std/[strutils, sequtils, tables, sets]

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

import compiler/utils/astrepr
import compiler/ast/trees

export parseCppString

proc wrap*(ntype: NType[PNode], kind: CxxTypeKind): NType[PNode] =
  case kind:
    of ctkPtr:
       return newNType("ptr", @[ntype])

    of ctkDynamicArray:
      return newNType("ptr", @[newNType("UncheckedArray", @[ntype])])

    else:
      raise newImplementKindError(kind)

proc skip*(node: CppNode, idx: int, kind: set[CppNodeKind]): CppNode =
  if node.kind in kind:
    result = node[idx]

  else:
    result = node

proc fillStmt*(node: PNode): PNode =
  if node.isEmptyTree():
    return newPTree(nnkDiscardStmt, newEmptyPNode())

  else:
    node

type
  MethodList = object
    forward: HashSet[string] ## Names of methods that were forward-declared
    moved: HashSet[string] ## Methods that were later found in the
                           ## out-of-body definitions.

  ConvConf = object
    conf: CodegenConf
    fix: CxxFixConf
    objects: Table[CxxName, CxxObject]
    methodTable: Table[CxxName, MethodList]

proc dropForwardMethods(obj: var CxxObject, conf: ConvConf) =
  var idx = 0
  while idx < obj.methods.len():
    let class = obj.cxxName()
    let name = obj.methods[idx].cxxName().lastScope()
    if class in conf.methodTable and
       name in conf.methodTable[class].forward and
       name in conf.methodTable[class].moved:
      obj.methods.del(idx)
    else:
      inc idx

proc updateOutOfBody(pr: var CxxProc, conf: ConvConf) =
  let class = pr.cxxName()[0..^2]
  let name = pr.cxxName().lastScope()
  if class in conf.objects:
    let obj = conf.objects[class]
    assertRef(obj)
    pr.arguments.insert(cxxArg(
      cxxPair("this", cxxName("this")),
      obj.decl.cxxTypeUse()))

    pr.cxxName = pr.cxxName()[^1]
    pr.head.genParams.insert(obj.decl.genParams)


proc conv*(
    node: CppNode,
    str: string,
    c: var StringNameCache,
    conf: ConvConf
  ): PNode =
  template `~`(expr: CppNode): untyped =
    conv(expr, str, c, conf)

  var pc = addr c
  proc updateNim(pair: sink CxxNamePair): CxxNamePair =
    result = pair
    result.nim = result.cxx.scopes.join("_")

  proc toName(node: CppNode): PNode =
    return newPIdent(conf.fix.fixName(
      node.cxxNamePair(cncNone).updateNim(), pc[],
      default(CxxNameFixContext)))

  proc toTypeWithAnon(
    node: CppNode, parent, user: Option[CxxNamePair] = none CxxNamePair):
    tuple[nimt: NType[PNode], anon: PNode] =

    let cxx = toCxxTypeWraps(
        node,
        parent = parent,
        user = user).
        postFixEntries(conf.fix, CxxLibImport())

    var anon: seq[NimDecl[PNode]]
    result.nimt = cxx.toNNode(conf.conf, anon)
    result.anon = newPStmtList()
    for an in anon:
      result.anon.add an.toNNode()

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

    of cppTemplateFunction:
      result = newPStmtList()
      var call = nnkBracketExpr.newPTree(~node[0])
      for arg in node[cpfArgs]:
        let (typ, anon) = toTypeWithAnon(arg)
        call.add typ.toNNode()
        result.add anon

      result.add call

    of cppBinaryExpression, cppAssignmentExpression, cppUnaryExpression:
      let
        op = node{1}.strVal()
        lhs = ~node[0]
        map = op.mapOpName(true)

      if node of cppUnaryExpression:
        result = newXCall(map, lhs)

      else:
        result = newXCall(map, ~node[0], ~node[1])

      if op in cxxAsgnOps:
        # NOTE this should be configurable, since not simply rewriting all
        # the operators is not the best idea for a more complex C++ code
        # that might actually have all the necesary overloads in place.
        result = nnkAsgn.newPTree(lhs, newPar(result))


    of cppParenthesizedExpression:
      result = nnkPar.newPTree(~node[0])

    of cppIdentifier,
       cppFieldIdentifier,
       cppTypeIdentifier,
       cppNamespaceIdentifier:
      result = node.toName()

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
      let call =
        if node{0} of cppDoubleMinusTok: "preDec"
        elif node{0} of cppDoublePlusTok: "preInc"
        elif node{1} of cppDoubleMinusTok: "postDec"
        elif node{1} of cppDoublePlusTok: "postInc"
        else:
          failNode(node)

      result = newXCall(call, ~node[0])

    of cppStringLiteral:
      result = newPLit(node.getBase()[node.slice()].strip(chars = {'"'}))

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
      let (decl, anon) = toTypeWithAnon(node[cpfType])
      result = nnkCast.newPTree(decl.toNNode(), ~node["value"])

    of cppNull:
      result = newPIdent("nil")

    of cppPreprocCall, cppPreprocDef:
      result = newEmptyPNode()

    of cppPreprocIfdef:
      if node[0] of cppIdentifier:
        # Include guard detection
        let head = node[0].strVal()
        if node[1] of cppPreprocDef and
           node[1][0] of cppIdentifier and node[1][0].strVal() == head:

          result = newPStmtList()
          for item in node[2..^1]:
            result.add ~item

        else:
          result = newEmptyPNode()

      else:
        result = newEmptyPNode()


    of cppSwitchStatement:
      var before = newPStmtList()
      result = newCase(~node[cpfCond])
      var emptyList: seq[PNode]
      for branch in node["body"]:
        case branch.kind:
          of cppComment: discard
          of cppCaseStatement:
            let default = branch{0}.strVal() == "default"
            if branch.len == 1 and not default:
              emptyList.add ~branch[0]
              continue

            var body: seq[PNode]
            for item in branch[tern(default, 0, 1)..^1]:
              body.add ~item

            if 0 < body.len and body[^1].kind == nkBreakStmt:
              body.setLen(body.len - 1)

            let body1 = newPStmtList(body).fillStmt()

            if default:
              result.addBranch(body1)

            elif "value" notin branch:
              result.addBranch(emptyList, body1)

            elif 0 < body.len:
              result.addBranch(emptyList & ~branch["value"], body1)

            else:
              result.addBranch(emptyList & ~branch["value"], body1)

            emptyList = @[]

          else:
            # It is possible to arbitrary statements and declarations after
            # the `switch` part, so everything is going to be added before
            # the `case`
            before.add ~branch

      if 0 < before.len:
        before.add result
        return before

    of cppTypeDefinition, cppTypeSpecSpec:
      var coms: seq[CxxComment]
      var anon: seq[NimDecl[PNode]]
      result = newPStmtList()

      for ent in toCxxTypeDefinition(node, coms).
                 postFixEntries(conf.fix, CxxLibImport()):

        var ent = ent
        if ent of cekObject:
          dropForwardMethods(ent.cxxObject, conf)

        for en in ent.toNNode(conf.conf, anon):
          result.add toNNode(en, standalone = true)

        for an in anon:
          result.add toNNode(an)

    of cppLabeledStatement:
      result = newPTree(nnkBlockStmt, ~node[0], ~node[1])

    of cppEnumSpecifier:
      var coms: seq[CxxComment]
      let
        entrs = postFixEntries(@[box(toCxxEnum(node, coms))], conf.fix)

      for item in toNNode[PNode](entrs[0].cxxEnum, conf.conf):
        result = item.toNNode()

    of cppFunctionDefinition, cppTemplateDeclaration:
      var
        coms: seq[CxxComment]
        anon: seq[NimDecl[PNode]]
        impl: PNode

      if node of cppFunctionDefinition or
         node[1] of cppFunctionDefinition or
         (node.has([1, 1]) and node[1][1] of cppFunctionDeclarator):
        var pr = toCxxProc(node, coms)
        pr.updateOutOfBody(conf)
        var conv = postFixEntries(@[box(pr)], conf.fix)[0].
          cxxProc.
          toNNode(conf.conf, anon)

        conv.impl = fillStmt(conv.impl)
        impl = conv.toNNode()

      elif node[1] of cppTypeSpecSpec:
        var obj = toCxxObject(node, coms)
        dropForwardMethods(obj, conf)
        impl = postFixEntries(@[box(obj)], conf.fix)[0].
          cxxObject.
          toNNode(conf.conf, anon).toNNode()

      else:
        failNode node



      result = newPStmtList()
      for an in anon:
        result.add an.toNNode()

      result.add impl

    of cppInitializerList:
      result = newPTree(nnkPar)
      for item in node:
        result.add ~item

    of cppThis:
      result = newPIdent("this")

    of cppGotoStatement:
      # Nim does not have support for 'goto' statements, so translating
      # them literally here, this code would have to be rewritten manually.
      result = newPTree(nnkCommand, newPIdent("cxx_goto"), ~node[0])

    of cppDeleteExpression:
      # There is no corresponding 'delete' expression either, so I wrap it
      # with placeholder proc, it should be supplied from user, or
      # rewritten.
      result = newPTree(nnkCommand, newPIdent("cxx_delete"), ~node[0])

    of cppSyntaxError:
      result = newXCall(newPIdent"CXX_SYNTAX_ERROR", @[newPIdent(node.strVal())])

    of cppStatementIdentifier:
      result = newPIdent(node.strVal())

    of cppInitializerPair:
      result = newPTree(nnkExprColonExpr, ~node[0], ~node[1])

    of cppFieldDesignator:
      result = ~node[0]

    of cppNewExpression:
      result = newXCall(
        newPIdent("new" & node[cpfType].strVal()),
        tern(cpfArgs in node, node[cpfArgs].mapIt(~it), @[]))

    of cppForStatement:
      let update = tern("update" in node, ~node["update"], newEmptyPNode())
      result = newWhile(~node[cpfCond], ~node[^1], update)
      if cpfInit in node:
        result = newBlock(~node[cpfInit], result)

    of cppWhileStatement:
      result = newWhile(newPar ~node[cpfCond], ~node["body"])

    of cppConcatenatedString:
      result = newPLit(node.strVal())

    of cppConditionClause:
      result = ~node[0]

    of cppSizeofExpression:
      # debug node
      if "value" in node:
        result = newXCall("sizeof", ~node["value"])

      else:
        result = newXCall("sizeof", ~node["type"])

    of cppTypeDescriptor:
      let (decl, anon) = toTypeWithAnon(node)
      result = decl.toNNode()

    of cppCharLiteral:
      if node.len != 0 and node[0].kind == cppEscapeSequence:
        result = newPLit('\\')
      else:
        result = newPLit(node.strVal()[1])

    of cppIfStatement:
      result = newIf(~node[cpfCond], fillStmt(~node["consequence"]))
      if cpfAlter in node:
        result.addBranch(fillStmt(~node[cpfAlter]))

    of cppDoStatement:
      let body = ~node["body"]
      result = newPStmtList(body, newWhile(~node[cpfCond], body))

    of cppDeclaration:
      let decl = node[cpfDecl]
      var value = newEmptyPNode()

      result = newPStmtList()
      # Construct wrapped type declaration. Supplying `parent/name` in case
      # type is an anonymous struct/union.
      let (declType, anon) = toTypeWithAnon(
        node,
        some CxxNamePair(),
        some node.getNameNode().cxxNamePair(cncArg).updateNim()
      )

      for an in anon:
        result.add an

      if "value" in decl:
        if decl[cpfDecl] of cppArrayDeclarator:
          value = newPTree(nnkBracket)
          for item in decl["value"]:
            value.add ~item

          value = newXCall("@", value)

        else:
          value = ~decl["value"]
          if decl["value"] of cppArgumentList:
            # `Instr16 instr16(&emu, &instr);` is converted into
            # `let instr16: Instr16 = initInstr16(addr emu, addr instr)`
            value = newXCall(newPIdent("init" & node[cpfType].strVal()),
                             value[0..^1])

      result.add nnkVarSection.newPTree(
        nnkIdentDefs.newPTree(
          node.getNameNode().toName(),
          declType.toNNode(),
          value))

    of cppQualifiedIdentifier:
      result = newXCall(".", ~node[0], ~node[1])

    of cppContinueStatement:
      result = newPTree(nnkContinueStmt)

    of cppCatchClause:
      result = newPTree(nnkExceptBranch)
      let param = node["parameters"]
      if param.len == 0:
        result.add newEmptyPNode()

      else:
        result.add newXCall("as", ~param[0][cpfType], ~param[0][cpfDecl])

      result.add ~node["body"]

    of cppTryStatement:
      result = newPTree(nnkTryStmt, ~node["body"])
      for clause in node[1..^1]:
        result.add ~clause

    of cppThrowStatement:
      result = newPTree(nnkRaiseStmt, ~node[0])

    of cppConditionalExpression:
      let cond = node[cpfCond]
      if cond of cppAssignmentExpression:
        result = nnkAsgn.newPTree(
          ~cond["left"],
          newPar(
            newIf(~cond["right"], ~node["consequence"], ~node[cpfAlter])))

      else:
        result = newPar(newIf(~cond, ~node["consequence"], ~node[cpfAlter]))

    of cppArgumentList:
      result = newPTree(nnkPar)
      for item in node:
        result.add ~item

    of cppTemplateInstantiation:
      # IIRC template instantiations are used to circumvent some
      # shortcomings of the C++ template system when it comes to splitting
      # things into different files, so nothing to be done here.
      result = newPStmtList()

    else:
      failNode node


import compiler/tools/docgen_code_renderer except `of`

if isMainModule:
  const full = on
  when full:
    let files = toSeq(walkDir(
      AbsDir"/tmp/infiles",
      AbsFile,
      recurse = true,
      exts = @["h", "c", "hpp", "cpp"]))

  else:
    let files = [AbsFile"/tmp/in.cpp"]

  startHax()

  var conv = ConvConf()
  for file in files:
    var str = file.readFile()
    let node = parseCppString(addr str)
    var coms: seq[CxxComment]
    for entry in toCxx(node, coms):
      if entry of cekObject:
        let user = entry.cxxName()
        conv.objects[user] = entry.cxxObject
        for meth in entry.cxxObject.methods:
          let name = meth.cxxName().scopes.last()
          conv.methodTable.mgetOrPut(
            user, default MethodList).forward.incl name

      elif entry of cekProc:
        if not entry.isForward():
          let name = entry.cxxName()
          conv.methodTable.mgetOrPut(
            name[0..^2], default MethodList).moved.incl(
              name[^1].lastScope())

  for file in files:
    echo file
    var str = file.readFile()
    var c: StringNameCache
    let node = parseCppString(addr str)
    var conf = cxxCodegenConf.withIt do:
      it.nameStyle = idsSnake
      it.helperEnum = false

    let fix = baseFixConf.withIt do:
      it.typeStore = newTypeStore()
      it.onFixName():
        if name.nim.len == 0:
          result = name.cxx.scopes.join("_")
        else:
          result = name.nim

        if not cache.knownRename(name.nim):
          cache.newRename(name.nim, result)

      it.onGetBind():
        # We are performing code translation here, so there is no need to
        # add any bindings to the generated entries.
        return cxxNotImported()

    conf.postProc = proc(
      def: CxxProc,
      impl: var ProcDecl[PNode],
      conf: CodegenConf
    ): seq[NimDecl[PNode]] =
      assert def.userData.notNil()
      let node = cast[CppNode](def.userData)
      if tern(node.kind == cppTemplateDeclaration,
              node.len < 2 or
              # `template <class T> uint32_t update_eflags_add(T v1, uint32_t v2);`
              "body" notin node[1],
              "body" notin node):
        # No body, only forward declaration.
        impl.impl = fillStmt(newPStmtList())

      else:
        # echov "--------------"
        # debug node
        impl.impl = fillStmt(
          conv(
            tern(
              node.kind == cppTemplateDeclaration,
              node[1]["body"],
              node["body"]),
            str, c, conv))


    # echo node.getTs().treeRepr(node.getBase(), unnamed = true)
    # debug node
    conv.conf = conf
    conv.fix = fix
    let nim = node.conv(str, c, conv)
    let code = nim.formatToStr()
    # let code = node.conv(str, c, conf, fix).`$`
    # let code = nim.toPString()
    writeFile(file.withBaseSuffix(file.ext()).withExt("nim"), code)


  echo "done"

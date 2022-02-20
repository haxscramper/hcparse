import
  htsparse/cpp/cpp

import
  std/[json, strutils, parseutils, tables]

import
  ../read_libclang/[
    hc_types,
    cxcommon
  ],

  ../processor/[
    wrap_store
  ]


import
  ../hc_typeconv

import
  hmisc/core/all,
  hmisc/wrappers/[treesitter],
  hmisc/other/oswrap

export parseCppString, treeRepr

proc debug*(node: CppNode) =
  echo node.treeRepr()

proc failNode*(node: CppNode) {.noreturn.} =
  raise newUnexpectedKindError(
    node,
    node.strVal() &
      "\n" &
      $node.treeRepr(
        unnamed = true,
        opts = hdisplay(maxLen = 5, maxDepth = 3)))

type
  CppFieldNode* = enum
    cpfType = "type"
    cpfDecl = "declarator"
    cpfArgs = "arguments"
    cpfInit = "initializer"
    cpfCond = "condition"
    cpfAlter = "alternative"

proc contains*(node: CppNode, field: CppFieldNode): bool =
  $field in node

proc `[]`*(node: CppNode, field: CppFieldNode): CppNode =
  node[$field]

using coms: var seq[CxxComment]

proc primitiveName*(node: CppNode): string =
  proc aux(ts: TsCppNode): string =
    if ts.len == 0:
      result = node.getBase()[ts]

    elif ts.len(unnamed = true) == 2:
      result = node.getBase()[ts{0}] & " " & aux(ts{1})

    else:
      raise newImplementKindError(
        node, ts.treeRepr(node.getBase(), unnamed = true))

    if result.len == 0:
      echo ts.len
      echo ts.len(unnamed = true)
      echo node.getBase()[ts]


  return aux(node.getTs())

const cxxAsgnOps* = [
  "<<=", ">>=", "%=", "&=", "|=", "^=",
  # basic math operators are more likely to work with the operands, but in
  # general it is not really possible to know what is going on in the C++
  # code from the syntax alone, so the assumption here is that rewriting
  # all `+=` would reduce the amount of errors in the final code because
  # most of the times `+=` was used for integer addition.
  "+=", "-=", "/=", "*="
]

proc mapOpName*(op: string, mapAsgn: bool = false): string =
  case op:
    of "|", "||": "or"
    of "<<": "shl"
    of ">>": "shr"
    of "&", "&&": "and"
    of "^": "xor"
    of "%": "mod"
    of "~": "not"
    of "!": "not" # QUESTION what is the difference between ~ and !
    else:
      if mapAsgn and op in cxxAsgnOps:
        mapOpName(op[0..^2])

      else:
        op

proc mapTypeName*(node: CppNode): string =
  case node.kind:
    of cppTypeIdentifier:
      result = node.strVal()

    of cppQualifiedIdentifier:
      for item in items(node):
        result.add item.strVal()

    else:
      result = mapPrimitiveName(node.primitiveName())


proc toCxxComment*(comm: CppNode): CxxComment =
  cxxComment(comm.strVal().stripComment())

proc toCxxArg*(node: CppNode, idx: int): CxxArg

proc toCxxEnum*(node: CppNode, coms): CxxEnum
proc toCxxObject*(main: CppNode, coms): CxxObject

proc pointerWraps*(node: CppNode, ftype: var CxxTypeUse) =
  case node.kind:
    of cppPointerDeclarator:
      ftype = ftype.wrap(ctkPtr)
      if node[0] of cppTypeQualifier:
        pointerWraps(node[1], ftype)

      else:
        pointerWraps(node[0], ftype)

    of cppArrayDeclarator:
      if "size" in node:
        let size = node["size"]
        assert size.kind == cppNumberLiteral
        ftype = wrapArray(
          CxxTypeUse(
            kind: ctkStaticParam,
            value: CxxExpr(
              kind: cekIntLit,
              intVal: size.strVal().parseInt())),
          ftype
        )

        pointerWraps(node[0], ftype)

      else:
        ftype = ftype.wrap(ctkDynamicArray)
        pointerWraps(node[0], ftype)

    of cppInitDeclarator,
       cppTypeQualifier #[ TODO convert for CxxType? ]#:
      pointerWraps(node[0], ftype)

    of cppReferenceDeclarator:
      ftype = ftype.wrap(ctkLVRef)
      pointerWraps(node[0], ftype)

    of cppAbstractPointerDeclarator:
      ftype = ftype.wrap(ctkPtr)
      if node.len > 0:
        pointerWraps(node[0], ftype)

    of cppParenthesizedDeclarator:
      pointerWraps(node[0], ftype)

    of {
      cppFieldIdentifier,
      cppTypeIdentifier,
      cppIdentifier,
      cppFunctionDeclarator
    }:
      discard

    else:
      failNode node

proc toCxxType*(node: CppNode, parent, user: Option[CxxNamePair]): CxxTypeUse =
  case node.kind:
    of cppTypeIdentifier:
      result = cxxTypeUse(cxxPair(
        mapTypeName(node),
        cxxName(@[node.strVal()])))

    of cppSizedTypeSpecifier, cppPrimitiveType:
      result = cxxTypeUse(node.primitiveName().mapPrimitivePod())

    of cppQualifiedIdentifier:
      var names: seq[string]
      for name in items(node):
        names.add name.strVal()

      result = cxxPair(mapTypeName(node), cxxName(names)).cxxTypeUse()

    of cppStructSpecifier, cppEnumSpecifier, cppUnionSpecifier:
      if node[0] of cppFieldDeclarationList:
        var coms: seq[CxxComment]
        case node.kind:
          of cppEnumSpecifier:
            result = cxxTypeUse(
              toCxxEnum(node, coms), parent.get(), user.get())

          of cppUnionSpecifier, cppStructSpecifier:
            result = CxxTypeUse(
              kind: ctkAnonObject, objDef: toCxxObject(node, coms))

            if parent.isSome(): result.objParent = parent.get()
            if user.isSome(): result.objUser = user.get()

          else:
            failNode node

      else:
        result = toCxxType(node[0], parent, user)

    of cppFieldDeclaration:
      var args: seq[CxxArg]
      var idx = 0
      for param in node[cpfDecl]["parameters"]:
        args.add toCxxArg(param, idx)
        inc idx

      result = cxxTypeUse(args, toCxxType(node[cpfType], parent, user))
      pointerWraps(node[cpfDecl][cpfDecl], result)
      # debug node

    else:
      failNode node


proc toCxxTypeWraps*(
    node: CppNode,
    parent, user: Option[CxxNamePair] = none(CxxNamePair),
    typField: CppFieldNode = cpfType,
    declField: CppFieldNode = cpfDecl
  ): CxxTypeUse =
  ## Convert C++ type usage to registered type usage. Wrap resulting type
  ## in pointers if declarator contains them.
  result = toCxxType(node[typField], parent, user)
  if declField in node:
    pointerWraps(node[declField], result)


proc toCxxMacro*(node: CppNode, coms): CxxMacro =
  assertKind(node, {cppPreprocFunctionDef, cppPreprocDef})

  result = cxxMacro(cxxPair(node["name"].strVal()))

  if "parameters" in node:
    for arg in node["parameters"]:
      result.arguments.add arg.strVal()

  # TODO convert body


type
  CxxEvalCtx = object
    table: Table[string, BiggestInt]

proc evalEnumValue(node: CppNode, ctx: CxxEvalCtx): BiggestInt =
  case node.kind:
    of cppNumberLiteral:
      discard parseBiggestInt(node.strVal(), result)

    of cppIdentifier:
      result = ctx.table[node.strVal()]

    of cppCharLiteral:
      result = BiggestInt(node.strVal()[0])

    of cppBinaryExpression:
      let lhs = evalEnumValue(node[0], ctx)
      let rhs = evalEnumValue(node[1], ctx)

      case node{1}.strVal():
        of "|": result = lhs or rhs
        of "&": result = rhs and lhs
        of "+": result = lhs + rhs
        of "-": result = lhs - rhs
        of "<<": result = lhs shl rhs
        of ">>": result = lhs shr rhs
        else: raise newImplementKindError(node{1}.strVal())

    of cppParenthesizedExpression:
      result = evalEnumValue(node[0], ctx)

    else:
      raise newImplementKindError(
        node,
        $node & "\n" & node.treeRepr(),
      )

proc toCxxEnum*(node: CppNode, coms): CxxEnum =
  var name: CxxNamePair

  if "name" in node:
    name = cxxPair(node["name"].strVal())

  result = cxxEnum(name)
  result.add coms
  coms.clear()

  var env: CxxEvalCtx
  var value: BiggestInt = 0
  for en in node["body"]:
    case en.kind:
      of cppEnumerator:
        if "value" in en:
          value = evalEnumValue(en["value"], env)

        let name = en["name"].strVal()
        env.table[name] = value
        result.values.add cxxEnumValue(cxxPair(name), value)
        result.values[^1].add coms
        coms.clear()
        inc value


      of cppComment:
        coms.add toCxxComment(en)

      else:
        raise newImplementKindError(en)


proc skipPointer(node: CppNode): CppNode =
  case node.kind:
    of cppPointerDeclarator: skipPointer(node[0])
    else: node


proc getNameNode*(node: CppNode): CppNode =
  case node.kind:
    of cppFieldIdentifier,
       cppTypeIdentifier,
       cppIdentifier,
       cppQualifiedIdentifier,
       cppPrimitiveType:
      node

    of cppArrayDeclarator, cppFunctionDeclarator:
      node[0].getNameNode()

    of cppDeclaration, cppInitDeclarator:
      node[cpfDecl].getNameNode()

    else:
      if node.len > 0:
        node[^1].getNameNode()

      else:
        failNode node

# proc getName*(node: CppNode): string = node.getNameNode().strVal()

proc cxxNamePair*(node: CppNode, context: CxxNameContext): CxxNamePair =
  result.context = context
  case node.kind:
    of cppQualifiedIdentifier:
      result.cxx.scopes = (
        node[0].cxxNamePair(context).cxx.scopes &
          node[1].cxxNamePair(context).cxx.scopes )

    of cppNamespaceIdentifier,
       cppIdentifier,
       cppDestructorName,
       cppTypeIdentifier,
       cppFieldIdentifier:
      result.cxx.scopes = @[node.strVal()]

    else:
      failNode node



proc toCxxArg*(node: CppNode, idx: int): CxxArg =
  assertKind(node, {
    cppParameterDeclaration,
    cppOptionalParameterDeclaration
  })

  var
    name: CxxNamePair
    argt: CxxTypeUse

  if cpfDecl in node:
    if node[cpfDecl] of cppAbstractPointerDeclarator:
      name = cxxPair("a" & $idx, cncArg)
      argt = toCxxType(node[cpfType], none CxxNamePair, some name).wrap(ctkPtr)

    else:
      name = getNameNode(node[cpfDecl]).cxxNamePair(cncArg)
      argt = toCxxType(node[cpfType], none CxxNamePair, some name)
      pointerWraps(node[cpfDecl], argt)

  else:
    name = cxxPair("a" & $idx, cncArg)
    argt = toCxxType(node[cpfType], none CxxNamePair, some name)

  if "default_value" in node:
    echov "missing default value for argument"

  result = cxxArg(name, argt)


proc unpackTemplate(node: CppNode): tuple[
  body: CppNode, params: CxxGenParams] =
  if node.kind == cppTemplateDeclaration:
    for item in node[0]:
      let name = item[0]
      if item.len != 1:
        failNode item

      var defaultType: Option[CxxTypeUse]

      result.params.add(
        cxxNamePair(name, cncType), defaultType)

    result.body = node[1]

  else:
    result.body = node


proc toCxxProc*(
    main: CppNode,
    coms;
    parent: Option[CxxObject] = none(CxxObject),
  ): CxxProc =


  let (node, params) = unpackTemplate(main)
  result = node[cpfDecl].getNameNode().cxxNamePair(cncProc).cxxProc()
  result.userData = cast[pointer](main)
  result.head.genParams = params

  if cpfType in node:
    result.returnType = toCxxType(
      node[cpfType],
      some result.head.name,
      some result.head.name
    )

  if parent.isSome():
    result.flags.incl cpfMethod

  if node[0] of cppTypeQualifier:
    result.returnType.flags.incl ctfConst

  if node[0] of cppStorageClassSpecifier:
    result.flags.incl cpfStatic

  pointerWraps(node[cpfDecl], result.returnType)

  let decl =
    if node[cpfDecl].kind == cppPointerDeclarator:
      node[cpfDecl].skipPointer()

    else:
      node[cpfDecl]

  var argComs: seq[CxxComment]


  for idx, arg in decl["parameters"]:
    if arg of cppComment:
      argComs.add toCxxComment(arg)

    else:
      result.arguments.add toCxxArg(arg, idx).withIt do:
        it.add argComs
        argComs.clear()

  for p in items(decl["parameters"].getTs(), unnamed = true):
    if p.kind in {cppTripleDotTok}:
      result.flags.incl cpfVariadic


proc toCxxField*(node: CppNode, coms; parent: CxxNamePair): CxxField =
  assertKind(node, {cppFieldDeclaration})
  if cpfDecl notin node:
    # It is possible to declare field without actual name, only using size.
    result = cxxField(
      CxxNamePair(), toCxxTypeWraps(node, some parent))

  else:
    let decl = node[cpfDecl]
    let name = getNameNode(decl).cxxNamePair(cncField)
    if decl of cppFunctionDeclarator:
      result = cxxField(name, toCxxType(node, some parent, some name))

    else:
      result = cxxField(name, toCxxTypeWraps(node, some parent, some name))

  for item in node:
    if item of cppBitfieldClause:
      result.bitsize = some item[0].strVal().parseInt()

proc toCxxForwardType*(node: CppNode, coms): CxxForward =
  result = cxxForward(
    cxxPair(node["name"].strVal()),
    ctdkStruct #[ TEMP implement better detection ]#)

  result.add coms
  coms.clear()

proc updateAccess(item: CppNode, accs: var CxxAccessSpecifier, signal: var CxxQSignals) =
  let ts = item.getTs()
  case ts{0}.kind:
    of cppPublicTok: accs = casPublic
    of cppProtectedTok: accs = casProtected
    of cppPrivateTok: accs = casPrivate
    of cppSignalsTok: signal = cqsSignal
    of cppSlotsTok: signal = cqsSlot
    else: raise newUnexpectedKindError(ts{0})

  case ts{1}.kind:
    of cppColonTok:
      if ts{0} of {cppSignalsTok, cppSlotsTok}:
        # `signals:` or `slots:` - no visibility change here
        discard

      else:
        # `private:`, `public:`, `protected:` - visibility changed
        # already, but this closed signal/slot section
        signal = cqsNone

    of cppSlotsTok:
      # `public/private/protected slots:`
      signal = cqsSlot

    of cppSignalsTok:
      # `public/private/protected signals:`
      signal = cqsSignal

    else:
      raise newUnexpectedKindError(ts{1})


proc toCxxObject*(main: CppNode, coms): CxxObject =
  let (node, params) = unpackTemplate(main)

  var decl: CxxNamePair
  if "name" in node:
    decl = cxxPair(node["name"].strVal())

  result = cxxObject(decl)
  result.add coms
  result.decl.genParams = params
  coms.clear()

  var accs = casPublic

  case node.kind:
    of cppStructSpecifier:
      result.kind = cokStruct

    of cppUnionSpecifier:
      result.kind = cokUnion

    of cppClassSpecifier:
      accs = casPrivate
      result.kind = cokClass

    else:
      raise newUnexpectedKindError(node)

  var signal = cqsNone

  for part in node:
    if part of cppBaseClassClause:
      for class in part:
        if not(class of cppSyntaxError):
          result.super.add toCxxType(
            class, none CxxNamePair, none CxxNamePair)


  var res {.byaddr.} = result
  var resComs {.byaddr.} = coms
  proc toMethod(item: CppNode) =
    var meth = toCxxProc(item, resComs, some res).withIt do:
      it.add resComs
      resComs.clear()

    if meth.cxxName().scopes.last() == res.cxxName().scopes.last():
      meth.constructorOf = some res.name()

    elif not(item of cppTemplateDeclaration) and
         # Destructors can't be templated so we can omit checks here,
         # instead of reaching for the declarator body.
         item[cpfDecl][cpfDecl] of cppDestructorName:
      meth.destructorOf = some res.name()

    case signal:
      of cqsNone: discard
      of cqsSignal: meth.flags.incl cpfSignal
      of cqsSlot: meth.flags.incl cpfSlot

    meth.access = accs
    res.methods.add meth

  proc toField(item: CppNode) =
    res.mfields.add toCxxField(item, resComs, res.decl.name).withIt do:
      it.add resComs
      resComs.clear()
      it.access = accs

  for item in node["body"]:
    if item of cppAccessSpecifier:
      updateAccess(item, accs, signal)
      continue

    case item.kind:
      of cppDeclaration, cppFunctionDefinition:
        toMethod(item)

      of cppTemplateDeclaration:
        case item[1].kind:
          of cppFunctionDefinition:
            toMethod(item)

          of cppStructSpecifier:
            res.nested.add toCxxObject(item, coms).withIt do:
              it.access = accs

          else:
            failNode item

      of cppFieldDeclaration:
        if cpfDecl in item and item[cpfDecl] of cppFunctionDeclarator:
          # Either function declaration or field with callback type
          if item[cpfDecl][cpfDecl] of cppParenthesizedDeclarator:
            # Field with callback type
            toField(item)

          else:
            # Regular function
            toMethod(item)

        else:
          # Regular field
          toField(item)

      # elif cpfDecl notin item and
      #      item[cpfType] of { cppStructSpecifier }

      # of cppFieldDeclaration,
      #    cppFunctionDefinition,
      #    cppTemplateDeclaration:
      #   if :
      #

      #   # TODO This piece of code is not really legible, so I need to
      #   # provide at least some form of documentation that explains what
      #   # is going on here.
      #   elif item of { cppFunctionDefinition, cppTemplateDeclaration } or
      #        # `Sequencer *get_seq(void) { return &seq; };` - for function
      #        # definition.
      #        (cpfDecl in item and item[cpfDecl] of { cppFunctionDeclarator }):
      #     if not(item of { cppFunctionDefinition, cppTemplateDeclaration }) and
      #        # Not a standalone function declaration, and not a template
      #        # (function or non-function)
      #        item[cpfDecl][cpfDecl] of cppParenthesizedDeclarator:
      #       toField(item)

      #     else:
      #       toMethod(item)

      #   else:
      #     toField(item)

      of cppComment:
        coms.add toCxxComment(item)

      of cppQPropertyDeclaration:
        echov "skipping qproperty"

      of cppSyntaxError:
        discard

      else:
        failNode(item)

  if ?result.mfields:
    result.mfields.last().add coms
    coms.clear()


proc toCxxTypeDefinition*(node: CppNode, coms): seq[CxxEntry] =
  if node.len == 1 and node[0] of cppEnumSpecifier:
    return @[
      box toCxxEnum(node[0], coms).withIt do:
        it.add coms
        coms.clear()
    ]

  elif node.len == 1 and node of {
    cppStructSpecifier, cppClassSpecifier, cppUnionSpecifier
  }:
    # Forward class declaration
    return @[box toCxxForwardType(node, coms)]

  let main = tern("body" in node, node["body"], node[1])
  case main.kind:
    of cppFieldDeclarationList:
      result.add toCxxObject(node, coms)

    of cppTypeIdentifier,
       cppPointerDeclarator,
       cppPrimitiveType:
      let newType = toCxxType(
        node[cpfDecl],
        parent = none CxxNamePair,
        user = none CxxNamePair).toDecl(ctdkTypedef #[ XXXX ]#)

      let baseBody = node[0]
      if baseBody of {
        cppSizedTypeSpecifier, cppPrimitiveType, cppTypeIdentifier
      } or (
        baseBody of {
          cppStructSpecifier, cppUnionSpecifier, cppEnumSpecifier} and
        "body" notin baseBody
      ):
        var alias = cxxAlias(
          newType,
          toCxxType(node[cpfType], none CxxNamePair, none CxxNamePair)
        ).withIt do:
              it.add coms
              coms.clear()
        pointerWraps(node[cpfDecl], alias.baseType)

        if alias.baseType of ctkIdent and
          alias.decl.cxxName() == alias.baseType.cxxName():
          # `typedef struct T T;`
          result.add cxxForward(
            alias.decl.name, ctdkStruct #[ TEMP ]#).withIt do:

            it.add coms
            coms.clear()

        else:
          result.add alias

      elif baseBody of {
          cppStructSpecifier,
          cppUnionSpecifier,
          cppClassSpecifier
        }:
        # FIXME handle multiple trailing typedefs
        var struct = toCxxObject(baseBody, coms)
        if struct.cxxName().isEmpty():
          # Handle `typedef struct {} struct_name;`
          struct.name = newType.name
          result.add struct

        else:
          result.add struct
          if struct.cxxName() != newType.cxxName():
            result.add cxxAlias(newType, struct.decl.cxxTypeUse()).withIt do:
              it.add coms
              coms.clear()

      elif baseBody of {cppEnumSpecifier}:
        var enumd = toCxxEnum(node[0], coms)
        if enumd.cxxName().isEmpty():
          enumd.name = newType.name
          result.add enumd

        else:
          result.add enumd
          if enumd.cxxName() != newType.cxxName():
            result.add cxxAlias(newType, enumd.decl.cxxTypeUse()).withIt do:
              it.add coms
              coms.clear()

      else:
        raise newImplementKindError(
          baseBody, $node & " " & node.treeRepr())

    of cppFunctionDeclarator:
      let d = cpfDecl
      let body = node[d]
      var args: seq[CxxArg]
      var coms: seq[CxxComment]
      for arg in body["parameters"]:
        if arg of cppComment:
          coms.add toCxxComment(arg)

        else:
          let name =
            if d in arg and (arg[d].kind != cppAbstractPointerDeclarator):
              arg[d].getNameNode().cxxNamePair(cncProc)

            else:
              CxxNamePair()


          var t = toCxxType(arg[cpfType], none CxxNamePair, none CxxNamePair)
          if d in arg:
            pointerWraps(arg[d], t)

          args.add cxxArg(name, t).withIt do:
            it.add coms
            coms.clear()

      result.add cxxAlias(
        body[d].getNameNode().cxxNamePair(cncNone).cxxTypeDecl(ctdkTypedef),
        cxxTypeUse(
          args, toCxxType(node[cpfType], none CxxNamePair, none CxxNamePair)))

    else:
      raise newImplementKindError(node, node.treeRepr())



proc toCxx*(node: CppNode, coms): seq[CxxEntry] =
  case node.kind:
    of cppTranslationUnit,
       cppPreprocIfdef,
       cppPreprocIf,
       cppLinkageSpecification,
       cppDeclarationList,
       cppPreprocElse
         :
      for sub in node:
        result.add toCxx(sub, coms)

    of cppIdentifier,
       cppStringLiteral,
       cppPreprocInclude,
       cppParenthesizedExpression:
      discard

    of cppPreprocCall:
      discard

    of cppClassSpecifier, cppStructSpecifier, cppUnionSpecifier:
      if "body" in node:
        result.add toCxxObject(node, coms)

      else:
        result.add toCxxForwardType(node, coms)

    of cppPreprocDef:
      if node.len < 2:
        discard

      else:
        result.add toCxxMacro(node, coms).box()

    of cppPreprocFunctionDef:
      result.add toCxxMacro(node, coms).box()

    of cppPreprocDefined, cppPreprocElif:
      discard

    of cppComment:
      let text = node.strVal.stripComment()
      if "Copyright (C)" in text:
        discard

      else:
        coms.add cxxComment(text)

    of cppEnumSpecifier:
      result.add toCxxEnum(node, coms).box()

    of cppTypeDefinition:
      result.add toCxxTypeDefinition(node, coms)

    of cppDeclaration:
      case node[cpfDecl].skipPointer().kind:
        of cppFunctionDeclarator:
          result.add toCxxProc(node, coms)

        else:
          raise newImplementKindError(node[1], node.treeRepr())

    of cppTypeIdentifier:
      discard

    of cppExpressionStatement:
      # NOTE first encountered for incorrect code that had `;;` generated.
      # Maybe there is some expression statements that are allowed on
      # toplevel though, so this need to be REVIEW-ed
      discard

    else:
      raise newImplementKindError(
        node,
        node.strVal() & "\n" &
          node.treeRepr(opts = hdisplay(maxlen = 10, maxdepth = 3)))

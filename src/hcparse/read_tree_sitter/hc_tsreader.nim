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
proc toCxxObject*(node: CppNode, coms): CxxObject

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
            result = cxxTypeUse(
              toCxxObject(node, coms), parent.get(), user.get())

          else:
            raise newUnexpectedKindError(node)

      else:
        result = toCxxType(node[0], parent, user)

    of cppFieldDeclaration:
      var args: seq[CxxArg]
      var idx = 0
      for param in node[cpfDecl]["parameters"]:
        args.add toCxxArg(param, idx)
        inc idx

      result = cxxTypeUse(args, toCxxType(node[cpfType], parent, user))

    else:
      raise newImplementKindError(node, node.treeRepr())

template initPointerWraps*(newName, Type: untyped): untyped =
  proc pointerWraps(node: CppNode, ftype: var Type) =
    case node.kind:
      of cppPointerDeclarator:
        ftype = ftype.wrap(ctkPtr)
        if node[0] of cppTypeQualifier:
          pointerWraps(node[1], ftype)

        else:
          pointerWraps(node[0], ftype)

      of cppArrayDeclarator:
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

      of {
        cppFieldIdentifier,
        cppTypeIdentifier,
        cppIdentifier,
        cppFunctionDeclarator
      }:
        discard

      else:
        raise newImplementKindError(
          node, node.strVal() & "\n" & node.treeRepr())

initPointerWraps(cxxTypeUse, CxxTypeUse)

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


proc toCxxProc*(
    main: CppNode,
    coms;
    parent: Option[CxxObject] = none(CxxObject),
  ): CxxProc =

  var parameters = false
  let node =
    if main.kind == cppTemplateDeclaration:
      parameters = true
      main[1]

    else:
      main

  result = node[cpfDecl].getNameNode().cxxNamePair(cncProc).cxxProc()

  if parameters and false:
    echo main.treeRepr(opts = hdisplay(maxLen = 3))

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
      CxxNamePair(), toCxxType(node[cpfType],
                               some parent, none CxxNamePair))

  else:
    let decl = node[cpfDecl]
    let name = getNameNode(decl).cxxNamePair(cncField)
    if decl of cppFunctionDeclarator:
      result = cxxField(name, toCxxType(node, some parent, some name))

    else:
      result = cxxField(name, toCxxType(node[cpfType], some parent, some name))
      pointerWraps(decl, result.nimType)

  for item in node:
    if item of cppBitfieldClause:
      result.bitsize = some item[0].strVal().parseInt()

proc toCxxForwardType*(node: CppNode, coms): CxxForward =
  result = cxxForward(
    cxxPair(node["name"].strVal()),
    ctdkStruct #[ TEMP implement better detection ]#)

  result.add coms
  coms.clear()

proc toCxxObject*(node: CppNode, coms): CxxObject =
  var decl: CxxNamePair
  if "name" in node:
    decl = cxxPair(node["name"].strVal())

  result = cxxObject(decl)
  result.add coms
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
        result.super.add toCxxType(
          class, none CxxNamePair, none CxxNamePair)

  for item in node["body"]:
    if item of cppAccessSpecifier:
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


    elif accs == casPublic:
      case item.kind:
        of cppDeclaration:
          var meth = toCxxProc(item, coms, some result).withIt do:
            it.add coms
            coms.clear()

          if meth.cxxName().scopes.last() == result.cxxName().scopes.last():
            meth.constructorOf = some result.name()

          elif node[cpfDecl][cpfDecl] of cppDestructorName:
            meth.destructorOf = some result.name()

          result.methods.add meth

        of cppFieldDeclaration, cppFunctionDefinition:
          if cpfDecl notin item and
             item[cpfType] of { cppStructSpecifier }:
            result.nested.add toCxxObject(item[cpfType], coms)

          elif cpfDecl in item and
               item[cpfDecl] of cppFunctionDeclarator:
            let name = item[cpfDecl][cpfDecl]
            if name of cppParenthesizedDeclarator:
              result.mfields.add toCxxField(
                  item, coms, result.decl.name).withIt do:
                it.add coms
                coms.clear()

            else:
              var meth = toCxxProc(item, coms, some result).withIt do:
                it.add coms
                coms.clear()

              case signal:
                of cqsNone: discard
                of cqsSignal: meth.flags.incl cpfSignal
                of cqsSlot: meth.flags.incl cpfSlot

              result.methods.add meth

          else:
            result.mfields.add toCxxField(
                item, coms, result.decl.name).withIt do:
              it.add coms
              coms.clear()

        of cppComment:
          coms.add toCxxComment(item)

        of cppQPropertyDeclaration:
          echov "skipping qproperty"

        else:
          failNode(item)

  if ?result.mfields:
    result.mfields.last().add coms
    coms.clear()


proc toCxxTypeDefinition*(node: CppNode, coms): seq[CxxEntry] =
  if node.len == 1:
    case node[0].kind:
      of cppEnumSpecifier:
        result.add toCxxEnum(node[0], coms).withIt do:
          it.add coms
          coms.clear()

      else:
        raise newImplementKindError(node[0])

  elif node.len == 2:
    case node[1].kind:
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

        elif baseBody of {cppStructSpecifier, cppUnionSpecifier}:
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

  else:
    raise newImplementError(node.treeRepr())


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

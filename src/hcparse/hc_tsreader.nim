import
  htsparse/cpp/cpp

import
  std/[json, strutils, parseutils, tables]

import
  ./hc_types,
  ./hc_typeconv,
  ./cxcommon,
  ./interop_ir/wrap_store

import
  hmisc/core/all,
  hmisc/wrappers/[treesitter],
  hmisc/other/oswrap

export parseCppString, treeRepr

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

proc mapOpName*(node: CppNode): string =
  case node.strVal():
    of "|": "or"
    of "<<": "shl"
    of ">>": "shr"
    of "&": "and"
    of "^": "xor"
    of "~": "not"
    of "!": "not" # QUESTION what is the difference between ~ and !
    else: node.strVal()

proc mapTypeName*(node: CppNode): string =
  if node.kind == cppTypeIdentifier:
    node.strVal()

  else:
    mapPrimitiveName(node.primitiveName())

proc toCxxType*(node: CppNode): CxxTypeUse =
  case node.kind:
    of cppTypeIdentifier,
       cppSizedTypeSpecifier,
       cppPrimitiveType:
      result = cxxTypeUse(cxxPair(
        mapTypeName(node),
        cxxName(@[node.strVal()])))

      if node of {cppSizedTypeSpecifier, cppPrimitiveType}:
        result.podKind = node.primitiveName().mapPrimitivePod()
        result.flags.incl ctfIsPodType

    of cppStructSpecifier, cppEnumSpecifier, cppUnionSpecifier:
      result = toCxxType(node[0])

    else:
      raise newImplementKindError(node, node.treeRepr())



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


    else:
      raise newImplementKindError(node, node.treeRepr())

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
        inc value


      of cppComment:
        result.values[^1].add cxxComment(en.strVal().stripComment())

      else:
        raise newImplementKindError(en)


proc skipPointer(node: CppNode): CppNode =
  case node.kind:
    of cppPointerDeclarator: skipPointer(node[0])
    else: node

template initPointerWraps*(newName, Type: untyped): untyped =
  proc pointerWraps(node: CppNode, ftype: var TYpe) =
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

proc getName*(node: CppNode): string =
  case node.kind:
    of cppFieldIdentifier,
       cppTypeIdentifier,
       cppIdentifier,
       cppPrimitiveType:
      node.strVal()

    of cppArrayDeclarator, cppFunctionDeclarator:
      node[0].getName()

    of cppDeclaration, cppInitDeclarator:
      node["declarator"].getName()

    else:
      if node.len > 0:
        node[^1].getName()

      else:
        raise newImplementKindError(node, node.treeRepr())

proc toCxxArg*(node: CppNode, idx: int): CxxArg =
  assertKind(node, {cppParameterDeclaration})
  var
    name: CxxNamePair
    argt: CxxTypeUse = toCxxType(node["type"])

  if "declarator" in node:
    name = cxxPair(getName(node["declarator"]))
    pointerWraps(node["declarator"], argt)

  else:
    name = cxxPair("a" & $idx)

  result = cxxArg(name, argt)


proc toCxxProc*(
    node: CppNode,
    coms;
    parent: Option[CxxObject] = none(CxxObject),
  ): CxxProc =

  result = cxxProc(cxxPair(node["declarator"].getName()))
  result.returnType = toCxxType(node["type"])

  if parent.isSome():
    result.methodOf = some parent.get().decl.cxxTypeUse()

  if node[0].kind == cppTypeQualifier:
    result.returnType.flags.incl ctfConst

  pointerWraps(node["declarator"], result.returnType)

  let decl =
    if node["declarator"].kind == cppPointerDeclarator:
      node["declarator"].skipPointer()

    else:
      node["declarator"]

  for idx, arg in decl["parameters"]:
    result.arguments.add toCxxArg(arg, idx)


proc toCxxField*(node: CppNode, coms): CxxField =
  assertKind(node, {cppFieldDeclaration})
  result = cxxField(
    cxxPair(getName(node["declarator"])),
    toCxxType(node["type"]))

  pointerWraps(node["declarator"], result.nimType)

proc toCxxForwardType*(node: CppNode, coms): CxxForward =
  result = cxxForward(cxxPair(node["name"].strVal()))
  result.add coms
  coms.clear()

proc toCxxObject*(node: CppNode, coms): CxxObject =
  var decl: CxxNamePair
  if "name" in node:
    decl = cxxPair(node["name"].strVal())

  result = cxxObject(decl)
  result.add coms
  coms.clear()

  case node.kind:
    of cppStructSpecifier: result.kind = cokStruct
    of cppUnionSpecifier: result.kind = cokUnion
    of cppClassSpecifier: result.kind = cokClass
    else: raise newUnexpectedKindError(node)

  for field in node["body"]:
    case field.kind:
      of cppFieldDeclaration:
        if field["declarator"] of cppFunctionDeclarator:
          result.methods.add toCxxProc(field, coms, some result)

        else:
          result.mfields.add toCxxField(field, coms)

      of cppComment:
        result.mfields[^1].docComment.add cxxComment(field.strVal().stripComment())

      else:
        raise newImplementKindError(field, field.treeRepr())


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
      of cppTypeIdentifier,
         cppPointerDeclarator,
         cppPrimitiveType:
        let newType = toCxxType(node["declarator"]).toDecl()
        let baseBody = node[0]
        if baseBody of {
          cppSizedTypeSpecifier, cppPrimitiveType, cppTypeIdentifier
        } or (
          baseBody of {
            cppStructSpecifier, cppUnionSpecifier, cppEnumSpecifier} and
          "body" notin baseBody
        ):
          var alias = cxxAlias(newType, toCxxType(node["type"])).withIt do:
                it.add coms
                coms.clear()
          pointerWraps(node["declarator"], alias.baseType)

          if alias.baseType of ctkIdent and
            alias.decl.cxxName() == alias.baseType.cxxName():
            # `typedef struct T T;`
            result.add cxxForward(alias.decl.name).withIt do:
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
        let d = "declarator"
        let body = node[d]
        var args: seq[CxxArg]
        for arg in body["parameters"]:
          let name =
            if d in arg and (arg[d].kind != cppAbstractPointerDeclarator):
              arg[d].getName()

            else:
              ""


          var t = toCxxType(arg["type"])
          if d in arg:
            pointerWraps(arg[d], t)

          args.add cxxArg(cxxPair(name), t)

        result.add cxxAlias(
          body[d].getName().cxxPair().cxxTypeDecl(),
          cxxTypeUse(args, toCxxType(node["type"])))

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
      if "body" notin node:
        result.add toCxxForwardType(node, coms)

      else:
        result.add toCxxObject(node, coms)

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
      case node["declarator"].skipPointer().kind:
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

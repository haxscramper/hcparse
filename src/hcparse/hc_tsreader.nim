import
  htsparse/cpp/cpp

import
  std/[json, strutils, parseutils, tables]

import
  ./hc_types,
  ./hc_typeconv,
  ./interop_ir/wrap_store

import
  hmisc/core/all,
  hmisc/wrappers/[treesitter],
  hmisc/other/oswrap

export parseCppString, treeRepr

func stripComment*(text: string): string =
  const starts = ["/*!", "/**<", "/**", "/*", "*", "//<", "///", "//"]
  const ends  = ["*/"]
  var idx = 0
  for line in text.splitLines():
    var pos = line.skipWhile({' '})
    var final = line.high
    for start in starts:
      if line[pos .. pos + start.high] == start:
        pos = pos + start.len
        break

    for endc in ends:
      if line[final - endc.high .. final] == endc:
        final = final - endc.len
        break

    if idx != 0: result.add "\n"
    inc idx
    result.add line[pos .. final]

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
    of cppTypeIdentifier, cppSizedTypeSpecifier, cppPrimitiveType:
      result = cxxTypeUse(cxxPair(
        mapTypeName(node),
        cxxName(@[node.strVal()])))

    of cppStructSpecifier, cppEnumSpecifier, cppUnionSpecifier:
      result = toCxxType(node[0])

    else:
      raise newImplementKindError(node, node.treeRepr())



proc toCxxMacro*(node: CppNode): CxxMacro =
  assertKind(node, {cppPreprocFunctionDef, cppPreprocDef})

  result = cxxMacro(cxxPair(node["name"].strVal()))

  # result = CxxMacro(
  #   haxdocIdent: conf.getHaxdoc(@[]),
  #   name: (node["name"].strVal)
  # )

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

    else:
      raise newImplementKindError(node, node.treeRepr())

proc toCxxEnum*(node: CppNode): CxxEnum =
  var name: CxxNamePair

  if "name" in node:
    name = cxxPair(node["name"].strVal())

  result = cxxEnum(name)

  var env: CxxEvalCtx
  var value: BiggestInt = 0
  for en in node["body"]:
    case en.kind:
      of cppEnumerator:
        if "value" in en:
          value = evalEnumValue(en["value"], env)

        let name = en["name"].strVal()
        env.table[name] = value
        result.values.add CxxEnumValue(
          name: cxxPair(name),
          value: value)

        inc value


      of cppComment:
        result.values[^1].comment.add en.strVal()

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
    parent: Option[CxxObject] = none(CxxObject)
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


proc toCxxField*(node: CppNode): CxxField =
  assertKind(node, {cppFieldDeclaration})
  result = cxxField(
    cxxPair(getName(node["declarator"])),
    toCxxType(node["type"]))

  pointerWraps(node["declarator"], result.nimType)

proc toCxxForwardType*(node: CppNode): CxxForward =
  cxxForward(cxxPair(node["name"].strVal()))

proc toCxxObject*(node: CppNode): CxxObject =
  var decl: CxxNamePair
  if "name" in node:
    decl = cxxPair(node["name"].strVal())

  result = cxxObject(decl)

  case node.kind:
    of cppStructSpecifier: result.kind = cokStruct
    of cppUnionSpecifier: result.kind = cokUnion
    of cppClassSpecifier: result.kind = cokClass
    else: raise newUnexpectedKindError(node)

  for field in node["body"]:
    case field.kind:
      of cppFieldDeclaration:
        if field["declarator"] of cppFunctionDeclarator:
          result.methods.add toCxxProc(field, some result)

        else:
          result.mfields.add toCxxField(field)

      of cppComment:
        result.mfields[^1].docComment.mget().add field.strVal().stripComment()

      else:
        raise newImplementKindError(field, field.treeRepr())



proc toCxx*(node: CppNode): seq[CxxEntry] =
  case node.kind:
    of cppTranslationUnit,
       cppPreprocIfdef,
       cppPreprocIf,
       cppLinkageSpecification,
       cppDeclarationList,
       cppPreprocElse
         :
      for sub in node:
        result.add toCxx(sub)

    of cppIdentifier,
       cppStringLiteral,
       cppPreprocInclude,
       cppParenthesizedExpression:
      discard

    of cppPreprocCall:
      discard

    of cppClassSpecifier, cppStructSpecifier, cppUnionSpecifier:
      if "body" notin node:
        result.add toCxxForwardType(node)

      else:
        result.add toCxxObject(node)

    of cppPreprocDef:
      if node.len < 2:
        discard

      else:
        result.add toCxxMacro(node).box()

    of cppPreprocFunctionDef:
      result.add toCxxMacro(node).box()

    of cppPreprocDefined, cppPreprocElif:
      discard

    of cppComment:
      discard
      # result.add toCxxComment(node.strVal)

    of cppEnumSpecifier:
      result.add toCxxEnum(node).box()

    of cppTypeDefinition:
      if node.len == 1:
        case node[0].kind:
          of cppEnumSpecifier:
            result.add toCxxEnum(node[0]).box()

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
              var alias = cxxAlias(newType, toCxxType(node["type"]))
              pointerWraps(node["declarator"], alias.baseType)

              if alias.baseType of ctkIdent and
                alias.decl.cxxName() == alias.baseType.cxxName():
                # `typedef struct T T;`
                discard

              else:
                result.add alias

            elif baseBody of {cppStructSpecifier, cppUnionSpecifier}:
              # FIXME handle multiple trailing typedefs
              var struct = toCxxObject(baseBody)
              if struct.cxxName().isEmpty():
                # Handle `typedef struct {} struct_name;`
                struct.name = newType.name
                result.add struct

              else:
                result.add struct
                if struct.cxxName() != newType.cxxName():
                  result.add cxxAlias(newType, struct.decl.cxxTypeUse())

            elif baseBody of {cppEnumSpecifier}:
              var enumd = toCxxEnum(node[0])
              if enumd.cxxName().isEmpty():
                enumd.name = newType.name
                result.add enumd

              else:
                result.add enumd
                if enumd.cxxName() != newType.cxxName():
                  result.add cxxAlias(newType, enumd.decl.cxxTypeUse())

            else:
              raise newImplementKindError(
                baseBody, $node & " " & node.treeRepr())

          of cppFunctionDeclarator:
            result.add cxxAlias(
              toCxxType(node["type"]).toDecl(),
              cxxTypeUse(node["declarator"].getName(), @[]))

          else:
            raise newImplementKindError(node, node.treeRepr())

      else:
        raise newImplementError(node.treeRepr())

    of cppDeclaration:
      case node["declarator"].skipPointer().kind:
        of cppFunctionDeclarator:
          result.add toCxxProc(node)

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

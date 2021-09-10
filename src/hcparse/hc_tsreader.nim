import
  htsparse/cpp/cpp

import
  std/[json]

import
  ./hc_types,
  ./hc_save

import
  hmisc/core/all,
  hmisc/wrappers/[treesitter],
  hmisc/other/oswrap

import
  hnimast/interop/wrap_store

export parseCppString


proc getHaxdoc*(conf: WrapConf, parent: seq[CppNode]): JsonNode =
  newJNull()

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

proc toCxxType*(conf: WrapConf, node: CppNode): CxxType =
  case node.kind:
    of cppTypeIdentifier, cppSizedTypeSpecifier, cppPrimitiveType:
      result = CxxType(kind: ctkIdent, nimName: mapTypeName(node))

    else:
      raise newImplementKindError(node, node.treeRepr())



proc fixType*(save: CxxType): CxxType =
  result = save



proc toCxxMacro*(conf: WrapConf, node: CppNode): CxxMacro =
  assertKind(node, {cppPreprocFunctionDef, cppPreprocDef})

  result = CxxMacro(
    haxdocIdent: conf.getHaxdoc(@[]),
    cxxName: @[node["name"].strVal]
  )

  if "parameters" in node:
    for arg in node["parameters"]:
      result.arguments.add arg.strVal()

  # TODO convert body


proc toCxxEnum*(conf: WrapConf, node: CppNode): CxxEnum =
  result = CxxEnum(haxdocIdent: conf.getHaxdoc(@[node]))

  if "name" in node:
    result.cxxName = @[node["name"].strVal()]

  for en in node["body"]:
    case en.kind:
      of cppEnumerator:
        result.values.add CxxEnumValue(
          cxxName: @[en["name"].strVal()],
          value: 0 # TODO convert from `en["value"]`
        )

      of cppComment:
        result.values[^1].comment.add en.strVal()

      else:
        raise newImplementKindError(en)


proc skipPointer(node: CppNode): CppNode =
  case node.kind:
    of cppPointerDeclarator: skipPointer(node[0])
    else: node

template initPointerWraps*(newName, TYpe: untyped): untyped =
  proc pointerWraps(node: CppNode, ftype: var TYpe) =
    case node.kind:
      of cppPointerDeclarator:
        ftype = newName("ptr", @[ftype])
        if node[0] of cppTypeQualifier:
          pointerWraps(node[1], ftype)

        else:
          pointerWraps(node[0], ftype)

      of cppArrayDeclarator:
        ftype = newName("array", @[ftype])
        pointerWraps(node[0], ftype)

      of cppInitDeclarator,
         cppTypeQualifier #[ TODO convert for CxxType? ]#:
        pointerWraps(node[0], ftype)

      of cppAbstractPointerDeclarator:
        ftype = newName("ptr", @[ftype])
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

initPointerWraps(newCxxType, CxxType)

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

proc toCxxArg*(conf: WrapConf, node: CppNode, idx: int): CxxArg =
  assertKind(node, {cppParameterDeclaration})
  result = CxxArg(haxdocIdent: conf.getHaxdoc(@[node]))
  result.nimType = conf.toCxxType(node["type"])

  if "declarator" in node:
    result.cxxName = @[getName(node["declarator"])]
    pointerWraps(node["declarator"], result.nimType)

  else:
    result.nimName = "a" & $idx

proc toCxxProc*(conf: WrapConf, node: CppNode): CxxProc =
  result = CxxProc(haxdocIdent: conf.getHaxdoc(@[node]))
  result.cxxName = @[node["declarator"].getName()]

  result.returnType = conf.toCxxType(node["type"])


  if node[0].kind == cppTypeQualifier:
    result.returnType = newCxxType("const", @[result.returnType])

  pointerWraps(node["declarator"], result.returnType)

  let decl =
    if node["declarator"].kind == cppPointerDeclarator:
      node["declarator"].skipPointer()

    else:
      node["declarator"]

  for idx, arg in decl["parameters"]:
    result.arguments.add conf.toCxxArg(arg, idx)


proc toCxxField*(conf: WrapConf, node: CppNode): CxxField =
  assertKind(node, {cppFieldDeclaration})

  result = CxxField(
    haxdocIdent: conf.getHaxdoc(@[node]),
    nimName: getName(node["declarator"])
  )


  var ftype = conf.toCxxType(node["type"])
  pointerWraps(node["declarator"], ftype)
  result.nimType = fixType(ftype)

proc toCxxObject*(conf: WrapConf, node: CppNode): CxxObject =
  result = CxxObject(haxdocIdent: conf.getHaxdoc(@[node]))

  case node.kind:
    of cppStructSpecifier:
      result.kind = gokStruct

    of cppUnionSpecifier:
      result.kind = gokUnion

    else:
      raise newImplementKindError(node)

  if "name" in node:
    result.cxxName = @[node["name"].strVal()]

  for field in node["body"]:
    case field.kind:
      of cppFieldDeclaration:
        result.mfields.add conf.toCxxField(field)

      of cppComment:
        result.mfields[^1].docComment.add field.strVal()

      else:
        raise newImplementKindError(field, field.treeRepr())



proc toCxx*(conf: WrapConf, node: CppNode): seq[CxxEntry] =
  case node.kind:
    of cppTranslationUnit,
       cppPreprocIfdef,
       cppPreprocIf,
       cppLinkageSpecification,
       cppDeclarationList,
       cppPreprocElse
         :
      for sub in node:
        result.add conf.toCxx(sub)

    of cppIdentifier,
       cppStringLiteral,
       cppPreprocInclude,
       cppParenthesizedExpression
         :
      discard

    of cppPreprocCall:
      discard

    of cppPreprocDef:
      if node.len < 2:
        discard

      else:
        result.add toCxxMacro(conf, node).box()

    of cppPreprocFunctionDef:
      result.add toCxxMacro(conf, node).box()

    of cppComment:
      result.add toCxxComment(node.strVal)

    of cppEnumSpecifier:
      result.add toCxxEnum(conf, node).box()

    of cppTypeDefinition:
      if node.len == 1:
        case node[0].kind:
          of cppEnumSpecifier:
            result.add toCxxEnum(conf, node[0]).box()

          else:
            raise newImplementKindError(node[0])

      elif node.len == 2:
        case node[1].kind:
          of cppTypeIdentifier,
             cppPointerDeclarator,
             cppPrimitiveType
               :
            case node[0].kind:
              of cppSizedTypeSpecifier,
                 cppPrimitiveType
                   :
                var save = CxxAlias(
                  haxdocIdent: conf.getHaxdoc(@[]),
                  baseType: conf.toCxxType(node["type"]),
                  newAlias: newCxxType(node["declarator"].getName(), @[]))

                pointerWraps(node["declarator"], save.baseType)
                result.add save

              of cppStructSpecifier, cppUnionSpecifier:
                let struct = conf.toCxxObject(node[0])
                result.add CxxAlias(
                  haxdocIdent: conf.getHaxdoc(@[]),
                  baseType: CxxType(kind: ctkIdent, nimName: struct.nimName),
                  newAlias: conf.toCxxType(node["declarator"])
                )

              else:
                raise newImplementKindError(node[0], node.treeRepr())

          of cppFunctionDeclarator:
            result.add CxxAlias(
              haxdocIdent: conf.getHaxdoc(@[]),
              baseType: conf.toCxxType(node["type"]),
              newAlias: newCxxType(node["declarator"].getName(), @[])
            )

          else:
            raise newImplementKindError(node, node.treeRepr())

      else:
        raise newImplementError(node.treeRepr())

    of cppDeclaration:
      case node["declarator"].skipPointer().kind:
        of cppFunctionDeclarator:
          result.add conf.toCxxProc(node)

        else:
          raise newImplementKindError(node[1], node.treeRepr())

    else:
      raise newImplementKindError(node, node.treeRepr(
        opts = hdisplay(maxlen = 10, maxdepth = 3)))

proc toCxx*(
  conf: WrapConf, file: AbsFile, expand: bool = false): seq[CxxEntry] =

  var str =
    if expand:
      file.getExpanded(conf.parseConf)

    else:
      file.readFile()

  result = conf.toCxx(parseCppString(addr str))


proc toCxx*(conf: WrapConf, str: string): seq[CxxEntry] =
  let node = parseCppString(unsafeAddr str)
  result = conf.toCxx(node)

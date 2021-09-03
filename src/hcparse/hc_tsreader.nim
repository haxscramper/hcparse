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
    else: node.strVal()

proc mapTypeName*(node: CppNode): string =
  if node.kind == cppTypeIdentifier:
    node.strVal()

  else:
    case node.primitiveName():
      of "unsigned long": "ulong"
      of "long long": "clonglong"
      of "void": "void"
      of "int": "cint"
      of "char": "char"
      of "unsigned", "unsigned int": "cuint"
      of "float": "cfloat"
      of "uint8_t": "uint8"
      of "uint16_t": "uint16"
      of "int16_t": "int16"
      of "bool": "bool"
      else:
        raise newImplementKindError(node.primitiveName(), node.treeRepr())

proc toSaveType*(conf: WrapConf, node: CppNode): SaveType =
  case node.kind:
    of cppTypeIdentifier, cppSizedTypeSpecifier, cppPrimitiveType:
      result = SaveType(kind: ctkIdent, nimName: mapTypeName(node))

    else:
      raise newImplementKindError(node, node.treeRepr())



proc fixType*(save: SaveType): SaveType =
  result = save



proc toSaveMacro*(conf: WrapConf, node: CppNode): SaveMacro =
  assertKind(node, {cppPreprocFunctionDef, cppPreprocDef})

  result = SaveMacro(
    haxdocIdent: conf.getHaxdoc(@[]),
    cxxName: @[node["name"].strVal]
  )

  if "parameters" in node:
    for arg in node["parameters"]:
      result.arguments.add arg.strVal()

  # TODO convert body


proc toSaveEnum*(conf: WrapConf, node: CppNode): SaveEnum =
  result = SaveEnum(haxdocIdent: conf.getHaxdoc(@[node]))

  if "name" in node:
    result.cxxName = @[node["name"].strVal()]

  for en in node["body"]:
    case en.kind:
      of cppEnumerator:
        result.values.add SaveEnumValue(
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
         cppTypeQualifier #[ TODO convert for SaveType? ]#:
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

initPointerWraps(newSaveType, SaveType)

proc getName*(node: CppNode): string =
  case node.kind:
    of cppFieldIdentifier,
       cppTypeIdentifier,
       cppIdentifier:
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

proc toSaveArg*(conf: WrapConf, node: CppNode, idx: int): SaveArg =
  assertKind(node, {cppParameterDeclaration})
  result = SaveArg(haxdocIdent: conf.getHaxdoc(@[node]))
  result.nimType = conf.toSaveType(node["type"])

  if "declarator" in node:
    result.cxxName = @[getName(node["declarator"])]
    pointerWraps(node["declarator"], result.nimType)

  else:
    result.nimName = "a" & $idx

proc toSaveProc*(conf: WrapConf, node: CppNode): SaveProc =
  result = SaveProc(haxdocIdent: conf.getHaxdoc(@[node]))
  result.cxxName = @[node["declarator"].getName()]

  result.returnType = conf.toSaveType(node["type"])


  if node[0].kind == cppTypeQualifier:
    result.returnType = newSaveType("const", @[result.returnType])

  pointerWraps(node["declarator"], result.returnType)

  let decl =
    if node["declarator"].kind == cppPointerDeclarator:
      node["declarator"].skipPointer()

    else:
      node["declarator"]

  for idx, arg in decl["parameters"]:
    result.arguments.add conf.toSaveArg(arg, idx)


proc toSaveField*(conf: WrapConf, node: CppNode): SaveField =
  assertKind(node, {cppFieldDeclaration})

  result = SaveField(
    haxdocIdent: conf.getHaxdoc(@[node]),
    nimName: getName(node["declarator"])
  )


  var ftype = conf.toSaveType(node["type"])
  pointerWraps(node["declarator"], ftype)
  result.nimType = fixType(ftype)

proc toSaveObject*(conf: WrapConf, node: CppNode): SaveObject =
  result = SaveObject(haxdocIdent: conf.getHaxdoc(@[node]))

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
        result.mfields.add conf.toSaveField(field)

      of cppComment:
        result.mfields[^1].docComment.add field.strVal()

      else:
        raise newImplementKindError(field, field.treeRepr())



proc toSave*(conf: WrapConf, node: CppNode): seq[SaveEntry] =
  case node.kind:
    of cppTranslationUnit,
       cppPreprocIfdef,
       cppPreprocIf,
       cppLinkageSpecification,
       cppDeclarationList,
       cppPreprocElse
         :
      for sub in node:
        result.add conf.toSave(sub)

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
        result.add toSaveMacro(conf, node).box()

    of cppPreprocFunctionDef:
      result.add toSaveMacro(conf, node).box()

    of cppComment:
      result.add toSaveComment(node.strVal)

    of cppEnumSpecifier:
      result.add toSaveEnum(conf, node).box()

    of cppTypeDefinition:
      if node.len == 1:
        case node[0].kind:
          of cppEnumSpecifier:
            result.add toSaveEnum(conf, node[0]).box()

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
                var save = SaveAlias(
                  haxdocIdent: conf.getHaxdoc(@[]),
                  baseType: conf.toSaveType(node["type"]),
                  newAlias: newSaveType(node["declarator"].getName(), @[]))

                pointerWraps(node["declarator"], save.baseType)
                result.add save

              of cppStructSpecifier, cppUnionSpecifier:
                let struct = conf.toSaveObject(node[0])
                result.add SaveAlias(
                  haxdocIdent: conf.getHaxdoc(@[]),
                  baseType: SaveType(kind: ctkIdent, nimName: struct.nimName),
                  newAlias: conf.toSaveType(node["declarator"])
                )

              else:
                raise newImplementKindError(node[0], node.treeRepr())

          of cppFunctionDeclarator:
            result.add SaveAlias(
              haxdocIdent: conf.getHaxdoc(@[]),
              baseType: conf.toSaveType(node["type"]),
              newAlias: newSaveType(node["declarator"].getName(), @[])
            )

          else:
            raise newImplementKindError(node, node.treeRepr())

      else:
        raise newImplementError(node.treeRepr())

    of cppDeclaration:
      case node["declarator"].skipPointer().kind:
        of cppFunctionDeclarator:
          result.add conf.toSaveProc(node)

        else:
          raise newImplementKindError(node[1], node.treeRepr())

    else:
      raise newImplementKindError(node, node.treeRepr(
        opts = hdisplay(maxlen = 10, maxdepth = 3)))

proc toSave*(
  conf: WrapConf, file: AbsFile, expand: bool = false): seq[SaveEntry] =

  var str =
    if expand:
      file.getExpanded(conf.parseConf)

    else:
      file.readFile()

  result = conf.toSave(parseCppString(addr str))


proc toSave*(conf: WrapConf, str: string): seq[SaveEntry] =
  let node = parseCppString(unsafeAddr str)
  result = conf.toSave(node)

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
  echo node.getTs().treeRepr(node.getBase())

const cppTypeSpecSpec* = {
  cppClassSpecifier,
  cppUnionSpecifier,
  cppStructSpecifier }

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

proc parseCppInt*(val: string): int =
  if val.startsWith("0x"):
    return parseHexInt(val)

  else:
    return parseInt(val)

proc getNameNode*(node: CppNode): CppNode =
  case node.kind:
    of cppFieldIdentifier,
       cppTypeIdentifier,
       cppIdentifier,
       cppQualifiedIdentifier,
       cppNamespaceIdentifier,
       cppPrimitiveType:
      node

    of cppArrayDeclarator,
       cppFunctionDeclarator,
       cppEnumerator,
       cppTemplateType,
       cppPreprocDef,
       cppPreprocFunctionDef,
       cppAssignmentExpression:
      node[0].getNameNode()

    of cppDeclaration, cppInitDeclarator:
      node[cpfDecl].getNameNode()

    else:
      if node.len > 0:
        node[^1].getNameNode()

      else:
        failNode node

proc optCxxNamePair*(
  node: CppNode, context: CxxNameContext): Option[CxxNamePair] =
  case node.kind:
    of cppQualifiedIdentifier:
      result = some cxxPair(
        node[0].optCxxNamePair(context).get().cxx &
        node[1].optCxxNamePair(context).get().cxx,
        context
      )

    of cppFieldDeclaration:
      if cpfDecl in node:
        result = optCxxNamePair(node[cpfDecl], context)

    of cppNamespaceIdentifier,
       cppIdentifier,
       cppSizedTypeSpecifier,
       cppDestructorName,
       cppPrimitiveType,
       cppTypeIdentifier,
       cppFieldIdentifier:
      result = some cxxPair(
        cxxName node.getBase()[node.slice()], context)

    of cppTemplateType:
      # FIXME dropping the template arguments. In the future
      # `CxxNamePair`'s scopes should contain information about template
      # arguments instead of simple sequence of strings.
      #
      # NOTE when handling template arguments don't forget to switch
      # context of the template type parameters to a something different.
      result = optCxxNamePair(node["name"], context)

    else:
      discard


proc cxxNamePair*(node: CppNode, context: CxxNameContext): CxxNamePair =
  let pair = optCxxNamePair(node, context)
  if pair.isNone():
    failNode node

  else:
    return pair.get()

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
        let value =
          if size of cppNumberLiteral:
            CxxExpr(kind: cekIntLit, intVal: size.strVal().parseCppInt())

          else:
            CxxExpr(
              kind: cekVar, ident: getNameNode(size).cxxNamePair(cncNone))

        ftype = wrapArray(CxxTypeUse(kind: ctkStaticParam, value: value), ftype)

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

    of cppFunctionDeclarator:
      if node[cpfDecl] of {
        cppFieldIdentifier, # Simple procedure
        cppDestructorName,
        cppQualifiedIdentifier # wraps of `void* VGA::get_windowsize`
      } and
         node["parameters"] of cppParameterList:
        # `int proc();` `~Name()` are parsed as field declarator by
        # tree-sitter, so providing early detection here. This procedure is
        # also called for 'pointer wraps' of the standalone procs, so
        # filtering out identifiers here.
        return

      var args: seq[CxxArg]
      var idx = 0
      for param in node["parameters"]:
        args.add toCxxArg(param, idx)
        inc idx

      ftype = cxxTypeUse(args, ftype)
      if node[cpfDecl] of cppParenthesizedDeclarator and
         node[cpfDecl][0] of cppPointerDeclarator:
        # Skipping single pointer layer `(*(*foo))`, because 'ctkProc' already
        # implies this is a pointer (we can't have procedure as a value
        # type stored in the object, unless it is a literal blob of
        # executable opcodes).
        pointerWraps(node[cpfDecl][0][0], ftype)

      elif node[cpfDecl] of { cppFieldIdentifier, cppIdentifier }:
        # Ignoring simple field identifier here - `int (*foo)();` has no
        # additional indirection layers.
        discard

      else:
        # Sanity check fallback to avoid missing other weird edge cases.
        failNode node[cpfDecl]

    of {
      cppFieldIdentifier,
      cppTypeIdentifier,
      cppIdentifier,
      cppAuto
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

    of cppSizedTypeSpecifier, cppPrimitiveType, cppAuto:
      result = cxxTypeUse(node.primitiveName().mapPrimitivePod())

    of cppQualifiedIdentifier:
      # Iterate over qualified identifiers, compacting namespaces and
      # recusing into types
      proc aux(node: CppNode): seq[CppNode] =
        case node.kind:
          of cppQualifiedIdentifier:
            result.add aux(node[0])
            result.add aux(node[1])

          of cppNamespaceIdentifier,
             cppTemplateType,
             cppTypeIdentifier:
            result.add node

          else:
            failNode node

      let fold: seq[CppNode] = aux(node)

      result = CxxTypeUse(kind: ctkIdent)
      var curr = CxxTypeRef()
      for item in fold:
        # echo ">>>>>>>>>>>>>>"
        # debug item
        var name = getNameNode(item)
        # debug name
        curr.name.cxx.scopes.add name.strVal()
        if item of { cppTypeIdentifier, cppTemplateType }:
          result.types.add(curr, @[])

        if item of cppTemplateType:
          for param in item["arguments"]:
            result.types.last().genParams.add(
              param["type"].toCxxType(parent, user))

      # var names: seq[string]
      # for name in items(node):
      #   names.add name.strVal()

      # result = cxxPair(mapTypeName(node), cxxName(names)).cxxTypeUse()

    of cppStructSpecifier, cppEnumSpecifier, cppUnionSpecifier:
      if node[0] of cppFieldDeclarationList:
        var coms: seq[CxxComment]
        case node.kind:
          of cppEnumSpecifier:
            result = cxxTypeUse(toCxxEnum(node, coms), parent, user)

          of cppUnionSpecifier, cppStructSpecifier:
            result = cxxTypeUse(toCxxObject(node, coms), parent, user)

          else:
            failNode node

      else:
        result = toCxxType(node[0], parent, user)

    of cppFieldDeclaration:
      result = toCxxType(node[cpfType], parent, user)
      pointerWraps(node[cpfDecl], result)

    of cppDecltype:
      result = CxxTypeUse(kind: ctkDecltype, value: CxxExpr(
        kind: cekCallLit, strVal: node.getBase()[node[0].slice()]))

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


# proc getName*(node: CppNode): string = node.getNameNode().strVal()



proc toCxxEnum*(node: CppNode, coms): CxxEnum =
  var name = node.getNameNode().cxxNamePair(cncType)
  result = cxxEnum(name)
  result.add coms
  coms.clear()

  var env: CxxEvalCtx
  var elements: seq[CppNode]
  if "body" in node:
    for en in node["body"]:
      elements.add en

  # HACK it seems like current version of the C++ parser does not support
  # nested enum declarations. As a result `enum Kind {}` is treated like a
  # regular field declaration with initializer list. For now I will just
  # work around that, in the future grammar should be fixed instead.
  # Currently `class Name { enum class Kind { Item1, Item2 }; };` is parsed
  # incorrectly
  elif "default_value" in node:
    for en in node["default_value"]:
      elements.add en

  var value: BiggestInt = 0
  for en in elements:
    case en.kind:
      of cppEnumerator, cppAssignmentExpression, cppIdentifier:
        if "value" in en:
          value = evalEnumValue(en["value"], env)

        elif "right" in en:
          value = evalEnumValue(en["right"], env)

        let name = en.getNameNode().strVal()
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
  let name = node[cpfDecl].getNameNode()
  result = name.withResIt do:
    it.cxxNamePair(cncProc).cxxProc()

  if cpfType in node:
    result.returnType = toCxxType(
      node[cpfType],
      some result.head.name,
      some result.head.name
    )

  else:
    if "name" in name and
       name["name"] of cppDestructorName:
      result.destructorOf = some cxxNamePair(name, cncProc)

    else:
      result.constructorOf = some cxxNamePair(name, cncProc)

  result.userData = cast[pointer](main)
  result.head.genParams = params


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
      var arg = toCxxArg(arg, idx)
      if not (arg.typ of ctkPod and
              arg.typ.podKind == cptVoid):
        result.arguments.add arg.withIt do:
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


proc toCxxTypeDefinition*(node: CppNode, coms): seq[CxxEntry]

proc toCxxObject*(main: CppNode, coms): CxxObject =
  let (node, params) = unpackTemplate(main)

  var decl: CxxNamePair
  if "name" in node:
    decl = cxxPair(node["name"].strVal())

  result = cxxObject(decl)
  if "name" notin node:
    result.isAnonymous = true

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

          of cppTypeSpecSpec:
            res.nested.add toCxxObject(item, coms).withIt do:
              it.access = accs

          of cppDeclaration:
            if item[1][cpfDecl] of {
              cppFunctionDefinition, cppFunctionDeclarator }:
              toMethod(item)

            else:
              failNode(item)

          else:
            failNode item

      of cppTypeDefinition:
        for item in toCxxTypeDefinition(item, coms):
          res.nested.add withIt(item) do:
            discard
            # it.access = accs

      of cppFieldDeclaration:
        if cpfDecl in item and item[cpfDecl] of cppFunctionDeclarator:
          # Either function declaration or field with callback type
          if item[cpfDecl][cpfDecl] of cppParenthesizedDeclarator:
            # Field with callback type
            toField(item)

          else:
            # Regular function
            toMethod(item)

        elif cpfType in item and item[cpfType] of cppTypeSpecSpec:
          if "body" notin item[0]:
            # `struct ModRM modrm;` as a field
            toField(item)

          else:
            let obj = toCxxObject(item[0], coms)
            if obj.isAnonymous:
              # echov "Nested object declaration in the field"
              # debug item
              let user = optCxxNamePair(item, cncType)
              # echov user
              res.mfields.add cxxField(
                user,
                cxxTypeUse(obj, parent = some result.name, user = user))

            else:
              # echov "Standalone nested proc"
              # debug item[0]
              res.nested.add obj.withIt do:
                it.access = accs

        elif cpfType in item and item[cpfType] of { cppEnumSpecifier }:
          res.nested.add toCxxEnum(item, coms).withIt do:
            it.access = accs

        else:
          # Regular field
          toField(item)

      of cppComment:
        coms.add toCxxComment(item)

      of cppQPropertyDeclaration:
        echov "skipping qproperty"

      of cppSyntaxError, cppFriendDeclaration:
        discard

      else:
        failNode(item)

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

  elif node.len == 1 and node of cppTypeSpecSpec:
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

      elif baseBody of cppTypeSpecSpec:
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

    of cppTypeSpecSpec:
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
      case node[cpfDecl].kind:
        of cppFunctionDeclarator:
          result.add toCxxProc(node, coms)

        of cppIdentifier, cppInitDeclarator:
          # TODO skipping toplevel variable declarations
          discard

        else:
          failNode(node)

    of cppTemplateDeclaration:
      case node[1].kind:
        of cppFunctionDefinition:
          result.add toCxxProc(node, coms)

        of cppTypeSpecSpec:
          result.add toCxxObject(node, coms)

        of cppDeclaration:
          if node[1][cpfDecl] of {
            cppFunctionDefinition, cppFunctionDeclarator }:
            result.add toCxxProc(node, coms)

          else:
            failNode(node)

        else:
          failNode(node)

    of cppTemplateInstantiation:
      discard

    of cppFunctionDefinition:
      result.add toCxxProc(node, coms)

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

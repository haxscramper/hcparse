import std/[strutils, sequtils, strformat, tables, lenientops, bitops, with]

import hc_types, cxtypes, cxcommon, libclang_wrap, hc_typeconv

import hnimast, hnimast/pprint
import hmisc/macros/iflet
import hmisc/algo/htemplates
import hmisc/helpers
import hmisc/other/[colorlogger, oswrap]
import hmisc/types/colorstring

import hc_visitors, hc_types

proc toInitCall*(cursor: CXCursor, conf: WrapConfig): PNode =
  case cursor.cxKind():
    of ckUnexposedExpr:
      result = toInitCall(cursor[0], conf)

    of ckCallExpr:
      case cursor[0].cxKind():
        of ckUnexposedExpr, ckCallExpr, ckFunctionalCastExpr:
          result = toInitCall(cursor[0], conf)

        of ckIntegerLiteral:
          result = cursor[0].toInitCall(conf)

        else:
          info cursor[0].cxKind()
          debug "\n" & cursor.treeRepr(conf.unit)
          raiseAssert("#[ IMPLEMENT ]#")


      let str = "init" & $cursor.cxType()
      if result.kind in nkTokenKinds:
        result = newPCall(str, result)

      elif result.kind == nkCall and
           result[0].getStrVal() != str:
        debug result[0]
        debug newPIdent(str)
        result = newPCall(str, result)


    of ckFunctionalCastExpr:
      result = toInitCall(cursor[1], conf)

    of ckInitListExpr:
      result = newPCall("cxxInitList")
      for arg in cursor:
        result.add toInitCall(arg, conf)

    of ckIntegerLiteral, ckCharacterLiteral, ckFloatingLiteral:
      let tokens = cursor.tokenStrings(conf.unit)

      case cursor.cxKind():
        of ckIntegerLiteral:
          result = newPCall("cint", newPLit(parseInt(tokens[0])))

        of ckCharacterLiteral:
          result = newPLit(tokens[0][1])

        of ckFloatingLiteral:
          result = newPLit(parseFloat(tokens[0]))

        else:
          discard

    else:
      err "Implement for kind", cursor.cxKind()
      debug cursor.tokenStrings(conf.unit)
      debug cursor.treeRepr(conf.unit)
      raiseAssert("#[ IMPLEMENT ]#")

proc setDefaultForArg*(arg: var CArg, cursor: CXCursor, conf: WrapConfig) =
  ## Update default value for argument.
  ## - @arg{arg} :: Non-raw argument to update default for
  ## - @arg{cursor} :: original cursor for argument declaration
  ## - @arg{conf} :: Default wrap configuration

  if cursor.len == 2 and cursor[1].cxKind() == ckUnexposedExpr:
    debug cursor.treeRepr(conf.unit)
    arg.default = some(toInitCall(cursor[1], conf))
    debug arg.default

proc wrapOperator*(
    oper: CDecl,
    genParams: seq[NType[PNode]],
    conf: WrapConfig
  ): tuple[decl: GenProc, addThis: bool] =

  var it = initGenProc(oper.cursor)

  it.iinfo = currIInfo()
  it.name = oper.getNimName()
  it.genParams = genParams

  assert conf.isImportcpp
  let kind = oper.classifyOperator()
  it.kind = pkOperator

  if kind == cxoAsgnOp and it.name == "setFrom":
    it.icpp = &"# = #"
    it.kind = pkRegular

  else:
    case kind:
      of cxoAsgnOp:
        it.icpp = &"# {it.name} #"

      of cxoArrayOp:
        let rtype = oper.cursor.retType()
        let (_, mutable) = rtype.toNType(conf)
        if mutable:
          it.name = "[]="
          # WARNING potential source of horrible c++ codegen errors
          it.icpp = &"#[#]= #"

        else:
          it.icpp = &"#[#]"

      of cxoInfixOp:
        let namespace = (oper.namespace & newPType("operator")).toCppImport()

        it.icpp = &"{namespace}{it.name}(#, #)"

        if oper.args.len == 1:
          result.addThis = true

      of cxoArrowOp:
        # WARNING
        it.icpp = &"#.operator->(@)"

      of cxoCallOp:
        # NOTE nim does have experimental support for call
        # operator, but I think it is better to wrap this one as
        # separate function `call()`
        it.name = "call"
        it.kind = pkRegular
        it.icpp = &"#(@)"

      of cxoDerefOp:
        it.name = "[]"
        it.icpp = &"*#"

      of cxoPrefixOp:
        it.icpp = &"{it.name}#"

      of cxoCommaOp:
        it.name = "commaOp"
        it.icpp = &"commaOp(@)"
        it.kind = pkRegular

      of cxoConvertOp:
        let restype = oper.cursor.retType().toNType(conf).ntype

        with it:
          name = "to" & capitalizeAscii(restype.head)
          icpp = &"@"
          retType = resType
          declType = ptkConverter
          kind = pkRegular

      of cxoUserLitOp:
        let restype = oper.cursor.retType().toNType(conf).ntype

        with it:
          name = "to" & capitalizeAscii(restype.head)
          icpp = &"{oper.cursor}(@)"
          retType = restype
          kind = pkRegular

  it.header = conf.makeHeader(oper.cursor, conf)
  result.decl = it
  result.addThis = result.addThis or
    (kind in {
      cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp, cxoConvertOp
    })


proc wrapProcedure*(
    pr: CDecl,
    conf: WrapConfig,
    parent: Option[NType[PNode]],
    cache: var WrapCache,
    parentDecl: Option[CDecl],
    asNewConstructor: bool
  ): tuple[decl: WrappedEntry, canAdd: bool] =

  var it = initGenProc(pr.cursor)
  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction
    }
  )

  result.canAdd = true
  it.iinfo = currIInfo()

  if pr.isOperator():
    var genp: seq[NType[PNode]]
    iflet (par = parent):
      genp.add par.genParams

    let (decl, adt) = pr.wrapOperator(genp, conf)
    it = decl
    it.iinfo = currIInfo()
    addThis = adt

  else:
    it.name = pr.getNimName()

    iflet (par = parent):
      it.genParams = par.genParams

    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      it.iinfo = currIInfo()
      if pr.cursor.isStatic():
        let namespace = (@[parent.get()]).toCppImport()
        addThis = false
        it.icpp = &"{namespace}::{it.name}(@)"
      else:
        it.icpp = &"#.{it.name}(@)"

      it.header = conf.makeHeader(pr.cursor, conf)

    else:
      if conf.isImportcpp:
        let namespace = pr.namespace.toCppImport()
        if namespace.len > 1:
          it.icpp = &"{namespace}::{it.name}(@)"

        else:
          it.icpp = &"{it.name}(@)"

      else:
        it.icpp = &"{it.name}"

      it.iinfo = currIInfo()
      it.header = conf.makeHeader(pr.cursor, conf)

  if addThis:
    assert parent.isSome()
    it.args.add initCArg(
      "self", parent.get(), pr.cursor.isConstMethod.tern(nvdVar, nvdLet))

  for arg in pr.args:
    var (vtype, mutable) = arg.cursor.cxType().toNType(conf)
    if vtype.kind in {ntkIdent, ntkGenericSpec}:
      if vtype.head == "UNEXPOSED":
        # WARNING currently parameters which contain `tkUnexposed`
        # types are not handled but are skipped instead. I don't
        # know how to fix right now.
        result.canAdd = false

      if parentDecl.isSome() and
         arg.cursor.inheritsGenParamsOf(parentDecl.get().cursor) and
         parent.isSome() and
         (arg.cursor.cxType().kind notin {tkUnexposed})
        :
        # WARNING nested class definitions with additional template
        # parameters are not handled right now. It will break for
        # code like
        # `<Ta> struct A { <Tb> struct B {void func(); }; };`
        # and only add `Tb` as template parameter for `func()`.
        vtype.add parent.get().genParams

    else:
      # FIXME determine and implement edge case handling for procvar
      # arguments

      # WARNING might cause duplication, for wrapping C++ functors better
      # handling should be implemented
      vtype.pragma.add newPident("cdecl")

    var newArg = initCArg(fixIdentName(arg.name), vtype, mutable)
    setDefaultForArg(newArg, arg.cursor, conf)
    it.args.add newArg


  if pr.isOperator and pr.classifyOperator() == cxoAsgnOp:
    # Force override return type for assignment operators
    it.retType = newPType("void")

  elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
    # Override handling of return types for constructors
    if not pr.isOperator():
      # But ignore implicit user-defined conversion functions like
      # `operator T()`
      assert parent.isSome(), "Cannot wrap constructor without parent object"

      it.iinfo = currIInfo()
      it.header = conf.makeHeader(pr.cursor, conf)
      if asNewConstructor:
        it.retType = newNType("ptr", @[parent.get()])
        it.icpp = &"new {parent.get().head}(@)"

      else:
        it.retType = parent.get()
        it.icpp = &"{parent.get().head}(@)"
  else:
    # Default handling of return types
    var (rtype, mutable) = toNType(pr.cursor.retType(), conf)
    if parentDecl.isSome() and
       parent.isSome() and
       pr.cursor.retType().
       getTypeDeclaration().
       inheritsGenParamsOf(parentDecl.get().cursor):

      rtype.genParams = parent.get().genParams

    it.retType = rtype

    if rtype.hasUnexposed():
      # WARNING dropping all methods that use `tkUnexposed` type
      # in return value. This must be fixed in future versions.
      result.canAdd = false

    if pr.cursor.cxkind == ckDestructor:
      # Implicitly calling destructor on object (if someone ever needs
      # something like that)
      it.icpp = &"~{it.name}()"
      it.header = conf.makeHeader(pr.cursor, conf)

  if pr.cursor.kind == ckDestructor:
    it.name = "destroy" & it.name

  elif pr.cursor.cxkind in {ckConstructor, ckConversionFunction}:
    if not pr.isOperator():
      it.name = tern(asNewConstructor, "new", "init") &
        it.name.capitalizeAscii()

  if pr.cursor.isVariadic() == 1:
    it.pragma.add newPIdent("varargs")

  result.decl = newWrappedEntry(it)


proc fixNames(
  ppd: var GenProc, conf: WrapConfig, parent: NType[PNode]) =

  var idx: int = 0
  for param in mitems(ppd.genParams):
    conf.fixTypeName(param, conf, 0)
    inc idx

  idx = 0
  for arg in mitems(ppd.args):
    conf.fixTypeName(arg.ntype, conf, idx)
    inc idx

  conf.fixTypeName(ppd.retType, conf, 0)

  ppd.name = ppd.name.fixIdentName()





proc wrapMethods*(
  cd: CDecl, conf: WrapConfig,
  parent: NType[PNode], cache: var WrapCache): seq[WrappedEntry] =
  assert cd.kind in {cdkClass, cdkStruct}
  for meth in cd.methods({
    ckMethod, ckDestructor, ckConstructor, ckConversionFunction
  }):
    if meth.cursor.cxKind() in {ckConstructor, ckConversionFunction}:
      block:
        # Wrap as `new` constructor
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some(parent), cache, some(cd), true)

        if canAdd:
          result.add decl

      block:
        # Wrap as `init` constructor
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some(parent), cache, some(cd), false)

        if canAdd:
          result.add decl
    else:
      let (decl, canAdd) = wrapProcedure(
        meth, conf, some(parent), cache, some(cd), true)

      if canAdd:
        result.add decl


  for decl in mitems(result):
    fixNames(decl.gproc, conf, parent)

  result = result.deduplicate()

proc wrapFunction*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): seq[WrappedEntry] =

  var (decl, canAdd) = wrapProcedure(
    cd, conf, none(NType[PNode]), cache, none(CDecl), false)

  if canAdd:
    result.add decl


proc wrapTypeFromNamespace(
  namespace: CNamespace, conf: WrapConfig, cursor: CXCursor): PObjectDecl =
  ## `cursor` points to type declaration being wrapped
  # WARNING for now I assume that 'UNEXPOSED' type only occurs in
  # situations like `std::move_iterator<'0>::pointer` where typedef
  # uses it's semantic parent (class or struct declaration) to get
  # template parameters. This might not be a valid assumption in
  # general case.k

  var name: NType[PNode] = namespace.toNType()

  conf.fixTypeName(name, conf, 0)

  result = PObjectDecl(name: name, exported: true)

  result.annotation = some(newPPragma(
    newExprColonExpr(
      newPIdent (if conf.isImportcpp: "importcpp" else: "importc"),
      namespace.toCppImport().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cursor, conf).toNNode())))




proc wrapObject*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): tuple[
    obj: WrappedEntry, genAfter, genBefore: seq[WrappedEntry]
  ]

proc wrapAlias*(
    al: CDecl, parent: CNamespace, conf: WrapConfig, cache: var WrapCache,
  ): seq[WrappedEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, # *APtr` shit that can result in multple
  # declarations.

  var al = al
  al.name = al.inNamespace(parent)
  conf.fixTypeName(al.name, conf, 0)

  let aliasof = al.cursor.cxType().getCanonicalType()
  var full = aliasof.toNType(conf).ntype
  if aliasof.getNumTemplateArguments() > 0:
    let required =
      aliasof.getTypeDeclaration().
      getSpecializedCursorTemplate().
      requiredGenericParams()

    # WARNING HACK - ignore all template parameters that /might/ be
    # defaulted - i.e. only ones that *must* be specified (not
    # defaulted in declaration) are included.
    full.genParams = full.genParams[0 ..< required.len()]

  if full.hasUnexposed():
    let namespace = parent & @[newPType($al.cursor)]
    result = @[newWrappedEntry(
      toNimDecl(
        namespace.wrapTypeFromNamespace(conf, al.cursor)
      ),
      al, al.cursor
    )]

  else:
    if al.cursor[0].cxKind() notin {ckStructDecl}:
      # NOTE ignore `typedef struct` in C
      result = @[newWrappedEntry(
        toNimDecl(newAliasDecl(
          al.name, full, iinfo = currIInfo())), al, al.cursor
      )]
      
    else:
      if cache.canWrap(al.cursor[0]):
        # Multiple trailing typedefs result in several `ckTypedefDecl`
        # nodes

        cache.markWrap(al.cursor[0])
        let nested = visitClass(al.cursor[0], parent, conf)

        let (obj, pre, post) = wrapObject(nested, conf, cache)

        result.add pre & @[obj] & post

      if al.name != full:
        # `typedef struct {} A;` has the same typedef name and type, and
        # should only be wrapped as type definition and not alias.
        result.add newWrappedEntry(
          toNimDecl(newAliasDecl(
            al.name, full,
            iinfo = currIInfo(),
            isDistinct = false,
          )), al, al.cursor
        )

proc getParentFields*(
    inCursor: CXCursor, obj: PObjectDecl, wrapConf: WrapConfig
  ): seq[PProcDecl] =

  for class in inCUrsor.getClassBaseCursors():
    for entry in class:
      if entry.kind in {ckFieldDecl}:
        let
          (fldType, _) = entry.cxType().toNType(wrapConf)
          fldname = $entry


        result.add newPProcDecl(
          name = fldName,
          rtyp = some(fldType),
          args = { "self" : obj.name },
          iinfo = currIInfo(),
          pragma = newPPragma(newExprColonExpr(
            newPIdent "importcpp", newRStrLit(&"#.{fldName}")))
        )

        result[^1].genParams.add(obj.name.genParams)

        result[^1].addCodeComment(
          &"Parent field getter passtrough from {class}\n")

        if not entry.cxType().isConstQualifiedDeep():
          result.add newPProcDecl(
            name = fldName,
            iinfo = currIInfo(),
            args = { "self" : obj.name, "val" : fldType },
            pragma = newPPragma(newExprColonExpr(
              newPIdent "importcpp", newRStrLit(&"#.{fldName} = @")))
          )

          result[^1].genParams.add(obj.name.genParams)
          result[^1].signature.arguments[0].kind = nvdVar
          result[^1].addCodeComment(
            &"Parent field assignment passtrough from {class}\n")

          result[^1].kind = pkAssgn


proc wrapEnum*(declEn: CDecl, conf: WrapConfig): seq[WrappedEntry]

proc wrapObject*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): tuple[
    obj: WrappedEntry, genAfter, genBefore: seq[WrappedEntry]
  ] =

  let tdecl = cd.cursor.cxType().getTypeDeclaration()

  assert cd.kind in {cdkClass, cdkStruct}
  var obj = PObjectDecl(
    name: cd.inNamespace(cd.namespace),
    exported: true, iinfo: currIInfo(),
  )

  for entry in cd.cursor:
    case entry.cxKind():
      of ckEnumDecl:
        let visited = visitEnum(entry, cd.namespace & @[cd.name], conf)
        result.genBefore.add wrapEnum(visited, conf)

      of ckStructDecl, ckClassDecl, ckUnionDecl:
        let visited = visitClass(entry, cd.namespace & @[cd.name], conf)
        let (obj, pre, post) = wrapObject(visited, conf, cache)
        result.genBefore.add pre & @[obj] & post

      of ckFieldDecl, ckMethod, ckFriendDecl,
         ckFunctionTemplate, ckAccessSpecifier,
         ckConstructor, ckDestructor:
        # Constructors, field access and other implementation parts have
        # already been added in `visitClass`, now we can ignore them
        # altogether.
        discard

      else:
        warn &"#[ IMPLEMENT for kind {entry.cxkind()} {instantiationInfo()} ]#"

  # WARNING might die on `<T<T<T<T<T<T>>>>>` things
  for fld in cd.pubFields:
    var resFld = PObjectField(
      isTuple: false,
      name: fixIdentName(fld.name.head),
      exported: true,
      annotation: some newPPragma(
        newExprColonExpr(
          newPIdent("importc"), newRStrLit(fld.name.head))),
    )

    if fld.cursor[0].cxKind() in {ckUnionDecl, ckStructDecl}:
      let namespace = cd.namespace & @[newPType(cd.name.head)]
      var nested = visitClass(fld.cursor[0], namespace, conf)

      nested.name.head = resFld.name

      nested.name = nested.inNamespace(@[])

      let (obj, pre, post) = wrapObject(nested, conf, cache)

      result.genBefore.add(pre & @[obj] & post)

      resFld.fldType = obj.wrapped.objectDecl.name 

    else:
      resFld.fldType = fld.cursor.cxType().toNType(conf).ntype


    obj.flds.add resFld

  obj.annotation = some(newPPragma(
    newExprColonExpr(
      newPIdent (if conf.isImportcpp: "importcpp" else: "importc"),
      cd.namespaceName().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cd.cursor, conf).toNNode()
    ),
  ))

  if cd.cursor.cxKind() == ckUnionDecl:
    obj.annotation.get().add newPIdent("union")

  result.genAfter = cd.wrapMethods(conf, obj.name, cache)
  conf.fixTypeName(obj.name, conf, 0)

  for mem in cd.members:
    case mem.kind:
      of cdkAlias:
        result.genBefore.add mem.wrapAlias(
          cd.namespace & @[cd.name], conf, cache)
      else:
        discard

  result.genAfter.add getParentFields(cd.cursor, obj, conf).mapIt(
    newWrappedEntry(toNimDecl(it)))

  result.obj = newWrappedEntry(toNimDecl(obj), cd, cd.cursor)


proc getFields*(declEn: CDecl, conf: WrapConfig): tuple[
    namedvals: Table[string, BiggestInt],
    enfields: seq[tuple[name: string, value: Option[EnFieldVal]]]
  ] =

  for (name, value) in declEn.flds:
    let val = value.get()
    var resval: Option[EnFieldVal]
    case val.kind:
      of ckIntegerLiteral:
        resVal = some initEnFieldVal(
          val.tokenStrings(conf.unit)[0].parseInt())

      of ckBinaryOperator:
        # FIXME C++ allows to have arbitrary complex expressions for enum
        # values, so in the end I would have to implement simple math
        # expression AST interpreter in order to evaluate all of this, or
        # take some existing one (it is possible to convert expression back
        # to tokens and use something like
        # https://github.com/Yardanico/nim-mathexpr)
        let subn = val.children()
        let toks = val.tokenStrings(conf.unit)[1] # TEST for `(1 << 2) | (1 << 3)`
        case toks:
          of "<<":
            resval = some initEnFieldVal(
              subn[0].tokenStrings(conf.unit)[0].parseInt() shl
              subn[1].tokenStrings(conf.unit)[0].parseInt(),
            )

          of "|":
            let toks = val.tokenStrings(conf.unit)
            # NOTE assuming `EnumField | OtherField` for now
            let
              lhs = toks[0]
              rhs = toks[2]
              lhsVal = result.namedVals[lhs]
              rhsVal = result.namedVals[rhs]

            resval = some initEnFieldVal(bitor(lhsVal, rhsVal))

          else:
            discard

      of ckUnaryOperator:
        let toks = val.tokenStrings(conf.unit)
        case toks[0]:
          of "-":
            resval = some initEnFieldVal(toks[1].parseInt())

          else:
            raiseAssert("#[ IMPLEMENT ]#")

      elif $val.kind == "OverloadCandidate": # HACK
        resval = none EnFieldVal

      else:
        raiseAssert(
          &"#[ IMPLEMENT for kind {val.kind} {instantiationInfo()} ]#")

    result.enfields.add (name: name, value: resval)

proc wrapEnum*(declEn: CDecl, conf: WrapConfig): seq[WrappedEntry] =
  ## Generate wrapper for enum declaration
  ##
  ## Generates wrapper for enum declaration, using wrap configuration.
  ## Wrapping is performed in two steps - underlying C enum is wrapped
  ## as-is, and additional nim `enum` is generated. Nim version does not
  ## have holes, which allows it to be used in `array`, iterated upon etc.
  ##
  ## In order to perform conversion between 'proxy' and underlying enum
  ## several helper procs are introduces, such as `toInt`.


  var nt = declEn.inNamespace(declEn.namespace)
  conf.fixTypeName(nt, conf, 0)
  let namespace = (declEn.namespace & newPType($declEn.cursor)).toCppImport()

  var ennames: seq[string]

  proc cEnumName(str: string): string {.closure.} =
    result = nt.head
    result[0] = result[0].toLowerAscii()
    result &= "_" & str

  var vals: OrderedTable[string, tuple[
    resName: string,
    resVal: BiggestInt,
    stringif: string
  ]]

  let implName = nt.head & "_Impl"
  block:
    # Generate wrapper for default implementation of the enum
    var implEn = newPEnumDecl(name = implName, iinfo = currIInfo())

    implEn.pragma.add newPIdentColonString(
      (if conf.isImportcpp: "importcpp" else: "importc"),
      namespace
    )

    implEn.exported = true


    # Get list of all enum fields with values, construct table of values
    # without filtering.
    let (namedvals, enfields) = getFields(declEn, conf)
    var fldVals: Table[BiggestInt, string]
    var repeated: Table[string, seq[string]]

    for (key, val) in enfields:
      if val.isSome():
        if val.get().isRefOther:
          repeated.mgetOrPut(val.get().othername, @[ key ]).add key
        else:
          let val = val.get().value
          if val notin fldVals:
            fldVals[val] = key
          else:
            repeated.mgetOrPut(fldVals[val], @[ key ]).add key


    # List of field with respective values. Holes are filled with correct
    # values, and duplicated fields are dropped.
    var flds: seq[(string, BiggestInt)]

    block:
      var prev: BiggestInt = 0
      for (key, val) in enfields:
        if val.isSome():
          if val.get().isRefOther:
            discard
          else:
            prev = val.get().value
            flds.add (key, prev)

        else:
          inc prev
          flds.add (key, prev)


      # Sort fields based on value
      flds = flds.sorted(
        proc(f1, f2: (string, BiggestInt)): int {.closure.} =
          cmp(f1[1], f2[1])
      )

    block:
      # Generate wrapped for C enum. Each field has value assigned to it.
      var prev = BiggestInt(-120948783)
      for (name, val) in flds:
        if val != prev:
          prev = val
          implEn.addField(name.cEnumName(), some newPLit(val))

          vals[name] = (
            resName: name.cEnumName(),
            resVal: val,
            stringif:
              declEn.inNamespace(declEn.namespace).head & "::" & name
          )


    result.add newWrappedEntry(toNimDecl(implEn), declEn, declEn.cursor)

  block: # Nim proxy proc declaration.
    # Determine common prefix for C++ enum (if any)
    let pref = declEn.flds.mapIt(it.fldName).commonPrefix()

    # Get name of the enum type itsel by combining first letters of
    # `PascalCase` or `snake_case` name.
    let enumPref = declEn.name.head.
      splitCamel().mapIt(it[0].toLowerAscii()).join("")

    var en = newPEnumDecl(name = nt.head, iinfo = currIInfo())

    proc renameField(fld: string): string {.closure.} =
      # Drop common prefix for enum declaration and add one generated from
      # enum name
      fld.dropPrefix(pref).dropPrefix("_").addPrefix(enumPref)

    # Metadata associated with proxy enum
    var arr = nnkBracket.newPTree()

    for name, wrap in vals:
      # Add fields to nim enum without values
      en.addField(name.renameField())

      arr.add pquote do:
        (
          name: `newPLit(name)`, # Name of the original enum
          cEnum: `newPIdent(name.cEnumName())`, # Original enum value
          cName: `newPLit(wrap.stringif)`, # Namespaced C++ enum name
          value: `newPLit(wrap.resVal)` # Integer value for field
        )

    let
      enName = newPIdent(en.name)
      arrName = newPIdent("arr" & en.name & "mapping")

    let helpers = pquote do:
      const `arrName`: array[`enName`, tuple[
        name: string,
        cEnum: `newPIdent(implName)`,
        cName: string,
        value: int
      ]] = `arr`

      # Convert proxy enum to integer value
      proc toInt*(en: `enName`): int {.inline.} =
        `arrName`[en].value

      # Convert set of enums to bitmasked integer
      proc toInt*(en: set[`enName`]): int {.inline.} =
        for val in en:
          result = bitor(result, `arrName`[val].value)

      # Return namespaced name of the original enum
      proc `$`*(en: `enName`): string {.inline.} =
        `arrName`[en].cName



    result.add newWrappedEntry(toNimDecl(helpers), true)

    en.exported = true

    result.add newWrappedEntry(toNimDecl(en), declEn, declEn.cursor)

proc wrapMacros*(declMacros: seq[CDecl], conf: WrapConfig): seq[WrappedEntry] =
  return

  info "Wrapping macros"
  for decl in declMacros:
    debug decl.cursor
    logIndented:
      for (tok, kind) in decl.cursor.tokenKinds(conf.unit):
        debug tok.toGreen(), kind

      # debug decl.cursor.tokenStrings(conf.unit).join(", ", ("<\e[31m", "\e[39m>"))


proc wrapApiUnit*(
  api: CApiUnit, conf: WrapConfig,
  cache: var WrapCache, index: FileIndex): seq[WrappedEntry] =
  ## Generate wrapper for api unit.
  var macrolist: seq[CDecl]
  for decl in api.decls:
    if cache.canWrap(decl.cursor):
      cache.markWrap(decl.cursor)
    else:
      continue

    case decl.kind:
      of cdkClass:
        identLog()
        let spec = decl.cursor.getSpecializedCursorTemplate()

        if spec.cxKind() != ckFirstInvalid:
          discard

        else:
          let (obj, procs, other) = decl.wrapObject(conf, cache)

          result.add obj

          for it in other:
            result.add it

          for pr in procs:
            result.add pr

        dedentLog()

      of cdkAlias:
        result.add decl.wrapAlias(decl.namespace, conf, cache)

      of cdkEnum:
        result.add decl.wrapEnum(conf)

      of cdkFunction:
        for f in decl.wrapFunction(conf, cache):
          result.add f

      of cdkMacro:
        macrolist.add decl

      else:
        debug decl.kind

  result.add wrapMacros(macrolist, conf)

proc getNType*(carg: CArg): NType[PNode] =
  if carg.isRaw:
    raiseAssert("#[ IMPLEMENT ]#")

  else:
    return carg.ntype

proc toNNode*(gp: GenProc): PProcDecl =
  result = newPProcDecl(
    name = gp.name,
    iinfo = gp.iinfo,
    exported = true,
    rtyp = some(gp.retType),
    genParams = gp.genParams,
  )

  for arg in gp.args:
    result.signature.arguments.add newNIdentDefs(
      vname = arg.name,
      value = arg.default,
      vtype = arg.getNTYpe(),
      kind = arg.varkind
    )

  result.signature.pragma = gp.pragma

  result.signature.pragma.add(
    newPIdentColonString("importcpp", gp.icpp))

  result.signature.pragma.add(
    newExprColonExpr(newPIdent "header", gp.header.toNNode()))

